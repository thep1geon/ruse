// TODO: 
//      Tail call optimization
//      Write Ruse in Ruse?
//
// Standard Library TODO:
//      cond
//      String manipluation
//          - Various string to other types
//      IO  - Piggy back off of C's IO
//          File handling
//          Printing
//          User input

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <editline/readline.h>
#include <string.h>
#include <unistd.h>
#include <pigeon/arena.h>
#include <pigeon/allocator.h>
#include <pigeon/string.h>

Arena     g_arena = {0};
Allocator g_allocator = {0};

// Lexing

char*     g_start = 0, *g_current = 0;
u32       g_line = 1;

typedef enum {
    TOKEN_LPAREN,
    TOKEN_RPAREN,

    TOKEN_NUMBER,
    TOKEN_SYMBOL,
    TOKEN_STR,

    TOKEN_ERROR,
    TOKEN_EOF,
} TokenType;

typedef struct token {
    TokenType type;
    char* lexeme;
    u16 len;
    u16 line;
} Token;

Token token_new(TokenType type) {
    return (Token) {
        .type = type,
        .lexeme = g_start,
        .len = (u16)(g_current - g_start),
        .line = g_line,
    };
}

Token token_new_error(char* msg) {
    return (Token) {
        .type = TOKEN_ERROR,
        .line = g_line,
        .lexeme = msg,
        .len = strlen(msg),
    };
}

void lex(char* src) {
    g_current = src; 
}

static inline bool is_digit(char c) {
    return '0' <= c && c <= '9';
}

static inline bool is_space(char c) {
    return c == ' ' || c == '\r' || c == '\t';
}

static inline bool is_alpha(char c) {
    return ('A' <= c && c <= 'Z') ||
    ('a' <= c && c <= 'z') || c == '_';
}

static inline bool is_special(char c) {
    switch (c) {
        case '?':
        case '!':
        case '.':
        case '+':
        case '-':
        case '*':
        case '/':
        case '<':
        case '=':
        case '>':
        case ':':
        case '$':
        case '%':
        case '^':
        case '&':
        case '_':
        case '~': return true;
        default:  return false;
    }
}

static inline bool is_symbol(char c) {
    return is_alpha(c) || is_special(c);
}

Token next_token() {
next_token: // Tail-call recusion in C goes crazy
    g_start = g_current;
    if (*g_current == '\0') return token_new(TOKEN_EOF);

    char c = *g_current++;

    if (is_space(c)) goto next_token;

    switch (c) {
        case '(': return token_new(TOKEN_LPAREN);
        case ')': return token_new(TOKEN_RPAREN);
        case '"': {
            g_start++;
            while ((c = *g_current++) != '"') {
                if (c == '\0') 
                    return token_new_error("Unterminated string");
            }

            g_current--;

            Token tok = token_new(TOKEN_STR);

            g_current++;

            return tok;
        }
        case ';': {
            while (*g_current != '\n' && *g_current != '\0') {
                g_current++;
            }
            goto next_token;
        }
        case '\n': {
            g_line++;
            goto next_token;
        }
    }

    if (is_digit(c)) {
        char* ptr = g_current;
        while (is_digit(*ptr) || *ptr == '.') {
            g_current++;
            ptr = g_current;
        }

        return token_new(TOKEN_NUMBER);
    }

    if (is_symbol(c)) {
        char* ptr = g_current;
        while (is_digit(*ptr) || is_symbol(*ptr)) {
            g_current++;
            ptr = g_current;
        }

        return token_new(TOKEN_SYMBOL);
    }

    return token_new_error("Unexpected character");
}

// End Lexing
//
// Parsing
typedef struct LexResult LexResult;
typedef struct ParseResult ParseResult;
typedef struct EvalResult EvalResult;

typedef struct Env Env;
EvalResult file(Env* env, const char* path);
void repl(Env* env);

typedef struct Expr Expr;

typedef struct Atom Atom;
// Tagged unions baby!
struct Atom {
    enum {
        ATOM_NUMBER,
        ATOM_SYMBOL,
        ATOM_BOOL,
        ATOM_STR,
        ATOM_NIL,
    } tag;

    union {
        f64 number;

        struct {
            char* sym;
            u16 len;
        } str;

        bool b;
    } as;
};

Atom* atom_new_number(f64 n);
Atom* atom_new_symbol(char* sym, u16 len);
Atom* atom_new_bool(bool b);
Atom* atom_new_str(char* sym, u16 len);
Atom* atom_new_nil();
bool atom_is_truthy(Atom* atom);
void atom_print(Atom* atom);

typedef struct Cons Cons;
struct Cons {
    Expr* car;
    Cons* cdr;
};
Cons* cons_new(Expr* car, Cons* cdr);
u32 cons_len(Cons* cons);
void cons_print(Cons* cons);

typedef EvalResult(*Native)(Cons*,Env*);
typedef struct Lambda Lambda;

struct Expr {
    enum {
        EXPR_ATOM,
        EXPR_CONS,
        EXPR_NATIVE,
        EXPR_LAMBDA,
    }tag;

    union {
        Atom* atom;
        Cons* cons;
        Native native;
        Lambda* lambda;
    } as;
};

Expr* expr_new_atom(Atom* atom);
Expr* expr_new_cons(Cons* cons);
Expr* expr_new_native(Native native);
Expr* expr_new_lambda(Lambda* lambda);
void expr_print(Expr* expr);
bool expr_is_truthy(Expr* expr);

struct Env {
    Cons* pairs;
    Env* parent;
};

Env env_new(Env* parent) {
    return (Env) {
        .pairs = cons_new(NULL, NULL),
        .parent = parent,
    };
}

Expr* env_lookup(const Env* env, String key) {
    Cons* pair = env->pairs;
    while (pair->car) {
        assert(pair->car->tag == EXPR_CONS);
        Cons* entry = pair->car->as.cons;

        assert(entry->car->tag == EXPR_ATOM && entry->car->as.atom->tag == ATOM_STR);
        Atom* str = entry->car->as.atom;
        String entry_key = string_from_parts(str->as.str.sym, str->as.str.len);

        Expr* entry_expr = entry->cdr->car;

        if (string_eq(&key, &entry_key)) {
            return entry_expr;
        }

        pair = pair->cdr;
    }

    if (env->parent) {
        return env_lookup(env->parent, key);
    }

    return NULL;
}

void env_bind(Env* env, String key, Expr* expr) {
    Expr* entry = expr_new_cons(
                    cons_new(
                        expr_new_atom(atom_new_str((char*)key.data, key.len)), 
                        cons_new(expr, NULL)));

    env->pairs = cons_new(entry, env->pairs);
}

Atom* atom_new_number(f64 n) {
    Atom* atom = alloc(g_allocator, sizeof(Atom));
    atom->tag = ATOM_NUMBER;
    atom->as.number = n;
    return atom;
}

Atom* atom_new_symbol(char* sym, u16 len) {
    Atom* atom = alloc(g_allocator, sizeof(Atom));
    atom->tag = ATOM_SYMBOL;
    atom->as.str.sym = sym;
    atom->as.str.len = len;
    return atom;
}

Atom* atom_new_str(char* sym, u16 len) {
    Atom* atom = alloc(g_allocator, sizeof(Atom));
    atom->tag = ATOM_STR;
    atom->as.str.sym = sym;
    atom->as.str.len = len;
    return atom;
}

Atom* atom_new_bool(bool b) {
    Atom* atom = alloc(g_allocator, sizeof(Atom));
    atom->tag = ATOM_BOOL;
    atom->as.b = b;
    return atom;
}

Atom* atom_new_nil() {
    Atom* atom = alloc(g_allocator, sizeof(Atom));
    atom->tag = ATOM_NIL;
    return atom;
}

bool atom_is_truthy(Atom* atom) {
    switch (atom->tag) {
        case ATOM_SYMBOL: return true;
        case ATOM_STR: return atom->as.str.len > 0;
        case ATOM_NUMBER: return atom->as.number > 0;
        case ATOM_BOOL: return atom->as.b;
        case ATOM_NIL: return false;
    }

    assert(0 && "unreachable");
}

void atom_print(Atom* atom) {
    switch (atom->tag) {
        case ATOM_NUMBER: {
            printf(" %.2f ", atom->as.number);
            break;
        }

        case ATOM_SYMBOL: {
            printf(" %.*s ", atom->as.str.len, atom->as.str.sym);
            break;
        }

        case ATOM_STR: {
            printf(" %.*s ", atom->as.str.len, atom->as.str.sym);
            break;
        }

        case ATOM_BOOL: {
            printf(" %s ", atom->as.b ? "t" : "f");
            break;
        }

        case ATOM_NIL: {
            printf(" NIL ");
            break;
        }
    }
}

Cons* cons_new(Expr* car, Cons* cdr) {
    Cons* cons = alloc(g_allocator, sizeof(Cons));
    cons->car = car;
    cons->cdr = cdr;
    return cons;
}

u32 cons_len(Cons* cons) {
    u32 len = 0;
    Cons* curr = cons;
    while (curr->cdr) {
        curr = curr->cdr;
        len++;
    }
    return len;
}

void cons_print(Cons* cons) {
    printf("(");
    expr_print(cons->car);
    if (cons->cdr) {
        cons_print(cons->cdr);
    } else {
        printf(" NIL ");
    }
    printf(")");
}

struct Lambda {
    Expr* body;
    Cons* params;
    Env env;
};

Lambda* lambda_new(Expr* body, Cons* params, Env env) {
    Lambda* lambda =  alloc(g_allocator, sizeof(Lambda));
    lambda->body = body;
    lambda->params = params;
    lambda->env = env;
    return lambda;
}

EvalResult lambda_call(Lambda* lambda, Cons* args, Env* env);

Expr* expr_new_atom(Atom* atom) {
    Expr* expr = alloc(g_allocator, sizeof(Expr));
    expr->tag = EXPR_ATOM;
    expr->as.atom = atom;
    return expr;
}

Expr* expr_new_cons(Cons* cons) {
    Expr* expr = alloc(g_allocator, sizeof(Expr));
    expr->tag = EXPR_CONS;
    expr->as.cons = cons;
    return expr;
}

Expr* expr_new_native(Native native) {
    Expr* expr = alloc(g_allocator, sizeof(Expr));
    expr->tag = EXPR_NATIVE;
    expr->as.native = native;
    return expr;
}

Expr* expr_new_lambda(Lambda* lambda) {
    Expr* expr = alloc(g_allocator, sizeof(Expr));
    expr->tag = EXPR_LAMBDA;
    expr->as.lambda = lambda;
    return expr;
}

bool expr_is_truthy(Expr* expr) {
    if (!expr) return false;

    switch (expr->tag) {
        case EXPR_ATOM: return atom_is_truthy(expr->as.atom);
        case EXPR_LAMBDA: return true;
        case EXPR_CONS: return cons_len(expr->as.cons) > 0;
        case EXPR_NATIVE: return true;
    }

    assert(0 && "unreachable");
}

void expr_print(Expr* expr) {
    if (!expr) return;

    switch (expr->tag) {
        case EXPR_ATOM: {
            atom_print(expr->as.atom);
            break;
        }

        case EXPR_CONS: {
            cons_print(expr->as.cons);
            break;
        }

        case EXPR_NATIVE: {
            printf("<native fn>");
            break;
        }

        case EXPR_LAMBDA: {
            printf("<lambda fn>");
            break;
        }
    }

}

struct ParseResult {
    enum {
        PARSE_OK,
        PARSE_EOF,
        PARSE_UNEXPECTED_TOKEN,
        PARSE_TYPE_COUNT,
    } tag;

    union {
        Expr* expr;

        struct {
            u16 len;
            char* errmsg;
        } errmsg;
    } as;
};

const char* parse_result_lut[PARSE_TYPE_COUNT] = {
    [PARSE_OK] = "OK",
    [PARSE_EOF] = "EOF",
    [PARSE_UNEXPECTED_TOKEN] = "UNEXPECTED TOKEN",
};

ParseResult parse_result_ok(Expr* expr) {
    return (ParseResult) {
        .tag = PARSE_OK,
        .as.expr = expr,
    };
}

ParseResult parse_result_err(int tag, char* msg) {
    return (ParseResult) {
        .tag = tag,
        .as.errmsg = {
            .len = strlen(msg),
            .errmsg = msg,
        },
    };
}

ParseResult parse_expression(Token tok);
ParseResult parse_list(Token tok);
ParseResult parse_atom(Token tok);

ParseResult parse(char* src) {
    lex(src);

    return parse_expression(next_token());
}

ParseResult parse_expression(Token tok) {
    if (tok.type == TOKEN_EOF) return parse_result_err(PARSE_EOF, "EOF");

    if (tok.type == TOKEN_LPAREN) return parse_list(tok);

    return parse_atom(tok);
}

ParseResult parse_list(Token tok) {
    Cons* cons = cons_new(NULL, NULL);
    Cons* curr = cons;

    while ((tok = next_token()).type != TOKEN_RPAREN) {
        if (tok.type == TOKEN_EOF) break;
        ParseResult result = parse_expression(tok);
        if (result.tag != PARSE_OK) return result;
        curr->car = result.as.expr;
        curr->cdr = cons_new(NULL, NULL);
        curr = curr->cdr;
    } 

    if (tok.type == TOKEN_EOF)
        return parse_result_err(PARSE_UNEXPECTED_TOKEN, "Expected closing parathesis");

    return parse_result_ok(expr_new_cons(cons));
}

ParseResult parse_atom(Token tok) {
    switch (tok.type) {
        case TOKEN_NUMBER: {
            char c = tok.lexeme[tok.len];
            tok.lexeme[tok.len] = '\0';
            f64 n = atof(tok.lexeme);
            tok.lexeme[tok.len] = c;
            return parse_result_ok(expr_new_atom(atom_new_number(n)));
        }

        case TOKEN_SYMBOL: 
            return parse_result_ok(expr_new_atom(atom_new_symbol(tok.lexeme, tok.len))); 

        case TOKEN_STR: 
            return parse_result_ok(expr_new_atom(atom_new_str(tok.lexeme, tok.len))); 

        default: 
            return (ParseResult){
                .tag = PARSE_UNEXPECTED_TOKEN,
                .as.errmsg = {
                    .len = tok.len,
                    .errmsg = tok.lexeme,
                },
            };
    }
}

// End parsing
//
// Evaluating

struct EvalResult {
    enum {
        EVAL_OK,
        EVAL_INVALID_TYPE,
        EVAL_INVALID_ARG_COUNT,
        EVAL_UNDEFINED_VARIABLE,
        EVAL_ERR,
        EVAL_TYPE_COUNT,
    } tag;

    union {
        Expr* expr;

        struct {
            const char* errmsg;
            u16 len;
        } errmsg;
    } as;
};

const char* eval_result_lut[EVAL_TYPE_COUNT] = {
    [EVAL_OK] = "OK",
    [EVAL_INVALID_TYPE] = "INVALID TYPE",
    [EVAL_INVALID_ARG_COUNT] = "INVALID ARG COUNT",
    [EVAL_UNDEFINED_VARIABLE] = "UNDEFINED VARIABLE",
    [EVAL_ERR] = "ERR",
};

EvalResult eval_result_ok(Expr* expr) {
    return (EvalResult){
        .tag = EVAL_OK,
        .as.expr = expr,
    };
}

EvalResult eval_result_err(int tag, const char* msg) {
    return (EvalResult){
        .tag = tag,
        .as.errmsg = {
            msg, 
            strlen(msg)
        },
    };
}

EvalResult eval_atom(Expr* expr, Env* env);
EvalResult eval_list(Expr* expr, Env* env);

EvalResult eval(Expr* expr, Env* env) {
    if (!expr) return eval_result_ok(expr_new_cons(cons_new(NULL, NULL)));
    switch (expr->tag) {
        case EXPR_ATOM: return eval_atom(expr, env);
        case EXPR_CONS: return eval_list(expr, env);
        default: assert(0 && "unreachable");
    }
}

EvalResult eval_atom(Expr* expr, Env* env) {
    Atom* atom = expr->as.atom;
    switch (atom->tag) {
        case ATOM_NUMBER: return eval_result_ok(expr);
        case ATOM_SYMBOL: {
            Expr* expr = env_lookup(
                env, 
                string_from_parts(atom->as.str.sym, atom->as.str.len));
            if (expr) {
                return eval_result_ok(expr);
            } else {
                return (EvalResult) {
                    .tag = EVAL_UNDEFINED_VARIABLE,
                    .as.errmsg = {
                        atom->as.str.sym, 
                        atom->as.str.len
                    },
                };
            }
            break;
        }
        case ATOM_STR: return eval_result_ok(expr);
        default: assert(0 && "unreachable");
    }
}

EvalResult eval_list(Expr* expr, Env* env) {
    Cons* cons = expr->as.cons;

    EvalResult result = eval(cons->car, env);
    if (result.tag != EVAL_OK) {
        return result;
    }

    Expr* func = result.as.expr;
    if (func->tag == EXPR_NATIVE) {
        return func->as.native(cons->cdr, env);
    } else if (func->tag == EXPR_LAMBDA) {
        return lambda_call(func->as.lambda, cons->cdr, env);
    } else {
        return eval_result_err(EVAL_INVALID_TYPE, "Symbol must eval to function or lambda");
    }
}

EvalResult lambda_call(Lambda* lambda, Cons* args, Env* env) {
    u32 arity = cons_len(lambda->params);

    if (cons_len(args) != arity)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Wrong number for args provided");

    Cons* lambda_params = lambda->params;
    Cons* argv = args;
    while (lambda_params->car && argv->car) {
        EvalResult result = eval(argv->car, env);
        if (result.tag != EVAL_OK) return result;
        Expr* expr = result.as.expr;

        env_bind(&lambda->env, 
                 string_from_parts(lambda_params->car->as.atom->as.str.sym,
                                   lambda_params->car->as.atom->as.str.len),
                 expr);

        lambda_params = lambda_params->cdr;
        argv = argv->cdr;
    }

    return eval(lambda->body, &lambda->env);
}

// End evaluating
//
// Native functions
EvalResult native_sub(Cons* args, Env* env) {
    if (cons_len(args) != 2) 
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'-' needs two numbers");

    EvalResult first_result = eval(args->car, env);
    if (first_result.tag != EVAL_OK) return first_result;
    Expr* first = first_result.as.expr;

    EvalResult second_result = eval(args->cdr->car, env);
    if (second_result.tag != EVAL_OK) return second_result;
    Expr* second = second_result.as.expr;

    if (first->tag != EXPR_ATOM || first->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "First arg must be a number");

    if (second->tag != EXPR_ATOM || second->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "Second arg must be a number");

    f64 a = first->as.atom->as.number;
    f64 b = second->as.atom->as.number;

    return eval_result_ok(expr_new_atom(atom_new_number(a - b)));
}

EvalResult native_add(Cons* args, Env* env) {
    if (cons_len(args) != 2) 
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'+' needs two numbers");

    EvalResult first_result = eval(args->car, env);
    if (first_result.tag != EVAL_OK) return first_result;
    Expr* first = first_result.as.expr;

    EvalResult second_result = eval(args->cdr->car, env);
    if (second_result.tag != EVAL_OK) return second_result;
    Expr* second = second_result.as.expr;

    if (first->tag != EXPR_ATOM || first->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "First arg must be a number");

    if (second->tag != EXPR_ATOM || second->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "Second arg must be a number");

    f64 a = first->as.atom->as.number;
    f64 b = second->as.atom->as.number;

    return eval_result_ok(expr_new_atom(atom_new_number(a + b)));
}

EvalResult native_div(Cons* args, Env* env) {
    if (cons_len(args) != 2) 
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'/' takes two numbers");

    EvalResult first_result = eval(args->car, env);
    if (first_result.tag != EVAL_OK) return first_result;
    Expr* first = first_result.as.expr;

    EvalResult second_result = eval(args->cdr->car, env);
    if (second_result.tag != EVAL_OK) return second_result;
    Expr* second = second_result.as.expr;

    if (first->tag != EXPR_ATOM || first->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "First arg must be a number");

    if (second->tag != EXPR_ATOM || second->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "Second arg must be a number");

    f64 a = first->as.atom->as.number;
    f64 b = second->as.atom->as.number;

    return eval_result_ok(expr_new_atom(atom_new_number(a / b)));
}

EvalResult native_mod(Cons* args, Env* env) {
    if (cons_len(args) != 2) 
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'%' takes two numbers");

    EvalResult first_result = eval(args->car, env);
    if (first_result.tag != EVAL_OK) return first_result;
    Expr* first = first_result.as.expr;

    EvalResult second_result = eval(args->cdr->car, env);
    if (second_result.tag != EVAL_OK) return second_result;
    Expr* second = second_result.as.expr;

    if (first->tag != EXPR_ATOM || first->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "First arg must be a number");

    if (second->tag != EXPR_ATOM || second->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "Second arg must be a number");

    f64 a = first->as.atom->as.number;
    f64 b = second->as.atom->as.number;

    return eval_result_ok(expr_new_atom(atom_new_number((i64)a % (i64)b)));
}

EvalResult native_mult(Cons* args, Env* env) {
    if (cons_len(args) != 2) 
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'*' takes two numbers");

    EvalResult first_result = eval(args->car, env);
    if (first_result.tag != EVAL_OK) return first_result;
    Expr* first = first_result.as.expr;

    EvalResult second_result = eval(args->cdr->car, env);
    if (second_result.tag != EVAL_OK) return second_result;
    Expr* second = second_result.as.expr;

    if (first->tag != EXPR_ATOM || first->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "First arg must be a number");

    if (second->tag != EXPR_ATOM || second->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "Second arg must be a number");

    f64 a = first->as.atom->as.number;
    f64 b = second->as.atom->as.number;

    return eval_result_ok(expr_new_atom(atom_new_number(a * b)));
}

EvalResult native_lessthan(Cons* args, Env* env) {
    if (cons_len(args) != 2) 
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'<' takes two numbers");

    EvalResult first_result = eval(args->car, env);
    if (first_result.tag != EVAL_OK) return first_result;
    Expr* first = first_result.as.expr;

    EvalResult second_result = eval(args->cdr->car, env);
    if (second_result.tag != EVAL_OK) return second_result;
    Expr* second = second_result.as.expr;

    if (first->tag != EXPR_ATOM || first->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "First arg must be a number");

    if (second->tag != EXPR_ATOM || second->as.atom->tag != ATOM_NUMBER)
        return eval_result_err(EVAL_INVALID_TYPE, "Second arg must be a number");

    f64 a = first->as.atom->as.number;
    f64 b = second->as.atom->as.number;

    return eval_result_ok(expr_new_atom(atom_new_bool(a < b)));

}

EvalResult native_define(Cons* args, Env* env) {
    if (cons_len(args) != 2) 
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'define' takes two args");

    if (args->car->tag != EXPR_ATOM) {
        return eval_result_err(EVAL_INVALID_TYPE, "'define' takes a symbol as the fist arg");
    }

    Atom* var = args->car->as.atom;

    EvalResult result = eval(args->cdr->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    env_bind(env, string_from_parts(var->as.str.sym, var->as.str.len), expr);
    return eval_result_ok(expr);
}

EvalResult native_list(Cons* args, Env* env) {
    (void)env;
    return eval_result_ok(expr_new_cons(args));
}

EvalResult native_cons(Cons* args, Env* env) {
    if (cons_len(args) != 2)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Expected two args");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    result = eval(args->cdr->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* list = result.as.expr;

    if (list->tag != EXPR_CONS)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Expected a list as second arg");

    return eval_result_ok(expr_new_cons(cons_new(expr, list->as.cons)));
}

EvalResult native_println(Cons* args, Env* env) {
    if (cons_len(args) != 1) 
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'println' takes two args");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;
    expr_print(expr);
    putchar('\n');
    return eval_result_ok(expr);
}

EvalResult native_car(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'car' takes one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    if (expr->tag != EXPR_CONS)
        return eval_result_err(EVAL_INVALID_TYPE, "'car' takes cons as argument");

    return eval_result_ok(expr->as.cons->car);
}

EvalResult native_cdr(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'cdr' takes one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    if (expr->tag != EXPR_CONS)
        return eval_result_err(EVAL_INVALID_TYPE, "'cdr' takes cons as argument");

    return eval_result_ok(expr_new_cons(expr->as.cons->cdr));
}

EvalResult native_lambda(Cons* args, Env* env) {
    if (cons_len(args) != 2)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'lambda' takes two args");

    if (args->car->tag != EXPR_CONS) 
        return eval_result_err(EVAL_INVALID_TYPE, "'lambda' expects a list of symbols");

    Cons* lambda_params = args->car->as.cons;

    Cons* curr = lambda_params;
    while (curr->car) {
        if (curr->car->tag != EXPR_ATOM && curr->car->as.atom->tag != ATOM_SYMBOL)
            return eval_result_err(EVAL_INVALID_TYPE, "'lambda' expects a list of symbols");

        curr = curr->cdr;
    }

    Expr* body = args->cdr->car;

    Lambda* lambda = lambda_new(body, lambda_params, env_new(env));
    return eval_result_ok(expr_new_lambda(lambda));
}

EvalResult native_do(Cons* args, Env* env) {
    Cons* curr = args;

    EvalResult result = eval_result_err(EVAL_ERR, "");
    while (curr->car) {
        result = eval(curr->car, env);
        if (result.tag != EVAL_OK) return result;

        curr = curr->cdr;
    }

    if (result.tag == EVAL_ERR) {
        return eval_result_ok(expr_new_atom(atom_new_nil()));
    }

    return result;
}

EvalResult native_let(Cons* args, Env* env) {
    if (cons_len(args) < 1)
        eval_result_err(EVAL_INVALID_ARG_COUNT, "'let' needs atleast one arg");

    if (args->car->tag != EXPR_CONS)
        return eval_result_err(EVAL_INVALID_TYPE, "'let' Expected a list of symbol and expr pairs");

    Env new_env = env_new(env);

    Cons* pairs = args->car->as.cons;
    while (pairs->car) {
        if (pairs->car->tag != EXPR_CONS)
            return eval_result_err(EVAL_INVALID_TYPE, "'let' Expected a symbol and expr list");

        Cons* pair = pairs->car->as.cons;
        
        if (cons_len(pair) != 2) 
            return eval_result_err(EVAL_INVALID_TYPE, "'let' Expected a symbol and expr list");

        if (pair->car->tag != EXPR_ATOM && pair->car->as.atom->tag != ATOM_SYMBOL)
            return eval_result_err(EVAL_INVALID_TYPE, "'let' Expected a symbol");

        Atom* sym = pair->car->as.atom;

        EvalResult result = eval(pair->cdr->car, &new_env);
        if (result.tag != EVAL_OK) return result;

        env_bind(&new_env, 
                 string_from_parts(
                     sym->as.str.sym,
                     sym->as.str.len), 
                 result.as.expr);

        pairs = pairs->cdr;
    }

    Cons* curr = args->cdr;

    EvalResult result = eval_result_err(EVAL_ERR, "");
    while (curr->car) {
        result = eval(curr->car, &new_env);
        if (result.tag != EVAL_OK) return result;

        curr = curr->cdr;
    }

    if (result.tag == EVAL_ERR) {
        return eval_result_ok(expr_new_atom(atom_new_nil()));
    }

    return result;
}

EvalResult native_cond(Cons* args, Env* env) {
    Cons* branches = args;

    while (branches->car) {
        if (branches->car->tag != EXPR_CONS)
            eval_result_err(EVAL_INVALID_TYPE, "'cond' expected a condtion and an expr");

        Cons* branch = branches->car->as.cons;

        if (cons_len(branch) != 2)
            return eval_result_err(EVAL_INVALID_TYPE, "'cond' expects a condtion and expr");

        EvalResult result = eval(branch->car, env);
        if (result.tag != EVAL_OK) return result;
        Expr* expr = result.as.expr;

        if (expr_is_truthy(expr)) {
            return eval(branch->cdr->car, env);
        }

        branches = branches->cdr;
    }

    return eval_result_ok(expr_new_atom(atom_new_nil()));
}

EvalResult native_if(Cons* args, Env* env) {
    if (cons_len(args) != 3)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'if' takes 3 args");

    Expr* condition = args->car;
    Expr* then = args->cdr->car;
    Expr* else_ = args->cdr->cdr->car;

    EvalResult result = eval(condition, env);
    if (result.tag != EVAL_OK) return result;

    if (expr_is_truthy(result.as.expr)) {
        return eval(then, env);
    } else {
        return eval(else_, env);
    }
}

EvalResult native_import(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'import' takes one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    if (expr->tag != EXPR_ATOM && expr->as.atom->tag != ATOM_STR)
        return eval_result_err(EVAL_INVALID_TYPE, "'import' takes a string");

    Atom* atom = expr->as.atom;
    char c = atom->as.str.sym[atom->as.str.len];
    atom->as.str.sym[atom->as.str.len] = '\0';

    i32 liblen = strlen("/usr/local/lib/ruse/");
    char* libname = calloc(atom->as.str.len + 1 + liblen, 1);
    memcpy(libname, (void*)"/usr/local/lib/ruse/", liblen);
    libname = strcat(libname, atom->as.str.sym);

    FILE* fd;
    if ((fd = fopen(libname, "r"))) {
        result = file(env, libname);
        fclose(fd);
    } else {
        result = file(env, atom->as.str.sym);
    }

    free(libname);

    atom->as.str.sym[atom->as.str.len-1] = c;

    return result;
}

EvalResult native_read(Cons* args, Env* env) {
    (void)env;
    if (cons_len(args) != 0)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'read' takes no args");

    #define READ_LEN 1024

    char* buffer = alloc(g_allocator, READ_LEN);

    fgets(buffer, READ_LEN, stdin);

    #undef READ_LEN

    return eval_result_ok(expr_new_atom(atom_new_str(buffer, strlen(buffer)-1)));
}

EvalResult native_cons_p(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Expected one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    return eval_result_ok(expr_new_atom(atom_new_bool(expr->tag == EXPR_CONS)));
}

EvalResult native_native_p(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Expected one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    return eval_result_ok(expr_new_atom(atom_new_bool(expr->tag == EXPR_NATIVE)));
}

EvalResult native_lambda_p(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Expected one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    return eval_result_ok(expr_new_atom(atom_new_bool(expr->tag == EXPR_LAMBDA)));
}

EvalResult native_atom_p(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Expected one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    return eval_result_ok(expr_new_atom(atom_new_bool(expr->tag == EXPR_ATOM)));
}

EvalResult native_bool_p(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Expected one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    return eval_result_ok(expr_new_atom(atom_new_bool(expr->tag == EXPR_ATOM && expr->as.atom->tag == ATOM_BOOL)));
}

EvalResult native_num_p(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Expected one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    return eval_result_ok(expr_new_atom(atom_new_bool(
        expr->tag == EXPR_ATOM && expr->as.atom->tag == ATOM_NUMBER)));
}
EvalResult native_sym_p(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Expected one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    return eval_result_ok(expr_new_atom(atom_new_bool(
        expr->tag == EXPR_ATOM && expr->as.atom->tag == ATOM_SYMBOL)));
}
EvalResult native_str_p(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Expected one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    return eval_result_ok(expr_new_atom(atom_new_bool(
        expr->tag == EXPR_ATOM && expr->as.atom->tag == ATOM_STR)));
}

EvalResult native_nil_p(Cons* args, Env* env) {
    if (cons_len(args) != 1)
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "Expected one arg");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    Expr* expr = result.as.expr;

    return eval_result_ok(expr_new_atom(atom_new_bool(
        expr->tag == EXPR_ATOM && expr->as.atom->tag == ATOM_NIL)));
}

// End native functions

void repl(Env* env) {
    while (1) {
        char* input = readline("ruse> ");

        if (!input) {
            free(input);
            break;
        }

        if (strlen(input) == 0) {
            free(input);
            putchar('\n');
            continue;
        }

        add_history(input);

        ParseResult p_result = parse(input);
        if (p_result.tag != PARSE_OK) {
            fprintf(stderr, "Parsing Error: [%s] %.*s\n\n", 
                    parse_result_lut[p_result.tag],
                    p_result.as.errmsg.len, 
                    p_result.as.errmsg.errmsg);
            continue;
        }

        EvalResult result = eval(p_result.as.expr, env);
        if (result.tag == EVAL_OK) {
            expr_print(result.as.expr);
            putchar('\n');
        } else {
            fprintf(stderr, "Eval Error: [%s] %.*s\n", 
                    eval_result_lut[result.tag],
                    result.as.errmsg.len, 
                    result.as.errmsg.errmsg);
        }
        putchar('\n');

        free(input);
    }
}

EvalResult file(Env* env, const char* path) {
    FILE* fd = fopen(path, "r");
    if (!fd)
        return eval_result_err(EVAL_ERR, "Failed to open file");

    fseek(fd, 0, SEEK_END);
    usize len = ftell(fd);

    if (len == 0) {
        fclose(fd);
        return eval_result_err(EVAL_ERR, "Empty file");
    }

    rewind(fd);

    char* input = alloc(g_allocator, len);
    fread(input, 1, len, fd);
    input[len-1] = '\0';

    fclose(fd);

    ParseResult p_result = parse(input);
    if (p_result.tag != PARSE_OK) {
        fprintf(stderr, "Parsing Error: [%s] %.*s\n\n", 
                parse_result_lut[p_result.tag],
                p_result.as.errmsg.len, 
                p_result.as.errmsg.errmsg);
        return eval_result_err(EVAL_ERR, "Failed to parse file");
    }

    EvalResult result = eval(p_result.as.expr, env);
    if (result.tag != EVAL_OK) {
        fprintf(stderr, "Eval Error: [%s] %.*s\n", 
                eval_result_lut[result.tag],
                result.as.errmsg.len, 
                result.as.errmsg.errmsg);
        return result;
    }

    return eval_result_ok(NULL);
}

i32 main(i32 argc, char** argv) {
    g_arena = arena_new();
    g_allocator = arena_to_allocator(&g_arena);

    Env env = env_new(NULL);
    env_bind(&env, string_new("-"), expr_new_native(native_sub));
    env_bind(&env, string_new("+"), expr_new_native(native_add));
    env_bind(&env, string_new("/"), expr_new_native(native_div));
    env_bind(&env, string_new("%"), expr_new_native(native_mod));
    env_bind(&env, string_new("*"), expr_new_native(native_mult));
    env_bind(&env, string_new("<"), expr_new_native(native_lessthan));

    env_bind(&env, string_new("define"), expr_new_native(native_define));
    env_bind(&env, string_new("do"), expr_new_native(native_do));
    env_bind(&env, string_new("let"), expr_new_native(native_let));
    env_bind(&env, string_new("cond"), expr_new_native(native_cond));
    env_bind(&env, string_new("println"), expr_new_native(native_println));
    env_bind(&env, string_new("if"), expr_new_native(native_if));

    env_bind(&env, string_new("cons?"), expr_new_native(native_cons_p));
    env_bind(&env, string_new("atom?"), expr_new_native(native_atom_p));
    env_bind(&env, string_new("native?"), expr_new_native(native_native_p));
    env_bind(&env, string_new("lambda?"), expr_new_native(native_lambda_p));
    env_bind(&env, string_new("num?"), expr_new_native(native_num_p));
    env_bind(&env, string_new("bool?"), expr_new_native(native_bool_p));
    env_bind(&env, string_new("str?"), expr_new_native(native_str_p));
    env_bind(&env, string_new("sym?"), expr_new_native(native_sym_p));
    env_bind(&env, string_new("nil?"), expr_new_native(native_nil_p));

    env_bind(&env, string_new("import"), expr_new_native(native_import));
    env_bind(&env, string_new("read"), expr_new_native(native_read));

    env_bind(&env, string_new("list"), expr_new_native(native_list));
    env_bind(&env, string_new("cons"), expr_new_native(native_cons));
    env_bind(&env, string_new("car"), expr_new_native(native_car));
    env_bind(&env, string_new("cdr"), expr_new_native(native_cdr));

    env_bind(&env, string_new("lambda"), expr_new_native(native_lambda));

    env_bind(&env, string_new("t"), expr_new_atom(atom_new_bool(true)));
    env_bind(&env, string_new("f"), expr_new_atom(atom_new_bool(false)));
    env_bind(&env, string_new("nil"), expr_new_atom(atom_new_nil()));

    if (argc >= 2) {
        EvalResult result = file(&env, argv[1]);
        if (result.tag != EVAL_OK) goto end;
    }

    repl(&env);

end:
    arena_free(&g_arena);
    return 0;
}
