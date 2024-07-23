// TODO: 
//      Split project into multiple files
//      Use multiple files
//      Strings
//      Garbage collector - The arena allocator will not hold forever
//      Execute files along with REPL
//      Tail call recursion
//      Write a real program in Ruse
//      
// General TODO: 
//      Improve the Env type and functions
//
// Standard Library TODO:
//      if expression
//      Rest of arithmetics: *
//      Logical operators
//      IO  - Piggy back off of C's IO
//          File handling
//          Printing
//          User input

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <editline/readline.h>
#include <string.h>
#include <pigeon/alias.h>
#include <pigeon/arena.h>
#include <pigeon/allocator.h>
#include <pigeon/string.h>

Arena     g_arena = {0};
Allocator g_allocator = {0};

// Lexing

char*     g_start = 0;
char*     g_current = 0;
u32       g_line = 1;

typedef enum {
    TOKEN_LPAREN,
    TOKEN_RPAREN,

    TOKEN_NUMBER,
    TOKEN_SYMBOL,

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
    g_start = src; 
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
    if (*g_current == '\0' || *g_current == '\n') return token_new(TOKEN_EOF);

    char c = *g_current++;

    if (is_space(c)) goto next_token;

    switch (c) {
        case '(': return token_new(TOKEN_LPAREN);
        case ')': return token_new(TOKEN_RPAREN);
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

typedef struct Expr Expr;
typedef struct Atom Atom;
typedef struct Cons Cons;
typedef EvalResult(*Native)(Cons*,Env*);
typedef struct Lambda Lambda;

#define ENV_LEN (512)
struct Env {
    Expr** data;
    u32 len;
    Env* parent;
};

Env env_new(Env* parent) {
    return (Env) {
        .len = ENV_LEN,
        .data = alloc(g_allocator, sizeof(Expr**) * ENV_LEN),
        .parent = parent,
    };
}

Expr* env_get(const Env* env, String value) {
    u64 hash = string_hash(&value) % env->len;
    Expr* expr = env->data[hash];

    if (expr) {
        return expr;
    }

    if (env->parent) {
        return env_get(env->parent, value);
    }

    return NULL;
}

void env_put(Env* env, String value, Expr* expr) {
    u64 hash = string_hash(&value) % env->len;
    printf("\t\t%lu\n", hash);
    env->data[hash] = expr;
}

// Tagged unions baby!
struct Atom {
    enum {
        ATOM_NUMBER,
        ATOM_SYMBOL,
        ATOM_BOOl,
    } tag;

    union {
        f64 number;

        struct {
            char* sym;
            u16 len;
        } symbol;

        bool b;
    } as;
};

Atom* atom_new_number(f64 n) {
    Atom* atom = alloc(g_allocator, sizeof(Atom));
    atom->tag = ATOM_NUMBER;
    atom->as.number = n;
    return atom;
}

Atom* atom_new_symbol(char* sym, u16 len) {
    Atom* atom = alloc(g_allocator, sizeof(Atom));
    atom->tag = ATOM_SYMBOL;
    atom->as.symbol.sym = sym;
    atom->as.symbol.len = len;
    return atom;
}

Atom* atom_new_bool(bool b) {
    Atom* atom = alloc(g_allocator, sizeof(Atom));
    atom->tag = ATOM_BOOl;
    atom->as.b = b;
    return atom;
}

void atom_print(Atom* atom) {
    switch (atom->tag) {
        case ATOM_NUMBER: {
            printf(" %.2f ", atom->as.number);
            break;
        }

        case ATOM_SYMBOL: {
            printf(" %.*s ", atom->as.symbol.len, atom->as.symbol.sym);
            break;
        }

        case ATOM_BOOl: {
            printf(" %s ", atom->as.b ? "t" : "f");
            break;
        }
    }
}

struct Cons {
    Expr* car;
    Cons* cdr;
};

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

void expr_print(Expr* expr);

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
            Expr* expr = env_get(
                                env, 
                                string_from_parts(atom->as.symbol.sym, atom->as.symbol.len));
            if (expr) {
                return eval_result_ok(expr);
            } else {
                return (EvalResult) {
                    .tag = EVAL_UNDEFINED_VARIABLE,
                    .as.errmsg = {
                        atom->as.symbol.sym, 
                        atom->as.symbol.len
                    },
                };
            }
        default: assert(0 && "unreachable");
        }
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

        env_put(&lambda->env, 
                string_from_parts(lambda_params->car->as.atom->as.symbol.sym,
                                  lambda_params->car->as.atom->as.symbol.len),
                expr);

        lambda_params = lambda_params->cdr;
        argv = argv->cdr;
    }

    return eval(lambda->body, &lambda->env);
}

// End evaluating
//
// Native functions
EvalResult native_len(Cons* args, Env* env) {
    if (cons_len(args) != 1) return eval_result_err(EVAL_INVALID_ARG_COUNT, "'len' needs one list");

    EvalResult result = eval(args->car, env);
    if (result.tag != EVAL_OK) return result;
    if (result.as.expr->tag != EXPR_CONS) return eval_result_err(EVAL_INVALID_TYPE, "'len' needs a list");
    Cons* list = result.as.expr->as.cons;

    return eval_result_ok(expr_new_atom(atom_new_number(cons_len(list))));
}

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

    env_put(env, string_from_parts(var->as.symbol.sym, var->as.symbol.len), expr);
    return eval_result_ok(expr);
}

EvalResult native_list(Cons* args, Env* env) {
    (void)env;
    return eval_result_ok(expr_new_cons(args));
}

EvalResult native_println(Cons* args, Env* env) {
    if (cons_len(args) != 2) 
        return eval_result_err(EVAL_INVALID_ARG_COUNT, "'define' takes two args");

    EvalResult result = eval(args->cdr->car, env);
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

// End native functions

void repl(Env* env) {
    while (1) {
        char* input = readline("ruse> ");

        if (!input) break;

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

void file(const char* path) {
    FILE* fd = fopen(path, "r");

    fseek(fd, 0, SEEK_END);
    usize len = ftell(fd);
    rewind(fd);

    printf("%lud", len);
}

i32 main(void) {
    g_arena = arena_new();
    g_allocator = arena_to_allocator(&g_arena);

    Env env = env_new(NULL);
    env_put(&env, string_new("-"), expr_new_native(native_sub));
    env_put(&env, string_new("/"), expr_new_native(native_div));
    env_put(&env, string_new("<"), expr_new_native(native_lessthan));

    env_put(&env, string_new("define"), expr_new_native(native_define));
    env_put(&env, string_new("println"), expr_new_native(native_println));

    env_put(&env, string_new("list"), expr_new_native(native_list));
    env_put(&env, string_new("car"), expr_new_native(native_car));
    env_put(&env, string_new("cdr"), expr_new_native(native_cdr));
    env_put(&env, string_new("len"), expr_new_native(native_len));

    env_put(&env, string_new("lambda"), expr_new_native(native_lambda));

    env_put(&env, string_new("t"), expr_new_atom(atom_new_bool(true)));
    env_put(&env, string_new("f"), expr_new_atom(atom_new_bool(false)));

    repl(&env);

    arena_free(&g_arena);
    return 0;
}
