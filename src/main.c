// TODO: 
//      Error handling
//      Split project into multiple files
//      Execute files along with REPL
//      Lambdas/user-defined functions
//      Booleans    - lexing and parsing
//      Strings     - lexing and parsing
//      Garbage collector - The arena allocator will not hold forever
//      Write a real program in Ruse
//      
// General TODO: 
//      Improve the Env type and functions
//
// Maybes (some ideas that I'm still on the fence):
//      Rewrite in Zig
//          - Better standard library
//          - No need to hand roll basic data structures
//          - Better memory safety
//
// Standard Library TODO:
//      if expression
//      Rest of arithmetics: + * /
//      Logical operators
//      Use multiple files
//      IO  - Piggy back off of C's IO
//          File handling
//          Printing
//          User input

#include <assert.h>
#include <editline/readline.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pigeon/alias.h>
#include <pigeon/arena.h>
#include <pigeon/allocator.h>
#include <pigeon/string.h>

Arena arena = {0};
Allocator allocator = {0};

// Lexing

char* start = 0;
char* current = 0;
u32 line = 1;

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
        .lexeme = start,
        .len = (u16)(current - start),
        .line = line,
    };
}

Token token_new_error(char* msg) {
    return (Token) {
        .type = TOKEN_ERROR,
        .line = line,
        .lexeme = msg,
        .len = strlen(msg),
    };
}

void lex(char* src) {
    start = src; 
    current = src;
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
    start = current;
    if (*current == '\0' || *current == '\n') return token_new(TOKEN_EOF);

    char c = *current++;

    if (is_space(c)) return next_token();

    switch (c) {
        case '(': return token_new(TOKEN_LPAREN);
        case ')': return token_new(TOKEN_RPAREN);
        case '\n': {
            line++;
            return next_token();
        }
    }

    if (is_digit(c)) {
        char* ptr = current;
        while (is_digit(*ptr) || *ptr == '.') {
            current++;
            ptr = current;
        }

        return token_new(TOKEN_NUMBER);
    }

    if (is_symbol(c)) {
        char* ptr = current;
        while (is_digit(*ptr) || is_symbol(*ptr)) {
            current++;
            ptr = current;
        }

        return token_new(TOKEN_SYMBOL);
    }

    return token_new_error("Unexpected character");
}

// End Lexing
//
// Parsing
typedef struct Env Env;

typedef struct Expr Expr;

typedef struct Atom Atom;
typedef struct Cons Cons;
typedef Expr*(*Native)(Expr*,Env*);

// Tagged unions baby!
struct Atom {
    enum {
        ATOM_NUMBER,
        ATOM_SYMBOL,
    } tag;

    union {
        f64 number;

        struct {
            char* sym;
            u16 len;
        } symbol;
    } as;
};

Atom* atom_new_number(f64 n) {
    Atom* atom = alloc(allocator, sizeof(Atom));
    atom->tag = ATOM_NUMBER;
    atom->as.number = n;
    return atom;
}

Atom* atom_new_symbol(char* sym, u16 len) {
    Atom* atom = alloc(allocator, sizeof(Atom));
    atom->tag = ATOM_SYMBOL;
    atom->as.symbol.sym = sym;
    atom->as.symbol.len = len;
    return atom;
}

void atom_print(Atom* atom) {
    switch (atom->tag) {
        case ATOM_NUMBER: {
            printf(" %f ", atom->as.number);
            break;
        }

        case ATOM_SYMBOL: {
            printf(" %.*s ", atom->as.symbol.len, atom->as.symbol.sym);
            break;
        }
    }
}

struct Cons {
    Expr* car;
    Cons* cdr;
};

Cons* cons_new(Expr* car, Cons* cdr) {
    Cons* cons = alloc(allocator, sizeof(Cons));
    cons->car = car;
    cons->cdr = cdr;
    return cons;
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

struct Expr {
    enum {
        EXPR_ATOM,
        EXPR_CONS,
        EXPR_NATIVE,
    }tag;

    union {
        Atom* atom;
        Cons* cons;
        Native native;
    } as;
};

Expr* expr_new_atom(Atom* atom) {
    Expr* expr = alloc(allocator, sizeof(Expr));
    expr->tag = EXPR_ATOM;
    expr->as.atom = atom;
    return expr;
}

Expr* expr_new_cons(Cons* cons) {
    Expr* expr = alloc(allocator, sizeof(Expr));
    expr->tag = EXPR_CONS;
    expr->as.cons = cons;
    return expr;
}

Expr* expr_new_native(Native native) {
    Expr* expr = alloc(allocator, sizeof(Expr));
    expr->tag = EXPR_NATIVE;
    expr->as.native = native;
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
    }

}


Expr* parse_expression(Token tok);
Expr* parse_list(Token tok);
Expr* parse_atom(Token tok);

Expr* parse(char* src) {
    lex(src);

    return parse_expression(next_token());
}

Expr* parse_expression(Token tok) {
    if (tok.type == TOKEN_EOF) return NULL;

    if (tok.type == TOKEN_LPAREN) return parse_list(tok);

    return parse_atom(tok);
}

Expr* parse_list(Token tok) {
    Cons* cons = cons_new(NULL, NULL);
    Cons* curr = cons;

    while ((tok = next_token()).type != TOKEN_RPAREN) {
        if (tok.type == TOKEN_EOF) break;
        curr->car = parse_expression(tok);
        curr->cdr = cons_new(NULL, NULL);
        curr = curr->cdr;
    } 

    return expr_new_cons(cons);
}

Expr* parse_atom(Token tok) {
    switch (tok.type) {
        case TOKEN_NUMBER: {
            char c = tok.lexeme[tok.len];
            tok.lexeme[tok.len] = '\0';
            f64 n = atof(tok.lexeme);
            tok.lexeme[tok.len] = c;
            return expr_new_atom(atom_new_number(n));
        }

        case TOKEN_SYMBOL: return expr_new_atom(atom_new_symbol(tok.lexeme, tok.len)); 

        default: {
            printf("%d\n", tok.type);
            printf("We should not have gotten here :/\n");
            return NULL;
        }
    }
}

// End parsing
//
// Evaluating
#define ENV_LEN (512)
struct Env {
    Expr** data;
    u32 len;
    Env* parent;
};

Env env_new(Env* parent) {
    return (Env) {
        .len = ENV_LEN,
        .data = alloc(allocator, sizeof(Expr**) * ENV_LEN),
        .parent = parent,
    };
}

Expr* env_get(const Env* env, String value) {
    u64 hash = string_hash(&value) % env->len;
    return env->data[hash];
}

void env_put(Env* env, String value, Expr* expr) {
    u64 hash = string_hash(&value) % env->len;
    env->data[hash] = expr;
}

Expr* eval_atom(Expr* expr, Env* env);
Expr* eval_list(Expr* expr, Env* env);

Expr* eval(Expr* expr, Env* env) {
    switch (expr->tag) {
        case EXPR_ATOM: return eval_atom(expr, env);
        case EXPR_CONS: return eval_list(expr, env);
        default: assert(0 && "unreachable");
    }
}

Expr* eval_atom(Expr* expr, Env* env) {
    Atom* atom = expr->as.atom;
    switch (atom->tag) {
        case ATOM_NUMBER: return expr;
        case ATOM_SYMBOL: {
            return env_get(
                    env, 
                    string_from_parts(
                        atom->as.symbol.sym, 
                        atom->as.symbol.len));
        default: assert(0 && "unreachable");
        }
    }
}

Expr* eval_list(Expr* expr, Env* env) {
    Cons* cons = expr->as.cons;

    assert(cons->car->tag == EXPR_ATOM);
    assert(cons->car->as.atom->tag == ATOM_SYMBOL);

    Expr* func = eval_atom(cons->car, env);
    assert(func->tag == EXPR_NATIVE);

    return func->as.native(expr_new_cons(cons->cdr), env);
}

// End evaluating
//
// Native functions

Expr* native_sub(Expr* expr, Env* env) {
    Cons* cons = expr->as.cons;
    assert(cons->car);
    assert(cons->cdr && cons->cdr->car);
    Expr* first = eval(cons->car, env);
    Expr* second = eval(cons->cdr->car, env);

    assert(first->tag == EXPR_ATOM && first->as.atom->tag == ATOM_NUMBER);
    assert(second->tag == EXPR_ATOM && second->as.atom->tag == ATOM_NUMBER);

    f64 a = first->as.atom->as.number;
    f64 b = second->as.atom->as.number;

    return expr_new_atom(atom_new_number(a - b));
}

Expr* native_define(Expr* expr, Env* env) {
    Cons* cons = expr->as.cons;
    assert(cons->car);
    assert(cons->cdr && cons->cdr->car);
    assert(cons->car->tag == EXPR_ATOM);
    assert(cons->car->as.atom->tag == ATOM_SYMBOL);
    Atom* sym = cons->car->as.atom;
    Expr* new_expr = eval(cons->cdr->car, env);
    env_put(env, string_from_parts(sym->as.symbol.sym, sym->as.symbol.len), new_expr);
    return new_expr;
}

// End native functions

i32 main(void) {
    arena = arena_new();
    allocator = arena_to_allocator(&arena);

    Env env = env_new(NULL);
    env_put(&env, string_new("-"), expr_new_native(native_sub));
    env_put(&env, string_new("define"), expr_new_native(native_define));

    while (1) {
        char* input = readline("ruse> ");

        if (!input) break;

        add_history(input);

        expr_print(eval(parse(input), &env));
        putchar('\n');

        free(input);
    }

    arena_free(&arena);
    return 0;
}
