#ifndef SBASIC_AST_H
#define SBASIC_AST_H

#include "value.h"

/* Expression node types */
typedef enum {
    EXPR_LITERAL,       /* integer, double, or string constant */
    EXPR_VARIABLE,      /* variable reference */
    EXPR_UNARY,         /* unary op: -expr, NOT expr */
    EXPR_BINARY,        /* binary op: a + b, a AND b, etc. */
    EXPR_COMPARE,       /* comparison: a = b, a < b, etc. */
    EXPR_CALL,          /* built-in function call: ABS(x), LEFT$(s,n) */
} expr_type_t;

/* Binary operators */
typedef enum {
    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_IDIV, OP_MOD, OP_POW,
    OP_AND, OP_OR,
} binop_t;

/* Unary operators */
typedef enum {
    OP_NEG, OP_NOT,
} unaryop_t;

/* Comparison operators */
typedef enum {
    CMP_EQ, CMP_NE, CMP_LT, CMP_GT, CMP_LE, CMP_GE,
} cmpop_t;

/* Expression node */
typedef struct expr {
    expr_type_t type;
    int line;
    union {
        /* EXPR_LITERAL */
        value_t literal;

        /* EXPR_VARIABLE */
        struct {
            char name[64];
            val_type_t var_type;
        } var;

        /* EXPR_UNARY */
        struct {
            unaryop_t op;
            struct expr *operand;
        } unary;

        /* EXPR_BINARY */
        struct {
            binop_t op;
            struct expr *left;
            struct expr *right;
        } binary;

        /* EXPR_COMPARE */
        struct {
            cmpop_t op;
            struct expr *left;
            struct expr *right;
        } compare;

        /* EXPR_CALL */
        struct {
            char name[64];
            struct expr *args[8];
            int nargs;
        } call;
    };
} expr_t;

/* Statement node types */
typedef enum {
    STMT_PRINT,
    STMT_INPUT,
    STMT_ASSIGN,
    STMT_IF,
    STMT_FOR,
    STMT_WHILE,
    STMT_END,
    STMT_REM,
} stmt_type_t;

/* Print item: expression + separator */
typedef struct {
    expr_t *expr;       /* NULL for trailing separator */
    char sep;           /* ';' ',' or '\0' (newline) */
} print_item_t;

/* Statement node */
typedef struct stmt {
    stmt_type_t type;
    int line;
    struct stmt *next;  /* linked list of statements */
    union {
        /* STMT_PRINT */
        struct {
            print_item_t *items;
            int nitems;
        } print;

        /* STMT_INPUT */
        struct {
            char *prompt;           /* prompt string (may be NULL) */
            char varnames[8][64];   /* variable names */
            val_type_t vartypes[8]; /* variable types */
            int nvars;
        } input;

        /* STMT_ASSIGN */
        struct {
            char name[64];
            val_type_t var_type;
            expr_t *value;
        } assign;

        /* STMT_IF */
        struct {
            expr_t *condition;
            struct stmt *then_body;
            struct stmt *else_body; /* NULL if no ELSE */
        } if_stmt;

        /* STMT_FOR */
        struct {
            char var_name[64];
            val_type_t var_type;
            expr_t *start;
            expr_t *end;
            expr_t *step;          /* NULL = default 1 */
            struct stmt *body;
        } for_stmt;

        /* STMT_WHILE */
        struct {
            expr_t *condition;
            struct stmt *body;
        } while_stmt;
    };
} stmt_t;

/* Expression constructors */
expr_t *expr_literal(value_t val, int line);
expr_t *expr_variable(const char *name, val_type_t type, int line);
expr_t *expr_unary(unaryop_t op, expr_t *operand, int line);
expr_t *expr_binary(binop_t op, expr_t *left, expr_t *right, int line);
expr_t *expr_compare(cmpop_t op, expr_t *left, expr_t *right, int line);
expr_t *expr_call(const char *name, int line);
void expr_call_add_arg(expr_t *call, expr_t *arg);

/* Expression destructor */
void expr_free(expr_t *e);

/* Statement constructors */
stmt_t *stmt_alloc(stmt_type_t type, int line);
stmt_t *stmt_print(int line);
void stmt_print_add(stmt_t *s, expr_t *expr, char sep);
stmt_t *stmt_input(const char *prompt, int line);
void stmt_input_add_var(stmt_t *s, const char *name, val_type_t type);
stmt_t *stmt_assign(const char *name, val_type_t type, expr_t *value, int line);
stmt_t *stmt_if(expr_t *cond, stmt_t *then_body, stmt_t *else_body, int line);
stmt_t *stmt_for(const char *var, val_type_t type,
                 expr_t *start, expr_t *end, expr_t *step,
                 stmt_t *body, int line);
stmt_t *stmt_while(expr_t *cond, stmt_t *body, int line);
stmt_t *stmt_end(int line);

/* Statement destructor (frees entire chain) */
void stmt_free(stmt_t *s);

/* Append statement to end of chain */
void stmt_append(stmt_t **head, stmt_t *s);

#endif
