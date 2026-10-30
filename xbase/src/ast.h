#ifndef AST_H
#define AST_H

#include "expr.h"
#include "memvar.h"

/* ---- AST node types ---- */

typedef enum {
    AST_LITERAL,        /* value_t embedded */
    AST_FIELD,          /* field name — resolved at eval time */
    AST_ALIAS_FIELD,    /* alias->field */
    AST_MEMVAR,         /* bare name (field-first lookup at eval time) */
    AST_ARRAY_ACCESS,   /* arr[i] or arr[i,j] */
    AST_BINOP,          /* left OP right */
    AST_UNOP,           /* OP child (negate, .NOT.) */
    AST_FUNC_CALL,      /* name(args...) */
    AST_ASSIGN,         /* target := value, target op= value */
    AST_INCDEC,         /* ++target, target++, --target, target-- */
    AST_ARRAY_LIT,      /* { a, b, ... } */
    AST_INDEX,          /* expr[i] -- on any value, so a[3][2] */
    AST_BLOCK,          /* {|p, q| e1, e2, ...} */
    AST_REF,            /* @name: an argument passed by reference */
    AST_SEND,           /* obj:name, obj:name(args): the send operator */
    AST_MACRO           /* &(expr): the string compiled and evaluated at run time */
} ast_type_t;

typedef enum {
    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_POWER,
    OP_EQ, OP_EXACT_EQ, OP_NE, OP_LT, OP_GT, OP_LE, OP_GE, OP_SUBSTR,
    OP_AND, OP_OR,
    OP_NOT, OP_NEGATE,
    OP_MOD,
    OP_NONE             /* AST_ASSIGN: plain := */
} ast_op_t;

typedef struct ast_node {
    ast_type_t type;
    union {
        value_t literal;                                        /* AST_LITERAL */
        char name[64];                                          /* AST_FIELD, AST_MEMVAR */
        struct { char alias[64]; char field[64]; } alias_field; /* AST_ALIAS_FIELD */
        struct { char name[64]; struct ast_node *index1;
                 struct ast_node *index2; } array;              /* AST_ARRAY_ACCESS */
        struct { ast_op_t op; struct ast_node *left;
                 struct ast_node *right; } binop;               /* AST_BINOP */
        struct { ast_op_t op; struct ast_node *child; } unop;   /* AST_UNOP */
        struct { char name[64]; struct ast_node **args;
                 int nargs; } call;                             /* AST_FUNC_CALL */
        struct { ast_op_t op; struct ast_node *target;
                 struct ast_node *value; } assign;              /* AST_ASSIGN */
        struct { int delta; int prefix;
                 struct ast_node *target; } incdec;             /* AST_INCDEC */
        struct { struct ast_node **elems; int n; } list;        /* AST_ARRAY_LIT */
        struct { struct ast_node *base;
                 struct ast_node *index; } index;               /* AST_INDEX */
        struct { char (*params)[MEMVAR_NAMELEN]; int nparams;
                 struct ast_node **exprs; int n; } block;       /* AST_BLOCK */
        struct { struct ast_node *base; char name[64]; struct ast_node **args;
                 int nargs; int is_call; } send;                 /* AST_SEND */
        struct { struct ast_node *expr; } macro;                /* AST_MACRO */
    };
} ast_node_t;

/* ---- API ---- */

/* Parse expression string into AST.  Returns NULL on error (sets *error).
   Does NOT evaluate — pure parsing, no side effects. */
ast_node_t *ast_compile(const char *expr, memvar_store_t *store, const char **error);

/* Same, but advances *pp past consumed input. */
ast_node_t *ast_compile_adv(const char **pp, memvar_store_t *store, const char **error);

/* Evaluate AST.  Returns 0 on success, -1 on error. */
int ast_eval(ast_node_t *node, expr_ctx_t *ctx, value_t *result);

/* Compile, evaluate, and free a fresh AST in one shot.
   Used for expressions containing &macros that must re-expand each time. */
int ast_eval_dynamic(const char *expr, expr_ctx_t *ctx, value_t *result);
int ast_eval_macro_value(expr_ctx_t *ctx, const value_t *text, value_t *result);   /* &(expr), expr evaluated */

/* Evaluate the expression at *pp through the compiled-expression cache and
   advance *pp past it (the advancing form of ast_eval_dynamic). */
int ast_eval_adv(expr_ctx_t *ctx, const char **pp, value_t *result);

/* The operation of each node kind, which ast_eval applies as it walks the
 * tree; compiled code (xbc/) calls them directly.  0, or -1 with
 * ctx->error set. */
int ast_eval_field(expr_ctx_t *ctx, const char *name, value_t *result);
int ast_eval_name(expr_ctx_t *ctx, const char *name, value_t *result);
int ast_eval_alias_field(expr_ctx_t *ctx, const char *alias, const char *field, value_t *result);
int ast_eval_elem(expr_ctx_t *ctx, const char *name, const value_t *row,
                  const value_t *col, value_t *result);   /* col NULL: one index */
int ast_eval_unop(expr_ctx_t *ctx, ast_op_t op, const value_t *child, value_t *result);
int val_find(const value_t *hay, const value_t *needle, int from);   /* a byte search over values */
int ast_eval_binop(expr_ctx_t *ctx, ast_op_t op, const value_t *left, const value_t *right,
                   value_t *result);

/* Stores, for := and its kin.  An undeclared name is a memory variable
 * (CA-Clipper: an ambiguous reference is assumed MEMVAR); M-> and MEMVAR->
 * name one, FIELD-> a field of the current work area, another alias a
 * field there. */
int ast_store_name(expr_ctx_t *ctx, const char *name, const value_t *val);
int ast_store_elem(expr_ctx_t *ctx, const char *name, const value_t *row,
                   const value_t *col, const value_t *val);
int ast_store_alias_field(expr_ctx_t *ctx, const char *alias, const char *field,
                          const value_t *val);

/* Arrays: a new array of n values; base[index] read and stored */
int ast_make_array(expr_ctx_t *ctx, const value_t *vals, int n, value_t *result);
int ast_eval_index(expr_ctx_t *ctx, const value_t *base, const value_t *index, value_t *result);
int ast_store_index(expr_ctx_t *ctx, const value_t *base, const value_t *index, const value_t *val);
/* The send operator: an instance variable read (is_call 0) or a method
 * called with args, and an instance variable stored */
int ast_eval_send(expr_ctx_t *ctx, const value_t *base, const char *name, value_t *args, int nargs,
                  int is_call, value_t *result);
int ast_store_send(expr_ctx_t *ctx, const value_t *base, const char *name, const value_t *val);

/* A code block value: its parameters, the call depth it was made at (EVAL
 * runs it there, so it sees its maker's LOCALs, not its caller's), and
 * its body -- a copy of the tree (the interpreter) or a C function
 * (compiled code).  Parameters are bound as PRIVATEs are: saved, set,
 * restored.  Blocks are not freed (as arrays). */
#define MAX_BLOCK_PARAMS 16
#define MAX_BLOCK_CAPTURES 16
typedef struct block_s {
    int nparams;
    char params[MAX_BLOCK_PARAMS][MEMVAR_NAMELEN];
    int depth;
    /* The variables its body names, as they were when it was made: used
     * only when one is gone at EVAL (its function returned -- Clipper's
     * detached local), and kept up to date with what the block does */
    int ncap;
    char cap_names[MAX_BLOCK_CAPTURES][MEMVAR_NAMELEN];
    value_t cap_vals[MAX_BLOCK_CAPTURES];
    struct ast_node *body;                          /* AST_BLOCK, or NULL */
    int (*fn)(expr_ctx_t *ctx, value_t *result);    /* compiled, or NULL */
} block_t;

int ast_make_block(expr_ctx_t *ctx, int (*fn)(expr_ctx_t *, value_t *), int nparams,
                   const char *const *params, int nfree, const char *const *free_names,
                   value_t *result);
int ast_block_free_names(const ast_node_t *blk, char (*out)[MEMVAR_NAMELEN]);
/* EVAL(block, args...) */
int ast_eval_block(expr_ctx_t *ctx, const value_t *block, value_t *args, int nargs, value_t *result);
int ast_block_pcount(void);   /* the block being evaluated: its argument count; -1 outside one */

/* Free AST recursively.  Safe to call with NULL. */
void ast_free(ast_node_t *node);

#endif
