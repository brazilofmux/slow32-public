/* ast.h -- AST node types for stage12 compiler
 *
 * Phase 1 narrow spike: int variables, arithmetic, if/while, functions.
 * No unions: flat struct, access only relevant fields per kind.
 * Compiled by stage11 s32-cc (no block-scoped vars, no unions).
 */

/* --- Additional libc prototypes --- */
/* Use char* not void* — s32-cc confuses void* param with void) */
char *strdup(char *s);
char *malloc(int size);
char *calloc(int n, int size);
void free(char *p);

/* --- Node kinds --- */
#define ND_NUM       1   /* integer literal */
#define ND_VAR       2   /* variable reference */
#define ND_BINOP     3   /* binary operation */
#define ND_UNARY     4   /* unary operation */
#define ND_ASSIGN    5   /* assignment */
#define ND_CALL      6   /* function call */
#define ND_RETURN    7   /* return statement */
#define ND_IF        8   /* if / if-else */
#define ND_WHILE     9   /* while loop */
#define ND_BLOCK    10   /* compound statement { ... } */
#define ND_FUNC     11   /* function definition */
#define ND_PROGRAM  12   /* top-level program */
#define ND_EXPR_STMT 13  /* expression statement (expr;) */

/* --- AST node --- */
struct Node {
    int kind;
    int op;           /* BINOP/UNARY: operator token (TK_PLUS etc) */
    int val;          /* NUM: integer value */
    char *name;       /* VAR/FUNC/CALL: name */
    int offset;       /* VAR: stack offset from fp */
    int is_local;     /* VAR: 1=local, 0=parameter */
    int nparams;      /* FUNC: number of parameters */
    int locals_size;  /* FUNC: total local stack bytes */
    struct Node *lhs; /* BINOP/ASSIGN/UNARY: left/operand */
    struct Node *rhs; /* BINOP/ASSIGN: right */
    struct Node *cond;/* IF/WHILE: condition */
    struct Node *body;/* BLOCK/FUNC/WHILE/IF(then): body/statements */
    struct Node *els; /* IF: else branch */
    struct Node *args;/* CALL: argument list; FUNC: parameter list */
    struct Node *next;/* linked list sibling */
};

/* --- Constructors --- */

static struct Node *nd_new(int kind) {
    struct Node *n;
    n = calloc(1, sizeof(struct Node));
    if (!n) {
        fputs("s12cc: out of memory\n", stderr);
        exit(1);
    }
    n->kind = kind;
    return n;
}

static struct Node *nd_num(int v) {
    struct Node *n;
    n = nd_new(ND_NUM);
    n->val = v;
    return n;
}

static struct Node *nd_var(char *nm, int off) {
    struct Node *n;
    n = nd_new(ND_VAR);
    n->name = strdup(nm);
    n->offset = off;
    return n;
}

static struct Node *nd_binop(int op, struct Node *l, struct Node *r) {
    struct Node *n;
    n = nd_new(ND_BINOP);
    n->op = op;
    n->lhs = l;
    n->rhs = r;
    return n;
}

static struct Node *nd_unary(int op, struct Node *operand) {
    struct Node *n;
    n = nd_new(ND_UNARY);
    n->op = op;
    n->lhs = operand;
    return n;
}

static struct Node *nd_assign(struct Node *l, struct Node *r) {
    struct Node *n;
    n = nd_new(ND_ASSIGN);
    n->lhs = l;
    n->rhs = r;
    return n;
}

static struct Node *nd_call(char *nm, struct Node *a, int na) {
    struct Node *n;
    n = nd_new(ND_CALL);
    n->name = strdup(nm);
    n->args = a;
    n->nparams = na;
    return n;
}

static struct Node *nd_return(struct Node *expr) {
    struct Node *n;
    n = nd_new(ND_RETURN);
    n->lhs = expr;
    return n;
}

static struct Node *nd_if(struct Node *c, struct Node *then_b, struct Node *else_b) {
    struct Node *n;
    n = nd_new(ND_IF);
    n->cond = c;
    n->body = then_b;
    n->els = else_b;
    return n;
}

static struct Node *nd_while(struct Node *c, struct Node *b) {
    struct Node *n;
    n = nd_new(ND_WHILE);
    n->cond = c;
    n->body = b;
    return n;
}

static struct Node *nd_block(struct Node *stmts) {
    struct Node *n;
    n = nd_new(ND_BLOCK);
    n->body = stmts;
    return n;
}

static struct Node *nd_expr_stmt(struct Node *expr) {
    struct Node *n;
    n = nd_new(ND_EXPR_STMT);
    n->lhs = expr;
    return n;
}
