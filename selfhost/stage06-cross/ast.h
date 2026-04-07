/* ast.h -- AST node types for stage04 compiler
 *
 * Phase 2: types, pointers, strings, globals, arrays.
 * No unions: flat struct, access only relevant fields per kind.
 * Compiled by stage03 s32-cc (no block-scoped vars, no unions).
 */

/* --- Additional libc prototypes --- */
/* void* works correctly — ty_size(TY_VOID) returns 1 (GCC extension) */
char *strdup(char *s);
char *malloc(int size);
char *calloc(int n, int size);
void free(char *p);
char *memcpy(char *dst, char *src, int n);

/* --- Type encoding --- */
#define TY_INT    0
#define TY_CHAR   1
#define TY_SHORT  2
#define TY_VOID   3
#define TY_LLONG  4       /* long long (64-bit) */
#define TY_FLOAT  5       /* float (32-bit IEEE 754) */
#define TY_DOUBLE 6       /* double (64-bit IEEE 754) */
#define TY_STRUCT_BASE 7  /* struct index i → type = 7+i; fits 7..255 */
#define TY_PTR  256   /* add to base: TY_PTR+TY_CHAR = char*, TY_PTR+TY_INT = int* */

#define TY_UNSIGNED  0x4000  /* flag bit: unsigned qualifier */
#define TY_BASE_MASK 0x00FF  /* bits 0-7: base type */
#define TY_PTR_MASK  0x3F00  /* bits 8-13: pointer depth */

/* --- Struct definition tables --- */
#define ST_MAX_STRUCTS 64
#define ST_MAX_MEMBERS 512

static char *st_name[ST_MAX_STRUCTS];
static int   st_nfields[ST_MAX_STRUCTS];
static int   st_first[ST_MAX_STRUCTS];
static int   st_size[ST_MAX_STRUCTS];
static int   st_is_union[ST_MAX_STRUCTS]; /* 1 if union, 0 if struct */
static int   st_count;

static char *stm_name[ST_MAX_MEMBERS];
static int   stm_type[ST_MAX_MEMBERS];
static int   stm_off[ST_MAX_MEMBERS];
static int   stm_count;

/* --- Type helpers --- */

static int ty_is_llong(int ty) {
    return (ty & TY_BASE_MASK) == TY_LLONG && (ty & TY_PTR_MASK) == 0;
}

static int ty_is_float(int ty) {
    return (ty & TY_BASE_MASK) == TY_FLOAT && (ty & TY_PTR_MASK) == 0;
}

static int ty_is_double(int ty) {
    return (ty & TY_BASE_MASK) == TY_DOUBLE && (ty & TY_PTR_MASK) == 0;
}

static int ty_is_fp(int ty) {
    int base;
    base = ty & TY_BASE_MASK;
    return (base == TY_FLOAT || base == TY_DOUBLE) && (ty & TY_PTR_MASK) == 0;
}

static int ty_is_struct(int ty) {
    int base;
    base = ty & TY_BASE_MASK;
    return base >= TY_STRUCT_BASE && (ty & TY_PTR_MASK) == 0;
}

static int ty_struct_idx(int ty) {
    return (ty & TY_BASE_MASK) - TY_STRUCT_BASE;
}

static int ty_size(int ty) {
    if (ty & TY_PTR_MASK) return 4;
    if ((ty & TY_BASE_MASK) >= TY_STRUCT_BASE)
        return st_size[(ty & TY_BASE_MASK) - TY_STRUCT_BASE];
    if ((ty & TY_BASE_MASK) == TY_DOUBLE) return 8;
    if ((ty & TY_BASE_MASK) == TY_LLONG) return 8;
    if ((ty & TY_BASE_MASK) == TY_FLOAT) return 4;
    if ((ty & TY_BASE_MASK) == TY_CHAR) return 1;
    if ((ty & TY_BASE_MASK) == TY_SHORT) return 2;
    if ((ty & TY_BASE_MASK) == TY_VOID) return 1;
    return 4;
}

static int ty_is_ptr(int ty) {
    return (ty & TY_PTR_MASK) != 0;
}

static int ty_deref(int ty) {
    if (ty & TY_PTR_MASK) return ty - TY_PTR;
    return TY_INT;
}

static int ty_is_unsigned(int ty) {
    return (ty & TY_UNSIGNED) != 0;
}

/* --- Node kinds --- */
#define ND_NUM       1   /* integer literal */
#define ND_VAR       2   /* variable reference */
#define ND_BINOP     3   /* binary operation */
#define ND_UNARY     4   /* unary operation (-, !, *, &) */
#define ND_ASSIGN    5   /* assignment */
#define ND_CALL      6   /* function call */
#define ND_RETURN    7   /* return statement */
#define ND_IF        8   /* if / if-else */
#define ND_WHILE     9   /* while loop */
#define ND_BLOCK    10   /* compound statement { ... } */
#define ND_FUNC     11   /* function definition */
#define ND_PROGRAM  12   /* top-level program */
#define ND_EXPR_STMT 13  /* expression statement (expr;) */
#define ND_STRING   14   /* string literal */
#define ND_DO_WHILE 15   /* do-while loop */
#define ND_BREAK    16   /* break statement */
#define ND_CONTINUE 17   /* continue statement */
#define ND_FOR      18   /* for loop (first-class) */
#define ND_COMP_ASSIGN 19 /* compound assignment (+=, -=, etc.) */
#define ND_POST_INC 20   /* postfix ++ */
#define ND_POST_DEC 21   /* postfix -- */
#define ND_TERNARY  22   /* ternary ?: */
#define ND_CAST     23   /* type cast */
#define ND_COMMA    24   /* comma operator */
#define ND_MEMBER   25   /* struct member access: lhs=struct expr, val=offset, ty=member type */
#define ND_SWITCH   26   /* switch: cond=expr, body=block */
#define ND_CASE     27   /* case label: val=constant value */
#define ND_DEFAULT  28   /* default label (no data) */
#define ND_GOTO     29   /* goto label */
#define ND_LABEL    30   /* label: stmt */
#define ND_FUNC_REF 31   /* bare function name as address */
#define ND_CALL_PTR 32   /* indirect call through expression: lhs=callee, args=arglist */
#define ND_VA_START 33   /* va_start: lhs = ap variable */
#define ND_VA_ARG   34   /* va_arg:   lhs = ap variable, ty = requested type */
#define ND_FNUM     35   /* float/double literal: val=lo bits, val_hi=hi bits (f64) */

/* --- AST node --- */
struct Node {
    int kind;
    int ty;           /* expression type: TY_INT, TY_PTR+TY_CHAR, etc */
    int op;           /* BINOP/UNARY: operator token (TK_PLUS etc) */
    int val;          /* NUM: integer value; STRING: string pool index; FNUM: lo bits */
    int val_hi;       /* FNUM: hi 32 bits for f64 literal */
    char *name;       /* VAR/FUNC/CALL: name */
    int offset;       /* VAR: stack offset from fp (locals) */
    int is_local;     /* VAR: 1=local, 0=global */
    int is_array;     /* VAR: 1=array (address, no load) */
    int nparams;      /* FUNC: number of parameters */
    int is_varargs;   /* FUNC: 1 if variadic (...) */
    int locals_size;  /* FUNC: total local stack bytes */
    struct Node *lhs; /* BINOP/ASSIGN/UNARY: left/operand */
    struct Node *rhs; /* BINOP/ASSIGN: right */
    struct Node *cond;/* IF/WHILE: condition */
    struct Node *body;/* BLOCK/FUNC/WHILE/IF(then)/FOR: body/statements */
    struct Node *init;/* FOR: init expression */
    struct Node *step;/* FOR: step expression */
    struct Node *els; /* IF: else branch */
    struct Node *args;/* CALL: argument list; FUNC: parameter list */
    struct Node *next;/* linked list sibling */
};

typedef struct Node Node;

/* --- Constructors --- */

static Node *nd_new(int kind) {
    Node *n;
    n = calloc(1, sizeof(struct Node));
    if (!n) {
        fdputs("s12cc: out of memory\n", 2);
        exit(1);
    }
    n->kind = kind;
    return n;
}

static Node *nd_num(int v) {
    Node *n;
    n = nd_new(ND_NUM);
    n->val = v;
    n->ty = TY_INT;
    return n;
}

static Node *nd_fnum(int lo, int hi, int ty) {
    Node *n;
    n = nd_new(ND_FNUM);
    n->val = lo;
    n->val_hi = hi;
    n->ty = ty;
    return n;
}

static Node *nd_string(int pool_id) {
    Node *n;
    n = nd_new(ND_STRING);
    n->val = pool_id;
    n->ty = TY_PTR + TY_CHAR;
    return n;
}

static Node *nd_var(char *nm, int off, int ty) {
    Node *n;
    n = nd_new(ND_VAR);
    n->name = strdup(nm);
    n->offset = off;
    n->ty = ty;
    return n;
}

static Node *nd_binop(int op, Node *l, Node *r) {
    Node *n;
    n = nd_new(ND_BINOP);
    n->op = op;
    n->lhs = l;
    n->rhs = r;
    /* type: comparisons→int, pointer arithmetic→pointer, else int */
    if (op == TK_EQ || op == TK_NE || op == TK_LT || op == TK_GT ||
        op == TK_LE || op == TK_GE) {
        n->ty = TY_INT;
        /* Mark unsigned if either operand is unsigned (for codegen) */
        if ((l->ty & TY_UNSIGNED) || (r->ty & TY_UNSIGNED))
            n->ty = TY_INT | TY_UNSIGNED;
    } else if (op == TK_LAND || op == TK_LOR) {
        n->ty = TY_INT;
    } else if (ty_is_ptr(l->ty)) {
        n->ty = l->ty;
    } else if (ty_is_ptr(r->ty)) {
        n->ty = r->ty;
    } else if (ty_is_llong(l->ty) || ty_is_llong(r->ty)) {
        n->ty = TY_LLONG;
        if ((l->ty & TY_UNSIGNED) || (r->ty & TY_UNSIGNED))
            n->ty = TY_LLONG | TY_UNSIGNED;
    } else {
        n->ty = TY_INT;
        /* Propagate unsigned for arithmetic/bitwise ops */
        if ((l->ty & TY_UNSIGNED) || (r->ty & TY_UNSIGNED))
            n->ty = TY_INT | TY_UNSIGNED;
    }
    return n;
}

static Node *nd_unary(int op, Node *operand) {
    Node *n;
    n = nd_new(ND_UNARY);
    n->op = op;
    n->lhs = operand;
    if (op == TK_STAR) {
        n->ty = ty_deref(operand->ty);
    } else if (op == TK_AMP) {
        n->ty = operand->ty + TY_PTR;
    } else if (op == TK_MINUS || op == TK_TILDE) {
        n->ty = operand->ty;
    } else {
        n->ty = TY_INT;
    }
    return n;
}

static Node *nd_assign(Node *l, Node *r) {
    Node *n;
    n = nd_new(ND_ASSIGN);
    n->lhs = l;
    n->rhs = r;
    n->ty = l->ty;
    return n;
}

static Node *nd_call(char *nm, Node *a, int na) {
    Node *n;
    n = nd_new(ND_CALL);
    n->name = strdup(nm);
    n->args = a;
    n->nparams = na;
    n->ty = TY_INT;  /* updated by parser via find_func_type() */
    return n;
}

static Node *nd_return(Node *expr) {
    Node *n;
    n = nd_new(ND_RETURN);
    n->lhs = expr;
    return n;
}

static Node *nd_if(Node *c, Node *then_b, Node *else_b) {
    Node *n;
    n = nd_new(ND_IF);
    n->cond = c;
    n->body = then_b;
    n->els = else_b;
    return n;
}

static Node *nd_while(Node *c, Node *b) {
    Node *n;
    n = nd_new(ND_WHILE);
    n->cond = c;
    n->body = b;
    return n;
}

static Node *nd_block(Node *stmts) {
    Node *n;
    n = nd_new(ND_BLOCK);
    n->body = stmts;
    return n;
}

static Node *nd_expr_stmt(Node *expr) {
    Node *n;
    n = nd_new(ND_EXPR_STMT);
    n->lhs = expr;
    return n;
}

static Node *nd_do_while(Node *c, Node *b) {
    Node *n;
    n = nd_new(ND_DO_WHILE);
    n->cond = c;
    n->body = b;
    return n;
}

static Node *nd_for(Node *init_e, Node *cond_e,
                    Node *step_e, Node *body_s) {
    Node *n;
    n = nd_new(ND_FOR);
    n->init = init_e;
    n->cond = cond_e;
    n->step = step_e;
    n->body = body_s;
    return n;
}

static Node *nd_comp_assign(int op, Node *l, Node *r) {
    Node *n;
    n = nd_new(ND_COMP_ASSIGN);
    n->op = op;
    n->lhs = l;
    n->rhs = r;
    n->ty = l->ty;
    return n;
}

static Node *nd_ternary(Node *c, Node *then_e,
                        Node *else_e) {
    Node *n;
    n = nd_new(ND_TERNARY);
    n->cond = c;
    n->lhs = then_e;
    n->rhs = else_e;
    n->ty = then_e->ty;
    return n;
}

static Node *nd_cast(Node *expr, int ty) {
    Node *n;
    n = nd_new(ND_CAST);
    n->lhs = expr;
    n->ty = ty;
    return n;
}

static Node *nd_comma(Node *l, Node *r) {
    Node *n;
    n = nd_new(ND_COMMA);
    n->lhs = l;
    n->rhs = r;
    n->ty = r->ty;
    return n;
}

static Node *nd_member(Node *lhs, int offset, int mty) {
    Node *n;
    n = nd_new(ND_MEMBER);
    n->lhs = lhs;
    n->val = offset;
    n->ty = mty;
    return n;
}

static Node *nd_goto(int label_id) {
    Node *n;
    n = nd_new(ND_GOTO);
    n->val = label_id;
    return n;
}

static Node *nd_label(int label_id, Node *stmt) {
    Node *n;
    n = nd_new(ND_LABEL);
    n->val = label_id;
    n->body = stmt;
    return n;
}

static Node *nd_func_ref(char *nm) {
    Node *n;
    n = nd_new(ND_FUNC_REF);
    n->name = strdup(nm);
    n->ty = TY_INT;
    return n;
}

static Node *nd_call_ptr(Node *callee, Node *a, int na) {
    Node *n;
    n = nd_new(ND_CALL_PTR);
    n->lhs = callee;
    n->args = a;
    n->nparams = na;
    n->ty = TY_INT;
    return n;
}
