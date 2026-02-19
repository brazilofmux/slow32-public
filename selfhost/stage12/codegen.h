/* codegen.h -- Tree-walk code generator for stage12 compiler
 *
 * Phase 1: walks AST, emits SLOW-32 assembly directly.
 * No IR layers yet — just get working output.
 * Uses stack-machine style: expressions evaluate into r1,
 * binary ops push lhs, evaluate rhs, pop lhs into r2.
 */

/* --- Output buffer --- */
#define CG_MAX_OUT 2097152

static char cg_out[CG_MAX_OUT];
static int  cg_olen;
static int  cg_lbl;    /* label counter (monotonically increasing) */
static int  cg_epilog; /* current function's epilog label */

static void cg_c(int ch) {
    if (cg_olen < CG_MAX_OUT - 1) {
        cg_out[cg_olen] = ch;
        cg_olen = cg_olen + 1;
    }
}

static void cg_s(char *s) {
    int i;
    i = 0;
    while (s[i] != 0) {
        cg_c(s[i]);
        i = i + 1;
    }
}

static void cg_n(int v) {
    char buf[12];
    int i;
    int neg;
    int d;

    if (v == 0) { cg_c(48); return; }
    neg = 0;
    if (v < 0) { neg = 1; v = 0 - v; }
    i = 0;
    while (v > 0) {
        buf[i] = 48 + (v % 10);
        v = v / 10;
        i = i + 1;
    }
    if (neg) cg_c(45);
    while (i > 0) {
        i = i - 1;
        cg_c(buf[i]);
    }
}

static int cg_label(void) {
    int l;
    l = cg_lbl;
    cg_lbl = cg_lbl + 1;
    return l;
}

static void cg_ldef(int l) {
    cg_s(".L");
    cg_n(l);
    cg_s(":\n");
}

static void cg_lref(int l) {
    cg_s(".L");
    cg_n(l);
}

static void cg_push(void) {
    cg_s("    addi r29, r29, -4\n    stw r29, r1, 0\n");
}

static void cg_pop(void) {
    cg_s("    ldw r2, r29, 0\n    addi r29, r29, 4\n");
}

/* Load immediate into r1 */
static void cg_li(int v) {
    int hi;
    int lo;

    if (v >= -2048 && v <= 2047) {
        cg_s("    addi r1, r0, ");
        cg_n(v);
        cg_c(10);
    } else {
        hi = (v + 2048) >> 12;
        hi = hi & 1048575;
        lo = v & 4095;
        if (lo >= 2048) lo = lo - 4096;
        cg_s("    lui r1, ");
        cg_n(hi);
        cg_c(10);
        cg_s("    addi r1, r1, ");
        cg_n(lo);
        cg_c(10);
    }
}

/* --- Code generation --- */

static void gen_expr(struct Node *n);
static void gen_stmt(struct Node *n);

/* Generate address of lvalue into r1 */
static void gen_addr(struct Node *n) {
    if (n->kind == ND_VAR) {
        cg_s("    addi r1, r30, ");
        cg_n(n->offset);
        cg_c(10);
        return;
    }
    p_error("not an lvalue");
}

/* Generate expression, result in r1 */
static void gen_expr(struct Node *n) {
    int l1;
    int l2;
    struct Node *a;
    int i;

    if (n->kind == ND_NUM) {
        cg_li(n->val);
        return;
    }

    if (n->kind == ND_VAR) {
        cg_s("    ldw r1, r30, ");
        cg_n(n->offset);
        cg_c(10);
        return;
    }

    if (n->kind == ND_ASSIGN) {
        /* evaluate rhs into r1, save to lhs address */
        gen_expr(n->rhs);
        cg_push();
        gen_addr(n->lhs);
        cg_pop();
        /* r2 = value, r1 = address */
        cg_s("    addi r11, r1, 0\n");
        cg_s("    addi r1, r2, 0\n");
        cg_s("    stw r11, r1, 0\n");
        /* result is the assigned value (in r1) */
        return;
    }

    if (n->kind == ND_UNARY) {
        gen_expr(n->lhs);
        if (n->op == TK_MINUS) {
            cg_s("    sub r1, r0, r1\n");
        } else if (n->op == TK_BANG) {
            cg_s("    seq r1, r1, r0\n");
        }
        return;
    }

    if (n->kind == ND_BINOP) {
        /* Short-circuit for && and || */
        if (n->op == TK_LAND) {
            l1 = cg_label();
            l2 = cg_label();
            gen_expr(n->lhs);
            cg_s("    beq r1, r0, ");
            cg_lref(l1);
            cg_c(10);
            gen_expr(n->rhs);
            cg_s("    sne r1, r1, r0\n");
            cg_s("    jal r0, ");
            cg_lref(l2);
            cg_c(10);
            cg_ldef(l1);
            cg_s("    addi r1, r0, 0\n");
            cg_ldef(l2);
            return;
        }
        if (n->op == TK_LOR) {
            l1 = cg_label();
            l2 = cg_label();
            gen_expr(n->lhs);
            cg_s("    bne r1, r0, ");
            cg_lref(l1);
            cg_c(10);
            gen_expr(n->rhs);
            cg_s("    sne r1, r1, r0\n");
            cg_s("    jal r0, ");
            cg_lref(l2);
            cg_c(10);
            cg_ldef(l1);
            cg_s("    addi r1, r0, 1\n");
            cg_ldef(l2);
            return;
        }

        /* Regular binary op: lhs in r2, rhs in r1 */
        gen_expr(n->lhs);
        cg_push();
        gen_expr(n->rhs);
        cg_pop();
        /* r2 = lhs, r1 = rhs */

        if (n->op == TK_PLUS)    { cg_s("    add r1, r2, r1\n"); return; }
        if (n->op == TK_MINUS)   { cg_s("    sub r1, r2, r1\n"); return; }
        if (n->op == TK_STAR)    { cg_s("    mul r1, r2, r1\n"); return; }
        if (n->op == TK_SLASH)   { cg_s("    div r1, r2, r1\n"); return; }
        if (n->op == TK_PERCENT) { cg_s("    rem r1, r2, r1\n"); return; }
        if (n->op == TK_EQ)      { cg_s("    seq r1, r2, r1\n"); return; }
        if (n->op == TK_NE)      { cg_s("    sne r1, r2, r1\n"); return; }
        if (n->op == TK_LT)      { cg_s("    slt r1, r2, r1\n"); return; }
        if (n->op == TK_GT)      { cg_s("    sgt r1, r2, r1\n"); return; }
        if (n->op == TK_LE)      { cg_s("    sle r1, r2, r1\n"); return; }
        if (n->op == TK_GE)      { cg_s("    sge r1, r2, r1\n"); return; }
        if (n->op == TK_AMP)     { cg_s("    and r1, r2, r1\n"); return; }
        if (n->op == TK_PIPE)    { cg_s("    or r1, r2, r1\n"); return; }
        if (n->op == TK_CARET)   { cg_s("    xor r1, r2, r1\n"); return; }
        if (n->op == TK_LSHIFT)  { cg_s("    sll r1, r2, r1\n"); return; }
        if (n->op == TK_RSHIFT)  { cg_s("    sra r1, r2, r1\n"); return; }

        p_error("unknown binop");
        return;
    }

    if (n->kind == ND_CALL) {
        /* Push args right-to-left onto stack, then pop into r3-r10 */
        /* First, evaluate all args left-to-right and push */
        a = n->args;
        while (a) {
            gen_expr(a);
            cg_push();
            a = a->next;
        }
        /* Pop into argument registers: arg0 at highest offset, argN at sp */
        i = 0;
        while (i < n->nparams) {
            cg_s("    ldw r");
            cg_n(3 + i);
            cg_s(", r29, ");
            cg_n((n->nparams - 1 - i) * 4);
            cg_c(10);
            i = i + 1;
        }
        cg_s("    addi r29, r29, ");
        cg_n(n->nparams * 4);
        cg_c(10);
        /* Call */
        cg_s("    jal r31, ");
        cg_s(n->name);
        cg_c(10);
        /* Result is in r1 */
        return;
    }

    p_error("unknown expression node");
}

/* Generate statement */
static void gen_stmt(struct Node *n) {
    int l1;
    int l2;
    struct Node *s;

    if (!n) return;

    if (n->kind == ND_RETURN) {
        if (n->lhs) {
            gen_expr(n->lhs);
        }
        cg_s("    jal r0, ");
        cg_lref(cg_epilog);
        cg_c(10);
        return;
    }

    if (n->kind == ND_IF) {
        l1 = cg_label();
        gen_expr(n->cond);
        if (n->els) {
            l2 = cg_label();
            cg_s("    beq r1, r0, ");
            cg_lref(l1);
            cg_c(10);
            gen_stmt(n->body);
            cg_s("    jal r0, ");
            cg_lref(l2);
            cg_c(10);
            cg_ldef(l1);
            gen_stmt(n->els);
            cg_ldef(l2);
        } else {
            cg_s("    beq r1, r0, ");
            cg_lref(l1);
            cg_c(10);
            gen_stmt(n->body);
            cg_ldef(l1);
        }
        return;
    }

    if (n->kind == ND_WHILE) {
        l1 = cg_label();
        l2 = cg_label();
        cg_ldef(l1);
        gen_expr(n->cond);
        cg_s("    beq r1, r0, ");
        cg_lref(l2);
        cg_c(10);
        gen_stmt(n->body);
        cg_s("    jal r0, ");
        cg_lref(l1);
        cg_c(10);
        cg_ldef(l2);
        return;
    }

    if (n->kind == ND_BLOCK) {
        s = n->body;
        while (s) {
            gen_stmt(s);
            s = s->next;
        }
        return;
    }

    if (n->kind == ND_EXPR_STMT) {
        if (n->lhs) gen_expr(n->lhs);
        return;
    }

    p_error("unknown statement node");
}

/* Generate function */
static void gen_func(struct Node *fn) {
    int fs;
    int i;

    fs = fn->locals_size;
    /* Align to 4 */
    fs = ((fs + 3) / 4) * 4;

    /* Allocate a unique epilog label for this function */
    cg_epilog = cg_label();

    /* Function label */
    cg_s(".global ");
    cg_s(fn->name);
    cg_c(10);
    cg_s(fn->name);
    cg_s(":\n");

    /* Prolog */
    cg_s("    addi r29, r29, -");
    cg_n(fs);
    cg_c(10);
    cg_s("    stw r29, r31, ");
    cg_n(fs - 4);
    cg_c(10);
    cg_s("    stw r29, r30, ");
    cg_n(fs - 8);
    cg_c(10);
    cg_s("    addi r30, r29, ");
    cg_n(fs);
    cg_c(10);

    /* Copy params from arg registers to stack slots */
    i = 0;
    while (i < fn->nparams) {
        cg_s("    stw r30, r");
        cg_n(3 + i);
        cg_s(", ");
        cg_n(-8 - (i + 1) * 4);
        cg_c(10);
        i = i + 1;
    }

    /* Body */
    gen_stmt(fn->body);

    /* Epilog */
    cg_ldef(cg_epilog);
    cg_s("    ldw r31, r29, ");
    cg_n(fs - 4);
    cg_c(10);
    cg_s("    ldw r30, r29, ");
    cg_n(fs - 8);
    cg_c(10);
    cg_s("    addi r29, r29, ");
    cg_n(fs);
    cg_c(10);
    cg_s("    jalr r0, r31, 0\n\n");
}

/* Generate entire program */
static void gen_program(struct Node *prog) {
    struct Node *fn;

    cg_s(".text\n\n");
    fn = prog->body;
    while (fn) {
        gen_func(fn);
        fn = fn->next;
    }
}
