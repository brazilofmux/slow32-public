/* codegen.h -- Tree-walk code generator for stage04 compiler
 *
 * Phase 2: type-aware loads/stores, pointers, strings, globals.
 * Walks AST, emits SLOW-32 assembly directly.
 * Uses stack-machine style: expressions evaluate into r1,
 * binary ops push lhs, evaluate rhs, pop lhs into r2.
 */

/* --- Output buffer --- */
#define CG_MAX_OUT 2097152

static char cg_out[CG_MAX_OUT];
static int  cg_olen;
/* cg_lbl and cg_label() are defined in parser.h (shared with parser) */
static int  cg_epilog; /* current function's epilog label */

#define CG_MAX_LOOP 16
static int cg_break_lbl[CG_MAX_LOOP];
static int cg_cont_lbl[CG_MAX_LOOP];
static int cg_loop_depth;

/* Switch state (nested switch support) */
#define CG_MAX_CASE     256
#define CG_MAX_SW_DEPTH 8
static int cg_sw_val[CG_MAX_CASE];   /* case values */
static int cg_sw_lbl[CG_MAX_CASE];   /* case labels */
static int cg_sw_base[CG_MAX_SW_DEPTH];  /* base index into val/lbl arrays */
static int cg_sw_count[CG_MAX_SW_DEPTH]; /* number of cases at this depth */
static int cg_sw_def[CG_MAX_SW_DEPTH];   /* default label (-1 if none) */
static int cg_sw_cur[CG_MAX_SW_DEPTH];   /* cursor for body generation */
static int cg_sw_depth;

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

/* --- Register stack for expression temporaries --- */
#define CG_RSP_MAX 8
static int cg_rsp;  /* register stack depth, uses r11..r18 */

static int expr_has_call(Node *n) {
    Node *a;
    if (!n) return 0;
    if (n->kind == ND_CALL || n->kind == ND_CALL_PTR) return 1;
    if (expr_has_call(n->lhs)) return 1;
    if (expr_has_call(n->rhs)) return 1;
    if (expr_has_call(n->cond)) return 1;
    a = n->args;
    while (a) { if (expr_has_call(a)) return 1; a = a->next; }
    return 0;
}

static void cg_rpush(void) {
    cg_s("    addi r");
    cg_n(11 + cg_rsp);
    cg_s(", r1, 0\n");
    cg_rsp = cg_rsp + 1;
}

static void cg_rpop(void) {
    cg_rsp = cg_rsp - 1;
    cg_s("    addi r2, r");
    cg_n(11 + cg_rsp);
    cg_s(", 0\n");
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

/* Compute r1 = r30 + off (handles large frame offsets via lui+addi+add) */
static void cg_frame_off(int off) {
    int hi;
    int lo;

    if (off >= -2048 && off <= 2047) {
        cg_s("    addi r1, r30, ");
        cg_n(off);
        cg_c(10);
    } else {
        hi = (off + 2048) >> 12;
        hi = hi & 1048575;
        lo = off & 4095;
        if (lo >= 2048) lo = lo - 4096;
        cg_s("    lui r1, ");
        cg_n(hi);
        cg_c(10);
        cg_s("    addi r1, r1, ");
        cg_n(lo);
        cg_c(10);
        cg_s("    add r1, r30, r1\n");
    }
}

/* Compute r1 = r1 + imm (uses r2 as scratch for large imm) */
static void cg_r1_add_imm(int imm) {
    int hi;
    int lo;

    if (imm >= -2048 && imm <= 2047) {
        cg_s("    addi r1, r1, ");
        cg_n(imm);
        cg_c(10);
    } else {
        hi = (imm + 2048) >> 12;
        hi = hi & 1048575;
        lo = imm & 4095;
        if (lo >= 2048) lo = lo - 4096;
        cg_s("    lui r2, ");
        cg_n(hi);
        cg_c(10);
        cg_s("    addi r2, r2, ");
        cg_n(lo);
        cg_c(10);
        cg_s("    add r1, r1, r2\n");
    }
}

/* Load address of symbol into r1 */
static void cg_la(char *sym) {
    cg_s("    lui r1, %hi(");
    cg_s(sym);
    cg_s(")\n    addi r1, r1, %lo(");
    cg_s(sym);
    cg_s(")\n");
}

/* Long-branch: beq r1, r0 → label (uses bne skip + jal target) */
static void cg_beq_long(int lbl) {
    int skip;
    skip = cg_label();
    cg_s("    bne r1, r0, ");
    cg_lref(skip);
    cg_c(10);
    cg_s("    jal r0, ");
    cg_lref(lbl);
    cg_c(10);
    cg_ldef(skip);
}

/* Long-branch: bne r1, r0 → label (uses beq skip + jal target) */
static void cg_bne_long(int lbl) {
    int skip;
    skip = cg_label();
    cg_s("    beq r1, r0, ");
    cg_lref(skip);
    cg_c(10);
    cg_s("    jal r0, ");
    cg_lref(lbl);
    cg_c(10);
    cg_ldef(skip);
}

/* Load from [r1] with appropriate width for type */
static void cg_load(int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED) cg_s("    ldbu r1, r1, 0\n");
        else                  cg_s("    ldb r1, r1, 0\n");
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        if (ty & TY_UNSIGNED) cg_s("    ldhu r1, r1, 0\n");
        else                  cg_s("    ldh r1, r1, 0\n");
    } else {
        cg_s("    ldw r1, r1, 0\n");
    }
}

/* Store r2 to [r1] with appropriate width for type */
static void cg_store(int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        cg_s("    stb r1, r2, 0\n");
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        cg_s("    sth r1, r2, 0\n");
    } else {
        cg_s("    stw r1, r2, 0\n");
    }
}

/* --- Code generation --- */

/* Apply binary operation: r1 = r2 op r1 (ty used for shift signedness) */
static void cg_apply_binop(int op, int ty) {
    if (op == TK_PLUS)    { cg_s("    add r1, r2, r1\n"); return; }
    if (op == TK_MINUS)   { cg_s("    sub r1, r2, r1\n"); return; }
    if (op == TK_STAR)    { cg_s("    mul r1, r2, r1\n"); return; }
    if (op == TK_SLASH)   { cg_s("    div r1, r2, r1\n"); return; }
    if (op == TK_PERCENT) { cg_s("    rem r1, r2, r1\n"); return; }
    if (op == TK_AMP)     { cg_s("    and r1, r2, r1\n"); return; }
    if (op == TK_PIPE)    { cg_s("    or r1, r2, r1\n"); return; }
    if (op == TK_CARET)   { cg_s("    xor r1, r2, r1\n"); return; }
    if (op == TK_LSHIFT)  { cg_s("    sll r1, r2, r1\n"); return; }
    if (op == TK_RSHIFT) {
        if (ty & TY_UNSIGNED) cg_s("    srl r1, r2, r1\n");
        else                  cg_s("    sra r1, r2, r1\n");
        return;
    }
    p_error("unknown binop");
}

static void gen_expr(Node *n);
static void gen_stmt(Node *n);

/* Generate address of lvalue into r1 */
static void gen_addr(Node *n) {
    char lbl[32];
    int i;

    if (n->kind == ND_VAR) {
        if (n->is_local) {
            cg_frame_off(n->offset);
        } else {
            /* Global variable address */
            cg_la(n->name);
        }
        return;
    }

    /* Dereference: address of *p is just the value of p */
    if (n->kind == ND_UNARY && n->op == TK_STAR) {
        gen_expr(n->lhs);
        return;
    }

    /* Member access: address of base + member offset */
    if (n->kind == ND_MEMBER) {
        gen_addr(n->lhs);
        if (n->val != 0) {
            cg_r1_add_imm(n->val);
        }
        return;
    }

    p_error("not an lvalue");
}

/* Generate expression, result in r1 */
static void gen_expr(Node *n) {
    int l1;
    int l2;
    Node *a;
    int i;
    int elem_sz;
    char slbl[16];

    if (n->kind == ND_NUM) {
        cg_li(n->val);
        return;
    }

    if (n->kind == ND_STRING) {
        /* Load address of string literal */
        cg_s("    lui r1, %hi(.LS");
        cg_n(n->val);
        cg_s(")\n    addi r1, r1, %lo(.LS");
        cg_n(n->val);
        cg_s(")\n");
        return;
    }

    if (n->kind == ND_VAR) {
        if (n->is_array || ty_is_struct(n->ty)) {
            /* Array or struct: evaluate to address (no load) */
            gen_addr(n);
            return;
        }
        if (n->is_local) {
            /* Local scalar: load from stack */
            if (n->offset >= -2048 && n->offset <= 2047) {
                if (!ty_is_ptr(n->ty) && (n->ty & TY_BASE_MASK) == TY_CHAR) {
                    if (n->ty & TY_UNSIGNED) cg_s("    ldbu r1, r30, ");
                    else                      cg_s("    ldb r1, r30, ");
                } else if (!ty_is_ptr(n->ty) && (n->ty & TY_BASE_MASK) == TY_SHORT) {
                    if (n->ty & TY_UNSIGNED) cg_s("    ldhu r1, r30, ");
                    else                      cg_s("    ldh r1, r30, ");
                } else {
                    cg_s("    ldw r1, r30, ");
                }
                cg_n(n->offset);
                cg_c(10);
            } else {
                /* Large offset: compute address, then load */
                cg_frame_off(n->offset);
                cg_load(n->ty);
            }
        } else {
            /* Global scalar: load via %hi/%lo */
            cg_la(n->name);
            cg_load(n->ty);
        }
        return;
    }

    if (n->kind == ND_ASSIGN) {
        /* evaluate rhs into r1, save; compute lhs addr; store */
        gen_expr(n->rhs);
        if (cg_rsp < CG_RSP_MAX) {
            cg_rpush();
            gen_addr(n->lhs);
            cg_rpop();
        } else {
            cg_push();
            gen_addr(n->lhs);
            cg_pop();
        }
        /* r1 = lhs address, r2 = rhs value */
        cg_store(n->ty);
        /* Result value = r2; move to r1 */
        cg_s("    addi r1, r2, 0\n");
        return;
    }

    if (n->kind == ND_UNARY) {
        if (n->op == TK_MINUS) {
            gen_expr(n->lhs);
            cg_s("    sub r1, r0, r1\n");
            return;
        }
        if (n->op == TK_BANG) {
            gen_expr(n->lhs);
            cg_s("    seq r1, r1, r0\n");
            return;
        }
        /* Dereference *p */
        if (n->op == TK_STAR) {
            gen_expr(n->lhs);
            /* Struct deref: result is address (no load) */
            if (!ty_is_struct(n->ty)) {
                cg_load(n->ty);
            }
            return;
        }
        /* Address-of &x */
        if (n->op == TK_AMP) {
            gen_addr(n->lhs);
            return;
        }
        /* Bitwise NOT ~x: r1 = r1 XOR -1 */
        if (n->op == TK_TILDE) {
            gen_expr(n->lhs);
            cg_s("    addi r2, r0, -1\n");
            cg_s("    xor r1, r1, r2\n");
            return;
        }
        p_error("unknown unary op");
        return;
    }

    if (n->kind == ND_BINOP) {
        /* Short-circuit for && and || */
        if (n->op == TK_LAND) {
            l1 = cg_label();
            l2 = cg_label();
            gen_expr(n->lhs);
            cg_beq_long(l1);
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
            cg_bne_long(l1);
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

        /* Pointer arithmetic: scale the non-pointer operand */
        if (n->op == TK_PLUS || n->op == TK_MINUS) {
            if (ty_is_ptr(n->lhs->ty)) {
                elem_sz = ty_size(ty_deref(n->lhs->ty));
                gen_expr(n->lhs);
                if (!expr_has_call(n->rhs) && cg_rsp < CG_RSP_MAX) {
                    cg_rpush();
                    gen_expr(n->rhs);
                    if (elem_sz > 1) {
                        cg_s("    addi r2, r0, ");
                        cg_n(elem_sz);
                        cg_c(10);
                        cg_s("    mul r1, r1, r2\n");
                    }
                    cg_rpop();
                } else {
                    cg_push();
                    gen_expr(n->rhs);
                    if (elem_sz > 1) {
                        cg_s("    addi r2, r0, ");
                        cg_n(elem_sz);
                        cg_c(10);
                        cg_s("    mul r1, r1, r2\n");
                    }
                    cg_pop();
                }
                /* r2 = lhs (pointer), r1 = rhs (scaled) */
                if (n->op == TK_PLUS) {
                    cg_s("    add r1, r2, r1\n");
                } else {
                    cg_s("    sub r1, r2, r1\n");
                }
                return;
            }
        }

        /* Regular binary op: lhs in r2, rhs in r1 */
        gen_expr(n->lhs);
        if (!expr_has_call(n->rhs) && cg_rsp < CG_RSP_MAX) {
            cg_rpush();
            gen_expr(n->rhs);
            cg_rpop();
        } else {
            cg_push();
            gen_expr(n->rhs);
            cg_pop();
        }
        /* r2 = lhs, r1 = rhs */

        if (n->op == TK_PLUS)    { cg_s("    add r1, r2, r1\n"); return; }
        if (n->op == TK_MINUS)   { cg_s("    sub r1, r2, r1\n"); return; }
        if (n->op == TK_STAR)    { cg_s("    mul r1, r2, r1\n"); return; }
        if (n->op == TK_SLASH)   { cg_s("    div r1, r2, r1\n"); return; }
        if (n->op == TK_PERCENT) { cg_s("    rem r1, r2, r1\n"); return; }
        if (n->op == TK_EQ)      { cg_s("    seq r1, r2, r1\n"); return; }
        if (n->op == TK_NE)      { cg_s("    sne r1, r2, r1\n"); return; }
        if (n->op == TK_LT) {
            if (n->ty & TY_UNSIGNED) cg_s("    sltu r1, r2, r1\n");
            else                     cg_s("    slt r1, r2, r1\n");
            return;
        }
        if (n->op == TK_GT) {
            if (n->ty & TY_UNSIGNED) cg_s("    sgtu r1, r2, r1\n");
            else                     cg_s("    sgt r1, r2, r1\n");
            return;
        }
        if (n->op == TK_LE) {
            if (n->ty & TY_UNSIGNED) cg_s("    sleu r1, r2, r1\n");
            else                     cg_s("    sle r1, r2, r1\n");
            return;
        }
        if (n->op == TK_GE) {
            if (n->ty & TY_UNSIGNED) cg_s("    sgeu r1, r2, r1\n");
            else                     cg_s("    sge r1, r2, r1\n");
            return;
        }
        if (n->op == TK_AMP)     { cg_s("    and r1, r2, r1\n"); return; }
        if (n->op == TK_PIPE)    { cg_s("    or r1, r2, r1\n"); return; }
        if (n->op == TK_CARET)   { cg_s("    xor r1, r2, r1\n"); return; }
        if (n->op == TK_LSHIFT)  { cg_s("    sll r1, r2, r1\n"); return; }
        if (n->op == TK_RSHIFT) {
            if (n->ty & TY_UNSIGNED) cg_s("    srl r1, r2, r1\n");
            else                     cg_s("    sra r1, r2, r1\n");
            return;
        }

        p_error("unknown binop");
        return;
    }

    if (n->kind == ND_COMP_ASSIGN) {
        /* Compound assignment: lhs op= rhs */
        if (!expr_has_call(n->rhs) && cg_rsp + 1 < CG_RSP_MAX) {
            /* Register path: addr in reg, old_val in reg */
            gen_addr(n->lhs);
            cg_rpush();             /* reg: addr in rN */
            cg_load(n->ty);         /* r1 = old_val (r1 still had addr) */
            cg_rpush();             /* reg: old_val in rN+1 */
            gen_expr(n->rhs);       /* r1 = rhs */
            if (ty_is_ptr(n->ty)) {
                if (n->op == TK_PLUS || n->op == TK_MINUS) {
                    elem_sz = ty_size(ty_deref(n->ty));
                    if (elem_sz > 1) {
                        cg_s("    addi r2, r0, ");
                        cg_n(elem_sz);
                        cg_c(10);
                        cg_s("    mul r1, r1, r2\n");
                    }
                }
            }
            cg_rpop();              /* r2 = old_val */
            cg_apply_binop(n->op, n->ty);  /* r1 = r2 op r1 = new_val */
            /* Store new_val back to addr: need r2=new_val, r1=addr */
            cg_s("    addi r2, r1, 0\n");   /* r2 = new_val */
            cg_rsp = cg_rsp - 1;
            cg_s("    addi r1, r");
            cg_n(11 + cg_rsp);
            cg_s(", 0\n");                  /* r1 = addr */
            cg_store(n->ty);               /* mem[r1] = r2 */
            cg_s("    addi r1, r2, 0\n");   /* r1 = new_val (result) */
        } else {
            /* Memory path (original) */
            gen_addr(n->lhs);
            cg_push();              /* stack: [addr], r1 = addr still */
            cg_load(n->ty);         /* r1 = old_val */
            cg_push();              /* stack: [addr, old_val] */
            gen_expr(n->rhs);       /* r1 = rhs */
            if (ty_is_ptr(n->ty)) {
                if (n->op == TK_PLUS || n->op == TK_MINUS) {
                    elem_sz = ty_size(ty_deref(n->ty));
                    if (elem_sz > 1) {
                        cg_s("    addi r2, r0, ");
                        cg_n(elem_sz);
                        cg_c(10);
                        cg_s("    mul r1, r1, r2\n");
                    }
                }
            }
            cg_pop();               /* r2 = old_val, stack: [addr] */
            cg_apply_binop(n->op, n->ty);  /* r1 = r2 op r1 = new_val */
            cg_s("    addi r2, r1, 0\n");   /* r2 = new_val */
            cg_s("    ldw r1, r29, 0\n    addi r29, r29, 4\n"); /* pop addr → r1 */
            cg_store(n->ty);               /* mem[r1] = r2 */
            cg_s("    addi r1, r2, 0\n");   /* r1 = new_val (result) */
        }
        return;
    }

    if (n->kind == ND_POST_INC || n->kind == ND_POST_DEC) {
        /* Postfix ++/--: result is old value */
        if (ty_is_ptr(n->ty)) {
            elem_sz = ty_size(ty_deref(n->ty));
        } else {
            elem_sz = 1;
        }
        if (cg_rsp + 1 < CG_RSP_MAX) {
            /* Register path: no calls after gen_addr */
            gen_addr(n->lhs);
            cg_rpush();             /* reg: addr in rN */
            cg_load(n->ty);         /* r1 = old_val */
            cg_rpush();             /* reg: old_val in rN+1 */
            if (n->kind == ND_POST_INC) {
                cg_s("    addi r1, r1, ");
                cg_n(elem_sz);
                cg_c(10);
            } else {
                cg_s("    addi r1, r1, -");
                cg_n(elem_sz);
                cg_c(10);
            }
            /* r1 = new_val, store back to addr */
            cg_s("    addi r2, r1, 0\n");     /* r2 = new_val */
            cg_rsp = cg_rsp - 2;
            cg_s("    addi r1, r");
            cg_n(11 + cg_rsp);
            cg_s(", 0\n");                    /* r1 = addr */
            cg_store(n->ty);                  /* mem[r1] = r2 */
            cg_s("    addi r1, r");
            cg_n(11 + cg_rsp + 1);
            cg_s(", 0\n");                    /* r1 = old_val (result) */
        } else {
            /* Memory path (original) */
            gen_addr(n->lhs);
            cg_push();
            cg_load(n->ty);
            cg_push();
            if (n->kind == ND_POST_INC) {
                cg_s("    addi r1, r1, ");
                cg_n(elem_sz);
                cg_c(10);
            } else {
                cg_s("    addi r1, r1, -");
                cg_n(elem_sz);
                cg_c(10);
            }
            cg_s("    addi r2, r1, 0\n");
            cg_s("    ldw r1, r29, 4\n");
            cg_store(n->ty);
            cg_s("    ldw r1, r29, 0\n    addi r29, r29, 8\n");
        }
        return;
    }

    if (n->kind == ND_TERNARY) {
        l1 = cg_label();   /* false branch */
        l2 = cg_label();   /* end */
        gen_expr(n->cond);
        cg_s("    beq r1, r0, ");
        cg_lref(l1);
        cg_c(10);
        gen_expr(n->lhs);  /* then expr */
        cg_s("    jal r0, ");
        cg_lref(l2);
        cg_c(10);
        cg_ldef(l1);
        gen_expr(n->rhs);  /* else expr */
        cg_ldef(l2);
        return;
    }

    if (n->kind == ND_CAST) {
        gen_expr(n->lhs);
        /* No-op for 32-bit types — just changes the type tag */
        return;
    }

    if (n->kind == ND_COMMA) {
        gen_expr(n->lhs);  /* discard result */
        gen_expr(n->rhs);  /* keep result in r1 */
        return;
    }

    if (n->kind == ND_MEMBER) {
        gen_addr(n);
        /* Struct-typed member: keep as address; scalar: load */
        if (!ty_is_struct(n->ty)) {
            cg_load(n->ty);
        }
        return;
    }

    if (n->kind == ND_CALL) {
        /* Evaluate all args left-to-right and push */
        a = n->args;
        while (a) {
            gen_expr(a);
            cg_push();
            a = a->next;
        }
        /* Pop into argument registers: arg0 at highest offset */
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

    if (n->kind == ND_FUNC_REF) {
        /* Load function address into r1 */
        cg_la(n->name);
        return;
    }

    if (n->kind == ND_CALL_PTR) {
        /* Indirect call: evaluate callee, push, evaluate args, call via jalr */
        gen_expr(n->lhs);
        cg_push();  /* push callee address */
        /* Evaluate all args left-to-right and push */
        a = n->args;
        while (a) {
            gen_expr(a);
            cg_push();
            a = a->next;
        }
        /* Pop args into argument registers */
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
        /* Pop callee address into r2 and call */
        cg_s("    ldw r2, r29, 0\n    addi r29, r29, 4\n");
        cg_s("    jalr r31, r2, 0\n");
        return;
    }

    p_error("unknown expression node");
}

/* Generate statement */
static void gen_stmt(Node *n) {
    int l1;
    int l2;
    int l3;
    int sw_d;
    int sw_b;
    int sw_n;
    int sw_i;
    int brk_lbl;
    int def_lbl;
    int cd;
    int ci2;
    int skip_lbl;
    Node *s;
    Node *cs;

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
            cg_beq_long(l1);
            gen_stmt(n->body);
            cg_s("    jal r0, ");
            cg_lref(l2);
            cg_c(10);
            cg_ldef(l1);
            gen_stmt(n->els);
            cg_ldef(l2);
        } else {
            cg_beq_long(l1);
            gen_stmt(n->body);
            cg_ldef(l1);
        }
        return;
    }

    if (n->kind == ND_WHILE) {
        l1 = cg_label();   /* loop_top = continue target */
        l2 = cg_label();   /* break target */
        cg_break_lbl[cg_loop_depth] = l2;
        cg_cont_lbl[cg_loop_depth] = l1;
        cg_loop_depth = cg_loop_depth + 1;
        cg_ldef(l1);
        gen_expr(n->cond);
        cg_beq_long(l2);
        gen_stmt(n->body);
        cg_s("    jal r0, ");
        cg_lref(l1);
        cg_c(10);
        cg_ldef(l2);
        cg_loop_depth = cg_loop_depth - 1;
        return;
    }

    if (n->kind == ND_DO_WHILE) {
        l1 = cg_label();   /* loop_top */
        l2 = cg_label();   /* cont_label */
        l3 = cg_label();   /* break_label */
        cg_break_lbl[cg_loop_depth] = l3;
        cg_cont_lbl[cg_loop_depth] = l2;
        cg_loop_depth = cg_loop_depth + 1;
        cg_ldef(l1);
        gen_stmt(n->body);
        cg_ldef(l2);
        gen_expr(n->cond);
        cg_bne_long(l1);
        cg_ldef(l3);
        cg_loop_depth = cg_loop_depth - 1;
        return;
    }

    if (n->kind == ND_FOR) {
        l1 = cg_label();   /* loop_top */
        l2 = cg_label();   /* cont_label (before step) */
        l3 = cg_label();   /* break_label */
        cg_break_lbl[cg_loop_depth] = l3;
        cg_cont_lbl[cg_loop_depth] = l2;
        cg_loop_depth = cg_loop_depth + 1;
        if (n->init) gen_expr(n->init);
        cg_ldef(l1);
        gen_expr(n->cond);
        cg_beq_long(l3);
        gen_stmt(n->body);
        cg_ldef(l2);
        if (n->step) gen_expr(n->step);
        cg_s("    jal r0, ");
        cg_lref(l1);
        cg_c(10);
        cg_ldef(l3);
        cg_loop_depth = cg_loop_depth - 1;
        return;
    }

    if (n->kind == ND_SWITCH) {
        /* Pre-scan: collect case values and assign labels */
        sw_d = cg_sw_depth;
        sw_b = 0;
        if (sw_d > 0) {
            sw_b = cg_sw_base[sw_d - 1] + cg_sw_count[sw_d - 1];
        }
        cg_sw_base[sw_d] = sw_b;
        cg_sw_count[sw_d] = 0;
        cg_sw_def[sw_d] = -1;

        /* Walk the body's statement list to find case/default */
        if (n->body && n->body->kind == ND_BLOCK) {
            cs = n->body->body;
            while (cs) {
                if (cs->kind == ND_CASE) {
                    sw_n = cg_sw_count[sw_d];
                    cg_sw_val[sw_b + sw_n] = cs->val;
                    cg_sw_lbl[sw_b + sw_n] = cg_label();
                    cg_sw_count[sw_d] = sw_n + 1;
                } else if (cs->kind == ND_DEFAULT) {
                    cg_sw_def[sw_d] = cg_label();
                }
                cs = cs->next;
            }
        }

        brk_lbl = cg_label();
        if (cg_sw_def[sw_d] >= 0) {
            def_lbl = cg_sw_def[sw_d];
        } else {
            def_lbl = brk_lbl;
        }

        /* Push break target */
        cg_break_lbl[cg_loop_depth] = brk_lbl;
        cg_loop_depth = cg_loop_depth + 1;

        /* Generate condition expression */
        gen_expr(n->cond);
        cg_s("    addi r2, r1, 0\n");  /* r2 = switch value */

        /* Comparison chain (long-branch: bne skip + jal case) */
        sw_n = cg_sw_count[sw_d];
        sw_i = 0;
        while (sw_i < sw_n) {
            cg_li(cg_sw_val[sw_b + sw_i]);
            skip_lbl = cg_label();
            cg_s("    bne r2, r1, ");
            cg_lref(skip_lbl);
            cg_c(10);
            cg_s("    jal r0, ");
            cg_lref(cg_sw_lbl[sw_b + sw_i]);
            cg_c(10);
            cg_ldef(skip_lbl);
            sw_i = sw_i + 1;
        }

        /* Jump to default or break */
        cg_s("    jal r0, ");
        cg_lref(def_lbl);
        cg_c(10);

        /* Generate body with case/default cursor */
        cg_sw_cur[sw_d] = 0;
        cg_sw_depth = sw_d + 1;
        gen_stmt(n->body);
        cg_sw_depth = sw_d;

        /* Break label */
        cg_ldef(brk_lbl);
        cg_loop_depth = cg_loop_depth - 1;
        return;
    }

    if (n->kind == ND_CASE) {
        cd = cg_sw_depth - 1;
        ci2 = cg_sw_cur[cd];
        cg_ldef(cg_sw_lbl[cg_sw_base[cd] + ci2]);
        cg_sw_cur[cd] = ci2 + 1;
        return;
    }

    if (n->kind == ND_DEFAULT) {
        cd = cg_sw_depth - 1;
        cg_ldef(cg_sw_def[cd]);
        return;
    }

    if (n->kind == ND_BREAK) {
        if (cg_loop_depth < 1) p_error("break outside loop/switch");
        cg_s("    jal r0, ");
        cg_lref(cg_break_lbl[cg_loop_depth - 1]);
        cg_c(10);
        return;
    }

    if (n->kind == ND_CONTINUE) {
        if (cg_loop_depth < 1) p_error("continue outside loop");
        cg_s("    jal r0, ");
        cg_lref(cg_cont_lbl[cg_loop_depth - 1]);
        cg_c(10);
        return;
    }

    if (n->kind == ND_GOTO) {
        cg_s("    jal r0, ");
        cg_lref(n->val);
        cg_c(10);
        return;
    }

    if (n->kind == ND_LABEL) {
        cg_ldef(n->val);
        if (n->body) gen_stmt(n->body);
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
static void gen_func(Node *fn) {
    int fs;
    int i;

    fs = fn->locals_size;
    /* Align to 4 */
    fs = ((fs + 3) / 4) * 4;

    /* Reset register stack for this function */
    cg_rsp = 0;

    /* Allocate a unique epilog label for this function */
    cg_epilog = cg_label();

    /* Function label */
    cg_s(".global ");
    cg_s(fn->name);
    cg_c(10);
    cg_s(fn->name);
    cg_s(":\n");

    /* Prolog */
    if (fs <= 2047) {
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
    } else {
        /* Large frame: save r31/r30 at small offsets from old SP,
           then set FP = old SP, then adjust SP via lui+sub */
        cg_s("    stw r29, r31, -4\n");
        cg_s("    stw r29, r30, -8\n");
        cg_s("    addi r30, r29, 0\n");
        cg_li(fs);
        cg_s("    sub r29, r30, r1\n");
    }

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
    if (fs <= 2047) {
        cg_s("    ldw r31, r29, ");
        cg_n(fs - 4);
        cg_c(10);
        cg_s("    ldw r30, r29, ");
        cg_n(fs - 8);
        cg_c(10);
        cg_s("    addi r29, r29, ");
        cg_n(fs);
        cg_c(10);
    } else {
        /* Large frame: restore from FP-relative small offsets */
        cg_s("    ldw r31, r30, -4\n");
        cg_s("    ldw r1, r30, -8\n");
        cg_s("    addi r29, r30, 0\n");
        cg_s("    addi r30, r1, 0\n");
    }
    cg_s("    jalr r0, r31, 0\n\n");
}

/* Emit .data section: string literals + global variables */
static void gen_data(void) {
    int i;
    int j;
    int len;
    int elem_sz;
    char *sp;
    int ch;

    cg_s(".data\n");

    /* String literals from the lexer pool */
    i = 0;
    while (i < lex_str_count) {
        cg_s(".LS");
        cg_n(i);
        cg_s(":\n    .byte ");
        sp = lex_strpool + lex_str_off[i];
        len = lex_str_len[i];
        j = 0;
        while (j < len) {
            if (j > 0) cg_s(", ");
            cg_n(sp[j] & 255);
            j = j + 1;
        }
        if (j > 0) cg_s(", ");
        cg_s("0\n");  /* null terminator */
        i = i + 1;
    }

    /* Global variables (initialized → .data, uninitialized → .bss) */
    i = 0;
    while (i < ps_nglobals) {
        if (ps_ginit_start[i] >= 0) {
            /* Array/struct initializer list */
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n");
            /* Determine element size: char arrays use .byte, else .word */
            elem_sz = 4;
            if (ty_is_ptr(ps_gtype[i]) &&
                (ty_deref(ps_gtype[i]) & TY_BASE_MASK) == TY_CHAR) {
                elem_sz = 1;
            }
            j = 0;
            while (j < ps_ginit_count[i]) {
                if (elem_sz == 1) {
                    cg_s("    .byte ");
                } else {
                    cg_s("    .word ");
                }
                cg_n(ps_ginit_pool[ps_ginit_start[i] + j]);
                cg_c(10);
                j = j + 1;
            }
            /* Remaining bytes zero-filled */
            len = ps_gsize[i] - (ps_ginit_count[i] * elem_sz);
            if (len > 0) {
                cg_s("    .space ");
                cg_n(len);
                cg_c(10);
            }
        } else if (ps_gsize[i] == 0 && ps_gstr[i] >= 0) {
            /* String-initialized scalar: .word .LSN */
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n    .word .LS");
            cg_n(ps_gstr[i]);
            cg_c(10);
        } else if (ps_gsize[i] == 0 && ps_ginit[i] != 0) {
            /* Integer-initialized scalar: stays in .data */
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n    .word ");
            cg_n(ps_ginit[i]);
            cg_c(10);
        }
        i = i + 1;
    }

    /* BSS section: uninitialized arrays and zero-init scalars */
    cg_s(".bss\n");
    i = 0;
    while (i < ps_nglobals) {
        if (ps_ginit_start[i] >= 0) {
            /* Already emitted in .data */
        } else if (ps_gsize[i] > 0) {
            /* Array */
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n    .space ");
            cg_n(ps_gsize[i]);
            cg_c(10);
        } else if (ps_ginit[i] == 0 && ps_gstr[i] < 0) {
            /* Zero-init scalar (not string-initialized) */
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n    .space 4\n");
        }
        i = i + 1;
    }
}

/* Generate entire program */
static void gen_program(Node *prog) {
    Node *fn;

    cg_s(".text\n\n");
    fn = prog->body;
    while (fn) {
        gen_func(fn);
        fn = fn->next;
    }

    gen_data();
}
