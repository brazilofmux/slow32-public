/* hir_lower.h -- AST to HIR lowering for s12cc
 *
 * Walks each function's AST and emits HIR instructions.
 * Phase A: no SSA, locals accessed via alloca+load/store.
 * Ternary/&&/|| use temp stack slots to merge values.
 */

/* --- Alloca map: fp-offset → HIR instruction --- */
#define HL_MAX_ALLOCA 256
static int hl_aoff[HL_MAX_ALLOCA];
static int hl_ainst[HL_MAX_ALLOCA];
static int hl_nalloca;

/* Temp stack for merge values (&&, ||, ternary) */
static int hl_temp_stack;

/* --- Loop break/continue targets --- */
#define HL_MAX_LOOP 16
static int hl_break_blk[HL_MAX_LOOP];
static int hl_cont_blk[HL_MAX_LOOP];
static int hl_loop_depth;

/* --- Switch state --- */
#define HL_MAX_SW_DEPTH 8
#define HL_MAX_CASE 256
static int hl_sw_val[HL_MAX_CASE];
static int hl_sw_blk[HL_MAX_CASE];
static int hl_sw_base[HL_MAX_SW_DEPTH];
static int hl_sw_count[HL_MAX_SW_DEPTH];
static int hl_sw_def[HL_MAX_SW_DEPTH];
static int hl_sw_cur[HL_MAX_SW_DEPTH];
static int hl_sw_depth;
static int hl_sw_ord[HL_MAX_CASE];

/* --- Goto label map --- */
#define HL_MAX_GOTO 512
static int hl_goto_id[HL_MAX_GOTO];
static int hl_goto_blk[HL_MAX_GOTO];
static int hl_ngoto;

/* --- 64-bit twin-value tracking --- */
/* After hl_expr() for a TY_LLONG expression, hl_hi holds the hi word index.
 * Callers must save hl_hi before calling hl_expr() again. */
static int hl_hi;

/* Widen a 32-bit value to 64-bit (sign or zero extend) */
static void hl_widen64(int val, int from_ty, int *out_lo, int *out_hi) {
    int c31;
    *out_lo = val;
    if (from_ty & TY_UNSIGNED) {
        *out_hi = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
    } else {
        c31 = hi_emit(HI_ICONST, TY_INT, -1, -1, 31, NULL);
        *out_hi = hi_emit(HI_SRA, TY_INT, val, c31, 0, NULL);
    }
}

/* Forward declarations */
static int hl_expr(Node *n);
static int hl_addr(Node *n);
static void hl_stmt(Node *n);

/* --- Helpers --- */

static int hl_get_alloca(int offset, int ty) {
    int i;
    i = 0;
    while (i < hl_nalloca) {
        if (hl_aoff[i] == offset) return hl_ainst[i];
        i = i + 1;
    }
    if (hl_nalloca >= HL_MAX_ALLOCA) {
        fputs("s12cc: too many allocas\n", stderr);
        exit(1);
    }
    hl_aoff[hl_nalloca] = offset;
    hl_ainst[hl_nalloca] = hi_emit(HI_ALLOCA, ty, -1, -1, offset, NULL);
    hl_nalloca = hl_nalloca + 1;
    return hl_ainst[hl_nalloca - 1];
}

static int hl_alloc_temp(void) {
    hl_temp_stack = hl_temp_stack + 4;
    return 0 - hl_temp_stack;
}

static int hl_label_block(int label_id) {
    int i;
    i = 0;
    while (i < hl_ngoto) {
        if (hl_goto_id[i] == label_id) return hl_goto_blk[i];
        i = i + 1;
    }
    if (hl_ngoto >= HL_MAX_GOTO) {
        fputs("s12cc: too many goto labels\n", stderr);
        exit(1);
    }
    hl_goto_id[hl_ngoto] = label_id;
    hl_goto_blk[hl_ngoto] = hir_new_block();
    hl_ngoto = hl_ngoto + 1;
    return hl_goto_blk[hl_ngoto - 1];
}

static int hl_sw_less_val(int a, int b, int is_unsigned) {
    if (is_unsigned) return ((unsigned)a) < ((unsigned)b);
    return a < b;
}

static void hl_sw_sort_cases(int sw_b, int sw_n, int is_unsigned) {
    int i;
    int j;
    int key;
    int keyv;
    int curv;

    i = 0;
    while (i < sw_n) {
        hl_sw_ord[sw_b + i] = sw_b + i;
        i = i + 1;
    }

    i = 1;
    while (i < sw_n) {
        key = hl_sw_ord[sw_b + i];
        keyv = hl_sw_val[key];
        j = i - 1;
        while (j >= 0) {
            curv = hl_sw_val[hl_sw_ord[sw_b + j]];
            if (!hl_sw_less_val(keyv, curv, is_unsigned)) break;
            hl_sw_ord[sw_b + j + 1] = hl_sw_ord[sw_b + j];
            j = j - 1;
        }
        hl_sw_ord[sw_b + j + 1] = key;
        i = i + 1;
    }
}

static void hl_sw_emit_chain(int lv, int def_blk, int sw_b, int sw_n) {
    int sw_i;
    int cv;
    int cmp;
    int next_blk;

    sw_i = 0;
    while (sw_i < sw_n) {
        cv = hi_emit(HI_ICONST, TY_INT, -1, -1, hl_sw_val[sw_b + sw_i], NULL);
        cmp = hi_emit(HI_SEQ, TY_INT, lv, cv, 0, NULL);
        next_blk = hir_new_block();
        hi_emit(HI_BRC, 0, cmp, hl_sw_blk[sw_b + sw_i], next_blk, NULL);
        hl_switch_block(next_blk);
        sw_i = sw_i + 1;
    }

    hi_emit(HI_BR, 0, -1, -1, def_blk, NULL);
}

static void hl_sw_emit_bsearch(int lv, int def_blk, int lt_kind, int lo, int hi) {
    int mid;
    int idx;
    int c;
    int cv;
    int cmp_eq;
    int cmp_lt;
    int neq_blk;
    int left_blk;
    int right_blk;

    if (lo > hi) {
        hi_emit(HI_BR, 0, -1, -1, def_blk, NULL);
        return;
    }

    mid = lo + (hi - lo) / 2;
    idx = hl_sw_ord[mid];
    c = hl_sw_val[idx];
    cv = hi_emit(HI_ICONST, TY_INT, -1, -1, c, NULL);
    cmp_eq = hi_emit(HI_SEQ, TY_INT, lv, cv, 0, NULL);
    if (lo == hi) {
        hi_emit(HI_BRC, 0, cmp_eq, hl_sw_blk[idx], def_blk, NULL);
        return;
    }

    neq_blk = hir_new_block();
    hi_emit(HI_BRC, 0, cmp_eq, hl_sw_blk[idx], neq_blk, NULL);

    hl_switch_block(neq_blk);
    cv = hi_emit(HI_ICONST, TY_INT, -1, -1, c, NULL);
    cmp_lt = hi_emit(lt_kind, TY_INT, lv, cv, 0, NULL);
    left_blk = hir_new_block();
    right_blk = hir_new_block();
    hi_emit(HI_BRC, 0, cmp_lt, left_blk, right_blk, NULL);

    hl_switch_block(left_blk);
    hl_sw_emit_bsearch(lv, def_blk, lt_kind, lo, mid - 1);

    hl_switch_block(right_blk);
    hl_sw_emit_bsearch(lv, def_blk, lt_kind, mid + 1, hi);
}

/* Map AST binary operator token to HIR instruction kind */
static int hl_binop_kind(int op, int ty) {
    if (op == TK_PLUS) return HI_ADD;
    if (op == TK_MINUS) return HI_SUB;
    if (op == TK_STAR) return HI_MUL;
    if (op == TK_SLASH) return HI_DIV;
    if (op == TK_PERCENT) return HI_REM;
    if (op == TK_AMP) return HI_AND;
    if (op == TK_PIPE) return HI_OR;
    if (op == TK_CARET) return HI_XOR;
    if (op == TK_LSHIFT) return HI_SLL;
    if (op == TK_RSHIFT) {
        if (ty & TY_UNSIGNED) return HI_SRL;
        return HI_SRA;
    }
    if (op == TK_EQ) return HI_SEQ;
    if (op == TK_NE) return HI_SNE;
    if (op == TK_LT) {
        if (ty & TY_UNSIGNED) return HI_SLTU;
        return HI_SLT;
    }
    if (op == TK_GT) {
        if (ty & TY_UNSIGNED) return HI_SGTU;
        return HI_SGT;
    }
    if (op == TK_LE) {
        if (ty & TY_UNSIGNED) return HI_SLEU;
        return HI_SLE;
    }
    if (op == TK_GE) {
        if (ty & TY_UNSIGNED) return HI_SGEU;
        return HI_SGE;
    }
    return HI_ADD;
}

/* --- Address computation (lvalue → HIR addr instruction) --- */

static int hl_addr(Node *n) {
    int addr;

    if (n->kind == ND_VAR) {
        if (n->is_local) {
            return hl_get_alloca(n->offset, n->ty);
        }
        /* Global variable address */
        return hi_emit(HI_GADDR, TY_PTR + (n->ty & TY_BASE_MASK), -1, -1, 0, n->name);
    }

    /* Dereference: address of *p is just value of p */
    if (n->kind == ND_UNARY && n->op == TK_STAR) {
        return hl_expr(n->lhs);
    }

    /* Member access: base address + offset */
    if (n->kind == ND_MEMBER) {
        addr = hl_addr(n->lhs);
        if (n->val != 0) {
            addr = hi_emit(HI_ADDI, TY_INT, addr, -1, n->val, NULL);
        }
        return addr;
    }

    p_error("not an lvalue (hir)");
    return -1;
}

/* --- Expression lowering --- */

static int hl_expr(Node *n) {
    int lv;
    int rv;
    int cv;
    int addr;
    int val;
    int zero;
    int tmp;
    int tmp_off;
    int elem_sz;
    int scale;
    int kind;
    int rhs_blk;
    int then_blk;
    int else_blk;
    int join_blk;
    int carg_base;
    int nargs;
    int i;
    int old_val;
    int new_val;
    int callee;
    Node *a;

    if (!n) return -1;

    /* Integer literal */
    if (n->kind == ND_NUM) {
        return hi_emit(HI_ICONST, n->ty, -1, -1, n->val, NULL);
    }

    /* String literal */
    if (n->kind == ND_STRING) {
        return hi_emit(HI_SADDR, n->ty, -1, -1, n->val, NULL);
    }

    /* Variable */
    if (n->kind == ND_VAR) {
        if (n->is_array || ty_is_struct(n->ty)) {
            return hl_addr(n);
        }
        addr = hl_addr(n);
        if (ty_is_llong(n->ty)) {
            lv = hi_emit(HI_LOAD, TY_INT, addr, -1, 0, NULL);
            tmp = hi_emit(HI_ADDI, TY_INT, addr, -1, 4, NULL);
            hl_hi = hi_emit(HI_LOAD, TY_INT, tmp, -1, 0, NULL);
            return lv;
        }
        return hi_emit(HI_LOAD, n->ty, addr, -1, 0, NULL);
    }

    /* Assignment */
    if (n->kind == ND_ASSIGN) {
        if (ty_is_llong(n->ty)) {
            int val_hi;
            int addr4;
            val = hl_expr(n->rhs);
            val_hi = hl_hi;
            if (!ty_is_llong(n->rhs->ty)) {
                hl_widen64(val, n->rhs->ty, &val, &val_hi);
            }
            addr = hl_addr(n->lhs);
            hi_emit(HI_STORE, TY_INT, addr, val, 0, NULL);
            addr4 = hi_emit(HI_ADDI, TY_INT, addr, -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, addr4, val_hi, 0, NULL);
            hl_hi = val_hi;
            return val;
        }
        val = hl_expr(n->rhs);
        addr = hl_addr(n->lhs);
        hi_emit(HI_STORE, n->ty, addr, val, 0, NULL);
        return val;
    }

    /* Unary operations */
    if (n->kind == ND_UNARY) {
        if (n->op == TK_MINUS) {
            if (ty_is_llong(n->ty)) {
                int lv_hi;
                int r_lo;
                int r_hi;
                int borrow;
                int z;
                lv = hl_expr(n->lhs);
                lv_hi = hl_hi;
                if (!ty_is_llong(n->lhs->ty)) hl_widen64(lv, n->lhs->ty, &lv, &lv_hi);
                z = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                r_lo = hi_emit(HI_SUB, TY_INT, z, lv, 0, NULL);
                borrow = hi_emit(HI_SLTU, TY_INT, z, lv, 0, NULL);
                r_hi = hi_emit(HI_SUB, TY_INT, z, lv_hi, 0, NULL);
                r_hi = hi_emit(HI_SUB, TY_INT, r_hi, borrow, 0, NULL);
                hl_hi = r_hi;
                return r_lo;
            }
            lv = hl_expr(n->lhs);
            return hi_emit(HI_NEG, n->ty, lv, -1, 0, NULL);
        }
        if (n->op == TK_BANG) {
            if (ty_is_llong(n->lhs->ty)) {
                int lv_hi;
                int eq_lo;
                int eq_hi;
                int z;
                lv = hl_expr(n->lhs);
                lv_hi = hl_hi;
                z = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                eq_lo = hi_emit(HI_SEQ, TY_INT, lv, z, 0, NULL);
                eq_hi = hi_emit(HI_SEQ, TY_INT, lv_hi, z, 0, NULL);
                return hi_emit(HI_AND, TY_INT, eq_lo, eq_hi, 0, NULL);
            }
            lv = hl_expr(n->lhs);
            return hi_emit(HI_NOT, n->ty, lv, -1, 0, NULL);
        }
        if (n->op == TK_TILDE) {
            if (ty_is_llong(n->ty)) {
                int lv_hi;
                lv = hl_expr(n->lhs);
                lv_hi = hl_hi;
                if (!ty_is_llong(n->lhs->ty)) hl_widen64(lv, n->lhs->ty, &lv, &lv_hi);
                lv = hi_emit(HI_BNOT, TY_INT, lv, -1, 0, NULL);
                hl_hi = hi_emit(HI_BNOT, TY_INT, lv_hi, -1, 0, NULL);
                return lv;
            }
            lv = hl_expr(n->lhs);
            return hi_emit(HI_BNOT, n->ty, lv, -1, 0, NULL);
        }
        if (n->op == TK_STAR) {
            lv = hl_expr(n->lhs);
            if (ty_is_struct(n->ty)) return lv;
            return hi_emit(HI_LOAD, n->ty, lv, -1, 0, NULL);
        }
        if (n->op == TK_AMP) {
            return hl_addr(n->lhs);
        }
        p_error("unknown unary (hir)");
        return -1;
    }

    /* Binary operations */
    if (n->kind == ND_BINOP) {
        /* Short-circuit && */
        if (n->op == TK_LAND) {
            tmp_off = hl_alloc_temp();
            tmp = hi_emit(HI_ALLOCA, TY_INT, -1, -1, tmp_off, NULL);
            lv = hl_expr(n->lhs);
            rhs_blk = hir_new_block();
            else_blk = hir_new_block();
            join_blk = hir_new_block();
            hi_emit(HI_BRC, 0, lv, rhs_blk, else_blk, NULL);

            hl_switch_block(rhs_blk);
            rv = hl_expr(n->rhs);
            zero = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
            val = hi_emit(HI_SNE, TY_INT, rv, zero, 0, NULL);
            hi_emit(HI_STORE, TY_INT, tmp, val, 0, NULL);
            hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

            hl_switch_block(else_blk);
            zero = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
            hi_emit(HI_STORE, TY_INT, tmp, zero, 0, NULL);
            hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

            hl_switch_block(join_blk);
            return hi_emit(HI_LOAD, TY_INT, tmp, -1, 0, NULL);
        }

        /* Short-circuit || */
        if (n->op == TK_LOR) {
            tmp_off = hl_alloc_temp();
            tmp = hi_emit(HI_ALLOCA, TY_INT, -1, -1, tmp_off, NULL);
            lv = hl_expr(n->lhs);
            rhs_blk = hir_new_block();
            then_blk = hir_new_block();
            join_blk = hir_new_block();
            hi_emit(HI_BRC, 0, lv, then_blk, rhs_blk, NULL);

            hl_switch_block(rhs_blk);
            rv = hl_expr(n->rhs);
            zero = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
            val = hi_emit(HI_SNE, TY_INT, rv, zero, 0, NULL);
            hi_emit(HI_STORE, TY_INT, tmp, val, 0, NULL);
            hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

            hl_switch_block(then_blk);
            val = hi_emit(HI_ICONST, TY_INT, -1, -1, 1, NULL);
            hi_emit(HI_STORE, TY_INT, tmp, val, 0, NULL);
            hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

            hl_switch_block(join_blk);
            return hi_emit(HI_LOAD, TY_INT, tmp, -1, 0, NULL);
        }

        /* 64-bit binary operations */
        if (ty_is_llong(n->ty) || (sema_is_cmp(n->op) && (ty_is_llong(n->lhs->ty) || ty_is_llong(n->rhs->ty)))) {
            int lv_hi;
            int rv_hi;
            int r_lo;
            int r_hi;
            int carry;
            int carg_base2;
            int eq_lo;
            int eq_hi;
            int hi_lt;
            int hi_eq;
            int lo_lt;
            int tmp2;

            lv = hl_expr(n->lhs);
            lv_hi = hl_hi;
            if (!ty_is_llong(n->lhs->ty)) hl_widen64(lv, n->lhs->ty, &lv, &lv_hi);
            rv = hl_expr(n->rhs);
            rv_hi = hl_hi;
            if (!ty_is_llong(n->rhs->ty)) hl_widen64(rv, n->rhs->ty, &rv, &rv_hi);

            if (n->op == TK_PLUS) {
                r_lo = hi_emit(HI_ADD, TY_INT, lv, rv, 0, NULL);
                carry = hi_emit(HI_SLTU, TY_INT, r_lo, lv, 0, NULL);
                r_hi = hi_emit(HI_ADD, TY_INT, lv_hi, rv_hi, 0, NULL);
                r_hi = hi_emit(HI_ADD, TY_INT, r_hi, carry, 0, NULL);
                hl_hi = r_hi;
                return r_lo;
            }
            if (n->op == TK_MINUS) {
                carry = hi_emit(HI_SLTU, TY_INT, lv, rv, 0, NULL);
                r_lo = hi_emit(HI_SUB, TY_INT, lv, rv, 0, NULL);
                r_hi = hi_emit(HI_SUB, TY_INT, lv_hi, rv_hi, 0, NULL);
                r_hi = hi_emit(HI_SUB, TY_INT, r_hi, carry, 0, NULL);
                hl_hi = r_hi;
                return r_lo;
            }
            if (n->op == TK_AMP) {
                r_lo = hi_emit(HI_AND, TY_INT, lv, rv, 0, NULL);
                hl_hi = hi_emit(HI_AND, TY_INT, lv_hi, rv_hi, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_PIPE) {
                r_lo = hi_emit(HI_OR, TY_INT, lv, rv, 0, NULL);
                hl_hi = hi_emit(HI_OR, TY_INT, lv_hi, rv_hi, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_CARET) {
                r_lo = hi_emit(HI_XOR, TY_INT, lv, rv, 0, NULL);
                hl_hi = hi_emit(HI_XOR, TY_INT, lv_hi, rv_hi, 0, NULL);
                return r_lo;
            }
            /* Comparisons — result is TY_INT */
            if (n->op == TK_EQ) {
                eq_lo = hi_emit(HI_SEQ, TY_INT, lv, rv, 0, NULL);
                eq_hi = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                return hi_emit(HI_AND, TY_INT, eq_lo, eq_hi, 0, NULL);
            }
            if (n->op == TK_NE) {
                eq_lo = hi_emit(HI_SNE, TY_INT, lv, rv, 0, NULL);
                eq_hi = hi_emit(HI_SNE, TY_INT, lv_hi, rv_hi, 0, NULL);
                return hi_emit(HI_OR, TY_INT, eq_lo, eq_hi, 0, NULL);
            }
            if (n->op == TK_LT) {
                if (n->ty & TY_UNSIGNED) {
                    hi_lt = hi_emit(HI_SLTU, TY_INT, lv_hi, rv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                } else {
                    hi_lt = hi_emit(HI_SLT, TY_INT, lv_hi, rv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                }
                lo_lt = hi_emit(HI_SLTU, TY_INT, lv, rv, 0, NULL);
                tmp2 = hi_emit(HI_AND, TY_INT, hi_eq, lo_lt, 0, NULL);
                return hi_emit(HI_OR, TY_INT, hi_lt, tmp2, 0, NULL);
            }
            if (n->op == TK_GT) {
                if (n->ty & TY_UNSIGNED) {
                    hi_lt = hi_emit(HI_SLTU, TY_INT, rv_hi, lv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                } else {
                    hi_lt = hi_emit(HI_SLT, TY_INT, rv_hi, lv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                }
                lo_lt = hi_emit(HI_SLTU, TY_INT, rv, lv, 0, NULL);
                tmp2 = hi_emit(HI_AND, TY_INT, hi_eq, lo_lt, 0, NULL);
                return hi_emit(HI_OR, TY_INT, hi_lt, tmp2, 0, NULL);
            }
            if (n->op == TK_LE) {
                /* a <= b  is  !(b < a) */
                if (n->ty & TY_UNSIGNED) {
                    hi_lt = hi_emit(HI_SLTU, TY_INT, rv_hi, lv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                } else {
                    hi_lt = hi_emit(HI_SLT, TY_INT, rv_hi, lv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                }
                lo_lt = hi_emit(HI_SLTU, TY_INT, rv, lv, 0, NULL);
                tmp2 = hi_emit(HI_AND, TY_INT, hi_eq, lo_lt, 0, NULL);
                tmp2 = hi_emit(HI_OR, TY_INT, hi_lt, tmp2, 0, NULL);
                return hi_emit(HI_NOT, TY_INT, tmp2, -1, 0, NULL);
            }
            if (n->op == TK_GE) {
                /* a >= b  is  !(a < b) */
                if (n->ty & TY_UNSIGNED) {
                    hi_lt = hi_emit(HI_SLTU, TY_INT, lv_hi, rv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                } else {
                    hi_lt = hi_emit(HI_SLT, TY_INT, lv_hi, rv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                }
                lo_lt = hi_emit(HI_SLTU, TY_INT, lv, rv, 0, NULL);
                tmp2 = hi_emit(HI_AND, TY_INT, hi_eq, lo_lt, 0, NULL);
                tmp2 = hi_emit(HI_OR, TY_INT, hi_lt, tmp2, 0, NULL);
                return hi_emit(HI_NOT, TY_INT, tmp2, -1, 0, NULL);
            }
            /* Mul/Div/Rem/Shifts — emit CALL to helper */
            if (n->op == TK_STAR) {
                carg_base2 = h_ncarg;
                h_carg[h_ncarg] = lv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = lv_hi;   h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv_hi;   h_ncarg = h_ncarg + 1;
                r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__muldi3");
                h_cbase[r_lo] = carg_base2;
                hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_SLASH) {
                carg_base2 = h_ncarg;
                h_carg[h_ncarg] = lv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = lv_hi;   h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv_hi;   h_ncarg = h_ncarg + 1;
                if (n->ty & TY_UNSIGNED) {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__udivdi3");
                } else {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__divdi3");
                }
                h_cbase[r_lo] = carg_base2;
                hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_PERCENT) {
                carg_base2 = h_ncarg;
                h_carg[h_ncarg] = lv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = lv_hi;   h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv_hi;   h_ncarg = h_ncarg + 1;
                if (n->ty & TY_UNSIGNED) {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__umoddi3");
                } else {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__moddi3");
                }
                h_cbase[r_lo] = carg_base2;
                hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_LSHIFT) {
                /* Inline constant shifts, call helper for variable shifts */
                if (n->rhs->kind == ND_NUM && n->rhs->val >= 0 && n->rhs->val <= 63) {
                    int sn;
                    int sc;
                    sn = n->rhs->val;
                    if (sn == 0) {
                        hl_hi = lv_hi;
                        return lv;
                    }
                    if (sn < 32) {
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn, NULL);
                        r_lo = hi_emit(HI_SLL, TY_INT, lv, sc, 0, NULL);
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn, NULL);
                        r_hi = hi_emit(HI_SLL, TY_INT, lv_hi, sc, 0, NULL);
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, 32 - sn, NULL);
                        tmp2 = hi_emit(HI_SRL, TY_INT, lv, sc, 0, NULL);
                        hl_hi = hi_emit(HI_OR, TY_INT, r_hi, tmp2, 0, NULL);
                        return r_lo;
                    }
                    if (sn == 32) {
                        hl_hi = lv;
                        return hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                    }
                    /* sn > 32 */
                    sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn - 32, NULL);
                    hl_hi = hi_emit(HI_SLL, TY_INT, lv, sc, 0, NULL);
                    return hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                }
                carg_base2 = h_ncarg;
                h_carg[h_ncarg] = lv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = lv_hi;   h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv;      h_ncarg = h_ncarg + 1;
                r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "__ashldi3");
                h_cbase[r_lo] = carg_base2;
                hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_RSHIFT) {
                /* Inline constant shifts, call helper for variable shifts */
                if (n->rhs->kind == ND_NUM && n->rhs->val >= 0 && n->rhs->val <= 63) {
                    int sn;
                    int sc;
                    int is_arith;
                    sn = n->rhs->val;
                    is_arith = !(n->ty & TY_UNSIGNED);
                    if (sn == 0) {
                        hl_hi = lv_hi;
                        return lv;
                    }
                    if (sn < 32) {
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn, NULL);
                        r_lo = hi_emit(HI_SRL, TY_INT, lv, sc, 0, NULL);
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, 32 - sn, NULL);
                        tmp2 = hi_emit(HI_SLL, TY_INT, lv_hi, sc, 0, NULL);
                        r_lo = hi_emit(HI_OR, TY_INT, r_lo, tmp2, 0, NULL);
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn, NULL);
                        hl_hi = hi_emit(is_arith ? HI_SRA : HI_SRL, TY_INT,
                                        lv_hi, sc, 0, NULL);
                        return r_lo;
                    }
                    if (sn == 32) {
                        r_lo = lv_hi;
                        if (is_arith) {
                            sc = hi_emit(HI_ICONST, TY_INT, -1, -1, 31, NULL);
                            hl_hi = hi_emit(HI_SRA, TY_INT, lv_hi, sc, 0, NULL);
                        } else {
                            hl_hi = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                        }
                        return r_lo;
                    }
                    /* sn > 32 */
                    sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn - 32, NULL);
                    r_lo = hi_emit(is_arith ? HI_SRA : HI_SRL, TY_INT,
                                   lv_hi, sc, 0, NULL);
                    if (is_arith) {
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, 31, NULL);
                        hl_hi = hi_emit(HI_SRA, TY_INT, lv_hi, sc, 0, NULL);
                    } else {
                        hl_hi = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                    }
                    return r_lo;
                }
                carg_base2 = h_ncarg;
                h_carg[h_ncarg] = lv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = lv_hi;   h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv;      h_ncarg = h_ncarg + 1;
                if (n->ty & TY_UNSIGNED) {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "__lshrdi3");
                } else {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "__ashrdi3");
                }
                h_cbase[r_lo] = carg_base2;
                hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
                return r_lo;
            }
            /* Logical && || handled elsewhere */
        }

        /* Pointer arithmetic: scale the integer operand */
        if ((n->op == TK_PLUS || n->op == TK_MINUS) && ty_is_ptr(n->lhs->ty)) {
            elem_sz = ty_size(ty_deref(n->lhs->ty));
            lv = hl_expr(n->lhs);
            rv = hl_expr(n->rhs);
            if (elem_sz > 1) {
                scale = hi_emit(HI_ICONST, TY_INT, -1, -1, elem_sz, NULL);
                rv = hi_emit(HI_MUL, TY_INT, rv, scale, 0, NULL);
            }
            if (n->op == TK_PLUS) {
                return hi_emit(HI_ADD, n->ty, lv, rv, 0, NULL);
            }
            return hi_emit(HI_SUB, n->ty, lv, rv, 0, NULL);
        }

        /* Regular binary op */
        lv = hl_expr(n->lhs);
        rv = hl_expr(n->rhs);
        kind = hl_binop_kind(n->op, n->ty);
        return hi_emit(kind, n->ty, lv, rv, 0, NULL);
    }

    /* Compound assignment (+=, -=, etc.) */
    if (n->kind == ND_COMP_ASSIGN) {
        addr = hl_addr(n->lhs);
        old_val = hi_emit(HI_LOAD, n->ty, addr, -1, 0, NULL);
        rv = hl_expr(n->rhs);
        /* Scale for pointer arithmetic */
        if (ty_is_ptr(n->ty) && (n->op == TK_PLUS || n->op == TK_MINUS)) {
            elem_sz = ty_size(ty_deref(n->ty));
            if (elem_sz > 1) {
                scale = hi_emit(HI_ICONST, TY_INT, -1, -1, elem_sz, NULL);
                rv = hi_emit(HI_MUL, TY_INT, rv, scale, 0, NULL);
            }
        }
        kind = hl_binop_kind(n->op, n->ty);
        new_val = hi_emit(kind, n->ty, old_val, rv, 0, NULL);
        hi_emit(HI_STORE, n->ty, addr, new_val, 0, NULL);
        return new_val;
    }

    /* Postfix ++ / -- */
    if (n->kind == ND_POST_INC || n->kind == ND_POST_DEC) {
        addr = hl_addr(n->lhs);
        old_val = hi_emit(HI_LOAD, n->ty, addr, -1, 0, NULL);
        if (ty_is_ptr(n->ty)) {
            elem_sz = ty_size(ty_deref(n->ty));
        } else {
            elem_sz = 1;
        }
        scale = hi_emit(HI_ICONST, TY_INT, -1, -1, elem_sz, NULL);
        if (n->kind == ND_POST_INC) {
            new_val = hi_emit(HI_ADD, n->ty, old_val, scale, 0, NULL);
        } else {
            new_val = hi_emit(HI_SUB, n->ty, old_val, scale, 0, NULL);
        }
        hi_emit(HI_STORE, n->ty, addr, new_val, 0, NULL);
        return old_val;
    }

    /* Ternary c ? a : b */
    if (n->kind == ND_TERNARY) {
        tmp_off = hl_alloc_temp();
        tmp = hi_emit(HI_ALLOCA, n->ty, -1, -1, tmp_off, NULL);
        cv = hl_expr(n->cond);
        then_blk = hir_new_block();
        else_blk = hir_new_block();
        join_blk = hir_new_block();
        hi_emit(HI_BRC, 0, cv, then_blk, else_blk, NULL);

        hl_switch_block(then_blk);
        val = hl_expr(n->lhs);
        hi_emit(HI_STORE, n->ty, tmp, val, 0, NULL);
        hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

        hl_switch_block(else_blk);
        val = hl_expr(n->rhs);
        hi_emit(HI_STORE, n->ty, tmp, val, 0, NULL);
        hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

        hl_switch_block(join_blk);
        return hi_emit(HI_LOAD, n->ty, tmp, -1, 0, NULL);
    }

    /* Type cast */
    if (n->kind == ND_CAST) {
        lv = hl_expr(n->lhs);
        if (ty_is_llong(n->ty) && !ty_is_llong(n->lhs->ty)) {
            /* Widen 32->64 */
            hl_widen64(lv, n->lhs->ty, &lv, &hl_hi);
            return lv;
        }
        if (!ty_is_llong(n->ty) && ty_is_llong(n->lhs->ty)) {
            /* Truncate 64->32: just use lo word */
            return lv;
        }
        return lv;
    }

    /* Comma operator */
    if (n->kind == ND_COMMA) {
        hl_expr(n->lhs);
        return hl_expr(n->rhs);
    }

    /* Member access */
    if (n->kind == ND_MEMBER) {
        addr = hl_addr(n);
        if (ty_is_struct(n->ty)) return addr;
        return hi_emit(HI_LOAD, n->ty, addr, -1, 0, NULL);
    }

    /* va_start(ap, last) → ap = __getfp() */
    if (n->kind == ND_VA_START) {
        addr = hl_addr(n->lhs);
        val = hi_emit(HI_GETFP, TY_INT, -1, -1, 0, NULL);
        hi_emit(HI_STORE, TY_INT, addr, val, 0, NULL);
        return val;
    }

    /* va_arg(ap, type) → val = *ap; ap += 4; return val */
    if (n->kind == ND_VA_ARG) {
        int ap_val;
        int new_ap;
        addr = hl_addr(n->lhs);
        ap_val = hi_emit(HI_LOAD, TY_INT, addr, -1, 0, NULL);
        if (ty_is_llong(n->ty)) {
            /* 64-bit: load lo and hi words, advance ap by 8 */
            int lo;
            int ap4;
            int hi;
            lo = hi_emit(HI_LOAD, TY_INT, ap_val, -1, 0, NULL);
            ap4 = hi_emit(HI_ADDI, TY_INT, ap_val, -1, 4, NULL);
            hi = hi_emit(HI_LOAD, TY_INT, ap4, -1, 0, NULL);
            new_ap = hi_emit(HI_ADDI, TY_INT, ap_val, -1, 8, NULL);
            hi_emit(HI_STORE, TY_INT, addr, new_ap, 0, NULL);
            hl_hi = hi;
            return lo;
        }
        val = hi_emit(HI_LOAD, n->ty, ap_val, -1, 0, NULL);
        new_ap = hi_emit(HI_ADDI, TY_INT, ap_val, -1, 4, NULL);
        hi_emit(HI_STORE, TY_INT, addr, new_ap, 0, NULL);
        return val;
    }

    /* Direct function call */
    if (n->kind == ND_CALL) {
        int av[16];
        int av_hi[16];
        int is64[16];
        int phys_count;
        int ret_ty;
        /* Lower all arguments first (inner calls may use h_carg) */
        a = n->args;
        nargs = 0;
        while (a) {
            if (nargs >= 16) p_error("too many call args (limit 16)");
            av[nargs] = hl_expr(a);
            if (ty_is_llong(a->ty)) {
                av_hi[nargs] = hl_hi;
                is64[nargs] = 1;
            } else {
                av_hi[nargs] = -1;
                is64[nargs] = 0;
            }
            nargs = nargs + 1;
            a = a->next;
        }
        /* Now record args in h_carg, expanding 64-bit args to lo/hi pairs */
        carg_base = h_ncarg;
        phys_count = 0;
        i = 0;
        while (i < nargs) {
            if (h_ncarg >= HIR_MAX_CARG) p_error("too many call args (hir)");
            h_carg[h_ncarg] = av[i];
            h_ncarg = h_ncarg + 1;
            phys_count = phys_count + 1;
            if (is64[i]) {
                if (h_ncarg >= HIR_MAX_CARG) p_error("too many call args (hir)");
                h_carg[h_ncarg] = av_hi[i];
                h_ncarg = h_ncarg + 1;
                phys_count = phys_count + 1;
            }
            i = i + 1;
        }
        ret_ty = find_func_type(n->name);
        i = hi_emit(HI_CALL, TY_INT, -1, -1, phys_count, n->name);
        h_cbase[i] = carg_base;
        if (ty_is_llong(ret_ty)) {
            hl_hi = hi_emit(HI_CALLHI, TY_INT, i, -1, 0, NULL);
        }
        return i;
    }

    /* Function reference (bare function name as address) */
    if (n->kind == ND_FUNC_REF) {
        return hi_emit(HI_FADDR, TY_INT, -1, -1, 0, n->name);
    }

    /* Indirect call through expression */
    if (n->kind == ND_CALL_PTR) {
        int av[16];
        int av_hi2[16];
        int is64_2[16];
        int phys_count2;
        callee = hl_expr(n->lhs);
        a = n->args;
        nargs = 0;
        while (a) {
            if (nargs >= 16) p_error("too many call args (limit 16)");
            av[nargs] = hl_expr(a);
            if (ty_is_llong(a->ty)) {
                av_hi2[nargs] = hl_hi;
                is64_2[nargs] = 1;
            } else {
                av_hi2[nargs] = -1;
                is64_2[nargs] = 0;
            }
            nargs = nargs + 1;
            a = a->next;
        }
        carg_base = h_ncarg;
        phys_count2 = 0;
        i = 0;
        while (i < nargs) {
            if (h_ncarg >= HIR_MAX_CARG) p_error("too many call args (hir)");
            h_carg[h_ncarg] = av[i];
            h_ncarg = h_ncarg + 1;
            phys_count2 = phys_count2 + 1;
            if (is64_2[i]) {
                if (h_ncarg >= HIR_MAX_CARG) p_error("too many call args (hir)");
                h_carg[h_ncarg] = av_hi2[i];
                h_ncarg = h_ncarg + 1;
                phys_count2 = phys_count2 + 1;
            }
            i = i + 1;
        }
        i = hi_emit(HI_CALLP, TY_INT, callee, -1, phys_count2, NULL);
        h_cbase[i] = carg_base;
        return i;
    }

    p_error("unknown expression node (hir)");
    return -1;
}

/* --- Statement lowering --- */

static void hl_stmt(Node *n) {
    int cv;
    int lv;
    int cmp;
    int head_blk;
    int body_blk;
    int step_blk;
    int exit_blk;
    int then_blk;
    int else_blk;
    int end_blk;
    int brk_blk;
    int def_blk;
    int case_blk;
    int next_blk;
    int sw_d;
    int sw_b;
    int sw_n;
    int sw_i;
    int cd;
    int ci;
    int lbl_blk;
    int zero_inst;
    int lt_kind;
    int use_bs;
    Node *s;
    Node *cs;

    if (!n) return;

    /* Return */
    if (n->kind == ND_RETURN) {
        if (n->lhs) {
            lv = hl_expr(n->lhs);
            if (ty_is_llong(n->lhs->ty)) {
                hi_emit(HI_RET, 0, lv, hl_hi, 0, NULL);
            } else {
                hi_emit(HI_RET, 0, lv, -1, 0, NULL);
            }
        } else {
            hi_emit(HI_RET, 0, -1, -1, 0, NULL);
        }
        return;
    }

    /* If / If-else */
    if (n->kind == ND_IF) {
        cv = hl_expr(n->cond);
        if (n->els) {
            then_blk = hir_new_block();
            else_blk = hir_new_block();
            end_blk = hir_new_block();
            hi_emit(HI_BRC, 0, cv, then_blk, else_blk, NULL);

            hl_switch_block(then_blk);
            hl_stmt(n->body);
            if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, end_blk, NULL);

            hl_switch_block(else_blk);
            hl_stmt(n->els);
            if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, end_blk, NULL);

            hl_switch_block(end_blk);
        } else {
            then_blk = hir_new_block();
            end_blk = hir_new_block();
            hi_emit(HI_BRC, 0, cv, then_blk, end_blk, NULL);

            hl_switch_block(then_blk);
            hl_stmt(n->body);
            if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, end_blk, NULL);

            hl_switch_block(end_blk);
        }
        return;
    }

    /* While */
    if (n->kind == ND_WHILE) {
        head_blk = hir_new_block();
        body_blk = hir_new_block();
        exit_blk = hir_new_block();

        hi_emit(HI_BR, 0, -1, -1, head_blk, NULL);

        hl_switch_block(head_blk);
        cv = hl_expr(n->cond);
        hi_emit(HI_BRC, 0, cv, body_blk, exit_blk, NULL);

        hl_break_blk[hl_loop_depth] = exit_blk;
        hl_cont_blk[hl_loop_depth] = head_blk;
        hl_loop_depth = hl_loop_depth + 1;

        hl_switch_block(body_blk);
        hl_stmt(n->body);
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, head_blk, NULL);

        hl_loop_depth = hl_loop_depth - 1;
        hl_switch_block(exit_blk);
        return;
    }

    /* Do-while */
    if (n->kind == ND_DO_WHILE) {
        body_blk = hir_new_block();
        step_blk = hir_new_block();
        exit_blk = hir_new_block();

        hi_emit(HI_BR, 0, -1, -1, body_blk, NULL);

        hl_break_blk[hl_loop_depth] = exit_blk;
        hl_cont_blk[hl_loop_depth] = step_blk;
        hl_loop_depth = hl_loop_depth + 1;

        hl_switch_block(body_blk);
        hl_stmt(n->body);
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, step_blk, NULL);

        hl_switch_block(step_blk);
        cv = hl_expr(n->cond);
        hi_emit(HI_BRC, 0, cv, body_blk, exit_blk, NULL);

        hl_loop_depth = hl_loop_depth - 1;
        hl_switch_block(exit_blk);
        return;
    }

    /* For */
    if (n->kind == ND_FOR) {
        head_blk = hir_new_block();
        body_blk = hir_new_block();
        step_blk = hir_new_block();
        exit_blk = hir_new_block();

        if (n->init) hl_expr(n->init);
        hi_emit(HI_BR, 0, -1, -1, head_blk, NULL);

        hl_switch_block(head_blk);
        cv = hl_expr(n->cond);
        hi_emit(HI_BRC, 0, cv, body_blk, exit_blk, NULL);

        hl_break_blk[hl_loop_depth] = exit_blk;
        hl_cont_blk[hl_loop_depth] = step_blk;
        hl_loop_depth = hl_loop_depth + 1;

        hl_switch_block(body_blk);
        hl_stmt(n->body);
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, step_blk, NULL);

        hl_switch_block(step_blk);
        if (n->step) hl_expr(n->step);
        hi_emit(HI_BR, 0, -1, -1, head_blk, NULL);

        hl_loop_depth = hl_loop_depth - 1;
        hl_switch_block(exit_blk);
        return;
    }

    /* Switch */
    if (n->kind == ND_SWITCH) {
        sw_d = hl_sw_depth;
        sw_b = 0;
        if (sw_d > 0) {
            sw_b = hl_sw_base[sw_d - 1] + hl_sw_count[sw_d - 1];
        }
        hl_sw_base[sw_d] = sw_b;
        hl_sw_count[sw_d] = 0;
        hl_sw_def[sw_d] = -1;

        /* Pre-scan body for case/default, allocate blocks */
        if (n->body && n->body->kind == ND_BLOCK) {
            cs = n->body->body;
            while (cs) {
                if (cs->kind == ND_CASE) {
                    sw_n = hl_sw_count[sw_d];
                    hl_sw_val[sw_b + sw_n] = cs->val;
                    hl_sw_blk[sw_b + sw_n] = hir_new_block();
                    hl_sw_count[sw_d] = sw_n + 1;
                } else if (cs->kind == ND_DEFAULT) {
                    hl_sw_def[sw_d] = hir_new_block();
                }
                cs = cs->next;
            }
        }

        brk_blk = hir_new_block();
        if (hl_sw_def[sw_d] >= 0) {
            def_blk = hl_sw_def[sw_d];
        } else {
            def_blk = brk_blk;
        }

        /* Push break target */
        hl_break_blk[hl_loop_depth] = brk_blk;
        hl_loop_depth = hl_loop_depth + 1;

        /* Evaluate switch expression */
        lv = hl_expr(n->cond);

        sw_n = hl_sw_count[sw_d];
        if (n->cond && (n->cond->ty & TY_UNSIGNED)) lt_kind = HI_SLTU;
        else lt_kind = HI_SLT;

        /* First slice for issue #32:
           keep tiny switches as linear chain; lower larger ones to a
           balanced binary decision tree by case value. */
        use_bs = (sw_n >= 6);
        if (use_bs) {
            hl_sw_sort_cases(sw_b, sw_n, (lt_kind == HI_SLTU));
            hl_sw_emit_bsearch(lv, def_blk, lt_kind, sw_b, sw_b + sw_n - 1);
        } else {
            hl_sw_emit_chain(lv, def_blk, sw_b, sw_n);
        }

        /* Generate body with case cursor */
        hl_sw_cur[sw_d] = 0;
        hl_sw_depth = sw_d + 1;
        hl_stmt(n->body);
        hl_sw_depth = sw_d;

        /* Break label */
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, brk_blk, NULL);
        hl_switch_block(brk_blk);
        hl_loop_depth = hl_loop_depth - 1;
        return;
    }

    /* Case label */
    if (n->kind == ND_CASE) {
        cd = hl_sw_depth - 1;
        ci = hl_sw_cur[cd];
        case_blk = hl_sw_blk[hl_sw_base[cd] + ci];
        hl_sw_cur[cd] = ci + 1;
        /* Fall-through from previous block */
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, case_blk, NULL);
        hl_switch_block(case_blk);
        return;
    }

    /* Default label */
    if (n->kind == ND_DEFAULT) {
        cd = hl_sw_depth - 1;
        def_blk = hl_sw_def[cd];
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, def_blk, NULL);
        hl_switch_block(def_blk);
        return;
    }

    /* Break */
    if (n->kind == ND_BREAK) {
        if (hl_loop_depth < 1) p_error("break outside loop/switch");
        hi_emit(HI_BR, 0, -1, -1, hl_break_blk[hl_loop_depth - 1], NULL);
        return;
    }

    /* Continue */
    if (n->kind == ND_CONTINUE) {
        if (hl_loop_depth < 1) p_error("continue outside loop");
        hi_emit(HI_BR, 0, -1, -1, hl_cont_blk[hl_loop_depth - 1], NULL);
        return;
    }

    /* Goto */
    if (n->kind == ND_GOTO) {
        lbl_blk = hl_label_block(n->val);
        hi_emit(HI_BR, 0, -1, -1, lbl_blk, NULL);
        return;
    }

    /* Label */
    if (n->kind == ND_LABEL) {
        lbl_blk = hl_label_block(n->val);
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, lbl_blk, NULL);
        hl_switch_block(lbl_blk);
        if (n->body) hl_stmt(n->body);
        return;
    }

    /* Block */
    if (n->kind == ND_BLOCK) {
        s = n->body;
        while (s) {
            hl_stmt(s);
            s = s->next;
        }
        return;
    }

    /* Expression statement */
    if (n->kind == ND_EXPR_STMT) {
        if (n->lhs) hl_expr(n->lhs);
        return;
    }

    p_error("unknown statement node (hir)");
}

/* --- Number of params for SSA (shared with hir_ssa.h) --- */
static int hl_nparams;

/* --- Lower one function --- */

static void hl_func(Node *fn) {
    int entry;
    int pi;
    int param_inst;
    int param_alloca;

    hir_reset();
    hl_nalloca = 0;
    hl_nparams = fn->nparams;  /* will be updated by param expansion for llong */
    hl_hi = -1;
    hl_temp_stack = fn->locals_size;
    hl_loop_depth = 0;
    hl_sw_depth = 0;
    hl_ngoto = 0;

    entry = hir_new_block();
    hl_switch_block(entry);

    /* Emit PARAM+STORE for each parameter (enables SSA promotion) */
    {
        int phys_idx;
        int param_hi;
        int a4;
        Node *pp;
        phys_idx = 0;
        pp = fn->args;
        while (pp) {
            if (ty_is_llong(pp->ty)) {
                param_inst = hi_emit(HI_PARAM, TY_INT, -1, -1, phys_idx, NULL);
                param_hi = hi_emit(HI_PARAM, TY_INT, -1, -1, phys_idx + 1, NULL);
                param_alloca = hl_get_alloca(pp->offset, TY_INT);
                hi_emit(HI_STORE, TY_INT, param_alloca, param_inst, 0, NULL);
                a4 = hi_emit(HI_ADDI, TY_INT, param_alloca, -1, 4, NULL);
                hi_emit(HI_STORE, TY_INT, a4, param_hi, 0, NULL);
                phys_idx = phys_idx + 2;
            } else {
                param_inst = hi_emit(HI_PARAM, TY_INT, -1, -1, phys_idx, NULL);
                param_alloca = hl_get_alloca(pp->offset, TY_INT);
                hi_emit(HI_STORE, TY_INT, param_alloca, param_inst, 0, NULL);
                phys_idx = phys_idx + 1;
            }
            pp = pp->next;
        }
        hl_nparams = phys_idx;
    }

    hl_stmt(fn->body);

    /* Ensure function ends with a return */
    if (!hl_terminated()) {
        hi_emit(HI_RET, 0, -1, -1, 0, NULL);
    }
}
