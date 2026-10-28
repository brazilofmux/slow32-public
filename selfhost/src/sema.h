/* sema.h -- Semantic analysis pass for stage04 compiler
 *
 * Phase 13: single tree walk (L-attributed grammar).
 * Propagates unsigned type info, validates + fixes expression types.
 * Included between parser.h and optimize.h.
 * Compiled by stage03 s32-cc.
 */

/* --- State --- */
static int sema_ret_ty;   /* current function's return type */

/* --- Helpers --- */

/* The integer promotions (C90 6.2.1.1): a char or short operand, signed
 * or unsigned, is an int in any arithmetic, comparison, shift or unary
 * minus -- so unsigned char - 48 is a signed int, and a ternary of it
 * against -1 compares signed.  Issue 82: the unsigned flag of an
 * unsigned char went straight into the result, and (c >= '0' && c <= '9'
 * ? c - '0' : -1) < 0 was an unsigned compare, never true. */
static int sema_promote(int ty) {
    int base;
    if (ty_is_ptr(ty)) return ty;
    base = ty & TY_BASE_MASK;
    if (base == TY_CHAR || base == TY_SHORT) return TY_INT;
    return ty;
}

static int sema_arith_type(int lty, int rty) {
    if (ty_is_ptr(lty)) return lty;
    if (ty_is_ptr(rty)) return rty;
    lty = sema_promote(lty);
    rty = sema_promote(rty);
    /* Float promotion: double > float > long long > int */
    if (ty_is_double(lty) || ty_is_double(rty)) return TY_DOUBLE;
    if (ty_is_float(lty) || ty_is_float(rty)) return TY_FLOAT;
    /* C99 6.3.1.8: the result is unsigned only when an unsigned operand
     * has the common rank.  An unsigned int beside a long long widens
     * to long long, which holds every unsigned int; "unsigned if either
     * side is" made SQLite's SMALLEST_INT64 unsigned and sent every
     * integer multiply through floating point (selfhost ISSUES-95). */
    if (ty_is_llong(lty) || ty_is_llong(rty)) {
        if (ty_common_unsigned(lty, rty))
            return TY_LLONG | TY_UNSIGNED;
        return TY_LLONG;
    }
    if ((lty & TY_UNSIGNED) || (rty & TY_UNSIGNED))
        return TY_INT | TY_UNSIGNED;
    return TY_INT;
}

static int sema_is_cmp(int op) {
    if (op == TK_EQ) return 1;
    if (op == TK_NE) return 1;
    if (op == TK_LT) return 1;
    if (op == TK_GT) return 1;
    if (op == TK_LE) return 1;
    if (op == TK_GE) return 1;
    return 0;
}

/* Marshalling class of a type: 0=word (int/char/short/ptr), 1=llong,
 * 2=double, 3=float, 4=struct.  Distinct classes occupy different
 * numbers/kinds of argument slots, so a class mismatch between the
 * argument expression and the declared parameter is a silent
 * miscompile unless we insert the implicit conversion (issue #6:
 * int passed to a long long parameter left the high word stale). */
static int sema_arg_class(int ty) {
    if (ty_is_struct(ty)) return 4;
    if (ty_is_ptr(ty)) return 0;
    if (ty_is_double(ty)) return 2;
    if (ty_is_float(ty)) return 3;
    if (ty_is_llong(ty)) return 1;
    return 0;
}

/* --- Expression walk --- */

/* sema_expr: each node's machine type and conversions (sema_expr_mty,
 * the walk as it was), then its C type (sema_cty_node, docs/TYPES.md).
 * The recursion goes through here, so every node gets both, children
 * first. */
static void sema_expr(Node *n);
static void sema_cty_node(Node *n);

static void sema_expr_mty(Node *n) {
    Node *a;
    Node *prev;
    Node *cast;
    int lty;
    int rty;
    int idx;
    int pt;
    int fpbase;
    int fpn;

    if (!n) return;

    /* Recurse into children first (post-order) */
    sema_expr(n->lhs);
    sema_expr(n->rhs);
    sema_expr(n->cond);

    /* Recurse into argument lists */
    a = n->args;
    while (a) {
        sema_expr(a);
        a = a->next;
    }

    /* ND_NUM, ND_VAR, ND_STRING, ND_FUNC_REF: already typed by parser */

    if (n->kind == ND_BINOP) {
        lty = n->lhs ? n->lhs->ty : TY_INT;
        rty = n->rhs ? n->rhs->ty : TY_INT;

        if (sema_is_cmp(n->op)) {
            /* Comparisons: result is int; the unsigned bit is what the
             * lowering reads for sltu against slt, and it follows the
             * operands' common type: -1LL < 0xffffffffU compares as
             * long long, signed.  Pointers keep the old rule. */
            n->ty = TY_INT;
            if (ty_is_ptr(lty) || ty_is_ptr(rty)) {
                if ((sema_promote(lty) & TY_UNSIGNED) || (sema_promote(rty) & TY_UNSIGNED))
                    n->ty = TY_INT | TY_UNSIGNED;
            } else if (sema_arith_type(lty, rty) & TY_UNSIGNED) {
                n->ty = TY_INT | TY_UNSIGNED;
            }
        } else if (n->op == TK_LAND || n->op == TK_LOR) {
            n->ty = TY_INT;
        } else if (n->op == TK_LSHIFT || n->op == TK_RSHIFT) {
            /* Shift: signedness from LHS, promoted */
            n->ty = sema_promote(lty);
        } else {
            /* Arithmetic/bitwise: propagate unsigned */
            n->ty = sema_arith_type(lty, rty);
        }
        return;
    }

    if (n->kind == ND_UNARY) {
        if (n->op == TK_MINUS || n->op == TK_TILDE) {
            n->ty = n->lhs ? sema_promote(n->lhs->ty) : TY_INT;
        }
        /* !, *, & already typed correctly by parser */
        return;
    }

    if (n->kind == ND_ASSIGN) {
        n->ty = n->lhs ? n->lhs->ty : TY_INT;
        /* Across the integer/floating-point line, or between float and
         * double, the stored value is the converted one: the cast
         * lowering does it.  Without this `u = d` stored the double's
         * low word and `f = u` the integer's bits (DBT-20). */
        if (n->lhs && n->rhs) {
            lty = sema_arg_class(n->lhs->ty);
            rty = sema_arg_class(n->rhs->ty);
            if (lty != rty && lty != 4 && rty != 4 &&
                (lty >= 2 || rty >= 2))
                n->rhs = nd_cast(n->rhs, n->lhs->ty);
        }
        return;
    }

    if (n->kind == ND_COMP_ASSIGN) {
        n->ty = n->lhs ? n->lhs->ty : TY_INT;
        return;
    }

    if (n->kind == ND_POST_INC || n->kind == ND_POST_DEC) {
        n->ty = n->lhs ? n->lhs->ty : TY_INT;
        return;
    }

    if (n->kind == ND_TERNARY) {
        lty = n->lhs ? n->lhs->ty : TY_INT;
        rty = n->rhs ? n->rhs->ty : TY_INT;
        n->ty = sema_arith_type(lty, rty);
        return;
    }

    if (n->kind == ND_COMMA) {
        n->ty = n->rhs ? n->rhs->ty : TY_INT;
        return;
    }

    /* ND_CALL: return type set by parser via find_func_type().  Here we
     * apply the implicit argument conversions the parser cannot: it may
     * not have seen the prototype yet at the call site, but by sema time
     * every prototype is registered.  An argument whose marshalling
     * class differs from the declared parameter's is wrapped in a cast
     * so the existing cast lowering (sign/zero extension, fp<->int,
     * truncation) produces a correctly-classed value in every backend. */
    if (n->kind == ND_CALL) {
        idx = 0;
        prev = NULL;
        a = n->args;
        while (a) {
            pt = find_func_param(n->name, idx);
            /* No declared parameter -- a variadic function's `...`, or a
             * callee without a prototype: the default argument promotions
             * (C90 6.3.2.2) make a float a double.  Without this, a
             * float handed to printf arrived as its 32-bit pattern where
             * va_arg(ap, double) reads a pair. */
            if (pt < 0 && ty_is_float(a->ty)) pt = TY_DOUBLE;
            if (pt >= 0 &&
                sema_arg_class(a->ty) != sema_arg_class(pt) &&
                sema_arg_class(a->ty) != 4 && sema_arg_class(pt) != 4) {
                cast = nd_cast(a, pt);
                cast->next = a->next;
                a->next = NULL;
                if (prev) prev->next = cast;
                else n->args = cast;
                a = cast;
            }
            prev = a;
            idx = idx + 1;
            a = a->next;
        }
        return;
    }
    /* ND_CALL_PTR: convert args from a recorded signature (fpbase/nparams).
     * Members store those on the ND_MEMBER node; locals/typedefs/globals
     * hang them on the side tables (GitHub issue 41). */
    if (n->kind == ND_CALL_PTR && n->lhs) {
        fpbase = -1;
        fpn = 0;
        if (n->lhs->kind == ND_MEMBER && n->lhs->offset >= 0) {
            fpbase = n->lhs->offset;
            fpn = n->lhs->nparams;
        } else if (n->lhs->is_fnptr) {
            fpbase = n->lhs->val;
            fpn = n->lhs->nparams;
            if (n->lhs->kind == ND_CALL) fpn = n->lhs->val_hi;
        }
        if (fpbase >= 0) {
            n->is_varargs = ps_fpvar[fpbase];
            idx = 0;
            prev = NULL;
            a = n->args;
            while (a) {
                pt = (idx < fpn) ? ps_fptypes[fpbase + idx] : -1;
                if (pt < 0 && ty_is_float(a->ty)) pt = TY_DOUBLE;
                if (pt >= 0 &&
                    sema_arg_class(a->ty) != sema_arg_class(pt) &&
                    sema_arg_class(a->ty) != 4 && sema_arg_class(pt) != 4) {
                    cast = nd_cast(a, pt);
                    cast->next = a->next;
                    a->next = NULL;
                    if (prev) prev->next = cast;
                    else n->args = cast;
                    a = cast;
                }
                prev = a;
                idx = idx + 1;
                a = a->next;
            }
            n->ty = ps_fp_get_ret(fpbase);
            return;
        }
    }
    /* ND_CALL_PTR otherwise: stays TY_INT (no return type info) */
    /* ND_CAST: preserve parser-assigned type */
    /* ND_MEMBER: preserve member type */
}

/* --- C types: the attribute grammar's cty (docs/TYPES.md S2) --- */

static int sema_cty_check = -1;

/* A child's C type: its own, or one rebuilt from its machine type where
 * nothing has given it one (a node sema made, a kind without a rule). */
static int sema_cty_of(Node *c) {
    if (!c) return ct_basic[CT_INT];
    if (c->cty > 0) return c->cty;
    return ct_from_mty(c->ty, 0);
}

/* S12CC_CTY_CHECK=1: report where the C type sema derives maps to a
 * machine type other than the one the node carries -- where the old
 * typing departs from C, kind and operator named. */
static void sema_cty_report(Node *n) {
    if (sema_cty_check < 0) sema_cty_check = getenv("S12CC_CTY_CHECK") != 0;
    if (!sema_cty_check || ct_mty(n->cty) == n->ty) return;
    fdputs("cty-mismatch kind=", 2);
    fdputuint(2, (unsigned)n->kind);
    fdputs(" op=", 2);
    fdputuint(2, (unsigned)n->op);
    fdputs(" ty=", 2);
    fdputuint(2, (unsigned)n->ty);
    fdputs(" c-says=", 2);
    fdputuint(2, (unsigned)ct_mty(n->cty));
    if (n->name) { fdputs(" name=", 2); fdputs(n->name, 2); }
    if (n->lhs) {
        fdputs(" L=", 2); fdputuint(2, (unsigned)n->lhs->kind);
        fdputs("/", 2); fdputuint(2, (unsigned)n->lhs->ty);
        fdputs("/", 2); fdputuint(2, (unsigned)ct_kindof(sema_cty_of(n->lhs)));
    }
    if (n->rhs) {
        fdputs(" R=", 2); fdputuint(2, (unsigned)n->rhs->kind);
        fdputs("/", 2); fdputuint(2, (unsigned)n->rhs->ty);
        fdputs("/", 2); fdputuint(2, (unsigned)ct_kindof(sema_cty_of(n->rhs)));
    }
    fdputs("\n", 2);
}

/* The C type of an operator's result, from its operands' (6.5).  Kinds
 * without a rule yet keep a type rebuilt from the machine type. */
static void sema_cty_node(Node *n) {
    int l;
    int r;
    if (n->cty > 0 && n->kind == ND_VAR) { sema_cty_report(n); return; }
    if (n->kind == ND_STRING) {
        n->cty = ct_array(ct_basic[CT_CHAR], lex_str_len[n->val] + 1);
    } else if (n->kind == ND_FUNC_REF) {
        n->cty = ct_func(ct_from_mty(find_func_type(n->name), 0), 0, -1, 0);
    } else if (n->kind == ND_BINOP) {
        l = ct_decay(sema_cty_of(n->lhs));
        r = ct_decay(sema_cty_of(n->rhs));
        if (sema_is_cmp(n->op) || n->op == TK_LAND || n->op == TK_LOR) {
            n->cty = ct_basic[CT_INT];
        } else if (n->op == TK_LSHIFT || n->op == TK_RSHIFT) {
            n->cty = ct_promote(l);
        } else if ((n->op == TK_PLUS || n->op == TK_MINUS) && ct_is_ptr(l) && ct_is_ptr(r)) {
            n->cty = ct_basic[ty_ptr_size == 8 ? CT_LONG : CT_INT];   /* ptrdiff_t */
        } else if ((n->op == TK_PLUS || n->op == TK_MINUS) && ct_is_ptr(l)) {
            n->cty = l;
        } else if (n->op == TK_PLUS && ct_is_ptr(r)) {
            n->cty = r;
        } else if (ct_is_arith(l) && ct_is_arith(r)) {
            n->cty = ct_usual(l, r);
        } else {
            n->cty = ct_from_mty(n->ty, 0);
            return;
        }
    } else if (n->kind == ND_UNARY) {
        l = sema_cty_of(n->lhs);
        if (n->op == TK_MINUS || n->op == TK_TILDE) n->cty = ct_promote(ct_decay(l));
        else if (n->op == TK_BANG) n->cty = ct_basic[CT_INT];
        else if (n->op == TK_AMP) n->cty = ct_ptr(l);
        else if (n->op == TK_STAR && ct_is_ptr(ct_decay(l))) n->cty = ct_baseof(ct_decay(l));
        else { n->cty = ct_from_mty(n->ty, 0); return; }
    } else if (n->kind == ND_ASSIGN || n->kind == ND_COMP_ASSIGN ||
               n->kind == ND_POST_INC || n->kind == ND_POST_DEC) {
        n->cty = ct_unqual(sema_cty_of(n->lhs));
    } else if (n->kind == ND_COMMA) {
        n->cty = ct_decay(sema_cty_of(n->rhs));
    } else if (n->kind == ND_TERNARY) {
        l = ct_decay(sema_cty_of(n->lhs));
        r = ct_decay(sema_cty_of(n->rhs));
        if (ct_is_arith(l) && ct_is_arith(r)) n->cty = ct_usual(l, r);
        else if (l == r) n->cty = l;
        else { n->cty = ct_from_mty(n->ty, 0); return; }
    } else {
        if (n->cty <= 0) n->cty = ct_from_mty(n->ty, 0);
        return;
    }
    sema_cty_report(n);
}

static void sema_expr(Node *n) {
    if (!n) return;
    sema_expr_mty(n);
    sema_cty_node(n);
}

/* --- Statement walk --- */

static void sema_stmt(Node *n) {
    Node *s;

    if (!n) return;

    if (n->kind == ND_RETURN) {
        sema_expr(n->lhs);
        /* The value's marshalling class must be the function's: a 32-bit
         * value returned from a long long function is otherwise handed
         * back with the pair's high register untouched (GitHub #13, the
         * return-side twin of #6).  The cast lowering extends it. */
        if (n->lhs && sema_ret_ty >= 0 &&
            sema_arg_class(n->lhs->ty) != sema_arg_class(sema_ret_ty) &&
            sema_arg_class(n->lhs->ty) != 4 && sema_arg_class(sema_ret_ty) != 4) {
            n->lhs = nd_cast(n->lhs, sema_ret_ty);
        }
        return;
    }

    if (n->kind == ND_IF) {
        sema_expr(n->cond);
        sema_stmt(n->body);
        sema_stmt(n->els);
        return;
    }

    if (n->kind == ND_WHILE || n->kind == ND_DO_WHILE) {
        sema_expr(n->cond);
        sema_stmt(n->body);
        return;
    }

    if (n->kind == ND_FOR) {
        sema_expr(n->init);
        sema_expr(n->cond);
        sema_expr(n->step);
        sema_stmt(n->body);
        return;
    }

    if (n->kind == ND_BLOCK) {
        s = n->body;
        while (s) {
            sema_stmt(s);
            s = s->next;
        }
        return;
    }

    if (n->kind == ND_EXPR_STMT) {
        sema_expr(n->lhs);
        return;
    }

    if (n->kind == ND_SWITCH) {
        sema_expr(n->cond);
        sema_stmt(n->body);
        return;
    }

    if (n->kind == ND_CASE || n->kind == ND_DEFAULT) {
        /* These are statement-level labels; nothing to walk */
        return;
    }

    if (n->kind == ND_LABEL) {
        sema_stmt(n->body);
        return;
    }

    /* ND_GOTO, ND_BREAK, ND_CONTINUE: nothing */
}

/* --- Entry point --- */

static void sema(Node *prog) {
    Node *fn;
    fn = prog->body;
    while (fn) {
        if (fn->kind == ND_FUNC) {
            sema_ret_ty = fn->ty;
            sema_stmt(fn->body);
        }
        fn = fn->next;
    }
}
