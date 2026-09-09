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

/* How a value is handed across a call boundary.  Only the coarse class
 * matters: 1 (long long) occupies a register PAIR, everything else in
 * class 0 occupies one. */
static int sema_arg_class(int ty) {
    if (ty_is_struct(ty)) return 4;
    if (ty_is_ptr(ty)) return 0;
    if (ty_is_double(ty)) return 2;
    if (ty_is_float(ty)) return 3;
    if (ty_is_llong(ty)) return 1;
    return 0;
}

static int sema_arith_type(int lty, int rty) {
    if (ty_is_ptr(lty)) return lty;
    if (ty_is_ptr(rty)) return rty;
    /* Float promotion: double > float > long long > int */
    if (ty_is_double(lty) || ty_is_double(rty)) return TY_DOUBLE;
    if (ty_is_float(lty) || ty_is_float(rty)) return TY_FLOAT;
    if (ty_is_llong(lty) || ty_is_llong(rty)) {
        if ((lty & TY_UNSIGNED) || (rty & TY_UNSIGNED))
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

/* --- Expression walk --- */

static void sema_expr(Node *n) {
    Node *a;
    int lty;
    int rty;

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
            /* Comparisons: result is int, but mark unsigned if either operand is */
            n->ty = TY_INT;
            if ((lty & TY_UNSIGNED) || (rty & TY_UNSIGNED))
                n->ty = TY_INT | TY_UNSIGNED;
        } else if (n->op == TK_LAND || n->op == TK_LOR) {
            n->ty = TY_INT;
        } else if (n->op == TK_LSHIFT || n->op == TK_RSHIFT) {
            /* Shift: signedness from LHS */
            n->ty = lty;
        } else {
            /* Arithmetic/bitwise: propagate unsigned */
            n->ty = sema_arith_type(lty, rty);
        }
        return;
    }

    if (n->kind == ND_UNARY) {
        if (n->op == TK_MINUS || n->op == TK_TILDE) {
            n->ty = n->lhs ? n->lhs->ty : TY_INT;
        }
        /* !, *, & already typed correctly by parser */
        return;
    }

    if (n->kind == ND_ASSIGN) {
        n->ty = n->lhs ? n->lhs->ty : TY_INT;
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

    /* ND_CALL: typed by parser via find_func_type() */
    /* ND_CALL_PTR: stays TY_INT (no return type info for indirect calls) */
    /* ND_CAST: preserve parser-assigned type */
    /* ND_MEMBER: preserve member type */
}

/* --- Statement walk --- */

static void sema_stmt(Node *n) {
    Node *s;

    if (!n) return;

    if (n->kind == ND_RETURN) {
        sema_expr(n->lhs);
        /* The value's marshalling class must be the function's: a 32-bit
         * value returned from a long long function is otherwise handed
         * back with the pair's high register untouched.  This is stage08's
         * GitHub issue 13 fix, ported back -- stage07 still builds gen1
         * and the libc, and dtoa.c returns narrower expressions from long
         * long functions, so the bug was live in the bootstrap.  It also
         * blocked widening the #if evaluator (GitHub issue 60), which is
         * how it was found.
         *
         * The argument-side twin (issue 6) is NOT ported: it needs the
         * parser to record parameter types, which this registry does not
         * do.  stage08's sources are int-only C, so no call in them
         * passes a class-1 value. */
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
