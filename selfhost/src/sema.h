/* sema.h -- Semantic analysis pass for stage04 compiler
 *
 * Phase 13: single tree walk (L-attributed grammar).
 * Propagates unsigned type info, validates + fixes expression types.
 * Included between parser.h and optimize.h.
 * Compiled by stage03 s32-cc.
 */

/* --- State --- */
static int sema_ret_ty;   /* current function's return type */
static int sema_ret_cty;  /* ... its C type (0: unknown) */

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
static void sema_attr_node(Node *n);
static void sema_constraints(Node *n);
static int sema_compat(int a, int b);
static int sema_callee_cty(Node *n);
static void sema_call_check(Node *n, int ft);
static void sema_stmt(Node *n);

/* Is C type t _Bool? */
static int sema_is_bool(int t) {
    return t > 0 && ct_kindof(ct_unqual(t)) == CT_BOOL;
}

/* e converted to _Bool: 1 when it compares unequal to 0, else 0
 * (6.3.1.2), written as the C it means -- e != 0 -- which the lowering
 * handles for every scalar type.  _Bool had been unsigned char, and the
 * conversion a truncation: 256 was 0, 0.5 was 0 (W2). */
static Node *sema_to_bool(Node *e) {
    Node *n;
    if (!e || sema_is_bool(e->cty)) return e;
    n = nd_binop(TK_NE, e, nd_num(0));
    n->ty = TY_INT;
    n->cty = ct_basic[CT_INT];
    n->next = e->next;
    e->next = NULL;
    n->rhs->cty = ct_basic[CT_INT];
    sema_attr_node(n->rhs);
    sema_attr_node(n);
    return n;
}

/* e as an operand: a bit-field promoted by its width (6.3.1.1p2,
 * nd_bf_promoted), written as a conversion the cast lowering performs.
 * It promoted by its declared type, so an unsigned :4 field stayed
 * unsigned and x.u - 2 < 0 was never true (W12). */
static Node *sema_bf_operand(Node *e) {
    Node *n;
    int ty;
    ty = nd_bf_promoted(e);
    if (ty < 0) return e;
    n = nd_cast(e, ty);
    n->cty = ct_from_mty(ty, 0);
    n->next = e->next;
    e->next = NULL;
    sema_attr_node(n);
    return n;
}

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

    /* Recurse into children first (post-order).  A GNU statement
     * expression's statements are children too: without them every
     * conversion sema makes (a call's arguments to its prototype, an
     * assignment's right side) was missing inside ({ ... }), and
     * ({ t = f(3); t; }) handed a double parameter the integer 3. */
    if (n->kind == ND_STMT_EXPR) sema_stmt(n->body);
    sema_expr(n->lhs);
    sema_expr(n->rhs);
    sema_expr(n->cond);

    /* Recurse into argument lists */
    a = n->args;
    while (a) {
        sema_expr(a);
        a = a->next;
    }

    /* Its operands typed, the node's constraints -- before the
     * conversions below hide the operands' own types in casts. */
    sema_constraints(n);
    if (n->kind == ND_CALL || n->kind == ND_CALL_PTR) sema_call_check(n, sema_callee_cty(n));

    /* ND_NUM, ND_VAR, ND_STRING, ND_FUNC_REF: already typed by parser */

    if (n->kind == ND_BINOP) {
        n->lhs = sema_bf_operand(n->lhs);
        n->rhs = sema_bf_operand(n->rhs);
        lty = n->lhs ? n->lhs->ty : TY_INT;
        rty = n->rhs ? n->rhs->ty : TY_INT;

        if (sema_is_cmp(n->op)) {
            /* Comparisons: result is int; the unsigned bit is what the
             * lowering reads for sltu against slt, and it follows the
             * operands' common type: -1LL < 0xffffffffU compares as
             * long long, signed.  Pointers keep the old rule. */
            /* The result is int (6.5.8p6, 6.5.9p3).  Whether the
             * operands compare unsigned is the operation's, not the
             * value's: it was the result's unsigned bit, and leaked --
             * n + (u < v) became unsigned arithmetic (docs/TYPES.md). */
            n->ty = TY_INT;
            if (ty_is_ptr(lty) || ty_is_ptr(rty)) {
                if ((sema_promote(lty) & TY_UNSIGNED) || (sema_promote(rty) & TY_UNSIGNED))
                    n->attr = n->attr | AT_UCMP;
            } else if (sema_arith_type(lty, rty) & TY_UNSIGNED) {
                n->attr = n->attr | AT_UCMP;
            }
        } else if (n->op == TK_LAND || n->op == TK_LOR) {
            n->ty = TY_INT;
        } else if (n->op == TK_LSHIFT || n->op == TK_RSHIFT) {
            /* Shift: signedness from LHS, promoted */
            n->ty = sema_promote(lty);
        } else if (n->op == TK_MINUS && ty_is_ptr(lty) && ty_is_ptr(rty)) {
            /* p - q is ptrdiff_t, signed and as wide as a pointer
             * (6.5.6p9); it was typed as the pointer, so (p - q) / 2 on
             * u8 * divided unsigned and (p - q) < 0 was never true */
            n->ty = ty_ptr_size == 8 ? TY_LLONG : TY_INT;
        } else {
            /* Arithmetic/bitwise: propagate unsigned */
            n->ty = sema_arith_type(lty, rty);
        }
        return;
    }

    if (n->kind == ND_UNARY) {
        if (n->op == TK_MINUS || n->op == TK_TILDE) {
            n->lhs = sema_bf_operand(n->lhs);
            n->ty = n->lhs ? sema_promote(n->lhs->ty) : TY_INT;
        }
        /* !, *, & already typed correctly by parser */
        return;
    }

    if (n->kind == ND_ASSIGN) {
        n->ty = n->lhs ? n->lhs->ty : TY_INT;
        if (n->lhs && sema_is_bool(n->lhs->cty)) n->rhs = sema_to_bool(n->rhs);
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
        n->lhs = sema_bf_operand(n->lhs);
        n->rhs = sema_bf_operand(n->rhs);
        lty = n->lhs ? n->lhs->ty : TY_INT;
        rty = n->rhs ? n->rhs->ty : TY_INT;
        /* two structures: the result is that structure (6.5.15p5), not
         * an arithmetic type -- c ? s1 : s2 became an int, so a struct
         * return gave back an address and no copy, and a by-value
         * argument passed one word */
        if (ty_is_struct(lty) && lty == rty) n->ty = lty;
        else n->ty = sema_arith_type(lty, rty);
        return;
    }

    if (n->kind == ND_COMMA) {
        n->ty = n->rhs ? n->rhs->ty : TY_INT;
        return;
    }

    if (n->kind == ND_CAST) {
        if (sema_is_bool(n->cty) && n->lhs) n->lhs = sema_to_bool(n->lhs);
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
            if (sema_is_bool(find_func_param_cty(n->name, idx))) {
                cast = sema_to_bool(a);
                if (prev) prev->next = cast;
                else n->args = cast;
                a = cast;
            }
            pt = find_func_param(n->name, idx);
            /* No declared parameter -- a variadic function's `...`, or a
             * callee without a prototype: the default argument promotions
             * (C90 6.3.2.2) make a float a double.  Without this, a
             * float handed to printf arrived as its 32-bit pattern where
             * va_arg(ap, double) reads a pair. */
            if (pt < 0 && ty_is_float(a->ty)) pt = TY_DOUBLE;
            if (pt < 0) pt = nd_bf_promoted(a);    /* likewise the integer promotions */
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
            if (pt < 0) pt = nd_bf_promoted(a);    /* likewise the integer promotions */
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
    if (!sema_cty_check || ct_mty_agrees(n->cty, n->ty)) return;
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

/* S12CC_CTY_CHECK=1: a node whose C type nothing gave, rebuilt from its
 * machine type -- the measure of how far S2 has to go. */
static void sema_cty_rebuilt(Node *n) {
    if (sema_cty_check < 0) sema_cty_check = getenv("S12CC_CTY_CHECK") != 0;
    if (!sema_cty_check) return;
    fdputs("cty-rebuilt kind=", 2);
    fdputuint(2, (unsigned)n->kind);
    fdputs("\n", 2);
}

/* The C type of an operator's result, from its operands' (6.5).  Kinds
 * without a rule yet keep a type rebuilt from the machine type. */
/* *p's machine type, from its operand's as sema leaves it: the parser
 * typed *p from p as it stood, and sema may retype p -- (*q)[j] is
 * *((*q) + j), whose sum the parser saw as an int.  When *p is an array
 * (p points to one: int (*q)[3], a 2-D array decayed, a jmp_buf *) its
 * value is the array decayed -- the address p holds -- typed as the C
 * type says, the machine types of those three spelling it differently. */
static void sema_deref_mty(Node *n) {
    if (!n->lhs || !ty_is_ptr(n->lhs->ty)) return;
    if (ct_is_array(n->cty)) n->ty = ct_mty(ct_decay(n->cty));
    else n->ty = ty_deref(n->lhs->ty);
}

static void sema_cty_node(Node *n) {
    int l;
    int r;
    if (n->cty > 0 && n->kind == ND_VAR) { sema_cty_report(n); return; }
    if (n->kind == ND_STRING) {
        /* an array of its elements: char, or a wide string's units */
        if (lex_str_width[n->val] == 4)
            n->cty = ct_array(ct_basic[CT_UINT], (lex_str_len[n->val] + 1) / 4);
        else if (lex_str_width[n->val] == 2)
            n->cty = ct_array(ct_basic[CT_USHORT], (lex_str_len[n->val] + 1) / 2);
        else
            n->cty = ct_array(ct_basic[CT_CHAR], lex_str_len[n->val] + 1);
    } else if (n->kind == ND_FUNC_REF) {
        n->cty = find_func_cty(n->name);
        if (n->cty <= 0) n->cty = ct_func(ct_from_mty(find_func_type(n->name), 0), 0, -1, 0);
    } else if (n->kind == ND_CALL && find_func_cty(n->name) > 0) {
        /* the callee's return type (6.5.2.2p5) */
        n->cty = ct_unqual(ct_baseof(find_func_cty(n->name)));
    } else if (n->kind == ND_CALL_PTR && ct_is_ptr(ct_decay(sema_cty_of(n->lhs))) &&
               ct_is_func(ct_baseof(ct_decay(sema_cty_of(n->lhs))))) {
        n->cty = ct_unqual(ct_baseof(ct_baseof(ct_decay(sema_cty_of(n->lhs)))));
    } else if (n->kind == ND_BINOP && n->op == TK_PLUS && n->arr_cols > 0 &&
               (ct_is_array(sema_cty_of(n->lhs)) || ct_is_ptr(sema_cty_of(n->lhs)))) {
        /* the parser's row of a 2-D subscript, a[i] or p[i] on a pointer
         * to an array: written as the row's address, typed as the row
         * itself (6.5.2.1p3), arr_cols of the innermost element -- the
         * declaration may hold the array flattened, the width beside it */
        l = ct_baseof(sema_cty_of(n->lhs));
        while (ct_is_array(l)) l = ct_baseof(l);
        n->cty = ps_rows_cty(l, n->arr_cols, n->arr_sub);
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
        else if (n->op == TK_STAR && ct_is_ptr(ct_decay(l))) {
            n->cty = ct_baseof(ct_decay(l));
            sema_deref_mty(n);
        }
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
        /* pointer arms (6.5.15p6): a null pointer constant takes the
         * other's type, void * wins, compatible ones are one type */
        else if (ct_is_ptr(l) && (n->rhs->attr & AT_NPC)) n->cty = l;
        else if (ct_is_ptr(r) && (n->lhs->attr & AT_NPC)) n->cty = r;
        else if (ct_is_ptr(l) && ct_is_ptr(r) && ct_kindof(ct_baseof(l)) == CT_VOID) n->cty = l;
        else if (ct_is_ptr(l) && ct_is_ptr(r) && ct_kindof(ct_baseof(r)) == CT_VOID) n->cty = r;
        else if (ct_is_ptr(l) && ct_is_ptr(r) && sema_compat(ct_unqual(ct_baseof(l)), ct_unqual(ct_baseof(r))))
            n->cty = ct_nparms(ct_baseof(l)) < 0 ? r : l;   /* the one with a prototype */
        else { n->cty = ct_from_mty(n->ty, 0); return; }
    } else if (n->kind == ND_MEMBER && n->cty > 0) {
        /* a member of a qualified structure is qualified too (6.5.2.3p3) */
        n->cty = ct_qualify(n->cty, ct_qual(sema_cty_of(n->lhs)));
    } else if (n->kind == ND_NUM || n->kind == ND_FNUM) {
        /* a constant's type is the lexer's, from its suffix and value
         * (6.4.4.1p5, 6.4.4.2p4): the machine type says it, but for a
         * long, which the parser names (its machine type is int's) */
        if (!(n->kind == ND_NUM && n->cty > 0 && ct_mty_agrees(n->cty, n->ty)))
            n->cty = ct_from_mty(n->ty, 0);
        return;
    } else if (n->cty > 0) {
        /* the parser gave it one: a cast's named type */
    } else {
        n->cty = ct_from_mty(n->ty, 0);
        sema_cty_rebuilt(n);
        return;
    }
    sema_cty_report(n);
}

/* --- The other attributes (docs/TYPES.md S2) --- */


/* Does struct or union type t have a const-qualified member, at any
 * depth (6.3.2.1p1: then it is not a modifiable lvalue)? */
static int sema_has_const_member(int t) {
    int si;
    int mi;
    if (!ct_is_record(t)) return 0;
    si = ct_countof(t);
    mi = 0;
    while (mi < stm_count) {
        if (stm_owner[mi] == si && stm_cty[mi] > 0) {
            if (ct_qual(stm_cty[mi]) & CQ_CONST) return 1;
            if (ct_is_record(stm_cty[mi]) && ct_countof(stm_cty[mi]) != si &&
                sema_has_const_member(stm_cty[mi])) return 1;
        }
        mi = mi + 1;
    }
    return 0;
}

/* v converted to integer type t (6.3.1.3): kept modulo 2^N when t is
 * unsigned; for a signed t the value wraps here, as the target does. */
static long long sema_conv(long long v, int t) {
    int b;
    b = ct_int_bytes(t);
    if (b >= 8) return v;
    if (ct_is_unsigned(t)) {
        if (b == 4) return v & 4294967295LL;
        if (b == 2) return v & 65535;
        return v & 255;
    }
    if (b == 4) return (v << 32) >> 32;
    if (b == 2) return (v << 48) >> 48;
    return (v << 56) >> 56;
}

static long long sema_cvalof(Node *c) {
    return ((long long)c->cval_hi << 32) | (long long)(unsigned)c->cval;
}

static void sema_set_cval(Node *n, long long v) {
    if (!ct_is_integer(n->cty) || ct_rank(n->cty) > 6) return;
    v = sema_conv(v, n->cty);
    n->cval = (int)v;
    n->cval_hi = (int)(v >> 32);
    n->attr = n->attr | AT_ICX;
}

/* The value of binary operator op on integer constants a and b, both
 * already in their common type t; *ok cleared where the result is not a
 * constant (division by zero, a shift past the width). */
static long long sema_fold(int op, long long a, long long b, int t, int *ok) {
    int uns;
    int bits;
    uns = ct_is_unsigned(t);
    bits = ct_int_bytes(t) * 8;
    if (op == TK_PLUS) return a + b;
    if (op == TK_MINUS) return a - b;
    if (op == TK_STAR) return a * b;
    if (op == TK_SLASH || op == TK_PERCENT) {
        if (b == 0) { *ok = 0; return 0; }
        if (uns) {
            if (op == TK_SLASH) return (long long)((unsigned long long)a / (unsigned long long)b);
            return (long long)((unsigned long long)a % (unsigned long long)b);
        }
        if (op == TK_SLASH) return a / b;
        return a % b;
    }
    if (op == TK_AMP) return a & b;
    if (op == TK_PIPE) return a | b;
    if (op == TK_CARET) return a ^ b;
    if (op == TK_LSHIFT || op == TK_RSHIFT) {
        if (b < 0 || b >= bits) { *ok = 0; return 0; }
        if (op == TK_LSHIFT) return a << b;
        if (uns) return (long long)((unsigned long long)a >> b);
        return a >> b;
    }
    if (op == TK_EQ) return a == b;
    if (op == TK_NE) return a != b;
    if (uns) {
        if (op == TK_LT) return (unsigned long long)a < (unsigned long long)b;
        if (op == TK_GT) return (unsigned long long)a > (unsigned long long)b;
        if (op == TK_LE) return (unsigned long long)a <= (unsigned long long)b;
        if (op == TK_GE) return (unsigned long long)a >= (unsigned long long)b;
    }
    if (op == TK_LT) return a < b;
    if (op == TK_GT) return a > b;
    if (op == TK_LE) return a <= b;
    if (op == TK_GE) return a >= b;
    *ok = 0;
    return 0;
}

static void sema_attr_node(Node *n) {
    Node *x;
    int t;
    int k;
    int ok;
    int ct;
    long long a;
    long long b;
    long long v;
    double d;
    float f;
    int w[2];
    n->attr = n->attr & (AT_UCMP | AT_INIT | AT_HEXOCT | AT_AMPF);    /* sema_expr_mty's, the parser's: kept */
    t = n->cty;
    k = n->kind;
    /* lvalues */
    if (k == ND_VAR || k == ND_STRING) {
        if (!ct_is_func(t)) n->attr = AT_LV;
    } else if (k == ND_UNARY && n->op == TK_STAR) {
        if (!ct_is_func(t)) n->attr = AT_LV;
    } else if (k == ND_MEMBER && n->lhs) {
        n->attr = n->lhs->attr & AT_LV;
    } else if (k == ND_COMMA && n->op == TK_LBRACE) {
        x = n;      /* a compound literal: the variable at the end */
        while (x->kind == ND_COMMA && x->rhs) x = x->rhs;
        n->attr = x->attr & AT_LV;
    }
    if ((n->attr & AT_LV) && !ct_is_array(t) && !ct_is_func(t) && ct_kindof(t) != CT_VOID &&
        !(ct_qual(t) & CQ_CONST) && !ty_is_incomplete_struct(n->ty) &&
        !sema_has_const_member(ct_unqual(t)) && k != ND_STRING)
        n->attr = n->attr | AT_MOD;
    /* integer constant expressions */
    if (k == ND_NUM) {
        if (ct_int_bytes(t) >= 8) v = ((long long)n->val_hi << 32) | (long long)(unsigned)n->val;
        else v = (long long)n->val;
        sema_set_cval(n, v);
    } else if (k == ND_CAST && n->lhs) {
        if (n->lhs->attr & AT_ICX) {
            sema_set_cval(n, sema_cvalof(n->lhs));
        } else if (n->lhs->kind == ND_FNUM && ct_is_integer(t)) {
            /* a floating constant as the immediate operand of a cast (6.6p6) */
            w[0] = n->lhs->val;
            w[1] = n->lhs->val_hi;
            if (ty_is_double(n->lhs->ty)) memcpy(&d, w, 8);
            else { memcpy(&f, w, 4); d = f; }
            sema_set_cval(n, (long long)d);
        }
        if ((n->lhs->attr & AT_NPC) && ct_is_ptr(t) &&
            ct_kindof(ct_baseof(t)) == CT_VOID && ct_qual(ct_baseof(t)) == 0)
            n->attr = n->attr | AT_NPC;
    } else if (k == ND_UNARY && n->lhs && (n->lhs->attr & AT_ICX)) {
        a = sema_cvalof(n->lhs);
        if (n->op == TK_MINUS) sema_set_cval(n, 0 - a);
        else if (n->op == TK_TILDE) sema_set_cval(n, ~a);
        else if (n->op == TK_BANG) sema_set_cval(n, a == 0);
    } else if (k == ND_BINOP && n->lhs && n->rhs && (n->lhs->attr & AT_ICX)) {
        a = sema_cvalof(n->lhs);
        if (n->op == TK_LAND && a == 0) sema_set_cval(n, 0);
        else if (n->op == TK_LOR && a != 0) sema_set_cval(n, 1);
        else if (n->rhs->attr & AT_ICX) {
            b = sema_cvalof(n->rhs);
            if (n->op == TK_LAND || n->op == TK_LOR) {
                sema_set_cval(n, b != 0);
            } else {
                /* the operands in their common type: the result's for
                 * arithmetic, the promoted left's for a shift */
                if (n->op == TK_LSHIFT || n->op == TK_RSHIFT) ct = ct_promote(n->lhs->cty);
                else ct = ct_usual(n->lhs->cty, n->rhs->cty);
                ok = 1;
                v = sema_fold(n->op, sema_conv(a, ct), sema_conv(b, ct), ct, &ok);
                if (ok) sema_set_cval(n, v);
            }
        }
    } else if (k == ND_TERNARY && n->cond && (n->cond->attr & AT_ICX) && n->lhs && n->rhs) {
        if (sema_cvalof(n->cond) != 0) { if (n->lhs->attr & AT_ICX) sema_set_cval(n, sema_cvalof(n->lhs)); }
        else if (n->rhs->attr & AT_ICX) sema_set_cval(n, sema_cvalof(n->rhs));
    }
    /* null pointer constants: an integer constant expression of value 0 */
    if ((n->attr & AT_ICX) && n->cval == 0 && n->cval_hi == 0)
        n->attr = n->attr | AT_NPC;
}

/* --- Constraints (docs/TYPES.md S5) --- */

/* A constraint violation in an expression: an error at the place the
 * node was read (5.1.1.3).  S12CC_CONSTRAINT_WARN=1 reports and goes
 * on, to survey a corpus. */
static int sema_cwarn = -1;
static void sema_violation(Node *n, char *msg) {
    if (sema_cwarn < 0) sema_cwarn = getenv("S12CC_CONSTRAINT_WARN") != 0;
    pp_where_at(n->file, n->line);
    fdputs(sema_cwarn ? "warning: " : "error: ", 2);
    fdputs(msg, 2);
    fdputc(10, 2);
    if (!sema_cwarn) exit(1);
}

/* The conversions 6.5.16.1 constrains, by class, graded as clang grades
 * them by default (the ruling in docs/TYPES.md, S5): an integer and a
 * pointer, incompatible pointer types, a difference of incompatible
 * pointers and a call of a non-function are errors; a dropped qualifier,
 * a sign mismatch, and pointer comparisons and ?: arms of distinct types
 * are warnings; a function pointer and void * convert, an extension
 * (POSIX's dlsym needs it, and DOOM passes its callbacks so).
 * S12CC_SURVEY=1 reports every one, tagged by class. */
/* S12CC_SURVEY_LOG=path appends them to a file instead, so a corpus
 * built by scripts that keep the compiler's stderr can be surveyed. */
#ifdef O_APPEND
#define SEMA_OPEN_LOG(p) open((p), O_WRONLY | O_CREAT | O_APPEND, 0644)
#else
#define SEMA_OPEN_LOG(p) open((p), 14)     /* the selfhost libc's O_WRONLY|O_CREAT|O_APPEND */
#endif
static int sema_survey_on = -1;
static int sema_survey_fd = 2;
static void sema_convert_diag(Node *n, char *cls, char *msg) {
    char *lp;
    int fd;
    if (sema_survey_on < 0) {
        sema_survey_on = getenv("S12CC_SURVEY") != 0;
        lp = getenv("S12CC_SURVEY_LOG");
        if (lp != 0) {
            fd = SEMA_OPEN_LOG(lp);
            if (fd >= 0) { sema_survey_fd = fd; sema_survey_on = 1; }
        }
    }
    if (sema_survey_on) {
        fd = sema_survey_fd;
        if (n->file >= 0 && n->file < pp_nfiles && pp_files[n->file][0] != 0) fdputs(pp_files[n->file], fd);
        fdputc(58, fd);
        fdputuint(fd, n->line);
        fdputs(": survey[", fd);
        fdputs(cls, fd);
        fdputs("]: ", fd);
        fdputs(msg, fd);
        fdputc(10, fd);
    }
    if (strcmp(cls, "fn-void") == 0) return;
    if (strcmp(cls, "int-to-ptr") == 0 || strcmp(cls, "ptr-to-int") == 0 ||
        strcmp(cls, "incompat-ptr") == 0 || strcmp(cls, "ptr-diff") == 0 ||
        strcmp(cls, "call-nonfunc") == 0 || strcmp(cls, "implicit-fn") == 0) {
        sema_violation(n, msg);
        return;
    }
    pp_where_at(n->file, n->line);
    fdputs("warning: ", 2);
    fdputs(msg, 2);
    fdputc(10, 2);
}

/* Compatible types (6.2.7): types are interned, so mostly the same
 * type -- with an array of unknown size, a function without a
 * prototype, and an enumeration and its integer type besides. */
static int sema_compat(int a, int b) {
    int ka;
    int kb;
    int i;
    if (a == b) return 1;
    if (ct_qual(a) != ct_qual(b)) return 0;
    ka = ct_kindof(a);
    kb = ct_kindof(b);
    if (ka == CT_ENUM && (kb == CT_INT || kb == CT_UINT)) return 1;
    if (kb == CT_ENUM && (ka == CT_INT || ka == CT_UINT)) return 1;
    if (ka != kb) return 0;
    if (ka == CT_PTR) return sema_compat(ct_baseof(a), ct_baseof(b));
    if (ka == CT_ARRAY)
        return sema_compat(ct_baseof(a), ct_baseof(b)) &&
               (ct_countof(a) < 0 || ct_countof(b) < 0 || ct_countof(a) == ct_countof(b));
    if (ka == CT_FUNC) {
        if (!sema_compat(ct_baseof(a), ct_baseof(b))) return 0;
        if (ct_nparms(a) < 0 || ct_nparms(b) < 0) return 1;
        if (ct_nparms(a) != ct_nparms(b) || ct_variadic(a) != ct_variadic(b)) return 0;
        i = 0;
        while (i < ct_nparms(a)) {
            if (!sema_compat(ct_unqual(ct_parmty(a, i)), ct_unqual(ct_parmty(b, i)))) return 0;
            i = i + 1;
        }
        return 1;
    }
    return 0;
}

/* Are a and b one type when every qualifier, at every pointer level,
 * is set aside? */
static int sema_compat_but_quals(int a, int b) {
    a = ct_unqual(a);
    b = ct_unqual(b);
    if (ct_is_ptr(a) && ct_is_ptr(b)) return sema_compat_but_quals(ct_baseof(a), ct_baseof(b));
    return sema_compat(a, b);
}

/* An operand's type as a value (6.3.2.1): decayed, unqualified. */
static int sema_vty(Node *x) {
    return ct_decay(sema_cty_of(x));
}

static int sema_is_void(int t) {
    return ct_kindof(t) == CT_VOID;
}

static int sema_is_objptr(int t) {
    return ct_is_ptr(t) && !ct_is_func(ct_baseof(t));
}

/* --- Constants that change value in an implicit conversion ---
 * Not constraints: clang's default warnings (-Wconstant-conversion,
 * -Wbitfield-constant-conversion, -Wliteral-conversion), which the
 * ruling has stage08 give where clang gives them. */

/* v kept to w bits, sign-extended for a signed target. */
static long long sema_trunc(long long v, int w, int uns) {
    long long m;
    m = ((long long)1 << w) - 1;
    v = v & m;
    if (!uns && (v >> (w - 1)) != 0) v = v - ((long long)1 << w);
    return v;
}

/* A floating literal, maybe signed (clang warns for a literal only, not
 * for 1.0 / 4): its value in *d. */
static int sema_fp_literal(Node *r, double *d) {
    int neg;
    int w[2];
    float f;
    neg = 0;
    while (r->kind == ND_UNARY && (r->op == TK_MINUS || r->op == TK_PLUS) && r->lhs) {
        if (r->op == TK_MINUS) neg = !neg;
        r = r->lhs;
    }
    if (r->kind != ND_FNUM) return 0;
    if (ty_is_double(r->ty)) {
        w[0] = r->val;
        w[1] = r->val_hi;
        memcpy(d, w, 8);
    } else {
        memcpy(&f, &r->val, 4);
        *d = f;
    }
    if (neg) *d = 0.0 - *d;
    return 1;
}

/* Is integer v (unsigned when uns64: past long long's range, held
 * negative) exact in a binary floating type of p significant bits?
 * Counted in bits: the host's own conversions are stage07's. */
static int sema_int_exact_fp(long long v, int uns64, int p) {
    unsigned long long m;
    int bits;
    m = (unsigned long long)v;
    if (!uns64 && v < 0) m = 0 - m;
    while (m != 0 && (m & 1) == 0) m = m >> 1;
    bits = 0;
    while (m != 0) {
        bits = bits + 1;
        m = m >> 1;
    }
    return bits <= p;
}

/* A constant r converted to integer type lt, a bit-field of bits bits
 * when bits > 0: does the value change?  A negative value going to an
 * unsigned type of its width is a sign conversion, not warned; a
 * positive one filling a signed type's width is warned, unless it was
 * written in hex or octal (not for a bit-field, where clang warns). */
static void sema_const_conv(Node *at, int lt, Node *r, int bits) {
    long long v;
    int w;
    int uns;
    int rt;
    double d;
    lt = ct_unqual(lt);
    if (ct_is_floating(lt) && (r->attr & AT_ICX)) {
        /* an integer constant a float or double cannot hold exactly
         * (-Wimplicit-const-int-float-conversion): float x = 16777217 */
        rt = sema_vty(r);
        if (!sema_int_exact_fp(sema_cvalof(r), ct_is_unsigned(rt) && ct_int_bytes(rt) >= 8,
                               ct_kindof(lt) == CT_FLOAT ? 24 : 53))
            sema_convert_diag(at, "const-int-float", "implicit conversion of a constant to a floating type changes its value");
        return;
    }
    if (!ct_is_integer(lt)) return;
    if (ct_kindof(lt) == CT_BOOL) return;      /* sema_bool_ctx's */
    if (sema_fp_literal(r, &d)) {
        w = bits > 0 ? bits : ct_int_bytes(lt) * 8;
        uns = ct_is_unsigned(lt);
        if (d <= -9.3e18 || d >= 1.85e19 || (uns && d <= -1.0) ||
            (w < 64 && (uns ? d >= (double)((long long)1 << w)
                            : (d >= (double)((long long)1 << (w - 1)) ||
                               d <= 0.0 - (double)((long long)1 << (w - 1)) - 1.0))))
            sema_convert_diag(at, "literal-conv", "implicit conversion of an out-of-range floating constant to an integer type is undefined");
        else if (d != (double)(long long)d)
            sema_convert_diag(at, "literal-conv", "implicit conversion of a floating constant to an integer type changes its value");
        return;
    }
    if (!(r->attr & AT_ICX)) return;
    w = bits > 0 ? bits : ct_int_bytes(lt) * 8;
    if (w >= 64) return;
    uns = ct_is_unsigned(lt);
    v = sema_cvalof(r);
    rt = sema_vty(r);
    if (v < 0 && ct_is_unsigned(rt) && ct_int_bytes(rt) >= 8) {
        /* past long long's range: changes in any narrower type */
    } else {
        if (sema_trunc(v, w, uns) == v) return;
        if (uns && v < 0 && v >= 0 - ((long long)1 << (w - 1))) return;
        if (!uns && v > 0 && v < ((long long)1 << w) && bits == 0 && (r->attr & AT_HEXOCT)) return;
    }
    if (bits > 0)
        sema_convert_diag(at, "bitfield-conv", "implicit truncation of a constant to a bit-field changes its value");
    else
        sema_convert_diag(at, "const-conv", "implicit conversion of a constant changes its value");
}

/* Does n name an object -- x, s.m, s.a.b -- whose address cannot be
 * null?  Not through a pointer: &p->m may be. */
static int sema_names_object(Node *n) {
    if (n->kind == ND_VAR) return 1;
    if (n->kind == ND_MEMBER && n->bit_width == 0 && n->lhs) return sema_names_object(n->lhs);
    return 0;
}

/* r converted to _Bool, as a condition, an operand of !, && or ||, or a
 * _Bool's value: where clang's defaults warn that the answer is fixed
 * -- an object's address, an array, a function named without & are
 * never null (-Wpointer-bool-conversion), a floating literal other than
 * 0 or 1 changes value (-Wliteral-conversion). */
static void sema_bool_ctx(Node *r) {
    double d;
    if (!r) return;
    if (sema_fp_literal(r, &d)) {
        if (d != 0.0 && d != 1.0)
            sema_convert_diag(r, "literal-conv", "implicit conversion of a floating constant to _Bool changes its value");
        return;
    }
    if ((r->kind == ND_FUNC_REF && !(r->attr & AT_AMPF)) ||
        (r->kind == ND_VAR && r->cty > 0 && ct_is_array(r->cty)) ||
        (r->kind == ND_UNARY && r->op == TK_AMP && r->lhs && sema_names_object(r->lhs)))
        sema_convert_diag(r, "pointer-bool", "an address that is never null always converts to true");
}

/* Simple assignment's constraints (6.5.16.1p1), which initialization
 * (6.7.9p11), argument passing (6.5.2.2p7) and return (6.8.6.4p3)
 * share: the value r converted to type lt. */
static void sema_assign_check(Node *at, int lt, Node *r) {
    int rt;
    int lp;
    int rp;
    lt = ct_unqual(lt);
    rt = sema_vty(r);
    if (sema_is_void(rt)) { sema_violation(at, "void value used where a value is required"); return; }
    if (ct_kindof(lt) == CT_BOOL) sema_bool_ctx(r);
    if (ct_is_arith(lt)) {
        sema_const_conv(at, lt, r, 0);
        if (ct_is_ptr(rt) && ct_kindof(lt) != CT_BOOL)
            sema_convert_diag(at, "ptr-to-int", "pointer converted to an integer without a cast");
        else if (!ct_is_scalar(rt))
            sema_violation(at, "assigning a value of incompatible type");
        return;
    }
    if (ct_is_record(lt)) {
        if (ct_unqual(rt) != lt) sema_violation(at, "assigning a structure or union of a different type");
        return;
    }
    if (!ct_is_ptr(lt)) return;
    if (r->attr & AT_NPC) return;
    if (ct_is_integer(rt)) { sema_convert_diag(at, "int-to-ptr", "integer converted to a pointer without a cast"); return; }
    if (!ct_is_ptr(rt)) { sema_violation(at, "assigning a value of incompatible type to a pointer"); return; }
    lp = ct_baseof(lt);
    rp = ct_baseof(rt);
    if ((ct_qual(rp) & ~ct_qual(lp)) != 0)
        sema_convert_diag(at, "discard-qual", "conversion discards qualifiers from the pointed-to type");
    if (sema_is_void(lp) || sema_is_void(rp)) {
        if (ct_is_func(lp) || ct_is_func(rp))
            sema_convert_diag(at, "fn-void", "conversion between a function pointer and void *");
        return;
    }
    if (sema_compat(ct_unqual(lp), ct_unqual(rp))) return;
    /* char *const * from const char **: the same but for qualifiers
     * further in -- clang's discards-qualifiers warning, not an error */
    if (sema_compat_but_quals(lp, rp)) {
        sema_convert_diag(at, "discard-qual", "conversion discards qualifiers in nested pointer types");
        return;
    }
    if (ct_is_integer(lp) && ct_is_integer(rp) &&
        ct_to_unsigned(ct_unqual(lp)) == ct_to_unsigned(ct_unqual(rp)))
        sema_convert_diag(at, "ptr-sign", "pointers to integer types that differ in sign");
    else
        sema_convert_diag(at, "incompat-ptr", "conversion between incompatible pointer types");
}

/* The constraints of 6.5 on one expression node, its operands done. */
static void sema_constraints(Node *n) {
    int k;
    int op;
    int l;
    int r;
    k = n->kind;
    op = n->op;
    l = n->lhs ? sema_vty(n->lhs) : 0;
    r = n->rhs ? sema_vty(n->rhs) : 0;
    if (k == ND_BINOP && n->lhs && n->rhs) {
        if (sema_is_void(l) || sema_is_void(r)) {
            sema_violation(n, "void value used where a value is required");
            return;
        }
        if (op == TK_PERCENT || op == TK_AMP || op == TK_PIPE || op == TK_CARET ||
            op == TK_LSHIFT || op == TK_RSHIFT) {
            if (!ct_is_integer(l) || !ct_is_integer(r)) sema_violation(n, "operands must have integer type");
        } else if (op == TK_STAR || op == TK_SLASH) {
            if (!ct_is_arith(l) || !ct_is_arith(r)) sema_violation(n, "operands must have arithmetic type");
        } else if (op == TK_PLUS) {
            if (!(ct_is_arith(l) && ct_is_arith(r)) && !(ct_is_ptr(l) && ct_is_integer(r)) &&
                !(ct_is_integer(l) && ct_is_ptr(r)))
                sema_violation(n, "invalid operands to binary +");
        } else if (op == TK_MINUS) {
            if (ct_is_ptr(l) && ct_is_ptr(r)) {
                if (!sema_compat(ct_unqual(ct_baseof(l)), ct_unqual(ct_baseof(r))))
                    sema_convert_diag(n, "ptr-diff", "subtracting pointers to incompatible types");
            } else if (!(ct_is_arith(l) && ct_is_arith(r)) && !(ct_is_ptr(l) && ct_is_integer(r))) {
                sema_violation(n, "invalid operands to binary -");
            }
        } else if (op == TK_LAND || op == TK_LOR) {
            if (!ct_is_scalar(l) || !ct_is_scalar(r)) sema_violation(n, "operands of && and || must have scalar type");
            sema_bool_ctx(n->lhs);
            sema_bool_ctx(n->rhs);
        } else if (op == TK_LT || op == TK_GT || op == TK_LE || op == TK_GE) {
            if (ct_is_ptr(l) && ct_is_ptr(r)) {
                if (!sema_compat(ct_unqual(ct_baseof(l)), ct_unqual(ct_baseof(r))))
                    sema_convert_diag(n, "ptr-cmp", "ordering pointers to incompatible types");
            } else if (ct_is_ptr(l) || ct_is_ptr(r)) {
                sema_convert_diag(n, "ptr-int-cmp", "ordering a pointer against an integer");
            } else if (!ct_is_arith(l) || !ct_is_arith(r)) {
                sema_violation(n, "invalid operands to a relational operator");
            }
        } else if (op == TK_EQ || op == TK_NE) {
            if (ct_is_ptr(l) && ct_is_ptr(r)) {
                if (!sema_is_void(ct_baseof(l)) && !sema_is_void(ct_baseof(r)) &&
                    !sema_compat(ct_unqual(ct_baseof(l)), ct_unqual(ct_baseof(r))))
                    sema_convert_diag(n, "ptr-cmp", "comparing pointers to incompatible types");
            } else if (ct_is_ptr(l) || ct_is_ptr(r)) {
                if (!(ct_is_ptr(l) ? (n->rhs->attr & AT_NPC) : (n->lhs->attr & AT_NPC)))
                    sema_convert_diag(n, "ptr-int-cmp", "comparing a pointer with an integer");
            } else if (!ct_is_arith(l) || !ct_is_arith(r)) {
                sema_violation(n, "invalid operands to an equality operator");
            }
        }
        return;
    }
    if (k == ND_UNARY && n->lhs) {
        if (sema_is_void(l) && op != TK_AMP) { sema_violation(n, "void value used where a value is required"); return; }
        if (op == TK_MINUS && !ct_is_arith(l)) sema_violation(n, "operand of unary - must have arithmetic type");
        if (op == TK_TILDE && !ct_is_integer(l)) sema_violation(n, "operand of ~ must have integer type");
        if (op == TK_BANG && !ct_is_scalar(l)) sema_violation(n, "operand of ! must have scalar type");
        if (op == TK_BANG) sema_bool_ctx(n->lhs);
        if (op == TK_STAR && !ct_is_ptr(l)) sema_violation(n, "indirection requires a pointer operand");
        return;
    }
    if (k == ND_TERNARY && n->cond) {
        if (!ct_is_scalar(sema_vty(n->cond))) sema_violation(n, "the condition of ?: must have scalar type");
        sema_bool_ctx(n->cond);
        if (n->lhs && n->rhs && ct_is_ptr(l) && ct_is_ptr(r) &&
            !sema_is_void(ct_baseof(l)) && !sema_is_void(ct_baseof(r)) &&
            !sema_compat(ct_unqual(ct_baseof(l)), ct_unqual(ct_baseof(r))))
            sema_convert_diag(n, "ptr-cond", "?: arms are pointers to incompatible types");
        else if (n->lhs && n->rhs && (ct_is_ptr(l) != ct_is_ptr(r)) &&
                 !(ct_is_ptr(l) ? (n->rhs->attr & AT_NPC) : (n->lhs->attr & AT_NPC)))
            sema_convert_diag(n, "ptr-int-cond", "?: arms are a pointer and an integer");
        return;
    }
    if ((k == ND_ASSIGN && op == TK_ASSIGN) || k == ND_COMP_ASSIGN || k == ND_POST_INC || k == ND_POST_DEC) {
        if (n->lhs && !(n->lhs->attr & AT_MOD)) {
            sema_violation(n, "expression is not assignable (not a modifiable lvalue)");
            return;
        }
    }
    /* An initializer's own stores into a member or element are made by
     * offset: checked where the parser gave the target its C type
     * (AT_INIT, 6.7.9p11), not where it is only the machine type's. */
    if (k == ND_ASSIGN && n->lhs && n->rhs &&
        (op == TK_ASSIGN || n->lhs->kind == ND_VAR || (n->attr & AT_INIT))) {
        sema_assign_check(n, sema_cty_of(n->lhs), n->rhs);
        if (n->lhs->kind == ND_MEMBER && n->lhs->bit_width > 0)
            sema_const_conv(n, sema_cty_of(n->lhs), n->rhs, n->lhs->bit_width);
        return;
    }
    if (k == ND_COMP_ASSIGN && n->lhs && n->rhs) {
        if (sema_is_void(r)) sema_violation(n, "void value used where a value is required");
        else if (op == TK_PLUS || op == TK_MINUS) {
            if (!(ct_is_arith(l) && ct_is_arith(r)) && !(ct_is_ptr(l) && ct_is_integer(r)))
                sema_violation(n, "invalid operands to a compound assignment");
        } else if (op == TK_STAR || op == TK_SLASH) {
            if (!ct_is_arith(l) || !ct_is_arith(r)) sema_violation(n, "invalid operands to a compound assignment");
        } else if (!ct_is_integer(l) || !ct_is_integer(r)) {
            sema_violation(n, "invalid operands to a compound assignment");
        }
        return;
    }
    if ((k == ND_POST_INC || k == ND_POST_DEC) && n->lhs) {
        if (!(ct_is_arith(l) || ct_is_ptr(l))) sema_violation(n, "operand of ++ or -- must have real or pointer type");
        return;
    }
    if (k == ND_CAST && n->lhs && n->cty > 0) {
        r = ct_unqual(n->cty);
        if (sema_is_void(r)) return;
        if (!ct_is_scalar(r)) { sema_violation(n, "a cast must name a scalar type or void"); return; }
        if (!ct_is_scalar(l)) { sema_violation(n, "operand of a cast must have scalar type"); return; }
        if ((ct_is_ptr(r) && ct_is_floating(l)) || (ct_is_floating(r) && ct_is_ptr(l)))
            sema_violation(n, "a pointer cannot be cast to or from a floating type");
        return;
    }
}

/* The callee's type: the prototype a direct call names, the pointed-to
 * function of an indirect one (6.5.2.2p1); 0 when not known. */
static int sema_callee_cty(Node *n) {
    int t;
    if (n->kind == ND_CALL) return find_func_cty(n->name);
    if (n->kind != ND_CALL_PTR || !n->lhs) return 0;
    t = sema_vty(n->lhs);
    if (ct_is_ptr(t) && ct_is_func(ct_baseof(t))) return ct_baseof(t);
    if (ct_is_arith(t) && !n->lhs->is_fnptr)
        sema_convert_diag(n, "call-nonfunc", "called object is not a function or function pointer");
    return 0;
}

/* A call against its prototype (6.5.2.2p2): the argument count, and
 * each argument as if assigned to its parameter. */
static void sema_call_check(Node *n, int ft) {
    Node *a;
    int i;
    int np;
    if (ft <= 0 || !ct_is_func(ft)) return;
    np = ct_nparms(ft);
    if (np < 0) return;
    i = 0;
    a = n->args;
    while (a) {
        if (i < np) sema_assign_check(a, ct_parmty(ft, i), a);
        else if (sema_is_void(sema_vty(a))) sema_violation(a, "void value used where a value is required");
        i = i + 1;
        a = a->next;
    }
    if (i < np) sema_violation(n, "too few arguments to function call");
    if (i > np && !ct_variadic(ft)) sema_violation(n, "too many arguments to function call");
}

/* Float bits fb widened to a double in *out, on the bits: exact. */
static void sema_float_bits(int fb, double *out) {
    int e;
    int w[2];
    e = (fb >> 23) & 255;
    if (e == 0) {
        /* zero or subnormal: the 23-bit fraction times 2^-149, exact */
        *out = (double)(fb & 0x7FFFFF) * 1.401298464324817070923729583289916e-45;
        if (fb < 0) *out = 0.0 - *out;
        return;
    }
    w[1] = (fb & (1 << 31)) | ((fb >> 3) & 0xFFFFF);
    if (e == 255) w[1] = w[1] | 0x7FF00000;              /* inf, NaN */
    else w[1] = w[1] | ((e - 127 + 1023) << 20);
    w[0] = fb << 29;
    memcpy(out, w, 8);
}

/* *a rounded to float.  The narrowing is a (float) cast; the widening
 * goes through the bits.  (No function here returns a double: stage07,
 * which builds this compiler, cannot, c_lexer.rl says -- the first
 * version did, and every float constant came back 0.) */
static void sema_round_float(double *a) {
    float f;
    int fb;
    f = (float)*a;
    memcpy(&fb, &f, 4);
    sema_float_bits(fb, a);
}

/* An integer constant's value as a double in *out (6.3.1.4p2): its two
 * words, each converted exactly, then one rounding in the sum. */
static void sema_int_to_double(Node *n, double *out) {
    double hi;
    double lo;
    hi = (double)n->cval_hi;
    if (n->cval_hi < 0 && ct_is_unsigned(n->cty)) hi = hi + 4294967296.0;
    lo = (double)n->cval;
    if (n->cval < 0) lo = lo + 4294967296.0;
    *out = hi * 4294967296.0 + lo;
}

/* The value of an arithmetic constant expression of floating type (6.6p8)
 * in *out: literals, integer constants, casts between arithmetic types,
 * unary minus, + - * /, and ?: on an integer constant.  A float result is
 * rounded to float at each step, as C evaluates it.  Returns 0 for
 * anything else.  (Static initializers were folded by a token evaluator
 * that knew only a leading * / chain: 2 + 0.25 stored 2.0, W6.) */
static int sema_fp_const(Node *n, double *out) {
    double a;
    double b;
    int w[2];
    if (!n) return 0;
    if (n->attr & AT_ICX) {
        sema_int_to_double(n, out);
        return 1;
    }
    if (n->kind == ND_FNUM && ty_is_float(n->ty)) {
        sema_float_bits(n->val, &a);     /* a float literal holds float bits */
    } else if (n->kind == ND_FNUM) {
        w[0] = n->val;
        w[1] = n->val_hi;
        memcpy(&a, w, 8);
    } else if (n->kind == ND_CAST && n->cty > 0 && ct_is_arith(n->cty)) {
        if (!sema_fp_const(n->lhs, &a)) return 0;
    } else if (n->kind == ND_UNARY && n->op == TK_MINUS) {
        if (!sema_fp_const(n->lhs, &a)) return 0;
        a = 0.0 - a;
    } else if (n->kind == ND_BINOP && (n->op == TK_PLUS || n->op == TK_MINUS ||
                                       n->op == TK_STAR || n->op == TK_SLASH)) {
        if (!sema_fp_const(n->lhs, &a) || !sema_fp_const(n->rhs, &b)) return 0;
        if (n->op == TK_PLUS) a = a + b;
        else if (n->op == TK_MINUS) a = a - b;
        else if (n->op == TK_STAR) a = a * b;
        else a = a / b;
    } else if (n->kind == ND_TERNARY && n->cond && (n->cond->attr & AT_ICX)) {
        if (sema_cvalof(n->cond) != 0) return sema_fp_const(n->lhs, out);
        return sema_fp_const(n->rhs, out);
    } else {
        return 0;
    }
    /* an integer-typed result here (a cast of a floating constant to
     * int) is not this evaluator's: sema did not call it a constant */
    if (n->cty <= 0 || !ct_is_arith(n->cty) || ct_is_integer(n->cty)) return 0;
    if (ct_kindof(ct_unqual(n->cty)) == CT_FLOAT) sema_round_float(&a);
    *out = a;
    return 1;
}

static void sema_expr(Node *n) {
    if (!n) return;
    sema_expr_mty(n);
    sema_cty_node(n);
    sema_attr_node(n);
}

/* S12CC_CTY_CHECK=1, after optimize(): every node the AST optimizer
 * folded to a literal must hold the value sema computed for it -- two
 * evaluators, written apart, held against each other. */
static void sema_check_folds(Node *n) {
    if (!n) return;
    if (sema_cty_check < 0) sema_cty_check = getenv("S12CC_CTY_CHECK") != 0;
    if (!sema_cty_check) return;
    if (n->kind == ND_NUM && (n->attr & AT_ICX) &&
        (n->val != n->cval || (ty_is_llong(n->ty) && n->val_hi != n->cval_hi))) {
        fdputs("cval-mismatch folded=", 2);
        fdputuint(2, (unsigned)n->val);
        fdputs(" sema=", 2);
        fdputuint(2, (unsigned)n->cval);
        fdputs(" ty=", 2);
        fdputuint(2, (unsigned)n->ty);
        fdputs("\n", 2);
    }
    sema_check_folds(n->lhs);
    sema_check_folds(n->rhs);
    sema_check_folds(n->cond);
    sema_check_folds(n->body);
    sema_check_folds(n->init);
    sema_check_folds(n->step);
    sema_check_folds(n->els);
    sema_check_folds(n->args);
    sema_check_folds(n->next);
}

/* --- Statement constraints (6.8) --- */

static int sema_switch_depth;   /* switch statements around the one walked */

/* A controlling expression: if, while, do and for need a scalar
 * (6.8.4.1p1, 6.8.5p2); if (v) on a structure was taken. */
static void sema_scalar_cond(Node *c) {
    if (c && !ct_is_scalar(sema_vty(c)))
        sema_violation(c, "statement requires expression of scalar type");
    sema_bool_ctx(c);
}

/* The case labels of one switch, those of a switch inside it left to
 * that switch: no two values the same after conversion to the
 * controlling expression's promoted type, at most one default
 * (6.8.4.2p3).  wide: that type is 64-bit. */
static int *sema_case_lo;
static int *sema_case_hi;
static int sema_ncases;
static int sema_case_cap;
static int sema_ndefaults;

static void sema_collect_cases(Node *s, int wide) {
    int i;
    while (s) {
        if (s->kind == ND_CASE) {
            i = 0;
            while (i < sema_ncases) {
                if (sema_case_lo[i] == s->val && (!wide || sema_case_hi[i] == s->val_hi))
                    sema_violation(s, "duplicate case value");
                i = i + 1;
            }
            if (sema_ncases >= sema_case_cap) {
                sema_case_cap = sema_case_cap > 0 ? sema_case_cap * 2 : 256;
                sema_case_lo = realloc(sema_case_lo, sema_case_cap * 4);
                sema_case_hi = realloc(sema_case_hi, sema_case_cap * 4);
            }
            sema_case_lo[sema_ncases] = s->val;
            sema_case_hi[sema_ncases] = s->val_hi;
            sema_ncases = sema_ncases + 1;
        } else if (s->kind == ND_DEFAULT) {
            sema_ndefaults = sema_ndefaults + 1;
            if (sema_ndefaults > 1) sema_violation(s, "multiple default labels in one switch");
        } else if (s->kind == ND_BLOCK) {
            sema_collect_cases(s->body, wide);
        } else if (s->kind == ND_IF) {
            sema_collect_cases(s->body, wide);
            sema_collect_cases(s->els, wide);
        } else if (s->kind == ND_WHILE || s->kind == ND_DO_WHILE || s->kind == ND_FOR ||
                   s->kind == ND_LABEL) {
            sema_collect_cases(s->body, wide);
        }
        s = s->next;
    }
}

static void sema_switch_check(Node *n) {
    int t;
    t = sema_vty(n->cond);
    if (!ct_is_integer(t)) {
        sema_violation(n, "statement requires expression of integer type");
        return;
    }
    sema_ncases = 0;
    sema_ndefaults = 0;
    sema_collect_cases(n->body, ct_int_bytes(ct_promote(t)) >= 8);
}

/* return against the function's type (6.8.6.4p1): a value from a void
 * function, none from a non-void one, and the value as if assigned. */
static void sema_return_check(Node *n) {
    int vt;
    if (sema_ret_cty <= 0) return;
    if (ct_kindof(sema_ret_cty) == CT_VOID) {
        if (n->lhs && ct_kindof(sema_vty(n->lhs)) != CT_VOID)
            sema_violation(n, "void function should not return a value");
        return;
    }
    if (!n->lhs) {
        sema_violation(n, "non-void function should return a value");
        return;
    }
    vt = sema_vty(n->lhs);
    if (ct_kindof(vt) == CT_VOID) {
        sema_violation(n, "returning a void value from a non-void function");
        return;
    }
    sema_assign_check(n, sema_ret_cty, n->lhs);
}

/* --- Statement walk --- */

static void sema_stmt(Node *n) {
    Node *s;

    if (!n) return;

    if (n->kind == ND_RETURN) {
        sema_expr(n->lhs);
        sema_return_check(n);
        if (n->lhs && sema_is_bool(sema_ret_cty)) n->lhs = sema_to_bool(n->lhs);
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
        sema_scalar_cond(n->cond);
        sema_stmt(n->body);
        sema_stmt(n->els);
        return;
    }

    if (n->kind == ND_WHILE || n->kind == ND_DO_WHILE) {
        sema_expr(n->cond);
        sema_scalar_cond(n->cond);
        sema_stmt(n->body);
        return;
    }

    if (n->kind == ND_FOR) {
        sema_expr(n->init);
        sema_expr(n->cond);
        sema_scalar_cond(n->cond);
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
        sema_switch_check(n);
        sema_switch_depth = sema_switch_depth + 1;
        sema_stmt(n->body);
        sema_switch_depth = sema_switch_depth - 1;
        return;
    }

    if (n->kind == ND_CASE || n->kind == ND_DEFAULT) {
        /* statement-level labels, each inside a switch (6.8.1p2) */
        if (sema_switch_depth == 0)
            sema_violation(n, n->kind == ND_CASE ? "'case' statement not in switch statement"
                                                 : "'default' statement not in switch statement");
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
            sema_ret_cty = find_func_cty(fn->name);
            if (sema_ret_cty > 0) sema_ret_cty = ct_baseof(sema_ret_cty);
            sema_stmt(fn->body);
        }
        fn = fn->next;
    }
}
