/* s32-cobc: conditions.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ====================================================================== */
/* Conditions                                                              */
/* ====================================================================== */

enum { C_AND, C_OR, C_NOT, C_REL, C_CLASS, C_SWITCH };
enum { R_EQ, R_LT, R_GT, R_LE, R_GE, R_NE };

typedef struct Cond {
    int kind;
    struct Cond *a, *b;
    Opnd x, y;
    int op, neg;            /* C_REL */
    int klass;              /* C_CLASS: 0 NUMERIC 1 ALPHABETIC 2 LOWER 3 UPPER, 4+i SPECIAL-NAMES class i */
    int uc0, uc1;           /* the root: user-function calls to make each time it is evaluated */
    int bstack;             /* C_REL: compared on the boolean stack (an ALL literal beside a run-time length) */
    int ptr;                /* C_REL: two data-pointer values, compared as addresses (8.8.4.2.16) */
} Cond;

static Cond *cond_new(int kind) { Cond *c = xmalloc(sizeof *c); memset(c, 0, sizeof *c); c->kind = kind; return c; }

static int opnd_is_boolean(const Opnd *o);
static void bool_fig_opnd(Opnd *o, int n);
static int at_operand(void);
static int is_verb(const char *w);
static int bool_positions(const Sym *s);
static int bool_opnd_len(const Opnd *o);
/* a boolean operand whose positions are known only at run time: a
 * reference modification with a computed length, or a function result
 * of run-time length (cobol ISSUES-94 B4) */
static int bool_len_dynamic(const Opnd *o)
{
    if (o->kind == O_REF) return o->ref.rm && !o->ref.rm_len && !o->ref.rm_odo;
    if (o->kind == O_FUNC) return o->fvar;
    return 0;
}
static int sym_strong_has_boolean(Sym *g);
static int opnd_is_ptr(const Opnd *o);
static int opnd_ptr_cat(const Opnd *o);
static const char *ptr_cat_name(int c);
static Cond *cond_rel(Opnd *x, int op, Opnd *y, int neg)
{
    {   /* data pointers (format 3): EQUAL or NOT EQUAL, and a pointer on
         * both sides, NULL counting as one (2023 8.8.4.2.3 rule 5) */
        int xp = opnd_is_ptr(x) && !(x->kind == O_FIG), yp = opnd_is_ptr(y) && !(y->kind == O_FIG);
        if (xp || yp) {
            if (!opnd_is_ptr(x) || !opnd_is_ptr(y))
                die_at(x->line, "a data pointer is compared only with ADDRESS OF, a pointer item or NULL (2023 8.8.4.2.3 rule 5)");
            if (op != R_EQ && op != R_NE)
                die_at(x->line, "data pointers are compared by EQUAL or NOT EQUAL only (2023 8.8.4.2.2 format 3)");
            {   /* of one category: a program-pointer with a program-pointer (8.8.4.2.4), a data-pointer with a data-pointer */
                int xc = opnd_ptr_cat(x), yc = opnd_ptr_cat(y);
                if (xc > 0 && yc > 0 && xc != yc)
                    die_at(x->line, "a %s is compared with a %s, not a %s (2023 8.8.4.2.3-4)", ptr_cat_name(xc), ptr_cat_name(xc), ptr_cat_name(yc));
            }
            Cond *c = cond_new(C_REL);
            c->x = *x; c->y = *y; c->op = op; c->neg = neg; c->ptr = 1;
            return c;
        }
    }
    /* a boolean operand is compared only with a boolean one (2023
     * 8.8.4.2.8); ZERO and ALL B"..." beside it are boolean */
    {   /* strongly-typed groups compare only with the same type (8.8.4.2.12) */
        int xs = x->kind == O_REF ? x->ref.sym->strong : 0, ys = y->kind == O_REF ? y->ref.sym->strong : 0;
        if ((xs || ys) && xs != ys)
            die_at(x->line, "a strongly-typed group is compared only with one of the same type (2023 8.8.4.2.12)");
    }
    int xb = opnd_is_boolean(x), yb = opnd_is_boolean(y);
    if (xb || yb) {
        if (!xb && !(x->kind == O_FIG || x->kind == O_ALL)) die_at(x->line, "a boolean operand is compared only with a boolean one (2023 8.8.4.2.8)");
        if (!yb && !(y->kind == O_FIG || y->kind == O_ALL)) die_at(y->line, "a boolean operand is compared only with a boolean one (2023 8.8.4.2.8)");
        /* boolean operands relate by EQUAL and NOT EQUAL only (8.8.4.2.2
         * format 2; cobol ISSUES-94 B11) */
        if (op != R_EQ && op != R_NE) die_at(x->line, "boolean operands are compared by EQUAL or NOT EQUAL only (2023 8.8.4.2.2)");
        /* ZERO beside a boolean is one zero, extended by the comparison;
         * ALL B"..." is repeated to the other operand's length -- at run
         * time when that length is known only then */
        if ((x->kind == O_ALL && bool_len_dynamic(y)) || (y->kind == O_ALL && bool_len_dynamic(x))) {
            Opnd *a = x->kind == O_ALL ? x : y;
            if (!a->tok->boolv) { bool_fig_opnd(a, a->tok->len / (a->tok->nat ? 2 : 1)); a->kind = O_ALL; }   /* checked, its value kept ALL */
            Cond *c = cond_new(C_REL);
            c->x = *x; c->y = *y; c->op = op; c->neg = neg; c->bstack = 1;
            return c;
        }
        bool_fig_opnd(x, yb ? bool_opnd_len(y) : 1); bool_fig_opnd(y, xb ? bool_opnd_len(x) : 1);
    } else if (op != R_EQ && op != R_NE && x->kind == O_REF && x->ref.sym->strong && sym_strong_has_boolean(x->ref.sym))
        die_at(x->line, "a strongly-typed group holding a boolean item is compared by EQUAL or NOT EQUAL only (2023 8.8.4.2.3 rule 4)");
    Cond *c = cond_new(C_REL);
    c->x = *x; c->y = *y; c->op = op; c->neg = neg;
    return c;
}

static Cond *cond_bin(int kind, Cond *a, Cond *b) { Cond *c = cond_new(kind); c->a = a; c->b = b; return c; }

/* a written relation's operand rules (X3.23-1985 6.3.1.1, 6.3.1.1.2; 2023
 * 8.8.4.2.1, 8.8.4.2.5): at least one operand not a literal; and a numeric
 * operand compared with a nonnumeric one an integer item of usage DISPLAY
 * (or national) or an integer literal -- a noninteger, an expression or a
 * binary item beside text has no defined comparison.  A figurative
 * constant fits either side. */
static void rel_rules(const Opnd *x, const Opnd *y, int line)
{
    int xl = (x->kind == O_NUM && !x->folded) || x->kind == O_STR || x->kind == O_FIG || x->kind == O_ALL;
    int yl = (y->kind == O_NUM && !y->folded) || y->kind == O_STR || y->kind == O_FIG || y->kind == O_ALL;
    if (xl && yl)
        die_at(line, "a relation condition needs at least one operand that is not a literal (%s)",
               g_std < 2002 ? "X3.23-1985 6.3.1.1" : "2023 8.8.4.2.1");
    if (x->kind == O_FIG || y->kind == O_FIG) return;
    for (int k = 0; k < 2; k++) {
        const Opnd *n = k ? y : x, *o = k ? x : y;
        if (!opnd_numeric((Opnd *)n)) continue;
        /* the other side nonnumeric: text, or an item of an alphanumeric-
         * class category (national and boolean have their own rules) */
        int other_text = o->kind == O_STR || o->kind == O_ALL ||
            (o->kind == O_REF && (o->ref.rm || o->ref.sym->is_group ||
                                  o->ref.sym->pi.category == PIC_ALPHANUMERIC || o->ref.sym->pi.category == PIC_ALPHABETIC ||
                                  o->ref.sym->pi.category == PIC_ALPHANUMERIC_EDITED || o->ref.sym->pi.category == PIC_NUMERIC_EDITED));
        if (!other_text) continue;
        if (n->kind == O_EXPR)
            die_at(line, "an arithmetic expression is not compared with a nonnumeric operand (%s)",
                   g_std < 2002 ? "X3.23-1985 6.3.1.1.2" : "2023 8.8.4.2.5");
        int integer = n->kind == O_NUM ? numlit_is_int(&n->num)
                    : n->ref.sym->pi.scale == 0 && !strchr(n->ref.sym->pi.pat, 'P');
        if (!integer)
            die_at(line, "a noninteger numeric operand is not compared with a nonnumeric one (%s)",
                   g_std < 2002 ? "X3.23-1985 6.3.1.1.2 (3)" : "2023 8.8.4.2.5");
        if (n->kind == O_REF) cen_pin(n->ref.sym, "nonnumeric");    /* compared as characters */
        if (n->kind == O_REF && n->ref.sym->usage != U_DISPLAY && n->ref.sym->usage != U_NATIONAL)
            die_at(line, "'%s' is compared with a nonnumeric operand, so it must be of usage DISPLAY (%s)", n->ref.sym->name,
                   g_std < 2002 ? "X3.23-1985 6.3.1.1.2: the same usage" : "2023 8.8.4.2.5");
    }
}

static Cond *parse_cond(void);

static Opnd expr_opnd(void);
static int paren_is_condition(void);
static int at_arith_op(void);
static void emit_push_opnd(Opnd *o);

/* ---- boolean expressions (2023 8.8.2; cobol ISSUES-77) ------------------
 * Parsed and emitted in one pass, as parse_expr is: operands are pushed on
 * libcob's boolean stack, operators applied to it, in the order a shunting
 * yard gives -- B-NOT, then B-AND, B-XOR, B-OR, left to right; a shift
 * takes the precedence of the operation before it, B-AND's if none (rule
 * 7b), and carries its integer count with it.  Under g_noemit it only
 * scans.  Either way it builds the tree, each operator's node made as the
 * operator is applied, so emit_bexpr's walk of it, operands first, is the
 * same order.  Returns the widest operand's boolean positions. */
enum { BO_AND = 1, BO_OR, BO_XOR, BO_NOT, BO_SL, BO_SR, BO_SLC, BO_SRC, BO_PAREN };
static int bool_op(const Tok *t)
{
    if (g_std < 2002 || t->kind != T_WORD) return 0;
    static const char *w[] = { "", "b-and", "b-or", "b-xor", "b-not", "b-shift-l", "b-shift-r", "b-shift-lc", "b-shift-rc" };
    for (int i = 1; i <= 8; i++) if (!strcmp(t->s, w[i])) return i;
    return 0;
}
static int bool_opnd_len(const Opnd *o)
{
    if (o->kind == O_STR) return o->tok->len;
    if (o->kind == O_BEXPR) return o->fsize;
    if (o->kind == O_FUNC) return o->fsize;
    if (o->kind == O_REF) return o->ref.rm ? (o->ref.rm_len ? (int)o->ref.rm_len : 1) : bool_positions(o->ref.sym);
    return 1;
}
/* which boolean stack entries are ALL literals, simulated as the code is
 * emitted, for 8.8.2 rules 4 and 5 */
static int g_bsim[64], g_bsp;
static int g_bexpr_all;                  /* the expression just parsed was an ALL literal alone */
static void bool_emit_op(int op, Opnd *cnt)
{
    int line = op >= BO_SL && op <= BO_SRC ? cnt->line : cur()->line;   /* only a shift has its count operand */
    if (op == BO_NOT) { /* of an ALL literal: still one, each position inverted (cobol ISSUES-93) */ }
    else if (op <= BO_XOR) {
        if (g_bsp >= 2 && g_bsim[g_bsp - 1] && g_bsim[g_bsp - 2])
            die_at(line, "the two operands of a boolean operation cannot both be ALL literals (2023 8.8.2 rule 4)");
        if (g_bsp >= 2) { g_bsp--; g_bsim[g_bsp - 1] = 0; }
    } else if (g_bsp && g_bsim[g_bsp - 1])
        die_at(line, "the first operand of a boolean shift cannot be an ALL literal (2023 8.8.2 rule 5)");
    if (op == BO_NOT) { emit_call("cob_bnot"); return; }
    if (op <= BO_XOR) { emit_call(op == BO_AND ? "cob_band" : op == BO_OR ? "cob_bor" : "cob_bxor"); return; }
    emit_incompat(cnt);
    emit_push_opnd(cnt);
    emit_li("r3", op - BO_SL);                   /* 0 L, 1 R, 2 LC, 3 RC; the count taken whole (B7) */
    emit_call("cob_bshift_pop");
}
static void bool_emit_operand(Opnd *o)
{
    if (g_bsp < 64) g_bsim[g_bsp++] = o->kind == O_ALL;
    if (o->kind == O_ALL) {
        /* ALL B"...": its value, repeated to the other operand's length
         * when the operation runs */
        Arg a[2] = { arg_label(lit_label((unsigned char *)o->tok->s, o->tok->len)), arg_imm(o->tok->len) };
        emit_args(a, 2); emit_call("cob_bpush_all");
        return;
    }
    Arg a[2];
    emit_incompat(o);                            /* 14.6.13.2 rule 1 */
    opnd_args(o, &a[0], &a[1], 0, 0);
    emit_args(a, 2);
    emit_call("cob_bpush");
}
typedef struct { BExpr *n[64]; int sp; } BStack;
static BExpr *bx_new(int op, BExpr *l, BExpr *r, const Opnd *o)
{
    BExpr *b = ex_alloc(sizeof *b);
    b->op = op; b->l = l; b->r = r;
    if (o) { b->o = ex_alloc(sizeof *b->o); *b->o = *o; }
    return b;
}
/* an operator applied: its code, and its node over the operands' */
static void bool_apply(BStack *bs, int op, Opnd *cnt)
{
    bool_emit_op(op, cnt);
    if (op <= BO_XOR && op != BO_NOT) {
        if (bs->sp >= 2) { bs->sp--; bs->n[bs->sp - 1] = bx_new(op, bs->n[bs->sp - 1], bs->n[bs->sp], NULL); }
    } else if (bs->sp >= 1) bs->n[bs->sp - 1] = bx_new(op, bs->n[bs->sp - 1], NULL, op == BO_NOT ? NULL : cnt);
}
static BExpr *g_bexpr_tree;              /* the expression parse_bexpr just parsed */
static int parse_bexpr(void)
{
    int save_bsp = g_bsp; g_bsp = 0;
    BStack bs; bs.sp = 0;
    struct { int op, prec; Opnd cnt; } st[64]; int sp = 0;
    int lastprec[32], lv = 0; lastprec[0] = 0;
    int want = 1, width = 0, line = cur()->line;
    static const int prec[] = { 0, 3, 1, 2, 4 };
    for (;;) {
        Tok *t = cur(); int op = bool_op(t);
        if (want) {
            if (op == BO_NOT) {
                /* B-NOT is an operation too: a shift after its operand
                 * takes its precedence (8.8.2 rule 7b; cobol ISSUES-94 B16) */
                advance(); st[sp].op = BO_NOT; st[sp].prec = 4; sp++; lastprec[lv] = 4; continue;
            }
            if (t->kind == T_LP) {
                advance();
                if (sp == 64 || lv == 31) die_at(t->line, "a boolean expression nested too deeply");
                st[sp].op = BO_PAREN; st[sp].prec = 0; sp++; lastprec[++lv] = 0; continue;
            }
            if (op) die_at(t->line, "a boolean operand is expected before %s (2023 8.8.2, Table 4)", t->s);
            if (!at_operand() || (t->kind == T_WORD && is_verb(t->s))) die_at(line, "a boolean expression ends without an operand (2023 8.8.2 rule 2)");
            Opnd o; parse_operand(&o);
            if (o.kind == O_ALL && !o.tok->boolv) die_at(o.line, "ALL in a boolean expression takes a boolean literal");
            if (o.kind == O_FIG) bool_fig_opnd(&o, 1);            /* ZERO: a boolean zero, extended as needed */
            if (!opnd_is_boolean(&o)) die_at(o.line, "a boolean expression takes boolean operands (2023 8.8.2)");
            bool_emit_operand(&o);
            if (bs.sp < 64) bs.n[bs.sp++] = bx_new(0, NULL, NULL, &o);
            int w = bool_opnd_len(&o); if (w > width) width = w;
            want = 0;
            continue;
        }
        if (t->kind == T_RP && lv > 0) {
            advance();
            while (sp && st[sp - 1].op != BO_PAREN) { sp--; bool_apply(&bs, st[sp].op, &st[sp].cnt); }
            sp--; lv--;
            continue;
        }
        if (op == BO_AND || op == BO_OR || op == BO_XOR) {
            advance();
            int p = prec[op];
            while (sp && st[sp - 1].op != BO_PAREN && st[sp - 1].prec >= p) { sp--; bool_apply(&bs, st[sp].op, &st[sp].cnt); }
            if (sp == 64) die_at(t->line, "a boolean expression too long");
            st[sp].op = op; st[sp].prec = p; sp++;
            lastprec[lv] = p; want = 1;
            continue;
        }
        if (op >= BO_SL && op <= BO_SRC) {
            advance();
            int p = lastprec[lv] ? lastprec[lv] : 3;
            Opnd cnt; parse_operand(&cnt);
            if (!((cnt.kind == O_NUM && numlit_is_int(&cnt.num) && !cnt.num.neg) || (cnt.kind == O_REF && is_int_item(cnt.ref.sym))))
                die_at(cnt.line, "a boolean shift takes an integer (2023 8.8.2 rule 5)");
            while (sp && st[sp - 1].op != BO_PAREN && st[sp - 1].prec >= p) { sp--; bool_apply(&bs, st[sp].op, &st[sp].cnt); }
            bool_apply(&bs, op, &cnt);
            continue;
        }
        break;
    }
    if (want) die_at(line, "a boolean expression ends without an operand");
    while (sp) {
        sp--;
        if (st[sp].op == BO_PAREN) die_at(line, "unbalanced parentheses in a boolean expression");
        bool_apply(&bs, st[sp].op, &st[sp].cnt);
    }
    g_bexpr_all = g_bsp == 1 && g_bsim[0];        /* the whole expression one ALL literal */
    g_bsp = save_bsp;
    g_bexpr_tree = bs.sp == 1 ? bs.n[0] : NULL;
    return width;
}

/* the code parse_bexpr would have emitted for b: operands pushed and
 * operators applied in the order the shunting yard gave, a user
 * function's call made as its operand is pushed (ucall_make) */
static void emit_bexpr_node(BExpr *b)
{
    if (!b->op) { ucall_make(b->o); Opnd o = *b->o; bool_emit_operand(&o); return; }
    emit_bexpr_node(b->l);
    if (b->r) emit_bexpr_node(b->r);
    Opnd cnt; memset(&cnt, 0, sizeof cnt);
    if (b->o) { ucall_make(b->o); cnt = *b->o; }
    bool_emit_op(b->op, &cnt);
}
static void emit_bexpr(BExpr *b)
{
    int save_bsp = g_bsp; g_bsp = 0;
    emit_bexpr_node(b);
    g_bexpr_all = g_bsp == 1 && g_bsim[0];
    g_bsp = save_bsp;
}

/* does the parenthesis at the cursor open a boolean expression: a boolean
 * operator before its match */
static int paren_is_boolean(void)
{
    int depth = 0;
    for (int i = g_tp; i < g_ntok; i++) {
        if (g_tok[i].kind == T_LP) depth++;
        else if (g_tok[i].kind == T_RP) { if (--depth == 0) return 0; }
        else if (g_tok[i].kind == T_PERIOD) return 0;
        else if (bool_op(&g_tok[i])) return 1;
    }
    return 0;
}

/* a boolean expression as a condition operand: scanned now, emitted later */
static Opnd bexpr_opnd(void)
{
    Opnd o; memset(&o, 0, sizeof o);
    o.kind = O_BEXPR; o.line = cur()->line;
    g_noemit++; o.fsize = parse_bexpr(); g_noemit--;
    o.bx = g_bexpr_tree;
    if (!o.bx) die_at(o.line, "internal: a boolean expression without its tree");
    return o;
}

/* push any boolean operand, an expression's value included */
static void bool_push(Opnd *o)
{
    if (o->kind == O_BEXPR) { emit_bexpr(o->bx); return; }
    bool_emit_operand(o);
}

/* a condition operand: a plain operand, or an arithmetic expression */
static Opnd parse_cond_operand(void)
{
    if (g_std >= 2002) {
        /* a boolean expression: B-NOT, a parenthesis holding a boolean
         * operator, or a boolean operand followed by one */
        if (bool_op(cur()) == BO_NOT) return bexpr_opnd();
        if (cur()->kind == T_LP && !paren_is_condition() && paren_is_boolean()) return bexpr_opnd();
        int start = g_tp;
        if ((cur()->kind == T_WORD || cur()->kind == T_STR) && at_operand()) {
            g_noemit++; Opnd x; parse_operand(&x); g_noemit--;
            int op = bool_op(cur());
            g_tp = start;
            if (op && op != BO_NOT && opnd_is_boolean(&x)) return bexpr_opnd();
        }
    }
    if (cur()->kind == T_LP && !paren_is_condition()) return expr_opnd();
    if (cur()->kind == T_OP && (!strcmp(cur()->s, "-") || !strcmp(cur()->s, "+"))) return expr_opnd();   /* a unary sign begins an expression */
    int start = g_tp;
    Opnd x; parse_operand(&x);
    if (at_arith_op()) return expr_opnd_after(&x, start);
    return x;
}

static Opnd lit_opnd(Tok *t)
{
    Opnd o; memset(&o, 0, sizeof o);
    o.line = t->line;
    if (t->kind == T_STR) { o.kind = O_STR; o.tok = t; }
    else if (t->kind == T_NUM) { o.kind = O_NUM; numlit_parse(t, &o.num); }
    else { o.kind = O_FIG; o.tok = t; }
    return o;
}

/* level 88: (parent = v1) OR (parent >= lo AND parent <= hi) OR ... */
static Cond *cond_88(Ref *r, int neg)
{
    Sym *c = r->sym;
    Opnd p; memset(&p, 0, sizeof p);
    p.kind = O_REF; p.ref = *r; p.ref.sym = &g_sym[c->parent]; p.line = r->line;
    Cond *all = NULL;
    for (int i = 0; i < c->ncv; i++) {
        Opnd lo = lit_opnd(c->cv_lo[i]);
        if (c->cv_all & (1u << i)) lo.kind = O_ALL;
        Cond *one;
        if (c->cv_hi[i]) {
            Opnd hi = lit_opnd(c->cv_hi[i]);
            one = cond_bin(C_AND, cond_rel(&p, R_GE, &lo, 0), cond_rel(&p, R_LE, &hi, 0));
        } else one = cond_rel(&p, R_EQ, &lo, 0);
        all = all ? cond_bin(C_OR, all, one) : one;
    }
    if (neg) { Cond *n = cond_new(C_NOT); n->a = all; return n; }
    return all;
}

/* the relational operator at the cursor, consumed; -1 when there is none */
static int parse_relop(void)
{
    Tok *t = cur();
    int op = -1;
    if (t->kind == T_OP) {
        if (!strcmp(t->s, "=")) op = R_EQ;
        else if (!strcmp(t->s, "<")) op = R_LT;
        else if (!strcmp(t->s, ">")) op = R_GT;
        else if (!strcmp(t->s, "<=")) op = R_LE;
        else if (!strcmp(t->s, ">=")) op = R_GE;
        else if (!strcmp(t->s, "<>")) op = R_NE;
        if (op >= 0) advance();
    } else if (t->kind == T_WORD) {
        if (!strcmp(t->s, "equal") || !strcmp(t->s, "equals")) { advance(); accept_word("to"); op = R_EQ; }
        else if (!strcmp(t->s, "greater")) {
            advance(); accept_word("than"); op = R_GT;
            if (at_word("or")) { advance(); expect_word("equal"); accept_word("to"); op = R_GE; }
        } else if (!strcmp(t->s, "less")) {
            advance(); accept_word("than"); op = R_LT;
            if (at_word("or")) { advance(); expect_word("equal"); accept_word("to"); op = R_LE; }
        }
    }
    return op;
}

/* Abbreviated combined relation conditions (X3.23 6.5.3): after a
 * relation, AND/OR may be followed by just a relational operator and an
 * object, or by an object alone; the subject -- and, with the object
 * alone, the operator (NOT included when it preceded the operator) --
 * are those of the last relation.  A NOT followed by a relational
 * operator is part of the operator (parse_not leaves it); any other NOT
 * is the logical one. */
static Opnd g_abbr_x; static int g_abbr_op = -1, g_abbr_neg;
static int tok_is_relop(const Tok *t);

static Cond *parse_simple(void)
{
    int line = cur()->line;
    if (cur()->kind == T_WORD) {
        SwitchName *m = switch_find(cur()->s);
        if (m && m->on >= 0) {      /* a switch-status condition-name */
            advance();
            Cond *c = cond_new(C_SWITCH); c->klass = m->sw; c->neg = !m->on;
            return c;
        }
    }
    if (g_abbr_op >= 0 && ((cur()->kind == T_OP && strchr("=<>", cur()->s[0])) || at_word("equal") || at_word("equals") || at_word("greater") || at_word("less") || at_word("is") ||
                           (at_word("not") && tok_is_relop(cur() + 1)))) {
        /* [IS] [NOT] relop object: the last relation's subject */
        accept_word("is");
        int neg = accept_word("not");
        int op = parse_relop();
        if (op < 0) die_at(line, "expected a relational operator, found %s", tok_desc(cur()));
        Opnd y = parse_cond_operand();
        /* the last stated relational operator, NOT included, is the one an
         * object standing alone after this takes: a > b AND NOT < c OR d
         * is ... OR (a NOT < d) (X3.23-1985 VI-61).  It was not recorded,
         * and d took the operator before it (tests/gen found it) */
        g_abbr_op = op; g_abbr_neg = neg;
        return cond_rel(&g_abbr_x, op, &y, neg);
    }
    Opnd x = parse_cond_operand();
    accept_word("is");
    int neg = 0;
    if (accept_word("not")) neg = 1;
    Tok *t = cur();

    if (t->kind == T_WORD) {
        int klass = -1;
        if (!strcmp(t->s, "numeric")) klass = 0;
        else if (!strcmp(t->s, "alphabetic")) klass = 1;
        else if (!strcmp(t->s, "alphabetic-lower")) klass = 2;
        else if (!strcmp(t->s, "alphabetic-upper")) klass = 3;
        else if (g_std >= 2002 && !strcmp(t->s, "boolean")) klass = -2;     /* 2023 8.8.4.4: each position 0 or 1 */
        else if (g_std >= 2002 && !strcmp(t->s, "omitted")) {
            /* the omitted-argument condition (2023 8.8.4.8): a USING
             * parameter whose argument was OMITTED or not passed */
            if (x.kind != O_REF || x.ref.nsub || x.ref.rm || x.ref.sym->parent >= 0 || !x.ref.sym->is_linkage)
                die_at(line, "IS OMITTED tests a level 01 or 77 LINKAGE item, a parameter (2023 8.8.4.8)");
            advance();
            Cond *c = cond_new(C_CLASS); c->x = x; c->klass = -3; c->neg = neg;
            return c;
        }
        if (klass < 0)
            for (int i = 0; i < g_nclass; i++) if (!strcmp(t->s, g_class[i].name)) klass = 4 + i;
        if (klass < 0 && g_std >= 2002)
            /* alphabet-name (2023 8.8.4.4): every character one the alphabet names */
            for (int i = 0; i < g_nalphabet; i++) if (!strcmp(t->s, g_alphabet[i].name)) { klass = 100 + i; g_alphabet[i].used = 1; }
        if (klass >= 0 || klass == -2) {
            if (x.kind == O_FUNC && !opnd_fn_numeric(&x)) {
                /* an alphanumeric or national function's result (2023
                 * 8.8.4.4.3 rule 3): its characters tested */
                advance();
                Cond *c = cond_new(C_CLASS); c->x = x; c->klass = klass; c->neg = neg;
                return c;
            }
            if (x.kind != O_REF) die_at(line, "a class condition needs a data item");
            Sym *cs = x.ref.sym;
            const char *cw = klass == 0 ? "NUMERIC" : klass == 1 ? "ALPHABETIC" : klass == 2 ? "ALPHABETIC-LOWER" : klass == 3 ? "ALPHABETIC-UPPER" : klass == -2 ? "BOOLEAN" : t->s;
            /* 2023 8.8.4.4.3: rule 1, the classes that have no characters
             * to test; rule 3, the character tests want usage DISPLAY or
             * NATIONAL; rules 4-5, not of a numeric, numeric-edited (or,
             * but for BOOLEAN, boolean) item; rule 8, NUMERIC of a DISPLAY
             * or NATIONAL item, or one of category numeric */
            if (cs->strong) die_at(line, "a strongly-typed group takes no class condition (2023 8.8.4.4.3 rule 1)");
            if (cs->usage == U_INDEX || cs->usage == U_POINTER || cs->is_index)
                die_at(line, "the %s item '%s' takes no class condition (2023 8.8.4.4.3 rule 1)", cs->usage == U_POINTER ? "pointer" : "index", cs->name);
            if (cs->is_group && has_odo(cs) && !x.ref.rm) die_at(line, "the variable-length group '%s' takes no class condition (2023 8.8.4.4.3 rule 1)", cs->name);
            int disp = cs->is_group || cs->usage == U_DISPLAY || cs->usage == U_NATIONAL || cs->usage == U_BIT || sym_is_national(cs);
            int numcat = !cs->is_group && (cs->pi.category == PIC_NUMERIC || cs->pi.category == PIC_NUMERIC_EDITED);
            if (klass != 0 && !disp && !x.ref.rm)
                die_at(line, "%s tests characters: '%s' is not of usage DISPLAY or NATIONAL (2023 8.8.4.4.3 rule 3)", cw, cs->name);
            if (klass != 0 && klass != -2 && numcat && !x.ref.rm)
                die_at(line, "%s is no class test for the %s item '%s' (2023 8.8.4.4.3 rule 4)", cw, cs->pi.category == PIC_NUMERIC ? "numeric" : "numeric-edited", cs->name);
            if (klass != 0 && klass != -2 && !cs->is_group && cs->pi.category == PIC_BOOLEAN && !x.ref.rm)
                die_at(line, "%s is no class test for the boolean item '%s' (2023 8.8.4.4.3 rule 4)", cw, cs->name);
            if (klass == -2 && numcat) die_at(line, "BOOLEAN is no class test for the %s item '%s' (2023 8.8.4.4.3 rule 5)", cs->pi.category == PIC_NUMERIC ? "numeric" : "numeric-edited", cs->name);
            if (klass == 0 && !disp && !numcat && !x.ref.rm)
                die_at(line, "NUMERIC tests a DISPLAY or NATIONAL item, or a numeric one: not '%s' (2023 8.8.4.4.3 rule 8)", cs->name);
            if (klass == 0 && !cs->is_group && cs->pi.category == PIC_BOOLEAN && cs->usage == U_BIT && !x.ref.rm)
                die_at(line, "NUMERIC tests a DISPLAY or NATIONAL item, or a numeric one: not the bit item '%s' (2023 8.8.4.4.3 rule 8)", cs->name);
            if (cs->usage == U_DFLOAT && klass != 0) die_at(line, "the floating-point item '%s' takes only the NUMERIC class condition (2023 8.8.4.4.3 rule 3)", cs->name);
            if (cs->usage == U_FLOAT && (klass != 0 || g_std < 2002))
                die_at(line, g_std < 2002 ? "the floating-point item '%s' takes no class condition (Micro Focus: class condition rules)"
                                          : "the floating-point item '%s' takes only the NUMERIC class condition (2023 8.8.4.4.3 rule 3)", cs->name);
            advance();
            cen_flag(x.ref.sym, CEN_CLASS);
            Cond *c = cond_new(C_CLASS); c->x = x; c->klass = klass; c->neg = neg;
            return c;
        }
        int sop = -1;
        if (!strcmp(t->s, "positive")) sop = R_GT;
        else if (!strcmp(t->s, "negative")) sop = R_LT;
        else if (!strcmp(t->s, "zero") || !strcmp(t->s, "zeros") || !strcmp(t->s, "zeroes")) sop = R_EQ;
        if (sop >= 0) {
            if (!opnd_numeric(&x) && !opnd_func_numeric(&x)) {
                if (sop != R_EQ) die_at(line, "a sign condition needs a numeric operand");
                /* alphanumeric compared with ZERO: the figurative */
                advance();
                Opnd z = lit_opnd(t);
                return cond_rel(&x, R_EQ, &z, neg);
            }
            advance();
            if (x.kind == O_REF && (x.ref.sym->usage == U_FLOAT || x.ref.sym->usage == U_DFLOAT) && !x.ref.rm && sop != R_EQ) {
                /* format 2 (2023 8.8.4.7): a floating-point item named
                 * bare is tested by its sign bit -- -0.0 is NEGATIVE, as
                 * are -INF and a NaN with the sign set; in parentheses it
                 * is an expression, tested by value (format 1) */
                Cond *c = cond_new(C_CLASS); c->x = x; c->klass = sop == R_LT ? -4 : -5; c->neg = neg;
                return c;
            }
            Opnd z; memset(&z, 0, sizeof z); z.kind = O_NUM; numlit_zero(&z.num); z.line = line;
            return cond_rel(&x, sop, &z, neg);
        }
    }

    int op = parse_relop();
    if (op < 0 && opnd_is_boolean(&x)) {
        /* a simple boolean condition (2023 8.8.4.3): one boolean position,
         * true when it is 1 */
        int len = x.kind == O_BEXPR ? x.fsize : x.kind == O_STR ? x.tok->len : x.kind == O_FUNC ? x.fsize :
                  x.ref.rm ? (int)x.ref.rm_len : bool_positions(x.ref.sym);
        if (len != 1) die_at(line, "a boolean condition takes a boolean item of one position (2023 8.8.4.3.3 rule 1)");
        Tok *one = xmalloc(sizeof *one); memset(one, 0, sizeof *one);
        one->kind = T_STR; one->s = "1"; one->len = 1; one->boolv = 1; one->line = line;
        Opnd y; memset(&y, 0, sizeof y); y.kind = O_STR; y.tok = one; y.line = line;
        return cond_rel(&x, R_EQ, &y, neg);
    }
    if (op < 0) {
        if (x.kind == O_REF && x.ref.sym->is_cond) return cond_88(&x.ref, neg);
        if (g_abbr_op >= 0)             /* an object alone: the last relation's subject and operator */
            return cond_rel(&g_abbr_x, g_abbr_op, &x, g_abbr_neg ^ neg);
        {
            /* the floating-point conditions of COBOL 2014 (2023 8.8.4.3, 8.8.4.7) */
            static const char *fw[] = { "infinity", "nan", "finite", "normal", "subnormal", "quiet", "signaling", "signalling", NULL };
            for (int k = 0; fw[k]; k++)
                if (at_word(fw[k]) && !sym_lookup_quiet(fw[k]))
                    die_at(line, "the %s condition of a floating-point item is COBOL 2014 (2023 8.8.4); not implemented", cur()->s);
        }
        if (x.kind == O_REF && !neg)
            die_at(line, "expected a relational operator after '%s'", x.ref.sym->name);
        die_at(line, "expected a relational operator, found %s", tok_desc(t));
    }
    Opnd y = parse_cond_operand();

    g_abbr_x = x; g_abbr_op = op; g_abbr_neg = neg;
    rel_rules(&x, &y, line);
    return cond_rel(&x, op, &y, neg);
}

/* is t a relational operator's first word (X3.23-1985 VI-61: GREATER, >,
 * LESS, <, EQUAL, =) */
static int tok_is_relop(const Tok *t)
{
    if (t->kind == T_OP) return strchr("=<>", t->s[0]) != NULL;
    return t->kind == T_WORD && (!strcmp(t->s, "greater") || !strcmp(t->s, "less") ||
                                 !strcmp(t->s, "equal") || !strcmp(t->s, "equals"));
}

static Cond *parse_not(void)
{
    /* in an abbreviated combined relation, a NOT followed by a relational
     * operator is part of the operator, not a logical NOT (X3.23-1985
     * VI-61 rule 1): parse_simple takes it, and records NOT with the
     * operator for the abbreviations that follow it */
    if (at_word("not") && g_abbr_op >= 0 && tok_is_relop(cur() + 1)) return parse_simple();
    if (accept_word("not")) {
        if (at_word("not") && !sym_lookup_quiet("not")) die_at(cur()->line, "NOT NOT is not a permitted pair of elements (2023 8.8.4.11.3, table 5)");
        if ((at_word("and") || at_word("or")) && !sym_lookup_quiet(cur()->s)) die_at(cur()->line, "NOT %s is not a permitted pair of elements (2023 8.8.4.11.3, table 5)", at_word("and") ? "AND" : "OR");
        Cond *c = cond_new(C_NOT); c->a = parse_not(); return c;
    }
    if (cur()->kind == T_LP && paren_is_condition()) {
        advance(); Cond *c = parse_cond();
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')'");
        advance(); return c;
    }
    return parse_simple();
}

static Cond *parse_and(void)
{
    Cond *a = parse_not();
    while (accept_word("and")) a = cond_bin(C_AND, a, parse_not());
    return a;
}

static Cond *parse_cond(void)
{
    int top = g_cond_depth == 0, uc0 = g_nucall;
    if (g_cond_depth++ == 0) g_abbr_op = -1;       /* a new condition: nothing to abbreviate yet */
    Cond *a = parse_and();
    while (accept_word("or")) a = cond_bin(C_OR, a, parse_and());
    if ((at_word("xor") || at_word("exclusive-or")) && !sym_lookup_quiet(cur()->s))
        die_at(cur()->line, "the logical operator %s is COBOL 2023 (8.7.6); not implemented", at_word("xor") ? "XOR" : "EXCLUSIVE-OR");
    if (at_word("not") && !sym_lookup_quiet("not") && (is_word(peek(1), "or") || is_word(peek(1), "and")))
        die_at(cur()->line, "NOT %s is not a permitted pair of elements (2023 8.8.4.11.3, table 5): a condition is followed by AND or OR", is_word(peek(1), "or") ? "OR" : "AND");
    g_cond_depth--;
    if (top && g_nucall > uc0) {
        /* user functions in the condition are called where it is evaluated,
         * which for PERFORM UNTIL or a WHEN is not where it was parsed */
        Cond *r = cond_new(C_AND); *r = *a; r->uc0 = uc0; r->uc1 = g_nucall;
        a = r;
    }
    return a;
}

/* A class condition's operand is plain alphanumeric bytes: an elementary
 * alphanumeric item, or a reference-modified part that is not national,
 * bits, or an occurs-depending group's (the conditions of the direct
 * alphanumeric move, move.h). */
static int class_bytes_ok(const Opnd *o)
{
    if (o->kind != O_REF || o->ref.sym->is_cond) return 0;
    const Ref *r = &o->ref; Sym *s = r->sym;
    if (r->rm) {
        int di = sym_desc(s);               /* first: it may allocate g_desc (the first descriptor of the unit) */
        const Desc *d = &g_desc[di];
        return !r->rm_nat && !r->rm_bit && !r->rm_odo && !r->bitsub && !sym_bitlike(s) && !s->any_len &&
               d->cat != COB_BOOLEAN && d->cat != COB_NATIONAL && d->usage != COB_U_NATIONAL;
    }
    return !is_numeric_sym(s) && !s->is_group && s->pi.category == PIC_ALPHANUMERIC && !sym_bitlike(s) && !s->any_len;
}

/* r1 = 0/1 for a simple condition */
static void emit_cond_value(Cond *c)
{
    if (c->uc1 > c->uc0) emit_ucalls(c->uc0, c->uc1);
    if (c->kind == C_SWITCH) {
        emit_la("r3", "cob_switches");
        emit("\tldw r1, r3+%d", 4 * (c->klass - 1));
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    if (c->kind == C_CLASS && (c->klass == -4 || c->klass == -5)) {
        /* the sign bit of a floating-point item: NEGATIVE when set (-4),
         * POSITIVE when clear (-5) */
        Arg a[1] = { arg_ref(&c->x.ref) };
        emit_args(a, 1);
        emit("\tldbu r1, r3+%d", c->x.ref.sym->fbig ? 0 : c->x.ref.sym->size - 1);   /* the sign bit's byte: the last in the machine's order, the first HIGH-ORDER-LEFT */
        emit("\tsrli r1, r1, 7");
        if ((c->klass == -5) != (c->neg != 0)) emit("\txori r1, r1, 1");
        return;
    }
    if (c->kind == C_CLASS && c->klass == -3) {
        /* IS OMITTED: the parameter's cell holds no address */
        emit_la("r1", g_sym[c->x.ref.sym->record].label);
        emit("\tldw r1, r1+0");
        emit("\tseq r1, r1, r0");
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    if (c->kind == C_CLASS && !g_nohx && c->klass >= 0 && c->klass <= 3 && class_bytes_ok(&c->x)) {
        /* NUMERIC, ALPHABETIC, -LOWER or -UPPER of alphanumeric bytes --
         * an alphanumeric item, or a part of anything that has parts: the
         * test is of characters and no descriptor has anything to add.
         * One character is tested here (a scan asks "is this one a
         * digit" once a character: docs/performance.md); more, or a
         * length only known when running, are the runtime's loop over
         * the bytes, the part's length checked as its descriptor's
         * would have been. */
        const Ref *r = &c->x.ref;
        long n = r->rm ? (long)r->rm_len : (long)r->sym->size;     /* 0: a part of computed or omitted length */
        if (n == 1) {
            Arg a[1] = { arg_ref(r) };
            emit_args(a, 1);
            emit("\tldbu r1, r3+0");
            if (c->klass == 0) { emit("\taddi r1, r1, -48"); emit("\tsltiu r1, r1, 10"); }
            else {
                /* a letter of the class asked for, or a space */
                if (c->klass == 1) { emit("\tori r2, r1, 32"); emit("\taddi r2, r2, -97"); }
                else emit("\taddi r2, r1, %d", c->klass == 2 ? -97 : -65);
                emit("\tsltiu r2, r2, 26");
                emit("\txori r1, r1, 32"); emit("\tsltiu r1, r1, 1");
                emit("\tor r1, r1, r2");
            }
        } else {
            Arg a[3] = { arg_ref(r), n ? arg_imm(n) : arg_rlenc(r), arg_imm(c->klass) };
            emit_args(a, 3);
            emit_call("cob_class_bytes");
        }
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    if (c->kind == C_CLASS) {
        Arg a[3]; Arg d;
        opnd_args(&c->x, &a[0], &d, 0, 0); a[1] = d;
        if (c->klass >= 100) {  /* an alphabet-name: the characters it names */
            a[2] = arg_label(lit_label(g_alphabet[c->klass - 100].member, 256));
            emit_args(a, 3);
            emit_call("cob_class_user");
        } else if (c->klass >= 4) {    /* a SPECIAL-NAMES class: its table */
            a[2] = arg_label(lit_label(g_class[c->klass - 4].tab, 256));
            emit_args(a, 3);
            emit_call("cob_class_user");
        } else {
            a[2] = arg_imm(c->klass == -2 ? 4 : c->klass);       /* -2: BOOLEAN, the runtime's kind 4 */
            emit_args(a, 3);
            emit_call("cob_class");
        }
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    /* C_REL */
    emit_incompat(&c->x); emit_incompat(&c->y);
    if (c->ptr) {
        emit_ptr_value(&c->x, "r1");
        int base = g_slot_base++;
        if (g_slot_base > NSLOTS) die_at(c->x.line, "internal: too many staged operands");
        emit("\tstw sp+%d, r1", SLOT(base));
        emit_ptr_value(&c->y, "r1");
        emit("\tldw r2, sp+%d", SLOT(base));
        g_slot_base = base;
        emit("\t%s r1, r2, r1", c->op == R_EQ ? "seq" : "sne");
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    if (c->x.kind == O_REF && c->x.ref.sym->strong && !c->x.ref.rm) {
        /* two groups of one strong type: element by element, in order
         * (8.8.4.2.12), from a table of each elementary item's offset in
         * the group and descriptor */
        int lab = new_label();
        emit("\t.data");
        emit("\t.p2align 2");
        emit(".L%d:", lab);
        int n = strong_table(c->x.ref.sym, 0);
        emit("\t.text");
        char tl[32]; snprintf(tl, sizeof tl, ".L%d", lab);
        Arg a[4] = { arg_ref(&c->x.ref), arg_ref(&c->y.ref), arg_label(tl), arg_imm(n) };
        emit_args(a, 4);
        emit_call("cob_cmp_struct");
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r1, r0"); break;
        case R_NE: emit("\tsne r1, r1, r0"); break;
        case R_LT: emit("\tslt r1, r1, r0"); break;
        case R_GT: emit("\tsgt r1, r1, r0"); break;
        case R_LE: emit("\tsle r1, r1, r0"); break;
        case R_GE: emit("\tsge r1, r1, r0"); break;
        }
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    if (c->x.kind == O_BEXPR || c->y.kind == O_BEXPR || c->bstack) {
        bool_push(&c->x);
        bool_push(&c->y);
        emit_call("cob_bcmp");
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r1, r0"); break;
        case R_NE: emit("\tsne r1, r1, r0"); break;
        case R_LT: emit("\tslt r1, r1, r0"); break;
        case R_GT: emit("\tsgt r1, r1, r0"); break;
        case R_LE: emit("\tsle r1, r1, r0"); break;
        case R_GE: emit("\tsge r1, r1, r0"); break;
        }
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    if (c->x.kind == O_EXPR || c->y.kind == O_EXPR) {
        Opnd xy[2] = { c->x, c->y };
        int was = g_wide;
        g_wide = was || opnds_wide(xy, 2);
        emit_push_opnd(&c->x);
        emit_push_opnd(&c->y);
        emit_call("cob_ncmp");
        g_wide = was; if (!was) g_fstmt = g_qstmt = 0;
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r1, r0"); break;
        case R_NE: emit("\tsne r1, r1, r0"); break;
        case R_LT: emit("\tslt r1, r1, r0"); break;
        case R_GT: emit("\tsgt r1, r1, r0"); break;
        case R_LE: emit("\tsle r1, r1, r0"); break;
        case R_GE: emit("\tsge r1, r1, r0"); break;
        }
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    if (cmp_is_onebyte(&c->x, &c->y)) {
        emit_onebyte_value(&c->x);
        emit("\tstw sp+%d, r1", SLOT_A);
        emit_onebyte_value(&c->y);
        emit("\tldw r2, sp+%d", SLOT_A);
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r2, r1"); break;
        case R_NE: emit("\tsne r1, r2, r1"); break;
        case R_LT: emit("\tsltu r1, r2, r1"); break;
        case R_GT: emit("\tsgtu r1, r2, r1"); break;
        case R_LE: emit("\tsleu r1, r2, r1"); break;
        case R_GE: emit("\tsgeu r1, r2, r1"); break;
        }
    } else if (!g_nohx && (cmp_is_bytewise(&c->x, &c->y) || cmp_is_rm_lit(&c->x, &c->y)) && (c->op == R_EQ || c->op == R_NE) &&
               (c->x.kind == O_REF && !c->x.ref.rm ? (long)c->x.ref.sym->size : c->x.kind == O_REF ? (long)c->x.ref.rm_len : c->x.tok->len) <= 16) {
        /* equality of at most 16 bytes: the chunks' xor, no call (unaligned
         * loads are SLOW-32's: docs/ ruling) -- a serial SEARCH compares a
         * short key per entry */
        long n = c->x.kind == O_REF && !c->x.ref.rm ? (long)c->x.ref.sym->size : c->x.kind == O_REF ? (long)c->x.ref.rm_len : c->x.tok->len;
        if (g_slot_base >= NSLOTS) die_at(c->x.line, "internal: no frame slot for a compare");
        int t = g_slot_base++;
        Opnd *ox = &c->x, *oy = &c->y;
        int same = ox->kind == O_REF && oy->kind == O_REF && cmp_is_bytewise(ox, oy);
        if (same) { cen_same(ox->ref.sym, oy->ref.sym); g_cen_hold++; }     /* two items of one description: equal bytes, equal values -- if both are written one way */
        if (ox->kind == O_REF) emit_ref_addr(&ox->ref, "r3"); else emit_la("r3", lit_label((const unsigned char *)ox->tok->s, ox->tok->len));
        emit("\tstw sp+%d, r3", SLOT(t));
        if (oy->kind == O_REF) emit_ref_addr(&oy->ref, "r4"); else emit_la("r4", lit_label((const unsigned char *)oy->tok->s, oy->tok->len));
        emit("\tldw r3, sp+%d", SLOT(t));
        if (same) g_cen_hold--;
        g_slot_base--;
        emit("\tadd r1, r0, r0");
        for (long o = 0; o < n; ) {
            const char *ld = n - o >= 4 ? "ldw" : n - o >= 2 ? "ldhu" : "ldbu";
            int w = n - o >= 4 ? 4 : n - o >= 2 ? 2 : 1;
            emit("\t%s r5, r3+%ld", ld, o); emit("\t%s r6, r4+%ld", ld, o);
            emit("\txor r5, r5, r6"); emit("\tor r1, r1, r5");
            o += w;
        }
        emit(c->op == R_EQ ? "\tseq r1, r1, r0" : "\tsne r1, r1, r0");
    } else if (cmp_is_bytewise(&c->x, &c->y) || cmp_is_rm_lit(&c->x, &c->y)) {
        Arg a[3];
        for (int k = 0; k < 2; k++) {
            Opnd *o = k ? &c->y : &c->x;
            a[k] = o->kind == O_REF ? arg_ref(&o->ref) : arg_label(lit_label((const unsigned char *)o->tok->s, o->tok->len));
        }
        a[2] = arg_imm(c->x.kind == O_REF && !c->x.ref.rm ? (long)c->x.ref.sym->size : c->x.kind == O_REF ? (long)c->x.ref.rm_len : c->x.tok->len);
        int same = cmp_is_bytewise(&c->x, &c->y);
        if (same) { cen_same(c->x.ref.sym, c->y.ref.sym); g_cen_hold++; }
        emit_args(a, 3);
        if (same) g_cen_hold--;
        emit_call("memcmp");
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r1, r0"); break;
        case R_NE: emit("\tsne r1, r1, r0"); break;
        case R_LT: emit("\tslt r1, r1, r0"); break;
        case R_GT: emit("\tsgt r1, r1, r0"); break;
        case R_LE: emit("\tsle r1, r1, r0"); break;
        case R_GE: emit("\tsge r1, r1, r0"); break;
        }
    } else if (opnd_hot_cmp(&c->x) && opnd_hot_cmp(&c->y)) {
        /* unsigned ordering whenever neither side can be negative: it is
         * equally correct for the operands a signed compare would also have
         * handled, and it is the only correct one when a four-byte unsigned
         * item uses the top bit.  EQ and NE do not care either way. */
        int u = opnd_nonneg(&c->x) && opnd_nonneg(&c->y);
        /* r2 = x, r1 = y; a constant side needs no spill (a PERFORM or
         * SEARCH limit is one on every iteration) */
        int ky = !g_nohx && (c->y.kind == O_NUM || c->y.kind == O_FIG), kx = !g_nohx && (c->x.kind == O_NUM || c->x.kind == O_FIG);
        if (ky) {
            emit_cmp_value(&c->x);
            emit("\tadd r2, r1, r0");
            emit_li("r1", c->y.kind == O_NUM ? (long)numlit_int(&c->y.num) : 0);
        } else if (kx) {
            emit_cmp_value(&c->y);
            emit_li("r2", c->x.kind == O_NUM ? (long)numlit_int(&c->x.num) : 0);
        } else {
            emit_cmp_value(&c->x);
            emit("\tstw sp+%d, r1", SLOT_A);
            emit_cmp_value(&c->y);
            emit("\tldw r2, sp+%d", SLOT_A);
        }
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r2, r1"); break;
        case R_NE: emit("\tsne r1, r2, r1"); break;
        case R_LT: emit(u ? "\tsltu r1, r2, r1" : "\tslt r1, r2, r1"); break;
        case R_GT: emit(u ? "\tsgtu r1, r2, r1" : "\tsgt r1, r2, r1"); break;
        case R_LE: emit(u ? "\tsleu r1, r2, r1" : "\tsle r1, r2, r1"); break;
        case R_GE: emit(u ? "\tsgeu r1, r2, r1" : "\tsge r1, r2, r1"); break;
        }
    } else {
        Arg a[4];
        /* a figurative constant or ALL literal against a national operand
         * is national itself: HIGH-VALUE is U+FFFF, not the byte FF */
        if (opnd_is_national(&c->x)) nat_fig_opnd(&c->y, opnd_size_bound(&c->x));
        if (opnd_is_national(&c->y)) nat_fig_opnd(&c->x, opnd_size_bound(&c->y));
        /* two numbers -- numeric items, numeric literals, ZERO beside one of
         * them (the number: opnd_args): cob_cmp compares their values, and
         * the lengths asked for below expand nothing */
        int vx = c->x.kind == O_REF && !c->x.ref.rm && is_numeric_sym(c->x.ref.sym);
        int vy = c->y.kind == O_REF && !c->y.ref.rm && is_numeric_sym(c->y.ref.sym);
        int zx = c->x.kind == O_FIG && !strncmp(c->x.tok->s, "zero", 4), zy = c->y.kind == O_FIG && !strncmp(c->y.tok->s, "zero", 4);
        int numbers = (vx || vy) && (vx || c->x.kind == O_NUM || zx) && (vy || c->y.kind == O_NUM || zy);
        if (numbers) g_cen_quiet++;
        int xs = opnd_size_bound(&c->x), ys = opnd_size_bound(&c->y);
        if (numbers) g_cen_quiet--;
        int xn = opnd_numeric(&c->x) || opnd_func_numeric(&c->x), yn = opnd_numeric(&c->y) || opnd_func_numeric(&c->y);
        opnd_args(&c->x, &a[0], &a[1], ys, yn);
        opnd_args(&c->y, &a[2], &a[3], xs, xn);
        emit_args(a, 4);
        if (numbers) {
            if (vx) cen_bless(c->x.ref.sym);
            if (vy) cen_bless(c->y.ref.sym);
        }
        emit_call("cob_cmp");
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r1, r0"); break;
        case R_NE: emit("\tsne r1, r1, r0"); break;
        case R_LT: emit("\tslt r1, r1, r0"); break;
        case R_GT: emit("\tsgt r1, r1, r0"); break;
        case R_LE: emit("\tsle r1, r1, r0"); break;
        case R_GE: emit("\tsge r1, r1, r0"); break;
        }
    }
    if (c->neg) emit("\txori r1, r1, 1");
}

static void cond_jump_true(Cond *c, int L);

static void cond_jump_false(Cond *c, int L)
{
    /* its user functions called once, here: the rest of the condition
     * without them (emit_cond_value would call them again) */
    Cond d;
    if (c->uc1 > c->uc0) { emit_ucalls(c->uc0, c->uc1); d = *c; d.uc0 = d.uc1 = 0; c = &d; }
    switch (c->kind) {
    case C_AND: cond_jump_false(c->a, L); cond_jump_false(c->b, L); return;
    case C_OR: { int Lt = new_label(); cond_jump_true(c->a, Lt); cond_jump_false(c->b, L); emit_label(Lt); return; }
    case C_NOT: cond_jump_true(c->a, L); return;
    default: emit_cond_value(c); emit("\tbeq r1, r0, .L%d", L); return;
    }
}

static void cond_jump_true(Cond *c, int L)
{
    /* its user functions called once, here: the rest of the condition
     * without them (emit_cond_value would call them again) */
    Cond d;
    if (c->uc1 > c->uc0) { emit_ucalls(c->uc0, c->uc1); d = *c; d.uc0 = d.uc1 = 0; c = &d; }
    switch (c->kind) {
    case C_AND: { int Ls = new_label(); cond_jump_false(c->a, Ls); cond_jump_true(c->b, L); emit_label(Ls); return; }
    case C_OR: cond_jump_true(c->a, L); cond_jump_true(c->b, L); return;
    case C_NOT: cond_jump_false(c->a, L); return;
    default: emit_cond_value(c); emit("\tbne r1, r0, .L%d", L); return;
    }
}
