/* s32-cobc: arithmetic expressions.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- arithmetic expressions: COMPUTE and condition operands ----------- */

static int at_arith_op(void)
{
    return at_op("+") || at_op("-") || at_op("*") || at_op("/") || at_op("**");
}

/* the digits each value on the evaluation stack can need, in step with it:
 * a product the narrow stack would compute past 18 digits makes the
 * expression wide (g_saw_wide), since cob_nmul would shed its operands'
 * fraction digits to fit 64 bits.  An intermediate's precision is the
 * implementor's, but losing the eighth significant digit of a result
 * the receiver keeps is not a choice worth having: K * L * M over three
 * 9-digit items gave -513019436.446481 for -513019442.647578 (tests/gen
 * found it; tests/free/computewide).  A sum takes the larger operand and
 * a carry; a quotient and a power keep the implementor's precision. */
#define XD_MAX 64
static int g_xd_sp, g_xd_int[XD_MAX], g_xd_frac[XD_MAX];
static int g_xd_div;                 /* the expression divides: ROUNDED into 18 digits needs the wide stack (round_wide) */
static void xd_push(int in, int fr)
{
    if (g_xd_sp < XD_MAX) { g_xd_int[g_xd_sp] = in; g_xd_frac[g_xd_sp] = fr; }
    g_xd_sp++;
}
static void xd_binop(char op)
{
    if (op == '/') g_xd_div = 1;
    if (g_xd_sp < 2) { g_xd_sp = 0; return; }
    g_xd_sp--;
    if (g_xd_sp >= XD_MAX) return;
    int b = g_xd_sp, a = g_xd_sp - 1, in, fr;
    if (op == '*') { in = g_xd_int[a] + g_xd_int[b]; fr = g_xd_frac[a] + g_xd_frac[b]; }
    else if (op == '+') {
        in = (g_xd_int[a] > g_xd_int[b] ? g_xd_int[a] : g_xd_int[b]) + 1;
        fr = g_xd_frac[a] > g_xd_frac[b] ? g_xd_frac[a] : g_xd_frac[b];
    } else if (op == '/') { in = g_xd_int[a] + g_xd_frac[b]; fr = g_xd_frac[a]; }
    else { in = g_xd_int[a]; fr = g_xd_frac[a]; }              /* a power: as the base */
    if ((op == '*' || op == '+') && in + fr > 18) g_saw_wide = 1;
    g_xd_int[a] = in; g_xd_frac[a] = fr;
}

static Expr *ex_node(char op, Expr *l, Expr *r)
{
    Expr *e = ex_alloc(sizeof *e);
    e->op = op; e->l = l; e->r = r;
    return e;
}

/* an operand already read, which the expression about to be parsed
 * begins with (expr_opnd_after), and its first token */
static Opnd *g_ex_first; static int g_ex_first_tp;

/* parsing emits as it goes (a COMPUTE's final pass), or nothing under
 * g_noemit (a scan); either way it returns the tree */
static Expr *parse_primary(void)
{
    if (g_ex_first) {                           /* the first operand, read already */
        Expr *e = ex_node(0, NULL, NULL);
        e->o = ex_alloc(sizeof *e->o); e->tp = g_ex_first_tp;
        *e->o = *g_ex_first; g_ex_first = NULL;
        Opnd *o = e->o;
        check_numeric_opnd(o);
        { int in = 0, fr = 0; opnd_int_frac(o, &in, &fr); xd_push(in, fr); }
        emit_push(o);
        return e;
    }
    Tok *t = cur();
    if (t->kind == T_LP) {
        advance(); Expr *e = parse_expr();
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' in the expression");
        advance();
        return e;
    }
    if (at_op("+")) { advance(); return parse_primary(); }
    if (at_op("-")) { advance(); Expr *e = parse_primary(); emit_call("cob_nneg"); return ex_node('n', e, NULL); }
    Expr *e = ex_node(0, NULL, NULL);
    e->o = ex_alloc(sizeof *e->o); e->tp = g_tp;
    Opnd *o = e->o; parse_operand(o);
    check_numeric_opnd(o);
    { int in = 0, fr = 0; opnd_int_frac(o, &in, &fr); xd_push(in, fr); }
    emit_push(o);
    return e;
}

/* consecutive exponentiations left to right, as every level: 2 ** 3 ** 2
 * is 64 (X3.23-1985 6.2.3 (2); 2023 8.8.1.2 rule 3; MF's reference says
 * the same; GnuCOBOL takes it right to left, 512) */
static Expr *parse_power(void)
{
    Expr *e = parse_primary();
    while (at_op("**")) { advance(); Expr *r = parse_primary(); xd_binop('^'); emit_call("cob_npow"); e = ex_node('^', e, r); }
    return e;
}

static Expr *parse_term(void)
{
    Expr *e = parse_power();
    while (at_op("*") || at_op("/")) {
        int mul = at_op("*"); advance();
        Expr *r = parse_power();
        xd_binop(mul ? '*' : '/');
        emit_call(mul ? "cob_nmul" : "cob_ndiv");
        e = ex_node(mul ? '*' : '/', e, r);
    }
    return e;
}

static int g_xd_depth;
static Expr *parse_expr(void)
{
    if (g_xd_depth++ == 0) g_xd_sp = 0;         /* a whole expression: the digit stack starts empty */
    Expr *e = parse_term();
    while (at_op("+") || at_op("-")) {
        int add = at_op("+"); advance();
        Expr *r = parse_term();
        xd_binop('+');
        emit_call(add ? "cob_nadd" : "cob_nsub");
        e = ex_node(add ? '+' : '-', e, r);
    }
    g_xd_depth--;
    return e;
}

/* an expression read now and emitted later: its tree, and whether it
 * holds an operand past 18 digits or a float (what the wide stack is
 * for) -- known for itself, and seen by what it is part of as before */
static Expr *scan_expr(void)
{
    int sw = g_saw_wide, sf = g_saw_float, sq = g_saw_qfloat; g_saw_wide = g_saw_float = g_saw_qfloat = 0;
    g_noemit++; Expr *e = parse_expr(); g_noemit--;
    e->wide = g_saw_wide; e->flt = g_saw_float; e->qflt = g_saw_qfloat;
    g_saw_wide |= sw; g_saw_float |= sf; g_saw_qfloat |= sq;
    /* in a statement that emits as it reads, its user functions are called
     * now, where they are written (or queued with the condition being
     * read), not each time the expression's code is made; a scan's wait
     * for the statement (ucall_make) */
    expr_calls(e);
    return e;
}

/* an expression operand in a condition: scanned now, emitted later */
static Opnd expr_opnd(void)
{
    Opnd o; memset(&o, 0, sizeof o);
    o.kind = O_EXPR; o.line = cur()->line;
    o.ex = scan_expr();
    o.wide = o.ex->wide; o.flt = o.ex->flt; o.qflt = o.ex->qflt;
    return o;
}

/* an operand just read, and an arithmetic operator after it: the
 * expression it begins, read on from there -- not again from its first
 * token, which would read the operand twice (and make a user function's
 * call twice) */
static Opnd expr_opnd_after(const Opnd *first, int start)
{
    Opnd f = *first;
    g_ex_first = &f; g_ex_first_tp = start;
    Opnd o = expr_opnd();
    if (g_ex_first) die_at(g_tok[start].line, "internal: an expression did not take its first operand");
    o.line = g_tok[start].line;
    return o;
}

/* the code parse_expr would have emitted for e, side effects and all:
 * the digit stack, the width it notes, a user function's call */
static void emit_expr_node(Expr *e)
{
    if (!e->op) {
        ucall_make(e->o);                       /* a call still waiting (ALLOCATE's size): made here, once */
        Opnd o = *e->o;
        { int in = 0, fr = 0; opnd_int_frac(&o, &in, &fr); xd_push(in, fr); }
        emit_push(&o);
        return;
    }
    if (e->op == 'n') { emit_expr_node(e->l); emit_call("cob_nneg"); return; }
    emit_expr_node(e->l);
    emit_expr_node(e->r);
    switch (e->op) {
    case '^': xd_binop('^'); emit_call("cob_npow"); break;
    case '*': xd_binop('*'); emit_call("cob_nmul"); break;
    case '/': xd_binop('/'); emit_call("cob_ndiv"); break;
    case '+': xd_binop('+'); emit_call("cob_nadd"); break;
    default:  xd_binop('+'); emit_call("cob_nsub"); break;
    }
}
static void emit_expr(Expr *e)
{
    if (g_xd_depth++ == 0) g_xd_sp = 0;
    g_incompat_push++;          /* its operands are sending items (14.6.13.2 rule 2) */
    emit_expr_node(e);
    g_incompat_push--;
    g_xd_depth--;
}

/* the data items an expression names, its operands' subscripts and
 * reference modifiers and function arguments included: f on each, until
 * it returns nonzero */
static int sym_is(const Sym *s, const void *cx) { return s == cx; }
static int ref_names(const Ref *r, SymVisit f, const void *cx)
{
    if (!r->sym) return 0;
    if (f(r->sym, cx)) return 1;
    for (int k = 0; k < r->nsub; k++) {
        if (r->sub[k].sym == &g_subx) { if (expr_names(r->sub[k].x, f, cx)) return 1; }
        else if (r->sub[k].sym && f(r->sub[k].sym, cx)) return 1;
    }
    return (r->rm_sx && expr_names(r->rm_sx, f, cx)) || (r->rm_lx && expr_names(r->rm_lx, f, cx));
}
static int opnd_names(const Opnd *o, SymVisit f, const void *cx)
{
    switch (o->kind) {
    case O_REF: case O_ADDR: return ref_names(&o->ref, f, cx);
    case O_EXPR: return expr_names(o->ex, f, cx);
    case O_FUNC:
        if ((o->farg && opnd_names(o->farg, f, cx)) || (o->farg2 && opnd_names(o->farg2, f, cx))) return 1;
        for (int k = 0; k < o->nfargs; k++) if (opnd_names(o->fargs[k], f, cx)) return 1;
        return (o->fsx && expr_names(o->fsx, f, cx)) || (o->flx && expr_names(o->flx, f, cx));
    default: return 0;
    }
}
static int expr_names(const Expr *e, SymVisit f, const void *cx)
{
    if (!e->op) return opnd_names(e->o, f, cx);
    return expr_names(e->l, f, cx) || (e->r && expr_names(e->r, f, cx));
}

static void emit_item_addr(const char *reg, Sym *s, int off);
static void emit_push_opnd(Opnd *o)
{
    if (o->kind != O_EXPR) { emit_push(o); return; }
    if (o->nsave) {                             /* its value, kept from the one evaluation */
        emit_item_addr("r3", o->nsave, o->nsave->offset);
        emit_call("cob_npush_saved");
        return;
    }
    emit_expr(o->ex);
}

/* does the parenthesis at the cursor open a condition or an expression? */
static int paren_is_condition(void)
{
    int depth = 0, words = 0;
    Tok *only = NULL;
    for (int i = g_tp; i < g_ntok; i++) {
        Tok *t = &g_tok[i];
        if (t->kind == T_LP) depth++;
        else if (t->kind == T_RP) { if (--depth == 0) break; }
        else if (t->kind == T_OP && (!strcmp(t->s, "=") || !strcmp(t->s, "<") || !strcmp(t->s, ">") ||
                 !strcmp(t->s, "<=") || !strcmp(t->s, ">=") || !strcmp(t->s, "<>"))) return 1;
        else if (t->kind == T_WORD) {
            static const char *cw[] = { "is", "not", "and", "or", "equal", "equals", "greater", "less",
                "than", "numeric", "alphabetic", "alphabetic-lower", "alphabetic-upper", "positive", "negative", NULL };
            for (int k = 0; cw[k]; k++) if (!strcmp(t->s, cw[k])) return 1;
            /* the 2002 tests, and a class-name: no expression has them
             * -- ( a OMITTED ) is a condition (X-COBOL's cobcurses) */
            if (g_std >= 2002 && (!strcmp(t->s, "omitted") || !strcmp(t->s, "boolean"))) return 1;
            for (int k = 0; k < g_nclass; k++) if (!strcmp(t->s, g_class[k].name)) return 1;
            words++; only = t;
        }
        else if (t->kind == T_PERIOD || t->kind == T_EOF) break;
    }
    /* (cond-name) alone is a condition */
    if (words == 1 && only) {
        for (int i = g_sym_base; i < g_nsym; i++) if (g_sym[i].is_cond && !strcmp(g_sym[i].name, only->s)) return 1;
    }
    return 0;
}

static int lw_compute(Ref *rs, int *rd, int nr, Expr *e, int size_err);     /* lower.h */
static void parse_compute(void)
{
    Ref rs[MAXOPS]; int rd[MAXOPS];
    g_noemit++;                                 /* receivers: their calls wait for the store (recv_calls) */
    int nr = parse_ref_list(rs, rd, MAXOPS, 2);
    g_noemit--;
    if (!nr) die_at(cur()->line, "COMPUTE needs a receiving item");
    if (!at_op("=")) die_at(cur()->line, "expected '=' in COMPUTE, found %s", tok_desc(cur()));
    advance();
    int nb = 0;
    for (int i = 0; i < nr; i++) nb += sym_is_boolean(rs[i].sym);
    if (nb) {
        /* a boolean-compute (2023 14.9.8, format 2): the expression's
         * value stored in each receiver by the MOVE rules */
        if (nb != nr) die_at(rs[0].line, "COMPUTE: boolean and numeric receivers cannot be mixed (2023 14.9.8.3)");
        for (int i = 0; i < nr; i++) if (rd[i]) die_at(rs[i].line, "ROUNDED does not apply to a boolean receiver");
        parse_bexpr();
        if (g_bexpr_all) die_at(rs[0].line, "a boolean COMPUTE's expression cannot be an ALL literal alone (2023 14.9.8.3 rule 3)");
        for (int i = 0; i < nr; i++) {
            Arg a[2] = { arg_ref(&rs[i]), rs[i].rm ? (rs[i].rm_len ? arg_desc(bool_desc((int)rs[i].rm_len)) : arg_rdesc(&rs[i])) : arg_desc(sym_desc(rs[i].sym)) };
            emit_args(a, 2);
            emit_call("cob_bstore");
        }
        emit_call("cob_bdrop");
        accept_word("end-compute");
        return;
    }
    /* the expression read once: whether it needs the wide stack, then the
     * code -- in registers where that is the stack's answer, else the
     * stack's -- all from the one tree */
    int saw = g_saw_wide, sawf = g_saw_float, sawq = g_saw_qfloat; g_saw_wide = 0; g_saw_float = 0; g_saw_qfloat = 0;
    g_xd_div = 0;
    g_noemit++; Expr *e = parse_expr(); g_noemit--;
    expr_calls(e);                              /* its user functions, before any path's code */
    int rw = refs_wide(rs, nr);                 /* first: it marks a float or software-float receiver's statement */
    int wide = g_saw_wide || g_saw_float || g_saw_qfloat || rw || g_arith_sd || (g_xd_div && round_wide(rs, rd, nr)), flt = (g_saw_float || g_fstmt) && !g_arith_sd;
    if (g_arith_sd) g_fstmt = 0;
    if (g_saw_qfloat || g_arith_sd) g_qstmt = 1;
    g_saw_wide = saw; g_saw_float = sawf; g_saw_qfloat = sawq;
    /* the SIZE ERROR phrases, before any code (their statements must not
     * leave this statement's ROUNDED MODE or width behind them) */
    int size_err = at_size_error_clause() || ec_size_on();
    int rmode = g_rmode; SizePh ph; parse_size_phrases(&ph, size_err, "end-compute"); g_rmode = rmode;
    g_wide = wide;
    if (flt) g_fstmt = 1;
    g_hn_wants_chk = 0;
    lw_compute(rs, rd, nr, e, size_err);        /* an island's too (lower.h): its placeholder, then the text */
    if (!g_wide) {                              /* integers in a word */
        g_nhn = 0; int root = hn_tree(e, hx_leaf);
        long long bd; int nn;
        int mode = root >= 0 ? hx_ok(root, rs, rd, nr, NULL, size_err, &bd, &nn) : 0;
        if (mode) {
            int Lslow = mode == 2 ? new_label() : -1;
            hx_store(root, rs, rd, nr, NULL, bd, nn, Lslow);
            if (mode == 2) {                    /* a word overflowed: the stack, from the start */
                int Ldone = new_label();
                emit_jump(Ldone); emit_label(Lslow);
                emit_expr(e);
                emit_store_receivers(rs, rd, nr, 0, 1, 0, 0, -1, 0);
                emit_label(Ldone);
            }
            emit_size_phrases(&ph);
            return;
        }
        g_nhn = 0; root = hn_tree(e, dx_leaf);
        if (dx_ok(root, rs, nr, size_err)) {           /* decimals in registers */
            dx_store(root, rs, rd, nr);
            emit_size_phrases(&ph);
            return;
        }
    }
    if ((g_wide || g_hn_wants_chk) && !g_nohx && !flt && !refs_wide(rs, nr) && !(g_xd_div && round_wide(rs, rd, nr))) {
        /* wide only because an intermediate could pass 18 digits by the
         * pictures -- or not wide, but a MOD or REM by an item, which only
         * the checked path takes: in 64 bits with tests, the stack's code
         * behind them (checked arithmetic, arith_reg.h) */
        int was_wide = g_wide;
        g_wide = 0;
        g_dx_chk = 1; g_dx_tests = 0;
        g_nhn = 0; int root = hn_tree(e, dx_leaf);
        int ok = dx_ok(root, rs, nr, size_err);
        g_dx_chk = 0;
        if (ok) {
            if (!g_dx_tests) dx_store(root, rs, rd, nr);        /* the bounds prove it after all */
            else {
                int Lslow = new_label(), Ldone = new_label();
                g_dx_slow = Lslow;
                dx_store(root, rs, rd, nr);
                g_dx_slow = -1;
                emit_jump(Ldone);
                emit_label(Lslow);
                g_wide = was_wide;
                emit_expr(e);
                emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err, -1, 0);
                g_wide = 0; g_fstmt = g_qstmt = 0;
                emit_label(Ldone);
            }
            emit_size_phrases(&ph);
            return;
        }
        g_wide = was_wide;
    }
    emit_expr(e);
    emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err, -1, 0);
    g_wide = 0; g_fstmt = g_qstmt = 0;
    emit_size_phrases(&ph);
}
