/* s32-cobc: arithmetic expressions.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- arithmetic expressions: COMPUTE and condition operands ----------- */

static void parse_expr(void);

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

static void parse_primary(void)
{
    Tok *t = cur();
    if (t->kind == T_LP) {
        advance(); parse_expr();
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' in the expression");
        advance();
        return;
    }
    if (at_op("+")) { advance(); parse_primary(); return; }
    if (at_op("-")) { advance(); parse_primary(); emit_call("cob_nneg"); return; }
    Opnd o; parse_operand(&o);
    check_numeric_opnd(&o);
    { int in = 0, fr = 0; opnd_int_frac(&o, &in, &fr); xd_push(in, fr); }
    emit_push(&o);
}

/* consecutive exponentiations left to right, as every level: 2 ** 3 ** 2
 * is 64 (X3.23-1985 6.2.3 (2); 2023 8.8.1.2 rule 3; MF's reference says
 * the same; GnuCOBOL takes it right to left, 512) */
static void parse_power(void)
{
    parse_primary();
    while (at_op("**")) { advance(); parse_primary(); xd_binop('^'); emit_call("cob_npow"); }
}

static void parse_term(void)
{
    parse_power();
    while (at_op("*") || at_op("/")) {
        int mul = at_op("*"); advance();
        parse_power();
        xd_binop(mul ? '*' : '/');
        emit_call(mul ? "cob_nmul" : "cob_ndiv");
    }
}

static int g_xd_depth;
static void parse_expr(void)
{
    if (g_xd_depth++ == 0) g_xd_sp = 0;         /* a whole expression: the digit stack starts empty */
    parse_term();
    while (at_op("+") || at_op("-")) {
        int add = at_op("+"); advance();
        parse_term();
        xd_binop('+');
        emit_call(add ? "cob_nadd" : "cob_nsub");
    }
    g_xd_depth--;
}

/* an expression operand in a condition: scanned now, emitted later */
static Opnd expr_opnd(void)
{
    Opnd o; memset(&o, 0, sizeof o);
    o.kind = O_EXPR; o.line = cur()->line; o.e_start = g_tp;
    int saw = g_saw_wide, sawf = g_saw_float; g_saw_wide = 0; g_saw_float = 0;
    g_noemit++; parse_expr(); g_noemit--;
    o.wide = g_saw_wide; g_saw_wide |= saw;
    o.flt = g_saw_float; g_saw_float |= sawf;
    o.e_end = g_tp;
    return o;
}

static void emit_expr_tokens(int s0, int s1)
{
    int save = g_tp;
    g_tp = s0;
    g_incompat_push++;          /* its operands are sending items (14.6.13.2 rule 2) */
    parse_expr();
    g_incompat_push--;
    if (g_tp != s1) die_at(g_tok[s0].line, "internal: expression re-parse drifted");
    g_tp = save;
}

static void emit_push_opnd(Opnd *o)
{
    if (o->kind != O_EXPR) { emit_push(o); return; }
    emit_expr_tokens(o->e_start, o->e_end);
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

static void parse_compute(void)
{
    Ref rs[MAXOPS]; int rd[MAXOPS];
    int nr = parse_ref_list(rs, rd, MAXOPS, 2);
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
    /* wide or not: known from a pass that emits nothing, then for real */
    {
        int start = g_tp, saw = g_saw_wide, sawf = g_saw_float; g_saw_wide = 0; g_saw_float = 0;
        g_xd_div = 0;
        g_noemit++; parse_expr(); g_noemit--;
        g_wide = g_saw_wide || g_saw_float || refs_wide(rs, nr) || (g_xd_div && round_wide(rs, rd, nr));
        if (g_saw_float) g_fstmt = 1;
        g_saw_wide = saw; g_saw_float = sawf; g_tp = start;
    }
    if (!g_wide) {                              /* integers in a word, where that is the stack's answer */
        int start = g_tp;
        g_noemit++; g_nhn = 0; int root = hx_expr(); g_noemit--;
        int size_err = at_size_error_clause() || ec_size_on();
        long long bd; int nn;
        int mode = root >= 0 ? hx_ok(root, rs, rd, nr, NULL, size_err, &bd, &nn) : 0;
        if (mode) {
            int Lslow = mode == 2 ? new_label() : -1;
            hx_store(root, rs, rd, nr, NULL, bd, nn, Lslow);
            if (mode == 2) {                    /* a word overflowed: the stack, from the start */
                int Ldone = new_label(), end = g_tp;
                emit_jump(Ldone); emit_label(Lslow);
                g_tp = start;
                g_incompat_push++; parse_expr(); g_incompat_push--;
                if (g_tp != end) die_at(rs[0].line, "internal: COMPUTE re-parse drifted");
                emit_store_receivers(rs, rd, nr, 0, 1, 0, 0, -1, 0);
                emit_label(Ldone);
            }
            parse_size_error_clauses(size_err, "end-compute");
            return;
        }
        g_tp = start;
        g_noemit++; g_nhn = 0; root = dx_expr(); g_noemit--;
        size_err = at_size_error_clause() || ec_size_on();   /* where this parse stopped: the integer one may have quit early */
        if (dx_ok(root, rs, nr, size_err)) {           /* decimals in registers */
            dx_store(root, rs, rd, nr);
            parse_size_error_clauses(size_err, "end-compute");
            return;
        }
        g_tp = start;
    }
    g_incompat_push++;
    parse_expr();
    g_incompat_push--;
    int size_err = at_size_error_clause() || ec_size_on();
    emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err, -1, 0);
    g_wide = 0; g_fstmt = 0;
    parse_size_error_clauses(size_err, "end-compute");
}
