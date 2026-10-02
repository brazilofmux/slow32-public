/* s32-cobc: statement dispatch.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- dispatch ---------------------------------------------------------- */

static void parse_raise(void);
static void parse_statement_1(void);

/* a statement, then the EC-DATA-CONVERSION its conversion functions noted */
static int g_para_body_tp = -1;     /* where the current paragraph's first sentence begins */
static int cur_use_is_global(void)
{
    for (int u = 0; u < g_nuse; u++)
        if (g_use[u].unit == g_unit && g_use[u].sec == g_cur_sec_id && g_use[u].global) return 1;
    return 0;
}
static void parse_statement(void)
{
    int outer = g_stmt_convcheck;
    char stmt[16]; memcpy(stmt, g_cur_stmt, sizeof stmt);
    const Tok *stok = g_stmt_tok;
    g_stmt_convcheck = 0;
    g_stmt_tok = cur();
    /* its calls are collected as they are made, and go first */
    CallList outer_calls = g_stmt_calls; int outer_on = g_stmt_calls_on, outer_hold = g_stmt_calls_hold;
    memset(&g_stmt_calls, 0, sizeof g_stmt_calls); g_stmt_calls_on = 1; g_stmt_calls_hold = 0;
    int b0 = block_begin();
    if (g_proflines && !g_noemit) {
        /* -fprofile-lines: a global label where each statement's code
         * begins, for bench/prof.py to attribute instructions to lines */
        static int seq;
        emit("\t.globl __ln_%d_%d", cur()->line, seq);
        emit("__ln_%d_%d:", cur()->line, seq); seq++;
    }
    static int cen_depth;
    int cn0 = g_cen_nnamed, ca0 = g_cen_naddr;
    cen_depth++;
    parse_statement_1();
    cen_depth--;
    if (g_cen_on) { cen_stmt_end(); cen_stmt_owed(cn0, ca0, cen_depth == 0); }
    if (g_stmt_convcheck) {
        int Lok = new_label();
        emit_call("cob_fn_conv_bad");
        emit("\tbeq r1, r0, .L%d", Lok);
        emit_ec_raise(ec_find("EC-DATA-CONVERSION", 0));
        emit_label(Lok);
    }
    if (g_stmt_calls.n) {
        Block body = block_cut(b0);
        for (int i = 0; i < g_stmt_calls.n; i++) block_put(&g_stmt_calls.b[i]);
        block_put(&body);
    }
    free(g_stmt_calls.b);
    g_stmt_calls = outer_calls; g_stmt_calls_on = outer_on; g_stmt_calls_hold = outer_hold;
    g_stmt_convcheck = outer;
    memcpy(g_cur_stmt, stmt, sizeof stmt); g_stmt_tok = stok;
}

/* ALLOCATE {arithmetic-expression CHARACTERS | data-name-1} [INITIALIZED]
 * [RETURNING data-name-2] (2002 14.8.3) */
static void parse_allocate(void)
{
    Ref based; int has_based = 0, line = cur()->line;
    long fixed = -1;
    const Sym *first = cur()->kind == T_WORD ? sym_lookup_quiet(cur()->s) : NULL;
    if (first && !first->is_based && !is_word(peek(1), "characters") && peek(1)->kind != T_OP && peek(1)->kind != T_LP)
        die_at(line, "ALLOCATE '%s': it is not a BASED entry (2002 14.8.3 rule 1)", first->name);
    if (first && first->is_based) {
        parse_ref(&based); has_based = 1;
        if (based.nsub || based.rm || based.sym->parent >= 0 || !based.sym->is_based)
            die_at(based.line, "ALLOCATE '%s': it is not a BASED entry (2002 14.8.3 rule 1)", based.sym->name);
        fixed = based.sym->size;                 /* an ODO table at its maximum (GR 3), as laid out */
    } else {
        g_noemit++; Expr *e = parse_expr(); g_noemit--;
        expect_word("characters");
        emit_expr(e); emit_call("cob_pop_alloc_size");
    }
    int init = accept_word("initialized");
    Ref ret; int has_ret = 0;
    if (accept_word("returning")) {
        parse_ref(&ret); has_ret = 1;
        if (ret.sym->is_group || ret.sym->usage != U_POINTER)
            die_at(ret.line, "ALLOCATE RETURNING '%s': a data-pointer item (2002 14.8.3 rule 3)", ret.sym->name);
    }
    if (!has_based && !has_ret) die_at(line, "ALLOCATE of a number of characters needs RETURNING (2002 14.8.3 rule 2)");
    /* the storage comes zeroed, which INITIALIZED asks of characters
     * (GR 6) and leaves pointers NULL (GR 9) */
    if (fixed >= 0) emit_li("r3", fixed); else emit("\tadd r3, r1, r0");
    emit("\tstw sp+%d, r3", SLOT_B);
    emit_call("cob_allocate");
    emit("\tstw sp+%d, r1", SLOT_A);
    if (ec_on_name("EC-STORAGE-NOT-AVAIL")) {
        /* none to be had (GR 5c); a count of 0 or less is NULL, no exception (GR 2) */
        int Lok = new_label();
        emit("\tbne r1, r0, .L%d", Lok);
        emit("\tldw r2, sp+%d", SLOT_B);
        emit("\tbge r0, r2, .L%d", Lok);
        emit_ec_raise(ec_find("EC-STORAGE-NOT-AVAIL", 0));
        emit_label(Lok);
    }
    if (has_based) { emit_la("r3", g_sym[based.sym->record].label); emit("\tldw r1, sp+%d", SLOT_A); emit("\tstw r3+0, r1"); }
    if (has_ret) { emit_ref_addr(&ret, "r3"); emit("\tldw r1, sp+%d", SLOT_A); emit("\tstw r3+0, r1"); }
    if (init && has_based) {
        /* as INITIALIZE data-name-1 WITH FILLER ALL TO VALUE THEN TO
         * DEFAULT (GR 7) -- when there was storage to be had */
        int Lnone = new_label();
        emit("\tldw r1, sp+%d", SLOT_A);
        emit("\tbeq r1, r0, .L%d", Lnone);
        InitSpec sp; memset(&sp, 0, sizeof sp);
        sp.filler = sp.value = sp.value_all = sp.deflt = 1;
        long sub[MAXDIM];
        init_walk(based.sym, &based, &sp, sub, 0, based.line, 1);
        emit_label(Lnone);
    }
}

/* FREE {data-name-1}... (2002 14.8.14): each pointer's storage released
 * and the pointer NULL; NULL is left alone; anything else is
 * EC-STORAGE-NOT-ALLOC, the pointer unchanged */
static void parse_free(void)
{
    int n = 0;
    while (at_operand()) {
        Ref r; parse_ref(&r);
        if (r.sym->is_group || r.sym->usage != U_POINTER)
            die_at(r.line, "FREE '%s': a data-pointer item (2002 14.8.14 rule 1)", r.sym->name);
        emit_ref_addr(&r, "r3");
        emit("\tldw r3, r3+0");
        emit_call("cob_free");
        int Lnot = new_label(), Ldone = new_label();
        emit("\tbne r1, r0, .L%d", Lnot);
        emit_ref_addr(&r, "r3");
        emit("\tstw r3+0, r0");
        emit("\tjal r0, .L%d", Ldone);
        emit_label(Lnot);
        if (ec_on_name("EC-STORAGE-NOT-ALLOC")) {
            emit_li("r2", 1);
            emit("\tbne r1, r2, .L%d", Ldone);
            emit_ec_raise(ec_find("EC-STORAGE-NOT-ALLOC", 0));
        }
        emit_label(Ldone);
        n++;
    }
    if (!n) die_at(cur()->line, "FREE needs a pointer item");
}
