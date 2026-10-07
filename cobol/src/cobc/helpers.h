/* s32-cobc: small emission helpers.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- small emission helpers ------------------------------------------- */

static void emit_rw_ldw(Report *r, int off, const char *reg)
{
    emit_report_addr("r1", r);
    emit("\tldw %s, r1+%d", reg, off);
}

static void emit_rw_stw_imm(Report *r, int off, int v)
{
    emit_report_addr("r1", r);
    emit_li("r2", v);
    emit("\tstw r1+%d, r2", off);
}

static void emit_rw_move_sym(int from, int to)
{
    emit_item_addr("r3", &g_sym[from], g_sym[from].offset);
    emit_desc_addr("r4", sym_desc(&g_sym[from]));
    emit_item_addr("r5", &g_sym[to], g_sym[to].offset);
    emit_desc_addr("r6", sym_desc(&g_sym[to]));
    emit_call("cob_move");
}

/* counter += source (both plain items): through the numeric stack */
static void emit_rw_add_into(int src, int ctr)
{
    emit_item_addr("r3", &g_sym[src], g_sym[src].offset);
    emit_desc_addr("r4", sym_desc(&g_sym[src]));
    emit_call("cob_push");
    emit_item_addr("r3", &g_sym[ctr], g_sym[ctr].offset);
    emit_desc_addr("r4", sym_desc(&g_sym[ctr]));
    emit_li("r5", 0);
    emit_call("cob_top_addto");
    emit_call("cob_drop");
}

static void emit_rw_zero(int ctr)
{
    emit_li("r3", 0); emit_li("r4", 0); emit_li("r5", 0);
    emit_call("cob_push_lit");
    emit_item_addr("r3", &g_sym[ctr], g_sym[ctr].offset);
    emit_desc_addr("r4", sym_desc(&g_sym[ctr]));
    emit_li("r5", 0);
    emit_call("cob_top_store");
    emit_call("cob_drop");
}

/* the counter arithmetic a control level L owes when it breaks: every
 * counter summing a counter that resets at L takes its value (rolling
 * forward, crossfooting), in the order the entries stand; then, after
 * the footing presents, the counters resetting at L go to zero */
static void emit_rw_rolls(Report *r, int level)
{
    for (int gi = 0; gi < r->ng; gi++)
        for (int li = 0; li < r->g[gi].nl; li++)
            for (int fi = 0; fi < r->g[gi].l[li].nf; fi++) {
                RField *f = &r->g[gi].l[li].f[fi];
                if (!f->has_sum) continue;
                for (int k = 0; k < f->nsum; k++)
                    if (f->sum_is_ctr[k]) {
                        RField *sf = NULL;
                        for (int gj = 0; gj < r->ng && !sf; gj++)
                            for (int lj = 0; lj < r->g[gj].nl && !sf; lj++)
                                for (int fj = 0; fj < r->g[gj].l[lj].nf; fj++)
                                    if (r->g[gj].l[lj].f[fj].ctr_sym == f->sum_sym[k]) { sf = &r->g[gj].l[lj].f[fj]; break; }
                        if (sf && sf->reset_lvl == level) emit_rw_add_into(f->sum_sym[k], f->ctr_sym);
                    }
            }
}

static void emit_rw_resets(Report *r, int level)
{
    for (int gi = 0; gi < r->ng; gi++)
        for (int li = 0; li < r->g[gi].nl; li++)
            for (int fi = 0; fi < r->g[gi].l[li].nf; fi++) {
                RField *f = &r->g[gi].l[li].f[fi];
                if (f->has_sum && f->reset_lvl == level) emit_rw_zero(f->ctr_sym);
            }
}

/* the page advance: pad, count, and render the page heading */
/* the PAGE FOOTING groups, on a page that was started */
static void emit_page_footing(Report *r)
{
    int any = 0;
    for (int k = 0; k < r->ng; k++) if (r->g[k].type == RG_PAGE_FOOTING) any = 1;
    if (!any) return;
    int Lskip = new_label();
    emit_report_addr("r3", r);
    emit_call("cob_rw_page_started");
    emit("\tbeq r1, r0, .L%d", Lskip);
    for (int k = 0; k < r->ng; k++)
        if (r->g[k].type == RG_PAGE_FOOTING) emit_report_group(r, &r->g[k]);
    emit_label(Lskip);
}

static void emit_page_advance(Report *r)
{
    emit_page_footing(r);
    emit_report_addr("r3", r);
    emit_call("cob_rw_page_end");
    for (int k = 0; k < r->ng; k++)
        if (r->g[k].type == RG_PAGE_HEADING) emit_report_group(r, &r->g[k]);
}

/* render one group's lines at this point in the code; a body line that
 * would pass its bound spills onto a new page first.  The body groups
 * are DETAIL, CONTROL HEADING and CONTROL FOOTING (X3.23 VIII); a
 * CONTROL FOOTING's bound is the RD FOOTING line, the others' LAST
 * DETAIL -- the runtime reads the kind from is_body (1 or 2). */
static void emit_report_group(Report *r, RGroup *g)
{
    int is_body = g->type == RG_DETAIL || g->type == RG_CONTROL_HEADING ? 1
                : g->type == RG_CONTROL_FOOTING ? 2 : 0;
    int Lsupp = -1;
    if (g->type == RG_REPORT_FOOTING && g->nl) {
        emit_report_addr("r3", r);
        emit_li("r4", g->l[0].abs); emit_li("r5", g->l[0].plus);
        emit_call("cob_rw_rf_begin");
    }
    if (g->use_sec >= 0) {
        int Lret = new_label();
        char lab[32]; snprintf(lab, sizeof lab, ".L%d", Lret);
        emit_call("cob_rw_use_in");                   /* a GENERATE, INITIATE or TERMINATE reached from here is EC-FLOW-REPORT */
        emit_para_cell("r3", g_unit, g->use_sec);
        emit_la("r4", lab);
        emit_call("cob_perform_push");
        emit("\tjal r0, .Lp%d_%d", g_unit, g->use_sec);
        emit_label(Lret);
        emit_call("cob_rw_use_out");
        Lsupp = new_label();
        emit_rw_ldw(r, RW_OFF_SUPPRESS, "r2");
        int Lrender = new_label();
        emit("\tbeq r2, r0, .L%d", Lrender);
        emit_rw_stw_imm(r, RW_OFF_SUPPRESS, 0);
        emit_jump(Lsupp);
        emit_label(Lrender);
    }
    for (int i = 0; i < g->nl; i++) {
        RLine *ln = &g->l[i];
        if (ln->np && is_body) {                    /* LINE ... NEXT PAGE */
            emit_page_advance(r);
        }
        if (is_body) {
            emit_report_addr("r3", r);
            emit_li("r4", ln->abs); emit_li("r5", ln->plus); emit_li("r6", 1);
            emit_call("cob_rw_line_overflows");
            int Lok = new_label();
            emit("\tbeq r1, r0, .L%d", Lok);
            emit_page_advance(r);
            emit_label(Lok);
        }
        /* the line's position first: LINE-COUNTER holds it while the SOURCE
         * items are moved (X3.23 VIII-5 2.4.5: the PH line prints 1) */
        emit_report_addr("r3", r);
        emit_li("r4", ln->abs); emit_li("r5", ln->plus);
        emit_li("r6", is_body);
        emit_call("cob_rw_line_begin");
        for (int k = 0; k < ln->nf; k++) {
            RField *f = &ln->f[k];
            if (!f->column) continue;               /* no COLUMN: not presented (X3.23-1985 XIII 3.11.4 rule 1) */
            int Lgi = -1;
            if (f->gi && g->type == RG_DETAIL) {    /* GROUP INDICATE: spaces except first after a page or break */
                Lgi = new_label();
                emit_rw_ldw(r, RW_OFF_GI, "r2");
                emit("\tandi r2, r2, %d", 1 << (int)(g - r->g));
                emit("\tbeq r2, r0, .L%d", Lgi);
            }
            Arg a[4];
            a[0] = arg_imm(f->column);
            a[1] = arg_desc(rfield_desc(f));
            if (f->ctr_sym) {                       /* a SUM entry prints its counter */
                Ref *rf = xmalloc(sizeof *rf);
                memset(rf, 0, sizeof *rf);
                rf->sym = &g_sym[f->ctr_sym]; rf->line = f->line;
                a[2] = arg_ref(rf); a[3] = arg_desc(sym_desc(rf->sym));
            } else if (f->has_source) {
                if (!f->source) {                   /* parsed at the first GENERATE, kept */
                    f->source = xmalloc(sizeof *f->source);
                    int save_tp = g_tp;
                    g_tp = f->source_tp; parse_ref(f->source); g_tp = save_tp;
                }
                Ref *rf = f->source;
                if (rf->sym->is_cond) die_at(f->line, "SOURCE '%s' is a condition-name", rf->sym->name);
                if (sym_is_national(rf->sym) && f->pi.category != PIC_NATIONAL)
                    die_at(f->line, "SOURCE '%s' is national: it goes to a national field (PICTURE N), not this one (2023 14.9.25.3 rule 3)", rf->sym->name);
                a[2] = arg_ref(rf); a[3] = arg_desc(sym_desc(rf->sym));
            } else if (f->value->kind == T_STR) {
                a[2] = arg_label(lit_label((unsigned char *)f->value->s, f->value->len));
                a[3] = arg_desc(f->value->nat ? nat_desc(f->value->len) : str_desc(f->value->len));
            } else {
                NumLit n; numlit_parse(f->value, &n);
                int d; a[2] = arg_label(num_lit_label(&n, &d)); a[3] = arg_desc(d);
            }
            emit_args(a, 4);
            /* a numeric SOURCE to a numeric, edited or alphanumeric field:
             * cob_rw_field is cob_move to the print line, which takes the
             * item's number, or the digits of it */
            if (!f->ctr_sym && f->has_source && !f->source->rm && is_numeric_sym(f->source->sym) &&
                (f->pi.category == PIC_NUMERIC || f->pi.category == PIC_NUMERIC_EDITED ||
                 f->pi.category == PIC_ALPHANUMERIC || f->pi.category == PIC_ALPHANUMERIC_EDITED))
                cen_bless(f->source->sym);
            emit_call("cob_rw_field");
            if (Lgi >= 0) emit_label(Lgi);
        }
        emit_report_addr("r3", r);
        emit_li("r4", is_body);
        emit_call("cob_rw_line_write");
    }
    if (g->type == RG_DETAIL) {
        int gi_any = 0;
        for (int i = 0; i < g->nl; i++) for (int k = 0; k < g->l[i].nf; k++) if (g->l[i].f[k].gi) gi_any = 1;
        if (gi_any) {                               /* this group's GROUP INDICATE fields wait for the next page or break */
            emit_report_addr("r1", r);
            emit("\tldw r2, r1+%d", RW_OFF_GI);
            emit_li("r3", ~(1 << (int)(g - r->g)));
            emit("\tand r2, r2, r3");
            emit("\tstw r1+%d, r2", RW_OFF_GI);
        }
    }
    if (g->next_kind) {
        int Lskip = -1;
        if (g->type == RG_CONTROL_FOOTING) {        /* NEXT GROUP on a CF applies only at its own break level (VIII 2.15.4(3)) */
            Lskip = new_label();
            emit_rw_ldw(r, RW_OFF_BRK, "r2");
            emit_li("r3", g->ctl_level);
            emit("\tbne r2, r3, .L%d", Lskip);
        }
        emit_report_addr("r3", r);
        emit_li("r4", g->next_kind);
        emit_li("r5", g->next_n);
        emit_call("cob_rw_next_group");
        if (Lskip >= 0) emit_label(Lskip);
    }
    if (Lsupp >= 0) emit_label(Lsupp);
}

/* a CODE on one report of a file is on each report of it (X3.23-1985 XIII
 * 3.6.3 rule 2; 2023 13.18.12.3 rule 3) */
static void rw_check_code(Report *r)
{
    int any = 0, all = 1;
    for (int i = g_report_base; i < g_nreport; i++) {
        if (g_reports[i].file != r->file) continue;
        int has = g_reports[i].code_lit || g_reports[i].code_tp;
        any |= has; all &= has;
    }
    if (any && !all) die_at(r->line, "CODE is on one report of the file '%s' but not on each (2023 13.18.12.3 rule 3)", g_files[r->file].name);
}

/* a report's CODE for the runtime: the literal once, at INITIATE; an
 * identifier's value at each body group's start (GENERATE) */
static void emit_rw_code(Report *r)
{
    if (r->code_lit) {
        Arg a[3] = { arg_imm(0), arg_label(lit_label((unsigned char *)r->code_lit->s, r->code_lit->len)), arg_imm(r->code_lit->len) };
        emit_args(a + 1, 2);
        emit("\tadd r5, r4, r0"); emit("\tadd r4, r3, r0");
        emit_report_addr("r3", r);
        emit_call("cob_rw_code");
    } else if (r->code_tp) {
        if (!r->code_ref) {                     /* parsed at first use, kept */
            r->code_ref = xmalloc(sizeof *r->code_ref);
            int save = g_tp; g_tp = r->code_tp;
            parse_ref(r->code_ref);
            g_tp = save;
        }
        Ref cr = *r->code_ref;
        if (cr.sym->is_group || (cr.sym->pi.category != PIC_ALPHANUMERIC))
            die_at(cr.line, "CODE: '%s' is not an alphanumeric data item (2023 13.18.12.3 rule 2)", cr.sym->name);
        Arg a[2] = { arg_ref(&cr), arg_imm(cr.sym->size) };
        emit_args(a, 2);
        emit("\tadd r5, r4, r0"); emit("\tadd r4, r3, r0");
        emit_report_addr("r3", r);
        emit_call("cob_rw_code");
    }
}

/* no GENERATE, INITIATE or TERMINATE in a USE BEFORE REPORTING procedure
 * (X3.23-1985 USE rule 7; 2023 14.9.49.3 rule 10) */
static void rw_not_in_use(const char *verb, int line)
{
    if (!g_in_decl) return;
    for (int i = 0; i < g_nrwuse; i++)
        if (g_rwuse[i].unit == g_unit && g_rwuse[i].sec == g_cur_sec_id)
            die_at(line, "%s in a USE BEFORE REPORTING procedure (2023 14.9.49.3 rule 10)", verb);
}

static void parse_initiate(void)
{
    rw_not_in_use("INITIATE", cur()->line);
    /* INITIATE report-name ... (X3.23-1985 XIII 4.2) */
    do {
        Report *r = expect_report();
        rw_check_code(r);
        emit_ec_query("EC-FLOW-REPORT", "cob_rw_in_use", 1);         /* inside a USE BEFORE REPORTING procedure (2023 14.9.49.3 rule 10, 14.9.21.4) */
        emit_report_addr("r3", r);
        emit_ec_query("EC-REPORT-ACTIVE", "cob_rw_active", 1);       /* INITIATE of an active report (14.9.21.4 rule 1) */
        if (ec_on_name("EC-REPORT-FILE-MODE")) {
            /* the report's file not open OUTPUT or EXTEND (14.9.21.4 rule 2) */
            int Lok = new_label();
            emit_file_addr("r3", &g_files[r->file]); emit_call("cob_open_mode");
            emit_li("r2", COB_OPEN_OUTPUT); emit("\tbeq r1, r2, .L%d", Lok);
            emit_li("r2", COB_OPEN_EXTEND); emit("\tbeq r1, r2, .L%d", Lok);
            emit_ec_raise(ec_find("EC-REPORT-FILE-MODE", 0));
            emit_label(Lok);
        }
        emit_report_addr("r3", r);
        emit_call("cob_rw_initiate");
        if (r->code_lit) emit_rw_code(r);
    } while (cur()->kind == T_WORD && report_find(cur()->s));
}

/* the counters a GENERATE subtotals: plain (non-counter) sources, the
 * UPON list honoured -- at GENERATE report-name only unrestricted
 * counters take their sources (X3.23 VIII 2.21.4(11)) */
static void emit_rw_subtotals(Report *r, RGroup *det)
{
    for (int gi = 0; gi < r->ng; gi++)
        for (int li = 0; li < r->g[gi].nl; li++)
            for (int fi = 0; fi < r->g[gi].l[li].nf; fi++) {
                RField *f = &r->g[gi].l[li].f[fi];
                if (!f->has_sum) continue;
                if (f->nupon) {
                    int hit = 0;
                    for (int k = 0; k < f->nupon; k++) if (det && &r->g[f->upon_g[k]] == det) hit = 1;
                    if (!hit) continue;
                }
                for (int k = 0; k < f->nsum; k++)
                    if (!f->sum_is_ctr[k]) emit_rw_add_into(f->sum_sym[k], f->ctr_sym);
            }
}

/* the control footing sequence for every level from the most minor up
 * to `to_level` (1 = most major, 0 = FINAL too): the rolls, the group,
 * the resets -- a level with no footing group still rolls and resets */
static void emit_rw_cf_level(Report *r, int L)
{
    emit_rw_rolls(r, L);
    for (int k = 0; k < r->ng; k++)
        if (r->g[k].type == RG_CONTROL_FOOTING && r->g[k].ctl_level == L) emit_report_group(r, &r->g[k]);
    emit_rw_resets(r, L);
}

static void emit_rw_generate(Report *r, RGroup *det)
{
    rw_resolve(r);
    int Lsense = new_label(), Lbody = new_label();
    emit_rw_ldw(r, RW_OFF_FIRST_GEN, "r2");
    emit("\tbne r2, r0, .L%d", Lsense);

    /* the first GENERATE: the controls remembered, the REPORT HEADING,
     * the first page, CONTROL HEADINGs FINAL then major to minor */
    for (int L = 0; L < r->nctl; L++) emit_rw_move_sym(r->ctl_sym[L], r->ctl_clone[L]);
    emit_rw_stw_imm(r, RW_OFF_FIRST_GEN, 1);
    for (int k = 0; k < r->ng; k++) if (r->g[k].type == RG_REPORT_HEADING) emit_report_group(r, &r->g[k]);
    {
        int Lpg = new_label();
        emit_rw_ldw(r, 52, "r2");                   /* next_page: an RH that kept the page to itself */
        emit("\tbne r2, r0, .L%d", Lpg);
        emit_report_addr("r3", r);
        emit_call("cob_rw_first_page");
        for (int k = 0; k < r->ng; k++) if (r->g[k].type == RG_PAGE_HEADING) emit_report_group(r, &r->g[k]);
        emit_label(Lpg);
    }
    for (int L = 0; L <= r->nctl; L++)
        for (int k = 0; k < r->ng; k++)
            if (r->g[k].type == RG_CONTROL_HEADING && r->g[k].ctl_level == L) emit_report_group(r, &r->g[k]);
    emit_jump(Lbody);

    emit_label(Lsense);
    if (r->nctl) {
        /* sense: the most major control whose value moved */
        int Lfound = new_label();
        emit_rw_stw_imm(r, RW_OFF_BRK, 0);
        for (int L = 1; L <= r->nctl; L++) {
            Sym *it = &g_sym[r->ctl_sym[L - 1]], *cl = &g_sym[r->ctl_clone[L - 1]];
            emit_item_addr("r3", it, it->offset); emit_desc_addr("r4", sym_desc(it));
            emit_item_addr("r5", cl, cl->offset); emit_desc_addr("r6", sym_desc(cl));
            emit_call("cob_cmp");
            int Lnx = new_label();
            emit("\tbeq r1, r0, .L%d", Lnx);
            emit_report_addr("r1", r); emit_li("r2", L);
            emit("\tstw r1+%d, r2", RW_OFF_BRK);
            emit_jump(Lfound);
            emit_label(Lnx);
        }
        emit_jump(Lbody);
        emit_label(Lfound);
        /* CONTROL FOOTINGs, most minor up to the break level.  During
         * them the control items themselves hold the prior values (VIII
         * 2.21.4(13)): the new values wait aside, the clones move in --
         * so a USE BEFORE REPORTING procedure sees what the footing sees */
        for (int L = 0; L < r->nctl; L++) {
            emit_rw_move_sym(r->ctl_sym[L], r->ctl_held[L]);
            emit_rw_move_sym(r->ctl_clone[L], r->ctl_sym[L]);
        }
        for (int L = r->nctl; L >= 1; L--) {
            int Lskip = new_label();
            emit_rw_ldw(r, RW_OFF_BRK, "r2");
            emit_li("r3", L);
            emit("\tblt r3, r2, .L%d", Lskip);      /* L < brk: this level did not break */
            emit_rw_cf_level(r, L);
            emit_label(Lskip);
        }
        for (int L = 0; L < r->nctl; L++) {
            emit_rw_move_sym(r->ctl_held[L], r->ctl_sym[L]);
            emit_rw_move_sym(r->ctl_sym[L], r->ctl_clone[L]);
        }
        emit_report_addr("r1", r);
        emit_li("r2", -1);
        emit("\tstw r1+%d, r2", RW_OFF_GI);
        for (int L = 1; L <= r->nctl; L++) {
            int Lskip = new_label();
            emit_rw_ldw(r, RW_OFF_BRK, "r2");
            emit_li("r3", L);
            emit("\tblt r3, r2, .L%d", Lskip);
            for (int k = 0; k < r->ng; k++)
                if (r->g[k].type == RG_CONTROL_HEADING && r->g[k].ctl_level == L) emit_report_group(r, &r->g[k]);
            emit_label(Lskip);
        }
    }
    emit_label(Lbody);
    emit_rw_subtotals(r, det);
    if (det) {
        int last_printing = 0;
        for (int i = 0; i < det->nl; i++) if (det->l[i].nf) last_printing = i;
        int height = 0;
        for (int i = 1; i <= last_printing; i++) height += det->l[i].plus;
        emit_report_addr("r3", r);
        emit_li("r4", det->l[0].abs); emit_li("r5", det->l[0].plus); emit_li("r6", height);
        emit_call("cob_rw_fit");
        int Lfits = new_label();
        emit("\tbeq r1, r0, .L%d", Lfits);
        emit_page_advance(r);
        emit_label(Lfits);
        emit_report_group(r, det);
    }
}

static void parse_terminate_1(Report *r);
static void parse_terminate(void)
{
    rw_not_in_use("TERMINATE", cur()->line);
    /* TERMINATE report-name ... (X3.23-1985 XIII 4.4) */
    do parse_terminate_1(expect_report());
    while (cur()->kind == T_WORD && report_find(cur()->s));
}
static void parse_terminate_1(Report *r)
{
    rw_resolve(r);
    emit_ec_query("EC-FLOW-REPORT", "cob_rw_in_use", 1);
    emit_report_addr("r3", r);
    emit_ec_query("EC-REPORT-INACTIVE", "cob_rw_active", 0);        /* TERMINATE of an inactive report (2023 14.9.46.4 rule 1) */
    int Lend = new_label();
    emit_rw_ldw(r, RW_OFF_FIRST_GEN, "r2");
    emit("\tbeq r2, r0, .L%d", Lend);              /* no GENERATE ran: TERMINATE presents nothing */
    /* a break in the most major control, FINAL included, then the
     * REPORT FOOTING; the footings read the last GENERATE's values */
    emit_rw_stw_imm(r, RW_OFF_BRK, 1);
    for (int L = 0; L < r->nctl; L++) {         /* the footings read the last GENERATE's values */
        emit_rw_move_sym(r->ctl_sym[L], r->ctl_held[L]);
        emit_rw_move_sym(r->ctl_clone[L], r->ctl_sym[L]);
    }
    for (int L = r->nctl; L >= 1; L--) emit_rw_cf_level(r, L);
    if (r->ctl_final || r->nctl == 0) emit_rw_cf_level(r, 0);
    emit_page_footing(r);                       /* the page's last group, before the REPORT FOOTING (VIII 3.4.4) */
    for (int k = 0; k < r->ng; k++) if (r->g[k].type == RG_REPORT_FOOTING) emit_report_group(r, &r->g[k]);
    for (int L = 0; L < r->nctl; L++) emit_rw_move_sym(r->ctl_held[L], r->ctl_sym[L]);
    emit_report_addr("r3", r);
    emit_call("cob_rw_terminate");
    emit_rw_stw_imm(r, RW_OFF_FIRST_GEN, 0);
    emit_label(Lend);
}

static void parse_generate(void)
{
    rw_not_in_use("GENERATE", cur()->line);
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a report group after GENERATE");
    Report *r = NULL; RGroup *g = NULL;
    for (int i = g_report_base; i < g_nreport && !g; i++)
        for (int k = 0; k < g_reports[i].ng; k++)
            if (g_reports[i].g[k].name[0] && !strcmp(g_reports[i].g[k].name, t->s)) { r = &g_reports[i]; g = &g_reports[i].g[k]; break; }
    if (!g) {
        r = report_find(t->s);
        if (!r) die_at(t->line, "'%s' is not a report group", t->s);
        /* summary reporting: the RD has a CONTROL clause and at most one
         * DETAIL group (X3.23-1985 XIII 4.3.3 rule 2; the body group, 3.20.3 rule 7) */
        int ndet = 0;
        for (int k = 0; k < r->ng; k++) ndet += r->g[k].type == RG_DETAIL;
        if (!r->nctl && !r->ctl_final) die_at(t->line, "GENERATE %s: the RD has no CONTROL clause (X3.23-1985 XIII 4.3.3 rule 2a)", r->name);
        if (ndet > 1 && g_std < 2002)          /* 1985's rule; 2002 and 2023 drop it */
            die_at(t->line, "GENERATE %s: the RD has %d DETAIL groups, one at most (X3.23-1985 XIII 4.3.3 rule 2b; 2002 allows more -- compile with -std=2002)", r->name, ndet);
        advance();
        emit_ec_query("EC-FLOW-REPORT", "cob_rw_in_use", 1);
        emit_report_addr("r3", r);
        emit_ec_query("EC-REPORT-INACTIVE", "cob_rw_active", 0);    /* GENERATE for an inactive report (2023 14.9.16.4 rule 1) */
        if (r->code_tp) emit_rw_code(r);
        emit_rw_generate(r, NULL);                  /* GENERATE report-name: summary reporting */
        return;
    }
    if (g->type != RG_DETAIL) die_at(t->line, "GENERATE needs a DETAIL group or the report-name");
    advance();
    emit_ec_query("EC-FLOW-REPORT", "cob_rw_in_use", 1);
    emit_report_addr("r3", r);
    emit_ec_query("EC-REPORT-INACTIVE", "cob_rw_active", 0);
    if (r->code_tp) emit_rw_code(r);
    emit_rw_generate(r, g);
}
