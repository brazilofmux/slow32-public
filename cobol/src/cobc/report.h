/* s32-cobc: Report Writer.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- Report Writer ------------------------------------------------------ */

static void emit_report_addr(const char *reg, Report *r)
{
    char lab[32]; snprintf(lab, sizeof lab, ".Lrpt%d_%d", g_unit, (int)(r - g_reports));
    emit_la(reg, lab);
}

static Report *expect_report(void)
{
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a report-name, found %s", tok_desc(t));
    Report *r = report_find(t->s);
    if (!r) die_at(t->line, "'%s' is not a report (no RD)", t->s);
    advance();
    return r;
}

/* a report field's columns: a national one's character positions (cobol ISSUES-92) */
static int rfield_cols(const RField *f) { return (f->pi.category == PIC_NATIONAL ? f->pi.bytes / 2 : f->pi.bytes) + f->sign_sep; }
static int rfield_is_nat(const RField *f) { return f->pi.category == PIC_NATIONAL || f->usage_nat; }

static int rfield_desc(RField *f)
{
    Desc d; memset(&d, 0, sizeof d);
    switch (f->pi.category) {
    case PIC_NATIONAL: d.cat = COB_NATIONAL; break;
    case PIC_ALPHABETIC: d.cat = COB_ALPHA; break;
    case PIC_ALPHANUMERIC: d.cat = COB_ALNUM; break;
    case PIC_ALPHANUMERIC_EDITED: d.cat = COB_ALNUM_ED; break;
    case PIC_NUMERIC: d.cat = COB_NUM; break;
    default: d.cat = COB_NUM_ED; break;
    }
    d.usage = COB_U_DISPLAY;
    d.digits = (unsigned char)f->pi.digits; d.scale = (signed char)f->pi.scale;
    if (f->pi.is_signed) d.flags |= COB_F_SIGNED;
    if (f->just) d.flags |= COB_F_JUST;
    if (f->blank_zero) d.flags |= COB_F_BLANKZ;
    if (f->pi.edited) snprintf(d.picstr, sizeof d.picstr, "%s", f->pi.pat);
    d.size = f->pi.bytes;
    if (f->sign_sep) { d.flags |= f->sign_lead ? COB_F_SEPLEAD : COB_F_SEPTRAIL; d.size++; }   /* its own character */
    if (f->usage_nat) { d.usage = COB_U_NATIONAL; d.size = 2 * f->pi.bytes; }
    return desc_add(&d);
}

static void emit_report_group(Report *r, RGroup *g);

/* ---- resolution at first use (INITIATE/GENERATE/TERMINATE) ----------- */

static Sym *rw_ref_sym(int tp, int line)
{
    Ref rr; int save_tp = g_tp;
    g_tp = tp; parse_ref(&rr); g_tp = save_tp;
    if (rr.nsub || rr.rm) die_at(line, "a report control or SUM operand is a plain data-name");
    return rr.sym;
}

static int rw_ctl_level_of(Report *r, Sym *sym, int line)
{
    for (int i = 0; i < r->nctl; i++) if (r->ctl_sym[i] == sym_idx(sym)) return i + 1;
    die_at(line, "'%s' is not in RD %s's CONTROL clause", sym->name, r->name);
    return 0;
}

static int rw_sym_is_counter(Report *r, int symidx)
{
    for (int gi = 0; gi < r->ng; gi++)
        for (int li = 0; li < r->g[gi].nl; li++)
            for (int fi = 0; fi < r->g[gi].l[li].nf; fi++)
                if (r->g[gi].l[li].f[fi].ctr_sym == symidx) return 1;
    return 0;
}

static void rw_resolve(Report *r)
{
    if (r->resolved) return;
    r->resolved = 1;
    for (int gi = 0; gi < r->ng; gi++) {
        RGroup *g = &r->g[gi];
        g->ctl_level = -1;
        if (g->type == RG_CONTROL_HEADING || g->type == RG_CONTROL_FOOTING) {
            if (!g->ctl_tp) {
                if (!r->ctl_final) die_at(g->line, "TYPE CONTROL %s FINAL needs CONTROL FINAL in the RD", g->type == RG_CONTROL_FOOTING ? "FOOTING" : "HEADING");
                g->ctl_level = 0;
            } else g->ctl_level = rw_ctl_level_of(r, rw_ref_sym(g->ctl_tp, g->line), g->line);
        }
        for (int li = 0; li < g->nl; li++)
            for (int fi = 0; fi < g->l[li].nf; fi++) {
                RField *f = &g->l[li].f[fi];
                if (!f->has_sum) continue;
                for (int k = 0; k < f->nsum; k++) {
                    Sym *op = rw_ref_sym(f->sum_tp[k], f->line);
                    f->sum_sym[k] = sym_idx(op);
                    f->sum_is_ctr[k] = rw_sym_is_counter(r, f->sum_sym[k]);
                    if (!is_numeric_sym(op)) die_at(f->line, "SUM '%s': identifier-1 is numeric (X3.23-1985 XIII 3.19.3 rule 1)", op->name);
                    if (f->nupon && f->sum_is_ctr[k]) die_at(f->line, "SUM '%s' UPON: with UPON the operands are not sum counters (X3.23-1985 XIII 3.19.3 rule 1)", op->name);
                }
                for (int k = 0; k < f->nupon; k++) {
                    Tok *nt = &g_tok[f->upon_tp[k]];
                    int found = -1;
                    for (int gj = 0; gj < r->ng; gj++)
                        if (r->g[gj].type == RG_DETAIL && r->g[gj].name[0] && !strcmp(r->g[gj].name, nt->s)) found = gj;
                    if (found < 0) die_at(f->line, "UPON '%s' is not a DETAIL group of RD %s", nt->s, r->name);
                    f->upon_g[k] = found;
                }
                if (f->reset_final) {
                    if (!r->ctl_final) die_at(f->line, "RESET ON FINAL: FINAL is in the CONTROL clause too (X3.23-1985 XIII 3.19.3 rule 4)");
                    f->reset_lvl = 0;
                }
                else if (f->reset_tp) {
                    f->reset_lvl = rw_ctl_level_of(r, rw_ref_sym(f->reset_tp, f->line), f->line);
                    if (g->ctl_level >= 0 && f->reset_lvl > g->ctl_level)
                        die_at(f->line, "RESET ON '%s': a control no lower than the footing's own (X3.23-1985 XIII 3.19.3 rule 4)", g_tok[f->reset_tp].s);
                }
                else f->reset_lvl = g->ctl_level;   /* its own footing's level (FINAL = 0) */
            }
    }
}
