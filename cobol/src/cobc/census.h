/* s32-cobc: the census -- which items stand alone.  A part of one
 * translation unit, included by s32-cobc.c in order; not a header to
 * include anywhere else. */

/* ====================================================================== */
/* The census (S32_CENSUS_DIR; docs/plans/census.md)                       */
/* ====================================================================== */

/* An elementary item stands alone when the statements that name it are
 * the only way its bytes are reached: no group over it is named, no
 * redefinition or renaming of it is named, no other program is given its
 * address and the runtime keeps none.  The DATA DIVISION says where such
 * an item lies and how it is encoded, and nothing can tell whether that
 * was obeyed -- so its layout is the compiler's to choose.
 *
 * The names statements resolved were counted as they were parsed
 * (cen_ref in symtab.h, parse_ref in operand.h, and the few places that
 * know more: CALL's arguments, ADDRESS OF, a class condition).  Here, at
 * a unit's end, each elementary item gets one line: what it is, how it
 * was named, and what else that overlaps it was named.  The verdict is
 * left to the reader of the lines (tests/census.py), which can weigh the
 * reasons differently without the compiler being built again.
 *
 * A line, tab-separated:
 *   unit line level name shape section category usage picture size refs
 *   flags verbs group-verbs alias-verbs value
 * shape is scalar or table; flags a comma-separated list, of which
 * `group` is a named group over the item, `alias` a named redefinition
 * or renaming over its bytes, `unref` the item named by nothing; the
 * verb lists say which statements named it, the groups over it, and its
 * aliases; value is v (its own VALUE), g (a group's over it) or -. */

static FILE *g_cen_out;

static int cen_named(int i)
{
    return i < g_cen_cap && (g_cen[i].refs > 0 || (g_cen[i].flags & (CEN_PTR | CEN_DD | CEN_ODO | CEN_CALL | CEN_ADDR | CEN_SQL)));
}

/* x's bytes in its record, every occurrence of it: from the outermost
 * table between x and top (an ancestor of x, or x), or x's own */
static void cen_extent(int x, int top, int *lo, int *hi)
{
    *lo = g_sym[x].offset; *hi = *lo + g_sym[x].size * (g_sym[x].occurs ? g_sym[x].occurs : 1);
    for (int p = x; ; p = g_sym[p].parent) {
        if (g_sym[p].occurs) { *lo = g_sym[p].offset; *hi = *lo + g_sym[p].size * g_sym[p].occurs; }
        if (p == top || g_sym[p].parent < 0) break;
    }
}

/* how t lies to the elementary item s, both of one record: 1 a group
 * over it, 2 other bytes of it (a redefinition, a renaming), 0 apart */
static int cen_overlap(int s, int t)
{
    int ps[64], pt[64], ns = 0, nt = 0;
    for (int p = s; p >= 0 && ns < 64; p = g_sym[p].parent) ps[ns++] = p;
    for (int p = t; p >= 0 && nt < 64; p = g_sym[p].parent) pt[nt++] = p;
    for (int k = 1; k < ns; k++) if (ps[k] == t) return 1;
    /* the chains from their roots part at tops and topt: siblings, or two
     * records of one storage */
    int is = ns - 1, it = nt - 1;
    while (is > 0 && it > 0 && ps[is] == pt[it]) { is--; it--; }
    if (ps[is] == pt[it]) return 0;             /* t is under s: a condition-name, an index */
    int lo1, hi1, lo2, hi2;
    cen_extent(s, ps[is], &lo1, &hi1);
    cen_extent(t, pt[it], &lo2, &hi2);
    return lo1 < hi2 && lo2 < hi1 ? 2 : 0;
}

static const char *cen_category(Sym *s)
{
    if (s->is_index) return "index-name";
    switch (s->usage) {
    case U_POINTER: return "pointer";
    case U_INDEX:   return "usage-index";
    case U_FLOAT:   return "float";
    case U_BIT:     return "bit";
    default: break;
    }
    switch (s->pi.category) {
    case PIC_NUMERIC:
        if (s->usage == U_NATIONAL) return "national-num";
        if (s->usage == U_DISPLAY)
            return is_display_int(s) ? "display-int" : s->pi.scale ? "display-dec" : "display-sint";
        if (s->usage == U_PACKED) return s->pi.scale ? "packed-dec" : "packed-int";
        if (is_hot_int(s)) return "binary-int";
        if (s->pi.scale) return "binary-dec";
        return s->size == 8 ? "binary-int8" : "binary-other";
    case PIC_NUMERIC_EDITED:        return "num-edited";
    case PIC_ALPHANUMERIC_EDITED:   return "alnum-edited";
    case PIC_NATIONAL:              return "national";
    case PIC_BOOLEAN:               return "boolean";
    default:                        return s->size == 1 ? "alnum-1" : "alnum";
    }
}

static void cen_verbs(FILE *f, unsigned long long v)
{
    int any = 0;
    for (int k = 0; k < 64; k++)
        if (v >> k & 1) { fprintf(f, "%s%s", any ? "," : "", k < g_cen_nverb ? g_cen_verb[k] : "?"); any = 1; }
    if (!any) fputc('-', f);
}

static void census_unit(void)
{
    if (!g_cen_dir) return;
    if (!g_cen_out) {
        char path[1024]; unsigned h = 2166136261u;
        for (const char *p = g_file; *p; p++) h = (h ^ (unsigned char)*p) * 16777619u;
        const char *base = strrchr(g_file, '/'); base = base ? base + 1 : g_file;
        snprintf(path, sizeof path, "%s/%s.%08x.census", g_cen_dir, base, h);
        g_cen_out = fopen(path, "w");
        if (!g_cen_out) { fprintf(stderr, "s32-cobc: cannot write the census to %s\n", path); exit(1); }
        fprintf(g_cen_out, "#file\t%s\n", g_file);
    }
    FILE *f = g_cen_out;
    if (g_nsym > 0) cen_of(&g_sym[g_nsym - 1]);
    /* names the clauses of the other divisions hold: the runtime, or the
     * compiled code of every reference, reads and writes them there */
    for (int i = g_file_base; i < g_nfile; i++) {
        File *fl = &g_files[i];
        if (fl->assign_sym) cen_flag(fl->assign_sym, CEN_PTR);
        if (fl->status_sym) cen_flag(fl->status_sym, CEN_PTR);
        if (fl->relkey_sym) cen_flag(fl->relkey_sym, CEN_PTR);
        if (fl->dep_sym) cen_flag(fl->dep_sym, CEN_PTR);
        for (int w = 0; w < 4; w++) if (fl->lin_sym[w]) cen_flag(fl->lin_sym[w], CEN_PTR);
    }
    for (int i = g_sym_base; i < g_nsym; i++)
        if (g_sym[i].odo_dep_sym) cen_flag(g_sym[i].odo_dep_sym, CEN_ODO);
    /* a condition-name's uses are its item's */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *c = &g_sym[i];
        if (!c->is_cond || c->parent < 0 || !g_cen[i].refs) continue;
        Cen *p = &g_cen[c->parent];
        p->refs += g_cen[i].refs; p->verbs |= g_cen[i].verbs; p->flags |= CEN_COND | (g_cen[i].flags & (CEN_INNER | CEN_OTHER));
        g_cen[i].refs = 0;
    }
    /* the named items that are not elementary data of their own: groups, renamings */
    int *named = xmalloc(sizeof *named * (size_t)(g_nsym - g_sym_base + 1)), nnamed = 0;
    for (int i = g_sym_base; i < g_nsym; i++)
        if (!g_sym[i].is_cond && !g_sym[i].is_index && cen_named(i)) named[nnamed++] = i;
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_group || s->is_cond || s->is_rename || s->is_ftemp || s->ftemp_scan || s->standin || s->is_rc) continue;
        if (s->lin_file >= 0 || s->rep_ctr >= 0) continue;
        if (s->is_filler && !s->value_tok && s->redefines < 0) continue;      /* nothing can name it: the group over it, or nothing */
        Cen *c = &g_cen[i];
        Sym *rec = &g_sym[s->record >= 0 ? s->record : i];
        int table = s->occurs > 0;
        for (int p = s->parent; p >= 0; p = g_sym[p].parent) if (g_sym[p].occurs) table = 1;
        const char *sect = rec->fd >= 0 ? "file" : rec->is_linkage ? "linkage" : rec->is_based ? "based" : rec->is_external ? "external"
                         : rec->is_local ? "local" : "working";
        unsigned long long gv = 0, av = 0; int group = 0, alias = 0;
        if (!s->is_index)
            for (int k = 0; k < nnamed; k++) {
                int t = named[k];
                if (t == i || g_sym[t].record != s->record || s->record < 0) continue;
                int o = cen_overlap(i, t);
                if (o == 1) { group = 1; gv |= g_cen[t].verbs; }
                else if (o == 2) { alias = 1; av |= g_cen[t].verbs; }
            }
        char pic[80]; int n = 0;
        for (const char *p = s->pic; *p && n < 78; p++) pic[n++] = *p == '\t' ? ' ' : *p;
        pic[n] = 0;
        fprintf(f, "%s\t%d\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%d\t%d\t", g_progid, s->line, s->level, s->is_filler ? "FILLER" : s->name,
                table ? "table" : "scalar", sect, cen_category(s), usage_name(s->usage), pic[0] ? pic : "-", s->size, c->refs);
        static const struct { unsigned bit; const char *name; } fl[] = {
            { CEN_RM, "refmod" }, { CEN_CALL, "call" }, { CEN_ADDR, "address" }, { CEN_PTR, "runtime" }, { CEN_DD, "screen" },
            { CEN_SQL, "sql" }, { CEN_CLASS, "class" }, { CEN_OTHER, "other" }, { CEN_SUB, "subscript" }, { CEN_COND, "cond" },
            { CEN_INNER, "inner" }, { CEN_HDR, "header" }, { CEN_ODO, "odo" } };
        int any = 0;
        for (unsigned k = 0; k < sizeof fl / sizeof fl[0]; k++)
            if (c->flags & fl[k].bit) { fprintf(f, "%s%s", any ? "," : "", fl[k].name); any = 1; }
        if (group) { fprintf(f, "%sgroup", any ? "," : ""); any = 1; }
        if (alias) { fprintf(f, "%salias", any ? "," : ""); any = 1; }
        if (s->any_len) { fprintf(f, "%sanylen", any ? "," : ""); any = 1; }
        if (s->is_global) { fprintf(f, "%sglobal", any ? "," : ""); any = 1; }
        if (s->redefines >= 0) { fprintf(f, "%sredefines", any ? "," : ""); any = 1; }
        if (!cen_named(i)) { fprintf(f, "%sunref", any ? "," : ""); any = 1; }
        if (!any) fputc('-', f);
        fputc('\t', f); cen_verbs(f, c->verbs);
        fputc('\t', f); cen_verbs(f, gv);
        fputc('\t', f); cen_verbs(f, av);
        int val = s->value_tok ? 'v' : '-';
        for (int p = s->parent; p >= 0 && val == '-'; p = g_sym[p].parent) if (g_sym[p].value_tok) val = 'g';
        fprintf(f, "\t%c\n", val);
    }
    free(named);
    fflush(f);
    for (int i = g_sym_base; i < g_nsym && i < g_cen_cap; i++) memset(&g_cen[i], 0, sizeof g_cen[i]);
}
