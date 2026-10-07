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
 *   flags verbs group-verbs alias-verbs value pins native
 * shape is scalar or table; flags a comma-separated list, of which
 * `group` is a named group over the item, `alias` a named redefinition
 * or renaming over its bytes, `unref` the item named by nothing; the
 * verb lists say which statements named it, the groups over it, and its
 * aliases; value is v (its own VALUE), g (a group's over it) or -; pins
 * is what used its bytes (symtab.h); native is y when the item is
 * written the machine's way (native.h), or what keeps it as written. */

static FILE *g_cen_out;

/* The runtime routines that take an item's address with its own
 * descriptor and read or write it as the number it holds: the same
 * number comes of it, or goes into it, whichever way the item is
 * written.  Each is here because libcob's source was read for it. */
static int cen_value_fn(const char *fn)
{
    static const char *v[] = {
        "cob_push",             /* the number, on the numeric stack (cob_get_num) */
        "cob_load_int",         /* the number, as an integer (cob_get_num) */
        "cob_get_num",
        "cob_put_num", "cob_put_num_x", "cob_store_int",    /* a number, stored (cob_put_num_x) */
        "cob_top_store", "cob_top_addto", "cob_top_subfrom",    /* the stack's top stored, added to what is there, taken from it (put_rmode) */
        "cob_display_field",    /* its digits: a DISPLAY item's as stored, any other's made of its value, as many */
        NULL };
    for (int i = 0; v[i]; i++) if (!strcmp(v[i], fn)) return 1;
    return 0;
}

static void cen_names(FILE *f, unsigned long long v, char names[][28], int n)
{
    int any = 0;
    for (int k = 0; k < 64; k++)
        if (v >> k & 1) { fprintf(f, "%s%s", any ? "," : "", k < n ? names[k] : "?"); any = 1; }
    if (!any) fputc('-', f);
}

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
    case U_DFLOAT:  return "float";
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

static void native_verdicts(const char *native, int stride);

/* a VALUE that leaves a number in the item: a numeric literal, or ZERO */
static int cen_value_is_number(const Sym *s)
{
    const Tok *v = s->value_tok;
    if (!v) return 1;
    if (s->value_fig) return !strncmp(v->s, "zero", 4);
    return v->kind == T_NUM && !s->value_all;
}

/* May item i be written the machine's way?  It stands alone (nothing
 * named over it, its storage the program's own and its address kept by
 * nobody), it is a number of a kind taken (cen_native_size), every use
 * of it is a use of its number (no pin), and its first bytes come from
 * nowhere but a
 * numeric VALUE of its own or none: not from a group's VALUE over it,
 * not from the VALUE of something that redefines it, and it redefines
 * nothing (an item laid over another starts with what the other was
 * given, which for an alphanumeric one is spaces).
 * Its condition-names' values are numbers. */
/* The bytes item s takes written the machine's way, or 0 when it is not
 * of a kind taken.  The kinds: a number of 18 digits or fewer with no P
 * in its picture -- DISPLAY with its sign, if it has one, on its last
 * digit; COMP-3; COMP (which keeps its size).  The form: binary of two
 * bytes to four digits, four to nine, eight to eighteen, as a COMP item
 * of that picture has -- or one byte, for an integer of one or two
 * digits whose place is one byte.  *inplace: the item's own bytes are
 * enough, and it stays where it is; a packed item of five digits, or of
 * ten to thirteen, has three bytes, or six or seven, where four or
 * eight are wanted, and is given a cell outside its record
 * (native.h). */
static int cen_native_size(const Sym *s, int *inplace)
{
    if (inplace) *inplace = 1;
    if (s->is_group || s->pi.category != PIC_NUMERIC || s->pi.edited || strchr(s->pi.pat, 'P')) return 0;
    if (s->sign_sep || s->sign_lead || s->blank_zero || s->just || s->sync || s->uvar != UV_NONE) return 0;
    int d = s->pi.digits, have;
    if (d < 1 || d > 18 || s->pi.scale < 0 || s->pi.scale > d) return 0;
    if (s->usage == U_BINARY) return sym_be(s) && (s->size == 2 || s->size == 4 || s->size == 8) ? s->size : 0;
    if (s->usage == U_DISPLAY) { if (s->size != d) return 0; have = d; }
    else if (s->usage == U_PACKED) { if (s->size != d / 2 + 1) return 0; have = s->size; }
    else return 0;
    int need = d <= 4 ? 2 : d <= 9 ? 4 : 8;
    if (need <= have) return need;
    if (d <= 2 && s->pi.scale == 0 && have == 1) return 1;
    if (inplace) *inplace = 0;
    return need;
}

static const char *cen_native_cand(int i, int group, int alias)
{
    Sym *s = &g_sym[i];
    Cen *c = &g_cen[i];
    if (s->is_group || s->is_cond || s->is_index || s->is_filler || s->is_rename || s->is_ftemp || s->is_rc) return "-";
    if (s->pi.category != PIC_NUMERIC || (s->usage != U_DISPLAY && s->usage != U_PACKED && !(s->usage == U_BINARY && sym_be(s)))) return "-";
    if (!cen_native_size(s, NULL)) return "kind";
    if (s->record < 0) return "-";
    if (s->ndims) {
        /* an element of a table: where it is, each occurrence in its own
         * bytes (the table's stride is its entries', not the item's size);
         * not of a table whose length varies */
        int inplace; cen_native_size(s, &inplace);
        if (!inplace) return "table";
        for (int p = i; p >= 0; p = g_sym[p].parent) if (g_sym[p].odo_dep[0]) return "table";
    }
    Sym *rec = &g_sym[s->record];
    if (rec->fd >= 0 || rec_indirect(rec) || s->any_len || s->is_global) return "storage";
    if (!c->refs) return "unnamed";
    if (group) return "group";
    if (alias) return "alias";
    if (c->flags & (CEN_RM | CEN_CALL | CEN_ADDR | CEN_PTR | CEN_DD | CEN_SQL | CEN_CLASS | CEN_OTHER | CEN_INNER | CEN_HDR | CEN_ODO)) return "address";
    if (c->pins) return "bytes";
    if (!cen_value_is_number(s)) return "value";
    for (int p = s->parent; p >= 0; p = g_sym[p].parent) if (g_sym[p].value_tok) return "value";
    /* laid over something else: what that left there is what it starts with */
    for (int p = i; p >= 0; p = g_sym[p].parent) if (g_sym[p].redefines >= 0) return "redefines";
    for (int t = g_sym_base; t < g_nsym; t++) {
        Sym *o = &g_sym[t];
        if (t == i || o->record != s->record) continue;
        if (o->is_cond) {
            if (o->parent != i) continue;
            for (int k = 0; k < o->ncv; k++) {
                if (o->cv_lo[k]->kind != T_NUM && strncmp(o->cv_lo[k]->s, "zero", 4)) return "value";
                if (o->cv_hi[k] && o->cv_hi[k]->kind != T_NUM && strncmp(o->cv_hi[k]->s, "zero", 4)) return "value";
            }
            if (o->cv_all) return "value";
            if (o->cv_false && o->cv_false->kind != T_NUM && strncmp(o->cv_false->s, "zero", 4)) return "value";
            continue;
        }
        if (o->is_index || !o->value_tok) continue;
        if (cen_overlap(i, t)) return "value";
    }
    return NULL;
}

static void census_unit(void)
{
    if (!g_cen_on) return;
    if (g_cen_dir && !g_cen_out) {
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
    int nu = g_nsym - g_sym_base;
    int *named = xmalloc(sizeof *named * (size_t)(nu + 1)), nnamed = 0;
    struct CenV { unsigned long long gv, av; char group, alias, native; const char *held; } *cv = xmalloc(sizeof *cv * (size_t)(nu + 1));
    memset(cv, 0, sizeof *cv * (size_t)(nu + 1));
    for (int i = g_sym_base; i < g_nsym; i++)
        if (!g_sym[i].is_cond && !g_sym[i].is_index && cen_named(i)) named[nnamed++] = i;
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        struct CenV *v = &cv[i - g_sym_base];
        if (s->is_group || s->is_cond || s->is_rename || s->is_index || s->record < 0) continue;
        for (int k = 0; k < nnamed; k++) {
            int t = named[k];
            if (t == i || g_sym[t].record != s->record) continue;
            int o = cen_overlap(i, t);
            if (o == 1) { v->group = 1; v->gv |= g_cen[t].verbs; }
            else if (o == 2) { v->alias = 1; v->av |= g_cen[t].verbs; }
        }
        v->held = cen_native_cand(i, v->group, v->alias);
        v->native = v->held == NULL;
    }
    /* two items copied or compared byte for byte are written one way:
     * the machine's only if both may be */
    for (int again = 1; again; ) {
        again = 0;
        for (int e = 0; e < g_cen_nedge; e++) {
            int a = g_cen_edge[e].a, b = g_cen_edge[e].b;
            int ain = a >= g_sym_base && a < g_nsym, bin = b >= g_sym_base && b < g_nsym;
            if (!ain && !bin) continue;
            int na = ain && cv[a - g_sym_base].native, nb = bin && cv[b - g_sym_base].native;
            if (na == nb) continue;
            if (na) cv[a - g_sym_base].held = "partner";
            if (nb) cv[b - g_sym_base].held = "partner";
            if (ain) cv[a - g_sym_base].native = 0;
            if (bin) cv[b - g_sym_base].native = 0;
            again = 1;
        }
    }
    {   /* this unit's pairs are done with */
        int w = 0;
        for (int e = 0; e < g_cen_nedge; e++) {
            int a = g_cen_edge[e].a, b = g_cen_edge[e].b;
            if ((a >= g_sym_base && a < g_nsym) || (b >= g_sym_base && b < g_nsym)) continue;
            g_cen_edge[w++] = g_cen_edge[e];
        }
        g_cen_nedge = w;
    }
    native_verdicts(cv ? &cv->native : NULL, (int)sizeof *cv);
    for (int i = g_sym_base; f && i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        struct CenV *v = &cv[i - g_sym_base];
        if (s->is_group || s->is_cond || s->is_rename || s->is_ftemp || s->ftemp_scan || s->standin || s->is_rc) continue;
        if (s->lin_file >= 0 || s->rep_ctr >= 0) continue;
        if (s->is_filler && !s->value_tok && s->redefines < 0) continue;      /* nothing can name it: the group over it, or nothing */
        Cen *c = &g_cen[i];
        Sym *rec = &g_sym[s->record >= 0 ? s->record : i];
        int table = s->occurs > 0;
        for (int p = s->parent; p >= 0; p = g_sym[p].parent) if (g_sym[p].occurs) table = 1;
        const char *sect = rec->fd >= 0 ? "file" : rec->is_linkage ? "linkage" : rec->is_based ? "based" : rec->is_external ? "external"
                         : rec->is_local ? "local" : "working";
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
        if (v->group) { fprintf(f, "%sgroup", any ? "," : ""); any = 1; }
        if (v->alias) { fprintf(f, "%salias", any ? "," : ""); any = 1; }
        if (s->any_len) { fprintf(f, "%sanylen", any ? "," : ""); any = 1; }
        if (s->is_global) { fprintf(f, "%sglobal", any ? "," : ""); any = 1; }
        if (s->redefines >= 0) { fprintf(f, "%sredefines", any ? "," : ""); any = 1; }
        if (!cen_named(i)) { fprintf(f, "%sunref", any ? "," : ""); any = 1; }
        if (!any) fputc('-', f);
        fputc('\t', f); cen_verbs(f, c->verbs);
        fputc('\t', f); cen_verbs(f, v->gv);
        fputc('\t', f); cen_verbs(f, v->av);
        int val = s->value_tok ? 'v' : '-';
        for (int p = s->parent; p >= 0 && val == '-'; p = g_sym[p].parent) if (g_sym[p].value_tok) val = 'g';
        fprintf(f, "\t%c\t", val);
        cen_names(f, c->pins, g_cen_why, g_cen_nwhy);
        fprintf(f, "\t%s\n", v->native ? "y" : v->held ? v->held : "-");
    }
    free(named); free(cv);
    if (f) fflush(f);
    for (int i = g_sym_base; i < g_nsym && i < g_cen_cap; i++) memset(&g_cen[i], 0, sizeof g_cen[i]);
}
