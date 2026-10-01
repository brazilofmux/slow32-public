/* s32-cobc: OCCURS and VALUE rules.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- the OCCURS clause's rules (2023 13.18.38.3; X3.23-1985 5.8.3) --- */

static int sym_under(int j, int i)          /* is entry j entry i or below it? */
{
    for (int a = j; a >= 0; a = g_sym[a].parent) if (a == i) return 1;
    return 0;
}
static void occurs_rules_one(int i)
{
    Sym *s = &g_sym[i];
    int e85 = g_std < 2002;
    if (!s->occurs || s->is_cond) return;
    /* no ODO table below a table (rule 1b) */
    for (int j = i + 1; j < g_nsym; j++)       /* index-names sit among the entries: test each, stop at none */
        if (g_sym[j].odo_dep[0] && !g_sym[j].is_index && sym_under(j, i))
            die_at(g_sym[j].line, "'%s' has OCCURS DEPENDING ON inside the table '%s'; a variable table is not subordinate to another table (%s)",
                   g_sym[j].name, s->name, e85 ? "X3.23-1985 OCCURS syntax rule 1b" : "2023 13.18.38.3 rule 1b");
    for (int k = 0; k < s->nokey; k++) {
        int found = -1;
        for (int j = i; j < g_nsym; j++)
            if (!g_sym[j].is_cond && !g_sym[j].is_index && !strcmp(g_sym[j].name, s->okey[k]) && sym_under(j, i)) { found = j; break; }
        if (found < 0 || (k > 0 && found == i))
            die_at(s->line, "KEY '%s' of the table '%s': %s (%s)", s->okey[k], s->name,
                   found < 0 ? "a key is the table's entry or an entry below it" : "only the first key may be the table's own entry",
                   e85 ? "X3.23-1985 OCCURS syntax rule 3" : "2023 13.18.38.3 rule 3");
        Sym *x = &g_sym[found];
        if (found != i && x->occurs)
            die_at(s->line, "KEY '%s' of the table '%s' has an OCCURS clause of its own (%s)", x->name, s->name,
                   e85 ? "X3.23-1985 OCCURS syntax rule 11" : "2023 13.18.38.3 rule 6");
        for (int a = x->parent; a >= 0 && a != i; a = g_sym[a].parent)
            if (g_sym[a].occurs)
                die_at(s->line, "KEY '%s' of the table '%s' is inside '%s', which has an OCCURS clause (%s)", x->name, s->name, g_sym[a].name,
                       e85 ? "X3.23-1985 OCCURS syntax rule 12" : "2023 13.18.38.3 rule 4");
        if (!x->is_group && (x->usage == U_BIT || x->pi.category == PIC_BOOLEAN || x->usage == U_POINTER))
            die_at(s->line, "KEY '%s' of the table '%s' is %s (2023 13.18.38.3 rule 8)", x->name, s->name,
                   x->usage == U_POINTER ? "a pointer" : "boolean");
    }
}
/* JUSTIFIED, SIGN, SYNCHRONIZED (2023 13.18.32, .52, .55; X3.23-1985
 * 5.6, 5.12, 5.13) */
static void clause_rules_one(int i)
{
    Sym *s = &g_sym[i];
    int e85 = g_std < 2002;
    if (s->is_cond || s->is_index || s->is_rename || s->is_ftemp) return;
    if (s->just && s->is_group)
        die_at(s->line, "'%s' is a group; JUSTIFIED is for an elementary item (%s)", s->name, e85 ? "X3.23-1985 JUSTIFIED syntax rule 1" : "2023 13.18.32.3 rule 1");
    if (s->just && !s->is_group && s->pi.edited)
        die_at(s->line, "'%s' is %s; JUSTIFIED is not for an edited item (%s)", s->name, pic_category_name(s->pi.category),
               e85 ? "X3.23-1985 JUSTIFIED syntax rule 3" : "2023 13.18.32.3 rule 3");
    if (e85 && s->sync && s->is_group)
        die_at(s->line, "'%s' is a group; in COBOL 85 SYNCHRONIZED is for an elementary item, COBOL 2002 allows it (X3.23-1985 SYNCHRONIZED syntax rule 1)", s->name);
    if (e85 && s->is_group && (s->sign_lead || s->sign_sep)) {
        int any = 0;
        for (int j = i + 1; j < g_nsym && !any; j++)
            if (!g_sym[j].is_cond && !g_sym[j].is_index && !g_sym[j].is_group && sym_under(j, i) && g_sym[j].pi.category == PIC_NUMERIC && g_sym[j].pi.is_signed && g_sym[j].usage == U_DISPLAY) any = 1;
        if (!any)
            die_at(s->line, "the group '%s' has a SIGN clause but no signed numeric DISPLAY item below it (X3.23-1985 SIGN syntax rule 1)", s->name);
    }
}
static void occurs_rules(void)
{
    for (int i = g_sym_base; i < g_nsym; i++) {
        jmp_buf jb, *outer = g_recover;
        if (setjmp(jb)) { g_recover = outer; continue; }
        g_recover = &jb;
        occurs_rules_one(i);
        clause_rules_one(i);
        g_recover = outer;
    }
}

/* ---- the VALUE clause's rules (2023 13.18.63.3; X3.23-1985 5.15) --- */

/* the digits of a numeric literal past the picture's decimal places are
 * zeros (no truncation of nonzero digits: 85 rule 3, 2023 rule 2) */
static int numlit_frac_fits(const NumLit *n, int scale)
{
    for (int i = n->ndigits - n->scale + (scale > 0 ? scale : 0); i < n->ndigits; i++) if (n->digits[i] != '0') return 0;
    return 1;
}
static int numlit_cmp(const NumLit *a, const NumLit *b)     /* the values' order */
{
    char x[80], y[80]; int sc = a->scale > b->scale ? a->scale : b->scale, dg = 38;
    numlit_align(a, dg, sc, x); numlit_align(b, dg, sc, y);
    int c = memcmp(x, y, (size_t)dg), na = a->neg, nb = b->neg;
    int za = 1, zb = 1; for (int i = 0; i < dg; i++) { if (x[i] != '0') za = 0; if (y[i] != '0') zb = 0; }
    if (za) na = 0; if (zb) nb = 0;
    if (na != nb) return na ? -1 : 1;
    return na ? -c : c;
}
/* one literal against the item it gives a value to (the item itself, or
 * a condition-name's conditional variable) */
static void value_literal_check(Sym *x, Tok *v, int is_all, const char *who, int e85)
{
    if (x->is_group || x->is_index || x->usage == U_POINTER || x->usage == U_INDEX || x->usage == U_BIT) return;
    int cat = x->pi.category, fig = v->kind == T_WORD;
    if (cat == PIC_NUMERIC) {
        if (fig && strncmp(v->s, "zero", 4))
            die_at(v->line, "VALUE %s for the numeric %s '%s': a numeric item takes a numeric literal or ZERO (%s)", v->s, who, x->name,
                   e85 ? "X3.23-1985 VALUE general rule 1a" : "2023 13.18.63.3 rule 2");
        /* a nonnumeric literal of digits: CCVS-85 gives one to numeric
         * items (NC107A, NC108M), so it is taken, as before; one with
         * anything else is refused (an item's own as its image is built) */
        if (v->kind == T_STR && strcmp(who, "item"))
            for (int i = 0; i < v->len; i++)
                if (!isdigit((unsigned char)v->s[i]))
                    die_at(v->line, "VALUE \"%.*s\" for the numeric %s '%s': a numeric item takes a numeric literal (%s)", v->len, v->s, who, x->name,
                           e85 ? "X3.23-1985 VALUE general rule 1a" : "2023 13.18.63.3 rule 2");
        if (v->kind == T_NUM) {
            NumLit n; numlit_parse(v, &n);
            if (n.neg && !x->pi.is_signed)
                die_at(v->line, "VALUE %.*s: the %s '%s' is unsigned (%s)", v->len, v->s, who, x->name,
                       e85 ? "X3.23-1985 VALUE syntax rule 2" : "2023 13.18.63.3 rule 3");
            char d[40];
            int item = !strcmp(who, "item");       /* an item's own VALUE: its integer part is checked as the image is built */
            if (x->has_pic && x->pi.scale >= 0 && (!numlit_frac_fits(&n, x->pi.scale) || (!item && !numlit_align(&n, x->pi.digits, x->pi.scale, d))))
                die_at(v->line, "VALUE %.*s does not fit the PICTURE of the %s '%s' without losing nonzero digits (%s)", v->len, v->s, who, x->name,
                       e85 ? "X3.23-1985 VALUE syntax rule 3" : "2023 13.18.63.3 rule 2");
        }
        return;
    }
    if (cat == PIC_ALPHABETIC || cat == PIC_ALPHANUMERIC || cat == PIC_ALPHANUMERIC_EDITED) {
        if (v->kind == T_NUM)
            die_at(v->line, "a numeric VALUE for the %s %s '%s': it takes a nonnumeric literal (%s)", pic_category_name(cat), who, x->name,
                   e85 ? "X3.23-1985 VALUE general rule 1b" : "2023 13.18.63.3 rule 4");
        if (v->kind == T_STR && !is_all && !v->nat && !v->boolv && v->len > x->size && strcmp(who, "item"))   /* an item's own: as the image is built */
            die_at(v->line, "VALUE literal (%d characters) is longer than the %s '%s' (%d) (%s)", v->len, who, x->name, x->size,
                   e85 ? "X3.23-1985 VALUE syntax rule 3" : "2023 13.18.63.3 rule 4");
    }
}
static void value_rules_one(int i);
/* each entry on its own: an error is reported and the next one checked
 * (ISSUES-41), as the records' images are */
static void value_rules(void)
{
    for (int i = g_sym_base; i < g_nsym; i++) {
        jmp_buf jb, *outer = g_recover;
        if (setjmp(jb)) { g_recover = outer; continue; }
        g_recover = &jb;
        value_rules_one(i);
        g_recover = outer;
    }
}
static void value_rules_one(int i)
{
    int e85 = g_std < 2002;
    {
        Sym *s = &g_sym[i];
        if (s->is_ftemp || s->is_rename) return;
        if (s->is_cond) {
            Sym *x = &g_sym[s->parent];
            for (int k = 0; k < s->ncv; k++) {
                int all = (s->cv_all >> k) & 1;
                value_literal_check(x, s->cv_lo[k], all, "conditional variable", e85);
                if (!s->cv_hi[k]) continue;
                value_literal_check(x, s->cv_hi[k], 0, "conditional variable", e85);
                Tok *lo = s->cv_lo[k], *hi = s->cv_hi[k];
                int bad = 0;
                if (lo->kind == T_NUM && hi->kind == T_NUM) { NumLit a, b; numlit_parse(lo, &a); numlit_parse(hi, &b); bad = numlit_cmp(&a, &b) > 0; }
                else if (lo->kind == T_STR && hi->kind == T_STR && !g_collate_name[0]) {
                    int n = lo->len < hi->len ? lo->len : hi->len, c = memcmp(lo->s, hi->s, (size_t)n);
                    bad = c > 0 || (c == 0 && lo->len > hi->len);
                }
                if (bad)
                    die_at(lo->line, "'%s': VALUE ... THRU runs from the higher value to the lower (%s)", s->name,
                           e85 ? "X3.23-1985 condition-name rule 2" : "2023 13.18.63.3 rule 26");
            }
            if (s->cv_false) {
                Tok *f = s->cv_false;
                value_literal_check(x, f, 0, "conditional variable", e85);
                /* literal-4 is none of the values, nor inside a range (rule 27) */
                for (int k = 0; k < s->ncv; k++) {
                    Tok *lo = s->cv_lo[k], *hi = s->cv_hi[k] ? s->cv_hi[k] : s->cv_lo[k];
                    int in = 0;
                    if (f->kind == T_NUM && lo->kind == T_NUM && hi->kind == T_NUM) {
                        NumLit a, b, c; numlit_parse(lo, &a); numlit_parse(hi, &b); numlit_parse(f, &c);
                        in = numlit_cmp(&a, &c) <= 0 && numlit_cmp(&c, &b) <= 0;
                    } else if (f->kind == T_STR && lo->kind == T_STR && hi->kind == T_STR && (!s->cv_hi[k] || !g_collate_name[0])) {
                        int n1 = lo->len < f->len ? lo->len : f->len, n2 = hi->len < f->len ? hi->len : f->len;
                        int c1 = memcmp(lo->s, f->s, (size_t)n1), c2 = memcmp(f->s, hi->s, (size_t)n2);
                        if (!c1) c1 = lo->len - f->len;
                        if (!c2) c2 = f->len - hi->len;
                        in = s->cv_hi[k] ? (c1 <= 0 && c2 <= 0) : c1 == 0;
                    }
                    if (in) die_at(f->line, "'%s': the FALSE value is one of the condition's own values (2023 13.18.63.3 rule 27)", s->name);
                }
            }
            return;
        }
        if (!s->value_tok) return;
        if (e85 && s->record >= 0 && (g_sym[s->record].fd >= 0 || g_sym[s->record].is_linkage))
            die_at(s->value_tok->line, "'%s': in the %s SECTION a VALUE clause belongs to a condition-name; COBOL 2002 allows it (X3.23-1985 VALUE rule 5.15.6(1))",
                   s->name, g_sym[s->record].fd >= 0 ? "FILE" : "LINKAGE");
        if (!s->is_group) { value_literal_check(s, s->value_tok, s->value_all, "item", e85); return; }
        if (s->bitgroup || s->natgroup || s->strong) return;       /* their own rules, checked where they are initialized */
        Tok *v = s->value_tok;
        if (v->kind == T_STR && v->boolv) return;                  /* refused where the image is built, more precisely */
        if (v->kind == T_STR && !v->boolv && !s->value_all && !s->value_fig && v->len > s->size)
            die_at(v->line, "VALUE literal (%d characters) is longer than the group '%s' (%d) (%s)", v->len, s->name, s->size,
                   e85 ? "X3.23-1985 VALUE syntax rule 3" : "2023 13.18.63.3 rule 4");
        for (int j = i + 1; j < g_nsym; j++) {          /* the group's subordinates */
            Sym *q = &g_sym[j];
            int in = 0; for (int a = q->parent; a >= 0; a = g_sym[a].parent) if (a == i) { in = 1; break; }
            if (!in) { if (q->is_cond || q->is_index) continue; break; }
            if (q->is_cond) continue;
            if (q->value_tok)
                die_at(q->line, "'%s' has a VALUE clause inside the group '%s', which has one (%s)", q->name, s->name,
                       e85 ? "X3.23-1985 VALUE rule 5.15.6(3)" : "2023 13.18.63.3 rule 13");
            if (!q->is_group && (q->just || q->sync || (q->usage != U_DISPLAY)))
                die_at(q->line, "'%s' is %s, inside the group '%s' whose VALUE clause sets it as characters (%s)", q->name,
                       q->just ? "JUSTIFIED" : q->sync ? "SYNCHRONIZED" : "not USAGE DISPLAY", s->name,
                       e85 ? "X3.23-1985 VALUE rule 5.15.6(4)" : "2023 13.18.63.3 rule 14");
        }
    }
}
