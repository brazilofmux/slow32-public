/* s32-cobc: tree, layout, images.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- tree, layout, images ------------------------------------------- */

static void build_tree(void)
{
    int stack[64], sp = 0;              /* open items by level */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond) continue;
        if (s->is_index) { s->record = i; sym_finish(s); continue; }
        if (s->is_rename) {                 /* belongs to the record it follows, outside its tree */
            if (sp == 0) die_at(s->line, "level 66 '%s' follows no record", s->name);
            s->parent = stack[0];
            continue;
        }
        if (s->level == 1 || s->level == 77) { sp = 0; }
        else {
            while (sp > 0 && g_sym[stack[sp - 1]].level >= s->level) sp--;
            if (sp == 0) die_at(s->line, "level %02d '%s' has no group above it", s->level, s->name);
            if (g_sym[stack[sp - 1]].level == 77) die_at(s->line, "a level 77 item cannot have subordinates");
        }
        if (sp > 0) {
            int p = stack[sp - 1];
            s->parent = p;
            g_sym[p].is_group = 1;
            /* append as last child */
            if (g_sym[p].child < 0) g_sym[p].child = i;
            else { int c = g_sym[p].child; while (g_sym[c].sibling >= 0) c = g_sym[c].sibling; g_sym[c].sibling = i; }
        }
        stack[sp++] = i;
    }
    /* a group must not carry PICTURE/USAGE of its own; an item with no
     * children is elementary */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->is_index || s->is_rename) continue;
        /* a national group (2023 13.18.29.3): a group, with no USAGE of its
         * own; its subordinate groups are national groups and its
         * elementary items national.  Parents precede children here. */
        if (s->natgroup == 2 && !s->is_group) die_at(s->line, "GROUP-USAGE: '%s' is not a group (2023 13.18.29.3 rule 1)", s->name);
        if ((s->natgroup == 2 || s->bitgroup == 2) && s->strong)
            die_at(s->line, "GROUP-USAGE: '%s' is strongly typed (2023 13.18.29.3 rule 1)", s->name);
        /* a subordinate group of a bit or national group is one of the same
         * kind, by its own clause or implied -- never the other kind, and
         * with no USAGE of its own (rules 2 and 3) */
        if (s->parent >= 0 && s->is_group && (g_sym[s->parent].bitgroup || g_sym[s->parent].natgroup)) {
            int pb = g_sym[s->parent].bitgroup != 0;
            if (pb ? s->natgroup == 2 : s->bitgroup == 2)
                die_at(s->line, "'%s' is in the %s group '%s' and cannot be GROUP-USAGE %s (2023 13.18.29.3 rule %d)", s->name,
                       pb ? "bit" : "national", g_sym[s->parent].name, pb ? "NATIONAL" : "BIT", pb ? 2 : 3);
            if (s->has_usage && !(pb ? s->usage == U_BIT : 0))
                die_at(s->line, "'%s' is in the %s group '%s': a subordinate group is GROUP-USAGE %s, with no USAGE of its own (2023 13.18.29.3 rule %d)",
                       s->name, pb ? "bit" : "national", g_sym[s->parent].name, pb ? "BIT" : "NATIONAL", pb ? 2 : 3);
        }
        if (s->redefines >= 0 && (sym_in_strong(s) || sym_in_strong(&g_sym[s->redefines])))
            die_at(s->line, "'%s': a strongly-typed group is not redefined, in whole or in part (2023 13.18.57.3 rule 4)", s->name);
        if (s->bitgroup == 2 && !s->is_group) die_at(s->line, "GROUP-USAGE: '%s' is not a group (2023 13.18.29.3 rule 1)", s->name);
        if (s->bitgroup == 2 && s->has_usage) die_at(s->line, "GROUP-USAGE BIT: '%s' cannot have a USAGE clause too (2023 13.18.29.3 rule 2)", s->name);
        if (!s->bitgroup && s->parent >= 0 && g_sym[s->parent].bitgroup) {
            /* a bit group's subordinates are bit groups and USAGE BIT items (rule 2) */
            if (s->is_group) s->bitgroup = 1;
            else if (s->has_usage && s->usage != U_BIT)
                die_at(s->line, "'%s' is in the bit group '%s' and must be USAGE BIT (2023 13.18.29.3 rule 2)", s->name, g_sym[s->parent].name);
            else { s->usage = U_BIT; s->has_usage = 1; }
        }
        if (s->natgroup == 2 && (s->has_usage || s->nat_usage)) die_at(s->line, "GROUP-USAGE NATIONAL: '%s' cannot have a USAGE clause too (2023 13.18.29.3 rule 3)", s->name);
        if (!s->natgroup && s->parent >= 0 && g_sym[s->parent].natgroup) {
            if (s->is_group) s->natgroup = 1;
            else if (s->has_usage)
                die_at(s->line, "'%s' is in the national group '%s' and must be USAGE NATIONAL (2023 13.18.29.3 rule 3)",
                       s->name, g_sym[s->parent].name);
            else { s->nat_usage = 1; s->in_natgroup = 1; }        /* implied (rule 3); a PICTURE of X or A is refused when finished */
        }
        if (s->is_based && s->redefines >= 0) die_at(s->line, "'%s': a BASED entry takes no REDEFINES", s->name);
        if (s->any_len) {
            /* 2023 13.18.2.3 rules 1-2: an elementary level 1 LINKAGE entry,
             * PICTURE one X, N or 1 */
            if (!s->is_linkage || s->level != 1 || s->is_group)
                die_at(s->line, "'%s': ANY LENGTH is for an elementary level 1 entry of the LINKAGE SECTION (2023 13.18.2.3 rule 2)", s->name);
            if (!s->has_pic || !(!strcasecmp(s->pic, "x") || !strcasecmp(s->pic, "x(1)") || !strcasecmp(s->pic, "n") || !strcasecmp(s->pic, "n(1)")) ||
                (s->has_usage && s->usage != U_DISPLAY && s->usage != U_NATIONAL))
                die_at(s->line, "'%s': ANY LENGTH takes PICTURE X or N, one symbol (2023 13.18.2.3 rule 1; PICTURE 1 is not implemented)", s->name);
            if (!g_is_function && g_udepth == 0) bp(BP_E28_ANY_LENGTH_OUTER, s->line);
        }
        if (!s->is_group && s->usage == U_POINTER && s->level != 1 && !sym_in_strong(s))
            die_at(s->line, "'%s': a USAGE POINTER item is at level 1, or in a strongly-typed group (2023 13.18.60.3 rule 14; 2002 rule 13)", s->name);
        if (s->is_group && s->has_pic && s->standin) s->has_pic = 0;      /* it stood in for a group: no second error */
        if (s->is_group && s->has_pic)
            die_at(s->line, "'%s' is a group and cannot have a PICTURE (%s)", s->name,
                   g_std < 2002 ? "X3.23-1985 VI-21, data description entry general rule 1" : "2023 13.16.3 rule 11");
        if (s->is_group && s->blank_zero)
            die_at(s->line, "'%s' is a group and cannot have BLANK WHEN ZERO (%s)", s->name,
                   g_std < 2002 ? "X3.23-1985 VI-21, data description entry general rule 1" : "2023 13.16.3 rule 11");
        if (s->is_group && s->nat_usage) {
            /* USAGE NATIONAL on a group: every subordinate's, as any USAGE */
            for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
                if (g_sym[c].is_cond) continue;
                if (g_sym[c].has_usage) die_at(g_sym[c].line, "USAGE of '%s' contradicts the USAGE of its group '%s'", g_sym[c].name, s->name);
                g_sym[c].nat_usage = 1;
            }
        }
        if (s->is_group && s->has_usage) {
            /* USAGE on a group is every subordinate's that does not say
             * otherwise (X3.23 5.3.x); the children follow in the table, so
             * they are finished after this with the usage in place */
            for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
                if (g_sym[c].is_cond) continue;
                if (!g_sym[c].has_usage) { g_sym[c].usage = s->usage; g_sym[c].uvar = s->uvar; g_sym[c].has_usage = 1; }
                else if (g_sym[c].usage != s->usage || g_sym[c].uvar != s->uvar)
                    die_at(g_sym[c].line, "USAGE of '%s' contradicts the USAGE of its group '%s'", g_sym[c].name, s->name);
            }
        }
        if (!s->is_group) sym_finish(s);
    }
    /* no VALUE in an EXTERNAL record under 85, except on its 88s (X-23,
     * EXTERNAL syntax rule 3); 2023 lets INITIALIZE apply it (13.18.63) */
    if (g_std < 2002)
        for (int i = g_sym_base; i < g_nsym; i++) {
            const Sym *s = &g_sym[i];
            if (s->is_cond || !s->value_tok) continue;
            int r = i;
            while (g_sym[r].parent >= 0) r = g_sym[r].parent;
            if (g_sym[r].is_external)
                die_at(s->line, "'%s': no VALUE clause in or under the EXTERNAL record '%s', except on a level 88 (X3.23-1985 X-23, EXTERNAL syntax rule 3)",
                       s->name, g_sym[r].name);
        }
    /* level 88 parents: the item they follow; a 88 under an 88 shares it */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (!s->is_cond) continue;
        int p = s->parent;
        if (g_sym[p].is_cond) s->parent = g_sym[p].parent;
        const Sym *cv = &g_sym[s->parent];
        int e85 = g_std < 2002;
        if (cv->level == 66)
            die_at(s->line, "'%s': a level 66 entry is not a conditional variable (%s)", s->name,
                   e85 ? "X3.23-1985 VI-21, data description entry general rule 2b" : "2023 13.16.3 rule 24b");
        if (cv->is_group) {
            /* not a group with JUSTIFIED or SYNCHRONIZED items, nor (an
             * alphanumeric group, 2002 on) with items of another usage
             * than display (85 general rule 2c; 2023 rule 24c and d) */
            int alnum = !cv->natgroup && !cv->bitgroup;
            for (int j = s->parent + 1; j < g_nsym; j++) {
                const Sym *m = &g_sym[j];
                if (m->is_index) continue;               /* index-names sit among the entries */
                if (!sym_under(j, s->parent)) break;
                if (m->is_cond || m->level == 66) continue;
                const char *why = m->just ? "JUSTIFIED" : m->sync ? "SYNCHRONIZED" :
                                  (!m->is_group && m->usage != U_DISPLAY && (e85 || alnum) && !m->nat_usage) ? "a usage other than DISPLAY" : NULL;
                if (why) { bp(BP_E23_CONDNAME_GROUP, s->line); break; }   /* majesty's 88 ... VALUE HIGH-VALUES flags */
            }
        }
        if (!cv->is_group && (cv->usage == U_INDEX || cv->usage == U_POINTER))
            die_at(s->line, "'%s': a %s item is not a conditional variable (%s)", s->name,
                   cv->usage == U_INDEX ? "USAGE INDEX" : "USAGE POINTER",
                   g_std < 2002 && cv->usage == U_INDEX ? "X3.23-1985 USAGE syntax rule 7" : "2023 13.18.60.3 rule 11");
    }
}

static int align_of(Sym *s)
{
    if (!s->sync || s->is_group) return 1;
    switch (s->usage) {
    case U_BINARY: case U_COMP5: case U_SINT: case U_UINT: case U_SSHORT: case U_USHORT:
    case U_POINTER: case U_INDEX: case U_SDBL: case U_UDBL: case U_FLOAT:
        return s->size >= 8 ? 8 : s->size;
    default: return 1;
    }
}

/* lay out s at `base` (offset within the record); returns one occurrence's size */
/* a bit item or a bit group: laid out at bit positions (cobol ISSUES-78) */
static int sym_bitlike(const Sym *s) { return (!s->is_group && s->usage == U_BIT) || s->bitgroup; }
/* the bits a bit item or bit group takes, every occurrence of a bit
 * array's elements following one another (cobol ISSUES-84) */
static int bit_total(const Sym *s) { return s->bits * (!s->is_group && s->occurs ? s->occurs : 1); }
static int g_lay_bit;                   /* the bit offset the next layout() call starts at */

static int layout(int si, int base)
{
    Sym *s = &g_sym[si];
    int bo = g_lay_bit; g_lay_bit = 0;
    s->offset = base;
    if (sym_bitlike(s)) s->bitoff = bo;
    if (!s->is_group) {
        if (s->usage == U_BIT) s->size = (s->bitoff + bit_total(s) + 7) / 8;
        return s->size;
    }
    if (s->bitgroup && s->occurs)
        die_at(s->line, "'%s': OCCURS on a bit group is not implemented yet", s->name);
    int off = base, end = base;
    /* bit items and bit groups that follow one another at a level take
     * the next bit position; anything else the next byte (8.5.1.6.3) --
     * inside a bit group, from the group's own first bit */
    int run = s->bitgroup != 0, cur = s->bitgroup ? s->bitoff : 0;
    for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
        Sym *ch = &g_sym[c];
        /* SYNCHRONIZED on a bit item or bit group: implementor-defined
         * (8.5.1.6.3); here it starts at a byte, and what follows it at the
         * next byte (cobol ISSUES-93) */
        int cbase, isbit = sym_bitlike(ch) && ch->redefines < 0;
        if (ch->redefines >= 0) {
            /* the first bit of the redefined item (13.18.44.4 rule 1) -- a bit
             * item over a byte item starts at its first bit; a byte item
             * over a bit item needs that item to start a byte (cobol ISSUES-85) */
            Sym *t = &g_sym[ch->redefines];
            cbase = t->offset;
            if (sym_bitlike(ch)) g_lay_bit = sym_bitlike(t) ? t->bitoff : 0;
            else if (sym_bitlike(t) && t->bitoff)
                die_at(ch->line, "'%s' redefines '%s', which starts inside a byte (its bit %d): a character item at a bit position is not implemented (13.18.44.4 rule 1)", ch->name, t->name, t->bitoff + 1);
        } else if (isbit && run && !ch->sync && !ch->type_lvl1) {
            cbase = off; g_lay_bit = cur;
        } else {
            if (run && cur) off++;          /* leave the partly used byte */
            run = 0; cur = 0;
            int a = align_of(ch);
            cbase = (off + a - 1) / a * a;
        }
        int sz = layout(c, cbase);
        if (sz <= 0) die_at(ch->line, "'%s' has no size", ch->name);
        int cend;
        if (isbit) {
            int tot = ch->bitoff + bit_total(ch);
            off = cbase + tot / 8; cur = tot % 8; run = 1;
            cend = cbase + (tot + 7) / 8;
            if (ch->sync) { off = cend; cur = 0; run = 0; }
        } else {
            cend = cbase + (sym_bitlike(ch) ? sz : sz * (ch->occurs ? ch->occurs : 1));   /* a bit item's size spans its occurrences */
            if (ch->redefines < 0) off = cend;
            else if (!sym_bitlike(ch) && run) {
                /* a character item, REDEFINES or not, ends a run of bits: the
                 * next bit item follows it, not the bit before (8.5.1.6.3;
                 * cobol ISSUES-94 B17) */
                if (cur) off++;
                run = 0; cur = 0;
            }
        }
        /* A REDEFINES larger than the original is allowed: the group grows. */
        if (cend > end) end = cend;
    }
    if (s->bitgroup) s->bits = (off - base) * 8 + cur - s->bitoff;
    s->size = end - base;
    return s->size;
}

static void set_dims(int si, int ndims, const int *counts, const int *strides)
{
    Sym *s = &g_sym[si];
    int cnt[MAXDIM], str[MAXDIM];
    memcpy(cnt, counts, ndims * sizeof *cnt); memcpy(str, strides, ndims * sizeof *str);
    if (s->occurs) {
        if (ndims >= MAXDIM) die_at(s->line, "too many OCCURS levels");
        cnt[ndims] = s->occurs; str[ndims] = (!s->is_group && s->usage == U_BIT) ? 0 : s->size; ndims++;   /* bits: the element is a bit position */
    }
    s->ndims = ndims;
    memcpy(s->dim_count, cnt, ndims * sizeof *cnt); memcpy(s->dim_stride, str, ndims * sizeof *str);
    for (int c = s->child; c >= 0; c = g_sym[c].sibling) set_dims(c, ndims, cnt, str);
}

/* write VALUE / default initialisation for one instance of s at image+base */
static void init_instance(Sym *rec, int si, int base, int defaults);

/* a national figurative constant's character (2023 8.3.3.6) */
static unsigned nat_fig(const char *w)
{
    if (!strncmp(w, "zero", 4)) return 0x30;
    if (!strncmp(w, "space", 5)) return 0x20;
    if (!strncmp(w, "quote", 5)) return 0x22;
    if (!strncmp(w, "high-value", 10)) return 0xFFFF;
    return 0;                                       /* LOW-VALUE */
}

/* a national item's initial value: national spaces by default; a VALUE
 * must be a national literal no longer than the item, or a figurative
 * constant (2023 13.18.63 syntax rule 5) */
static void init_national(Sym *s, unsigned char *p, int defaults)
{
    int n = s->size / 2;
    if (defaults) for (int i = 0; i < n; i++) { p[2 * i] = 0; p[2 * i + 1] = 0x20; }
    if (!s->value_tok || g_no_values) return;
    Tok *v = s->value_tok;
    if (s->value_fig) {
        unsigned u = nat_fig(v->s);
        for (int i = 0; i < n; i++) { p[2 * i] = (unsigned char)(u >> 8); p[2 * i + 1] = (unsigned char)u; }
        return;
    }
    if (v->kind != T_STR || !v->nat) die_at(v->line, "the VALUE of the national item '%s' must be a national literal (N\"...\") or a figurative constant", s->name);
    if (s->value_all) { for (int i = 0; i < s->size; i++) p[i] = (unsigned char)v->s[i % v->len]; return; }
    if (v->len > s->size) die_at(v->line, "VALUE literal (%d national characters) is longer than '%s' (%d)", v->len / 2, s->name, n);
    memcpy(p, v->s, (size_t)v->len);
    for (int i = v->len / 2; i < n; i++) { p[2 * i] = 0; p[2 * i + 1] = 0x20; }
}

static void init_elem(Sym *s, unsigned char *p, int defaults);
static void init_one(Sym *rec, int si, int base, int defaults)
{
    Sym *s = &g_sym[si];
    unsigned char *p = rec->image + base;
    if (s->is_group) {
        if (s->bitgroup && s->value_tok && !g_no_values) {
            /* a bit group's VALUE: a boolean literal, ZERO or ALL B"...", over
             * the group's bits from its first, aligned left and zero-filled
             * as for a boolean item (13.18.63; cobol ISSUES-93); the items
             * in it take their defaults first */
            for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
                Sym *ch = &g_sym[c];
                init_instance(rec, c, base + (ch->offset - s->offset), ch->redefines >= 0 ? 0 : defaults);
            }
            Tok *v = s->value_tok;
            if (s->value_fig && strncmp(v->s, "zero", 4)) die_at(v->line, "VALUE %s is not a boolean value for the bit group '%s' (2023 14.9.25 rule 7)", v->s, s->name);
            if (!s->value_fig && (v->kind != T_STR || !v->boolv)) die_at(v->line, "the VALUE of the bit group '%s' must be a boolean literal (B\"...\") or ZERO", s->name);
            if (!s->value_fig && !s->value_all && v->len > s->bits) die_at(v->line, "VALUE literal (%d boolean positions) is longer than the bit group '%s' (%d)", v->len, s->name, s->bits);
            for (int i = 0; i < s->bits; i++) {
                char c = s->value_fig ? '0' : s->value_all ? v->s[i % (v->len ? v->len : 1)] : i < v->len ? v->s[i] : '0';
                int b = s->bitoff + i; unsigned char m = (unsigned char)(0x80 >> (b % 8));
                if (c == '1') p[b / 8] |= m; else p[b / 8] &= (unsigned char)~m;
            }
            return;
        }
        if (s->strong && s->value_tok && !g_no_values)
            die_at(s->value_tok->line, "a VALUE on the strongly-typed group '%s' (2023 13.18.63.3 rule 1)", s->name);
        if (s->natgroup && s->value_tok && !g_no_values) {
            init_national(s, p, 1);                 /* a national literal, as for PIC N (13.18.63 rule 5) */
            defaults = 0;
        } else if (s->value_tok && !g_no_values) {
            Tok *v = s->value_tok;
            if (v->kind != T_STR && !s->value_fig) die_at(v->line, "VALUE of the group '%s' must be a nonnumeric literal", s->name);
            if (v->kind == T_STR && v->boolv)
                die_at(v->line, "a boolean VALUE belongs to a bit group (GROUP-USAGE BIT); '%s' is an alphanumeric group (2023 13.18.29.4 rule 3)", s->name);
            if (s->value_fig) memset(p, fig_byte(v->s), s->size);
            else if (s->value_all) for (int i = 0; i < s->size; i++) p[i] = (unsigned char)v->s[i % v->len];
            else { int n = v->len < s->size ? v->len : s->size; memcpy(p, v->s, n); memset(p + n, ' ', s->size - n); }
            defaults = 0;
        }
        for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
            Sym *ch = &g_sym[c];
            int cbase = base + (ch->offset - s->offset);
            init_instance(rec, c, cbase, ch->redefines >= 0 ? 0 : defaults);
        }
        return;
    }
    init_elem(s, p, defaults);
}

/* an elementary item's initial value at p */
static void init_elem(Sym *s, unsigned char *p, int defaults)
{
    int numeric = is_numeric_sym(s);
    if (s->pi.category == PIC_NATIONAL) { init_national(s, p, defaults); return; }
    if (s->usage == U_BIT) {
        /* bits: initialized as the DISPLAY form, then packed at the item's
         * bit offset, the bits around it left as they are (cobol ISSUES-78) */
        int n = s->bits;
        unsigned char *t = xmalloc((size_t)n + 1);
        for (int i = 0; i < n; i++) { int b = s->bitoff + i; t[i] = (unsigned char)('0' + ((p[b / 8] >> (7 - b % 8)) & 1)); }
        int sz = s->size;
        s->usage = U_DISPLAY; s->size = n;
        init_elem(s, t, defaults);
        s->usage = U_BIT; s->size = sz;
        for (int i = 0; i < n; i++) {
            int b = s->bitoff + i; unsigned char m = (unsigned char)(0x80 >> (b % 8));
            if (t[i] == '1') p[b / 8] |= m; else p[b / 8] &= (unsigned char)~m;
        }
        free(t);
        return;
    }
    if (s->pi.category == PIC_BOOLEAN && s->usage == U_DISPLAY) {
        /* boolean: zeros by default; a VALUE is a boolean literal or ZERO,
         * aligned left and zero-filled (2023 13.18.63; 14.6.8.6) */
        if (defaults) memset(p, '0', s->size);
        if (!s->value_tok || g_no_values) return;
        Tok *v = s->value_tok;
        if (s->value_fig) {
            if (strncmp(v->s, "zero", 4)) die_at(v->line, "VALUE %s is not a boolean value for '%s' (2023 14.9.25 rule 7)", v->s, s->name);
            memset(p, '0', s->size);
            return;
        }
        if (v->kind != T_STR || !v->boolv) die_at(v->line, "the VALUE of the boolean item '%s' must be a boolean literal (B\"...\") or ZERO", s->name);
        if (s->value_all) { for (int i = 0; i < s->size; i++) p[i] = (unsigned char)v->s[i % (v->len ? v->len : 1)]; return; }
        if (v->len > s->size) die_at(v->line, "VALUE literal (%d boolean positions) is longer than '%s' (%d)", v->len, s->name, s->size);
        memcpy(p, v->s, (size_t)v->len);
        memset(p + v->len, '0', (size_t)(s->size - v->len));
        return;
    }
    if (s->usage == U_NATIONAL) {
        /* numeric national: initialized as its DISPLAY form, then widened;
         * a nonnumeric VALUE is a national literal (13.18.63 rule 5) */
        int n = s->size / 2;
        unsigned char *t = xmalloc((size_t)n + 1);
        for (int i = 0; i < n; i++) t[i] = p[2 * i + 1];
        Tok *save = s->value_tok, narrow;
        if (save && save->kind == T_STR && !s->value_fig && !(save->boolv && s->pi.category == PIC_BOOLEAN)) {
            if (!save->nat) die_at(save->line, "the VALUE of the USAGE NATIONAL item '%s' must be a national literal (N\"...\") (2023 13.18.63 rule 5)", s->name);
            narrow = *save; narrow.nat = 0; narrow.len = save->len / 2; narrow.s = xmalloc((size_t)narrow.len + 1);
            for (int i = 0; i < narrow.len; i++) {
                if (save->s[2 * i]) die_at(save->line, "the VALUE of '%s' holds a character that is no digit, sign or editing symbol", s->name);
                narrow.s[i] = save->s[2 * i + 1];
            }
            s->value_tok = &narrow;
        }
        s->usage = U_DISPLAY; s->size = n;
        init_elem(s, t, defaults);
        s->usage = U_NATIONAL; s->size = 2 * n; s->value_tok = save;
        for (int i = 0; i < n; i++) { p[2 * i] = 0; p[2 * i + 1] = t[i]; }
        free(t);
        return;
    }
    if (defaults) {
        if (s->usage == U_DISPLAY && !numeric) memset(p, ' ', s->size);
        else if (s->usage == U_DISPLAY) {
            memset(p, '0', s->size);
            if (s->sign_sep) p[s->sign_lead ? 0 : s->size - 1] = '+';       /* a separate sign of zero */
        }
        else if (s->usage == U_PACKED) { NumLit z; numlit_zero(&z); store_numeric(s, &z, p, s->line); }
        else memset(p, 0, s->size);
    }
    if (!s->value_tok || g_no_values) return;
    Tok *v = s->value_tok;
    if (s->value_fig) {
        int fill = fig_byte(v->s);
        if (numeric) {
            if (!strncmp(v->s, "zero", 4)) { NumLit z; numlit_zero(&z); store_numeric(s, &z, p, v->line); }
            else if (s->usage == U_DISPLAY && (fill == ' ' || fill == 0 || fill == 0xFF)) memset(p, fill, s->size);
            else die_at(v->line, "VALUE %s is not valid for the numeric item '%s'", v->s, s->name);
        } else memset(p, fill, s->size);
        return;
    }
    if (v->kind == T_NUM) {
        if (s->pi.category == PIC_NUMERIC_EDITED)
            die_at(v->line, g_std < 2002 ? "the VALUE of the numeric-edited item '%s' must be a nonnumeric literal (X3.23-1985 VALUE general rule 1b)"
                                         : "a numeric VALUE for the numeric-edited item '%s', edited as a MOVE would (2023 13.18.63.3 rule 6), is not implemented; write it edited, as \"...\"", s->name);
        if (!numeric) die_at(v->line, "a numeric VALUE is not valid for the alphanumeric item '%s'", s->name);
        NumLit n; numlit_parse(v, &n);
        store_numeric(s, &n, p, v->line);
        return;
    }
    if (numeric && s->usage != U_DISPLAY)
        die_at(v->line, "a nonnumeric VALUE is not valid for the %s item '%s'", usage_name(s->usage), s->name);
    if (s->value_all) {
        if (v->len < 1) die_at(v->line, "VALUE ALL of an empty literal");
        for (int i = 0; i < s->size; i++) p[i] = (unsigned char)v->s[i % v->len];
        return;
    }
    if (v->len > s->size && !numeric && g_dialect_mf) {
        /* BP-D7, by the user's ruling: cut on the right -- initialization is
         * not affected by JUSTIFIED (2023 13.18.63.4 rule 7) -- and always
         * said */
        bp(BP_D7_MF_VALUE_TRUNCATED, v->line);
        warn_at(v->line, "the VALUE literal (%d characters) is cut to '%s' (%d)", v->len, s->name, s->size);
        memcpy(p, v->s, (size_t)s->size);
        return;
    }
    if (v->len > s->size)
        die_at(v->line, "VALUE literal (%d characters) is longer than '%s' (%d)", v->len, s->name, s->size);
    if (numeric) {
        for (int i = 0; i < v->len; i++)
            if (!isdigit((unsigned char)v->s[i])) die_at(v->line, "VALUE of the numeric item '%s' must be numeric", s->name);
        memset(p, '0', s->size);
        memcpy(p + s->size - v->len, v->s, v->len);
    } else {
        memcpy(p, v->s, v->len);
        memset(p + v->len, ' ', s->size - v->len);
    }
}

static void init_instance(Sym *rec, int si, int base, int defaults)
{
    Sym *s = &g_sym[si];
    int n = s->occurs ? s->occurs : 1;
    if (!s->is_group && s->usage == U_BIT) {
        /* a bit array: each occurrence at the next bits (cobol ISSUES-84) */
        int bo = s->bitoff;
        for (int k = 0; k < n; k++) { s->bitoff = bo + k * s->bits; init_one(rec, si, base, defaults); }
        s->bitoff = bo;
        return;
    }
    for (int k = 0; k < n; k++) init_one(rec, si, base + k * s->size, defaults);
}

/* A record's initial image.  An error in its VALUE clauses is reported
 * and the compile goes on to the next record (ISSUES-41): the images are
 * independent, and no code is generated once anything has failed. */
static void init_record(Sym *rec, int si, int defaults)
{
    jmp_buf jb, *outer = g_recover;
    if (setjmp(jb)) { g_recover = outer; return; }
    g_recover = &jb;
    init_instance(rec, si, 0, defaults);
    g_recover = outer;
}

/* a split key (2002 12.3.4.12 SOURCE IS; Micro Focus's "=", BP-D2): the
 * concatenation of its parts, kept in a slot of a tail the record area
 * gains, which the runtime fills from the parts before each keyed
 * operation (libcob split_fill).  The record-key-name is an item over its
 * slot -- READ and START name it as they name any key. */
static int sym_is_national(const Sym *s);
static Sym *odo_table_for(Sym *s);
static Sym *split_key_make(File *f, const char *name, char (*parts)[64], int n, int mf, int *tail, int *w, int *nw)
{
    int rec = g_sym[f->rec].record, len = 0, nat = -1;
    w[(*nw)++] = *tail; w[(*nw)++] = n;
    for (int k = 0; k < n; k++) {
        Sym *d = NULL; int nd = 0;
        for (int j = 0; j < g_nsym; j++)
            if (!g_sym[j].is_cond && !g_sym[j].is_filler && !g_sym[j].split_key && !strcmp(g_sym[j].name, parts[k]) && g_sym[j].record == rec) { d = &g_sym[j]; nd++; }
        if (!d) die_at(f->line, "the split key '%s': '%s' is not an item of file '%s'", name, parts[k], f->name);
        if (nd > 1) die_at(f->line, "the split key '%s': '%s' is ambiguous in file '%s'", name, parts[k], f->name);
        if (d->ndims) die_at(f->line, "the split key '%s': '%s' is in a table", name, parts[k]);
        if (odo_table_for(d)) die_at(f->line, "the split key '%s': '%s' is of variable length (2002 12.3.4.12 rule 3)", name, parts[k]);
        int dn = sym_is_national(d) || (!d->is_group && d->usage == U_NATIONAL);
        if (!mf) {
            if (!d->is_group && d->pi.category != PIC_ALPHANUMERIC && d->pi.category != PIC_NATIONAL)
                die_at(f->line, "the split key '%s': '%s' is not alphanumeric or national (2002 12.3.4.12 rule 2; Micro Focus's '=' form takes any)", name, parts[k]);
            if (nat >= 0 && nat != dn) die_at(f->line, "the split key '%s': its parts are all of one category (2002 12.3.4.12 rule 2)", name);
        }
        nat = dn;
        w[(*nw)++] = d->offset; w[(*nw)++] = d->size;
        len += d->size;
    }
    if (len > 255) die_at(f->line, "the split key '%s' is %d bytes; a key is 1 to 255 here", name, len);
    Sym *k = sym_new();
    snprintf(k->name, sizeof k->name, "%s", name);
    k->level = 1; k->line = f->line; k->record = rec; k->redefines = rec; k->parent = -1; k->fd = -1;
    k->offset = *tail; k->size = len; k->split_key = 1; k->desc_id = -1; k->is_filler = 0;
    k->has_pic = 1;
    if (nat == 1 && !mf) { snprintf(k->pic, sizeof k->pic, "n(%d)", len / 2); k->usage = U_NATIONAL; }
    else { snprintf(k->pic, sizeof k->pic, "x(%d)", len); k->usage = U_DISPLAY; }
    if (pic_analyse(k->pic, &k->pi) < 0) die_at(f->line, "internal: split key picture");
    *tail += len;
    return k;
}

static void finish_data_division(void)
{
    /* ASSIGN TO a data-name declared nowhere (BP-D6): Micro Focus declares
     * it, an alphanumeric item long enough for a file name (its SELECT
     * rule 4) -- 1024 characters here, the reference leaving the size to
     * the operating system.  A WORKING-STORAGE 01 like any other, made
     * before the records are put together */
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->assign_name[0] || sym_lookup_quiet(f->assign_name)) continue;
        int dup = 0;
        for (int j = g_file_base; j < i; j++) if (!strcmp(g_files[j].assign_name, f->assign_name)) dup = 1;
        if (dup) continue;
        bp(BP_D6_MF_ASSIGN_IMPLICIT, f->line);     /* without -dialect=mf: refused, naming the switch */
        Sym *s = sym_new();
        snprintf(s->name, sizeof s->name, "%s", f->assign_name);
        s->level = 1; s->line = f->line; s->usage = U_DISPLAY;
        s->has_pic = 1; snprintf(s->pic, sizeof s->pic, "x(1024)");
        if (pic_analyse(s->pic, &s->pi) < 0) die_at(f->line, "internal: implicit ASSIGN item");
    }
    build_tree();
    /* GLOBAL reaches down: a GLOBAL item's subordinates and conditions, the
     * records of a GLOBAL FD (parents precede children in the table) */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->fd >= 0 && g_files[s->fd].global) s->is_global = 1;
        if (s->parent >= 0 && g_sym[s->parent].is_global) s->is_global = 1;
        if (s->fd >= 0 && g_files[s->fd].external && s->parent < 0) s->is_external = 1;
    }
    for (int i = g_file_base; i < g_nfile; i++)
        if (!g_files[i].external && !g_files[i].assign_lit && !g_files[i].assign_name[0] && !g_files[i].report_name[0])
            die_at(g_files[i].line, "SELECT %s names nothing in ASSIGN TO (only an EXTERNAL file may leave it to another program)", g_files[i].name);
    int nrec = 0;
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0) continue;
        /* a record: 01, 77, or an index */
        int zero[1] = { 0 };
        if (s->rep_ctr >= 0) {
            /* LINE-COUNTER / PAGE-COUNTER: cells of the report block (line_counter at 20, page_counter at 24) */
            snprintf(s->label, sizeof s->label, ".Lrpt%d_%d", g_unit, s->rep_ctr);
            s->record = i;
            continue;
        }
        if (s->lin_file >= 0) {
            /* LINAGE-COUNTER: the cell in the file's cob_file image */
            s->record = i; s->offset = COB_FILE_LIN_COUNTER_OFF;
            snprintf(s->label, sizeof s->label, ".Lf%d_%d", g_files[s->lin_file].unit, s->lin_file);
            continue;
        }
        layout(i, 0);
        set_dims(i, 0, zero, zero);
        s->record = i;
        if (s->is_linkage) snprintf(s->label, sizeof s->label, ".Llk%d_%d", g_unit, nrec++);
        else if (s->is_local) snprintf(s->label, sizeof s->label, ".Lls%d_%d", g_unit, nrec++);
        else if (s->is_external) snprintf(s->label, sizeof s->label, ".Lex%d_%d", g_unit, nrec++);
        else snprintf(s->label, sizeof s->label, "ws%d_%d", g_unit, nrec++);
    }
    /* propagate record ownership down, and 88s take their parent's dims */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond) {
            Sym *p = &g_sym[s->parent];
            s->record = p->record; s->ndims = p->ndims;
            memcpy(s->dim_count, p->dim_count, sizeof s->dim_count);
            memcpy(s->dim_stride, p->dim_stride, sizeof s->dim_stride);
            continue;
        }
        int r = i; while (g_sym[r].parent >= 0) r = g_sym[r].parent;
        s->record = r;
    }
    /* SAME RECORD AREA: no GLOBAL on its files or their records
     * (X3.23-1985 X-24, GLOBAL syntax rule 3; 2023 13.18.27.3 rule 2) */
    for (int g = 0; g < g_nsame_groups; g++)
        for (int k = 0; k < g_nsame[g]; k++) {
            int fi = g_same[g][k];
            int glob = g_files[fi].global;
            for (int i = g_sym_base; i < g_nsym && !glob; i++)
                if (g_sym[i].fd == fi && g_sym[i].level == 1 && g_sym[i].is_global) glob = 1;
            if (glob)
                die_at(g_files[fi].line, "file '%s' is in a SAME RECORD AREA, so neither it nor its records can be GLOBAL (%s)", g_files[fi].name,
                       g_std < 2002 ? "X3.23-1985 X-24, GLOBAL syntax rule 3" : "2023 13.18.27.3 rule 2");
        }
    /* SAME RECORD AREA: the later files' first 01s redefine the first file's */
    for (int g = 0; g < g_nsame_groups; g++)
        for (int k = 1; k < g_nsame[g]; k++) {
            File *a = &g_files[g_same[g][0]], *b = &g_files[g_same[g][k]];
            if (a->rec < 0 || b->rec < 0) die_at(b->line, "SAME RECORD AREA: file '%s' has no record description", b->name);
            if (g_sym[b->rec].redefines < 0) g_sym[b->rec].redefines = a->rec;
        }
    occurs_rules();
    value_rules();
    /* the REDEFINES rules that need sizes and subordinates (2023
     * 13.18.44.3 rules 5, 8, 9, 12, 14; X3.23-1985 REDEFINES rules 5, 6, 9) */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->redefines < 0 || !s->redef_clause) continue;
        Sym *o = &g_sym[s->redefines];
        int e85 = g_std < 2002;
        if (o->occurs)
            die_at(s->line, "'%s' REDEFINES '%s', which has an OCCURS clause; redefine an item containing the table, or one inside it (%s)",
                   s->name, o->name, e85 ? "X3.23-1985 REDEFINES syntax rule 5" : "2023 13.18.44.3 rule 5");
        if (odo_table_for(s) != odo_table_for(o) || (s->is_group && odo_table_below(s)))
            die_at(s->line, "'%s' REDEFINES '%s': neither may include an OCCURS DEPENDING ON table (%s)",
                   s->name, o->name, e85 ? "X3.23-1985 REDEFINES syntax rule 5" : "2023 13.18.44.3 rule 5");
        if (!e85 && ((!s->is_group && s->usage == U_POINTER) || (!o->is_group && o->usage == U_POINTER)))
            die_at(s->line, "'%s' REDEFINES '%s': a pointer item is neither redefined nor a redefinition (2023 13.18.44.3 rules 12 and 14)", s->name, o->name);
        if (!sym_bitlike(s) && !sym_bitlike(o) && !(o->level == 1 && !o->is_external)) {
            long ssz = (long)s->size * (s->occurs ? s->occurs : 1);
            if (ssz > o->size)
                die_at(s->line, "'%s' (%ld bytes) REDEFINES the smaller '%s' (%d bytes); only a level 01 item that is not EXTERNAL may be redefined by a larger one (%s)",
                       s->name, ssz, o->name, o->size, e85 ? "X3.23-1985 REDEFINES syntax rule 6" : "2023 13.18.44.3 rule 8");
        }
        for (int j = i; j < g_nsym; j++) {             /* this entry and its subordinates: no VALUE but at level 88 */
            Sym *q = &g_sym[j];
            if (j > i) { int in = 0; for (int a = q->parent; a >= 0; a = g_sym[a].parent) if (a == i) { in = 1; break; } if (!in) { if (!q->is_cond && !q->is_index) break; continue; } }
            if (!q->is_cond && q->value_tok)
                die_at(q->line, "'%s' is %s REDEFINES entry and cannot have a VALUE clause; only a level 88 below it can (%s)", q->name,
                       j == i ? "a" : "under a", e85 ? "X3.23-1985 REDEFINES syntax rule 9" : "2023 13.18.44.3 rule 9");
        }
    }
    /* 01 REDEFINES 01: share the earlier record's storage */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines < 0) continue;
        int r = s->redefines;
        while (g_sym[r].redefines >= 0) r = g_sym[r].redefines;
        s->record = r;
        strcpy(s->label, g_sym[r].label);
        if (s->size > g_sym[r].image_size && s->size > g_sym[r].size) g_sym[r].image_size = s->size;
        for (int j = 0; j < g_nsym; j++) if (g_sym[j].record == i) g_sym[j].record = r;
    }
    /* RENAMES: the range from a to the end of b (or a alone) in the
     * record; a alone and elementary is an alias, anything else a group */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (!s->is_rename) continue;
        /* the names are the record's own: its name is an implicit last qualifier */
        char *aq[9], *bq[9]; int naq = s->rn_naq, nbq = s->rn_nbq;
        for (int k = 0; k < 8; k++) { aq[k] = s->rn_aq[k]; bq[k] = s->rn_bq[k]; }
        Sym *rec = &g_sym[s->parent];       /* the 01 the entry follows (a REDEFINES 01 keeps its own name) */
        if (!rec->is_filler && !(naq && !strcmp(aq[naq - 1], rec->name))) aq[naq++] = rec->name;
        if (!rec->is_filler && !(nbq && !strcmp(bq[nbq - 1], rec->name))) bq[nbq++] = rec->name;
        int e85 = g_std < 2002;
        /* a record, a 77 or another record's item: say which rule, not
         * "not declared" (85 rules 2, 4; 2023 rules 2, 4, 5) */
        for (int k = 0; k < 2; k++) {
            const char *nm = k ? s->rn_b : s->rn_a;
            if (!nm[0]) continue;
            Sym *q = sym_lookup_quiet(nm);
            int inrec = 0;                          /* a 02-49 item of that name in this record: the lookup below finds it */
            for (int j = g_sym_base; j < g_nsym && !inrec; j++)
                if (!g_sym[j].is_cond && g_sym[j].level > 1 && g_sym[j].level < 50 && g_sym[j].record == s->record && !strcmp(g_sym[j].name, nm)) inrec = 1;
            if (!inrec && (!strcmp(nm, rec->name) || (q && (q->level == 1 || q->level == 77) && !q->is_filler)))
                die_at(s->line, "RENAMES '%s': '%s' is a level %02d entry; a RENAMES entry names items at levels 02-49 (%s)", s->name, nm,
                       !strcmp(nm, rec->name) ? rec->level : q->level, e85 ? "X3.23-1985 RENAMES syntax rule 4" : "2023 13.18.45.3 rule 5");
        }
        Sym *a = sym_lookup(s->rn_a, aq, naq, s->line), *b = NULL;
        if (s->rn_b[0]) b = sym_lookup(s->rn_b, bq, nbq, s->line);
        Sym *chk[2] = { a, b };
        for (int k = 0; k < 2; k++) {
            Sym *x = chk[k];
            if (!x) continue;
            if (x->record != s->record) die_at(s->line, "RENAMES '%s': '%s' is not in the same record", s->name, x->name);
            if (x->level == 1 || x->level == 66 || x->level == 77 || x->is_cond) die_at(s->line, "RENAMES '%s': '%s' is not a level 02-49 item", s->name, x->name);
            if (sym_in_strong(x)) die_at(s->line, "RENAMES '%s': '%s' is in a strongly-typed group (2023 13.18.57.3 rule 3)", s->name, x->name);
            if (x->ndims) die_at(s->line, "RENAMES '%s': '%s' has OCCURS or lies in a table", s->name, x->name);
        }
        if (b == a) die_at(s->line, "RENAMES '%s': THRU names '%s' again; the two data-names differ (%s)", s->name, a->name,
                           e85 ? "X3.23-1985 RENAMES syntax rule 4" : "2023 13.18.45.3 rule 4");
        long aend = (long)a->offset + a->size;
        if (b && (b->offset < a->offset || (long)b->offset + b->size <= aend))
            die_at(s->line, "RENAMES '%s': '%s' must begin no earlier than '%s' and end after it (%s)", s->name, b->name, a->name,
                   e85 ? "X3.23-1985 RENAMES syntax rule 8" : "2023 13.18.45.3 rule 11");
        for (int k = 0; k < 2; k++) {               /* whole bytes (2023 rule 10) */
            Sym *x = chk[k];
            if (x && sym_bitlike(x) && ((k == 0 && x->bitoff) || (k == 1 && (x->bitoff + bit_total(x)) % 8)))
                die_at(s->line, "RENAMES '%s': the range starts or ends inside a byte at '%s' (2023 13.18.45.3 rule 10)", s->name, x->name);
        }
        int end = b ? (int)(b->offset + b->size) : (int)(a->offset + a->size);
        for (int j = g_sym_base; j < g_nsym; j++) {  /* nothing in the range strongly typed (2023 rule 8) */
            Sym *q = &g_sym[j];
            if (q->is_cond || q->is_rename || q->record != s->record || q->level == 1) continue;
            if (q->offset >= a->offset && q->offset < end && sym_in_strong(q))
                die_at(s->line, "RENAMES '%s': '%s' in the range is in a strongly-typed group (2023 13.18.45.3 rule 8)", s->name, q->name);
        }
        s->offset = a->offset; s->size = end - (int)a->offset; s->ndims = 0;
        if (!b && !a->is_group) {
            s->usage = a->usage; s->uvar = a->uvar; s->has_usage = a->has_usage; s->pi = a->pi; s->has_pic = a->has_pic;
            memcpy(s->pic, a->pic, sizeof s->pic); s->sign_lead = a->sign_lead; s->sign_sep = a->sign_sep;
            s->is_group = 0;
        } else s->is_group = 1;
    }
    /* OCCURS DEPENDING ON: the item must be an integer outside the table */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (!s->odo_dep[0]) continue;
        s->odo_dep_sym = sym_lookup(s->odo_dep, NULL, 0, s->line);
        if (!is_int_item(s->odo_dep_sym)) die_at(s->line, "DEPENDING ON '%s' must be an integer item", s->odo_dep);
        if (s->odo_dep_sym->record == s->record && s->odo_dep_sym->offset >= s->offset)
            die_at(s->line, "DEPENDING ON '%s' must not be inside or after the table", s->odo_dep);
        /* the table may be followed in its record only by entries
         * subordinate to it (X3.23-1985 OCCURS format 2 syntax rule 10;
         * 2023 13.18.38.3 rule 22): no item after it at any level above */
        for (Sym *k = s; k->parent >= 0 && k->level != 1; k = &g_sym[k->parent])
            for (int c = k->sibling; c >= 0; c = g_sym[c].sibling)
                if (g_sym[c].level != 88 && g_sym[c].level != 66)
                    die_at(g_sym[c].line, "'%s' follows the OCCURS DEPENDING ON table '%s' in its record, which only the table's own subordinate entries may (2023 13.18.38.3 rule 22)",
                           g_sym[c].name, s->name);
    }
    /* files: names, status, the record area */
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if ((f->org == COB_ORG_SEQ || f->org == COB_ORG_LINESEQ) && f->access)
            die_at(f->line, "file '%s' is sequential: ACCESS %s is for a relative or indexed file (%s)", f->name, f->access == 1 ? "RANDOM" : "DYNAMIC",
                   g_std < 2002 ? "X3.23-1985 sequential file control entry format" : "2023 12.4.5.5.2 rule 2");
        if (f->rec < 0 && !f->report_name[0]) {
            if (!f->fd_line) die_at(f->line, "file '%s' has no FD", f->name);
            if (g_std < 2002) die_at(f->fd_line, "FD %s has no record description entry (X3.23-1985 file description syntax rule 3)", f->name);
            die_at(f->fd_line, "FD %s without a record description entry (READ INTO, WRITE FILE ... FROM; 2023 13.4.5.3 rule 3) is not implemented", f->name);
        }
        for (int d = 0; d < f->ndata_rec; d++) {        /* DATA RECORDS names its own 01s (85 DATA RECORDS rule 1) */
            int ok = 0;
            for (int j = 0; j < g_nsym && !ok; j++) if (g_sym[j].fd == i && g_sym[j].level == 1 && !strcmp(g_sym[j].name, f->data_rec[d])) ok = 1;
            if (!ok) die_at(f->fd_line, "FD %s: DATA RECORDS names '%s', which is not one of its record descriptions (X3.23-1985 DATA RECORDS syntax rule 1)", f->name, f->data_rec[d]);
        }
        if (f->assign_name[0]) {
            f->assign_sym = sym_lookup(f->assign_name, NULL, 0, f->line);
            /* a LINKAGE, LOCAL-STORAGE or EXTERNAL item names the file as well
             * as any other (2023 12.4.5.2 rule 7 forbids only an item of the
             * file's own record): its address goes into the file at entry,
             * as a FILE STATUS item's does.  Its length must be known. */
            if (f->assign_sym->any_len)
                die_at(f->line, "ASSIGN TO '%s': an item of ANY LENGTH cannot name a file yet", f->assign_name);
            if (f->assign_sym->record >= 0 && g_sym[f->assign_sym->record].fd == i)
                die_at(f->line, "ASSIGN TO '%s': an item of the file's own record cannot name it (2023 12.4.5.2 rule 7)", f->assign_name);
            /* a group is alphanumeric by the standard's own rules: the suite
             * builds "GENTBL." + module suffix that way (GitHub #34) */
            if (!f->assign_sym->is_group && f->assign_sym->pi.category == PIC_NUMERIC)
                die_at(f->line, "ASSIGN TO '%s': the data-name must be alphanumeric", f->assign_name);
        }
        if (f->status_name[0]) {
            char *sq[1] = { f->status_qual };
            f->status_sym = sym_lookup(f->status_name, sq, f->status_qual[0] ? 1 : 0, f->line);
            if (f->status_sym->size != 2) die_at(f->line, "FILE STATUS '%s' must be PIC XX", f->status_name);
            if (f->status_sym->ndims || (f->status_sym->record >= 0 && g_sym[f->status_sym->record].fd >= 0))
                die_at(f->line, "FILE STATUS '%s': %s (%s)", f->status_name, f->status_sym->ndims ? "an item in a table" : "an item of a file's record; it belongs in WORKING-STORAGE, LOCAL-STORAGE or LINKAGE",
                       g_std < 2002 ? "X3.23-1985 FILE STATUS syntax rule 2" : "2023 12.4.5.8.3 rules 1-2");
            if (!f->status_sym->is_group && f->status_sym->pi.category != PIC_ALPHANUMERIC) bp(BP_E17_NUMERIC_STATUS, f->line);
        }
        int minrec = 0;
        for (int j = 0; j < g_nsym; j++)
            if (g_sym[j].fd == i && g_sym[j].level == 1) {
                if (g_sym[j].size > f->recsize) f->recsize = g_sym[j].size;
                if (!minrec || g_sym[j].size < minrec) minrec = g_sym[j].size;
            }
        /* 01s of different lengths under a sequential FD: mode V, as cobc370 infers */
        if (f->org == COB_ORG_SEQ && minrec && minrec != f->recsize) f->varying = 1;
        /* RECORD CONTAINS larger than the 01s: the record area is that
         * size (GnuCOBOL's reading of majesty's sglentry, 98 over a
         * 92-byte 01); smaller is a contradiction */
        if (f->maxlen && f->recsize && f->maxlen < f->recsize && f->rec >= 0 && !f->dep_name[0])
            die_at(f->line, "FD %s: RECORD CONTAINS says %d characters but the largest 01 is %d", f->name, f->maxlen, f->recsize);
        if (f->rc_given && f->maxlen > f->minlen && minrec && minrec < f->minlen)
            die_at(f->fd_line, "FD %s: RECORD CONTAINS %d TO %d, but a record description is %d characters (%s)", f->name, f->minlen, f->maxlen, minrec,
                   g_std < 2002 ? "X3.23-1985 RECORD syntax rule 2" : "2023 13.18.43.3 rule 4");
        if (f->maxlen > f->recsize && f->rec >= 0) f->recsize = f->maxlen;
        if (f->dep_name[0]) {
            f->dep_sym = sym_lookup(f->dep_name, NULL, 0, f->line);
            if (!is_int_item(f->dep_sym)) die_at(f->line, "DEPENDING ON '%s' must be an integer item", f->dep_name);
            if (f->dep_sym->pi.is_signed || f->dep_sym->is_group || (f->dep_sym->record >= 0 && g_sym[f->dep_sym->record].fd >= 0))
                die_at(f->line, "RECORD ... DEPENDING ON '%s': an elementary unsigned integer in WORKING-STORAGE, LOCAL-STORAGE or LINKAGE (%s)", f->dep_name,
                       g_std < 2002 ? "X3.23-1985 RECORD syntax rule 4" : "2023 13.18.43.3 rule 6");
            if (rec_indirect(&g_sym[f->dep_sym->record]))
                die_at(f->line, "DEPENDING ON '%s' cannot be a %s item", f->dep_name, indirect_kind(&g_sym[f->dep_sym->record]));
            if (!f->maxlen) f->maxlen = f->recsize;
            if (f->maxlen > f->recsize) die_at(f->line, "FD %s: VARYING TO %d is larger than its record area (%d)", f->name, f->maxlen, f->recsize);
        }
        int tail = f->recsize, sw[1 + 17 * 34], nsw = 1, nspl = 0;    /* the split keys' slots follow the record */
        {
            int any = f->nksplit != 0;
            for (int a = 0; a < f->nalt; a++) any |= f->alt[a].nsplit != 0;
            if (any) {
                if (f->org != COB_ORG_INDEXED) die_at(f->line, "file '%s': a split key is for an INDEXED file", f->name);
                if (f->rec < 0) die_at(f->line, "file '%s': a split key needs the file's record", f->name);
                if (f->varying || f->dep_name[0] || (f->minlen && f->minlen < f->recsize))
                    die_at(f->line, "file '%s': split keys with variable-length records are not implemented", f->name);
            }
        }
        if (f->nksplit) {
            f->key_sym = split_key_make(f, f->key_name, f->ksplit, f->nksplit, f->ksplit_mf, &tail, sw, &nsw); nspl++;
        } else
        if (f->key_name[0]) {
            /* the RECORD KEY must be an item inside this file's record */
            Sym *k = NULL; int nk = 0;
            if (f->key_qual[0]) { char *q[1] = { f->key_qual }; k = sym_lookup(f->key_name, q, 1, f->line); nk = 1; }
            else for (int j = 0; j < g_nsym; j++)
                if (!g_sym[j].is_cond && !g_sym[j].is_filler && !strcmp(g_sym[j].name, f->key_name) &&
                    f->rec >= 0 && g_sym[j].record == g_sym[f->rec].record) { k = &g_sym[j]; nk++; }
            if (!k || f->rec < 0 || k->record != g_sym[f->rec].record) die_at(f->line, "RECORD KEY '%s' is not an item of file '%s'", f->key_name, f->name);
            if (nk > 1) die_at(f->line, "RECORD KEY '%s' is ambiguous in file '%s'", f->key_name, f->name);
            if (k->ndims) die_at(f->line, "RECORD KEY '%s' cannot be a table item", f->key_name);
            if (k->size < 1 || k->size > 255) die_at(f->line, "RECORD KEY '%s' must be 1 to 255 bytes", f->key_name);
            if (f->org != COB_ORG_INDEXED) die_at(f->line, "file '%s': RECORD KEY is for an INDEXED file (%s)", f->name,
                                                  g_std < 2002 ? "X3.23-1985 indexed file control entry format" : "2023 12.4.5.2 rule 8");
            if (!k->is_group && k->pi.category != PIC_ALPHANUMERIC && k->pi.category != PIC_NATIONAL)
                bp(BP_E16_NUMERIC_KEY, f->line);
            f->key_sym = k;
        }
        if (f->linage) {
            if (f->org != COB_ORG_LINESEQ && f->org != COB_ORG_SEQ) die_at(f->line, "FD %s: LINAGE needs a sequential file", f->name);
            if (!f->lin_name[0][0] && !f->lin_name[1][0] && f->lin_lit[1] > f->lin_lit[0])
                die_at(f->fd_line, "FD %s: LINAGE %ld WITH FOOTING AT %ld: the footing must begin within the page body (%s)", f->name, f->lin_lit[0], f->lin_lit[1],
                       g_std < 2002 ? "X3.23-1985 LINAGE syntax rule 3" : "2023 13.18.34.3 rule 3");
            f->org = COB_ORG_LINESEQ;               /* a LINAGE file is a print file: its records are lines */
            for (int w = 0; w < 4; w++)
                if (f->lin_name[w][0]) {
                    f->lin_sym[w] = sym_lookup(f->lin_name[w], NULL, 0, f->line);
                    if (!is_int_item(f->lin_sym[w])) die_at(f->line, "LINAGE: '%s' must be an integer item", f->lin_name[w]);
                    if (f->lin_sym[w]->pi.is_signed || f->lin_sym[w]->ndims)
                        die_at(f->line, "LINAGE: '%s' is %s; the LINAGE data-names are elementary unsigned integers, not in a table (%s)", f->lin_name[w],
                               f->lin_sym[w]->ndims ? "in a table" : "signed", g_std < 2002 ? "X3.23-1985 LINAGE syntax rule 1" : "2023 13.18.34.3 rules 1-2");
                    if (rec_indirect(&g_sym[f->lin_sym[w]->record]))
                        die_at(f->line, "LINAGE: '%s' cannot be a %s item", f->lin_name[w], indirect_kind(&g_sym[f->lin_sym[w]->record]));
                }
        }
        for (int a = 0; a < f->nalt; a++) {
            if (f->alt[a].nsplit) {
                f->alt[a].sym = split_key_make(f, f->alt[a].name, f->alt[a].split, f->alt[a].nsplit, f->alt[a].split_mf, &tail, sw, &nsw); nspl++;
                continue;
            }
            Sym *k = NULL; int nk = 0;
            if (f->alt[a].qual[0]) { char *q[1] = { f->alt[a].qual }; k = sym_lookup(f->alt[a].name, q, 1, f->line); nk = 1; }
            else for (int j = 0; j < g_nsym; j++)
                if (!g_sym[j].is_cond && !g_sym[j].is_filler && !strcmp(g_sym[j].name, f->alt[a].name) &&
                    f->rec >= 0 && g_sym[j].record == g_sym[f->rec].record) { k = &g_sym[j]; nk++; }
            if (!k || f->rec < 0 || k->record != g_sym[f->rec].record) die_at(f->line, "ALTERNATE RECORD KEY '%s' is not an item of file '%s'", f->alt[a].name, f->name);
            if (nk > 1) die_at(f->line, "ALTERNATE RECORD KEY '%s' is ambiguous in file '%s'", f->alt[a].name, f->name);
            if (k->ndims) die_at(f->line, "ALTERNATE RECORD KEY '%s' cannot be a table item", f->alt[a].name);
            if (k->size < 1 || k->size > 255) die_at(f->line, "ALTERNATE RECORD KEY '%s' must be 1 to 255 bytes", f->alt[a].name);
            if (f->org != COB_ORG_INDEXED) die_at(f->line, "ALTERNATE RECORD KEY needs ORGANIZATION INDEXED");
            if (!k->is_group && k->pi.category != PIC_ALPHANUMERIC && k->pi.category != PIC_NATIONAL)
                bp(BP_E16_NUMERIC_KEY, f->line);
            /* no two keys start at the same byte (2023 12.4.5.6.3 rule 4) */
            if (f->key_sym && k->offset == f->key_sym->offset)
                die_at(f->line, "ALTERNATE RECORD KEY '%s' begins where the RECORD KEY '%s' does (%s)", k->name, f->key_sym->name,
                       g_std < 2002 ? "X3.23-1985 ALTERNATE RECORD KEY syntax rule 4" : "2023 12.4.5.6.3 rule 4");
            for (int b = 0; b < a; b++)
                if (f->alt[b].sym && f->alt[b].sym->offset == k->offset)
                    die_at(f->line, "ALTERNATE RECORD KEY '%s' begins where '%s' does (%s)", k->name, f->alt[b].sym->name,
                           g_std < 2002 ? "X3.23-1985 ALTERNATE RECORD KEY syntax rule 4" : "2023 12.4.5.6.3 rule 4");
            f->alt[a].sym = k;
        }
        if (f->org == COB_ORG_RELATIVE) {
            if (f->relkey_name[0]) {
                Sym *k = sym_lookup(f->relkey_name, NULL, 0, f->line);
                if (!is_int_item(k)) die_at(f->line, "RELATIVE KEY '%s' must be an unsigned integer item", f->relkey_name);
                if (f->rec >= 0 && k->record == g_sym[f->rec].record)
                    die_at(f->line, "RELATIVE KEY '%s' must not be an item of file '%s' (the record number lives outside the record)", f->relkey_name, f->name);
                if (rec_indirect(&g_sym[k->record]))
                    die_at(f->line, "RELATIVE KEY '%s' cannot be a %s item", f->relkey_name, indirect_kind(&g_sym[k->record]));
                f->relkey_sym = k;
            } else if (f->access != 0)
                die_at(f->line, "file '%s': ACCESS RANDOM or DYNAMIC on a RELATIVE file needs a RELATIVE KEY", f->name);
            if (f->key_name[0]) die_at(f->line, "file '%s': RECORD KEY is for INDEXED files; a RELATIVE file has a RELATIVE KEY", f->name);
        } else if (f->relkey_name[0]) die_at(f->line, "file '%s': RELATIVE KEY needs ORGANIZATION RELATIVE", f->name);
        if (nspl) {
            sw[0] = nspl;
            f->splitw = xmalloc((size_t)nsw * sizeof *f->splitw); memcpy(f->splitw, sw, (size_t)nsw * sizeof *sw); f->nsplitw = nsw;
            f->recsize = tail;                      /* the record area and the stored record carry the slots */
        }
        if (f->rec >= 0 && g_sym[f->rec].image_size < f->recsize) g_sym[f->rec].image_size = f->recsize;
    }
    /* images */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0) continue;
        if (s->image_size < s->size) s->image_size = s->size;
        s->image = xmalloc(s->image_size);
        if (!s->is_linkage && !s->is_external) init_record(s, i, 1);
    }
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines < 0) continue;
        init_record(&g_sym[s->record], i, 0);
    }
}
