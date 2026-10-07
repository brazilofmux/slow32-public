/* s32-cobc: Data Division.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* Data Division                                                           */
/* ====================================================================== */

static int binary_bytes(int digits, int usage)
{
    /* The standard leaves COMP size to the implementor.  For COMP, two,
     * four and eight bytes by digit count is the IBM convention and what
     * the SLOW-32 C ABI's own types make natural.  COMP-5 is GnuCOBOL's
     * usage (via Micro Focus), so it takes GnuCOBOL's default 1-2-4-8 and
     * its rule that the item holds the binary field's full capacity, not
     * just the picture's digits.  docs/dialect.md. */
    if (usage == U_COMP5 && digits <= 2) return 1;
    if (digits <= 4) return 2;
    if (digits <= 9) return 4;
    if (digits <= 18) return 8;
    return 16;                          /* 19-31 digits (COBOL 2002; docs/wide.md) */
}

static int capacity_digits(int bytes)
{
    /* the digits of the largest value the bytes hold, 256^n - 1 (COMP-X
     * sizes by MF's rule run 1 to 8); eight bytes, 19 */
    static const int d[9] = { 0, 3, 5, 8, 10, 13, 15, 17, 19 };
    return bytes >= 1 && bytes <= 8 ? d[bytes] : 19;
}

static int is_int_item(Sym *s);

/* elementary size and numeric attributes */
static void bwz_check(const char *name, const PicInfo *pi, int bad_usage, int line);
static int bool_picture(const char *pic, PicInfo *pi, int line);
static int nat_picture(const char *pic, PicInfo *pi, int line);
static void sym_finish(Sym *s)
{
    int u = s->usage;
    if (s->is_group) { s->pi.category = PIC_ALPHANUMERIC; return; }
    if (s->uvar == UV_COMP1) {                  /* RM's binary with a PICTURE, MF's float without */
        int flt = g_comp1 >= 0 ? !g_comp1 : !s->has_pic;
        if (flt) { u = s->usage = U_FLOAT; s->uvar = UV_FSHORT; }
        else s->uvar = UV_NONE;
    }
    if (u == U_FLOAT) {
        /* IEEE single or double; no PICTURE (MF), no editing clauses */
        if (s->has_pic) die_at(s->line, "'%s': a floating-point item takes no PICTURE%s", s->name,
                               s->uvar == UV_FLONG ? " (ACUCOBOL's decimal COMP-2 is not implemented)" : "");
        if (s->just || s->blank_zero || s->sign_lead || s->sign_sep)
            die_at(s->line, "'%s': a floating-point item takes no JUSTIFIED, BLANK WHEN ZERO or SIGN clause", s->name);
        s->size = s->uvar == UV_FSHORT ? 4 : 8;
        memset(&s->pi, 0, sizeof s->pi);
        s->pi.category = PIC_NUMERIC; s->pi.is_signed = 1; s->pi.digits = 18;   /* 18: the narrow paths' reading, an integer part */
        return;
    }
    int native = usage_is_native(u);

    if (!s->has_pic && !native && g_std >= 2002 && s->value_tok && s->value_tok->kind == T_STR && !s->value_all && s->value_tok->len > 0) {
        /* no PICTURE, but an alphanumeric, boolean or national literal in
         * the VALUE clause: PICTURE X(length), 1(length) or N(length) is
         * implied (2023 13.16.3 rule 9) */
        Tok *v = s->value_tok;
        int n = v->nat ? v->len / 2 : v->len;
        s->has_pic = 1;
        snprintf(s->pic, sizeof s->pic, "%c(%d)", v->boolv ? '1' : v->nat ? 'n' : 'x', n);
        if (v->boolv) { if (!bool_picture(s->pic, &s->pi, s->line)) die_at(s->line, "internal: the implied boolean picture"); }
        else if (v->nat) { if (!nat_picture(s->pic, &s->pi, s->line)) die_at(s->line, "internal: the implied national picture"); }
        else if (pic_analyse(s->pic, &s->pi) < 0) die_at(s->line, "internal: the implied picture '%s': %s", s->pic, s->pi.err);
    }
    if (!s->has_pic && !native)
        die_at(s->line, "'%s' has no PICTURE clause%s (%s)", s->name,
               s->level == 1 || s->level == 77 ? " (and no subordinate items: an empty group is RM/COBOL's, not taken -- docs/dialect.md)" : "",
               g_std < 2002 ? "X3.23-1985 VI-21, data description entry syntax rule 3" : "2023 13.16.3 rule 8");
    if (s->has_pic && native)
        die_at(s->line, "'%s': USAGE %s takes no PICTURE", s->name, usage_name(u));

    if (native && (u == U_INDEX || u == U_POINTER) && !s->is_index && !s->is_ftemp) {
        /* no VALUE, JUSTIFIED or BLANK WHEN ZERO on an index or pointer item,
         * nor (1985) SYNCHRONIZED: X3.23-1985 USAGE syntax rule 6; 2023
         * 13.16.3 rule 10, 13.18.32.3 rule 3, 13.18.8.3 rule 1 */
        const char *what = s->value_tok ? "VALUE" : s->just ? "JUSTIFIED" : s->blank_zero ? "BLANK WHEN ZERO" :
                           (s->sync && g_std < 2002 && u == U_INDEX) ? "SYNCHRONIZED" : NULL;
        if (what)
            die_at(s->line, "'%s': a USAGE %s item takes no %s clause (%s)", s->name, u == U_INDEX ? "INDEX" : "POINTER", what,
                   g_std < 2002 && u == U_INDEX ? "X3.23-1985 USAGE syntax rule 6" :
                   s->value_tok ? "2023 13.16.3 rule 10" : s->just ? "2023 13.18.32.3 rule 3" : "2023 13.18.8.3 rule 1");
    }
    if (native) {
        switch (u) {
        case U_SINT:   s->size = 4; s->pi.digits = 10; s->pi.is_signed = 1; break;
        case U_UINT:   s->size = 4; s->pi.digits = 10; break;
        case U_SDBL:   s->size = 8; s->pi.digits = 20; s->pi.is_signed = 1; break;   /* 20 shown, as GnuCOBOL; the capacity limits it */
        case U_UDBL:   s->size = 8; s->pi.digits = 20; break;
        case U_SSHORT: s->size = 2; s->pi.digits = 5;  s->pi.is_signed = 1; break;
        case U_USHORT: s->size = 2; s->pi.digits = 5;  break;
        case U_BCHAR:  s->size = 1; s->pi.digits = 3;  s->pi.is_signed = 1; break;
        case U_UBCHAR: s->size = 1; s->pi.digits = 3;  break;
        case U_POINTER: case U_INDEX: s->size = 4; s->pi.digits = 10; break;
        }
        s->pi.category = PIC_NUMERIC;
        return;
    }

    const PicInfo *pi = &s->pi;
    /* a PICTURE with N takes no USAGE but NATIONAL, its own or its
     * group's (2023 13.18.60.3 rule 20; a compiler-made copy carries the
     * original's usage) */
    if (pi->category == PIC_NATIONAL && s->has_usage && !s->is_ftemp)
        die_at(s->line, "'%s': a PICTURE with N takes only USAGE NATIONAL, not %s (2023 13.18.60.3 rule 20)", s->name, usage_name(u));
    if (s->nat_usage && pi->category != PIC_NATIONAL) {
        /* numeric and numeric-edited USAGE NATIONAL: the DISPLAY form, each
         * character two bytes (2023 13.18.60.3 rule 12) */
        if (pi->category != PIC_NUMERIC && pi->category != PIC_NUMERIC_EDITED && pi->category != PIC_BOOLEAN)
            die_at(s->line, "'%s': USAGE NATIONAL takes a PICTURE of N, or a numeric, numeric-edited or boolean one (2023 13.18.60.3 rule 12)", s->name);
        u = s->usage = U_NATIONAL;
    }
    if (u == U_BIT) {
        /* boolean positions as bits (2023 13.18.60); the bit offset and the
         * bytes spanned come with the layout (8.5.1.6.3) */
        if (pi->category != PIC_BOOLEAN) die_at(s->line, "'%s': USAGE BIT needs a boolean PICTURE (1) (2023 13.18.60.3 rule 5)", s->name);
        s->bits = pi->bytes; s->size = (s->bits + 7) / 8;
        return;
    }
    switch (u) {
    case U_DISPLAY: case U_NATIONAL:
        s->size = pi->bytes;
        if (g_currency_len > 1 && pi->category == PIC_NUMERIC_EDITED && strchr(pi->pat, '$'))
            s->size += g_currency_len - 1;          /* the first currency symbol is the string's length (13.18.40.4, cs) */
        if (!s->sign_lead && !s->sign_sep && pi->category == PIC_NUMERIC && pi->is_signed)
            for (int a = s->parent; a >= 0; a = g_sym[a].parent)          /* a group's SIGN clause reaches down */
                if (g_sym[a].sign_lead || g_sym[a].sign_sep) { s->sign_lead = g_sym[a].sign_lead; s->sign_sep = g_sym[a].sign_sep; break; }
        if (s->sign_sep) s->size++;                 /* SIGN SEPARATE: its own character */
        if (u == U_NATIONAL) {
            if (s->in_natgroup && pi->is_signed && !s->sign_sep)
                die_at(s->line, "'%s': a signed numeric item in a national group needs SIGN SEPARATE (2023 13.18.29.3 rule 3)", s->name);
            s->size *= 2;
        }
        break;
    case U_BINARY: case U_COMP5: {
        int allx = u == U_COMP5 && pi->category == PIC_ALPHANUMERIC;   /* X's only (pi->pat is empty unless edited) */
        for (const char *c = s->pic; allx && *c; c++) {
            if (*c == '(') { while (c[1] && c[1] != ')') c++; if (c[1]) c++; continue; }
            if (*c != 'x' && *c != 'X') allx = 0;
        }
        if (s->uvar == UV_COMPX || allx) {
            /* MF's COMP-X, and COMP-5 with X's: PIC X(n) is n bytes,
             * unsigned, holding what n bytes hold (as the digits of
             * 256^n - 1) -- COMP-X big-endian, COMP-5 in the machine's
             * order; COMP-X's PIC 9(n) is the fewest bytes that hold n
             * nines, never signed */
            const char *un = s->uvar == UV_COMPX ? "COMP-X" : "COMP-5";
            if (allx) {
                int n = pi->bytes;
                if (n == 8 && s->uvar != UV_COMPX) {
                    /* eight bytes in the machine's order hold 2^64 - 1, twenty
                     * digits: BINARY-DOUBLE UNSIGNED is the same item, on the
                     * wide path (docs/wide.md) */
                    if (g_std < 2002)
                        die_at(s->line, "'%s': PIC X(8) COMP-5 holds twenty digits, which need the wide arithmetic of -std=2002 (docs/wide.md); compile with -std=2002", s->name);
                    s->usage = U_UDBL; s->has_pic = 0; s->pic[0] = 0;
                    memset(&s->pi, 0, sizeof s->pi);
                    s->size = 8; s->pi.digits = 20; s->pi.category = PIC_NUMERIC;
                    return;
                }
                if (n == 8 && s->uvar == UV_COMPX) {
                    /* eight bytes big-endian: 2^64 - 1, twenty digits, the
                     * same wide item as COMP-5's above in COMP-X's byte order
                     * (sym_be: COMP-X always).  ACAS's CBL-FILE-SIZE, which
                     * CBL_CHECK_FILE_EXIST fills (cobol ISSUES-124) */
                    if (g_std < 2002)
                        die_at(s->line, "'%s': PIC X(8) COMP-X holds twenty digits, which need the wide arithmetic of -std=2002 (docs/wide.md); compile with -std=2002", s->name);
                    s->usage = U_UDBL; s->has_pic = 0; s->pic[0] = 0;
                    memset(&s->pi, 0, sizeof s->pi);
                    s->size = 8; s->pi.digits = 20; s->pi.category = PIC_NUMERIC;
                    return;
                }
                if (n > 8) die_at(s->line, "'%s': PIC X(%d) %s is not implemented (up to eight bytes)", s->name, n, un);
                static const int capd[8] = { 0, 3, 5, 8, 10, 13, 15, 17 };
                snprintf(s->pic, sizeof s->pic, "9(%d)", capd[n]);
                if (pic_analyse(s->pic, &s->pi) < 0) die_at(s->line, "internal: %s picture", un);
                s->size = n; s->compx_x = 1;
                break;
            }
            if (pi->category != PIC_NUMERIC) die_at(s->line, "'%s': USAGE COMP-X needs a PICTURE of 9s or of Xs", s->name);
            if (pi->is_signed) die_at(s->line, "'%s': a COMP-X item is unsigned (Micro Focus)", s->name);
            if (pi->digits > 18) die_at(s->line, "'%s': COMP-X of more than 18 digits is not implemented", s->name);
            unsigned long long top = 1; int b = 0;
            for (int i = 0; i < pi->digits; i++) top *= 10;              /* 10^digits, the first value too big */
            while (b < 8 && (b == 0 || (1ULL << (8 * b)) < top)) b++;
            s->size = b;
            break;
        }
        if (pi->category != PIC_NUMERIC) {
            if (u == U_COMP5) die_at(s->line, "'%s': USAGE COMP-5 needs a PICTURE of 9s or of Xs (Micro Focus)", s->name);
            die_at(s->line, "'%s': USAGE %s needs a numeric PICTURE (2023 13.18.60.3 rule 3)", s->name, usage_name(u));
        }
        s->size = binary_bytes(pi->digits, u);
        break;
    }
    case U_PACKED:
        if (pi->category != PIC_NUMERIC)
            die_at(s->line, "'%s': USAGE PACKED-DECIMAL (COMP-3) needs a numeric PICTURE (2023 13.18.60.3 rule 3)", s->name);
        if (s->uvar == UV_NOSIGN && pi->is_signed) s->uvar = UV_NONE;    /* signed COMP-6 is COMP-3 (MF COMP-6"2") */
        s->size = s->uvar == UV_NOSIGN ? (pi->digits + 1) / 2 : pi->digits / 2 + 1;
        break;
    }
    if (s->just && pi->category == PIC_NUMERIC)
        die_at(s->line, "'%s': JUSTIFIED is only for alphanumeric items", s->name);
    if (s->blank_zero && !s->is_ftemp) bwz_check(s->name, pi, u != U_DISPLAY && u != U_NATIONAL, s->line);
}

static int is_numeric_sym(Sym *s) { return !s->is_group && s->pi.category == PIC_NUMERIC; }

/* Encode a numeric literal into storage described by s, at p. */
static void store_numeric(Sym *s, const NumLit *n, unsigned char *p, int line)
{
    if (s->usage == U_FLOAT) {
        /* the literal to the nearest float or double, laid down in the
         * machine's order (the host that compiles is little-endian, as
         * SLOW-32 is) */
        char t[128]; snprintf(t, sizeof t, "%s%.*se%d", n->neg ? "-" : "", n->ndigits, n->digits, -n->scale);
        double x = strtod(t, NULL);
        if (s->size == 4) { float f = (float)x; memcpy(p, &f, 4); } else memcpy(p, &x, 8);
        return;
    }
    const PicInfo *pi = &s->pi;
    int digits = pi->digits, scale = pi->scale;
    char d[40];
    /* trailing P (scale < 0): the picture's digits are the integer's own,
     * the P positions being its low zeros; align as an integer */
    if (!numlit_align(n, digits, scale < 0 ? 0 : scale, d))
        die_at(line, "VALUE %s%.*s does not fit PICTURE of '%s'", n->neg ? "-" : "",
               n->ndigits, n->digits, s->name);
    int neg = n->neg && pi->is_signed;

    switch (s->usage) {
    case U_DISPLAY: {
        /* the stored digits: all of them, or -- with P in the picture --
         * the last `bytes` (leading P) or the first `bytes` (trailing P);
         * then the sign where the SIGN clause put it */
        int stored = pi->bytes;
        const char *src = d;
        if (stored < digits) src = scale < 0 ? d : d + (digits - stored);
        unsigned char *q = p;
        if (s->sign_sep && s->sign_lead) { *q++ = neg ? '-' : '+'; }
        memcpy(q, src, stored);
        if (s->sign_sep && !s->sign_lead) q[stored] = neg ? '-' : '+';
        else if (neg && !s->sign_sep) {
            int k = s->sign_lead ? 0 : stored - 1;
            q[k] = (unsigned char)(q[k] - '0' + 'p');
        }
        break;
    }
    case U_PACKED: {
        int bytes = s->size;
        memset(p, 0, bytes);
        if (s->uvar == UV_NOSIGN) {                 /* COMP-6: the digits right-aligned, no sign */
            for (int i = digits - 1, nib = bytes * 2 - 1; i >= 0; i--, nib--)
                p[nib / 2] |= (unsigned char)(nib & 1 ? d[i] - '0' : (d[i] - '0') << 4);
            break;
        }
        int nib = bytes * 2 - 2;
        for (int i = digits - 1; i >= 0; i--, nib--) {
            int v = d[i] - '0';
            if (nib & 1) p[nib / 2] |= (unsigned char)v; else p[nib / 2] |= (unsigned char)(v << 4);
        }
        p[bytes - 1] |= pi->is_signed ? (neg ? 0xD : 0xC) : 0xF;
        break;
    }
    default: {
        if (s->size > 8) {                          /* 19-31 digits: sixteen bytes (docs/wide.md) */
            wl_t m[WL];
            w_from_digits(m, d, digits);
            if (neg) { for (int i = 0; i < WL; i++) m[i] = ~m[i]; wl_t one[WL] = { 1, 0, 0, 0 }; mp_add(m, one, WL); }
            for (int i = 0; i < s->size; i++) p[i] = i < 16 ? (unsigned char)(m[i / 4] >> (8 * (i % 4))) : (neg ? 0xFF : 0);
            break;
        }
        unsigned long long mag = 0;
        for (int i = 0; i < digits; i++) mag = mag * 10 + (d[i] - '0');
        if (s->size < 8 && (mag >> (s->size * 8 - (pi->is_signed ? 1 : 0))))
            die_at(line, "VALUE does not fit the %d-byte binary item '%s'", s->size, s->name);
        long long v = neg ? -(long long)mag : (long long)mag;
        for (int i = 0; i < s->size; i++) p[i] = (unsigned char)(v >> (8 * i));
        break;
    }
    }
    if (sym_be(s))                              /* built little-endian above; COMP is stored big-endian */
        for (int i = 0, j = s->size - 1; i < j; i++, j--) { unsigned char c = p[i]; p[i] = p[j]; p[j] = c; }
}

static int parse_level(void)
{
    Tok *t = cur();
    if (t->kind != T_NUM) return -1;
    for (char *k = t->s; *k; k++) if (!isdigit((unsigned char)*k)) return -1;
    if (strlen(t->s) > 2) return -1;
    return atoi(t->s);
}

static int g_last_item = -1;        /* the previous non-88 item, for 88s */
static int g_no_values;             /* building an INITIALIZE template: VALUE clauses do not apply */
static int g_in_linkage = 0;        /* parsing the LINKAGE SECTION */
static int g_in_local = 0;          /* parsing the LOCAL-STORAGE SECTION */

/* Where a parse resumes after an error in a data entry: the entry's
 * period, unless something that plainly starts the next entry or section
 * comes first (a level number opening a line, as when the period was
 * left off).  An error found after the period -- an entry's own checks
 * -- resumes where it stands. */
static int at_division(void);
static void resync_data(int start)
{
    if (g_tp > start && g_tok[g_tp - 1].kind == T_PERIOD) return;
    if (g_tp == start) advance();
    while (cur()->kind != T_PERIOD && cur()->kind != T_EOF && !at_division()) {
        Tok *t = cur(), *n = peek(1);
        if (t->kind == T_NUM && g_tok[g_tp - 1].line != t->line &&
            (n->kind == T_PERIOD || (n->kind == T_WORD && (!strcmp(n->s, "filler") || !is_reserved85(n->s))))) return;
        if (g_tok[g_tp - 1].line != t->line &&
            (is_word(t, "fd") || is_word(t, "sd") || is_word(t, "rd") || is_word(n, "section"))) return;
        advance();
    }
    if (cur()->kind == T_PERIOD) advance();
}

static void parse_data_item1(void);
static int g_entry_level;             /* the level number of the entry being parsed */
/* The rules of one data description entry that its own clauses decide
 * (X3.23-1985 VI-18 and VI-21, X-21 to X-24; 2023 13.16.3, 13.18.22,
 * 13.18.27): the 77's name, and where EXTERNAL and GLOBAL may stand. */
static void entry_rules(Sym *s, int level, int line)
{
    int e85 = g_std < 2002;
    int ws = !g_in_linkage && !g_in_local && g_cur_fd < 0;
    if (level == 77 && s->is_filler)
        die_at(line, "a level 77 entry needs a data-name (%s)",
               e85 ? "X3.23-1985 VI-18, noncontiguous working storage" : "2023 13.16.3 rule 2");
    if ((s->is_external || s->is_global) && s->is_filler)
        die_at(line, "an entry with %s needs a data-name, not FILLER (%s)", s->is_external ? "EXTERNAL" : "GLOBAL",
               e85 ? "X3.23-1985 X-21, data description entry syntax rule 5" : "2023 13.16.3 rule 7");
    if (s->is_external) {
        if (level != 1 || !ws)
            die_at(line, "'%s': EXTERNAL is for a level 01 entry in the WORKING-STORAGE SECTION (%s)", s->name,
                   e85 ? "X3.23-1985 X-21, data description entry syntax rule 2" : "2023 13.18.22.3 rule 1");
        if (s->redef_clause)
            die_at(line, "'%s': EXTERNAL and REDEFINES cannot be in the same entry (%s)", s->name,
                   e85 ? "X3.23-1985 X-21, data description entry syntax rule 3" : "2023 13.16.3 rule 5");
        if (s->is_based)
            die_at(line, "'%s': EXTERNAL and BASED cannot be in the same entry (2002 13.13.2 rule 5; 2023 13.16.3 rule 5)", s->name);
        for (int i = g_sym_base; i < sym_idx(s); i++)
            if (g_sym[i].is_external && g_sym[i].level == 1 && !strcmp(g_sym[i].name, s->name))
                die_at(line, "'%s' is described EXTERNAL twice in this program (%s)", s->name,
                       e85 ? "X3.23-1985 X-23, EXTERNAL syntax rule 2" : "2023 13.18.22.3 rule 2");
    }
    if (s->is_global) {
        if (level != 1 || (e85 && (g_in_linkage || g_in_local)))
            die_at(line, "'%s': GLOBAL is for a level 01 entry in the %s (%s)", s->name,
                   e85 ? "FILE or WORKING-STORAGE SECTION" : "FILE, WORKING-STORAGE, LOCAL-STORAGE or LINKAGE SECTION",
                   e85 ? "X3.23-1985 X-24, GLOBAL syntax rule 1" : "2023 13.18.27.3 rule 1");
        if (e85)
            for (int i = g_sym_base; i < sym_idx(s); i++)
                if (g_sym[i].is_global && !g_sym[i].is_cond && !strcmp(g_sym[i].name, s->name))
                    die_at(line, "'%s' names two GLOBAL items in this DATA DIVISION (X3.23-1985 X-24, GLOBAL syntax rule 2)", s->name);
    }
}

static void parse_data_item(void)
{
    jmp_buf jb, *outer = g_recover;
    int start = g_tp, nsym = g_nsym, last = g_last_item;
    g_entry_level = -1;
    if (setjmp(jb)) {
        /* the entry is dropped; later references to its name fail quietly */
        g_nsym = nsym; g_last_item = last;
        g_recover = outer;
        int lv = g_entry_level;
        if (lv >= 1 && lv != 66 && lv != 78 && lv != 88) {   /* a data item's level (an expansion's may pass 49) */
            /* a FILLER PIC X stands in its place, so the record keeps its
             * shape: a group whose only item failed is still a group */
            Sym *f = sym_new();
            f->level = lv; f->line = g_tok[start].line; f->usage = U_DISPLAY; f->is_linkage = g_in_linkage; f->is_local = g_in_local;
            f->is_filler = 1; snprintf(f->name, sizeof f->name, "filler");
            f->has_pic = 1; snprintf(f->pic, sizeof f->pic, "x"); pic_analyse(f->pic, &f->pi); f->standin = 1;
            if (g_cur_fd >= 0 && lv == 1) {
                File *fl = &g_files[g_cur_fd];
                f->fd = g_cur_fd;
                if (fl->rec < 0 || fl->rec == sym_idx(f)) fl->rec = sym_idx(f); else f->redefines = fl->rec;
            }
            g_last_item = sym_idx(f);
        }
        /* every name the entry and the resync passed over: the entry's own,
         * and any entry swallowed with it when its period was missing */
        resync_data(start);
        for (int k = start; k < g_tp; k++)
            if (g_tok[k].kind == T_WORD && !is_reserved85(g_tok[k].s) && g_npoison < 64)
                snprintf(g_poison[g_npoison++], sizeof g_poison[0], "%s", g_tok[k].s);
        return;
    }
    g_recover = &jb;
    parse_data_item1();
    g_recover = outer;
}

/* PICTURE N...: a national item (COBOL 2002 13.18.40), N(k) and N
 * repeated, each character two bytes.  Other pictures are pic_analyse's. */
/* PICTURE 1...: a boolean item (2023 13.18.40), 1(k) and 1 repeated,
 * one boolean position each (cobol ISSUES-76) */
static int bool_picture(const char *pic, PicInfo *pi, int line)
{
    int n = 0;
    for (const char *p = pic; *p; ) {
        if (*p != '1') return 0;
        p++;
        if (*p == '(') {
            char *e; long k = strtol(p + 1, &e, 10);
            if (*e != ')' || k < 1) return 0;
            n += (int)k; p = e + 1;
        } else n++;
    }
    if (!n) return 0;
    if (g_std < 2002) die_at(line, "PICTURE 1 (boolean) is COBOL 2002; compile with -std=2002");
    memset(pi, 0, sizeof *pi);
    pi->category = PIC_BOOLEAN; pi->bytes = n;
    pi->patlen = n < PIC_MAXPAT - 1 ? n : PIC_MAXPAT - 1;
    memset(pi->pat, '1', (size_t)pi->patlen);
    return 1;
}

/* a PICTURE character-string of at most 30 characters (X3.23-1985
 * PICTURE syntax rule 4), 50 in 2002 (13.16.38.2 rule 4); 2023 allows 63 */
static void pic_len_check(const char *pic, int line)
{
    int lim = g_std < 2002 ? 30 : 50;
    if ((int)strlen(pic) > lim)
        die_at(line, "the PICTURE '%s' has %d characters, more than %d (%s)", pic, (int)strlen(pic), lim,
               g_std < 2002 ? "X3.23-1985 PICTURE syntax rule 4" : "2002 13.16.38.2 rule 4; 2023 allows 63");
}

/* BLANK WHEN ZERO: a numeric or numeric-edited item of usage display (or
 * national), no S and no * (85 BLANK WHEN ZERO rules 1-2 and PICTURE
 * rule 7; 2023 13.18.8.3 rules 1-2 and 13.18.40.3 rule 22) */
static void bwz_check(const char *name, const PicInfo *pi, int bad_usage, int line)
{
    int e85 = g_std < 2002;
    if (pi->category != PIC_NUMERIC && pi->category != PIC_NUMERIC_EDITED)
        die_at(line, "'%s': BLANK WHEN ZERO is for a numeric or numeric-edited item (%s)", name,
               e85 ? "X3.23-1985 BLANK WHEN ZERO rule 1" : "2023 13.18.8.3 rule 1");
    if (bad_usage)
        die_at(line, "'%s': BLANK WHEN ZERO is for an item of usage display%s (%s)", name, e85 ? "" : " or national",
               e85 ? "X3.23-1985 BLANK WHEN ZERO rule 2" : "2023 13.18.8.3 rule 2");
    if (strchr(pi->pat, 'S'))
        die_at(line, "'%s': BLANK WHEN ZERO is not for a PICTURE with S (%s)", name,
               e85 ? "X3.23-1985: it makes the item numeric-edited, which has no S" : "2023 13.18.8.3 rule 1");
    if (strchr(pi->pat, '*'))
        die_at(line, "'%s': BLANK WHEN ZERO and the zero-suppression symbol * exclude each other (%s)", name,
               e85 ? "X3.23-1985 PICTURE rule 7" : "2023 13.18.40.3 rule 22");
}

static int nat_picture(const char *pic, PicInfo *pi, int line)
{
    /* with B, 0 or / as well, national-edited (cobol ISSUES-73); the
     * flattened pattern holds one symbol per character position */
    char flat[PIC_MAXPAT]; int n = 0, nn = 0, edit = 0;
    for (const char *p = pic; *p; ) {
        char c = (char)toupper((unsigned char)*p);
        if (c != 'N' && c != 'B' && c != '0' && c != '/') return 0;
        p++;
        long k = 1;
        if (*p == '(') {
            char *e; k = strtol(p + 1, &e, 10);
            if (*e != ')' || k < 1) return 0;
            p = e + 1;
        }
        if (c == 'N') nn += (int)k; else edit = 1;
        for (long q = 0; q < k; q++) { if (n < PIC_MAXPAT - 1) flat[n] = c; n++; }
    }
    if (!nn) return 0;                               /* B, 0 and / alone are no national picture */
    if (g_std < 2002) die_at(line, "PICTURE N (national) is COBOL 2002; compile with -std=2002");
    if (edit && n >= PIC_MAXPAT) die_at(line, "a national-edited PICTURE longer than %d characters is not implemented", PIC_MAXPAT - 1);
    memset(pi, 0, sizeof *pi);
    pi->category = PIC_NATIONAL; pi->bytes = 2 * n; pi->edited = edit;
    pi->patlen = n < PIC_MAXPAT - 1 ? n : PIC_MAXPAT - 1;
    memcpy(pi->pat, flat, (size_t)pi->patlen);
    return 1;
}

static int sym_is_boolean(const Sym *s);
static int g_is_function;           /* (defined with the units, below) */
static void parse_constant_entry(int line);
static void const_pic_check(void);
static int g_cpicbad[64], g_cpicbad_ci[64], g_ncpicbad;  /* PICTURE repetitions no constant could fill */
static void parse_data_item1(void)
{
    int line = cur()->line;
    int level = parse_level();
    if (level < 0) die_at(line, "expected a level number, found %s", tok_desc(cur()));
    advance();
    g_entry_level = level;

    if (level == 78) {                      /* Micro Focus's constant-name (BP-E26) */
        bp(BP_E26_LEVEL_78, line);
        parse_constant_entry(line);
        return;
    }
    if (!((level >= 1 && level <= 49) || level == 66 || level == 77 || level == 88) &&
        !(level <= 99 && g_tok[g_tp - 1].orig && !strcmp(g_tok[g_tp - 1].orig, "\001xlevel")))   /* a TYPE's or SAME AS's expansion past 49 */
        die_at(line, "level number %d is not valid", level);
    if (level == 1 && g_std >= 2002 && is_word(peek(1), "constant") && !is_word(peek(2), "record")) {
        parse_constant_entry(line);
        return;
    }

    Sym *s = sym_new();
    s->level = level; s->line = line; s->usage = U_DISPLAY;
    s->is_linkage = g_in_linkage;
    s->is_local = g_in_local;
    if (accept_word("filler")) {
        s->is_filler = 1;
        snprintf(s->name, sizeof s->name, "filler");
    } else if (cur()->kind == T_WORD && !at_word("redefines") && !at_word("pic") &&
               !at_word("picture") && !at_word("value") && !at_word("occurs") && !at_word("usage")) {
        user_word(cur()->s, line, "a data item");
        snprintf(s->name, sizeof s->name, "%s", cur()->s);
        advance();
    } else {
        s->is_filler = 1;                       /* 85 lets the name be omitted */
        snprintf(s->name, sizeof s->name, "filler");
    }

    if (level == 88) {
        if (g_last_item < 0) die_at(line, "level 88 '%s' has no conditional variable", s->name);
        if (g_sym[g_last_item].any_len)
            die_at(line, "level 88 '%s': an ANY LENGTH item takes no condition-name (2023 13.16.3 rule 24f)", s->name);
        s->is_cond = 1;
        s->parent = g_last_item;
        if (!(accept_word("value") || accept_word("values")))
            die_at(line, "level 88 '%s' needs a VALUE clause", s->name);
        accept_word("is"); accept_word("are");
        for (;;) {
            if (at_word("when") || at_word("false")) {
                /* [WHEN SET TO] FALSE IS literal-4: the value SET ... TO
                 * FALSE gives (2002; 2023 13.18.63 format 3) */
                if (g_std < 2002) die_at(cur()->line, "the FALSE phrase of a level 88 VALUE is COBOL 2002; compile with -std=2002");
                if (accept_word("when")) { expect_word("set"); expect_word("to"); }
                expect_word("false"); accept_word("is");
                Tok *f = cur();
                if (!(f->kind == T_STR || f->kind == T_NUM || (f->kind == T_WORD && is_figurative(f->s))))
                    die_at(f->line, "expected a literal after FALSE in the VALUE of '%s'", s->name);
                s->cv_false = f; advance();
                break;
            }
            int is_all = accept_word("all");
            Tok *v = cur();
            if (v->kind == T_WORD && (!strcmp(v->s, "usage") || !strcmp(v->s, "comp") || !strcmp(v->s, "display") || !strcmp(v->s, "binary")))
                die_at(v->line, "a level 88 entry takes no USAGE clause (%s)", g_std < 2002 ? "X3.23-1985 level 88 format" : "2023 13.18.60.3 rule 1");
            if (!(v->kind == T_STR || v->kind == T_NUM || (v->kind == T_WORD && is_figurative(v->s))))
                die_at(v->line, "expected a literal in the VALUE of '%s'", s->name);
            if (s->ncv >= MAXCV) die_at(v->line, "too many values for '%s'", s->name);
            if (is_all && v->kind != T_STR) die_at(v->line, "ALL needs a non-numeric literal");
            if (is_all) s->cv_all |= 1u << s->ncv;
            s->cv_lo[s->ncv] = v; s->cv_hi[s->ncv] = NULL;
            advance();
            if (accept_word("thru") || accept_word("through")) {
                Tok *h = cur();
                if (sym_is_boolean(&g_sym[g_last_item]) || v->boolv)
                    die_at(h->line, "THROUGH is not specified for the boolean item '%s' (2023 13.18.63.3 rule 29)", g_sym[g_last_item].name);
                if (!(h->kind == T_STR || h->kind == T_NUM)) die_at(h->line, "expected a literal after THRU");
                s->cv_hi[s->ncv] = h;
                advance();
            }
            s->ncv++;
            if (cur()->kind == T_PERIOD) break;
        }
        if (!s->ncv) die_at(line, "level 88 '%s' needs a value before FALSE", s->name);
        expect_period();
        return;
    }

    g_last_item = sym_idx(s);

    if (level == 66) {
        /* 66 name RENAMES a [THRU b]: another name for the storage from a to
         * the end of b, in the record it follows; resolved after layout */
        if (s->is_filler) die_at(line, "a level 66 entry needs a name");
        expect_word("renames");
        s->is_rename = 1;
        for (int which = 0; which < 2; which++) {
            if (which && !(accept_word("thru") || accept_word("through"))) break;
            if (cur()->kind != T_WORD) die_at(line, "RENAMES needs a data-name");
            snprintf(which ? s->rn_b : s->rn_a, 64, "%s", cur()->s); advance();
            int *nq = which ? &s->rn_nbq : &s->rn_naq;
            while (accept_word("of") || accept_word("in")) {
                if (cur()->kind != T_WORD) die_at(line, "RENAMES: expected a qualifier after OF/IN");
                if (*nq == 8) die_at(line, "RENAMES: too many qualifiers");
                snprintf(which ? s->rn_bq[*nq] : s->rn_aq[*nq], 64, "%s", cur()->s); (*nq)++; advance();
            }
        }
        expect_period();
        return;
    }

    Tok *first_clause = cur();
    while (cur()->kind != T_PERIOD) {
        Tok *t = cur();
        if (t->kind != T_WORD) die_at(t->line, "unexpected %s in the description of '%s'", tok_desc(t), s->name);
        if (!strcmp(t->s, "is")) { advance(); continue; }        /* 01 X IS GLOBAL: a noise word */
        if (g_sql_declare && !strcmp(t->s, "character") && is_word(peek(1), "set")) {
            /* ISO 9075's embedded COBOL: a host variable's CHARACTER SET
             * [IS] name, in a DECLARE SECTION; SQLite has none (docs/esql.md) */
            advance(); advance(); accept_word("is");
            if (cur()->kind != T_WORD) die_at(t->line, "CHARACTER SET needs a character set name");
            advance(); continue;
        }
        if (t->strong) { s->strong = t->strong; advance(); continue; }   /* expand_types()'s strong-type marker */
        if (!strcmp(t->s, "\001lvl1")) { s->type_lvl1 = 1; advance(); continue; }   /* ... and its group-type marker */

        if (!strcmp(t->s, "pic") || !strcmp(t->s, "picture")) {
            advance();
            if (cur()->kind != T_PIC) die_at(t->line, "expected a PICTURE character-string");
            if (g_ncpicbad) const_pic_check();
            if (s->has_pic) die_at(t->line, "'%s' has two PICTURE clauses", s->name);
            s->has_pic = 1;
            snprintf(s->pic, sizeof s->pic, "%s", cur()->s);
            pic_len_check(s->pic, t->line);
            if (nat_picture(s->pic, &s->pi, t->line)) { advance(); continue; }
            if (bool_picture(s->pic, &s->pi, t->line)) { advance(); continue; }
            if (pic_analyse(s->pic, &s->pi) < 0) {
                /* a symbol 1 or N (not a repeat count) says which category
                 * was meant: name its rule (2023 13.18.40.4 rules 8-10) */
                int has1 = 0, hasn = 0, paren = 0;
                for (const char *c = s->pic; *c; c++) {
                    if (*c == '(') paren = 1; else if (*c == ')') paren = 0;
                    else if (!paren && *c == '1') has1 = 1;
                    else if (!paren && (*c == 'n' || *c == 'N')) hasn = 1;
                }
                for (const char *c = s->pic; *c; c++)
                    if (*c == '(') {
                        const char *d = c + 1; while (*d == '0') d++;
                        if (d > c + 1 && *d == ')')
                            die_at(t->line, "'%s': PICTURE '%s': a repeat count is a nonzero integer (%s)", s->name, s->pic,
                                   g_std < 2002 ? "X3.23-1985 VI-30 PICTURE general rule 7" : "2023 13.18.40.3 rule 6");
                    }
                if (g_std < 2002 && (hasn || has1))
                    die_at(t->line, "'%s': PICTURE '%s': the symbol %s is COBOL 2002's; compile with -std=2002", s->name, s->pic, hasn ? "N" : "1");
                if (hasn)
                    die_at(t->line, "'%s': PICTURE '%s': a national PICTURE holds only N, and B, 0 or / for a national-edited one (2023 13.18.40.4 rules 9-10)",
                           s->name, s->pic);
                if (has1)
                    die_at(t->line, "'%s': PICTURE '%s': a boolean PICTURE holds only the symbol 1 (2023 13.18.40.4 rule 8)", s->name, s->pic);
                {
                    /* a floating-point numeric-edited PICTURE: mantissa, E, a sign, exponent (2023 13.18.40.2.2) */
                    const char *pe = s->pic; int fl = 0;
                    for (; *pe; pe++) if ((*pe == 'E' || *pe == 'e') && (pe[1] == '+' || pe[1] == '-') && pe > s->pic && strchr("9.V()+-", pe[-1])) fl = 1;
                    if (fl) die_at(t->line, "'%s': a floating-point numeric-edited PICTURE is COBOL 2002 (2023 13.18.40); not implemented", s->name);
                }
                die_at(t->line, "'%s': %s", s->name, s->pi.err);
            }
            advance();
            continue;
        }
        if (!strcmp(t->s, "usage")) { advance(); accept_word("is"); t = cur(); if (t->kind != T_WORD) die_at(t->line, "expected a USAGE"); }
        int u = -1, uv = UV_NONE;
        if (!strcmp(t->s, "display")) u = U_DISPLAY;
        else if (!strcmp(t->s, "comp") || !strcmp(t->s, "computational") || !strcmp(t->s, "binary")) u = U_BINARY;
        else if (!strcmp(t->s, "comp-4") || !strcmp(t->s, "computational-4")) { bp(BP_E3_COMP_N, t->line); u = U_BINARY; }   /* IBM, MF: BINARY */
        else if (!strcmp(t->s, "comp-x") || !strcmp(t->s, "computational-x")) {
            /* MF: unsigned big-endian binary, the field's capacity the limit;
             * a PICTURE of X(n) is n bytes (sym_finish) */
            bp(BP_E3_COMP_N, t->line); u = U_COMP5; uv = UV_COMPX;
        }
        else if (!strcmp(t->s, "comp-6") || !strcmp(t->s, "computational-6")) {
            /* MF's default COMP-6"2" (RM's, for an unsigned item): packed
             * decimal with no sign nibble; a signed one is COMP-3 (sym_finish) */
            bp(BP_E3_COMP_N, t->line); u = U_PACKED; uv = UV_NOSIGN;
        }
        else if (!strcmp(t->s, "comp-3") || !strcmp(t->s, "computational-3") || !strcmp(t->s, "packed-decimal")) {
            if (strcmp(t->s, "packed-decimal")) bp(BP_E3_COMP_N, t->line);
            u = U_PACKED;
        }
        else if (!strcmp(t->s, "comp-5") || !strcmp(t->s, "computational-5")) { bp(BP_E3_COMP_N, t->line); u = U_COMP5; }
        else if (!strcmp(t->s, "binary-long") || !strcmp(t->s, "binary-short")) {
            /* [SIGNED | UNSIGNED], signed by default (2023 13.18.60.2) */
            if (g_std < 2002) bp(BP_E5_BINARY_2002, t->line);
            int lng = t->s[7] == 'l';
            advance();
            int uns = accept_word("unsigned");
            if (!uns) accept_word("signed");
            u = lng ? (uns ? U_UINT : U_SINT) : (uns ? U_USHORT : U_SSHORT);
            if (s->has_usage) die_at(t->line, "'%s' has two USAGE clauses", s->name);
            s->usage = u; s->has_usage = 1;
            continue;
        }
        else if (!strcmp(t->s, "binary-double")) {
            /* [SIGNED | UNSIGNED], signed by default; its 19 digits need the
             * wide path (docs/wide.md), so COBOL 2002 only */
            if (g_std < 2002) die_at(t->line, "USAGE BINARY-DOUBLE is COBOL 2002; compile with -std=2002");
            advance();
            int uns = accept_word("unsigned");
            if (!uns) accept_word("signed");
            if (s->has_usage) die_at(t->line, "'%s' has two USAGE clauses", s->name);
            s->usage = uns ? U_UDBL : U_SDBL; s->has_usage = 1;
            continue;
        }
        else if (!strcmp(t->s, "signed-int")) { bp(BP_E4_VENDOR_BINARY, t->line); u = U_SINT; }
        else if (!strcmp(t->s, "unsigned-int")) { bp(BP_E4_VENDOR_BINARY, t->line); u = U_UINT; }
        else if (!strcmp(t->s, "signed-short")) { bp(BP_E4_VENDOR_BINARY, t->line); u = U_SSHORT; }
        else if (!strcmp(t->s, "unsigned-short")) { bp(BP_E4_VENDOR_BINARY, t->line); u = U_USHORT; }
        else if (!strcmp(t->s, "binary-char")) {
            if (g_std < 2002) bp(BP_E5_BINARY_2002, t->line);
            advance();
            u = accept_word("unsigned") ? U_UBCHAR : U_BCHAR;
            if (u == U_BCHAR) accept_word("signed");
            s->usage = u; s->has_usage = 1;
            continue;
        }
        else if (g_std >= 2002 && !strcmp(t->s, "bit")) u = U_BIT;
        else if (g_std < 2002 && (!strcmp(t->s, "typedef") || (!strcmp(t->s, "type") && is_word(peek(1), "to"))))
            die_at(t->line, "%s is COBOL 2002; compile with -std=2002", !strcmp(t->s, "typedef") ? "TYPEDEF" : "TYPE TO");
        else if (!strcmp(t->s, "group-usage")) {
            /* GROUP-USAGE IS NATIONAL (2023 13.18.29): the group is treated
             * as one national item; checked once the tree is built */
            if (g_std < 2002) die_at(t->line, "GROUP-USAGE is COBOL 2002; compile with -std=2002");
            advance(); accept_word("is");
            if (accept_word("bit")) { s->bitgroup = 2; continue; }     /* 2023 13.18.29.4 rule 1 */
            if (!accept_word("national")) die_at(t->line, "expected NATIONAL or BIT after GROUP-USAGE");
            s->natgroup = 2;                    /* 2: written here; 1: inherited */
            continue;
        }
        else if (!strcmp(t->s, "national")) {
            /* USAGE NATIONAL (COBOL 2002): here with a PICTURE of N only */
            if (g_std < 2002) die_at(t->line, "USAGE NATIONAL is COBOL 2002; compile with -std=2002");
            s->nat_usage = 1; advance(); continue;
        }
        else if (!strcmp(t->s, "pointer")) { if (g_std < 2002) bp(BP_E5_BINARY_2002, t->line); u = U_POINTER; }
        else if (!strcmp(t->s, "program-pointer")) {
            /* USAGE PROGRAM-POINTER [TO program-prototype-name] (2023
             * 13.18.60): a program's entry address, NULL or one ADDRESS
             * OF PROGRAM gave it; TO restricts it to the prototype's
             * signature, which a CALL through it then checks against */
            if (g_std < 2002) die_at(t->line, "USAGE PROGRAM-POINTER is COBOL 2002 (2023 13.18.60); compile with -std=2002");
            u = U_POINTER; uv = UV_PPTR;
            if (is_word(peek(1), "to")) {
                advance();
                if (peek(1)->kind != T_WORD || repo_pg_find(peek(1)->s) < 0)
                    die_at(peek(1)->line, "'%s': PROGRAM-POINTER TO names a program prototype of the REPOSITORY (2023 13.18.60)", s->name);
                snprintf(s->ptr_proto, sizeof s->ptr_proto, "%s", peek(1)->s);
                advance();                  /* at the prototype's name, which the usage's advance passes */
            }
        }
        else if (!strcmp(t->s, "index")) u = U_INDEX;
        else if (!strcmp(t->s, "comp-1") || !strcmp(t->s, "computational-1")) {
            /* with a PICTURE, RM/COBOL's binary integer (the Open Systems
             * suite's COMP-1 items all carry one); without, MF's IEEE
             * single: sym_finish decides, or -fcomp1= */
            bp(BP_E3_COMP_N, t->line);
            u = U_BINARY; uv = UV_COMP1;
        }
        else if (!strcmp(t->s, "comp-2") || !strcmp(t->s, "computational-2")) { bp(BP_E3_COMP_N, t->line); u = U_FLOAT; uv = UV_FLONG; }
        else if (!strcmp(t->s, "float-short") || !strcmp(t->s, "float-long") || !strcmp(t->s, "float-extended")) {
            /* FLOAT-EXTENDED need only hold what FLOAT-LONG holds (2023
             * 13.18.60.4 rule 13): a double here too */
            if (g_std < 2002) die_at(t->line, "USAGE %s is COBOL 2002; compile with -std=2002 (COMP-1 and COMP-2 are the same)", t->s);
            u = U_FLOAT; uv = t->s[6] == 's' ? UV_FSHORT : UV_FLONG;
        }
        else if (!strncmp(t->s, "float-binary-", 13) || !strncmp(t->s, "float-decimal-", 14))
            die_at(t->line, "USAGE %s is COBOL 2014 (ISO/IEC 60559 formats); not implemented", t->s);
        if (u >= 0) {
            if (s->has_usage) die_at(t->line, "'%s' has two USAGE clauses", s->name);
            s->usage = u; s->uvar = uv; s->has_usage = 1;
            advance();
            continue;
        }

        if (!strcmp(t->s, "value")) {
            advance(); accept_word("is");
            if (accept_word("all")) s->value_all = 1;
            Tok *v = cur();
            if (v->kind == T_STR || v->kind == T_NUM) { s->value_tok = v; advance(); }
            else if (v->kind == T_WORD && is_figurative(v->s)) { s->value_fig = 1; s->value_tok = v; advance(); }
            else die_at(v->line, "expected a literal after VALUE, found %s", tok_desc(v));
            if (at_word("thru") || at_word("through"))
                die_at(cur()->line, "VALUE ... THRU is only for level 88");
            continue;
        }
        if (!strcmp(t->s, "occurs")) {
            advance();
            if (at_word("unbounded")) die_at(t->line, "OCCURS UNBOUNDED is COBOL 2002 (not in the 1985 text)");
            if (at_word("dynamic")) die_at(t->line, "OCCURS DYNAMIC (a dynamic-capacity table) is COBOL 2014 (2023 13.18.38 format 4); not implemented");
            if (cur()->kind != T_NUM) die_at(t->line, "expected a count after OCCURS");
            s->occurs = atoi(cur()->s);
            advance();
            if (accept_word("to")) {
                /* OCCURS m TO n DEPENDING ON d: laid out at n (the 85 rule for a
                 * receiving item); d says how many are in use */
                if (cur()->kind != T_NUM) die_at(t->line, "expected the maximum after OCCURS m TO");
                s->odo_min = s->occurs; s->occurs = atoi(cur()->s); advance();
                if (s->occurs <= s->odo_min)
                    die_at(t->line, "OCCURS %d TO %d: the maximum must be greater than the minimum (%s)", s->odo_min, s->occurs,
                           g_std < 2002 ? "X3.23-1985 OCCURS syntax rule 5" : "2023 13.18.38.3 rule 16");
                accept_word("times");
                if (!accept_word("depending")) die_at(t->line, "OCCURS m TO n needs DEPENDING ON");
                accept_word("on");
                if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after DEPENDING ON");
                snprintf(s->odo_dep, sizeof s->odo_dep, "%s", cur()->s); advance();
            }
            accept_word("times");
            for (;;) {
                int desc = at_word("descending");
                if (accept_word("ascending") || accept_word("descending")) {
                    accept_word("key"); accept_word("is");
                    while (cur()->kind == T_WORD && !at_word("indexed") && !at_word("ascending") &&
                           !at_word("descending") && !at_word("pic") && !at_word("picture") &&
                           !at_word("value") && !at_word("usage")) {
                        if (s->nokey < 8) {     /* kept for SEARCH ALL's binary search */
                            snprintf(s->okey[s->nokey], sizeof s->okey[0], "%s", cur()->s);
                            s->okey_desc[s->nokey++] = (unsigned char)desc;
                        }
                        advance();
                        /* a qualified key (85 OCCURS rule 2): the qualifiers
                         * name the table and its groups, not further keys */
                        while (at_word("of") || at_word("in")) { advance(); if (cur()->kind == T_WORD) advance(); }
                    }
                    continue;
                }
                if (accept_word("indexed")) {
                    accept_word("by");
                    while (cur()->kind == T_WORD && !at_word("pic") && !at_word("picture") &&
                           !at_word("value") && !at_word("usage") && !at_word("ascending") &&
                           !at_word("descending") && !at_word("comp") && !at_word("comp-3") &&
                           !at_word("comp-5") && !at_word("display") && !at_word("sync")) {
                        user_word(cur()->s, cur()->line, "an index");
                        Sym *ix = sym_new();
                        snprintf(ix->name, sizeof ix->name, "%s", cur()->s);
                        ix->line = cur()->line; ix->usage = U_INDEX; ix->has_usage = 1;
                        ix->is_index = 1; ix->level = 1;
                        int ixi = sym_idx(ix);
                        advance();
                        s = &g_sym[g_last_item];      /* sym_new may have moved the array */
                        if (s->idx1 < 0) s->idx1 = ixi;
                        g_sym[ixi].ix_table = g_last_item;
                    }
                    continue;
                }
                break;
            }
            if (s->occurs < 1) die_at(t->line, "OCCURS needs a count of at least 1");
            continue;
        }
        if (!strcmp(t->s, "redefines")) {
            if (t != first_clause)
                die_at(t->line, "'%s': REDEFINES comes first, immediately after the data-name or FILLER (%s)", s->name,
                       g_std < 2002 ? "X3.23-1985 VI-21, data description entry syntax rule 2" : "2023 13.16.3 rule 4");
            advance();
            if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after REDEFINES");
            if (!strcmp(cur()->s, "filler")) die_at(t->line, "REDEFINES FILLER: the redefined item needs a name (FILLER cannot be referenced)");
            /* data-name-2 is the entry just before this one at its level,
             * with nothing at a lower level between (13.18.44.3 rule 4) and
             * no other storage between (rule 10) -- or, when that entry is
             * itself a redefinition, the entry it redefines (rule 7) */
            int e85 = g_std < 2002, lv = level == 77 ? 1 : level, prev = -1;
            for (int i = sym_idx(s) - 1; i >= g_sym_base; i--) {
                Sym *q = &g_sym[i];
                if (q->is_cond || q->is_index || q->level == 66 || q->is_ftemp) continue;
                int ql = q->level == 77 ? 1 : q->level;
                if (ql > lv) continue;                  /* inside an earlier sibling */
                if (ql == lv) prev = i;
                break;
            }
            int orig = prev >= 0 && g_sym[prev].redefines >= 0 && g_sym[prev].redef_clause ? g_sym[prev].redefines : prev;
            if (orig >= 0 && !strcmp(g_sym[orig].name, cur()->s) && g_sym[orig].level == level) s->redefines = orig;
            else if (prev >= 0 && !strcmp(g_sym[prev].name, cur()->s) && g_sym[prev].level == level)
                die_at(t->line, "'%s' REDEFINES '%s', itself a redefinition: name the entry that first described the storage, '%s' (%s)",
                       s->name, cur()->s, g_sym[orig].name, e85 ? "X3.23-1985 REDEFINES syntax rule 8" : "2023 13.18.44.3 rule 7");
            else {
                int any = -1;
                for (int i = sym_idx(s) - 1; i >= g_sym_base; i--)
                    if (!g_sym[i].is_cond && !strcmp(g_sym[i].name, cur()->s)) { any = i; break; }
                if (any < 0) die_at(t->line, "'%s' REDEFINES '%s', which is not declared before it", s->name, cur()->s);
                if (g_sym[any].level != level)
                    die_at(t->line, "'%s' (level %02d) REDEFINES '%s' (level %02d): the levels must be the same (%s)", s->name, level, cur()->s, g_sym[any].level,
                           e85 ? "X3.23-1985 REDEFINES syntax rule 2" : "2023 13.18.44.3 rule 2");
                die_at(t->line, "'%s' REDEFINES '%s', but other entries come between them; a redefinition follows the entry it redefines (%s)",
                       s->name, cur()->s, e85 ? "X3.23-1985 REDEFINES syntax rules 10 and 11" : "2023 13.18.44.3 rules 4 and 10");
            }
            s->redef_clause = 1;
            advance();
            continue;
        }
        if (!strcmp(t->s, "sync") || !strcmp(t->s, "synchronized")) {
            advance(); accept_word("left"); accept_word("right");
            s->sync = 1; continue;
        }
        if (!strcmp(t->s, "based")) {
            /* BASED (2002 13.16.5): a template reached through an implicit
             * data-address pointer, NULL until SET ADDRESS OF gives it one */
            if (g_std < 2002) die_at(t->line, "BASED is COBOL 2002; compile with -std=2002");
            if (level != 1 && level != 77) die_at(t->line, "'%s': BASED is for a level 01 or 77 entry here", s->name);
            advance(); s->is_based = 1; continue;
        }
        if (!strcmp(t->s, "just") || !strcmp(t->s, "justified")) {
            advance(); accept_word("right");
            s->just = 1; continue;
        }
        if (!strcmp(t->s, "blank")) {
            advance(); accept_word("when"); if (!(accept_word("zero") || accept_word("zeros") || accept_word("zeroes")))
                die_at(t->line, "expected ZERO after BLANK WHEN");
            s->blank_zero = 1; continue;
        }
        if (!strcmp(t->s, "sign") || !strcmp(t->s, "leading") || !strcmp(t->s, "trailing")) {
            /* [SIGN IS] LEADING|TRAILING [SEPARATE [CHARACTER]] */
            if (accept_word("sign")) accept_word("is");
            if (accept_word("leading")) s->sign_lead = 1;
            else if (accept_word("trailing")) s->sign_lead = 0;
            else die_at(t->line, "SIGN needs LEADING or TRAILING");
            if (accept_word("separate")) { s->sign_sep = 1; accept_word("character"); }
            continue;
        }
        if (!strcmp(t->s, "global")) { advance(); s->is_global = 1; continue; }
        if (!strcmp(t->s, "external")) {
            advance(); s->is_external = 1;
            if (at_word("as")) {
                /* AS literal: the externalized name the storage is shared
                 * under (2023 13.18.22 rules 2-3) */
                if (g_std < 2002) die_at(cur()->line, "EXTERNAL AS is not COBOL 85 (X3.23-1985 X-23)");
                advance();
                if (cur()->kind != T_STR || cur()->len == 0) die_at(cur()->line, "'%s': EXTERNAL AS takes a nonempty alphanumeric literal (2023 13.18.22.3 rule 3)", s->name);
                snprintf(s->ext_as, sizeof s->ext_as, "%.*s", cur()->len < 63 ? cur()->len : 63, cur()->s);
                advance();
            }
            continue;
        }
        if (!strcmp(t->s, "constant") && is_word(peek(1), "record"))
            die_at(t->line, "'%s': the CONSTANT RECORD clause is COBOL 2014, beyond %s (2023 13.18.15)", s->name, g_std < 2002 ? "COBOL 85" : "-std=2002");
        if (!strcmp(t->s, "constant"))
            die_at(t->line, g_std < 2002 ? "a constant entry (level 01 CONSTANT) is COBOL 2002; compile with -std=2002" :
                   "'%s': CONSTANT comes right after the name of a level 01 entry (2023 13.10)", s->name);
        if (!strcmp(t->s, "dynamic") && is_word(peek(1), "length"))
            die_at(t->line, "'%s': the DYNAMIC LENGTH clause is COBOL 2014, beyond %s (2023 13.18.19)", s->name, g_std < 2002 ? "COBOL 85" : "-std=2002");
        if (!strcmp(t->s, "any") && is_word(peek(1), "length") && g_std >= 2002) {
            advance(); advance(); s->any_len = 1; continue;
        }
        if ((!strcmp(t->s, "same") && is_word(peek(1), "as")) || (!strcmp(t->s, "any") && is_word(peek(1), "length")) || !strcmp(t->s, "locale")) {
            const char *what = !strcmp(t->s, "same") ? "the SAME AS clause (2023 13.18.49)" :
                               !strcmp(t->s, "any") ? "the ANY LENGTH clause (2023 13.18.2)" : "the LOCALE phrase of PICTURE (2023 13.18.40)";
            if (g_std < 2002) die_at(t->line, "'%s': %s is COBOL 2002, not 85", s->name, what);
            die_at(t->line, "'%s': %s is not implemented", s->name, what);
        }
        if (!strcmp(t->s, "aligned")) {
            /* ALIGNED (2023 13.18.1): a bit item or bit group at the first
             * bit of the next byte, each occurrence so; checked once the
             * usage is known (data_rules) */
            if (g_std < 2002) die_at(t->line, "'%s': the ALIGNED clause is COBOL 2002 (2023 13.18.1); compile with -std=2002", s->name);
            s->aligned = 1; advance(); continue;
        }
        if (!strcmp(t->s, "function-pointer") || !strcmp(t->s, "message-tag"))
            die_at(t->line, "'%s': USAGE %s is COBOL %s (2023 13.18.60); not implemented", s->name, t->s,
                   t->s[0] == 'f' ? "2014" : "2023");
        if (!strcmp(t->s, "no") && is_word(peek(1), "sign"))
            die_at(t->line, "'%s': USAGE PACKED-DECIMAL NO SIGN is COBOL 2023 (13.18.60); not implemented", s->name);
        die_at(t->line, "unexpected %s in the description of '%s'", tok_desc(t), s->name);
    }
    expect_period();

    entry_rules(s, level, line);
    if (level == 77 && s->occurs)
        die_at(line, "a level 77 item cannot have OCCURS");
    if ((s->sign_lead || s->sign_sep) && s->has_pic && (s->usage != U_DISPLAY || s->pi.category != PIC_NUMERIC || !s->pi.is_signed))
        die_at(line, "SIGN applies to a signed numeric DISPLAY item; '%s' is not one", s->name);
    if (level == 1 && s->occurs)
        die_at(line, "OCCURS is not allowed at level 01");
    if (g_cur_fd >= 0 && level == 1) {
        /* every 01 under an FD is a view of the same record area */
        if (s->redef_clause)
            die_at(line, "'%s': a level 01 entry in the FILE SECTION takes no REDEFINES; its records already share the area (%s)", s->name,
                   g_std < 2002 ? "X3.23-1985 REDEFINES syntax rule 3" : "2023 13.18.44.3 rule 3");
        File *f = &g_files[g_cur_fd];
        s->fd = g_cur_fd;
        if (f->rec < 0) f->rec = sym_idx(s); else s->redefines = f->rec;
    } else if (g_cur_fd >= 0 && level == 77)
        die_at(line, "a level 77 item cannot appear in the FILE SECTION");
}
