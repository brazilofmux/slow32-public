/* s32-cobc: source reader: reference formats.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ====================================================================== */
/* Source reader: reference formats                                        */
/* ====================================================================== */

/* Fixed: columns 1-6 sequence, 7 indicator, 8-72 program text, 73+ ignored.
 * Free (GnuCOBOL -free, majesty; COBOL 2002 6.4): the whole line is text.
 * Comments: '*' or '/' in column 7 (fixed); '*>' to end of line (both --
 * the floating comment is 2002 but majesty is written with it and it is
 * harmless).  The format is chosen on the command line; under -std=2002 a
 * >>SOURCE FORMAT line changes it for the rest of the text, and a literal
 * may be continued with the floating indicator "- or '- in either format
 * (cobol ISSUES-51). */

typedef struct { char *text; int line; int dbg, dir; const char *file; int ff; } SrcLine;   /* dbg: a D in column 7; dir: a compiler directive kept for the parser; file: where it was read; ff: read in free form */
static SrcLine *g_lines;
static int g_nlines;

static int g_col_bytes;             /* -fixed-columns=bytes: reference-format columns are bytes */

/* the byte offset of character column k (0-based) of a line: code points
 * (a byte that is not a UTF-8 continuation byte begins a character), or
 * bytes under -fixed-columns=bytes; len when the line is shorter */
static int colb(const char *p, int len, int k)
{
    if (g_col_bytes) return k < len ? k : len;
    int c = -1;
    for (int i = 0; i < len; i++) {
        if (((unsigned char)p[i] & 0xC0) != 0x80) c++;
        if (c == k) return i;
    }
    return len;
}

/* the columns a piece of text occupies, by the same count */
static int colcount(const char *p, int len)
{
    if (g_col_bytes) return len;
    int c = 0;
    for (int i = 0; i < len; i++) if (((unsigned char)p[i] & 0xC0) != 0x80) c++;
    return c;
}

/* read a source file (or a copybook) into lines of program text; 0 if
 * it cannot be opened */
/* are the lines so far inside an EXEC SQL ... END-EXEC?  (the last of
 * the two to appear, looking back) */
static int in_exec_sql(const SrcLine *lines, int n)
{
    for (int k = n - 1; k >= 0 && k >= n - 400; k--) {
        const char *t = lines[k].text, *hit = NULL; int kind = 0;
        for (const char *q = t; *q; q++) {
            if (!strncasecmp(q, "end-exec", 8)) { hit = q; kind = 2; }
            else if (!strncasecmp(q, "exec", 4) && (q[4] == ' ' || q[4] == '\t')) {
                const char *r = q + 4; while (*r == ' ' || *r == '\t') r++;
                if (!strncasecmp(r, "sql", 3)) { hit = q; kind = 1; }
            }
        }
        if (hit) return kind == 1;
    }
    return 0;
}

static int g_read_ff_start;      /* 0: the default format; else 1 + the format a COPY's library text starts in */
static int read_lines(const char *path, SrcLine **out, int *nout)
{
    FILE *f = fopen(path, "rb");
    if (!f) return 0;
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = xmalloc(sz + 1);
    if (fread(buf, 1, sz, f) != (size_t)sz) { fprintf(stderr, "s32-cobc: read error\n"); exit(1); }
    fclose(f);
    buf[sz] = 0;

    int cap = 256, n = 0;
    SrcLine *lines = xmalloc(cap * sizeof *lines);
    int lineno = 0;
    const char *save_file = g_tok_file;
    g_tok_file = path;              /* an error while reading names this file */
    const char *fpath = xstrndup(path, (int)strlen(path));
    char *p = buf;
    /* the format this text starts in: the compilation group's default, or
     * for library text the format in effect at its COPY (2023 7.3.24.3
     * rule 3); >>SOURCE FORMAT changes it for the rest of this text, and
     * the COPY's own format is untouched by what happens here (rule 5) */
    int free_form = g_read_ff_start ? g_read_ff_start - 1 : g_free;
    char pending = 0;               /* a literal continued by a floating indicator ("- or '-) */
    while (*p) {
        char *e = strchr(p, '\n');
        int len = e ? (int)(e - p) : (int)strlen(p);
        lineno++;
        if (len && p[len - 1] == '\r') len--;
        if (!free_form && memchr(p, '\t', (size_t)len)) {
            /* a tab in reference format: spaces to the next stop of every
             * eight columns, as GnuCOBOL and Micro Focus read it (the
             * standard leaves tabs to the implementor) -- ACAS's mapser
             * starts lines with one at column 7 (cobol ISSUES-124).  The
             * line is read from the expanded copy; e still ends it in buf */
            static char *tabx; static size_t tabcap;
            if ((size_t)len * 8 + 1 > tabcap) { tabcap = (size_t)len * 8 + 1; tabx = xrealloc(tabx, tabcap); }
            size_t o = 0; int col = 0;
            for (int i = 0; i < len; i++) {
                unsigned char ch = (unsigned char)p[i];
                if (ch == '\t') { do { tabx[o++] = ' '; col++; } while (col % 8); }
                else { tabx[o++] = (char)ch; if ((ch & 0xC0) != 0x80) col++; }   /* columns are code points */
            }
            tabx[o] = 0;
            p = tabx; len = (int)o;
        }
        char *text = NULL; int dbg = 0;
        /* a compiler-directive line (COBOL 2002 7.3): >> as the first
         * non-blank, in free form anywhere, in fixed form from column 7 */
        {
            int from = free_form ? 0 : colb(p, len, 6);
            if (!free_form && lineno == 1) {
                /* a SOURCE FORMAT directive that is the first line of a
                 * compilation group or of library text may be in either
                 * form (2023 7.3.24.3 rule 4): in fixed form, before column 7 */
                const char *q = p, *qe = p + len;
                while (q < qe && *q == ' ') q++;
                if (qe - q >= 8 && q[0] == '>' && q[1] == '>' && !strncasecmp(q + 2, "source", 6)) from = (int)(q - p);
            }
            const char *d = p + (len > from ? from : len), *de = p + len;
            while (d < de && (*d == ' ' || *d == '\t')) d++;
            if (de - d >= 2 && d[0] == '>' && d[1] == '>') {
                if (g_std < 2002) die_at(lineno, "compiler directives (>>) are COBOL 2002; compile with -std=2002");
                d += 2; while (d < de && *d == ' ') d++;
                char w[4][32]; int nw = 0;
                while (d < de && nw < 4) {
                    int k = 0;
                    while (d < de && *d != ' ' && *d != '\t' && k < 31) w[nw][k++] = (char)tolower((unsigned char)*d++);
                    w[nw++][k] = 0;
                    while (d < de && (*d == ' ' || *d == '\t')) d++;
                    if (d + 1 < de && d[0] == '*' && d[1] == '>') break;      /* an inline comment ends it */
                }
                int k = 1;
                if (nw && !strcmp(w[0], "turn")) {
                    /* >>TURN: applied by the parser where it stands among the statements */
                    const char *t0 = p + (len > from ? from : len);
                    while (*t0 == ' ' || *t0 == '\t') t0++;
                    t0 += 2;
                    int tl = (int)(de - t0);
                    const char *cm = NULL;
                    for (const char *q = t0; q + 1 < de; q++) if (q[0] == '*' && q[1] == '>') { cm = q; break; }
                    if (cm) tl = (int)(cm - t0);
                    if (n == cap) { cap *= 2; lines = realloc(lines, cap * sizeof *lines); }
                    lines[n].text = xstrndup(t0, tl); lines[n].line = lineno; lines[n].dbg = 0; lines[n].dir = 1; lines[n].file = fpath; lines[n].ff = free_form;
                    n++;
                    if (!e) break;
                    p = e + 1;
                    continue;
                }
                if (nw && !strcmp(w[0], "source")) {
                    if (k < nw && !strcmp(w[k], "format")) k++;
                    if (k < nw && !strcmp(w[k], "is")) k++;
                    if (k < nw && !strcmp(w[k], "free")) free_form = 1;
                    else if (k < nw && !strcmp(w[k], "fixed")) free_form = 0;
                    else die_at(lineno, ">>SOURCE FORMAT needs FIXED or FREE");
                    if (k + 1 < nw) die_at(lineno, "unexpected '%s' after >>SOURCE FORMAT", w[k + 1]);
                } else if (nw && !strcmp(w[0], "d")) {
                    die_at(lineno, "the >>D debugging indicator is not implemented (debugging lines were removed in COBOL 2014)");
                } else die_at(lineno, "the compiler directive >>%s is not implemented yet", nw ? w[0] : "");
                if (!e) break;
                p = e + 1;
                continue;
            }
        }
        /* a Micro Focus directive line: $ in the indicator column in fixed
         * form, the first non-blank in free form (BP-E30) */
        {
            int at = free_form ? 0 : colb(p, len, 6);
            const char *d = p + (len > at ? at : len), *de = p + len;
            if (free_form) while (d < de && (*d == ' ' || *d == '\t')) d++;
            if (d + 1 < de && *d == '$' && isalpha((unsigned char)d[1])) {   /* $$$9.99 is a picture going on */
                d++;
                char w[32]; int k = 0;
                while (d < de && isalpha((unsigned char)*d) && k < 31) w[k++] = (char)tolower((unsigned char)*d++);
                w[k] = 0;
                if (strcmp(w, "set")) die_at(lineno, "the Micro Focus directive line $%s is not implemented (only $SET)", w);
                bp(BP_E30_DOLLAR_SET, lineno);
                /* directives: NAME, NAME"value", NAME'value' or NAME(value) */
                static const char *quiet[] = { "list", "nolist", "listwidth", "listpath", "form", "noform", "echo", "noecho",
                    "xref", "noxref", "ref", "noref", "settings", "nosettings", "confirm", "noconfirm", "warning", "nowarning",
                    "anim", "noanim", NULL };
                for (;;) {
                    while (d < de && (*d == ' ' || *d == '\t')) d++;
                    if (d >= de || (d + 1 < de && d[0] == '*' && d[1] == '>')) break;
                    char nm[40], val[40]; int nk = 0, vk = 0;
                    while (d < de && (isalnum((unsigned char)*d) || *d == '-' || *d == '_') && nk < 39) nm[nk++] = (char)tolower((unsigned char)*d++);
                    nm[nk] = 0;
                    if (!nk) die_at(lineno, "$SET: expected a directive, found '%c'", *d);
                    val[0] = 0;
                    if (d < de && (*d == '"' || *d == '\'' || *d == '(')) {
                        char close = *d == '(' ? ')' : *d;
                        d++;
                        while (d < de && *d != close && vk < 39) val[vk++] = (char)tolower((unsigned char)*d++);
                        val[vk] = 0;
                        if (d >= de) die_at(lineno, "$SET %s: the value is not closed", nm);
                        d++;
                    }
                    if (!strcmp(nm, "sourceformat")) {
                        if (!strcmp(val, "free")) free_form = 1;
                        else if (!strcmp(val, "fixed")) free_form = 0;
                        else die_at(lineno, "$SET SOURCEFORMAT\"%s\" is not implemented (FREE and FIXED are)", val);
                        continue;
                    }
                    int ok = 0;
                    for (int q = 0; quiet[q]; q++) if (!strcmp(nm, quiet[q])) ok = 1;
                    if (!ok) die_at(lineno, "$SET %s: this Micro Focus directive is not implemented (SOURCEFORMAT is taken, and listing directives are without effect)", nm);
                }
                if (!e) break;
                p = e + 1;
                continue;
            }
        }
        if (free_form) {
            text = xstrndup(p, len);
        } else {
            /* the reference format's columns: code points, so a card image
             * keeps its layout when its text becomes UTF-8 (-fixed-columns=
             * bytes counts bytes, as GnuCOBOL and IBM's byte columns do) */
            int i7 = colb(p, len, 6), i8 = colb(p, len, 7), i73 = colb(p, len, 72);
            if (i7 < len) {
                char ind = p[i7];
                if ((unsigned char)ind >= 0x80) ind = '?';
                if (ind == '*' || ind == '/') text = NULL;         /* comment */
                else if (ind == 'D' || ind == 'd') {
                    /* a debugging line: text for COPY/REPLACE matching ("as
                     * if the D did not appear"), dropped afterwards unless
                     * the program says WITH DEBUGGING MODE (tokenize) */
                    int cn = i73 - i8; if (cn < 0) cn = 0;
                    text = xstrndup(p + i8, cn); dbg = 1;
                }
                else if (ind == '-') {
                    /* continuation: the previous text line goes on here.  If
                     * it stopped inside a non-numeric literal, this line's
                     * first non-blank must be that literal's quote and the
                     * text after the quote joins directly (the previous line
                     * kept its trailing spaces up to column 72); otherwise
                     * the first non-blank joins with no space between. */
                    if (n == 0) die_at(lineno, "a continuation line with nothing to continue");
                    char *prev = lines[n - 1].text;
                    char open = 0;                      /* quote of an unclosed literal */
                    for (char *q = prev; *q; q++) {
                        if (open) { if (*q == open) open = 0; }
                        else if (*q == '"' || *q == '\'') open = *q;
                    }
                    int cn = i73 - i8; if (cn < 0) cn = 0;
                    const char *c = p + i8, *ce = p + i8 + cn;
                    while (c < ce && (*c == ' ' || *c == '\t')) c++;
                    /* a literal whose quotes look balanced but whose last character,
                     * at the end of the line, is a quote, met by a continuation
                     * line beginning with the same quote: the two are the halves
                     * of an embedded doubled quote, the literal still open (NC215A:
                     * "...8J" at column 72, then -    ""9K...) */
                    if (!open) {
                        size_t pl = strlen(prev);
                        if (colcount(prev, (int)pl) == 65 && (prev[pl - 1] == '"' || prev[pl - 1] == '\'') && c < ce && *c == prev[pl - 1]) open = prev[pl - 1];   /* column 72 exactly */
                    }
                    if (open) {
                        /* a SQL string continued with the other quote mark:
                         * the NIST SQL suite continues '...' with a " (docs/esql.md) */
                        if (c < ce && *c != open && (*c == '"' || *c == '\'') && in_exec_sql(lines, n)) open = *c;
                        if (c >= ce || *c != open)
                            die_at(lineno, "a continuation of a literal must begin with its quote (%c)", open);
                        c++;
                    }
                    size_t pl = strlen(prev), cl = (size_t)(ce - c);
                    if (!open) while (pl > 0 && (prev[pl - 1] == ' ' || prev[pl - 1] == '\t')) pl--;
                    char *joined = xmalloc(pl + cl + 1);
                    memcpy(joined, prev, pl); memcpy(joined + pl, c, cl); joined[pl + cl] = 0;
                    free(prev);
                    lines[n - 1].text = joined;
                    text = NULL;
                }
                else if (ind != ' ')
                    die_at(lineno, "unrecognised indicator '%c' in column 7 "
                           "(free-format source? compile it with -free)", ind);
                else {
                    int n = i73 - i8; if (n < 0) n = 0;             /* 8..72 */
                    text = xstrndup(p + i8, n);
                }
            }
        }
        /* a floating literal continuation (COBOL 2002 6.2.3, 6.4.2): the
         * line before ended an open literal with "- (or '-); this one
         * resumes it after the same quote.  Comment and blank lines may
         * come between. */
        if (text) {
            const char *c = text;
            while (*c == ' ' || *c == '\t') c++;
            int comment = c[0] == '*' && c[1] == '>';
            if (pending && (comment || !*c)) { free(text); text = NULL; }   /* between the parts of the literal */
            else if (pending) {
                if (*c != pending) die_at(lineno, "the continuation of a literal must begin with its quote (%c)", pending);
                char *prev = lines[n - 1].text;
                size_t pl = strlen(prev), cl = strlen(c + 1);
                char *joined = xmalloc(pl + cl + 1);
                memcpy(joined, prev, pl); memcpy(joined + pl, c + 1, cl); joined[pl + cl] = 0;
                free(prev); free(text);
                lines[n - 1].text = joined;
                text = NULL;
                pending = 0;
                /* the joined line may itself end in a continuation */
                char *t = lines[n - 1].text;
                size_t tl = strlen(t);
                while (tl && (t[tl - 1] == ' ' || t[tl - 1] == '\t')) tl--;
                char open = 0;
                for (size_t q = 0; q + 2 < tl; q++) {
                    if (open) { if (t[q] == open) open = 0; }
                    else if (t[q] == '"' || t[q] == '\'') open = t[q];
                }
                if (open && tl >= 2 && t[tl - 1] == '-' && t[tl - 2] == open) {
                    if (g_std < 2002) die_at(lineno, "a floating literal continuation (\"- or '-) is COBOL 2002; compile with -std=2002");
                    t[tl - 2] = 0; pending = open;
                }
            } else if (!comment && *c) {
                size_t tl = strlen(text);
                while (tl && (text[tl - 1] == ' ' || text[tl - 1] == '\t')) tl--;
                char open = 0;
                for (size_t q = 0; q + 2 < tl; q++) {
                    if (open) { if (text[q] == open) open = 0; }
                    else if (text[q] == '"' || text[q] == '\'') open = text[q];
                }
                if (open && tl >= 2 && text[tl - 1] == '-' && text[tl - 2] == open) {
                    if (g_std < 2002) die_at(lineno, "a floating literal continuation (\"- or '-) is COBOL 2002; compile with -std=2002");
                    text[tl - 2] = 0; pending = open;
                }
            }
        }
        if (text) {
            if (n == cap) { cap *= 2; lines = realloc(lines, cap * sizeof *lines); }
            lines[n].text = text;
            lines[n].line = lineno;
            lines[n].dbg = dbg; lines[n].dir = 0; lines[n].file = fpath; lines[n].ff = free_form;
            n++;
        }
        if (!e) break;
        p = e + 1;
    }
    if (pending) die_at(lineno, "the text ends inside a continued literal");
    g_tok_file = save_file;
    free(buf);
    *out = lines; *nout = n;
    return 1;
}

static void read_source(const char *path)
{
    if (!read_lines(path, &g_lines, &g_nlines)) { fprintf(stderr, "s32-cobc: cannot open %s\n", path); exit(1); }
}
