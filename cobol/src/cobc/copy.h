/* s32-cobc: COPY and REPLACE.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ---- COPY: the Library module ------------------------------------------ */

/* COPY text-name [OF/IN library] [SUPPRESS] [REPLACING ...]. is replaced,
 * period included, by the copybook's tokens; the copybook is read in the
 * same reference format, may itself COPY, and is looked for as the name
 * given, then name.cpy / .CPY / .cbl / .cob (GnuCOBOL's list), in the
 * source's directory and the -I directories. */
static const char *g_incdirs[16]; static int g_nincdir;

static int copy_open(const char *name, SrcLine **lines, int *n, char *found, size_t foundsz)
{
    static const char *exts[] = { "", ".cpy", ".CPY", ".cbl", ".CBL", ".cob", ".COB", NULL };
    /* A text-name is case-insensitive and the tokenizer lowercased it; a
     * copybook kept under its uppercase name (Open Systems' SCONFIG,
     * TAGSFILE) is found on a case-sensitive filesystem by trying the
     * name upper-cased too -- a literal text-name arrives as written. */
    char upper[256]; snprintf(upper, sizeof upper, "%s", name);
    for (char *k = upper; *k; k++) *k = (char)toupper((unsigned char)*k);
    const char *names[] = { name, strcmp(upper, name) ? upper : NULL, NULL };
    char srcdir[1024]; snprintf(srcdir, sizeof srcdir, "%s", g_file);
    char *sl = strrchr(srcdir, '/'); if (sl) *sl = 0; else strcpy(srcdir, ".");
    for (int d = -1; d < g_nincdir; d++) {
        const char *dir = d < 0 ? srcdir : g_incdirs[d];
        for (int v = 0; names[v]; v++)
            for (int e = 0; exts[e]; e++) {
                snprintf(found, foundsz, "%s/%s%s", dir, names[v], exts[e]);
                if (read_lines(found, lines, n)) return 1;
            }
    }
    return 0;
}

static void join_concat(void);
static struct { int pos; Tok tok; } *g_dir; static int g_ndir, g_dircap, g_ndir_done;   /* >>TURN directives, by token position */

static int g_dp_comma;      /* SPECIAL-NAMES DECIMAL-POINT IS COMMA */
static int g_currency;      /* SPECIAL-NAMES CURRENCY SIGN IS "c": the picture symbol standing for '$', 0 for '$' itself */
static char g_currency_str[32]; static int g_currency_len;   /* ... WITH PICTURE SYMBOL: the currency string the symbol stands for (2023 12.3.7 rule 23), its length; 0: the symbol itself */

/* DECIMAL-POINT IS COMMA swaps the roles of '.' and ',' in numeric
 * literals and pictures.  It may arrive by COPY (SM103A), so it is
 * settled here, after the text is whole: literals '12,5' are joined
 * and pictures rewritten into the ordinary form the rest of the
 * compiler reads; the runtime swaps the characters back when it edits. */
static void apply_decimal_point(void)
{
    for (int i = 0; i + 1 < g_ntok; i++) {
        if (g_tok[i].kind != T_WORD || strcmp(g_tok[i].s, "decimal-point")) continue;
        int j = i + 1;
        if (g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "is")) j++;
        if (j < g_ntok && g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "comma")) { g_dp_comma = 1; break; }
    }
    /* CURRENCY [SIGN] [IS] "c": in every picture c stands for '$', which
     * is what the analyser and the editor read; the runtime prints c */
    g_currency_len = 0;
    for (int i = 0; i + 1 < g_ntok; i++) {
        if (g_tok[i].kind != T_WORD || strcmp(g_tok[i].s, "currency")) continue;
        int j = i + 1;
        if (g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "sign")) j++;
        if (j < g_ntok && g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "is")) j++;
        if (j >= g_ntok || g_tok[j].kind != T_STR) die_at(g_tok[i].line, "CURRENCY SIGN needs a literal");
        if (g_currency) die_at(g_tok[i].line, "a second CURRENCY SIGN clause: one currency symbol per source unit is implemented (2023 12.3.7 rule 21 allows more)");
        Tok *lit = &g_tok[j];
        int k = j + 1, with_ps = 0;
        if (k < g_ntok && g_tok[k].kind == T_WORD && !strcmp(g_tok[k].s, "with")) k++;
        if (k < g_ntok && g_tok[k].kind == T_WORD && !strcmp(g_tok[k].s, "picture")) {
            /* WITH PICTURE SYMBOL literal-8 (2023 12.3.7 rules 23, 26-27):
             * literal-7 the currency string, literal-8 the symbol */
            k++;
            /* the tokenizer took the word after PICTURE for a picture string */
            if (!(k < g_ntok && (g_tok[k].kind == T_WORD || g_tok[k].kind == T_PIC) && !strcasecmp(g_tok[k].s, "symbol"))) die_at(g_tok[k].line, "expected SYMBOL after WITH PICTURE");
            k++;
            if (g_std < 2002) die_at(lit->line, "CURRENCY SIGN ... WITH PICTURE SYMBOL is COBOL 2002 (2023 12.3.7); compile with -std=2002");
            if (k >= g_ntok || g_tok[k].kind != T_STR || g_tok[k].len != 1) die_at(g_tok[k].line, "PICTURE SYMBOL takes a literal of one character (2023 12.3.7 rule 26)");
            if (lit->len < 1 || lit->len > 31) die_at(lit->line, "the currency string has 1 to 31 characters");
            int nonsp = 0;
            for (int q = 0; q < lit->len; q++) {
                unsigned char c = (unsigned char)lit->s[q];
                if (c != ' ') nonsp = 1;
                if (isdigit(c) || strchr("+-,.*", c)) die_at(lit->line, "the currency string has no digit and none of + - , . * (2023 12.3.7 rule 23)");
            }
            if (!nonsp) die_at(lit->line, "the currency string has at least one character that is not a space (2023 12.3.7 rule 23)");
            memcpy(g_currency_str, lit->s, (size_t)lit->len); g_currency_str[lit->len] = 0; g_currency_len = lit->len;
            lit = &g_tok[k]; with_ps = 1;
        }
        if (lit->hex && g_std >= 2014) die_at(lit->line, "CURRENCY SIGN IS: a hexadecimal literal is not the currency symbol, whose meaning is fixed at compile time (2014; 2023 12.3.7 rule 24)");
        if (lit->len != 1) die_at(lit->line, "CURRENCY SIGN IS: the currency symbol is one character; a longer currency string takes WITH PICTURE SYMBOL (2023 12.3.7 rules 22-23)");
        unsigned char c = (unsigned char)lit->s[0];
        if (isdigit(c) || c == ' ' || strchr("ABCDPRSVXZabcdprsvxz*+-,.;()\"/=", c))
            die_at(lit->line, "CURRENCY SIGN IS '%c': that character has a meaning of its own in a PICTURE (2023 12.3.7 rule %s)", c, with_ps ? "27" : "22");
        if (with_ps && (c == 'E' || c == 'e' || c == 'N' || c == 'n'))
            die_at(lit->line, "PICTURE SYMBOL '%c': E and N have a meaning of their own in a PICTURE (2023 12.3.7 rule 27)", c);
        g_currency = c;
        if (g_currency_len == 1 && g_currency_str[0] == c) g_currency_len = 0;   /* the string is the symbol: as without the phrase */
    }
    int w = 0;
    /* a joined literal drops a token: the positional directives recorded
     * by token index (expand_types) move with the stream, or a >>TURN
     * after a 45296,5 would apply one statement late */
    int d = 0;
    for (int i = 0; i < g_ntok; i++) {
        Tok *t = &g_tok[i];
        while (d < g_ndir && g_dir[d].pos <= i) { if (g_dir[d].pos == i) g_dir[d].pos = w; d++; }
        if (g_currency && t->kind == T_PIC && g_currency != '$')
            for (char *q = t->s; *q; q++) if (toupper((unsigned char)*q) == toupper(g_currency)) *q = '$';
        if (t->kind == T_OP && !strcmp(t->s, ",")) {
            if (g_dp_comma && w > 0 && g_tok[w - 1].kind == T_NUM && i + 1 < g_ntok && g_tok[i + 1].kind == T_NUM && g_tok[i + 1].line == t->line) {
                Tok *a = &g_tok[w - 1], *b = &g_tok[i + 1];
                if (strchr(a->s, '.') || strchr(b->s, '.')) die_at(t->line, "a numeric literal with two decimal points");
                char *joined = xmalloc(strlen(a->s) + strlen(b->s) + 2);
                sprintf(joined, "%s.%s", a->s, b->s);
                a->s = joined; a->len = (int)strlen(joined);
                i++;                                    /* the fraction is consumed */
                continue;
            }
            die_at(t->line, "',' is a separator only when followed by a space");
        }
        if (g_dp_comma && t->kind == T_PIC)
            for (char *q = t->s; *q; q++) { if (*q == '.') *q = ','; else if (*q == ',') *q = '.'; }
        g_tok[w++] = *t;
    }
    while (d < g_ndir) { if (g_dir[d].pos >= g_ntok) g_dir[d].pos = w; d++; }
    g_ntok = w;
}

/* ---- Text manipulation: COPY and REPLACE over text-words (2023 7.2) ----
 *
 * The standard defines COPY and REPLACE on text-words, before anything is
 * a COBOL token (7.2.2.5): a literal with its delimiters is one; '(', ')'
 * and ':' are always separators and text-words of their own; a period,
 * comma or semicolon separates only when a space follows; everything else
 * runs to a separator.  So PIC X(5) is four text-words and ==(5)== matches
 * inside it, and IBM's ==:TAG:== matches the three text-words of :TAG: in
 * :TAG:-REC, the replacement joining the -REC that followed with no space.
 * Each text-word keeps whether a space stood before it on its line; the
 * words are put back into lines with those spaces, and the ordinary
 * tokenizer reads the result, after replacement, as 7.2 orders it.
 *
 * Step 1 incorporates library text (COPY, nested, then its REPLACING);
 * step 3 applies REPLACE statements (7.2.4). */

enum { TW_WORD, TW_LIT, TW_SEP, TW_PERIOD, TW_PDELIM, TW_DIR, TW_RAW };
    /* TW_SEP: a separator comma or semicolon, a space for matching;
     * TW_PDELIM: ==; TW_DIR: a directive line, whole; TW_RAW: EXEC SQL
     * text up to END-EXEC, never matched (docs/esql.md) */
typedef struct {
    char *s; int len;           /* as written */
    int line; const char *file;
    unsigned char kind, dbg, glued;   /* glued: no space before it on its line */
    unsigned char ff;                 /* read in free form (a COPY here starts its library text so) */
} TW;
typedef struct { TW *w; int n, cap; } TWV;

static void twv_push(TWV *v, TW t)
{
    if (v->n == v->cap) { v->cap = v->cap ? 2 * v->cap : 256; v->w = realloc(v->w, (size_t)v->cap * sizeof *v->w); }
    v->w[v->n++] = t;
}

static void tw_die(const TW *t, const char *fmt, const char *a)
{
    g_tok_file = t->file;
    die_at(t->line, fmt, a);
}

static int tw_is(const TW *t, const char *w)
{
    return t->kind == TW_WORD && (int)strlen(w) == t->len && !strncasecmp(t->s, w, (size_t)t->len);
}

/* the prefix letters of a literal that begins at p, 0 when none begins there */
static int lit_prefix(const char *p)
{
    static const char *pf[] = { "nx", "bx", "gx", "x", "n", "b", "z", "g", "u", NULL };
    if (*p == '"' || *p == '\'') return 0;
    for (int k = 0; pf[k]; k++) {
        size_t l = strlen(pf[k]);
        if (!strncasecmp(p, pf[k], l) && (p[l] == '"' || p[l] == '\'')) return (int)l;
    }
    return -1;
}

static void tw_lex(const SrcLine *lines, int nlines, TWV *out)
{
    int sql = 0; char sq = 0;           /* inside EXEC SQL; the quote open there */
    for (int li = 0; li < nlines; li++) {
        const SrcLine *L = &lines[li];
        TW t; memset(&t, 0, sizeof t);
        t.line = L->line; t.file = L->file; t.dbg = (unsigned char)L->dbg; t.ff = (unsigned char)L->ff;
        if (L->dir) { t.kind = TW_DIR; t.s = L->text; t.len = (int)strlen(L->text); twv_push(out, t); continue; }
        const char *p = L->text, *pe = p + strlen(p);
        int glued = 0, run = 0;         /* run: the last word pushed is a run this lexeme may extend */
        while (p < pe) {
            if (sql) {
                /* EXEC SQL text: one opaque piece to END-EXEC or the line's
                 * end, quotes and -- comments respected as take_exec_sql does */
                const char *s = p;
                while (*p) {
                    if (sq) { if (*p == sq) sq = 0; }
                    else if (*p == '\'' || *p == '"') sq = *p;
                    else if (p[0] == '-' && p[1] == '-' && (p == L->text || !sql_wordch((unsigned char)p[-1]))) { p += strlen(p); break; }
                    else if (!strncasecmp(p, "end-exec", 8) && !sql_wordch((unsigned char)p[8]) && (p == L->text || !sql_wordch((unsigned char)p[-1]))) {
                        p += 8; sql = 0; break;
                    }
                    p++;
                }
                t.kind = TW_RAW; t.s = xstrndup(s, (int)(p - s)); t.len = (int)(p - s); t.glued = (unsigned char)glued;
                twv_push(out, t); glued = 1; run = 0;
                continue;
            }
            /* the scanner's lexemes (lex.rl): a literal with its delimiters,
             * '(', ')', ':', == and a separating period, comma or semicolon
             * are text-words of their own; everything else runs together to
             * the next separator (7.2.2.5) */
            Lexeme l;
            lx_next(p, pe, &l);
            if (l.kind == LX_SPACE) { glued = 0; run = 0; p += l.len; continue; }
            if (l.kind == LX_COMMENT) break;                            /* a comment to the end of the line */
            int kind = TW_WORD, own = 1;
            switch (l.kind) {
            case LX_PDELIM: kind = TW_PDELIM; break;
            case LX_LP: case LX_RP: case LX_COLON: break;
            case LX_PERIOD: kind = TW_PERIOD; break;
            case LX_SEP: kind = TW_SEP; break;
            case LX_LIT: kind = TW_LIT; break;
            default: own = 0; break;                                    /* a word, a number, an operator, a tight period or comma, any other byte */
            }
            if (!own && run) {
                /* glued to the run before it: one text-word */
                TW *w = &out->w[out->n - 1];
                char *ns = xmalloc((size_t)w->len + (size_t)l.len + 1);
                memcpy(ns, w->s, (size_t)w->len); memcpy(ns + w->len, l.s, (size_t)l.len); ns[w->len + l.len] = 0;
                free(w->s); w->s = ns; w->len += l.len;
                p += l.len;
                continue;
            }
            t.kind = (unsigned char)kind; t.s = xstrndup(l.s, l.len); t.len = l.len; t.glued = (unsigned char)glued;
            twv_push(out, t); glued = 1; run = !own;
            p += l.len;
            if (kind == TW_WORD && tw_is(&out->w[out->n - 1], "sql")) {
                int k = out->n - 2;
                while (k >= 0 && out->w[k].kind == TW_DIR) k--;
                if (k >= 0 && tw_is(&out->w[k], "exec")) sql = 1;
            }
        }
    }
}

/* the words back into lines: a word glued to the one before joins it with
 * no space, whatever line it came from; otherwise a new line starts where
 * the file, line or debugging flag changes */
static void tw_lines(const TWV *v, SrcLine **out, int *nout)
{
    int cap = 64, n = 0;
    SrcLine *ls = xmalloc((size_t)cap * sizeof *ls);
    size_t bcap = 0, blen = 0; char *buf = NULL;
    int open = 0;
    for (int i = 0; i <= v->n; i++) {
        const TW *t = i < v->n ? &v->w[i] : NULL;
        int join = t && open && t->kind != TW_DIR &&
                   (t->glued || (t->file == ls[n - 1].file && t->line == ls[n - 1].line && t->dbg == ls[n - 1].dbg));
        if (open && !join) { ls[n - 1].text = xstrndup(buf ? buf : "", (int)blen); open = 0; }
        if (!t) break;
        if (!join) {
            if (n == cap) { cap *= 2; ls = realloc(ls, (size_t)cap * sizeof *ls); }
            memset(&ls[n], 0, sizeof ls[n]);
            ls[n].line = t->line; ls[n].file = t->file; ls[n].dbg = t->dbg; ls[n].ff = t->ff;
            n++;
            if (t->kind == TW_DIR) { ls[n - 1].text = xstrndup(t->s, t->len); ls[n - 1].dir = 1; continue; }
            open = 1; blen = 0;
        }
        if (blen + (size_t)t->len + 2 > bcap) { bcap = (blen + (size_t)t->len + 2) * 2; buf = realloc(buf, bcap); }
        if (blen && !t->glued) buf[blen++] = ' ';
        memcpy(buf + blen, t->s, (size_t)t->len); blen += (size_t)t->len;
    }
    free(buf);
    *out = ls; *nout = n;
}

/* two text-words equal for matching (2023 7.2.3.4 rule 9c, 7.2.4.4 rule
 * 8c): words without regard to case; literals by prefix, content with a
 * doubled quote as one, either quotation mark (the rule's two
 * representations; this compiler takes the apostrophe in COBOL 85 too),
 * a hexadecimal literal's digits without regard to case */
static int tw_eq(const TW *a, const TW *b)
{
    if (a->kind != b->kind) return 0;
    if (a->kind == TW_PERIOD) return 1;
    if (a->kind == TW_WORD) return a->len == b->len && !strncasecmp(a->s, b->s, (size_t)a->len);
    if (a->kind != TW_LIT) return 0;
    int pa = lit_prefix(a->s), pb = lit_prefix(b->s);
    if (pa != pb || strncasecmp(a->s, b->s, (size_t)pa)) return 0;
    int hex = memchr(a->s, 'x', (size_t)pa) || memchr(a->s, 'X', (size_t)pa);
    char qa = a->s[pa], qb = b->s[pb];
    const char *x = a->s + pa + 1, *xe = a->s + a->len, *y = b->s + pb + 1, *ye = b->s + b->len;
    if (xe > x && xe[-1] == qa) xe--;
    if (ye > y && ye[-1] == qb) ye--;
    while (x < xe && y < ye) {
        char cx = *x, cy = *y;
        if (cx == qa && x + 1 < xe && x[1] == qa) x++;
        if (cy == qb && y + 1 < ye && y[1] == qb) y++;
        if (cx == qa) cx = '"';
        if (cy == qb) cy = '"';
        if (hex ? tolower((unsigned char)cx) != tolower((unsigned char)cy) : cx != cy) return 0;
        x++; y++;
    }
    return x == xe && y == ye;
}

/* a REPLACING operand or a REPLACE operand */
enum { OP_PSEUDO, OP_LEADING, OP_TRAILING };
typedef struct { int kind; TW *from; int nf; TW *to; int nt; } TWOp;

/* ==pseudo-text== at w[*j]: its words (separators kept) copied aside */
static void tw_pseudo(const TWV *v, int *j, TW **words, int *nw, const TW *stmt, const char *what)
{
    int k = *j + 1, s = k;
    while (k < v->n && v->w[k].kind != TW_PDELIM) {
        if (v->w[k].kind == TW_DIR) tw_die(&v->w[k], "%s: a compiler directive line inside pseudo-text (2023 7.2.3.3 rule 10)", what);
        k++;
    }
    if (k >= v->n) tw_die(stmt, "%s: pseudo-text not closed by ==", what);
    *nw = k - s;
    *words = xmalloc((size_t)(*nw + 1) * sizeof **words);
    memcpy(*words, &v->w[s], (size_t)*nw * sizeof **words);
    *j = k + 1;
}

static int tw_nonsep(const TW *w, int n)
{
    int c = 0;
    for (int k = 0; k < n; k++) if (w[k].kind != TW_SEP) c++;
    return c;
}

/* the operands of COPY ... REPLACING or of a format 1 REPLACE, from w[*j]
 * to the period; 85 also has COPY's identifier, literal and word operands,
 * taken as pseudo-text holding them */
static int tw_operands(const TWV *v, int *j, TWOp **ops, const TW *stmt, const char *what, int is_copy)
{
    int n = 0, cap = 8;
    *ops = xmalloc((size_t)cap * sizeof **ops);
    for (;;) {
        while (*j < v->n && v->w[*j].kind == TW_SEP) (*j)++;
        if (*j >= v->n) tw_die(stmt, "%s needs its period", what);
        if (v->w[*j].kind == TW_PERIOD) break;
        TWOp op; memset(&op, 0, sizeof op);
        if (tw_is(&v->w[*j], "leading") || tw_is(&v->w[*j], "trailing")) {
            if (g_std < 2002) tw_die(&v->w[*j], "%s LEADING and TRAILING (partial words) are COBOL 2002; compile with -std=2002", what);
            op.kind = tw_is(&v->w[*j], "leading") ? OP_LEADING : OP_TRAILING;
            (*j)++;
        }
        for (int side = 0; side < 2; side++) {
            if (side == 1) {
                while (*j < v->n && v->w[*j].kind == TW_SEP) (*j)++;
                if (!(*j < v->n && tw_is(&v->w[*j], "by"))) tw_die(stmt, "%s: expected BY", what);
                (*j)++;
            }
            TW *w; int nw;
            if (*j < v->n && v->w[*j].kind == TW_PDELIM) tw_pseudo(v, j, &w, &nw, stmt, what);
            else if (op.kind != OP_PSEUDO || !is_copy)
                tw_die(stmt, "%s takes ==pseudo-text== BY ==pseudo-text==", what);
            else {
                const TW *a = *j < v->n ? &v->w[*j] : stmt;
                if (a->kind != TW_WORD && a->kind != TW_LIT)
                    tw_die(stmt, "%s: expected a word, a literal or ==pseudo-text==", what);
                bp(BP_R4_COPY_REPLACING_WORD, a->line);      /* removed by 2023 (Annex E.2 item 1) */
                int s = (*j)++;
                if (a->kind == TW_WORD) {
                    /* an identifier: qualifiers and a subscript list belong to it */
                    while (*j + 1 < v->n && (tw_is(&v->w[*j], "of") || tw_is(&v->w[*j], "in")) && v->w[*j + 1].kind == TW_WORD) *j += 2;
                    if (*j < v->n && tw_is(&v->w[*j], "(")) {
                        int d = 0;
                        do {
                            if (*j >= v->n) tw_die(stmt, "%s: unbalanced parentheses", what);
                            if (tw_is(&v->w[*j], "(")) d++; else if (tw_is(&v->w[*j], ")")) d--;
                            (*j)++;
                        } while (d > 0);
                    }
                }
                nw = *j - s;
                w = xmalloc((size_t)nw * sizeof *w);
                memcpy(w, &v->w[s], (size_t)nw * sizeof *w);
            }
            if (side == 0) { op.from = w; op.nf = nw; } else { op.to = w; op.nt = nw; }
        }
        if (!tw_nonsep(op.from, op.nf))
            tw_die(stmt, "%s: the text to replace is empty, or only commas and semicolons (2023 7.2.3.3 rule 6, 7.2.4.3 rule 3)", what);
        if (op.kind != OP_PSEUDO) {
            int nf = tw_nonsep(op.from, op.nf), nt = tw_nonsep(op.to, op.nt);
            if (nf != 1 || nt > 1)
                tw_die(stmt, "%s LEADING or TRAILING: partial-word-1 is one text-word, partial-word-2 one or none (2023 7.2.3.3 rules 11-12)", what);
            for (int k = 0; k < op.nf; k++) if (op.from[k].kind == TW_LIT) tw_die(stmt, "%s LEADING or TRAILING: a partial word is not a literal (2023 7.2.3.3 rule 13)", what);
            for (int k = 0; k < op.nt; k++) if (op.to[k].kind == TW_LIT) tw_die(stmt, "%s LEADING or TRAILING: a partial word is not a literal (2023 7.2.3.3 rule 13)", what);
        }
        if (n == cap) { cap *= 2; *ops = realloc(*ops, (size_t)cap * sizeof **ops); }
        (*ops)[n++] = op;
    }
    return n;
}

static const TW *tw_first(const TW *w, int n) { for (int k = 0; k < n; k++) if (w[k].kind != TW_SEP) return &w[k]; return NULL; }

/* does op match the text at w[i]?  *end: the word after the match */
static int tw_match(const TW *w, int n, int i, const TWOp *op, int *end)
{
    const TW *t = &w[i];
    if (op->kind != OP_PSEUDO) {
        const TW *f = tw_first(op->from, op->nf);
        if (t->kind != TW_WORD || t->len < f->len) return 0;
        const char *at = op->kind == OP_LEADING ? t->s : t->s + t->len - f->len;
        if (strncasecmp(at, f->s, (size_t)f->len)) return 0;
        *end = i + 1;
        return 1;
    }
    int k = i;
    for (int m = 0; m < op->nf; m++) {
        if (op->from[m].kind == TW_SEP) continue;
        while (k < n && w[k].kind == TW_SEP) k++;
        if (k >= n || !tw_eq(&w[k], &op->from[m])) return 0;
        k++;
    }
    *end = k;
    return 1;
}

/* the replacement for a match of op at w[i..end), onto out; the spacing
 * of the matched text is kept: the first word takes the space (or none)
 * before the match, and when nothing replaces it, the word after joins
 * the one before only if neither side had a space (rules 9f and 11) */
static void tw_emit(TWV *out, TW *w, int n, int i, int end, const TWOp *op)
{
    const TW *m = &w[i];
    int emitted = 0;
    if (op->kind == OP_PSEUDO) {
        for (int k = 0; k < op->nt; k++) {
            TW t = op->to[k];
            t.line = m->line; t.file = m->file; t.dbg = m->dbg; t.ff = m->ff;
            if (!emitted) t.glued = m->glued;
            twv_push(out, t); emitted = 1;
        }
    } else {
        const TW *f = tw_first(op->from, op->nf), *r = tw_first(op->to, op->nt);
        int rl = r ? r->len : 0, keep = m->len - f->len;
        if (keep + rl > 0) {
            TW t = *m;
            t.s = xmalloc((size_t)(keep + rl + 1)); t.len = keep + rl;
            if (op->kind == OP_LEADING) { if (rl) memcpy(t.s, r->s, (size_t)rl); memcpy(t.s + rl, m->s + f->len, (size_t)keep); }
            else { memcpy(t.s, m->s, (size_t)keep); if (rl) memcpy(t.s + keep, r->s, (size_t)rl); }
            t.s[t.len] = 0;
            twv_push(out, t); emitted = 1;
        }
    }
    if (!emitted && end < n) w[end].glued = (unsigned char)(w[end].glued && m->glued);
}

/* rule 9 of COPY and rule 8 of REPLACE: from the leftmost word, each
 * operand in turn; a match is replaced and the scan goes on after it,
 * the replacement never rescanned */
static void tw_apply(TWV *out, TW *w, int n, const TWOp *ops, int nops)
{
    for (int i = 0; i < n; ) {
        int end = 0, hit = -1;
        if (w[i].kind != TW_SEP && w[i].kind != TW_DIR && w[i].kind != TW_RAW)
            for (int q = 0; q < nops && hit < 0; q++) if (tw_match(w, n, i, &ops[q], &end)) hit = q;
        if (hit < 0) { twv_push(out, w[i]); i++; continue; }
        tw_emit(out, w, n, i, end, &ops[hit]);
        i = end;
    }
}

/* Step 1: every COPY statement replaced by its library text, that text's
 * own COPY statements first (nested; at least five levels, 7.2.3.4 rule
 * 12), then the REPLACING phrase over the whole of it */
static const char *g_copy_stack[16]; static int g_copy_depth;

/* ---- conditional compilation: >>DEFINE, >>IF, >>EVALUATE (2023 7.3.5-8,
 * 7.3.11, 7.3.13, 7.3.16) ----------------------------------------------
 *
 * Done in step 1, as the text words go by with their library text
 * expanded in place: a directive applies to the text that follows it
 * (7.3.4 rule 5), so a COPY in an omitted branch is never read, and a
 * compilation variable defined in library text is known after it.  The
 * conditional directives leave nothing behind; the omitted text is
 * dropped.  Compile-time arithmetic is done in long double and the
 * result truncated to its integer part (7.3.6.3 rules 2-3; documented
 * in docs/conformance/directives.md). */
typedef struct { char kind; long double n; char *s; int len; } CVal;   /* kind: 'n' numeric, 'a' alphanumeric, 'b' boolean */
typedef struct { char name[64]; int defined; CVal v; } CVar;
static CVar g_cvar[256]; static int g_ncvar;
static const char *g_cv_param[64]; static int g_ncv_param;   /* -D name=value: the values PARAMETER takes */

static CVar *cvar_find(const char *nm)
{
    for (int i = 0; i < g_ncvar; i++) if (!strcasecmp(g_cvar[i].name, nm)) return &g_cvar[i];
    return NULL;
}

typedef struct { int parent, active, taken, eval, else_seen, when_seen; CVal subj; int truth; int depth_at; } CondLevel;
static CondLevel g_cond[64]; static int g_ncond;
static int cond_active(void) { return !g_ncond || g_cond[g_ncond - 1].active; }
static int g_in_unit;       /* the text words are inside a compilation unit: IDENTIFICATION DIVISION seen, its END not yet */
static int g_units_seen;    /* a compilation unit has begun: >>COBOL-WORDS comes before the first (2023 7.3.10.3 rule 1) */
static int g_propagate_dir; /* >>PROPAGATE ON in force (7.3.21.4 rules 1, 3-4) */

/* a directive's text as tokens */
typedef struct { char t; char *s; int len; } CTok;      /* t: 'w' word, 'n' number, 'a' alnum literal, 'b' boolean literal, 'o' operator */
static CTok g_ct[128]; static int g_nct, g_cp; static const TW *g_ctw;

static void cdie(const char *fmt, const char *a) { tw_die(g_ctw, fmt, a); }
/* a FLAG-14 warning at the text manipulation stage, at the directive under way */
static void f14_text(int opt, const char *what)
{
    if (!g_f14_text[opt]) return;
    unsigned char save[NF14]; memcpy(save, g_f14, sizeof save); memcpy(g_f14, g_f14_text, sizeof g_f14);
    char id[64]; int k = 0;
    for (const char *p = g_f14_names[opt]; *p && k < 63; p++) id[k++] = (char)toupper((unsigned char)*p);
    id[k] = 0;
    fprintf(stderr, "%s:%d: warning: [F14-%s] %s (2023 7.3.15.4)\n", g_ctw && g_ctw->file ? g_ctw->file : "", g_ctw ? g_ctw->line : 0, id, what);
    memcpy(g_f14, save, sizeof g_f14);
}

static void ctok(const char *p)
{
    g_nct = 0;
    while (*p) {
        if (*p == ' ' || *p == '\t') { p++; continue; }
        if (p[0] == '*' && p[1] == '>') break;
        if (g_nct == 128) cdie("a compiler directive too long to read%s", "");
        CTok *t = &g_ct[g_nct++];
        const char *s = p;
        if ((*p == 'b' || *p == 'B') && (p[1] == '"' || p[1] == '\'')) {
            char q = p[1]; p += 2; const char *b = p;
            while (*p && *p != q) p++;
            if (!*p) cdie("an unclosed literal in a compiler directive%s", "");
            t->t = 'b'; t->s = xstrndup(b, (int)(p - b)); t->len = (int)(p - b); p++;
            for (int i = 0; i < t->len; i++) if (t->s[i] != '0' && t->s[i] != '1') cdie("a boolean literal holds only 0 and 1%s", "");
            continue;
        }
        if (*p == '"' || *p == '\'' || ((*p == 'x' || *p == 'X' || *p == 'n' || *p == 'N') && (p[1] == '"' || p[1] == '\''))) {
            if (*p != '"' && *p != '\'') cdie("a hexadecimal or national literal in a compiler directive is not implemented%s", "");
            char q = *p++; char buf[256]; int k = 0;
            for (;;) {
                if (!*p) cdie("an unclosed literal in a compiler directive%s", "");
                if (*p == q) { if (p[1] == q) { if (k < 255) buf[k++] = q; p += 2; continue; } p++; break; }
                if (k < 255) buf[k++] = *p;
                p++;
            }
            t->t = 'a'; t->s = xstrndup(buf, k); t->len = k;
            continue;
        }
        if (isdigit((unsigned char)*p) || (*p == '.' && isdigit((unsigned char)p[1]))) {
            while (isdigit((unsigned char)*p) || (*p == '.' && isdigit((unsigned char)p[1]))) p++;
            t->t = 'n'; t->s = xstrndup(s, (int)(p - s)); t->len = (int)(p - s);
            continue;
        }
        if (isalpha((unsigned char)*p)) {
            while (isalnum((unsigned char)*p) || *p == '-' || *p == '_') p++;
            while (p > s && p[-1] == '-') p--;
            t->t = 'w'; t->s = xstrndup(s, (int)(p - s)); t->len = (int)(p - s);
            continue;
        }
        if ((p[0] == '<' && (p[1] == '=' || p[1] == '>')) || (p[0] == '>' && p[1] == '=')) p += 2;
        else if (strchr("+-*/()=<>", *p)) p++;
        else cdie("'%c' is not taken in a compiler directive", (char[]){ *p, 0 });
        t->t = 'o'; t->s = xstrndup(s, (int)(p - s)); t->len = (int)(p - s);
    }
}
static CTok *ccur(void) { static CTok eof = { 0, "", 0 }; return g_cp < g_nct ? &g_ct[g_cp] : &eof; }
static int cword(const char *w) { CTok *t = ccur(); return t->t == 'w' && !strcasecmp(t->s, w); }
static int cop(const char *o) { CTok *t = ccur(); return t->t == 'o' && !strcmp(t->s, o); }
static int caccept(const char *w) { if (cword(w)) { g_cp++; return 1; } return 0; }

/* a compile-time arithmetic expression (7.3.6): numeric literals and
 * numeric compilation variables, + - * / and parentheses, no ** */
static long double c_arith(void);
static int c_operand(CVal *v);
static long double c_prim(void)
{
    if (cop("(")) { g_cp++; long double x = c_arith(); if (!cop(")")) cdie("expected ')' in a compile-time expression%s", ""); g_cp++; return x; }
    if (cop("+")) { g_cp++; return c_prim(); }
    if (cop("-")) { g_cp++; return -c_prim(); }
    CVal v;
    if (!c_operand(&v) || v.kind != 'n') cdie("a compile-time arithmetic expression takes numeric literals and numeric compilation variables (2023 7.3.6.2 rule 1)%s", "");
    return v.n;
}
static long double c_term(void)
{
    long double x = c_prim();
    for (;;) {
        if (cop("*")) { g_cp++; if (cop("*")) cdie("no exponentiation in a compile-time expression (2023 7.3.6.2 rule 1a)%s", ""); x *= c_prim(); }
        else if (cop("/")) {
            g_cp++; long double y = c_prim(); if (y == 0) cdie("a division by zero in a compile-time expression (2023 7.3.6.2 rule 1c)%s", "");
            if (g_f14_text[F14_ARITH]) f14_text(F14_ARITH, "a compile-time division: the arithmetic mode and its intermediate results are the implementor's in 2023 (E.2 item 6)");
            x /= y;
        }
        else return x;
    }
}
static long double c_arith(void)
{
    long double x = c_term();
    for (;;) {
        if (cop("+")) { g_cp++; x += c_term(); }
        else if (cop("-")) { g_cp++; x -= c_term(); }
        else return x;
    }
}
/* one literal or compilation variable, without consuming an operator */
static int c_operand(CVal *v)
{
    CTok *t = ccur();
    memset(v, 0, sizeof *v);
    if (t->t == 'n') { v->kind = 'n'; v->n = strtold(t->s, NULL); g_cp++; return 1; }
    if (t->t == 'a') { v->kind = 'a'; v->s = t->s; v->len = t->len; g_cp++; return 1; }
    if (t->t == 'b') { v->kind = 'b'; v->s = t->s; v->len = t->len; g_cp++; return 1; }
    if (t->t == 'w') {
        CVar *c = cvar_find(t->s);
        if (!c) cdie("'%s' is not a compilation variable (no >>DEFINE)", t->s);
        if (!c->defined) cdie("'%s' is not defined here (a >>DEFINE ... OFF; 2023 7.3.11.4 rule 2)", t->s);
        *v = c->v; g_cp++; return 1;
    }
    return 0;
}
/* a value: an arithmetic expression, or a literal or variable of another category */
static void c_value(CVal *v)
{
    int save = g_cp;
    CVal o;
    if (c_operand(&o) && o.kind != 'n') { *v = o; return; }
    g_cp = save;
    v->kind = 'n'; v->n = c_arith(); v->s = NULL; v->len = 0;
}
static int c_eq(const CVal *a, const CVal *b)
{
    if (a->kind != b->kind) cdie("the operands of a compile-time comparison are of one category (2023 7.3.8.2 rule 1a)%s", "");
    if (a->kind == 'n') return a->n == b->n;
    return a->len == b->len && !memcmp(a->s, b->s, (size_t)a->len);     /* by encoding, unequal lengths unequal (7.3.8.3 rule 2) */
}
/* relational operator: 0 =, 1 >, 2 >=, 3 <, 4 <=, 5 <>; -1 none */
static int c_relop(int *neg)
{
    *neg = 0;
    int save = g_cp;
    caccept("is");
    if (caccept("not")) *neg = 1;
    if (cop("=")) { g_cp++; return 0; }
    if (cop("<>")) { g_cp++; return 5; }
    if (cop(">=")) { g_cp++; return 2; }
    if (cop("<=")) { g_cp++; return 4; }
    if (cop(">")) { g_cp++; return 1; }
    if (cop("<")) { g_cp++; return 3; }
    if (caccept("equal")) { caccept("to"); return 0; }
    if (caccept("greater")) { caccept("than"); if (caccept("or")) { if (!caccept("equal")) cdie("expected EQUAL%s", ""); caccept("to"); return 2; } return 1; }
    if (caccept("less")) { caccept("than"); if (caccept("or")) { if (!caccept("equal")) cdie("expected EQUAL%s", ""); caccept("to"); return 4; } return 3; }
    g_cp = save; *neg = 0;
    return -1;
}
static int c_cond(void);
static int c_simple(void)
{
    if (caccept("not")) return !c_simple();
    if (cop("(")) {
        /* a parenthesized condition, or an arithmetic expression that a relation compares */
        int save = g_cp;
        g_cp++;
        int ok = 1, r = 0;
        /* try the condition reading: it must close and not be followed by an operator */
        int depth = 1, k = g_cp, has_rel = 0;
        for (; k < g_nct && depth; k++) {
            if (g_ct[k].t == 'o' && !strcmp(g_ct[k].s, "(")) depth++;
            else if (g_ct[k].t == 'o' && !strcmp(g_ct[k].s, ")")) depth--;
            else if (depth == 1 && ((g_ct[k].t == 'o' && strchr("=<>", g_ct[k].s[0])) ||
                     (g_ct[k].t == 'w' && (!strcasecmp(g_ct[k].s, "and") || !strcasecmp(g_ct[k].s, "or") || !strcasecmp(g_ct[k].s, "defined") ||
                                           !strcasecmp(g_ct[k].s, "equal") || !strcasecmp(g_ct[k].s, "greater") || !strcasecmp(g_ct[k].s, "less")))))
                has_rel = 1;
        }
        if (has_rel) {
            r = c_cond();
            if (!cop(")")) cdie("expected ')' in a compile-time condition%s", "");
            g_cp++;
            (void)ok;
            return r;
        }
        g_cp = save;                                    /* (arithmetic) relop ... */
    }
    if (ccur()->t == 'w' && g_cp + 1 < g_nct) {
        int save = g_cp; g_cp++;
        int neg = 0; caccept("is"); if (caccept("not")) neg = 1;
        if (caccept("defined")) {
            CVar *c = cvar_find(g_ct[save].s);
            int d = c && c->defined;
            return neg ? !d : d;
        }
        g_cp = save;
    }
    CVal a; c_value(&a);
    int neg, op = c_relop(&neg);
    if (op < 0) {
        if (a.kind == 'b') { int any = 0; for (int i = 0; i < a.len; i++) if (a.s[i] == '1') any = 1; return any; }   /* a boolean condition (8.8.4.3) */
        cdie("expected a relational operator in a compile-time condition%s", "");
    }
    CVal b; c_value(&b);
    if (a.kind != 'n' && op != 0 && op != 5) cdie("only EQUAL and NOT EQUAL compare literals that are not numeric (2023 7.3.8.2 rule 1a2)%s", "");
    int r;
    if (a.kind == 'n' && b.kind == 'n')
        r = op == 0 ? a.n == b.n : op == 1 ? a.n > b.n : op == 2 ? a.n >= b.n : op == 3 ? a.n < b.n : op == 4 ? a.n <= b.n : a.n != b.n;
    else { r = c_eq(&a, &b); if (op == 5) r = !r; }
    return neg ? !r : r;
}
static int c_and(void) { int r = c_simple(); while (caccept("and")) { int s2 = c_simple(); r = r && s2; } return r; }
static int c_cond(void) { int r = c_and(); while (caccept("or")) { int s2 = c_and(); r = r || s2; } return r; }
static void c_end(const char *what) { if (g_cp < g_nct) cdie("unexpected '%s' in the directive", ccur()->s), (void)what; }

/* a CVal as the text a constant entry reads */
static TW cval_tw(const TW *at, const CVal *v)
{
    TW t = *at; char buf[300];
    if (v->kind == 'n') {
        long double x = v->n; char *e;
        snprintf(buf, sizeof buf, "%.18Lg", x);
        if ((e = strchr(buf, 'e')) != NULL) cdie("a compilation variable's value out of range for a literal%s", "");
        t.kind = TW_WORD;
    } else if (v->kind == 'a') {
        int k = 0; buf[k++] = '"';
        for (int i = 0; i < v->len && k < 290; i++) { if (v->s[i] == '"') buf[k++] = '"'; buf[k++] = v->s[i]; }
        buf[k++] = '"'; buf[k] = 0; t.kind = TW_LIT;
    } else { snprintf(buf, sizeof buf, "b\"%.*s\"", v->len, v->s); t.kind = TW_LIT; }
    t.s = xstrndup(buf, (int)strlen(buf)); t.len = (int)strlen(buf);
    return t;
}

static int cv_param(const char *nm, CVal *v)
{
    for (int i = 0; i < g_ncv_param; i++) {
        const char *e = strchr(g_cv_param[i], '=');
        size_t l = e ? (size_t)(e - g_cv_param[i]) : strlen(g_cv_param[i]);
        if (strlen(nm) != l || strncasecmp(nm, g_cv_param[i], l)) continue;
        const char *val = e ? e + 1 : "1";
        memset(v, 0, sizeof *v);
        int num = *val != 0;
        for (const char *q = val; *q; q++) if (!isdigit((unsigned char)*q) && !(*q == '.' && q > val) && !(q == val && (*q == '-' || *q == '+'))) num = 0;
        if (num) { v->kind = 'n'; v->n = strtold(val, NULL); }
        else { v->kind = 'a'; v->s = xstrndup(val, (int)strlen(val)); v->len = (int)strlen(val); }
        return 1;
    }
    return 0;
}

/* >>PUSH / >>POP (2023 7.3.22, 7.3.20) at the text manipulation stage: the
 * states saved -- DEFINE (the whole table), PROPAGATE, COBOL-WORDS; the
 * positional directives' (TURN, REF-MOD-ZERO-LENGTH, FLAG-14) are the
 * parser's (control.h apply_turn), SOURCE the reader's */
typedef struct { CVar *cv; int ncv; } CvarSave;
static CvarSave g_push_cv[64]; static int g_npush_cv;
static int g_push_prop[64], g_npush_prop;
static struct { CobolWord *cw; int ncw; } g_push_cw[64]; static int g_npush_cw;
static void push_dir_text(const char *name, int all)
{
    if (all || !strcasecmp(name, "define")) {
        if (g_npush_cv == 64) cdie(">>PUSH DEFINE nests deeper than 64%s", "");
        CvarSave *s = &g_push_cv[g_npush_cv++];
        s->ncv = g_ncvar; s->cv = xmalloc((size_t)(g_ncvar ? g_ncvar : 1) * sizeof *s->cv);
        memcpy(s->cv, g_cvar, (size_t)g_ncvar * sizeof *s->cv);
    }
    if (all || !strcasecmp(name, "propagate")) { if (g_npush_prop == 64) cdie(">>PUSH PROPAGATE nests deeper than 64%s", ""); g_push_prop[g_npush_prop++] = g_propagate_dir; }
    if (all || !strcasecmp(name, "cobol-words")) {
        if (g_npush_cw == 64) cdie(">>PUSH COBOL-WORDS nests deeper than 64%s", "");
        g_push_cw[g_npush_cw].ncw = g_ncw; g_push_cw[g_npush_cw].cw = xmalloc((size_t)(g_ncw ? g_ncw : 1) * sizeof *g_cw);
        memcpy(g_push_cw[g_npush_cw].cw, g_cw, (size_t)g_ncw * sizeof *g_cw); g_npush_cw++;
    }
}
/* 1 when something was restored; a POP with nothing pushed is unsuccessful
 * and warned of (7.3.20.4 rule 2) */
static int pop_dir_text(const char *name, int all)
{
    int did = 0;
    if (all || !strcasecmp(name, "define")) {
        if (g_npush_cv) { CvarSave *s = &g_push_cv[--g_npush_cv]; g_ncvar = s->ncv; memcpy(g_cvar, s->cv, (size_t)s->ncv * sizeof *s->cv); free(s->cv); did = 1; }
    }
    if (all || !strcasecmp(name, "propagate")) { if (g_npush_prop) { g_propagate_dir = g_push_prop[--g_npush_prop]; did = 1; } }
    if (all || !strcasecmp(name, "cobol-words")) {
        if (g_npush_cw) { g_npush_cw--; g_ncw = g_push_cw[g_npush_cw].ncw; memcpy(g_cw, g_push_cw[g_npush_cw].cw, (size_t)g_ncw * sizeof *g_cw); free(g_push_cw[g_npush_cw].cw); did = 1; }
    }
    return did;
}
/* the directives PUSH and POP may name (7.3.20.3 rule 1, 7.3.22.3 rule 1):
 * -1 unknown, 0 a text-stage or stateless one, 1 one the parser applies */
static int push_dir_kind(const char *name)
{
    static const char *const text[] = { "define", "propagate", "cobol-words", "source", "call-convention", "leap-second", "listing", "display", "imp", NULL };
    static const char *const pos[] = { "turn", "ref-mod-zero-length", "flag-14", NULL };
    for (int i = 0; text[i]; i++) if (!strcasecmp(name, text[i])) return 0;
    for (int i = 0; pos[i]; i++) if (!strcasecmp(name, pos[i])) return 1;
    return -1;
}
/* a directive word on a TW_DIR line: 1 when it was a conditional one (or
 * omitted), and is gone; 0 when it is to be kept (>>TURN) */
static int cond_directive(const TW *t)
{
    g_ctw = t;
    char *txt = xstrndup(t->s, t->len);
    {   /* >>PAGE comment-text: not checked syntactically (7.3.19.3 rule 2) */
        const char *q = txt; while (*q == ' ' || *q == '\t') q++;
        if (!strncasecmp(q, "page", 4) && (!q[4] || q[4] == ' ' || q[4] == '\t')) return 1;
    }
    ctok(txt);
    g_cp = 0;
    if (!g_nct) cdie("an empty compiler directive%s", "");
    CTok *k0 = &g_ct[0];
    const char *w = k0->t == 'w' ? k0->s : "";
    int act = cond_active();
    if (!strcasecmp(w, "if")) {
        g_cp = 1;
        CondLevel *L = &g_cond[g_ncond];
        if (g_ncond == 64) cdie(">>IF nests deeper than 64%s", "");
        memset(L, 0, sizeof *L); L->parent = act; L->eval = 0; L->depth_at = g_copy_depth;
        int r = act ? c_cond() : 0;
        if (act) c_end("IF");
        L->taken = r; L->active = act && r;
        g_ncond++;
        return 1;
    }
    if (!strcasecmp(w, "else")) {
        if (!g_ncond || g_cond[g_ncond - 1].eval) cdie(">>ELSE without its >>IF%s", "");
        CondLevel *L = &g_cond[g_ncond - 1];
        if (L->else_seen) cdie("two >>ELSE in one >>IF%s", "");
        if (L->depth_at != g_copy_depth) cdie("the phrases of an >>IF are all in one library text or all in source text (2023 7.3.16.3 rule 7)%s", "");
        if (g_nct > 1) { g_cp = 1; c_end("ELSE"); }
        L->else_seen = 1; L->active = L->parent && !L->taken;
        return 1;
    }
    if (!strcasecmp(w, "end-if")) {
        if (!g_ncond || g_cond[g_ncond - 1].eval) cdie(">>END-IF without its >>IF%s", "");
        if (g_cond[g_ncond - 1].depth_at != g_copy_depth) cdie("the phrases of an >>IF are all in one library text or all in source text (2023 7.3.16.3 rule 7)%s", "");
        if (g_nct > 1) { g_cp = 1; c_end("END-IF"); }
        g_ncond--;
        return 1;
    }
    if (!strcasecmp(w, "evaluate")) {
        g_cp = 1;
        if (g_ncond == 64) cdie(">>EVALUATE nests deeper than 64%s", "");
        CondLevel *L = &g_cond[g_ncond];
        memset(L, 0, sizeof *L); L->parent = act; L->eval = 1; L->depth_at = g_copy_depth;
        if (act) {
            if (cword("true") && g_nct == 2) { L->truth = 1; g_cp++; }
            else c_value(&L->subj);
            c_end("EVALUATE");
        }
        L->active = 0;                          /* nothing before the first >>WHEN */
        g_ncond++;
        return 1;
    }
    if (!strcasecmp(w, "when")) {
        if (!g_ncond || !g_cond[g_ncond - 1].eval) cdie(">>WHEN without its >>EVALUATE%s", "");
        CondLevel *L = &g_cond[g_ncond - 1];
        if (L->depth_at != g_copy_depth) cdie("the phrases of an >>EVALUATE are all in one library text or all in source text (2023 7.3.13.3 rule 9)%s", "");
        if (L->else_seen) cdie(">>WHEN after >>WHEN OTHER%s", "");
        g_cp = 1;
        if (cword("other") && g_nct == 2) {
            L->else_seen = 1; L->active = L->parent && !L->taken; if (L->active) L->taken = 1;
            if (g_f14_text[F14_EVALUATE] && L->when_seen) f14_text(F14_EVALUATE, ">>EVALUATE with both a >>WHEN and a >>WHEN OTHER");
            return 1;
        }
        L->when_seen = 1;
        int r = 0;
        if (L->parent && !L->taken) {
            if (L->truth) r = c_cond();
            else {
                CVal a; c_value(&a);
                if (caccept("through") || caccept("thru")) {
                    CVal b; c_value(&b);
                    if (a.kind != 'n' || b.kind != 'n' || L->subj.kind != 'n') cdie("THROUGH takes numeric operands (2023 7.3.13.3 rule 12)%s", "");
                    r = L->subj.n >= a.n && L->subj.n <= b.n;
                } else r = c_eq(&L->subj, &a);
            }
            c_end("WHEN");
        }
        L->active = r;
        if (r) L->taken = 1;
        return 1;
    }
    if (!strcasecmp(w, "end-evaluate")) {
        if (!g_ncond || !g_cond[g_ncond - 1].eval) cdie(">>END-EVALUATE without its >>EVALUATE%s", "");
        if (g_cond[g_ncond - 1].depth_at != g_copy_depth) cdie("the phrases of an >>EVALUATE are all in one library text or all in source text (2023 7.3.13.3 rule 9)%s", "");
        g_ncond--;
        return 1;
    }
    if (!act) return 1;                         /* any other directive in omitted text is omitted */
    if (!strcasecmp(w, "define")) {
        g_cp = 1;
        if (ccur()->t != 'w') cdie(">>DEFINE needs a compilation-variable name%s", "");
        const char *nm = ccur()->s; g_cp++;
        static const char *dirw[] = { "define", "if", "else", "end-if", "evaluate", "when", "end-evaluate", "turn", "source",
                                      "as", "off", "override", "parameter", "defined", "true", "false", "other", NULL };
        /* the compiler-directive words 2023 added (E.2 item 5), under -std=2023 */
        static const char *dirw2023[] = { "cobol-words", "display", "flag-14", "i-o-status-04", "num-ed-zero-fig-constant", "pop", "push", "ref-mod-zero-length", "upon", NULL };
        for (int i = 0; dirw[i]; i++) if (!strcasecmp(nm, dirw[i])) cdie("'%s' is a compiler-directive word, not a compilation variable (2023 7.3.11.3 rule 1)", nm);
        if (g_std >= 2023) for (int i = 0; dirw2023[i]; i++) if (!strcasecmp(nm, dirw2023[i])) cdie("'%s' is a compiler-directive word of COBOL 2023, not a compilation variable (2023 7.3.11.3 rule 1; E.2 item 5)", nm);
        CVar *c = cvar_find(nm);
        caccept("as");
        if (caccept("off")) { c_end("DEFINE"); if (c) c->defined = 0; return 1; }
        CVal v; memset(&v, 0, sizeof v);
        int have = 1;
        if (caccept("parameter")) have = cv_param(nm, &v);
        else {
            int single = g_nct - g_cp == 1 || (g_nct - g_cp == 2 && cword("override"));
            if (single && ccur()->t == 'n') { v.kind = 'n'; v.n = strtold(ccur()->s, NULL); g_cp++; }   /* one numeric literal: a literal (rule 5) */
            else { c_value(&v); if (v.kind == 'n') { if (v.n > 9e18L || v.n < -9e18L) cdie("a compile-time result past 18 digits%s", ""); v.n = (long double)(long long)v.n; } }     /* an expression: its integer part (7.3.6.3 rule 3) */
        }
        int ovr = caccept("override");
        c_end("DEFINE");
        if (!c) {
            if (g_ncvar == 256) cdie("more than 256 compilation variables%s", "");
            c = &g_cvar[g_ncvar++]; memset(c, 0, sizeof *c);
            snprintf(c->name, sizeof c->name, "%s", nm);
        } else if (c->defined && !ovr && have) {
            CVal old = c->v;
            if (old.kind != v.kind || !c_eq(&old, &v))
                cdie("'%s' is defined already, with another value: write OFF first, or OVERRIDE (2023 7.3.11.3 rule 2)", nm);
        }
        if (have) { c->v = v; c->defined = 1; } else c->defined = 0;   /* PARAMETER with no value: not defined (rule 4) */
        return 1;
    }
    if (!strcasecmp(w, "listing")) {
        /* no listing is produced, so the directive has no effect (7.3.18.3 rule 1) */
        g_cp = 1;
        if (!caccept("on")) caccept("off");
        c_end("LISTING");
        return 1;
    }
    if (!strcasecmp(w, "page")) return 1;       /* comment-text, unchecked; no listing (7.3.19) */
    if (!strcasecmp(w, "leap-second")) {
        /* the run-time clock reports POSIX time, which has no leap second:
         * a seconds value is never above 59 either way (7.3.17.4 rules 2-7) */
        g_cp = 1;
        if (!caccept("on") && !caccept("off")) cdie(">>LEAP-SECOND takes ON or OFF (2023 7.3.17.2)%s", "");
        c_end("LEAP-SECOND");
        if (g_in_unit) cdie(">>LEAP-SECOND is written outside a compilation unit: before its IDENTIFICATION DIVISION, or after its END PROGRAM (2023 7.3.17.3 rule 1)%s", "");
        return 1;
    }
    if (!strcasecmp(w, "call-convention")) {
        /* COBOL, the default (7.3.9.3 rule 1), is the one convention here */
        g_cp = 1;
        if (!caccept("cobol")) cdie(ccur()->t ? ">>CALL-CONVENTION %s: the one call convention here is COBOL (2023 7.3.9.3 rule 2b leaves the others to the implementor)"
                                               : ">>CALL-CONVENTION needs COBOL or a call-convention name%s", ccur()->t ? ccur()->s : "");
        c_end("CALL-CONVENTION");
        return 1;
    }
    if (!strcasecmp(w, "propagate")) {
        g_cp = 1;
        int on = caccept("on");
        if (!on && !caccept("off")) cdie(">>PROPAGATE takes ON or OFF (2023 7.3.21.2)%s", "");
        c_end("PROPAGATE");
        if (g_in_unit) cdie(">>PROPAGATE is written outside a compilation unit (2023 7.3.21.3 rule 1)%s", "");
        g_propagate_dir = on;
        return 1;
    }
    if (!strcasecmp(w, "turn")) return 0;
    if (!strcasecmp(w, "display")) {
        /* >>DISPLAY operand ... [UPON device | LISTING] (7.3.12): no listing
         * is produced, so the compile-time device is the standard error,
         * one line, the operands in order (rules 1, 2, 4); PARAMETER name
         * is the variable's value from -D, nothing when it has none (rule 3) */
        g_cp = 1;
        char out[1024]; int o = 0; int any = 0;
        while (g_cp < g_nct && !cword("upon")) {
            CVal v; int have = 1;
            if (caccept("parameter")) {
                if (ccur()->t != 'w') cdie(">>DISPLAY PARAMETER needs a compilation-variable name%s", "");
                have = cv_param(ccur()->s, &v); g_cp++;
            } else c_value(&v);
            if (!have) continue;
            any = 1;
            if (v.kind == 'n') o += snprintf(out + o, sizeof out - (size_t)o, "%.18Lg", v.n);
            else for (int i = 0; i < v.len && o < (int)sizeof out - 1; i++) out[o++] = v.s[i];
            if (o >= (int)sizeof out - 1) break;
        }
        out[o < (int)sizeof out ? o : (int)sizeof out - 1] = 0;
        if (caccept("upon")) { if (!caccept("listing")) { if (ccur()->t != 'w') cdie(">>DISPLAY UPON needs LISTING or a device name%s", ""); g_cp++; } }
        c_end("DISPLAY");
        if (any) fprintf(stderr, "%s:%d: >>DISPLAY %s\n", t->file ? t->file : "", t->line, out);
        return 1;
    }
    if (!strcasecmp(w, "flag-14")) {
        /* >>FLAG-14 option ... ON|OFF (7.3.15): validated here, in force at
         * this stage for the directives it flags, and kept for the parser
         * as >>TURN is (apply_turn) for the rest */
        if (g_std < 2023) cdie(">>FLAG-14 is COBOL 2023 (7.3.15); compile with -std=2023%s", "");
        const char *e = f14_set(txt, g_f14_text);
        if (e) cdie("%s", e);
        return 0;
    }
    if (!strcasecmp(w, "push") || !strcasecmp(w, "pop")) {
        /* >>PUSH / >>POP directive-name | ALL (7.3.22, 7.3.20) */
        int push = !strcasecmp(w, "push");
        if (g_std < 2023) cdie(">>%s is COBOL 2023 (7.3.20, 7.3.22); compile with -std=2023", push ? "PUSH" : "POP");
        g_cp = 1;
        if (ccur()->t != 'w') cdie(">>%s needs a directive-name or ALL (2023 7.3.20.2, 7.3.22.2)", push ? "PUSH" : "POP");
        const char *nm = ccur()->s; g_cp++;
        c_end(push ? "PUSH" : "POP");
        int all = !strcasecmp(nm, "all");
        int kind = all ? 1 : push_dir_kind(nm);
        if (kind < 0) {
            static const char *const no[] = { "evaluate", "if", "page", "pop", "push", "else", "end-if", "when", "end-evaluate", NULL };
            for (int i = 0; no[i]; i++) if (!strcasecmp(nm, no[i])) cdie(">>%s names no EVALUATE, IF, PAGE, POP or PUSH directive (2023 7.3.20.3 rule 1, 7.3.22.3 rule 1)", push ? "PUSH" : "POP");
            cdie("'%s' is not a compiler directive's name", nm);
        }
        if (push) push_dir_text(nm, all);
        else if (!pop_dir_text(nm, all) && !all && kind == 0 && push_dir_kind(nm) == 0 &&
                 (!strcasecmp(nm, "define") || !strcasecmp(nm, "propagate") || !strcasecmp(nm, "cobol-words")))
            fprintf(stderr, "%s:%d: warning: >>POP %s: nothing was pushed (2023 7.3.20.4 rule 2)\n", t->file ? t->file : "", t->line, nm);
        return kind == 0 ? 1 : 0;                   /* the parser's part (TURN, REF-MOD-ZERO-LENGTH, FLAG-14, ALL) stays in the text */
    }
    if (!strcasecmp(w, "cobol-words")) {
        /* >>COBOL-WORDS EQUATE a WITH b | UNDEFINE a | SUBSTITUTE a BY b | RESERVE b (7.3.10) */
        if (g_std < 2023) cdie(">>COBOL-WORDS is COBOL 2023 (7.3.10); compile with -std=2023%s", "");
        if (g_in_unit || g_units_seen) cdie(">>COBOL-WORDS comes before the first IDENTIFICATION DIVISION of the compilation group (2023 7.3.10.3 rule 1)%s", "");
        g_cp = 1;
        int kind = caccept("equate") ? CW_EQUATE : caccept("undefine") ? CW_UNDEFINE : caccept("substitute") ? CW_SUBSTITUTE : caccept("reserve") ? CW_RESERVE : 0;
        if (!kind) cdie(">>COBOL-WORDS takes EQUATE, UNDEFINE, SUBSTITUTE or RESERVE (2023 7.3.10.2)%s", "");
        char lit[2][64]; int nl = 0;
        for (int i = 0; i < (kind == CW_EQUATE || kind == CW_SUBSTITUTE ? 2 : 1); i++) {
            if (i == 1 && !caccept(kind == CW_EQUATE ? "with" : "by")) cdie(">>COBOL-WORDS: expected %s between the two literals (2023 7.3.10.2)", kind == CW_EQUATE ? "WITH" : "BY");
            CTok *c = ccur();
            if (c->t != 'a') cdie(">>COBOL-WORDS: each operand is an alphanumeric literal (2023 7.3.10.3 rule 2)%s", "");
            if (c->len < 1 || c->len > 63 || memchr(c->s, ' ', (size_t)c->len)) cdie(">>COBOL-WORDS: a literal is one COBOL word without a space (2023 7.3.10.3 rule 2)%s", "");
            for (int k = 0; k < c->len; k++) lit[nl][k] = (char)tolower((unsigned char)c->s[k]);
            lit[nl][c->len] = 0; nl++; g_cp++;
        }
        c_end("COBOL-WORDS");
        /* the word freed or renamed (a, literal-1/3/4) is a reserved word,
         * a context-sensitive word or a function name: the latter are not
         * tabled, so only the shape is checked; the word brought in (b,
         * literal-2/5/6) is a user-defined word, not reserved (rule 4) */
        const char *a = kind == CW_RESERVE ? NULL : lit[0], *b = kind == CW_RESERVE ? lit[0] : kind == CW_UNDEFINE ? NULL : lit[1];
        for (int i = 0; i < nl; i++) {
            const char *s = lit[i]; int alpha = 0, n = (int)strlen(s);
            for (int k = 0; k < n; k++) { if (isalpha((unsigned char)s[k])) alpha = 1; else if (!isdigit((unsigned char)s[k]) && s[k] != '-' && s[k] != '_') cdie(">>COBOL-WORDS: '%s' is not a COBOL word (2023 7.3.10.3 rules 3-4; 8.3.1)", s); }
            if (!alpha || s[0] == '-' || s[n - 1] == '-') cdie(">>COBOL-WORDS: '%s' is not a COBOL word (2023 7.3.10.3 rules 3-4; 8.3.1)", s);
        }
        if (b && (is_reserved85(b) || fn89_known(b))) cdie(">>COBOL-WORDS: '%s' is a reserved word or an intrinsic function's name; the word brought in is a user-defined word (2023 7.3.10.3 rule 4)", b);
        for (int i = 0; i < g_ncw; i++)
            for (int j = 0; j < nl; j++)
                if (!strcmp(lit[j], g_cw[i].a) || (g_cw[i].b[0] && !strcmp(lit[j], g_cw[i].b)))
                    cdie(">>COBOL-WORDS: '%s' is in an earlier COBOL-WORDS directive (2023 7.3.10.3 rule 5)", lit[j]);
        if (g_ncw == 64) cdie("more than 64 >>COBOL-WORDS directives%s", "");
        CobolWord *c = &g_cw[g_ncw++]; memset(c, 0, sizeof *c);
        c->kind = kind;
        snprintf(c->a, sizeof c->a, "%s", a ? a : b);        /* RESERVE: the word in a */
        if (a && b) snprintf(c->b, sizeof c->b, "%s", b);
        return 1;
    }
    if (!strcasecmp(w, "ref-mod-zero-length")) {
        /* >>REF-MOD-ZERO-LENGTH ON|OFF (2023 7.3.23): whether a reference
         * modification may resolve to a zero-length item; positional, kept
         * for the parser as >>TURN is (apply_dirs) */
        g_cp = 1;
        if (!caccept("on") && !caccept("off")) cdie(">>REF-MOD-ZERO-LENGTH takes ON or OFF (2023 7.3.23.2)%s", "");
        c_end("REF-MOD-ZERO-LENGTH");
        return 0;
    }
    if (!strcasecmp(w, "d")) cdie("the >>D debugging indicator is not implemented (debugging lines were removed in COBOL 2014)%s", "");
    cdie("the compiler directive >>%s is not implemented yet", w);
    return 1;
}

/* the word "constant", then FROM a compilation variable: AS its value
 * (13.10, format 2) -- the value in effect here */
static int cv_constant_from(TWV *out, TW *in, int n, int i)
{
    if (!tw_is(&in[i], "from")) return 0;
    int b = out->n - 1;
    while (b >= 0 && out->w[b].kind == TW_SEP) b--;
    if (b < 0 || !tw_is(&out->w[b], "constant")) {
        if (b < 2 || !tw_is(&out->w[b], "global")) return 0;     /* CONSTANT IS GLOBAL FROM ... */
        int c = b - 1; if (tw_is(&out->w[c], "is")) c--;
        if (c < 0 || !tw_is(&out->w[c], "constant")) return 0;
    }
    int j = i + 1;
    while (j < n && in[j].kind == TW_SEP) j++;
    if (j >= n || in[j].kind != TW_WORD) return 0;
    char nm[64]; snprintf(nm, sizeof nm, "%.*s", in[j].len > 63 ? 63 : in[j].len, in[j].s);
    CVar *c = cvar_find(nm);
    g_ctw = &in[j];
    if (!c) cdie("'%s' is not a compilation variable: CONSTANT ... FROM names one a >>DEFINE made (2023 13.10)", nm);
    if (!c->defined) cdie("'%s' is not defined here (2023 7.3.11.4 rule 2)", nm);
    TW as = in[i]; as.s = "as"; as.len = 2;
    twv_push(out, as);
    twv_push(out, cval_tw(&in[j], &c->v));
    return j - i + 1;
}



static void tw_copy(TWV *in, TWV *out)
{
    int pt = 0;                                 /* inside pseudo-text */
    for (int i = 0; i < in->n; i++) {
        TW *t = &in->w[i];
        if (t->kind == TW_DIR && !pt) {
            if (cond_directive(t)) continue;    /* a conditional directive, or one in omitted text */
            twv_push(out, *t); continue;        /* >>TURN, applied by the parser */
        }
        if (!cond_active()) continue;           /* omitted text (7.3.16.4 rules 2-3, 7.3.13.4 rules 4-6) */
        if (!pt && t->kind == TW_WORD) {
            /* where a compilation unit begins and ends, for the directives
             * that stand outside one (LEAP-SECOND) */
            int k = i + 1; while (k < in->n && in->w[k].kind == TW_SEP) k++;
            if ((tw_is(t, "identification") || tw_is(t, "id")) && k < in->n && tw_is(&in->w[k], "division")) {
                g_in_unit++; g_units_seen++;
                if (g_propagate_dir) {           /* a mark the parser reads at the unit's start (apply_turn) */
                    TW m = *t; m.kind = TW_DIR; m.s = "propagate-unit"; m.len = 14;
                    twv_push(out, m);
                }
            }
            if (tw_is(t, "end") && k < in->n && (tw_is(&in->w[k], "program") || tw_is(&in->w[k], "function")) && g_in_unit > 0) g_in_unit--;
        }
        if (!pt) { int used = cv_constant_from(out, in->w, in->n, i); if (used) { i += used - 1; continue; } }
        if (t->kind == TW_PDELIM) pt = !pt;
        if (pt || !tw_is(t, "copy") || t->dbg) { twv_push(out, *t); continue; }   /* a COPY on a debugging line is a comment */
        int j = i + 1;
        if (j >= in->n || (in->w[j].kind != TW_WORD && in->w[j].kind != TW_LIT)) tw_die(t, "COPY needs a text-name%s", "");
        char name[256], lib[256] = "";
        for (int side = 0; side < 2; side++) {
            TW *nm = &in->w[j++];
            char *dst = side ? lib : name;
            if (nm->kind == TW_LIT) {                  /* a literal: its characters */
                int pl = lit_prefix(nm->s), cl = nm->len - pl - 2;
                snprintf(dst, 256, "%.*s", cl < 0 ? 0 : cl > 250 ? 250 : cl, nm->s + pl + 1);
            } else {                                   /* a word: as the tokenizer had it, lowercased */
                snprintf(dst, 256, "%.*s", nm->len > 250 ? 250 : nm->len, nm->s);
                for (char *k = dst; *k; k++) *k = (char)tolower((unsigned char)*k);
            }
            if (side == 1) break;
            if (!(j < in->n && (tw_is(&in->w[j], "of") || tw_is(&in->w[j], "in")))) break;
            j++;
            if (j >= in->n || (in->w[j].kind != TW_WORD && in->w[j].kind != TW_LIT)) tw_die(t, "COPY ... OF needs a library-name%s", "");
        }
        if (j < in->n && tw_is(&in->w[j], "suppress")) { j++; if (j < in->n && tw_is(&in->w[j], "printing")) j++; }
        TWOp *ops = NULL; int nops = 0;
        if (j < in->n && tw_is(&in->w[j], "replacing")) { j++; nops = tw_operands(in, &j, &ops, t, "COPY REPLACING", 1); }
        while (j < in->n && in->w[j].kind == TW_SEP) j++;
        if (j >= in->n || in->w[j].kind != TW_PERIOD) tw_die(t, "COPY %s needs its period", name);
        if (g_copy_depth >= 8) tw_die(t, "COPY nests deeper than 8 (%s)", name);

        SrcLine *lines; int n; char found[1200];
        char qual[512]; int ok = 0;
        g_read_ff_start = 1 + t->ff;            /* the library text starts in the COPY's format */
        if (lib[0]) { snprintf(qual, sizeof qual, "%s/%s", lib, name); ok = copy_open(qual, &lines, &n, found, sizeof found); }
        if (!ok) ok = copy_open(name, &lines, &n, found, sizeof found);
        g_read_ff_start = 0;
        if (!ok) {
            g_tok_file = t->file;
            die_at(t->line, "COPY: cannot find '%s' (looked beside the source and in the -I directories, as %s, %s.cpy, %s.cbl, %s.cob, and upper-cased)", name, name, name, name, name);
        }
        for (int d = 0; d < g_copy_depth; d++)
            if (!strcmp(g_copy_stack[d], found)) tw_die(t, "COPY %s: the library text copies itself (2023 7.2.3.4 rule 12)", name);
        TWV lib_w = { 0 }, lib_x = { 0 };
        tw_lex(lines, n, &lib_w);
        g_copy_stack[g_copy_depth++] = xstrndup(found, (int)strlen(found));
        int nc0 = g_ncond;
        tw_copy(&lib_w, &lib_x);
        if (g_ncond != nc0) tw_die(t, "COPY %s: the library text ends inside a >>IF or >>EVALUATE (2023 7.3.16.3 rule 7, 7.3.13.3 rule 9)", name);
        g_copy_depth--;
        int first = out->n;
        if (nops) tw_apply(out, lib_x.w, lib_x.n, ops, nops);
        else for (int k = 0; k < lib_x.n; k++) twv_push(out, lib_x.w[k]);
        if (out->n > first) out->w[first].glued = 0;
        free(lib_w.w); free(lib_x.w); free(ops);
        i = j;
        if (i + 1 < in->n) in->w[i + 1].glued = 0;       /* the text after the period is not joined to the library text */
    }
}

/* Step 3: REPLACE statements, in order (7.2.4.4 rules 4-8); the active
 * statement's operands, and the queue that ALSO pushes and LAST OFF pops */
static void tw_replace(TWV *in, TWV *out)
{
    TWOp *act = NULL; int nact = 0;
    struct { TWOp *ops; int n; } q[64]; int nq = 0;
    for (int i = 0; i < in->n; ) {
        TW *t = &in->w[i];
        int k = i + 1;
        while (k < in->n && in->w[k].kind == TW_SEP) k++;
        int stmt = tw_is(t, "replace") && (i == 0 || !t->glued) && k < in->n &&
                   (in->w[k].kind == TW_PDELIM || tw_is(&in->w[k], "off") || tw_is(&in->w[k], "also") ||
                    tw_is(&in->w[k], "last") || tw_is(&in->w[k], "leading") || tw_is(&in->w[k], "trailing"));
        if (!stmt) {
            int end = 0, hit = -1;
            if (nact && t->kind != TW_SEP && t->kind != TW_DIR && t->kind != TW_RAW)
                for (int o = 0; o < nact && hit < 0; o++) if (tw_match(in->w, in->n, i, &act[o], &end)) hit = o;
            if (hit < 0) { twv_push(out, *t); i++; continue; }
            tw_emit(out, in->w, in->n, i, end, &act[hit]);
            i = end;
            continue;
        }
        if (g_std < 2002) {
            /* 1985: it follows a separator period, or begins the program */
            int b = out->n - 1;
            while (b >= 0 && (out->w[b].kind == TW_SEP || out->w[b].kind == TW_DIR)) b--;
            if (b >= 0 && out->w[b].kind != TW_PERIOD)
                tw_die(t, "REPLACE follows a separator period, or begins the program (X3.23-1985 REPLACE syntax rule 1)%s", "");
        }
        int j = k, also = 0, last = 0;
        if (tw_is(&in->w[j], "also")) { also = 1; j++; }
        else if (tw_is(&in->w[j], "last")) { last = 1; j++; }
        if ((also || last) && g_std < 2002) tw_die(t, "REPLACE ALSO and REPLACE LAST OFF are COBOL 2002; compile with -std=2002%s", "");
        if (last || tw_is(&in->w[j], "off")) {
            if (!tw_is(&in->w[j], "off")) tw_die(t, "REPLACE LAST: expected OFF%s", "");
            j++;
            while (j < in->n && in->w[j].kind == TW_SEP) j++;
            if (j >= in->n || in->w[j].kind != TW_PERIOD) tw_die(t, "REPLACE OFF needs its period%s", "");
            if (last && nq) { nq--; act = q[nq].ops; nact = q[nq].n; }   /* rule 7c: the last one pushed, active again */
            else { nact = 0; nq = 0; }                                    /* rule 7d, and LAST with nothing queued */
        } else {
            TWOp *ops; int n = tw_operands(in, &j, &ops, t, "REPLACE", 0);
            if (also && nact) {
                /* rule 7a: the active one queued, and this one with its operands after its own */
                if (nq == 64) tw_die(t, "REPLACE ALSO: more than 64 statements queued%s", "");
                q[nq].ops = act; q[nq].n = nact; nq++;
                TWOp *all = xmalloc((size_t)(n + nact) * sizeof *all);
                memcpy(all, ops, (size_t)n * sizeof *all); memcpy(all + n, act, (size_t)nact * sizeof *all);
                act = all; nact = n + nact;
            } else { act = ops; nact = n; nq = 0; }                      /* rules 6a and 7b */
        }
        i = j + 1;
        if (i < in->n) in->w[i].glued = 0;
    }
}

/* lines -> text-words -> COPY -> REPLACE -> lines, for the tokenizer */
static void text_manipulation(SrcLine *lines, int n, SrcLine **out, int *nout, int replace)
{
    TWV a = { 0 }, b = { 0 };
    tw_lex(lines, n, &a);
    int nc0 = g_ncond;
    tw_copy(&a, &b);
    if (g_ncond != nc0 && b.n) tw_die(&b.w[b.n - 1], "the text ends inside a >>IF or >>EVALUATE (no >>END-IF or >>END-EVALUATE)%s", "");
    else if (g_ncond != nc0) die_at(1, "the text ends inside a >>IF or >>EVALUATE (no >>END-IF or >>END-EVALUATE)");
    if (replace) { TWV c = { 0 }; tw_replace(&b, &c); free(b.w); b = c; }
    tw_lines(&b, out, nout);
    free(a.w); free(b.w);
}

/* EXEC SQL INCLUDE SQLCA: DB2's communication area, when the program has
 * no copybook of that name; BINARY where DB2 writes COMP-5, so a COBOL 85
 * program meets no extension (docs/esql.md) */
static const char *g_sqlca_text[] = {
    "01 SQLCA.",
    "   05 SQLCAID     PIC X(8) VALUE \"SQLCA\".",
    "   05 SQLCABC     PIC S9(9) BINARY VALUE 136.",
    "   05 SQLCODE     PIC S9(9) BINARY VALUE 0.",
    "   05 SQLERRM.",
    "      49 SQLERRML PIC S9(4) BINARY VALUE 0.",
    "      49 SQLERRMC PIC X(70) VALUE SPACES.",
    "   05 SQLERRP     PIC X(8) VALUE SPACES.",
    "   05 SQLERRD     PIC S9(9) BINARY OCCURS 6 TIMES.",
    "   05 SQLWARN.",
    "      10 SQLWARN0 PIC X VALUE SPACE.",
    "      10 SQLWARN1 PIC X VALUE SPACE.",
    "      10 SQLWARN2 PIC X VALUE SPACE.",
    "      10 SQLWARN3 PIC X VALUE SPACE.",
    "      10 SQLWARN4 PIC X VALUE SPACE.",
    "      10 SQLWARN5 PIC X VALUE SPACE.",
    "      10 SQLWARN6 PIC X VALUE SPACE.",
    "      10 SQLWARN7 PIC X VALUE SPACE.",
    "   05 SQLEXT.",
    "      10 SQLWARN8 PIC X VALUE SPACE.",
    "      10 SQLWARN9 PIC X VALUE SPACE.",
    "      10 SQLWARNA PIC X VALUE SPACE.",
    "      10 SQLSTATE PIC X(5) VALUE \"00000\".",
    NULL };

/* EXEC SQL INCLUDE member: COPY member, SQLCA built in -- the member read
 * as library text (its own COPY statements expanded), then tokenized */
static void expand_sql_includes(void)
{
    for (int i = 0; i < g_ntok; i++) {
        /* EXEC SQL INCLUDE member: COPY member, SQLCA built in */
        if (g_tok[i].kind == T_SQL && !strncasecmp(g_tok[i].s, "include", 7) && (g_tok[i].s[7] == ' ' || !g_tok[i].s[7])) {
            int line = g_tok[i].line;
            const char *q = g_tok[i].s + 7; while (*q == ' ') q++;
            char name[256]; int nn = 0;
            while (*q && *q != ' ' && nn < 250) { if (*q != '\'' && *q != '"') name[nn++] = *q; q++; }
            name[nn] = 0;
            if (!nn) die_at(line, "EXEC SQL INCLUDE needs a member name");
            int j = i + 1 < g_ntok && g_tok[i + 1].kind == T_PERIOD ? i + 1 : i;
            SrcLine *lines; int n; char found[1200];
            g_read_ff_start = 1 + g_tok[i].ff;
            int inc_ok = copy_open(name, &lines, &n, found, sizeof found);
            g_read_ff_start = 0;
            if (!inc_ok) {
                if (strcasecmp(name, "sqlca")) die_at(line, "EXEC SQL INCLUDE: cannot find '%s' (looked as COPY does)", name);
                /* in the LINKAGE SECTION (a subprogram given its caller's
                 * SQLCA) without the VALUE clauses; and SQLCODE or SQLSTATE
                 * left to the program when it declares its own (X/Open's
                 * SQLCA has no SQLSTATE; ISO programs declare both) */
                int in_link = 0, own_code = 0, own_state = 0;
                for (int k = i - 1; k >= 0; k--) {
                    if (g_tok[k].kind == T_WORD && g_tok[k + 1].kind == T_WORD && !strcmp(g_tok[k + 1].s, "section")) {
                        if (!strcmp(g_tok[k].s, "linkage")) in_link = 1;
                        if (!strcmp(g_tok[k].s, "linkage") || !strcmp(g_tok[k].s, "working-storage") || !strcmp(g_tok[k].s, "local-storage")) break;
                    }
                }
                for (int k = 1; k < g_ntok; k++)
                    if (g_tok[k].kind == T_WORD && g_tok[k - 1].kind == T_NUM) {
                        if (!strcmp(g_tok[k].s, "sqlcode")) own_code = 1;
                        if (!strcmp(g_tok[k].s, "sqlstate")) own_state = 1;
                    }
                n = 0; while (g_sqlca_text[n]) n++;
                lines = xmalloc((size_t)n * sizeof *lines);
                for (int k = 0; k < n; k++) {
                    char buf[128]; snprintf(buf, sizeof buf, "%s", g_sqlca_text[k]);
                    if (in_link) { char *v = strstr(buf, " VALUE "); if (v) strcpy(v, "."); }
                    for (int f = 0; f < 2; f++) {           /* the program's own: FILLER here */
                        const char *nm = f ? " SQLSTATE " : " SQLCODE ";
                        char *v = (f ? own_state : own_code) ? strstr(buf, nm) : NULL;
                        if (v) { char rest[128]; snprintf(rest, sizeof rest, "%s", v + strlen(nm)); snprintf(v, sizeof buf - (size_t)(v - buf), " FILLER %s", rest); }
                    }
                    memset(&lines[k], 0, sizeof lines[k]); lines[k].text = xstrndup(buf, (int)strlen(buf)); lines[k].line = line;
                }
                snprintf(found, sizeof found, "SQLCA (built in)");
            }
            Tok *save_tok = g_tok; int save_n = g_ntok, save_cap = g_tcap;
            const char *save_file = g_tok_file;
            g_tok = NULL; g_ntok = 0; g_tcap = 0; g_tok_file = xstrndup(found, (int)strlen(found));
            { SrcLine *tl; int tn; text_manipulation(lines, n, &tl, &tn, 0); tokenize_lines(tl, tn); }
            Tok *ctok = g_tok; int cn = g_ntok;
            g_tok = save_tok; g_ntok = save_n; g_tcap = save_cap; g_tok_file = save_file;
            if (cn && ctok[cn - 1].kind == T_EOF) cn--;
            int removed = j - i + 1, newn = g_ntok - removed + cn;
            if (newn > g_tcap) { g_tcap = newn + 1024; g_tok = realloc(g_tok, g_tcap * sizeof *g_tok); }
            memmove(&g_tok[i + cn], &g_tok[j + 1], (size_t)(g_ntok - (j + 1)) * sizeof *g_tok);
            memcpy(&g_tok[i], ctok, (size_t)cn * sizeof *ctok);
            g_ntok = newn;
            free(ctok);
            i--;
            continue;
        }
    }
}

/* Identification Division comment-entries (GitHub #37).  The text after
 * AUTHOR., INSTALLATION., DATE-WRITTEN., DATE-COMPILED., SECURITY. or
 * REMARKS., up to the next paragraph or division header, is a comment-entry
 * in the 1985 text: any characters, an apostrophe included, so it must not
 * reach the tokenizer (which saw an unterminated literal).  The header keeps
 * its name and period; the rest of that line and the lines after it are
 * blanked. */
static void strip_comment_entries(SrcLine *lines, int n)
{
    static const char *paras[] = { "author", "installation", "date-written", "date-compiled", "security", "remarks", NULL };
    int in_entry = 0;
    for (int li = 0; li < n; li++) {
        char *p = lines[li].text;
        while (*p == ' ' || *p == '\t') p++;
        char w[32]; int wl = 0;
        while (is_wordch((unsigned char)p[wl]) && wl < (int)sizeof w - 1) { w[wl] = (char)tolower((unsigned char)p[wl]); wl++; }
        w[wl] = 0;
        char *q = p + wl;
        while (*q == ' ' || *q == '\t') q++;
        int next_is_division = !strncasecmp(q, "division", 8) && !is_wordch((unsigned char)q[8]);
        if (wl && next_is_division && (!strcmp(w, "identification") || !strcmp(w, "id") || !strcmp(w, "environment") ||
                                       !strcmp(w, "data") || !strcmp(w, "procedure"))) { in_entry = 0; continue; }
        if (wl && *q == '.' && !strcmp(w, "program-id")) { in_entry = 0; continue; }
        int is_para = 0;
        for (int i = 0; wl && paras[i]; i++) if (!strcmp(w, paras[i])) is_para = 1;
        if (is_para && *q == '.') { q[1] = 0; in_entry = 1; continue; }     /* keep "AUTHOR." */
        if (in_entry) *p = 0;
    }
}

static int is_word(Tok *t, const char *w);
