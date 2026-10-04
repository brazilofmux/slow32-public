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

static int g_dp_comma;      /* SPECIAL-NAMES DECIMAL-POINT IS COMMA */
static int g_currency;      /* SPECIAL-NAMES CURRENCY SIGN IS "c": the picture symbol standing for '$', 0 for '$' itself */

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
    for (int i = 0; i + 1 < g_ntok; i++) {
        if (g_tok[i].kind != T_WORD || strcmp(g_tok[i].s, "currency")) continue;
        int j = i + 1;
        if (g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "sign")) j++;
        if (j < g_ntok && g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "is")) j++;
        if (j >= g_ntok || g_tok[j].kind != T_STR) die_at(g_tok[i].line, "CURRENCY SIGN needs a literal");
        if (g_tok[j].len != 1) die_at(g_tok[j].line, "CURRENCY SIGN IS: the literal is one character");
        unsigned char c = (unsigned char)g_tok[j].s[0];
        if (isdigit(c) || c == ' ' || strchr("ABCDPRSVXZabcdprsvxz*+-,.;()\"/=", c))
            die_at(g_tok[j].line, "CURRENCY SIGN IS '%c': that character has a meaning of its own in a PICTURE", c);
        g_currency = c;
        break;
    }
    int w = 0;
    for (int i = 0; i < g_ntok; i++) {
        Tok *t = &g_tok[i];
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

/* a period, comma or semicolon is a separator before a space, the line's
 * end, or a closing pseudo-text delimiter (==... PIC 9(5).==) */
static int sep_after(const char *q) { return !q[1] || q[1] == ' ' || q[1] == '\t' || (q[1] == '=' && q[2] == '='); }

static void tw_lex(const SrcLine *lines, int nlines, TWV *out)
{
    int sql = 0; char sq = 0;           /* inside EXEC SQL; the quote open there */
    for (int li = 0; li < nlines; li++) {
        const SrcLine *L = &lines[li];
        TW t; memset(&t, 0, sizeof t);
        t.line = L->line; t.file = L->file; t.dbg = (unsigned char)L->dbg; t.ff = (unsigned char)L->ff;
        if (L->dir) { t.kind = TW_DIR; t.s = L->text; t.len = (int)strlen(L->text); twv_push(out, t); continue; }
        const char *p = L->text;
        int glued = 0;
        while (*p) {
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
                twv_push(out, t); glued = 1;
                continue;
            }
            if (*p == ' ' || *p == '\t') { glued = 0; p++; continue; }
            if (p[0] == '*' && p[1] == '>') break;                      /* a comment to the end of the line */
            const char *s = p;
            int kind = TW_WORD, pl;
            if (p[0] == '=' && p[1] == '=') { kind = TW_PDELIM; p += 2; }
            else if (*p == '(' || *p == ')' || *p == ':') p++;
            else if (*p == '.' && sep_after(p)) { kind = TW_PERIOD; p++; }
            else if ((*p == ',' || *p == ';') && sep_after(p)) { kind = TW_SEP; p++; }
            else if ((pl = lit_prefix(p)) >= 0) {
                char q = p[pl];
                p += pl + 1;
                while (*p) {
                    if (*p == q) { if (p[1] == q) { p += 2; continue; } p++; break; }
                    p++;
                }
                kind = TW_LIT;
            } else {
                while (*p && *p != ' ' && *p != '\t' && *p != '(' && *p != ')' && *p != ':' && *p != '"' && *p != '\'' &&
                       !(p[0] == '=' && p[1] == '=') && !(p[0] == '*' && p[1] == '>') &&
                       !((*p == '.' || *p == ',' || *p == ';') && sep_after(p))) p++;
            }
            t.kind = (unsigned char)kind; t.s = xstrndup(s, (int)(p - s)); t.len = (int)(p - s); t.glued = (unsigned char)glued;
            twv_push(out, t); glued = 1;
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

static void tw_copy(TWV *in, TWV *out)
{
    int pt = 0;                                 /* inside pseudo-text */
    for (int i = 0; i < in->n; i++) {
        TW *t = &in->w[i];
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
        tw_copy(&lib_w, &lib_x);
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
    tw_copy(&a, &b);
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
static struct { int pos; Tok tok; } *g_dir; static int g_ndir, g_dircap, g_ndir_done;   /* >>TURN directives, by token position */
