/* s32-cobc: tokenizer.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ====================================================================== */
/* Tokenizer                                                               */
/* ====================================================================== */

enum { T_EOF, T_WORD, T_NUM, T_STR, T_PIC, T_PERIOD, T_LP, T_RP, T_COLON, T_OP, T_DIR,   /* T_DIR: a >>TURN, taken out of the stream */
       T_SQL };   /* EXEC SQL ... END-EXEC: s is the statement's text (docs/esql.md) */

typedef struct {
    int kind, line;
    char *s;        /* word (lowercased), number text, literal bytes, picture, op */
    int len;        /* literal byte length (literals may hold NULs) */
    const char *file;
    int dbg;        /* from a debugging line: matched by COPY REPLACING, then dropped without DEBUGGING MODE */
    unsigned char after_comma;   /* a separator comma or semicolon stood before this token */
    unsigned char nat;           /* T_STR: a national literal, its bytes UTF-16 big-endian (cobol ISSUES-62) */
    char *orig;                  /* T_WORD: as written, before lowercasing; 0 when the same */
    unsigned char boolv;         /* T_STR: a boolean literal, one character 0 or 1 per position (cobol ISSUES-76) */
    int strong;                  /* the strong-type marker expand_types() puts in an entry: its type key + 1 (cobol ISSUES-80) */
} Tok;

static Tok *g_tok;
static int g_ntok, g_tcap;
static int g_sql_declare;               /* between EXEC SQL BEGIN and END DECLARE SECTION */
static int g_tok_dbg;

static int g_pending_comma;

static Tok *push_tok(int kind, int line, const char *s, int len)
{
    if (g_ntok == g_tcap) { g_tcap = g_tcap ? g_tcap * 2 : 1024; g_tok = realloc(g_tok, g_tcap * sizeof *g_tok); }
    Tok *t = &g_tok[g_ntok++];
    t->after_comma = (unsigned char)g_pending_comma; g_pending_comma = 0;
    t->kind = kind; t->line = line; t->s = xstrndup(s, len); t->len = len; t->file = g_tok_file; t->dbg = g_tok_dbg; t->nat = 0; t->orig = 0; t->boolv = 0; t->strong = 0;
    return t;
}

static int is_wordch(int c) { return isalnum(c) || c == '-' || c == '_'; }

/* a word is matched lowercased; its spelling is kept for the names the
 * program can see at run time (EXCEPTION-LOCATION, EXCEPTION-FILE) */
static void word_lower(Tok *w)
{
    int up = 0;
    for (char *k = w->s; *k; k++) if (isupper((unsigned char)*k)) up = 1;
    if (!up) return;
    w->orig = xstrndup(w->s, (int)strlen(w->s));
    for (char *k = w->s; *k; k++) *k = (char)tolower((unsigned char)*k);
}
static const char *tok_orig(const Tok *t) { return t->orig ? t->orig : t->s; }

/* UTF-8 source text to national bytes, UTF-16 big-endian, as libcob's
 * utf8_to_nat does it; returns the bytes written, 2 per code unit, or -1
 * at a byte that begins no valid UTF-8 sequence (the source is UTF-8) */
static int utf8_to_utf16be(const unsigned char *p, int n, unsigned char *out)
{
    int k = 0, i = 0;
    while (i < n) {
        uint32_t cp;
        int len = (int)s32u_decode(p + i, (size_t)(n - i), &cp);
        if (cp == S32U_REPL && !(len == 3 && p[i] == 0xEF && p[i + 1] == 0xBF && p[i + 2] == 0xBD)) return -1;
        i += len;
        k += 2 * s32u_u16_put(out + k, cp);
    }
    return k;
}

static int hexval(int c)
{
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

/* EXEC SQL: the text up to END-EXEC, across lines (joined by a space),
 * as one T_SQL token -- SQL is never COBOL-tokenized.  Quotes ('...'
 * strings, "..." identifiers) are respected and -- comments dropped.  *li
 * and the return value are where tokenizing resumes, just past END-EXEC.
 * Returns NULL when the word after EXEC is not SQL. */
static int sql_wordch(int c) { return isalnum(c) || c == '-' || c == '_'; }
static const char *take_exec_sql(SrcLine *lines, int nlines, int *li, const char *p, int line)
{
    int l = *li;
    const char *q = p;
    for (;;) {                                   /* the word after EXEC, maybe on a later line */
        while (*q == ' ' || *q == '\t') q++;
        if (*q || l + 1 >= nlines) break;
        l++; q = lines[l].text;
    }
    if (strncasecmp(q, "sql", 3) || sql_wordch((unsigned char)q[3])) return NULL;
    q += 3;
    size_t cap = 256, n = 0; char *buf = xmalloc(cap);
    char quote = 0;
    for (;;) {
        if (!*q) {
            /* a line break is a space -- inside a quoted string too: the
             * NIST suite writes '... AS' and 'TIMESTAMP))' on two lines
             * with no COBOL continuation, one SQL string (docs/esql.md) */
            if (++l >= nlines) die_at(line, quote ? "EXEC SQL: a quoted string or name is not closed" : "EXEC SQL without END-EXEC");
            q = lines[l].text;
            if (!quote) while (*q == ' ' || *q == '\t') q++;
            if (n && (quote || buf[n - 1] != ' ')) { if (n + 1 >= cap) buf = xrealloc(buf, cap *= 2); buf[n++] = ' '; }
            continue;
        }
        char c = *q;
        if (quote) { if (c == quote) quote = 0; }
        else if (c == '\'' || c == '"') quote = c;
        else if (c == '-' && q[1] == '-' && (n == 0 || !sql_wordch((unsigned char)buf[n - 1]))) { q += strlen(q); continue; }
            /* a SQL comment to the end of the line -- not inside a COBOL host
             * name, which may hold hyphens: :CITY1---city1 (the NIST suite) */
        else if ((c == 'e' || c == 'E') && !strncasecmp(q, "end-exec", 8) && !sql_wordch((unsigned char)q[8]) &&
                 (q == lines[l].text || !sql_wordch((unsigned char)q[-1]))) {
            while (n && buf[n - 1] == ' ') n--;
            size_t b = 0; while (b < n && buf[b] == ' ') b++;
            push_tok(T_SQL, line, buf + b, (int)(n - b));
            free(buf);
            *li = l;
            return q + 8;
        }
        if (n + 2 >= cap) buf = xrealloc(buf, cap *= 2);
        buf[n++] = c == '\t' ? ' ' : c;
        q++;
    }
}

static void tokenize_lines(SrcLine *lines, int nlines)
{
    static int sql_decl_tok;    /* between EXEC SQL BEGIN and END DECLARE SECTION */
    int pic_ctx = 0;    /* after PIC/PICTURE [IS]: the next token is a picture */
    for (int li = 0; li < nlines; li++) {
        const char *t = lines[li].text;
        int line = lines[li].line;
        const char *p = t;
        g_tok_dbg = lines[li].dbg;
        if (lines[li].file) g_tok_file = lines[li].file;
        if (lines[li].dir) { push_tok(T_DIR, line, t, (int)strlen(t)); continue; }
        while (*p) {
            if (*p == ' ' || *p == '\t') { p++; continue; }
            if (p[0] == '*' && p[1] == '>') break;            /* comment to EOL */

            if (pic_ctx) {
                /* A picture runs to the next space; a period is part of it
                 * unless it is the last character before that space, in which
                 * case it is the sentence separator. */
                const char *q = p;
                while (*q && *q != ' ' && *q != '\t' && !(q[0] == '=' && q[1] == '=')) q++;   /* == ends pseudo-text */
                int n = (int)(q - p);
                int sep = 0;
                if (n > 1 && p[n - 1] == '.') { n--; sep = 1; }
                else if (n > 1 && (p[n - 1] == ';' || p[n - 1] == ',')) n--;    /* a separator, not a symbol: PICTURE 99; VALUE 8 */
                push_tok(T_PIC, line, p, n);
                if (sep) push_tok(T_PERIOD, line, ".", 1);
                p = q;
                pic_ctx = 0;
                continue;
            }

            int c = (unsigned char)*p;

            /* A zero-length literal is COBOL 2014's: 1985 has 1 through
             * 160 characters, 2002 more than zero (8.3.1.2.1.2 rule 1, X"" too;
             * .3.2 rule 1 boolean, .4.2 rule 1 national) */
            #define NO_EMPTY_LIT(n, what, rule) do { if ((n) == 0) die_at(line, "a zero-length %s literal is COBOL 2014 (%s)", what, \
                g_std < 2002 ? "X3.23-1985: 1 through 160 characters" : rule); \
                if ((n) > 8191) die_at(line, "this %s literal has %d positions, more than 8,191, the most any edition allows (%s)", what, (int)(n), \
                g_std < 2002 ? "X3.23-1985 nonnumeric literals allow 160" : rule); \
                if ((n) > 160) bp(BP_E20_LONG_LITERAL, line); } while (0)   /* 1985 and 2002 say 160: taken (preservation) */
            /* Hexadecimal literal X'..' */
            if ((c == 'x' || c == 'X') && (p[1] == '\'' || p[1] == '"')) {
                char q = p[1];
                const char *s = p + 2, *e = s;
                while (*e && *e != q) e++;
                if (!*e) die_at(line, "unterminated hexadecimal literal");
                int n = (int)(e - s);
                if (n & 1) die_at(line, "hexadecimal literal needs an even number of digits");
                if (g_std < 2002) bp(BP_E8_HEX_LITERAL, line);
                NO_EMPTY_LIT(n, "hexadecimal", "2002 8.3.1.2.1.2 rule 1");
                char *bytes = xmalloc(n / 2 + 1);
                for (int i = 0; i < n; i += 2) {
                    int h = hexval(s[i]), l = hexval(s[i + 1]);
                    if (h < 0 || l < 0) die_at(line, "bad hexadecimal digit in literal");
                    bytes[i / 2] = (char)(h * 16 + l);
                }
                push_tok(T_STR, line, bytes, n / 2);
                free(bytes);
                p = e + 1;
                continue;
            }
            /* National literals (2023 8.3.3.5): N"..." in the source's
             * UTF-8, NX"..." as hexadecimal code units; both stored UTF-16BE */
            if ((c == 'n' || c == 'N') && (p[1] == '\'' || p[1] == '"' ||
                ((p[1] == 'x' || p[1] == 'X') && (p[2] == '\'' || p[2] == '"')))) {
                if (g_std < 2002) die_at(line, "national literals (N\"...\") are COBOL 2002; compile with -std=2002");
                int hex = p[1] == 'x' || p[1] == 'X';
                char q = p[hex ? 2 : 1];
                const char *s = p + (hex ? 3 : 2);
                char *raw = xmalloc(strlen(p) + 1); int rn = 0;
                for (;;) {
                    if (!*s) die_at(line, "unterminated national literal");
                    if (*s == q) { if (!hex && s[1] == q) { raw[rn++] = q; s += 2; continue; } break; }
                    raw[rn++] = *s++;
                }
                char *out; int on;
                if (hex) {
                    if (rn % 4) die_at(line, "NX\"...\" needs four hexadecimal digits for each national character (2023 8.3.3.5.3 rule 5: UTF-16 here)");
                    out = xmalloc((size_t)rn / 2 + 1); on = rn / 2;
                    for (int i = 0; i < rn; i += 2) {
                        int h = hexval(raw[i]), l = hexval(raw[i + 1]);
                        if (h < 0 || l < 0) die_at(line, "bad hexadecimal digit in a national literal (2023 8.3.3.5.3 rule 4)");
                        out[i / 2] = (char)(h * 16 + l);
                    }
                } else {
                    out = xmalloc((size_t)rn * 4 + 1); on = utf8_to_utf16be((const unsigned char *)raw, rn, (unsigned char *)out);
                    if (on < 0) die_at(line, "a national literal must be UTF-8 text (the source is UTF-8)");
                }
                NO_EMPTY_LIT(on / 2, "national", "2002 8.3.1.2.4.2 rule 1");
                Tok *nt = push_tok(T_STR, line, out, on);
                nt->nat = 1;
                free(raw); free(out);
                p = s + 1;
                continue;
            }
            /* Boolean literals (2023 8.3.3.4): B"0101", BX"5"; held as one
             * character 0 or 1 per boolean position */
            if ((c == 'b' || c == 'B') && (p[1] == '\'' || p[1] == '"' ||
                ((p[1] == 'x' || p[1] == 'X') && (p[2] == '\'' || p[2] == '"')))) {
                if (g_std < 2002) die_at(line, "boolean literals (B\"...\") are COBOL 2002; compile with -std=2002");
                int hex = p[1] == 'x' || p[1] == 'X';
                char q = p[hex ? 2 : 1];
                const char *s = p + (hex ? 3 : 2), *e = s;
                while (*e && *e != q) e++;
                if (!*e) die_at(line, "unterminated boolean literal");
                int n = (int)(e - s);
                char *out = xmalloc((size_t)n * 4 + 1); int on = 0;
                for (int i = 0; i < n; i++) {
                    if (hex) {
                        int h = hexval(s[i]);
                        if (h < 0) die_at(line, "bad hexadecimal digit in a boolean literal (2023 8.3.3.4.3 rule 3)");
                        for (int b = 3; b >= 0; b--) out[on++] = (char)('0' + ((h >> b) & 1));
                    } else {
                        if (s[i] != '0' && s[i] != '1') die_at(line, "a boolean literal holds only the characters 0 and 1 (2023 8.3.3.4.3 rule 2)");
                        out[on++] = s[i];
                    }
                }
                NO_EMPTY_LIT(on, "boolean", "2002 8.3.1.2.3.2 rule 1");
                Tok *bt = push_tok(T_STR, line, out, on);
                bt->boolv = 1;
                free(out);
                p = e + 1;
                continue;
            }
            if ((c == 'z' || c == 'Z') && (p[1] == '\'' || p[1] == '"'))
                die_at(line, "%c'...' literals are not in COBOL 85", toupper(c));

            /* Nonnumeric literal, with the doubled-quote escape */
            if (c == '\'' || c == '"') {
                char q = (char)c;
                char *out = xmalloc(strlen(p) + 1);
                int n = 0;
                const char *s = p + 1;
                for (;;) {
                    if (!*s) die_at(line, "unterminated literal");
                    if (*s == q) {
                        if (s[1] == q) { out[n++] = q; s += 2; continue; }
                        break;
                    }
                    out[n++] = *s++;
                }
                NO_EMPTY_LIT(n, "alphanumeric", "2002 8.3.1.2.1.2 rule 1");
                push_tok(T_STR, line, out, n);
                free(out);
                p = s + 1;
                continue;
            }

            /* Numeric literal: [+-]digits[.digits], sign only when it stands
             * at a word boundary.  A run of digits followed by more word
             * characters (0100-main, 9000-end) is a user-word. */
            int signed_num = (c == '+' || c == '-') && (isdigit((unsigned char)p[1]) || (p[1] == '.' && isdigit((unsigned char)p[2]))) &&
                             (p == t || p[-1] == ' ' || p[-1] == '\t' || p[-1] == '(' || p[-1] == '=');
            int dot_num = c == '.' && isdigit((unsigned char)p[1]) &&
                          (p == t || p[-1] == ' ' || p[-1] == '\t' || p[-1] == '(' || p[-1] == '=');
            if (isdigit(c) || signed_num || dot_num) {
                const char *s = p + (signed_num ? 1 : 0), *e = s;
                while (isdigit((unsigned char)*e)) e++;
                if (*e == '.' && isdigit((unsigned char)e[1])) { e++; while (isdigit((unsigned char)*e)) e++; }
                if (is_wordch((unsigned char)*e) && !signed_num) {
                    /* ".00-EXIT" is a period with no space after it, not a
                     * word: the loop below would take nothing, forever */
                    if (c == '.') die_at(line, "a period must be followed by a space or the end of the line");
                    e = p; while (is_wordch((unsigned char)*e)) e++;
                    Tok *w = push_tok(T_WORD, line, p, (int)(e - p));
                    word_lower(w);
                    p = e;
                    continue;
                }
                push_tok(T_NUM, line, p, (int)(e - p));
                p = e;
                continue;
            }

            if (isalpha(c)) {
                const char *e = p;
                while (is_wordch((unsigned char)*e)) e++;
                if (e - p == 4 && !strncasecmp(p, "exec", 4)) {
                    const char *r = take_exec_sql(lines, nlines, &li, e, line);
                    if (r) {
                        /* inside a DECLARE SECTION a character set name may be
                         * qualified (CTS1.CS): the tokenizer keeps it one word */
                        const char *sq = g_tok[g_ntok - 1].s;
                        if (!strncasecmp(sq, "begin declare section", 21)) sql_decl_tok = 1;
                        else if (!strncasecmp(sq, "end declare section", 19)) sql_decl_tok = 0;
                        p = r; t = lines[li].text; line = lines[li].line; continue;
                    }
                }
                if (sql_decl_tok) while (*e == '.' && isalpha((unsigned char)e[1])) { e++; while (is_wordch((unsigned char)*e)) e++; }
                Tok *w = push_tok(T_WORD, line, p, (int)(e - p));
                word_lower(w);
                if (!strcmp(w->s, "pic") || !strcmp(w->s, "picture")) pic_ctx = 1;
                p = e;
                if (pic_ctx) {
                    const char *q = p;
                    while (*q == ' ' || *q == '\t') q++;
                    if ((q[0] == 'i' || q[0] == 'I') && (q[1] == 's' || q[1] == 'S') &&
                        (q[2] == ' ' || q[2] == '\t' || !q[2])) p = q + 2;     /* PICTURE IS at a line's end, the picture below (NC107A) */
                }
                continue;
            }

            if (c == '.') {
                if (p[1] == 0 || p[1] == ' ' || p[1] == '\t' || (p[1] == '*' && p[2] == '>') || (p[1] == '=' && p[2] == '=')) {   /* ".==": a period ending pseudo-text */
                    push_tok(T_PERIOD, line, ".", 1); p++; continue;
                }
                if (p[1] == '.' && (p[2] == 0 || p[2] == ' ' || p[2] == '\t')) {
                    /* a doubled period, one separator: RM's reader let "VALUE 12370121.." through (APENTER) */
                    push_tok(T_PERIOD, line, ".", 1); p += 2; continue;
                }
                die_at(line, "a period must be followed by a space or the end of the line");
            }
            if (c == ',' && p > t && isdigit((unsigned char)p[-1]) && isdigit((unsigned char)p[1])) {
                /* a comma tight between digits: the decimal point under
                 * DECIMAL-POINT IS COMMA, settled once the whole text is in */
                push_tok(T_OP, line, ",", 1); p++; continue;
            }
            if (c == ',' || c == ';') {
                if (p[1] == 0 || p[1] == ' ' || p[1] == '\t') { g_pending_comma = 1; p++; continue; }
                bp(BP_E22_SEPARATOR_SPACE, line);        /* "... is ",SQL-COD (the NIST SQL suite) */
                g_pending_comma = 1; p++; continue;
            }
            if (c == '(') { push_tok(T_LP, line, "(", 1); p++; continue; }
            if (c == ')') { push_tok(T_RP, line, ")", 1); p++; continue; }
            if (c == ':') { push_tok(T_COLON, line, ":", 1); p++; continue; }
            if (c == '*' && p[1] == '*') { push_tok(T_OP, line, "**", 2); p += 2; continue; }
            if (c == '=' && p[1] == '=') { push_tok(T_OP, line, "==", 2); p += 2; continue; }      /* pseudo-text delimiter */
            if ((c == '>' || c == '<') && p[1] == '=') { push_tok(T_OP, line, p, 2); p += 2; continue; }
            if (c == '<' && p[1] == '>') { push_tok(T_OP, line, "<>", 2); p += 2; continue; }
            if (strchr("=<>+-*/", c)) { push_tok(T_OP, line, p, 1); p++; continue; }
            if (c == '&') { push_tok(T_OP, line, "&", 1); p++; continue; }     /* concatenation (join_concat) */
            die_at(line, "unexpected character '%c'", c);
        }
    }
}
