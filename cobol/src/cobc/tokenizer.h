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
    unsigned char ff;            /* read in free form: a COPY here starts its library text so (2023 7.3.24.3 rule 3); last, after the positional initializers' fields */
    unsigned char hex;           /* T_STR: written as a hexadecimal literal X".." (CURRENCY SIGN refuses one as the symbol, 2014 E.2 item 10) */
    unsigned char uw;            /* T_WORD: a user word by >>COBOL-WORDS UNDEFINE or SUBSTITUTE -- no keyword matches it (is_word) */
} Tok;

static Tok *g_tok;
static int g_ntok, g_tcap;
static int g_sql_declare;               /* between EXEC SQL BEGIN and END DECLARE SECTION */
static int g_tok_dbg;
static int g_tok_ff;             /* the format of the line being tokenized */

static int g_pending_comma;

static Tok *push_tok(int kind, int line, const char *s, int len)
{
    if (g_ntok == g_tcap) { g_tcap = g_tcap ? g_tcap * 2 : 1024; g_tok = realloc(g_tok, g_tcap * sizeof *g_tok); }
    Tok *t = &g_tok[g_ntok++];
    t->after_comma = (unsigned char)g_pending_comma; g_pending_comma = 0;
    t->kind = kind; t->line = line; t->s = xstrndup(s, len); t->len = len; t->file = g_tok_file; t->dbg = g_tok_dbg; t->ff = (unsigned char)g_tok_ff; t->nat = 0; t->orig = 0; t->boolv = 0; t->strong = 0; t->hex = 0; t->uw = 0;   /* hex was left as the buffer had it (found with item 34) */
    return t;
}

static int is_wordch(int c) { return isalnum(c) || c == '-' || c == '_' || (c & 0x80); }

/* ---- extended letters (2023 8.1.3, Annex B; standard-queue item 47) ----
 * A user-defined word may hold the letters of other scripts: the
 * scanner takes any UTF-8 sequence as a word character, and the tokenizer
 * checks each code point against Annex B's two sets (xid_table.h) -- the
 * start characters anywhere, the others not first -- and refuses the
 * rest (U+00D7, U+037A, a symbol).  KATAKANA MIDDLE DOT is medial (B.3
 * item 3).  Words are matched without regard to case: the simple
 * one-to-one case pairs of Latin-1, Latin Extended-A, Greek and Cyrillic
 * fold to their lowercase (14.1 general case mappings; the two 2023 E.2
 * item 14 deleted are not pairs here); other scripts have no case. */
#include "xid_tables.h"
/* one code point's UTF-8 bytes through a membership DFA (libutf's reader):
 * 1 a member, 0 not; the first accepting state decides */
static int xid_dfa(const unsigned char *itt, const unsigned short *sot, const unsigned short *sbt, int accept_start,
                   const unsigned char *p, int n)
{
    int st = 0;
    for (int i = 0; i < n && st < accept_start; i++) {
        unsigned char col = itt[p[i]]; unsigned short off = sot[st];
        for (;;) {
            int y = sbt[off];
            if (y < 128) {
                if (col < y) { st = sbt[off + 1]; break; }
                col = (unsigned char)(col - y); off = (unsigned short)(off + 2);
            } else {
                y = 256 - y;
                if (col < y) { st = sbt[off + col + 1]; break; }
                col = (unsigned char)(col - y); off = (unsigned short)(off + y + 1);
            }
        }
    }
    return st >= accept_start ? st - accept_start : 0;
}
/* the sequence's class: 1 a start character, 2 a continue character, 0 neither */
static int xid_class(const unsigned char *p, int n)
{
    if (xid_dfa(xid_start_itt, xid_start_sot, xid_start_sbt, XID_START_ACCEPTING_STATES_START, p, n)) return 1;
    if (xid_dfa(xid_cont_itt, xid_cont_sot, xid_cont_sbt, XID_CONT_ACCEPTING_STATES_START, p, n)) return 2;
    return 0;
}
/* the code point at p (a UTF-8 sequence the scanner accepted), its length in *len */
static unsigned utf8_cp(const unsigned char *p, int *len)
{
    if (p[0] < 0xC0) { *len = 1; return p[0]; }
    if (p[0] < 0xE0) { *len = 2; return ((unsigned)(p[0] & 0x1F) << 6) | (p[1] & 0x3F); }
    if (p[0] < 0xF0) { *len = 3; return ((unsigned)(p[0] & 0x0F) << 12) | ((unsigned)(p[1] & 0x3F) << 6) | (p[2] & 0x3F); }
    *len = 4; return ((unsigned)(p[0] & 0x07) << 18) | ((unsigned)(p[1] & 0x3F) << 12) | ((unsigned)(p[2] & 0x3F) << 6) | (p[3] & 0x3F);
}
/* the lowercase of an extended letter's code point, or itself */
static unsigned ext_lower(unsigned cp)
{
    if (cp >= 0xC0 && cp <= 0xDE && cp != 0xD7) return cp + 0x20;           /* Latin-1 Supplement */
    if (cp >= 0x100 && cp <= 0x137 && !(cp & 1) && cp != 0x130) return cp + 1;   /* Latin Extended-A, the pairs (İ excepted: no simple pair) */
    if (cp >= 0x139 && cp <= 0x148 && (cp & 1)) return cp + 1;
    if (cp >= 0x14A && cp <= 0x177 && !(cp & 1)) return cp + 1;
    if (cp == 0x178) return 0xFF;
    if (cp >= 0x179 && cp <= 0x17E && (cp & 1)) return cp + 1;
    if (cp >= 0x391 && cp <= 0x3A9 && cp != 0x3A2) return cp + 0x20;        /* Greek, and its accented capitals */
    if (cp == 0x386) return 0x3AC;
    if (cp >= 0x388 && cp <= 0x38A) return cp + 0x25;
    if (cp == 0x38C) return 0x3CC;
    if (cp == 0x38E || cp == 0x38F) return cp + 0x3F;
    if (cp >= 0x410 && cp <= 0x42F) return cp + 0x20;                       /* Cyrillic */
    if (cp >= 0x400 && cp <= 0x40F) return cp + 0x50;
    return cp;
}
static int utf8_put(unsigned cp, char *o)
{
    if (cp < 0x80) { o[0] = (char)cp; return 1; }
    if (cp < 0x800) { o[0] = (char)(0xC0 | (cp >> 6)); o[1] = (char)(0x80 | (cp & 0x3F)); return 2; }
    if (cp < 0x10000) { o[0] = (char)(0xE0 | (cp >> 12)); o[1] = (char)(0x80 | ((cp >> 6) & 0x3F)); o[2] = (char)(0x80 | (cp & 0x3F)); return 3; }
    o[0] = (char)(0xF0 | (cp >> 18)); o[1] = (char)(0x80 | ((cp >> 12) & 0x3F)); o[2] = (char)(0x80 | ((cp >> 6) & 0x3F)); o[3] = (char)(0x80 | (cp & 0x3F)); return 4;
}
/* a name's text folded to lowercase in place: ASCII and the extended
 * letters' pairs (a program-name literal against a PROGRAM-ID's word); the
 * pairs keep their byte length */
static void str_fold(char *s)
{
    for (unsigned char *k = (unsigned char *)s; *k; ) {
        if (*k < 0x80) { *k = (unsigned char)tolower(*k); k++; continue; }
        int len; unsigned cp = utf8_cp(k, &len), lc = ext_lower(cp);
        if (lc != cp) utf8_put(lc, (char *)k);
        k += len;
    }
}
/* a word with bytes beyond ASCII: each code point one of Annex B's, in
 * its place; the word's text folded to lowercase */
static void word_extended(Tok *w)
{
    const unsigned char *p = (const unsigned char *)w->s;
    int n = (int)strlen(w->s), i = 0, first = 1, lower = 0;
    char *out = xmalloc((size_t)n + 1); int o = 0;
    while (i < n) {
        int len; unsigned cp = utf8_cp(p + i, &len);
        if (i + len > n) die_at(w->line, "'%s': a UTF-8 sequence cut short", w->s);
        int last = i + len >= n;
        if (cp >= 0x80) {
            int cls = xid_class(p + i, len);
            if (cp == 0x30FB && (first || last)) die_at(w->line, "'%s': KATAKANA MIDDLE DOT (U+30FB) is neither the first nor the last character of a user-defined word (2023 Annex B item 3)", w->s);
            else if (cp != 0x30FB && !cls) die_at(w->line, "'%s': U+%04X is not a character of a user-defined word (2023 8.1.3, Annex B)", w->s, cp);
            else if (cls == 2 && first) die_at(w->line, "'%s': U+%04X does not begin a user-defined word (2023 Annex B item 2)", w->s, cp);
            unsigned lc = ext_lower(cp);
            if (lc != cp) lower = 1;
            o += utf8_put(lc, out + o);
        } else { if (isupper(cp)) lower = 1; out[o++] = (char)tolower((int)cp); }
        i += len; first = 0;
    }
    out[o] = 0;
    if (lower) { w->orig = w->s; w->s = out; } else free(out);
}

/* a word is matched lowercased; its spelling is kept for the names the
 * program can see at run time (EXCEPTION-LOCATION, EXCEPTION-FILE) */
static void word_lower(Tok *w)
{
    int up = 0, ext = 0;
    for (char *k = w->s; *k; k++) { if (isupper((unsigned char)*k)) up = 1; if (*k & 0x80) ext = 1; }
    if (ext) { word_extended(w); return; }
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
        uint32_t cp = 0;
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

/* a floating-point literal (2023 8.3.3.3.3): the significand, 1 to 36
 * digits with a point, times ten to the exponent, at most four digits --
 * checked (rules 2-4), then written as the fixed-point literal it is
 * worth, exactly, for every reader of a T_NUM: 1.5E+3 is 1500, 1.5E-3 is
 * 0.0015.  The exponent's range is the implementor's (rule 3): here the
 * one that keeps the value within the 31 digits a numeric literal has. */
static char *numlit_float_fixed(const char *p, int n, int line)
{
    const char *e = p; while (e < p + n && *e != 'e' && *e != 'E') e++;
    int sd = 0, zero = 1, neg = *p == '-';
    for (const char *k = p; k < e; k++) if (isdigit((unsigned char)*k)) { sd++; if (*k != '0') zero = 0; }
    if (sd > 36) die_at(line, "a floating-point literal's significand has at most 36 digits (2023 8.3.3.3.3 rule 2)");
    int xd = 0, xz = 1, xneg = e[1] == '-', ex = 0;
    for (const char *k = e + 1; k < p + n; k++) if (isdigit((unsigned char)*k)) { xd++; if (*k != '0') xz = 0; ex = ex * 10 + (*k - '0'); }
    if (xd > 4) die_at(line, "a floating-point literal's exponent has at most four digits (2023 8.3.3.3.3 rule 3)");
    if (zero && (!xz || neg || xneg)) die_at(line, "a floating-point literal whose significand is zero has a zero exponent and no minus sign (2023 8.3.3.3.3 rule 4)");
    /* the significand's digits and scale, the exponent moving the point */
    char d[48]; int nd = 0, scale = 0, seen = 0;
    for (const char *k = p; k < e; k++) { if (*k == '.') seen = 1; else if (isdigit((unsigned char)*k)) { d[nd++] = *k; if (seen) scale++; } }
    if (xneg) scale += ex;
    else if (ex <= scale) scale -= ex;
    else { int z = ex - scale; if (nd + z > 36) die_at(line, "the floating-point literal's value has more than 31 digits (its exponent range here: 2023 8.3.3.3.3 rule 3)"); memset(d + nd, '0', (size_t)z); nd += z; scale = 0; }
    /* leading zeros of the integer part, trailing ones of the fraction */
    int lead = 0; while (lead < nd - scale - 1 && d[lead] == '0') lead++;
    while (scale > 0 && d[nd - 1] == '0') { nd--; scale--; }
    int ip = nd - lead - scale;
    if ((ip > 0 ? ip : 0) + scale > 31) die_at(line, "the floating-point literal's value has more than 31 digits (its exponent range here: 2023 8.3.3.3.3 rule 3)");
    char *out = xmalloc(64); int o = 0;
    if (neg) out[o++] = '-';
    if (ip > 0) { memcpy(out + o, d + lead, (size_t)ip); o += ip; }
    if (scale > 0) {
        if (ip <= 0) out[o++] = '0';
        out[o++] = '.';
        for (int z = ip; z < 0; z++) out[o++] = '0';                 /* 1.5E-3: the point's zeros before the digits */
        int fd = nd - lead - (ip > 0 ? ip : 0);                     /* the fraction's own digits */
        memcpy(out + o, d + lead + (ip > 0 ? ip : 0), (size_t)fd); o += fd;
    }
    if (ip <= 0 && scale == 0) out[o++] = '0';
    out[o] = 0;
    return out;
}

/* a literal lexeme (lex.rl: its prefix letters, the quotes, the text with
 * doubled quotes) into a T_STR token: hexadecimal, national, boolean or
 * plain, with the edition and length rules */
static void push_literal(const Lexeme *l, int line)
{
    const char *p = l->s;
    int c = (unsigned char)p[0];
    if (l->bad) die_at(line, l->prefix == 1 && (c == 'x' || c == 'X') ? "unterminated hexadecimal literal" :
                             l->prefix && (c == 'n' || c == 'N') ? "unterminated national literal" :
                             l->prefix && (c == 'b' || c == 'B') ? "unterminated boolean literal" : "unterminated literal");
    /* A zero-length literal is COBOL 2014's: 1985 has 1 through 160
     * characters, 2002 more than zero (8.3.1.2.1.2 rule 1, X"" too; .3.2
     * rule 1 boolean, .4.2 rule 1 national) */
    #define NO_EMPTY_LIT(n, what, rule) do { if ((n) == 0 && g_std < 2014) die_at(line, "a zero-length %s literal is COBOL 2014 (%s); compile with -std=2014", what, \
        g_std < 2002 ? "X3.23-1985: 1 through 160 characters" : rule); \
        if ((n) > 8191) die_at(line, "this %s literal has %d positions, more than 8,191, the most any edition allows (%s)", what, (int)(n), \
        g_std < 2002 ? "X3.23-1985 nonnumeric literals allow 160" : rule); \
        if ((n) > 160) bp(BP_E20_LONG_LITERAL, line); } while (0)   /* 1985 and 2002 say 160: taken (preservation) */
    if (l->prefix == 1 && (c == 'x' || c == 'X')) {
        /* Hexadecimal literal X'..' */
        const char *s0 = p + 2, *e = l->s + l->len - 1;
        int n = (int)(e - s0);
        if (n & 1) die_at(line, "hexadecimal literal needs an even number of digits");
        if (g_std < 2002) bp(BP_E8_HEX_LITERAL, line);
        NO_EMPTY_LIT(n, "hexadecimal", "2002 8.3.1.2.1.2 rule 1");
        char *bytes = xmalloc(n / 2 + 1);
        for (int i = 0; i < n; i += 2) {
            int h = hexval(s0[i]), lo = hexval(s0[i + 1]);
            if (h < 0 || lo < 0) die_at(line, "bad hexadecimal digit in literal");
            bytes[i / 2] = (char)(h * 16 + lo);
        }
        push_tok(T_STR, line, bytes, n / 2)->hex = 1;
        free(bytes);
        return;
    }
    if (l->prefix && (c == 'n' || c == 'N')) {
        /* National literals (2023 8.3.3.5): N"..." in the source's
         * UTF-8, NX"..." as hexadecimal code units; both stored UTF-16BE */
        if (g_std < 2002) die_at(line, "national literals (N\"...\") are COBOL 2002; compile with -std=2002");
        int hex = l->prefix == 2;
        char q = p[l->prefix];
        const char *s0 = p + l->prefix + 1, *e = l->s + l->len - 1;
        char *raw = xmalloc((size_t)l->len + 1); int rn = 0;
        for (const char *k = s0; k < e; k++) { raw[rn++] = *k; if (*k == q) k++; }   /* a doubled quote is one */
        char *out; int on;
        if (hex) {
            if (rn % 4) die_at(line, "NX\"...\" needs four hexadecimal digits for each national character (2023 8.3.3.5.3 rule 5: UTF-16 here)");
            out = xmalloc((size_t)rn / 2 + 1); on = rn / 2;
            for (int i = 0; i < rn; i += 2) {
                int h = hexval(raw[i]), lo = hexval(raw[i + 1]);
                if (h < 0 || lo < 0) die_at(line, "bad hexadecimal digit in a national literal (2023 8.3.3.5.3 rule 4)");
                out[i / 2] = (char)(h * 16 + lo);
            }
        } else {
            out = xmalloc((size_t)rn * 4 + 1); on = utf8_to_utf16be((const unsigned char *)raw, rn, (unsigned char *)out);
            if (on < 0) die_at(line, "a national literal must be UTF-8 text (the source is UTF-8)");
        }
        NO_EMPTY_LIT(on / 2, "national", "2002 8.3.1.2.4.2 rule 1");
        Tok *nt = push_tok(T_STR, line, out, on);
        nt->nat = 1;
        free(raw); free(out);
        return;
    }
    if (l->prefix && (c == 'b' || c == 'B')) {
        /* Boolean literals (2023 8.3.3.4): B"0101", BX"5"; held as one
         * character 0 or 1 per boolean position */
        if (g_std < 2002) die_at(line, "boolean literals (B\"...\") are COBOL 2002; compile with -std=2002");
        int hex = l->prefix == 2;
        const char *s0 = p + l->prefix + 1, *e = l->s + l->len - 1;
        int n = (int)(e - s0);
        char *out = xmalloc((size_t)n * 4 + 1); int on = 0;
        for (int i = 0; i < n; i++) {
            if (hex) {
                int h = hexval(s0[i]);
                if (h < 0) die_at(line, "bad hexadecimal digit in a boolean literal (2023 8.3.3.4.3 rule 3)");
                for (int b = 3; b >= 0; b--) out[on++] = (char)('0' + ((h >> b) & 1));
            } else {
                if (s0[i] != '0' && s0[i] != '1') die_at(line, "a boolean literal holds only the characters 0 and 1 (2023 8.3.3.4.3 rule 2)");
                out[on++] = s0[i];
            }
        }
        NO_EMPTY_LIT(on, "boolean", "2002 8.3.1.2.3.2 rule 1");
        Tok *bt = push_tok(T_STR, line, out, on);
        bt->boolv = 1;
        free(out);
        return;
    }
    if (l->prefix) die_at(line, "%c'...' literals are not in COBOL 85", toupper(c));
    /* Nonnumeric literal, with the doubled-quote escape */
    {
        char q = (char)c;
        char *out = xmalloc((size_t)l->len + 1);
        int n = 0;
        const char *e = l->s + l->len - 1;
        for (const char *k = p + 1; k < e; k++) { out[n++] = *k; if (*k == q) k++; }
        NO_EMPTY_LIT(n, "alphanumeric", "2002 8.3.1.2.1.2 rule 1");
        push_tok(T_STR, line, out, n);
        free(out);
    }
}

/* >>COBOL-WORDS over one word (2023 7.3.10.4): a synonym or substitute
 * becomes the standard word (its spelling kept in orig for messages); an
 * undefined or substituted-for word is marked a user word */
static void cobol_words_apply(Tok *w)
{
    for (int i = 0; i < g_ncw; i++) {
        CobolWord *c = &g_cw[i];
        if ((c->kind == CW_EQUATE || c->kind == CW_SUBSTITUTE) && !strcmp(w->s, c->b)) {
            if (!w->orig) w->orig = w->s;
            w->s = xstrndup(c->a, (int)strlen(c->a));
            return;
        }
        if ((c->kind == CW_UNDEFINE || c->kind == CW_SUBSTITUTE) && !strcmp(w->s, c->a)) { w->uw = 1; return; }
    }
}
static void tokenize_lines(SrcLine *lines, int nlines)
{
    static int sql_decl_tok;    /* between EXEC SQL BEGIN and END DECLARE SECTION */
    int pic_ctx = 0;    /* after PIC/PICTURE [IS]: the next token is a picture */
    for (int li = 0; li < nlines; li++) {
        const char *t = lines[li].text;
        int line = lines[li].line;
        const char *p = t, *pe = t + strlen(t);
        g_tok_dbg = lines[li].dbg; g_tok_ff = lines[li].ff;
        if (lines[li].file) g_tok_file = lines[li].file;
        if (lines[li].dir) { push_tok(T_DIR, line, t, (int)strlen(t)); continue; }
        /* a sign or a leading point begins a number at a boundary only:
         * the line's start, after a space, a '(' or an '=' */
        int boundary = 1;
        while (p < pe) {
            if (pic_ctx) {
                if (*p == ' ' || *p == '\t') { p++; continue; }
                /* A picture runs to the next space; a period is part of it
                 * unless it is the last character before that space, in which
                 * case it is the sentence separator. */
                const char *q = p;
                while (q < pe && *q != ' ' && *q != '\t' && !(q[0] == '=' && q[1] == '=')) q++;   /* == ends pseudo-text */
                int n = (int)(q - p);
                int sep = 0;
                if (n > 1 && p[n - 1] == '.') { n--; sep = 1; }
                else if (n > 1 && (p[n - 1] == ';' || p[n - 1] == ',')) n--;    /* a separator, not a symbol: PICTURE 99; VALUE 8 */
                push_tok(T_PIC, line, p, n);
                if (sep) push_tok(T_PERIOD, line, ".", 1);
                p = q;
                pic_ctx = 0;
                boundary = 1;
                continue;
            }
            Lexeme l;
            lx_next(p, pe, &l);
            const char *next = p + l.len;
            int c = (unsigned char)*p;
            switch (l.kind) {
            case LX_SPACE: boundary = 1; p = next; continue;
            case LX_COMMENT: p = pe; continue;
            case LX_LIT: push_literal(&l, line); break;
            case LX_NUM:
                if ((c == '+' || c == '-' || c == '.') && !boundary) {
                    /* the sign or point is not a number's here: an operator
                     * (A-1 is A minus 1), or a period glued to what follows */
                    if (c == '.') die_at(line, "a period must be followed by a space or the end of the line");
                    push_tok(T_OP, line, p, 1);
                    next = p + 1;
                    break;
                }
                if (l.exp) {
                    if (g_std < 2002) die_at(line, "floating-point literals (1.5E+3) are COBOL 2002 (2023 8.3.3.3.3); compile with -std=2002");
                    char *fx = numlit_float_fixed(p, l.len, line);
                    push_tok(T_NUM, line, fx, (int)strlen(fx));
                    free(fx);
                    break;
                }
                push_tok(T_NUM, line, p, l.len);
                break;
            case LX_WORD: {
                const char *e = next;
                if (l.len == 4 && !strncasecmp(p, "exec", 4)) {
                    const char *r = take_exec_sql(lines, nlines, &li, e, line);
                    if (r) {
                        /* inside a DECLARE SECTION a character set name may be
                         * qualified (CTS1.CS): the tokenizer keeps it one word */
                        const char *sq = g_tok[g_ntok - 1].s;
                        if (!strncasecmp(sq, "begin declare section", 21)) sql_decl_tok = 1;
                        else if (!strncasecmp(sq, "end declare section", 19)) sql_decl_tok = 0;
                        p = r; t = lines[li].text; pe = t + strlen(t); line = lines[li].line; boundary = 1; continue;
                    }
                }
                if (sql_decl_tok) while (e < pe && *e == '.' && isalpha((unsigned char)e[1])) { e++; while (is_wordch((unsigned char)*e)) e++; }
                Tok *w = push_tok(T_WORD, line, p, (int)(e - p));
                word_lower(w);
                if (g_ncw) cobol_words_apply(w);
                if (!w->uw && (!strcmp(w->s, "pic") || !strcmp(w->s, "picture"))) pic_ctx = 1;
                next = e;
                if (pic_ctx) {
                    const char *q = e;
                    while (*q == ' ' || *q == '\t') q++;
                    if ((q[0] == 'i' || q[0] == 'I') && (q[1] == 's' || q[1] == 'S') &&
                        (q[2] == ' ' || q[2] == '\t' || !q[2])) next = q + 2;     /* PICTURE IS at a line's end, the picture below (NC107A) */
                }
                break;
            }
            case LX_PERIOD: push_tok(T_PERIOD, line, ".", 1); break;
            case LX_DOT:
                /* ".00-EXIT": a period with no space after it (a leading
                 * point begins a number at a boundary; that case is LX_NUM) */
                die_at(line, "a period must be followed by a space or the end of the line");
            case LX_SEP: g_pending_comma = 1; break;
            case LX_COMMA:
                if (c == ',' && p > t && isdigit((unsigned char)p[-1]) && isdigit((unsigned char)p[1])) {
                    /* a comma tight between digits: the decimal point under
                     * DECIMAL-POINT IS COMMA, settled once the whole text is in */
                    push_tok(T_OP, line, ",", 1);
                    break;
                }
                bp(BP_E22_SEPARATOR_SPACE, line);        /* "... is ",SQL-COD (the NIST SQL suite) */
                g_pending_comma = 1;
                break;
            case LX_LP: push_tok(T_LP, line, "(", 1); break;
            case LX_RP: push_tok(T_RP, line, ")", 1); break;
            case LX_COLON: push_tok(T_COLON, line, ":", 1); break;
            case LX_PDELIM: push_tok(T_OP, line, "==", 2); break;      /* pseudo-text delimiter */
            case LX_OP: push_tok(T_OP, line, p, l.len); break;
            default: die_at(line, "unexpected character '%c'", c);
            }
            /* what may begin a number next: a sign after '(' or '=', a
             * point likewise (the old rule: the character before it) */
            boundary = l.kind == LX_LP || (l.kind == LX_OP && l.len == 1 && c == '=');
            p = next;
        }
    }
}
