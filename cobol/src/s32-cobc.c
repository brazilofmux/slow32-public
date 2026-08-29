/* s32-cobc -- COBOL 85 for SLOW-32.  Host cross-compiler.
 *
 * Reads ANSI X3.23-1985 COBOL (fixed or free reference format) plus the
 * implementor modules listed in docs/dialect.md, and emits SLOW-32
 * assembler for slow32asm / s32-ld.  Not SSA, not BURG: the IR is the
 * symbol table (Sym[]), and each verb is a lowering against it -- an inline
 * sequence for the hot cases, otherwise a call into libcob with a
 * descriptor the compiler built.  docs/architecture.md.
 *
 * Stage 1 (docs/plan.md): reader for both formats, tokenizer, the four
 * divisions with WORKING-STORAGE elementary items, DISPLAY, STOP RUN,
 * GOBACK, and a refusal with a message for everything else.  Unimplemented
 * is a diagnostic, never silence.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include "picture.h"

#define VERSION "0.1 (stage 1)"

/* ====================================================================== */
/* Diagnostics                                                             */
/* ====================================================================== */

static const char *g_file = "?";
static int g_free = 0;              /* -free: majesty; default is fixed */

static void die_at(int line, const char *fmt, ...)
{
    va_list ap;
    fprintf(stderr, "%s:%d: error: ", g_file, line);
    va_start(ap, fmt); vfprintf(stderr, fmt, ap); va_end(ap);
    fputc('\n', stderr);
    exit(1);
}

static void *xmalloc(size_t n)
{
    void *p = calloc(1, n ? n : 1);
    if (!p) { fprintf(stderr, "s32-cobc: out of memory\n"); exit(2); }
    return p;
}

static char *xstrndup(const char *s, int n)
{
    char *p = xmalloc(n + 1);
    memcpy(p, s, n); p[n] = 0;
    return p;
}

/* ====================================================================== */
/* Source reader: reference formats                                        */
/* ====================================================================== */

/* Fixed: columns 1-6 sequence, 7 indicator, 8-72 program text, 73+ ignored.
 * Free (GnuCOBOL -free, majesty): the whole line is text.  Comments:
 * '*' or '/' in column 7 (fixed); '*>' to end of line (both -- the floating
 * comment is post-85 but majesty is written with it and it is harmless).
 * A program is one format or the other, chosen on the command line. */

typedef struct { char *text; int line; } SrcLine;
static SrcLine *g_lines;
static int g_nlines;

static void read_source(const char *path)
{
    FILE *f = fopen(path, "rb");
    if (!f) { fprintf(stderr, "s32-cobc: cannot open %s\n", path); exit(1); }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = xmalloc(sz + 1);
    if (fread(buf, 1, sz, f) != (size_t)sz) { fprintf(stderr, "s32-cobc: read error\n"); exit(1); }
    fclose(f);
    buf[sz] = 0;

    int cap = 256;
    g_lines = xmalloc(cap * sizeof *g_lines);
    int lineno = 0;
    char *p = buf;
    while (*p) {
        char *e = strchr(p, '\n');
        int len = e ? (int)(e - p) : (int)strlen(p);
        lineno++;
        if (len && p[len - 1] == '\r') len--;
        char *text = NULL;
        if (g_free) {
            text = xstrndup(p, len);
        } else {
            if (len > 6) {
                char ind = p[6];
                if (ind == '*' || ind == '/') text = NULL;         /* comment */
                else if (ind == 'D' || ind == 'd') text = NULL;    /* debugging line */
                else if (ind == '-')
                    die_at(lineno, "continuation lines are not implemented yet");
                else if (ind != ' ')
                    die_at(lineno, "unrecognised indicator '%c' in column 7 "
                           "(free-format source? compile it with -free)", ind);
                else {
                    int n = len - 7;
                    if (n > 65) n = 65;                             /* 8..72 */
                    text = xstrndup(p + 7, n);
                }
            }
        }
        if (text) {
            if (g_nlines == cap) { cap *= 2; g_lines = realloc(g_lines, cap * sizeof *g_lines); }
            g_lines[g_nlines].text = text;
            g_lines[g_nlines].line = lineno;
            g_nlines++;
        }
        if (!e) break;
        p = e + 1;
    }
    free(buf);
}

/* ====================================================================== */
/* Tokenizer                                                               */
/* ====================================================================== */

enum { T_EOF, T_WORD, T_NUM, T_STR, T_PIC, T_PERIOD, T_LP, T_RP, T_COLON, T_OP };

typedef struct {
    int kind, line;
    char *s;        /* word (lowercased), number text, literal bytes, picture, op */
    int len;        /* literal byte length (literals may hold NULs) */
} Tok;

static Tok *g_tok;
static int g_ntok, g_tcap;

static Tok *push_tok(int kind, int line, const char *s, int len)
{
    if (g_ntok == g_tcap) { g_tcap = g_tcap ? g_tcap * 2 : 1024; g_tok = realloc(g_tok, g_tcap * sizeof *g_tok); }
    Tok *t = &g_tok[g_ntok++];
    t->kind = kind; t->line = line; t->s = xstrndup(s, len); t->len = len;
    return t;
}

static int is_wordch(int c) { return isalnum(c) || c == '-' || c == '_'; }

static int hexval(int c)
{
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

static void tokenize(void)
{
    int pic_ctx = 0;    /* after PIC/PICTURE [IS]: the next token is a picture */
    for (int li = 0; li < g_nlines; li++) {
        const char *t = g_lines[li].text;
        int line = g_lines[li].line;
        const char *p = t;
        while (*p) {
            if (*p == ' ' || *p == '\t') { p++; continue; }
            if (p[0] == '*' && p[1] == '>') break;            /* comment to EOL */

            if (pic_ctx) {
                /* A picture runs to the next space; a period is part of it
                 * unless it is the last character before that space, in which
                 * case it is the sentence separator. */
                const char *q = p;
                while (*q && *q != ' ' && *q != '\t') q++;
                int n = (int)(q - p);
                int sep = 0;
                if (n > 1 && p[n - 1] == '.') { n--; sep = 1; }
                push_tok(T_PIC, line, p, n);
                if (sep) push_tok(T_PERIOD, line, ".", 1);
                p = q;
                pic_ctx = 0;
                continue;
            }

            int c = (unsigned char)*p;

            /* Hexadecimal literal X'..' */
            if ((c == 'x' || c == 'X') && (p[1] == '\'' || p[1] == '"')) {
                char q = p[1];
                const char *s = p + 2, *e = s;
                while (*e && *e != q) e++;
                if (!*e) die_at(line, "unterminated hexadecimal literal");
                int n = (int)(e - s);
                if (n & 1) die_at(line, "hexadecimal literal needs an even number of digits");
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
            if ((c == 'z' || c == 'Z' || c == 'n' || c == 'N') && (p[1] == '\'' || p[1] == '"'))
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
                push_tok(T_STR, line, out, n);
                free(out);
                p = s + 1;
                continue;
            }

            /* Numeric literal: [+-]digits[.digits], sign only when it stands
             * at a word boundary.  A run of digits followed by more word
             * characters (0100-main, 9000-end) is a user-word. */
            int signed_num = (c == '+' || c == '-') && isdigit((unsigned char)p[1]) &&
                             (p == t || p[-1] == ' ' || p[-1] == '\t' || p[-1] == '(');
            if (isdigit(c) || signed_num) {
                const char *s = p + (signed_num ? 1 : 0), *e = s;
                while (isdigit((unsigned char)*e)) e++;
                if (*e == '.' && isdigit((unsigned char)e[1])) { e++; while (isdigit((unsigned char)*e)) e++; }
                if (is_wordch((unsigned char)*e) && !signed_num) {
                    /* user-word beginning with digits */
                    e = p; while (is_wordch((unsigned char)*e)) e++;
                    Tok *w = push_tok(T_WORD, line, p, (int)(e - p));
                    for (char *k = w->s; *k; k++) *k = (char)tolower((unsigned char)*k);
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
                Tok *w = push_tok(T_WORD, line, p, (int)(e - p));
                for (char *k = w->s; *k; k++) *k = (char)tolower((unsigned char)*k);
                if (!strcmp(w->s, "pic") || !strcmp(w->s, "picture")) pic_ctx = 1;
                p = e;
                /* PIC IS 9(3): let IS through without leaving picture context */
                if (pic_ctx) {
                    const char *q = p;
                    while (*q == ' ' || *q == '\t') q++;
                    if ((q[0] == 'i' || q[0] == 'I') && (q[1] == 's' || q[1] == 'S') &&
                        (q[2] == ' ' || q[2] == '\t')) p = q + 2;
                }
                continue;
            }

            if (c == '.') {
                if (p[1] == 0 || p[1] == ' ' || p[1] == '\t' || (p[1] == '*' && p[2] == '>')) {
                    push_tok(T_PERIOD, line, ".", 1); p++; continue;
                }
                die_at(line, "a period must be followed by a space or the end of the line");
            }
            if (c == ',' || c == ';') {
                if (p[1] == 0 || p[1] == ' ' || p[1] == '\t') { p++; continue; }
                die_at(line, "'%c' is a separator only when followed by a space", c);
            }
            if (c == '(') { push_tok(T_LP, line, "(", 1); p++; continue; }
            if (c == ')') { push_tok(T_RP, line, ")", 1); p++; continue; }
            if (c == ':') { push_tok(T_COLON, line, ":", 1); p++; continue; }
            if (c == '*' && p[1] == '*') { push_tok(T_OP, line, "**", 2); p += 2; continue; }
            if ((c == '>' || c == '<') && p[1] == '=') { push_tok(T_OP, line, p, 2); p += 2; continue; }
            if (strchr("=<>+-*/", c)) { push_tok(T_OP, line, p, 1); p++; continue; }
            die_at(line, "unexpected character '%c'", c);
        }
    }
    push_tok(T_EOF, g_nlines ? g_lines[g_nlines - 1].line : 1, "", 0);
}

/* ---- cursor ---------------------------------------------------------- */

static int g_tp;

static Tok *cur(void)  { return &g_tok[g_tp]; }
static Tok *peek(int k){ int i = g_tp + k; if (i >= g_ntok) i = g_ntok - 1; return &g_tok[i]; }
static void advance(void) { if (g_tp < g_ntok - 1) g_tp++; }

static int is_word(Tok *t, const char *w) { return t->kind == T_WORD && !strcmp(t->s, w); }
static int at_word(const char *w) { return is_word(cur(), w); }
static int accept_word(const char *w) { if (at_word(w)) { advance(); return 1; } return 0; }

static const char *tok_desc(Tok *t)
{
    static char b[96];
    switch (t->kind) {
    case T_EOF:    return "end of file";
    case T_PERIOD: return "'.'";
    case T_STR:    snprintf(b, sizeof b, "literal '%.*s'", t->len > 40 ? 40 : t->len, t->s); return b;
    case T_PIC:    snprintf(b, sizeof b, "picture '%s'", t->s); return b;
    default:       snprintf(b, sizeof b, "'%s'", t->s); return b;
    }
}

static void expect_word(const char *w)
{
    if (!accept_word(w)) die_at(cur()->line, "expected '%s', found %s", w, tok_desc(cur()));
}

static void expect_period(void)
{
    if (cur()->kind != T_PERIOD) die_at(cur()->line, "expected '.', found %s", tok_desc(cur()));
    advance();
}

/* ====================================================================== */
/* Symbol table -- the IR                                                  */
/* ====================================================================== */

enum {
    U_DISPLAY, U_BINARY, U_PACKED, U_COMP5,
    U_SINT, U_UINT, U_SSHORT, U_USHORT, U_BCHAR, U_UBCHAR, U_POINTER, U_INDEX
};

static const char *usage_name(int u)
{
    static const char *n[] = { "display", "comp", "comp-3", "comp-5", "signed-int",
        "unsigned-int", "signed-short", "unsigned-short", "binary-char",
        "binary-char unsigned", "pointer", "index" };
    return n[u];
}

typedef struct Sym {
    char name[64];
    int  level, line, is_filler;
    int  usage, has_usage;
    int  has_pic;
    char pic[PIC_MAXPAT];
    PicInfo pi;
    int  size;
    int  has_value;
    unsigned char *value;       /* size bytes */
    char label[80];             /* assembler label for the storage */
} Sym;

static Sym *g_sym;
static int g_nsym, g_scap;

static Sym *sym_new(void)
{
    if (g_nsym == g_scap) { g_scap = g_scap ? g_scap * 2 : 64; g_sym = realloc(g_sym, g_scap * sizeof *g_sym); }
    Sym *s = &g_sym[g_nsym++];
    memset(s, 0, sizeof *s);
    return s;
}

static Sym *sym_find(const char *name)
{
    for (int i = 0; i < g_nsym; i++)
        if (!g_sym[i].is_filler && !strcmp(g_sym[i].name, name)) return &g_sym[i];
    return NULL;
}

static char g_progid[64];

/* ====================================================================== */
/* Numeric literals                                                        */
/* ====================================================================== */

typedef struct {
    int neg;
    char digits[40];    /* all digits, no point, no leading sign */
    int ndigits, scale; /* scale = digits after the point */
} NumLit;

static void numlit_parse(Tok *t, NumLit *n)
{
    memset(n, 0, sizeof *n);
    const char *s = t->s;
    if (*s == '+' || *s == '-') { n->neg = (*s == '-'); s++; }
    int seen = 0;
    for (; *s; s++) {
        if (*s == '.') { seen = 1; continue; }
        if (n->ndigits >= 36) die_at(t->line, "numeric literal too long");
        n->digits[n->ndigits++] = *s;
        if (seen) n->scale++;
    }
    /* strip leading zeros, keeping at least one digit */
    int lead = 0;
    while (lead < n->ndigits - 1 && n->digits[lead] == '0' && lead + n->scale < n->ndigits) lead++;
    (void)lead;
    if (n->ndigits - n->scale > 18 || n->ndigits > 36)
        die_at(t->line, "numeric literal has more than 18 digits");
}

/* Value of the literal scaled to `scale` decimal places, as digit text
 * `out` of exactly `digits` characters (right-aligned, zero-filled).
 * Returns 0 if the integer part does not fit. */
static int numlit_align(const NumLit *n, int digits, int scale, char *out)
{
    int int_digits = n->ndigits - n->scale;       /* digits left of the point */
    int want_int = digits - scale;
    memset(out, '0', digits);
    /* integer part */
    for (int i = 0; i < int_digits; i++) {
        int pos = want_int - int_digits + i;
        if (pos < 0) { if (n->digits[i] != '0') return 0; continue; }
        out[pos] = n->digits[i];
    }
    /* fraction, truncated to the receiving scale */
    for (int i = 0; i < n->scale && i < scale; i++)
        out[want_int + i] = n->digits[int_digits + i];
    return 1;
}

/* ====================================================================== */
/* Data Division                                                           */
/* ====================================================================== */

static int binary_bytes(int digits, int usage)
{
    /* The standard leaves COMP size to the implementor.  For COMP, two,
     * four and eight bytes by digit count is the IBM convention and what
     * the SLOW-32 C ABI's own types make natural.  COMP-5 is GnuCOBOL's
     * usage (via Micro Focus), so it takes GnuCOBOL's default 1-2-4-8 and
     * its rule that the item holds the binary field's full capacity, not
     * just the picture's digits -- majesty's `pic 9 comp-5` is one byte
     * there.  docs/dialect.md. */
    if (usage == U_COMP5 && digits <= 2) return 1;
    if (digits <= 4) return 2;
    if (digits <= 9) return 4;
    return 8;
}

/* Digits a native binary field can show: what DISPLAY of a COMP-5 or
 * C-ABI item prints (GnuCOBOL convention). */
static int capacity_digits(int bytes)
{
    return bytes == 1 ? 3 : bytes == 2 ? 5 : bytes == 4 ? 10 : 19;
}

static void sym_finish(Sym *s)
{
    int u = s->usage;
    int native = (u == U_SINT || u == U_UINT || u == U_SSHORT || u == U_USHORT ||
                  u == U_BCHAR || u == U_UBCHAR || u == U_POINTER || u == U_INDEX);

    if (!s->has_pic && !native)
        die_at(s->line, "'%s' has no PICTURE clause", s->name);
    if (s->has_pic && native)
        die_at(s->line, "'%s': USAGE %s takes no PICTURE", s->name, usage_name(u));

    if (native) {
        /* C ABI types: the implementor module for CALL of existing C. */
        switch (u) {
        case U_SINT:   s->size = 4; s->pi.digits = 10; s->pi.is_signed = 1; break;
        case U_UINT:   s->size = 4; s->pi.digits = 10; break;
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
    switch (u) {
    case U_DISPLAY:
        s->size = pi->bytes;
        break;
    case U_BINARY: case U_COMP5:
        if (pi->category != PIC_NUMERIC)
            die_at(s->line, "'%s': USAGE %s needs a numeric PICTURE", s->name, usage_name(u));
        s->size = binary_bytes(pi->digits, u);
        break;
    case U_PACKED:
        if (pi->category != PIC_NUMERIC)
            die_at(s->line, "'%s': USAGE COMP-3 needs a numeric PICTURE", s->name);
        s->size = pi->digits / 2 + 1;
        break;
    }
}

/* Encode a numeric literal into the item's storage. */
static void store_numeric(Sym *s, const NumLit *n, int line)
{
    const PicInfo *pi = &s->pi;
    int digits = pi->digits, scale = pi->scale;
    char d[40];
    if (!numlit_align(n, digits, scale, d))
        die_at(line, "VALUE %s%.*s does not fit PICTURE of '%s'", n->neg ? "-" : "",
               n->ndigits, n->digits, s->name);
    int neg = n->neg && pi->is_signed;      /* unsigned items take the magnitude */

    switch (s->usage) {
    case U_DISPLAY:
        memcpy(s->value, d, digits);
        if (neg) s->value[digits - 1] = (unsigned char)(d[digits - 1] - '0' + 'p');
        break;
    case U_PACKED: {
        /* digits, high nibble first, padded with a leading zero nibble when
         * the count is even; sign nibble last */
        int nb = s->size, k = 0;
        int pad = (digits % 2 == 0);
        unsigned char *v = s->value;
        memset(v, 0, nb);
        if (pad) { v[0] = (unsigned char)(d[0] - '0'); k = 1; }
        for (int i = k ? 1 : 0, j = pad ? 1 : 0; i < digits; ) {
            int hi = d[i++] - '0';
            int lo = (i < digits) ? d[i++] - '0' : (pi->is_signed ? (neg ? 0xD : 0xC) : 0xF);
            v[j++] = (unsigned char)(hi * 16 + lo);
        }
        if (pad) {
            /* even digit count: the last byte holds the last digit and the sign */
            v[nb - 1] = (unsigned char)((d[digits - 1] - '0') * 16 + (pi->is_signed ? (neg ? 0xD : 0xC) : 0xF));
        }
        break;
    }
    default: {
        /* binary: two's complement little-endian of the scaled value */
        unsigned long long mag = 0;
        for (int i = 0; i < digits; i++) mag = mag * 10 + (d[i] - '0');
        if (s->size < 8 && mag >> (s->size * 8 - (pi->is_signed ? 1 : 0)))
            die_at(line, "VALUE does not fit the %d-byte binary item '%s'", s->size, s->name);
        long long v = neg ? -(long long)mag : (long long)mag;
        for (int i = 0; i < s->size; i++) s->value[i] = (unsigned char)(v >> (8 * i));
        break;
    }
    }
}

static void default_init(Sym *s)
{
    /* The standard leaves uninitialised WORKING-STORAGE undefined.  The
     * implementor rule here is GnuCOBOL's: alphanumeric to spaces, numeric
     * to zero -- majesty was written against it. */
    s->value = xmalloc(s->size);
    if (s->usage == U_DISPLAY && s->pi.category != PIC_NUMERIC) {
        memset(s->value, ' ', s->size);
    } else {
        NumLit z; memset(&z, 0, sizeof z); z.digits[0] = '0'; z.ndigits = 1;
        if (s->usage == U_DISPLAY) memset(s->value, '0', s->size);
        else if (s->usage == U_PACKED) store_numeric(s, &z, s->line);
        else memset(s->value, 0, s->size);
    }
}

static int parse_level(void)
{
    Tok *t = cur();
    if (t->kind != T_NUM) return -1;
    int all = 1;
    for (char *k = t->s; *k; k++) if (!isdigit((unsigned char)*k)) all = 0;
    if (!all || strlen(t->s) > 2) return -1;
    return atoi(t->s);
}

static void parse_data_item(void)
{
    int line = cur()->line;
    int level = parse_level();
    if (level < 0) die_at(line, "expected a level number, found %s", tok_desc(cur()));
    advance();

    if (level == 66) die_at(line, "level 66 (RENAMES) is not implemented yet");
    if (level == 88) die_at(line, "level 88 condition-names are not implemented yet (stage 2)");
    if (level != 1 && level != 77) {
        if (level >= 2 && level <= 49)
            die_at(line, "group items (level %02d) are not implemented yet (stage 2)", level);
        die_at(line, "level number %d is not valid", level);
    }

    Sym *s = sym_new();
    s->level = level; s->line = line; s->usage = U_DISPLAY;
    if (accept_word("filler")) {
        s->is_filler = 1;
        snprintf(s->name, sizeof s->name, "filler");
    } else if (cur()->kind == T_WORD) {
        if (sym_find(cur()->s)) die_at(line, "'%s' is already declared", cur()->s);
        snprintf(s->name, sizeof s->name, "%s", cur()->s);
        advance();
    } else if (cur()->kind == T_PERIOD) {
        s->is_filler = 1;                       /* 85 lets the name be omitted */
        snprintf(s->name, sizeof s->name, "filler");
    } else die_at(line, "expected a data-name, found %s", tok_desc(cur()));

    Tok *value_tok = NULL; int value_all = 0; int value_fig = 0; const char *fig = NULL;

    while (cur()->kind != T_PERIOD) {
        Tok *t = cur();
        if (t->kind != T_WORD) die_at(t->line, "unexpected %s in the description of '%s'", tok_desc(t), s->name);

        if (!strcmp(t->s, "pic") || !strcmp(t->s, "picture")) {
            advance();
            if (cur()->kind != T_PIC) die_at(t->line, "expected a PICTURE character-string");
            if (s->has_pic) die_at(t->line, "'%s' has two PICTURE clauses", s->name);
            s->has_pic = 1;
            snprintf(s->pic, sizeof s->pic, "%s", cur()->s);
            if (pic_analyse(s->pic, &s->pi) < 0) die_at(t->line, "'%s': %s", s->name, s->pi.err);
            advance();
            continue;
        }
        if (!strcmp(t->s, "usage")) { advance(); accept_word("is"); t = cur(); if (t->kind != T_WORD) die_at(t->line, "expected a USAGE"); }
        int u = -1;
        if (!strcmp(t->s, "display")) u = U_DISPLAY;
        else if (!strcmp(t->s, "comp") || !strcmp(t->s, "computational") || !strcmp(t->s, "binary")) u = U_BINARY;
        else if (!strcmp(t->s, "comp-3") || !strcmp(t->s, "computational-3") || !strcmp(t->s, "packed-decimal")) u = U_PACKED;
        else if (!strcmp(t->s, "comp-5") || !strcmp(t->s, "computational-5")) u = U_COMP5;
        else if (!strcmp(t->s, "signed-int") || !strcmp(t->s, "binary-long")) u = U_SINT;
        else if (!strcmp(t->s, "unsigned-int")) u = U_UINT;
        else if (!strcmp(t->s, "signed-short") || !strcmp(t->s, "binary-short")) u = U_SSHORT;
        else if (!strcmp(t->s, "unsigned-short")) u = U_USHORT;
        else if (!strcmp(t->s, "binary-char")) {
            advance();
            u = accept_word("unsigned") ? U_UBCHAR : U_BCHAR;
            if (u == U_BCHAR) accept_word("signed");
            s->usage = u; s->has_usage = 1;
            continue;
        }
        else if (!strcmp(t->s, "pointer")) u = U_POINTER;
        else if (!strcmp(t->s, "index")) u = U_INDEX;
        else if (!strcmp(t->s, "comp-1") || !strcmp(t->s, "comp-2") || !strcmp(t->s, "float-short") || !strcmp(t->s, "float-long"))
            die_at(t->line, "floating-point USAGE %s is not implemented", t->s);
        if (u >= 0) {
            if (s->has_usage) die_at(t->line, "'%s' has two USAGE clauses", s->name);
            s->usage = u; s->has_usage = 1;
            advance();
            continue;
        }

        if (!strcmp(t->s, "value")) {
            advance(); accept_word("is");
            if (accept_word("all")) value_all = 1;
            Tok *v = cur();
            if (v->kind == T_STR || v->kind == T_NUM) { value_tok = v; advance(); }
            else if (v->kind == T_WORD) {
                static const char *figs[] = { "space", "spaces", "zero", "zeros", "zeroes",
                    "low-value", "low-values", "high-value", "high-values", "quote", "quotes", "null", "nulls", NULL };
                for (int i = 0; figs[i]; i++) if (!strcmp(v->s, figs[i])) fig = figs[i];
                if (!fig) die_at(v->line, "expected a literal after VALUE, found %s", tok_desc(v));
                value_fig = 1; value_tok = v; advance();
            } else die_at(v->line, "expected a literal after VALUE, found %s", tok_desc(v));
            if (at_word("thru") || at_word("through"))
                die_at(cur()->line, "VALUE ... THRU is only for level 88");
            continue;
        }

        static const char *later[] = { "occurs", "redefines", "sync", "synchronized",
            "just", "justified", "blank", "sign", "global", "external", "renames", NULL };
        for (int i = 0; later[i]; i++)
            if (!strcmp(t->s, later[i]))
                die_at(t->line, "the %s clause is not implemented yet", later[i]);
        die_at(t->line, "unexpected %s in the description of '%s'", tok_desc(t), s->name);
    }
    expect_period();

    sym_finish(s);
    default_init(s);
    snprintf(s->label, sizeof s->label, "ws_%d", (int)(s - g_sym));

    if (!value_tok) return;
    s->has_value = 1;
    int numeric = (s->pi.category == PIC_NUMERIC);

    if (value_fig) {
        unsigned char fill = 0;
        if (!strncmp(fig, "space", 5)) fill = ' ';
        else if (!strncmp(fig, "zero", 4)) fill = '0';
        else if (!strncmp(fig, "low", 3) || !strncmp(fig, "null", 4)) fill = 0x00;
        else if (!strncmp(fig, "high", 4)) fill = 0xFF;       /* ASCII: HIGH-VALUE is X'FF' */
        else if (!strncmp(fig, "quote", 5)) fill = '"';
        if (numeric) {
            if (!strncmp(fig, "zero", 4)) { NumLit z; memset(&z, 0, sizeof z); z.digits[0] = '0'; z.ndigits = 1; store_numeric(s, &z, line); }
            else if (s->usage == U_DISPLAY && (fill == ' ' || fill == 0 || fill == 0xFF)) memset(s->value, fill, s->size);
            else die_at(value_tok->line, "VALUE %s is not valid for the numeric item '%s'", fig, s->name);
        } else memset(s->value, fill, s->size);
        return;
    }

    if (value_tok->kind == T_NUM) {
        if (s->pi.category == PIC_NUMERIC_EDITED)
            die_at(value_tok->line, "VALUE of a numeric-edited item must be a nonnumeric literal in this compiler");
        if (!numeric) die_at(value_tok->line, "a numeric VALUE is not valid for the alphanumeric item '%s'", s->name);
        NumLit n; numlit_parse(value_tok, &n);
        store_numeric(s, &n, value_tok->line);
        return;
    }

    /* nonnumeric literal */
    if (numeric && s->usage != U_DISPLAY)
        die_at(value_tok->line, "a nonnumeric VALUE is not valid for the %s item '%s'", usage_name(s->usage), s->name);
    if (value_all) {
        if (value_tok->len < 1) die_at(value_tok->line, "VALUE ALL of an empty literal");
        for (int i = 0; i < s->size; i++) s->value[i] = (unsigned char)value_tok->s[i % value_tok->len];
        return;
    }
    if (value_tok->len > s->size)
        die_at(value_tok->line, "VALUE literal (%d characters) is longer than '%s' (%d)", value_tok->len, s->name, s->size);
    memcpy(s->value, value_tok->s, value_tok->len);
    if (numeric) {
        /* right-justify digits into a numeric DISPLAY item, as a MOVE of a
         * numeric literal would */
        for (int i = 0; i < value_tok->len; i++)
            if (!isdigit((unsigned char)value_tok->s[i]))
                die_at(value_tok->line, "VALUE of the numeric item '%s' must be numeric", s->name);
        memset(s->value, '0', s->size);
        memcpy(s->value + s->size - value_tok->len, value_tok->s, value_tok->len);
    } else {
        memset(s->value + value_tok->len, ' ', s->size - value_tok->len);
    }
}

/* ====================================================================== */
/* Emitter                                                                 */
/* ====================================================================== */

static FILE *g_out;
static int g_nlit;

typedef struct { char label[32]; unsigned char *bytes; int len; } Lit;
static Lit *g_lit; static int g_lcap;

static void emit(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt); vfprintf(g_out, fmt, ap); va_end(ap);
    fputc('\n', g_out);
}

static const char *lit_label(const unsigned char *bytes, int len)
{
    for (int i = 0; i < g_nlit; i++)
        if (g_lit[i].len == len && !memcmp(g_lit[i].bytes, bytes, len)) return g_lit[i].label;
    if (g_nlit == g_lcap) { g_lcap = g_lcap ? g_lcap * 2 : 32; g_lit = realloc(g_lit, g_lcap * sizeof *g_lit); }
    Lit *l = &g_lit[g_nlit++];
    snprintf(l->label, sizeof l->label, ".Lstr%d", g_nlit - 1);
    l->bytes = xmalloc(len); memcpy(l->bytes, bytes, len); l->len = len;
    return l->label;
}

/* rd = symbol address */
static void emit_la(const char *rd, const char *sym)
{
    emit("\tlui %s, %%hi(%s)", rd, sym);
    emit("\taddi %s, %s, %%lo(%s)", rd, rd, sym);
}

/* rd = 32-bit constant */
static void emit_li(const char *rd, long v)
{
    if (v >= -2048 && v <= 2047) { emit("\taddi %s, r0, %ld", rd, v); return; }
    unsigned long u = (unsigned long)v;
    unsigned long hi = ((u + 0x800) >> 12) & 0xFFFFF;
    long lo = (long)(u & 0xFFF); if (lo >= 2048) lo -= 4096;
    emit("\tlui %s, %lu", rd, hi);
    if (lo) emit("\taddi %s, %s, %ld", rd, rd, lo);
}

static void emit_call(const char *fn) { emit("\tjal r31, %s", fn); }

static void emit_bytes(const unsigned char *b, int n)
{
    for (int i = 0; i < n; i += 16) {
        fputs("\t.byte ", g_out);
        for (int j = i; j < n && j < i + 16; j++)
            fprintf(g_out, "%s%d", j == i ? "" : ",", b[j]);
        fputc('\n', g_out);
    }
}

static char *mangle(const char *name)
{
    static char b[128];
    int n = 0;
    for (const char *p = name; *p && n < 120; p++) b[n++] = isalnum((unsigned char)*p) ? *p : '_';
    b[n] = 0;
    return b;
}

/* ====================================================================== */
/* Procedure Division                                                      */
/* ====================================================================== */

static int g_nlabel;

static int is_verb(const char *w)
{
    static const char *verbs[] = { "accept", "add", "alter", "call", "cancel", "close",
        "compute", "continue", "delete", "disable", "display", "divide", "enable",
        "enter", "evaluate", "exit", "generate", "go", "goback", "if", "initialize",
        "initiate", "inspect", "merge", "move", "multiply", "open", "perform", "purge",
        "read", "receive", "release", "return", "rewrite", "search", "send", "set",
        "sort", "start", "stop", "string", "subtract", "suppress", "terminate",
        "unstring", "use", "write", NULL };
    for (int i = 0; verbs[i]; i++) if (!strcmp(w, verbs[i])) return 1;
    return 0;
}

/* DISPLAY {identifier | literal | figurative}... [UPON ...] [WITH NO ADVANCING] */
static void parse_display(void)
{
    int line = cur()->line;
    int n = 0, no_adv = 0;
    for (;;) {
        Tok *t = cur();
        if (t->kind == T_STR) {
            const char *l = lit_label((unsigned char *)t->s, t->len);
            emit_la("r3", l); emit_li("r4", t->len); emit_call("cob_display");
            advance(); n++; continue;
        }
        if (t->kind == T_NUM) {
            /* a numeric literal displays as written */
            const char *l = lit_label((unsigned char *)t->s, (int)strlen(t->s));
            emit_la("r3", l); emit_li("r4", (long)strlen(t->s)); emit_call("cob_display");
            advance(); n++; continue;
        }
        if (t->kind == T_WORD) {
            if (!strcmp(t->s, "upon")) die_at(t->line, "DISPLAY UPON is not implemented yet");
            if (!strcmp(t->s, "with") || !strcmp(t->s, "no")) {
                accept_word("with");
                expect_word("no"); expect_word("advancing");
                no_adv = 1;
                break;
            }
            if (is_verb(t->s)) break;
            static const char *figs[] = { "space", "spaces", "zero", "zeros", "zeroes",
                "quote", "quotes", "low-value", "low-values", "high-value", "high-values", NULL };
            int isfig = 0;
            for (int i = 0; figs[i]; i++) if (!strcmp(t->s, figs[i])) isfig = 1;
            if (isfig) {
                unsigned char c = ' ';
                if (!strncmp(t->s, "zero", 4)) c = '0'; else if (!strncmp(t->s, "quote", 5)) c = '"';
                else if (!strncmp(t->s, "low", 3)) c = 0; else if (!strncmp(t->s, "high", 4)) c = 0xFF;
                const char *l = lit_label(&c, 1);
                emit_la("r3", l); emit_li("r4", 1); emit_call("cob_display");
                advance(); n++; continue;
            }
            if (!strcmp(t->s, "all")) die_at(t->line, "DISPLAY ALL literal is not implemented yet");
            Sym *s = sym_find(t->s);
            if (!s) die_at(t->line, "'%s' is not declared", t->s);
            advance();
            if (cur()->kind == T_LP) die_at(cur()->line, "subscripts and reference modification are not implemented yet (stage 2)");
            emit_la("r3", s->label);
            switch (s->usage) {
            case U_DISPLAY:
                if (s->pi.category == PIC_NUMERIC) {
                    emit_li("r4", s->size); emit_li("r5", s->pi.scale); emit_li("r6", s->pi.is_signed); emit_call("cob_display_numdisp");
                } else { emit_li("r4", s->size); emit_call("cob_display"); }
                break;
            case U_PACKED:
                emit_li("r4", s->pi.digits); emit_li("r5", s->pi.scale); emit_li("r6", s->pi.is_signed);
                emit_call("cob_display_packed");
                break;
            default:
                emit_li("r4", s->size);
                emit_li("r5", s->usage == U_BINARY ? s->pi.digits : capacity_digits(s->size));
                emit_li("r6", s->pi.scale);
                emit_li("r7", s->pi.is_signed);
                emit_call("cob_display_bin");
                break;
            }
            n++;
            continue;
        }
        break;
    }
    if (!n) die_at(line, "DISPLAY needs at least one operand");
    if (!no_adv) emit_call("cob_display_nl");
}

static void parse_statement(void)
{
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a statement, found %s", tok_desc(t));

    if (!strcmp(t->s, "display")) { advance(); parse_display(); return; }
    if (!strcmp(t->s, "stop")) {
        advance();
        if (accept_word("run")) {
            emit_li("r3", 0);
            emit_call("cob_stop_run");
            return;
        }
        die_at(t->line, "STOP literal is not implemented");
    }
    if (!strcmp(t->s, "goback")) { advance(); emit("\tjal r0, .Lgoback"); return; }
    if (!strcmp(t->s, "continue")) { advance(); return; }
    if (!strcmp(t->s, "exit")) {
        advance();
        if (at_word("program")) die_at(t->line, "EXIT PROGRAM is not implemented yet (stage 6)");
        if (at_word("perform") || at_word("paragraph") || at_word("section"))
            die_at(t->line, "EXIT %s is not in COBOL 85", cur()->s);
        return;                                     /* EXIT: no operation */
    }
    if (!strcmp(t->s, "alter"))
        die_at(t->line, "ALTER is not in COBOL 85 (obsolete in the 1985 standard); refused");
    if (!strcmp(t->s, "enter") || !strcmp(t->s, "disable") || !strcmp(t->s, "enable") ||
        !strcmp(t->s, "purge") || !strcmp(t->s, "receive") || !strcmp(t->s, "send"))
        die_at(t->line, "%s is not supported (the Communication module is deliberately out)", t->s);
    if (is_verb(t->s))
        die_at(t->line, "the verb %s is not implemented yet", t->s);
    die_at(t->line, "'%s' is not a COBOL verb", t->s);
}

static void parse_procedure_division(void)
{
    expect_word("procedure"); expect_word("division");
    if (at_word("using")) die_at(cur()->line, "PROCEDURE DIVISION USING is not implemented yet (stage 6)");
    expect_period();

    emit("\t.text");
    emit("\t.globl main");
    emit("\t.p2align 2");
    emit("\t.type main,@function");
    emit("main:");
    emit("\taddi sp, sp, -16");
    emit("\tstw sp+0, lr");
    emit_call("cob_init");

    for (;;) {
        Tok *t = cur();
        if (t->kind == T_EOF) break;
        if (is_word(t, "end") && is_word(peek(1), "program")) break;

        /* paragraph-name. or section-name SECTION. */
        if (t->kind == T_WORD && !is_verb(t->s) && (peek(1)->kind == T_PERIOD ||
            (is_word(peek(1), "section") && peek(2)->kind == T_PERIOD))) {
            int is_section = is_word(peek(1), "section");
            emit(".L%s_%s_%d:", is_section ? "sec" : "par", mangle(t->s), g_nlabel++);
            advance(); if (is_section) advance();
            expect_period();
            continue;
        }
        if (t->kind == T_WORD && !is_verb(t->s) && peek(1)->kind == T_NUM && is_word(peek(2), "section"))
            die_at(t->line, "section segment numbers are obsolete in COBOL 85; not supported");

        /* a sentence */
        for (;;) {
            parse_statement();
            if (cur()->kind == T_PERIOD) { advance(); break; }
            if (cur()->kind == T_EOF) die_at(cur()->line, "missing '.' at the end of the last sentence");
        }
    }

    emit(".Lgoback:");
    emit("\taddi r1, r0, 0");
    emit("\tldw lr, sp+0");
    emit("\taddi sp, sp, 16");
    emit("\tjalr r0, r31, 0");

    if (accept_word("end")) {
        expect_word("program");
        if (cur()->kind != T_WORD || strcmp(cur()->s, g_progid))
            die_at(cur()->line, "END PROGRAM names '%s' but the program is '%s'", cur()->s, g_progid);
        advance();
        expect_period();
        if (cur()->kind != T_EOF) die_at(cur()->line, "more than one program in a source file is not implemented yet (stage 6)");
    }
}

/* ====================================================================== */
/* The other divisions                                                     */
/* ====================================================================== */

static int at_division(void)
{
    return cur()->kind == T_WORD && is_word(peek(1), "division") &&
           (at_word("environment") || at_word("data") || at_word("procedure"));
}

static void parse_identification_division(void)
{
    if (!(accept_word("identification") || accept_word("id")))
        die_at(cur()->line, "expected IDENTIFICATION DIVISION, found %s", tok_desc(cur()));
    expect_word("division"); expect_period();
    expect_word("program-id"); expect_period();
    if (cur()->kind != T_WORD) die_at(cur()->line, "expected a program-name");
    snprintf(g_progid, sizeof g_progid, "%s", cur()->s);
    advance();
    if (accept_word("is")) {
        if (accept_word("initial")) accept_word("program");
        else if (accept_word("common")) die_at(cur()->line, "COMMON programs are not implemented yet");
        else die_at(cur()->line, "expected INITIAL after IS");
    }
    expect_period();

    /* The remaining paragraphs are comment-entries: anything up to the next
     * paragraph header or division. */
    static const char *paras[] = { "author", "installation", "date-written",
        "date-compiled", "security", "remarks", NULL };
    while (!at_division() && cur()->kind != T_EOF) {
        Tok *t = cur();
        int known = 0;
        for (int i = 0; paras[i]; i++) if (is_word(t, paras[i])) known = 1;
        if (!known) die_at(t->line, "unexpected %s in the IDENTIFICATION DIVISION", tok_desc(t));
        advance(); expect_period();
        while (!at_division() && cur()->kind != T_EOF) {
            int hdr = 0;
            for (int i = 0; paras[i]; i++) if (at_word(paras[i]) && peek(1)->kind == T_PERIOD) hdr = 1;
            if (hdr) break;
            advance();
        }
    }
}

static void skip_to_period(void)
{
    while (cur()->kind != T_PERIOD && cur()->kind != T_EOF) advance();
    expect_period();
}

static void parse_environment_division(void)
{
    if (!accept_word("environment")) return;
    expect_word("division"); expect_period();
    if (accept_word("configuration")) {
        expect_word("section"); expect_period();
        for (;;) {
            if (accept_word("source-computer") || accept_word("object-computer")) { expect_period(); if (cur()->kind == T_WORD && !at_word("special-names") && !at_word("input-output") && !at_division()) skip_to_period(); continue; }
            if (at_word("special-names")) {
                advance(); expect_period();
                if (cur()->kind != T_PERIOD && !at_division() && !at_word("input-output"))
                    die_at(cur()->line, "SPECIAL-NAMES clauses are not implemented yet");
                continue;
            }
            if (at_word("repository"))
                die_at(cur()->line, "REPOSITORY is COBOL 2002; rewrite user-defined functions as CALL (docs/functions.md)");
            break;
        }
    }
    if (at_word("input-output")) die_at(cur()->line, "the INPUT-OUTPUT SECTION is not implemented yet (stage 4)");
    if (!at_division()) die_at(cur()->line, "unexpected %s in the ENVIRONMENT DIVISION", tok_desc(cur()));
}

static void parse_data_division(void)
{
    if (!accept_word("data")) return;
    expect_word("division"); expect_period();
    for (;;) {
        if (at_word("file") && is_word(peek(1), "section"))
            die_at(cur()->line, "the FILE SECTION is not implemented yet (stage 4)");
        if (at_word("working-storage")) {
            advance(); expect_word("section"); expect_period();
            while (cur()->kind == T_NUM) parse_data_item();
            continue;
        }
        if ((at_word("linkage") || at_word("screen") || at_word("report") || at_word("communication")) && is_word(peek(1), "section"))
            die_at(cur()->line, "the %s SECTION is not implemented yet", cur()->s);
        break;
    }
    if (!at_division() && cur()->kind != T_EOF) die_at(cur()->line, "unexpected %s in the DATA DIVISION", tok_desc(cur()));
}

/* ====================================================================== */
/* Driver                                                                  */
/* ====================================================================== */

static void emit_data(void)
{
    emit("");
    emit("\t.data");
    for (int i = 0; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        emit("\t.p2align 2");
        emit("%s:\t# %02d %s pic %s usage %s (%d bytes)", s->label, s->level, s->name,
             s->has_pic ? s->pic : "-", usage_name(s->usage), s->size);
        emit_bytes(s->value, s->size);
    }
    if (g_nlit) {
        emit("");
        emit("\t.section .rodata");
        for (int i = 0; i < g_nlit; i++) {
            emit("%s:", g_lit[i].label);
            emit_bytes(g_lit[i].bytes, g_lit[i].len);
        }
    }
}

static void usage(void)
{
    fprintf(stderr, "s32-cobc %s -- COBOL 85 for SLOW-32\n"
        "usage: s32-cobc [-free|-fixed] [-o out.s] source.cbl\n"
        "  -fixed   reference format (columns 7/8-72); the default\n"
        "  -free    free format (GnuCOBOL -free; majesty)\n", VERSION);
    exit(2);
}

int main(int argc, char **argv)
{
    const char *in = NULL, *out = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-free")) g_free = 1;
        else if (!strcmp(argv[i], "-fixed")) g_free = 0;
        else if (!strcmp(argv[i], "-o") && i + 1 < argc) out = argv[++i];
        else if (!strcmp(argv[i], "--version")) { printf("s32-cobc %s\n", VERSION); return 0; }
        else if (argv[i][0] == '-') usage();
        else if (in) usage();
        else in = argv[i];
    }
    if (!in) usage();
    g_file = in;

    char outbuf[1024];
    if (!out) {
        const char *base = strrchr(in, '/'); base = base ? base + 1 : in;
        snprintf(outbuf, sizeof outbuf, "%s", base);
        char *dot = strrchr(outbuf, '.'); if (dot) *dot = 0;
        strcat(outbuf, ".s");
        out = outbuf;
    }

    read_source(in);
    tokenize();

    g_out = fopen(out, "w");
    if (!g_out) { fprintf(stderr, "s32-cobc: cannot write %s\n", out); return 1; }
    emit("\t.file\t\"%s\"", in);
    emit("# s32-cobc %s", VERSION);

    parse_identification_division();
    parse_environment_division();
    parse_data_division();
    if (!at_word("procedure")) die_at(cur()->line, "expected PROCEDURE DIVISION, found %s", tok_desc(cur()));
    parse_procedure_division();
    if (cur()->kind != T_EOF) die_at(cur()->line, "unexpected %s after the program", tok_desc(cur()));
    emit_data();
    fclose(g_out);
    return 0;
}
