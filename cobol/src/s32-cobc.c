/* s32-cobc -- COBOL 85 for SLOW-32.  Host cross-compiler.
 *
 * Reads ANSI X3.23-1985 COBOL (fixed or free reference format) plus the
 * implementor modules listed in docs/dialect.md, and emits SLOW-32
 * assembler for slow32asm / s32-ld.  Not SSA, not BURG: the IR is the
 * symbol table (Sym[]), and each verb is a lowering against it -- an inline
 * sequence for the hot cases, otherwise a call into libcob with a
 * descriptor the compiler built (libcob/cobrt.h).  docs/architecture.md.
 *
 * Stage 2 (docs/plan.md): the Data Division as a tree -- groups,
 * REDEFINES, OCCURS with subscripts, 77, 88, qualification -- the
 * conversion matrix behind MOVE, the arithmetic statements on a scaled-i64
 * numeric stack with COMP-integer hot cases inline, conditions, IF,
 * every PERFORM form, GO TO, SET.  Stage 3: edited MOVE and de-edit
 * through the shared software editor (libcob/cobedit.h), COMPUTE with
 * arithmetic expressions (also as condition operands), ROUNDED, ON SIZE
 * ERROR, REMAINDER.  Stage 4: SELECT/FD, line sequential and fixed
 * sequential files (OPEN, CLOSE, READ, WRITE), STRING, the case
 * intrinsics.  Stage 5: INDEXED files -- READ KEY / NEXT, WRITE, REWRITE,
 * DELETE, START, INVALID KEY.  Stage 6: several program units per
 * source, LINKAGE SECTION, PROCEDURE DIVISION USING, CALL on the SLOW-32
 * C ABI (BY REFERENCE / BY VALUE / RETURNING at the C seam), so COBOL, C
 * and Fortran link with no glue.  Stage 7: Report Writer, the cheap
 * half -- RD with PAGE LIMIT / HEADING / FIRST and LAST DETAIL, PAGE
 * HEADING and DETAIL groups, LINE / COLUMN / SOURCE / VALUE, INITIATE /
 * GENERATE / TERMINATE, rendered per GENERATE site against a page engine
 * in libcob (docs/report-writer.md).  Stage 8: SCREEN SECTION -- a table
 * of slots per 01, DISPLAY paints and ACCEPT runs the focus loop, on the
 * term service (docs/screen.md).  Stage 9, what menu and taskdt drag
 * in: EVALUATE, INSPECT, INITIALIZE, reference modification with
 * arithmetic, FUNCTION LENGTH and CURRENT-DATE.  Stage 10: sequential
 * mode V -- RECORDING MODE V, RECORD CONTAINS m TO n, RECORD IS VARYING
 * DEPENDING ON, or unequal 01s -- with the IBM RDW on disk.  Stage 12:
 * COPY (the Library module) as token-stream inclusion, copybooks found
 * through -I.  Unimplemented is a diagnostic, never silence.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include "picture.h"
#include "../libcob/cobrt.h"

#define VERSION "0.15 (stage 15: SEARCH)"

/* ====================================================================== */
/* Diagnostics                                                             */
/* ====================================================================== */

static const char *g_file = "?";
static const char *g_tok_file = "?";    /* the file being tokenized (a copybook, or the source) */
static int g_free = 0;              /* -free: majesty; default is fixed */
static const char *diag_file(int line);
static int g_module = 0;            /* -m: no main entry; every unit is a subprogram */
static int g_unit = 0;              /* program unit being compiled, for label spaces */

/* SPECIAL-NAMES CLASS name IS lit [THROUGH lit] ...: a user class is a
 * 256-entry membership table, per program unit, tested like NUMERIC */
typedef struct { char name[64]; unsigned char tab[256]; } UClass;
static UClass g_class[16];
static int g_nclass;

static void die_at(int line, const char *fmt, ...)
{
    va_list ap;
    fprintf(stderr, "%s:%d: error: ", diag_file(line), line);
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

/* read a source file (or a copybook) into lines of program text; 0 if
 * it cannot be opened */
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
            if (n == cap) { cap *= 2; lines = realloc(lines, cap * sizeof *lines); }
            lines[n].text = text;
            lines[n].line = lineno;
            n++;
        }
        if (!e) break;
        p = e + 1;
    }
    free(buf);
    *out = lines; *nout = n;
    return 1;
}

static void read_source(const char *path)
{
    if (!read_lines(path, &g_lines, &g_nlines)) { fprintf(stderr, "s32-cobc: cannot open %s\n", path); exit(1); }
}

/* ====================================================================== */
/* Tokenizer                                                               */
/* ====================================================================== */

enum { T_EOF, T_WORD, T_NUM, T_STR, T_PIC, T_PERIOD, T_LP, T_RP, T_COLON, T_OP };

typedef struct {
    int kind, line;
    char *s;        /* word (lowercased), number text, literal bytes, picture, op */
    int len;        /* literal byte length (literals may hold NULs) */
    const char *file;
} Tok;

static Tok *g_tok;
static int g_ntok, g_tcap;

static Tok *push_tok(int kind, int line, const char *s, int len)
{
    if (g_ntok == g_tcap) { g_tcap = g_tcap ? g_tcap * 2 : 1024; g_tok = realloc(g_tok, g_tcap * sizeof *g_tok); }
    Tok *t = &g_tok[g_ntok++];
    t->kind = kind; t->line = line; t->s = xstrndup(s, len); t->len = len; t->file = g_tok_file;
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

static void tokenize_lines(SrcLine *lines, int nlines)
{
    int pic_ctx = 0;    /* after PIC/PICTURE [IS]: the next token is a picture */
    for (int li = 0; li < nlines; li++) {
        const char *t = lines[li].text;
        int line = lines[li].line;
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
            if (c == '<' && p[1] == '>') { push_tok(T_OP, line, "<>", 2); p += 2; continue; }
            if (strchr("=<>+-*/", c)) { push_tok(T_OP, line, p, 1); p++; continue; }
            die_at(line, "unexpected character '%c'", c);
        }
    }
}

/* ---- COPY: the Library module ------------------------------------------ */

/* COPY text-name [OF/IN library] [SUPPRESS] [REPLACING ...]. is replaced,
 * period included, by the copybook's tokens; the copybook is read in the
 * same reference format, may itself COPY, and is looked for as the name
 * given, then name.cpy / .CPY / .cbl, in the source's directory and the
 * -I directories. */
static const char *g_incdirs[16]; static int g_nincdir;

static int copy_open(const char *name, SrcLine **lines, int *n, char *found, size_t foundsz)
{
    static const char *exts[] = { "", ".cpy", ".CPY", ".cbl", ".CBL", NULL };
    char srcdir[1024]; snprintf(srcdir, sizeof srcdir, "%s", g_file);
    char *sl = strrchr(srcdir, '/'); if (sl) *sl = 0; else strcpy(srcdir, ".");
    for (int d = -1; d < g_nincdir; d++) {
        const char *dir = d < 0 ? srcdir : g_incdirs[d];
        for (int e = 0; exts[e]; e++) {
            snprintf(found, foundsz, "%s/%s%s", dir, name, exts[e]);
            if (read_lines(found, lines, n)) return 1;
        }
    }
    return 0;
}

static void expand_copies(int depth)
{
    for (int i = 0; i < g_ntok; i++) {
        if (!(g_tok[i].kind == T_WORD && !strcmp(g_tok[i].s, "copy"))) continue;
        int line = g_tok[i].line;
        int j = i + 1;
        if (j >= g_ntok || !(g_tok[j].kind == T_WORD || g_tok[j].kind == T_STR)) die_at(line, "COPY needs a text-name");
        char name[256]; snprintf(name, sizeof name, "%.*s", g_tok[j].len > 250 ? 250 : g_tok[j].len, g_tok[j].s);
        j++;
        if (j < g_ntok && g_tok[j].kind == T_WORD && (!strcmp(g_tok[j].s, "of") || !strcmp(g_tok[j].s, "in"))) {
            j++;
            if (j >= g_ntok || !(g_tok[j].kind == T_WORD || g_tok[j].kind == T_STR)) die_at(line, "COPY ... OF needs a library-name");
            j++;                                        /* the library: the -I directories serve */
        }
        if (j < g_ntok && g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "suppress")) j++;
        if (j < g_ntok && g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "replacing"))
            die_at(line, "COPY ... REPLACING is not implemented yet");
        if (j >= g_ntok || g_tok[j].kind != T_PERIOD) die_at(line, "COPY %s needs its period", name);
        if (depth > 8) die_at(line, "COPY nests deeper than 8 (%s)", name);

        SrcLine *lines; int n; char found[1200];
        if (!copy_open(name, &lines, &n, found, sizeof found))
            die_at(line, "COPY: cannot find '%s' (looked beside the source and in the -I directories, as %s, %s.cpy, %s.cbl)", name, name, name, name);

        /* tokenize the copybook into its own vector, then splice */
        Tok *save_tok = g_tok; int save_n = g_ntok, save_cap = g_tcap;
        const char *save_file = g_tok_file;
        g_tok = NULL; g_ntok = 0; g_tcap = 0; g_tok_file = xstrndup(found, (int)strlen(found));
        tokenize_lines(lines, n);
        Tok *ctok = g_tok; int cn = g_ntok;
        g_tok = save_tok; g_ntok = save_n; g_tcap = save_cap; g_tok_file = save_file;

        int removed = j - i + 1;
        int newn = g_ntok - removed + cn;
        if (newn > g_tcap) { g_tcap = newn + 1024; g_tok = realloc(g_tok, g_tcap * sizeof *g_tok); }
        memmove(&g_tok[i + cn], &g_tok[j + 1], (size_t)(g_ntok - (j + 1)) * sizeof *g_tok);
        memcpy(&g_tok[i], ctok, (size_t)cn * sizeof *ctok);
        g_ntok = newn;
        free(ctok);
        i--;                                            /* rescan from the spliced text: nested COPY */
        depth++;
    }
}

static void tokenize(void)
{
    g_tok_file = g_file;
    tokenize_lines(g_lines, g_nlines);
    expand_copies(0);
    push_tok(T_EOF, g_nlines ? g_lines[g_nlines - 1].line : 1, "", 0);
}

/* ---- cursor ---------------------------------------------------------- */

static int g_tp;

static Tok *cur(void)  { return &g_tok[g_tp]; }

static const char *diag_file(int line)
{
    if (g_tok && g_tp < g_ntok && g_tok[g_tp].file && g_tok[g_tp].line == line) return g_tok[g_tp].file;
    if (g_tok && g_tp > 0 && g_tp <= g_ntok && g_tok[g_tp - 1].file && g_tok[g_tp - 1].line == line) return g_tok[g_tp - 1].file;
    return g_tok_file ? g_tok_file : g_file;
}
static Tok *peek(int k){ int i = g_tp + k; if (i >= g_ntok) i = g_ntok - 1; return &g_tok[i]; }
static void advance(void) { if (g_tp < g_ntok - 1) g_tp++; }

static int is_word(Tok *t, const char *w) { return t->kind == T_WORD && !strcmp(t->s, w); }
static int at_word(const char *w) { return is_word(cur(), w); }
static int accept_word(const char *w) { if (at_word(w)) { advance(); return 1; } return 0; }
static int at_op(const char *o) { return cur()->kind == T_OP && !strcmp(cur()->s, o); }

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

static int is_figurative(const char *w)
{
    static const char *figs[] = { "space", "spaces", "zero", "zeros", "zeroes",
        "low-value", "low-values", "high-value", "high-values", "quote", "quotes",
        "null", "nulls", NULL };
    for (int i = 0; figs[i]; i++) if (!strcmp(w, figs[i])) return 1;
    return 0;
}

static int fig_byte(const char *w)
{
    if (!strncmp(w, "space", 5)) return ' ';
    if (!strncmp(w, "zero", 4)) return '0';
    if (!strncmp(w, "high", 4)) return 0xFF;                 /* ASCII: HIGH-VALUE is X'FF' */
    if (!strncmp(w, "quote", 5)) return '"';
    return 0;                                                 /* LOW-VALUE, NULL */
}

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
    if (n->ndigits - n->scale > 18 || n->ndigits > 36)
        die_at(t->line, "numeric literal has more than 18 digits");
}

static void numlit_zero(NumLit *n) { memset(n, 0, sizeof *n); n->digits[0] = '0'; n->ndigits = 1; }

static int numlit_is_int(const NumLit *n)
{
    for (int i = n->ndigits - n->scale; i < n->ndigits; i++) if (n->digits[i] != '0') return 0;
    return 1;
}

static long long numlit_int(const NumLit *n)       /* integer part */
{
    long long v = 0;
    for (int i = 0; i < n->ndigits - n->scale; i++) v = v * 10 + (n->digits[i] - '0');
    return n->neg ? -v : v;
}

static long long numlit_scaled(const NumLit *n)    /* all digits as an integer */
{
    long long v = 0;
    for (int i = 0; i < n->ndigits; i++) v = v * 10 + (n->digits[i] - '0');
    return n->neg ? -v : v;
}

/* Value of the literal scaled to `scale` decimal places, as digit text
 * `out` of exactly `digits` characters (right-aligned, zero-filled).
 * Returns 0 if the integer part does not fit. */
static int numlit_align(const NumLit *n, int digits, int scale, char *out)
{
    int int_digits = n->ndigits - n->scale;
    int want_int = digits - scale;
    memset(out, '0', digits);
    for (int i = 0; i < int_digits; i++) {
        int pos = want_int - int_digits + i;
        if (pos < 0) { if (n->digits[i] != '0') return 0; continue; }
        out[pos] = n->digits[i];
    }
    for (int i = 0; i < n->scale && i < scale; i++)
        out[want_int + i] = n->digits[int_digits + i];
    return 1;
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

static int usage_is_native(int u)
{
    return u == U_SINT || u == U_UINT || u == U_SSHORT || u == U_USHORT ||
           u == U_BCHAR || u == U_UBCHAR || u == U_POINTER || u == U_INDEX;
}

#define MAXDIM 7
#define MAXCV 32

typedef struct Sym {
    char name[64];
    int  level, line, is_filler;
    int  parent, child, sibling;    /* tree, as indices; -1 = none */
    int  record;                    /* the 01/77 (or index) owning the storage */
    int  usage, has_usage, has_pic;
    char pic[PIC_MAXPAT];
    PicInfo pi;
    int  is_group, is_cond, is_index;
    int  size;                      /* one occurrence */
    int  offset;                    /* from the start of the record */
    int  occurs;                    /* 0 = no OCCURS; with DEPENDING ON, the maximum */
    int  odo_min; char odo_dep[64]; struct Sym *odo_dep_sym;   /* OCCURS m TO n DEPENDING ON */
    int  idx1;                      /* the table's first INDEXED BY item, or -1 */
    int  redefines;                 /* sym index, -1 */
    int  sync, just, blank_zero;
    int  ndims, dim_count[MAXDIM], dim_stride[MAXDIM];
    /* VALUE (elementary or group) */
    Tok *value_tok; int value_all, value_fig;
    /* level 88 */
    int  ncv; Tok *cv_lo[MAXCV], *cv_hi[MAXCV];
    int  fd;                        /* file index for an 01 under an FD, else -1 */
    int  is_linkage;                /* a LINKAGE SECTION record: storage is the caller's */
    /* records */
    unsigned char *image; int image_size;
    char label[48];
    /* descriptor */
    int  desc_id;                   /* -1 until emitted */
} Sym;

static Sym *g_sym;
static int g_nsym, g_scap;

static Sym *sym_new(void)
{
    if (g_nsym == g_scap) { g_scap = g_scap ? g_scap * 2 : 128; g_sym = realloc(g_sym, g_scap * sizeof *g_sym); }
    Sym *s = &g_sym[g_nsym++];
    memset(s, 0, sizeof *s);
    s->parent = s->child = s->sibling = s->redefines = -1;
    s->desc_id = -1; s->fd = -1; s->idx1 = -1;
    return s;
}

static int sym_idx(Sym *s) { return (int)(s - g_sym); }

/* name [OF|IN qualifier]...: the unique item that matches */
static Sym *sym_lookup(const char *name, char **quals, int nq, int line)
{
    Sym *found = NULL; int nfound = 0;
    for (int i = 0; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_filler || strcmp(s->name, name)) continue;
        int ok = 1, at = i;
        for (int q = 0; q < nq && ok; q++) {
            int hit = -1;
            for (int p = g_sym[at].parent; p >= 0; p = g_sym[p].parent)
                if (!strcmp(g_sym[p].name, quals[q])) { hit = p; break; }
            if (hit < 0) ok = 0; else at = hit;
        }
        if (ok) { found = s; nfound++; }
    }
    if (!nfound) {
        if (nq) die_at(line, "'%s' is not declared under '%s'", name, quals[0]);
        die_at(line, "'%s' is not declared", name);
    }
    if (nfound > 1) die_at(line, "'%s' is ambiguous; qualify it with OF/IN", name);
    return found;
}

static Sym *sym_lookup_quiet(const char *name)
{
    for (int i = 0; i < g_nsym; i++)
        if (!g_sym[i].is_filler && !strcmp(g_sym[i].name, name)) return &g_sym[i];
    return NULL;
}

static char g_progid[64];

/* ---- files: SELECT + FD ------------------------------------------------ */

typedef struct {
    char name[64];
    int  line, org, access, optional;
    Tok *assign_lit;                 /* ASSIGN TO literal ... */
    char assign_name[64];            /* ... or to a data-name */
    char status_name[64], key_name[64], report_name[64];
    Sym *assign_sym, *status_sym, *key_sym;
    int  rec;                        /* sym index of the first 01, -1 */
    int  recsize;
    int  org_given;                  /* an ORGANIZATION clause was written */
    int  varying;                    /* mode V: RECORDING MODE V, RECORD CONTAINS m TO n, VARYING, unequal 01s */
    int  minlen, maxlen;             /* from RECORD CONTAINS / VARYING; 0 = unset */
    char dep_name[64]; Sym *dep_sym; /* RECORD IS VARYING ... DEPENDING ON */
} File;

static File *g_files; static int g_nfile, g_fcap;
static int g_cur_fd = -1;            /* the FD whose 01s are being parsed */

static File *file_find(const char *name)
{
    for (int i = 0; i < g_nfile; i++) if (!strcmp(g_files[i].name, name)) return &g_files[i];
    return NULL;
}

/* ---- reports: RD and its groups --------------------------------------- */

typedef struct {
    int column, line;
    int has_pic; char pic[PIC_MAXPAT]; PicInfo pi;
    int has_source; char source_name[64]; char source_qual[64]; int nq;   /* resolved at GENERATE time */
    Tok *value;
    int just, blank_zero;
} RField;

typedef struct {
    int abs, plus, line;
    RField *f; int nf, fcap;
} RLine;

enum { RG_PAGE_HEADING, RG_DETAIL };

typedef struct {
    char name[64];
    int type, line;
    RLine *l; int nl, lcap;
} RGroup;

typedef struct {
    char name[64];
    int line, file;                  /* the FD whose REPORT IS names it */
    int page_limit, heading, first_detail, last_detail;
    RGroup *g; int ng, gcap;
} Report;

static Report *g_reports; static int g_nreport, g_rcap;

/* ---- screens: SCREEN SECTION 01s as slot tables ------------------------ */

typedef struct {
    int kind, flags, line, col, width, srcline;
    Tok *value;
    int has_pic; char pic[PIC_MAXPAT]; PicInfo pi; int blank_zero;
    Sym *item;
} SField;

typedef struct {
    char name[64];
    int line, blank_screen;
    SField *f; int nf, fcap;
} Screen;

static Screen *g_screens; static int g_nscreen, g_scrcap;

static Screen *screen_find(const char *name)
{
    for (int i = 0; i < g_nscreen; i++) if (!strcmp(g_screens[i].name, name)) return &g_screens[i];
    return NULL;
}

static Report *report_find(const char *name)
{
    for (int i = 0; i < g_nreport; i++) if (!strcmp(g_reports[i].name, name)) return &g_reports[i];
    return NULL;
}

static File *file_of_record(Sym *s, int line)
{
    if (s->level != 1 || s->fd < 0) die_at(line, "'%s' is not a record of a file", s->name);
    return &g_files[s->fd];
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
     * just the picture's digits.  docs/dialect.md. */
    if (usage == U_COMP5 && digits <= 2) return 1;
    if (digits <= 4) return 2;
    if (digits <= 9) return 4;
    return 8;
}

static int capacity_digits(int bytes)
{
    return bytes == 1 ? 3 : bytes == 2 ? 5 : bytes == 4 ? 10 : 19;
}

static int is_int_item(Sym *s);

/* elementary size and numeric attributes */
static void sym_finish(Sym *s)
{
    int u = s->usage;
    if (s->is_group) { s->pi.category = PIC_ALPHANUMERIC; return; }
    int native = usage_is_native(u);

    if (!s->has_pic && !native)
        die_at(s->line, "'%s' has no PICTURE clause", s->name);
    if (s->has_pic && native)
        die_at(s->line, "'%s': USAGE %s takes no PICTURE", s->name, usage_name(u));

    if (native) {
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
    if (s->just && pi->category == PIC_NUMERIC)
        die_at(s->line, "'%s': JUSTIFIED is only for alphanumeric items", s->name);
}

static int is_numeric_sym(Sym *s) { return !s->is_group && s->pi.category == PIC_NUMERIC; }

/* Encode a numeric literal into storage described by s, at p. */
static void store_numeric(Sym *s, const NumLit *n, unsigned char *p, int line)
{
    const PicInfo *pi = &s->pi;
    int digits = pi->digits, scale = pi->scale;
    char d[40];
    if (!numlit_align(n, digits, scale, d))
        die_at(line, "VALUE %s%.*s does not fit PICTURE of '%s'", n->neg ? "-" : "",
               n->ndigits, n->digits, s->name);
    int neg = n->neg && pi->is_signed;

    switch (s->usage) {
    case U_DISPLAY:
        memcpy(p, d, digits);
        if (neg) p[digits - 1] = (unsigned char)(d[digits - 1] - '0' + 'p');
        break;
    case U_PACKED: {
        int bytes = s->size;
        memset(p, 0, bytes);
        int nib = bytes * 2 - 2;
        for (int i = digits - 1; i >= 0; i--, nib--) {
            int v = d[i] - '0';
            if (nib & 1) p[nib / 2] |= (unsigned char)v; else p[nib / 2] |= (unsigned char)(v << 4);
        }
        p[bytes - 1] |= pi->is_signed ? (neg ? 0xD : 0xC) : 0xF;
        break;
    }
    default: {
        unsigned long long mag = 0;
        for (int i = 0; i < digits; i++) mag = mag * 10 + (d[i] - '0');
        if (s->size < 8 && (mag >> (s->size * 8 - (pi->is_signed ? 1 : 0))))
            die_at(line, "VALUE does not fit the %d-byte binary item '%s'", s->size, s->name);
        long long v = neg ? -(long long)mag : (long long)mag;
        for (int i = 0; i < s->size; i++) p[i] = (unsigned char)(v >> (8 * i));
        break;
    }
    }
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

static void parse_data_item(void)
{
    int line = cur()->line;
    int level = parse_level();
    if (level < 0) die_at(line, "expected a level number, found %s", tok_desc(cur()));
    advance();

    if (level == 66) die_at(line, "level 66 (RENAMES) is not implemented yet");
    if (!((level >= 1 && level <= 49) || level == 77 || level == 88))
        die_at(line, "level number %d is not valid", level);

    Sym *s = sym_new();
    s->level = level; s->line = line; s->usage = U_DISPLAY;
    s->is_linkage = g_in_linkage;
    if (accept_word("filler")) {
        s->is_filler = 1;
        snprintf(s->name, sizeof s->name, "filler");
    } else if (cur()->kind == T_WORD && !at_word("redefines") && !at_word("pic") &&
               !at_word("picture") && !at_word("value") && !at_word("occurs") && !at_word("usage")) {
        snprintf(s->name, sizeof s->name, "%s", cur()->s);
        advance();
    } else {
        s->is_filler = 1;                       /* 85 lets the name be omitted */
        snprintf(s->name, sizeof s->name, "filler");
    }

    if (level == 88) {
        if (g_last_item < 0) die_at(line, "level 88 '%s' has no conditional variable", s->name);
        s->is_cond = 1;
        s->parent = g_last_item;
        if (!(accept_word("value") || accept_word("values")))
            die_at(line, "level 88 '%s' needs a VALUE clause", s->name);
        accept_word("is"); accept_word("are");
        for (;;) {
            Tok *v = cur();
            if (!(v->kind == T_STR || v->kind == T_NUM || (v->kind == T_WORD && is_figurative(v->s))))
                die_at(v->line, "expected a literal in the VALUE of '%s'", s->name);
            if (s->ncv >= MAXCV) die_at(v->line, "too many values for '%s'", s->name);
            s->cv_lo[s->ncv] = v; s->cv_hi[s->ncv] = NULL;
            advance();
            if (accept_word("thru") || accept_word("through")) {
                Tok *h = cur();
                if (!(h->kind == T_STR || h->kind == T_NUM)) die_at(h->line, "expected a literal after THRU");
                s->cv_hi[s->ncv] = h;
                advance();
            }
            s->ncv++;
            if (cur()->kind == T_PERIOD) break;
        }
        expect_period();
        return;
    }

    g_last_item = sym_idx(s);

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
            if (cur()->kind != T_NUM) die_at(t->line, "expected a count after OCCURS");
            s->occurs = atoi(cur()->s);
            advance();
            if (accept_word("to")) {
                /* OCCURS m TO n DEPENDING ON d: laid out at n (the 85 rule for a
                 * receiving item); d says how many are in use */
                if (cur()->kind != T_NUM) die_at(t->line, "expected the maximum after OCCURS m TO");
                s->odo_min = s->occurs; s->occurs = atoi(cur()->s); advance();
                accept_word("times");
                if (!accept_word("depending")) die_at(t->line, "OCCURS m TO n needs DEPENDING ON");
                accept_word("on");
                if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after DEPENDING ON");
                snprintf(s->odo_dep, sizeof s->odo_dep, "%s", cur()->s); advance();
            }
            accept_word("times");
            for (;;) {
                if (accept_word("ascending") || accept_word("descending")) {
                    accept_word("key"); accept_word("is");
                    while (cur()->kind == T_WORD && !at_word("indexed") && !at_word("ascending") &&
                           !at_word("descending") && !at_word("pic") && !at_word("picture") &&
                           !at_word("value") && !at_word("usage")) advance();
                    continue;
                }
                if (accept_word("indexed")) {
                    accept_word("by");
                    while (cur()->kind == T_WORD && !at_word("pic") && !at_word("picture") &&
                           !at_word("value") && !at_word("usage") && !at_word("ascending") &&
                           !at_word("descending") && !at_word("comp") && !at_word("comp-3") &&
                           !at_word("comp-5") && !at_word("display") && !at_word("sync")) {
                        Sym *ix = sym_new();
                        snprintf(ix->name, sizeof ix->name, "%s", cur()->s);
                        ix->line = cur()->line; ix->usage = U_INDEX; ix->has_usage = 1;
                        ix->is_index = 1; ix->level = 1;
                        int ixi = sym_idx(ix);
                        advance();
                        s = &g_sym[g_last_item];      /* sym_new may have moved the array */
                        if (s->idx1 < 0) s->idx1 = ixi;
                    }
                    continue;
                }
                break;
            }
            if (s->occurs < 1) die_at(t->line, "OCCURS needs a count of at least 1");
            continue;
        }
        if (!strcmp(t->s, "redefines")) {
            advance();
            if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after REDEFINES");
            /* the redefined item must be an earlier sibling in the same group */
            int found = -1;
            for (int i = sym_idx(s) - 1; i >= 0; i--)
                if (!g_sym[i].is_cond && !strcmp(g_sym[i].name, cur()->s) && g_sym[i].level == level) { found = i; break; }
            if (found < 0) die_at(t->line, "'%s' does not redefine an item at level %02d", cur()->s, level);
            s->redefines = found;
            advance();
            continue;
        }
        if (!strcmp(t->s, "sync") || !strcmp(t->s, "synchronized")) {
            advance(); accept_word("left"); accept_word("right");
            s->sync = 1; continue;
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
        if (!strcmp(t->s, "sign"))
            die_at(t->line, "the SIGN clause is not implemented yet");
        if (!strcmp(t->s, "global") || !strcmp(t->s, "external"))
            die_at(t->line, "the %s clause is not implemented yet (stage 6)", t->s);
        die_at(t->line, "unexpected %s in the description of '%s'", tok_desc(t), s->name);
    }
    expect_period();

    if (level == 77 && (s->occurs || s->redefines >= 0))
        die_at(line, "a level 77 item cannot have OCCURS or REDEFINES");
    if (level == 1 && s->occurs)
        die_at(line, "OCCURS is not allowed at level 01");
    if (g_cur_fd >= 0 && level == 1) {
        /* every 01 under an FD is a view of the same record area */
        File *f = &g_files[g_cur_fd];
        s->fd = g_cur_fd;
        if (f->rec < 0) f->rec = sym_idx(s); else s->redefines = f->rec;
    } else if (g_cur_fd >= 0 && level == 77)
        die_at(line, "a level 77 item cannot appear in the FILE SECTION");
}

/* ---- tree, layout, images ------------------------------------------- */

static void build_tree(void)
{
    int stack[64], sp = 0;              /* open items by level */
    for (int i = 0; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond) continue;
        if (s->is_index) { s->record = i; sym_finish(s); continue; }
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
    for (int i = 0; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->is_index) continue;
        if (s->is_group && s->has_pic) die_at(s->line, "'%s' is a group and cannot have a PICTURE", s->name);
        if (s->is_group && s->usage != U_DISPLAY && s->has_usage)
            die_at(s->line, "USAGE on the group '%s' is not implemented yet", s->name);
        if (!s->is_group) sym_finish(s);
    }
    /* level 88 parents: the item they follow; a 88 under an 88 shares it */
    for (int i = 0; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (!s->is_cond) continue;
        int p = s->parent;
        if (g_sym[p].is_cond) s->parent = g_sym[p].parent;
    }
}

static int align_of(Sym *s)
{
    if (!s->sync || s->is_group) return 1;
    switch (s->usage) {
    case U_BINARY: case U_COMP5: case U_SINT: case U_UINT: case U_SSHORT: case U_USHORT:
    case U_POINTER: case U_INDEX:
        return s->size >= 8 ? 8 : s->size;
    default: return 1;
    }
}

/* lay out s at `base` (offset within the record); returns one occurrence's size */
static int layout(int si, int base)
{
    Sym *s = &g_sym[si];
    s->offset = base;
    if (!s->is_group) return s->size;
    int off = base, end = base;
    for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
        Sym *ch = &g_sym[c];
        int cbase;
        if (ch->redefines >= 0) {
            cbase = g_sym[ch->redefines].offset;
        } else {
            int a = align_of(ch);
            cbase = (off + a - 1) / a * a;
        }
        int sz = layout(c, cbase);
        if (sz <= 0) die_at(ch->line, "'%s' has no size", ch->name);
        int cend = cbase + sz * (ch->occurs ? ch->occurs : 1);
        if (ch->redefines < 0) off = cend;
        if (cend > end) end = cend;
        if (ch->redefines >= 0) {
            Sym *r = &g_sym[ch->redefines];
            int rsz = r->size * (r->occurs ? r->occurs : 1);
            if (sz * (ch->occurs ? ch->occurs : 1) > rsz && s->level != 0)
                ; /* larger than the redefined item: allowed here, the group grows */
        }
    }
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
        cnt[ndims] = s->occurs; str[ndims] = s->size; ndims++;
    }
    s->ndims = ndims;
    memcpy(s->dim_count, cnt, ndims * sizeof *cnt); memcpy(s->dim_stride, str, ndims * sizeof *str);
    for (int c = s->child; c >= 0; c = g_sym[c].sibling) set_dims(c, ndims, cnt, str);
}

/* write VALUE / default initialisation for one instance of s at image+base */
static void init_instance(Sym *rec, int si, int base, int defaults);

static void init_one(Sym *rec, int si, int base, int defaults)
{
    Sym *s = &g_sym[si];
    unsigned char *p = rec->image + base;
    if (s->is_group) {
        if (s->value_tok && !g_no_values) {
            Tok *v = s->value_tok;
            if (v->kind != T_STR && !s->value_fig) die_at(v->line, "VALUE of the group '%s' must be a nonnumeric literal", s->name);
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
    int numeric = is_numeric_sym(s);
    if (defaults) {
        if (s->usage == U_DISPLAY && !numeric) memset(p, ' ', s->size);
        else if (s->usage == U_DISPLAY) memset(p, '0', s->size);
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
            die_at(v->line, "the VALUE of the numeric-edited item '%s' must be a nonnumeric literal (X3.23-1985 VALUE clause rule; GnuCOBOL -std=cobol85 agrees)", s->name);
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
    for (int k = 0; k < n; k++) init_one(rec, si, base + k * s->size, defaults);
}

static void finish_data_division(void)
{
    build_tree();
    int nrec = 0;
    for (int i = 0; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0) continue;
        /* a record: 01, 77, or an index */
        int zero[1] = { 0 };
        layout(i, 0);
        set_dims(i, 0, zero, zero);
        s->record = i;
        if (s->is_linkage) snprintf(s->label, sizeof s->label, ".Llk%d_%d", g_unit, nrec++);
        else snprintf(s->label, sizeof s->label, "ws%d_%d", g_unit, nrec++);
    }
    /* propagate record ownership down, and 88s take their parent's dims */
    for (int i = 0; i < g_nsym; i++) {
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
    /* 01 REDEFINES 01: share the earlier record's storage */
    for (int i = 0; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines < 0) continue;
        int r = s->redefines;
        while (g_sym[r].redefines >= 0) r = g_sym[r].redefines;
        s->record = r;
        strcpy(s->label, g_sym[r].label);
        if (s->size > g_sym[r].image_size && s->size > g_sym[r].size) g_sym[r].image_size = s->size;
        for (int j = 0; j < g_nsym; j++) if (g_sym[j].record == i) g_sym[j].record = r;
    }
    /* OCCURS DEPENDING ON: the item must be an integer outside the table */
    for (int i = 0; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (!s->odo_dep[0]) continue;
        s->odo_dep_sym = sym_lookup(s->odo_dep, NULL, 0, s->line);
        if (!is_int_item(s->odo_dep_sym)) die_at(s->line, "DEPENDING ON '%s' must be an integer item", s->odo_dep);
        if (s->odo_dep_sym->record == s->record && s->odo_dep_sym->offset >= s->offset)
            die_at(s->line, "DEPENDING ON '%s' must not be inside or after the table", s->odo_dep);
    }
    /* files: names, status, the record area */
    for (int i = 0; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (f->rec < 0 && !f->report_name[0]) die_at(f->line, "file '%s' has no FD", f->name);
        if (f->assign_name[0]) {
            f->assign_sym = sym_lookup(f->assign_name, NULL, 0, f->line);
            if (g_sym[f->assign_sym->record].is_linkage) die_at(f->line, "ASSIGN TO '%s': a LINKAGE item cannot name a file", f->assign_name);
            if (f->assign_sym->is_group || f->assign_sym->pi.category == PIC_NUMERIC)
                die_at(f->line, "ASSIGN TO '%s': the data-name must be alphanumeric", f->assign_name);
        }
        if (f->status_name[0]) {
            f->status_sym = sym_lookup(f->status_name, NULL, 0, f->line);
            if (g_sym[f->status_sym->record].is_linkage) die_at(f->line, "FILE STATUS '%s' cannot be a LINKAGE item", f->status_name);
            if (f->status_sym->size != 2) die_at(f->line, "FILE STATUS '%s' must be PIC XX", f->status_name);
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
        if (f->maxlen > f->recsize && f->rec >= 0) f->recsize = f->maxlen;
        if (f->dep_name[0]) {
            f->dep_sym = sym_lookup(f->dep_name, NULL, 0, f->line);
            if (!is_int_item(f->dep_sym)) die_at(f->line, "DEPENDING ON '%s' must be an integer item", f->dep_name);
            if (g_sym[f->dep_sym->record].is_linkage) die_at(f->line, "DEPENDING ON '%s' cannot be a LINKAGE item", f->dep_name);
            if (!f->maxlen) f->maxlen = f->recsize;
            if (f->maxlen > f->recsize) die_at(f->line, "FD %s: VARYING TO %d is larger than its record area (%d)", f->name, f->maxlen, f->recsize);
        }
        if (f->key_name[0]) {
            /* the RECORD KEY must be an item inside this file's record */
            Sym *k = NULL; int nk = 0;
            for (int j = 0; j < g_nsym; j++)
                if (!g_sym[j].is_cond && !g_sym[j].is_filler && !strcmp(g_sym[j].name, f->key_name) &&
                    f->rec >= 0 && g_sym[j].record == g_sym[f->rec].record) { k = &g_sym[j]; nk++; }
            if (!k) die_at(f->line, "RECORD KEY '%s' is not an item of file '%s'", f->key_name, f->name);
            if (nk > 1) die_at(f->line, "RECORD KEY '%s' is ambiguous in file '%s'", f->key_name, f->name);
            if (k->ndims) die_at(f->line, "RECORD KEY '%s' cannot be a table item", f->key_name);
            if (k->size < 1 || k->size > 255) die_at(f->line, "RECORD KEY '%s' must be 1 to 255 bytes", f->key_name);
            f->key_sym = k;
        }
        if (f->rec >= 0 && g_sym[f->rec].image_size < f->recsize) g_sym[f->rec].image_size = f->recsize;
    }
    /* images */
    for (int i = 0; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines >= 0) continue;
        if (s->image_size < s->size) s->image_size = s->size;
        s->image = xmalloc(s->image_size);
        if (!s->is_linkage) init_instance(s, i, 0, 1);
    }
    for (int i = 0; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines < 0) continue;
        init_instance(&g_sym[s->record], i, 0, 0);
    }
}

/* ====================================================================== */
/* Emitter                                                                 */
/* ====================================================================== */

static FILE *g_out;
static int g_nlabel;

typedef struct { char label[32]; unsigned char *bytes; int len; } Lit;
static Lit *g_lit; static int g_nlit, g_lcap;

/* descriptors: emitted into .rodata at the end */
typedef struct { unsigned char cat, usage, digits; signed char scale; unsigned char flags; int size; char picstr[PIC_MAXPAT]; } Desc;
static Desc *g_desc; static int g_ndesc, g_dcap;

static int g_noemit;        /* >0 while a lookahead parse runs: no code */

static void emit(const char *fmt, ...)
{
    if (g_noemit) return;
    va_list ap;
    va_start(ap, fmt); vfprintf(g_out, fmt, ap); va_end(ap);
    fputc('\n', g_out);
}

static int new_label(void) { return g_nlabel++; }

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

static int desc_add(const Desc *d)
{
    for (int i = 0; i < g_ndesc; i++)
        if (!memcmp(&g_desc[i], d, sizeof *d)) return i;
    if (g_ndesc == g_dcap) { g_dcap = g_dcap ? g_dcap * 2 : 64; g_desc = realloc(g_desc, g_dcap * sizeof *g_desc); }
    g_desc[g_ndesc] = *d;
    return g_ndesc++;
}

static int sym_desc(Sym *s)
{
    if (s->desc_id >= 0) return s->desc_id;
    Desc d; memset(&d, 0, sizeof d);
    if (s->is_group) { d.cat = COB_GROUP; d.usage = COB_U_DISPLAY; }
    else {
        switch (s->pi.category) {
        case PIC_ALPHABETIC: d.cat = COB_ALPHA; break;
        case PIC_ALPHANUMERIC: d.cat = COB_ALNUM; break;
        case PIC_ALPHANUMERIC_EDITED: d.cat = COB_ALNUM_ED; break;
        case PIC_NUMERIC: d.cat = COB_NUM; break;
        default: d.cat = COB_NUM_ED; break;
        }
        switch (s->usage) {
        case U_DISPLAY: d.usage = COB_U_DISPLAY; break;
        case U_PACKED: d.usage = COB_U_PACKED; break;
        default: d.usage = COB_U_BINARY; break;
        }
        d.digits = (unsigned char)s->pi.digits; d.scale = (signed char)s->pi.scale;
        if (s->pi.is_signed) d.flags |= COB_F_SIGNED;
        if (s->usage == U_COMP5 || usage_is_native(s->usage)) d.flags |= COB_F_NOTRUNC;
        if (s->just) d.flags |= COB_F_JUST;
        if (s->blank_zero) d.flags |= COB_F_BLANKZ;
        if (s->pi.edited) snprintf(d.picstr, sizeof d.picstr, "%s", s->pi.pat);
    }
    d.size = s->size;
    s->desc_id = desc_add(&d);
    return s->desc_id;
}

/* a nonnumeric literal's descriptor */
static int str_desc(int len)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_ALNUM; d.usage = COB_U_DISPLAY; d.size = len;
    return desc_add(&d);
}

/* a numeric literal: DISPLAY digits with a separate leading sign */
static const char *num_lit_label(const NumLit *n, int *desc)
{
    char img[40];
    img[0] = n->neg ? '-' : '+';
    memcpy(img + 1, n->digits, n->ndigits);
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_NUM; d.usage = COB_U_DISPLAY; d.digits = (unsigned char)n->ndigits;
    d.scale = (signed char)n->scale; d.flags = COB_F_SIGNED | COB_F_SEPLEAD; d.size = n->ndigits + 1;
    *desc = desc_add(&d);
    return lit_label((unsigned char *)img, n->ndigits + 1);
}

/* rd = address of sym+off */
static void emit_la_off(const char *rd, const char *sym, int off)
{
    if (off) { emit("\tlui %s, %%hi(%s+%d)", rd, sym, off); emit("\taddi %s, %s, %%lo(%s+%d)", rd, rd, sym, off); }
    else { emit("\tlui %s, %%hi(%s)", rd, sym); emit("\taddi %s, %s, %%lo(%s)", rd, rd, sym); }
}

static void emit_la(const char *rd, const char *sym) { emit_la_off(rd, sym, 0); }

static void emit_desc_addr(const char *rd, int desc)
{
    char b[32]; snprintf(b, sizeof b, ".Ld%d", desc);
    emit_la(rd, b);
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
static void emit_jump(int label) { emit("\tjal r0, .L%d", label); }
static void emit_label(int label) { emit(".L%d:", label); }

static void emit_bytes(const unsigned char *b, int n)
{
    for (int i = 0; i < n; i += 16) {
        fputs("\t.byte ", g_out);
        for (int j = i; j < n && j < i + 16; j++)
            fprintf(g_out, "%s%d", j == i ? "" : ",", b[j]);
        fputc('\n', g_out);
    }
}

/* frame: sp+0 lr, sp+4 r11, sp+8.. operand slots, then three scratch words */
#define FRAME       96
#define SLOT(i)     (8 + 4 * (i))
#define NSLOTS      16
#define SLOT_A      (8 + 4 * NSLOTS)
#define SLOT_B      (SLOT_A + 4)
#define SLOT_C      (SLOT_A + 8)

/* ====================================================================== */
/* Operands and addresses                                                  */
/* ====================================================================== */

typedef struct {
    Sym *sym;
    int nsub;
    struct { Sym *sym; long lit; long adj; } sub[MAXDIM];   /* sym == NULL: literal */
    int line;
    int rm;                         /* reference modification item(start:len) */
    long rm_start, rm_len;          /* literal values, or 0 when an expression / omitted */
    int rm_s0, rm_s1, rm_l0, rm_l1; /* token ranges of the expressions (rm_l0 < 0: no length) */
} Ref;

static void parse_expr(void);
static void emit_expr_tokens(int s0, int s1);

enum { O_REF, O_STR, O_NUM, O_FIG, O_ALL, O_EXPR, O_FUNC };

typedef struct Opnd_ {
    int kind;
    Ref ref;
    Tok *tok;           /* O_STR / O_FIG / O_ALL's literal */
    NumLit num;         /* O_NUM */
    int line;
    int e_start, e_end; /* O_EXPR: token range, re-parsed when emitted */
    int fn; struct Opnd_ *farg; int fsize;   /* O_FUNC: intrinsic, its argument, result width */
} Opnd;

static int is_int_item(Sym *s)
{
    return is_numeric_sym(s) && s->pi.scale == 0;
}

/* a "hot" integer: binary, at most 4 bytes, no scale */
static int is_hot_int(Sym *s)
{
    if (s->is_group || s->pi.category != PIC_NUMERIC || s->pi.scale != 0) return 0;
    if (s->usage == U_DISPLAY || s->usage == U_PACKED) return 0;
    return s->size <= 4;
}

/* identifier [OF|IN qualifier]... [( subscripts )] */
static void parse_ref(Ref *r)
{
    memset(r, 0, sizeof *r);
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a data-name, found %s", tok_desc(t));
    r->line = t->line;
    char *name = t->s; advance();
    char *quals[8]; int nq = 0;
    while (at_word("of") || at_word("in")) {
        advance();
        if (cur()->kind != T_WORD) die_at(cur()->line, "expected a data-name after OF/IN");
        if (nq < 8) quals[nq++] = cur()->s;
        advance();
    }
    r->sym = sym_lookup(name, quals, nq, t->line);
    /* an unsubscripted item's parenthesis holding a ':' is a reference
     * modification, not a subscript list */
    int lead_rm = 0;
    if (cur()->kind == T_LP && r->sym->ndims == 0) {
        int depth = 0;
        for (int i = g_tp; i < g_ntok; i++) {
            if (g_tok[i].kind == T_LP) depth++;
            else if (g_tok[i].kind == T_RP) { if (--depth == 0) break; }
            else if (g_tok[i].kind == T_COLON && depth == 1) { lead_rm = 1; break; }
            else if (g_tok[i].kind == T_PERIOD) break;
        }
    }
    if (cur()->kind == T_LP && !lead_rm) {
        advance();
        for (;;) {
            if (r->nsub >= MAXDIM) die_at(cur()->line, "too many subscripts");
            Tok *st = cur();
            if (st->kind == T_NUM) {
                NumLit n; numlit_parse(st, &n);
                if (!numlit_is_int(&n) || n.neg) die_at(st->line, "a subscript must be a positive integer");
                r->sub[r->nsub].lit = numlit_int(&n);
                advance();
            } else if (st->kind == T_WORD) {
                char *sname = st->s; advance();
                char *sq[8]; int snq = 0;
                while (at_word("of") || at_word("in")) { advance(); if (snq < 8) sq[snq++] = cur()->s; advance(); }
                Sym *ss = sym_lookup(sname, sq, snq, st->line);
                if (!is_int_item(ss)) die_at(st->line, "the subscript '%s' must be an integer item", ss->name);
                if (ss->ndims) die_at(st->line, "a subscript cannot itself be subscripted in COBOL 85");
                r->sub[r->nsub].sym = ss;
                if (at_op("+") || at_op("-")) {
                    int neg = at_op("-"); advance();
                    if (cur()->kind != T_NUM) die_at(cur()->line, "expected an integer after '%s' in a subscript", neg ? "-" : "+");
                    NumLit n; numlit_parse(cur(), &n);
                    r->sub[r->nsub].adj = neg ? -numlit_int(&n) : numlit_int(&n);
                    advance();
                }
            } else die_at(st->line, "expected a subscript, found %s", tok_desc(st));
            r->nsub++;
            if (cur()->kind == T_RP) { advance(); break; }
        }
    }
    /* item(start:len) -- after the subscripts, or alone on an unsubscripted
     * item: the parenthesis holds a ':' at depth one */
    int is_rm = 0;
    if (cur()->kind == T_LP) {
        int depth = 0;
        for (int i = g_tp; i < g_ntok; i++) {
            if (g_tok[i].kind == T_LP) depth++;
            else if (g_tok[i].kind == T_RP) { if (--depth == 0) break; }
            else if (g_tok[i].kind == T_COLON && depth == 1) { is_rm = 1; break; }
            else if (g_tok[i].kind == T_PERIOD) break;
        }
    }
    if (is_rm) {
        if (r->sym->is_cond) die_at(r->line, "a condition-name cannot be reference-modified");
        advance();
        r->rm = 1; r->rm_l0 = -1;
        if (cur()->kind == T_NUM && peek(1)->kind == T_COLON) {
            NumLit n; numlit_parse(cur(), &n);
            if (!numlit_is_int(&n) || n.neg || numlit_int(&n) < 1) die_at(cur()->line, "the start of a reference modification must be a positive integer");
            r->rm_start = (long)numlit_int(&n); advance();
        } else {
            r->rm_s0 = g_tp; g_noemit++; parse_expr(); g_noemit--; r->rm_s1 = g_tp;
        }
        if (cur()->kind != T_COLON) die_at(cur()->line, "expected ':' in the reference modification");
        advance();
        if (cur()->kind == T_RP) { /* (start:) runs to the end */ }
        else if (cur()->kind == T_NUM && peek(1)->kind == T_RP) {
            NumLit n; numlit_parse(cur(), &n);
            if (!numlit_is_int(&n) || n.neg || numlit_int(&n) < 1) die_at(cur()->line, "the length of a reference modification must be a positive integer");
            r->rm_len = (long)numlit_int(&n); advance();
        } else {
            r->rm_l0 = g_tp; g_noemit++; parse_expr(); g_noemit--; r->rm_l1 = g_tp;
        }
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the reference modification");
        advance();
        if (r->rm_start && r->rm_start > r->sym->size) die_at(r->line, "reference modification starts past the end of '%s'", r->sym->name);
        if (r->rm_start && r->rm_len && r->rm_start - 1 + r->rm_len > r->sym->size) die_at(r->line, "reference modification runs past the end of '%s'", r->sym->name);
        if (r->rm_start && !r->rm_len && r->rm_l0 < 0) r->rm_len = r->sym->size - r->rm_start + 1;
    }
    if (r->nsub != r->sym->ndims) {
        if (r->sym->ndims == 0) die_at(r->line, "'%s' is not a table item and takes no subscript", r->sym->name);
        die_at(r->line, "'%s' needs %d subscript%s, %d given", r->sym->name, r->sym->ndims,
               r->sym->ndims == 1 ? "" : "s", r->nsub);
    }
    for (int i = 0; i < r->nsub; i++)
        if (!r->sub[i].sym && (r->sub[i].lit < 1 || r->sub[i].lit > r->sym->dim_count[i]))
            die_at(r->line, "subscript %ld is outside OCCURS %d of '%s'", r->sub[i].lit, r->sym->dim_count[i], r->sym->name);
}

enum { FN_UPPER, FN_LOWER, FN_CURDATE };
static int opnd_size(Opnd *o);

static void numlit_from_int(NumLit *n, long v)
{
    memset(n, 0, sizeof *n);
    char b[24]; snprintf(b, sizeof b, "%ld", v < 0 ? -v : v);
    n->neg = v < 0; n->ndigits = (int)strlen(b); memcpy(n->digits, b, n->ndigits);
}

static void parse_operand(Opnd *o)
{
    memset(o, 0, sizeof *o);
    Tok *t = cur();
    o->line = t->line;
    /* LENGTH OF item: the IBM register the corpus writes (damm), the same
     * compile-time size as FUNCTION LENGTH; a data item named LENGTH wins */
    if (t->kind == T_WORD && !strcmp(t->s, "length") && g_tp + 1 < g_ntok &&
        g_tok[g_tp + 1].kind == T_WORD && !strcmp(g_tok[g_tp + 1].s, "of") && !sym_lookup_quiet("length")) {
        advance(); advance();
        Opnd x; parse_operand(&x);
        if (x.kind != O_REF) die_at(t->line, "LENGTH OF takes a data item");
        int len = opnd_size(&x);
        if (len < 0) die_at(t->line, "LENGTH OF a reference modification with a variable length is not implemented");
        o->kind = O_NUM; numlit_from_int(&o->num, len);
        return;
    }
    if (t->kind == T_WORD && !strcmp(t->s, "function")) {
        advance();
        Tok *n = cur();
        if (n->kind != T_WORD) die_at(n->line, "expected an intrinsic function name");
        if (!strcmp(n->s, "upper-case")) o->fn = FN_UPPER;
        else if (!strcmp(n->s, "lower-case")) o->fn = FN_LOWER;
        else if (!strcmp(n->s, "current-date")) {
            advance();
            o->kind = O_FUNC; o->fn = FN_CURDATE; o->fsize = 21;
            return;
        } else if (!strcmp(n->s, "length")) {
            /* known at compile time, except for a variable reference modification */
            advance();
            if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION LENGTH");
            advance();
            Opnd x; parse_operand(&x);
            if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the function argument");
            advance();
            if (x.kind != O_REF && x.kind != O_STR && x.kind != O_FUNC) die_at(n->line, "FUNCTION LENGTH takes an item or a literal");
            int len = opnd_size(&x);
            if (len < 0) die_at(n->line, "FUNCTION LENGTH of a reference modification with a variable length is not implemented");
            o->kind = O_NUM; numlit_from_int(&o->num, len);
            return;
        }
        else die_at(n->line, "FUNCTION %s is not implemented", n->s);
        advance();
        if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION %s", n->s);
        advance();
        o->farg = xmalloc(sizeof *o->farg);
        parse_operand(o->farg);
        if (o->farg->kind != O_REF && o->farg->kind != O_STR)
            die_at(n->line, "FUNCTION %s takes an alphanumeric item or literal", n->s);
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the function argument");
        advance();
        o->kind = O_FUNC;
        o->fsize = o->farg->kind == O_REF ? o->farg->ref.sym->size : o->farg->tok->len;
        return;
    }
    if (t->kind == T_STR) { o->kind = O_STR; o->tok = t; advance(); return; }
    if (t->kind == T_NUM) { o->kind = O_NUM; numlit_parse(t, &o->num); advance(); return; }
    if (t->kind == T_WORD && is_figurative(t->s)) { o->kind = O_FIG; o->tok = t; advance(); return; }
    if (t->kind == T_WORD && !strcmp(t->s, "all")) {
        advance();
        if (cur()->kind == T_STR) { o->kind = O_ALL; o->tok = cur(); advance(); return; }
        if (cur()->kind == T_WORD && is_figurative(cur()->s)) { o->kind = O_FIG; o->tok = cur(); advance(); return; }
        die_at(t->line, "expected a literal after ALL");
    }
    o->kind = O_REF;
    parse_ref(&o->ref);
}

static int ref_needs_call(const Ref *r)
{
    for (int i = 0; i < r->nsub; i++)
        if (r->sub[i].sym && !is_hot_int(r->sub[i].sym)) return 1;
    if (r->rm && !r->rm_start) return 1;           /* the start is an expression */
    return 0;
}

/* the literal length of a reference-modified item, or -1 when it is
 * only known at run time */
static int ref_static_len(const Ref *r)
{
    if (!r->rm) return r->sym->size;
    return r->rm_len ? (int)r->rm_len : -1;
}

/* load the integer value of a hot item at address in areg into dreg */
static void emit_load_int(Sym *s, const char *areg, const char *dreg)
{
    int sg = s->pi.is_signed;
    if (s->size == 1) emit("\t%s %s, %s+0", sg ? "ldb" : "ldbu", dreg, areg);
    else if (s->size == 2) emit("\t%s %s, %s+0", sg ? "ldh" : "ldhu", dreg, areg);
    else emit("\tldw %s, %s+0", dreg, areg);
}

static void emit_store_int(Sym *s, const char *areg, const char *vreg)
{
    if (s->size == 1) emit("\tstb %s+0, %s", areg, vreg);
    else if (s->size == 2) emit("\tsth %s+0, %s", areg, vreg);
    else emit("\tstw %s+0, %s", areg, vreg);
}

/* reg = address of item s plus off: WORKING-STORAGE by label, a LINKAGE
 * item through its cell, which the entry sequence filled from the
 * caller's argument register */
static void emit_item_addr(const char *reg, Sym *s, int off)
{
    Sym *rec = &g_sym[s->record];
    if (!rec->is_linkage) { emit_la_off(reg, rec->label, off); return; }
    emit_la(reg, rec->label);
    emit("\tldw %s, %s+0", reg, reg);
    if (off >= -2048 && off <= 2047) { if (off) emit("\taddi %s, %s, %d", reg, reg, off); }
    else { emit_li("r2", off); emit("\tadd %s, %s, r2", reg, reg); }
}

static int ref_has_runtime_sub(const Ref *r)
{
    for (int i = 0; i < r->nsub; i++) if (r->sub[i].sym) return 1;
    if (r->rm && !r->rm_start) return 1;
    return 0;
}

/* reg = address of the reference.  Literal subscripts fold into the
 * displacement.  Runtime subscripts accumulate in r11 (callee-saved, so a
 * cob_load_int call for a DISPLAY-numeric subscript does not lose the
 * sum); r1/r2 are scratch.  A reference whose subscript needs that call
 * clobbers r3-r10, so callers stage such operands through frame slots
 * (emit_args) before loading argument registers. */
static void emit_ref_addr(const Ref *r, const char *reg)
{
    Sym *s = r->sym;
    int off = s->offset;
    int runtime = ref_has_runtime_sub(r);
    for (int i = 0; i < r->nsub; i++)
        if (!r->sub[i].sym) off += (int)(r->sub[i].lit - 1) * s->dim_stride[i];
    if (r->rm && r->rm_start) off += (int)r->rm_start - 1;
    if (runtime) emit("\tadd r11, r0, r0");
    for (int i = 0; i < r->nsub; i++) {
        if (!r->sub[i].sym) continue;
        Sym *ss = r->sub[i].sym;
        if (is_hot_int(ss)) {
            emit_item_addr("r1", ss, ss->offset);
            emit_load_int(ss, "r1", "r1");
        } else {
            emit_item_addr("r3", ss, ss->offset);
            emit_desc_addr("r4", sym_desc(ss));
            emit_call("cob_load_int");
        }
        long adj = r->sub[i].adj - 1;
        if (adj) emit("\taddi r1, r1, %ld", adj);
        emit_li("r2", s->dim_stride[i]);
        emit("\tmul r1, r1, r2");
        emit("\tadd r11, r11, r1");
    }
    if (r->rm && !r->rm_start) {
        /* the start expression: onto the numeric stack, then off as an int */
        emit_expr_tokens(r->rm_s0, r->rm_s1);
        emit_call("cob_pop_int");
        emit("\taddi r1, r1, -1");
        emit("\tadd r11, r11, r1");
    }
    emit_item_addr(reg, s, off);
    if (runtime) emit("\tadd %s, %s, r11", reg, reg);
}

/* ---- argument staging ------------------------------------------------- */

enum { A_REF, A_LABEL, A_DESC, A_IMM, A_FUNC, A_VALUE, A_RDESC, A_RLEN };
typedef struct { int kind; const Ref *ref; const char *label; int desc; long imm; Opnd *fn; } Arg;
static Arg arg_func(Opnd *o)       { Arg a = { A_FUNC, 0, 0, 0, 0, o }; return a; }
static Arg arg_value(Opnd *o)      { Arg a = { A_VALUE, 0, 0, 0, 0, o }; return a; }
static Arg arg_rdesc(const Ref *r) { Arg a = { A_RDESC, r, 0, 0, 0, 0 }; return a; }
static Arg arg_rlen(const Ref *r)  { Arg a = { A_RLEN, r, 0, 0, 0, 0 }; return a; }
static int g_slot_base;             /* staged operands of nested evaluations use higher slots */

static Arg arg_ref(const Ref *r)   { Arg a = { A_REF, r, 0, 0, 0, 0 }; return a; }
static Arg arg_label(const char *l){ Arg a = { A_LABEL, 0, l, 0, 0, 0 }; return a; }
static Arg arg_desc(int d)         { Arg a = { A_DESC, 0, 0, d, 0, 0 }; return a; }
static Arg arg_imm(long v)         { Arg a = { A_IMM, 0, 0, 0, v, 0 }; return a; }

static const char *argreg(int i)
{
    static const char *r[] = { "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10" };
    return r[i];
}

/* load r3.. with the arguments; operands whose address needs a runtime
 * call are computed first and parked in frame slots */
/* r1 = the reference modification's start; its length in SLOT(slot).
 * The expressions may stage operands of their own, above this call's
 * slots. */
static void emit_rm_start_len(const Ref *r, int slot)
{
    if (r->rm_len) emit_li("r1", r->rm_len);
    else if (r->rm_l0 >= 0) { emit_expr_tokens(r->rm_l0, r->rm_l1); emit_call("cob_pop_int"); }
    else emit_li("r1", 0);
    emit("\tstw sp+%d, r1", SLOT(slot));
    if (r->rm_start) emit_li("r1", r->rm_start);
    else { emit_expr_tokens(r->rm_s0, r->rm_s1); emit_call("cob_pop_int"); }
}

static void emit_args(const Arg *a, int n)
{
    int slotted[8] = { 0 };
    int base = g_slot_base;
    if (base + n > NSLOTS) die_at(cur()->line, "internal: too many staged operands");
    g_slot_base += n;
    for (int i = 0; i < n; i++) {
        if (a[i].kind == A_REF && ref_needs_call(a[i].ref)) {
            emit_ref_addr(a[i].ref, "r1");
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_RDESC || a[i].kind == A_RLEN) {
            const Ref *r = a[i].ref;
            emit_rm_start_len(r, base + i);
            emit("\tadd r4, r1, r0");
            emit("\tldw r5, sp+%d", SLOT(base + i));
            emit_desc_addr("r3", sym_desc(r->sym));
            emit_call(a[i].kind == A_RDESC ? "cob_refmod_desc" : "cob_refmod_len");
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_VALUE) {
            /* BY VALUE: the item's integer value, widened to a word */
            Opnd *o = a[i].fn;
            if (o->kind == O_REF && is_hot_int(o->ref.sym)) { emit_ref_addr(&o->ref, "r3"); emit_load_int(o->ref.sym, "r3", "r1"); }
            else if (o->kind == O_REF) { emit_ref_addr(&o->ref, "r3"); emit_desc_addr("r4", sym_desc(o->ref.sym)); emit_call("cob_load_int"); }
            else emit_li("r1", (long)numlit_int(&o->num));
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_FUNC) {
            /* an intrinsic: evaluate into libcob's buffer, park the pointer */
            Opnd *f = a[i].fn, *x = f->farg;
            if (f->fn == FN_CURDATE) emit_call("cob_fn_current_date");
            else {
                if (x->kind == O_REF) emit_ref_addr(&x->ref, "r3");
                else emit_la("r3", lit_label((unsigned char *)x->tok->s, x->tok->len));
                emit_li("r4", f->fsize);
                emit_call(f->fn == FN_UPPER ? "cob_fn_upper" : "cob_fn_lower");
            }
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        }
    }
    for (int i = 0; i < n; i++) {
        const char *reg = argreg(i);
        if (slotted[i]) { emit("\tldw %s, sp+%d", reg, SLOT(base + i)); continue; }
        switch (a[i].kind) {
        case A_REF:   emit_ref_addr(a[i].ref, reg); break;
        case A_LABEL: emit_la(reg, a[i].label); break;
        case A_DESC:  emit_desc_addr(reg, a[i].desc); break;
        case A_IMM:   emit_li(reg, a[i].imm); break;
        default: die_at(cur()->line, "internal: unstaged argument kind");
        }
    }
    g_slot_base = base;
}

/* address + descriptor of an operand, as two Args.  Figuratives need the
 * other operand's size and are expanded by the caller. */
static void opnd_args(Opnd *o, Arg *addr, Arg *desc, int other_size, int other_numeric)
{
    switch (o->kind) {
    case O_REF:
        *addr = arg_ref(&o->ref);
        if (!o->ref.rm) *desc = arg_desc(sym_desc(o->ref.sym));
        else if (o->ref.rm_len) *desc = arg_desc(str_desc((int)o->ref.rm_len));
        else *desc = arg_rdesc(&o->ref);
        return;
    case O_FUNC:
        *addr = arg_func(o); *desc = arg_desc(str_desc(o->fsize)); return;
    case O_STR:
        *addr = arg_label(lit_label((unsigned char *)o->tok->s, o->tok->len));
        *desc = arg_desc(str_desc(o->tok->len)); return;
    case O_NUM: {
        int d; const char *l = num_lit_label(&o->num, &d);
        *addr = arg_label(l); *desc = arg_desc(d); return;
    }
    case O_FIG: case O_ALL: {
        /* ZERO against a numeric item is the number; otherwise a fill of
         * the other operand's length */
        if (o->kind == O_FIG && other_numeric && !strncmp(o->tok->s, "zero", 4)) {
            NumLit z; numlit_zero(&z);
            int d; const char *l = num_lit_label(&z, &d);
            *addr = arg_label(l); *desc = arg_desc(d); return;
        }
        int n = other_size > 0 ? other_size : 1;
        unsigned char *buf = xmalloc(n);
        if (o->kind == O_ALL) for (int i = 0; i < n; i++) buf[i] = (unsigned char)o->tok->s[i % o->tok->len];
        else memset(buf, fig_byte(o->tok->s), n);
        *addr = arg_label(lit_label(buf, n)); *desc = arg_desc(str_desc(n));
        free(buf);
        return;
    }
    }
}

static int opnd_size(Opnd *o)
{
    switch (o->kind) {
    case O_REF: return ref_static_len(&o->ref);
    case O_STR: return o->tok->len;
    case O_NUM: return o->num.ndigits;
    case O_FUNC: return o->fsize;
    default: return 0;
    }
}

static int opnd_numeric(Opnd *o)
{
    if (o->kind == O_REF) return !o->ref.rm && is_numeric_sym(o->ref.sym);
    return o->kind == O_NUM || o->kind == O_EXPR;
}

/* the byte length of an operand as an Arg: a literal, or for a
 * reference-modified item whose length is an expression, evaluated */
static Arg arg_len(Opnd *o)
{
    if (o->kind == O_REF && o->ref.rm && !o->ref.rm_len) return arg_rlen(&o->ref);
    return arg_imm(opnd_size(o));
}

/* an integer operand usable on the hot path: a hot-int item, or an
 * integer literal that fits a word */
static int opnd_hot_int(Opnd *o)
{
    if (o->kind == O_REF) return !o->ref.rm && is_hot_int(o->ref.sym) && !(o->ref.sym->size == 4 && !o->ref.sym->pi.is_signed);
    if (o->kind == O_NUM) return numlit_is_int(&o->num) && numlit_int(&o->num) <= 2147483647LL && numlit_int(&o->num) >= -2147483647LL;
    if (o->kind == O_FIG) return !strncmp(o->tok->s, "zero", 4);
    return 0;
}

/* r1 = integer value of a hot operand; uses r3 (address) and r1/r2/r11 */
static void emit_hot_value(Opnd *o)
{
    if (o->kind == O_NUM) { emit_li("r1", (long)numlit_int(&o->num)); return; }
    if (o->kind == O_FIG) { emit_li("r1", 0); return; }
    emit_ref_addr(&o->ref, "r3");
    emit_load_int(o->ref.sym, "r3", "r1");
}

static long pow10l(int n) { long v = 1; while (n-- > 0) v *= 10; return v; }

/* truncate r1 to the receiver's picture when it is a COMP item (COMP-5 and
 * the C types keep the binary field's capacity) */
static void emit_trunc(Sym *s)
{
    if (s->usage != U_BINARY) return;
    if (s->pi.digits >= capacity_digits(s->size)) return;
    emit_li("r2", pow10l(s->pi.digits));
    emit("\trem r1, r1, r2");
}


/* ====================================================================== */
/* Conditions                                                              */
/* ====================================================================== */

enum { C_AND, C_OR, C_NOT, C_REL, C_CLASS };
enum { R_EQ, R_LT, R_GT, R_LE, R_GE, R_NE };

typedef struct Cond {
    int kind;
    struct Cond *a, *b;
    Opnd x, y;
    int op, neg;            /* C_REL */
    int klass;              /* C_CLASS: 0 NUMERIC 1 ALPHABETIC 2 LOWER 3 UPPER, 4+i SPECIAL-NAMES class i */
} Cond;

static Cond *cond_new(int kind) { Cond *c = xmalloc(sizeof *c); c->kind = kind; return c; }

static Cond *cond_rel(Opnd *x, int op, Opnd *y, int neg)
{
    Cond *c = cond_new(C_REL);
    c->x = *x; c->y = *y; c->op = op; c->neg = neg;
    return c;
}

static Cond *cond_bin(int kind, Cond *a, Cond *b) { Cond *c = cond_new(kind); c->a = a; c->b = b; return c; }

static Cond *parse_cond(void);

static Opnd expr_opnd(void);
static int paren_is_condition(void);
static int at_arith_op(void);
static void emit_push_opnd(Opnd *o);

/* a condition operand: a plain operand, or an arithmetic expression */
static Opnd parse_cond_operand(void)
{
    if (cur()->kind == T_LP && !paren_is_condition()) return expr_opnd();
    int start = g_tp;
    Opnd x; parse_operand(&x);
    if (at_arith_op()) { g_tp = start; return expr_opnd(); }
    return x;
}

static Opnd lit_opnd(Tok *t)
{
    Opnd o; memset(&o, 0, sizeof o);
    o.line = t->line;
    if (t->kind == T_STR) { o.kind = O_STR; o.tok = t; }
    else if (t->kind == T_NUM) { o.kind = O_NUM; numlit_parse(t, &o.num); }
    else { o.kind = O_FIG; o.tok = t; }
    return o;
}

/* level 88: (parent = v1) OR (parent >= lo AND parent <= hi) OR ... */
static Cond *cond_88(Ref *r, int neg)
{
    Sym *c = r->sym;
    Opnd p; memset(&p, 0, sizeof p);
    p.kind = O_REF; p.ref = *r; p.ref.sym = &g_sym[c->parent]; p.line = r->line;
    Cond *all = NULL;
    for (int i = 0; i < c->ncv; i++) {
        Opnd lo = lit_opnd(c->cv_lo[i]);
        Cond *one;
        if (c->cv_hi[i]) {
            Opnd hi = lit_opnd(c->cv_hi[i]);
            one = cond_bin(C_AND, cond_rel(&p, R_GE, &lo, 0), cond_rel(&p, R_LE, &hi, 0));
        } else one = cond_rel(&p, R_EQ, &lo, 0);
        all = all ? cond_bin(C_OR, all, one) : one;
    }
    if (neg) { Cond *n = cond_new(C_NOT); n->a = all; return n; }
    return all;
}

static Cond *parse_simple(void)
{
    int line = cur()->line;
    Opnd x = parse_cond_operand();
    accept_word("is");
    int neg = 0;
    if (accept_word("not")) neg = 1;
    Tok *t = cur();

    if (t->kind == T_WORD) {
        int klass = -1;
        if (!strcmp(t->s, "numeric")) klass = 0;
        else if (!strcmp(t->s, "alphabetic")) klass = 1;
        else if (!strcmp(t->s, "alphabetic-lower")) klass = 2;
        else if (!strcmp(t->s, "alphabetic-upper")) klass = 3;
        if (klass < 0)
            for (int i = 0; i < g_nclass; i++) if (!strcmp(t->s, g_class[i].name)) klass = 4 + i;
        if (klass >= 0) {
            if (x.kind != O_REF) die_at(line, "a class condition needs a data item");
            advance();
            Cond *c = cond_new(C_CLASS); c->x = x; c->klass = klass; c->neg = neg;
            return c;
        }
        int sop = -1;
        if (!strcmp(t->s, "positive")) sop = R_GT;
        else if (!strcmp(t->s, "negative")) sop = R_LT;
        else if (!strcmp(t->s, "zero") || !strcmp(t->s, "zeros") || !strcmp(t->s, "zeroes")) sop = R_EQ;
        if (sop >= 0) {
            if (!opnd_numeric(&x)) {
                if (sop != R_EQ) die_at(line, "a sign condition needs a numeric operand");
                /* alphanumeric compared with ZERO: the figurative */
                advance();
                Opnd z = lit_opnd(t);
                return cond_rel(&x, R_EQ, &z, neg);
            }
            advance();
            Opnd z; memset(&z, 0, sizeof z); z.kind = O_NUM; numlit_zero(&z.num); z.line = line;
            return cond_rel(&x, sop, &z, neg);
        }
    }

    int op = -1;
    if (t->kind == T_OP) {
        if (!strcmp(t->s, "=")) op = R_EQ;
        else if (!strcmp(t->s, "<")) op = R_LT;
        else if (!strcmp(t->s, ">")) op = R_GT;
        else if (!strcmp(t->s, "<=")) op = R_LE;
        else if (!strcmp(t->s, ">=")) op = R_GE;
        else if (!strcmp(t->s, "<>")) op = R_NE;
        if (op >= 0) advance();
    } else if (t->kind == T_WORD) {
        if (!strcmp(t->s, "equal") || !strcmp(t->s, "equals")) { advance(); accept_word("to"); op = R_EQ; }
        else if (!strcmp(t->s, "greater")) {
            advance(); accept_word("than"); op = R_GT;
            if (at_word("or")) { advance(); expect_word("equal"); accept_word("to"); op = R_GE; }
        } else if (!strcmp(t->s, "less")) {
            advance(); accept_word("than"); op = R_LT;
            if (at_word("or")) { advance(); expect_word("equal"); accept_word("to"); op = R_LE; }
        }
    }
    if (op < 0) {
        if (x.kind == O_REF && x.ref.sym->is_cond) {
            if (neg == 0 && is_word(t, "not")) { /* handled above */ }
            return cond_88(&x.ref, neg);
        }
        if (x.kind == O_REF && !neg)
            die_at(line, "expected a relational operator after '%s' (abbreviated combined conditions are not implemented)", x.ref.sym->name);
        die_at(line, "expected a relational operator, found %s", tok_desc(t));
    }
    Opnd y = parse_cond_operand();
    if (x.kind != O_REF && y.kind != O_REF && x.kind != O_EXPR && y.kind != O_EXPR)
        die_at(line, "a condition needs at least one data item");
    return cond_rel(&x, op, &y, neg);
}

static Cond *parse_not(void)
{
    if (accept_word("not")) { Cond *c = cond_new(C_NOT); c->a = parse_not(); return c; }
    if (cur()->kind == T_LP && paren_is_condition()) {
        advance(); Cond *c = parse_cond();
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')'");
        advance(); return c;
    }
    return parse_simple();
}

static Cond *parse_and(void)
{
    Cond *a = parse_not();
    while (accept_word("and")) a = cond_bin(C_AND, a, parse_not());
    return a;
}

static Cond *parse_cond(void)
{
    Cond *a = parse_and();
    while (accept_word("or")) a = cond_bin(C_OR, a, parse_and());
    return a;
}

/* r1 = 0/1 for a simple condition */
static void emit_cond_value(Cond *c)
{
    if (c->kind == C_CLASS) {
        Arg a[3]; Arg d;
        opnd_args(&c->x, &a[0], &d, 0, 0); a[1] = d;
        if (c->klass >= 4) {    /* a SPECIAL-NAMES class: its table */
            a[2] = arg_label(lit_label(g_class[c->klass - 4].tab, 256));
            emit_args(a, 3);
            emit_call("cob_class_user");
        } else {
            a[2] = arg_imm(c->klass);
            emit_args(a, 3);
            emit_call("cob_class");
        }
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    /* C_REL */
    if (c->x.kind == O_EXPR || c->y.kind == O_EXPR) {
        emit_push_opnd(&c->x);
        emit_push_opnd(&c->y);
        emit_call("cob_ncmp");
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r1, r0"); break;
        case R_NE: emit("\tsne r1, r1, r0"); break;
        case R_LT: emit("\tslt r1, r1, r0"); break;
        case R_GT: emit("\tsgt r1, r1, r0"); break;
        case R_LE: emit("\tsle r1, r1, r0"); break;
        case R_GE: emit("\tsge r1, r1, r0"); break;
        }
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    if (opnd_hot_int(&c->x) && opnd_hot_int(&c->y)) {
        emit_hot_value(&c->x);
        emit("\tstw sp+%d, r1", SLOT_A);
        emit_hot_value(&c->y);
        emit("\tldw r2, sp+%d", SLOT_A);
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r2, r1"); break;
        case R_NE: emit("\tsne r1, r2, r1"); break;
        case R_LT: emit("\tslt r1, r2, r1"); break;
        case R_GT: emit("\tsgt r1, r2, r1"); break;
        case R_LE: emit("\tsle r1, r2, r1"); break;
        case R_GE: emit("\tsge r1, r2, r1"); break;
        }
    } else {
        Arg a[4];
        int xs = opnd_size(&c->x), ys = opnd_size(&c->y);
        int xn = opnd_numeric(&c->x), yn = opnd_numeric(&c->y);
        opnd_args(&c->x, &a[0], &a[1], ys, yn);
        opnd_args(&c->y, &a[2], &a[3], xs, xn);
        emit_args(a, 4);
        emit_call("cob_cmp");
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r1, r0"); break;
        case R_NE: emit("\tsne r1, r1, r0"); break;
        case R_LT: emit("\tslt r1, r1, r0"); break;
        case R_GT: emit("\tsgt r1, r1, r0"); break;
        case R_LE: emit("\tsle r1, r1, r0"); break;
        case R_GE: emit("\tsge r1, r1, r0"); break;
        }
    }
    if (c->neg) emit("\txori r1, r1, 1");
}

static void cond_jump_true(Cond *c, int L);

static void cond_jump_false(Cond *c, int L)
{
    switch (c->kind) {
    case C_AND: cond_jump_false(c->a, L); cond_jump_false(c->b, L); return;
    case C_OR: { int Lt = new_label(); cond_jump_true(c->a, Lt); cond_jump_false(c->b, L); emit_label(Lt); return; }
    case C_NOT: cond_jump_true(c->a, L); return;
    default: emit_cond_value(c); emit("\tbeq r1, r0, .L%d", L); return;
    }
}

static void cond_jump_true(Cond *c, int L)
{
    switch (c->kind) {
    case C_AND: { int Ls = new_label(); cond_jump_false(c->a, Ls); cond_jump_true(c->b, L); emit_label(Ls); return; }
    case C_OR: cond_jump_true(c->a, L); cond_jump_true(c->b, L); return;
    case C_NOT: cond_jump_false(c->a, L); return;
    default: emit_cond_value(c); emit("\tbne r1, r0, .L%d", L); return;
    }
}

/* ====================================================================== */
/* Procedure Division: statements                                          */
/* ====================================================================== */

static int is_verb(const char *w)
{
    static const char *verbs[] = { "accept", "add", "alter", "call", "cancel", "close",
        "compute", "continue", "delete", "disable", "display", "divide", "enable",
        "enter", "evaluate", "exit", "generate", "go", "goback", "if", "initialize",
        "initiate", "inspect", "merge", "move", "multiply", "open", "perform", "purge",
        "read", "receive", "release", "return", "rewrite", "search", "send", "set",
        "sort", "start", "stop", "string", "subtract", "suppress", "terminate",
        "unstring", "use", "write", "next", NULL };
    for (int i = 0; verbs[i]; i++) if (!strcmp(w, verbs[i])) return 1;
    return 0;
}

static int is_terminator(const char *w)
{
    static const char *t[] = { "else", "end-if", "end-perform", "when", "end-evaluate",
        "end-read", "end-write", "end-add", "end-subtract", "end-multiply", "end-divide",
        "end-compute", "end-call", "end-string", "end-unstring", "end-search", "end-start",
        "end-delete", "end-rewrite", "end-return", "end-accept", "end-display", "end-program", NULL };
    for (int i = 0; t[i]; i++) if (!strcmp(w, t[i])) return 1;
    return 0;
}

static int at_scope_end(void)
{
    Tok *t = cur();
    if (t->kind == T_PERIOD || t->kind == T_EOF) return 1;
    if (t->kind != T_WORD) return 0;
    if (!strcmp(t->s, "not") && (is_word(peek(1), "on") || is_word(peek(1), "size") ||
                                 is_word(peek(1), "at") || is_word(peek(1), "invalid") ||
                                 is_word(peek(1), "overflow"))) return 1;
    return is_terminator(t->s);
}

/* the operand list of a statement continues while the next token can
 * start an operand and is not a verb or a clause word */
static int at_operand(void)
{
    Tok *t = cur();
    if (t->kind == T_STR || t->kind == T_NUM) return 1;
    if (t->kind != T_WORD) return 0;
    if (is_verb(t->s) || is_terminator(t->s)) return 0;
    static const char *clause[] = { "to", "from", "by", "into", "giving", "rounded", "on",
        "size", "upon", "with", "thru", "through", "until", "varying", "times", "after",
        "before", "remainder", "depending", "corresponding", "corr", "then", "and", "or",
        "is", "not", "up", "down", "delimited", "pointer", "overflow", "at", "next", "record",
        "key", "invalid", "advancing", "lines", "line", "page", "input", "output", "i-o",
        "extend", "lock", "rewind", "end-string", "returning", "reference", "content",
        "exception", "end-call", "also", "when", "other", "tallying", "replacing", "converting",
        "characters", "leading", "first", "initial", "true", "false", "any", "end-search", NULL };
    for (int i = 0; clause[i]; i++) if (!strcmp(t->s, clause[i])) return 0;
    return 1;
}

static void parse_statement(void);
static void parse_statements(void) { while (!at_scope_end()) parse_statement(); }

static int g_sentence_label = -1;   /* NEXT SENTENCE target, made on demand */

/* ---- paragraphs ------------------------------------------------------- */

typedef struct { char name[64]; int id, is_section, line; } Para;
static Para *g_para; static int g_npara, g_pcap;

static Para *para_find(const char *name)
{
    for (int i = 0; i < g_npara; i++) if (!strcmp(g_para[i].name, name)) return &g_para[i];
    return NULL;
}

static Para *para_add(const char *name, int is_section, int line)
{
    if (para_find(name)) die_at(line, "the procedure-name '%s' is declared twice (qualified paragraph names are not implemented yet)", name);
    if (g_npara == g_pcap) { g_pcap = g_pcap ? g_pcap * 2 : 64; g_para = realloc(g_para, g_pcap * sizeof *g_para); }
    Para *p = &g_para[g_npara];
    snprintf(p->name, sizeof p->name, "%s", name);
    p->id = g_npara + 1; p->is_section = is_section; p->line = line;
    g_npara++;
    return p;
}

static void emit_para_label(Para *p) { emit(".Lp%d_%d:\t# %s%s", g_unit, p->id, p->name, p->is_section ? " section" : ""); }

/* prescan the Procedure Division for paragraph and section headers */
static void prescan_paragraphs(int from)
{
    int sentence_start = 1;
    for (int i = from; i < g_ntok; i++) {
        Tok *t = &g_tok[i];
        if (t->kind == T_EOF) break;
        if (sentence_start && t->kind == T_WORD && !is_verb(t->s) && !is_terminator(t->s)) {
            if (g_tok[i + 1].kind == T_PERIOD) { para_add(t->s, 0, t->line); }
            else if (is_word(&g_tok[i + 1], "section") && g_tok[i + 2].kind == T_PERIOD) para_add(t->s, 1, t->line);
            else if (!strcmp(t->s, "declaratives")) die_at(t->line, "DECLARATIVES are not implemented yet");
            else if (!strcmp(t->s, "end") && is_word(&g_tok[i + 1], "program")) break;
        }
        sentence_start = (t->kind == T_PERIOD);
    }
}

static Para *expect_para(void)
{
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a procedure-name, found %s", tok_desc(t));
    Para *p = para_find(t->s);
    if (!p) die_at(t->line, "'%s' is not a paragraph or section", t->s);
    advance();
    return p;
}

/* ---- DISPLAY ---------------------------------------------------------- */

static void emit_screen_addr(const char *reg, Screen *sc)
{
    char lab[32]; snprintf(lab, sizeof lab, ".Lscr%d_%d", g_unit, (int)(sc - g_screens));
    emit_la(reg, lab);
}

static void parse_accept(void)
{
    Tok *t = cur();
    if (t->kind == T_WORD) {
        Screen *sc = screen_find(t->s);
        if (sc) { advance(); emit_screen_addr("r3", sc); emit_call("cob_screen_accept"); return; }
    }
    Ref r; parse_ref(&r);
    if (accept_word("from")) {
        if (at_word("argument-number") || at_word("argument-value") || at_word("command-line")) {
            const char *fn = at_word("argument-number") ? "cob_accept_argnum"
                           : at_word("argument-value") ? "cob_accept_argval" : "cob_accept_cmdline";
            if (at_word("argument-number") && !is_numeric_sym(r.sym)) die_at(r.line, "ACCEPT ... FROM ARGUMENT-NUMBER needs a numeric item");
            advance();
            Arg a[2] = { arg_ref(&r), arg_desc(sym_desc(r.sym)) };
            emit_args(a, 2);
            emit_call(fn);
            if (at_word("on") || at_word("exception") || at_word("not")) die_at(cur()->line, "ACCEPT ... ON EXCEPTION is not implemented");
            accept_word("end-accept");
            return;
        }
        if (at_word("date") || at_word("day") || at_word("time") || at_word("day-of-week"))
            die_at(cur()->line, "ACCEPT FROM %s is not implemented yet; FUNCTION CURRENT-DATE is stage 9", cur()->s);
        die_at(cur()->line, "ACCEPT FROM %s is not implemented", tok_desc(cur()));
    }
    /* ACCEPT identifier: a line from standard input */
    {
        Arg a[2] = { arg_ref(&r), arg_desc(sym_desc(r.sym)) };
        emit_args(a, 2);
        emit_call("cob_accept_console");
        accept_word("end-accept");
    }
}

static void parse_display(void)
{
    int line = cur()->line;
    int n = 0, no_adv = 0;
    if (cur()->kind == T_WORD) {
        Screen *sc = screen_find(cur()->s);
        if (sc) { advance(); emit_screen_addr("r3", sc); emit_call("cob_screen_display"); return; }
    }
    /* DISPLAY n UPON ARGUMENT-NUMBER: the next ARGUMENT-VALUE will be n */
    if (is_word(peek(1), "upon") && is_word(peek(2), "argument-number")) {
        Opnd o; parse_operand(&o);
        if (!opnd_hot_int(&o)) {
            if (o.kind != O_REF || !is_int_item(o.ref.sym)) die_at(o.line, "DISPLAY ... UPON ARGUMENT-NUMBER needs an integer");
            Arg a[2] = { arg_ref(&o.ref), arg_desc(sym_desc(o.ref.sym)) }; emit_args(a, 2); emit_call("cob_load_int");
        } else emit_hot_value(&o);
        emit("\tadd r3, r1, r0");
        emit_call("cob_display_upon_argnum");
        advance(); advance();
        return;
    }
    if (is_word(peek(1), "upon") && (is_word(peek(2), "sysout") || is_word(peek(2), "console") || is_word(peek(2), "syserr") || is_word(peek(2), "stderr"))) {
        /* the console: an ordinary DISPLAY */
    }
    for (;;) {
        Tok *t = cur();
        if (t->kind == T_WORD && !strcmp(t->s, "upon")) {
            advance();
            if (accept_word("sysout") || accept_word("console") || accept_word("syserr") || accept_word("stderr")) continue;
            die_at(t->line, "DISPLAY UPON %s is not implemented (ARGUMENT-NUMBER takes one operand)", cur()->s);
        }
        if (t->kind == T_WORD && (!strcmp(t->s, "with") || !strcmp(t->s, "no"))) {
            accept_word("with"); expect_word("no"); expect_word("advancing");
            no_adv = 1; break;
        }
        if (!at_operand() && !(t->kind == T_WORD && (is_figurative(t->s) || !strcmp(t->s, "all")))) break;
        Opnd o; parse_operand(&o);
        n++;
        switch (o.kind) {
        case O_STR: {
            Arg a[2] = { arg_label(lit_label((unsigned char *)o.tok->s, o.tok->len)), arg_imm(o.tok->len) };
            emit_args(a, 2); emit_call("cob_display"); break;
        }
        case O_NUM: {  /* a numeric literal displays as written */
            char txt[48]; int k = 0;
            if (o.num.neg) txt[k++] = '-';
            for (int i = 0; i < o.num.ndigits; i++) {
                if (o.num.scale && i == o.num.ndigits - o.num.scale) txt[k++] = '.';
                txt[k++] = o.num.digits[i];
            }
            Arg a[2] = { arg_label(lit_label((unsigned char *)txt, k)), arg_imm(k) };
            emit_args(a, 2); emit_call("cob_display"); break;
        }
        case O_FIG: case O_ALL: {
            int len = o.kind == O_ALL ? o.tok->len : 1;
            unsigned char *b = xmalloc(len);
            if (o.kind == O_ALL) memcpy(b, o.tok->s, len); else b[0] = (unsigned char)fig_byte(o.tok->s);
            Arg a[2] = { arg_label(lit_label(b, len)), arg_imm(len) };
            free(b);
            emit_args(a, 2); emit_call("cob_display"); break;
        }
        default: {
            Arg a[2];
            opnd_args(&o, &a[0], &a[1], 0, 0);
            emit_args(a, 2); emit_call("cob_display_field"); break;
        }
        }
    }
    if (!n) die_at(line, "DISPLAY needs at least one operand");
    if (!no_adv) emit_call("cob_display_nl");
}

/* ---- MOVE ------------------------------------------------------------- */

/* does a group's length depend on an OCCURS DEPENDING ON below it?  One
 * occurrence of the table itself (always subscripted) is fixed-length. */
static int has_odo(Sym *s)
{
    for (int c = s->child; c >= 0; c = g_sym[c].sibling)
        if (g_sym[c].odo_dep[0] || has_odo(&g_sym[c])) return 1;
    return 0;
}

static void emit_move(Opnd *src, Ref *dst)
{
    Sym *d = dst->sym;
    if (d->is_group && has_odo(d))
        die_at(dst->line, "MOVE to the group '%s', which holds an OCCURS DEPENDING ON table, is not implemented (its length varies); its entries can be moved by subscript", d->name);
    if (src->kind == O_REF && src->ref.sym->is_group && has_odo(src->ref.sym))
        die_at(src->line, "MOVE of the group '%s', which holds an OCCURS DEPENDING ON table, is not implemented (its length varies); its entries can be moved by subscript", src->ref.sym->name);
    if (d->is_cond) die_at(dst->line, "'%s' is a condition-name and cannot receive a MOVE", d->name);
    if (!d->is_group && (d->pi.category == PIC_NUMERIC_EDITED || d->pi.category == PIC_ALPHANUMERIC_EDITED)) {
        int ned = d->pi.category == PIC_NUMERIC_EDITED;
        if (src->kind == O_FIG && !(ned && !strncmp(src->tok->s, "zero", 4))) {
            Arg a[3] = { arg_ref(dst), arg_imm(d->size), arg_imm(fig_byte(src->tok->s)) };
            emit_args(a, 3); emit_call("cob_fill");
            return;
        }
        if (src->kind == O_ALL) {
            Arg a[4] = { arg_ref(dst), arg_imm(d->size), arg_label(lit_label((unsigned char *)src->tok->s, src->tok->len)), arg_imm(src->tok->len) };
            emit_args(a, 4); emit_call("cob_fill_all");
            return;
        }
        if (src->kind == O_REF && src->ref.sym->is_cond) die_at(src->line, "'%s' is a condition-name and cannot be moved", src->ref.sym->name);
        Arg a[4];
        opnd_args(src, &a[0], &a[1], d->size, ned);
        a[2] = arg_ref(dst); a[3] = arg_desc(sym_desc(d));
        emit_args(a, 4); emit_call("cob_move");
        return;
    }
    int dnum = is_numeric_sym(d);

    if (dst->rm || (src->kind == O_REF && src->ref.rm)) {
        /* a reference-modified side is an alphanumeric of runtime extent */
        if (src->kind == O_FIG || src->kind == O_ALL) {
            Arg len = dst->rm_len ? arg_imm((long)dst->rm_len) : arg_rlen(dst);
            if (src->kind == O_ALL && src->tok->len > 1) {
                Arg b[4] = { arg_ref(dst), len, arg_label(lit_label((unsigned char *)src->tok->s, src->tok->len)), arg_imm(src->tok->len) };
                emit_args(b, 4); emit_call("cob_fill_all"); return;
            }
            Arg a[3] = { arg_ref(dst), len, arg_imm(src->kind == O_ALL ? (unsigned char)src->tok->s[0] : fig_byte(src->tok->s)) };
            emit_args(a, 3); emit_call("cob_fill"); return;
        }
        Arg a[4];
        opnd_args(src, &a[0], &a[1], ref_static_len(dst) > 0 ? ref_static_len(dst) : 1, dnum && !dst->rm);
        a[2] = arg_ref(dst);
        a[3] = dst->rm ? (dst->rm_len ? arg_desc(str_desc((int)dst->rm_len)) : arg_rdesc(dst)) : arg_desc(sym_desc(d));
        emit_args(a, 4); emit_call("cob_move");
        return;
    }

    if (!dnum) {
        switch (src->kind) {
        case O_STR: case O_NUM: {
            if (src->kind == O_NUM && !numlit_is_int(&src->num))
                die_at(src->line, "MOVE of a non-integer numeric literal to the alphanumeric item '%s' is not valid COBOL", d->name);
            const char *txt = src->tok ? src->tok->s : NULL;
            int len = src->tok ? src->tok->len : 0;
            char dig[40];
            if (src->kind == O_NUM) { memcpy(dig, src->num.digits, src->num.ndigits); txt = dig; len = src->num.ndigits; }
            const char *l = lit_label((unsigned char *)txt, len);
            if (len == d->size && !d->just) {
                Arg a[3] = { arg_ref(dst), arg_label(l), arg_imm(len) };
                emit_args(a, 3); emit_call("memcpy");
            } else {
                Arg a[5] = { arg_label(l), arg_imm(len), arg_ref(dst), arg_imm(d->size), arg_imm(d->just) };
                emit_args(a, 5); emit_call("cob_move_alnum");
            }
            return;
        }
        case O_FIG: {
            Arg a[3] = { arg_ref(dst), arg_imm(d->size), arg_imm(fig_byte(src->tok->s)) };
            emit_args(a, 3); emit_call("cob_fill");
            return;
        }
        case O_ALL: {
            Arg a[4] = { arg_ref(dst), arg_imm(d->size), arg_label(lit_label((unsigned char *)src->tok->s, src->tok->len)), arg_imm(src->tok->len) };
            emit_args(a, 4); emit_call("cob_fill_all");
            return;
        }
        case O_FUNC: {
            Arg a[4];
            opnd_args(src, &a[0], &a[1], d->size, 0);
            a[2] = arg_ref(dst); a[3] = arg_desc(sym_desc(d));
            emit_args(a, 4); emit_call("cob_move");
            return;
        }
        default: {
            Sym *s = src->ref.sym;
            if (s->is_cond) die_at(src->line, "'%s' is a condition-name and cannot be moved", s->name);
            if (is_numeric_sym(s) && s->pi.scale != 0)
                die_at(src->line, "MOVE of the non-integer numeric item '%s' to the alphanumeric item '%s' is not valid COBOL", s->name, d->name);
            if (!is_numeric_sym(s) && s->size == d->size && !d->just) {
                Arg a[3] = { arg_ref(dst), arg_ref(&src->ref), arg_imm(d->size) };
                emit_args(a, 3); emit_call("memcpy");
                return;
            }
            Arg a[4] = { arg_ref(&src->ref), arg_desc(sym_desc(s)), arg_ref(dst), arg_desc(sym_desc(d)) };
            emit_args(a, 4); emit_call("cob_move");
            return;
        }
        }
    }

    /* numeric receiver */
    if (src->kind == O_FIG && !strncmp(src->tok->s, "zero", 4)) {
        Opnd z; memset(&z, 0, sizeof z); z.kind = O_NUM; numlit_zero(&z.num); z.line = src->line;
        emit_move(&z, dst);
        return;
    }
    if (src->kind == O_FIG || src->kind == O_ALL) {
        if (d->usage != U_DISPLAY) die_at(src->line, "%s cannot be moved to the %s item '%s'", src->tok->s, usage_name(d->usage), d->name);
        Arg a[3] = { arg_ref(dst), arg_imm(d->size), arg_imm(src->kind == O_ALL ? (unsigned char)src->tok->s[0] : fig_byte(src->tok->s)) };
        emit_args(a, 3); emit_call("cob_fill");
        return;
    }
    if (src->kind == O_NUM && is_hot_int(d)) {
        long long v = numlit_int(&src->num);
        if (d->usage == U_BINARY) v %= pow10l(d->pi.digits);
        if (!d->pi.is_signed && v < 0) v = -v;
        emit_ref_addr(dst, "r3");
        emit_li("r1", (long)v);
        emit_store_int(d, "r3", "r1");
        return;
    }
    if (src->kind == O_REF && is_hot_int(d) && is_hot_int(src->ref.sym) &&
        (d->pi.is_signed || !src->ref.sym->pi.is_signed)) {
        Sym *s = src->ref.sym;
        emit_ref_addr(&src->ref, "r3");
        emit_load_int(s, "r3", "r1");
        if (d->usage == U_BINARY && !(s->usage == U_BINARY && s->pi.digits <= d->pi.digits)) emit_trunc(d);
        emit("\tstw sp+%d, r1", SLOT_A);
        emit_ref_addr(dst, "r3");
        emit("\tldw r1, sp+%d", SLOT_A);
        emit_store_int(d, "r3", "r1");
        return;
    }
    Arg a[4]; Arg da = arg_ref(dst), dd = arg_desc(sym_desc(d));
    opnd_args(src, &a[0], &a[1], d->size, 1);
    a[2] = da; a[3] = dd;
    emit_args(a, 4);
    emit_call("cob_move");
}

static void parse_move(void)
{
    if (at_word("corresponding") || at_word("corr")) die_at(cur()->line, "MOVE CORRESPONDING is not implemented yet");
    Opnd src; parse_operand(&src);
    expect_word("to");
    int n = 0;
    while (at_operand()) {
        Ref dst; parse_ref(&dst);
        emit_move(&src, &dst);
        n++;
    }
    if (!n) die_at(cur()->line, "MOVE needs a receiving item");
}

/* ---- arithmetic ------------------------------------------------------- */

/* [NOT] [ON] SIZE ERROR follows the receivers; whether it is there
 * decides the store options, so look before emitting the stores */
static int at_size_error_clause(void)
{
    if (at_word("on") || at_word("size")) return 1;
    return at_word("not") && (is_word(peek(1), "on") || is_word(peek(1), "size"));
}

static void accept_size_error_words(void)
{
    accept_word("on"); expect_word("size"); expect_word("error");
}

/* after the stores: branch on the accumulated status in SLOT_B */
static void parse_size_error_clauses(int size_err, const char *end_word)
{
    if (size_err) {
        int Lok = new_label(), Lend = new_label();
        emit("\tldw r1, sp+%d", SLOT_B);
        emit("\tbeq r1, r0, .L%d", Lok);
        if (at_word("on") || at_word("size")) { accept_size_error_words(); parse_statements(); }
        emit_jump(Lend);
        emit_label(Lok);
        if (accept_word("not")) { accept_size_error_words(); parse_statements(); }
        emit_label(Lend);
    }
    accept_word(end_word);
}

static void check_numeric_opnd(Opnd *o)
{
    if (o->kind == O_STR || o->kind == O_ALL) die_at(o->line, "an arithmetic operand must be numeric");
    if (o->kind == O_FIG && strncmp(o->tok->s, "zero", 4)) die_at(o->line, "an arithmetic operand must be numeric");
    if (o->kind == O_REF && !is_numeric_sym(o->ref.sym)) die_at(o->line, "'%s' is not numeric", o->ref.sym->name);
}

/* push an operand onto the numeric stack */
static void emit_push(Opnd *o)
{
    if (o->kind == O_EXPR) die_at(o->line, "internal: expression pushed as an operand");
    if (o->kind == O_NUM || o->kind == O_FIG) {
        long long v = o->kind == O_NUM ? numlit_scaled(&o->num) : 0;
        int scale = o->kind == O_NUM ? o->num.scale : 0;
        emit_li("r3", (long)(int)(v & 0xFFFFFFFF));
        emit_li("r4", (long)(int)(v >> 32));
        emit_li("r5", scale);
        emit_call("cob_push_lit");
        return;
    }
    Arg a[2] = { arg_ref(&o->ref), arg_desc(sym_desc(o->ref.sym)) };
    emit_args(a, 2);
    emit_call("cob_push");
}

/* store from the stack top; opts 1 = ROUNDED, 2 = size-error check.
 * With the check on, the status accumulates in SLOT_B. */
static void emit_top_op(Ref *r, const char *fn, int opts)
{
    Arg a[3] = { arg_ref(r), arg_desc(sym_desc(r->sym)), arg_imm(opts) };
    emit_args(a, 3);
    emit_call(fn);
    if (opts & 2) {
        emit("\tldw r2, sp+%d", SLOT_B);
        emit("\tor r2, r2, r1");
        emit("\tstw sp+%d, r2", SLOT_B);
    }
}

static int all_hot(Opnd *ops, int n)
{
    for (int i = 0; i < n; i++) if (!opnd_hot_int(&ops[i])) return 0;
    return 1;
}

static int refs_hot(Ref *rs, int n)
{
    for (int i = 0; i < n; i++) if (!is_hot_int(rs[i].sym)) return 0;
    return 1;
}

/* SLOT_A = sum of the operands (hot path) */
static void emit_hot_sum(Opnd *ops, int n)
{
    for (int i = 0; i < n; i++) {
        emit_hot_value(&ops[i]);
        if (i) { emit("\tldw r2, sp+%d", SLOT_A); emit("\tadd r1, r1, r2"); }
        emit("\tstw sp+%d, r1", SLOT_A);
    }
}

#define MAXOPS 16

static int parse_operand_list(Opnd *ops, int max)
{
    int n = 0;
    while (at_operand() || (cur()->kind == T_WORD && is_figurative(cur()->s))) {
        if (n >= max) die_at(cur()->line, "too many operands");
        parse_operand(&ops[n]); check_numeric_opnd(&ops[n]); n++;
    }
    return n;
}

/* receivers, each with an optional ROUNDED; GIVING and COMPUTE receivers
 * may be numeric-edited */
static int parse_ref_list(Ref *rs, int *rounded, int max, int edited_ok)
{
    int n = 0;
    while (at_operand()) {
        if (n >= max) die_at(cur()->line, "too many receiving items");
        parse_ref(&rs[n]);
        Sym *d = rs[n].sym;
        if (rs[n].rm) die_at(rs[n].line, "a reference-modified item cannot be an arithmetic receiver");
        if (d->is_group || (d->pi.category != PIC_NUMERIC && !(edited_ok && d->pi.category == PIC_NUMERIC_EDITED)))
            die_at(rs[n].line, "'%s' is not numeric", d->name);
        rounded[n] = 0;
        if (accept_word("rounded")) {
            rounded[n] = 1;
            if (at_word("mode")) die_at(cur()->line, "ROUNDED MODE is COBOL 2002; plain ROUNDED is the 1985 form");
        }
        n++;
    }
    return n;
}

static int any_rounded(const int *r, int n) { for (int i = 0; i < n; i++) if (r[i]) return 1; return 0; }

/* store the sum on the stack top (general) or in SLOT_A (hot) to receivers */
static void emit_store_receivers(Ref *rs, int *rounded, int nr, int hot, int giving, int subtract, int size_err)
{
    if (size_err) emit("\tstw sp+%d, r0", SLOT_B);
    for (int i = 0; i < nr; i++) {
        int opts = (rounded[i] ? 1 : 0) | (size_err ? 2 : 0);
        if (hot) {
            Sym *d = rs[i].sym;
            emit_ref_addr(&rs[i], "r3");
            if (giving) emit("\tldw r1, sp+%d", SLOT_A);
            else {
                emit_load_int(d, "r3", "r1");
                emit("\tldw r2, sp+%d", SLOT_A);
                emit(subtract ? "\tsub r1, r1, r2" : "\tadd r1, r1, r2");
            }
            emit_trunc(d);
            emit_store_int(d, "r3", "r1");
        } else {
            emit_top_op(&rs[i], giving ? "cob_top_store" : subtract ? "cob_top_subfrom" : "cob_top_addto", opts);
        }
    }
    if (!hot) emit_call("cob_drop");
}

static void parse_add(void)
{
    if (at_word("corresponding") || at_word("corr")) die_at(cur()->line, "ADD CORRESPONDING is not implemented yet");
    Opnd ops[MAXOPS]; Ref rs[MAXOPS]; int rd[MAXOPS];
    int n = parse_operand_list(ops, MAXOPS);
    if (!n) die_at(cur()->line, "ADD needs an operand");
    int giving = 0, nr = 0;
    if (accept_word("to")) {
        /* ADD a TO b [GIVING c]: b is a receiver unless GIVING follows */
        int save = g_tp;
        g_noemit++;
        Opnd extra[MAXOPS]; int ne = parse_operand_list(extra, MAXOPS);
        int has_giving = accept_word("giving");
        g_noemit--;
        if (has_giving) {
            for (int i = 0; i < ne; i++) { if (n >= MAXOPS) die_at(cur()->line, "too many operands"); ops[n++] = extra[i]; }
            giving = 1;
            nr = parse_ref_list(rs, rd, MAXOPS, 1);
        } else { g_tp = save; nr = parse_ref_list(rs, rd, MAXOPS, 0); }
    } else if (accept_word("giving")) {
        giving = 1; nr = parse_ref_list(rs, rd, MAXOPS, 1);
    } else die_at(cur()->line, "expected TO or GIVING in ADD");
    if (!nr) die_at(cur()->line, "ADD needs a receiving item");
    int size_err = at_size_error_clause();

    int hot = !size_err && !any_rounded(rd, nr) && all_hot(ops, n) && refs_hot(rs, nr);
    if (hot) emit_hot_sum(ops, n);
    else { for (int i = 0; i < n; i++) { emit_push(&ops[i]); if (i) emit_call("cob_nadd"); } }
    emit_store_receivers(rs, rd, nr, hot, giving, 0, size_err);
    parse_size_error_clauses(size_err, "end-add");
}

static void parse_subtract(void)
{
    if (at_word("corresponding") || at_word("corr")) die_at(cur()->line, "SUBTRACT CORRESPONDING is not implemented yet");
    Opnd ops[MAXOPS]; Ref rs[MAXOPS]; int rd[MAXOPS];
    int n = parse_operand_list(ops, MAXOPS);
    if (!n) die_at(cur()->line, "SUBTRACT needs an operand");
    expect_word("from");
    int giving = 0, nr = 0;
    Opnd minuend; memset(&minuend, 0, sizeof minuend);
    int save = g_tp;
    g_noemit++;
    Opnd extra[MAXOPS]; int ne = parse_operand_list(extra, MAXOPS);
    int has_giving = accept_word("giving");
    g_noemit--;
    if (has_giving) {
        if (ne != 1) die_at(cur()->line, "SUBTRACT ... FROM x GIVING takes one item after FROM");
        minuend = extra[0]; giving = 1;
        nr = parse_ref_list(rs, rd, MAXOPS, 1);
    } else { g_tp = save; nr = parse_ref_list(rs, rd, MAXOPS, 0); }
    if (!nr) die_at(cur()->line, "SUBTRACT needs a receiving item");
    int size_err = at_size_error_clause();

    int hot = !size_err && !any_rounded(rd, nr) && all_hot(ops, n) && refs_hot(rs, nr) && (!giving || opnd_hot_int(&minuend));
    if (hot) {
        emit_hot_sum(ops, n);
        if (giving) {
            emit_hot_value(&minuend);
            emit("\tldw r2, sp+%d", SLOT_A);
            emit("\tsub r1, r1, r2");
            emit("\tstw sp+%d, r1", SLOT_A);
        }
    } else {
        if (giving) emit_push(&minuend);
        for (int i = 0; i < n; i++) { emit_push(&ops[i]); if (i) emit_call("cob_nadd"); }
        if (giving) emit_call("cob_nsub");
    }
    emit_store_receivers(rs, rd, nr, hot, giving, !giving, size_err);
    parse_size_error_clauses(size_err, "end-subtract");
}

static void parse_multiply(void)
{
    Opnd a; parse_operand(&a); check_numeric_opnd(&a);
    expect_word("by");
    Ref rs[MAXOPS]; int rd[MAXOPS]; int nr = 0;
    int save = g_tp;
    g_noemit++;
    Opnd b; parse_operand(&b); check_numeric_opnd(&b);
    int has_giving = accept_word("giving");
    g_noemit--;
    if (has_giving) {
        nr = parse_ref_list(rs, rd, MAXOPS, 1);
        if (!nr) die_at(cur()->line, "MULTIPLY needs a receiving item");
        int size_err = at_size_error_clause();
        emit_push(&a); emit_push(&b); emit_call("cob_nmul");
        emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err);
        parse_size_error_clauses(size_err, "end-multiply");
        return;
    }
    g_tp = save;
    nr = parse_ref_list(rs, rd, MAXOPS, 0);
    if (!nr) die_at(cur()->line, "MULTIPLY needs a receiving item");
    int size_err = at_size_error_clause();
    if (size_err) emit("\tstw sp+%d, r0", SLOT_B);
    for (int i = 0; i < nr; i++) {
        Opnd r; memset(&r, 0, sizeof r); r.kind = O_REF; r.ref = rs[i]; r.line = rs[i].line;
        emit_push(&r); emit_push(&a); emit_call("cob_nmul");
        emit_top_op(&rs[i], "cob_top_store", (rd[i] ? 1 : 0) | (size_err ? 2 : 0)); emit_call("cob_drop");
    }
    parse_size_error_clauses(size_err, "end-multiply");
}

/* REMAINDER r: dividend - (quotient as stored, truncated) * divisor */
static void emit_remainder(Opnd *dividend, Ref *q, int q_rounded, Opnd *divisor)
{
    if (!accept_word("remainder")) return;
    if (q_rounded) die_at(cur()->line, "REMAINDER with a ROUNDED quotient is not implemented");
    Ref r; parse_ref(&r);
    if (!is_numeric_sym(r.sym)) die_at(r.line, "'%s' is not numeric", r.sym->name);
    Opnd qo; memset(&qo, 0, sizeof qo); qo.kind = O_REF; qo.ref = *q; qo.line = q->line;
    emit_push(dividend); emit_push(&qo); emit_push(divisor);
    emit_call("cob_nmul"); emit_call("cob_nsub");
    emit_top_op(&r, "cob_top_store", 0); emit_call("cob_drop");
}

static void parse_divide(void)
{
    Opnd a; parse_operand(&a); check_numeric_opnd(&a);
    Ref rs[MAXOPS]; int rd[MAXOPS]; int nr;
    if (accept_word("into")) {
        int save = g_tp;
        g_noemit++;
        Opnd b; parse_operand(&b); check_numeric_opnd(&b);
        int has_giving = accept_word("giving");
        g_noemit--;
        if (has_giving) {
            nr = parse_ref_list(rs, rd, MAXOPS, 1);
            if (!nr) die_at(cur()->line, "DIVIDE needs a receiving item");
            int has_rem = at_word("remainder");
            int size_err = at_size_error_clause();
            emit_push(&b); emit_push(&a); emit_call("cob_ndiv");
            emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err && !has_rem);
            emit_remainder(&b, &rs[0], rd[0], &a);
            size_err = at_size_error_clause();
            if (size_err && has_rem) die_at(cur()->line, "SIZE ERROR together with REMAINDER is not implemented");
            parse_size_error_clauses(size_err, "end-divide");
            return;
        }
        g_tp = save;
        nr = parse_ref_list(rs, rd, MAXOPS, 0);
        if (!nr) die_at(cur()->line, "DIVIDE needs a receiving item");
        int size_err = at_size_error_clause();
        if (size_err) emit("\tstw sp+%d, r0", SLOT_B);
        for (int i = 0; i < nr; i++) {
            Opnd r; memset(&r, 0, sizeof r); r.kind = O_REF; r.ref = rs[i]; r.line = rs[i].line;
            emit_push(&r); emit_push(&a); emit_call("cob_ndiv");
            emit_top_op(&rs[i], "cob_top_store", (rd[i] ? 1 : 0) | (size_err ? 2 : 0)); emit_call("cob_drop");
        }
        parse_size_error_clauses(size_err, "end-divide");
        return;
    }
    expect_word("by");
    Opnd b; parse_operand(&b); check_numeric_opnd(&b);
    expect_word("giving");
    nr = parse_ref_list(rs, rd, MAXOPS, 1);
    if (!nr) die_at(cur()->line, "DIVIDE needs a receiving item");
    int has_rem = at_word("remainder");
    int size_err = at_size_error_clause();
    emit_push(&a); emit_push(&b); emit_call("cob_ndiv");
    emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err && !has_rem);
    emit_remainder(&a, &rs[0], rd[0], &b);
    size_err = at_size_error_clause();
    if (size_err && has_rem) die_at(cur()->line, "SIZE ERROR together with REMAINDER is not implemented");
    parse_size_error_clauses(size_err, "end-divide");
}

/* ---- arithmetic expressions: COMPUTE and condition operands ----------- */

static void parse_expr(void);

static int at_arith_op(void)
{
    return at_op("+") || at_op("-") || at_op("*") || at_op("/") || at_op("**");
}

static void parse_primary(void)
{
    Tok *t = cur();
    if (t->kind == T_LP) {
        advance(); parse_expr();
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' in the expression");
        advance();
        return;
    }
    if (at_op("+")) { advance(); parse_primary(); return; }
    if (at_op("-")) { advance(); parse_primary(); emit_call("cob_nneg"); return; }
    Opnd o; parse_operand(&o);
    check_numeric_opnd(&o);
    emit_push(&o);
}

static void parse_power(void)
{
    parse_primary();
    if (at_op("**")) { advance(); parse_power(); emit_call("cob_npow"); }
}

static void parse_term(void)
{
    parse_power();
    while (at_op("*") || at_op("/")) {
        int mul = at_op("*"); advance();
        parse_power();
        emit_call(mul ? "cob_nmul" : "cob_ndiv");
    }
}

static void parse_expr(void)
{
    parse_term();
    while (at_op("+") || at_op("-")) {
        int add = at_op("+"); advance();
        parse_term();
        emit_call(add ? "cob_nadd" : "cob_nsub");
    }
}

/* an expression operand in a condition: scanned now, emitted later */
static Opnd expr_opnd(void)
{
    Opnd o; memset(&o, 0, sizeof o);
    o.kind = O_EXPR; o.line = cur()->line; o.e_start = g_tp;
    g_noemit++; parse_expr(); g_noemit--;
    o.e_end = g_tp;
    return o;
}

static void emit_expr_tokens(int s0, int s1)
{
    int save = g_tp;
    g_tp = s0;
    parse_expr();
    if (g_tp != s1) die_at(g_tok[s0].line, "internal: expression re-parse drifted");
    g_tp = save;
}

static void emit_push_opnd(Opnd *o)
{
    if (o->kind != O_EXPR) { emit_push(o); return; }
    emit_expr_tokens(o->e_start, o->e_end);
}

/* does the parenthesis at the cursor open a condition or an expression? */
static int paren_is_condition(void)
{
    int depth = 0, words = 0;
    Tok *only = NULL;
    for (int i = g_tp; i < g_ntok; i++) {
        Tok *t = &g_tok[i];
        if (t->kind == T_LP) depth++;
        else if (t->kind == T_RP) { if (--depth == 0) break; }
        else if (t->kind == T_OP && (!strcmp(t->s, "=") || !strcmp(t->s, "<") || !strcmp(t->s, ">") ||
                 !strcmp(t->s, "<=") || !strcmp(t->s, ">=") || !strcmp(t->s, "<>"))) return 1;
        else if (t->kind == T_WORD) {
            static const char *cw[] = { "is", "not", "and", "or", "equal", "equals", "greater", "less",
                "than", "numeric", "alphabetic", "alphabetic-lower", "alphabetic-upper", "positive", "negative", NULL };
            for (int k = 0; cw[k]; k++) if (!strcmp(t->s, cw[k])) return 1;
            words++; only = t;
        }
        else if (t->kind == T_PERIOD || t->kind == T_EOF) break;
    }
    /* (cond-name) alone is a condition */
    if (words == 1 && only) {
        for (int i = 0; i < g_nsym; i++) if (g_sym[i].is_cond && !strcmp(g_sym[i].name, only->s)) return 1;
    }
    return 0;
}

static void parse_compute(void)
{
    Ref rs[MAXOPS]; int rd[MAXOPS];
    int nr = parse_ref_list(rs, rd, MAXOPS, 1);
    if (!nr) die_at(cur()->line, "COMPUTE needs a receiving item");
    if (!at_op("=")) die_at(cur()->line, "expected '=' in COMPUTE, found %s", tok_desc(cur()));
    advance();
    parse_expr();
    int size_err = at_size_error_clause();
    emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err);
    parse_size_error_clauses(size_err, "end-compute");
}

/* ---- IF ---------------------------------------------------------------- */

static void parse_branch_body(void)
{
    if (at_word("next")) {
        advance(); expect_word("sentence");
        if (g_sentence_label < 0) g_sentence_label = new_label();
        emit_jump(g_sentence_label);
        return;
    }
    parse_statements();
}

static void parse_if(void)
{
    Cond *c = parse_cond();
    accept_word("then");
    int Lelse = new_label();
    cond_jump_false(c, Lelse);
    parse_branch_body();
    if (accept_word("else")) {
        int Lend = new_label();
        emit_jump(Lend);
        emit_label(Lelse);
        parse_branch_body();
        emit_label(Lend);
    } else emit_label(Lelse);
    accept_word("end-if");
}

/* ---- PERFORM ---------------------------------------------------------- */

static int g_ncnt;      /* TIMES counters */

typedef struct { Para *from, *thru; int inline_body; } Body;

static void emit_body(Body *b)
{
    if (!b->inline_body) {
        int Lret = new_label();
        char lab[32]; snprintf(lab, sizeof lab, ".L%d", Lret);
        emit_li("r3", b->thru ? b->thru->id : b->from->id);
        emit_la("r4", lab);
        emit_call("cob_perform_push");
        emit("\tjal r0, .Lp%d_%d", g_unit, b->from->id);
        emit_label(Lret);
    } else {
        parse_statements();
    }
}

static void emit_add_to_ref(Opnd *by, Ref *var)
{
    Opnd ops[1] = { *by }; Ref rs[1] = { *var };
    int hot = opnd_hot_int(by) && is_hot_int(var->sym);
    int rd[1] = { 0 };
    if (hot) emit_hot_sum(ops, 1);
    else emit_push(by);
    emit_store_receivers(rs, rd, 1, hot, 0, 0, 0);
}

typedef struct { Ref var; Opnd from, by; Cond *until; } Vary;

static void emit_varying(Vary *v, int nv, int level, Body *body, int test_after)
{
    Vary *x = &v[level];
    emit_move(&x->from, &x->var);
    int Ltop = new_label(), Lend = new_label();
    emit_label(Ltop);
    if (!test_after) cond_jump_true(x->until, Lend);
    if (level + 1 < nv) emit_varying(v, nv, level + 1, body, test_after);
    else emit_body(body);
    if (test_after) cond_jump_true(x->until, Lend);
    emit_add_to_ref(&x->by, &x->var);
    emit_jump(Ltop);
    emit_label(Lend);
}

static void parse_perform(void)
{
    int line = cur()->line;
    Body body; memset(&body, 0, sizeof body);
    if (cur()->kind == T_WORD && para_find(cur()->s)) {
        body.from = expect_para();
        if (accept_word("thru") || accept_word("through")) body.thru = expect_para();
    } else body.inline_body = 1;

    int test_after = 0;
    if (accept_word("with")) { expect_word("test"); if (accept_word("after")) test_after = 1; else expect_word("before"); }
    else if (accept_word("test")) { if (accept_word("after")) test_after = 1; else expect_word("before"); }

    if (accept_word("until")) {
        Cond *c = parse_cond();
        int Ltop = new_label(), Lend = new_label();
        emit_label(Ltop);
        if (!test_after) cond_jump_true(c, Lend);
        emit_body(&body);
        if (test_after) cond_jump_false(c, Ltop); else emit_jump(Ltop);
        emit_label(Lend);
    } else if (accept_word("varying")) {
        Vary v[3]; int nv = 0;
        for (;;) {
            if (nv >= 3) die_at(cur()->line, "more than three VARYING/AFTER levels are not implemented");
            parse_ref(&v[nv].var);
            if (!is_numeric_sym(v[nv].var.sym)) die_at(v[nv].var.line, "the VARYING item must be numeric");
            expect_word("from"); parse_operand(&v[nv].from); check_numeric_opnd(&v[nv].from);
            expect_word("by"); parse_operand(&v[nv].by); check_numeric_opnd(&v[nv].by);
            expect_word("until"); v[nv].until = parse_cond();
            nv++;
            if (!accept_word("after")) break;
        }
        if (test_after && nv > 1) die_at(line, "WITH TEST AFTER together with AFTER is not implemented yet");
        emit_varying(v, nv, 0, &body, test_after);
    } else if (at_operand() && (peek(1)->kind == T_WORD && (!strcmp(peek(1)->s, "times")))) {
        Opnd n; parse_operand(&n); check_numeric_opnd(&n);
        expect_word("times");
        char cnt[32]; snprintf(cnt, sizeof cnt, ".Lcnt%d", g_ncnt++);
        if (opnd_hot_int(&n)) emit_hot_value(&n);
        else {
            if (n.kind != O_REF) die_at(n.line, "TIMES needs an integer");
            Arg a[2] = { arg_ref(&n.ref), arg_desc(sym_desc(n.ref.sym)) };
            emit_args(a, 2); emit_call("cob_load_int");
        }
        emit_la("r2", cnt);
        emit("\tstw r2+0, r1");
        int Ltop = new_label(), Lend = new_label();
        emit_label(Ltop);
        emit_la("r2", cnt);
        emit("\tldw r1, r2+0");
        emit("\tbge r0, r1, .L%d", Lend);
        emit("\taddi r1, r1, -1");
        emit("\tstw r2+0, r1");
        emit_body(&body);
        emit_jump(Ltop);
        emit_label(Lend);
    } else if (body.inline_body) {
        emit_body(&body);
    } else {
        emit_body(&body);
    }
    if (body.inline_body) expect_word("end-perform");
    /* an out-of-line PERFORM has no END-PERFORM: the next one belongs to
     * whatever inline PERFORM encloses this statement */
}

/* ---- GO TO, SET ------------------------------------------------------- */

static void parse_goto(void)
{
    accept_word("to");
    Para *ps[64]; int n = 0;
    while (cur()->kind == T_WORD && !at_word("depending") && !is_verb(cur()->s) && !is_terminator(cur()->s)) {
        if (n >= 64) die_at(cur()->line, "too many GO TO targets");
        ps[n++] = expect_para();
    }
    if (!n) die_at(cur()->line, "GO TO without a procedure-name needs ALTER, which is not in COBOL 85");
    if (accept_word("depending")) {
        accept_word("on");
        Opnd o; parse_operand(&o);
        if (o.kind != O_REF || !is_int_item(o.ref.sym)) die_at(o.line, "GO TO DEPENDING ON needs an integer item");
        if (is_hot_int(o.ref.sym)) emit_hot_value(&o);
        else { Arg a[2] = { arg_ref(&o.ref), arg_desc(sym_desc(o.ref.sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
        for (int i = 0; i < n; i++) {
            emit_li("r2", i + 1);
            emit("\tbeq r1, r2, .Lp%d_%d", g_unit, ps[i]->id);
        }
        return;
    }
    if (n != 1) die_at(cur()->line, "GO TO with several procedure-names needs DEPENDING ON");
    emit("\tjal r0, .Lp%d_%d", g_unit, ps[0]->id);
}

static void parse_set(void)
{
    Ref rs[MAXOPS]; int nr = 0;
    while (at_operand()) { if (nr >= MAXOPS) die_at(cur()->line, "too many items in SET"); parse_ref(&rs[nr++]); }
    if (!nr) die_at(cur()->line, "SET needs an item");
    if (accept_word("to")) {
        if (accept_word("true")) {
            for (int i = 0; i < nr; i++) {
                Sym *c = rs[i].sym;
                if (!c->is_cond) die_at(rs[i].line, "'%s' is not a condition-name", c->name);
                Opnd v = lit_opnd(c->cv_lo[0]);
                Ref p = rs[i]; p.sym = &g_sym[c->parent];
                emit_move(&v, &p);
            }
            return;
        }
        if (accept_word("false")) die_at(cur()->line, "SET ... TO FALSE is not in COBOL 85");
        Opnd v; parse_operand(&v);
        for (int i = 0; i < nr; i++) {
            if (!is_numeric_sym(rs[i].sym)) die_at(rs[i].line, "SET ... TO needs an index or integer item");
            emit_move(&v, &rs[i]);
        }
        return;
    }
    int down = 0;
    if (accept_word("up")) down = 0; else if (accept_word("down")) down = 1;
    else die_at(cur()->line, "expected TO, UP BY or DOWN BY in SET");
    expect_word("by");
    Opnd v; parse_operand(&v); check_numeric_opnd(&v);
    for (int i = 0; i < nr; i++) {
        Opnd ops[1] = { v };
        int hot = opnd_hot_int(&v) && is_hot_int(rs[i].sym);
        int rd[1] = { 0 };
        if (hot) emit_hot_sum(ops, 1); else emit_push(&v);
        emit_store_receivers(&rs[i], rd, 1, hot, 0, down, 0);
    }
}

/* ---- files: OPEN, CLOSE, READ, WRITE ---------------------------------- */

static void emit_file_addr(const char *reg, File *f)
{
    char lab[32]; snprintf(lab, sizeof lab, ".Lf%d_%d", g_unit, (int)(f - g_files));
    emit_la(reg, lab);
}

static File *expect_file(void)
{
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a file-name, found %s", tok_desc(t));
    File *f = file_find(t->s);
    if (!f) die_at(t->line, "'%s' is not a file (no SELECT)", t->s);
    if (f->org == COB_ORG_RELATIVE)
        die_at(t->line, "'%s' is RELATIVE; RELATIVE files are not implemented yet (after v1)", t->s);
    advance();
    return f;
}

static void parse_open(void)
{
    int n = 0;
    for (;;) {
        int mode;
        if (accept_word("input")) mode = COB_OPEN_INPUT;
        else if (accept_word("output")) mode = COB_OPEN_OUTPUT;
        else if (accept_word("i-o")) mode = COB_OPEN_IO;
        else if (accept_word("extend")) mode = COB_OPEN_EXTEND;
        else break;
        while (cur()->kind == T_WORD && !at_word("input") && !at_word("output") && !at_word("i-o") &&
               !at_word("extend") && !is_verb(cur()->s) && !is_terminator(cur()->s)) {
            File *f = expect_file();
            if (accept_word("with")) { accept_word("no"); accept_word("rewind"); accept_word("lock"); }
            if (accept_word("reversed")) die_at(cur()->line, "OPEN REVERSED is not supported");
            emit_file_addr("r3", f); emit_li("r4", mode); emit_call("cob_open");
            n++;
        }
    }
    if (!n) die_at(cur()->line, "OPEN needs INPUT, OUTPUT, I-O or EXTEND and a file-name");
}

static void parse_close(void)
{
    int n = 0;
    while (cur()->kind == T_WORD && !is_verb(cur()->s) && !is_terminator(cur()->s)) {
        File *f = expect_file();
        if (accept_word("with")) { accept_word("no"); accept_word("rewind"); accept_word("lock"); }
        else if (accept_word("reel") || accept_word("unit")) die_at(cur()->line, "CLOSE REEL/UNIT is not supported");
        emit_file_addr("r3", f); emit_call("cob_close");
        n++;
    }
    if (!n) die_at(cur()->line, "CLOSE needs a file-name");
}

/* [NOT] INVALID KEY / [NOT] AT END after a keyed verb, on the result in
 * SLOT_C: 0 done, 1 the condition, 2 an error already reported */
static void parse_condition_clauses(const char *w1, const char *w2, const char *end_word)
{
    int Lend = new_label();
    if (at_word(w1)) {
        advance(); expect_word(w2);
        int Lnot = new_label();
        emit("\tldw r1, sp+%d", SLOT_C);
        emit_li("r2", 1);
        emit("\tbne r1, r2, .L%d", Lnot);
        parse_statements();
        emit_jump(Lend);
        emit_label(Lnot);
    }
    if (at_word("not") && is_word(peek(1), w1)) {
        advance(); advance(); expect_word(w2);
        emit("\tldw r1, sp+%d", SLOT_C);
        emit("\tbne r1, r0, .L%d", Lend);
        parse_statements();
    }
    emit_label(Lend);
    accept_word(end_word);
}

static void parse_read(void)
{
    File *f = expect_file();
    int has_next = accept_word("next"); accept_word("record");
    Ref into; int has_into = 0;
    if (accept_word("into")) { parse_ref(&into); has_into = 1; }
    int keyed = 0;
    if (accept_word("key")) {
        accept_word("is");
        Ref k; parse_ref(&k);
        if (f->org != COB_ORG_INDEXED) die_at(k.line, "READ ... KEY needs an INDEXED file");
        if (k.sym != f->key_sym) die_at(k.line, "READ ... KEY IS '%s': only the RECORD KEY is supported (no ALTERNATE keys)", k.sym->name);
        keyed = 1;
    }
    if (f->org == COB_ORG_INDEXED) {
        if (has_next && keyed) die_at(cur()->line, "READ NEXT cannot name a KEY");
        if (!has_next && !keyed && f->access == 1) keyed = 1;         /* ACCESS RANDOM: every READ is by key */
        if (has_next && f->access == 1) die_at(cur()->line, "READ NEXT needs ACCESS SEQUENTIAL or DYNAMIC");
        if (keyed && f->access == 0) die_at(cur()->line, "READ ... KEY needs ACCESS RANDOM or DYNAMIC");
    } else if (keyed) die_at(cur()->line, "READ ... KEY needs an INDEXED file");

    emit_file_addr("r3", f);
    emit_call(keyed ? "cob_read_key" : "cob_read");
    emit("\tstw sp+%d, r1", SLOT_C);
    if (has_into) {
        int Lskip = new_label();
        emit("\tbne r1, r0, .L%d", Lskip);
        Opnd src; memset(&src, 0, sizeof src); src.kind = O_REF; src.line = into.line;
        src.ref.sym = &g_sym[f->rec]; src.ref.line = into.line;
        emit_move(&src, &into);
        emit_label(Lskip);
    }
    if (keyed) {
        if (at_word("at")) die_at(cur()->line, "a READ by key takes INVALID KEY, not AT END");
        parse_condition_clauses("invalid", "key", "end-read");
    } else {
        if (at_word("invalid")) die_at(cur()->line, "a sequential READ takes AT END, not INVALID KEY");
        parse_condition_clauses("at", "end", "end-read");
    }
}

static void parse_write(void)
{
    Ref rec; parse_ref(&rec);
    File *f = file_of_record(rec.sym, rec.line);
    if (accept_word("from")) {
        Opnd src; parse_operand(&src);
        emit_move(&src, &rec);
    }
    int before = 0, after = 0, after_kw = 0; Opnd n; int dyn = 0;
    if (at_word("before") || at_word("after")) {
        after_kw = accept_word("after"); if (!after_kw) accept_word("before");
        accept_word("advancing");
        if (accept_word("page")) die_at(cur()->line, "ADVANCING PAGE is not implemented (line sequential print files carry no form feed)");
        parse_operand(&n);
        if (n.kind == O_NUM) { long v = (long)numlit_int(&n.num); if (after_kw) before = (int)v - 1; else after = (int)v - 1; }
        else if (n.kind == O_REF && is_int_item(n.ref.sym)) dyn = 1;
        else die_at(n.line, "ADVANCING needs an integer");
        accept_word("line"); accept_word("lines");
    }
    if (f->org == COB_ORG_INDEXED && (before || after || dyn)) die_at(rec.line, "ADVANCING is not valid on an INDEXED file");
    if (f->org != COB_ORG_INDEXED && at_word("invalid")) die_at(cur()->line, "INVALID KEY needs an INDEXED file");
    if (dyn) {
        if (is_hot_int(n.ref.sym)) emit_hot_value(&n);
        else { Arg a[2] = { arg_ref(&n.ref), arg_desc(sym_desc(n.ref.sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
        emit("\taddi r1, r1, -1");
        emit("\tstw sp+%d, r1", SLOT_C);
        emit_file_addr("r3", f);
        if (after_kw) { emit("\tldw r4, sp+%d", SLOT_C); emit_li("r5", 0); }
        else { emit_li("r4", 0); emit("\tldw r5, sp+%d", SLOT_C); }
    } else {
        emit_file_addr("r3", f); emit_li("r4", before); emit_li("r5", after);
    }
    emit_li("r6", rec.sym->size);          /* the 01 named: a mode-V record's length */
    emit_call("cob_write");
    if (f->org == COB_ORG_INDEXED) {
        emit("\tstw sp+%d, r1", SLOT_C);
        parse_condition_clauses("invalid", "key", "end-write");
    } else accept_word("end-write");
}

/* REWRITE record [FROM x] [INVALID KEY ...] */
static void parse_rewrite(void)
{
    Ref rec; parse_ref(&rec);
    File *f = file_of_record(rec.sym, rec.line);
    if (f->org == COB_ORG_LINESEQ) die_at(rec.line, "REWRITE is not valid on a LINE SEQUENTIAL file");
    if (accept_word("from")) { Opnd src; parse_operand(&src); emit_move(&src, &rec); }
    emit_file_addr("r3", f);
    emit_call("cob_rewrite");
    emit("\tstw sp+%d, r1", SLOT_C);
    if (f->org == COB_ORG_INDEXED) parse_condition_clauses("invalid", "key", "end-rewrite");
    else { if (at_word("invalid")) die_at(cur()->line, "INVALID KEY needs an INDEXED file"); accept_word("end-rewrite"); }
}

/* DELETE file [RECORD] [INVALID KEY ...] */
static void parse_delete(void)
{
    File *f = expect_file();
    accept_word("record");
    if (f->org != COB_ORG_INDEXED) die_at(cur()->line, "DELETE needs an INDEXED file");
    emit_file_addr("r3", f);
    emit_call("cob_delete");
    emit("\tstw sp+%d, r1", SLOT_C);
    parse_condition_clauses("invalid", "key", "end-delete");
}

/* START file [KEY IS relation key] [INVALID KEY ...] */
static void parse_start(void)
{
    File *f = expect_file();
    if (f->org != COB_ORG_INDEXED) die_at(cur()->line, "START needs an INDEXED file");
    if (f->access == 1) die_at(cur()->line, "START needs ACCESS SEQUENTIAL or DYNAMIC");
    int op = 0;                     /* = */
    if (accept_word("key")) {
        accept_word("is");
        int neg = 0;
        if (accept_word("not")) neg = 1;
        if (at_op("=") || at_word("equal") || at_word("equals")) { advance(); accept_word("to"); op = 0; }
        else if (at_op(">") || at_word("greater")) { advance(); accept_word("than"); op = 1; if (accept_word("or")) { expect_word("equal"); accept_word("to"); op = 2; } }
        else if (at_op(">=")) { advance(); op = 2; }
        else if (at_op("<") || at_word("less")) { advance(); accept_word("than"); op = 3; if (accept_word("or")) { expect_word("equal"); accept_word("to"); op = 4; } }
        else if (at_op("<=")) { advance(); op = 4; }
        else die_at(cur()->line, "expected a relation in START ... KEY IS");
        if (neg) { if (op == 3) op = 2; else if (op == 1) op = 4; else die_at(cur()->line, "START KEY IS NOT takes LESS or GREATER"); }
        Ref k; parse_ref(&k);
        if (k.sym != f->key_sym) die_at(k.line, "START ... KEY IS '%s': only the RECORD KEY is supported", k.sym->name);
    }
    emit_file_addr("r3", f);
    emit_li("r4", op);
    emit_call("cob_start");
    emit("\tstw sp+%d, r1", SLOT_C);
    parse_condition_clauses("invalid", "key", "end-start");
}

/* ---- STRING ------------------------------------------------------------ */

static void parse_string(void)
{
    Opnd srcs[MAXOPS]; Opnd delims[MAXOPS]; int has_delim[MAXOPS];
    int n = 0, pending = 0;
    for (;;) {
        while (at_operand() || at_word("function")) {
            if (n >= MAXOPS) die_at(cur()->line, "too many STRING sources");
            parse_operand(&srcs[n]);
            if (srcs[n].kind == O_EXPR) die_at(srcs[n].line, "a STRING source must be an item, a literal or a figurative constant");
            has_delim[n] = 0; n++; pending++;
        }
        if (accept_word("delimited")) {
            accept_word("by");
            Opnd d; memset(&d, 0, sizeof d);
            if (accept_word("size")) d.kind = O_ALL;      /* stands for SIZE here */
            else { parse_operand(&d); if (d.kind != O_STR && d.kind != O_REF && d.kind != O_FIG) die_at(d.line, "DELIMITED BY needs SIZE, a literal or an item"); }
            for (int i = n - pending; i < n; i++) { delims[i] = d; has_delim[i] = 1; }
            pending = 0;
            continue;
        }
        break;
    }
    if (!n) die_at(cur()->line, "STRING needs a source");
    /* DELIMITED BY is mandatory in the 1985 text; GnuCOBOL lets it be
     * omitted and takes SIZE, and taskdt does exactly that (dialect.md) */
    for (int i = 0; i < n; i++) if (!has_delim[i]) { memset(&delims[i], 0, sizeof delims[i]); delims[i].kind = O_ALL; has_delim[i] = 1; }
    expect_word("into");
    Ref dst; parse_ref(&dst);
    if (dst.sym->is_group || dst.sym->pi.category == PIC_NUMERIC)
        die_at(dst.line, "the STRING receiver must be an elementary alphanumeric item");
    if (dst.rm) die_at(dst.line, "a reference-modified STRING receiver is not implemented");
    Ref ptr; int has_ptr = 0;
    if (accept_word("with")) { expect_word("pointer"); parse_ref(&ptr); has_ptr = 1; if (!is_int_item(ptr.sym)) die_at(ptr.line, "the POINTER must be an integer item"); }
    else if (accept_word("pointer")) { parse_ref(&ptr); has_ptr = 1; }

    /* begin: receiver, its length, the pointer's value */
    if (has_ptr) {
        if (is_hot_int(ptr.sym)) { Opnd po; memset(&po, 0, sizeof po); po.kind = O_REF; po.ref = ptr; emit_hot_value(&po); }
        else { Arg a[2] = { arg_ref(&ptr), arg_desc(sym_desc(ptr.sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
        emit("\tstw sp+%d, r1", SLOT_C);
    }
    Arg b[2] = { arg_ref(&dst), arg_imm(dst.sym->size) };
    emit_args(b, 2);
    if (has_ptr) emit("\tldw r5, sp+%d", SLOT_C); else emit_li("r5", 0);
    emit_call("cob_str_begin");

    for (int i = 0; i < n; i++) {
        Arg a[4]; Arg dd;
        if (srcs[i].kind == O_FIG) {
            /* SPACE, ZERO, ...: a one-character source */
            unsigned char c = (unsigned char)fig_byte(srcs[i].tok->s);
            a[0] = arg_label(lit_label(&c, 1)); a[1] = arg_imm(1);
        } else if (srcs[i].kind == O_ALL) {
            a[0] = arg_label(lit_label((unsigned char *)srcs[i].tok->s, srcs[i].tok->len)); a[1] = arg_imm(srcs[i].tok->len);
        } else {
            opnd_args(&srcs[i], &a[0], &dd, 0, 0);
            a[1] = arg_len(&srcs[i]);
        }
        Opnd *d = &delims[i];
        if (d->kind == O_ALL) { a[2] = arg_imm(0); a[3] = arg_imm(0); }
        else if (d->kind == O_FIG) { unsigned char c = (unsigned char)fig_byte(d->tok->s); a[2] = arg_label(lit_label(&c, 1)); a[3] = arg_imm(1); }
        else { Arg x; opnd_args(d, &a[2], &x, 0, 0); a[3] = arg_len(d); }
        emit_args(a, 4);
        emit_call("cob_str_src");
    }
    if (has_ptr) {
        emit_call("cob_str_pointer");
        emit("\tstw sp+%d, r1", SLOT_C);
        Arg a[2] = { arg_ref(&ptr), arg_desc(sym_desc(ptr.sym)) };
        emit_args(a, 2);
        emit("\tldw r5, sp+%d", SLOT_C);
        emit_call("cob_store_int");
    }
    int has_ovf = at_word("on") || at_word("overflow") || (at_word("not") && (is_word(peek(1), "on") || is_word(peek(1), "overflow")));
    if (has_ovf) {
        int Lok = new_label(), Lend = new_label();
        emit_call("cob_str_overflow");
        emit("\tbeq r1, r0, .L%d", Lok);
        if (at_word("on") || at_word("overflow")) { accept_word("on"); expect_word("overflow"); parse_statements(); }
        emit_jump(Lend);
        emit_label(Lok);
        if (accept_word("not")) { accept_word("on"); expect_word("overflow"); parse_statements(); }
        emit_label(Lend);
    }
    accept_word("end-string");
}

/* ---- CALL -------------------------------------------------------------- */

/* a PROGRAM-ID or CALL literal as a linker symbol: the SLOW-32 C ABI's
 * name space, shared with C and Fortran (docs/lowering.md) */
static const char *link_name(const char *name)
{
    static char b[128];
    int n = 0;
    for (const char *p = name; *p && n < 120; p++) b[n++] = (isalnum((unsigned char)*p) || *p == '_') ? *p : '_';
    b[n] = 0;
    return b;
}

static void parse_call(void)
{
    int line = cur()->line;
    Tok *t = cur();
    if (t->kind != T_STR) {
        if (t->kind == T_WORD) die_at(line, "CALL of an identifier (dynamic CALL) is not implemented; CALL a literal");
        die_at(line, "expected a program-name literal after CALL");
    }
    char name[128]; snprintf(name, sizeof name, "%.*s", t->len > 120 ? 120 : t->len, t->s);
    for (char *k = name; *k; k++) *k = (char)tolower((unsigned char)*k);
    advance();
    Arg a[8]; Opnd ops[8]; int n = 0;
    if (accept_word("using")) {
        int mode = 0;               /* 0 reference, 1 content, 2 value */
        for (;;) {
            if (accept_word("by")) {
                if (accept_word("reference")) mode = 0;
                else if (accept_word("content")) mode = 1;
                else if (accept_word("value")) mode = 2;
                else die_at(cur()->line, "expected REFERENCE, CONTENT or VALUE after BY");
                continue;
            }
            if (accept_word("reference")) { mode = 0; continue; }
            if (accept_word("value")) { mode = 2; continue; }
            if (accept_word("content")) { mode = 1; continue; }
            if (!at_operand()) break;
            if (n >= 8) die_at(cur()->line, "more than eight CALL arguments (stack arguments) are not implemented yet");
            parse_operand(&ops[n]);
            Opnd *o = &ops[n];
            if (mode == 1) die_at(o->line, "BY CONTENT is not implemented yet");
            if (mode == 2) {
                if (o->kind == O_REF) {
                    if (!is_int_item(o->ref.sym)) die_at(o->line, "BY VALUE '%s' must be an integer item", o->ref.sym->name);
                    if (o->ref.sym->size > 4) die_at(o->line, "BY VALUE '%s': only items up to four bytes (a word) are passed by value", o->ref.sym->name);
                    a[n] = arg_value(o);
                } else if (o->kind == O_NUM) {
                    if (!numlit_is_int(&o->num)) die_at(o->line, "BY VALUE needs an integer");
                    a[n] = arg_imm((long)numlit_int(&o->num));
                } else die_at(o->line, "BY VALUE needs an integer item or literal");
            } else {
                if (o->kind == O_REF) { if (o->ref.sym->is_cond) die_at(o->line, "a condition-name cannot be passed"); a[n] = arg_ref(&o->ref); }
                else if (o->kind == O_STR) a[n] = arg_label(lit_label((unsigned char *)o->tok->s, o->tok->len));
                else if (o->kind == O_NUM) { int d; a[n] = arg_label(num_lit_label(&o->num, &d)); }
                else die_at(o->line, "a CALL argument must be an item or a literal");
            }
            n++;
        }
    }
    Ref ret; int has_ret = 0;
    if (accept_word("returning") || accept_word("giving")) {
        parse_ref(&ret); has_ret = 1;
        if (!is_int_item(ret.sym)) die_at(ret.line, "RETURNING '%s' must be an integer item (the C ABI returns a word)", ret.sym->name);
    }
    if (at_word("on") || at_word("exception") || at_word("overflow") || (at_word("not") && (is_word(peek(1), "on") || is_word(peek(1), "exception"))))
        die_at(cur()->line, "CALL ... ON EXCEPTION is not implemented (every CALL is resolved by the linker)");
    emit_args(a, n);
    emit("\tjal r31, %s", link_name(name));
    if (has_ret) {
        if (is_hot_int(ret.sym)) {
            emit("\tstw sp+%d, r1", SLOT_C);
            emit_ref_addr(&ret, "r3");
            emit("\tldw r1, sp+%d", SLOT_C);
            emit_store_int(ret.sym, "r3", "r1");
        } else {
            emit("\tstw sp+%d, r1", SLOT_C);
            Arg b[2] = { arg_ref(&ret), arg_desc(sym_desc(ret.sym)) };
            emit_args(b, 2);
            emit("\tldw r5, sp+%d", SLOT_C);
            emit_call("cob_store_int");
        }
    }
    accept_word("end-call");
}

/* ---- Report Writer ------------------------------------------------------ */

static void emit_report_addr(const char *reg, Report *r)
{
    char lab[32]; snprintf(lab, sizeof lab, ".Lrpt%d_%d", g_unit, (int)(r - g_reports));
    emit_la(reg, lab);
}

static Report *expect_report(void)
{
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a report-name, found %s", tok_desc(t));
    Report *r = report_find(t->s);
    if (!r) die_at(t->line, "'%s' is not a report (no RD)", t->s);
    advance();
    return r;
}

static int rfield_desc(RField *f)
{
    Desc d; memset(&d, 0, sizeof d);
    switch (f->pi.category) {
    case PIC_ALPHABETIC: d.cat = COB_ALPHA; break;
    case PIC_ALPHANUMERIC: d.cat = COB_ALNUM; break;
    case PIC_ALPHANUMERIC_EDITED: d.cat = COB_ALNUM_ED; break;
    case PIC_NUMERIC: d.cat = COB_NUM; break;
    default: d.cat = COB_NUM_ED; break;
    }
    d.usage = COB_U_DISPLAY;
    d.digits = (unsigned char)f->pi.digits; d.scale = (signed char)f->pi.scale;
    if (f->pi.is_signed) d.flags |= COB_F_SIGNED;
    if (f->just) d.flags |= COB_F_JUST;
    if (f->blank_zero) d.flags |= COB_F_BLANKZ;
    if (f->pi.edited) snprintf(d.picstr, sizeof d.picstr, "%s", f->pi.pat);
    d.size = f->pi.bytes;
    return desc_add(&d);
}

static void emit_report_group(Report *r, RGroup *g);

/* the page advance: pad, count, and render the page heading */
static void emit_page_advance(Report *r)
{
    emit_report_addr("r3", r);
    emit_call("cob_rw_page_end");
    for (int k = 0; k < r->ng; k++)
        if (r->g[k].type == RG_PAGE_HEADING) emit_report_group(r, &r->g[k]);
}

/* render one group's lines at this point in the code; a body line that
 * would pass LAST DETAIL spills onto a new page first */
static void emit_report_group(Report *r, RGroup *g)
{
    int is_body = g->type == RG_DETAIL;
    for (int i = 0; i < g->nl; i++) {
        RLine *ln = &g->l[i];
        if (is_body) {
            emit_report_addr("r3", r);
            emit_li("r4", ln->abs); emit_li("r5", ln->plus); emit_li("r6", 1);
            emit_call("cob_rw_line_overflows");
            int Lok = new_label();
            emit("\tbeq r1, r0, .L%d", Lok);
            emit_page_advance(r);
            emit_label(Lok);
        }
        emit_call("cob_rw_line_begin");
        for (int k = 0; k < ln->nf; k++) {
            RField *f = &ln->f[k];
            Arg a[4];
            a[0] = arg_imm(f->column);
            a[1] = arg_desc(rfield_desc(f));
            if (f->has_source) {
                Ref *rf = xmalloc(sizeof *rf);
                char *q[1] = { f->source_qual };
                rf->sym = sym_lookup(f->source_name, q, f->nq, f->line);
                rf->line = f->line;
                if (rf->sym->ndims) die_at(f->line, "SOURCE '%s' needs subscripts, which are not implemented in reports yet", rf->sym->name);
                if (rf->sym->is_cond) die_at(f->line, "SOURCE '%s' is a condition-name", rf->sym->name);
                a[2] = arg_ref(rf); a[3] = arg_desc(sym_desc(rf->sym));
            } else if (f->value->kind == T_STR) {
                a[2] = arg_label(lit_label((unsigned char *)f->value->s, f->value->len));
                a[3] = arg_desc(str_desc(f->value->len));
            } else {
                NumLit n; numlit_parse(f->value, &n);
                int d; a[2] = arg_label(num_lit_label(&n, &d)); a[3] = arg_desc(d);
            }
            emit_args(a, 4);
            emit_call("cob_rw_field");
        }
        emit_report_addr("r3", r);
        emit_li("r4", ln->abs); emit_li("r5", ln->plus);
        emit_li("r6", is_body);
        emit_call("cob_rw_line_write");
    }
}

static void parse_initiate(void)
{
    Report *r = expect_report();
    emit_report_addr("r3", r);
    emit_call("cob_rw_initiate");
}

static void parse_terminate(void)
{
    Report *r = expect_report();
    emit_report_addr("r3", r);
    emit_call("cob_rw_terminate");
}

static void parse_generate(void)
{
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a report group after GENERATE");
    Report *r = NULL; RGroup *g = NULL;
    for (int i = 0; i < g_nreport && !g; i++)
        for (int k = 0; k < g_reports[i].ng; k++)
            if (g_reports[i].g[k].name[0] && !strcmp(g_reports[i].g[k].name, t->s)) { r = &g_reports[i]; g = &g_reports[i].g[k]; break; }
    if (!g) {
        if (report_find(t->s)) die_at(t->line, "GENERATE %s (summary reporting) is not implemented; GENERATE a DETAIL group", t->s);
        die_at(t->line, "'%s' is not a report group", t->s);
    }
    if (g->type != RG_DETAIL) die_at(t->line, "GENERATE needs a DETAIL group; '%s' is a page heading", t->s);
    advance();

    /* the fit test: the group's first line as it would land, plus the
     * relative extent of the rest */
    /* the fit test counts the lines that print something; a trailing
     * LINE with no fields is a blank line that may run into the footing
     * area -- measured on majesty's profit-and-loss report, where GnuCOBOL
     * put a "Net Profit" line on LAST DETAIL with its empty trailing line
     * beyond it (report-writer.md) */
    int last_printing = 0;
    for (int i = 0; i < g->nl; i++) if (g->l[i].nf) last_printing = i;
    int height = 0;
    for (int i = 1; i <= last_printing; i++) height += g->l[i].plus;
    emit_report_addr("r3", r);
    emit_li("r4", g->l[0].abs); emit_li("r5", g->l[0].plus); emit_li("r6", height);
    emit_call("cob_rw_fit");
    int Lfits = new_label();
    emit("\tbeq r1, r0, .L%d", Lfits);
    emit_page_advance(r);
    emit_label(Lfits);
    emit_report_group(r, g);
}

/* ---- EVALUATE ---------------------------------------------------------- */

typedef struct { int kind; Opnd o; } Subject;      /* kind: 0 value, 1 TRUE, 2 FALSE */

static Cond *cond_never(void)
{
    Opnd z, one; memset(&z, 0, sizeof z); memset(&one, 0, sizeof one);
    z.kind = O_NUM; numlit_zero(&z.num); one.kind = O_NUM; numlit_from_int(&one.num, 1);
    return cond_rel(&z, R_EQ, &one, 0);
}

static void parse_evaluate(void)
{
    Subject subj[8]; int ns = 0;
    for (;;) {
        if (ns >= 8) die_at(cur()->line, "too many EVALUATE subjects");
        if (accept_word("true")) subj[ns].kind = 1;
        else if (accept_word("false")) subj[ns].kind = 2;
        else { subj[ns].kind = 0; subj[ns].o = parse_cond_operand(); }
        ns++;
        if (!accept_word("also")) break;
    }
    int Lend = new_label();
    while (at_word("when")) {
        Cond *group = NULL; int other = 0;
        while (accept_word("when")) {
            if (accept_word("other")) { other = 1; break; }
            Cond *all = NULL;
            for (int i = 0; i < ns; i++) {
                if (i) expect_word("also");
                Cond *c = NULL;
                if (accept_word("any")) c = NULL;
                else if (subj[i].kind) {
                    if (at_word("true") || at_word("false")) {
                        int t = at_word("true"); advance();
                        if ((subj[i].kind == 1) != t) c = cond_never();
                    } else {
                        c = parse_cond();
                        if (subj[i].kind == 2) { Cond *nn = cond_new(C_NOT); nn->a = c; c = nn; }
                    }
                } else {
                    int neg = accept_word("not");
                    Opnd x = parse_cond_operand();
                    if (accept_word("thru") || accept_word("through")) {
                        Opnd y = parse_cond_operand();
                        c = cond_bin(C_AND, cond_rel(&subj[i].o, R_GE, &x, 0), cond_rel(&subj[i].o, R_LE, &y, 0));
                    } else c = cond_rel(&subj[i].o, R_EQ, &x, 0);
                    if (neg) { Cond *nn = cond_new(C_NOT); nn->a = c; c = nn; }
                }
                if (c) all = all ? cond_bin(C_AND, all, c) : c;
            }
            if (!all) { Cond *nn = cond_new(C_NOT); nn->a = cond_never(); all = nn; }   /* every ANY: always */
            group = group ? cond_bin(C_OR, group, all) : all;
        }
        if (other) { parse_statements(); emit_jump(Lend); break; }
        int Lnext = new_label();
        cond_jump_false(group, Lnext);
        parse_statements();
        emit_jump(Lend);
        emit_label(Lnext);
    }
    emit_label(Lend);
    accept_word("end-evaluate");
}

/* ---- INSPECT ----------------------------------------------------------- */

/* a pattern operand: address and length as Args */
static void pattern_args(Opnd *o, Arg *addr, Arg *len)
{
    if (o->kind == O_FIG) { unsigned char c = (unsigned char)fig_byte(o->tok->s); *addr = arg_label(lit_label(&c, 1)); *len = arg_imm(1); return; }
    Arg d; opnd_args(o, addr, &d, 0, 0);
    *len = arg_len(o);
}

static Opnd ref_opnd(const Ref *r)
{
    Opnd o; memset(&o, 0, sizeof o);
    o.kind = O_REF; o.ref = *r; o.line = r->line;
    return o;
}

static void parse_inspect(void)
{
    Ref item; parse_ref(&item);
    if (item.sym->is_cond) die_at(item.line, "INSPECT of a condition-name");
    Opnd itemo = ref_opnd(&item);
    if (accept_word("converting")) die_at(cur()->line, "INSPECT CONVERTING is not implemented (after v1)");
    int any = 0;
    if (accept_word("tallying")) {
        any = 1;
        for (;;) {
            Ref tally; parse_ref(&tally);
            if (!is_int_item(tally.sym)) die_at(tally.line, "the INSPECT tally '%s' must be an integer item", tally.sym->name);
            expect_word("for");
            for (;;) {
                int kind = 0; Opnd pat; memset(&pat, 0, sizeof pat);
                if (accept_word("characters")) kind = 0;
                else if (accept_word("all")) { kind = 1; parse_operand(&pat); }
                else if (accept_word("leading")) { kind = 2; parse_operand(&pat); }
                else die_at(cur()->line, "expected CHARACTERS, ALL or LEADING in INSPECT TALLYING");
                if (at_word("before") || at_word("after")) die_at(cur()->line, "INSPECT ... BEFORE/AFTER INITIAL is not implemented");
                Arg a[5];
                a[0] = arg_ref(&item); a[1] = arg_len(&itemo); a[2] = arg_imm(kind);
                if (kind) pattern_args(&pat, &a[3], &a[4]); else { a[3] = arg_imm(0); a[4] = arg_imm(0); }
                emit_args(a, 5);
                emit_call("cob_inspect_tally");
                emit("\tstw sp+%d, r1", SLOT_C);
                if (is_hot_int(tally.sym)) {
                    emit_ref_addr(&tally, "r3");
                    emit_load_int(tally.sym, "r3", "r1");
                    emit("\tldw r2, sp+%d", SLOT_C);
                    emit("\tadd r1, r1, r2");
                    emit_trunc(tally.sym);
                    emit_store_int(tally.sym, "r3", "r1");
                } else {
                    emit("\tldw r3, sp+%d", SLOT_C);
                    emit("\tsrai r4, r3, 31");
                    emit_li("r5", 0);
                    emit_call("cob_push_lit");
                    emit_top_op(&tally, "cob_top_addto", 0);
                    emit_call("cob_drop");
                }
                if (!(at_word("characters") || at_word("all") || at_word("leading"))) break;
            }
            if (!at_operand() || at_word("replacing")) break;
        }
    }
    if (accept_word("replacing")) {
        any = 1;
        for (;;) {
            int kind = 0; Opnd pat, rep; memset(&pat, 0, sizeof pat); memset(&rep, 0, sizeof rep);
            if (accept_word("characters")) { kind = 0; expect_word("by"); parse_operand(&rep); }
            else {
                if (accept_word("all")) kind = 1;
                else if (accept_word("leading")) kind = 2;
                else if (accept_word("first")) kind = 3;
                else die_at(cur()->line, "expected CHARACTERS, ALL, LEADING or FIRST in INSPECT REPLACING");
                parse_operand(&pat); expect_word("by"); parse_operand(&rep);
                int pl = pat.kind == O_FIG ? 1 : opnd_size(&pat), rl = rep.kind == O_FIG ? 1 : opnd_size(&rep);
                if (pl > 0 && rl > 0 && pl != rl) die_at(rep.line, "INSPECT REPLACING: the two operands must be the same length");
            }
            if (at_word("before") || at_word("after")) die_at(cur()->line, "INSPECT ... BEFORE/AFTER INITIAL is not implemented");
            Arg a[6];
            a[0] = arg_ref(&item); a[1] = arg_len(&itemo); a[2] = arg_imm(kind);
            if (kind) pattern_args(&pat, &a[3], &a[4]); else { a[3] = arg_imm(0); a[4] = arg_imm(1); }
            Arg rl; pattern_args(&rep, &a[5], &rl);
            emit_args(a, 6);
            emit_call("cob_inspect_replace");
            if (!(at_word("characters") || at_word("all") || at_word("leading") || at_word("first"))) break;
        }
    }
    if (!any) die_at(item.line, "INSPECT needs TALLYING or REPLACING");
}

/* ---- INITIALIZE -------------------------------------------------------- */

static void parse_initialize(void)
{
    int n = 0;
    while (at_operand()) {
        Ref r; parse_ref(&r);
        if (r.sym->is_cond) die_at(r.line, "INITIALIZE of a condition-name");
        if (r.rm) die_at(r.line, "INITIALIZE of a reference-modified item is not implemented");
        /* the template: the item's default initialisation, VALUEs ignored */
        Sym tmp; memset(&tmp, 0, sizeof tmp);
        tmp.image = xmalloc(r.sym->size); tmp.image_size = r.sym->size;
        g_no_values = 1;
        init_one(&tmp, sym_idx(r.sym), 0, 1);
        g_no_values = 0;
        Arg a[3] = { arg_ref(&r), arg_label(lit_label(tmp.image, r.sym->size)), arg_imm(r.sym->size) };
        emit_args(a, 3);
        emit_call("memcpy");
        free(tmp.image);
        n++;
    }
    if (!n) die_at(cur()->line, "INITIALIZE needs an item");
    if (at_word("replacing")) die_at(cur()->line, "INITIALIZE ... REPLACING is not implemented");
    if (at_word("with") || at_word("default")) die_at(cur()->line, "INITIALIZE WITH FILLER / DEFAULT is COBOL 2002");
}

/* ---- SEARCH ------------------------------------------------------------ */

/* SEARCH table [VARYING id] [AT END s] {WHEN cond s}... [END-SEARCH]
 * walks the table's first index from its current value; SEARCH ALL sets
 * it to 1 first.  Both are a scan here: SEARCH ALL's table is ordered by
 * its key and its keys are unique in every use the corpus makes, so the
 * first entry satisfying the WHEN is the one a binary search would
 * report.  The bound is the OCCURS count, or the DEPENDING ON item. */
static void parse_search(void)
{
    int all = accept_word("all");
    /* the table is named without subscripts */
    Tok *tt = cur();
    if (tt->kind != T_WORD) die_at(tt->line, "SEARCH needs a table name");
    Ref t; memset(&t, 0, sizeof t); t.line = tt->line;
    t.sym = sym_lookup(tt->s, NULL, 0, tt->line); advance();
    Sym *tbl = t.sym;
    if (!tbl->occurs) die_at(t.line, "SEARCH needs a table (an item with OCCURS)");
    if (cur()->kind == T_LP) die_at(t.line, "SEARCH names the table without subscripts");
    if (tbl->idx1 < 0) die_at(t.line, "SEARCH needs the table to have INDEXED BY");
    Sym *ix = &g_sym[tbl->idx1];
    Ref ixr; memset(&ixr, 0, sizeof ixr); ixr.sym = ix; ixr.line = t.line;
    Ref vary; int has_vary = 0;
    if (accept_word("varying")) { parse_ref(&vary); has_vary = 1; if (!is_int_item(vary.sym)) die_at(vary.line, "VARYING needs an integer or index item"); }
    if (all && has_vary) die_at(t.line, "SEARCH ALL takes no VARYING");

    int Lend = new_label(), Ltop = new_label(), Latend = new_label();
    Opnd one; memset(&one, 0, sizeof one); one.kind = O_NUM; numlit_from_int(&one.num, 1); one.line = t.line;
    if (all) emit_move(&one, &ixr);
    emit_label(Ltop);
    /* at end when the index passes the bound */
    Opnd ixo; memset(&ixo, 0, sizeof ixo); ixo.kind = O_REF; ixo.ref = ixr; ixo.line = t.line;
    emit_hot_value(&ixo);
    emit("\tstw sp+%d, r1", SLOT_A);
    if (tbl->odo_dep_sym) {
        Opnd d; memset(&d, 0, sizeof d); d.kind = O_REF; d.ref.sym = tbl->odo_dep_sym; d.ref.line = t.line;
        if (is_hot_int(tbl->odo_dep_sym)) emit_hot_value(&d);
        else { Arg a[2] = { arg_ref(&d.ref), arg_desc(sym_desc(tbl->odo_dep_sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
    } else emit_li("r1", tbl->occurs);
    emit("\tldw r2, sp+%d", SLOT_A);
    emit("\tslt r1, r1, r2");                    /* bound < index */
    emit("\tbne r1, r0, .L%d", Latend);

    /* the WHENs are parsed once; their bodies are emitted after the
     * loop, so the loop only holds the tests */
    int Lwhen[16]; int nwhen = 0;
    int save_atend = -1, atend_start = -1;
    if (accept_word("at")) {
        expect_word("end");
        /* the AT END imperative comes before the WHENs in the source; scan
         * past it now (no code), emit it at Latend later */
        atend_start = g_tp;
        g_noemit++; parse_statements(); g_noemit--;
        save_atend = g_tp;
    }
    int when_start[16], when_body_end[16];
    while (at_word("when")) {
        if (nwhen >= 16) die_at(cur()->line, "too many WHENs in SEARCH");
        advance();
        Cond *c = parse_cond();
        Lwhen[nwhen] = new_label();
        cond_jump_true(c, Lwhen[nwhen]);
        when_start[nwhen] = g_tp;
        g_noemit++;
        if (at_word("next")) { advance(); expect_word("sentence"); } else parse_statements();
        g_noemit--;
        when_body_end[nwhen] = g_tp;
        nwhen++;
    }
    if (!nwhen) die_at(t.line, "SEARCH needs at least one WHEN");
    /* no WHEN held: step and go round */
    Opnd step; memset(&step, 0, sizeof step); step.kind = O_NUM; numlit_from_int(&step.num, 1); step.line = t.line;
    emit_add_to_ref(&step, &ixr);
    if (has_vary) emit_add_to_ref(&step, &vary);
    emit_jump(Ltop);

    /* AT END */
    emit_label(Latend);
    if (atend_start >= 0) { int here = g_tp; g_tp = atend_start; parse_statements(); if (g_tp != save_atend) die_at(t.line, "internal: AT END re-parse drifted"); g_tp = here; }
    emit_jump(Lend);
    /* WHEN bodies */
    for (int i = 0; i < nwhen; i++) {
        emit_label(Lwhen[i]);
        int here = g_tp; g_tp = when_start[i];
        if (at_word("next")) { advance(); expect_word("sentence"); if (g_sentence_label < 0) g_sentence_label = new_label(); emit_jump(g_sentence_label); }
        else parse_statements();
        if (g_tp != when_body_end[i]) die_at(t.line, "internal: WHEN re-parse drifted");
        g_tp = here;
        emit_jump(Lend);
    }
    emit_label(Lend);
    accept_word("end-search");
}

/* ---- dispatch ---------------------------------------------------------- */

static void parse_statement(void)
{
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a statement, found %s", tok_desc(t));
    const char *v = t->s;

    if (!strcmp(v, "display")) { advance(); parse_display(); return; }
    if (!strcmp(v, "move")) { advance(); parse_move(); return; }
    if (!strcmp(v, "add")) { advance(); parse_add(); return; }
    if (!strcmp(v, "subtract")) { advance(); parse_subtract(); return; }
    if (!strcmp(v, "multiply")) { advance(); parse_multiply(); return; }
    if (!strcmp(v, "divide")) { advance(); parse_divide(); return; }
    if (!strcmp(v, "compute")) { advance(); parse_compute(); return; }
    if (!strcmp(v, "open")) { advance(); parse_open(); return; }
    if (!strcmp(v, "close")) { advance(); parse_close(); return; }
    if (!strcmp(v, "read")) { advance(); parse_read(); return; }
    if (!strcmp(v, "write")) { advance(); parse_write(); return; }
    if (!strcmp(v, "rewrite")) { advance(); parse_rewrite(); return; }
    if (!strcmp(v, "delete")) { advance(); parse_delete(); return; }
    if (!strcmp(v, "start")) { advance(); parse_start(); return; }
    if (!strcmp(v, "string")) { advance(); parse_string(); return; }
    if (!strcmp(v, "call")) { advance(); parse_call(); return; }
    if (!strcmp(v, "initiate")) { advance(); parse_initiate(); return; }
    if (!strcmp(v, "accept")) { advance(); parse_accept(); return; }
    if (!strcmp(v, "evaluate")) { advance(); parse_evaluate(); return; }
    if (!strcmp(v, "search")) { advance(); parse_search(); return; }
    if (!strcmp(v, "inspect")) { advance(); parse_inspect(); return; }
    if (!strcmp(v, "initialize")) { advance(); parse_initialize(); return; }
    if (!strcmp(v, "generate")) { advance(); parse_generate(); return; }
    if (!strcmp(v, "terminate")) { advance(); parse_terminate(); return; }
    if (!strcmp(v, "cancel")) {
        /* everything is linked statically; there is nothing to release */
        advance();
        while (cur()->kind == T_STR || (cur()->kind == T_WORD && !is_verb(cur()->s) && !is_terminator(cur()->s))) advance();
        return;
    }
    if (!strcmp(v, "if")) { advance(); parse_if(); return; }
    if (!strcmp(v, "perform")) { advance(); parse_perform(); return; }
    if (!strcmp(v, "go")) { advance(); parse_goto(); return; }
    if (!strcmp(v, "set")) { advance(); parse_set(); return; }
    if (!strcmp(v, "stop")) {
        advance();
        if (accept_word("run")) { emit_li("r3", 0); emit_call("cob_stop_run"); return; }
        die_at(t->line, "STOP literal is not implemented");
    }
    if (!strcmp(v, "goback")) { advance(); emit("\tjal r0, .Lgb%d", g_unit); return; }
    if (!strcmp(v, "continue")) { advance(); return; }
    if (!strcmp(v, "exit")) {
        advance();
        if (accept_word("program")) { emit("\tjal r0, .Lgb%d", g_unit); return; }
        if (at_word("perform") || at_word("paragraph") || at_word("section"))
            die_at(t->line, "EXIT %s is not in COBOL 85", cur()->s);
        return;
    }
    if (!strcmp(v, "next")) die_at(t->line, "NEXT SENTENCE is only valid inside IF (or SEARCH)");
    if (!strcmp(v, "alter"))
        die_at(t->line, "ALTER is not in COBOL 85 (obsolete in the 1985 standard); refused");
    if (!strcmp(v, "enter") || !strcmp(v, "disable") || !strcmp(v, "enable") ||
        !strcmp(v, "purge") || !strcmp(v, "receive") || !strcmp(v, "send"))
        die_at(t->line, "%s is not supported (the Communication module is deliberately out)", v);
    static const struct { const char *verb; const char *when; } later[] = {

        { "unstring", "after v1" }, { "sort", "after v1" },
        { "merge", "after v1" }, { "release", "after v1" }, { "return", "after v1" },
        { "use", "after v1" }, { "suppress", "after v1" }, { NULL, NULL } };
    for (int i = 0; later[i].verb; i++)
        if (!strcmp(v, later[i].verb)) die_at(t->line, "the verb %s is not implemented yet (%s)", v, later[i].when);
    if (is_terminator(v)) die_at(t->line, "'%s' without a matching statement", v);
    die_at(t->line, "'%s' is not a COBOL verb", v);
}

static void emit_exit_check(int id)
{
    int Ln = new_label();
    emit_li("r3", id);
    emit_call("cob_perform_exit");
    emit("\tbeq r1, r0, .L%d", Ln);
    emit("\tjalr r0, r1, 0");
    emit_label(Ln);
}

static int g_saw_end_program;

static void parse_procedure_division(void)
{
    expect_word("procedure"); expect_word("division");
    Sym *using[8]; int nusing = 0;
    if (accept_word("using")) {
        while (cur()->kind == T_WORD && !at_word("returning")) {
            if (nusing >= 8) die_at(cur()->line, "more than eight USING items (stack arguments) are not implemented yet");
            Sym *u = sym_lookup(cur()->s, NULL, 0, cur()->line);
            if (!g_sym[u->record].is_linkage || u->parent >= 0)
                die_at(cur()->line, "USING '%s' must be a level 01 or 77 item of the LINKAGE SECTION", u->name);
            using[nusing++] = u;
            advance();
        }
    }
    if (at_word("returning"))
        die_at(cur()->line, "PROCEDURE DIVISION RETURNING is COBOL 2002; make the result the last USING item (docs/functions.md)");
    expect_period();
    prescan_paragraphs(g_tp);

    char entry[128];
    snprintf(entry, sizeof entry, "%s", link_name(g_progid));   /* link_name's buffer is static; CALLs reuse it */
    emit("\t.text");
    emit("\t.globl %s", entry);
    emit("\t.p2align 2");
    emit("\t.type %s,@function", entry);
    emit("%s:", entry);
    emit("\taddi sp, sp, -%d", FRAME);
    emit("\tstw sp+0, lr");
    emit("\tstw sp+4, r11");
    /* the caller's addresses go into the LINKAGE cells */
    for (int i = 0; i < nusing; i++) {
        emit_la("r1", g_sym[using[i]->record].label);
        emit("\tstw r1+0, %s", argreg(i));
    }

    int cur_par = -1, cur_sec = -1;
    for (;;) {
        Tok *t = cur();
        if (t->kind == T_EOF) break;
        if (is_word(t, "end") && is_word(peek(1), "program")) break;

        if (t->kind == T_WORD && !is_verb(t->s) && (peek(1)->kind == T_PERIOD ||
            (is_word(peek(1), "section") && peek(2)->kind == T_PERIOD))) {
            Para *p = para_find(t->s);
            if (!p) die_at(t->line, "internal: paragraph '%s' not prescanned", t->s);
            if (cur_par >= 0) emit_exit_check(cur_par);
            if (p->is_section && cur_sec >= 0) emit_exit_check(cur_sec);
            emit_para_label(p);
            if (p->is_section) { cur_sec = p->id; cur_par = -1; } else cur_par = p->id;
            advance(); if (p->is_section) advance();
            expect_period();
            continue;
        }
        if (t->kind == T_WORD && !is_verb(t->s) && peek(1)->kind == T_NUM && is_word(peek(2), "section"))
            die_at(t->line, "section segment numbers are obsolete in COBOL 85; not supported");

        /* a sentence */
        g_sentence_label = -1;
        for (;;) {
            parse_statement();
            if (cur()->kind == T_PERIOD) { advance(); break; }
            if (cur()->kind == T_EOF) die_at(cur()->line, "missing '.' at the end of the last sentence");
            if (at_scope_end()) die_at(cur()->line, "'%s' without a matching statement", cur()->s);
        }
        if (g_sentence_label >= 0) emit_label(g_sentence_label);
    }
    if (cur_par >= 0) emit_exit_check(cur_par);
    if (cur_sec >= 0) emit_exit_check(cur_sec);

    emit(".Lgb%d:", g_unit);
    emit("\taddi r1, r0, 0");
    emit("\tldw r11, sp+4");
    emit("\tldw lr, sp+0");
    emit("\taddi sp, sp, %d", FRAME);
    emit("\tjalr r0, r31, 0");

    if (g_unit == 0 && !g_module) {
        /* the first unit of an executable is the main program */
        emit("\t.globl main");
        emit("\t.p2align 2");
        emit("\t.type main,@function");
        emit("main:");
        emit("\taddi sp, sp, -16");
        emit("\tstw sp+0, lr");
        emit_call("cob_set_args");          /* r3 = argc, r4 = argv, as crt0 hands them */
        emit_call("cob_init");
        emit("\tjal r31, %s", entry);
        emit_li("r3", 0);
        emit_call("cob_stop_run");          /* flushes, restores the terminal, exits */
    }

    g_saw_end_program = 0;
    if (accept_word("end")) {
        expect_word("program");
        if (cur()->kind != T_WORD || strcmp(cur()->s, g_progid))
            die_at(cur()->line, "END PROGRAM names '%s' but the program is '%s'", cur()->s, g_progid);
        advance();
        expect_period();
        g_saw_end_program = 1;
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

/* SELECT [OPTIONAL] file ASSIGN TO ... [ORGANIZATION ...] [ACCESS ...]
 * [RECORD KEY ...] [FILE STATUS ...] [SHARING ...]. */
static void parse_select(void)
{
    int line = cur()->line;
    if (g_nfile == g_fcap) { g_fcap = g_fcap ? g_fcap * 2 : 16; g_files = realloc(g_files, g_fcap * sizeof *g_files); }
    File *f = &g_files[g_nfile++];
    memset(f, 0, sizeof *f);
    f->line = line; f->rec = -1; f->org = COB_ORG_SEQ;
    if (accept_word("optional")) f->optional = 1;
    if (cur()->kind != T_WORD) die_at(line, "expected a file-name after SELECT");
    if (file_find(cur()->s)) die_at(line, "file '%s' is SELECTed twice", cur()->s);
    snprintf(f->name, sizeof f->name, "%s", cur()->s);
    advance();
    int has_assign = 0;
    while (cur()->kind != T_PERIOD) {
        Tok *t = cur();
        if (t->kind != T_WORD) die_at(t->line, "unexpected %s in SELECT %s", tok_desc(t), f->name);
        if (accept_word("assign")) {
            accept_word("to");
            if (cur()->kind == T_STR) { f->assign_lit = cur(); advance(); }
            else if (cur()->kind == T_WORD) {
                if (at_word("disk") || at_word("keyboard") || at_word("display") || at_word("printer"))
                    die_at(t->line, "ASSIGN TO %s (a device) is not supported; name a file", cur()->s);
                snprintf(f->assign_name, sizeof f->assign_name, "%s", cur()->s); advance();
            } else die_at(t->line, "expected a literal or data-name after ASSIGN TO");
            has_assign = 1;
            continue;
        }
        if (accept_word("organization") || accept_word("organisation")) {
            accept_word("is"); f->org_given = 1;
            if (accept_word("line")) { expect_word("sequential"); f->org = COB_ORG_LINESEQ; }
            else if (accept_word("sequential")) f->org = COB_ORG_SEQ;
            else if (accept_word("indexed")) f->org = COB_ORG_INDEXED;
            else if (accept_word("relative")) f->org = COB_ORG_RELATIVE;
            else die_at(t->line, "unknown ORGANIZATION %s", cur()->s);
            continue;
        }
        if (accept_word("access")) {
            accept_word("mode"); accept_word("is");
            if (accept_word("sequential")) f->access = 0;
            else if (accept_word("random")) f->access = 1;
            else if (accept_word("dynamic")) f->access = 2;
            else die_at(t->line, "unknown ACCESS MODE %s", cur()->s);
            continue;
        }
        if (accept_word("record")) {
            accept_word("key"); accept_word("is");
            if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after RECORD KEY");
            snprintf(f->key_name, sizeof f->key_name, "%s", cur()->s); advance();
            continue;
        }
        if (accept_word("alternate")) die_at(t->line, "ALTERNATE RECORD KEY is not implemented (after v1)");
        if (accept_word("relative")) die_at(t->line, "RELATIVE KEY is not implemented yet");
        if (accept_word("file")) {
            expect_word("status"); accept_word("is");
            if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after FILE STATUS");
            snprintf(f->status_name, sizeof f->status_name, "%s", cur()->s); advance();
            continue;
        }
        if (accept_word("status")) {
            accept_word("is");
            snprintf(f->status_name, sizeof f->status_name, "%s", cur()->s); advance();
            continue;
        }
        if (accept_word("sharing")) {
            /* SHARING WITH ALL OTHER: accepted and ignored on this machine */
            accept_word("with");
            if (accept_word("all")) accept_word("other");
            else if (accept_word("no")) accept_word("other");
            else if (accept_word("read")) accept_word("only");
            continue;
        }
        if (accept_word("lock")) {
            accept_word("mode"); accept_word("is");
            while (cur()->kind == T_WORD && !at_word("assign") && !at_word("organization") &&
                   !at_word("access") && !at_word("file") && !at_word("record") && !at_word("sharing")) advance();
            continue;
        }
        if (accept_word("reserve")) { while (cur()->kind != T_PERIOD && !at_word("organization") && !at_word("access") && !at_word("file")) advance(); continue; }
        die_at(t->line, "unexpected %s in SELECT %s", tok_desc(t), f->name);
    }
    expect_period();
    if (!has_assign) die_at(line, "SELECT %s has no ASSIGN clause", f->name);
    if (f->org == COB_ORG_INDEXED && !f->key_name[0]) die_at(line, "an INDEXED file needs RECORD KEY");
}

static void parse_environment_division(void)
{
    if (!accept_word("environment")) return;
    expect_word("division"); expect_period();
    if (accept_word("configuration")) {
        expect_word("section"); expect_period();
        for (;;) {
            if (accept_word("source-computer") || accept_word("object-computer")) {
                expect_period();
                if (cur()->kind == T_WORD && !at_word("special-names") && !at_word("input-output") &&
                    !at_word("source-computer") && !at_word("object-computer") && !at_division()) skip_to_period();
                continue;
            }
            if (at_word("special-names")) {
                advance(); expect_period();
                for (;;) {
                    if (cur()->kind == T_PERIOD) { advance(); continue; }
                    if (accept_word("class")) {
                        if (cur()->kind != T_WORD) die_at(cur()->line, "expected a class-name after CLASS");
                        if (g_nclass == (int)(sizeof g_class / sizeof g_class[0])) die_at(cur()->line, "too many CLASS clauses");
                        UClass *uc = &g_class[g_nclass++];
                        memset(uc, 0, sizeof *uc);
                        snprintf(uc->name, sizeof uc->name, "%s", cur()->s); advance();
                        accept_word("is");
                        int any = 0;
                        while (cur()->kind == T_STR) {
                            Tok *lo = cur(); advance();
                            if (lo->len != 1) die_at(lo->line, "CLASS %s: each literal is one character (this one is %d)", uc->name, lo->len);
                            unsigned a = (unsigned char)lo->s[0], b = a;
                            if (accept_word("through") || accept_word("thru")) {
                                if (cur()->kind != T_STR || cur()->len != 1) die_at(cur()->line, "CLASS %s: THROUGH needs a one-character literal", uc->name);
                                b = (unsigned char)cur()->s[0]; advance();
                            }
                            if (b < a) { unsigned t = a; a = b; b = t; }
                            for (unsigned c = a; c <= b; c++) uc->tab[c] = 1;
                            any = 1;
                        }
                        if (!any) die_at(cur()->line, "CLASS %s: expected a one-character literal", uc->name);
                        continue;
                    }
                    if (at_division() || at_word("input-output")) break;
                    die_at(cur()->line, "SPECIAL-NAMES clause '%s' is not implemented yet (CLASS is)", cur()->s);
                }
                continue;
            }
            if (at_word("repository"))
                die_at(cur()->line, "REPOSITORY is COBOL 2002; rewrite user-defined functions as CALL (docs/functions.md)");
            break;
        }
    }
    if (accept_word("input-output")) {
        expect_word("section"); expect_period();
        if (accept_word("file-control")) {
            expect_period();
            while (accept_word("select")) parse_select();
        }
        if (at_word("i-o-control")) die_at(cur()->line, "I-O-CONTROL is not implemented yet");
    }
    if (!at_division()) die_at(cur()->line, "unexpected %s in the ENVIRONMENT DIVISION", tok_desc(cur()));
}

/* FD file-name [clauses]. followed by its 01s */
static void parse_fd(void)
{
    int line = cur()->line;
    if (accept_word("sd")) die_at(line, "SD (sort files) is not implemented (after v1)");
    expect_word("fd");
    if (cur()->kind != T_WORD) die_at(line, "expected a file-name after FD");
    File *f = file_find(cur()->s);
    if (!f) die_at(line, "FD %s has no SELECT", cur()->s);
    advance();
    while (cur()->kind != T_PERIOD) {
        Tok *t = cur();
        if (t->kind != T_WORD) die_at(t->line, "unexpected %s in FD %s", tok_desc(t), f->name);
        if (accept_word("block")) {
            /* BLOCK CONTAINS: a blocking hint with no meaning on a byte stream */
            accept_word("contains");
            if (cur()->kind == T_NUM) advance();
            if (accept_word("to")) { if (cur()->kind == T_NUM) advance(); }
            accept_word("records"); accept_word("characters");
            continue;
        }
        if (accept_word("record")) {
            if (accept_word("is") || accept_word("are")) { }
            if (accept_word("varying")) {
                accept_word("in"); accept_word("size");
                if (accept_word("from")) { if (cur()->kind != T_NUM) die_at(t->line, "expected a number after FROM"); f->minlen = atoi(cur()->s); advance(); }
                if (accept_word("to")) { if (cur()->kind != T_NUM) die_at(t->line, "expected a number after TO"); f->maxlen = atoi(cur()->s); advance(); }
                accept_word("characters");
                if (accept_word("depending")) {
                    accept_word("on");
                    if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after DEPENDING ON");
                    snprintf(f->dep_name, sizeof f->dep_name, "%s", cur()->s); advance();
                }
                f->varying = 1;
                continue;
            }
            accept_word("contains");
            if (cur()->kind != T_NUM) die_at(t->line, "expected a number after RECORD CONTAINS");
            f->minlen = atoi(cur()->s); advance();
            if (accept_word("to")) {
                if (cur()->kind != T_NUM) die_at(t->line, "expected a number after TO");
                f->maxlen = atoi(cur()->s); advance();
                f->varying = 1;                     /* m TO n: variable, as cobc370 infers */
            } else { f->maxlen = f->minlen; }
            accept_word("characters");
            continue;
        }
        if (accept_word("label")) { accept_word("record"); accept_word("records"); accept_word("is"); accept_word("are"); accept_word("standard"); accept_word("omitted"); continue; }
        if (accept_word("data")) { accept_word("record"); accept_word("records"); accept_word("is"); accept_word("are"); while (cur()->kind == T_WORD && !at_word("block") && !at_word("record") && !at_word("label") && !at_word("report") && !at_word("value")) advance(); continue; }
        if (accept_word("report") || accept_word("reports")) {
            accept_word("is"); accept_word("are");
            if (cur()->kind != T_WORD) die_at(t->line, "expected a report-name");
            snprintf(f->report_name, sizeof f->report_name, "%s", cur()->s); advance();
            continue;
        }
        if (accept_word("recording")) {
            accept_word("mode"); accept_word("is");
            if (accept_word("f")) { f->varying = 0; continue; }
            if (accept_word("v")) { f->varying = 1; continue; }
            die_at(t->line, "RECORDING MODE %s is refused (U and S are tapemgr's business; docs/framing.md)", cur()->s);
        }
        if (accept_word("value")) { expect_word("of"); while (cur()->kind != T_PERIOD && !at_word("block") && !at_word("record") && !at_word("data")) advance(); continue; }
        if (accept_word("linage")) die_at(t->line, "LINAGE is not implemented");
        if (accept_word("code-set")) die_at(t->line, "CODE-SET is not supported (ASCII only)");
        die_at(t->line, "unexpected %s in FD %s", tok_desc(t), f->name);
    }
    expect_period();
    g_cur_fd = (int)(f - g_files);
    while (cur()->kind == T_NUM) parse_data_item();
    g_cur_fd = -1;
    if (f->varying && f->org == COB_ORG_LINESEQ)
        die_at(line, "FD %s: variable records need ORGANIZATION SEQUENTIAL (LINE SEQUENTIAL names its own framing; docs/framing.md)", f->name);
}

/* RD report-name [PAGE [LIMIT IS] n [LINE(S)]] [HEADING n] [FIRST DETAIL n]
 * [LAST DETAIL n] [FOOTING n]. then the group descriptions */
static void parse_rd(void)
{
    int line = cur()->line;
    expect_word("rd");
    if (cur()->kind != T_WORD) die_at(line, "expected a report-name after RD");
    if (g_nreport == g_rcap) { g_rcap = g_rcap ? g_rcap * 2 : 4; g_reports = realloc(g_reports, g_rcap * sizeof *g_reports); }
    Report *r = &g_reports[g_nreport++];
    memset(r, 0, sizeof *r);
    r->line = line; r->file = -1;
    snprintf(r->name, sizeof r->name, "%s", cur()->s);
    advance();
    for (int i = 0; i < g_nfile; i++) if (!strcmp(g_files[i].report_name, r->name)) r->file = i;
    if (r->file < 0) die_at(line, "no FD says REPORT IS %s", r->name);
    /* a print file SELECTed without ORGANIZATION is line sequential: that
     * is what GnuCOBOL made of gl036's, and its .prn is the oracle */
    if (!g_files[r->file].org_given && g_files[r->file].org == COB_ORG_SEQ) g_files[r->file].org = COB_ORG_LINESEQ;
    if (g_files[r->file].org != COB_ORG_LINESEQ) die_at(line, "the print file of report %s must be LINE SEQUENTIAL", r->name);
    while (cur()->kind != T_PERIOD) {
        Tok *t = cur();
        if (accept_word("page")) {
            accept_word("limit"); accept_word("limits"); accept_word("is"); accept_word("are");
            if (cur()->kind != T_NUM) die_at(t->line, "expected a number after PAGE LIMIT");
            r->page_limit = atoi(cur()->s); advance();
            accept_word("line"); accept_word("lines");
            continue;
        }
        if (accept_word("heading")) { if (cur()->kind != T_NUM) die_at(t->line, "expected a number after HEADING"); r->heading = atoi(cur()->s); advance(); continue; }
        if (accept_word("first")) { expect_word("detail"); if (cur()->kind != T_NUM) die_at(t->line, "expected a number"); r->first_detail = atoi(cur()->s); advance(); continue; }
        if (accept_word("last")) { expect_word("detail"); if (cur()->kind != T_NUM) die_at(t->line, "expected a number"); r->last_detail = atoi(cur()->s); advance(); continue; }
        if (accept_word("footing")) die_at(t->line, "FOOTING is not implemented (no footing groups in v1)");
        if (accept_word("control") || accept_word("controls")) die_at(t->line, "CONTROL is not implemented (after v1; majesty's totals are Procedure Division items)");
        if (accept_word("code")) die_at(t->line, "the CODE clause is not implemented");
        die_at(t->line, "unexpected %s in RD %s", tok_desc(t), r->name);
    }
    expect_period();
    if (!r->page_limit) die_at(line, "RD %s needs PAGE LIMIT", r->name);
    if (!r->heading) r->heading = 1;
    if (!r->first_detail) r->first_detail = r->heading;
    if (!r->last_detail) r->last_detail = r->page_limit;

    /* groups: 01 [name] TYPE ... . then LINE entries with their fields */
    while (cur()->kind == T_NUM && !strcmp(cur()->s, "01")) {
        advance();
        if (r->ng == r->gcap) { r->gcap = r->gcap ? r->gcap * 2 : 8; r->g = realloc(r->g, r->gcap * sizeof *r->g); }
        RGroup *g = &r->g[r->ng++];
        memset(g, 0, sizeof *g);
        g->line = cur()->line;
        if (cur()->kind == T_WORD && !at_word("type")) { snprintf(g->name, sizeof g->name, "%s", cur()->s); advance(); }
        int has_type = 0;
        while (cur()->kind != T_PERIOD) {
            Tok *t = cur();
            if (accept_word("type")) {
                accept_word("is");
                if (accept_word("page")) { expect_word("heading"); g->type = RG_PAGE_HEADING; }
                else if (accept_word("ph")) g->type = RG_PAGE_HEADING;
                else if (accept_word("detail") || accept_word("de")) g->type = RG_DETAIL;
                else if (at_word("report") || at_word("rh") || at_word("rf") || at_word("control") || at_word("ch") || at_word("cf") || at_word("pf"))
                    die_at(t->line, "TYPE %s groups are not implemented (v1 is PAGE HEADING and DETAIL)", cur()->s);
                else die_at(t->line, "unknown report group TYPE %s", cur()->s);
                has_type = 1;
                continue;
            }
            if (at_word("line") || at_word("next") || at_word("column") || at_word("pic") || at_word("picture") || at_word("source") || at_word("value"))
                die_at(t->line, "clauses on the 01 report group '%s' are not implemented; put LINE on a 02 entry", g->name);
            die_at(t->line, "unexpected %s in report group '%s'", tok_desc(t), g->name);
        }
        expect_period();
        if (!has_type) die_at(g->line, "report group '%s' needs a TYPE", g->name);

        /* 02 LINE entries, each with its 05 fields */
        while (cur()->kind == T_NUM && strcmp(cur()->s, "01")) {
            int lvl = parse_level(); int lline = cur()->line; advance();
            if (lvl <= 1 || lvl > 49) die_at(lline, "bad level %d in a report group", lvl);
            if (cur()->kind == T_WORD && !at_word("line") && !at_word("column") && !at_word("pic") && !at_word("picture") && !at_word("source") && !at_word("value"))
                advance();                          /* a name on the line entry */
            if (!at_word("line")) die_at(lline, "expected LINE on the level %02d entry of report group '%s' (fields go on entries below a LINE)", lvl, g->name);
            if (g->nl == g->lcap) { g->lcap = g->lcap ? g->lcap * 2 : 4; g->l = realloc(g->l, g->lcap * sizeof *g->l); }
            RLine *ln = &g->l[g->nl++];
            memset(ln, 0, sizeof *ln);
            ln->line = lline;
            advance(); accept_word("number"); accept_word("is");
            if (accept_word("plus")) { if (cur()->kind != T_NUM) die_at(lline, "expected a number after LINE PLUS"); ln->plus = atoi(cur()->s); advance(); }
            else if (at_op("+")) { advance(); if (cur()->kind != T_NUM) die_at(lline, "expected a number after LINE +"); ln->plus = atoi(cur()->s); advance(); }
            else if (cur()->kind == T_NUM) {
                /* the tokenizer read "+1" as a signed literal */
                if (cur()->s[0] == '+') ln->plus = atoi(cur()->s + 1);
                else if (cur()->s[0] == '-') die_at(lline, "LINE cannot be negative");
                else ln->abs = atoi(cur()->s);
                advance();
            } else if (accept_word("next")) die_at(lline, "LINE NEXT PAGE is not implemented");
            else die_at(lline, "expected a line number after LINE");
            if (!ln->abs && !ln->plus) die_at(lline, "LINE needs a number");
            if (ln->abs && ln->abs > r->page_limit) die_at(lline, "LINE %d is past PAGE LIMIT %d", ln->abs, r->page_limit);
            while (cur()->kind != T_PERIOD) {
                Tok *t = cur();
                if (at_word("column") || at_word("pic") || at_word("picture") || at_word("source") || at_word("value"))
                    die_at(t->line, "a field clause on the LINE entry itself is not implemented; put fields on entries below it");
                die_at(t->line, "unexpected %s after LINE in report group '%s'", tok_desc(t), g->name);
            }
            expect_period();
            /* fields: deeper level numbers */
            int next_col = 1;
            while (cur()->kind == T_NUM) {
                int fl = parse_level();
                if (fl <= lvl) break;
                int fline = cur()->line; advance();
                if (ln->nf == ln->fcap) { ln->fcap = ln->fcap ? ln->fcap * 2 : 8; ln->f = realloc(ln->f, ln->fcap * sizeof *ln->f); }
                RField *fd = &ln->f[ln->nf++];
                memset(fd, 0, sizeof *fd);
                fd->line = fline;
                if (cur()->kind == T_WORD && !at_word("column") && !at_word("pic") && !at_word("picture") && !at_word("source") && !at_word("value") && !at_word("line"))
                    advance();                      /* a name on the field */
                while (cur()->kind != T_PERIOD) {
                    Tok *t = cur();
                    if (accept_word("column")) {
                        accept_word("number"); accept_word("is");
                        if (cur()->kind != T_NUM) die_at(t->line, "expected a number after COLUMN");
                        fd->column = atoi(cur()->s); advance();
                        continue;
                    }
                    if (accept_word("pic") || accept_word("picture")) {
                        if (cur()->kind != T_PIC) die_at(t->line, "expected a PICTURE character-string");
                        fd->has_pic = 1;
                        snprintf(fd->pic, sizeof fd->pic, "%s", cur()->s);
                        if (pic_analyse(fd->pic, &fd->pi) < 0) die_at(t->line, "report field: %s", fd->pi.err);
                        advance();
                        continue;
                    }
                    if (accept_word("source")) {
                        accept_word("is");
                        if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after SOURCE");
                        fd->has_source = 1;
                        snprintf(fd->source_name, sizeof fd->source_name, "%s", cur()->s); advance();
                        if (at_word("of") || at_word("in")) { advance(); snprintf(fd->source_qual, sizeof fd->source_qual, "%s", cur()->s); fd->nq = 1; advance(); }
                        if (cur()->kind == T_LP) die_at(t->line, "a subscripted SOURCE is not implemented yet");
                        continue;
                    }
                    if (accept_word("value")) {
                        accept_word("is");
                        if (cur()->kind != T_STR && cur()->kind != T_NUM) die_at(t->line, "VALUE in a report field needs a literal");
                        fd->value = cur(); advance();
                        continue;
                    }
                    if (accept_word("just") || accept_word("justified")) { accept_word("right"); fd->just = 1; continue; }
                    if (accept_word("blank")) { accept_word("when"); accept_word("zero"); accept_word("zeros"); fd->blank_zero = 1; continue; }
                    if (accept_word("sum")) die_at(t->line, "SUM is not implemented (after v1)");
                    if (accept_word("group")) die_at(t->line, "GROUP INDICATE is not implemented (after v1)");
                    if (at_word("line")) die_at(t->line, "LINE on a field entry: nested lines are not implemented");
                    die_at(t->line, "unexpected %s in a report field", tok_desc(t));
                }
                expect_period();
                if (!fd->has_pic && fd->value && fd->value->kind == T_STR) {
                    /* VALUE without PICTURE: an alphanumeric of the literal's width */
                    fd->has_pic = 1;
                    snprintf(fd->pic, sizeof fd->pic, "x(%d)", fd->value->len > 0 ? fd->value->len : 1);
                    if (pic_analyse(fd->pic, &fd->pi) < 0) die_at(fline, "report field: %s", fd->pi.err);
                }
                if (!fd->has_pic) die_at(fline, "a report field needs a PICTURE");
                if (fd->has_source == !!fd->value) die_at(fline, "a report field needs exactly one of SOURCE and VALUE");
                if (!fd->column) fd->column = next_col;
                next_col = fd->column + fd->pi.bytes;
            }
        }
        if (!g->nl) die_at(g->line, "report group '%s' has no LINE", g->name);
    }
}

/* 01 screen-name. then slot entries at deeper levels, each with LINE /
 * COLUMN / VALUE / PIC FROM|TO|USING / attributes */
static void parse_screen_section(void)
{
    while (cur()->kind == T_NUM && !strcmp(cur()->s, "01")) {
        int line = cur()->line; advance();
        if (cur()->kind != T_WORD) die_at(line, "expected a screen-name after 01");
        if (g_nscreen == g_scrcap) { g_scrcap = g_scrcap ? g_scrcap * 2 : 4; g_screens = realloc(g_screens, g_scrcap * sizeof *g_screens); }
        Screen *sc = &g_screens[g_nscreen++];
        memset(sc, 0, sizeof *sc);
        sc->line = line;
        snprintf(sc->name, sizeof sc->name, "%s", cur()->s); advance();
        if (sym_lookup_quiet(sc->name)) die_at(line, "'%s' is both a data item and a screen", sc->name);
        while (cur()->kind != T_PERIOD) {
            if (accept_word("blank")) { expect_word("screen"); sc->blank_screen = 1; continue; }
            die_at(cur()->line, "unexpected %s on screen '%s' (v1 takes BLANK SCREEN on the 01, fields below it)", tok_desc(cur()), sc->name);
        }
        expect_period();
        while (cur()->kind == T_NUM && strcmp(cur()->s, "01")) {
            int fl = parse_level(); int fline = cur()->line; advance();
            if (fl <= 1 || fl > 49) die_at(fline, "bad level %d in a screen", fl);
            if (cur()->kind == T_WORD && !at_word("blank") && !at_word("line") && !at_word("column") && !at_word("col") &&
                !at_word("value") && !at_word("pic") && !at_word("picture") && !at_word("highlight") && !at_word("underline") &&
                !at_word("auto") && !at_word("reverse-video") && !at_word("from") && !at_word("to") && !at_word("using"))
                advance();                                       /* a name on the slot */
            if (sc->nf == sc->fcap) { sc->fcap = sc->fcap ? sc->fcap * 2 : 16; sc->f = realloc(sc->f, sc->fcap * sizeof *sc->f); }
            SField *f = &sc->f[sc->nf];
            memset(f, 0, sizeof *f);
            f->srcline = fline; f->kind = -1;
            int blank_screen_entry = 0;
            while (cur()->kind != T_PERIOD) {
                Tok *t = cur();
                if (accept_word("blank")) {
                    if (accept_word("screen")) { blank_screen_entry = 1; sc->blank_screen = 1; continue; }
                    if (accept_word("line")) die_at(t->line, "BLANK LINE is not implemented");
                    accept_word("when"); if (!(accept_word("zero") || accept_word("zeros") || accept_word("zeroes"))) die_at(t->line, "expected ZERO after BLANK WHEN");
                    f->blank_zero = 1; continue;
                }
                if (accept_word("line")) {
                    accept_word("number"); accept_word("is");
                    if (at_word("plus")) die_at(t->line, "LINE PLUS in a screen is not implemented; give the line");
                    if (cur()->kind != T_NUM) die_at(t->line, "expected a number after LINE");
                    f->line = atoi(cur()->s); advance(); continue;
                }
                if (accept_word("column") || accept_word("col")) {
                    accept_word("number"); accept_word("is");
                    if (at_word("plus")) die_at(t->line, "COLUMN PLUS in a screen is not implemented; give the column");
                    if (cur()->kind != T_NUM) die_at(t->line, "expected a number after COLUMN");
                    f->col = atoi(cur()->s); advance(); continue;
                }
                if (accept_word("value")) {
                    accept_word("is");
                    if (cur()->kind != T_STR) die_at(t->line, "a screen VALUE needs a nonnumeric literal");
                    f->value = cur(); advance(); f->kind = COB_SCR_VALUE; continue;
                }
                if (accept_word("pic") || accept_word("picture")) {
                    if (cur()->kind != T_PIC) die_at(t->line, "expected a PICTURE character-string");
                    f->has_pic = 1;
                    snprintf(f->pic, sizeof f->pic, "%s", cur()->s);
                    if (pic_analyse(f->pic, &f->pi) < 0) die_at(t->line, "screen field: %s", f->pi.err);
                    advance(); continue;
                }
                if (at_word("from") || at_word("to") || at_word("using")) {
                    int kind = at_word("from") ? COB_SCR_FROM : at_word("to") ? COB_SCR_TO : COB_SCR_USING;
                    advance();
                    if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name");
                    f->item = sym_lookup(cur()->s, NULL, 0, t->line); advance();
                    if (cur()->kind == T_LP) die_at(t->line, "a subscripted screen item is not implemented");
                    if (f->item->ndims) die_at(t->line, "screen item '%s' is a table item; give it a subscript is not implemented", f->item->name);
                    if (g_sym[f->item->record].is_linkage) die_at(t->line, "a LINKAGE item cannot be a screen item yet");
                    f->kind = kind; continue;
                }
                if (accept_word("highlight")) { f->flags |= COB_SF_HIGHLIGHT; continue; }
                if (accept_word("underline")) { f->flags |= COB_SF_UNDERLINE; continue; }
                if (accept_word("auto") || accept_word("auto-skip")) { f->flags |= COB_SF_AUTO; continue; }
                if (accept_word("reverse-video")) { f->flags |= COB_SF_REVERSE; continue; }
                if (accept_word("bell") || accept_word("beep")) continue;
                if (accept_word("erase")) { accept_word("eol"); accept_word("eos"); continue; }
                if (accept_word("foreground-color") || accept_word("background-color")) { accept_word("is"); if (cur()->kind == T_NUM) advance(); continue; }
                if (accept_word("secure") || accept_word("required") || accept_word("full") || accept_word("lowlight") || accept_word("blink"))
                    die_at(t->line, "the %s clause is not implemented", t->s);
                die_at(t->line, "unexpected %s in screen '%s'", tok_desc(t), sc->name);
            }
            expect_period();
            if (blank_screen_entry && f->kind < 0 && !f->has_pic) continue;   /* just BLANK SCREEN */
            if (f->kind < 0) die_at(fline, "a screen slot needs VALUE, or PIC with FROM, TO or USING");
            if (f->kind == COB_SCR_VALUE) { if (f->has_pic) die_at(fline, "a VALUE slot takes no PICTURE"); f->width = f->value->len; }
            else { if (!f->has_pic) die_at(fline, "a FROM/TO/USING slot needs a PICTURE"); f->width = f->pi.bytes; }
            if (!f->line || !f->col) die_at(fline, "a screen slot needs LINE and COLUMN");
            sc->nf++;
        }
    }
}

static void parse_data_division(void)
{
    if (!accept_word("data")) { finish_data_division(); return; }
    expect_word("division"); expect_period();
    for (;;) {
        if (at_word("file") && is_word(peek(1), "section")) {
            advance(); advance(); expect_period();
            while (at_word("fd") || at_word("sd")) parse_fd();
            g_cur_fd = -1;
            continue;
        }
        if (at_word("working-storage")) {
            advance(); expect_word("section"); expect_period();
            while (cur()->kind == T_NUM) parse_data_item();
            continue;
        }
        if (at_word("linkage") && is_word(peek(1), "section")) {
            advance(); advance(); expect_period();
            g_in_linkage = 1;
            while (cur()->kind == T_NUM) parse_data_item();
            g_in_linkage = 0;
            continue;
        }
        if (at_word("report") && is_word(peek(1), "section")) {
            advance(); advance(); expect_period();
            while (at_word("rd")) parse_rd();
            continue;
        }
        if (at_word("screen") && is_word(peek(1), "section")) {
            advance(); advance(); expect_period();
            parse_screen_section();
            continue;
        }
        if (at_word("communication") && is_word(peek(1), "section"))
            die_at(cur()->line, "the COMMUNICATION SECTION is deliberately out");
        break;
    }
    if (!at_division() && cur()->kind != T_EOF) die_at(cur()->line, "unexpected %s in the DATA DIVISION", tok_desc(cur()));
    finish_data_division();
}

/* ====================================================================== */
/* Driver                                                                  */
/* ====================================================================== */

static void emit_unit_data(void)
{
    emit("");
    emit("\t.data");
    for (int i = 0; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines >= 0) continue;
        if (s->is_linkage) {
            emit("\t.p2align 2");
            emit("%s:\t# linkage %02d %s (%d bytes at the caller's)", s->label, s->level, s->name, s->image_size);
            emit("\t.word 0");
            continue;
        }
        emit("\t.p2align 3");
        emit("%s:\t# %02d %s (%d bytes)", s->label, s->level, s->name, s->image_size);
        emit_bytes(s->image, s->image_size);
    }
    for (int i = 0; i < g_nfile; i++) {
        File *f = &g_files[i];
        emit("\t.p2align 2");
        emit(".Lf%d_%d:\t# %s", g_unit, i, f->name);
        emit("\t.byte %d,%d,%d,0", f->org, f->access, f->optional);
        emit("\t.word 0");
        if (f->rec >= 0) emit("\t.word %s", g_sym[g_sym[f->rec].record].label); else emit("\t.word 0");
        emit("\t.word %d", f->recsize);
        if (f->status_sym) emit("\t.word %s+%d", g_sym[f->status_sym->record].label, f->status_sym->offset); else emit("\t.word 0");
        if (f->assign_lit) {
            unsigned char *z = xmalloc(f->assign_lit->len + 1);
            memcpy(z, f->assign_lit->s, f->assign_lit->len);
            emit("\t.word %s", lit_label(z, f->assign_lit->len + 1));
            free(z);
        } else emit("\t.word 0");
        if (f->assign_sym) { emit("\t.word %s+%d", g_sym[f->assign_sym->record].label, f->assign_sym->offset); emit("\t.word %d", f->assign_sym->size); }
        else { emit("\t.word 0"); emit("\t.word 0"); }
        emit("\t.word 0");
        emit("\t.word 0");
        if (f->key_sym) { emit("\t.word %d", f->key_sym->offset); emit("\t.word %d", f->key_sym->size); }
        else { emit("\t.word 0"); emit("\t.word 0"); }
        emit("\t.word 0");
        emit("\t.word %d", f->varying);
        emit("\t.word %d", f->minlen);
        if (f->dep_sym) { emit("\t.word %s+%d", g_sym[f->dep_sym->record].label, f->dep_sym->offset); emit("\t.word .Ld%d", sym_desc(f->dep_sym)); }
        else { emit("\t.word 0"); emit("\t.word 0"); }
    }
    for (int i = 0; i < g_nscreen; i++) {
        Screen *sc = &g_screens[i];
        emit("\t.p2align 2");
        emit(".Lscrf%d_%d:\t# screen %s slots", g_unit, i, sc->name);
        for (int k = 0; k < sc->nf; k++) {
            SField *f = &sc->f[k];
            emit("\t.byte %d,%d", f->kind, f->flags);
            emit("\t.short %d", f->line); emit("\t.short %d", f->col);
            emit("\t.short 0");                    /* pad: width is word-aligned in cob_scr_field */
            emit("\t.word %d", f->width);
            if (f->kind == COB_SCR_VALUE) emit("\t.word %s", lit_label((unsigned char *)f->value->s, f->value->len)); else emit("\t.word 0");
            if (f->has_pic) {
                Desc d; memset(&d, 0, sizeof d);
                switch (f->pi.category) {
                case PIC_ALPHABETIC: d.cat = COB_ALPHA; break;
                case PIC_ALPHANUMERIC: d.cat = COB_ALNUM; break;
                case PIC_ALPHANUMERIC_EDITED: d.cat = COB_ALNUM_ED; break;
                case PIC_NUMERIC: d.cat = COB_NUM; break;
                default: d.cat = COB_NUM_ED; break;
                }
                d.usage = COB_U_DISPLAY; d.digits = (unsigned char)f->pi.digits; d.scale = (signed char)f->pi.scale;
                if (f->pi.is_signed) d.flags |= COB_F_SIGNED;
                if (f->blank_zero) d.flags |= COB_F_BLANKZ;
                if (f->pi.edited) snprintf(d.picstr, sizeof d.picstr, "%s", f->pi.pat);
                d.size = f->pi.bytes;
                emit("\t.word .Ld%d", desc_add(&d));
            } else emit("\t.word 0");
            if (f->item) { emit("\t.word %s+%d", g_sym[f->item->record].label, f->item->offset); emit("\t.word .Ld%d", sym_desc(f->item)); }
            else { emit("\t.word 0"); emit("\t.word 0"); }
        }
        emit(".Lscr%d_%d:\t# screen %s", g_unit, i, sc->name);
        emit("\t.word %d", sc->nf);
        emit("\t.word %d", sc->blank_screen);
        emit("\t.word .Lscrf%d_%d", g_unit, i);
    }
    for (int i = 0; i < g_nreport; i++) {
        Report *r = &g_reports[i];
        emit("\t.p2align 2");
        emit(".Lrpt%d_%d:\t# report %s", g_unit, i, r->name);
        emit("\t.word .Lf%d_%d", g_unit, r->file);
        emit("\t.word %d", r->page_limit); emit("\t.word %d", r->heading);
        emit("\t.word %d", r->first_detail); emit("\t.word %d", r->last_detail);
        emit("\t.word 0"); emit("\t.word 0"); emit("\t.word 0");
    }
}

static void emit_rodata(void)
{
    emit("");
    emit("\t.data");
    for (int i = 0; i < g_ncnt; i++) { emit("\t.p2align 2"); emit(".Lcnt%d:", i); emit("\t.word 0"); }
    emit("");
    emit("\t.section .rodata");
    for (int i = 0; i < g_nlit; i++) {
        emit("%s:", g_lit[i].label);
        emit_bytes(g_lit[i].bytes, g_lit[i].len);
    }
    for (int i = 0; i < g_ndesc; i++) {
        Desc *d = &g_desc[i];
        if (d->picstr[0]) {
            emit(".Lpic%d:", i);
            emit_bytes((unsigned char *)d->picstr, (int)strlen(d->picstr) + 1);
        }
    }
    for (int i = 0; i < g_ndesc; i++) {
        Desc *d = &g_desc[i];
        emit("\t.p2align 2");
        emit(".Ld%d:", i);
        emit("\t.byte %d,%d,%d,%d,%d,0,0,0", d->cat, d->usage, d->digits, (unsigned char)d->scale, d->flags);
        emit("\t.word %d", d->size);
        if (d->picstr[0]) emit("\t.word .Lpic%d", i); else emit("\t.word 0");
    }
}

static void usage(void)
{
    fprintf(stderr, "s32-cobc %s -- COBOL 85 for SLOW-32\n"
        "usage: s32-cobc [-free|-fixed] [-o out.s] source.cbl\n"
        "  -fixed   reference format (columns 7/8-72); the default\n"
        "  -free    free format (GnuCOBOL -free; majesty)\n"
        "  -m       module: no main entry, every unit a subprogram\n"
        "  -I dir   where COPY looks for copybooks (repeatable)\n", VERSION);
    exit(2);
}

int main(int argc, char **argv)
{
    const char *in = NULL, *out = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-free")) g_free = 1;
        else if (!strcmp(argv[i], "-fixed")) g_free = 0;
        else if (!strcmp(argv[i], "-m")) g_module = 1;
        else if (!strcmp(argv[i], "-I") && i + 1 < argc) { if (g_nincdir < 16) g_incdirs[g_nincdir++] = argv[++i]; }
        else if (!strncmp(argv[i], "-I", 2) && argv[i][2]) { if (g_nincdir < 16) g_incdirs[g_nincdir++] = argv[i] + 2; }
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

    for (;;) {
        /* one program unit; a source file may hold several, each closed
         * by END PROGRAM */
        g_nsym = 0; g_nfile = 0; g_npara = 0; g_nreport = 0; g_nscreen = 0; g_nclass = 0; g_last_item = -1; g_cur_fd = -1; g_in_linkage = 0;
        parse_identification_division();
        parse_environment_division();
        parse_data_division();
        if (!at_word("procedure")) die_at(cur()->line, "expected PROCEDURE DIVISION, found %s", tok_desc(cur()));
        parse_procedure_division();
        emit_unit_data();
        if (cur()->kind == T_EOF) break;
        if (!g_saw_end_program) die_at(cur()->line, "unexpected %s after the program (a further program needs END PROGRAM before it)", tok_desc(cur()));
        g_unit++;
    }
    emit_rodata();
    fclose(g_out);
    return 0;
}
