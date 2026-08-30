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

/* the cob_file image (cobrt.h): the byte offset of lin_counter, which a
 * program reads as LINAGE-COUNTER; lin_eop follows it */
#define COB_FILE_LIN_COUNTER_OFF 136

/* a SORT statement's key table, emitted into .data with the unit's files */
typedef struct { int offset, desc, descending; } SortKey;
typedef struct { int id; SortKey k[16]; int nk; } SortTab;
static SortTab *g_sorttab; static int g_nsorttab, g_sorttabcap;

/* SPECIAL-NAMES CLASS name IS lit [THROUGH lit] ...: a user class is a
 * 256-entry membership table, per program unit, tested like NUMERIC */
typedef struct { char name[64]; unsigned char tab[256]; } UClass;
static UClass g_class[16];
static int g_nclass;

/* SPECIAL-NAMES SWITCH-n [IS mnemonic] [ON [STATUS] [IS] cond] [OFF ...]:
 * eight implementor switches, all off unless SET; a condition-name tests
 * one, a mnemonic names one for SET */
typedef struct { char name[64]; int sw, on; } SwitchName;   /* on: 1 ON cond, 0 OFF cond, -1 mnemonic */
static SwitchName g_switch[32];
static int g_nswitch;
static SwitchName *switch_find(const char *name)
{
    for (int i = 0; i < g_nswitch; i++) if (!strcmp(g_switch[i].name, name)) return &g_switch[i];
    return NULL;
}

/* SPECIAL-NAMES ALPHABET name IS STANDARD-1|NATIVE|...: only the native
 * (ASCII) sequence exists here; another alphabet is recorded and refused
 * where it would be used */
typedef struct { char name[64]; int native; unsigned char rank[256]; } Alphabet;   /* rank: the collating position of each character */
static Alphabet g_alphabet[16];
static int g_nalphabet;
static int g_collate = -1;                  /* PROGRAM COLLATING SEQUENCE: an alphabet index, -1 native */
static char g_collate_name[64];
/* g_lowval / g_highval (declared with fig_byte): LOW-VALUE / HIGH-VALUE under the program collating sequence */

/* I-O-CONTROL SAME RECORD AREA FOR f1 f2 ...: the files share one record
 * area, so a record read from one is the record of the others */
static int g_same[8][16], g_nsame[8], g_nsame_groups;

/* SPECIAL-NAMES SYSIN|SYSOUT|CONSOLE|SYSERR|FORMFEED IS mnemonic-name:
 * kind 1 the console for ACCEPT, 2 the console for DISPLAY, 3 a page */
typedef struct { char name[64]; int kind; } Mnemonic;
static Mnemonic g_mnemonic[16];
static int g_nmnemonic;
static int mnemonic_kind(const char *name)
{
    for (int i = 0; i < g_nmnemonic; i++) if (!strcmp(g_mnemonic[i].name, name)) return g_mnemonic[i].kind;
    return 0;
}

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

typedef struct { char *text; int line; int dbg; } SrcLine;   /* dbg: a D in column 7 */
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
        char *text = NULL; int dbg = 0;
        if (g_free) {
            text = xstrndup(p, len);
        } else {
            if (len > 6) {
                char ind = p[6];
                if (ind == '*' || ind == '/') text = NULL;         /* comment */
                else if (ind == 'D' || ind == 'd') {
                    /* a debugging line: text for COPY/REPLACE matching ("as
                     * if the D did not appear"), dropped afterwards -- there
                     * is no WITH DEBUGGING MODE here */
                    int cn = len - 7; if (cn > 65) cn = 65; if (cn < 0) cn = 0;
                    text = xstrndup(p + 7, cn); dbg = 1;
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
                    int cn = len - 7; if (cn > 65) cn = 65; if (cn < 0) cn = 0;
                    const char *c = p + 7, *ce = p + 7 + cn;
                    while (c < ce && (*c == ' ' || *c == '\t')) c++;
                    if (open) {
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
            lines[n].dbg = dbg;
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
    int dbg;        /* from a debugging line: matched by COPY REPLACING, then dropped */
} Tok;

static Tok *g_tok;
static int g_ntok, g_tcap;
static int g_tok_dbg;

static Tok *push_tok(int kind, int line, const char *s, int len)
{
    if (g_ntok == g_tcap) { g_tcap = g_tcap ? g_tcap * 2 : 1024; g_tok = realloc(g_tok, g_tcap * sizeof *g_tok); }
    Tok *t = &g_tok[g_ntok++];
    t->kind = kind; t->line = line; t->s = xstrndup(s, len); t->len = len; t->file = g_tok_file; t->dbg = g_tok_dbg;
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
        g_tok_dbg = lines[li].dbg;
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
            int signed_num = (c == '+' || c == '-') && (isdigit((unsigned char)p[1]) || (p[1] == '.' && isdigit((unsigned char)p[2]))) &&
                             (p == t || p[-1] == ' ' || p[-1] == '\t' || p[-1] == '(' || p[-1] == '=');
            int dot_num = c == '.' && isdigit((unsigned char)p[1]) &&
                          (p == t || p[-1] == ' ' || p[-1] == '\t' || p[-1] == '(' || p[-1] == '=');
            if (isdigit(c) || signed_num || dot_num) {
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
                if (p[1] == 0 || p[1] == ' ' || p[1] == '\t' || (p[1] == '*' && p[2] == '>') || (p[1] == '=' && p[2] == '=')) {   /* ".==": a period ending pseudo-text */
                    push_tok(T_PERIOD, line, ".", 1); p++; continue;
                }
                die_at(line, "a period must be followed by a space or the end of the line");
            }
            if (c == ',' && p > t && isdigit((unsigned char)p[-1]) && isdigit((unsigned char)p[1])) {
                /* a comma tight between digits: the decimal point under
                 * DECIMAL-POINT IS COMMA, settled once the whole text is in */
                push_tok(T_OP, line, ",", 1); p++; continue;
            }
            if (c == ',' || c == ';') {
                if (p[1] == 0 || p[1] == ' ' || p[1] == '\t') { p++; continue; }
                die_at(line, "'%c' is a separator only when followed by a space", c);
            }
            if (c == '(') { push_tok(T_LP, line, "(", 1); p++; continue; }
            if (c == ')') { push_tok(T_RP, line, ")", 1); p++; continue; }
            if (c == ':') { push_tok(T_COLON, line, ":", 1); p++; continue; }
            if (c == '*' && p[1] == '*') { push_tok(T_OP, line, "**", 2); p += 2; continue; }
            if (c == '=' && p[1] == '=') { push_tok(T_OP, line, "==", 2); p += 2; continue; }      /* pseudo-text delimiter */
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

static int g_copy_guard;
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

/* REPLACE ==pseudo-text== BY ==pseudo-text== ... / REPLACE OFF: from the
 * statement on, every matching token sequence of the source is replaced,
 * until the next REPLACE (the Library module's other verb, after COPY) */
static void apply_replace(void)
{
    struct { Tok *from; int fl; Tok *to; int tl; } pairs[32]; int npairs = 0;
    int sentence_start = 1;
    for (int i = 0; i < g_ntok; ) {
        Tok *t = &g_tok[i];
        if (sentence_start && t->kind == T_WORD && !strcmp(t->s, "replace")) {
            int line = t->line, j = i + 1;
            npairs = 0;
            if (j < g_ntok && g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "off")) j++;
            else for (;;) {
                int r[2][2];
                for (int side = 0; side < 2; side++) {
                    if (side == 1) { if (!(j < g_ntok && g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "by"))) die_at(line, "REPLACE: expected BY"); j++; }
                    if (!(j < g_ntok && g_tok[j].kind == T_OP && !strcmp(g_tok[j].s, "==")))
                        die_at(line, "REPLACE takes ==pseudo-text== BY ==pseudo-text==");
                    j++; r[side][0] = j;
                    while (j < g_ntok && !(g_tok[j].kind == T_OP && !strcmp(g_tok[j].s, "=="))) {
                        if (g_tok[j].kind == T_EOF) die_at(line, "REPLACE: pseudo-text not closed by ==");
                        j++;
                    }
                    r[side][1] = j; j++;
                    if (side == 0 && r[0][1] == r[0][0]) die_at(line, "REPLACE: the text to replace is empty");
                }
                if (npairs == 32) die_at(line, "REPLACE: too many pairs");
                int fl = r[0][1] - r[0][0], tl = r[1][1] - r[1][0];
                /* the operands, copied aside: the statement itself goes */
                Tok *aside = xmalloc((size_t)(fl + tl + 1) * sizeof *aside);
                memcpy(aside, &g_tok[r[0][0]], (size_t)fl * sizeof *aside);
                memcpy(aside + fl, &g_tok[r[1][0]], (size_t)tl * sizeof *aside);
                pairs[npairs].from = aside; pairs[npairs].fl = fl; pairs[npairs].to = aside + fl; pairs[npairs].tl = tl; npairs++;
                if (j < g_ntok && g_tok[j].kind == T_PERIOD) break;
            }
            if (j >= g_ntok || g_tok[j].kind != T_PERIOD) die_at(line, "REPLACE needs its period");
            memmove(&g_tok[i], &g_tok[j + 1], (size_t)(g_ntok - (j + 1)) * sizeof *g_tok);
            g_ntok -= j - i + 1;
            sentence_start = 1;
            continue;
        }
        if (npairs) {
            int hit = -1;
            for (int q = 0; q < npairs && hit < 0; q++) {
                int fl = pairs[q].fl;
                if (i + fl > g_ntok) continue;
                int same = 1;
                for (int m = 0; m < fl && same; m++) {
                    const Tok *a = &g_tok[i + m], *b = &pairs[q].from[m];
                    if (a->kind != b->kind) same = 0;
                    else if (a->kind == T_STR) same = a->len == b->len && !memcmp(a->s, b->s, (size_t)a->len);
                    else same = !strcmp(a->s, b->s);
                }
                if (same) hit = q;
            }
            if (hit >= 0) {
                int fl = pairs[hit].fl, tl = pairs[hit].tl;
                int line = g_tok[i].line; const char *file = g_tok[i].file;
                int delta = tl - fl;
                if (delta > 0) {
                    if (g_ntok + delta > g_tcap) { g_tcap = g_ntok + delta + 1024; g_tok = realloc(g_tok, g_tcap * sizeof *g_tok); }
                    memmove(&g_tok[i + tl], &g_tok[i + fl], (size_t)(g_ntok - (i + fl)) * sizeof *g_tok);
                } else if (delta < 0) memmove(&g_tok[i + tl], &g_tok[i + fl], (size_t)(g_ntok - (i + fl)) * sizeof *g_tok);
                g_ntok += delta;
                for (int m = 0; m < tl; m++) { g_tok[i + m] = pairs[hit].to[m]; g_tok[i + m].line = line; g_tok[i + m].file = file; }
                if (tl > 0) sentence_start = (g_tok[i + tl - 1].kind == T_PERIOD);
                i += tl;
                continue;
            }
        }
        sentence_start = (t->kind == T_PERIOD);
        i++;
    }
}

static void expand_copies(int depth)
{
    for (int i = 0; i < g_ntok; i++) {
        if (!(g_tok[i].kind == T_WORD && !strcmp(g_tok[i].s, "copy")) || g_tok[i].dbg) continue;   /* a COPY on a debugging line is a comment */
        int line = g_tok[i].line;
        int j = i + 1;
        if (j >= g_ntok || !(g_tok[j].kind == T_WORD || g_tok[j].kind == T_STR)) die_at(line, "COPY needs a text-name");
        char name[256]; snprintf(name, sizeof name, "%.*s", g_tok[j].len > 250 ? 250 : g_tok[j].len, g_tok[j].s);
        j++;
        char lib[256] = "";
        if (j < g_ntok && g_tok[j].kind == T_WORD && (!strcmp(g_tok[j].s, "of") || !strcmp(g_tok[j].s, "in"))) {
            j++;
            if (j >= g_ntok || !(g_tok[j].kind == T_WORD || g_tok[j].kind == T_STR)) die_at(line, "COPY ... OF needs a library-name");
            snprintf(lib, sizeof lib, "%.*s", g_tok[j].len > 250 ? 250 : g_tok[j].len, g_tok[j].s);
            j++;                                        /* the library: a subdirectory of the -I directories, else they serve */
        }
        if (j < g_ntok && g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "suppress")) j++;
        /* REPLACING {==pseudo-text== | word | literal} BY {the same} ...: the
         * operands are token ranges of this statement, matched against the
         * copied text token for token */
        struct { int f0, f1, t0, t1; } pairs[32]; int npairs = 0;
        if (j < g_ntok && g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "replacing")) {
            j++;
            for (;;) {
                int r[2][2];
                for (int side = 0; side < 2; side++) {
                    if (side == 1) { if (!(j < g_ntok && g_tok[j].kind == T_WORD && !strcmp(g_tok[j].s, "by"))) die_at(line, "COPY REPLACING: expected BY"); j++; }
                    if (j < g_ntok && g_tok[j].kind == T_OP && !strcmp(g_tok[j].s, "==")) {
                        j++; r[side][0] = j;
                        while (j < g_ntok && !(g_tok[j].kind == T_OP && !strcmp(g_tok[j].s, "=="))) {
                            if (g_tok[j].kind == T_EOF) die_at(line, "COPY REPLACING: pseudo-text not closed by ==");
                            j++;
                        }
                        r[side][1] = j; j++;
                        if (side == 0 && r[0][1] == r[0][0]) die_at(line, "COPY REPLACING: the text to replace is empty");
                    } else {
                        if (j >= g_ntok || g_tok[j].kind == T_PERIOD || g_tok[j].kind == T_EOF) die_at(line, "COPY REPLACING: expected a word, a literal or ==pseudo-text==");
                        r[side][0] = j; j++;
                        if (g_tok[j - 1].kind == T_WORD) {
                            /* an identifier: qualifiers and a subscript list belong to it */
                            while (j + 1 < g_ntok && g_tok[j].kind == T_WORD && (!strcmp(g_tok[j].s, "of") || !strcmp(g_tok[j].s, "in")) && g_tok[j + 1].kind == T_WORD) j += 2;
                            if (j < g_ntok && g_tok[j].kind == T_LP) {
                                int depth_p = 0;
                                do { if (g_tok[j].kind == T_LP) depth_p++; else if (g_tok[j].kind == T_RP) depth_p--; else if (g_tok[j].kind == T_EOF) die_at(line, "COPY REPLACING: unbalanced parentheses"); j++; } while (depth_p > 0);
                            }
                        }
                        r[side][1] = j;
                    }
                }
                if (npairs == 32) die_at(line, "COPY REPLACING: too many pairs");
                pairs[npairs].f0 = r[0][0]; pairs[npairs].f1 = r[0][1]; pairs[npairs].t0 = r[1][0]; pairs[npairs].t1 = r[1][1]; npairs++;
                if (j < g_ntok && g_tok[j].kind == T_PERIOD) break;
            }
        }
        if (j >= g_ntok || g_tok[j].kind != T_PERIOD) die_at(line, "COPY %s needs its period", name);
        if (depth > 8) die_at(line, "COPY nests deeper than 8 (%s)", name);
        if (g_copy_guard++ > 4000) die_at(line, "COPY: more than 4000 expansions -- a copybook that copies itself?");

        SrcLine *lines; int n; char found[1200];
        char qual[512]; int ok = 0;
        if (lib[0]) { snprintf(qual, sizeof qual, "%s/%s", lib, name); ok = copy_open(qual, &lines, &n, found, sizeof found); }
        if (!ok && !copy_open(name, &lines, &n, found, sizeof found))
            die_at(line, "COPY: cannot find '%s' (looked beside the source and in the -I directories, as %s, %s.cpy, %s.cbl)", name, name, name, name);

        /* tokenize the copybook into its own vector, then splice */
        Tok *save_tok = g_tok; int save_n = g_ntok, save_cap = g_tcap;
        const char *save_file = g_tok_file;
        g_tok = NULL; g_ntok = 0; g_tcap = 0; g_tok_file = xstrndup(found, (int)strlen(found));
        tokenize_lines(lines, n);
        Tok *ctok = g_tok; int cn = g_ntok;
        g_tok = save_tok; g_ntok = save_n; g_tcap = save_cap; g_tok_file = save_file;

        if (npairs) {
            /* the copied text with every matching token sequence replaced */
            int rcap = cn + 64, rn = 0;
            Tok *rtok = xmalloc((size_t)rcap * sizeof *rtok);
            for (int k = 0; k < cn; ) {
                int hit = -1;
                for (int q = 0; q < npairs && hit < 0; q++) {
                    int len = pairs[q].f1 - pairs[q].f0;
                    if (k + len > cn) continue;
                    int same = 1;
                    for (int m = 0; m < len && same; m++) {
                        const Tok *a = &ctok[k + m], *b = &g_tok[pairs[q].f0 + m];
                        if (a->kind != b->kind) same = 0;
                        else if (a->kind == T_STR) same = a->len == b->len && !memcmp(a->s, b->s, (size_t)a->len);
                        else same = !strcmp(a->s, b->s);
                    }
                    if (same) hit = q;
                }
                int add = hit < 0 ? 1 : pairs[hit].t1 - pairs[hit].t0;
                if (rn + add > rcap) { rcap = rn + add + 64; rtok = realloc(rtok, (size_t)rcap * sizeof *rtok); }
                if (hit < 0) rtok[rn++] = ctok[k++];
                else {
                    for (int m = pairs[hit].t0; m < pairs[hit].t1; m++) { rtok[rn] = g_tok[m]; rtok[rn].line = ctok[k].line; rtok[rn].file = ctok[k].file; rn++; }
                    k += pairs[hit].f1 - pairs[hit].f0;
                }
            }
            free(ctok); ctok = rtok; cn = rn;
        }

        int removed = j - i + 1;
        int newn = g_ntok - removed + cn;
        if (newn > g_tcap) { g_tcap = newn + 1024; g_tok = realloc(g_tok, g_tcap * sizeof *g_tok); }
        memmove(&g_tok[i + cn], &g_tok[j + 1], (size_t)(g_ntok - (j + 1)) * sizeof *g_tok);
        memcpy(&g_tok[i], ctok, (size_t)cn * sizeof *ctok);
        g_ntok = newn;
        free(ctok);
        i--;                                            /* rescan from the spliced text: nested COPY */
    }
}

static void tokenize(void)
{
    g_tok_file = g_file;
    tokenize_lines(g_lines, g_nlines);
    expand_copies(0);
    apply_replace();
    {
        int w = 0;
        for (int r = 0; r < g_ntok; r++) if (!g_tok[r].dbg) g_tok[w++] = g_tok[r];
        g_ntok = w;
    }
    apply_decimal_point();
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

static int g_lowval, g_highval;
static int fig_byte(const char *w)
{
    if (!strncmp(w, "space", 5)) return ' ';
    if (!strncmp(w, "zero", 4)) return '0';
    if (!strncmp(w, "high", 4)) return g_highval;            /* X'FF', or the program collating sequence's last */
    if (!strncmp(w, "quote", 5)) return '"';
    if (!strncmp(w, "low", 3)) return g_lowval;              /* X'00', or the sequence's first */
    return 0;                                                 /* NULL */
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
    int  ix_table;                  /* an index item: the table it indexes */
    int  lin_file;                  /* LINAGE-COUNTER of file lin_file (a cell in its cob_file), -1 otherwise */
    int  rep_ctr;                   /* LINE-COUNTER / PAGE-COUNTER of report rep_ctr (a cell in its cob_report), -1 otherwise */
    int  redefines;                 /* sym index, -1 */
    int  sync, just, blank_zero;
    int  sign_lead, sign_sep;        /* SIGN IS LEADING/TRAILING [SEPARATE] */
    int  ndims, dim_count[MAXDIM], dim_stride[MAXDIM];
    /* VALUE (elementary or group) */
    Tok *value_tok; int value_all, value_fig;
    /* level 88 */
    int  ncv; Tok *cv_lo[MAXCV], *cv_hi[MAXCV];
    unsigned cv_all;                 /* bit i: value i is ALL literal */
    int  fd;                        /* file index for an 01 under an FD, else -1 */
    int  is_linkage;                /* a LINKAGE SECTION record: storage is the caller's */
    int  is_global;                 /* GLOBAL (or under a GLOBAL item / a GLOBAL FD): contained programs see it */
    int  is_external;               /* EXTERNAL record (or a record of an EXTERNAL FD): storage shared by name, through a cell */
    int  is_rename;                 /* level 66: another name for a range of the record, resolved after layout */
    char rn_a[64], rn_b[64]; char rn_aq[8][64], rn_bq[8][64]; int rn_naq, rn_nbq;
    /* records */
    unsigned char *image; int image_size;
    char label[48];
    /* descriptor */
    int  desc_id;                   /* -1 until emitted */
} Sym;

static Sym *g_sym;
static int g_nsym, g_scap;

/* Program units share one symbol, file and paragraph table; a unit's own
 * entries begin at the bases.  A contained program (COBOL 85 nesting)
 * pushes the containing unit's state on g_ustack and starts its bases at
 * the current ends; on END PROGRAM the tables are cut back and the
 * containing unit resumes.  Name lookup falls through the stack to the
 * ancestors' GLOBAL items and files. */
static int g_sym_base, g_file_base, g_para_base;
static int g_unit_counter;          /* units so far in this source file, for label spaces */
typedef struct UnitSave UnitSave;
static UnitSave *g_ustack[8]; static int g_udepth;

static Sym *sym_new(void)
{
    if (g_nsym == g_scap) { g_scap = g_scap ? g_scap * 2 : 128; g_sym = realloc(g_sym, g_scap * sizeof *g_sym); }
    Sym *s = &g_sym[g_nsym++];
    memset(s, 0, sizeof *s);
    s->parent = s->child = s->sibling = s->redefines = -1;
    s->desc_id = -1; s->fd = -1; s->idx1 = -1; s->ix_table = -1; s->lin_file = -1; s->rep_ctr = -1;
    return s;
}

static int sym_idx(Sym *s) { return (int)(s - g_sym); }

/* name [OF|IN qualifier]...: the unique item that matches */
static void unit_range(int level, int *from, int *to);   /* an ancestor's symbol range */

static Sym *sym_lookup(const char *name, char **quals, int nq, int line)
{
    Sym *found = NULL; int nfound = 0;
    int from = g_sym_base, to = g_nsym, global_only = 0;
    for (int level = g_udepth; level >= 0 && !nfound; level--) {
        if (level < g_udepth) { unit_range(level, &from, &to); global_only = 1; }
        for (int i = from; i < to; i++) {
            Sym *s = &g_sym[i];
            if (s->is_filler || strcmp(s->name, name)) continue;
            if (global_only && !s->is_global) continue;
            int ok = 1, at = i;
            for (int q = 0; q < nq && ok; q++) {
                int hit = -1;
                for (int p = g_sym[at].parent; p >= 0; p = g_sym[p].parent)
                    if (!strcmp(g_sym[p].name, quals[q])) { hit = p; break; }
                if (hit < 0) ok = 0; else at = hit;
            }
            if (ok) { found = s; nfound++; }
        }
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
    for (int i = g_sym_base; i < g_nsym; i++)
        if (!g_sym[i].is_filler && !strcmp(g_sym[i].name, name)) return &g_sym[i];
    for (int level = g_udepth - 1; level >= 0; level--) {
        int from, to; unit_range(level, &from, &to);
        for (int i = from; i < to; i++)
            if (g_sym[i].is_global && !g_sym[i].is_filler && !strcmp(g_sym[i].name, name)) return &g_sym[i];
    }
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
    char status_qual[64];            /* FILE STATUS name OF group */
    char relkey_name[64];            /* RELATIVE KEY IS data-name */
    char key_qual[64];               /* RECORD KEY IS name IN group */
    int  linage;                     /* FD LINAGE: lin_lit/lin_name for LINES, FOOTING, TOP, BOTTOM */
    long lin_lit[4]; char lin_name[4][64]; Sym *lin_sym[4];
    int  lin_counter_sym;            /* the LINAGE-COUNTER item, or -1 */
    struct { char name[64]; char qual[64]; Sym *sym; int dups; } alt[16]; int nalt;   /* ALTERNATE RECORD KEY ... [WITH DUPLICATES] */
    Sym *assign_sym, *status_sym, *key_sym, *relkey_sym;
    int  use_para;                   /* DECLARATIVES: the USE section for this file, 0 none */
    int  rec;                        /* sym index of the first 01, -1 */
    int  recsize;
    int  org_given;                  /* an ORGANIZATION clause was written */
    int  varying;                    /* mode V: RECORDING MODE V, RECORD CONTAINS m TO n, VARYING, unequal 01s */
    int  minlen, maxlen;             /* from RECORD CONTAINS / VARYING; 0 = unset */
    char dep_name[64]; Sym *dep_sym; /* RECORD IS VARYING ... DEPENDING ON */
    int  unit;                       /* the program unit that declares it (its image is .Lf<unit>_<index>) */
    int  global;                     /* FD ... GLOBAL: contained programs may use it */
    int  external;                   /* FD ... EXTERNAL: one file connector for every program naming it */
} File;

static File *g_files; static int g_nfile, g_fcap;
static int g_cur_fd = -1;            /* the FD whose 01s are being parsed */

static void unit_file_range(int level, int *from, int *to);

static File *file_find(const char *name)
{
    for (int i = g_file_base; i < g_nfile; i++) if (!strcmp(g_files[i].name, name)) return &g_files[i];
    for (int level = g_udepth - 1; level >= 0; level--) {
        int from, to; unit_file_range(level, &from, &to);
        for (int i = from; i < to; i++) if (g_files[i].global && !strcmp(g_files[i].name, name)) return &g_files[i];
    }
    return NULL;
}

/* ---- reports: RD and its groups --------------------------------------- */

typedef struct {
    int column, line;
    int has_pic; char pic[PIC_MAXPAT]; PicInfo pi;
    int has_source; int source_tp;      /* token position of the SOURCE reference, parsed at GENERATE time */
    Tok *value;
    int just, blank_zero;
} RField;

typedef struct {
    int abs, plus, line;
    RField *f; int nf, fcap;
} RLine;

enum { RG_PAGE_HEADING, RG_DETAIL, RG_PAGE_FOOTING };

typedef struct {
    char name[64];
    int type, line;
    RLine *l; int nl, lcap;
} RGroup;

typedef struct {
    char name[64];
    int line, file;                  /* the FD whose REPORT IS names it */
    int page_limit, heading, first_detail, last_detail, footing;
    int lc_sym, pc_sym;              /* the synthetic LINE-COUNTER and PAGE-COUNTER items */
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
        if (!s->sign_lead && !s->sign_sep && pi->category == PIC_NUMERIC && pi->is_signed)
            for (int a = s->parent; a >= 0; a = g_sym[a].parent)          /* a group's SIGN clause reaches down */
                if (g_sym[a].sign_lead || g_sym[a].sign_sep) { s->sign_lead = g_sym[a].sign_lead; s->sign_sep = g_sym[a].sign_sep; break; }
        if (s->sign_sep) s->size++;                 /* SIGN SEPARATE: its own character */
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

    if (!((level >= 1 && level <= 49) || level == 66 || level == 77 || level == 88))
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
            int is_all = accept_word("all");
            Tok *v = cur();
            if (!(v->kind == T_STR || v->kind == T_NUM || (v->kind == T_WORD && is_figurative(v->s))))
                die_at(v->line, "expected a literal in the VALUE of '%s'", s->name);
            if (s->ncv >= MAXCV) die_at(v->line, "too many values for '%s'", s->name);
            if (is_all && v->kind != T_STR) die_at(v->line, "ALL needs a non-numeric literal");
            if (is_all) s->cv_all |= 1u << s->ncv;
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

    while (cur()->kind != T_PERIOD) {
        Tok *t = cur();
        if (t->kind != T_WORD) die_at(t->line, "unexpected %s in the description of '%s'", tok_desc(t), s->name);
        if (!strcmp(t->s, "is")) { advance(); continue; }        /* 01 X IS GLOBAL: a noise word */

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
            advance();
            if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after REDEFINES");
            if (!strcmp(cur()->s, "filler")) die_at(t->line, "REDEFINES FILLER: the redefined item needs a name (FILLER cannot be referenced)");
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
        if (!strcmp(t->s, "external")) { advance(); s->is_external = 1; continue; }
        die_at(t->line, "unexpected %s in the description of '%s'", tok_desc(t), s->name);
    }
    expect_period();

    if (level == 77 && s->occurs)
        die_at(line, "a level 77 item cannot have OCCURS");
    if ((s->sign_lead || s->sign_sep) && s->has_pic && (s->usage != U_DISPLAY || s->pi.category != PIC_NUMERIC || !s->pi.is_signed))
        die_at(line, "SIGN applies to a signed numeric DISPLAY item; '%s' is not one", s->name);
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
        if (s->is_group && s->has_pic) die_at(s->line, "'%s' is a group and cannot have a PICTURE", s->name);
        if (s->is_group && s->has_usage) {
            /* USAGE on a group is every subordinate's that does not say
             * otherwise (X3.23 5.3.x); the children follow in the table, so
             * they are finished after this with the usage in place */
            for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
                if (g_sym[c].is_cond) continue;
                if (!g_sym[c].has_usage) { g_sym[c].usage = s->usage; g_sym[c].has_usage = 1; }
                else if (g_sym[c].usage != s->usage)
                    die_at(g_sym[c].line, "USAGE of '%s' contradicts the USAGE of its group '%s'", g_sym[c].name, s->name);
            }
        }
        if (!s->is_group) sym_finish(s);
    }
    /* level 88 parents: the item they follow; a 88 under an 88 shares it */
    for (int i = g_sym_base; i < g_nsym; i++) {
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
    /* SAME RECORD AREA: the later files' first 01s redefine the first file's */
    for (int g = 0; g < g_nsame_groups; g++)
        for (int k = 1; k < g_nsame[g]; k++) {
            File *a = &g_files[g_same[g][0]], *b = &g_files[g_same[g][k]];
            if (a->rec < 0 || b->rec < 0) die_at(b->line, "SAME RECORD AREA: file '%s' has no record description", b->name);
            if (g_sym[b->rec].redefines < 0) g_sym[b->rec].redefines = a->rec;
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
        Sym *a = sym_lookup(s->rn_a, aq, naq, s->line), *b = NULL;
        if (s->rn_b[0]) b = sym_lookup(s->rn_b, bq, nbq, s->line);
        Sym *chk[2] = { a, b };
        for (int k = 0; k < 2; k++) {
            Sym *x = chk[k];
            if (!x) continue;
            if (x->record != s->record) die_at(s->line, "RENAMES '%s': '%s' is not in the same record", s->name, x->name);
            if (x->level == 1 || x->level == 66 || x->level == 77 || x->is_cond) die_at(s->line, "RENAMES '%s': '%s' is not a level 02-49 item", s->name, x->name);
            if (x->ndims) die_at(s->line, "RENAMES '%s': '%s' has OCCURS or lies in a table", s->name, x->name);
        }
        int end = b ? (int)(b->offset + b->size) : (int)(a->offset + a->size);
        if (end <= (int)a->offset) die_at(s->line, "RENAMES '%s': '%s' does not follow '%s'", s->name, b->name, a->name);
        s->offset = a->offset; s->size = end - (int)a->offset; s->ndims = 0;
        if (!b && !a->is_group) {
            s->usage = a->usage; s->has_usage = a->has_usage; s->pi = a->pi; s->has_pic = a->has_pic;
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
    }
    /* files: names, status, the record area */
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (f->rec < 0 && !f->report_name[0]) die_at(f->line, "file '%s' has no FD", f->name);
        if (f->assign_name[0]) {
            f->assign_sym = sym_lookup(f->assign_name, NULL, 0, f->line);
            if (g_sym[f->assign_sym->record].is_linkage) die_at(f->line, "ASSIGN TO '%s': a LINKAGE item cannot name a file", f->assign_name);
            if (f->assign_sym->is_group || f->assign_sym->pi.category == PIC_NUMERIC)
                die_at(f->line, "ASSIGN TO '%s': the data-name must be alphanumeric", f->assign_name);
        }
        if (f->status_name[0]) {
            char *sq[1] = { f->status_qual };
            f->status_sym = sym_lookup(f->status_name, sq, f->status_qual[0] ? 1 : 0, f->line);
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
            if (f->key_qual[0]) { char *q[1] = { f->key_qual }; k = sym_lookup(f->key_name, q, 1, f->line); nk = 1; }
            else for (int j = 0; j < g_nsym; j++)
                if (!g_sym[j].is_cond && !g_sym[j].is_filler && !strcmp(g_sym[j].name, f->key_name) &&
                    f->rec >= 0 && g_sym[j].record == g_sym[f->rec].record) { k = &g_sym[j]; nk++; }
            if (!k || f->rec < 0 || k->record != g_sym[f->rec].record) die_at(f->line, "RECORD KEY '%s' is not an item of file '%s'", f->key_name, f->name);
            if (nk > 1) die_at(f->line, "RECORD KEY '%s' is ambiguous in file '%s'", f->key_name, f->name);
            if (k->ndims) die_at(f->line, "RECORD KEY '%s' cannot be a table item", f->key_name);
            if (k->size < 1 || k->size > 255) die_at(f->line, "RECORD KEY '%s' must be 1 to 255 bytes", f->key_name);
            f->key_sym = k;
        }
        if (f->linage) {
            if (f->org != COB_ORG_LINESEQ && f->org != COB_ORG_SEQ) die_at(f->line, "FD %s: LINAGE needs a sequential file", f->name);
            f->org = COB_ORG_LINESEQ;               /* a LINAGE file is a print file: its records are lines */
            for (int w = 0; w < 4; w++)
                if (f->lin_name[w][0]) {
                    f->lin_sym[w] = sym_lookup(f->lin_name[w], NULL, 0, f->line);
                    if (!is_int_item(f->lin_sym[w])) die_at(f->line, "LINAGE: '%s' must be an integer item", f->lin_name[w]);
                }
        }
        for (int a = 0; a < f->nalt; a++) {
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
            f->alt[a].sym = k;
        }
        if (f->org == COB_ORG_RELATIVE) {
            if (f->relkey_name[0]) {
                Sym *k = sym_lookup(f->relkey_name, NULL, 0, f->line);
                if (!is_int_item(k)) die_at(f->line, "RELATIVE KEY '%s' must be an unsigned integer item", f->relkey_name);
                if (f->rec >= 0 && k->record == g_sym[f->rec].record)
                    die_at(f->line, "RELATIVE KEY '%s' must not be an item of file '%s' (the record number lives outside the record)", f->relkey_name, f->name);
                if (g_sym[k->record].is_linkage) die_at(f->line, "RELATIVE KEY '%s' cannot be a LINKAGE item", f->relkey_name);
                f->relkey_sym = k;
            } else if (f->access != 0)
                die_at(f->line, "file '%s': ACCESS RANDOM or DYNAMIC on a RELATIVE file needs a RELATIVE KEY", f->name);
            if (f->key_name[0]) die_at(f->line, "file '%s': RECORD KEY is for INDEXED files; a RELATIVE file has a RELATIVE KEY", f->name);
        } else if (f->relkey_name[0]) die_at(f->line, "file '%s': RELATIVE KEY needs ORGANIZATION RELATIVE", f->name);
        if (f->rec >= 0 && g_sym[f->rec].image_size < f->recsize) g_sym[f->rec].image_size = f->recsize;
    }
    /* images */
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0) continue;
        if (s->image_size < s->size) s->image_size = s->size;
        s->image = xmalloc(s->image_size);
        if (!s->is_linkage && !s->is_external) init_instance(s, i, 0, 1);
    }
    for (int i = g_sym_base; i < g_nsym; i++) {
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

/* The assembly is kept in memory until the end so that conditional
 * branches can be relaxed: a bcond reaches +/-4096 bytes and a big
 * program's IF or PERFORM body can be longer than that (gl008 was the
 * first).  Every instruction line the compiler writes is one 4-byte
 * instruction -- li and la are already spelled out -- so positions in
 * .text are exact, and a branch that cannot reach becomes its inverse
 * over a jal (+/-1 MB), iterated to a fixed point. */
static char **g_asm; static int g_nasm, g_asmcap;
static int new_label(void);

static void emit(const char *fmt, ...)
{
    if (g_noemit) return;
    char buf[4096];
    va_list ap;
    va_start(ap, fmt); vsnprintf(buf, sizeof buf, fmt, ap); va_end(ap);
    if (g_nasm == g_asmcap) { g_asmcap = g_asmcap ? g_asmcap * 2 : 4096; g_asm = realloc(g_asm, g_asmcap * sizeof *g_asm); }
    g_asm[g_nasm++] = xstrndup(buf, strlen(buf));
}

/* a label definition line: ".L12:", ".Lp0_3:\t# name", "ws0_1:\t# ..." */
static int line_label(const char *l, char *name, int cap)
{
    if (l[0] == '\t' || l[0] == ' ' || l[0] == '#' || !l[0]) return 0;
    const char *c = strchr(l, ':');
    if (!c || c - l >= cap) return 0;
    memcpy(name, l, (size_t)(c - l)); name[c - l] = 0;
    return 1;
}

/* a conditional branch line: "\tbeq r1, r0, .L12" -> op, operands, target */
static int line_branch(const char *l, char *op, char *ops, char *target)
{
    static const char *bops[] = { "beq", "bne", "blt", "bge", "bltu", "bgeu", NULL };
    if (l[0] != '\t' || l[1] != 'b') return 0;
    const char *sp = strchr(l, ' ');
    if (!sp || sp - l - 1 > 7) return 0;
    memcpy(op, l + 1, (size_t)(sp - l - 1)); op[sp - l - 1] = 0;
    int k; for (k = 0; bops[k] && strcmp(bops[k], op); k++) ;
    if (!bops[k]) return 0;
    const char *last = strrchr(sp, ',');
    if (!last) return 0;
    memcpy(ops, sp + 1, (size_t)(last - sp - 1)); ops[last - sp - 1] = 0;   /* "r1, r0" */
    while (*++last == ' ') ;
    snprintf(target, 64, "%s", last);
    return 1;
}

static const char *branch_inverse(const char *op)
{
    if (!strcmp(op, "beq")) return "bne";
    if (!strcmp(op, "bne")) return "beq";
    if (!strcmp(op, "blt")) return "bge";
    if (!strcmp(op, "bge")) return "blt";
    if (!strcmp(op, "bltu")) return "bgeu";
    return "bltu";
}

typedef struct { char *name; long pos; } LabelPos;

static int labelpos_cmp(const void *a, const void *b) { return strcmp(((const LabelPos *)a)->name, ((const LabelPos *)b)->name); }

static void relax_branches(void)
{
    unsigned char *islong = calloc((size_t)g_nasm, 1);
    long *pos = xmalloc((size_t)g_nasm * sizeof *pos);
    LabelPos *labels = xmalloc((size_t)g_nasm * sizeof *labels);
    char name[128], op[8], ops[64], target[64];
    for (int pass = 0; pass < 8; pass++) {
        /* positions: .text only; a label's position is the next instruction's */
        int in_text = 0, nl = 0; long at = 0;
        for (int i = 0; i < g_nasm; i++) {
            const char *l = g_asm[i];
            pos[i] = at;
            if (!strcmp(l, "\t.text")) { in_text = 1; continue; }
            if (!strcmp(l, "\t.data") || !strcmp(l, "\t.rodata") || !strncmp(l, "\t.section", 9)) { in_text = 0; continue; }
            if (!in_text) continue;
            if (line_label(l, name, sizeof name)) { labels[nl].name = xstrndup(name, strlen(name)); labels[nl].pos = at; nl++; continue; }
            if (l[0] != '\t') continue;
            if (l[1] == '.') { if (!strncmp(l, "\t.p2align", 9)) at += 12; continue; }   /* padding, over-estimated */
            at += islong[i] ? 8 : 4;
        }
        qsort(labels, (size_t)nl, sizeof *labels, labelpos_cmp);
        int changed = 0;
        for (int i = 0; i < g_nasm; i++) {
            if (islong[i] || !line_branch(g_asm[i], op, ops, target)) continue;
            LabelPos key = { target, 0 };
            LabelPos *lp = bsearch(&key, labels, (size_t)nl, sizeof *labels, labelpos_cmp);
            if (!lp) continue;                         /* a symbol elsewhere: leave it */
            long d = lp->pos - pos[i];
            if (d > 4000 || d < -4000) { islong[i] = 1; changed = 1; }
        }
        for (int k = 0; k < nl; k++) free(labels[k].name);
        if (!changed) break;
    }
    for (int i = 0; i < g_nasm; i++) {
        if (islong[i] && line_branch(g_asm[i], op, ops, target)) {
            int L = new_label();
            fprintf(g_out, "\t%s %s, .L%d\n\tjal r0, %s\n.L%d:\n", branch_inverse(op), ops, L, target, L);
        } else fprintf(g_out, "%s\n", g_asm[i]);
    }
    free(islong); free(pos); free(labels);
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
        if (s->sign_sep) d.flags |= s->sign_lead ? COB_F_SEPLEAD : COB_F_SEPTRAIL;
        else if (s->sign_lead) d.flags |= COB_F_LEAD;
        if (s->pi.edited || strchr(s->pi.pat, 'P')) snprintf(d.picstr, sizeof d.picstr, "%s", s->pi.pat);   /* P: the runtime counts the stored digits */
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

/* an unsigned integer of n DISPLAY digits (a calendar function's result) */
static int num_desc(int digits)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_NUM; d.usage = COB_U_DISPLAY; d.digits = (unsigned char)digits; d.scale = 0; d.size = digits;
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

/* a numeric literal as a CALL argument: the callee reads it through its
 * own picture, so the bytes are the plain digits (a negative one zoned
 * in its last digit), as GnuCOBOL stores literals -- not the pool's
 * sign-led image the runtime's descriptors describe */
static const char *call_num_lit_label(const NumLit *n)
{
    char img[40];
    memcpy(img, n->digits, n->ndigits);
    if (n->neg && n->ndigits) img[n->ndigits - 1] = (char)('p' + (img[n->ndigits - 1] - '0'));
    return lit_label((unsigned char *)img, n->ndigits);
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
        char line[128]; int k = snprintf(line, sizeof line, "\t.byte ");
        for (int j = i; j < n && j < i + 16; j++)
            k += snprintf(line + k, sizeof line - (size_t)k, "%s%d", j == i ? "" : ",", b[j]);
        emit("%s", line);
    }
}

/* frame: sp+0 lr, sp+4 r11, sp+8.. operand slots, then three scratch words */
#define FRAME       112
#define SLOT_COLL   96          /* the caller's collating table, when this unit sets its own */
#define SLOT_DP     100         /* the caller's decimal point, under DECIMAL-POINT IS COMMA */
#define SLOT_CUR    104         /* the caller's currency sign, under CURRENCY SIGN */
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
    int rm_odo; Sym *odo_dep; int odo_base, odo_elem;   /* a whole group over an ODO table, sent at its current length */
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
    if (!strcmp(t->s, "line-counter") || !strcmp(t->s, "page-counter")) {
        /* the report's counters: cells of its block, four-byte unsigned */
        int which = t->s[0] == 'p';
        advance();
        Report *rp = NULL;
        if (accept_word("of") || accept_word("in")) {
            if (cur()->kind != T_WORD || !report_find(cur()->s)) die_at(t->line, "%s-COUNTER OF needs a report-name", which ? "PAGE" : "LINE");
            rp = report_find(cur()->s); advance();
        } else {
            if (g_nreport != 1) die_at(t->line, g_nreport ? "%s-COUNTER is ambiguous: say %s-COUNTER OF report-name" : "%s-COUNTER: there is no RD", which ? "PAGE" : "LINE", which ? "PAGE" : "LINE");
            rp = &g_reports[0];
        }
        r->sym = &g_sym[which ? rp->pc_sym : rp->lc_sym];
        return;
    }
    if (!strcmp(t->s, "linage-counter")) {
        /* LINAGE-COUNTER [OF|IN file-name]: the cell of that file, or of the one LINAGE file */
        advance();
        File *lf = NULL;
        if (accept_word("of") || accept_word("in")) {
            if (cur()->kind != T_WORD || !file_find(cur()->s)) die_at(t->line, "LINAGE-COUNTER OF needs a file-name");
            lf = file_find(cur()->s); advance();
            if (!lf->linage) die_at(t->line, "file '%s' has no LINAGE clause", lf->name);
        } else {
            int n = 0;
            for (int i = g_file_base; i < g_nfile; i++) if (g_files[i].linage) { lf = &g_files[i]; n++; }
            if (!lf) die_at(t->line, "LINAGE-COUNTER: no file has a LINAGE clause");
            if (n > 1) die_at(t->line, "LINAGE-COUNTER is ambiguous: say LINAGE-COUNTER OF file-name");
        }
        r->sym = &g_sym[lf->lin_counter_sym];
        return;
    }
    char *name = t->s; advance();
    char *quals[64]; int nq = 0;                    /* NC207A qualifies 48 deep */
    while (at_word("of") || at_word("in")) {
        advance();
        if (cur()->kind != T_WORD) die_at(cur()->line, "expected a data-name after OF/IN");
        if (nq < 64) quals[nq++] = cur()->s; else die_at(cur()->line, "more than 64 qualifiers");
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

enum { FN_UPPER, FN_LOWER, FN_CURDATE, FN_INTDATE, FN_DATEINT, FN_DAYINT, FN_INTDAY };
/* the calendar functions (1989 addendum) take an integer and give one back;
 * the runtime renders the result as numeric DISPLAY digits in its buffer */
static int fn_is_numeric(int fn) { return fn >= FN_INTDATE; }
static const char *fn_runtime_name(int fn)
{
    switch (fn) {
    case FN_INTDATE: return "cob_fn_integer_of_date";
    case FN_DATEINT: return "cob_fn_date_of_integer";
    case FN_DAYINT:  return "cob_fn_day_of_integer";
    default:         return "cob_fn_integer_of_day";
    }
}
static int opnd_size(Opnd *o);

static void numlit_from_int(NumLit *n, long v)
{
    memset(n, 0, sizeof *n);
    char b[24]; snprintf(b, sizeof b, "%ld", v < 0 ? -v : v);
    n->neg = v < 0; n->ndigits = (int)strlen(b); memcpy(n->digits, b, n->ndigits);
}

static int has_odo(Sym *s);
static Sym *odo_table_below(Sym *s);

/* an operand that is a whole group over an OCCURS DEPENDING ON table
 * (no subscript, no reference modification) has the group's current
 * length wherever it is sent -- MOVE, STRING, UNSTRING, INSPECT, a
 * comparison, DISPLAY: it becomes (1:length) computed at run time.
 * Receivers are not operands here and keep the maximum, the 85 rule. */
static void operand_odo_length(Opnd *o)
{
    if (o->kind != O_REF || o->ref.rm || o->ref.nsub) return;
    Sym *g = o->ref.sym;
    if (!g->is_group || !has_odo(g)) return;
    Sym *tbl = odo_table_below(g);
    if (!tbl || !tbl->odo_dep_sym) return;
    for (Sym *k = tbl; k != g; k = &g_sym[k->parent])
        if (k->sibling >= 0)
            die_at(o->line, "'%s': items follow its OCCURS DEPENDING ON table (variable-location items are not implemented)", g->name);
    o->ref.rm = 1; o->ref.rm_start = 1; o->ref.rm_len = 0; o->ref.rm_l0 = -1;
    o->ref.rm_odo = 1; o->ref.odo_dep = tbl->odo_dep_sym;
    o->ref.odo_base = g->size - tbl->occurs * tbl->size; o->ref.odo_elem = tbl->size;
}

static void parse_operand_raw(Opnd *o);
static void parse_operand(Opnd *o) { parse_operand_raw(o); operand_odo_length(o); }

static void parse_operand_raw(Opnd *o)
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
        } else if (!strcmp(n->s, "integer-of-date") || !strcmp(n->s, "date-of-integer") ||
                   !strcmp(n->s, "day-of-integer") || !strcmp(n->s, "integer-of-day")) {
            int fn = !strcmp(n->s, "integer-of-date") ? FN_INTDATE : !strcmp(n->s, "date-of-integer") ? FN_DATEINT
                   : !strcmp(n->s, "day-of-integer") ? FN_DAYINT : FN_INTDAY;
            advance();
            if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION %s", n->s);
            advance();
            o->farg = xmalloc(sizeof *o->farg);
            parse_operand(o->farg);
            if (o->farg->kind == O_REF && !is_int_item(o->farg->ref.sym))
                die_at(n->line, "FUNCTION %s takes an integer; '%s' is not one", n->s, o->farg->ref.sym->name);
            if (o->farg->kind != O_REF && o->farg->kind != O_NUM)
                die_at(n->line, "FUNCTION %s takes an integer item or literal", n->s);
            if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the function argument");
            advance();
            o->kind = O_FUNC; o->fn = fn;
            o->fsize = fn == FN_DATEINT ? 8 : fn == FN_DAYINT ? 7 : 10;   /* DISPLAYed directly: yyyymmdd, yyyyddd, or ten digits, as GnuCOBOL shows them */
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
    if (!rec->is_linkage && !rec->is_external) { emit_la_off(reg, rec->label, off); return; }
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

enum { A_REF, A_LABEL, A_DESC, A_IMM, A_FUNC, A_VALUE, A_RDESC, A_RLEN, A_CONTENT };
typedef struct { int kind; const Ref *ref; const char *label; int desc; long imm; Opnd *fn; } Arg;
static Arg arg_func(Opnd *o)       { Arg a = { A_FUNC, 0, 0, 0, 0, o }; return a; }
static Arg arg_value(Opnd *o)      { Arg a = { A_VALUE, 0, 0, 0, 0, o }; return a; }
static Arg arg_content(Opnd *o)    { Arg a = { A_CONTENT, 0, 0, 0, 0, o }; return a; }   /* BY CONTENT: a copy's address */
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
static void emit_args(const Arg *a, int n);
static void emit_hot_value(Opnd *o);

static void emit_rm_start_len(const Ref *r, int slot)
{
    if (r->rm_odo) {
        /* the group's current length: base + DEPENDING ON x element */
        Opnd po; memset(&po, 0, sizeof po); po.kind = O_REF; po.ref.sym = r->odo_dep; po.ref.line = r->line;
        if (is_hot_int(r->odo_dep)) emit_hot_value(&po);
        else { Arg a[2] = { arg_ref(&po.ref), arg_desc(sym_desc(r->odo_dep)) }; emit_args(a, 2); emit_call("cob_load_int"); }
        emit("\tadd r3, r0, r1"); emit_li("r4", r->odo_base); emit_li("r5", r->odo_elem);
        emit_call("cob_odo_length");
        emit("\tstw sp+%d, r1", SLOT(slot));
        emit_li("r1", 1);
        return;
    }
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
        } else if (a[i].kind == A_CONTENT) {
            /* BY CONTENT: the callee gets a copy, from the runtime's arena,
             * released after the CALL (cob_content_pop) */
            Opnd *o = a[i].fn;
            if (o->kind == O_REF) { emit_ref_addr(&o->ref, "r3"); emit_li("r4", o->ref.sym->size); }
            else if (o->kind == O_STR) { emit_la("r3", lit_label((unsigned char *)o->tok->s, o->tok->len)); emit_li("r4", o->tok->len); }
            else { emit_la("r3", call_num_lit_label(&o->num)); emit_li("r4", o->num.ndigits); }
            emit_call("cob_content_push");
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
            else if (fn_is_numeric(f->fn)) {
                if (x->kind == O_REF && is_hot_int(x->ref.sym)) { emit_ref_addr(&x->ref, "r3"); emit_load_int(x->ref.sym, "r3", "r1"); }
                else if (x->kind == O_REF) { emit_ref_addr(&x->ref, "r3"); emit_desc_addr("r4", sym_desc(x->ref.sym)); emit_call("cob_load_int"); }
                else emit_li("r1", (long)numlit_int(&x->num));
                emit("\tadd r3, r1, r0");
                emit_call(fn_runtime_name(f->fn));
            } else {
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
        *addr = arg_func(o);
        *desc = arg_desc(fn_is_numeric(o->fn) ? num_desc(o->fsize) : str_desc(o->fsize));
        return;
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

enum { C_AND, C_OR, C_NOT, C_REL, C_CLASS, C_SWITCH };
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
        if (c->cv_all & (1u << i)) lo.kind = O_ALL;
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

/* the relational operator at the cursor, consumed; -1 when there is none */
static int parse_relop(void)
{
    Tok *t = cur();
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
    return op;
}

/* Abbreviated combined relation conditions (X3.23 6.5.3): after a
 * relation, AND/OR may be followed by just a relational operator and an
 * object, or by an object alone; the subject -- and, with the object
 * alone, the operator (NOT included when it preceded the operator) --
 * are those of the last relation.  NOT before an abbreviation is the
 * ordinary negation (parse_not); the truth is the same as the text's. */
static Opnd g_abbr_x; static int g_abbr_op = -1, g_abbr_neg;
static int g_cond_depth;

static Cond *parse_simple(void)
{
    int line = cur()->line;
    if (cur()->kind == T_WORD) {
        SwitchName *m = switch_find(cur()->s);
        if (m && m->on >= 0) {      /* a switch-status condition-name */
            advance();
            Cond *c = cond_new(C_SWITCH); c->klass = m->sw; c->neg = !m->on;
            return c;
        }
    }
    if (g_abbr_op >= 0 && (cur()->kind == T_OP || at_word("equal") || at_word("equals") || at_word("greater") || at_word("less") || at_word("is"))) {
        /* [IS] [NOT] relop object: the last relation's subject */
        accept_word("is");
        int neg = accept_word("not");
        int op = parse_relop();
        if (op < 0) die_at(line, "expected a relational operator, found %s", tok_desc(cur()));
        Opnd y = parse_cond_operand();
        return cond_rel(&g_abbr_x, op, &y, neg);
    }
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

    int op = parse_relop();
    if (op < 0) {
        if (x.kind == O_REF && x.ref.sym->is_cond) return cond_88(&x.ref, neg);
        if (g_abbr_op >= 0)             /* an object alone: the last relation's subject and operator */
            return cond_rel(&g_abbr_x, g_abbr_op, &x, g_abbr_neg ^ neg);
        if (x.kind == O_REF && !neg)
            die_at(line, "expected a relational operator after '%s'", x.ref.sym->name);
        die_at(line, "expected a relational operator, found %s", tok_desc(t));
    }
    Opnd y = parse_cond_operand();
    if (x.kind != O_REF && y.kind != O_REF && x.kind != O_EXPR && y.kind != O_EXPR)
        die_at(line, "a condition needs at least one data item");
    g_abbr_x = x; g_abbr_op = op; g_abbr_neg = neg;
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
    if (g_cond_depth++ == 0) g_abbr_op = -1;       /* a new condition: nothing to abbreviate yet */
    Cond *a = parse_and();
    while (accept_word("or")) a = cond_bin(C_OR, a, parse_and());
    g_cond_depth--;
    return a;
}

/* r1 = 0/1 for a simple condition */
static void emit_cond_value(Cond *c)
{
    if (c->kind == C_SWITCH) {
        emit_la("r3", "cob_switches");
        emit("\tldw r1, r3+%d", 4 * (c->klass - 1));
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
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
                                 is_word(peek(1), "end") || is_word(peek(1), "overflow") ||
                                 is_word(peek(1), "exception") || is_word(peek(1), "end-of-page") ||
                                 is_word(peek(1), "eop"))) return 1;
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

typedef struct { char name[64]; int id, is_section, line, section, unit; } Para;   /* section: id of the enclosing section, 0 none; unit: where it is */
static Para *g_para; static int g_npara, g_pcap;

static int g_cur_sec_id;            /* the section being parsed (or prescanned), -1 outside one */

/* a paragraph name may be repeated in different sections; an unqualified
 * reference means the one in the current section, else the only one */
static Para *para_find_in(const char *name, int section)
{
    for (int i = g_para_base; i < g_npara; i++)
        if (!strcmp(g_para[i].name, name) && (g_para[i].is_section || g_para[i].section == section)) return &g_para[i];
    return NULL;
}

static Para *para_find(const char *name)
{
    Para *found = NULL;
    for (int i = g_para_base; i < g_npara; i++) {
        if (strcmp(g_para[i].name, name)) continue;
        if (g_para[i].is_section || g_para[i].section == g_cur_sec_id) return &g_para[i];
        if (!found) found = &g_para[i];
    }
    return found;
}

static Para *para_add(const char *name, int is_section, int line)
{
    if (is_section) { for (int i = g_para_base; i < g_npara; i++) if (!strcmp(g_para[i].name, name)) die_at(line, "the procedure-name '%s' is declared twice", name); }
    else if (para_find_in(name, g_cur_sec_id)) die_at(line, "the paragraph '%s' is declared twice in the same section", name);
    if (g_npara == g_pcap) { g_pcap = g_pcap ? g_pcap * 2 : 64; g_para = realloc(g_para, g_pcap * sizeof *g_para); }
    Para *p = &g_para[g_npara];
    snprintf(p->name, sizeof p->name, "%s", name);
    p->id = g_npara + 1; p->is_section = is_section; p->line = line; p->unit = g_unit;
    p->section = is_section ? 0 : (g_cur_sec_id >= 0 ? g_cur_sec_id : 0);
    if (is_section) g_cur_sec_id = p->id;
    g_npara++;
    return p;
}

static void emit_para_label(Para *p) { emit(".Lp%d_%d:\t# %s%s", g_unit, p->id, p->name, p->is_section ? " section" : ""); }

/* prescan the Procedure Division for paragraph and section headers */
static void prescan_paragraphs(int from)
{
    int sentence_start = 1;
    g_cur_sec_id = -1;
    for (int i = from; i < g_ntok; i++) {
        Tok *t = &g_tok[i];
        if (t->kind == T_EOF) break;
        if (sentence_start && t->kind == T_NUM && !strchr(t->s, '.') && !strchr(t->s, '+') && !strchr(t->s, '-')) {
            /* a procedure-name of digits only (NC107A's paragraphs 3, 4, 5) */
            if (g_tok[i + 1].kind == T_PERIOD) para_add(t->s, 0, t->line);
            else if (is_word(&g_tok[i + 1], "section") && g_tok[i + 2].kind == T_PERIOD) para_add(t->s, 1, t->line);
        }
        if (sentence_start && t->kind == T_WORD && !is_verb(t->s) && !is_terminator(t->s)) {
            if (!strcmp(t->s, "declaratives")) { }
            else if (!strcmp(t->s, "end") && (is_word(&g_tok[i + 1], "declaratives") || is_word(&g_tok[i + 1], "program"))) { if (is_word(&g_tok[i + 1], "program")) break; }
            else if ((!strcmp(t->s, "identification") || !strcmp(t->s, "id")) && is_word(&g_tok[i + 1], "division")) break;   /* a contained program's */
            else if (g_tok[i + 1].kind == T_PERIOD) { para_add(t->s, 0, t->line); }
            else if (is_word(&g_tok[i + 1], "section") && g_tok[i + 2].kind == T_PERIOD) para_add(t->s, 1, t->line);
        }
        sentence_start = (t->kind == T_PERIOD);
    }
    g_cur_sec_id = -1;
}

/* procedure-name [OF|IN section-name] */
/* a token that may name a procedure: a word, or a number of digits only */
static int at_para_name(Tok *t)
{
    if (t->kind == T_WORD) return 1;
    return t->kind == T_NUM && !strchr(t->s, '.') && !strchr(t->s, '+') && !strchr(t->s, '-');
}

static Para *expect_para(void)
{
    Tok *t = cur();
    if (!at_para_name(t)) die_at(t->line, "expected a procedure-name, found %s", tok_desc(t));
    Para *p;
    if (is_word(peek(1), "of") || is_word(peek(1), "in")) {
        Tok *q = peek(2);
        if (q->kind != T_WORD) die_at(t->line, "expected a section-name after OF/IN");
        Para *sec = NULL;
        for (int i = g_para_base; i < g_npara; i++) if (g_para[i].is_section && !strcmp(g_para[i].name, q->s)) sec = &g_para[i];
        if (!sec) die_at(q->line, "'%s' is not a section", q->s);
        p = para_find_in(t->s, sec->id);
        if (!p || p->is_section) die_at(t->line, "'%s' is not a paragraph of section '%s'", t->s, q->s);
        advance(); advance();
    } else {
        p = para_find(t->s);
        if (!p) die_at(t->line, "'%s' is not a paragraph or section", t->s);
    }
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
        if (cur()->kind == T_WORD && mnemonic_kind(cur()->s) == 1) {
            advance();
            Arg a[2] = { arg_ref(&r), arg_desc(sym_desc(r.sym)) };
            emit_args(a, 2);
            emit_call("cob_accept_console");
            accept_word("end-accept");
            return;
        }
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
            if (cur()->kind == T_WORD && mnemonic_kind(cur()->s) == 2) { advance(); continue; }
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
                if (o.num.scale && i == o.num.ndigits - o.num.scale) txt[k++] = g_dp_comma ? ',' : '.';
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

/* the OCCURS DEPENDING ON table below a group, at any depth (85 allows one) */
static Sym *odo_table_below(Sym *s)
{
    for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
        if (g_sym[c].odo_dep[0]) return &g_sym[c];
        Sym *t = odo_table_below(&g_sym[c]);
        if (t) return t;
    }
    return NULL;
}

static void emit_move(Opnd *src, Ref *dst)
{
    Sym *d = dst->sym;
    /* a receiving group holding an OCCURS DEPENDING ON table has its
     * maximum length (the 1985 rule), which is how it is laid out */
    if (src->kind == O_REF && src->ref.sym->is_group && has_odo(src->ref.sym) && !src->ref.rm_odo && !src->ref.rm && !src->ref.nsub) {
        /* a sending group's length is its current one.  The group is laid
         * out with the table at its maximum, so however deep the table
         * sits, as long as nothing follows it: length = size - (max - d) * elem */
        Sym *g = src->ref.sym, *tbl = odo_table_below(g);
        if (!tbl || !tbl->odo_dep_sym)
            die_at(src->line, "MOVE of the group '%s': its OCCURS DEPENDING ON table's DEPENDING ON item is not resolved", g->name);
        /* the table must be the last thing in the group: items after it
         * would sit at variable locations, which this layout (the maximum)
         * does not give them */
        for (Sym *k = tbl; k != g; k = &g_sym[k->parent])
            if (k->sibling >= 0)
                die_at(src->line, "MOVE of the group '%s': items follow its OCCURS DEPENDING ON table (variable-location items are not implemented)", g->name);
        Opnd dep; memset(&dep, 0, sizeof dep); dep.kind = O_REF; dep.ref.sym = tbl->odo_dep_sym; dep.ref.line = src->line;
        Arg a[6] = { arg_ref(&src->ref), arg_ref(dst), arg_value(&dep), arg_imm(d->size),
                     arg_imm(g->size - tbl->occurs * tbl->size), arg_imm(tbl->size) };
        emit_args(a, 6);
        emit_call("cob_move_odo");
        return;
    }
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

/* CORRESPONDING (X3.23 6.4.2): items of the two groups with the same
 * name and the same qualifiers below them, neither FILLER, neither with
 * REDEFINES or OCCURS (nor subordinate to one: such a child is skipped
 * with its subtree), no condition-names.  Two groups that correspond
 * are searched further; MOVE moves a pair when at least one is
 * elementary, ADD/SUBTRACT act on a pair of elementary numeric items.
 * The operands' own subscripts and qualification carry to every pair. */
static void emit_store_receivers(Ref *rs, int *rounded, int nr, int hot, int giving, int subtract, int size_err);
static void emit_push(Opnd *o);
static Opnd ref_opnd(const Ref *r);
static int at_size_error_clause(void);
static void parse_size_error_clauses(int size_err, const char *end_word);

static int corr_eligible(Sym *c)
{
    return !c->is_filler && !c->is_cond && c->level != 66 && c->redefines < 0 && !c->occurs && !c->odo_dep[0];
}

static int corr_walk(Ref *a, Ref *b, int mode, int rounded, int size_err)
{
    int n = 0;
    for (int i = a->sym->child; i >= 0; i = g_sym[i].sibling) {
        Sym *c1 = &g_sym[i];
        if (!corr_eligible(c1)) continue;
        Sym *c2 = NULL;
        for (int j = b->sym->child; j >= 0; j = g_sym[j].sibling)
            if (corr_eligible(&g_sym[j]) && !strcmp(g_sym[j].name, c1->name)) { c2 = &g_sym[j]; break; }
        if (!c2) continue;
        Ref r1 = *a, r2 = *b; r1.sym = c1; r2.sym = c2;
        if (c1->is_group && c2->is_group) { n += corr_walk(&r1, &r2, mode, rounded, size_err); continue; }
        if (mode == 0) {
            Opnd o = ref_opnd(&r1);
            emit_move(&o, &r2); n++;
        } else {
            if (c1->is_group || c2->is_group || c1->pi.category != PIC_NUMERIC || c2->pi.category != PIC_NUMERIC) continue;
            Opnd o = ref_opnd(&r1);
            emit_push(&o);
            int rd = rounded;
            emit_store_receivers(&r2, &rd, 1, 0, 0, mode == 2, size_err);
            if (size_err) {         /* the size error of any pair is the statement's */
                emit("\tldw r1, sp+%d", SLOT_B); emit("\tldw r2, sp+%d", SLOT_A);
                emit("\tor r1, r1, r2"); emit("\tstw sp+%d, r1", SLOT_A);
            }
            n++;
        }
    }
    return n;
}

/* the two group operands of a CORRESPONDING statement */
static void parse_corr_operands(Ref *a, Ref *b, const char *between)
{
    parse_ref(a);
    if (!a->sym->is_group) die_at(a->line, "CORRESPONDING: '%s' is not a group", a->sym->name);
    if (a->rm) die_at(a->line, "CORRESPONDING: no reference modification on a group");
    expect_word(between);
    parse_ref(b);
    if (!b->sym->is_group) die_at(b->line, "CORRESPONDING: '%s' is not a group", b->sym->name);
    if (b->rm) die_at(b->line, "CORRESPONDING: no reference modification on a group");
}

static void parse_arith_corr(int mode, const char *between, const char *end_word)
{
    Ref a, b; parse_corr_operands(&a, &b, between);
    int rounded = accept_word("rounded");
    if (rounded && at_word("mode")) die_at(cur()->line, "ROUNDED MODE is COBOL 2002; plain ROUNDED is the 1985 form");
    int size_err = at_size_error_clause();
    if (size_err) emit("\tstw sp+%d, r0", SLOT_A);
    corr_walk(&a, &b, mode, rounded, size_err);
    if (size_err) { emit("\tldw r1, sp+%d", SLOT_A); emit("\tstw sp+%d, r1", SLOT_B); }
    parse_size_error_clauses(size_err, end_word);
}

static void parse_move(void)
{
    if (accept_word("corresponding") || accept_word("corr")) {
        Ref a, b; parse_corr_operands(&a, &b, "to");
        corr_walk(&a, &b, 0, 0, 0);
        return;
    }
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
    /* ON EXCEPTION after an arithmetic statement inside a CALL's clause
     * belongs to the CALL: only ON SIZE / SIZE is ours */
    if (at_word("size")) return 1;
    if (at_word("on")) return is_word(peek(1), "size");
    return at_word("not") && (is_word(peek(1), "size") || (is_word(peek(1), "on") && is_word(peek(2), "size")));
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
        if (at_word("size") || (at_word("on") && is_word(peek(1), "size"))) { accept_size_error_words(); parse_statements(); }
        emit_jump(Lend);
        emit_label(Lok);
        if (at_size_error_clause() && accept_word("not")) { accept_size_error_words(); parse_statements(); }
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
    if (accept_word("corresponding") || accept_word("corr")) { parse_arith_corr(1, "to", "end-add"); return; }
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
    if (accept_word("corresponding") || accept_word("corr")) { parse_arith_corr(2, "from", "end-subtract"); return; }
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
/* REMAINDER r: the dividend less the product of the divisor and the
 * quotient as it would be stored *before* ROUNDED -- the quotient
 * truncated to the receiver's decimals (X3.23 6.9.4), recomputed here
 * rather than read back from the receiver */
static void emit_remainder(Opnd *dividend, Ref *q, int q_rounded, Opnd *divisor, int size_err)
{
    if (!accept_word("remainder")) return;
    (void)q_rounded;
    Ref r; parse_ref(&r);
    if (r.sym->is_group || (r.sym->pi.category != PIC_NUMERIC && r.sym->pi.category != PIC_NUMERIC_EDITED))
        die_at(r.line, "REMAINDER '%s' is not numeric (or numeric-edited)", r.sym->name);
    emit_push(dividend);
    emit_push(dividend); emit_push(divisor); emit_call("cob_ndiv");
    emit_li("r3", q->sym->pi.scale); emit_call("cob_ntrunc");
    emit_push(divisor); emit_call("cob_nmul");
    emit_call("cob_nsub");
    /* ON SIZE ERROR: a quotient that overflowed leaves the remainder alone;
     * a remainder that overflows is the statement's size error too */
    int Lskip = new_label();
    if (size_err) { emit("\tldw r1, sp+%d", SLOT_B); emit("\tbne r1, r0, .L%d", Lskip); }
    emit_top_op(&r, "cob_top_store", size_err ? 2 : 0);
    emit_label(Lskip);
    emit_call("cob_drop");
}

/* is ON SIZE ERROR written after a REMAINDER phrase?  The quotient's store
 * needs to know before the phrase is parsed */
static int size_error_after_remainder(void)
{
    if (!at_word("remainder")) return at_size_error_clause();
    int save = g_tp; g_noemit++;
    advance(); Ref tmp; parse_ref(&tmp);
    int se = at_size_error_clause();
    g_noemit--; g_tp = save;
    return se;
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
            int size_err = size_error_after_remainder();
            emit_push(&b); emit_push(&a); emit_call("cob_ndiv");
            emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err);
            emit_remainder(&b, &rs[0], rd[0], &a, size_err);
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
    int size_err = size_error_after_remainder();
    emit_push(&a); emit_push(&b); emit_call("cob_ndiv");
    emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err);
    emit_remainder(&a, &rs[0], rd[0], &b, size_err);
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
        for (int i = g_sym_base; i < g_nsym; i++) if (g_sym[i].is_cond && !strcmp(g_sym[i].name, only->s)) return 1;
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

/* ---- DECLARATIVES: USE AFTER ERROR PROCEDURE --------------------------- */

static File *expect_file(void);
static void emit_file_addr(const char *reg, File *f);
static File *g_io_file;             /* the file the statement being parsed acts on, for the USE dispatch */

/* the unit's declarative sections: each USE names files or open modes.
 * After an I/O statement the compiler emits the choice: this unit's USE
 * for the file, then this unit's for the open mode, then outward through
 * the containing programs' GLOBAL ones (X3.23-1985 USE general rules). */
typedef struct { int sec, unit, global, mode; File *file; } UseEntry;
static UseEntry g_use[64]; static int g_nuse;
static int g_in_decl;

/* USE [GLOBAL] AFTER [STANDARD] {ERROR|EXCEPTION} PROCEDURE [ON] {file... | INPUT | OUTPUT | I-O | EXTEND} */
static void parse_use(void)
{
    int line = cur()->line;
    if (!g_in_decl) die_at(line, "USE belongs in a DECLARATIVES section");
    if (g_cur_sec_id < 0) die_at(line, "USE must be the first sentence of a section in DECLARATIVES");
    int global = accept_word("global");
    expect_word("after"); accept_word("standard");
    if (!accept_word("error") && !accept_word("exception")) die_at(line, "USE AFTER ... : expected ERROR or EXCEPTION PROCEDURE (the other USE forms are not implemented)");
    expect_word("procedure"); accept_word("on");
    int sec = g_cur_sec_id, any = 0;
    for (;;) {
        int mode = 0;
        if (accept_word("input")) mode = COB_OPEN_INPUT;
        else if (accept_word("output")) mode = COB_OPEN_OUTPUT;
        else if (accept_word("i-o")) mode = COB_OPEN_IO;
        else if (accept_word("extend")) mode = COB_OPEN_EXTEND;
        File *f = NULL;
        if (!mode) {
            if (!(cur()->kind == T_WORD && file_find(cur()->s))) break;
            f = expect_file();
        }
        for (int i = 0; i < g_nuse; i++)
            if (g_use[i].unit == g_unit && g_use[i].mode == mode && g_use[i].file == f)
                die_at(line, mode ? "two USE procedures for the same open mode" : "two USE procedures for file '%s'", f ? f->name : "");
        if (g_nuse == 64) die_at(line, "too many USE procedures");
        g_use[g_nuse].sec = sec; g_use[g_nuse].unit = g_unit; g_use[g_nuse].global = global; g_use[g_nuse].mode = mode; g_use[g_nuse].file = f;
        g_nuse++; any = 1;
    }
    if (!any) die_at(line, "USE AFTER ERROR PROCEDURE needs a file-name or INPUT/OUTPUT/I-O/EXTEND");
}

/* after an I/O statement, with its result in SLOT_C: if the condition is
 * not handled by the statement's own clause and a USE procedure applies,
 * perform that section (the runtime picks it: the file's, else the open
 * mode's), then continue with the next statement */
static void unit_use_range(int level, int *from, int *to);
static int unit_use_own_from(void);                     /* where this unit's own USE entries begin */

static void emit_use_dispatch(File *f, int has_clause)
{
    /* the candidates, in the order the text gives them: this unit's USE
     * for the file, its USE for the open mode, then each containing
     * program's GLOBAL ones the same way */
    UseEntry *c[64]; int nc = 0, any_mode = 0;
    for (int level = g_udepth; level >= 0; level--) {
        int from, to;
        if (level == g_udepth) { from = unit_use_own_from(); to = g_nuse; } else unit_use_range(level, &from, &to);
        for (int pass = 0; pass < 2; pass++)
            for (int i = from; i < to; i++) {
                UseEntry *u = &g_use[i];
                if (level < g_udepth && !u->global) continue;
                if (pass == 0 ? u->file != f : !u->mode) continue;
                if (u->mode) any_mode = 1;
                c[nc++] = u;
            }
    }
    /* SLOT_C after the statement: 0 fine, 1 the statement's own condition,
     * 2 an error with a FILE STATUS to record it, 3 an error nothing but a
     * USE procedure can take -- the run stops if none does */
    int Ldone = new_label();
    emit("\tldw r13, sp+%d", SLOT_C);
    emit("\tbeq r13, r0, .L%d", Ldone);
    if (has_clause) { emit_li("r2", 1); emit("\tbeq r13, r2, .L%d", Ldone); }
    if (any_mode) { emit_file_addr("r3", f); emit_call("cob_open_mode"); emit("\tadd r12, r0, r1"); }
    for (int i = 0; i < nc; i++) {
        int Lnext = new_label(), Lret = new_label();
        char lab[32]; snprintf(lab, sizeof lab, ".L%d", Lret);
        if (c[i]->mode) { emit_li("r2", c[i]->mode); emit("\tbne r12, r2, .L%d", Lnext); }
        emit_li("r3", c[i]->sec);
        emit_la("r4", lab);
        emit_call("cob_perform_push");
        emit("\tjal r0, .Lp%d_%d", c[i]->unit, c[i]->sec);
        emit_label(Lret);
        emit_jump(Ldone);
        emit_label(Lnext);
    }
    emit_li("r2", 3);
    emit("\tbne r13, r2, .L%d", Ldone);
    emit_file_addr("r3", f);
    emit_call("cob_io_unhandled");
    emit_label(Ldone);
}

/* ---- PERFORM ---------------------------------------------------------- */

static int g_ncnt;      /* TIMES counters */

typedef struct { Para *from, *thru; int inline_body; } Body;

static void emit_body(Body *b);

/* ---- SORT / RELEASE / RETURN ------------------------------------------ */

static File *expect_file(void);
static void emit_file_addr(const char *reg, File *f);
static void parse_condition_clauses(const char *w1, const char *w2, const char *end_word);

/* SORT sd {ON ASCENDING|DESCENDING KEY item...}... [WITH DUPLICATES IN ORDER]
 *   {USING file... | INPUT PROCEDURE IS para [THRU para]}
 *   {GIVING file... | OUTPUT PROCEDURE IS para [THRU para]}
 * The records live in memory for the statement's duration; the sort is
 * stable whether or not DUPLICATES IN ORDER is written. */
static int g_is_merge;    /* parse_sort is parsing MERGE: USING of two or more files, no INPUT PROCEDURE */

static void parse_sort(void)
{
    int line = cur()->line;
    const char *verb = g_is_merge ? "MERGE" : "SORT";
    File *sd = expect_file();
    if (sd->org != COB_ORG_SORT) die_at(line, "%s '%s': the file must be described by an SD (a table SORT is COBOL 2002; sort the table in a paragraph)", verb, sd->name);
    if (sd->rec < 0) die_at(line, "SD %s has no record description", sd->name);
    if (g_nsorttab == g_sorttabcap) { g_sorttabcap = g_sorttabcap ? g_sorttabcap * 2 : 4; g_sorttab = realloc(g_sorttab, g_sorttabcap * sizeof *g_sorttab); }
    SortTab *t = &g_sorttab[g_nsorttab++];
    memset(t, 0, sizeof *t); t->id = new_label();
    while (at_word("on") || at_word("ascending") || at_word("descending")) {
        accept_word("on");
        int descending = 0;
        if (accept_word("descending")) descending = 1;
        else if (!accept_word("ascending")) die_at(cur()->line, "expected ASCENDING or DESCENDING in SORT");
        accept_word("key");
        int any = 0;
        while (cur()->kind == T_WORD && !at_word("on") && !at_word("ascending") && !at_word("descending") &&
               !at_word("with") && !at_word("collating") && !at_word("using") && !at_word("input") &&
               !at_word("giving") && !at_word("output")) {
            Ref k; parse_ref(&k);
            if (k.sym->record != g_sym[sd->rec].record) die_at(k.line, "SORT key '%s' is not an item of the SD %s", k.sym->name, sd->name);
            if (k.nsub || k.rm) die_at(k.line, "a SORT key is a plain data item of the SD record");
            if (t->nk == 16) die_at(k.line, "too many SORT keys (16)");
            t->k[t->nk].offset = k.sym->offset; t->k[t->nk].desc = sym_desc(k.sym); t->k[t->nk].descending = descending; t->nk++;
            any = 1;
        }
        if (!any) die_at(cur()->line, "expected a key data-name after KEY");
    }
    if (!t->nk) die_at(line, "SORT needs at least one KEY");
    int dups = 0;
    if (accept_word("with")) { expect_word("duplicates"); accept_word("in"); accept_word("order"); dups = 1; }
    if (at_word("collating")) die_at(cur()->line, "SORT ... COLLATING SEQUENCE is not implemented (the sequence is ASCII)");
    char tab[32]; snprintf(tab, sizeof tab, ".Lsk%d_%d", g_unit, t->id);
    emit_file_addr("r3", sd); emit_la("r4", tab); emit_li("r5", t->nk); emit_li("r6", dups);
    emit_call("cob_sort_begin");
    if (accept_word("using")) {
        int n = 0;
        while (cur()->kind == T_WORD && !at_word("giving") && !at_word("output")) {
            File *in = expect_file();
            if (in->org == COB_ORG_SORT) die_at(line, "%s USING names a sort file", verb);
            emit_file_addr("r3", sd); emit_file_addr("r4", in); emit_call("cob_sort_using"); n++;
        }
        if (!n) die_at(cur()->line, "expected a file-name after USING");
        if (g_is_merge && n < 2) die_at(line, "MERGE USING needs at least two files");
    } else if (g_is_merge) die_at(cur()->line, "MERGE needs USING");
    else if (accept_word("input")) {
        expect_word("procedure"); accept_word("is");
        Body b; memset(&b, 0, sizeof b);
        b.from = expect_para();
        if (accept_word("thru") || accept_word("through")) b.thru = expect_para();
        emit_body(&b);
    } else die_at(cur()->line, "SORT needs USING or INPUT PROCEDURE");
    emit_file_addr("r3", sd); emit_call("cob_sort_perform");
    if (accept_word("giving")) {
        int n = 0;
        while (cur()->kind == T_WORD && file_find(cur()->s)) {
            File *out = expect_file();
            if (out->org == COB_ORG_SORT) die_at(line, "SORT GIVING names a sort file");
            emit_file_addr("r3", sd); emit_file_addr("r4", out); emit_call("cob_sort_giving"); n++;
        }
        if (!n) die_at(cur()->line, "expected a file-name after GIVING");
    } else if (accept_word("output")) {
        expect_word("procedure"); accept_word("is");
        Body b; memset(&b, 0, sizeof b);
        b.from = expect_para();
        if (accept_word("thru") || accept_word("through")) b.thru = expect_para();
        emit_body(&b);
    } else die_at(cur()->line, "SORT needs GIVING or OUTPUT PROCEDURE");
    emit_file_addr("r3", sd); emit_call("cob_sort_end");
}

/* RELEASE record [FROM x] */
static void parse_release(void)
{
    Ref rec; parse_ref(&rec);
    File *f = file_of_record(rec.sym, rec.line);
    if (f->org != COB_ORG_SORT) die_at(rec.line, "RELEASE '%s': the record must belong to an SD", rec.sym->name);
    if (accept_word("from")) { Opnd src; parse_operand(&src); emit_move(&src, &rec); }
    emit_file_addr("r3", f);
    emit_call("cob_release");
}

/* RETURN sd [RECORD] [INTO x] AT END ... [NOT AT END ...] [END-RETURN] */
static void parse_return(void)
{
    File *f = expect_file();
    if (f->org != COB_ORG_SORT) die_at(cur()->line, "RETURN '%s': the file must be an SD", f->name);
    accept_word("record");
    Ref into; int has_into = 0;
    if (accept_word("into")) { parse_ref(&into); has_into = 1; }
    emit_file_addr("r3", f);
    emit_call("cob_return");
    emit("\tstw sp+%d, r1", SLOT_C);
    g_io_file = NULL;                   /* an SD has no USE procedure */
    if (has_into) {
        int Lskip = new_label();
        emit("\tbne r1, r0, .L%d", Lskip);
        Opnd src; memset(&src, 0, sizeof src); src.kind = O_REF; src.line = into.line;
        src.ref.sym = &g_sym[f->rec]; src.ref.line = into.line;
        emit_move(&src, &into);
        emit_label(Lskip);
    }
    parse_condition_clauses("at", "end", "end-return");
}

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
    /* an inner item goes back to its FROM when its condition is true and
     * the outer one is augmented (6.20.4), so it reads FROM at the end */
    if (level > 0) emit_move(&x->from, &x->var);
}

/* VARYING ... AFTER ... WITH TEST AFTER (X3.23 6.20.4, the figure for
 * two identifiers): every item takes its FROM once; after each execution
 * of the body the innermost condition is tested -- false: its item is
 * augmented and the body runs again; true: the next outer condition is
 * tested -- false: every inner item goes back to its FROM, the outer is
 * augmented and the body runs again; true: outward again, the first
 * condition's truth ending the statement.  The items keep the values at
 * which their conditions came true. */
static void emit_varying_test_after(Vary *v, int nv, Body *body)
{
    for (int k = 0; k < nv; k++) emit_move(&v[k].from, &v[k].var);
    int Ltop = new_label();
    emit_label(Ltop);
    emit_body(body);
    for (int k = nv - 1; k >= 0; k--) {
        int Ldone = new_label();
        cond_jump_true(v[k].until, Ldone);
        for (int j = k + 1; j < nv; j++) emit_move(&v[j].from, &v[j].var);
        emit_add_to_ref(&v[k].by, &v[k].var);
        emit_jump(Ltop);
        emit_label(Ldone);
    }
}

/* is the operand at the cursor followed by TIMES?  (a data-name may carry
 * OF/IN qualifiers and a subscript) */
static int times_follows(void)
{
    int j = g_tp;
    if (g_tok[j].kind == T_NUM) return is_word(&g_tok[j + 1], "times");
    if (g_tok[j].kind != T_WORD) return 0;
    j++;
    while (is_word(&g_tok[j], "of") || is_word(&g_tok[j], "in")) j += 2;
    if (g_tok[j].kind == T_LP) {
        int depth = 0;
        do { if (g_tok[j].kind == T_LP) depth++; else if (g_tok[j].kind == T_RP) depth--; else if (g_tok[j].kind == T_EOF) return 0; j++; } while (depth > 0);
    }
    return is_word(&g_tok[j], "times");
}

static void parse_perform(void)
{
    int line = cur()->line;
    Body body; memset(&body, 0, sizeof body);
    if (at_para_name(cur()) && para_find(cur()->s)) {
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
        Vary v[8]; int nv = 0;                 /* the text sets no limit; NC233A/NC243A nest four */
        for (;;) {
            if (nv >= 8) die_at(cur()->line, "more than eight VARYING/AFTER levels");
            parse_ref(&v[nv].var);
            if (!is_numeric_sym(v[nv].var.sym)) die_at(v[nv].var.line, "the VARYING item must be numeric");
            expect_word("from"); parse_operand(&v[nv].from); check_numeric_opnd(&v[nv].from);
            expect_word("by"); parse_operand(&v[nv].by); check_numeric_opnd(&v[nv].by);
            expect_word("until"); v[nv].until = parse_cond();
            nv++;
            if (!accept_word("after")) break;
        }
        if (test_after && nv > 1) emit_varying_test_after(v, nv, &body);
        else emit_varying(v, nv, 0, &body, test_after);
    } else if (at_operand() && times_follows()) {
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
    while (at_para_name(cur()) && !at_word("depending") && !(cur()->kind == T_WORD && (is_verb(cur()->s) || is_terminator(cur()->s))) && para_find(cur()->s)) {
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
    if (cur()->kind == T_WORD && switch_find(cur()->s) && switch_find(cur()->s)->on < 0) {
        /* SET {mnemonic-name ... TO ON | OFF}... (NC174A: SET SW-1 TO ON SW-2 TO OFF) */
        while (cur()->kind == T_WORD && switch_find(cur()->s) && switch_find(cur()->s)->on < 0) {
            int sws[8], ns = 0;
            while (cur()->kind == T_WORD && switch_find(cur()->s) && switch_find(cur()->s)->on < 0) {
                if (ns < 8) sws[ns++] = switch_find(cur()->s)->sw;
                advance();
            }
            expect_word("to");
            int v = 0;
            if (accept_word("on")) v = 1; else if (accept_word("off")) v = 0;
            else die_at(cur()->line, "SET switch: expected ON or OFF");
            emit_la("r3", "cob_switches"); emit_li("r1", v);
            for (int i = 0; i < ns; i++) emit("\tstw r3+%d, r1", 4 * (sws[i] - 1));
        }
        return;
    }
    while (at_operand()) { if (nr >= MAXOPS) die_at(cur()->line, "too many items in SET"); parse_ref(&rs[nr++]); }
    if (!nr) die_at(cur()->line, "SET needs an item");
    if (accept_word("to")) {
        if (accept_word("true")) {
            for (int i = 0; i < nr; i++) {
                Sym *c = rs[i].sym;
                if (!c->is_cond) die_at(rs[i].line, "'%s' is not a condition-name", c->name);
                Opnd v = lit_opnd(c->cv_lo[0]);
                if (c->cv_all & 1u) v.kind = O_ALL;
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
    char lab[32]; snprintf(lab, sizeof lab, ".Lf%s%d_%d", f->external ? "x" : "", f->unit, (int)(f - g_files));
    emit_la(reg, lab);
    if (f->external) emit("\tldw %s, %s+0", reg, reg);      /* the shared connector, from cob_ext_file_enter */
}

static File *expect_file(void)
{
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a file-name, found %s", tok_desc(t));
    File *f = file_find(t->s);
    if (!f) die_at(t->line, "'%s' is not a file (no SELECT)", t->s);
    advance();
    return f;
}

static void parse_open(void)
{
    int n = 0;
    for (;;) {
        int mode;
        if (cur()->kind == T_WORD && file_find(cur()->s) && file_find(cur()->s)->org == COB_ORG_SORT)
            die_at(cur()->line, "'%s' is a sort file (SD); SORT opens it", cur()->s);
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
            emit("\tstw sp+%d, r1", SLOT_C); emit_use_dispatch(f, 0);
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
        int lock = 0;
        accept_word("with");
        if (accept_word("no")) accept_word("rewind");
        if (accept_word("lock")) lock = 1;
        if (lock) { emit_file_addr("r3", f); emit_call("cob_close_lock"); emit("\tstw sp+%d, r1", SLOT_C); emit_use_dispatch(f, 0); n++; continue; }
        if (accept_word("reel") || accept_word("unit")) {
            /* closes a reel, not the file; a disk file has one reel, so the
             * runtime only reports 07 (successful, no reel) */
            if (accept_word("for")) accept_word("removal");
            if (accept_word("with")) { accept_word("no"); accept_word("rewind"); }
            emit_file_addr("r3", f); emit_call("cob_close_reel");
            emit("\tstw sp+%d, r1", SLOT_C); emit_use_dispatch(f, 0);
            n++; continue;
        }
        emit_file_addr("r3", f); emit_call("cob_close");
        emit("\tstw sp+%d, r1", SLOT_C); emit_use_dispatch(f, 0);
        n++;
    }
    if (!n) die_at(cur()->line, "CLOSE needs a file-name");
}

/* [NOT] INVALID KEY / [NOT] AT END after a keyed verb, on the result in
 * SLOT_C: 0 done, 1 the condition, 2 an error already reported */
static void emit_use_dispatch(File *f, int has_clause);

static void parse_condition_clauses(const char *w1, const char *w2, const char *end_word)
{
    int Lend = new_label();
    int has_clause = at_word(w1) || at_word(w2);
    if (g_io_file) emit_use_dispatch(g_io_file, has_clause);
    if (at_word(w1) || at_word(w2)) {
        /* AT END / INVALID KEY: AT and KEY may be omitted */
        if (accept_word(w1)) accept_word(w2); else advance();
        int Lnot = new_label();
        emit("\tldw r1, sp+%d", SLOT_C);
        emit_li("r2", 1);
        emit("\tbne r1, r2, .L%d", Lnot);
        parse_statements();
        emit_jump(Lend);
        emit_label(Lnot);
    }
    if (at_word("not") && (is_word(peek(1), w1) || is_word(peek(1), w2))) {
        advance();
        if (accept_word(w1)) accept_word(w2); else advance();
        emit("\tldw r1, sp+%d", SLOT_C);
        emit("\tbne r1, r0, .L%d", Lend);
        parse_statements();
    }
    emit_label(Lend);
    accept_word(end_word);
}

/* which key of an indexed file a data item names: 0 the RECORD KEY, i the
 * i-th ALTERNATE, -1 none.  An item that begins where a key begins and is
 * no longer is a leading part of it (START on a partial key): *len is
 * then the item's size. */
static int file_key_index(File *f, Sym *s, int *len)
{
    *len = 0;
    if (s == f->key_sym) return 0;
    for (int a = 0; a < f->nalt; a++) if (s == f->alt[a].sym) return a + 1;
    if (f->rec < 0 || s->record != g_sym[f->rec].record || s->ndims) return -1;
    if (f->key_sym && s->offset == f->key_sym->offset && s->size <= f->key_sym->size) { *len = s->size; return 0; }
    for (int a = 0; a < f->nalt; a++)
        if (s->offset == f->alt[a].sym->offset && s->size <= f->alt[a].sym->size) { *len = s->size; return a + 1; }
    return -1;
}

static void parse_read(void)
{
    File *f = expect_file();
    if (f->org == COB_ORG_SORT) die_at(cur()->line, "READ of the sort file '%s': use RETURN inside the OUTPUT PROCEDURE", f->name);
    int has_next = accept_word("next"); accept_word("record");
    Ref into; int has_into = 0;
    if (accept_word("into")) { parse_ref(&into); has_into = 1; }
    int keyed = 0, ki = 0;
    if (accept_word("key")) {
        accept_word("is");
        Ref k; parse_ref(&k);
        if (f->org == COB_ORG_RELATIVE) die_at(k.line, "READ ... KEY IS is for INDEXED files; a RELATIVE file reads the record its RELATIVE KEY names");
        if (f->org != COB_ORG_INDEXED) die_at(k.line, "READ ... KEY needs an INDEXED file");
        int klen; ki = file_key_index(f, k.sym, &klen);
        if (ki < 0 || klen) die_at(k.line, "READ ... KEY IS '%s': not the RECORD KEY or an ALTERNATE RECORD KEY of '%s'", k.sym->name, f->name);
        keyed = 1;
    }
    if (f->org == COB_ORG_INDEXED) {
        if (has_next && keyed) die_at(cur()->line, "READ NEXT cannot name a KEY");
        if (!has_next && !keyed && f->access != 0) keyed = 1;         /* ACCESS RANDOM or DYNAMIC: a READ without NEXT is by the prime key */
        if (has_next && f->access == 1) die_at(cur()->line, "READ NEXT needs ACCESS SEQUENTIAL or DYNAMIC");
        if (keyed && f->access == 0) die_at(cur()->line, "READ ... KEY needs ACCESS RANDOM or DYNAMIC");
    } else if (f->org == COB_ORG_RELATIVE) {
        /* random or dynamic access: a READ without NEXT is by the RELATIVE KEY */
        if (has_next && f->access == 1) die_at(cur()->line, "READ NEXT needs ACCESS SEQUENTIAL or DYNAMIC");
        if (!has_next && f->access != 0) keyed = 1;
    } else if (keyed) die_at(cur()->line, "READ ... KEY needs an INDEXED file");

    g_io_file = f;
    emit_file_addr("r3", f); emit_li("r4", ki);
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
    if (f->org == COB_ORG_SORT) die_at(rec.line, "WRITE to the sort file '%s': use RELEASE inside the INPUT PROCEDURE", f->name);
    if (accept_word("from")) {
        Opnd src; parse_operand(&src);
        emit_move(&src, &rec);
    }
    int before = 0, after = 0, after_kw = 0; Opnd n; int dyn = 0;
    if (at_word("before") || at_word("after")) {
        after_kw = accept_word("after"); if (!after_kw) accept_word("before");
        accept_word("advancing");
        if (accept_word("page") || (cur()->kind == T_WORD && mnemonic_kind(cur()->s) == 3 && (advance(), 1))) {
            /* a form feed before (AFTER PAGE) or after (BEFORE PAGE) the record */
            if (after_kw) before = -1; else after = -1;
            accept_word("line"); accept_word("lines");
            goto advancing_done;
        }
        parse_operand(&n);
        if (n.kind == O_NUM) { long v = (long)numlit_int(&n.num); if (f->linage) { if (after_kw) before = (int)v; else after = (int)v; } else if (after_kw) before = (int)v - 1; else after = (int)v - 1; }
        else if (n.kind == O_REF && is_int_item(n.ref.sym)) dyn = 1;
        else die_at(n.line, "ADVANCING needs an integer");
        accept_word("line"); accept_word("lines");
    }
advancing_done:;
    /* a file written WITH ADVANCING and no ORGANIZATION clause is a print
     * file: its records are lines (GnuCOBOL's "line advancing" file) */
    if ((before || after || dyn) && f->org == COB_ORG_SEQ && !f->org_given && !f->varying) f->org = COB_ORG_LINESEQ;
    /* (a LINAGE file took the line counts themselves above, not n-1: AFTER n
     * in r4, BEFORE n in r5, -1 for PAGE, 0/0 for no ADVANCING) */
    int keyed_org = f->org == COB_ORG_INDEXED || f->org == COB_ORG_RELATIVE;
    if (keyed_org && (before || after || dyn)) die_at(rec.line, "ADVANCING is not valid on an %s file", f->org == COB_ORG_INDEXED ? "INDEXED" : "RELATIVE");
    if (!keyed_org && at_word("invalid")) die_at(cur()->line, "INVALID KEY needs an INDEXED or RELATIVE file");
    if (dyn) {
        if (is_hot_int(n.ref.sym)) emit_hot_value(&n);
        else { Arg a[2] = { arg_ref(&n.ref), arg_desc(sym_desc(n.ref.sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
        if (!f->linage) emit("\taddi r1, r1, -1");
        emit("\tstw sp+%d, r1", SLOT_C);
        emit_file_addr("r3", f);
        if (after_kw) { emit("\tldw r4, sp+%d", SLOT_C); emit_li("r5", 0); }
        else { emit_li("r4", 0); emit("\tldw r5, sp+%d", SLOT_C); }
    } else {
        emit_file_addr("r3", f); emit_li("r4", before); emit_li("r5", after);
    }
    emit_li("r6", rec.sym->size);          /* the 01 named: a mode-V record's length */
    emit_call("cob_write");
    emit("\tstw sp+%d, r1", SLOT_C);
    g_io_file = f;
    if (keyed_org) parse_condition_clauses("invalid", "key", "end-write");
    else if (f->linage) {
        emit_use_dispatch(f, 0);
        /* [NOT] [AT] END-OF-PAGE (EOP): the runtime's verdict on this WRITE */
        for (int j = g_tp; j < g_ntok && g_tok[j].kind != T_PERIOD && !is_word(&g_tok[j], "end-write"); j++)
            if (is_word(&g_tok[j], "eop")) { free(g_tok[j].s); g_tok[j].s = xstrndup("end-of-page", 11); }
        if (at_word("at") || at_word("end-of-page") || (at_word("not") && (is_word(peek(1), "at") || is_word(peek(1), "end-of-page")))) {
            emit_file_addr("r3", f);
            emit("\tldw r1, r3+%d", COB_FILE_LIN_COUNTER_OFF + 4);    /* lin_eop */
            emit("\tstw sp+%d, r1", SLOT_C);
            g_io_file = NULL;
            parse_condition_clauses("at", "end-of-page", "end-write");
        } else accept_word("end-write");
    }
    else { emit_use_dispatch(f, 0); accept_word("end-write"); }
}

/* REWRITE record [FROM x] [INVALID KEY ...] */
static void parse_rewrite(void)
{
    Ref rec; parse_ref(&rec);
    File *f = file_of_record(rec.sym, rec.line);
    if (f->org == COB_ORG_LINESEQ) die_at(rec.line, "REWRITE is not valid on a LINE SEQUENTIAL file");
    if (accept_word("from")) { Opnd src; parse_operand(&src); emit_move(&src, &rec); }
    emit_file_addr("r3", f); emit_li("r4", rec.sym->size);
    emit_call("cob_rewrite");
    emit("\tstw sp+%d, r1", SLOT_C);
    g_io_file = f;
    if (f->org == COB_ORG_INDEXED || f->org == COB_ORG_RELATIVE) parse_condition_clauses("invalid", "key", "end-rewrite");
    else { if (at_word("invalid")) die_at(cur()->line, "INVALID KEY needs an INDEXED or RELATIVE file"); emit_use_dispatch(f, 0); accept_word("end-rewrite"); }
}

/* DELETE file [RECORD] [INVALID KEY ...] */
static void parse_delete(void)
{
    File *f = expect_file();
    accept_word("record");
    if (f->org != COB_ORG_INDEXED && f->org != COB_ORG_RELATIVE) die_at(cur()->line, "DELETE needs an INDEXED or RELATIVE file");
    emit_file_addr("r3", f);
    emit_call("cob_delete");
    emit("\tstw sp+%d, r1", SLOT_C);
    g_io_file = f;
    parse_condition_clauses("invalid", "key", "end-delete");
}

/* START file [KEY IS relation key] [INVALID KEY ...] */
static void parse_start(void)
{
    File *f = expect_file();
    if (f->org != COB_ORG_INDEXED && f->org != COB_ORG_RELATIVE) die_at(cur()->line, "START needs an INDEXED or RELATIVE file");
    if (f->access == 1) die_at(cur()->line, "START needs ACCESS SEQUENTIAL or DYNAMIC");
    int op = 0;                     /* = */
    int ki = 0, klen = 0;           /* the key: prime, or an alternate; a leading part's length */
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
        if (f->org == COB_ORG_RELATIVE) { if (k.sym != f->relkey_sym) die_at(k.line, "START ... KEY IS '%s': a RELATIVE file starts on its RELATIVE KEY", k.sym->name); }
        else {
            ki = file_key_index(f, k.sym, &klen);
            if (ki < 0) die_at(k.line, "START ... KEY IS '%s': not a key of '%s', nor an item that begins where one begins", k.sym->name, f->name);
        }
    }
    emit_file_addr("r3", f);
    emit_li("r4", op); emit_li("r5", ki); emit_li("r6", klen);
    emit_call("cob_start");
    emit("\tstw sp+%d, r1", SLOT_C);
    g_io_file = f;
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

/* UNSTRING src [DELIMITED BY [ALL] d [OR [ALL] d]...] INTO {r [DELIMITER IN
 * r] [COUNT IN r]}... [WITH POINTER p] [TALLYING IN t] [[NOT] ON OVERFLOW]
 * [END-UNSTRING]; the runtime does the scanning (cob_unstr_*) */
static void parse_unstring(void)
{
    int line = cur()->line;
    Opnd src; parse_operand(&src);
    if (src.kind != O_REF) die_at(src.line, "UNSTRING needs a data item to take apart");
    if (!src.ref.rm && !src.ref.sym->is_group && src.ref.sym->pi.category == PIC_NUMERIC && src.ref.sym->usage != U_DISPLAY)
        die_at(src.line, "UNSTRING: '%s' is not a DISPLAY item", src.ref.sym->name);
    Opnd delims[16]; int dall[16]; int nd = 0;
    if (accept_word("delimited")) {
        accept_word("by");
        for (;;) {
            if (nd == 16) die_at(cur()->line, "UNSTRING: more than 16 delimiters");
            dall[nd] = accept_word("all");
            parse_operand(&delims[nd]);
            if (delims[nd].kind != O_STR && delims[nd].kind != O_REF && delims[nd].kind != O_FIG)
                die_at(delims[nd].line, "DELIMITED BY needs a literal or an item");
            nd++;
            if (!accept_word("or")) break;
        }
    }
    expect_word("into");
    Ref rcv[MAXOPS], dlm[MAXOPS], cnt[MAXOPS]; int has_d[MAXOPS], has_c[MAXOPS], n = 0;
    while (at_operand() && cur()->kind == T_WORD && !at_word("with") && !at_word("pointer") && !at_word("tallying") && !at_word("on") && !at_word("overflow") && !at_word("not") && !at_word("end-unstring")) {
        if (n >= MAXOPS) die_at(cur()->line, "too many UNSTRING receivers");
        parse_ref(&rcv[n]);
        if (rcv[n].sym->is_cond) die_at(rcv[n].line, "'%s' is a condition-name", rcv[n].sym->name);
        has_d[n] = has_c[n] = 0;
        for (;;) {
            if (accept_word("delimiter")) { accept_word("in"); parse_ref(&dlm[n]); has_d[n] = 1; continue; }
            if (accept_word("count")) { accept_word("in"); parse_ref(&cnt[n]); has_c[n] = 1; if (!is_int_item(cnt[n].sym)) die_at(cnt[n].line, "COUNT IN needs an integer item"); continue; }
            break;
        }
        if (has_d[n] && !nd) die_at(rcv[n].line, "DELIMITER IN without DELIMITED BY");
        if (has_c[n] && !nd) die_at(rcv[n].line, "COUNT IN without DELIMITED BY");
        n++;
    }
    if (!n) die_at(line, "UNSTRING needs a receiver after INTO");
    Ref ptr; int has_ptr = 0;
    if (accept_word("with")) { expect_word("pointer"); parse_ref(&ptr); has_ptr = 1; }
    else if (accept_word("pointer")) { parse_ref(&ptr); has_ptr = 1; }
    if (has_ptr && !is_int_item(ptr.sym)) die_at(ptr.line, "the POINTER must be an integer item");
    Ref tly; int has_tly = 0;
    if (accept_word("tallying")) { accept_word("in"); parse_ref(&tly); has_tly = 1; if (!is_int_item(tly.sym)) die_at(tly.line, "TALLYING IN needs an integer item"); }

    /* begin: the source, its length, the pointer */
    if (has_ptr) {
        Arg a[2] = { arg_ref(&ptr), arg_desc(sym_desc(ptr.sym)) }; emit_args(a, 2); emit_call("cob_load_int");
        emit("\tstw sp+%d, r1", SLOT_C);
    }
    { Arg a[2], dd; opnd_args(&src, &a[0], &dd, 0, 0); a[1] = arg_len(&src); emit_args(a, 2); }
    if (has_ptr) emit("\tldw r5, sp+%d", SLOT_C); else emit_li("r5", 0);
    emit_call("cob_unstr_begin");
    for (int i = 0; i < nd; i++) {
        Arg a[3];
        if (delims[i].kind == O_FIG) { unsigned char c = (unsigned char)fig_byte(delims[i].tok->s); a[0] = arg_label(lit_label(&c, 1)); a[1] = arg_imm(1); }
        else { Arg x; opnd_args(&delims[i], &a[0], &x, 0, 0); a[1] = arg_len(&delims[i]); }
        a[2] = arg_imm(dall[i]);
        emit_args(a, 3);
        emit_call("cob_unstr_delim");
    }
    for (int i = 0; i < n; i++) {
        Arg a[6];
        a[0] = arg_ref(&rcv[i]); a[1] = arg_desc(sym_desc(rcv[i].sym));
        if (has_d[i]) { a[2] = arg_ref(&dlm[i]); a[3] = arg_desc(sym_desc(dlm[i].sym)); } else { a[2] = arg_imm(0); a[3] = arg_imm(0); }
        if (has_c[i]) { a[4] = arg_ref(&cnt[i]); a[5] = arg_desc(sym_desc(cnt[i].sym)); } else { a[4] = arg_imm(0); a[5] = arg_imm(0); }
        emit_args(a, 6);
        emit_call("cob_unstr_into");
    }
    if (has_ptr) {
        emit_call("cob_unstr_pointer");
        emit("\tstw sp+%d, r1", SLOT_C);
        Arg a[2] = { arg_ref(&ptr), arg_desc(sym_desc(ptr.sym)) };
        emit_args(a, 2);
        emit("\tldw r5, sp+%d", SLOT_C);
        emit_call("cob_store_int");
    }
    if (has_tly) {
        /* TALLYING IN is incremented by the receivers acted on */
        Arg a[2] = { arg_ref(&tly), arg_desc(sym_desc(tly.sym)) };
        emit_args(a, 2); emit_call("cob_load_int");
        emit("\tstw sp+%d, r1", SLOT_C);
        emit_call("cob_unstr_tally");
        emit("\tldw r2, sp+%d", SLOT_C);
        emit("\tadd r1, r1, r2");
        emit("\tstw sp+%d, r1", SLOT_C);
        emit_args(a, 2);
        emit("\tldw r5, sp+%d", SLOT_C);
        emit_call("cob_store_int");
    }
    int has_ovf = at_word("on") || at_word("overflow") || (at_word("not") && (is_word(peek(1), "on") || is_word(peek(1), "overflow")));
    if (has_ovf) {
        int Lok = new_label(), Lend = new_label();
        emit_call("cob_unstr_overflow");
        emit("\tbeq r1, r0, .L%d", Lok);
        if (at_word("on") || at_word("overflow")) { accept_word("on"); expect_word("overflow"); parse_statements(); }
        emit_jump(Lend);
        emit_label(Lok);
        if (accept_word("not")) { accept_word("on"); expect_word("overflow"); parse_statements(); }
        emit_label(Lend);
    }
    accept_word("end-unstring");
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
    char name[128]; Ref target; int dynamic = 0;
    if (t->kind == T_STR) {
        snprintf(name, sizeof name, "%.*s", t->len > 120 ? 120 : t->len, t->s);
        for (char *k = name; *k; k++) *k = (char)tolower((unsigned char)*k);
        advance();
    } else if (t->kind == T_WORD) {
        /* CALL identifier: the item names the program; resolved at run
         * time against the registry every unit joins at start-up */
        parse_ref(&target); dynamic = 1;
        if (target.sym->is_cond) die_at(line, "CALL: a condition-name cannot name a program");
    } else die_at(line, "expected a program-name literal or an identifier after CALL");
    Arg a[8]; Opnd ops[8]; int n = 0, ncontent = 0;
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
            if (mode == 1) {
                if (o->kind == O_REF && o->ref.sym->is_cond) die_at(o->line, "a condition-name cannot be passed");
                if (o->kind == O_REF && o->ref.rm) die_at(o->line, "BY CONTENT of a reference-modified item is not implemented");
                if (!(o->kind == O_REF || o->kind == O_STR || o->kind == O_NUM)) die_at(o->line, "a CALL argument must be an item or a literal");
                a[n] = arg_content(o); ncontent++;
            } else if (mode == 2) {
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
                else if (o->kind == O_NUM) a[n] = arg_label(call_num_lit_label(&o->num));
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
    /* [ON] EXCEPTION|OVERFLOW ... [NOT [ON] EXCEPTION|OVERFLOW ...]: the
     * exception is the program not being in this executable.  A literal
     * CALL with the clause goes through the registry too, so the link
     * does not demand the program; without it, the linker resolves it. */
    int has_clause = at_word("on") || at_word("exception") || at_word("overflow") ||
                     (at_word("not") && (is_word(peek(1), "on") || is_word(peek(1), "exception") || is_word(peek(1), "overflow")));
    int Lcall = new_label(), Lafter = new_label();
    if (dynamic || has_clause) {
        if (dynamic) { emit_ref_addr(&target, "r3"); emit_li("r4", target.sym->size); }
        else { emit_la("r3", lit_label((const unsigned char *)t->s, t->len)); emit_li("r4", t->len); }
        emit_li("r5", !has_clause);                     /* no clause: the runtime stops on a missing program */
        emit_call("cob_resolve");
        emit("\tadd r12, r0, r1");                      /* callee-saved; the compiler uses no other of r12-r28 */
        if (has_clause) {
            emit("\tbne r12, r0, .L%d", Lcall);
            emit_li("r1", 1); emit("\tstw sp+%d, r1", SLOT_C);
            emit_jump(Lafter);
            emit_label(Lcall);
        }
    }
    emit_args(a, n);
    if (dynamic || has_clause) emit("\tjalr r31, r12, 0");
    else emit("\tjal r31, %s", link_name(name));
    if (ncontent) {                 /* the BY CONTENT copies go, the result kept */
        emit("\tstw sp+%d, r1", SLOT_C);
        emit_li("r3", ncontent); emit_call("cob_content_pop");
        emit("\tldw r1, sp+%d", SLOT_C);
    }
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
    if (has_clause) {
        emit("\tstw sp+%d, r0", SLOT_C);
        emit_label(Lafter);
        int Lend = new_label();
        if (at_word("on") || at_word("exception") || at_word("overflow")) {
            accept_word("on");
            if (!accept_word("exception") && !accept_word("overflow")) die_at(cur()->line, "expected EXCEPTION or OVERFLOW after ON");
            int Lnot = new_label();
            emit("\tldw r1, sp+%d", SLOT_C);
            emit("\tbeq r1, r0, .L%d", Lnot);
            parse_statements();
            emit_jump(Lend);
            emit_label(Lnot);
        }
        if (at_word("not")) {
            advance(); accept_word("on");
            if (!accept_word("exception") && !accept_word("overflow")) die_at(cur()->line, "expected EXCEPTION or OVERFLOW after NOT");
            emit("\tldw r1, sp+%d", SLOT_C);
            emit("\tbne r1, r0, .L%d", Lend);
            parse_statements();
        }
        emit_label(Lend);
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
/* the PAGE FOOTING groups, on a page that was started */
static void emit_page_footing(Report *r)
{
    int any = 0;
    for (int k = 0; k < r->ng; k++) if (r->g[k].type == RG_PAGE_FOOTING) any = 1;
    if (!any) return;
    int Lskip = new_label();
    emit_report_addr("r3", r);
    emit_call("cob_rw_page_started");
    emit("\tbeq r1, r0, .L%d", Lskip);
    for (int k = 0; k < r->ng; k++)
        if (r->g[k].type == RG_PAGE_FOOTING) emit_report_group(r, &r->g[k]);
    emit_label(Lskip);
}

static void emit_page_advance(Report *r)
{
    emit_page_footing(r);
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
        /* the line's position first: LINE-COUNTER holds it while the SOURCE
         * items are moved (X3.23 VIII-5 2.4.5: the PH line prints 1) */
        emit_report_addr("r3", r);
        emit_li("r4", ln->abs); emit_li("r5", ln->plus);
        emit_li("r6", is_body);
        emit_call("cob_rw_line_begin");
        for (int k = 0; k < ln->nf; k++) {
            RField *f = &ln->f[k];
            Arg a[4];
            a[0] = arg_imm(f->column);
            a[1] = arg_desc(rfield_desc(f));
            if (f->has_source) {
                Ref *rf = xmalloc(sizeof *rf);
                int save_tp = g_tp;
                g_tp = f->source_tp; parse_ref(rf); g_tp = save_tp;
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
        emit_li("r4", is_body);
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
    emit_page_footing(r);
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
    if (g->type != RG_DETAIL) die_at(t->line, "GENERATE needs a DETAIL group; '%s' is a page %s", t->s, g->type == RG_PAGE_HEADING ? "heading" : "footing");
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

typedef struct { int kind; Opnd o; Cond *c; } Subject;      /* kind: 0 value, 1 TRUE, 2 FALSE, 3 a condition */

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
        else {
            int start = g_tp;
            subj[ns].kind = 0; subj[ns].o = parse_cond_operand();
            /* an operand followed by a class word or a relation is a condition
             * subject, matched by WHEN TRUE / WHEN FALSE */
            static const char *cw[] = { "numeric", "alphabetic", "alphabetic-lower", "alphabetic-upper", "positive", "negative",
                "is", "not", "equal", "equals", "greater", "less", "=", "<", ">", "<=", ">=", "<>", NULL };
            int is_cond = 0;
            if (cur()->kind == T_WORD || cur()->kind == T_OP) for (int k = 0; cw[k]; k++) if (!strcmp(cur()->s, cw[k])) is_cond = 1;
            if (cur()->kind == T_WORD && switch_find(cur()->s)) is_cond = 0;
            /* a condition-name alone is a condition subject too (NC225A: ALSO IT-IS-81 ... WHEN ... ALSO TRUE) */
            if (subj[ns].o.kind == O_REF && subj[ns].o.ref.sym->is_cond) is_cond = 1;
            if (is_cond) { g_tp = start; subj[ns].kind = 3; subj[ns].c = parse_cond(); }
        }
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
                else if (subj[i].kind == 3) {
                    /* a condition subject against TRUE or FALSE */
                    if (accept_word("true")) c = subj[i].c;
                    else if (accept_word("false")) { Cond *nn = cond_new(C_NOT); nn->a = subj[i].c; c = nn; }
                    else die_at(cur()->line, "WHEN for a condition subject takes TRUE, FALSE or ANY");
                }
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

/* [BEFORE|AFTER] [INITIAL] operand, either or both, after a TALLYING or
 * REPLACING phrase: the runtime is told the range for the next phrase */
static void parse_inspect_range(void)
{
    Opnd before, after; int hb = 0, ha = 0;
    for (;;) {
        if (accept_word("before")) { if (hb) die_at(cur()->line, "two BEFORE phrases"); accept_word("initial"); parse_operand(&before); hb = 1; }
        else if (accept_word("after")) { if (ha) die_at(cur()->line, "two AFTER phrases"); accept_word("initial"); parse_operand(&after); ha = 1; }
        else break;
    }
    if (!hb && !ha) return;
    Arg a[4];
    if (hb) pattern_args(&before, &a[0], &a[1]); else { a[0] = arg_imm(0); a[1] = arg_imm(0); }
    if (ha) pattern_args(&after, &a[2], &a[3]); else { a[2] = arg_imm(0); a[3] = arg_imm(0); }
    emit_args(a, 4);
    emit_call("cob_inspect_range");
}

/* after a run: each TALLYING phrase's count added to its item */
static void emit_inspect_tallies(Ref *tallies, int *tally_ph, int nt)
{
    for (int t = 0; t < nt; t++) {
        Ref *tally = &tallies[t];
        emit_li("r3", tally_ph[t]);
        emit_call("cob_inspect_count");
        emit("\tstw sp+%d, r1", SLOT_C);
        if (is_hot_int(tally->sym)) {
            emit_ref_addr(tally, "r3");
            emit_load_int(tally->sym, "r3", "r1");
            emit("\tldw r2, sp+%d", SLOT_C);
            emit("\tadd r1, r1, r2");
            emit_trunc(tally->sym);
            emit_store_int(tally->sym, "r3", "r1");
        } else {
            emit("\tldw r3, sp+%d", SLOT_C);
            emit("\tsrai r4, r3, 31");
            emit_li("r5", 0);
            emit_call("cob_push_lit");
            emit_top_op(tally, "cob_top_addto", 0);
            emit_call("cob_drop");
        }
    }
}

static void parse_inspect(void)
{
    Ref item; parse_ref(&item);
    if (item.sym->is_cond) die_at(item.line, "INSPECT of a condition-name");
    Opnd itemo = ref_opnd(&item);
    operand_odo_length(&itemo);             /* a group over an ODO table is inspected at its current length */
    /* the phrases are registered with the runtime, which makes the one pass
     * the text describes (cob_inspect_run); then each tally is added.  A
     * statement with both TALLYING and REPLACING is two statements, the
     * tallying pass first (X3.23 general rule): two begin/run rounds. */
    { Arg a[3] = { arg_ref(&itemo.ref), arg_len(&itemo), itemo.ref.rm ? arg_imm(0) : arg_desc(sym_desc(item.sym)) }; emit_args(a, 3); emit_call("cob_inspect_begin"); }
    Ref tallies[32]; int tally_ph[32], nt = 0, np = 0, any = 0;
    if (accept_word("converting")) {
        Opnd from, to; parse_operand(&from); expect_word("to"); parse_operand(&to);
        int fl = from.kind == O_FIG ? 1 : opnd_size(&from), tl = to.kind == O_FIG ? 1 : opnd_size(&to);
        if (fl > 0 && tl > 0 && fl != tl && to.kind != O_FIG) die_at(to.line, "INSPECT CONVERTING: the two operands must be the same length");
        parse_inspect_range();
        Arg a[3], x;
        if (to.kind == O_FIG && fl > 1) {
            /* CONVERTING "abc" TO SPACE: the figurative is as long as the other */
            unsigned char *f = xmalloc((size_t)fl); memset(f, fig_byte(to.tok->s), (size_t)fl);
            a[2] = arg_label(lit_label(f, fl)); free(f);
        } else pattern_args(&to, &a[2], &x);
        pattern_args(&from, &a[0], &a[1]);
        emit_args(a, 3);
        emit_call("cob_inspect_convert");
        emit_call("cob_inspect_run");
        return;
    }
    if (accept_word("tallying")) {
        any = 1;
        for (;;) {
            Ref tally; parse_ref(&tally);
            if (!is_int_item(tally.sym)) die_at(tally.line, "the INSPECT tally '%s' must be an integer item", tally.sym->name);
            expect_word("for");
            for (;;) {
                int kind = 0;
                if (accept_word("characters")) kind = 0;
                else if (accept_word("all")) kind = 1;
                else if (accept_word("leading")) kind = 2;
                else die_at(cur()->line, "expected CHARACTERS, ALL or LEADING in INSPECT TALLYING");
                /* CHARACTERS [range]; ALL|LEADING {operand [range]}... */
                for (;;) {
                    Opnd pat; memset(&pat, 0, sizeof pat);
                    if (kind) parse_operand(&pat);
                    parse_inspect_range();
                    if (np == 32) die_at(cur()->line, "INSPECT: more than 32 phrases");
                    Arg a[5];
                    a[0] = arg_imm(1); a[1] = arg_imm(kind);
                    if (kind) pattern_args(&pat, &a[2], &a[3]); else { a[2] = arg_imm(0); a[3] = arg_imm(0); }
                    a[4] = arg_imm(0);
                    emit_args(a, 5);
                    emit_call("cob_inspect_phrase");
                    if (nt == 32) die_at(tally.line, "INSPECT: more than 32 tallies");
                    tallies[nt] = tally; tally_ph[nt] = np; nt++; np++;
                    /* another operand under the same ALL/LEADING: not a keyword, not the next tally (an identifier followed by FOR) */
                    if (!kind || !at_operand() || at_word("characters") || at_word("all") || at_word("leading") || at_word("replacing")) break;
                    if (cur()->kind == T_WORD && is_word(peek(1), "for")) break;
                }
                if (!(at_word("characters") || at_word("all") || at_word("leading"))) break;
            }
            if (!at_operand() || at_word("replacing")) break;
        }
    }
    if (at_word("replacing") && nt) {
        /* the tallying pass first, its counts added; then the replacing pass */
        emit_call("cob_inspect_run");
        emit_inspect_tallies(tallies, tally_ph, nt);
        nt = 0; np = 0;
        Arg a[3] = { arg_ref(&itemo.ref), arg_len(&itemo), itemo.ref.rm ? arg_imm(0) : arg_desc(sym_desc(item.sym)) }; emit_args(a, 3); emit_call("cob_inspect_begin");
    }
    if (accept_word("replacing")) {
        any = 1;
        for (;;) {
            int kind = 0;
            if (accept_word("characters")) kind = 0;
            else if (accept_word("all")) kind = 1;
            else if (accept_word("leading")) kind = 2;
            else if (accept_word("first")) kind = 3;
            else die_at(cur()->line, "expected CHARACTERS, ALL, LEADING or FIRST in INSPECT REPLACING");
            /* CHARACTERS BY rep [range]; ALL|LEADING|FIRST {pat BY rep [range]}... */
            for (;;) {
                Opnd pat, rep; memset(&pat, 0, sizeof pat); memset(&rep, 0, sizeof rep);
                if (kind) parse_operand(&pat);
                expect_word("by"); parse_operand(&rep);
                if (kind) {
                    int pl = pat.kind == O_FIG ? 1 : opnd_size(&pat), rl = rep.kind == O_FIG ? 1 : opnd_size(&rep);
                    if (pl > 0 && rl > 0 && pl != rl) die_at(rep.line, "INSPECT REPLACING: the two operands must be the same length");
                }
                parse_inspect_range();
                if (np == 32) die_at(cur()->line, "INSPECT: more than 32 phrases");
                Arg a[5];
                a[0] = arg_imm(0); a[1] = arg_imm(kind);
                if (kind) pattern_args(&pat, &a[2], &a[3]); else { a[2] = arg_imm(0); a[3] = arg_imm(1); }
                Arg rl; pattern_args(&rep, &a[4], &rl);
                emit_args(a, 5);
                emit_call("cob_inspect_phrase");
                np++;
                if (!kind || !at_operand() || at_word("characters") || at_word("all") || at_word("leading") || at_word("first")) break;
            }
            if (!(at_word("characters") || at_word("all") || at_word("leading") || at_word("first"))) break;
        }
    }
    if (!any) die_at(item.line, "INSPECT needs TALLYING, REPLACING or CONVERTING");
    emit_call("cob_inspect_run");
    emit_inspect_tallies(tallies, tally_ph, nt);
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
    if (has_vary && vary.sym->is_index && vary.sym->ix_table == sym_idx(tbl)) {
        /* VARYING one of the table's own indexes: that index does the search */
        ix = vary.sym; ixr.sym = ix; has_vary = 0;
    }

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
    if (has_vary && vary.sym != ixr.sym) emit_add_to_ref(&step, &vary);   /* VARYING the table's own index: once */
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
    if (!strcmp(v, "use")) { advance(); parse_use(); return; }
    if (!strcmp(v, "sort")) { advance(); g_is_merge = 0; parse_sort(); return; }
    if (!strcmp(v, "merge")) { advance(); g_is_merge = 1; parse_sort(); g_is_merge = 0; return; }
    if (!strcmp(v, "release")) { advance(); parse_release(); return; }
    if (!strcmp(v, "return")) { advance(); parse_return(); return; }
    if (!strcmp(v, "string")) { advance(); parse_string(); return; }
    if (!strcmp(v, "unstring")) { advance(); parse_unstring(); return; }
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
        /* nothing to release -- the program is linked in -- but its next
         * CALL finds it in its initial state: the registry's cancel routine */
        advance();
        while (cur()->kind == T_STR || (cur()->kind == T_WORD && !is_verb(cur()->s) && !is_terminator(cur()->s))) {
            if (cur()->kind == T_STR) { emit_la("r3", lit_label((const unsigned char *)cur()->s, cur()->len)); emit_li("r4", cur()->len); advance(); }
            else { Ref r; parse_ref(&r); emit_ref_addr(&r, "r3"); emit_li("r4", r.sym->size); }
            emit_call("cob_cancel");
        }
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

        { "suppress", "after v1" }, { NULL, NULL } };
    for (int i = 0; later[i].verb; i++)
        if (!strcmp(v, later[i].verb)) die_at(t->line, "the verb %s is not implemented yet (%s)", v, later[i].when);
    if (is_terminator(v)) die_at(t->line, "'%s' without a matching statement", v);
    if (!strcmp(v, "identification") || !strcmp(v, "id"))
        die_at(t->line, "IDENTIFICATION DIVISION in the middle of a sentence (a contained program begins after a period)");
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
static int g_initial;               /* PROGRAM-ID ... IS INITIAL: WORKING-STORAGE fresh on every CALL */

/* everything a unit keeps in globals, saved while a contained program is compiled */
struct UnitSave {
    int unit, sym_base, sym_end, file_base, file_end, para_base, para_end, use_end;
    char progid[64];
    int nreport, nscreen, nclass, nswitch, nalphabet, nmnemonic, last_item, nsame_groups, collate, lowval, highval, cur_fd, in_linkage;
    char collate_name[64];
    int nuse, in_decl, cur_sec_id, saw_end, initial, nsorttab;
    UseEntry use[64];
    File *io_file;
    UClass cls[16]; SwitchName sw[32]; Alphabet alph[16]; Mnemonic mn[16]; int same[8][16], nsame[8];
    SortTab *sorttab;
};
static void unit_range(int level, int *from, int *to) { *from = g_ustack[level]->sym_base; *to = g_ustack[level]->sym_end; }
static void unit_file_range(int level, int *from, int *to) { *from = g_ustack[level]->file_base; *to = g_ustack[level]->file_end; }
static void unit_use_range(int level, int *from, int *to) { *from = level ? g_ustack[level - 1]->use_end : 0; *to = g_ustack[level]->use_end; }
static int unit_use_own_from(void) { return g_udepth ? g_ustack[g_udepth - 1]->use_end : 0; }

static void parse_identification_division(void);
static void parse_environment_division(void);
static void parse_data_division(void);
static void emit_unit_data(void);
static void parse_procedure_division(void);

/* IDENTIFICATION DIVISION inside a program: a contained program.  It is
 * compiled as a unit of its own -- its own entry, WORKING-STORAGE, files,
 * paragraphs -- seeing the containing programs' GLOBAL items, files and
 * USE procedures.  The tables are shared: the contained unit's entries
 * are appended and cut back on its END PROGRAM; the USE entries of every
 * enclosing unit stay in g_use below this unit's own. */
static void compile_nested_unit(void)
{
    if (g_udepth == 8) die_at(cur()->line, "programs nested more than 8 deep");
    UnitSave *u = xmalloc(sizeof *u);
    u->unit = g_unit; u->sym_base = g_sym_base; u->sym_end = g_nsym; u->file_base = g_file_base; u->file_end = g_nfile;
    u->para_base = g_para_base; u->para_end = g_npara; u->use_end = g_nuse;
    memcpy(u->progid, g_progid, sizeof u->progid);
    u->nreport = g_nreport; u->nscreen = g_nscreen; u->nclass = g_nclass; u->nswitch = g_nswitch; u->nalphabet = g_nalphabet;
    u->nmnemonic = g_nmnemonic; u->last_item = g_last_item; u->nsame_groups = g_nsame_groups; u->collate = g_collate;
    u->lowval = g_lowval; u->highval = g_highval; u->cur_fd = g_cur_fd; u->in_linkage = g_in_linkage;
    memcpy(u->collate_name, g_collate_name, sizeof u->collate_name);
    u->nuse = g_nuse; memcpy(u->use, g_use, sizeof u->use); u->in_decl = g_in_decl; u->cur_sec_id = g_cur_sec_id;
    u->saw_end = g_saw_end_program; u->initial = g_initial; u->io_file = g_io_file;
    memcpy(u->cls, g_class, sizeof u->cls); memcpy(u->sw, g_switch, sizeof u->sw); memcpy(u->alph, g_alphabet, sizeof u->alph);
    memcpy(u->mn, g_mnemonic, sizeof u->mn); memcpy(u->same, g_same, sizeof u->same); memcpy(u->nsame, g_nsame, sizeof u->nsame);
    u->nsorttab = g_nsorttab; u->sorttab = xmalloc((size_t)(g_nsorttab + 1) * sizeof *g_sorttab);
    memcpy(u->sorttab, g_sorttab, (size_t)g_nsorttab * sizeof *g_sorttab);
    g_ustack[g_udepth++] = u;

    g_unit = ++g_unit_counter;
    g_sym_base = g_nsym; g_file_base = g_nfile; g_para_base = g_npara;
    /* the contained unit's own USE entries follow every enclosing unit's */
    g_nreport = 0; g_nscreen = 0; g_nclass = 0; g_nswitch = 0; g_nalphabet = 0; g_nmnemonic = 0; g_last_item = -1;
    g_nsame_groups = 0; g_collate = -1; g_collate_name[0] = 0; g_lowval = 0x00; g_highval = 0xFF; g_cur_fd = -1; g_in_linkage = 0;
    g_nsorttab = 0; g_initial = 0;
    parse_identification_division();
    parse_environment_division();
    parse_data_division();
    if (!at_word("procedure")) die_at(cur()->line, "expected PROCEDURE DIVISION, found %s", tok_desc(cur()));
    parse_procedure_division();
    emit_unit_data();
    if (!g_saw_end_program) die_at(cur()->line, "a contained program needs its END PROGRAM");

    g_udepth--;
    g_unit = u->unit; g_sym_base = u->sym_base; g_nsym = u->sym_end; g_file_base = u->file_base; g_nfile = u->file_end;
    g_para_base = u->para_base; g_npara = u->para_end;
    memcpy(g_progid, u->progid, sizeof g_progid);
    g_nreport = u->nreport; g_nscreen = u->nscreen; g_nclass = u->nclass; g_nswitch = u->nswitch; g_nalphabet = u->nalphabet;
    g_nmnemonic = u->nmnemonic; g_last_item = u->last_item; g_nsame_groups = u->nsame_groups; g_collate = u->collate;
    g_lowval = u->lowval; g_highval = u->highval; g_cur_fd = u->cur_fd; g_in_linkage = u->in_linkage;
    memcpy(g_collate_name, u->collate_name, sizeof g_collate_name);
    g_nuse = u->nuse; memcpy(g_use, u->use, sizeof g_use); g_in_decl = u->in_decl; g_cur_sec_id = u->cur_sec_id;
    g_saw_end_program = u->saw_end; g_initial = u->initial; g_io_file = u->io_file;
    memcpy(g_class, u->cls, sizeof g_class); memcpy(g_switch, u->sw, sizeof g_switch); memcpy(g_alphabet, u->alph, sizeof g_alphabet);
    memcpy(g_mnemonic, u->mn, sizeof g_mnemonic); memcpy(g_same, u->same, sizeof g_same); memcpy(g_nsame, u->nsame, sizeof g_nsame);
    g_nsorttab = u->nsorttab;
    if (g_nsorttab > g_sorttabcap) { g_sorttabcap = g_nsorttab; g_sorttab = realloc(g_sorttab, (size_t)g_sorttabcap * sizeof *g_sorttab); }
    memcpy(g_sorttab, u->sorttab, (size_t)g_nsorttab * sizeof *g_sorttab);
    free(u->sorttab); free(u);
}

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
    if (g_collate >= 0) {       /* PROGRAM COLLATING SEQUENCE: this unit's table, the caller's kept */
        char lab[32]; snprintf(lab, sizeof lab, ".Lcoll%d", g_unit);
        emit_la("r3", lab); emit_call("cob_set_collating"); emit("\tstw sp+%d, r1", SLOT_COLL);
    }
    if (g_dp_comma) { emit("\taddi r3, r0, 1"); emit_call("cob_set_decimal_point"); emit("\tstw sp+%d, r1", SLOT_DP); }
    if (g_currency && g_currency != '$') { emit_li("r3", g_currency); emit_call("cob_set_currency"); emit("\tstw sp+%d, r1", SLOT_CUR); }
    if (g_initial) { char cl[32]; snprintf(cl, sizeof cl, ".Lcan%d", g_unit); emit_call(cl); }   /* INITIAL: as after CANCEL */
    /* the caller's addresses go into the LINKAGE cells */
    for (int i = 0; i < nusing; i++) {
        emit_la("r1", g_sym[using[i]->record].label);
        emit("\tstw r1+0, %s", argreg(i));
    }
    /* a FILE STATUS item in the LINKAGE SECTION (or EXTERNAL): the image
     * takes its address now that the cell is filled (status is at 16) */
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->status_sym) continue;
        Sym *rec = &g_sym[f->status_sym->record];
        if (!rec->is_linkage && !rec->is_external) continue;
        emit_item_addr("r1", f->status_sym, f->status_sym->offset);
        char lab[32]; snprintf(lab, sizeof lab, ".Lf%d_%d", f->unit, i);
        emit_la("r2", lab);
        emit("\tstw r2+16, r1");
    }
    /* EXTERNAL records: the block every program of this name shares (the
     * records of an EXTERNAL FD share one block under the file's name) */
    int has_ext_file = 0;
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0 || !s->is_external) continue;
        char nm[80];
        if (s->fd >= 0) snprintf(nm, sizeof nm, "file:%s", g_files[s->fd].name); else snprintf(nm, sizeof nm, "%s", s->name);
        emit_la("r3", lit_label((const unsigned char *)nm, (int)strlen(nm) + 1));
        emit_li("r4", s->image_size);
        emit_call("cob_external");
        emit("\tadd r2, r0, r1");
        emit_la("r1", s->label);
        emit("\tstw r1+0, r2");
    }
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->external) continue;
        has_ext_file = 1;
        char nm[80]; snprintf(nm, sizeof nm, "%s", f->name);
        emit_la("r3", lit_label((const unsigned char *)nm, (int)strlen(nm) + 1));
        char lab[32]; snprintf(lab, sizeof lab, ".Lf%d_%d", f->unit, i);
        emit_la("r4", lab);
        if (f->rec >= 0) { emit_la("r5", g_sym[g_sym[f->rec].record].label); emit("\tldw r5, r5+0"); } else emit_li("r5", 0);
        emit_call("cob_ext_file_enter");
        snprintf(lab, sizeof lab, ".Lfx%d_%d", f->unit, i);
        emit("\tadd r2, r0, r1");
        emit_la("r1", lab);
        emit("\tstw r1+0, r2");
    }

    int cur_par = -1, cur_sec = -1;
    int Ldecl_end = -1;
    if (!g_udepth) g_nuse = 0;              /* a contained unit's USE entries follow the enclosing units' */
    g_cur_sec_id = -1; g_in_decl = 0;
    if (accept_word("declaratives")) {
        /* the declarative sections are reached only through USE; jump over them */
        expect_period();
        Ldecl_end = new_label(); emit_jump(Ldecl_end); g_in_decl = 1;
    }
    for (;;) {
        Tok *t = cur();
        if (t->kind == T_EOF) break;
        if (is_word(t, "end") && is_word(peek(1), "program")) break;
        if ((is_word(t, "identification") || is_word(t, "id")) && is_word(peek(1), "division")) {
            /* a contained program: from here to END PROGRAM the text is nested
             * programs; the containing program's flow ends as at its last line */
            if (cur_par >= 0) emit_exit_check(cur_par);
            if (cur_sec >= 0) emit_exit_check(cur_sec);
            cur_par = -1; cur_sec = -1; g_cur_sec_id = -1;
            emit("\tjal r0, .Lgb%d", g_unit);
            compile_nested_unit();
            emit("\t.text");                   /* the contained unit's data left the section */
            continue;
        }
        if (is_word(t, "end") && is_word(peek(1), "declaratives")) {
            if (!g_in_decl) die_at(t->line, "END DECLARATIVES without DECLARATIVES");
            if (cur_par >= 0) emit_exit_check(cur_par);
            if (cur_sec >= 0) emit_exit_check(cur_sec);
            cur_par = -1; cur_sec = -1; g_cur_sec_id = -1;
            advance(); advance(); expect_period();
            emit_label(Ldecl_end); g_in_decl = 0;
            continue;
        }

        if (((t->kind == T_WORD && !is_verb(t->s)) || (t->kind != T_WORD && at_para_name(t))) && (peek(1)->kind == T_PERIOD ||
            (is_word(peek(1), "section") && peek(2)->kind == T_PERIOD))) {
            Para *p = is_word(peek(1), "section") ? para_find(t->s) : para_find_in(t->s, cur_sec >= 0 ? cur_sec : -1);
            if (!p) p = para_find(t->s);
            if (!p) die_at(t->line, "internal: paragraph '%s' not prescanned", t->s);
            if (cur_par >= 0) emit_exit_check(cur_par);
            if (p->is_section && cur_sec >= 0) emit_exit_check(cur_sec);
            emit_para_label(p);
            if (p->is_section) { cur_sec = p->id; cur_par = -1; g_cur_sec_id = p->id; } else cur_par = p->id;
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
    if (has_ext_file)
        for (int i = g_file_base; i < g_nfile; i++) {
            File *f = &g_files[i];
            if (!f->external) continue;
            char nm[80]; snprintf(nm, sizeof nm, "%s", f->name);
            emit_la("r3", lit_label((const unsigned char *)nm, (int)strlen(nm) + 1));
            char lab[32]; snprintf(lab, sizeof lab, ".Lf%d_%d", f->unit, i);
            emit_la("r4", lab);
            emit_call("cob_ext_file_exit");
        }
    if (g_collate >= 0) { emit("\tldw r3, sp+%d", SLOT_COLL); emit_call("cob_set_collating"); }
    if (g_dp_comma) { emit("\tldw r3, sp+%d", SLOT_DP); emit_call("cob_set_decimal_point"); }
    if (g_currency && g_currency != '$') { emit("\tldw r3, sp+%d", SLOT_CUR); emit_call("cob_set_currency"); }
    emit("\taddi r1, r0, 0");
    emit("\tldw r11, sp+4");
    emit("\tldw lr, sp+0");
    emit("\taddi sp, sp, %d", FRAME);
    emit("\tjalr r0, r31, 0");

    /* the unit joins the program registry at start-up (CALL identifier) */
    {
        char nm[130]; int nl = (int)strlen(g_progid);
        memcpy(nm, g_progid, (size_t)nl); nm[nl] = 0;
        const char *nlab = lit_label((const unsigned char *)nm, nl + 1);
        /* CANCEL: every WORKING-STORAGE record back to its initial state */
        emit("\t.p2align 2");
        emit(".Lcan%d:", g_unit);
        emit("\taddi sp, sp, -8");
        emit("\tstw sp+0, lr");
        for (int i = g_sym_base; i < g_nsym; i++) {
            Sym *s = &g_sym[i];
            if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0 || s->is_linkage || s->is_external) continue;
            emit_la("r3", s->label);
            char il[80]; snprintf(il, sizeof il, "%s_i", s->label);
            emit_la("r4", il);
            emit_li("r5", s->image_size);
            emit_call("memcpy");
        }
        emit("\tldw lr, sp+0");
        emit("\taddi sp, sp, 8");
        emit("\tjalr r0, r31, 0");
        emit("\t.p2align 2");
        emit(".Lreg%d:", g_unit);
        emit("\taddi sp, sp, -8");
        emit("\tstw sp+0, lr");
        emit_la("r3", nlab);
        emit_la("r4", entry);
        char cl[32]; snprintf(cl, sizeof cl, ".Lcan%d", g_unit);
        emit_la("r5", cl);
        emit_call("cob_register");
        emit("\tldw lr, sp+0");
        emit("\taddi sp, sp, 8");
        emit("\tjalr r0, r31, 0");
        emit("\t.section .init_array");
        emit("\t.p2align 2");
        emit("\t.word .Lreg%d", g_unit);
        emit("\t.text");
    }

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
    accept_word("is");
    for (;;) {
        if (accept_word("initial")) g_initial = 1;              /* fresh WORKING-STORAGE on every CALL */
        else if (accept_word("common")) { }                      /* callable by the siblings too: every program here is */
        else break;
    }
    accept_word("program");
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

static void skip_to_period(void) __attribute__((unused));
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
    f->line = line; f->rec = -1; f->org = COB_ORG_SEQ; f->unit = g_unit;
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
            else if (cur()->kind == T_PERIOD || at_word("file") || at_word("organization") || at_word("organisation") || at_word("access") || at_word("record") || at_word("status"))
                has_assign = -1;                        /* nothing named: allowed for an EXTERNAL file */
            else if (cur()->kind == T_WORD) {
                if (at_word("disk") || at_word("keyboard") || at_word("display") || at_word("printer"))
                    die_at(t->line, "ASSIGN TO %s (a device) is not supported; name a file", cur()->s);
                snprintf(f->assign_name, sizeof f->assign_name, "%s", cur()->s); advance();
            } else die_at(t->line, "expected a literal or data-name after ASSIGN TO");
            has_assign = 1;
            continue;
        }
        if (at_word("sequential") || at_word("indexed") || (at_word("line") && is_word(peek(1), "sequential"))) {
            /* ORGANIZATION IS may be omitted */
            f->org_given = 1;
            if (accept_word("line")) { expect_word("sequential"); f->org = COB_ORG_LINESEQ; }
            else if (accept_word("sequential")) f->org = COB_ORG_SEQ;
            else { advance(); f->org = COB_ORG_INDEXED; }
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
            if (accept_word("delimiter")) { accept_word("is"); if (cur()->kind == T_WORD) advance(); continue; }   /* RECORD DELIMITER IS STANDARD-1 */
            accept_word("key"); accept_word("is");
            if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after RECORD KEY");
            snprintf(f->key_name, sizeof f->key_name, "%s", cur()->s); advance();
            if ((at_word("in") || at_word("of")) && peek(1)->kind == T_WORD) { advance(); snprintf(f->key_qual, sizeof f->key_qual, "%s", cur()->s); advance(); }
            continue;
        }
        if (accept_word("alternate")) {
            /* ALTERNATE [RECORD] [KEY] [IS] data-name [WITH DUPLICATES] */
            accept_word("record"); accept_word("key"); accept_word("is");
            if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after ALTERNATE RECORD KEY");
            if (f->nalt == 16) die_at(t->line, "too many ALTERNATE RECORD KEYs (16)");
            snprintf(f->alt[f->nalt].name, sizeof f->alt[f->nalt].name, "%s", cur()->s); advance();
            if ((at_word("in") || at_word("of")) && peek(1)->kind == T_WORD) { advance(); snprintf(f->alt[f->nalt].qual, sizeof f->alt[f->nalt].qual, "%s", cur()->s); advance(); }
            if (accept_word("with")) { expect_word("duplicates"); f->alt[f->nalt].dups = 1; }
            else if (accept_word("duplicates")) f->alt[f->nalt].dups = 1;
            f->nalt++;
            continue;
        }
        if (accept_word("relative")) {
            /* RELATIVE [KEY IS] data-name -- or ORGANIZATION IS omitted before
             * a bare RELATIVE, told apart by what follows */
            static const char *clause_words[] = { "access", "assign", "organization", "organisation", "record",
                "alternate", "file", "status", "sharing", "lock", "reserve", "padding", "sequential", "indexed",
                "relative", "line", "select", NULL };
            int has_key = accept_word("key");
            if (has_key) accept_word("is");
            int is_clause = 0;
            if (cur()->kind == T_WORD) for (int k = 0; clause_words[k]; k++) if (!strcmp(cur()->s, clause_words[k])) is_clause = 1;
            if (has_key || (cur()->kind == T_WORD && !is_clause)) {
                if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after RELATIVE KEY");
                snprintf(f->relkey_name, sizeof f->relkey_name, "%s", cur()->s); advance();
            } else { f->org_given = 1; f->org = COB_ORG_RELATIVE; }
            continue;
        }
        if (at_word("file") || at_word("status")) {
            accept_word("file"); expect_word("status"); accept_word("is");
            if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after FILE STATUS");
            snprintf(f->status_name, sizeof f->status_name, "%s", cur()->s); advance();
            if (accept_word("of") || accept_word("in")) {           /* status-name OF group */
                if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after OF/IN");
                snprintf(f->status_qual, sizeof f->status_qual, "%s", cur()->s); advance();
            }
            continue;
        }
        if (accept_word("padding")) {           /* PADDING CHARACTER: block padding, no blocks here */
            accept_word("character"); accept_word("is");
            if (cur()->kind == T_STR || cur()->kind == T_WORD) advance();
            continue;
        }
        if (accept_word("reserve")) {           /* RESERVE n AREAS: buffering is the host's */
            if (cur()->kind == T_NUM || at_word("no")) advance();
            accept_word("area"); accept_word("areas");
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
    if (has_assign < 0) f->assign_name[0] = 0, f->assign_lit = NULL;   /* checked against EXTERNAL once the FD is in */
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
                while (cur()->kind == T_WORD && !at_word("special-names") && !at_word("input-output") &&
                       !at_word("source-computer") && !at_word("object-computer") && !at_division()) {
                    if (accept_word("collating")) {         /* [PROGRAM] COLLATING SEQUENCE IS alphabet-name */
                        accept_word("sequence"); accept_word("is");
                        if (cur()->kind != T_WORD) die_at(cur()->line, "expected an alphabet-name after COLLATING SEQUENCE");
                        snprintf(g_collate_name, sizeof g_collate_name, "%s", cur()->s);
                    }
                    advance();
                }
                if (cur()->kind == T_PERIOD) advance();
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
                            if (at_word("through") || at_word("thru")) {
                                /* a range: one character to one character */
                                advance();
                                if (lo->len != 1) die_at(lo->line, "CLASS %s: THROUGH takes one-character literals", uc->name);
                                if (cur()->kind != T_STR || cur()->len != 1) die_at(cur()->line, "CLASS %s: THROUGH needs a one-character literal", uc->name);
                                unsigned a = (unsigned char)lo->s[0], b = (unsigned char)cur()->s[0]; advance();
                                if (b < a) { unsigned t = a; a = b; b = t; }
                                for (unsigned c = a; c <= b; c++) uc->tab[c] = 1;
                            } else {
                                /* every character of the literal is in the class ("ABCD") */
                                if (lo->len < 1) die_at(lo->line, "CLASS %s: an empty literal", uc->name);
                                for (int k = 0; k < lo->len; k++) uc->tab[(unsigned char)lo->s[k]] = 1;
                            }
                            any = 1;
                        }
                        if (!any) die_at(cur()->line, "CLASS %s: expected a one-character literal", uc->name);
                        continue;
                    }
                    if (cur()->kind == T_WORD && !strncmp(cur()->s, "switch-", 7) && isdigit((unsigned char)cur()->s[7])) {
                        int sw = atoi(cur()->s + 7); advance();
                        if (sw < 1 || sw > 8) die_at(cur()->line, "SWITCH-%d: switches are 1 to 8", sw);
                        if (accept_word("is")) {
                            if (cur()->kind != T_WORD) die_at(cur()->line, "expected a mnemonic-name after SWITCH-%d IS", sw);
                            if (g_nswitch == 32) die_at(cur()->line, "too many switch names");
                            SwitchName *m = &g_switch[g_nswitch++];
                            snprintf(m->name, sizeof m->name, "%s", cur()->s); m->sw = sw; m->on = -1; advance();
                        }
                        while (at_word("on") || at_word("off")) {
                            int on = accept_word("on"); if (!on) accept_word("off");
                            accept_word("status"); accept_word("is");
                            if (cur()->kind != T_WORD) die_at(cur()->line, "expected a condition-name after ON/OFF STATUS");
                            if (g_nswitch == 32) die_at(cur()->line, "too many switch names");
                            SwitchName *m = &g_switch[g_nswitch++];
                            snprintf(m->name, sizeof m->name, "%s", cur()->s); m->sw = sw; m->on = on; advance();
                        }
                        continue;
                    }
                    if (accept_word("currency")) {            /* already applied to the pictures; see apply_decimal_point */
                        accept_word("sign"); accept_word("is");
                        if (cur()->kind != T_STR) die_at(cur()->line, "CURRENCY SIGN needs a literal");
                        advance(); continue;
                    }
                    if (accept_word("decimal-point")) {       /* already applied to the text; see apply_decimal_point */
                        accept_word("is");
                        if (!accept_word("comma")) die_at(cur()->line, "DECIMAL-POINT IS COMMA is the only form");
                        continue;
                    }
                    if (accept_word("alphabet")) {
                        if (cur()->kind != T_WORD) die_at(cur()->line, "expected an alphabet-name after ALPHABET");
                        if (g_nalphabet == 16) die_at(cur()->line, "too many ALPHABET clauses");
                        Alphabet *a = &g_alphabet[g_nalphabet++];
                        snprintf(a->name, sizeof a->name, "%s", cur()->s); advance();
                        accept_word("is");
                        if (accept_word("native") || accept_word("standard-1") || accept_word("standard-2")) a->native = 1;
                        else if (accept_word("ebcdic")) die_at(cur()->line, "ALPHABET %s IS EBCDIC is not implemented (the machine is ASCII)", a->name);
                        else {
                            /* literal phrases: lit [THROUGH lit | ALSO lit ...] ... -- the
                             * characters named take the first collating positions in that
                             * order (ALSO: the same position), the rest follow in native order */
                            a->native = 0;
                            int seen[256] = { 0 }, rank = 0, any = 0;
                            #define ALPHA_CH(tok, out) do { \
                                Tok *_t = (tok); \
                                if (_t->kind == T_STR) { if (_t->len != 1) die_at(_t->line, "ALPHABET %s: a literal of one character (or THROUGH a range)", a->name); *(out) = (unsigned char)_t->s[0]; } \
                                else if (_t->kind == T_NUM) { int _v = atoi(_t->s); if (_v < 1 || _v > 256) die_at(_t->line, "ALPHABET %s: an ordinal position is 1 to 256", a->name); *(out) = (unsigned char)(_v - 1); } \
                                else if (_t->kind == T_WORD && is_figurative(_t->s)) *(out) = (unsigned char)fig_byte(_t->s); \
                                else die_at(_t->line, "ALPHABET %s: expected a literal", a->name); } while (0)
                            for (;;) {
                                Tok *lo = cur();
                                if (!(lo->kind == T_STR || lo->kind == T_NUM || (lo->kind == T_WORD && is_figurative(lo->s)))) break;
                                if (lo->kind == T_STR && lo->len > 1) {
                                    /* a longer literal: each character in turn */
                                    for (int c = 0; c < lo->len; c++) { unsigned char ch = (unsigned char)lo->s[c]; if (!seen[ch]) { seen[ch] = 1; a->rank[ch] = (unsigned char)rank++; } }
                                    advance(); any = 1; continue;
                                }
                                unsigned char c1; ALPHA_CH(lo, &c1); advance();
                                if (accept_word("through") || accept_word("thru")) {
                                    unsigned char c2 = 0; ALPHA_CH(cur(), &c2); advance();
                                    int step = c2 >= c1 ? 1 : -1;
                                    for (int c = c1; ; c += step) { if (!seen[c]) { seen[c] = 1; a->rank[c] = (unsigned char)rank++; } if (c == c2) break; }
                                } else {
                                    if (!seen[c1]) { seen[c1] = 1; a->rank[c1] = (unsigned char)rank; }
                                    while (accept_word("also")) { unsigned char c3 = 0; ALPHA_CH(cur(), &c3); advance(); if (!seen[c3]) { seen[c3] = 1; a->rank[c3] = (unsigned char)rank; } }
                                    rank++;
                                }
                                any = 1;
                            }
                            #undef ALPHA_CH
                            if (!any) die_at(cur()->line, "ALPHABET %s: expected NATIVE, STANDARD-1 or literals", a->name);
                            for (int c = 0; c < 256; c++) if (!seen[c]) a->rank[c] = (unsigned char)(rank < 255 ? rank++ : 255);
                        }
                        continue;
                    }
                    if (cur()->kind == T_WORD) {
                        int mk = 0;
                        if (at_word("sysin") || at_word("stdin") || at_word("sysipt")) mk = 1;
                        else if (at_word("sysout") || at_word("stdout") || at_word("console") || at_word("syserr") || at_word("stderr") || at_word("syslst") || at_word("sysprint")) mk = 2;
                        else if (at_word("formfeed") || at_word("c01") || at_word("csp")) mk = 3;
                        if (mk) {
                            advance(); accept_word("is");
                            if (cur()->kind != T_WORD) die_at(cur()->line, "expected a mnemonic-name after the device name");
                            if (g_nmnemonic == 16) die_at(cur()->line, "too many mnemonic-names");
                            Mnemonic *m = &g_mnemonic[g_nmnemonic++];
                            snprintf(m->name, sizeof m->name, "%s", cur()->s); m->kind = mk; advance();
                            continue;
                        }
                    }
                    if (at_division() || at_word("input-output")) break;
                    die_at(cur()->line, "SPECIAL-NAMES clause '%s' is not implemented yet (CLASS, SWITCH-n, ALPHABET and the device names are)", cur()->s);
                }
                continue;
            }
            if (at_word("repository"))
                die_at(cur()->line, "REPOSITORY is COBOL 2002; rewrite user-defined functions as CALL (docs/functions.md)");
            break;
        }
    }
    if (g_collate_name[0]) {
        int found = -1;
        for (int i = 0; i < g_nalphabet; i++) if (!strcmp(g_alphabet[i].name, g_collate_name)) found = i;
        if (found < 0) die_at(cur()->line, "PROGRAM COLLATING SEQUENCE '%s' is not an ALPHABET of SPECIAL-NAMES", g_collate_name);
        if (!g_alphabet[found].native) {
            g_collate = found;
            /* LOW-VALUE and HIGH-VALUE are the sequence's first and last characters */
            int lo = 0, hi = 0;
            for (int c = 0; c < 256; c++) { if (g_alphabet[found].rank[c] < g_alphabet[found].rank[lo]) lo = c; if (g_alphabet[found].rank[c] >= g_alphabet[found].rank[hi]) hi = c; }
            g_lowval = lo; g_highval = hi;
        }
    }
    if (accept_word("input-output")) {
        expect_word("section"); expect_period();
        if (accept_word("file-control")) {
            expect_period();
            while (accept_word("select")) parse_select();
        }
        if (accept_word("i-o-control")) {
            /* SAME RECORD AREA means what it says; SAME AREA / SORT AREA,
             * RERUN and MULTIPLE FILE TAPE are hints for machines with tapes
             * and scarce memory, and are read past */
            expect_period();
            while (!at_division() && cur()->kind != T_EOF) {
                if (accept_word("same")) {
                    int is_record = accept_word("record");
                    if (!is_record) { accept_word("sort"); accept_word("sort-merge"); }
                    accept_word("area"); accept_word("for");
                    int g = -1;
                    if (is_record) {
                        if (g_nsame_groups == 8) die_at(cur()->line, "too many SAME RECORD AREA clauses");
                        g = g_nsame_groups++; g_nsame[g] = 0;
                    }
                    while (cur()->kind == T_WORD && file_find(cur()->s)) {
                        if (g >= 0 && g_nsame[g] < 16) g_same[g][g_nsame[g]++] = (int)(file_find(cur()->s) - g_files);
                        advance();
                    }
                    continue;
                }
                advance();
            }
        }
    }
    if (!at_division()) die_at(cur()->line, "unexpected %s in the ENVIRONMENT DIVISION", tok_desc(cur()));
}

/* FD file-name [clauses]. followed by its 01s */
static void parse_fd(void)
{
    int line = cur()->line;
    int is_sd = accept_word("sd");
    if (!is_sd) expect_word("fd");
    if (cur()->kind != T_WORD) die_at(line, "expected a file-name after %s", is_sd ? "SD" : "FD");
    File *f = file_find(cur()->s);
    if (!f) die_at(line, "%s %s has no SELECT", is_sd ? "SD" : "FD", cur()->s);
    f->lin_counter_sym = -1;
    if (is_sd) f->org = COB_ORG_SORT;         /* a sort file: SORT opens it, RELEASE/RETURN use it */
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
                accept_word("from");
                if (cur()->kind == T_NUM) { f->minlen = atoi(cur()->s); advance(); }
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
        if (accept_word("is")) continue;
        if (accept_word("global")) { f->global = 1; continue; }
        if (accept_word("external")) { f->external = 1; continue; }
        if (accept_word("linage")) {
            /* LINAGE [IS] n [LINES] [WITH FOOTING [AT] f] [LINES AT TOP t] [LINES AT BOTTOM b] */
            accept_word("is");
            f->linage = 1;
            int which = 0;
            for (;;) {
                if (cur()->kind == T_NUM) { f->lin_lit[which] = atol(cur()->s); advance(); }
                else if (cur()->kind == T_WORD && !at_word("lines") && !at_word("with") && !at_word("footing") && !at_word("at") && !at_word("top") && !at_word("bottom")) { snprintf(f->lin_name[which], sizeof f->lin_name[which], "%s", cur()->s); advance(); }
                else die_at(t->line, "LINAGE: expected an integer or a data-name");
                if (which == 0) accept_word("lines");
                if (accept_word("with")) { expect_word("footing"); accept_word("at"); which = 1; continue; }
                if (accept_word("footing")) { accept_word("at"); which = 1; continue; }
                if (accept_word("lines")) { accept_word("at"); if (accept_word("top")) which = 2; else if (accept_word("bottom")) which = 3; else die_at(t->line, "LINAGE: LINES AT TOP or BOTTOM"); continue; }
                if (accept_word("at")) { if (accept_word("top")) which = 2; else if (accept_word("bottom")) which = 3; else die_at(t->line, "LINAGE: AT TOP or BOTTOM"); continue; }
                if (accept_word("top")) { which = 2; continue; }
                if (accept_word("bottom")) { which = 3; continue; }
                break;
            }
            /* the file's LINAGE-COUNTER: a four-byte unsigned cell in its cob_file */
            Sym *lc = sym_new();
            snprintf(lc->name, sizeof lc->name, "linage-counter");
            lc->line = t->line; lc->level = 77; lc->usage = U_COMP5; lc->has_usage = 1;
            lc->has_pic = 1; snprintf(lc->pic, sizeof lc->pic, "9(9)"); pic_analyse(lc->pic, &lc->pi);
            lc->size = 4; lc->lin_file = (int)(f - g_files);
            f = &g_files[lc->lin_file];              /* sym_new may have moved nothing of files; keep f */
            f->lin_counter_sym = sym_idx(lc);
            continue;
        }
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
    for (int i = g_file_base; i < g_nfile; i++) if (!strcmp(g_files[i].report_name, r->name)) r->file = i;
    if (r->file < 0) die_at(line, "no FD says REPORT IS %s", r->name);
    /* a print file SELECTed without ORGANIZATION is line sequential: that
     * is what GnuCOBOL made of gl036's, and its .prn is the oracle */
    if (!g_files[r->file].org_given && g_files[r->file].org == COB_ORG_SEQ) g_files[r->file].org = COB_ORG_LINESEQ;
    /* (a print file of another organization takes each line as a record) */
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
        if (accept_word("footing")) { if (cur()->kind != T_NUM) die_at(t->line, "expected a number after FOOTING"); r->footing = atoi(cur()->s); advance(); continue; }
        if (accept_word("control") || accept_word("controls")) die_at(t->line, "CONTROL is not implemented (after v1; majesty's totals are Procedure Division items)");
        if (accept_word("code")) die_at(t->line, "the CODE clause is not implemented");
        die_at(t->line, "unexpected %s in RD %s", tok_desc(t), r->name);
    }
    expect_period();
    /* no PAGE clause: no page control -- one endless page (the runtime
     * pads nothing and never ends it) */
    if (!r->page_limit) { r->heading = 1; r->first_detail = 1; r->last_detail = 1 << 30; r->footing = 1 << 30; }
    if (!r->heading) r->heading = 1;
    if (!r->first_detail) r->first_detail = r->heading;
    if (!r->last_detail) r->last_detail = r->footing ? r->footing : r->page_limit;
    if (!r->footing) r->footing = r->page_limit;
    /* LINE-COUNTER and PAGE-COUNTER: four-byte unsigned cells of the report block */
    for (int which = 0; which < 2; which++) {
        Sym *c = sym_new();
        snprintf(c->name, sizeof c->name, which ? "page-counter" : "line-counter");
        c->line = line; c->level = 77; c->usage = U_COMP5; c->has_usage = 1;
        c->has_pic = 1; snprintf(c->pic, sizeof c->pic, "9(9)"); pic_analyse(c->pic, &c->pi);
        c->size = 4; c->offset = which ? 24 : 20; c->rep_ctr = (int)(r - g_reports);
        r = &g_reports[c->rep_ctr];
        if (which) r->pc_sym = sym_idx(c); else r->lc_sym = sym_idx(c);
    }

    /* groups: 01 [name] with TYPE, and every entry's clauses in any order
     * (X3.23 VIII-7): LINE begins a line of the group (on the 01 too),
     * COLUMN / PICTURE / SOURCE / VALUE make the entry a printable field
     * of the current line (an entry may carry both -- the elementary
     * report group RW101A and RW301M write) */
    while (cur()->kind == T_NUM && !strcmp(cur()->s, "01")) {
        advance();
        if (r->ng == r->gcap) { r->gcap = r->gcap ? r->gcap * 2 : 8; r->g = realloc(r->g, r->gcap * sizeof *r->g); }
        RGroup *g = &r->g[r->ng++];
        memset(g, 0, sizeof *g);
        g->line = cur()->line;
        int has_type = 0, first = 1;
        static const char *clause_words[] = { "type", "line", "next", "column", "pic", "picture", "source", "value", "just", "justified", "blank", "sum", "group", "usage", "display", NULL };
        for (;;) {
            int eline = cur()->line, lvl = 1;
            if (!first) {
                if (cur()->kind != T_NUM || !strcmp(cur()->s, "01")) break;
                lvl = parse_level(); advance();
                if (lvl < 2 || lvl > 49) die_at(eline, "bad level %d in report group '%s'", lvl, g->name);
            }
            if (cur()->kind == T_WORD) {
                int is_clause = 0;
                for (int k = 0; clause_words[k]; k++) if (at_word(clause_words[k])) is_clause = 1;
                if (!is_clause) { if (first) snprintf(g->name, sizeof g->name, "%s", cur()->s); advance(); }   /* a name */
            }
            /* the entry's clauses */
            int has_line = 0, labs = 0, lplus = 0, is_field = 0;
            RField fd; memset(&fd, 0, sizeof fd); fd.line = eline;
            while (cur()->kind != T_PERIOD) {
                Tok *t = cur();
                if (accept_word("type")) {
                    if (!first) die_at(t->line, "TYPE belongs on the 01 of report group '%s'", g->name);
                    accept_word("is");
                    if (accept_word("page")) { if (accept_word("heading")) g->type = RG_PAGE_HEADING; else if (accept_word("footing")) g->type = RG_PAGE_FOOTING; else die_at(t->line, "TYPE PAGE: HEADING or FOOTING"); }
                    else if (accept_word("ph")) g->type = RG_PAGE_HEADING;
                    else if (accept_word("pf")) g->type = RG_PAGE_FOOTING;
                    else if (accept_word("detail") || accept_word("de")) g->type = RG_DETAIL;
                    else if (at_word("report") || at_word("rh") || at_word("rf") || at_word("control") || at_word("ch") || at_word("cf"))
                        die_at(t->line, "TYPE %s groups are not implemented (PAGE HEADING, PAGE FOOTING and DETAIL are)", cur()->s);
                    else die_at(t->line, "unknown report group TYPE %s", cur()->s);
                    has_type = 1;
                    continue;
                }
                if (accept_word("line")) {
                    accept_word("number"); accept_word("is");
                    if (accept_word("plus")) { if (cur()->kind != T_NUM) die_at(t->line, "expected a number after LINE PLUS"); lplus = atoi(cur()->s); advance(); }
                    else if (at_op("+")) { advance(); if (cur()->kind != T_NUM) die_at(t->line, "expected a number after LINE +"); lplus = atoi(cur()->s); advance(); }
                    else if (cur()->kind == T_NUM) {
                        if (cur()->s[0] == '+') lplus = atoi(cur()->s + 1);      /* "+1" read as a signed literal */
                        else if (cur()->s[0] == '-') die_at(t->line, "LINE cannot be negative");
                        else labs = atoi(cur()->s);
                        advance();
                    } else if (accept_word("next")) die_at(t->line, "LINE NEXT PAGE is not implemented");
                    else die_at(t->line, "expected a line number after LINE");
                    if (!labs && !lplus) die_at(t->line, "LINE needs a number");
                    if (r->page_limit && labs > r->page_limit) die_at(t->line, "LINE %d is past PAGE LIMIT %d", labs, r->page_limit);
                    has_line = 1;
                    continue;
                }
                if (accept_word("next")) die_at(t->line, "NEXT GROUP is not implemented");
                if (accept_word("column")) {
                    accept_word("number"); accept_word("is");
                    if (cur()->kind != T_NUM) die_at(t->line, "expected a number after COLUMN");
                    fd.column = atoi(cur()->s); advance(); is_field = 1;
                    continue;
                }
                if (accept_word("pic") || accept_word("picture")) {
                    accept_word("is");
                    if (cur()->kind != T_PIC) die_at(t->line, "expected a PICTURE character-string");
                    fd.has_pic = 1;
                    snprintf(fd.pic, sizeof fd.pic, "%s", cur()->s);
                    if (pic_analyse(fd.pic, &fd.pi) < 0) die_at(t->line, "report field: %s", fd.pi.err);
                    advance(); is_field = 1;
                    continue;
                }
                if (accept_word("source")) {
                    accept_word("is");
                    if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name after SOURCE");
                    /* keep the reference's position: parse_ref reads it at GENERATE, when every item is declared */
                    fd.has_source = 1; fd.source_tp = g_tp; advance();
                    while (at_word("of") || at_word("in")) { advance(); if (cur()->kind == T_WORD) advance(); }
                    while (cur()->kind == T_LP) {
                        int depth = 0;
                        do {
                            if (cur()->kind == T_LP) depth++;
                            else if (cur()->kind == T_RP) depth--;
                            else if (cur()->kind == T_PERIOD || cur()->kind == T_EOF) die_at(t->line, "unbalanced parentheses in SOURCE");
                            advance();
                        } while (depth > 0);
                    }
                    is_field = 1;
                    continue;
                }
                if (accept_word("value")) {
                    accept_word("is");
                    if (cur()->kind != T_STR && cur()->kind != T_NUM) die_at(t->line, "VALUE in a report field needs a literal");
                    fd.value = cur(); advance(); is_field = 1;
                    continue;
                }
                if (accept_word("just") || accept_word("justified")) { accept_word("right"); fd.just = 1; is_field = 1; continue; }
                if (accept_word("blank")) { accept_word("when"); accept_word("zero"); accept_word("zeros"); fd.blank_zero = 1; is_field = 1; continue; }
                if (accept_word("usage")) { accept_word("is"); expect_word("display"); continue; }
                if (accept_word("display")) continue;
                if (accept_word("sum")) die_at(t->line, "SUM is not implemented (after v1)");
                if (accept_word("group")) die_at(t->line, "GROUP INDICATE is not implemented (after v1)");
                die_at(t->line, "unexpected %s in report group '%s'", tok_desc(t), g->name);
            }
            expect_period();
            if (first && !has_type) die_at(g->line, "report group '%s' needs a TYPE", g->name);
            if (has_line) {
                if (g->nl == g->lcap) { g->lcap = g->lcap ? g->lcap * 2 : 4; g->l = realloc(g->l, g->lcap * sizeof *g->l); }
                RLine *ln = &g->l[g->nl++];
                memset(ln, 0, sizeof *ln);
                ln->line = eline; ln->abs = labs; ln->plus = lplus;
            }
            if (is_field) {
                if (!g->nl) die_at(eline, "a printable entry of report group '%s' before any LINE", g->name);
                RLine *ln = &g->l[g->nl - 1];
                if (!fd.has_pic && fd.value && fd.value->kind == T_STR) {
                    /* VALUE without PICTURE: an alphanumeric of the literal's width */
                    fd.has_pic = 1;
                    snprintf(fd.pic, sizeof fd.pic, "x(%d)", fd.value->len > 0 ? fd.value->len : 1);
                    if (pic_analyse(fd.pic, &fd.pi) < 0) die_at(eline, "report field: %s", fd.pi.err);
                }
                if (!fd.has_pic) die_at(eline, "a report field needs a PICTURE");
                if (fd.has_source == !!fd.value) die_at(eline, "a report field needs exactly one of SOURCE and VALUE");
                if (!fd.column) fd.column = ln->nf ? ln->f[ln->nf - 1].column + ln->f[ln->nf - 1].pi.bytes : 1;
                if (ln->nf == ln->fcap) { ln->fcap = ln->fcap ? ln->fcap * 2 : 8; ln->f = realloc(ln->f, ln->fcap * sizeof *ln->f); }
                ln->f[ln->nf++] = fd;
            }
            first = 0;
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
            if (g_udepth) die_at(cur()->line, "a REPORT SECTION in a contained program is not implemented");
            advance(); advance(); expect_period();
            while (at_word("rd")) parse_rd();
            continue;
        }
        if (at_word("screen") && is_word(peek(1), "section")) {
            if (g_udepth) die_at(cur()->line, "a SCREEN SECTION in a contained program is not implemented");
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
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0) continue;
        if (s->is_linkage || s->is_external) {
            emit("\t.p2align 2");
            emit("%s:\t# %s %02d %s (%d bytes %s)", s->label, s->is_linkage ? "linkage" : "external", s->level, s->name, s->image_size,
                 s->is_linkage ? "at the caller's" : "shared by name");
            emit("\t.word 0");
            continue;
        }
        emit("\t.p2align 3");
        emit("%s:\t# %02d %s (%d bytes)", s->label, s->level, s->name, s->image_size);
        emit_bytes(s->image, s->image_size);
        /* the record's initial state, for CANCEL */
        emit("\t.section .rodata");
        emit("\t.p2align 3");
        emit("%s_i:", s->label);
        emit_bytes(s->image, s->image_size);
        emit("\t.data");
    }
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        emit("\t.p2align 2");
        emit(".Lf%d_%d:\t# %s", f->unit, i, f->name);
        emit("\t.byte %d,%d,%d,0", f->org, f->access, f->optional);
        emit("\t.word 0");
        if (f->rec >= 0 && !f->external) emit("\t.word %s", g_sym[g_sym[f->rec].record].label); else emit("\t.word 0");   /* an EXTERNAL file's record area is set at entry */
        emit("\t.word %d", f->recsize);
        if (f->status_sym && !g_sym[f->status_sym->record].is_linkage && !g_sym[f->status_sym->record].is_external)
            emit("\t.word %s+%d", g_sym[f->status_sym->record].label, f->status_sym->offset);
        else emit("\t.word 0");                            /* a LINKAGE or EXTERNAL status item: its address is stored at entry */
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
        if (f->relkey_sym) { emit("\t.word %s+%d", g_sym[f->relkey_sym->record].label, f->relkey_sym->offset); emit("\t.word .Ld%d", sym_desc(f->relkey_sym)); }
        else { emit("\t.word 0"); emit("\t.word 0"); }
        emit("\t.word 0");                  /* rel_pos, rel_last: the runtime's */
        emit("\t.word 0");
        emit("\t.word 0");                 /* (use_para, use_modes: the compiler now emits the USE choice itself) */
        emit("\t.word .Luse%d", g_unit);
        emit("\t.word 0");
        emit("\t.word 0");                  /* locked (CLOSE WITH LOCK) */
        emit("\t.word 0");                  /* eof_seen */
        emit("\t.word 0");                  /* fpos */
        if (f->nalt) emit("\t.word .Lak%d_%d", g_unit, i); else emit("\t.word 0");   /* ALTERNATE RECORD KEYs */
        emit("\t.word %d", f->nalt);
        if (f->linage) emit("\t.word .Llin%d_%d", g_unit, i); else emit("\t.word 0");   /* LINAGE: lines/footing/top/bottom */
        for (int w = 0; w < 7; w++) emit("\t.word 0");    /* lin_lines lin_foot lin_top lin_bot lin_counter lin_eop lin_needs_top */
        emit("\t.word 0");                                 /* saved_status (EXTERNAL) */
        if (f->external) { emit(".Lfx%d_%d:\t# the shared connector of EXTERNAL %s", f->unit, i, f->name); emit("\t.word 0"); }
    }
    emit("\t.p2align 2");
    emit(".Luse%d:\t# USE sections by open mode", g_unit);
    for (int m = 0; m < 5; m++) emit("\t.word 0");
    if (g_collate >= 0) {
        emit(".Lcoll%d:\t# PROGRAM COLLATING SEQUENCE %s: rank of each character", g_unit, g_alphabet[g_collate].name);
        emit_bytes(g_alphabet[g_collate].rank, 256);
    }
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->linage) continue;
        emit("\t.p2align 2");
        emit(".Llin%d_%d:\t# LINAGE of %s: lines, footing, top, bottom -- literal, item, descriptor", g_unit, i, f->name);
        for (int w = 0; w < 4; w++) {
            emit("\t.word %ld", f->lin_lit[w]);
            if (f->lin_sym[w]) { emit("\t.word %s+%d", g_sym[f->lin_sym[w]->record].label, f->lin_sym[w]->offset); emit("\t.word .Ld%d", sym_desc(f->lin_sym[w])); }
            else { emit("\t.word 0"); emit("\t.word 0"); }
        }
    }
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->nalt) continue;
        emit("\t.p2align 2");
        emit(".Lak%d_%d:\t# ALTERNATE RECORD KEYs of %s", g_unit, i, f->name);
        for (int a = 0; a < f->nalt; a++) { emit("\t.word %d", f->alt[a].sym->offset); emit("\t.word %d", f->alt[a].sym->size); emit("\t.word %d", f->alt[a].dups); }
    }
    for (int i = 0; i < g_nsorttab; i++) {
        SortTab *t = &g_sorttab[i];
        emit("\t.p2align 2");
        emit(".Lsk%d_%d:\t# SORT keys", g_unit, t->id);
        for (int k = 0; k < t->nk; k++) { emit("\t.word %d", t->k[k].offset); emit("\t.word .Ld%d", t->k[k].desc); emit("\t.word %d", t->k[k].descending); }
    }
    g_nsorttab = 0;
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
        emit("\t.word .Lf%d_%d", g_files[r->file].unit, r->file);
        emit("\t.word %d", r->page_limit); emit("\t.word %d", r->heading);
        emit("\t.word %d", r->first_detail); emit("\t.word %d", r->last_detail);
        emit("\t.word 0"); emit("\t.word 0"); emit("\t.word 0");    /* line_counter (20), page_counter (24), body_seen */
        emit("\t.word %d", r->footing); emit("\t.word 0");           /* footing, page_started */
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
        g_nsym = 0; g_nfile = 0; g_npara = 0; g_nreport = 0; g_nscreen = 0; g_nclass = 0; g_nswitch = 0; g_nalphabet = 0; g_nmnemonic = 0; g_last_item = -1;
        g_nsame_groups = 0; g_collate = -1; g_collate_name[0] = 0; g_lowval = 0x00; g_highval = 0xFF; g_cur_fd = -1; g_in_linkage = 0;
        g_sym_base = g_file_base = g_para_base = 0; g_udepth = 0; g_nuse = 0; g_initial = 0;
        parse_identification_division();
        parse_environment_division();
        parse_data_division();
        if (!at_word("procedure")) die_at(cur()->line, "expected PROCEDURE DIVISION, found %s", tok_desc(cur()));
        parse_procedure_division();
        emit_unit_data();
        if (cur()->kind == T_EOF) break;
        if (!g_saw_end_program) die_at(cur()->line, "unexpected %s after the program (a further program needs END PROGRAM before it)", tok_desc(cur()));
        g_unit = ++g_unit_counter;
    }
    emit_rodata();
    relax_branches();
    fclose(g_out);
    return 0;
}
