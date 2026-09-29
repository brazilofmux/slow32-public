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
/* localtime()'s tm_gmtoff (FUNCTION WHEN-COMPILED's zone offset) is a BSD
 * extension that POSIX only standardised in 2024.  Apple's headers expose it
 * under -std=c99; glibc's hide it unless a feature-test macro asks, so
 * without this the compiler does not build on Linux at all. */
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#endif
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <unistd.h>
#include <ctype.h>
#include <strings.h>
#include <sys/mman.h>
#include "picture.h"
#include "../libcob/cobrt.h"
#include "../../common/s32utf.h"   /* the one Unicode model: coding, width, clusters (cobol ISSUES-94) */

#define VERSION "0.63 (stage 63: IF module)"

/* ====================================================================== */
/* Diagnostics                                                             */
/* ====================================================================== */

static const char *g_file = "?";
static const char *g_tok_file = "?";    /* the file being tokenized (a copybook, or the source) */
static int g_free = 0;              /* -free: majesty; default is fixed */
static const char *diag_file(int line);
static int g_module = 0;            /* -m: no main entry; every unit is a subprogram */
static int g_std;                   /* -std=85 or 2002; set in main (defined with the program state) */
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
typedef struct { char name[64]; int native, used; unsigned char rank[256]; } Alphabet;   /* rank: the collating position of each character; used: a SORT names it, its table is emitted */
static Alphabet g_alphabet[16];
static int g_nalphabet;
static int g_collate = -1;                  /* PROGRAM COLLATING SEQUENCE: an alphabet index, -1 native */
static char g_collate_name[64];
static char g_crt_status_name[64];   /* SPECIAL-NAMES CRT STATUS IS name */
/* g_lowval / g_highval (declared with fig_byte): LOW-VALUE / HIGH-VALUE under the program collating sequence */

/* I-O-CONTROL SAME RECORD AREA FOR f1 f2 ...: the files share one record
 * area, so a record read from one is the record of the others */
static int g_same[8][16], g_nsame[8], g_nsame_groups;

/* SPECIAL-NAMES SYSIN|SYSOUT|CONSOLE|SYSERR|FORMFEED IS mnemonic-name:
 * kind 1 the console for ACCEPT, 2 the console for DISPLAY, 3 a page */
typedef struct { char name[64]; int kind; } Mnemonic;
/* The COBOL 85 reserved words (X3.23-1985 as GnuCOBOL's -std=cobol85
 * lists them, 348), sorted for bsearch.  A reserved word is never a
 * user-defined word; user_word() refuses one where a program names a
 * data item, index, file, paragraph or section, or a SPECIAL-NAMES
 * class, alphabet, symbolic character or mnemonic (cobol ISSUES-43). */
static const char *const g_rw85[] = {
    "accept", "access", "add", "advancing", "after", "all", "alphabet",
    "alphabetic", "alphabetic-lower", "alphabetic-upper", "alphanumeric",
    "alphanumeric-edited", "also", "alter", "alternate", "and", "any",
    "are", "area", "areas", "ascending", "assign", "at", "author", "before",
    "binary", "binary-sequential", "blank", "block", "bottom", "by", "call",
    "cancel", "cd", "cf", "ch", "character", "characters", "class",
    "clock-units", "close", "cobol", "code", "code-set", "collating",
    "column", "comma", "common", "communication", "comp", "computational",
    "compute", "configuration", "contains", "content", "continue",
    "control", "controls", "converting", "copy", "corr", "corresponding",
    "count", "currency", "data", "date", "date-compiled", "date-written",
    "day", "day-of-week", "de", "debug-item", "debugging", "decimal-point",
    "declaratives", "delete", "delimited", "delimiter", "depending",
    "descending", "destination", "detail", "disable", "display", "divide",
    "division", "down", "duplicates", "dynamic", "egi", "else", "emi",
    "enable", "end", "end-add", "end-call", "end-compute", "end-delete",
    "end-divide", "end-evaluate", "end-if", "end-multiply", "end-of-page",
    "end-perform", "end-read", "end-receive", "end-return", "end-rewrite",
    "end-search", "end-start", "end-string", "end-subtract", "end-unstring",
    "end-write", "enter", "environment", "eop", "equal", "error", "esi",
    "evaluate", "every", "exception", "exit", "extend", "external", "false",
    "fd", "file", "file-control", "filler", "final", "first", "footing",
    "for", "from", "function", "generate", "giving", "global", "go",
    "greater", "group", "heading", "high-value", "high-values", "i-o",
    "i-o-control", "identification", "if", "in", "index", "indexed",
    "indicate", "initial", "initialize", "initiate", "input",
    "input-output", "inspect", "installation", "internal", "into",
    "invalid", "is", "just", "justified", "key", "label", "last", "leading",
    "left", "length", "less", "limit", "limits", "linage", "linage-counter",
    "line", "line-counter", "line-sequential", "lines", "linkage", "lock",
    "low-value", "low-values", "memory", "merge", "message", "mode",
    "modules", "move", "multiple", "multiply", "native", "negative", "next",
    "no", "not", "number", "numeric", "numeric-edited", "object-computer",
    "occurs", "of", "off", "omitted", "on", "open", "optional", "or",
    "order", "organization", "other", "output", "overflow",
    "packed-decimal", "padding", "page", "page-counter", "perform", "pf",
    "ph", "pic", "picture", "plus", "pointer", "position", "positive",
    "printing", "procedure", "procedures", "proceed", "program",
    "program-id", "purge", "queue", "quote", "quotes", "random", "rd",
    "read", "receive", "record", "records", "redefines", "reel",
    "reference", "references", "relative", "release", "remainder",
    "removal", "renames", "replace", "replacing", "report", "reporting",
    "reports", "rerun", "reserve", "reserved", "reset", "return",
    "reversed", "rewind", "rewrite", "rf", "rh", "right", "rounded", "run",
    "same", "sd", "search", "section", "security", "segment",
    "segment-limit", "select", "send", "sentence", "separate", "sequence",
    "sequential", "set", "sign", "size", "sort", "sort-merge", "source",
    "source-computer", "space", "spaces", "special-names", "standard",
    "standard-1", "standard-2", "start", "status", "stop", "string",
    "sub-queue-1", "sub-queue-2", "sub-queue-3", "subtract", "sum",
    "suppress", "symbolic", "sync", "synchronized", "table", "tallying",
    "tape", "terminal", "terminate", "test", "text", "than", "then",
    "through", "thru", "time", "times", "to", "top", "trailing", "true",
    "type", "unit", "unstring", "until", "up", "upon", "usage", "use",
    "using", "value", "values", "varying", "when", "with", "words",
    "working-storage", "write", "zero", "zeroes", "zeros",
};
static int rw_cmp(const void *a, const void *b) { return strcmp(*(const char *const *)a, *(const char *const *)b); }
static int is_reserved85(const char *w)
{
    char lw[64]; int i = 0;
    for (; w[i] && i < 63; i++) lw[i] = (char)tolower((unsigned char)w[i]);
    lw[i] = 0;
    const char *k = lw;
    return bsearch(&k, g_rw85, sizeof g_rw85 / sizeof *g_rw85, sizeof *g_rw85, rw_cmp) != 0;
}
static Mnemonic g_mnemonic[16];
static int g_nmnemonic;
static int mnemonic_kind(const char *name)
{
    for (int i = 0; i < g_nmnemonic; i++) if (!strcmp(g_mnemonic[i].name, name)) return g_mnemonic[i].kind;
    return 0;
}

/* Errors (cobol ISSUES-41).  An error inside a sentence or a data entry
 * is reported and the parse resumes after it: those two loops set
 * g_recover, die_at jumps back to them, and the sentence or entry is
 * dropped.  Anywhere else an error is still the end.  Once anything has
 * failed nothing is generated -- fail() removes the partial output --
 * and a cap stops a cascade.  The recipe is cobc370's #41. */
#define MAX_ERRORS 30
static jmp_buf *g_recover;           /* the loop that resumes after an error, or NULL */
static int g_nerrors;
static FILE *g_out;
static const char *g_out_path;
static void fail(void)
{
    if (g_out) { fclose(g_out); g_out = NULL; if (g_out_path) unlink(g_out_path); }
    exit(1);
}
static void die_at(int line, const char *fmt, ...)
{
    va_list ap;
    fprintf(stderr, "%s:%d: error: ", diag_file(line), line);
    va_start(ap, fmt); vfprintf(stderr, fmt, ap); va_end(ap);
    fputc('\n', stderr);
    if (++g_nerrors >= MAX_ERRORS) { fprintf(stderr, "s32-cobc: %d errors; stopping\n", g_nerrors); fail(); }
    if (g_recover) longjmp(*g_recover, 1);
    fail();
}

/* Behavior points (docs/behavior-points.md).  Every place the compiler
 * meets a construct whose treatment depends on the standard year calls
 * bp() with its point; the policy -- silent, warn -- lives here, in one
 * table keyed by -std and -warn-74, never at the site.  Class 'M': COBOL 85
 * changed what the construct means and the 85 meaning is applied (a 74
 * program compiles and silently computes something else).  Class 'O': an
 * obsolete element of the 1985 text, deleted in COBOL 2002 (debugging lines
 * in 2014), accepted here.
 * Class 'N': a word COBOL 85 reserved, used as a name by a 74-era program
 * and accepted as one (user_word).
 * The ids are stable: the docs, the messages and the tests all cite them. */
enum { BP_M1_VARYING_AFTER, BP_M2_ODO_RECEIVE,
       BP_O1_ALTER, BP_O2_COMMENT_ENTRY, BP_O3_STOP_LITERAL, BP_O4_REVERSED,
       BP_O5_MEMORY_SIZE, BP_O6_LABEL_RECORDS, BP_O7_VALUE_OF, BP_O8_DATA_RECORDS,
       BP_O9_ALL_NUMERIC, BP_O10_RERUN, BP_O11_MULTIPLE_FILE, BP_O12_DEBUG_LINES,
       BP_N1_RESERVED_NAME,
       BP_COUNT };
static const struct { const char *id; char cls; const char *msg; } g_bp[BP_COUNT] = {
    { "BP-M1", 'M', "this AFTER item's FROM reads an outer VARYING item: COBOL 85 augments the outer item before "
                    "resetting this one, COBOL 74 did the reverse, so a 74 program's loop bounds change here" },
    { "BP-M2", 'M', "the receiving group holds an OCCURS DEPENDING ON table and takes its maximum length "
                    "(COBOL 85); COBOL 74 used the current length" },
    { "BP-O1", 'O', "ALTER is obsolete in COBOL 85 and deleted in COBOL 2002; use GO TO ... DEPENDING ON or EVALUATE" },
    { "BP-O2", 'O', "comment-entries are obsolete in COBOL 85 and deleted in COBOL 2002; use comment lines" },
    { "BP-O3", 'O', "STOP literal is obsolete in COBOL 85 and deleted in COBOL 2002; DISPLAY the literal" },
    { "BP-O4", 'O', "OPEN ... REVERSED is obsolete in COBOL 85 and deleted in COBOL 2002" },
    { "BP-O5", 'O', "MEMORY SIZE is obsolete in COBOL 85 and deleted in COBOL 2002; it has no effect here" },
    { "BP-O6", 'O', "LABEL RECORDS is obsolete in COBOL 85 and deleted in COBOL 2002; it has no effect here" },
    { "BP-O7", 'O', "VALUE OF is obsolete in COBOL 85 and deleted in COBOL 2002; it has no effect here" },
    { "BP-O8", 'O', "DATA RECORDS is obsolete in COBOL 85 and deleted in COBOL 2002; it has no effect here" },
    { "BP-O9", 'O', "ALL with a literal of more than one character, moved to a numeric or numeric-edited item, is obsolete "
                    "in COBOL 85 and deleted in COBOL 2002; move the digits themselves" },
    { "BP-O10", 'O', "RERUN is obsolete in COBOL 85 and deleted in COBOL 2002; it has no effect here" },
    { "BP-O11", 'O', "MULTIPLE FILE TAPE is obsolete in COBOL 85 and deleted in COBOL 2002; it has no effect here" },
    { "BP-O12", 'O', "debugging lines and WITH DEBUGGING MODE are obsolete in COBOL 85 and 2002 "
                     "and deleted in COBOL 2014; make the line code or a comment" },
    { "BP-N1", 'N', "this name became a reserved word in COBOL 85; accepted for a COBOL 74 program, but rename it" },
};
static int g_warn74;                 /* -warn-74: say where a 74-era program needs updating */
static void bp(int point, int line)
{
    static int last_point = -1, last_line = -1;
    if (!g_warn74) return;
    if (point == last_point && line == last_line) return;     /* one per point per line */
    last_point = point; last_line = line;
    fprintf(stderr, "%s:%d: warning: [%s] %s\n", diag_file(line), line, g_bp[point].id, g_bp[point].msg);
}

/* A user-defined word must not be a reserved word (X3.23-1985).  The
 * exception is a word COBOL 85 newly reserved that 74-era programs
 * really use as a name: the Open Systems suite's payroll programs name
 * data items CLASS and OTHER (PAACEMP, PACHKTBL, PAMANCHK, PAPRECHK), and
 * TRUE, FALSE and ANY were taken with OTHER in 91e6807f.  Those are
 * accepted, as behavior point BP-N1.  Surveyed over majesty, the Open
 * Systems suite, CCVS-85 and the tests: no other reserved word is used
 * as a name anywhere (cobol ISSUES-43). */
static void user_word(const char *w, int line, const char *what)
{
    if (!is_reserved85(w)) return;
    static const char *const n1[] = { "class", "other", "true", "false", "any", NULL };
    for (int i = 0; n1[i]; i++) if (!strcasecmp(w, n1[i])) { bp(BP_N1_RESERVED_NAME, line); return; }
    die_at(line, "'%s' is a reserved word and cannot name %s", w, what);
}

static void *xrealloc(void *p, size_t n);
static void *xmalloc(size_t n)
{
    void *p = calloc(1, n ? n : 1);
    if (!p) { fprintf(stderr, "s32-cobc: out of memory\n"); exit(2); }
    return p;
}
static void *xrealloc(void *p, size_t n)
{
    p = realloc(p, n ? n : 1);
    if (!p) { fprintf(stderr, "s32-cobc: out of memory\n"); exit(1); }
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
 * Free (GnuCOBOL -free, majesty; COBOL 2002 6.4): the whole line is text.
 * Comments: '*' or '/' in column 7 (fixed); '*>' to end of line (both --
 * the floating comment is 2002 but majesty is written with it and it is
 * harmless).  The format is chosen on the command line; under -std=2002 a
 * >>SOURCE FORMAT line changes it for the rest of the text, and a literal
 * may be continued with the floating indicator "- or '- in either format
 * (cobol ISSUES-51). */

typedef struct { char *text; int line; int dbg, dir; } SrcLine;   /* dbg: a D in column 7; dir: a compiler directive kept for the parser */
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
    char *p = buf;
    int free_form = g_free;         /* >>SOURCE FORMAT changes it for the rest of this text */
    char pending = 0;               /* a literal continued by a floating indicator ("- or '-) */
    while (*p) {
        char *e = strchr(p, '\n');
        int len = e ? (int)(e - p) : (int)strlen(p);
        lineno++;
        if (len && p[len - 1] == '\r') len--;
        char *text = NULL; int dbg = 0;
        /* a compiler-directive line (COBOL 2002 7.3): >> as the first
         * non-blank, in free form anywhere, in fixed form from column 7 */
        {
            int from = free_form ? 0 : colb(p, len, 6);
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
                    lines[n].text = xstrndup(t0, tl); lines[n].line = lineno; lines[n].dbg = 0; lines[n].dir = 1;
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
            lines[n].dbg = dbg; lines[n].dir = 0;
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

/* ====================================================================== */
/* Tokenizer                                                               */
/* ====================================================================== */

enum { T_EOF, T_WORD, T_NUM, T_STR, T_PIC, T_PERIOD, T_LP, T_RP, T_COLON, T_OP, T_DIR };   /* T_DIR: a >>TURN, taken out of the stream */

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

static void tokenize_lines(SrcLine *lines, int nlines)
{
    int pic_ctx = 0;    /* after PIC/PICTURE [IS]: the next token is a picture */
    for (int li = 0; li < nlines; li++) {
        const char *t = lines[li].text;
        int line = lines[li].line;
        const char *p = t;
        g_tok_dbg = lines[li].dbg;
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
                if ((n) > 160) die_at(line, "this %s literal has %d positions, more than 160 (%s; 2023 allows 8,191)", what, (int)(n), \
                g_std < 2002 ? "X3.23-1985 nonnumeric literals" : rule); } while (0)
            /* Hexadecimal literal X'..' */
            if ((c == 'x' || c == 'X') && (p[1] == '\'' || p[1] == '"')) {
                char q = p[1];
                const char *s = p + 2, *e = s;
                while (*e && *e != q) e++;
                if (!*e) die_at(line, "unterminated hexadecimal literal");
                int n = (int)(e - s);
                if (n & 1) die_at(line, "hexadecimal literal needs an even number of digits");
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
                Tok *w = push_tok(T_WORD, line, p, (int)(e - p));
                word_lower(w);
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
            die_at(line, "COPY: cannot find '%s' (looked beside the source and in the -I directories, as %s, %s.cpy, %s.cbl, and upper-cased)", name, name, name, name);

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

/* ---- TYPEDEF and TYPE (COBOL 2002 13.18.58, 13.18.57; cobol ISSUES-79) ----
 * The TYPE clause is "as though the data description identified by
 * type-name-1 had been coded in place of the TYPE clause", subordinate
 * level-numbers adjusted (13.18.57.4 rules 1-2): so it is expanded here,
 * over the tokens, before anything is parsed.  A TYPEDEF entry and its
 * subordinates are recorded -- its clauses without TYPEDEF, GLOBAL and the
 * name, its subordinate entries with levels relative to it -- and dropped:
 * a type declaration has no storage (13.18.58.4 rule 2).  Each TYPE [TO]
 * name is replaced by the type's clauses, and its subordinate entries
 * follow the entry.  Types are defined before use, and a type's own
 * TYPE clauses are expanded as it is recorded. */
typedef struct { char name[64]; Tok *clause; int nclause; Tok *sub; int nsub; int *sublvl; int level, strong, key; } TypeDef;
/* strong types (13.18.58, STRONG; cobol ISSUES-80): each strongly-typed
 * group -- the entry using the type, and each group inside it -- gets a
 * marker token carrying a key; the same key is the same type */
static char (*g_strong_key)[80]; static int g_nstrong_key, g_strong_cap;
static int strong_key(const char *k)
{
    for (int i = 0; i < g_nstrong_key; i++) if (!strcmp(g_strong_key[i], k)) return i;
    if (g_nstrong_key == g_strong_cap) { g_strong_cap = g_strong_cap ? g_strong_cap * 2 : 16; g_strong_key = realloc(g_strong_key, (size_t)g_strong_cap * sizeof *g_strong_key); }
    snprintf(g_strong_key[g_nstrong_key], sizeof g_strong_key[0], "%s", k);
    return g_nstrong_key++;
}
static const char *strong_name(int key) { return g_strong_key[key]; }
static int g_type_recording_strong;     /* recording a STRONG type: its TYPE clauses may name strong types */
static TypeDef *g_types; static int g_ntypes, g_typecap;
static Tok *g_xt; static int g_nxt, g_xtcap;
static void xt_push(const Tok *t)
{
    if (g_nxt == g_xtcap) { g_xtcap = g_xtcap ? g_xtcap * 2 : 1024; g_xt = realloc(g_xt, (size_t)g_xtcap * sizeof *g_xt); }
    g_xt[g_nxt++] = *t;
}
static int tok_is(const Tok *t, const char *w) { return t->kind == T_WORD && !strcmp(t->s, w); }
static int tok_level(const Tok *t)
{
    if (t->kind != T_NUM || strlen(t->s) > 2) return -1;
    for (const char *k = t->s; *k; k++) if (!isdigit((unsigned char)*k)) return -1;
    return atoi(t->s);
}
static TypeDef *type_find(const char *name)
{
    for (int i = 0; i < g_ntypes; i++) if (!strcmp(g_types[i].name, name)) return &g_types[i];
    return NULL;
}
static Tok strong_tok(const Tok *like, int key)
{
    Tok t = *like; t.kind = T_WORD; t.s = "\001strong"; t.len = 7; t.orig = 0; t.strong = key + 1;
    return t;
}
static Tok level_tok(const Tok *like, int level)
{
    Tok t = *like; char b[8]; snprintf(b, sizeof b, "%02d", level);
    t.kind = T_NUM; t.s = xstrndup(b, (int)strlen(b)); t.len = (int)strlen(b); t.orig = 0;
    return t;
}
/* the words Report Writer's TYPE clause starts with (13.16.x TYPE) */
static int rw_type_word(const char *w)
{
    static const char *k[] = { "is", "report", "page", "control", "detail", "de", "rh", "ph", "ch", "cf", "pf", "rf", NULL };
    for (int i = 0; k[i]; i++) if (!strcmp(w, k[i])) return 1;
    return 0;
}
/* one entry's tokens [a, e] (e its period) to the output, TYPE clauses
 * expanded; the expansion's subordinate entries follow, at level + rel */
static void type_emit_entry(const Tok *tk, int a, int e, int level)
{
    /* the TYPE clause, if any: its type and its tokens [ti, tj] */
    TypeDef *used = NULL; int ti = -1, tj = -1;
    for (int i = a + 2; i < e; i++) {
        if (!tok_is(&tk[i], "type") || i + 1 >= e) continue;
        int j = i + 1;
        if (tok_is(&tk[j], "to") && j + 1 < e) j++;
        TypeDef *ty = tk[j].kind == T_WORD ? type_find(tk[j].s) : NULL;
        if (!ty && tk[j].kind == T_WORD && (j > i + 1 || !rw_type_word(tk[j].s)))
            /* TYPE TO, or a TYPE that is not Report Writer's: a type-name
             * declared before this entry -- a type does not refer to itself
             * or to one declared later (13.18.58.3 rule 2; ISSUES-94) */
            die_at(tk[j].line, "'%s' is not a type declared before this entry (TYPEDEF)", tk[j].s);
        if (!ty) continue;
        if (used) die_at(tk[i].line, "two TYPE clauses in one entry");
        used = ty; ti = i; tj = j;
    }
    if (!used) { for (int i = a; i <= e; i++) xt_push(&tk[i]); return; }
    /* the level and the name, then the type's clauses, then the entry's
     * own: where both say the same (VALUE), the entry's comes later and
     * is the one used (13.18.57.4 rule 3; cobol ISSUES-94 B9) */
    xt_push(&tk[a]); xt_push(&tk[a + 1]);
    if (used->strong) {
        /* a strong type at level 01, or inside a strong type (13.18.57.3 rule 6) */
        if (level != 1 && !g_type_recording_strong)
            die_at(tk[ti].line, "the strong type '%s' is used only at level 01 or inside a strong type (2023 13.18.57.3 rule 6)", used->name);
        Tok m = strong_tok(&tk[ti], used->key); xt_push(&m);
    }
    if (used->nsub && level != 1 && level != 77) {
        /* a group type is aligned as a level 1 item (rule 2d; B10) */
        Tok m = tk[ti]; m.kind = T_WORD; m.s = "\001lvl1"; m.len = 5; m.orig = 0; xt_push(&m);
    }
    for (int k = 0; k < used->nclause; k++) xt_push(&used->clause[k]);
    for (int i = a + 2; i <= e; i++) if (i < ti || i > tj) xt_push(&tk[i]);
    if (level == 77 && used->nsub) die_at(tk[a].line, "a level 77 item takes an elementary type (2023 13.18.57.3 rule 7)");
    for (int k = 0; k < used->nsub; k++) {
        if (used->sublvl[k] >= 0) {
            int lv = used->sublvl[k] >= 66 ? used->sublvl[k] : level + used->sublvl[k];
            if (lv > 49 && lv < 66) die_at(tk[a].line, "the type '%s' expands past level 49 here, which is not implemented", used->name);
            Tok lt = level_tok(&used->sub[k], lv); xt_push(&lt);
        } else xt_push(&used->sub[k]);
    }
}
static void expand_types(void)
{
    if (g_std < 2002) return;
    int any = 0;
    for (int i = 0; i < g_ntok; i++)
        if (tok_is(&g_tok[i], "typedef") || (tok_is(&g_tok[i], "type") && i + 1 < g_ntok && tok_is(&g_tok[i + 1], "to"))) { any = 1; break; }
    if (!any) return;
    int *map = xmalloc((size_t)(g_ntok + 1) * sizeof *map);
    g_nxt = 0; g_ntypes = 0;
    int in_data = 0;
    for (int i = 0; i < g_ntok; ) {
        Tok *t = &g_tok[i];
        if (tok_is(t, "data") && i + 1 < g_ntok && tok_is(&g_tok[i + 1], "division")) in_data = 1;
        if (tok_is(t, "procedure") && i + 1 < g_ntok && tok_is(&g_tok[i + 1], "division")) in_data = 0;
        int lv = tok_level(t);
        int at_entry = in_data && lv >= 1 && i > 0 && g_tok[i - 1].kind == T_PERIOD && i + 1 < g_ntok;
        if (!at_entry) { map[i] = g_nxt; xt_push(t); i++; continue; }
        int e = i; while (e < g_ntok && g_tok[e].kind != T_PERIOD && g_tok[e].kind != T_EOF) e++;
        int td = -1;
        for (int k = i + 1; k < e; k++) if (tok_is(&g_tok[k], "typedef")) td = k;
        if (td < 0) {
            for (int k = i; k <= e && k < g_ntok; k++) map[k] = g_nxt;
            type_emit_entry(g_tok, i, e, lv);
            i = e + 1;
            continue;
        }
        /* a type declaration: recorded, not emitted */
        int strong = td + 1 < e && tok_is(&g_tok[td + 1], "strong");
        if (g_tok[i + 1].kind != T_WORD) die_at(t->line, "a TYPEDEF entry needs a name");
        if (lv != 1 && lv != 77) die_at(t->line, "a type declaration here is a level 01 or 77 entry");
        if (g_ntypes == g_typecap) { g_typecap = g_typecap ? g_typecap * 2 : 16; g_types = realloc(g_types, (size_t)g_typecap * sizeof *g_types); }
        TypeDef *ty = &g_types[g_ntypes]; memset(ty, 0, sizeof *ty);
        snprintf(ty->name, sizeof ty->name, "%s", g_tok[i + 1].s);
        ty->level = lv; ty->strong = strong;
        if (strong) ty->key = strong_key(ty->name);
        g_type_recording_strong = strong;
        /* its own clauses, TYPE clauses expanded, without TYPEDEF, IS
         * before it, and GLOBAL */
        int save = g_nxt;
        type_emit_entry(g_tok, i, e, lv);
        int n = g_nxt - save;
        ty->clause = xmalloc((size_t)(n + 1) * sizeof *ty->clause);
        for (int k = save + 2; k < g_nxt - 1; k++) {           /* past the level and name, before the period */
            Tok *c = &g_xt[k];
            if (tok_is(c, "typedef") || tok_is(c, "global") || (strong && tok_is(c, "strong"))) continue;
            if (tok_is(c, "is") && k + 1 < g_nxt && tok_is(&g_xt[k + 1], "typedef")) continue;
            ty->clause[ty->nclause++] = *c;
        }
        /* a TYPE clause inside a type: its subordinates follow, already emitted after the period */
        int tail = g_nxt;
        for (int k = save; k < g_nxt; k++) if (g_xt[k].kind == T_PERIOD) { tail = k + 1; break; }
        /* the subordinate entries: to the next entry at this level or above */
        int j = e + 1, subst = tail;              /* a TYPE clause's expansion is its first subordinates */
        while (j < g_ntok) {
            int sl = tok_level(&g_tok[j]);
            if (sl < 0 || g_tok[j - 1].kind != T_PERIOD) break;
            if (sl != 66 && sl != 88 && sl <= lv) break;
            if (sl == 77) break;
            int se = j; while (se < g_ntok && g_tok[se].kind != T_PERIOD && g_tok[se].kind != T_EOF) se++;
            type_emit_entry(g_tok, j, se, sl);
            j = se + 1;
        }
        int ns = g_nxt - subst;
        ty->sub = xmalloc((size_t)(ns + 1) * sizeof *ty->sub); ty->sublvl = xmalloc((size_t)(ns + 1) * sizeof *ty->sublvl);
        for (int k = 0; k < ns; k++) {
            Tok *c = &g_xt[subst + k];
            ty->sub[ty->nsub] = *c;
            /* a level-number opens each subordinate entry: made relative */
            int at = (subst + k == subst) || g_xt[subst + k - 1].kind == T_PERIOD;
            int l2 = at ? tok_level(c) : -1;
            ty->sublvl[ty->nsub] = l2 < 0 ? -1 : (l2 == 66 || l2 == 88) ? l2 : l2 - lv;
            ty->nsub++;
        }
        g_type_recording_strong = 0;
        if (strong && !ns) die_at(t->line, "TYPEDEF STRONG: '%s' is not a group (2023 13.18.58.3 rule 1)", ty->name);
        if (strong) {
            /* each subordinate group of a strong type is strong too: a marker,
             * keyed type#n, after its level and name */
            Tok *ns2 = xmalloc((size_t)(ty->nsub * 2 + 1) * sizeof *ns2); int *nl2 = xmalloc((size_t)(ty->nsub * 2 + 1) * sizeof *nl2);
            int m = 0, gn = 0;
            for (int k = 0; k < ty->nsub; k++) {
                ns2[m] = ty->sub[k]; nl2[m] = ty->sublvl[k]; m++;
                if (ty->sublvl[k] >= 0 && ty->sublvl[k] < 66 && k + 1 < ty->nsub) {
                    int e2 = k; while (e2 < ty->nsub && ty->sub[e2].kind != T_PERIOD) e2++;
                    int nextlv = -1;
                    for (int q = e2 + 1; q < ty->nsub; q++) if (ty->sublvl[q] >= 0) { nextlv = ty->sublvl[q]; break; }
                    int already = 0;
                    for (int q = k + 1; q < e2; q++) if (ty->sub[q].strong) already = 1;
                    if (nextlv > ty->sublvl[k] && nextlv < 66 && !already && k + 1 < e2) {
                        char key[80]; snprintf(key, sizeof key, "%s#%d", ty->name, ++gn);
                        ns2[m] = ty->sub[k + 1]; nl2[m] = -1; m++;         /* the name */
                        ns2[m] = strong_tok(&ty->sub[k], strong_key(key)); nl2[m] = -1; m++;
                        k++;
                    }
                }
            }
            ty->sub = ns2; ty->sublvl = nl2; ty->nsub = m;
        }
        g_nxt = save;                                   /* no storage: nothing of it stays */
        g_ntypes++;
        for (int k = i; k < j; k++) map[k] = g_nxt;
        i = j;
    }
    map[g_ntok] = g_nxt;
    for (int d = 0; d < g_ndir; d++) g_dir[d].pos = g_dir[d].pos <= g_ntok ? map[g_dir[d].pos] : g_nxt;
    free(g_tok); g_tok = g_xt; g_ntok = g_nxt; g_tcap = g_xtcap;
    g_xt = NULL; g_nxt = g_xtcap = 0;
    free(map);
}

static void tokenize(void)
{
    g_tok_file = g_file;
    strip_comment_entries(g_lines, g_nlines);
    tokenize_lines(g_lines, g_nlines);
    expand_copies(0);
    apply_replace();
    {
        int w = 0;
        /* Debugging lines (D in column 7) are compiled under WITH DEBUGGING
         * MODE and are comments otherwise (X3.23-1985 VI-10, SOURCE-COMPUTER
         * rules 4-5).  The clause is found here, before parsing, because it
         * decides which tokens exist, and is taken for the whole source file:
         * exact for one program and the programs nested in it. */
        int mode = 0, last = -1;
        for (int r = 0; r + 1 < g_ntok; r++)
            if (!g_tok[r].dbg && is_word(&g_tok[r], "debugging") && is_word(&g_tok[r + 1], "mode")) {
                mode = 1; bp(BP_O12_DEBUG_LINES, g_tok[r].line);
            }
        for (int r = 0; r < g_ntok; r++) {
            if (g_tok[r].dbg && g_tok[r].line != last) { last = g_tok[r].line; if (!mode) bp(BP_O12_DEBUG_LINES, last); }
            if (g_tok[r].kind == T_DIR) {
                /* a >>TURN: the parser applies it on reaching this point */
                if (g_ndir == g_dircap) { g_dircap = g_dircap ? 2 * g_dircap : 16; g_dir = realloc(g_dir, (size_t)g_dircap * sizeof *g_dir); }
                g_dir[g_ndir].pos = w; g_dir[g_ndir].tok = g_tok[r]; g_ndir++;
                continue;
            }
            if (mode || !g_tok[r].dbg) g_tok[w++] = g_tok[r];
        }
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

/* SPECIAL-NAMES SYMBOLIC CHARACTERS: figurative constants of the program's
 * own, each a character named by its ordinal position (1-based) in the
 * native character set */
static struct { char name[64]; int byte; } g_symch[32]; static int g_nsymch;
static int symch_find(const char *w)
{
    for (int i = 0; i < g_nsymch; i++) if (!strcmp(g_symch[i].name, w)) return g_symch[i].byte;
    return -1;
}

static int is_figurative(const char *w)
{
    static const char *figs[] = { "space", "spaces", "zero", "zeros", "zeroes",
        "low-value", "low-values", "high-value", "high-values", "quote", "quotes",
        "null", "nulls", NULL };
    for (int i = 0; figs[i]; i++) if (!strcmp(w, figs[i])) return 1;
    return symch_find(w) >= 0;
}

static int g_lowval, g_highval;
static int fig_byte(const char *w)
{
    int sc = symch_find(w);
    if (sc >= 0) return sc;
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
        die_at(t->line, "numeric literal has more than 18 digits%s", g_std >= 2002 ? " -- COBOL 2002's 31 are not implemented" : "");
}

static void numlit_zero(NumLit *n) { memset(n, 0, sizeof *n); n->digits[0] = '0'; n->ndigits = 1; }

static int numlit_is_zero(const NumLit *n)
{
    for (int i = 0; i < n->ndigits; i++) if (n->digits[i] != '0') return 0;
    return 1;
}

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
    U_SINT, U_UINT, U_SSHORT, U_USHORT, U_BCHAR, U_UBCHAR, U_POINTER, U_INDEX,
    U_NATIONAL,                     /* numeric and numeric-edited USAGE NATIONAL (cobol ISSUES-72); PIC N keeps U_DISPLAY */
    U_BIT                           /* boolean USAGE BIT: bits, packed (cobol ISSUES-78) */
};

static const char *usage_name(int u)
{
    static const char *n[] = { "display", "comp", "comp-3", "comp-5", "signed-int",
        "unsigned-int", "signed-short", "unsigned-short", "binary-char",
        "binary-char unsigned", "pointer", "index", "national", "bit" };
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
    int  type_lvl1;                 /* expanded from a group TYPE: aligned as a level 1 item (13.18.57.4 rule 2d) */
    int  size;                      /* one occurrence */
    int  offset;                    /* from the start of the record */
    int  occurs;                    /* 0 = no OCCURS; with DEPENDING ON, the maximum */
    int  nokey; char okey[8][64]; unsigned char okey_desc[8];   /* OCCURS ASCENDING/DESCENDING KEY: names in order, 1 = descending (SEARCH ALL) */
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
    int  is_based;                  /* a BASED entry: reached through a cell SET ADDRESS OF fills, NULL at first (2002 8.6.4) */
    int  is_local;                  /* a LOCAL-STORAGE record: storage is the activation's (COBOL 2002) */
    int  is_ftemp;                  /* a user function's result or BY CONTENT argument, made by the compiler */
    int  nat_usage;                 /* USAGE NATIONAL was written (cobol ISSUES-62) */
    int  natgroup;                  /* GROUP-USAGE NATIONAL, written or inherited: a national group (cobol ISSUES-71) */
    int  bitgroup;                  /* GROUP-USAGE BIT, written (2) or inherited (1): a bit group (cobol ISSUES-78) */
    int  bits, bitoff;              /* USAGE BIT and bit groups: boolean positions, and the first bit's place in the first byte */
    int  strong;                    /* a strongly-typed group: its type key + 1 (cobol ISSUES-80) */
    int  standin;                   /* the FILLER PIC X put in place of an entry refused with an error */
    int  in_natgroup;               /* an elementary item of a national group */
    int  ftemp_scan;                /* ... made while scanning ahead (no code): must never be emitted */
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
static int sym_in_strong(const Sym *s);

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

/* The symbol table never moves.  Sym pointers live the whole compile --
 * in every Ref a statement holds while it parses, in odo_dep_sym, in a
 * file's keys -- and records are still made in the PROCEDURE DIVISION
 * (ftemp_new: a function's result, a BY CONTENT copy, a MOVE's sender).
 * A realloc under them was a use-after-free (a COMPUTE whose function
 * call grew the table, cobol ISSUES-96).  So the address space is
 * reserved once and committed as the table grows; untouched pages cost
 * nothing. */
enum { SYM_RESERVE = 1 << 20 };
static Sym *sym_new(void)
{
    if (g_nsym == g_scap) {
        if (!g_sym) {
            int fl = MAP_PRIVATE | MAP_ANON;
#ifdef MAP_NORESERVE
            fl |= MAP_NORESERVE;
#endif
            void *m = mmap(NULL, (size_t)SYM_RESERVE * sizeof *g_sym, PROT_NONE, fl, -1, 0);
            if (m == MAP_FAILED) { fprintf(stderr, "s32-cobc: cannot reserve the symbol table\n"); exit(1); }
            g_sym = m;
        }
        if (g_scap == SYM_RESERVE) { fprintf(stderr, "s32-cobc: more than %d data items\n", SYM_RESERVE); exit(1); }
        int n = g_scap ? 2 * g_scap : 128;
        if (n > SYM_RESERVE) n = SYM_RESERVE;
        if (mprotect(g_sym, (size_t)n * sizeof *g_sym, PROT_READ | PROT_WRITE)) { fprintf(stderr, "s32-cobc: cannot grow the symbol table\n"); exit(1); }
        g_scap = n;
    }
    Sym *s = &g_sym[g_nsym++];
    memset(s, 0, sizeof *s);
    s->parent = s->child = s->sibling = s->redefines = -1;
    s->desc_id = -1; s->fd = -1; s->idx1 = -1; s->ix_table = -1; s->lin_file = -1; s->rep_ctr = -1;
    return s;
}

static int sym_idx(Sym *s) { return (int)(s - g_sym); }
/* a record reached through a cell holding its address, not by its label:
 * LINKAGE (the caller's), LOCAL-STORAGE (the activation's), EXTERNAL */
static int rec_indirect(const Sym *rec) { return rec->is_linkage || rec->is_local || rec->is_external || rec->is_based; }
static const char *indirect_kind(const Sym *rec) { return rec->is_based ? "BASED" : rec->is_linkage ? "LINKAGE" : rec->is_local ? "LOCAL-STORAGE" : "EXTERNAL"; }

/* name [OF|IN qualifier]...: the unique item that matches */
static void unit_range(int level, int *from, int *to);   /* an ancestor's symbol range */
static const char *file_name_of(int fd);                /* the FD's file-name (File is declared below) */

static char g_poison[64][64]; static int g_npoison;   /* names whose uses fail quietly: data entries dropped after an error, a refused module's registers */

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
                if (hit < 0) {
                    /* the outermost qualifier may be the file whose FD holds the record (SQ207M: PRINT-REC IN PRINT-FILE) */
                    int r = at; while (g_sym[r].parent >= 0) r = g_sym[r].parent;
                    if (g_sym[r].fd >= 0 && !strcmp(file_name_of(g_sym[r].fd), quals[q]) && q == nq - 1) hit = r;
                }
                if (hit < 0) ok = 0; else at = hit;
            }
            if (ok) { found = s; nfound++; }
        }
    }
    if (!nfound) {
        /* a data entry dropped after an error: its uses fail quietly,
         * rather than once each */
        for (int i = 0; i < g_npoison; i++)
            if (!strcmp(g_poison[i], name) && g_recover) longjmp(*g_recover, 1);
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

static char g_progid[64], g_progid_orig[64];    /* the program-name, and as written */

/* ---- files: SELECT + FD ------------------------------------------------ */

typedef struct {
    char name[64], oname[64];        /* the file-name, and as written (EXCEPTION-FILE) */
    int  line, org, access, optional;
    Tok *assign_lit;                 /* ASSIGN TO literal ... */
    char assign_name[64];            /* ... or to a data-name */
    char status_name[64], key_name[64], report_name[64];
    char (*report_more)[64]; int nreport_more;    /* REPORTS ARE: the names after the first */
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
static const char *file_name_of(int fd) { return g_files[fd].name; }
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
    int usage_nat;                      /* USAGE NATIONAL on a numeric or numeric-edited PICTURE */
    char ename[64];                     /* the entry's data-name (a SUM entry's names its counter) */
    int gi;                             /* GROUP INDICATE */
    int has_sum, nsum, sum_tp[8];       /* SUM operands: token positions, resolved when the report is first used */
    int nupon, upon_tp[4];              /* UPON detail-names */
    int reset_tp, reset_final;          /* RESET ON: a control's token position, or FINAL */
    int ctr_sym;                        /* the sum counter item (0 = none) */
    int sum_sym[8], sum_is_ctr[8];      /* resolved operands */
    int upon_g[4];                      /* resolved UPON detail groups (indexes into r->g) */
    int reset_lvl;                      /* resolved: 0 FINAL, 1..nctl; the own CF's level by default */
} RField;

typedef struct {
    int abs, plus, line, np;            /* np: LINE ... NEXT PAGE */
    RField *f; int nf, fcap;
} RLine;

enum { RG_PAGE_HEADING, RG_DETAIL, RG_PAGE_FOOTING,
       RG_REPORT_HEADING, RG_REPORT_FOOTING, RG_CONTROL_HEADING, RG_CONTROL_FOOTING };

typedef struct {
    char name[64];
    int type, line;
    int ctl_tp;                         /* CH/CF: the control reference's token position (0: FINAL) */
    int ctl_level;                      /* resolved: 0 FINAL, 1 most major .. nctl; -1 not a control group */
    int next_kind, next_n;              /* NEXT GROUP: 0 none, 1 integer, 2 PLUS, 3 NEXT PAGE */
    int use_sec;                        /* USE BEFORE REPORTING: the DECLARATIVES section, -1 none */
    RLine *l; int nl, lcap;
} RGroup;

typedef struct {
    char name[64];
    int line, file;                  /* the FD whose REPORT IS names it */
    int page_limit, heading, first_detail, last_detail, footing;
    int lc_sym, pc_sym;              /* the synthetic LINE-COUNTER and PAGE-COUNTER items */
    int nctl, ctl_final;             /* CONTROL: levels 1 (most major) .. nctl; FINAL besides */
    int ctl_sym[8], ctl_clone[8];    /* each control item and its prior-value clone (X3.23 VIII 2.21.4(13)) */
    int ctl_held[8];                 /* where the new value waits while the item holds the prior one */
    int resolved;                    /* SUM operands, UPON, RESET, CH/CF levels resolved at first use */
    RGroup *g; int ng, gcap;
    Tok *code_lit; int code_tp;      /* CODE: the literal, or the identifier's token position (0 none) */
} Report;

/* the report state block's cells past the ones with symbols (cobrt.h) */
#define RW_OFF_FIRST_GEN 40
#define RW_OFF_BRK       44
#define RW_OFF_SUPPRESS  56
#define RW_OFF_GI        60

static Report *g_reports; static int g_nreport, g_rcap;
static int g_report_base;           /* the unit's first report: a contained program's follow its container's (ISSUES-94) */

/* ---- screens: SCREEN SECTION 01s as slot tables ------------------------ */

typedef struct {
    int kind, flags, line, col, width, srcline, fg, bg;
    Tok *value;
    int has_pic; char pic[PIC_MAXPAT]; PicInfo pi; int blank_zero;
    Sym *item;
    int ref_tp, dyn;            /* the reference's token position; dyn: its address is computed at ACCEPT/DISPLAY */
    long stat_off;              /* static references (literal subscripts included): the resolved offset */
int ext, prompt;            /* positioned DISPLAY/ACCEPT: COB_SX_* bits, the PROMPT character */
int natlit;                 /* a VALUE slot's literal is national: its columns are its display width */
int line_tp, col_tp, at_tp; /* LINE / POSITION / AT given as identifiers: token positions, stored at run time */
} SField;

typedef struct { char name[64]; int first, count; } SGroup;   /* a named nested group: a window into the slot table */

typedef struct {
    char name[64];
    int line, blank_screen;
    SField *f; int nf, fcap;
    SGroup *sub; int nsub, subcap;
} Screen;

static Screen *g_screens; static int g_nscreen, g_scrcap;
static int g_screen_base;   /* the unit's screens start here (contained programs append) */

static Screen *screen_find(const char *name)
{
    for (int i = g_screen_base; i < g_nscreen; i++) if (!strcmp(g_screens[i].name, name)) return &g_screens[i];
    return NULL;
}

/* the record label for a screen-name or a named group inside one; the
 * window's slot range comes back for the dynamic-address fill */
static Screen *screen_ref(const char *name, char *lab, size_t n, int *first, int *count)
{
    Screen *sc = screen_find(name);
    if (sc) { snprintf(lab, n, ".Lscr%d_%d", g_unit, (int)(sc - g_screens)); *first = 0; *count = sc->nf; return sc; }
    for (int i = g_screen_base; i < g_nscreen; i++)
        for (int j = 0; j < g_screens[i].nsub; j++)
            if (!strcmp(g_screens[i].sub[j].name, name)) {
                snprintf(lab, n, ".Lscrg%d_%d_%d", g_unit, i, j);
                *first = g_screens[i].sub[j].first; *count = g_screens[i].sub[j].count;
                return &g_screens[i];
            }
    return NULL;
}

static void emit_screen_dyn_fill(Screen *sc, int first, int count);   /* below, after parse_ref exists */

static Report *report_find(const char *name)
{
    for (int i = g_report_base; i < g_nreport; i++) if (!strcmp(g_reports[i].name, name)) return &g_reports[i];
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
static void bwz_check(const char *name, const PicInfo *pi, int bad_usage, int line);
static void sym_finish(Sym *s)
{
    int u = s->usage;
    if (s->is_group) { s->pi.category = PIC_ALPHANUMERIC; return; }
    int native = usage_is_native(u);

    if (!s->has_pic && !native)
        die_at(s->line, "'%s' has no PICTURE clause", s->name);
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
        if (s->occurs && s->odo_dep[0]) die_at(s->line, "'%s': OCCURS DEPENDING ON a USAGE BIT item is not implemented yet", s->name);
        s->bits = pi->bytes; s->size = (s->bits + 7) / 8;
        return;
    }
    switch (u) {
    case U_DISPLAY: case U_NATIONAL:
        s->size = pi->bytes;
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
    case U_BINARY: case U_COMP5:
        if (pi->category != PIC_NUMERIC)
            die_at(s->line, "'%s': USAGE %s needs a numeric PICTURE (2023 13.18.60.3 rule 3)", s->name, usage_name(u));
        s->size = binary_bytes(pi->digits, u);
        break;
    case U_PACKED:
        if (pi->category != PIC_NUMERIC)
            die_at(s->line, "'%s': USAGE PACKED-DECIMAL (COMP-3) needs a numeric PICTURE (2023 13.18.60.3 rule 3)", s->name);
        s->size = pi->digits / 2 + 1;
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
        if ((lv >= 1 && lv <= 49) || lv == 77) {
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
static void parse_data_item1(void)
{
    int line = cur()->line;
    int level = parse_level();
    if (level < 0) die_at(line, "expected a level number, found %s", tok_desc(cur()));
    advance();
    g_entry_level = level;

    if (!((level >= 1 && level <= 49) || level == 66 || level == 77 || level == 88))
        die_at(line, "level number %d is not valid", level);

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
        s->is_cond = 1;
        s->parent = g_last_item;
        if (!(accept_word("value") || accept_word("values")))
            die_at(line, "level 88 '%s' needs a VALUE clause", s->name);
        accept_word("is"); accept_word("are");
        for (;;) {
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
        if (t->strong) { s->strong = t->strong; advance(); continue; }   /* expand_types()'s strong-type marker */
        if (!strcmp(t->s, "\001lvl1")) { s->type_lvl1 = 1; advance(); continue; }   /* ... and its group-type marker */

        if (!strcmp(t->s, "pic") || !strcmp(t->s, "picture")) {
            advance();
            if (cur()->kind != T_PIC) die_at(t->line, "expected a PICTURE character-string");
            if (s->has_pic) die_at(t->line, "'%s' has two PICTURE clauses", s->name);
            s->has_pic = 1;
            snprintf(s->pic, sizeof s->pic, "%s", cur()->s);
            pic_len_check(s->pic, t->line);
            if (nat_picture(s->pic, &s->pi, t->line)) { advance(); continue; }
            if (bool_picture(s->pic, &s->pi, t->line)) { advance(); continue; }
            if (pic_analyse(s->pic, &s->pi) < 0) {
                /* 2002 raised the limit to 31 digits (a gap here: the
                 * arithmetic is 64-bit, docs/refusals.md) */
                if (g_std >= 2002 && !strncmp(s->pi.err, "more than 18 digits", 19))
                    die_at(t->line, "'%s': more than 18 digits -- COBOL 2002's 31 are not implemented", s->name);
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
                die_at(t->line, "'%s': %s", s->name, s->pi.err);
            }
            advance();
            continue;
        }
        if (!strcmp(t->s, "usage")) { advance(); accept_word("is"); t = cur(); if (t->kind != T_WORD) die_at(t->line, "expected a USAGE"); }
        int u = -1;
        if (!strcmp(t->s, "display")) u = U_DISPLAY;
        else if (!strcmp(t->s, "comp") || !strcmp(t->s, "computational") || !strcmp(t->s, "binary")) u = U_BINARY;
        else if (!strcmp(t->s, "comp-3") || !strcmp(t->s, "computational-3") || !strcmp(t->s, "packed-decimal")) u = U_PACKED;
        else if (!strcmp(t->s, "comp-5") || !strcmp(t->s, "computational-5")) u = U_COMP5;
        else if (!strcmp(t->s, "binary-long") || !strcmp(t->s, "binary-short")) {
            /* [SIGNED | UNSIGNED], signed by default (2023 13.18.60.2) */
            int lng = t->s[7] == 'l';
            advance();
            int uns = accept_word("unsigned");
            if (!uns) accept_word("signed");
            u = lng ? (uns ? U_UINT : U_SINT) : (uns ? U_USHORT : U_SSHORT);
            if (s->has_usage) die_at(t->line, "'%s' has two USAGE clauses", s->name);
            s->usage = u; s->has_usage = 1;
            continue;
        }
        else if (!strcmp(t->s, "binary-double"))
            die_at(t->line, "USAGE BINARY-DOUBLE is not implemented (its range needs 19 digits; this compiler's arithmetic holds 18)");
        else if (!strcmp(t->s, "signed-int")) u = U_SINT;
        else if (!strcmp(t->s, "unsigned-int")) u = U_UINT;
        else if (!strcmp(t->s, "signed-short")) u = U_SSHORT;
        else if (!strcmp(t->s, "unsigned-short")) u = U_USHORT;
        else if (!strcmp(t->s, "binary-char")) {
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
        else if (!strcmp(t->s, "pointer")) u = U_POINTER;
        else if (!strcmp(t->s, "index")) u = U_INDEX;
        else if (!strcmp(t->s, "comp-1"))
            u = U_BINARY;   /* RM/COBOL: a binary integer with a PICTURE (S9(4) in two bytes), not a float; the Open Systems suite's COMP-1 items all carry one */
        else if (!strcmp(t->s, "comp-2") || !strcmp(t->s, "float-short") || !strcmp(t->s, "float-long"))
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
        if (!strcmp(t->s, "based")) {
            /* BASED (2002 13.16.5): a template reached through an implicit
             * data-address pointer, NULL until SET ADDRESS OF gives it one */
            if (g_std < 2002) die_at(t->line, "BASED is COBOL 2002; compile with -std=2002");
            if (level != 1 && level != 77) die_at(t->line, "'%s': BASED is for a level 01 or 77 entry here", s->name);
            if (g_in_local) die_at(t->line, "'%s': a BASED entry in LOCAL-STORAGE is not implemented", s->name);
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
        if (!s->is_group && s->usage == U_POINTER && s->level != 1 && !sym_in_strong(s))
            die_at(s->line, "'%s': a USAGE POINTER item is at level 1, or in a strongly-typed group (2023 13.18.60.3 rule 14; 2002 rule 13)", s->name);
        if (s->is_group && s->has_pic && s->standin) s->has_pic = 0;      /* it stood in for a group: no second error */
        if (s->is_group && s->has_pic) die_at(s->line, "'%s' is a group and cannot have a PICTURE", s->name);
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
        const Sym *cv = &g_sym[s->parent];
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
    case U_POINTER: case U_INDEX:
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
            if (sym_in_strong(x)) die_at(s->line, "RENAMES '%s': '%s' is in a strongly-typed group (2023 13.18.57.3 rule 3)", s->name, x->name);
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
        if (f->rec < 0 && !f->report_name[0]) die_at(f->line, "file '%s' has no FD", f->name);
        if (f->assign_name[0]) {
            f->assign_sym = sym_lookup(f->assign_name, NULL, 0, f->line);
            if (rec_indirect(&g_sym[f->assign_sym->record]))
                die_at(f->line, "ASSIGN TO '%s': a %s item cannot name a file", f->assign_name, indirect_kind(&g_sym[f->assign_sym->record]));
            /* a group is alphanumeric by the standard's own rules: the suite
             * builds "GENTBL." + module suffix that way (GitHub #34) */
            if (!f->assign_sym->is_group && f->assign_sym->pi.category == PIC_NUMERIC)
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
            if (rec_indirect(&g_sym[f->dep_sym->record]))
                die_at(f->line, "DEPENDING ON '%s' cannot be a %s item", f->dep_name, indirect_kind(&g_sym[f->dep_sym->record]));
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
                    if (rec_indirect(&g_sym[f->lin_sym[w]->record]))
                        die_at(f->line, "LINAGE: '%s' cannot be a %s item", f->lin_name[w], indirect_kind(&g_sym[f->lin_sym[w]->record]));
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
                if (rec_indirect(&g_sym[k->record]))
                    die_at(f->line, "RELATIVE KEY '%s' cannot be a %s item", f->relkey_name, indirect_kind(&g_sym[k->record]));
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
        if (!s->is_linkage && !s->is_external) init_record(s, i, 1);
    }
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines < 0) continue;
        init_record(&g_sym[s->record], i, 0);
    }
}

/* ====================================================================== */
/* Emitter                                                                 */
/* ====================================================================== */

static int g_nlabel;

/* label is heap-allocated, NOT an array in this struct.  lit_label hands
 * its pointer to callers who hold it while building an Arg list, and
 * g_lit is realloc'd -- an inline array would move out from under them.
 * See the comment on lit_label. */
typedef struct { char *label; unsigned char *bytes; int len; } Lit;
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
    for (char *e = target; *e; e++)                 /* a trailing comment or blank is not the label's name */
        if (*e == ' ' || *e == '\t' || *e == '#') { *e = 0; break; }
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
    /* islong only ever grows, so this terminates in at most one pass per
     * branch; a fixed cap left a long chain half-relaxed (GitHub #22) */
    for (;;) {
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

/* The returned pointer MUST outlive further calls to this function.
 *
 * Callers hold it: opnd_args stores it in an Arg, and a statement builds
 * several Args before emit_args consumes them -- parse_inspect_range does
 * exactly that, one pattern_args per BEFORE/AFTER phrase.  While the label
 * lived in an array inside g_lit, the second call could realloc the table
 * and leave the first caller's pointer dangling; the Arg then emitted
 * `%hi()` with no symbol, which the assembler resolves to address 0.
 *
 * That is CCVS NC122A: `REPLACING ALL "A" BY "E"` searched for whatever
 * byte sits at address 0 instead of "A", so nothing was replaced.  It
 * needs the table to cross a power of two between the two calls, which is
 * why it took a program with ~80 literals to show and why every small
 * reproduction of the statement looked fine.  #29 shape (2) did not create
 * it -- it changed how many literals a comparison emits, which moved the
 * boundary onto this pair.  It was latent for as long as Args have been
 * built before being emitted.
 *
 * So the label is allocated separately and never moves. */
static const char *lit_label(const unsigned char *bytes, int len)
{
    for (int i = 0; i < g_nlit; i++)
        if (g_lit[i].len == len && !memcmp(g_lit[i].bytes, bytes, len)) return g_lit[i].label;
    if (g_nlit == g_lcap) { g_lcap = g_lcap ? g_lcap * 2 : 32; g_lit = realloc(g_lit, g_lcap * sizeof *g_lit); }
    Lit *l = &g_lit[g_nlit++];
    char buf[32];
    snprintf(buf, sizeof buf, ".Lstr%d", g_nlit - 1);
    size_t n = strlen(buf) + 1;
    l->label = xmalloc(n); memcpy(l->label, buf, n);
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
    if (s->natgroup) { d.cat = COB_NATIONAL; d.usage = COB_U_DISPLAY; }   /* treated as PIC N(m) (13.18.29.4 rule 2b) */
    else if (sym_bitlike(s)) {           /* not a group with a USAGE BIT clause: that one is alphanumeric (13.18.60; B5) */
        /* bits: size the boolean positions, scale the first bit's place */
        d.cat = COB_BOOLEAN; d.usage = COB_U_BIT; d.size = s->bits; d.scale = (signed char)s->bitoff;
        s->desc_id = desc_add(&d);
        return s->desc_id;
    }
    else if (s->is_group) { d.cat = COB_GROUP; d.usage = COB_U_DISPLAY; }
    else {
        switch (s->pi.category) {
        case PIC_ALPHABETIC: d.cat = COB_ALPHA; break;
        case PIC_ALPHANUMERIC: d.cat = COB_ALNUM; break;
        case PIC_ALPHANUMERIC_EDITED: d.cat = COB_ALNUM_ED; break;
        case PIC_NUMERIC: d.cat = COB_NUM; break;
        case PIC_NATIONAL: d.cat = COB_NATIONAL; break;
        case PIC_BOOLEAN: d.cat = COB_BOOLEAN; break;
        default: d.cat = COB_NUM_ED; break;
        }
        switch (s->usage) {
        case U_DISPLAY: d.usage = COB_U_DISPLAY; break;
        case U_PACKED: d.usage = COB_U_PACKED; break;
        case U_NATIONAL: d.usage = COB_U_NATIONAL; break;
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

/* a national literal's descriptor: len bytes, len / 2 characters */
static int nat_desc(int len)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_NATIONAL; d.usage = COB_U_DISPLAY; d.size = len;
    return desc_add(&d);
}

/* the columns national text (len bytes of UTF-16BE) takes on a screen or
 * a report line: a grapheme cluster at a time, each its display width, a
 * mark with nothing to sit on one -- the runtime's nat_clusters, on the
 * shared model of common/s32utf.h (cobol ISSUES-92, -94) */
static int nat_lit_cols(const unsigned char *p, int len)
{
    s32u_clu st; memset(&st, 0, sizeof st);
    int w = 0, pend = 0, n = len / 2;
    for (int i = 0; i < n; ) {
        uint32_t cp;
        i += (int)s32u_u16_get(p, (size_t)n, (size_t)i, &cp);
        if (s32u_clu_step(&st, cp)) w += pend;
        pend = s32u_clu_lone(&st) ? 1 : s32u_clu_width(&st);
    }
    return w + pend;
}

/* a boolean literal's or part's descriptor: len boolean positions, DISPLAY */
static int bool_desc(int len)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_BOOLEAN; d.usage = COB_U_DISPLAY; d.size = len;
    return desc_add(&d);
}

/* national bytes (UTF-16BE) back to UTF-8, for DISPLAY of a literal */
static int utf16be_to_utf8(const unsigned char *p, int nbytes, char *out)
{
    int k = 0, n = nbytes / 2;
    for (int i = 0; i < n; ) {
        uint32_t cp;
        i += (int)s32u_u16_get(p, (size_t)n, (size_t)i, &cp);
        k += s32u_encode(cp, (unsigned char *)out + k);
    }
    return k;
}

/* an unsigned integer of n DISPLAY digits (a calendar function's result) */
static int num_desc(int digits)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_NUM; d.usage = COB_U_DISPLAY; d.digits = (unsigned char)digits; d.scale = 0; d.size = digits;
    return desc_add(&d);
}

/* an intrinsic's result: a sign and 18 DISPLAY digits at the given scale */
static int numfn_desc(int scale)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_NUM; d.usage = COB_U_DISPLAY; d.digits = 18; d.scale = (signed char)scale;
    d.flags = COB_F_SIGNED | COB_F_SEPLEAD; d.size = 19;
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

/* frame: sp+0 lr, sp+4 r11, sp+8.. operand slots, three scratch words, the slots named below; r12/r13 at SLOT_R12/SLOT_R13 */
#define FRAME       120
#define SLOT_R12    92          /* the caller's r12 and r13: callee-saved in the C ABI, and */
#define SLOT_R13    112         /* the generated code uses both as scratch (cobol ISSUES-57) */
#define SLOT_COLL   96          /* the caller's collating table, when this unit sets its own */
#define SLOT_DP     100         /* the caller's decimal point, under DECIMAL-POINT IS COMMA */
#define SLOT_CUR    104         /* the caller's currency sign, under CURRENCY SIGN */
#define SLOT_PBASE  108         /* the caller's PERFORM frame base (cob_perform_enter) */
#define SLOT_ACT    84          /* this activation's saved words and LOCAL-STORAGE (cob_act_enter, -std=2002) */
#define SLOT_RET    88          /* a function's result: the caller's temporary (-std=2002) */
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
    int rm_nat;                     /* a national item's: start and length count characters, two bytes each */
    int rm_bit;                     /* a USAGE BIT item's or bit group's: they count bits (cobol ISSUES-82) */
    int bitsub;                     /* a bit array's element: 1 + the subscript that picks it, as a bit position (cobol ISSUES-84) */
    long bitu_start;                /* ... and the start within the element: 1 without a reference modification, 0 computed (cobol ISSUES-93) */
    int user_rm;                    /* the program wrote a reference modification (rm is also set for a bit-array element) */
    int rm_s0, rm_s1, rm_l0, rm_l1; /* token ranges of the expressions (rm_l0 < 0: no length) */
    int rm_odo; Sym *odo_dep; int odo_base, odo_elem;   /* a whole group over an ODO table, sent at its current length */
} Ref;
static void emit_refmod_check(const Ref *r, long len, int slot);

static void parse_expr(void);
static void emit_expr_tokens(int s0, int s1);
static void emit_ucalls(int from, int to);
static int g_nucall;                    /* user-function calls recorded (cobol ISSUES-50) */
static const char *g_ufn_forbid;        /* where a user function may not appear yet, or NULL */
static int ec_size_on(void);
static void emit_ec_size(void);
static int ec_on_name(const char *name);
struct Sym;
static struct Sym *odo_table_for(struct Sym *s);
static void emit_ec_raise(int i);
static int ec_find(const char *w, int line);
static char g_cur_stmt[16];              /* the statement being compiled, for EXCEPTION-STATEMENT */
static const Tok *g_stmt_tok;            /* its first token: the line EXCEPTION-LOCATION names */
static const char *g_ec_file;            /* EC-I-O being raised: the file-name as written, for EXCEPTION-FILE */
static int g_ec_fidx = -1;               /* ... and its file index, for a TURN WITH LOCATION for that file */
static int g_recursive, g_std, g_cond_depth;   /* defined below */
static int g_fnsig_only;                /* -fnsig: write the functions' .s32fn files, compile nothing */
static void skip_unit_body(void);
static int g_is_function;           /* FUNCTION-ID: a user-defined function (COBOL 2002; always recursive) */
static int g_main_done;             /* the executable's main program has been emitted */
static Sym *g_returning;            /* the function's RETURNING item */

/* A user-defined function's signature (docs/functions.md, Stage B): its
 * RETURNING item and parameters, as descriptions a caller can rebuild.
 * Known from a definition earlier in the source, or from the external
 * repository -- a name.s32fn file the function's own compile wrote. */
typedef struct { int group, size, usage, has_pic, just, bwz, sign_lead, sign_sep; char pic[PIC_MAXPAT]; } FDesc;
typedef struct { char name[64], link[128]; int nparam; FDesc param[8], ret; } FnSig;
static FnSig g_fnsig[128]; static int g_nfnsig;
/* the unit's REPOSITORY: functions named there are invoked without FUNCTION */
static char g_repo_fn[32][64]; static int g_nrepo_fn;
static int g_repo_all_intrinsic;    /* FUNCTION ALL INTRINSIC */

enum { O_REF, O_STR, O_NUM, O_FIG, O_ALL, O_EXPR, O_FUNC, O_BEXPR, O_ADDR };   /* O_ADDR: ADDRESS OF ref, a data-address identifier */   /* O_BEXPR: a boolean expression, e_start..e_end, fsize its widest operand */

typedef struct Opnd_ {
    int kind;
    Ref ref;
    Tok *tok;           /* O_STR / O_FIG / O_ALL's literal */
    NumLit num;         /* O_NUM */
    int line;
    int e_start, e_end; /* O_EXPR: token range, re-parsed when emitted */
    int fn; struct Opnd_ *farg, *farg2; int fsize;   /* O_FUNC: intrinsic, its argument(s), result width */
    int ffull, frm;                          /* O_FUNC reference-modified: the width evaluated, the offset taken */
    int fvar, fnat, fbool;                   /* O_FUNC: length known only at run time (fsize its maximum); a national, a boolean result */
    int fwasvar;                             /* O_FUNC: a fixed part cut from a run-time-length result (cobol ISSUES-88) */
    int fs0, fs1, fl0, fl1, flen;            /* O_FUNC reference-modified at computed positions: the start's and length's
                                              * token ranges (fl0 < 0: flen, or to the end when 0) (cobol ISSUES-91) */
    int fnid, fkind, fscale;                 /* O_FUNC, 1989 amendment: cob_fn id, argument shape, result scale */
    struct Opnd_ **fargs; int nfargs;        /* its argument list (an ALL-subscript table arg has all_sub set) */
    int all_sub;                             /* O_REF: table(ALL) -- every element, expanded at emission */
    int fsaved;                              /* O_FUNC evaluated already: 1 + the label of its result's copy (MOVE, general rule 1) */
} Opnd;
static int opnd_is_national(const Opnd *o);
static int opnd_is_boolean(const Opnd *o);
static int ref_is_national(const Ref *r);
static int ref_static_len(const Ref *r);
static void nat_fig_opnd(Opnd *o, int nbytes);

/* national: an elementary PIC N item, or a national group, which is
 * treated as one (2023 13.18.29.4 rule 2b) */
static int sym_is_national(const Sym *s) { return s->natgroup || (!s->is_group && s->pi.category == PIC_NATIONAL); }
static int sym_is_boolean(const Sym *s) { return s->bitgroup || (!s->is_group && s->pi.category == PIC_BOOLEAN); }
/* does a group hold a boolean item anywhere below it */
static int sym_strong_has_boolean(Sym *g)
{
    for (int c = g->child; c >= 0; c = g_sym[c].sibling) {
        Sym *k = &g_sym[c];
        if (sym_is_boolean(k) || (k->is_group && sym_strong_has_boolean(k))) return 1;
    }
    return 0;
}
/* a strong group's elementary items, in order, as (offset in the group,
 * descriptor) words; an OCCURS repeats its entries; returns the count */
static int sym_desc(Sym *s);
static int strong_table_at(Sym *g, Sym *s, int base)
{
    int n = 0, times = s->occurs ? s->occurs : 1;
    for (int k = 0; k < times; k++) {
        int b = base + k * s->size;
        if (!s->is_group) { emit("\t.word %d, .Ld%d", b + s->offset - g->offset, sym_desc(s)); n++; continue; }
        for (int c = s->child; c >= 0; c = g_sym[c].sibling)
            if (!g_sym[c].is_cond && !g_sym[c].is_rename && g_sym[c].redefines < 0) n += strong_table_at(g, &g_sym[c], b);
    }
    return n;
}
static int strong_table(Sym *g, int base)
{
    int n = 0;
    for (int c = g->child; c >= 0; c = g_sym[c].sibling)
        if (!g_sym[c].is_cond && !g_sym[c].is_rename && g_sym[c].redefines < 0) n += strong_table_at(g, &g_sym[c], base);
    return n;
}

/* the descriptor of a reference-modified part with literal positions
 * (2023 8.4.3.3.4 rule 6): national for a national item or a numeric
 * USAGE NATIONAL one, boolean (in the item's usage) for a boolean one,
 * alphanumeric otherwise */
static int bool_desc(int len);
/* a bit array as one boolean item of all its bits, the base its
 * elements are reference-modified out of at run time */
static int bitarray_desc(Sym *s)
{
    Desc d; memset(&d, 0, sizeof d);
    d.cat = COB_BOOLEAN; d.usage = COB_U_BIT; d.size = bit_total(s); d.scale = (signed char)s->bitoff;
    return desc_add(&d);
}

static int part_desc(const Ref *r)
{
    Sym *s = r->sym; int len = (int)r->rm_len;
    if (r->rm_bit) {
        Desc d; memset(&d, 0, sizeof d);
        d.cat = COB_BOOLEAN; d.usage = COB_U_BIT; d.size = len; d.scale = (signed char)((s->bitoff + r->rm_start - 1) % 8);
        return desc_add(&d);
    }
    if (sym_is_boolean(s)) {
        if (!r->rm_nat) return bool_desc(len);
        Desc d; memset(&d, 0, sizeof d);
        d.cat = COB_BOOLEAN; d.usage = COB_U_NATIONAL; d.size = 2 * len;
        return desc_add(&d);
    }
    return r->rm_nat ? nat_desc(2 * len) : str_desc(len);
}

/* in a strongly-typed group: the group itself or anything under one */
static int sym_in_strong(const Sym *s)
{
    for (; ; s = &g_sym[s->parent]) { if (s->strong) return 1; if (s->parent < 0) return 0; }
}

static int is_int_item(Sym *s)
{
    return is_numeric_sym(s) && s->pi.scale == 0;
}

/* COMP-5 and the C-ABI types keep the binary field's capacity rather than
 * the picture's digit count (COB_F_NOTRUNC in the descriptor) */
static int sym_notrunc(Sym *s) { return s->usage == U_COMP5 || usage_is_native(s->usage); }

/* a "hot" integer: binary, at most 4 bytes, no scale */
static int is_hot_int(Sym *s)
{
    if (s->is_group || s->pi.category != PIC_NUMERIC || s->pi.scale != 0) return 0;
    if (s->usage == U_DISPLAY || s->usage == U_PACKED || s->usage == U_NATIONAL) return 0;
    return s->size <= 4;
}

/* identifier [OF|IN qualifier]... [( subscripts )] */
/* a bit array's element, subscripted: its bits are picked out of the
 * array as a reference modification does -- the element's bits, (i - 1)
 * * bits + 1 onward -- whatever builds the Ref (parse_ref, INITIALIZE's
 * walk; cobol ISSUES-84, -94 B2).  A reference modification the program
 * wrote counts bits within the element (8.4.3.3.4 rule 5a), its bounds
 * checked against the element's bits already. */
static void ref_resolve_bits(Ref *r)
{
    if (r->sym->is_group || r->sym->usage != U_BIT || !r->sym->occurs || r->nsub != r->sym->ndims || !r->nsub) return;
    int k = r->nsub - 1;
    if (r->rm) r->bitu_start = r->rm_start;
    else { r->rm = 1; r->rm_len = r->sym->bits; r->rm_l0 = -1; r->bitu_start = 1; }
    r->rm_bit = 1; r->bitsub = r->nsub;
    r->rm_start = !r->sub[k].sym && r->bitu_start ? (r->sub[k].lit - 1) * r->sym->bits + r->bitu_start : 0;
}

/* a bit data item passed BY REFERENCE starts a byte, with only literal
 * subscripts and a literal leftmost position (2023 14.9.4.3 rule 6;
 * cobol ISSUES-94 B6): the callee gets a byte address */
static void bit_arg_check(const Ref *r)
{
    for (int i = 0; i < r->nsub; i++)
        if (r->sub[i].sym) die_at(r->line, "'%s' is a bit data item passed BY REFERENCE: its subscripts must be literals (2023 14.9.4.3 rule 6)", r->sym->name);
    if (r->rm && !r->rm_start) die_at(r->line, "'%s' is a bit data item passed BY REFERENCE: its leftmost position must be a literal (2023 14.9.4.3 rule 6)", r->sym->name);
    long first = r->sym->bitoff + (r->rm ? r->rm_start - 1 : 0);
    if (first % 8) die_at(r->line, "'%s' is a bit data item passed BY REFERENCE and does not start a byte (its bit %ld; 2023 14.9.4.3 rule 6)", r->sym->name, first % 8 + 1);
}

static int g_fn_depth;              /* parsing a function-identifier's arguments (13.18.60.3 rules 8-10) */
static int g_in_proc;               /* in a PROCEDURE DIVISION's statements */
static int g_cond_depth;
/* an index data item is referenced only in SEARCH, SET, a relation
 * condition, a function argument or a USING phrase (2023 13.18.60.3
 * rule 10; X3.23-1985 USAGE syntax rule 5); a pointer only in CALL,
 * INITIALIZE, SET, a relation condition, a function argument or a
 * procedure division header (rules 8-9) */
static void index_ref_check(const Ref *r)
{
    const Sym *x = r->sym;
    if (!g_in_proc || x->is_group || x->is_index || (x->usage != U_INDEX && x->usage != U_POINTER)) return;
    if (g_cond_depth || g_fn_depth || !g_cur_stmt[0]) return;
    /* MOVE says so itself, pointing at SET (move_invalid) */
    static const char *ix_ok[] = { "SET", "SEARCH", "CALL", "EVALUATE", "MOVE", NULL };
    static const char *pt_ok[] = { "SET", "CALL", "INITIALIZE", "EVALUATE", "MOVE", "ALLOCATE", "FREE", NULL };
    const char *const *ok = x->usage == U_INDEX ? ix_ok : pt_ok;
    for (int i = 0; ok[i]; i++) if (!strcmp(g_cur_stmt, ok[i])) return;
    die_at(r->line, "the %s item '%s' is not an operand of %s (%s)", x->usage == U_INDEX ? "USAGE INDEX" : "USAGE POINTER", x->name, g_cur_stmt,
           x->usage == U_INDEX ? (g_std < 2002 ? "X3.23-1985 USAGE syntax rule 5" : "2023 13.18.60.3 rule 10") : "2023 13.18.60.3 rules 8-9");
}

static void parse_ref(Ref *r)
{
    memset(r, 0, sizeof *r);
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a data-name, found %s", tok_desc(t));
    if (!strcmp(t->s, "address") && is_word(peek(1), "of") && !sym_lookup_quiet("address"))
        die_at(t->line, "ADDRESS OF is a sending operand of SET or CALL, or a relation's operand; not here (2023 8.4.3.11 rule 5)");
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
            if (g_nreport - g_report_base != 1) die_at(t->line, g_nreport > g_report_base ? "%s-COUNTER is ambiguous: say %s-COUNTER OF report-name" : "%s-COUNTER: there is no RD", which ? "PAGE" : "LINE", which ? "PAGE" : "LINE");
            rp = &g_reports[g_report_base];
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
    if (cur()->kind == T_LP && !cur()->after_comma && r->sym->ndims == 0) {
        int depth = 0;
        for (int i = g_tp; i < g_ntok; i++) {
            if (g_tok[i].kind == T_LP) depth++;
            else if (g_tok[i].kind == T_RP) { if (--depth == 0) break; }
            else if (g_tok[i].kind == T_COLON && depth == 1) { lead_rm = 1; break; }
            else if (g_tok[i].kind == T_PERIOD) break;
        }
    }
    if (cur()->kind == T_LP && !cur()->after_comma && !lead_rm) {   /* MAX(B, (C + 1) / 2): the comma detaches the paren */
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
                char *sq[64]; int snq = 0;                 /* NC246A qualifies a subscript 18 deep */
                while (at_word("of") || at_word("in")) {
                    advance();
                    if (cur()->kind != T_WORD) die_at(cur()->line, "expected a qualifier after OF/IN");
                    if (snq == 64) die_at(cur()->line, "more than 64 qualifiers on a subscript");
                    sq[snq++] = cur()->s; advance();
                }
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
        r->rm = 1; r->rm_l0 = -1; r->user_rm = 1;
        if (r->sym->strong || (sym_in_strong(r->sym) && (is_numeric_sym(r->sym) || r->sym->pi.edited)))
            die_at(r->line, "'%s' is %s and is not reference-modified (2023 8.4.2.4)", r->sym->name,
                   r->sym->strong ? "a strongly-typed group" : "a numeric or edited item in a strongly-typed group");
        /* 2023 8.4.3.3.4: a USAGE NATIONAL item counts characters, its part
         * national, or boolean for a boolean item (rule 6); a USAGE BIT item
         * or bit group counts bits (rule 5a) */
        r->rm_nat = sym_is_national(r->sym) || (!r->sym->is_group && r->sym->usage == U_NATIONAL);
        r->rm_bit = (!r->sym->is_group && r->sym->usage == U_BIT) || r->sym->bitgroup;   /* 2023 8.4.2.4: character positions; a national group as elementary */
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
        long chars = r->rm_bit ? r->sym->bits : r->rm_nat ? r->sym->size / 2 : r->sym->size;
        if (r->rm_start && r->rm_start > chars) die_at(r->line, "reference modification starts past the end of '%s'", r->sym->name);
        if (r->rm_start && r->rm_len && r->rm_start - 1 + r->rm_len > chars) die_at(r->line, "reference modification runs past the end of '%s'", r->sym->name);
        if (r->rm_start && !r->rm_len && r->rm_l0 < 0) r->rm_len = chars - r->rm_start + 1;
    }
    ref_resolve_bits(r);
    if (r->nsub != r->sym->ndims) {
        if (r->sym->ndims == 0) die_at(r->line, "'%s' is not a table item and takes no subscript", r->sym->name);
        die_at(r->line, "'%s' needs %d subscript%s, %d given", r->sym->name, r->sym->ndims,
               r->sym->ndims == 1 ? "" : "s", r->nsub);
    }
    for (int i = 0; i < r->nsub; i++)
        if (!r->sub[i].sym && (r->sub[i].lit < 1 || r->sub[i].lit > r->sym->dim_count[i]))
            die_at(r->line, "subscript %ld is outside OCCURS %d of '%s'", r->sub[i].lit, r->sym->dim_count[i], r->sym->name);
    index_ref_check(r);
}

enum { FN_UPPER, FN_LOWER, FN_CURDATE, FN_INTDATE, FN_DATEINT, FN_DAYINT, FN_INTDAY, FN_EXCSTATUS, FN_EXCSTMT,
       FN_NATOF, FN_DISPOF, FN_CHARNAT, FN_VARLEN, FN_EXCFILE, FN_EXCLOC, FN_BOOLOFINT, FN_INTOFBOOL, FN_RMLEN };
/* the calendar functions (1989 addendum) take an integer and give one back;
 * the runtime renders the result as numeric DISPLAY digits in its buffer */
static int fn_is_numeric(int fn) { return (fn >= FN_INTDATE && fn <= FN_INTDAY) || fn == FN_VARLEN || fn == FN_INTOFBOOL || fn == FN_RMLEN; }
static int num_desc(int digits);
/* a run-time integer result (LENGTH of a run-time length, INTEGER-OF-
 * BOOLEAN): DISPLAYed as its value, no leading zeros, as a compile-time
 * LENGTH is and as GnuCOBOL shows integer functions */
static int fn_num_desc(const Opnd *o)
{
    int d = num_desc(o->fsize);
    if (o->fn != FN_VARLEN && o->fn != FN_RMLEN && o->fn != FN_INTOFBOOL) return d;
    Desc x = g_desc[d]; x.flags |= COB_F_INTFN;
    return desc_add(&x);
}
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
/* FUNCTION name [(args)] (leftmost:[length]) -- a reference modification
 * of an alphanumeric function's result (X3.23a-1989, the reference-
 * modifier format; cobol ISSUES-54).  Literal positions: the function is
 * evaluated at its full width and the operand is the part. */
static void function_refmod(Opnd *o)
{
    if (o->kind != O_FUNC || cur()->kind != T_LP) return;
    int d = 0, colon = 0;
    for (int k = g_tp; k < g_ntok && g_tok[k].kind != T_EOF; k++) {
        if (g_tok[k].kind == T_LP) d++;
        else if (g_tok[k].kind == T_RP) { if (--d == 0) break; }
        else if (g_tok[k].kind == T_COLON && d == 1) { colon = 1; break; }
    }
    if (!colon) return;
    int line = cur()->line;
    int numeric = o->fn == -1 ? o->fscale >= 0 : fn_is_numeric(o->fn);
    if (numeric) die_at(line, "a numeric function cannot be reference-modified (2023 8.4.3.3.3 rule 2)");
    /* positions are characters: two bytes each in a national result; a
     * result of run-time length is bounded by its maximum here (cobol
     * ISSUES-88) */
    int unit = o->fnat ? 2 : 1, chars = o->fsize / unit;
    advance();
    if (cur()->kind != T_NUM || peek(1)->kind != T_COLON || !(peek(2)->kind == T_RP || (peek(2)->kind == T_NUM && peek(3)->kind == T_RP))) {
        /* a computed start or length: evaluated after the function, the
         * part's place and length found at run time (cobol ISSUES-91) */
        o->fs0 = g_tp; g_noemit++; parse_expr(); g_noemit--; o->fs1 = g_tp;
        if (cur()->kind != T_COLON) die_at(cur()->line, "expected ':' in the reference modification");
        advance();
        o->fl0 = -1; o->flen = 0;
        if (cur()->kind != T_RP) { o->fl0 = g_tp; g_noemit++; parse_expr(); g_noemit--; o->fl1 = g_tp; }
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the reference modification");
        advance();
        if (!o->ffull) o->ffull = o->fsize;
        o->fwasvar = o->fvar;                   /* the whole result's length: the runtime's, or ffull */
        o->fvar = 1;                            /* the part's length is known at run time */
        o->frm = -1;                            /* marks the computed form */
        return;
    }
    NumLit a; numlit_parse(cur(), &a);
    long start = numlit_is_int(&a) && !a.neg ? (long)numlit_int(&a) : 0;
    if (start < 1 || start > chars) die_at(line, "the reference modification starts outside the function's %d characters", chars);
    advance(); advance();
    long len = chars - start + 1; int given = 0;
    if (cur()->kind != T_RP) {
        if (cur()->kind != T_NUM || peek(1)->kind != T_RP)
            die_at(line, "reference modification of a function with an expression length is not implemented yet");
        NumLit b; numlit_parse(cur(), &b);
        len = numlit_is_int(&b) && !b.neg ? (long)numlit_int(&b) : 0;
        if (len < 1 || start + len - 1 > chars) die_at(line, "the reference modification runs outside the function's %d characters", chars);
        given = 1;
        advance();
    }
    advance();
    if (!o->ffull) o->ffull = o->fsize;
    o->frm += ((int)start - 1) * unit; o->fsize = (int)len * unit;
    /* a run-time-length result: with a length, the part is fixed; to its
     * end, the part's length is the result's less the start (at run time) */
    if (o->fvar && given) { o->fvar = 0; o->fwasvar = 1; }
}
static void parse_operand(Opnd *o) { parse_operand_raw(o); function_refmod(o); operand_odo_length(o); }

/* the 1989 amendment's functions: argument shapes FK_NUMS (a list of
 * numerics onto the stack), FK_INT (one integer by value), FK_ALNUM
 * (one string), FK_NONE.  Results: numeric digit strings (scale 0 or
 * 9), or a string buffer. */
enum { FK_NUMS, FK_INT, FK_ALNUM, FK_NONE, FK_ALNUMS };
static const struct { const char *name; int id, kind, scale, minargs, maxargs, fsize, std; } g_fn89[] = {
    { "max", COB_FN_MAX, FK_NUMS, 9, 1, 99, 19, 85 },
    { "min", COB_FN_MIN, FK_NUMS, 9, 1, 99, 19, 85 },
    { "ord-max", COB_FN_ORD_MAX, FK_NUMS, 0, 1, 99, 19, 85 },
    { "ord-min", COB_FN_ORD_MIN, FK_NUMS, 0, 1, 99, 19, 85 },
    { "sum", COB_FN_SUM, FK_NUMS, 9, 1, 99, 19, 85 },
    { "range", COB_FN_RANGE, FK_NUMS, 9, 1, 99, 19, 85 },
    { "midrange", COB_FN_MIDRANGE, FK_NUMS, 9, 1, 99, 19, 85 },
    { "mean", COB_FN_MEAN, FK_NUMS, 9, 1, 99, 19, 85 },
    { "median", COB_FN_MEDIAN, FK_NUMS, 9, 1, 99, 19, 85 },
    { "variance", COB_FN_VARIANCE, FK_NUMS, 9, 1, 99, 19, 85 },
    { "standard-deviation", COB_FN_STDDEV, FK_NUMS, 9, 1, 99, 19, 85 },
    { "mod", COB_FN_MOD, FK_NUMS, 0, 2, 2, 19, 85 },
    { "rem", COB_FN_REM, FK_NUMS, 9, 2, 2, 19, 85 },
    { "integer", COB_FN_INTEGER, FK_NUMS, 0, 1, 1, 19, 85 },
    { "integer-part", COB_FN_INTEGER_PART, FK_NUMS, 0, 1, 1, 19, 85 },
    { "factorial", COB_FN_FACTORIAL, FK_NUMS, 0, 1, 1, 19, 85 },
    { "sqrt", COB_FN_SQRT, FK_NUMS, 9, 1, 1, 19, 85 },
    { "log", COB_FN_LOG, FK_NUMS, 9, 1, 1, 19, 85 },
    { "log10", COB_FN_LOG10, FK_NUMS, 9, 1, 1, 19, 85 },
    { "sin", COB_FN_SIN, FK_NUMS, 9, 1, 1, 19, 85 },
    { "cos", COB_FN_COS, FK_NUMS, 9, 1, 1, 19, 85 },
    { "tan", COB_FN_TAN, FK_NUMS, 9, 1, 1, 19, 85 },
    { "asin", COB_FN_ASIN, FK_NUMS, 9, 1, 1, 19, 85 },
    { "acos", COB_FN_ACOS, FK_NUMS, 9, 1, 1, 19, 85 },
    { "atan", COB_FN_ATAN, FK_NUMS, 9, 1, 1, 19, 85 },
    { "annuity", COB_FN_ANNUITY, FK_NUMS, 9, 2, 2, 19, 85 },
    { "present-value", COB_FN_PRESENT_VALUE, FK_NUMS, 9, 2, 99, 19, 85 },
    { "random", COB_FN_RANDOM, FK_NUMS, 9, 0, 1, 19, 85 },
    { "char", -2, FK_INT, -1, 1, 1, 1, 85 },
    { "ord", -3, FK_ALNUM, 0, 1, 1, 19, 85 },
    { "reverse", -4, FK_ALNUM, -1, 1, 1, 0, 85 },
    { "numval", -5, FK_ALNUM, 9, 1, 1, 19, 85 },
    { "numval-c", -6, FK_ALNUM, 9, 1, 2, 19, 85 },
    /* COBOL 2002 (15.x; cobol ISSUES-52), under -std=2002 */
    { "abs", COB_FN_ABS, FK_NUMS, 9, 1, 1, 19, 2002 },
    { "exp", COB_FN_EXP, FK_NUMS, 9, 1, 1, 19, 2002 },
    { "exp10", COB_FN_EXP10, FK_NUMS, 9, 1, 1, 19, 2002 },
    { "pi", COB_FN_PI, FK_NUMS, 9, 0, 0, 19, 2002 },
    { "sign", COB_FN_SIGN, FK_NUMS, 0, 1, 1, 19, 2002 },
    { "fraction-part", COB_FN_FRACTION_PART, FK_NUMS, 9, 1, 1, 19, 2002 },
    { "year-to-yyyy", COB_FN_YEAR_TO_YYYY, FK_NUMS, 0, 1, 3, 19, 2002 },
    { "date-to-yyyymmdd", COB_FN_DATE_TO_YYYYMMDD, FK_NUMS, 0, 1, 3, 19, 2002 },
    { "day-to-yyyyddd", COB_FN_DAY_TO_YYYYDDD, FK_NUMS, 0, 1, 3, 19, 2002 },
    { "test-date-yyyymmdd", COB_FN_TEST_DATE_YYYYMMDD, FK_NUMS, 0, 1, 1, 19, 2002 },
    { "test-day-yyyyddd", COB_FN_TEST_DAY_YYYYDDD, FK_NUMS, 0, 1, 1, 19, 2002 },
    { "numval-f", -7, FK_ALNUM, 9, 1, 1, 19, 2002 },
    { "test-numval", -8, FK_ALNUM, 0, 1, 1, 19, 2002 },
    { "test-numval-c", -9, FK_ALNUM, 0, 1, 2, 19, 2002 },
    { "test-numval-f", -10, FK_ALNUM, 0, 1, 1, 19, 2002 },
    { NULL, 0, 0, 0, 0, 0, 0, 0 }
};

static void parse_operand(Opnd *o);
static Opnd expr_opnd(void);
static int at_arith_op(void);

/* one function argument: an expression, an item, a literal -- or a
 * one-dimension table with the subscript ALL, every element an argument */
static Opnd *fn89_arg(const char *fname)
{
    Opnd *x = xmalloc(sizeof *x);
    if (cur()->kind == T_WORD && peek(1)->kind == T_LP && !peek(1)->after_comma && is_word(peek(2), "all") && peek(3)->kind == T_RP) {
        memset(x, 0, sizeof *x);
        x->kind = O_REF; x->line = cur()->line;
        x->ref.sym = sym_lookup(cur()->s, NULL, 0, cur()->line);
        if (x->ref.sym->ndims != 1) die_at(cur()->line, "FUNCTION %s: the ALL subscript takes a one-dimension table", fname);
        x->all_sub = 1;
        advance(); advance(); advance(); advance();
        return x;
    }
    if (cur()->kind == T_LP) { *x = expr_opnd(); return x; }   /* SIN((3 * PI) / 2) */
    int start = g_tp;
    parse_operand(x);
    if (at_arith_op()) { g_tp = start; *x = expr_opnd(); }
    return x;
}

/* an intrinsic function's name: the 1989 table, or one parsed by name */
static int fn89_known(const char *w)
{
    static const char *named[] = { "when-compiled", "upper-case", "lower-case", "current-date", "integer-of-date",
        "date-of-integer", "day-of-integer", "integer-of-day", "length", "byte-length", "highest-algebraic",
        "lowest-algebraic", "exception-status", "exception-statement", "national-of", "display-of", "char-national",
        "exception-file", "exception-file-n", "exception-location", "exception-location-n",
        "boolean-of-integer", "integer-of-boolean", NULL };
    for (int i = 0; g_fn89[i].name; i++) if (!strcmp(w, g_fn89[i].name)) return 1;
    for (int i = 0; named[i]; i++) if (!strcmp(w, named[i])) return 1;
    return 0;
}

static int fn89_parse(Opnd *o, Tok *n)
{
    int f = -1;
    for (int i = 0; g_fn89[i].name; i++) if (!strcmp(n->s, g_fn89[i].name)) { f = i; break; }
    if (f < 0) return 0;
    if (g_fn89[f].std > g_std) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
    advance();
    o->fnid = g_fn89[f].id; o->fkind = g_fn89[f].kind; o->fscale = g_fn89[f].scale;
    o->fsize = g_fn89[f].fsize; o->fn = -1;
    o->nfargs = 0;
    if (cur()->kind == T_LP) {
        advance();
        o->fargs = xmalloc(16 * sizeof *o->fargs);
        while (cur()->kind != T_RP) {
            if (o->nfargs == 16) die_at(n->line, "FUNCTION %s: more than 16 arguments", n->s);
            o->fargs[o->nfargs++] = fn89_arg(n->s);   /* the tokenizer drops the decorative commas */
        }
        advance();
    }
    if (o->nfargs < g_fn89[f].minargs || o->nfargs > g_fn89[f].maxargs)
        die_at(n->line, "FUNCTION %s takes %d to %d arguments", n->s, g_fn89[f].minargs, g_fn89[f].maxargs);
    if (g_fn89[f].kind == FK_ALNUM) {
        Opnd *x = o->fargs[0];
        if (x->kind != O_REF && x->kind != O_STR && x->kind != O_FUNC)
            die_at(n->line, "FUNCTION %s takes an alphanumeric item or literal", n->s);
        if (o->fsize == 0)
            o->fsize = x->kind == O_REF ? (int)x->ref.sym->size : x->kind == O_FUNC ? x->fsize : x->tok->len;   /* REVERSE: the argument's width */
    }
    if (g_fn89[f].kind == FK_NUMS &&
        (o->fnid == COB_FN_MAX || o->fnid == COB_FN_MIN || o->fnid == COB_FN_ORD_MAX || o->fnid == COB_FN_ORD_MIN)) {
        int alnum = 0, w = 0;
        for (int i = 0; i < o->nfargs; i++) {
            Opnd *x = o->fargs[i];
            int aw = x->kind == O_STR ? x->tok->len
                   : x->kind == O_REF && !x->all_sub && !is_numeric_sym(x->ref.sym) ? (int)x->ref.sym->size : 0;
            if (aw) { alnum = 1; if (aw > w) w = aw; }
        }
        if (alnum) {                        /* the largest ARGUMENT, as a string */
            o->fkind = FK_ALNUMS;
            if (o->fnid == COB_FN_MAX || o->fnid == COB_FN_MIN) { o->fscale = -1; o->fsize = w; }
        }
    }
    o->kind = O_FUNC;
    return 1;
}

static void parse_ufunc(Opnd *o, const char *name, int line);
static int ufn_named(const char *w);
/* a function this compiler does not have: the module or edition it needs */
static void fn_refuse(Tok *n)
{
    static const struct { const char *name, *why; } later[] = {

        { "locale-compare", "locale support" }, { "locale-date", "locale support" }, { "locale-time", "locale support" },
        { "locale-time-from-seconds", "locale support" }, { "standard-compare", "the ISO/IEC 14651 ordering" },
        { NULL, NULL } };
    static const char *y2014[] = { "combined-datetime", "formatted-current-date", "formatted-date", "formatted-datetime",
        "formatted-time", "integer-of-formatted-date", "seconds-from-formatted-time", "seconds-past-midnight",
        "test-formatted-datetime", "trim", NULL };
    static const char *y2023[] = { "baseconvert", "concat", "convert", "find-string", "module-name",
        "smallest-algebraic", "substitute", NULL };
    for (int i = 0; later[i].name; i++)
        if (!strcmp(n->s, later[i].name)) die_at(n->line, "FUNCTION %s is COBOL 2002 and needs %s, not implemented yet", n->s, later[i].why);
    for (int i = 0; y2014[i]; i++) if (!strcmp(n->s, y2014[i])) die_at(n->line, "FUNCTION %s is COBOL 2014; not implemented", n->s);
    for (int i = 0; y2023[i]; i++) if (!strcmp(n->s, y2023[i])) die_at(n->line, "FUNCTION %s is COBOL 2023; not implemented", n->s);
    die_at(n->line, "FUNCTION %s is not an intrinsic function", n->s);
}

/* HIGHEST-ALGEBRAIC / LOWEST-ALGEBRAIC (2002 15.33, 15.46): the extreme
 * values the argument can represent -- its picture's, or a native
 * binary usage's range */
static void algebraic_limit(Opnd *o, Opnd *x, int high, Tok *n)
{
    if (x->kind != O_REF || x->ref.rm) die_at(n->line, "FUNCTION %s takes a numeric or numeric-edited item", n->s);
    Sym *a = x->ref.sym;
    memset(o, 0, sizeof *o); o->kind = O_NUM; o->line = n->line;
    long long nat = 0; int sgn = 0;
    switch (a->usage) {
    case U_BCHAR: nat = high ? 127 : -128; sgn = 1; break;
    case U_UBCHAR: nat = high ? 255 : 0; sgn = 1; break;
    case U_SSHORT: nat = high ? 32767 : -32768; sgn = 1; break;
    case U_USHORT: nat = high ? 65535 : 0; sgn = 1; break;
    case U_SINT: nat = high ? 2147483647LL : -2147483648LL; sgn = 1; break;
    case U_UINT: nat = high ? 4294967295LL : 0; sgn = 1; break;
    default: break;
    }
    if (sgn) {
        o->num.neg = nat < 0; unsigned long long m = nat < 0 ? 0ULL - (unsigned long long)nat : (unsigned long long)nat;
        char b[24]; int k = snprintf(b, sizeof b, "%llu", m);
        memcpy(o->num.digits, b, (size_t)k); o->num.ndigits = k; o->num.scale = 0;
        return;
    }
    if (a->is_group || !a->has_pic || (a->pi.category != PIC_NUMERIC && a->pi.category != PIC_NUMERIC_EDITED))
        die_at(n->line, "FUNCTION %s takes a numeric or numeric-edited item", n->s);
    /* all nines in the picture's digit positions; P positions read as zeros */
    int digits = a->pi.digits, scale = a->pi.scale, stored = digits;
    int pz = 0;                                   /* trailing P: zeros left of the point */
    if (scale < 0) { pz = -scale; stored = digits - pz; scale = 0; }
    else if (scale > digits) stored = digits;     /* leading P after the point: V PPP99 */
    int nd = 0;
    int lead0 = scale > digits ? scale - digits : 0;
    for (int i = 0; i < lead0; i++) o->num.digits[nd++] = '0';
    for (int i = 0; i < stored; i++) o->num.digits[nd++] = '9';
    for (int i = 0; i < pz; i++) o->num.digits[nd++] = '0';
    if (nd == 0) o->num.digits[nd++] = '0';
    o->num.ndigits = nd; o->num.scale = scale;
    if (!high) { if (a->pi.is_signed) o->num.neg = 1; else { o->num.ndigits = 1; o->num.digits[0] = '0'; o->num.scale = 0; } }
}

static void parse_operand_raw_1(Opnd *o);
static int ref_has_runtime_sub(const Ref *r);
static void parse_operand_raw(Opnd *o)
{
    Tok *t = cur();
    int fn = t->kind == T_WORD && (!strcmp(t->s, "function") ||
             (!strcmp(t->s, "length") && g_tp + 1 < g_ntok && is_word(&g_tok[g_tp + 1], "of")) ||
             ((ufn_named(t->s) || (g_repo_all_intrinsic && fn89_known(t->s))) && !sym_lookup_quiet(t->s)));
    g_fn_depth += fn;
    parse_operand_raw_1(o);
    g_fn_depth -= fn;
}

static void parse_operand_raw_1(Opnd *o)
{
    memset(o, 0, sizeof *o);
    Tok *t = cur();
    o->line = t->line;
    /* ADDRESS OF identifier (2002 8.4.2.11; 2023 8.4.3.11): the address of
     * an item, a data-pointer value.  SET, CALL and relations take it. */
    if (t->kind == T_WORD && !strcmp(t->s, "address") && is_word(peek(1), "of") && !sym_lookup_quiet("address")) {
        if (g_std < 2002) die_at(t->line, "ADDRESS OF is COBOL 2002; compile with -std=2002");
        if (!g_cond_depth && strcmp(g_cur_stmt, "SET") && strcmp(g_cur_stmt, "CALL"))
            die_at(t->line, "ADDRESS OF is a sending operand of SET or CALL, or a relation's operand; not of %s", g_cur_stmt);
        advance(); advance();
        o->kind = O_ADDR;
        parse_ref(&o->ref);
        const Sym *x = o->ref.sym;
        if (x->is_cond || x->is_index) die_at(o->line, "ADDRESS OF '%s': it is not a data item", x->name);
        if (x->strong == 0 && !x->is_group && sym_in_strong(x))
            die_at(o->line, "ADDRESS OF '%s': an item inside a strongly-typed group (2023 8.4.3.11 rule 2)", x->name);
        if (sym_bitlike(x) && ((x->bitoff % 8) || ref_has_runtime_sub(&o->ref) || o->ref.rm))
            die_at(o->line, "ADDRESS OF '%s': a bit item not on a byte, or located at run time (2023 8.4.3.11 rule 4)", x->name);
        return;
    }
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
    /* a user-defined function named in REPOSITORY (or this function
     * itself), invoked without the word FUNCTION (COBOL 2002 8.4.3.2) */
    if (t->kind == T_WORD && ufn_named(t->s) && !sym_lookup_quiet(t->s)) {
        advance(); parse_ufunc(o, t->s, t->line); return;
    }
    /* FUNCTION ALL INTRINSIC: an intrinsic without the word FUNCTION too */
    int bare_fn = t->kind == T_WORD && g_repo_all_intrinsic && fn89_known(t->s) && !sym_lookup_quiet(t->s);
    if (t->kind == T_WORD && (!strcmp(t->s, "function") || bare_fn)) {
        if (!bare_fn) advance();
        Tok *n = cur();
        if (n->kind != T_WORD) die_at(n->line, "expected an intrinsic function name");
        if (ufn_named(n->s)) { advance(); parse_ufunc(o, n->s, n->line); return; }
        if (!strcmp(n->s, "when-compiled")) {
            advance();
            static Tok wc; static char wcbuf[22];
            if (!wcbuf[0]) {
                time_t now = time(0);
                struct tm *t = localtime(&now);
                int y = t->tm_year + 1900, mo = t->tm_mon + 1, da = t->tm_mday;
                int hh = t->tm_hour, mm = t->tm_min, ss = t->tm_sec;
                long off = t->tm_gmtoff; int oneg = off < 0; if (oneg) off = -off;
                int zh = (int)(off / 3600), zm = (int)((off % 3600) / 60);
                if (y < 0) y = 0;
                if (y > 9999) y = 9999;
                if (mo < 1) mo = 1;
                if (mo > 12) mo = 12;
                if (da < 1) da = 1;
                if (da > 31) da = 31;
                if (hh < 0) hh = 0;
                if (hh > 23) hh = 23;
                if (mm < 0) mm = 0;
                if (mm > 59) mm = 59;
                if (ss < 0) ss = 0;
                if (ss > 59) ss = 59;
                if (zh < 0) zh = 0;
                if (zh > 99) zh = 99;
                if (zm < 0) zm = 0;
                if (zm > 59) zm = 59;
                snprintf(wcbuf, sizeof wcbuf, "%04d%02d%02d%02d%02d%02d00%c%02d%02d",
                         y, mo, da, hh, mm, ss, oneg ? '-' : '+', zh, zm);
                wc.kind = T_STR; wc.s = wcbuf; wc.len = 21;
            }
            o->kind = O_STR; o->tok = &wc;
            return;
        }
        if (!strcmp(n->s, "upper-case")) o->fn = FN_UPPER;
        else if (!strcmp(n->s, "lower-case")) o->fn = FN_LOWER;
        else if (!strcmp(n->s, "national-of") || !strcmp(n->s, "display-of") || !strcmp(n->s, "char-national")) {
            /* COBOL 2002 15.66, 15.26, 15.16 (cobol ISSUES-64) */
            if (g_std < 2002) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
            int natof = !strcmp(n->s, "national-of"), dispof = !strcmp(n->s, "display-of");
            advance();
            if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION %s", n->s);
            advance();
            Opnd *a1 = xmalloc(sizeof *a1); parse_operand(a1);
            Opnd *a2 = NULL;
            if (cur()->kind != T_RP && (natof || dispof)) { a2 = xmalloc(sizeof *a2); parse_operand(a2); }
            if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the arguments of FUNCTION %s", n->s);
            advance();
            o->kind = O_FUNC; o->farg = a1; o->farg2 = a2; o->line = n->line;
            if (natof) {
                if (opnd_is_national(a1) || a1->kind == O_NUM || (a1->kind == O_REF && is_numeric_sym(a1->ref.sym)))
                    die_at(n->line, "FUNCTION NATIONAL-OF takes an alphanumeric argument (15.66.3)");
                if (a2 && !(opnd_is_national(a2) && a2->kind != O_FUNC && opnd_size(a2) == 2))
                    die_at(n->line, "FUNCTION NATIONAL-OF: the substitution character is one national character (15.66.3)");
                o->fn = FN_NATOF; o->fnat = 1; o->fvar = 1;
                /* at most one national character per alphanumeric byte */
                o->fsize = 2 * (a1->kind == O_FUNC ? a1->fsize : opnd_size(a1));
            } else if (dispof) {
                if (!opnd_is_national(a1)) die_at(n->line, "FUNCTION DISPLAY-OF takes a national argument (15.26.3)");
                if (a2 && (opnd_is_national(a2) || a2->kind == O_NUM || a2->kind == O_FUNC || opnd_size(a2) != 1))
                    die_at(n->line, "FUNCTION DISPLAY-OF: the substitution character is one alphanumeric character (15.26.3)");
                o->fn = FN_DISPOF; o->fvar = 1;
                /* at most three UTF-8 bytes per national character */
                o->fsize = 3 * ((a1->kind == O_FUNC ? a1->fsize : opnd_size(a1)) / 2);
            } else {
                if (a1->kind != O_NUM && !(a1->kind == O_REF && is_int_item(a1->ref.sym)))
                    die_at(n->line, "FUNCTION CHAR-NATIONAL takes an integer (15.16.3)");
                o->fn = FN_CHARNAT; o->fnat = 1; o->fsize = 2;
            }
            if (o->fsize > 8190) die_at(n->line, "FUNCTION %s: the result could exceed 8190 bytes", n->s);
            return;
        }
        else if (!strcmp(n->s, "boolean-of-integer") || !strcmp(n->s, "integer-of-boolean")) {
            /* COBOL 2002 15.13, 15.45 (cobol ISSUES-76) */
            if (g_std < 2002) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
            int boi = !strcmp(n->s, "boolean-of-integer");
            advance();
            if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION %s", n->s);
            advance();
            Opnd *a1 = xmalloc(sizeof *a1); parse_operand(a1);
            Opnd *a2 = NULL;
            if (boi) { a2 = xmalloc(sizeof *a2); parse_operand(a2); }
            if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the arguments of FUNCTION %s", n->s);
            advance();
            memset(o, 0, sizeof *o); o->kind = O_FUNC; o->farg = a1; o->farg2 = a2; o->line = n->line;
            if (boi) {
                for (Opnd *x = a1; x; x = x == a1 ? a2 : NULL)
                    if (!((x->kind == O_NUM && numlit_is_int(&x->num) && !x->num.neg) || (x->kind == O_REF && is_int_item(x->ref.sym))))
                        die_at(n->line, "FUNCTION BOOLEAN-OF-INTEGER takes two positive integers (15.13.3)");
                o->fn = FN_BOOLOFINT; o->fbool = 1;
                if (a2->kind == O_NUM) {
                    long long len = numlit_int(&a2->num);
                    if (len < 1 || len > 8190) die_at(n->line, "FUNCTION BOOLEAN-OF-INTEGER: a length of %lld boolean positions (1 to 8190 here)", len);
                    o->fsize = (int)len;
                } else { o->fvar = 1; o->fsize = 8190; }
            } else {
                if (!opnd_is_boolean(a1) || a1->kind == O_ALL)
                    die_at(n->line, "FUNCTION INTEGER-OF-BOOLEAN takes a boolean argument (15.45.3)");
                o->fn = FN_INTOFBOOL; o->fsize = 18;
            }
            return;
        }
        else if (!strcmp(n->s, "exception-status") || !strcmp(n->s, "exception-statement")) {
            /* COBOL 2002 15.32-15.33: the last exception status */
            if (g_std < 2002) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
            int st = !strcmp(n->s, "exception-status");
            advance();
            o->kind = O_FUNC; o->fn = st ? FN_EXCSTATUS : FN_EXCSTMT; o->fsize = st ? 31 : 63;
            return;
        }
        else if (!strcmp(n->s, "exception-file") || !strcmp(n->s, "exception-file-n") ||
                 !strcmp(n->s, "exception-location") || !strcmp(n->s, "exception-location-n")) {
            /* COBOL 2002 15.23-15.26: as long as their contents (cobol ISSUES-65) */
            if (g_std < 2002) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
            int file = !strncmp(n->s, "exception-file", 14), nat = n->s[strlen(n->s) - 2] == '-';
            advance();
            o->kind = O_FUNC; o->fn = file ? FN_EXCFILE : FN_EXCLOC; o->fvar = 1; o->fnat = nat; o->fnid = nat;
            o->fsize = (file ? 2 + 64 : 255) * (nat ? 2 : 1);   /* the file-name, the location string: their bounds */
            return;
        }
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
        } else if (fn89_parse(o, n)) {
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
            if (x.kind == O_FUNC && x.fvar) {       /* the length of a result known only at run time */
                Opnd *fx = xmalloc(sizeof *fx); *fx = x;
                memset(o, 0, sizeof *o); o->kind = O_FUNC; o->fn = FN_VARLEN; o->farg = fx; o->fsize = 9;
                o->fnid = x.fnat;                   /* characters of a national result */
                o->line = n->line;
                return;
            }
            if (x.kind == O_REF && x.ref.rm && !x.ref.rm_len) {
                /* the part's length is computed: counted at run time, in
                 * character positions (cobol ISSUES-81) */
                Opnd *fx = xmalloc(sizeof *fx); *fx = x;
                memset(o, 0, sizeof *o); o->kind = O_FUNC; o->fn = FN_RMLEN; o->farg = fx; o->fsize = 9;
                o->fnid = x.ref.rm_nat; o->line = n->line;
                return;
            }
            int len = opnd_size(&x);
            if (opnd_is_national(&x) || (x.kind == O_REF && x.ref.sym->usage == U_NATIONAL))
                len /= 2;                               /* national: character positions, two bytes each */
            if (x.kind == O_REF && !x.ref.rm && sym_bitlike(x.ref.sym))
                len = x.ref.sym->bits;                  /* bits: boolean positions */
            if (x.kind == O_REF && x.ref.rm_bit && x.ref.rm_len) len = (int)x.ref.rm_len;   /* a bit part or element */
            o->kind = O_NUM; numlit_from_int(&o->num, len);
            return;
        }
        else if (!strcmp(n->s, "byte-length") || !strcmp(n->s, "highest-algebraic") || !strcmp(n->s, "lowest-algebraic")) {
            /* COBOL 2002, known from the argument's description at compile time */
            if (g_std < 2002) die_at(n->line, "FUNCTION %s is COBOL 2002; compile with -std=2002", n->s);
            int bytes = !strcmp(n->s, "byte-length"), high = !strcmp(n->s, "highest-algebraic");
            advance();
            if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION %s", n->s);
            advance();
            Opnd x; parse_operand(&x);
            if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the function argument");
            advance();
            if (bytes) {
                if (x.kind != O_REF && x.kind != O_STR && x.kind != O_FUNC) die_at(n->line, "FUNCTION BYTE-LENGTH takes an item or a literal");
                if (x.kind == O_FUNC && x.fvar) {
                    Opnd *fx = xmalloc(sizeof *fx); *fx = x;
                    memset(o, 0, sizeof *o); o->kind = O_FUNC; o->fn = FN_VARLEN; o->farg = fx; o->fsize = 9; o->fnid = 0;
                    o->line = n->line;
                    return;
                }
                int len = opnd_size(&x);
                if (len < 0) die_at(n->line, "FUNCTION BYTE-LENGTH of a reference modification with a variable length is not implemented");
                o->kind = O_NUM; numlit_from_int(&o->num, len);
                return;
            }
            algebraic_limit(o, &x, high, n);
            return;
        }
        else fn_refuse(n);
        advance();
        if (cur()->kind != T_LP) die_at(cur()->line, "expected '(' after FUNCTION %s", n->s);
        advance();
        o->farg = xmalloc(sizeof *o->farg);
        parse_operand(o->farg);
        if (o->farg->kind != O_REF && o->farg->kind != O_STR && o->farg->kind != O_FUNC)
            die_at(n->line, "FUNCTION %s takes an alphanumeric item or literal", n->s);
        if (cur()->kind != T_RP) die_at(cur()->line, "expected ')' after the function argument");
        advance();
        o->kind = O_FUNC;
        Opnd *a = o->farg;
        if (a->kind == O_REF && a->ref.rm && ref_static_len(&a->ref) <= 0)
            die_at(n->line, "FUNCTION %s of a reference modification with a variable length is not implemented", n->s);
        o->fsize = a->kind == O_REF ? (a->ref.rm ? ref_static_len(&a->ref) : (int)a->ref.sym->size)
                 : a->kind == O_FUNC ? a->fsize : a->tok->len;
        /* a national argument, a national result (2002 15.78, 15.52); an
         * argument of run-time length, a result of the same length */
        o->fnat = opnd_is_national(a);
        o->fvar = a->kind == O_FUNC && a->fvar;
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
    if (ec_on_name("EC-BOUND-ODO") && odo_table_for(r->sym)) return 1;   /* the check loads the DEPENDING ON item */
    return 0;
}

/* the literal length of a reference-modified item, or -1 when it is
 * only known at run time */
static int ref_static_len(const Ref *r)
{
    if (!r->rm) return r->sym->size;
    if (r->rm_bit) return r->rm_start ? (int)((r->sym->bitoff + r->rm_start - 1) % 8 + r->rm_len + 7) / 8   /* the bytes the bits span */
                                      : (int)(r->rm_len + 7) / 8 + 1;                                          /* at most, from a computed bit */
    return r->rm_len ? (int)r->rm_len * (r->rm_nat ? 2 : 1) : -1;
}

/* load the integer value of a hot item at address in areg into dreg */
static void emit_display_decode(int n, const char *areg, const char *dreg);
static void emit_display_encode(int n, const char *areg, const char *vreg);
static int is_display_int(Sym *s);
static int opnd_display_int(Opnd *o);

static void emit_load_int(Sym *s, const char *areg, const char *dreg)
{
    if (is_display_int(s)) { emit_display_decode(s->pi.digits, areg, dreg); return; }
    int sg = s->pi.is_signed;
    if (s->size == 1) emit("\t%s %s, %s+0", sg ? "ldb" : "ldbu", dreg, areg);
    else if (s->size == 2) emit("\t%s %s, %s+0", sg ? "ldh" : "ldhu", dreg, areg);
    else emit("\tldw %s, %s+0", dreg, areg);
}

static void emit_store_int(Sym *s, const char *areg, const char *vreg)
{
    if (is_display_int(s)) { emit_display_encode(s->pi.digits, areg, vreg); return; }
    if (s->size == 1) emit("\tstb %s+0, %s", areg, vreg);
    else if (s->size == 2) emit("\tsth %s+0, %s", areg, vreg);
    else emit("\tstw %s+0, %s", areg, vreg);
}

/* reg = address of item s plus off: WORKING-STORAGE by label, a LINKAGE
 * item through its cell, which the entry sequence filled from the
 * caller's argument register (LOCAL-STORAGE and EXTERNAL likewise) */
static void emit_item_addr(const char *reg, Sym *s, int off)
{
    Sym *rec = &g_sym[s->record];
    if (rec->ftemp_scan && !g_noemit)
        die_at(rec->line, "internal: a user function's result from a scan-ahead was used without its call (a statement keeps scanned operands)");
    if (!rec_indirect(rec)) { emit_la_off(reg, rec->label, off); return; }
    emit_la(reg, rec->label);
    emit("\tldw %s, %s+0", reg, reg);
    if (rec->is_based && ec_on_name("EC-DATA-PTR-NULL")) {
        /* a based item referenced while its address is NULL (2002 13.16.5 GR 3) */
        int Lok = new_label();
        emit("\tbne %s, r0, .L%d", reg, Lok);
        emit_ec_raise(ec_find("EC-DATA-PTR-NULL", 0));
        emit_label(Lok);
    }
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
/* EC-BOUND-ODO (2023 13.18.38 general rule 7; cobol ISSUES-61): a
 * reference to an OCCURS DEPENDING ON table, to an item in it, or to a
 * group holding it, needs the DEPENDING ON value within the OCCURS
 * bounds.  Checked before the address is formed, with checking on. */
static Sym *odo_table_for(Sym *s)
{
    Sym *t = NULL;
    for (Sym *k = s; k && !t; k = k->parent >= 0 ? &g_sym[k->parent] : NULL) if (k->odo_dep_sym) t = k;
    if (!t && s->is_group) t = odo_table_below(s);
    return t && t->odo_dep_sym ? t : NULL;
}

static void emit_odo_check(Sym *s)
{
    Sym *t = odo_table_for(s);
    if (!t) return;
    Sym *d = t->odo_dep_sym;
    int Lok = new_label();
    if (is_hot_int(d)) { emit_item_addr("r1", d, d->offset); emit_load_int(d, "r1", "r1"); }
    else { emit_item_addr("r3", d, d->offset); emit_desc_addr("r4", sym_desc(d)); emit_call("cob_load_int"); }
    if (t->odo_min) emit("\taddi r1, r1, %d", -t->odo_min);
    emit_li("r2", t->occurs - t->odo_min + 1);
    emit("\tbltu r1, r2, .L%d", Lok);
    emit_ec_raise(ec_find("EC-BOUND-ODO", 0));
    emit_label(Lok);
}

/* r1 = where a bit array element's part begins, as a bit position in
 * the array from 1: (i - 1) * bits + the start within the element, the
 * subscript and the start either computed (cobol ISSUES-84, -93).  Worked
 * on the numeric stack, which leaves r11 alone.  chk: the length as
 * emit_refmod_check takes it, for a computed start under EC-BOUND-REF-MOD. */
/* a reference modification's computed position, off the numeric stack
 * into r1: with EC-BOUND-REF-MOD checked, a value that is not an integer
 * is noted for the bound check to raise (8.4.3.3.4 rule 5; cobol
 * ISSUES-94 E17) */
static void emit_pop_pos(void) { emit_call(ec_on_name("EC-BOUND-REF-MOD") ? "cob_pop_pos" : "cob_pop_int"); }

static void emit_bitelem_start(const Ref *r, long chk, int slot)
{
    Sym *s = r->sym;
    int k = r->bitsub - 1;
    if (r->bitu_start) emit_li("r1", r->bitu_start);
    else {
        emit_expr_tokens(r->rm_s0, r->rm_s1);
        emit_pop_pos();
        if (ec_on_name("EC-BOUND-REF-MOD")) emit_refmod_check(r, chk, slot);
    }
    emit("	add r3, r1, r0"); emit("	srai r4, r1, 31"); emit_li("r5", 0); emit_call("cob_push_lit");
    if (!r->sub[k].sym) { emit_li("r3", (r->sub[k].lit - 1) * s->bits); emit_li("r4", 0); emit_li("r5", 0); emit_call("cob_push_lit"); }
    else {
        Sym *ss = r->sub[k].sym;
        emit_item_addr("r3", ss, ss->offset); emit_desc_addr("r4", sym_desc(ss)); emit_call("cob_push");
        emit_li("r3", r->sub[k].adj - 1); emit("	srai r4, r3, 31"); emit_li("r5", 0); emit_call("cob_push_lit");
        emit_call("cob_nadd");
        emit_li("r3", s->bits); emit_li("r4", 0); emit_li("r5", 0); emit_call("cob_push_lit");
        emit_call("cob_nmul");
    }
    emit_call("cob_nadd");
    emit_call("cob_pop_int");
}

static void emit_ref_addr(const Ref *r, const char *reg)
{
    Sym *s = r->sym;
    if (ec_on_name("EC-BOUND-ODO")) emit_odo_check(s);
    int off = s->offset;
    int runtime = ref_has_runtime_sub(r);
    for (int i = 0; i < r->nsub; i++)
        if (!r->sub[i].sym) off += (int)(r->sub[i].lit - 1) * s->dim_stride[i];
    if (r->rm && r->rm_start) off += r->rm_bit ? (s->bitoff + (int)r->rm_start - 1) / 8 : ((int)r->rm_start - 1) * (r->rm_nat ? 2 : 1);
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
        if (ec_on_name("EC-BOUND-SUBSCRIPT")) {
            /* the occurrence number, now less one, must be below the
             * dimension's OCCURS maximum (2023 8.4.2.3.4 rule 2) */
            int Lok = new_label();
            emit_li("r2", s->dim_count[i]);
            emit("\tbltu r1, r2, .L%d", Lok);
            emit_ec_raise(ec_find("EC-BOUND-SUBSCRIPT", 0));
            emit_label(Lok);
        }
        emit_li("r2", s->dim_stride[i]);
        emit("\tmul r1, r1, r2");
        emit("\tadd r11, r11, r1");
    }
    if (r->rm && !r->rm_start && r->bitsub) {
        /* a bit array's element at a computed subscript, or its part at a
         * computed start: the byte holding its first bit (cobol ISSUES-84) */
        emit_bitelem_start(r, r->rm_len ? (long)r->rm_len : r->rm_l0 >= 0 ? -3 : -1, 0);
        emit("\taddi r1, r1, %d", s->bitoff - 1);
        emit("\tsrai r1, r1, 3");
        emit("\tadd r11, r11, r1");
    } else if (r->rm && !r->rm_start) {
        /* the start expression: onto the numeric stack, then off as an int */
        emit_expr_tokens(r->rm_s0, r->rm_s1);
        emit_pop_pos();
        if (ec_on_name("EC-BOUND-REF-MOD")) emit_refmod_check(r, r->rm_len ? (long)r->rm_len : r->rm_l0 >= 0 ? -3 : -1, 0);
        if (r->rm_bit) {
            /* bits: the byte holding bitoff + start - 1 */
            emit("\taddi r1, r1, %d", s->bitoff - 1);
            emit("\tsrai r1, r1, 3");
            emit("\tadd r11, r11, r1");
            goto addr_done;
        }
        emit("\taddi r1, r1, -1");
        if (r->rm_nat) emit("\tadd r1, r1, r1");      /* characters to bytes */
        emit("\tadd r11, r11, r1");
    }
addr_done:
    emit_item_addr(reg, s, off);
    if (runtime) emit("\tadd %s, %s, r11", reg, reg);
}

/* a data-pointer value (2023 8.4.3.11; 14.9.39 formats 7 and 10): ADDRESS
 * OF an item, a pointer item's content, or NULL */
static int opnd_is_ptr(const Opnd *o)
{
    return o->kind == O_ADDR || (o->kind == O_REF && !o->ref.sym->is_group && o->ref.sym->usage == U_POINTER) ||
           (o->kind == O_FIG && !strncmp(o->tok->s, "null", 4));
}

static void emit_ptr_value(const Opnd *o, const char *reg)
{
    if (o->kind == O_FIG) { emit("\tadd %s, r0, r0", reg); return; }
    if (o->kind == O_REF) { emit_ref_addr(&o->ref, "r3"); emit("\tldw %s, r3+0", reg); return; }
    const Ref *r = &o->ref;
    Sym *rec = &g_sym[r->sym->record];
    if (rec == r->sym && rec_indirect(rec) && !r->nsub && !r->rm) {
        /* a based or LINKAGE record's own address: its cell, NULL or not */
        emit_la(reg, rec->label);
        emit("\tldw %s, %s+0", reg, reg);
        return;
    }
    emit_ref_addr(r, reg);
}

/* a slot's columns: a national one's character positions (cobol ISSUES-92) */
static int sfield_cols(const SField *f) { return f->pi.category == PIC_NATIONAL ? f->pi.bytes / 2 : f->pi.bytes; }

/* a slot's reference, resolved once the data tree is complete: LINKAGE,
 * EXTERNAL and runtime subscripts make the slot dynamic; a literal
 * subscript folds into a static offset */
static void sfield_resolve(SField *f)
{
    if (f->item || f->kind == COB_SCR_VALUE || f->kind < 0 || !f->ref_tp) return;
    int save_tp = g_tp; g_tp = f->ref_tp;
    Ref rr; parse_ref(&rr);
    g_tp = save_tp;
    if (rr.rm) die_at(f->srcline, "reference modification in a screen item is not implemented");
    /* a national field and its item move as MOVE does: national text to a
     * national receiver only (2023 14.9.25.3 rule 3) */
    if (f->has_pic && f->pi.category != PIC_NATIONAL && sym_is_national(rr.sym) && f->kind != COB_SCR_TO)
        die_at(f->srcline, "'%s' is national: it is shown through a national field (PICTURE N), not this one", rr.sym->name);
    if (f->has_pic && f->pi.category == PIC_NATIONAL && !sym_is_national(rr.sym) && f->kind != COB_SCR_FROM)
        die_at(f->srcline, "'%s' is not national: a national field (PICTURE N) takes its input into a national item", rr.sym->name);
    f->item = rr.sym;
    if (rec_indirect(&g_sym[rr.sym->record]) || ref_has_runtime_sub(&rr))
        f->dyn = 1;
    long off = rr.sym->offset;
    for (int si = 0; si < rr.nsub; si++)
        if (!rr.sub[si].sym) off += (rr.sub[si].lit - 1) * rr.sym->dim_stride[si];
    f->stat_off = off;
}

/* the screen window's dynamic slots: re-parse each reference where its
 * tokens sit (Report Writer's SOURCE trick) and store the address into
 * the slot's cell before the runtime paints or focuses the window */
static void emit_screen_dyn_fill(Screen *sc, int first, int count)
{
    for (int k = first; k < first + count && k < sc->nf; k++) {
        SField *f = &sc->f[k];
        sfield_resolve(f);
        if (!f->dyn) continue;
        Ref rr; int save_tp = g_tp;
        g_tp = f->ref_tp; parse_ref(&rr); g_tp = save_tp;
        emit_ref_addr(&rr, "r1");
        char cell[48]; snprintf(cell, sizeof cell, ".Lsdyn%d_%d_%d", g_unit, (int)(sc - g_screens), k);
        emit_la("r2", cell);
        emit("\tstw r2+0, r1");
    }
}

/* ---- argument staging ------------------------------------------------- */

enum { A_REF, A_LABEL, A_DESC, A_IMM, A_FUNC, A_VALUE, A_RDESC, A_RLEN, A_CONTENT, A_FDESC };
typedef struct { int kind; const Ref *ref; const char *label; int desc; long imm; Opnd *fn; } Arg;
static Arg arg_func(Opnd *o)       { Arg a = { A_FUNC, 0, 0, 0, 0, o }; return a; }
static Arg arg_value(Opnd *o)      { Arg a = { A_VALUE, 0, 0, 0, 0, o }; return a; }
static Arg arg_content(Opnd *o)    { Arg a = { A_CONTENT, 0, 0, 0, 0, o }; return a; }   /* BY CONTENT: a copy's address */
static Arg arg_fdesc(Opnd *o)      { Arg a = { A_FDESC, 0, 0, 0, 0, o }; return a; }   /* the descriptor of the function result just evaluated */
static Arg arg_rdesc(const Ref *r) { Arg a = { A_RDESC, r, 0, 0, 0, 0 }; return a; }
static Arg arg_rlen(const Ref *r)  { Arg a = { A_RLEN, r, 0, 0, 0, 0 }; return a; }
static void opnd_args(Opnd *o, Arg *addr, Arg *desc, int other_size, int other_numeric);
static int g_slot_base;             /* staged operands of nested evaluations use higher slots */

static void emit_fn_value(Opnd *f);
static void emit_push_opnd(Opnd *o);

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

/* EC-BOUND-REF-MOD (2023 8.4.2.4): r1 holds the leftmost position; the
 * part must lie within the item.  len: the literal length, -1 when
 * omitted, -2 when computed (then in the frame slot emit_rm_start_len
 * filled).  r1 survives. */
static void emit_refmod_check(const Ref *r, long len, int slot)
{
    int Lok = new_label();
    emit("\tadd r12, r1, r0");
    emit("\tadd r3, r1, r0");
    if (len == -2) emit("\tldw r4, sp+%d", SLOT(slot));
    else emit_li("r4", len == -3 ? -1 : len);    /* -3: computed, and checked with the length */
    emit_li("r5", r->rm_bit ? r->sym->bits : r->rm_nat ? r->sym->size / 2 : r->sym->size);   /* in character positions, or bits */
    emit_call("cob_bound_refmod");
    emit("\tbeq r1, r0, .L%d", Lok);
    emit_ec_raise(ec_find("EC-BOUND-REF-MOD", 0));
    emit_label(Lok);
    emit("\tadd r1, r12, r0");
}

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
    else if (r->rm_l0 >= 0) { emit_expr_tokens(r->rm_l0, r->rm_l1); emit_pop_pos(); }
    else if (r->bitsub) {
        /* a bit array element's part to the element's end: its bits past the start */
        if (r->bitu_start) emit_li("r1", r->sym->bits - r->bitu_start + 1);
        else { emit_expr_tokens(r->rm_s0, r->rm_s1); emit_pop_pos(); emit_li("r2", r->sym->bits + 1); emit("\tsub r1, r2, r1"); }
    }
    else emit_li("r1", 0);
    emit("\tstw sp+%d, r1", SLOT(slot));
    if (r->bitsub) {
        /* a bit array's element: the array's bit (i - 1) * bits + start */
        if (!r->rm_start) { emit_bitelem_start(r, r->rm_l0 >= 0 ? -2 : r->rm_len ? (long)r->rm_len : -1, slot); return; }
        emit_li("r1", r->rm_start);
        if (r->rm_l0 >= 0 && ec_on_name("EC-BOUND-REF-MOD")) {
            emit_li("r1", r->bitu_start); emit_refmod_check(r, -2, slot); emit_li("r1", r->rm_start);
        }
        return;
    }
    if (r->rm_start) emit_li("r1", r->rm_start);
    else { emit_expr_tokens(r->rm_s0, r->rm_s1); emit_pop_pos(); }
    if (r->rm_l0 >= 0 && ec_on_name("EC-BOUND-REF-MOD")) emit_refmod_check(r, -2, slot);   /* a computed length */
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
            emit_desc_addr("r3", r->bitsub ? bitarray_desc(r->sym) : sym_desc(r->sym));
            emit_call(a[i].kind == A_RDESC ? "cob_refmod_desc" : "cob_refmod_len");
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_CONTENT) {
            /* BY CONTENT: the callee gets a copy, from the runtime's arena,
             * released after the CALL (cob_content_pop) */
            Opnd *o = a[i].fn;
            if (o->kind == O_REF && o->ref.rm) {
                /* a reference-modified part: its length in bytes, worked
                 * out as its descriptor's is (X3.23-1985 and 2023 put no
                 * restriction on it; cobol ISSUES-94) */
                const Ref *r = &o->ref;
                emit_rm_start_len(r, base + i);
                emit("\tadd r4, r1, r0");
                emit("\tldw r5, sp+%d", SLOT(base + i));
                emit_desc_addr("r3", sym_desc(r->sym));
                emit_call("cob_refmod_len");
                emit("\tstw sp+%d, r1", SLOT(base + i));
                emit_ref_addr(r, "r3");
                emit("\tldw r4, sp+%d", SLOT(base + i));
            }
            else if (o->kind == O_REF) { emit_ref_addr(&o->ref, "r3"); emit_li("r4", o->ref.sym->size); }
            else if (o->kind == O_STR) { emit_la("r3", lit_label((unsigned char *)o->tok->s, o->tok->len)); emit_li("r4", o->tok->len); }
            else { emit_la("r3", call_num_lit_label(&o->num)); emit_li("r4", o->num.ndigits); }
            emit_call("cob_content_push");
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_VALUE) {
            /* BY VALUE: the item's integer value, widened to a word */
            Opnd *o = a[i].fn;
            if (o->kind == O_ADDR) emit_ptr_value(o, "r1");
            else if (o->kind == O_REF && is_hot_int(o->ref.sym)) { emit_ref_addr(&o->ref, "r3"); emit_load_int(o->ref.sym, "r3", "r1"); }
            else if (o->kind == O_REF) { emit_ref_addr(&o->ref, "r3"); emit_desc_addr("r4", sym_desc(o->ref.sym)); emit_call("cob_load_int"); }
            else emit_li("r1", (long)numlit_int(&o->num));
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_FUNC) {
            if (a[i].fn->fsaved) { char l[24]; snprintf(l, sizeof l, ".L%d", a[i].fn->fsaved - 1); emit_la("r1", l); }
            else emit_fn_value(a[i].fn);   /* r1 = the result buffer */
            emit("\tstw sp+%d, r1", SLOT(base + i));
            slotted[i] = 1;
        } else if (a[i].kind == A_FDESC) {
            /* a result whose length is known only now: its descriptor, taken
             * while it is still the last function evaluated (the A_FUNC
             * before this one) */
            emit_li("r3", a[i].fn->fnat ? 1 : a[i].fn->fbool ? 2 : 0);
            emit_call("cob_fn_var_desc");
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

static void emit_move(Opnd *src, Ref *dst);
static Opnd expr_opnd(void);
static int at_arith_op(void);
static void init_record(Sym *rec, int si, int defaults);
static void emit_store_receivers(Ref *rs, int *rounded, int nr, int hot, int giving, int subtract, int size_err,
                                 long long sum_mag, int sum_nonneg);
static void sym_finish(Sym *s);
static int layout(int si, int base);
static void set_dims(int si, int ndims, const int *counts, const int *strides);
static const char *link_name(const char *name);
static void emit_args(const Arg *a, int n);
static Arg arg_ref(const Ref *r);

/* ---- user-defined functions: the caller's side (COBOL 2002; ISSUES-50) -- */

/* The external repository: name.s32fn, written by the function's own
 * compile (-fnsig, or any compile of it), found beside the output, beside
 * the source, or on -I.  One line per item: the RETURNING item, then each
 * parameter -- group size usage has_pic just bwz sign_lead sign_sep pic. */
static const char *g_outdir = ".";

static void fdesc_of(FDesc *d, const Sym *x)
{
    memset(d, 0, sizeof *d);
    d->group = x->is_group; d->size = x->size; d->usage = x->usage; d->has_pic = x->has_pic;
    d->just = x->just; d->bwz = x->blank_zero; d->sign_lead = x->sign_lead; d->sign_sep = x->sign_sep;
    snprintf(d->pic, sizeof d->pic, "%s", x->has_pic ? x->pic : "-");
}

static void fnsig_path(char *out, size_t n, const char *dir, const char *name)
{
    snprintf(out, n, "%s/%s.s32fn", dir, link_name(name));
}

static void fnsig_write(const FnSig *f)
{
    char path[1100]; fnsig_path(path, sizeof path, g_outdir, f->name);
    FILE *o = fopen(path, "w");
    if (!o) { fprintf(stderr, "s32-cobc: cannot write %s\n", path); fail(); }
    fprintf(o, "s32fn 1 %s %s %d\n", f->name, f->link, f->nparam);
    for (int k = -1; k < f->nparam; k++) {
        const FDesc *d = k < 0 ? &f->ret : &f->param[k];
        fprintf(o, "%d %d %d %d %d %d %d %d %s\n", d->group, d->size, d->usage, d->has_pic, d->just, d->bwz, d->sign_lead, d->sign_sep, d->pic);
    }
    fclose(o);
}

static int fnsig_read(const char *path, FnSig *f)
{
    FILE *in = fopen(path, "r");
    if (!in) return 0;
    memset(f, 0, sizeof *f);
    int ok = fscanf(in, "s32fn 1 %63s %127s %d", f->name, f->link, &f->nparam) == 3 && f->nparam >= 0 && f->nparam <= 8;
    for (int k = -1; ok && k < f->nparam; k++) {
        FDesc *d = k < 0 ? &f->ret : &f->param[k];
        ok = fscanf(in, "%d %d %d %d %d %d %d %d %255s", &d->group, &d->size, &d->usage, &d->has_pic, &d->just, &d->bwz,
                    &d->sign_lead, &d->sign_sep, d->pic) == 9;
    }
    fclose(in);
    return ok;
}

/* the function's signature: defined earlier in this source, or from the repository */
static int fnsig_find(const char *name)
{
    for (int i = 0; i < g_nfnsig; i++) if (!strcmp(g_fnsig[i].name, name)) return i;
    char srcdir[1024]; snprintf(srcdir, sizeof srcdir, "%s", g_file);
    char *sl = strrchr(srcdir, '/'); if (sl) *sl = 0; else strcpy(srcdir, ".");
    for (int d = -2; d < g_nincdir; d++) {
        const char *dir = d == -2 ? g_outdir : d == -1 ? srcdir : g_incdirs[d];
        char path[1100]; fnsig_path(path, sizeof path, dir, name);
        FnSig f;
        if (fnsig_read(path, &f) && !strcmp(f.name, name)) {
            if (g_nfnsig == 128) die_at(0, "more than 128 user-defined functions");
            g_fnsig[g_nfnsig] = f;
            return g_nfnsig++;
        }
    }
    return -1;
}

/* BY REFERENCE conformance (14.8.2.3.2): the same PICTURE, USAGE,
 * JUSTIFIED, BLANK WHEN ZERO and SIGN -- pictures compared as analysed,
 * so S9(4) and s9999 are the same picture */
/* a two's-complement binary integer with no truncation to a digit count:
 * COMP-5 with an integer picture, or a native usage (SIGNED-INT ...).
 * 1 signed, 2 unsigned, 0 neither */
static int fdesc_native_int(const FDesc *d)
{
    if (d->group) return 0;
    switch (d->usage) {
    case U_SINT: case U_SSHORT: case U_BCHAR: return 1;
    case U_UINT: case U_USHORT: case U_UBCHAR: return 2;
    case U_COMP5: {
        PicInfo pi;
        if (!d->has_pic || pic_analyse(d->pic, &pi) < 0 || pi.scale != 0) return 0;
        return pi.is_signed ? 1 : 2;
    }
    default: return 0;
    }
}

static int fdesc_match(const FDesc *a, const FDesc *b)
{
    /* an implementor extension (docs/behavior-points.md, class E): the same
     * storage under two spellings -- PIC S9(8) COMP-5 and SIGNED-INT -- is
     * taken as conforming, as GnuCOBOL takes it; majesty's holidays passes
     * a SIGNED-INT to floor-divmod's COMP-5 parameter */
    int na = fdesc_native_int(a), nb = fdesc_native_int(b);
    if (na && na == nb && a->size == b->size) return 1;
    if (a->group || b->group) return a->group == b->group && a->size == b->size;
    if (a->size != b->size || a->usage != b->usage || a->has_pic != b->has_pic || a->just != b->just || a->bwz != b->bwz ||
        a->sign_lead != b->sign_lead || a->sign_sep != b->sign_sep) return 0;
    if (!a->has_pic) return 1;
    PicInfo pa, pb;
    if (pic_analyse(a->pic, &pa) < 0 || pic_analyse(b->pic, &pb) < 0) return 0;
    return pa.category == pb.category && pa.digits == pb.digits && pa.scale == pb.scale && pa.is_signed == pb.is_signed &&
           pa.bytes == pb.bytes && !strcmp(pa.pat, pb.pat);
}

/* is this word a user function this unit may invoke without FUNCTION? */
static int ufn_named(const char *w)
{
    if (g_is_function && !strcmp(w, g_progid)) return 1;
    for (int i = 0; i < g_nrepo_fn; i++) if (!strcmp(g_repo_fn[i], w)) return 1;
    return 0;
}

/* a compiler-made record described by d: a function's result, or a BY
 * CONTENT argument.  LOCAL-STORAGE in a program that can be re-entered
 * (the same call site in two activations must not share it), static
 * otherwise. */
static Sym *ftemp_new(const FDesc *d, int line)
{
    static int n;
    Sym *t = sym_new();
    int idx = sym_idx(t);
    t->level = 1; t->line = line; t->is_filler = 1; t->is_ftemp = 1; t->ftemp_scan = g_noemit > 0;
    snprintf(t->name, sizeof t->name, "filler");
    t->usage = d->group ? U_DISPLAY : d->usage; t->has_usage = !d->group;
    t->is_local = g_recursive;
    if (d->group || !d->has_pic) {
        if (d->group) { t->has_pic = 1; snprintf(t->pic, sizeof t->pic, "x(%d)", d->size); }
    } else {
        t->has_pic = 1; snprintf(t->pic, sizeof t->pic, "%s", d->pic);
        t->just = d->just; t->blank_zero = d->bwz; t->sign_lead = d->sign_lead; t->sign_sep = d->sign_sep;
    }
    if (t->has_pic && pic_analyse(t->pic, &t->pi) < 0) die_at(line, "internal: function signature picture '%s': %s", t->pic, t->pi.err);
    sym_finish(t);
    int zero[1] = { 0 };
    layout(idx, 0);
    set_dims(idx, 0, zero, zero);
    t = &g_sym[idx];
    t->record = idx; t->desc_id = -1;
    snprintf(t->label, sizeof t->label, "%s%d_%d", t->is_local ? ".Llft" : "ft", g_unit, n++);
    t->image_size = t->size; t->image = xmalloc((size_t)t->size);
    init_record(t, idx, 1);
    return &g_sym[idx];
}

typedef struct { int sig, nargs, line, emitted; Opnd arg[8]; int byref[8]; Sym *ctmp[8]; Sym *res; } UCall;
static UCall *g_ucall; static int g_ucap;

static Ref ftemp_ref(Sym *t, int line)
{
    Ref r; memset(&r, 0, sizeof r); r.sym = t; r.line = line; r.rm_l0 = -1;
    return r;
}

/* the call: arguments by reference or into their content copies, the
 * result's address last; the function fills the result */
static void emit_ucall(UCall *u)
{
    FnSig *f = &g_fnsig[u->sig];
    Ref refs[9]; Arg a[9];
    for (int k = 0; k < u->nargs; k++) {
        if (u->byref[k]) { refs[k] = u->arg[k].ref; continue; }
        refs[k] = ftemp_ref(u->ctmp[k], u->line);
        if (u->arg[k].kind == O_EXPR) {
            int rd[1] = { 0 };
            emit_expr_tokens(u->arg[k].e_start, u->arg[k].e_end);
            emit_store_receivers(&refs[k], rd, 1, 0, 1, 0, 0, -1, 0);
        } else emit_move(&u->arg[k], &refs[k]);
    }
    refs[u->nargs] = ftemp_ref(u->res, u->line);
    for (int k = 0; k <= u->nargs; k++) a[k] = arg_ref(&refs[k]);
    emit_args(a, u->nargs + 1);
    emit_call(f->link);
    u->emitted = 1;
}

static void emit_ucalls(int from, int to)
{
    for (int k = from; k < to; k++) emit_ucall(&g_ucall[k]);
}

/* name(args), the cursor past the name: the operand becomes the result */
static void parse_ufunc(Opnd *o, const char *name, int line)
{
    if (g_ufn_forbid) die_at(line, "a user-defined function in %s is not implemented yet", g_ufn_forbid);
    int sig = fnsig_find(name);
    if (sig < 0)
        die_at(line, "no signature for the function '%s': define it earlier in this source, or compile its own source "
                     "first (compile.sh does), so its %s.s32fn is beside the output or on -I", name, link_name(name));
    UCall u; memset(&u, 0, sizeof u);
    u.sig = sig; u.line = line;
    if (cur()->kind == T_LP) {
        advance();
        while (cur()->kind != T_RP) {
            if (cur()->kind == T_EOF) die_at(line, "expected ')' after the arguments of '%s'", name);
            if (at_word("omitted")) die_at(cur()->line, "OMITTED arguments (OPTIONAL parameters) are not implemented yet");
            if (u.nargs == 8) die_at(line, "'%s': more than eight arguments", name);
            int start = g_tp;
            Opnd a; parse_operand(&a);
            if (at_arith_op()) { g_tp = start; a = expr_opnd(); }
            u.arg[u.nargs++] = a;
        }
        advance();
    }
    FnSig *f = &g_fnsig[sig];
    if (u.nargs != f->nparam) die_at(line, "the function '%s' takes %d argument%s, not %d", name, f->nparam, f->nparam == 1 ? "" : "s", u.nargs);
    for (int k = 0; k < u.nargs; k++) {
        Opnd *a = &u.arg[k];
        /* 8.4.3.2.4 rule 5: an identifier that could receive goes BY REFERENCE,
         * and must then be described as the parameter is (14.8.2.3); a
         * literal, an expression or a function result goes BY CONTENT, into
         * a copy described as the parameter is */
        if (a->kind == O_REF && !a->ref.sym->is_ftemp && !a->ref.rm) {
            FDesc ad; fdesc_of(&ad, a->ref.sym);
            FDesc *pd = &f->param[k];
            if (!fdesc_match(&ad, pd))
                die_at(a->line, "argument %d of '%s' must be described as the parameter is (PICTURE %s, %d bytes; "
                                "2023 14.8.2.3), or be a literal or expression", k + 1, name, pd->group ? "group" : pd->pic, pd->size);
            u.byref[k] = 1;
        } else {
            u.byref[k] = 0;
            u.ctmp[k] = ftemp_new(&f->param[k], line);
        }
    }
    u.res = ftemp_new(&f->ret, line);
    memset(o, 0, sizeof *o);
    o->kind = O_REF; o->ref = ftemp_ref(u.res, line); o->line = line;
    if (g_noemit) return;                       /* a scan: the re-parse makes the call */
    if (g_nucall == g_ucap) { g_ucap = g_ucap ? 2 * g_ucap : 64; g_ucall = realloc(g_ucall, (size_t)g_ucap * sizeof *g_ucall); }
    g_ucall[g_nucall] = u;
    if (g_cond_depth > 0) { g_nucall++; return; }   /* made where the condition is evaluated */
    emit_ucall(&g_ucall[g_nucall]);
}


/* evaluate an intrinsic into libcob's buffer; r1 holds the pointer */
static int g_stmt_convcheck;            /* this statement evaluated a checked NATIONAL-OF / DISPLAY-OF */

static void emit_fn_value_raw(Opnd *f);
/* after cob_fn_rm or cob_fn_var_skip, r1 the part: EC-BOUND-REF-MOD when
 * the runtime noted the positions out of range, r1 kept */
static void emit_fn_rm_check(void)
{
    if (!ec_on_name("EC-BOUND-REF-MOD")) return;
    int Lok = new_label();
    emit("\tadd r12, r1, r0");
    emit_call("cob_fn_rm_bad");
    emit("\tbeq r1, r0, .L%d", Lok);
    emit_ec_raise(ec_find("EC-BOUND-REF-MOD", 0));
    emit_label(Lok);
    emit("\tadd r1, r12, r0");
}
/* a function's value in libcob's buffer, r1 its address -- evaluated at
 * its full width, the address then moved to a reference modification's part */
static void emit_fn_value(Opnd *f)
{
    if (!f->ffull) { emit_fn_value_raw(f); return; }
    int part = f->fsize;
    if (f->frm < 0) {
        /* computed positions: the start and the length first -- they may
         * call functions themselves, which would overwrite this one's
         * result and its recorded length (cobol ISSUES-94 E1) -- then the
         * function, then cob_fn_rm, which finds the part (cobol ISSUES-91).
         * No length written is -1, so a computed 0 is out of range (E2). */
        int base = g_slot_base; g_slot_base += 3;
        if (g_slot_base > NSLOTS) die_at(f->line, "internal: too many staged operands");
        emit_expr_tokens(f->fs0, f->fs1); emit_pop_pos();
        emit("\tstw sp+%d, r1", SLOT(base + 1));
        if (f->fl0 >= 0) { emit_expr_tokens(f->fl0, f->fl1); emit_pop_pos(); } else emit_li("r1", -1);
        emit("\tstw sp+%d, r1", SLOT(base + 2));
        f->fsize = f->ffull; emit_fn_value_raw(f); f->fsize = part;
        emit("\tstw sp+%d, r1", SLOT(base));
        emit("\tldw r3, sp+%d", SLOT(base));
        emit_li("r4", f->fwasvar ? -1 : f->ffull);
        emit("\tldw r5, sp+%d", SLOT(base + 1));
        emit("\tldw r6, sp+%d", SLOT(base + 2));
        emit_li("r7", f->fnat ? 2 : 1);
        emit_call("cob_fn_rm");
        g_slot_base = base;
        emit_fn_rm_check();
        return;
    }
    f->fsize = f->ffull; emit_fn_value_raw(f); f->fsize = part;
    if (f->fvar && f->frm) {
        /* to the end of a run-time-length result: the pointer on, the
         * length the runtime keeps shortened (cobol ISSUES-88); a start
         * past the end is noted, and checked (E3) */
        emit("\tadd r3, r1, r0");
        emit_li("r4", f->frm);
        emit_call("cob_fn_var_skip");
        emit_fn_rm_check();
    } else {
        if (f->fwasvar && ec_on_name("EC-BOUND-REF-MOD")) {
            /* the part must lie within the result as it came out (8.4.3.3.4
             * rule 5): its end against the length the runtime recorded */
            int Lok = new_label();
            emit("\tadd r12, r1, r0");
            emit_call("cob_fn_last_len");
            emit_li("r2", f->frm + f->fsize);
            emit("\tbge r1, r2, .L%d", Lok);
            emit_ec_raise(ec_find("EC-BOUND-REF-MOD", 0));
            emit_label(Lok);
            emit("\tadd r1, r12, r0");
        }
        if (f->frm) emit("\taddi r1, r1, %d", f->frm);
    }
}

/* r3 = a string argument's address, r4 its length in bytes -- a
 * run-time-length function's taken from libcob as it is evaluated */
static void emit_str_arg(Opnd *x)
{
    if (x->kind == O_FUNC) {
        emit_fn_value(x);
        if (x->fvar) {
            emit("\tadd r12, r1, r0");
            emit_call("cob_fn_last_len");
            emit("\tadd r4, r1, r0");
            emit("\tadd r3, r12, r0");
        } else { emit("\tadd r3, r1, r0"); emit_li("r4", x->fsize); }
        return;
    }
    if (x->kind == O_STR) { emit_la("r3", lit_label((unsigned char *)x->tok->s, x->tok->len)); emit_li("r4", x->tok->len); return; }
    if (x->kind != O_REF || (x->ref.rm && ref_static_len(&x->ref) <= 0))
        die_at(x->line, "this function's argument must be an item or a literal of known length");
    emit_ref_addr(&x->ref, "r3");
    emit_li("r4", x->ref.rm ? ref_static_len(&x->ref) : (long)x->ref.sym->size);
}

static void emit_fn_value_raw(Opnd *f)
{
    Opnd *x = f->farg;
    if (f->fn == FN_NATOF || f->fn == FN_DISPOF) {
        /* the substitution character's address first, into a frame slot */
        int slot = g_slot_base++;
        if (f->farg2) {
            Opnd *s2 = f->farg2;
            if (s2->kind == O_STR) emit_la("r1", lit_label((unsigned char *)s2->tok->s, s2->tok->len));
            else emit_ref_addr(&s2->ref, "r1");
        } else emit_li("r1", 0);
        emit("\tstw sp+%d, r1", SLOT(slot));
        emit_str_arg(x);
        emit("\tldw r5, sp+%d", SLOT(slot));
        g_slot_base--;
        /* no substitution character and checking on: libcob notes a
         * substitution (15.66.4 rule 3, 15.26.4 rule 3), and the statement
         * raises EC-DATA-CONVERSION when it completes -- not here, in the
         * middle of its operands, where a declarative that returns would
         * leave the operands already staged behind it */
        int track = !f->farg2 && ec_on_name("EC-DATA-CONVERSION");
        emit_li("r6", track);
        if (track && !g_noemit) g_stmt_convcheck = 1;
        emit_call(f->fn == FN_NATOF ? "cob_fn_national_of" : "cob_fn_display_of");
        return;
    }
    if (f->fn == FN_BOOLOFINT) {
        emit_push_opnd(x);                      /* argument-1, on the numeric stack */
        if (f->fvar) { emit_push_opnd(f->farg2); emit_call("cob_pop_int"); emit("\tadd r3, r1, r0"); }
        else emit_li("r3", f->fsize);
        emit_call("cob_fn_boolean_of_integer");
        return;
    }
    if (f->fn == FN_INTOFBOOL) {
        Arg a[2]; opnd_args(x, &a[0], &a[1], 0, 0);
        emit_args(a, 2);
        emit_call("cob_fn_integer_of_boolean");
        return;
    }
    if (f->fn == FN_CHARNAT) {
        emit_push_opnd(x);
        emit_call("cob_pop_int");
        emit("\tadd r3, r1, r0");
        emit_call("cob_fn_char_national");
        return;
    }
    if (f->fn == FN_RMLEN) {
        Arg a[1] = { arg_rlen(&x->ref) };            /* the part's bytes */
        emit_args(a, 1);
        emit_li("r4", f->fnid);
        emit_call("cob_fn_len_digits");
        return;
    }
    if (f->fn == FN_VARLEN) {
        emit_fn_value(x);                        /* its length is libcob's now */
        emit_li("r3", f->fnid);
        emit_call("cob_fn_last_len_digits");
        return;
    }
    if (f->fn == -1) {
        if (f->fkind == FK_NUMS) {
            int cnt = 0;
            for (int i = 0; i < f->nfargs; i++) {
                Opnd *ax = f->fargs[i];
                if (ax->all_sub) {
                    Sym *sym = ax->ref.sym;
                    for (int k = 0; k < sym->dim_count[0]; k++) {
                        emit_item_addr("r3", sym, sym->offset + k * sym->dim_stride[0]);
                        emit_desc_addr("r4", sym_desc(sym));
                        emit_call("cob_push");
                        cnt++;
                    }
                } else { emit_push_opnd(ax); cnt++; }
            }
            emit_li("r3", f->fnid);
            emit_li("r4", cnt);
            emit_call("cob_fn_num");
            return;
        }
        if (f->fkind == FK_ALNUMS) {                    /* MAX/MIN over strings */
            for (int i = 0; i < f->nfargs; i++) {
                Opnd *ax = f->fargs[i];
                if (ax->kind == O_STR) { emit_la("r3", lit_label((unsigned char *)ax->tok->s, ax->tok->len)); emit_li("r4", ax->tok->len); }
                else if (ax->kind == O_FUNC) { emit_fn_value(ax); emit("\tadd r3, r1, r0"); emit_li("r4", ax->fsize); }
                else { emit_ref_addr(&ax->ref, "r3"); emit_li("r4", (long)ax->ref.sym->size); }
                emit_call("cob_fn_al_arg");
            }
            emit_li("r3", f->fnid);
            emit_li("r4", f->fsize);
            emit_call("cob_fn_al");
            return;
        }
        if (f->fkind == FK_INT) {                       /* CHAR: one integer, by value */
            emit_push_opnd(f->fargs[0]);
            emit_call("cob_pop_int");
            emit("\tadd r3, r1, r0");
            emit_call("cob_fn_char");
            return;
        }
        if ((f->fnid == -6 || f->fnid == -9) && f->nfargs == 2) {
            /* NUMVAL-C's currency string, argument-2: handed over first */
            Opnd *cx = f->fargs[1];
            if (cx->kind == O_STR) { emit_la("r3", lit_label((unsigned char *)cx->tok->s, cx->tok->len)); emit_li("r4", cx->tok->len); }
            else if (cx->kind == O_REF) { emit_ref_addr(&cx->ref, "r3"); emit_li("r4", (long)cx->ref.sym->size); }
            else die_at(f->line, "FUNCTION NUMVAL-C: the currency string must be an item or a literal");
            emit_call("cob_fn_currency_arg");
        }
        Opnd *ax = f->fargs[0];                         /* the string functions */
        int alen;
        if (ax->kind == O_FUNC) { emit_fn_value(ax); emit("\tadd r3, r1, r0"); alen = ax->fsize; }
        else if (ax->kind == O_REF) { emit_ref_addr(&ax->ref, "r3"); alen = (int)ax->ref.sym->size; }
        else { emit_la("r3", lit_label((unsigned char *)ax->tok->s, ax->tok->len)); alen = ax->tok->len; }
        switch (f->fnid) {
        case -3: emit_call("cob_fn_ord"); break;
        case -4: emit_li("r4", alen); emit_call("cob_fn_reverse"); break;
        case -7: emit_li("r4", alen); emit_call("cob_fn_numval_f"); break;
        case -8: case -9: case -10:
            emit_li("r4", alen); emit_li("r5", f->fnid == -8 ? 0 : f->fnid == -9 ? 1 : 2); emit_call("cob_fn_test_numval"); break;
        default: emit_li("r4", alen); emit_li("r5", f->fnid == -6); emit_call("cob_fn_numval"); break;
        }
        return;
    }
    if (f->fn == FN_CURDATE) { emit_call("cob_fn_current_date"); return; }
    if (f->fn == FN_EXCSTATUS) { emit_call("cob_fn_exception_status"); return; }
    if (f->fn == FN_EXCSTMT) { emit_call("cob_fn_exception_statement"); return; }
    if (f->fn == FN_EXCFILE || f->fn == FN_EXCLOC) {
        emit_li("r3", f->fnid);
        emit_call(f->fn == FN_EXCFILE ? "cob_fn_exception_file" : "cob_fn_exception_location");
        return;
    }
    if (fn_is_numeric(f->fn)) {
        if (x->kind == O_REF && is_hot_int(x->ref.sym)) { emit_ref_addr(&x->ref, "r3"); emit_load_int(x->ref.sym, "r3", "r1"); }
        else if (x->kind == O_REF) { emit_ref_addr(&x->ref, "r3"); emit_desc_addr("r4", sym_desc(x->ref.sym)); emit_call("cob_load_int"); }
        else emit_li("r1", (long)numlit_int(&x->num));
        emit("\tadd r3, r1, r0");
        emit_call(fn_runtime_name(f->fn));
        return;
    }
    if (f->fvar) emit_str_arg(x);
    else {
        if (x->kind == O_FUNC) { emit_fn_value(x); emit("\tadd r3, r1, r0"); }
        else if (x->kind == O_REF) emit_ref_addr(&x->ref, "r3");
        else emit_la("r3", lit_label((unsigned char *)x->tok->s, x->tok->len));
        emit_li("r4", f->fsize);
    }
    emit_call(f->fn == FN_UPPER ? (f->fnat ? "cob_fn_upper_nat" : "cob_fn_upper")
                                : (f->fnat ? "cob_fn_lower_nat" : "cob_fn_lower"));
}

/* address + descriptor of an operand, as two Args.  Figuratives need the
 * other operand's size and are expanded by the caller. */
static void opnd_args(Opnd *o, Arg *addr, Arg *desc, int other_size, int other_numeric)
{
    switch (o->kind) {
    case O_REF:
        *addr = arg_ref(&o->ref);
        if (!o->ref.rm) *desc = arg_desc(sym_desc(o->ref.sym));
        else if (o->ref.rm_len && (o->ref.rm_start || !o->ref.rm_bit)) *desc = arg_desc(part_desc(&o->ref));
        else *desc = arg_rdesc(&o->ref);
        return;
    case O_FUNC:
        *addr = arg_func(o);
        if (o->fvar) { *desc = arg_fdesc(o); return; }
        if (o->fnat) { *desc = arg_desc(nat_desc(o->fsize)); return; }
        if (o->fbool) { *desc = arg_desc(bool_desc(o->fsize)); return; }
        if (o->fn == -1) *desc = arg_desc(o->fscale >= 0 ? numfn_desc(o->fscale) : str_desc(o->fsize));
        else *desc = arg_desc(fn_is_numeric(o->fn) ? fn_num_desc(o) : str_desc(o->fsize));
        return;
    case O_STR:
        *addr = arg_label(lit_label((unsigned char *)o->tok->s, o->tok->len));
        *desc = arg_desc(o->tok->nat ? nat_desc(o->tok->len) : o->tok->boolv ? bool_desc(o->tok->len) : str_desc(o->tok->len)); return;
    case O_NUM: {
        int d; const char *l = num_lit_label(&o->num, &d);
        *addr = arg_label(l); *desc = arg_desc(d); return;
    }
    case O_FIG: case O_ALL: {
        /* ZERO against a numeric item is the number; otherwise a fill of
         * the other operand's length */
        if (o->kind == O_FIG && other_numeric && (!strncmp(o->tok->s, "zero", 4) || !strncmp(o->tok->s, "null", 4))) {   /* NULL: a pointer's zero */
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

/* a size to expand a figurative constant to: a run-time-length function
 * result's maximum, else the operand's size */
static int opnd_size_bound(Opnd *o) { return o->kind == O_FUNC && o->fvar ? o->fsize : opnd_size(o); }

static int opnd_size(Opnd *o)
{
    switch (o->kind) {
    case O_REF: return ref_static_len(&o->ref);
    case O_STR: return o->tok->len;
    case O_NUM: return o->num.ndigits;
    case O_FUNC:
        if (o->fvar) die_at(o->line, "a function whose length is known only at run time is not supported here yet");
        return o->fsize;
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
    /* An unsigned DISPLAY integer of <= 9 digits joins the hot path now that
     * emit_load_int decodes one and emit_store_int encodes one: its value is
     * below 10^9, so every partial sum hot_sum_fits admits still fits a word.
     * GitHub #29 shape (3). */
    if (opnd_display_int(o)) return 1;
    if (o->kind == O_REF) return !o->ref.rm && is_hot_int(o->ref.sym) && !(o->ref.sym->size == 4 && !o->ref.sym->pi.is_signed);
    if (o->kind == O_NUM) return numlit_is_int(&o->num) && numlit_int(&o->num) <= 2147483647LL && numlit_int(&o->num) >= -2147483647LL;
    if (o->kind == O_FIG) return !strncmp(o->tok->s, "zero", 4);
    return 0;
}

/* Two operands whose descriptors are byte-identical and are unsigned
 * DISPLAY numeric with no editing and no P positions: the comparison is a
 * memcmp.  Same length, same digit count, same scale, so the decimal points
 * line up and every character of a canonical field is '0'..'9' -- byte order
 * IS numeric order.
 *
 * It is exact for every value the standard defines, and NOT a conformance
 * fix -- an earlier draft of this comment claimed it was, on GnuCOBOL's
 * evidence alone, and that is wrong.  The 1985 text has a numeric relation
 * condition compare the *algebraic value* of the operands, whatever their
 * usage; for two canonical fields of one descriptor, byte order and
 * algebraic order coincide, so the two readings cannot disagree on any
 * datum the standard admits.
 *
 * They disagree only on a numeric item holding non-digits, which the
 * standard does not define, and there nothing is authoritative -- measured
 * 2026-09-02, three compilers give three answers.  A PIC 9(4) holding
 * '  12' against one holding '0012':
 *
 *      GnuCOBOL   differs, and LESS   (a byte compare; ' ' is 0x20)
 *      gcobol     equal               (decodes, reading a space as zero)
 *      us, before equal               (the same decode)
 *      us, now    differs, and LESS   (GnuCOBOL's answer)
 *
 * So this moves us off gcobol's answer and onto GnuCOBOL's on data that is
 * already outside the language.  Note which way that goes: gcobol's decode
 * is the literal reading of the text, and it is not the implementation we
 * would follow by preference.  The byte compare is adopted because it is
 * exact on every defined value and much cheaper -- NOT because GnuCOBOL
 * does it -- and matching GnuCOBOL here is a side effect, not a warrant.
 * It is a choice on undefined input; free/cmpbytes records it so that
 * changing it later is visible rather than silent.
 *
 * Do not reason from here to the identical-descriptor MOVE of #27, or back.
 * They look alike and rest on different ground: a MOVE between identical
 * descriptors is byte movement, and all three implementations agree on it
 * for exactly the bytes that split them here.  A numeric relation is
 * defined on the algebraic VALUE, which is why implementations diverge as
 * soon as the bytes are not one.
 *
 * Signed is excluded, and that is a correctness condition rather than
 * caution: an overpunched last byte does not order like its digit, and
 * memcmp lands on the opposite side of GnuCOBOL's answer ('001B' against
 * '0012' -- GnuCOBOL says less, memcmp says greater, because 'B' is 0x42
 * and '2' is 0x32).  So are the separate-sign forms, whose sign character
 * sorts against a digit, and BLANK WHEN ZERO, whose spaces are not digits.
 * GitHub #29. */
/* The flag test.  After #29's three shapes, 99.7% of the batch's remaining
 * cob_cmp calls were one shape: a one-byte alphanumeric item against another
 * or against a one-character literal -- "PERFORM UNTIL ws-eof-flag = 'Y'",
 * act-crdb, d-lin-type -- 1.06M calls at 84 instructions each, in every
 * program.  A one-byte alphanumeric relation under the native collating
 * sequence is a byte load and one compare: no padding (both sides are one
 * byte), and byte value IS collating order.  Ordering is unsigned, as
 * cmp_bytes orders it.  Bars: a PROGRAM COLLATING SEQUENCE (the runtime
 * compares through its table; the text says a unit without one is native,
 * and that is what this emits), groups, 88s, reference modification, and a
 * numeric class on either side (that is a digits-as-characters compare with
 * its own rules).  Both sides literal is a constant, left to the runtime.
 * GitHub #29, ISSUES-26. */
static int opnd_onebyte_alnum(Opnd *o)
{
    if (o->kind == O_REF) {
        Sym *s = o->ref.sym;
        if (o->ref.rm || s->is_group || s->is_cond) return 0;
        if (s->pi.category != PIC_ALPHANUMERIC && s->pi.category != PIC_ALPHABETIC) return 0;
        if (s->pi.edited || s->size != 1) return 0;
        return 1;
    }
    if (o->kind == O_STR || o->kind == O_ALL) return o->tok->len == 1;
    if (o->kind == O_FIG) return strncmp(o->tok->s, "null", 4) != 0;
    return 0;
}

static int cmp_is_onebyte(Opnd *x, Opnd *y)
{
    if (g_collate >= 0) return 0;
    if (x->kind != O_REF && y->kind != O_REF) return 0;
    return opnd_onebyte_alnum(x) && opnd_onebyte_alnum(y);
}

/* r1 = the byte of a one-byte operand (cmp_is_onebyte admitted it) */
static void emit_onebyte_value(Opnd *o)
{
    if (o->kind == O_REF) { emit_ref_addr(&o->ref, "r3"); emit("\tldbu r1, r3+0"); return; }
    emit_li("r1", o->kind == O_FIG ? fig_byte(o->tok->s) : (o->tok->s[0] & 255));
}

static int cmp_is_bytewise(Opnd *x, Opnd *y)
{
    if (x->kind != O_REF || y->kind != O_REF) return 0;
    if (x->ref.rm || y->ref.rm) return 0;
    Sym *a = x->ref.sym, *b = y->ref.sym;
    if (a->is_group || b->is_group || a->is_cond || b->is_cond) return 0;
    if (sym_desc(a) != sym_desc(b)) return 0;
    const Desc *d = &g_desc[sym_desc(a)];
    if (d->cat != COB_NUM || d->usage != COB_U_DISPLAY) return 0;
    if (d->flags & (COB_F_SIGNED | COB_F_SEPLEAD | COB_F_SEPTRAIL | COB_F_LEAD | COB_F_BLANKZ)) return 0;
    if (d->picstr[0]) return 0;          /* an edited picture, or P scaling */
    return 1;
}

/* An unsigned DISPLAY integer narrow enough to decode into a word: at most
 * nine digits, so its value is below 10^9 and fits a signed 32-bit register
 * with room to spare.  No scale (a scaled operand would have to be aligned
 * against the other side before comparing), no sign in any of its forms, no
 * editing and no P, and the picture's digits must fill the item exactly so
 * that digit i really is byte i.
 *
 * This is the compare path only.  Arithmetic keeps is_hot_int: a partial sum
 * of these can still leave the word, and the encode side is a different
 * problem from the decode side.  GitHub #29 shape (2). */
static int is_display_int(Sym *s)
{
    if (s->is_group || s->pi.category != PIC_NUMERIC) return 0;
    if (s->usage != U_DISPLAY) return 0;
    if (s->pi.scale != 0 || s->pi.is_signed || s->sign_sep || s->sign_lead) return 0;
    if (s->blank_zero || s->pi.edited || strchr(s->pi.pat, 'P')) return 0;
    if (s->pi.digits < 1 || s->pi.digits > 9) return 0;
    return s->size == s->pi.digits;
}

static int opnd_display_int(Opnd *o)
{
    return o->kind == O_REF && !o->ref.rm && is_display_int(o->ref.sym);
}

/* r1 = the value of such an item, decoded in line: a load, a mask and a
 * multiply-accumulate per digit, against cob_cmp's ~28 per digit through
 * cob_get_num.  The first digit needs no multiply.  r3 holds the address and
 * r11 the constant ten; emit_ref_addr has finished with r11 by then.
 *
 * The mask is `& 15`, not `- '0'`, and that is not a micro-optimisation: it
 * is what cob_get_num does, so the inline decode agrees with the runtime on
 * bytes that are not digits as well as on those that are.  '0'..'9' mask to
 * 0..9; a space (0x20) masks to 0, which is cob_get_num's explicit
 * space-is-zero rule; anything else masks to its low nibble, which is
 * cob_get_num's fallback.  Subtracting '0' would have agreed on digits and
 * diverged on everything else, which is a divergence worth not having for
 * free.  One case is left: cob_get_num reads 'p'..'y' as a NEGATIVE
 * overpunch even in an unsigned item, where this reads the low nibble and
 * stays positive.  An unsigned item cannot hold a negative and cob_put_num
 * would never write those bytes, so that is undefined input on both sides.
 * GitHub #29. */
/* NOTHING HERE MAY TOUCH r11.  r11 is the subscript accumulator, and
 * emit_ref_addr holds a partial sum in it across the reference-modification
 * start expression -- which goes through emit_expr_tokens, emit_push and so
 * reaches this function.  The first version of this loop kept the constant
 * ten in r11 and silently miscompiled `e(i)(d - 1:2)`: the accumulator
 * became 10, so the subscript resolved to the wrong element.  It read
 * correctly in testing only because the table's element size was also 10.
 * That was CCVS NC122A's regression.  Hence the multiply by ten as
 * (x << 3) + (x << 1), which needs no register beyond r2 and the
 * accumulator: two more instructions per digit than a `mul`, against the
 * ~28 per digit this replaces, and no invariant to remember. */
static void emit_display_decode(int n, const char *areg, const char *dreg)
{
    for (int i = 0; i < n; i++) {
        if (i) {
            emit("\tslli r2, %s, 3", dreg);        /* x * 8 */
            emit("\tslli %s, %s, 1", dreg, dreg);  /* x * 2 */
            emit("\tadd %s, %s, r2", dreg, dreg);  /* x * 10 */
        }
        emit("\tldbu r2, %s+%d", areg, i);
        emit("\tandi r2, r2, 15");
        if (i == 0) emit("\tadd %s, r2, r0", dreg);
        else emit("\tadd %s, %s, r2", dreg, dreg);
    }
}

/* The other direction: vreg's value as n digit characters.  The caller has
 * already brought it inside the picture (emit_trunc_bounded) and made it
 * non-negative, which is what cob_put_num_x would have done, so this is a
 * plain radix loop and vreg may be consumed.  GitHub #29 shape (3). */
static void emit_display_encode(int n, const char *areg, const char *vreg)
{
    /* Ten has to live in a register -- there is no divide-immediate -- and by
     * the rule above it must not be r11.  r4 is an argument register: caller
     * saved, dead outside a call's setup, and this sequence contains no call.
     * The store paths that reach here (emit_store_receivers' hot branch and
     * emit_move's) have finished with emit_ref_addr before calling, so no
     * argument is live either. */
    emit_li("r4", 10);
    for (int i = n - 1; i >= 0; i--) {
        emit("\trem r2, %s, r4", vreg);
        emit("\taddi r2, r2, 48");
        emit("\tstb %s+%d, r2", areg, i);
        if (i) emit("\tdiv %s, %s, r4", vreg, vreg);
    }
}

static void emit_display_value(const Ref *r)
{
    emit_ref_addr(r, "r3");
    emit_display_decode(r->sym->pi.digits, "r3", "r1");
}

/* Comparison is more permissive than arithmetic.  opnd_hot_int bars the
 * four-byte unsigned item because no signed SLT can order a value that uses
 * the top bit, and a partial sum of such operands overflows a word -- both
 * true, and both about arithmetic.  A comparison has neither problem: the
 * unsigned SLTU family orders the whole 32-bit range exactly, and a COBOL
 * unsigned item never holds a negative, so when every operand is
 * non-negative the unsigned compare is the right one for all of them.
 *
 * The bar cost a call: "PERFORM UNTIL ws-i > 56164" with ws-i PIC 9(9) COMP
 * built a descriptor for the literal and went through cob_cmp -- about 440
 * instructions for what is one SGTU.  GitHub #27.  Arithmetic keeps
 * opnd_hot_int; only the relation condition uses this. */
static int opnd_hot_cmp(Opnd *o)
{
    if (opnd_display_int(o)) return 1;
    if (o->kind == O_REF) return !o->ref.rm && is_hot_int(o->ref.sym);
    return opnd_hot_int(o);
}

/* r1 = the operand's value on the compare path */
static void emit_cmp_value(Opnd *o)
{
    if (opnd_display_int(o)) { emit_display_value(&o->ref); return; }
    emit_hot_value(o);
}

/* the operand cannot be negative, so an unsigned compare orders it */
static int opnd_nonneg(Opnd *o)
{
    if (o->kind == O_REF) return !o->ref.sym->pi.is_signed;
    if (o->kind == O_NUM) return numlit_int(&o->num) >= 0;
    return 1;   /* ZERO */
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

/* Truncate r1 to the receiver's picture when it is a COMP item (COMP-5 and
 * the C types keep the binary field's capacity).
 *
 * bound is an upper bound on |r1|, or -1 when the caller does not know one;
 * nonneg says r1 cannot be negative.  With a bound the divide usually goes:
 * a value that cannot reach the picture's limit needs no truncation at all,
 * and one that can pass it only once -- "ADD 1 TO" an item already inside
 * its picture, which is every PERFORM VARYING step -- wraps with a compare
 * and a subtract.  REM is a divide, ~30 cycles where the compare is one, and
 * it sat in the hottest loop COBOL has.  GitHub #27. */
static void emit_trunc_bounded(Sym *s, long long bound, int nonneg)
{
    int disp = is_display_int(s);
    if (!disp && s->usage != U_BINARY) return;
    /* a binary field wider than its picture needs no truncation; a DISPLAY
     * item is exactly its digits, so it always does */
    if (!disp && s->pi.digits >= capacity_digits(s->size)) return;
    long long lim = pow10l(s->pi.digits);
    if (bound >= 0 && bound < lim) return;                  /* cannot reach it */
    emit_li("r2", lim);
    if (bound >= 0 && nonneg && bound < 2 * lim) {           /* at most one wrap */
        int L = new_label();
        emit("\tbltu r1, r2, .L%d", L);
        emit("\tsub r1, r1, r2");
        emit_label(L);
        return;
    }
    emit("\trem r1, r1, r2");
}

static void emit_trunc(Sym *s) { emit_trunc_bounded(s, -1, 0); }


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
    int uc0, uc1;           /* the root: user-function calls to make each time it is evaluated */
    int bstack;             /* C_REL: compared on the boolean stack (an ALL literal beside a run-time length) */
    int ptr;                /* C_REL: two data-pointer values, compared as addresses (8.8.4.2.16) */
} Cond;

static Cond *cond_new(int kind) { Cond *c = xmalloc(sizeof *c); memset(c, 0, sizeof *c); c->kind = kind; return c; }

static int opnd_is_boolean(const Opnd *o);
static void bool_fig_opnd(Opnd *o, int n);
static int at_operand(void);
static int is_verb(const char *w);
static int bool_positions(const Sym *s);
static int bool_opnd_len(const Opnd *o);
/* a boolean operand whose positions are known only at run time: a
 * reference modification with a computed length, or a function result
 * of run-time length (cobol ISSUES-94 B4) */
static int bool_len_dynamic(const Opnd *o)
{
    if (o->kind == O_REF) return o->ref.rm && !o->ref.rm_len && !o->ref.rm_odo;
    if (o->kind == O_FUNC) return o->fvar;
    return 0;
}
static int sym_strong_has_boolean(Sym *g);
static int opnd_is_ptr(const Opnd *o);
static Cond *cond_rel(Opnd *x, int op, Opnd *y, int neg)
{
    {   /* data pointers (format 3): EQUAL or NOT EQUAL, and a pointer on
         * both sides, NULL counting as one (2023 8.8.4.2.3 rule 5) */
        int xp = opnd_is_ptr(x) && !(x->kind == O_FIG), yp = opnd_is_ptr(y) && !(y->kind == O_FIG);
        if (xp || yp) {
            if (!opnd_is_ptr(x) || !opnd_is_ptr(y))
                die_at(x->line, "a data pointer is compared only with ADDRESS OF, a pointer item or NULL (2023 8.8.4.2.3 rule 5)");
            if (op != R_EQ && op != R_NE)
                die_at(x->line, "data pointers are compared by EQUAL or NOT EQUAL only (2023 8.8.4.2.2 format 3)");
            Cond *c = cond_new(C_REL);
            c->x = *x; c->y = *y; c->op = op; c->neg = neg; c->ptr = 1;
            return c;
        }
    }
    /* a boolean operand is compared only with a boolean one (2023
     * 8.8.4.2.8); ZERO and ALL B"..." beside it are boolean */
    {   /* strongly-typed groups compare only with the same type (8.8.4.2.12) */
        int xs = x->kind == O_REF ? x->ref.sym->strong : 0, ys = y->kind == O_REF ? y->ref.sym->strong : 0;
        if ((xs || ys) && xs != ys)
            die_at(x->line, "a strongly-typed group is compared only with one of the same type (2023 8.8.4.2.12)");
    }
    int xb = opnd_is_boolean(x), yb = opnd_is_boolean(y);
    if (xb || yb) {
        if (!xb && !(x->kind == O_FIG || x->kind == O_ALL)) die_at(x->line, "a boolean operand is compared only with a boolean one (2023 8.8.4.2.8)");
        if (!yb && !(y->kind == O_FIG || y->kind == O_ALL)) die_at(y->line, "a boolean operand is compared only with a boolean one (2023 8.8.4.2.8)");
        /* boolean operands relate by EQUAL and NOT EQUAL only (8.8.4.2.2
         * format 2; cobol ISSUES-94 B11) */
        if (op != R_EQ && op != R_NE) die_at(x->line, "boolean operands are compared by EQUAL or NOT EQUAL only (2023 8.8.4.2.2)");
        /* ZERO beside a boolean is one zero, extended by the comparison;
         * ALL B"..." is repeated to the other operand's length -- at run
         * time when that length is known only then */
        if ((x->kind == O_ALL && bool_len_dynamic(y)) || (y->kind == O_ALL && bool_len_dynamic(x))) {
            Opnd *a = x->kind == O_ALL ? x : y;
            if (!a->tok->boolv) { bool_fig_opnd(a, a->tok->len / (a->tok->nat ? 2 : 1)); a->kind = O_ALL; }   /* checked, its value kept ALL */
            Cond *c = cond_new(C_REL);
            c->x = *x; c->y = *y; c->op = op; c->neg = neg; c->bstack = 1;
            return c;
        }
        bool_fig_opnd(x, yb ? bool_opnd_len(y) : 1); bool_fig_opnd(y, xb ? bool_opnd_len(x) : 1);
    } else if (op != R_EQ && op != R_NE && x->kind == O_REF && x->ref.sym->strong && sym_strong_has_boolean(x->ref.sym))
        die_at(x->line, "a strongly-typed group holding a boolean item is compared by EQUAL or NOT EQUAL only (2023 8.8.4.2.3 rule 4)");
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

/* ---- boolean expressions (2023 8.8.2; cobol ISSUES-77) ------------------
 * Parsed and emitted in one pass, as parse_expr is: operands are pushed on
 * libcob's boolean stack, operators applied to it, in the order a shunting
 * yard gives -- B-NOT, then B-AND, B-XOR, B-OR, left to right; a shift
 * takes the precedence of the operation before it, B-AND's if none (rule
 * 7b), and carries its integer count with it.  Under g_noemit it only
 * scans.  Returns the widest operand's boolean positions. */
enum { BO_AND = 1, BO_OR, BO_XOR, BO_NOT, BO_SL, BO_SR, BO_SLC, BO_SRC, BO_PAREN };
static int bool_op(const Tok *t)
{
    if (g_std < 2002 || t->kind != T_WORD) return 0;
    static const char *w[] = { "", "b-and", "b-or", "b-xor", "b-not", "b-shift-l", "b-shift-r", "b-shift-lc", "b-shift-rc" };
    for (int i = 1; i <= 8; i++) if (!strcmp(t->s, w[i])) return i;
    return 0;
}
static int bool_opnd_len(const Opnd *o)
{
    if (o->kind == O_STR) return o->tok->len;
    if (o->kind == O_BEXPR) return o->fsize;
    if (o->kind == O_FUNC) return o->fsize;
    if (o->kind == O_REF) return o->ref.rm ? (o->ref.rm_len ? (int)o->ref.rm_len : 1) : bool_positions(o->ref.sym);
    return 1;
}
/* which boolean stack entries are ALL literals, simulated as the code is
 * emitted, for 8.8.2 rules 4 and 5 */
static int g_bsim[64], g_bsp;
static int g_bexpr_all;                  /* the expression just parsed was an ALL literal alone */
static void bool_emit_op(int op, Opnd *cnt)
{
    int line = op >= BO_SL && op <= BO_SRC ? cnt->line : cur()->line;   /* only a shift has its count operand */
    if (op == BO_NOT) { /* of an ALL literal: still one, each position inverted (cobol ISSUES-93) */ }
    else if (op <= BO_XOR) {
        if (g_bsp >= 2 && g_bsim[g_bsp - 1] && g_bsim[g_bsp - 2])
            die_at(line, "the two operands of a boolean operation cannot both be ALL literals (2023 8.8.2 rule 4)");
        if (g_bsp >= 2) { g_bsp--; g_bsim[g_bsp - 1] = 0; }
    } else if (g_bsp && g_bsim[g_bsp - 1])
        die_at(line, "the first operand of a boolean shift cannot be an ALL literal (2023 8.8.2 rule 5)");
    if (op == BO_NOT) { emit_call("cob_bnot"); return; }
    if (op <= BO_XOR) { emit_call(op == BO_AND ? "cob_band" : op == BO_OR ? "cob_bor" : "cob_bxor"); return; }
    emit_push_opnd(cnt);
    emit_li("r3", op - BO_SL);                   /* 0 L, 1 R, 2 LC, 3 RC; the count taken whole (B7) */
    emit_call("cob_bshift_pop");
}
static void bool_emit_operand(Opnd *o)
{
    if (g_bsp < 64) g_bsim[g_bsp++] = o->kind == O_ALL;
    if (o->kind == O_ALL) {
        /* ALL B"...": its value, repeated to the other operand's length
         * when the operation runs */
        Arg a[2] = { arg_label(lit_label((unsigned char *)o->tok->s, o->tok->len)), arg_imm(o->tok->len) };
        emit_args(a, 2); emit_call("cob_bpush_all");
        return;
    }
    Arg a[2];
    opnd_args(o, &a[0], &a[1], 0, 0);
    emit_args(a, 2);
    emit_call("cob_bpush");
}
static int parse_bexpr(void)
{
    int save_bsp = g_bsp; g_bsp = 0;
    struct { int op, prec; Opnd cnt; } st[64]; int sp = 0;
    int lastprec[32], lv = 0; lastprec[0] = 0;
    int want = 1, width = 0, line = cur()->line;
    static const int prec[] = { 0, 3, 1, 2, 4 };
    for (;;) {
        Tok *t = cur(); int op = bool_op(t);
        if (want) {
            if (op == BO_NOT) {
                /* B-NOT is an operation too: a shift after its operand
                 * takes its precedence (8.8.2 rule 7b; cobol ISSUES-94 B16) */
                advance(); st[sp].op = BO_NOT; st[sp].prec = 4; sp++; lastprec[lv] = 4; continue;
            }
            if (t->kind == T_LP) {
                advance();
                if (sp == 64 || lv == 31) die_at(t->line, "a boolean expression nested too deeply");
                st[sp].op = BO_PAREN; st[sp].prec = 0; sp++; lastprec[++lv] = 0; continue;
            }
            if (op) die_at(t->line, "a boolean operand is expected before %s (2023 8.8.2, Table 4)", t->s);
            if (!at_operand() || (t->kind == T_WORD && is_verb(t->s))) die_at(line, "a boolean expression ends without an operand (2023 8.8.2 rule 2)");
            Opnd o; parse_operand(&o);
            if (o.kind == O_ALL && !o.tok->boolv) die_at(o.line, "ALL in a boolean expression takes a boolean literal");
            if (o.kind == O_FIG) bool_fig_opnd(&o, 1);            /* ZERO: a boolean zero, extended as needed */
            if (!opnd_is_boolean(&o)) die_at(o.line, "a boolean expression takes boolean operands (2023 8.8.2)");
            bool_emit_operand(&o);
            int w = bool_opnd_len(&o); if (w > width) width = w;
            want = 0;
            continue;
        }
        if (t->kind == T_RP && lv > 0) {
            advance();
            while (sp && st[sp - 1].op != BO_PAREN) { sp--; bool_emit_op(st[sp].op, &st[sp].cnt); }
            sp--; lv--;
            continue;
        }
        if (op == BO_AND || op == BO_OR || op == BO_XOR) {
            advance();
            int p = prec[op];
            while (sp && st[sp - 1].op != BO_PAREN && st[sp - 1].prec >= p) { sp--; bool_emit_op(st[sp].op, &st[sp].cnt); }
            if (sp == 64) die_at(t->line, "a boolean expression too long");
            st[sp].op = op; st[sp].prec = p; sp++;
            lastprec[lv] = p; want = 1;
            continue;
        }
        if (op >= BO_SL && op <= BO_SRC) {
            advance();
            int p = lastprec[lv] ? lastprec[lv] : 3;
            Opnd cnt; parse_operand(&cnt);
            if (!((cnt.kind == O_NUM && numlit_is_int(&cnt.num) && !cnt.num.neg) || (cnt.kind == O_REF && is_int_item(cnt.ref.sym))))
                die_at(cnt.line, "a boolean shift takes an integer (2023 8.8.2 rule 5)");
            while (sp && st[sp - 1].op != BO_PAREN && st[sp - 1].prec >= p) { sp--; bool_emit_op(st[sp].op, &st[sp].cnt); }
            bool_emit_op(op, &cnt);
            continue;
        }
        break;
    }
    if (want) die_at(line, "a boolean expression ends without an operand");
    while (sp) {
        sp--;
        if (st[sp].op == BO_PAREN) die_at(line, "unbalanced parentheses in a boolean expression");
        bool_emit_op(st[sp].op, &st[sp].cnt);
    }
    g_bexpr_all = g_bsp == 1 && g_bsim[0];        /* the whole expression one ALL literal */
    g_bsp = save_bsp;
    return width;
}

/* does the parenthesis at the cursor open a boolean expression: a boolean
 * operator before its match */
static int paren_is_boolean(void)
{
    int depth = 0;
    for (int i = g_tp; i < g_ntok; i++) {
        if (g_tok[i].kind == T_LP) depth++;
        else if (g_tok[i].kind == T_RP) { if (--depth == 0) return 0; }
        else if (g_tok[i].kind == T_PERIOD) return 0;
        else if (bool_op(&g_tok[i])) return 1;
    }
    return 0;
}

/* a boolean expression as a condition operand, re-parsed when emitted */
static Opnd bexpr_opnd(void)
{
    Opnd o; memset(&o, 0, sizeof o);
    o.kind = O_BEXPR; o.line = cur()->line; o.e_start = g_tp;
    g_noemit++; o.fsize = parse_bexpr(); g_noemit--;
    o.e_end = g_tp;
    return o;
}

/* push any boolean operand, an expression's value included */
static void bool_push(Opnd *o)
{
    if (o->kind == O_BEXPR) { int save = g_tp; g_tp = o->e_start; parse_bexpr(); g_tp = save; return; }
    bool_emit_operand(o);
}

/* a condition operand: a plain operand, or an arithmetic expression */
static Opnd parse_cond_operand(void)
{
    if (g_std >= 2002) {
        /* a boolean expression: B-NOT, a parenthesis holding a boolean
         * operator, or a boolean operand followed by one */
        if (bool_op(cur()) == BO_NOT) return bexpr_opnd();
        if (cur()->kind == T_LP && !paren_is_condition() && paren_is_boolean()) return bexpr_opnd();
        int start = g_tp;
        if ((cur()->kind == T_WORD || cur()->kind == T_STR) && at_operand()) {
            g_noemit++; Opnd x; parse_operand(&x); g_noemit--;
            int op = bool_op(cur());
            g_tp = start;
            if (op && op != BO_NOT && opnd_is_boolean(&x)) return bexpr_opnd();
        }
    }
    if (cur()->kind == T_LP && !paren_is_condition()) return expr_opnd();
    if (cur()->kind == T_OP && (!strcmp(cur()->s, "-") || !strcmp(cur()->s, "+"))) return expr_opnd();   /* a unary sign begins an expression */
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
    if (g_abbr_op >= 0 && ((cur()->kind == T_OP && strchr("=<>", cur()->s[0])) || at_word("equal") || at_word("equals") || at_word("greater") || at_word("less") || at_word("is"))) {
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
        else if (g_std >= 2002 && !strcmp(t->s, "boolean")) klass = -2;     /* 2023 8.8.4.4: each position 0 or 1 */
        if (klass < 0)
            for (int i = 0; i < g_nclass; i++) if (!strcmp(t->s, g_class[i].name)) klass = 4 + i;
        if (klass >= 0 || klass == -2) {
            if (x.kind != O_REF) die_at(line, "a class condition needs a data item");
            if (x.ref.sym->strong) die_at(line, "a strongly-typed group takes no class condition (2023 8.8.4.4.3 rule 1)");
            if (klass == -2 && is_numeric_sym(x.ref.sym)) die_at(line, "BOOLEAN is no class test for the numeric item '%s' (2023 8.8.4.4.3 rule 5)", x.ref.sym->name);
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
    if (op < 0 && opnd_is_boolean(&x)) {
        /* a simple boolean condition (2023 8.8.4.3): one boolean position,
         * true when it is 1 */
        int len = x.kind == O_BEXPR ? x.fsize : x.kind == O_STR ? x.tok->len : x.kind == O_FUNC ? x.fsize :
                  x.ref.rm ? (int)x.ref.rm_len : bool_positions(x.ref.sym);
        if (len != 1) die_at(line, "a boolean condition takes a boolean item of one position (2023 8.8.4.3.3 rule 1)");
        Tok *one = xmalloc(sizeof *one); memset(one, 0, sizeof *one);
        one->kind = T_STR; one->s = "1"; one->len = 1; one->boolv = 1; one->line = line;
        Opnd y; memset(&y, 0, sizeof y); y.kind = O_STR; y.tok = one; y.line = line;
        return cond_rel(&x, R_EQ, &y, neg);
    }
    if (op < 0) {
        if (x.kind == O_REF && x.ref.sym->is_cond) return cond_88(&x.ref, neg);
        if (g_abbr_op >= 0)             /* an object alone: the last relation's subject and operator */
            return cond_rel(&g_abbr_x, g_abbr_op, &x, g_abbr_neg ^ neg);
        if (x.kind == O_REF && !neg)
            die_at(line, "expected a relational operator after '%s'", x.ref.sym->name);
        die_at(line, "expected a relational operator, found %s", tok_desc(t));
    }
    Opnd y = parse_cond_operand();

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
    int top = g_cond_depth == 0, uc0 = g_nucall;
    if (g_cond_depth++ == 0) g_abbr_op = -1;       /* a new condition: nothing to abbreviate yet */
    Cond *a = parse_and();
    while (accept_word("or")) a = cond_bin(C_OR, a, parse_and());
    g_cond_depth--;
    if (top && g_nucall > uc0) {
        /* user functions in the condition are called where it is evaluated,
         * which for PERFORM UNTIL or a WHEN is not where it was parsed */
        Cond *r = cond_new(C_AND); *r = *a; r->uc0 = uc0; r->uc1 = g_nucall;
        a = r;
    }
    return a;
}

/* r1 = 0/1 for a simple condition */
static void emit_cond_value(Cond *c)
{
    if (c->uc1 > c->uc0) emit_ucalls(c->uc0, c->uc1);
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
            a[2] = arg_imm(c->klass == -2 ? 4 : c->klass);       /* -2: BOOLEAN, the runtime's kind 4 */
            emit_args(a, 3);
            emit_call("cob_class");
        }
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    /* C_REL */
    if (c->ptr) {
        emit_ptr_value(&c->x, "r1");
        int base = g_slot_base++;
        if (g_slot_base > NSLOTS) die_at(c->x.line, "internal: too many staged operands");
        emit("\tstw sp+%d, r1", SLOT(base));
        emit_ptr_value(&c->y, "r1");
        emit("\tldw r2, sp+%d", SLOT(base));
        g_slot_base = base;
        emit("\t%s r1, r2, r1", c->op == R_EQ ? "seq" : "sne");
        if (c->neg) emit("\txori r1, r1, 1");
        return;
    }
    if (c->x.kind == O_REF && c->x.ref.sym->strong && !c->x.ref.rm) {
        /* two groups of one strong type: element by element, in order
         * (8.8.4.2.12), from a table of each elementary item's offset in
         * the group and descriptor */
        int lab = new_label();
        emit("\t.data");
        emit("\t.p2align 2");
        emit(".L%d:", lab);
        int n = strong_table(c->x.ref.sym, 0);
        emit("\t.text");
        char tl[32]; snprintf(tl, sizeof tl, ".L%d", lab);
        Arg a[4] = { arg_ref(&c->x.ref), arg_ref(&c->y.ref), arg_label(tl), arg_imm(n) };
        emit_args(a, 4);
        emit_call("cob_cmp_struct");
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
    if (c->x.kind == O_BEXPR || c->y.kind == O_BEXPR || c->bstack) {
        bool_push(&c->x);
        bool_push(&c->y);
        emit_call("cob_bcmp");
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
    if (cmp_is_onebyte(&c->x, &c->y)) {
        emit_onebyte_value(&c->x);
        emit("\tstw sp+%d, r1", SLOT_A);
        emit_onebyte_value(&c->y);
        emit("\tldw r2, sp+%d", SLOT_A);
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r2, r1"); break;
        case R_NE: emit("\tsne r1, r2, r1"); break;
        case R_LT: emit("\tsltu r1, r2, r1"); break;
        case R_GT: emit("\tsgtu r1, r2, r1"); break;
        case R_LE: emit("\tsleu r1, r2, r1"); break;
        case R_GE: emit("\tsgeu r1, r2, r1"); break;
        }
    } else if (cmp_is_bytewise(&c->x, &c->y)) {
        Arg a[3] = { arg_ref(&c->x.ref), arg_ref(&c->y.ref), arg_imm(c->x.ref.sym->size) };
        emit_args(a, 3); emit_call("memcmp");
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r1, r0"); break;
        case R_NE: emit("\tsne r1, r1, r0"); break;
        case R_LT: emit("\tslt r1, r1, r0"); break;
        case R_GT: emit("\tsgt r1, r1, r0"); break;
        case R_LE: emit("\tsle r1, r1, r0"); break;
        case R_GE: emit("\tsge r1, r1, r0"); break;
        }
    } else if (opnd_hot_cmp(&c->x) && opnd_hot_cmp(&c->y)) {
        /* unsigned ordering whenever neither side can be negative: it is
         * equally correct for the operands a signed compare would also have
         * handled, and it is the only correct one when a four-byte unsigned
         * item uses the top bit.  EQ and NE do not care either way. */
        int u = opnd_nonneg(&c->x) && opnd_nonneg(&c->y);
        emit_cmp_value(&c->x);
        emit("\tstw sp+%d, r1", SLOT_A);
        emit_cmp_value(&c->y);
        emit("\tldw r2, sp+%d", SLOT_A);
        switch (c->op) {
        case R_EQ: emit("\tseq r1, r2, r1"); break;
        case R_NE: emit("\tsne r1, r2, r1"); break;
        case R_LT: emit(u ? "\tsltu r1, r2, r1" : "\tslt r1, r2, r1"); break;
        case R_GT: emit(u ? "\tsgtu r1, r2, r1" : "\tsgt r1, r2, r1"); break;
        case R_LE: emit(u ? "\tsleu r1, r2, r1" : "\tsle r1, r2, r1"); break;
        case R_GE: emit(u ? "\tsgeu r1, r2, r1" : "\tsge r1, r2, r1"); break;
        }
    } else {
        Arg a[4];
        /* a figurative constant or ALL literal against a national operand
         * is national itself: HIGH-VALUE is U+FFFF, not the byte FF */
        if (opnd_is_national(&c->x)) nat_fig_opnd(&c->y, opnd_size_bound(&c->x));
        if (opnd_is_national(&c->y)) nat_fig_opnd(&c->x, opnd_size_bound(&c->y));
        int xs = opnd_size_bound(&c->x), ys = opnd_size_bound(&c->y);
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
    if (c->uc1 > c->uc0) emit_ucalls(c->uc0, c->uc1);
    switch (c->kind) {
    case C_AND: cond_jump_false(c->a, L); cond_jump_false(c->b, L); return;
    case C_OR: { int Lt = new_label(); cond_jump_true(c->a, Lt); cond_jump_false(c->b, L); emit_label(Lt); return; }
    case C_NOT: cond_jump_true(c->a, L); return;
    default: emit_cond_value(c); emit("\tbeq r1, r0, .L%d", L); return;
    }
}

static void cond_jump_true(Cond *c, int L)
{
    if (c->uc1 > c->uc0) emit_ucalls(c->uc0, c->uc1);
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
        "sort", "start", "stop", "string", "subtract", "suppress", "terminate", "unlock",
        "unstring", "use", "write", "next", NULL };
    for (int i = 0; verbs[i]; i++) if (!strcmp(w, verbs[i])) return 1;
    if (g_std >= 2002 && (!strcmp(w, "raise") || !strcmp(w, "resume") || !strcmp(w, "allocate") || !strcmp(w, "free")))
        return 1;                                   /* COBOL 2002's verbs */
    return 0;
}

static int is_terminator(const char *w)
{
    static const char *t[] = { "else", "end-if", "end-perform", "when", "end-evaluate",
        "end-read", "end-write", "end-add", "end-subtract", "end-multiply", "end-divide",
        "end-compute", "end-call", "end-string", "end-unstring", "end-search", "end-start",
        "end-delete", "end-rewrite", "end-return", "end-accept", "end-display", "end-program", NULL };
    for (int i = 0; t[i]; i++) if (!strcmp(w, t[i])) return 1;
    if (g_std >= 2002 && !strcmp(w, "finally")) return 1;     /* an exception-checking PERFORM's (cobol ISSUES-89) */
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
    /* OTHER, TRUE, FALSE, ANY became reserved with COBOL-85; RM/COBOL 2
     * programs declare data items by those names (PAACEMP: MOVE ... TO OTHER (QY)).
     * A declared item wins over the clause word. */
    if ((!strcmp(t->s, "other") || !strcmp(t->s, "true") || !strcmp(t->s, "false") || !strcmp(t->s, "any"))
        && sym_lookup_quiet(t->s)) return 1;
    for (int i = 0; clause[i]; i++) if (!strcmp(t->s, clause[i])) return 0;
    return 1;
}

static void parse_statement(void);
static void parse_statements(void)
{
    while (!at_scope_end()) parse_statement();
}

static int g_sentence_label = -1;   /* NEXT SENTENCE target, made on demand */

/* ---- paragraphs ------------------------------------------------------- */

typedef struct { char name[64], oname[64]; int id, is_section, line, section, unit, in_decl; } Para;   /* oname: as written */   /* section: id of the enclosing section, 0 none; unit: where it is */
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

static int g_prescan_decl;          /* the prescan is between DECLARATIVES and END DECLARATIVES */
static Para *para_add(const char *name, const char *oname, int is_section, int line)
{
    user_word(name, line, is_section ? "a section" : "a paragraph");
    if (is_section) { for (int i = g_para_base; i < g_npara; i++) if (!strcmp(g_para[i].name, name)) die_at(line, "the procedure-name '%s' is declared twice", name); }
    else if (para_find_in(name, g_cur_sec_id)) die_at(line, "the paragraph '%s' is declared twice in the same section", name);
    if (g_npara == g_pcap) { g_pcap = g_pcap ? g_pcap * 2 : 64; g_para = realloc(g_para, g_pcap * sizeof *g_para); }
    Para *p = &g_para[g_npara];
    snprintf(p->name, sizeof p->name, "%s", name);
    snprintf(p->oname, sizeof p->oname, "%s", oname);
    p->id = g_npara + 1; p->is_section = is_section; p->line = line; p->unit = g_unit; p->in_decl = g_prescan_decl;
    p->section = is_section ? 0 : (g_cur_sec_id >= 0 ? g_cur_sec_id : 0);
    if (is_section) g_cur_sec_id = p->id;
    g_npara++;
    return p;
}

static void emit_para_label(Para *p) { emit(".Lp%d_%d:\t# %s%s", g_unit, p->id, p->name, p->is_section ? " section" : ""); }

/* prescan the Procedure Division for paragraph and section headers */
/* ALTER (obsolete in the 1985 text; NC302M, NC303M and NC401M use it): a
 * paragraph named in an ALTER statement holds one GO TO, which jumps
 * through a cell the ALTER rewrites.  The names are gathered before the
 * procedure division is compiled; the cells are laid out with the unit's
 * data, each initialised to the GO TO's own target (or 0 for a bare GO TO). */
static char g_altname[64][64]; static int g_naltname;
static struct { int para, target; } g_altcell[64]; static int g_naltcell;
static Para *g_cur_para;
static int is_altered_para(const char *name)
{
    for (int i = 0; i < g_naltname; i++) if (!strcmp(g_altname[i], name)) return 1;
    return 0;
}

static void prescan_paragraphs(int from)
{
    int sentence_start = 1;
    g_prescan_decl = 0;
    g_cur_sec_id = -1;
    for (int i = from; i < g_ntok; i++) {
        Tok *t = &g_tok[i];
        if (t->kind == T_EOF) break;
        if (t->kind == T_WORD && !strcmp(t->s, "alter")) {
            /* ALTER p1 TO [PROCEED TO] p2 [p3 TO [PROCEED TO] p4]... -- anywhere
             * in a sentence: GENSRT19's sit inside IF/ELSE (GitHub #36).  ALTER
             * is a reserved word, so the match cannot be a data-name. */
            for (int j = i + 1; j + 2 < g_ntok && g_tok[j].kind == T_WORD && is_word(&g_tok[j + 1], "to"); ) {
                if (g_naltname < 64) snprintf(g_altname[g_naltname++], 64, "%s", g_tok[j].s);
                j += 2;
                if (is_word(&g_tok[j], "proceed") && is_word(&g_tok[j + 1], "to")) j += 2;
                if (g_tok[j].kind != T_WORD) break;
                j++;
            }
        }
        if (sentence_start && t->kind == T_NUM && !strchr(t->s, '.') && !strchr(t->s, '+') && !strchr(t->s, '-')) {
            /* a procedure-name of digits only (NC107A's paragraphs 3, 4, 5) */
            if (g_tok[i + 1].kind == T_PERIOD) para_add(t->s, tok_orig(t), 0, t->line);
            else if (is_word(&g_tok[i + 1], "section") && g_tok[i + 2].kind == T_PERIOD) para_add(t->s, tok_orig(t), 1, t->line);
        }
        if (sentence_start && t->kind == T_WORD && !is_verb(t->s) && !is_terminator(t->s)) {
            if (!strcmp(t->s, "declaratives")) { g_prescan_decl = 1; }
            else if (!strcmp(t->s, "end") && (is_word(&g_tok[i + 1], "declaratives") || is_word(&g_tok[i + 1], "program"))) {
                if (is_word(&g_tok[i + 1], "program")) break;
                g_prescan_decl = 0;
            }
            else if ((!strcmp(t->s, "identification") || !strcmp(t->s, "id")) && is_word(&g_tok[i + 1], "division")) break;   /* a contained program's */
            else if (g_tok[i + 1].kind == T_PERIOD) { para_add(t->s, tok_orig(t), 0, t->line); }
            else if (is_word(&g_tok[i + 1], "section") && g_tok[i + 2].kind == T_PERIOD) para_add(t->s, tok_orig(t), 1, t->line);
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

/* ---- RM/COBOL positioned DISPLAY / ACCEPT (GitHub #32, #33) ------------
 *   DISPLAY x LINE n [,] POSITION m [,] ERASE [EOS|EOL] [,] HIGH|LOW|REVERSE
 *           [,] SIZE n ...        DISPLAY x AT rrcc [WITH ERASE EOS|EOL]
 *   ACCEPT  x LINE n POSITION m PROMPT [UPDATE] [NO BEEP] [ECHO] [TAB] ...
 *           ACCEPT x AT rrcc [WITH PROMPT]
 * The Open Systems suite (~/open) paints every screen this way: RM never
 * had a SCREEN SECTION.  Each statement becomes a screen of its own, one
 * slot per operand, on the SCREEN SECTION runtime.  LINE/POSITION are the
 * slot's line/col -- 0 when absent, which the runtime reads as "the line
 * after the last positioned statement" / column 1 -- and an identifier's
 * value is stored into the slot before the call (AT rrcc likewise, split by
 * cob_scr_at).  SIZE is the width; ERASE, PROMPT and NO BEEP are the slot's
 * ext bits; UPDATE makes the ACCEPT slot USING; HIGH/LOW/REVERSE are the
 * flags slots already carry.  ECHO, OFF, TAB, CONVERT, BLINK, BEEP, UNIT
 * and CONTROL are accepted and ignored. */
#define SCRF_SIZE 32               /* sizeof(cob_scr_field) on the guest */

static int pos_word_at(int i)      /* token i begins a positioning clause */
{
    Tok *t = &g_tok[i];
    if (t->kind != T_WORD) return 0;
    if (i > 0 && is_word(&g_tok[i - 1], "function")) return 0;   /* FUNCTION REVERSE is the function, not reverse video */
    static const char *strong[] = { "line", "position", "erase", "prompt", "size", "high", "low", "reverse", "update", "at", NULL };
    for (int k = 0; strong[k]; k++) if (!strcmp(t->s, strong[k])) return 1;
    if (!strcmp(t->s, "no") && is_word(&g_tok[i + 1], "beep")) return 1;
    if (!strcmp(t->s, "with") && (is_word(&g_tok[i + 1], "erase") || is_word(&g_tok[i + 1], "prompt") ||
                                  (is_word(&g_tok[i + 1], "no") && is_word(&g_tok[i + 2], "beep")))) return 1;
    return 0;
}

/* does the DISPLAY/ACCEPT at g_tp carry a positioning clause?  A look ahead
 * to the end of the sentence: a period, the next verb, or a terminator. */
static int stmt_positioned(void)
{
    for (int i = g_tp; i < g_ntok; i++) {
        Tok *t = &g_tok[i];
        if (t->kind == T_PERIOD || t->kind == T_EOF) return 0;
        if (t->kind == T_WORD && i > g_tp && (is_verb(t->s) || is_terminator(t->s))) return 0;
        if (t->kind == T_WORD) {
            /* the phrases of an enclosing statement end the DISPLAY/ACCEPT:
             * READ ... AT END DISPLAY x NOT AT END ..., COMPUTE ... ON SIZE
             * ERROR DISPLAY x NOT ON SIZE ERROR ... (the suite's compute and
             * lineseq tests), IF ... ELSE, EVALUATE ... WHEN */
            static const char *stop[] = { "not", "on", "else", "when", "invalid", "exception", "overflow", "end-of-page", "eop", "also", NULL };
            for (int k = 0; stop[k]; k++) if (!strcmp(t->s, stop[k])) return 0;
            if (!strcmp(t->s, "at") && (is_word(&g_tok[i + 1], "end") || is_word(&g_tok[i + 1], "end-of-page") || is_word(&g_tok[i + 1], "eop"))) return 0;
            if (!strcmp(t->s, "size") && is_word(&g_tok[i + 1], "error")) return 0;
        }
        if (pos_word_at(i)) return 1;
    }
    return 0;
}

/* LINE n / POSITION n: an integer literal, or an identifier whose value the
 * statement stores into the slot at run time (tp: where it sits) */
static SField *g_pos_field;    /* the slot pos_int is filling: an explicit 0 marks it CONT */
static void pos_int(int *val, int *tp, const char *what)
{
    accept_word("is"); accept_word("number");
    if (cur()->kind == T_NUM) {
        *val = atoi(cur()->s); advance();
        /* RM: LINE 0 / POSITION 0 is "where the cursor is", the position after
         * the last thing painted (APPKJRNL builds a title from three DISPLAYs);
         * an omitted clause is the next line / column 1.  The runtime's
         * continue-after-the-last-slot rule covers the explicit zero. */
        if (*val == 0 && g_pos_field) g_pos_field->ext |= COB_SX_CONT;
        return;
    }
    if (cur()->kind != T_WORD || !tp) die_at(cur()->line, "%s needs an integer%s", what, tp ? " or a numeric identifier" : "");
    *tp = g_tp;
    Ref r; parse_ref(&r);
    if (!is_numeric_sym(r.sym)) die_at(r.line, "%s needs a numeric identifier", what);
}

static void parse_pos_clauses(SField *f, int is_accept)
{
    g_pos_field = f;
    for (;;) {
        if (accept_word("with")) continue;
        if (accept_word("line")) { pos_int(&f->line, &f->line_tp, "LINE"); continue; }
        if (accept_word("position") || accept_word("column") || accept_word("col")) { pos_int(&f->col, &f->col_tp, "POSITION"); continue; }
        if (accept_word("at")) {
            if (accept_word("line")) {
                pos_int(&f->line, &f->line_tp, "AT LINE");
                if (accept_word("position") || accept_word("column") || accept_word("col")) pos_int(&f->col, &f->col_tp, "COLUMN");
                continue;
            }
            if (cur()->kind == T_NUM) { int v = atoi(cur()->s); advance(); f->line = v / 100; f->col = v % 100; continue; }
            if (cur()->kind != T_WORD) die_at(cur()->line, "AT needs rrcc or a numeric identifier");
            f->at_tp = g_tp;
            { Ref r; parse_ref(&r); if (!is_numeric_sym(r.sym)) die_at(r.line, "AT needs a numeric identifier"); }
            continue;
        }
        if (accept_word("erase")) {
            if (accept_word("eos")) f->ext |= COB_SX_ERASE_EOS;
            else if (accept_word("eol")) f->ext |= COB_SX_ERASE_EOL;
            else { accept_word("screen"); f->ext |= COB_SX_ERASE_ALL; }   /* ERASE [SCREEN]: the whole screen */
            continue;
        }
        if (accept_word("prompt")) { f->ext |= COB_SX_PROMPT; if (cur()->kind == T_STR) { f->prompt = (unsigned char)cur()->s[0]; advance(); } continue; }
        if (accept_word("size")) { pos_int(&f->width, NULL, "SIZE"); continue; }
        if (accept_word("high")) { f->flags |= COB_SF_HIGHLIGHT; continue; }
        if (accept_word("low")) { f->flags |= COB_SF_LOWLIGHT; continue; }
        if (accept_word("reverse") || accept_word("reverse-video")) { f->flags |= COB_SF_REVERSE; continue; }
        if (accept_word("update")) { if (is_accept) f->kind = COB_SCR_USING; continue; }
        if (accept_word("no")) { expect_word("beep"); f->ext |= COB_SX_NOBEEP; continue; }
        if (accept_word("blink") || accept_word("echo") || accept_word("off") || accept_word("tab") || accept_word("convert") || accept_word("beep")) continue;
        if (accept_word("unit") || accept_word("control")) { Opnd o; parse_operand(&o); continue; }
        break;
    }
}

static Screen *screen_synth(void)
{
    if (g_nscreen == g_scrcap) { g_scrcap = g_scrcap ? g_scrcap * 2 : 4; g_screens = realloc(g_screens, g_scrcap * sizeof *g_screens); }
    Screen *sc = &g_screens[g_nscreen++];
    memset(sc, 0, sizeof *sc);
    snprintf(sc->name, sizeof sc->name, "(positioned %d)", g_nscreen);   /* not a word: screen_ref never matches it */
    return sc;
}

static SField *screen_synth_field(Screen *sc)
{
    if (sc->nf == sc->fcap) { sc->fcap = sc->fcap ? sc->fcap * 2 : 4; sc->f = realloc(sc->f, sc->fcap * sizeof *sc->f); }
    SField *f = &sc->f[sc->nf++];
    memset(f, 0, sizeof *f);
    f->fg = f->bg = 255; f->ext = COB_SX_POS; f->srcline = cur()->line;
    return f;
}

/* a literal of width bytes: len bytes of text padded with fill, or, fill
 * < 0, the text repeated (a figurative constant, ALL) */
static Tok *pos_literal(const char *bytes, int len, int width, int fill)
{
    Tok *t = xmalloc(sizeof *t); memset(t, 0, sizeof *t);
    t->kind = T_STR; t->s = xmalloc(width + 1); t->len = width;
    for (int i = 0; i < width; i++) t->s[i] = i < len ? bytes[i] : fill < 0 ? bytes[i % len] : (char)fill;
    t->s[width] = 0;
    return t;
}

static void emit_pos_int(int tp)   /* r1 = the integer value of the identifier at tp */
{
    int save = g_tp; g_tp = tp;
    Opnd n; parse_operand(&n);
    g_tp = save;
    if (opnd_hot_int(&n)) emit_hot_value(&n);
    else { Arg a[2] = { arg_ref(&n.ref), arg_desc(sym_desc(n.ref.sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
}

/* the statement: item addresses and run-time positions into the slots, then the runtime */
static void emit_pos_stmt(int si, const char *fn)
{
    Screen *sc = &g_screens[si];
    emit_screen_dyn_fill(sc, 0, sc->nf);
    char rec[48]; snprintf(rec, sizeof rec, ".Lscrf%d_%d", g_unit, si);
    for (int k = 0; k < sc->nf; k++) {
        SField *f = &sc->f[k];
        if (f->line_tp) { emit_pos_int(f->line_tp); emit_la_off("r2", rec, k * SCRF_SIZE + 2); emit("\tsth r2+0, r1"); }
        if (f->col_tp)  { emit_pos_int(f->col_tp);  emit_la_off("r2", rec, k * SCRF_SIZE + 4); emit("\tsth r2+0, r1"); }
        if (f->at_tp)   { emit_pos_int(f->at_tp); emit("\tadd r4, r0, r1"); emit_la_off("r3", rec, k * SCRF_SIZE); emit_call("cob_scr_at"); }
    }
    char lab[48]; snprintf(lab, sizeof lab, ".Lscr%d_%d", g_unit, si);
    emit_la("r3", lab); emit_call(fn);
}

static void parse_display_positioned(void)
{
    int si = (int)(screen_synth() - g_screens);
    int first = 1;
    for (;;) {
        Tok *t = cur();
        if (t->kind == T_PERIOD || t->kind == T_EOF) break;
        if (!at_operand() && !(t->kind == T_WORD && (is_figurative(t->s) || !strcmp(t->s, "all")))) break;
        int tp = g_tp;
        Opnd o; parse_operand(&o);
        SField *f = screen_synth_field(&g_screens[si]);
        if (!first) f->ext |= COB_SX_CONT;
        first = 0;
        parse_pos_clauses(f, 0);
        switch (o.kind) {
        case O_REF:
            if (o.ref.rm) die_at(o.line, "reference modification in a positioned DISPLAY is not implemented");
            f->kind = COB_SCR_FROM; f->item = o.ref.sym; f->dyn = 1; f->ref_tp = tp;
            f->has_pic = 1;
            if (sym_is_national(o.ref.sym)) {
                /* national text in columns (cobol ISSUES-92): a column a character position, SIZE counting columns */
                int n = o.ref.sym->size / 2;
                f->pi.category = PIC_NATIONAL; f->pi.bytes = 2 * n;
                if (!f->width) f->width = n;
                break;
            }
            if (!f->width) f->width = !o.ref.sym->is_group && o.ref.sym->usage == U_NATIONAL ? o.ref.sym->size / 2 : o.ref.sym->size;
            f->pi.category = PIC_ALPHANUMERIC; f->pi.bytes = f->width;
            break;
        case O_STR:
            f->kind = COB_SCR_VALUE;
            if (o.tok->nat) {
                f->value = o.tok; f->natlit = 1;
                if (!f->width) f->width = nat_lit_cols((const unsigned char *)o.tok->s, o.tok->len);
                break;
            }
            if (!f->width || f->width == o.tok->len) { f->value = o.tok; f->width = o.tok->len; }
            else f->value = pos_literal(o.tok->s, o.tok->len, f->width, ' ');
            break;
        case O_NUM: {
            char txt[48]; int k = 0;
            if (o.num.neg) txt[k++] = '-';
            for (int i = 0; i < o.num.ndigits; i++) {
                if (o.num.scale && i == o.num.ndigits - o.num.scale) txt[k++] = g_dp_comma ? ',' : '.';
                txt[k++] = o.num.digits[i];
            }
            f->kind = COB_SCR_VALUE; if (!f->width) f->width = k;
            f->value = pos_literal(txt, k, f->width, ' ');
            break; }
        case O_FIG: case O_ALL: {
            int len = o.kind == O_ALL ? o.tok->len : 1;
            char one = o.kind == O_ALL ? 0 : (char)fig_byte(o.tok->s);
            f->kind = COB_SCR_VALUE; if (!f->width) f->width = len;
            f->value = pos_literal(o.kind == O_ALL ? o.tok->s : &one, len, f->width, -1);
            break; }
        default: die_at(o.line, "a positioned DISPLAY takes identifiers and literals");
        }
    }
    emit_pos_stmt(si, "cob_screen_display");
}

static void parse_accept_positioned(Ref *r, int tp)
{
    int si = (int)(screen_synth() - g_screens);
    SField *f = screen_synth_field(&g_screens[si]);
    if (r->rm) die_at(r->line, "reference modification in a positioned ACCEPT is not implemented");
    f->kind = COB_SCR_TO; f->item = r->sym; f->dyn = 1; f->ref_tp = tp;
    parse_pos_clauses(f, 1);
    f->has_pic = 1;
    if (sym_is_national(r->sym)) {
        /* national input (cobol ISSUES-92): the field a column a character position */
        f->pi.category = PIC_NATIONAL; f->pi.bytes = r->sym->size;
        if (!f->width) f->width = r->sym->size / 2;
    } else {
        if (!f->width) f->width = !r->sym->is_group && r->sym->usage == U_NATIONAL ? r->sym->pi.bytes : r->sym->size;
        if (r->sym->is_group || !r->sym->pi.bytes) { f->pi.category = PIC_ALPHANUMERIC; f->pi.bytes = f->width; }
        else { f->pi = r->sym->pi; snprintf(f->pic, sizeof f->pic, "%s", r->sym->pic); }
    }
    if (g_crt_status_name[0]) {                 /* the ACCEPT's ending goes to the CRT STATUS item */
        Sym *cs = sym_lookup(g_crt_status_name, NULL, 0, r->line);
        if (rec_indirect(&g_sym[cs->record])) die_at(r->line, "a %s item cannot be the CRT STATUS yet", indirect_kind(&g_sym[cs->record]));
        char b[80]; snprintf(b, sizeof b, "%s+%d", g_sym[cs->record].label, cs->offset);
        emit_la("r3", b);
        snprintf(b, sizeof b, ".Ld%d", sym_desc(cs));
        emit_la("r4", b);
        emit_call("cob_crt_status");
    }
    emit_pos_stmt(si, "cob_screen_accept");
    accept_word("end-accept");
}

static void parse_accept_1(void);
/* ACCEPT into a national item (cobol ISSUES-70): the text arrives as
 * UTF-8 and is moved, so a byte that begins no UTF-8 character becomes
 * U+FFFD and, checked, EC-DATA-CONVERSION (as a MOVE, 14.9.25 rule 6) */
static int g_accept_nat_check;
static void parse_accept(void)
{
    g_accept_nat_check = 0;
    parse_accept_1();
    if (g_accept_nat_check) {
        int Lok = new_label();
        emit_call("cob_nat_conv_bad");
        emit("\tbeq r1, r0, .L%d", Lok);
        emit_ec_raise(ec_find("EC-DATA-CONVERSION", 0));
        emit_label(Lok);
    }
}
static void parse_accept_1(void)
{
    Tok *t = cur();
    if (t->kind == T_WORD) {
        char scrlab[40]; int sfirst, scount;
        Screen *scp = screen_ref(t->s, scrlab, sizeof scrlab, &sfirst, &scount);
        if (scp) {
            advance();
            emit_screen_dyn_fill(scp, sfirst, scount);
            if (g_crt_status_name[0]) {                 /* the ACCEPT's ending goes to the CRT STATUS item */
                Sym *cs = sym_lookup(g_crt_status_name, NULL, 0, t->line);
                if (rec_indirect(&g_sym[cs->record])) die_at(t->line, "a %s item cannot be the CRT STATUS yet", indirect_kind(&g_sym[cs->record]));
                char b[80]; snprintf(b, sizeof b, "%s+%d", g_sym[cs->record].label, cs->offset);
                emit_la("r3", b);
                snprintf(b, sizeof b, ".Ld%d", sym_desc(cs));
                emit_la("r4", b);
                emit_call("cob_crt_status");
            }
            emit_la("r3", scrlab); emit_call("cob_screen_accept"); return;
        }
    }
    int ref_tp = g_tp;
    Ref r; parse_ref(&r);
    if (r.sym->strong) die_at(r.line, "ACCEPT into the strongly-typed group '%s' (2023 14.9.1.3 rule 1)", r.sym->name);
    int nat = ref_is_national(&r);
    if (stmt_positioned()) {
        parse_accept_positioned(&r, ref_tp); return;
    }
    if (nat && ec_on_name("EC-DATA-CONVERSION")) {
        emit_call("cob_nat_conv_bad");              /* clear what an earlier MOVE left: at end of file nothing is moved */
        g_accept_nat_check = 1;
    }
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
        if (at_word("date") || at_word("day") || at_word("time") || at_word("day-of-week")) {
            /* the unsigned integer of the text -- YYMMDD, YYDDD, HHMMSShh, 1 (Monday) to 7 -- by the MOVE rules */
            int which = at_word("date") ? 0 : at_word("day") ? 1 : at_word("time") ? 2 : 3;
            advance();
            Arg a[3] = { arg_imm(which), arg_ref(&r), arg_desc(sym_desc(r.sym)) };
            emit_args(a, 3);
            emit_call("cob_accept_datetime");
            accept_word("end-accept");
            return;
        }
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
        char scrlab[40]; int sfirst, scount;
        Screen *scp = screen_ref(cur()->s, scrlab, sizeof scrlab, &sfirst, &scount);
        if (scp) { advance(); emit_screen_dyn_fill(scp, sfirst, scount); emit_la("r3", scrlab); emit_call("cob_screen_display"); return; }
    }
    /* DISPLAY n UPON ARGUMENT-NUMBER: the next ARGUMENT-VALUE will be n */
    if (stmt_positioned()) { parse_display_positioned(); return; }
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
            if (o.tok->nat) {                       /* a national literal: written as UTF-8 */
                char *u = xmalloc((size_t)o.tok->len * 2 + 1);
                int un = utf16be_to_utf8((const unsigned char *)o.tok->s, o.tok->len, u);
                Arg a[2] = { arg_label(lit_label((unsigned char *)u, un)), arg_imm(un) };
                emit_args(a, 2); emit_call("cob_display"); free(u); break;
            }
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

/* A copy whose length the compiler knows.  memcpy is a call that saves five
 * registers, and then -- whenever the two addresses are not congruent mod 4,
 * which is the ordinary case for fields packed into a record -- copies a byte
 * at a time: about 90 instructions to move the ten bytes of a PIC 9(10).
 * Below the threshold the same copy is 2n inline instructions and needs no
 * alignment analysis at all, which is what makes it unconditionally safe;
 * above it, the loop earns its prologue back and memcpy is still the answer.
 * a[0] is the destination and a[1] the source, already staged as Args so the
 * subscripted and reference-modified forms marshal the way they always do.
 * GitHub #27. */
/* The threshold, 2026-09-01.  The engines disagree, because slow32-dbt
 * recognises the memcpy entry point by name and substitutes a native stub,
 * while the interpreters execute every instruction the call runs.  Both hosts
 * put the DBT's cliff between 8 and 16, so one constant is right -- but do
 * NOT settle it on bench/b3big, which is a MOVE-only loop and therefore
 * nothing but the thing being measured: at the 0/8 boundary it says 8 on
 * x86-64 and 0 on arm64.  The corpus decides, and says 8 on both.
 *
 *      COPY_INLINE_MAX      0            8           16           40
 *      corpus insns         2099450533   2046857172  1983245824   1963697060
 *      corpus batch.sh (s)  0.40         0.40        0.42         0.43
 *
 * Note the inversion: past 8, guest instructions go down while wall time goes
 * up.  The inline copy is fewer instructions and still slower than the DBT's
 * stub, so instruction count is the wrong metric for this one constant.
 * cobol/ISSUES.md section 24 carries both hosts' tables and the reasoning;
 * bench/sweep.sh re-runs the microbenchmark, but decide on the corpus. */
#ifndef COPY_INLINE_MAX
#define COPY_INLINE_MAX 8
#endif

static void emit_copy_fixed(const Arg *a, int n)
{
    if (n <= 0) return;
    if (n > COPY_INLINE_MAX) {
        Arg b[3] = { a[0], a[1], arg_imm(n) };
        emit_args(b, 3); emit_call("memcpy");
        return;
    }
    emit_args(a, 2);            /* r3 = destination, r4 = source */
    for (int i = 0; i < n; i++) {
        emit("\tldbu r1, r4+%d", i);
        emit("\tstb r3+%d, r1", i);
    }
}

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

/* MOVE ALL literal to a numeric or numeric-edited item.  The literal is
 * repeated to the receiver's character positions and then moved as any
 * alphanumeric literal is: an unsigned integer, aligned on the decimal
 * point (X3.23-1985 IV-11).  The text's own example (XVII-82, from X3J4
 * interpretation B-23): MOVE ALL "123" to a PIC 99V99 item gives 31.00,
 * the digits of "1231" that fit, not the 12.31 a fill would leave. */
static void emit_move(Opnd *src, Ref *dst);
static void emit_move_all_numeric(Opnd *src, Ref *dst, int n)
{
    if (src->tok->len > 1) bp(BP_O9_ALL_NUMERIC, src->line);
    Tok *t = xmalloc(sizeof *t); *t = *src->tok;
    t->s = xmalloc((size_t)n + 1);
    for (int i = 0; i < n; i++) t->s[i] = src->tok->s[i % src->tok->len];
    t->s[n] = 0; t->len = n;
    Opnd lit = *src; lit.kind = O_STR; lit.tok = t;
    emit_move(&lit, dst);
}

/* national data in a MOVE (COBOL 2002 14.9.25; cobol ISSUES-62).  A
 * national receiver takes anything alphanumeric, numeric or national,
 * converted by libcob (UTF-8 to UTF-16BE, national-space padding); a
 * national sender reaches only a national receiver or a group (moved as
 * bytes, general rule 4).  Returns 0 when neither side is national. */
static int opnd_is_national(const Opnd *o)
{
    if (o->kind == O_FUNC) return o->fnat;
    if (o->kind == O_STR || o->kind == O_ALL) return o->tok && o->tok->nat;
    /* a USAGE NATIONAL numeric item reference-modified: a national part (8.4.3.3.4 rule 6c) */
    return o->kind == O_REF && (sym_is_national(o->ref.sym) ||
           (o->ref.rm && !o->ref.sym->is_group && o->ref.sym->usage == U_NATIONAL && !sym_is_boolean(o->ref.sym)));
}

static int ref_is_national(const Ref *r) { return sym_is_national(r->sym); }

/* INSPECT, STRING and UNSTRING take items of usage display or national
 * only (2023 14.9.22.3 rules 1-2, 14.9.43.3 rule 1, 14.9.48.3 rules 2 and
 * 4): not bits (cobol ISSUES-86) */
static void no_bits(const Opnd *o, const char *stmt)
{
    static const char *rule[] = { "INSPECT", "14.9.22.3 rules 1 and 2", "STRING", "14.9.43.3 rule 1", "UNSTRING", "14.9.48.3 rules 2 and 4", NULL };
    const char *r = "";
    for (int i = 0; rule[i]; i += 2) if (!strcmp(stmt, rule[i])) r = rule[i + 1];
    if (o->kind == O_REF && sym_bitlike(o->ref.sym))
        die_at(o->line, "%s takes items of usage display or national, not the USAGE BIT item '%s' (2023 %s)", stmt, o->ref.sym->name, r);
}

/* STRING, UNSTRING: when one operand is national all are (2023 14.9.43.3
 * rule 1, 14.9.48.3 rule 3); a figurative constant takes the class */
static void nat_class_check(const Opnd *o, int nat, const char *stmt, const char *rule)
{
    if (o->kind == O_FIG) return;
    if (opnd_is_national(o) != nat)
        die_at(o->line, "%s: %s operand beside %s ones (2023 %s)", stmt, nat ? "a non-national" : "a national",
               nat ? "national" : "non-national", rule);
}

/* a one-character figurative constant as an address and length: one
 * byte, or one national character */
static void fig_char_args(const Opnd *o, int nat, Arg *addr, Arg *len)
{
    if (nat) {
        unsigned u = nat_fig(o->tok->s); unsigned char two[2] = { (unsigned char)(u >> 8), (unsigned char)u };
        *addr = arg_label(lit_label(two, 2)); *len = arg_imm(2);
    } else {
        unsigned char c = (unsigned char)fig_byte(o->tok->s);
        *addr = arg_label(lit_label(&c, 1)); *len = arg_imm(1);
    }
}

/* a figurative constant or ALL literal, as the national literal of nbytes
 * it stands for beside a national operand */
static void nat_fig_opnd(Opnd *o, int nbytes)
{
    if (o->kind != O_FIG && o->kind != O_ALL) return;
    if (nbytes < 2) nbytes = 2;
    unsigned char *b = xmalloc((size_t)nbytes);
    if (o->kind == O_FIG) {
        unsigned u = nat_fig(o->tok->s);
        for (int i = 0; i + 1 < nbytes; i += 2) { b[i] = (unsigned char)(u >> 8); b[i + 1] = (unsigned char)u; }
    } else {
        const unsigned char *lit = (const unsigned char *)o->tok->s; int len = o->tok->len;
        unsigned char *conv = NULL;
        if (!o->tok->nat) {
            conv = xmalloc((size_t)len * 4 + 2); len = utf8_to_utf16be(lit, len, conv); lit = conv;
            if (len < 0) die_at(o->line, "an ALL literal compared with a national item must be UTF-8 text");
        }
        for (int i = 0; i < nbytes; i++) b[i] = lit[i % len];
        free(conv);
    }
    Tok *t = xmalloc(sizeof *t); *t = *o->tok;
    t->kind = T_STR; t->s = (char *)b; t->len = nbytes & ~1; t->nat = 1;
    o->kind = O_STR; o->tok = t;
}

static int emit_move_national(Opnd *src, Ref *dst)
{
    Sym *d = dst->sym;
    int dn = sym_is_national(d) || (dst->rm && !d->is_group && d->usage == U_NATIONAL && !sym_is_boolean(d)),   /* a national part, 8.4.3.3.4 rule 6c */
        sn = opnd_is_national(src);
    if (!dn && !sn) return 0;
    if (!dn) {
        if (d->is_group) return 0;                  /* a group receives the bytes (14.9.25 general rule 4) */
        if (is_numeric_sym(d) || d->pi.category == PIC_NUMERIC_EDITED) {
            /* national to numeric or numeric-edited: valid (the 14.9.25
             * table), the characters taken as for an alphanumeric sender */
            if (dst->rm) die_at(dst->line, "a reference-modified numeric receiver of national data is not implemented");
            Arg a[4];
            opnd_args(src, &a[0], &a[1], d->size, 1);
            a[2] = arg_ref(dst); a[3] = arg_desc(sym_desc(d));
            emit_args(a, 4); emit_call("cob_move");
            return 1;
        }
        die_at(dst->line, "a national item cannot be moved to the alphanumeric item '%s' (2023 14.9.25): use FUNCTION DISPLAY-OF", d->name);
    }
    /* a numeric sender that is not an integer has no national form (the
     * 14.9.25 table: numeric noninteger to national, no) */
    if ((src->kind == O_NUM && src->num.scale > 0) ||
        (src->kind == O_REF && !src->ref.rm && is_numeric_sym(src->ref.sym) && src->ref.sym->pi.scale > 0))
        die_at(dst->line, "a numeric item that is not an integer cannot be moved to the national item '%s' (2023 14.9.25)", d->name);
    int n = d->size / 2;
    /* a reference-modified receiver: its bytes, known here or at run time */
    Arg dlen = !dst->rm ? arg_imm(d->size) : dst->rm_len ? arg_imm(2 * dst->rm_len) : arg_rlen(dst);
    Arg ddesc = !dst->rm ? arg_desc(sym_desc(d)) : dst->rm_len ? arg_desc(nat_desc(2 * (int)dst->rm_len)) : arg_rdesc(dst);
    if ((src->kind == O_FIG || src->kind == O_ALL) && d->pi.edited && !dst->rm) {
        /* national-edited: the figurative as a national literal of the
         * item's characters, which the move then edits (cobol ISSUES-73) */
        Opnd lit = *src;
        if (lit.kind == O_ALL && !lit.tok->nat) {
            unsigned char *conv = xmalloc((size_t)lit.tok->len * 4 + 2); int len = utf8_to_utf16be((const unsigned char *)lit.tok->s, lit.tok->len, conv);
            if (len < 0) die_at(src->line, "an ALL literal moved to a national item must be UTF-8 text");
            Tok *t = xmalloc(sizeof *t); *t = *lit.tok; t->s = (char *)conv; t->len = len; t->nat = 1; lit.tok = t;
        }
        nat_fig_opnd(&lit, d->size);
        Arg a[4];
        opnd_args(&lit, &a[0], &a[1], d->size, 0);
        a[2] = arg_ref(dst); a[3] = arg_desc(sym_desc(d));
        emit_args(a, 4); emit_call("cob_move");
        return 1;
    }
    if (src->kind == O_FIG && dst->rm) {
        unsigned u = nat_fig(src->tok->s); unsigned char two[2] = { (unsigned char)(u >> 8), (unsigned char)u };
        Arg a[4] = { arg_ref(dst), dlen, arg_label(lit_label(two, 2)), arg_imm(2) };
        emit_args(a, 4); emit_call("cob_fill_all");
        return 1;
    }
    if (src->kind == O_FIG) {
        Arg a[3] = { arg_ref(dst), arg_imm(n), arg_imm((long)nat_fig(src->tok->s)) };
        emit_args(a, 3); emit_call("cob_fill_nat");
        return 1;
    }
    if (src->kind == O_ALL) {
        /* ALL literal: its national characters repeated */
        const unsigned char *lit = (const unsigned char *)src->tok->s; int len = src->tok->len;
        unsigned char *conv = NULL;
        if (!src->tok->nat) {
            conv = xmalloc((size_t)len * 4 + 2); len = utf8_to_utf16be(lit, len, conv); lit = conv;
            if (len < 0) die_at(src->line, "an ALL literal moved to a national item must be UTF-8 text");
        }
        Arg a[4] = { arg_ref(dst), dlen, arg_label(lit_label(lit, len)), arg_imm(len) };
        emit_args(a, 4); emit_call("cob_fill_all");
        free(conv);
        return 1;
    }
    Arg a[4];
    opnd_args(src, &a[0], &a[1], ref_static_len(dst) > 0 ? ref_static_len(dst) : d->size, 0);
    a[2] = arg_ref(dst); a[3] = ddesc;
    emit_args(a, 4); emit_call("cob_move");
    if (!sn && ec_on_name("EC-DATA-CONVERSION")) {
        /* a byte that is not UTF-8 became U+FFFD (14.9.25 general rule 6) */
        int Lok = new_label();
        emit_call("cob_nat_conv_bad");
        emit("\tbeq r1, r0, .L%d", Lok);
        emit_ec_raise(ec_find("EC-DATA-CONVERSION", 0));
        emit_label(Lok);
    }
    return 1;
}

/* boolean positions of a boolean item */
static int bool_positions(const Sym *s) { return sym_bitlike(s) ? s->bits : s->usage == U_NATIONAL ? s->size / 2 : s->size; }

/* a figurative constant or ALL literal beside a boolean operand of n
 * positions: ZERO is boolean zeros, ALL B"..." its value repeated; any
 * other figurative is no boolean value (2023 14.9.25 rule 7) */
static void bool_fig_opnd(Opnd *o, int n)
{
    if (o->kind != O_FIG && o->kind != O_ALL) return;
    if (n < 1) n = 1;
    char *b = xmalloc((size_t)n + 1);
    if (o->kind == O_FIG) {
        if (strncmp(o->tok->s, "zero", 4)) die_at(o->line, "%s is not a boolean value (2023 14.9.25 rule 7)", o->tok->s);
        memset(b, '0', (size_t)n);
    } else {
        /* ALL "1" is as good as ALL B"1": rule 7 bars only characters that
         * are no boolean character (cobol ISSUES-94 B15) */
        Tok *t = o->tok;
        int w = t->nat ? 2 : 1, len = t->len / w;
        char *v = xmalloc((size_t)len + 1);
        for (int i = 0; i < len; i++) {
            unsigned ch = t->nat ? ((unsigned char)t->s[2 * i] << 8 | (unsigned char)t->s[2 * i + 1]) : (unsigned char)t->s[i];
            if (!t->boolv && ch != '0' && ch != '1') die_at(o->line, "ALL %s: a character that is not 0 or 1 is no boolean value (2023 14.9.25.3 rule 7)", tok_desc(t));
            v[i] = (char)ch;
        }
        for (int i = 0; i < n; i++) b[i] = v[i % (len ? len : 1)];
        free(v);
    }
    Tok *t = xmalloc(sizeof *t); *t = *o->tok;
    t->kind = T_STR; t->s = b; t->len = n; t->boolv = 1; t->nat = 0;
    o->kind = O_STR; o->tok = t;
}

static int opnd_is_boolean(const Opnd *o)
{
    if (o->kind == O_STR || o->kind == O_ALL) return o->tok && o->tok->boolv;
    if (o->kind == O_FUNC) return o->fbool;
    if (o->kind == O_BEXPR) return 1;
    return o->kind == O_REF && sym_is_boolean(o->ref.sym);
}

/* MOVE with a boolean side (2023 14.9.25 table): a boolean receiver takes
 * a boolean, alphanumeric or national sender, aligned left, zero-filled
 * or truncated on the right (14.6.8.6); a boolean sender goes to an
 * alphanumeric, national or group receiver as its characters 0 and 1.
 * Numeric and edited categories are no boolean's partners either way. */
static int emit_move_boolean(Opnd *src, Ref *dst)
{
    Sym *d = dst->sym;
    int db = sym_is_boolean(d), sb = opnd_is_boolean(src);
    if (!db && !sb) return 0;
    /* A move with a group on either side -- a group that is not a bit
     * group, which is treated as elementary (13.18.29.4 rule 1b) -- is no
     * elementary move: its bytes are copied without conversion (14.9.25.4
     * rule 4; cobol ISSUES-94 B14).  The group MOVE does that. */
    if (d->is_group && !d->bitgroup && !dst->rm) return 0;
    if (src->kind == O_REF && src->ref.sym->is_group && !src->ref.sym->bitgroup && !src->ref.rm) return 0;
    if (!db) {
        int c = d->pi.category;
        if (c == PIC_ALPHANUMERIC || c == PIC_ALPHANUMERIC_EDITED || c == PIC_NATIONAL) return 0;   /* as its characters */
        die_at(dst->line, "a boolean item cannot be moved to the %s item '%s' (2023 14.9.25)",
               c == PIC_ALPHABETIC ? "alphabetic" : "numeric", d->name);
    }
    int n = dst->rm ? (dst->rm_len ? (int)dst->rm_len : 1) : bool_positions(d);
    Opnd lit;
    if (src->kind == O_ALL && dst->rm && !dst->rm_len) {
        /* ALL to positions known only at run time: repeated to them there
         * (cobol ISSUES-94 B4) */
        lit = *src;
        if (!lit.tok->boolv) { bool_fig_opnd(&lit, lit.tok->len / (lit.tok->nat ? 2 : 1)); lit.kind = O_ALL; }
        bool_emit_operand(&lit);
        Arg a[2] = { arg_ref(dst), arg_rdesc(dst) };
        emit_args(a, 2);
        emit_call("cob_bstore");
        emit_call("cob_bdrop");
        return 1;
    }
    if (src->kind == O_FIG || src->kind == O_ALL) { lit = *src; bool_fig_opnd(&lit, n); src = &lit; }
    else if (!sb) {
        int ok = src->kind == O_STR ||
                 (src->kind == O_REF && (src->ref.sym->is_group || src->ref.sym->pi.category == PIC_ALPHANUMERIC ||
                                         (sym_is_national(src->ref.sym) && !src->ref.sym->pi.edited))) ||
                 (src->kind == O_FUNC && !fn_is_numeric(src->fn));
        if (!ok) die_at(src->line, "only a boolean, alphanumeric or national item can be moved to the boolean item '%s' (2023 14.9.25)", d->name);
    }
    Arg a[4];
    opnd_args(src, &a[0], &a[1], n, 0);
    a[2] = arg_ref(dst);
    a[3] = !dst->rm ? arg_desc(sym_desc(d)) : dst->rm_len && (dst->rm_start || !dst->rm_bit) ? arg_desc(part_desc(dst)) : arg_rdesc(dst);
    emit_args(a, 4); emit_call("cob_move");
    return 1;
}

static void emit_move(Opnd *src, Ref *dst)
{
    Sym *d = dst->sym;
    /* BP-M2: before the ODO-source path returns, so a group-to-group MOVE counts */
    if (d->is_group && !dst->rm && !dst->nsub && has_odo(d)) bp(BP_M2_ODO_RECEIVE, dst->line);
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
    {   /* a strongly-typed group receives only a group of its own type
         * (14.9.25.3 rule 2); as a sender it goes anywhere a group does
         * (Table 16; cobol ISSUES-94 B13) */
        int ss = src->kind == O_REF ? src->ref.sym->strong : 0, ds = d->strong;
        if (ds && ss != ds)
            die_at(dst->line, "MOVE: the strongly-typed group '%s' (%s) receives only a group of the same type, the sender %s (2023 14.9.25.3 rule 2)",
                   d->name, strong_name(ds - 1), ss ? strong_name(ss - 1) : "is not strongly typed");
    }
    if (emit_move_boolean(src, dst)) return;
    if (emit_move_national(src, dst)) return;
    /* Sending and receiving items with byte-identical descriptors -- same
     * category, usage, size, digit count, scale, flags and PICTURE -- so the
     * move is a byte copy.  Descriptors are deduplicated by a whole-struct
     * memcmp, which is why identity is one integer compare here.
     *
     * This is a conformance fix that happens to be fast.  Measured against
     * the oracle 2026-09-01: GnuCOBOL passes the bytes through unchanged,
     * including bytes cob_put_num would never write -- spaces in a numeric
     * field nothing has filled in, an 0xF sign nibble on a COMP-3 record
     * from a foreign system, a COMP holding more than its picture's digits.
     * The generic path decoded and re-encoded all three, so ' 12 45abc '
     * arrived as '0120451230' where GnuCOBOL delivered it verbatim.  The
     * cost went with it: a PIC 9(10) to PIC 9(10) MOVE ran a digit loop out
     * through cob_get_num and a divide loop back through cob_put_num, 646
     * instructions to copy ten bytes.  GitHub #27; tests/free/identmove. */
    if (src->kind == O_REF && !src->ref.rm && !dst->rm && !src->ref.sym->is_cond &&
        sym_desc(src->ref.sym) == sym_desc(d)) {
        Arg a[2] = { arg_ref(dst), arg_ref(&src->ref) };
        emit_copy_fixed(a, d->size);
        return;
    }
    if (src->kind == O_REF && src->ref.sym->is_group && !src->ref.rm && !dst->rm && !src->ref.sym->is_cond) {
        /* a group sending item: an alphanumeric-to-alphanumeric move whatever
         * the receiver -- no conversion, no editing (X3.23 6.18.2; NC105A
         * moves a group to numeric and to edited items and reads the bytes) */
        Sym *s = src->ref.sym;
        Arg a[4] = { arg_ref(&src->ref), arg_imm(s->size), arg_ref(dst), arg_imm(d->size) };
        emit_args(a, 4); emit_li("r7", d->just); emit_call("cob_move_alnum");
        return;
    }
    if (!d->is_group && (d->pi.category == PIC_NUMERIC_EDITED || d->pi.category == PIC_ALPHANUMERIC_EDITED)) {
        int ned = d->pi.category == PIC_NUMERIC_EDITED;
        if (src->kind == O_FIG && !ned) {
            /* MOVE SPACES to an alphanumeric-edited item: a literal of spaces
             * through the edit, the insertion characters appearing */
            unsigned char *f = xmalloc((size_t)d->size); memset(f, fig_byte(src->tok->s), (size_t)d->size);
            Arg a[4] = { arg_label(lit_label(f, d->size)), arg_desc(str_desc(d->size)), arg_ref(dst), arg_desc(sym_desc(d)) };
            free(f);
            emit_args(a, 4); emit_call("cob_move");
            return;
        }
        if (src->kind == O_FIG && !(ned && !strncmp(src->tok->s, "zero", 4))) {
            Arg a[3] = { arg_ref(dst), arg_imm(d->size), arg_imm(fig_byte(src->tok->s)) };
            emit_args(a, 3); emit_call("cob_fill");
            return;
        }
        if (src->kind == O_ALL && ned) { emit_move_all_numeric(src, dst, d->size); return; }
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
                Arg a[2] = { arg_ref(dst), arg_label(l) };
                emit_copy_fixed(a, len);
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
            /* a non-integer numeric item to an alphanumeric one: the 85 text
             * forbids it, the NIST cases (NC105A, NC114M, NC124A) want it --
             * the digits as stored, the sign and the point unrepresented; the
             * cases win (the user's ruling, 2026-08-31) */
            if (!is_numeric_sym(s) && s->size == d->size && !d->just) {
                Arg a[2] = { arg_ref(dst), arg_ref(&src->ref) };
                emit_copy_fixed(a, d->size);
                return;
            }
            if (d->is_group) {
                /* a group receiving item: an alphanumeric-to-alphanumeric move
                 * (a group sending item was taken above) */
                Arg a[4] = { arg_ref(&src->ref), arg_imm(s->size), arg_ref(dst), arg_imm(d->size) };
                emit_args(a, 4); emit_li("r7", d->just); emit_call("cob_move_alnum");
                return;
            }
            Arg a[4] = { arg_ref(&src->ref), arg_desc(sym_desc(s)), arg_ref(dst), arg_desc(sym_desc(d)) };
            emit_args(a, 4); emit_call("cob_move");
            return;
        }
        }
    }

    /* numeric receiver; NULL is a pointer's zero address (SET ... TO NULL) */
    if (src->kind == O_FIG && (!strncmp(src->tok->s, "zero", 4) || (!strncmp(src->tok->s, "null", 4) && d->usage == U_POINTER))) {
        Opnd z; memset(&z, 0, sizeof z); z.kind = O_NUM; numlit_zero(&z.num); z.line = src->line;
        emit_move(&z, dst);
        return;
    }
    if (src->kind == O_FIG || src->kind == O_ALL) {
        if (d->usage != U_DISPLAY) die_at(src->line, "%s cannot be moved to the %s item '%s'", src->tok->s, usage_name(d->usage), d->name);
        if (src->kind == O_ALL) { emit_move_all_numeric(src, dst, d->size); return; }
        Arg a[3] = { arg_ref(dst), arg_imm(d->size), arg_imm(fig_byte(src->tok->s)) };
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
static void emit_store_receivers(Ref *rs, int *rounded, int nr, int hot, int giving, int subtract, int size_err,
                                 long long sum_mag, int sum_nonneg);
static void emit_push(Opnd *o);
static Opnd ref_opnd(const Ref *r);
static int at_size_error_clause(void);
static void parse_size_error_clauses(int size_err, const char *end_word);

static int corr_eligible(Sym *c)
{
    return !c->is_filler && !c->is_cond && c->level != 66 && c->redefines < 0 && !c->occurs && !c->odo_dep[0] &&
           (c->is_group || (c->usage != U_INDEX && c->usage != U_POINTER));    /* 14.7.6 rule 4 */
}

/* The validity of a MOVE by category (2023 14.9.25.3 syntax rules 5, 6,
 * 8 and 10 with Table 16; 85 VI-104 general rule 3a-c).  Boolean moves
 * and strongly-typed groups are checked where they are emitted; a group
 * on either side is an alphanumeric move and always valid, and a
 * reference modification is alphanumeric (or national). */
enum { MC_NONE, MC_ALPHA, MC_ALNUM, MC_ALNUMED, MC_NAT, MC_NATED, MC_INT, MC_NONINT, MC_NUMED };
static const char *mc_name[] = { "", "alphabetic", "alphanumeric", "alphanumeric-edited", "national", "national-edited",
                                 "numeric integer", "numeric noninteger", "numeric-edited" };

static int move_cat_sym(const Sym *s, int rm)
{
    if (s->is_group || sym_is_boolean(s) || sym_bitlike(s)) return MC_NONE;
    if (rm) return sym_is_national(s) || s->usage == U_NATIONAL ? MC_NAT : MC_ALNUM;
    switch (s->pi.category) {
    case PIC_ALPHABETIC: return MC_ALPHA;
    case PIC_ALPHANUMERIC: return MC_ALNUM;
    case PIC_ALPHANUMERIC_EDITED: return MC_ALNUMED;
    case PIC_NATIONAL: return s->pi.edited ? MC_NATED : MC_NAT;
    case PIC_NUMERIC: return s->pi.scale > 0 ? MC_NONINT : MC_INT;
    case PIC_NUMERIC_EDITED: return MC_NUMED;
    }
    return MC_NONE;
}

/* why the move is invalid, into msg, or NULL: MOVE refuses it, and a
 * CORRESPONDING pair it names does not correspond (2023 14.7.6 rule 2,
 * 85 VI-68 6.4.3 rule 2) */
#define MV_BAD(...) do { snprintf(msg, MV_MSG, __VA_ARGS__); return msg; } while (0)
enum { MV_MSG = 256 };
static const char *move_invalid(const Opnd *src, const Ref *dst, char *msg)
{
    const Sym *d = dst->sym;
    const Sym *sy = src->kind == O_REF ? src->ref.sym : NULL;
    /* index and pointer items are set, not moved (rule 1; 85 syntax rule 4) */
    for (int k = 0; k < 2; k++) {
        const Sym *x = k ? d : sy;
        if (x && !x->is_group && (x->usage == U_INDEX || x->usage == U_POINTER))
            MV_BAD("MOVE: the %s item '%s' is not an operand of MOVE; use SET (%s)", x->usage == U_INDEX ? "index" : "pointer", x->name,
                   g_std < 2002 ? "85 VI-103 syntax rule 4" : "2023 14.9.25.3 rule 1");
    }
    int r = move_cat_sym(d, dst->rm);
    int rnum = r == MC_INT || r == MC_NONINT || r == MC_NUMED;
    /* binary-char, -short, -long go only to numeric items (rule 8) */
    if (sy && !src->ref.rm && !sy->is_group && (sy->usage == U_BCHAR || sy->usage == U_UBCHAR || sy->usage == U_SSHORT || sy->usage == U_USHORT ||
                                                   sy->usage == U_SINT || sy->usage == U_UINT) && !rnum)
        MV_BAD("MOVE: the %s item '%s' goes only to a numeric or numeric-edited item, not '%s' (2023 14.9.25.3 rule 8)",
               sy->usage == U_SSHORT || sy->usage == U_USHORT ? "binary-short" : sy->usage == U_SINT || sy->usage == U_UINT ? "binary-long" : "binary-char", sy->name, d->name);
    if (r == MC_NONE) return NULL;
    if (src->kind == O_FIG || src->kind == O_ALL) {
        const char *w = src->tok->s;
        if (src->kind == O_FIG && !strncmp(w, "null", 4)) return NULL;
        int zero = src->kind == O_FIG && !strncmp(w, "zero", 4);
        char up[64]; int n = 0;
        for (; w[n] && n < 63; n++) up[n] = (char)toupper((unsigned char)w[n]);
        up[n] = 0;
        if (zero && r == MC_ALPHA)
            MV_BAD("MOVE: ZERO cannot be moved to the alphabetic item '%s' (%s)", d->name, g_std < 2002 ? "85 VI-104 general rule 3b" : "2023 14.9.25.3 rule 6");
        if (zero || !rnum) return NULL;
        const char *rn = r == MC_NUMED ? "numeric-edited" : "numeric";
        if (g_std < 2002) {
            if (src->kind == O_FIG && !strncmp(w, "space", 5))
                MV_BAD("MOVE: SPACE cannot be moved to the %s item '%s' (85 VI-104 general rule 3a)", rn, d->name);
            return NULL;
        }
        /* an ALL literal of digits (or a symbolic character that is a
         * digit) may go to an integer: an obsolete feature */
        int digits = 1;
        if (src->kind == O_ALL && !src->tok->nat) for (int i = 0; i < src->tok->len; i++) digits &= isdigit((unsigned char)w[i]) != 0;
        else if (src->kind == O_ALL) digits = 0;
        else digits = symch_find(w) >= 0 && isdigit(fig_byte(w));
        if (digits && r == MC_INT) return NULL;
        MV_BAD("MOVE: the figurative constant %s%s cannot be moved to the %s item '%s' (2023 14.9.25.3 rule 5)",
               src->kind == O_ALL ? "ALL " : "", src->kind == O_ALL ? tok_desc(src->tok) : up, rn, d->name);
    }
    int s = MC_NONE;
    if (src->kind == O_NUM) s = numlit_is_int(&src->num) ? MC_INT : MC_NONINT;
    else if (src->kind == O_STR) s = src->tok->boolv ? MC_NONE : src->tok->nat ? MC_NAT : MC_ALNUM;
    else if (sy && !sy->is_cond) s = move_cat_sym(sy, src->ref.rm);
    if (s == MC_NONE) return NULL;
    int ok = 1; const char *r85 = NULL;
    switch (s) {
    case MC_ALPHA: case MC_ALNUMED: ok = !rnum; r85 = "3a"; break;
    case MC_NAT: ok = r != MC_ALPHA && r != MC_ALNUM && r != MC_ALNUMED; break;
    case MC_NATED: ok = r == MC_NAT || r == MC_NATED; break;
    case MC_INT: case MC_NUMED: ok = r != MC_ALPHA; r85 = "3b"; break;
    case MC_NONINT:
        if (r == MC_ALPHA) { ok = 0; r85 = "3b"; break; }
        if (r == MC_ALNUM || r == MC_ALNUMED) {
            /* the 85 text forbids it (3c) but NIST NC105A, NC114M and NC124A
             * move a noninteger item to an alphanumeric one, and the cases
             * win under -std=85 (the ruling of 2026-08-31; a literal was
             * always refused) */
            ok = g_std < 2002 && src->kind == O_REF; r85 = "3c"; break;
        }
        ok = r != MC_NAT && r != MC_NATED;
        break;
    }
    if (ok) return NULL;
    if (s == MC_NAT)
        MV_BAD("a national item cannot be moved to the %s item '%s' (2023 14.9.25.3 rule 10, Table 16): use FUNCTION DISPLAY-OF", mc_name[r], d->name);   /* r is alphanumeric or alphabetic here */
    char why[48];
    if (g_std < 2002 && r85) snprintf(why, sizeof why, "85 VI-104 general rule %s", r85);
    else snprintf(why, sizeof why, "2023 14.9.25.3 rule 10, Table 16");
    MV_BAD("MOVE: %s %s %s cannot be moved to the %s item '%s' (%s)", s == MC_ALPHA || s == MC_ALNUM || s == MC_ALNUMED ? "an" : "a",
           mc_name[s], src->kind == O_REF ? "item" : "literal", r == MC_INT || r == MC_NONINT ? "numeric" : mc_name[r], d->name, why);
}
#undef MV_BAD

static void move_valid(const Opnd *src, const Ref *dst)
{
    char msg[MV_MSG];
    const char *why = move_invalid(src, dst, msg);
    if (why) die_at(src->kind == O_FIG || src->kind == O_ALL ? src->line : dst->line, "%s", why);
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
            char msg[MV_MSG];
            if (move_invalid(&o, &r2, msg)) continue;       /* not a corresponding pair (14.7.6 rule 2) */
            emit_move(&o, &r2); n++;
        } else {
            if (c1->is_group || c2->is_group || c1->pi.category != PIC_NUMERIC || c2->pi.category != PIC_NUMERIC) continue;
            Opnd o = ref_opnd(&r1);
            emit_push(&o);
            int rd = rounded;
            emit_store_receivers(&r2, &rd, 1, 0, 0, mode == 2, size_err, -1, 0);
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
    int size_err = at_size_error_clause() || ec_size_on();
    if (size_err) emit("\tstw sp+%d, r0", SLOT_A);
    corr_walk(&a, &b, mode, rounded, size_err);
    if (size_err) { emit("\tldw r1, sp+%d", SLOT_A); emit("\tstw sp+%d, r1", SLOT_B); }
    parse_size_error_clauses(size_err, end_word);
}

/* storage in common: the two items' records, or one redefining the other's */
static int rec_base(const Sym *s)
{
    int r = s->record;
    while (r >= 0 && g_sym[r].redefines >= 0) r = g_sym[r].redefines;
    return r;
}

/* 0: the sender is safe to identify again for each receiver; 1: copy it
 * to a compiler-made record first; 2: an OCCURS DEPENDING ON group,
 * whose DEPENDING ON item is copied instead (its length is a run-time
 * one, and the bytes stay where they are) */
static int move_needs_temp(const Opnd *src, const Ref *dst, int n)
{
    if (n < 2 || src->kind != O_REF) return 0;
    const Ref *r = &src->ref;
    for (int i = 0; i < n - 1; i++) {
        int rb = rec_base(dst[i].sym);
        if (r->rm_odo) { if (r->odo_dep && rec_base(r->odo_dep) == rb) return 2; continue; }
        for (int k = 0; k < r->nsub; k++)
            if (r->sub[k].sym && rec_base(r->sub[k].sym) == rb) return 1;
    }
    if (r->rm_odo) return 0;
    /* a reference modifier's start that is an expression: any receiver
     * may be in it */
    if (r->rm && r->rm_len && !r->rm_bit && !r->rm_start) return 1;
    /* a length that is one would need a snapshot of run-time length: not
     * done, so refused when a receiver ahead of the last shares storage
     * with an item the expressions name (docs/conformance/move.md) */
    if (r->rm && !r->rm_len && r->rm_l0 >= 0)
        for (int t = r->rm_s0; t < r->rm_l1; t++) {
            if (g_tok[t].kind != T_WORD || (t >= r->rm_s1 && t < r->rm_l0)) continue;
            for (int k = g_sym_base; k < g_nsym; k++) {
                if (strcmp(g_sym[k].name, g_tok[t].s)) continue;
                for (int i = 0; i < n - 1; i++)
                    if (rec_base(&g_sym[k]) == rec_base(dst[i].sym))
                        die_at(src->line, "MOVE: the sender's reference modification uses '%s', which a receiver before the last changes; "
                               "identifying the sender once (general rule 1) with a computed length is not implemented", g_tok[t].s);
            }
        }
    return 0;
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
    int n = 0, cap = 0;
    Ref *dst = NULL;
    while (at_operand()) {
        if (n == cap) { cap = cap ? 2 * cap : 8; dst = xrealloc(dst, (size_t)cap * sizeof *dst); }
        parse_ref(&dst[n]);
        move_valid(&src, &dst[n]);
        n++;
    }
    if (!n) die_at(cur()->line, "MOVE needs a receiving item");
    /* the sender is identified once, before the first move (general rule
     * 1: MOVE a (b) TO b, c (b) moves a (b) to a temporary first).  When
     * a receiver ahead of the last shares storage with a subscript, the
     * DEPENDING ON item or a reference modifier's operands, the sender
     * is copied to a compiler-made record first */
    if (src.kind == O_FUNC && n > 1) {
        /* a function-identifier likewise: evaluated once, its result
         * kept for every receiver (RANDOM, CURRENT-DATE, or an argument
         * that a receiver changes) */
        int l = new_label(), sz = src.fsize > 0 ? src.fsize : 1;
        emit("\t.data"); emit("\t.p2align 3"); emit(".L%d:", l); emit("\t.space %d", sz); emit("\t.text");
        emit_fn_value(&src);
        emit("\tadd r4, r1, r0");
        char lb[24]; snprintf(lb, sizeof lb, ".L%d", l); emit_la("r3", lb);
        emit_li("r5", sz);
        emit_call("memcpy");
        src.fsaved = l + 1;
    }
    int snap = move_needs_temp(&src, dst, n);
    if (snap == 2) {
        FDesc fd; fdesc_of(&fd, src.ref.odo_dep);
        Sym *t = ftemp_new(&fd, src.line);
        Ref tr = ftemp_ref(t, src.line), dr; memset(&dr, 0, sizeof dr);
        dr.sym = src.ref.odo_dep; dr.line = src.line; dr.rm_l0 = -1;
        Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = dr; o.line = src.line;
        emit_move(&o, &tr);
        src.ref.odo_dep = t;
    } else if (snap == 1) {
        FDesc fd; fdesc_of(&fd, src.ref.sym);
        if (src.ref.rm) { fd.group = 1; fd.size = (int)(src.ref.rm_nat ? 2 * src.ref.rm_len : src.ref.rm_len); }
        Sym *t = ftemp_new(&fd, src.line);
        Ref tr = ftemp_ref(t, src.line);
        emit_move(&src, &tr);
        Opnd o; memset(&o, 0, sizeof o); o.kind = O_REF; o.ref = tr; o.line = src.line;
        src = o;
    }
    for (int i = 0; i < n; i++) emit_move(&src, &dst[i]);
    free(dst);
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
        else if (ec_size_on()) emit_ec_size();       /* no ON SIZE ERROR: EC-SIZE, if checking is on (2023 14.7.5) */
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
    if (o->kind == O_REF && o->ref.rm) die_at(o->line, "a reference-modified item is not numeric");
    if (o->kind == O_REF && !is_numeric_sym(o->ref.sym)) die_at(o->line, "'%s' is not numeric", o->ref.sym->name);
}

/* push an operand onto the numeric stack */
static void emit_push(Opnd *o)
{
    if (o->kind == O_EXPR) die_at(o->line, "internal: expression pushed as an operand");
    if (o->kind == O_FUNC) {
        emit_fn_value(o);
        emit("\tadd r3, r1, r0");
        emit_desc_addr("r4", o->fn == -1 ? (o->fscale >= 0 ? numfn_desc(o->fscale) : str_desc(o->fsize))
                            : fn_is_numeric(o->fn) ? fn_num_desc(o) : str_desc(o->fsize));
        emit_call("cob_push");
        return;
    }
    if (o->kind == O_NUM || o->kind == O_FIG) {
        long long v = o->kind == O_NUM ? numlit_scaled(&o->num) : 0;
        int scale = o->kind == O_NUM ? o->num.scale : 0;
        emit_li("r3", (long)(int)(v & 0xFFFFFFFF));
        emit_li("r4", (long)(int)(v >> 32));
        emit_li("r5", scale);
        emit_call("cob_push_lit");
        return;
    }
    if (opnd_display_int(o)) {
        /* GitHub #29 shape (3): an unsigned DISPLAY integer reaches the
         * numeric stack through the same inline decode the compare path
         * uses, instead of cob_push -> cob_get_num's digit loop.  The value
         * is below 10^9 so the high word is zero and the scale is zero;
         * everything above this -- the 64-bit arithmetic, the receiver's
         * truncation, ROUNDED, ON SIZE ERROR -- is untouched, which is what
         * keeps this a decode change rather than an arithmetic one. */
        emit_display_value(&o->ref);
        emit("\tadd r3, r1, r0");
        emit_li("r4", 0);
        emit_li("r5", 0);
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

/* May this receiver take the hot path's word?  A four-byte unsigned NOTRUNC
 * item uses its top bit for value, so the sign fixup in emit_store_receivers
 * cannot tell 4000000000 from -294967296 -- it negated the former, and
 * "ADD 2000000000 TO" a PIC 9(9) COMP-5 holding 2000000000 stored 294967296
 * (GitHub #28).  It stays hot only where the result cannot be negative, an
 * ADD of non-negative operands; a SUBTRACT, or an operand that may be
 * negative, takes the generic path, where cob_put_num_x has all 64 bits and
 * the 85 rule (an unsigned receiver takes the magnitude) is decidable.  The
 * narrower COMP-5 items keep their sign in a word and need none of this. */
static int ref_hot_store(Ref *r, int subtract, int nonneg)
{
    Sym *s = r->sym;
    if (!is_hot_int(s) && !is_display_int(s)) return 0;
    if (!s->pi.is_signed && s->size == 4 && sym_notrunc(s) && (subtract || !nonneg)) return 0;
    return 1;
}

static int refs_hot(Ref *rs, int n, int subtract, int nonneg)
{
    for (int i = 0; i < n; i++) if (!ref_hot_store(&rs[i], subtract, nonneg)) return 0;
    return 1;
}

/* 32-bit hot-path sum stays correct only if every partial sum fits a
 * signed word. S9(9) COMP with three max operands does not (GitHub #18). */
static long long hot_opnd_mag(Opnd *o)
{
    if (o->kind == O_NUM) {
        long long v = numlit_int(&o->num);
        return v < 0 ? -v : v;
    }
    if (o->kind == O_FIG) return 0;
    if (o->kind == O_REF) {
        int d = o->ref.sym->pi.digits;
        if (d > 0 && d < 10) return pow10l(d) - 1;
        if (o->ref.sym->size == 1) return o->ref.sym->pi.is_signed ? 127 : 255;
        if (o->ref.sym->size == 2) return o->ref.sym->pi.is_signed ? 32767 : 65535;
        return 2147483647;
    }
    return 2147483647;
}

/* The staged sum's magnitude bound, or -1 when there is not a sound one.
 *
 * hot_opnd_mag bounds an item by its PICTURE, which a COMP-5 or C-ABI item
 * does not obey -- it keeps the binary field's whole capacity.  hot_sum_fits
 * has always taken that bound at face value; this does not, because the
 * truncation it feeds would then wrap by a compare and a subtract where the
 * value needs a REM.  One NOTRUNC operand and the bound is unknown. */
static long long ops_sum_mag(Opnd *ops, int n)
{
    long long bound = 0;
    for (int i = 0; i < n; i++) {
        if (ops[i].kind == O_REF && sym_notrunc(ops[i].ref.sym)) return -1;
        bound += hot_opnd_mag(&ops[i]);
        if (bound > 2147483647LL) return -1;
    }
    return bound;
}

static int ops_all_nonneg(Opnd *ops, int n)
{
    for (int i = 0; i < n; i++) if (!opnd_nonneg(&ops[i])) return 0;
    return 1;
}

static int hot_sum_fits(Opnd *ops, int n)
{
    long long bound = 0;
    int i;
    for (i = 0; i < n; i++) {
        bound += hot_opnd_mag(&ops[i]);
        if (bound > 2147483647LL) return 0;
    }
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

#define MAXOPS 64                  /* NC106A/NC176A add and subtract 21 operands */

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
        if (d->is_group || (d->pi.category != PIC_NUMERIC && !(edited_ok && d->pi.category == PIC_NUMERIC_EDITED) &&
                            !(edited_ok == 2 && sym_is_boolean(d))))    /* 2: COMPUTE, whose format 2 stores a boolean */
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
/* sum_mag bounds |the staged sum| (-1: unknown) and sum_nonneg says it cannot
 * be negative; together with the receiver's own picture they bound the value
 * being stored, which is what lets the truncation and the sign fixup go. */
static void emit_store_receivers(Ref *rs, int *rounded, int nr, int hot, int giving, int subtract, int size_err,
                                 long long sum_mag, int sum_nonneg)
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
            /* An unsigned COMP receiver holds 0 .. 10^digits-1: every path
             * that stores one truncates, so adding a bounded non-negative
             * sum to it lands below twice the limit and cannot go negative. */
            long long bound = -1; int nonneg = 0;
            if (!subtract && !d->pi.is_signed && d->size == 4 && sym_notrunc(d)) {
                /* the whole word is value (ref_hot_store admitted it only
                 * with non-negative operands): no picture, no sign fixup */
                nonneg = sum_nonneg;
            } else if (sum_mag >= 0 && !subtract) {
                if (giving) { bound = sum_mag; nonneg = sum_nonneg; }
                else if (!d->pi.is_signed && (d->usage == U_BINARY || is_display_int(d)) &&
                         d->pi.digits > 0 && d->pi.digits < 19) {
                    bound = pow10l(d->pi.digits) - 1 + sum_mag; nonneg = sum_nonneg;
                }
            }
            emit_trunc_bounded(d, bound, nonneg);
            if (!d->pi.is_signed && !nonneg) {
                /* unsigned takes the magnitude, matching cob_put_num_x */
                int Lpos = new_label();
                emit("\tbge r1, r0, .L%d", Lpos);
                emit("\tsub r1, r0, r1");
                emit_label(Lpos);
            }
            emit_store_int(d, "r3", "r1");
        } else {
            emit_top_op(&rs[i], giving ? "cob_top_store" : subtract ? "cob_top_subfrom" : "cob_top_addto", opts);
        }
    }
    if (!hot) emit_call("cob_drop");
}

/* ---- the scaled ADD, in line ----------------------------------------------
 *
 * After #27, #29's three shapes and #30, the batch's largest remaining
 * runtime line item was cob_top_addto: 208,889 calls, every one of them a
 * COMP-3 receiver of eleven digits at scale 2 taking a same-scale operand,
 * DISPLAY 9(9)V99 or the same COMP-3 picture (ws-debits, ws-total-debits,
 * yt-debits(i)), at ~900 instructions each -- two cob_get_num, a 64-bit
 * alignment, cob_put_num_x with its digit loop.  ~12% of the batch.
 *
 * With the scales equal there is nothing to align: the two digit strings
 * add column-wise.  Each item is read into two limbs in base 10^9 -- hi for
 * the digits above the low nine, lo for the low nine -- and a sign; the
 * limbs add or subtract as sign-magnitude with one carry or borrow between
 * them; the result is brought inside the receiver's picture by one REM on
 * the limb the picture ends in; and it is written back as digits or
 * nibbles.  No call, no descriptor, no 64-bit arithmetic: everything fits a
 * word because a limb is below 10^9 and a sum of two is below 2^31.
 * Eighteen digits is the ceiling, two limbs.
 *
 * What it takes: ADD x TO r and SUBTRACT x FROM r, one operand, one or more
 * receivers, both DISPLAY (digits exactly the bytes, a trailing overpunch
 * sign or none) or COMP-3, the operand's scale the receiver's.  ROUNDED is
 * admitted because with one scale it has nothing to do.  SIZE ERROR is
 * not: that needs the overflow detected, and the generic path has it.
 * Literals and GIVING stay generic too -- the batch had 12k stores against
 * 209k adds, and a literal is a different decode.  The 85 rule for an
 * unsigned receiver, the magnitude, is kept; a zero result is positive.
 *
 * Registers: the values live in r5-r10 across the sequence, which holds no
 * call -- so a subscript that needs cob_load_int (r3-r10 clobbered) bars
 * the path.  r1/r2 scratch, r4 a constant, r3 the address, r11 untouched
 * (the subscript accumulator: see emit_display_decode).  GitHub #29. */

/* an item the inline decimal add can read and write */
static int sym_dec_ok(Sym *s)
{
    if (s->is_group || s->is_cond || s->pi.category != PIC_NUMERIC) return 0;
    if (s->sign_sep || s->sign_lead || s->blank_zero || s->pi.edited || strchr(s->pi.pat, 'P')) return 0;
    if (s->pi.digits < 1 || s->pi.digits > 18) return 0;
    if (s->usage == U_DISPLAY) return (int)s->size == s->pi.digits;
    if (s->usage == U_PACKED) return (int)s->size == (s->pi.digits + 2) / 2;
    return 0;
}

/* a reference whose address the sequence can form without a call */
static int ref_dec_addr_ok(const Ref *r)
{
    if (r->rm) return 0;
    for (int i = 0; i < r->nsub; i++) if (r->sub[i].sym && !is_hot_int(r->sub[i].sym)) return 0;
    return 1;
}

/* acc = acc * 10 with r2 scratch and no constant register */
static void emit_mul10(const char *acc)
{
    emit("\tslli r2, %s, 3", acc);
    emit("\tslli %s, %s, 1", acc, acc);
    emit("\tadd %s, %s, r2", acc, acc);
}

/* item s at areg -> limbs hi (digits above the low nine) and lo (the low
 * nine), sg = 1 if negative.  Digit d of D (0 the most significant) goes
 * to hi while d < D - 9. */
static void emit_dec_load(Sym *s, const char *areg, const char *hi, const char *lo, const char *sg)
{
    int D = s->pi.digits, split = D > 9 ? D - 9 : 0;
    emit("\tadd %s, r0, r0", hi);
    emit("\tadd %s, r0, r0", lo);
    if (s->usage == U_DISPLAY) {
        for (int d = 0; d < D; d++) {
            const char *acc = d < split ? hi : lo;
            if (d != 0 && d != split) emit_mul10(acc);
            emit("\tldbu r2, %s+%d", areg, d);
            emit("\tandi r2, r2, 15");            /* '0'..'9' and the overpunch 'p'..'y' alike */
            emit("\tadd %s, %s, r2", acc, acc);
        }
        if (s->pi.is_signed) {
            emit("\tldbu r2, %s+%d", areg, D - 1);
            emit("\tsltiu %s, r2, 112", sg);      /* below 'p': positive */
            emit("\txori %s, %s, 1", sg, sg);
        } else emit("\tadd %s, r0, r0", sg);
    } else {
        /* 2*size nibbles: a zero pad first when D is even, the D digits,
         * the sign last */
        int k0 = 2 * (int)s->size - 1 - D, curbyte = -1;
        for (int d = 0; d < D; d++) {
            int k = k0 + d, b = k / 2;
            const char *acc = d < split ? hi : lo;
            if (b != curbyte) { emit("\tldbu r1, %s+%d", areg, b); curbyte = b; }
            if (d != 0 && d != split) emit_mul10(acc);
            if (k % 2 == 0) emit("\tsrli r2, r1, 4"); else emit("\tandi r2, r1, 15");
            emit("\tadd %s, %s, r2", acc, acc);
        }
        if (s->pi.is_signed) {
            if (curbyte != (int)s->size - 1) emit("\tldbu r1, %s+%d", areg, (int)s->size - 1);
            emit("\tandi r2, r1, 15");
            emit("\txori r2, r2, 13");            /* 0xD: negative; C, F or anything else: not */
            emit("\tseq %s, r2, r0", sg);
        } else emit("\tadd %s, r0, r0", sg);
    }
}

/* (hi,lo,sg) += (oh,ol,os), sign-magnitude in limbs of base 10^9 */
static void emit_dec_add(const char *hi, const char *lo, const char *sg, const char *oh, const char *ol, const char *os)
{
    int Lsame = new_label(), Lless = new_label(), Lsub = new_label(), Ldone = new_label(), Lnz = new_label();
    emit_li("r4", 1000000000);
    emit("\tbeq %s, %s, .L%d", sg, os, Lsame);
    emit("\tbltu %s, %s, .L%d", hi, oh, Lless);
    emit("\tbne %s, %s, .L%d", hi, oh, Lsub);
    emit("\tbltu %s, %s, .L%d", lo, ol, Lless);
    emit_label(Lsub);                              /* |x| >= |y|: x - y, x's sign */
    emit("\tsltu r2, %s, %s", lo, ol);
    emit("\tsub %s, %s, %s", lo, lo, ol);
    emit("\tsub %s, %s, %s", hi, hi, oh);
    emit("\tsub %s, %s, r2", hi, hi);
    emit("\tbeq r2, r0, .L%d", Ldone);
    emit("\tadd %s, %s, r4", lo, lo);
    emit("\tjal r0, .L%d", Ldone);
    emit_label(Lless);                             /* |y| > |x|: y - x, y's sign */
    emit("\tsltu r2, %s, %s", ol, lo);
    emit("\tsub %s, %s, %s", lo, ol, lo);
    emit("\tsub %s, %s, %s", hi, oh, hi);
    emit("\tsub %s, %s, r2", hi, hi);
    emit("\tadd %s, %s, r0", sg, os);
    emit("\tbeq r2, r0, .L%d", Ldone);
    emit("\tadd %s, %s, r4", lo, lo);
    emit("\tjal r0, .L%d", Ldone);
    emit_label(Lsame);                             /* one sign: x + y */
    emit("\tadd %s, %s, %s", lo, lo, ol);
    emit("\tadd %s, %s, %s", hi, hi, oh);
    emit("\tbltu %s, r4, .L%d", lo, Ldone);
    emit("\tsub %s, %s, r4", lo, lo);
    emit("\taddi %s, %s, 1", hi, hi);
    emit_label(Ldone);
    emit("\tadd r2, %s, %s", hi, lo);              /* zero is positive */
    emit("\tbne r2, r0, .L%d", Lnz);
    emit("\tadd %s, r0, r0", sg);
    emit_label(Lnz);
}

/* bring (hi,lo) inside s's picture: the high-order digits past it go */
static void emit_dec_trunc(Sym *s, const char *hi, const char *lo)
{
    int D = s->pi.digits;
    if (D > 9) { emit_li("r2", pow10l(D - 9)); emit("\trem %s, %s, r2", hi, hi); }
    else { emit_li("r2", pow10l(D)); emit("\trem %s, %s, r2", lo, lo); emit("\tadd %s, r0, r0", hi); }
}

/* (hi,lo,sg), already inside the picture -> item s at areg */
static void emit_dec_store(Sym *s, const char *areg, const char *hi, const char *lo, const char *sg)
{
    int D = s->pi.digits, split = D > 9 ? D - 9 : 0, d = D - 1;
    emit_li("r4", 10);
    /* the next digit, least significant first, into reg; the limb is
     * divided down unless this was its last digit */
#define DEC_DIGIT(reg) do { \
        const char *src_ = d < split ? hi : lo; \
        emit("\trem %s, %s, r4", reg, src_); \
        if (d != split && d != 0) emit("\tdiv %s, %s, r4", src_, src_); \
        d--; \
    } while (0)
    if (s->usage == U_DISPLAY) {
        while (d >= 0) {
            int at = d;
            DEC_DIGIT("r2");
            emit("\taddi r2, r2, 48");
            emit("\tstb %s+%d, r2", areg, at);
        }
        if (s->pi.is_signed) {
            int L = new_label();
            emit("\tbeq %s, r0, .L%d", sg, L);
            emit("\tldbu r2, %s+%d", areg, D - 1);
            emit("\taddi r2, r2, 64");             /* '0'..'9' -> 'p'..'y' */
            emit("\tstb %s+%d, r2", areg, D - 1);
            emit_label(L);
        }
    } else {
        for (int b = (int)s->size - 1; b >= 0; b--) {
            if (b == (int)s->size - 1) {
                if (s->pi.is_signed) emit("\taddi r2, %s, 12", sg);   /* C, or D when negative */
                else emit("\taddi r2, r0, 15");
            } else DEC_DIGIT("r2");
            if (d >= 0) { DEC_DIGIT("r1"); emit("\tslli r1, r1, 4"); emit("\tadd r2, r2, r1"); }
            emit("\tstb %s+%d, r2", areg, b);
        }
    }
#undef DEC_DIGIT
}

static int dec_add_ok(Opnd *ops, int n, Ref *rs, int nr, int size_err)
{
    if (size_err || n != 1 || ops[0].kind != O_REF || ops[0].all_sub) return 0;
    if (!sym_dec_ok(ops[0].ref.sym) || !ref_dec_addr_ok(&ops[0].ref)) return 0;
    for (int i = 0; i < nr; i++) {
        if (!sym_dec_ok(rs[i].sym) || !ref_dec_addr_ok(&rs[i])) return 0;
        if (rs[i].sym->pi.scale != ops[0].ref.sym->pi.scale) return 0;
    }
    return 1;
}

static void emit_dec_addto(Opnd *op, Ref *rs, int nr, int subtract)
{
    for (int i = 0; i < nr; i++) {
        Sym *d = rs[i].sym;
        emit_ref_addr(&rs[i], "r3");
        emit("\tstw sp+%d, r3", SLOT_A);
        emit_ref_addr(&op->ref, "r3");
        emit_dec_load(op->ref.sym, "r3", "r8", "r9", "r10");
        if (subtract) emit("\txori r10, r10, 1");
        emit("\tldw r3, sp+%d", SLOT_A);
        emit_dec_load(d, "r3", "r5", "r6", "r7");
        emit_dec_add("r5", "r6", "r7", "r8", "r9", "r10");
        emit_dec_trunc(d, "r5", "r6");
        if (!d->pi.is_signed) emit("\tadd r7, r0, r0");   /* an unsigned receiver takes the magnitude */
        emit_dec_store(d, "r3", "r5", "r6", "r7");
    }
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
            /* again, for real: a user function among them is called here */
            g_tp = save; ne = parse_operand_list(extra, MAXOPS); expect_word("giving");
            for (int i = 0; i < ne; i++) { if (n >= MAXOPS) die_at(cur()->line, "too many operands"); ops[n++] = extra[i]; }
            giving = 1;
            nr = parse_ref_list(rs, rd, MAXOPS, 1);
        } else { g_tp = save; nr = parse_ref_list(rs, rd, MAXOPS, 0); }
    } else if (accept_word("giving")) {
        giving = 1; nr = parse_ref_list(rs, rd, MAXOPS, 1);
    } else die_at(cur()->line, "expected TO or GIVING in ADD");
    if (!nr) die_at(cur()->line, "ADD needs a receiving item");
    int size_err = at_size_error_clause() || ec_size_on();

    int hot = !size_err && !any_rounded(rd, nr) && all_hot(ops, n) &&
              refs_hot(rs, nr, 0, ops_all_nonneg(ops, n)) && hot_sum_fits(ops, n);
    if (hot) emit_hot_sum(ops, n);
    else if (!giving && dec_add_ok(ops, n, rs, nr, size_err)) {
        emit_dec_addto(&ops[0], rs, nr, 0);
        parse_size_error_clauses(size_err, "end-add");
        return;
    }
    else { for (int i = 0; i < n; i++) { emit_push(&ops[i]); if (i) emit_call("cob_nadd"); } }
    emit_store_receivers(rs, rd, nr, hot, giving, 0, size_err, ops_sum_mag(ops, n), ops_all_nonneg(ops, n));
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
        /* again, for real: a user function in the minuend is called here */
        g_tp = save; ne = parse_operand_list(extra, MAXOPS); expect_word("giving");
        if (ne != 1) die_at(cur()->line, "SUBTRACT ... FROM x GIVING takes one item after FROM");
        minuend = extra[0]; giving = 1;
        nr = parse_ref_list(rs, rd, MAXOPS, 1);
    } else { g_tp = save; nr = parse_ref_list(rs, rd, MAXOPS, 0); }
    if (!nr) die_at(cur()->line, "SUBTRACT needs a receiving item");
    int size_err = at_size_error_clause() || ec_size_on();

    int hot = !size_err && !any_rounded(rd, nr) && all_hot(ops, n) &&
              refs_hot(rs, nr, 1, 0) && (!giving || opnd_hot_int(&minuend)) &&
              hot_sum_fits(ops, n);
    if (!hot && !giving && dec_add_ok(ops, n, rs, nr, size_err)) {
        emit_dec_addto(&ops[0], rs, nr, 1);
        parse_size_error_clauses(size_err, "end-subtract");
        return;
    }
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
    emit_store_receivers(rs, rd, nr, hot, giving, !giving, size_err, -1, 0);
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
        g_tp = save; parse_operand(&b); expect_word("giving");   /* again, for real (a user function) */
        nr = parse_ref_list(rs, rd, MAXOPS, 1);
        if (!nr) die_at(cur()->line, "MULTIPLY needs a receiving item");
        int size_err = at_size_error_clause() || ec_size_on();
        emit_push(&a); emit_push(&b); emit_call("cob_nmul");
        emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err, -1, 0);
        parse_size_error_clauses(size_err, "end-multiply");
        return;
    }
    g_tp = save;
    nr = parse_ref_list(rs, rd, MAXOPS, 0);
    if (!nr) die_at(cur()->line, "MULTIPLY needs a receiving item");
    int size_err = at_size_error_clause() || ec_size_on();
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
            g_tp = save; parse_operand(&b); expect_word("giving");   /* again, for real (a user function) */
            nr = parse_ref_list(rs, rd, MAXOPS, 1);
            if (!nr) die_at(cur()->line, "DIVIDE needs a receiving item");
            int size_err = size_error_after_remainder() || ec_size_on();
            emit_push(&b); emit_push(&a); emit_call("cob_ndiv");
            emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err, -1, 0);
            emit_remainder(&b, &rs[0], rd[0], &a, size_err);
            parse_size_error_clauses(size_err, "end-divide");
            return;
        }
        g_tp = save;
        nr = parse_ref_list(rs, rd, MAXOPS, 0);
        if (!nr) die_at(cur()->line, "DIVIDE needs a receiving item");
        int size_err = at_size_error_clause() || ec_size_on();
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
    int size_err = size_error_after_remainder() || ec_size_on();
    emit_push(&a); emit_push(&b); emit_call("cob_ndiv");
    emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err, -1, 0);
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
    int nr = parse_ref_list(rs, rd, MAXOPS, 2);
    if (!nr) die_at(cur()->line, "COMPUTE needs a receiving item");
    if (!at_op("=")) die_at(cur()->line, "expected '=' in COMPUTE, found %s", tok_desc(cur()));
    advance();
    int nb = 0;
    for (int i = 0; i < nr; i++) nb += sym_is_boolean(rs[i].sym);
    if (nb) {
        /* a boolean-compute (2023 14.9.8, format 2): the expression's
         * value stored in each receiver by the MOVE rules */
        if (nb != nr) die_at(rs[0].line, "COMPUTE: boolean and numeric receivers cannot be mixed (2023 14.9.8.3)");
        for (int i = 0; i < nr; i++) if (rd[i]) die_at(rs[i].line, "ROUNDED does not apply to a boolean receiver");
        parse_bexpr();
        if (g_bexpr_all) die_at(rs[0].line, "a boolean COMPUTE's expression cannot be an ALL literal alone (2023 14.9.8.3 rule 3)");
        for (int i = 0; i < nr; i++) {
            Arg a[2] = { arg_ref(&rs[i]), rs[i].rm ? (rs[i].rm_len ? arg_desc(bool_desc((int)rs[i].rm_len)) : arg_rdesc(&rs[i])) : arg_desc(sym_desc(rs[i].sym)) };
            emit_args(a, 2);
            emit_call("cob_bstore");
        }
        emit_call("cob_bdrop");
        accept_word("end-compute");
        return;
    }
    parse_expr();
    int size_err = at_size_error_clause() || ec_size_on();
    emit_store_receivers(rs, rd, nr, 0, 1, 0, size_err, -1, 0);
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

/* ---- exception conditions (COBOL 2002 14.6.13; cobol ISSUES-53) ------- */

/* ISO/IEC 1989:2023 Table 13: every exception-name, its level (1 EC-ALL,
 * 2 a group, 3 a condition) and its fatality, 'F' fatal, 'N' nonfatal,
 * 'I' implementor-defined (taken here as nonfatal).  EC-USER-suffix names
 * are the user's, level 3 and nonfatal, and are added as they are met. */
static const struct { const char *name; char level; char fatal; } g_ec[] = {
    { "EC-ALL", 1, 0 },
    { "EC-ARGUMENT", 2, 0 },
    { "EC-ARGUMENT-FUNCTION", 3, 'F' },
    { "EC-ARGUMENT-IMP", 3, 'I' },
    { "EC-BOUND", 2, 0 },
    { "EC-BOUND-FUNC-RET-VALUE", 3, 'N' },
    { "EC-BOUND-IMP", 3, 'I' },
    { "EC-BOUND-ODO", 3, 'F' },
    { "EC-BOUND-OVERFLOW", 3, 'N' },
    { "EC-BOUND-PTR", 3, 'F' },
    { "EC-BOUND-REF-MOD", 3, 'F' },
    { "EC-BOUND-SET", 3, 'N' },
    { "EC-BOUND-SUBSCRIPT", 3, 'F' },
    { "EC-BOUND-TABLE-LIMIT", 3, 'F' },
    { "EC-CONTINUE", 2, 0 },
    { "EC-CONTINUE-IMP", 3, 'I' },
    { "EC-CONTINUE-LESS-THAN-ZERO", 3, 'N' },
    { "EC-DATA", 2, 0 },
    { "EC-DATA-CONVERSION", 3, 'N' },
    { "EC-DATA-IMP", 3, 'I' },
    { "EC-DATA-INCOMPATIBLE", 3, 'F' },
    { "EC-DATA-NOT-FINITE", 3, 'F' },
    { "EC-DATA-OVERFLOW", 3, 'F' },
    { "EC-DATA-PTR-NULL", 3, 'F' },
    { "EC-EXTERNAL", 2, 0 },
    { "EC-EXTERNAL-DATA-MISMATCH", 3, 'F' },
    { "EC-EXTERNAL-FILE-MISMATCH", 3, 'F' },
    { "EC-EXTERNAL-FORMAT-CONFLICT", 3, 'F' },
    { "EC-EXTERNAL-IMP", 3, 'I' },
    { "EC-FLOW", 2, 0 },
    { "EC-FLOW-APPLY-COMMIT", 3, 'F' },
    { "EC-FLOW-COMMIT", 3, 'F' },
    { "EC-FLOW-GLOBAL-EXIT", 3, 'F' },
    { "EC-FLOW-GLOBAL-GOBACK", 3, 'F' },
    { "EC-FLOW-IMP", 3, 'I' },
    { "EC-FLOW-RELEASE", 3, 'F' },
    { "EC-FLOW-REPORT", 3, 'F' },
    { "EC-FLOW-RETURN", 3, 'F' },
    { "EC-FLOW-ROLLBACK", 3, 'F' },
    { "EC-FLOW-SEARCH", 3, 'F' },
    { "EC-FLOW-USE", 3, 'F' },
    { "EC-FUNCTION", 2, 0 },
    { "EC-FUNCTION-ARG-OMITTED", 3, 'F' },
    { "EC-FUNCTION-IMP", 3, 'I' },
    { "EC-FUNCTION-NOT-FOUND", 3, 'F' },
    { "EC-FUNCTION-PTR-INVALID", 3, 'F' },
    { "EC-FUNCTION-PTR-NULL", 3, 'F' },
    { "EC-I-O", 2, 0 },
    { "EC-I-O-AT-END", 3, 'N' },
    { "EC-I-O-EOP", 3, 'N' },
    { "EC-I-O-EOP-OVERFLOW", 3, 'N' },
    { "EC-I-O-FILE-SHARING", 3, 'N' },
    { "EC-I-O-IMP", 3, 'I' },
    { "EC-I-O-INVALID-KEY", 3, 'N' },
    { "EC-I-O-LINAGE", 3, 'F' },
    { "EC-I-O-LOGIC-ERROR", 3, 'F' },
    { "EC-I-O-PERMANENT-ERROR", 3, 'F' },
    { "EC-I-O-RECORD-CONTENT", 3, 'F' },
    { "EC-I-O-RECORD-OPERATION", 3, 'N' },
    { "EC-I-O-WARNING", 3, 'N' },
    { "EC-IMP", 2, 0 },
    /* { "EC-IMP-suffix", 3, 'I' },  pattern entry (implementor/user supplies suffix), not a literal name */
    { "EC-LOCALE", 2, 0 },
    { "EC-LOCALE-IMP", 3, 'I' },
    { "EC-LOCALE-INCOMPATIBLE", 3, 'F' },
    { "EC-LOCALE-INVALID", 3, 'F' },
    { "EC-LOCALE-INVALID-PTR", 3, 'F' },
    { "EC-LOCALE-MISSING", 3, 'F' },
    { "EC-LOCALE-SIZE", 3, 'F' },
    { "EC-MCS", 2, 0 },
    { "EC-MCS-ABNORMAL-TERMINATION", 3, 'N' },
    { "EC-MCS-IMP", 3, 'I' },
    { "EC-MCS-INVALID-TAG", 3, 'N' },
    { "EC-MCS-MESSAGE-LENGTH", 3, 'N' },
    { "EC-MCS-NO-REQUESTER", 3, 'N' },
    { "EC-MCS-NO-SERVER", 3, 'N' },
    { "EC-MCS-NORMAL-TERMINATION", 3, 'N' },
    { "EC-MCS-REQUESTOR-FAILED", 3, 'N' },
    { "EC-OO", 2, 0 },
    { "EC-OO-ARG-OMITTED", 3, 'F' },
    { "EC-OO-CONFORMANCE", 3, 'F' },
    { "EC-OO-EXCEPTION", 3, 'F' },
    { "EC-OO-IMP", 3, 'I' },
    { "EC-OO-METHOD", 3, 'F' },
    { "EC-OO-NULL", 3, 'F' },
    { "EC-OO-RESOURCE", 3, 'F' },
    { "EC-OO-UNIVERSAL", 3, 'F' },
    { "EC-ORDER", 2, 0 },
    { "EC-ORDER-IMP", 3, 'I' },
    { "EC-ORDER-NOT-SUPPORTED", 3, 'F' },
    { "EC-OVERFLOW", 2, 0 },
    { "EC-OVERFLOW-IMP", 3, 'I' },
    { "EC-OVERFLOW-STRING", 3, 'N' },
    { "EC-OVERFLOW-UNSTRING", 3, 'N' },
    { "EC-PROGRAM", 2, 0 },
    { "EC-PROGRAM-ARG-MISMATCH", 3, 'F' },
    { "EC-PROGRAM-ARG-OMITTED", 3, 'F' },
    { "EC-PROGRAM-CANCEL-ACTIVE", 3, 'F' },
    { "EC-PROGRAM-IMP", 3, 'I' },
    { "EC-PROGRAM-NOT-FOUND", 3, 'F' },
    { "EC-PROGRAM-PTR-NULL", 3, 'F' },
    { "EC-PROGRAM-RECURSIVE-CALL", 3, 'F' },
    { "EC-PROGRAM-RESOURCES", 3, 'F' },
    { "EC-RAISING", 2, 0 },
    { "EC-RAISING-IMP", 3, 'I' },
    { "EC-RAISING-NOT-SPECIFIED", 3, 'F' },
    { "EC-RANGE", 2, 0 },
    { "EC-RANGE-IMP", 3, 'I' },
    { "EC-RANGE-INDEX", 3, 'F' },
    { "EC-RANGE-INSPECT-SIZE", 3, 'F' },
    { "EC-RANGE-INVALID", 3, 'N' },
    { "EC-RANGE-PERFORM-VARYING", 3, 'F' },
    { "EC-RANGE-PTR", 3, 'F' },
    { "EC-RANGE-SEARCH-INDEX", 3, 'N' },
    { "EC-RANGE-SEARCH-NO-MATCH", 3, 'N' },
    { "EC-REPORT", 2, 0 },
    { "EC-REPORT-ACTIVE", 3, 'F' },
    { "EC-REPORT-COLUMN-OVERLAP", 3, 'N' },
    { "EC-REPORT-FILE-MODE", 3, 'F' },
    { "EC-REPORT-IMP", 3, 'I' },
    { "EC-REPORT-INACTIVE", 3, 'F' },
    { "EC-REPORT-LINE-OVERLAP", 3, 'N' },
    { "EC-REPORT-NOT-TERMINATED", 3, 'N' },
    { "EC-REPORT-PAGE-LIMIT", 3, 'N' },
    { "EC-REPORT-PAGE-WIDTH", 3, 'N' },
    { "EC-REPORT-SUM-SIZE", 3, 'F' },
    { "EC-REPORT-VARYING", 3, 'F' },
    { "EC-SCREEN", 2, 0 },
    { "EC-SCREEN-FIELD-OVERLAP", 3, 'N' },
    { "EC-SCREEN-IMP", 3, 'I' },
    { "EC-SCREEN-ITEM-TRUNCATED", 3, 'N' },
    { "EC-SCREEN-LINE-NUMBER", 3, 'N' },
    { "EC-SCREEN-STARTING-COLUMN", 3, 'N' },
    { "EC-SIZE", 2, 0 },
    { "EC-SIZE-ADDRESS", 3, 'F' },
    { "EC-SIZE-EXPONENTIATION", 3, 'F' },
    { "EC-SIZE-IMP", 3, 'I' },
    { "EC-SIZE-OVERFLOW", 3, 'F' },
    { "EC-SIZE-TRUNCATION", 3, 'F' },
    { "EC-SIZE-UNDERFLOW", 3, 'F' },
    { "EC-SIZE-ZERO-DIVIDE", 3, 'F' },
    { "EC-SORT-MERGE", 2, 0 },
    { "EC-SORT-MERGE-ACTIVE", 3, 'F' },
    { "EC-SORT-MERGE-FILE-OPEN", 3, 'F' },
    { "EC-SORT-MERGE-IMP", 3, 'I' },
    { "EC-SORT-MERGE-RELEASE", 3, 'F' },
    { "EC-SORT-MERGE-RETURN", 3, 'F' },
    { "EC-SORT-MERGE-SEQUENCE", 3, 'F' },
    { "EC-STORAGE", 2, 0 },
    { "EC-STORAGE-IMP", 3, 'I' },
    { "EC-STORAGE-NOT-ALLOC", 3, 'N' },
    { "EC-STORAGE-NOT-AVAIL", 3, 'N' },
    { "EC-USER", 2, 0 },
    /* { "EC-USER-suffix", 3, 'N' },  pattern entry (implementor/user supplies suffix), not a literal name */
    { "EC-VALIDATE", 2, 0 },
    { "EC-VALIDATE-CONTENT", 3, 'N' },
    { "EC-VALIDATE-FORMAT", 3, 'N' },
    { "EC-VALIDATE-IMP", 3, 'I' },
    { "EC-VALIDATE-RELATION", 3, 'N' },
    { "EC-VALIDATE-VARYING", 3, 'F' },
    { NULL, 0, 0 }
};
#define NEC (int)(sizeof g_ec / sizeof g_ec[0] - 1)
static char g_ecu[64][64]; static int g_necu;             /* EC-USER-suffix names met so far */
/* TURN for one file (7.3.25 rules 4, 6, 8; cobol ISSUES-87): an override
 * of checking for one EC-I-O condition and one file, over the setting
 * for all files; a TURN without a file clears a condition's overrides */
typedef struct { int ec, file; unsigned char on, loc; } EcFile;
/* The exception checking in force at a point in the source (cobol
 * ISSUES-53, -87, -94): each level-3 condition's checking and WITH
 * LOCATION, the per-file overrides, and the setting EC-USER-names not
 * yet met will take.  One state, saved and restored whole. */
typedef struct {
    unsigned char on[NEC + 64], loc[NEC + 64];
    EcFile *f; int nf, fcap;
    int user_on, user_loc;
} EcState;
static EcState g_ecs;
static void ecs_copy(EcState *d, const EcState *s)
{
    EcFile *f = d->f; int cap = d->fcap;
    if (cap < s->nf) { cap = s->nf + 16; f = xrealloc(f, (size_t)cap * sizeof *f); }
    *d = *s; d->f = f; d->fcap = cap;
    if (s->nf) memcpy(d->f, s->f, (size_t)s->nf * sizeof *s->f);
}

static const char *ec_name(int i) { return i < NEC ? g_ec[i].name : g_ecu[i - NEC]; }
static int ec_level(int i) { return i < NEC ? g_ec[i].level : 3; }
static int ec_fatal(int i) { return i < NEC && g_ec[i].fatal == 'F'; }

/* the index of an exception-name, or -1; a new EC-USER-suffix is added */
static int ec_find(const char *w, int line)
{
    for (int i = 0; i < NEC; i++) if (!strcasecmp(w, g_ec[i].name)) return i;
    if (!strncasecmp(w, "ec-user-", 8) && w[8]) {
        size_t n = strlen(w);
        for (size_t k = 8; k < n; k++) if (!isalnum((unsigned char)w[k]) && w[k] != '-' && w[k] != '_') return -1;
        if (w[n - 1] == '-' || w[n - 1] == '_') return -1;
        for (int i = 0; i < g_necu; i++) if (!strcasecmp(w, g_ecu[i])) return NEC + i;
        if (g_necu == 64) die_at(line, "more than 64 EC-USER exception-names");
        snprintf(g_ecu[g_necu], sizeof g_ecu[0], "%s", w);
        for (char *c = g_ecu[g_necu]; *c; c++) *c = (char)toupper((unsigned char)*c);
        g_ecs.on[NEC + g_necu] = (unsigned char)g_ecs.user_on; g_ecs.loc[NEC + g_necu] = (unsigned char)g_ecs.user_loc;
        return NEC + g_necu++;
    }
    return -1;
}

/* a level-3 name's level-2 group */
static int ec_group(int i)
{
    if (i >= NEC) return ec_find("EC-USER", 0);
    int best = -1; size_t bl = 0;
    for (int k = 0; k < NEC; k++) {
        if (g_ec[k].level != 2) continue;
        size_t l = strlen(g_ec[k].name);
        if (l > bl && !strncmp(g_ec[i].name, g_ec[k].name, l) && g_ec[i].name[l] == '-') { best = k; bl = l; }
    }
    return best;
}

static int ec_on_io(const char *name, int file);
/* an exception-checking PERFORM (2023 14.9.28 format 3; cobol ISSUES-89)
 * whose imperative-statement-1 is being compiled: its WHEN phrases, the
 * labels of their handlers, and the data words a raise leaves for the
 * handler's return -- where to resume, and whether the condition was
 * fatal (general rule 20) */
typedef struct { int *ec, *file, n, cap, label; } EcpWhen;
typedef struct { EcpWhen *w; int nw, wcap, Lother, Lcommon, Lend, id, resume; } Ecp;
static Ecp **g_ecp; static int g_necp, g_ecp_cap, g_ecp_handler;

static void ecf_set(int c, int file, int on, int loc)
{
    EcState *st = &g_ecs;
    for (int k = 0; k < st->nf; k++) if (st->f[k].ec == c && st->f[k].file == file) { st->f[k].on = (unsigned char)on; st->f[k].loc = (unsigned char)loc; return; }
    if (st->nf == st->fcap) {
        st->fcap = st->fcap ? 2 * st->fcap : 16;
        st->f = xrealloc(st->f, (size_t)st->fcap * sizeof *st->f);
    }
    st->f[st->nf].ec = c; st->f[st->nf].file = file; st->f[st->nf].on = (unsigned char)on; st->f[st->nf].loc = (unsigned char)loc; st->nf++;
}
static void ecf_clear(int c)
{
    EcState *st = &g_ecs;
    int m = 0;
    for (int k = 0; k < st->nf; k++) if (st->f[k].ec != c) st->f[m++] = st->f[k];
    st->nf = m;
}
/* checking for condition c on file index file (-1: none), and WITH LOCATION */
static int ec_on_file(int c, int file, int *loc)
{
    for (int k = 0; file >= 0 && k < g_ecs.nf; k++)
        if (g_ecs.f[k].ec == c && g_ecs.f[k].file == file) { if (loc) *loc = g_ecs.f[k].loc; return g_ecs.f[k].on; }
    if (loc) *loc = g_ecs.loc[c];
    return g_ecs.on[c];
}
/* checking for condition c everywhere: on, and no file's override off */
static int ec_on_all(int c)
{
    if (!g_ecs.on[c]) return 0;
    for (int k = 0; k < g_ecs.nf; k++) if (g_ecs.f[k].ec == c && !g_ecs.f[k].on) return 0;
    return 1;
}
/* does exception-name i cover level-3 condition c: itself, its group's,
 * or EC-ALL's -- EC-I-O-WARNING only by its own name (14.6.13.1.2) */
static int ec_covers(int i, int c)
{
    if (ec_level(c) != 3) return 0;
    if (c == ec_find("EC-I-O-WARNING", 0) && c != i) return 0;
    int lv = ec_level(i);
    return c == i || lv == 1 || (lv == 2 && ec_group(c) == i);
}
/* does name i (at level 1, or EC-USER) also decide the EC-USER-names not yet met */
static int ec_covers_later_users(int i) { return ec_level(i) == 1 || (i < NEC && !strcmp(g_ec[i].name, "EC-USER")); }
/* one condition's checking set, for all files (their overrides cleared)
 * or for one */
static void ec_turn_c(int c, int file, int on, int loc)
{
    if (file >= 0) { ecf_set(c, file, on, on && loc); return; }
    g_ecs.on[c] = (unsigned char)on; g_ecs.loc[c] = (unsigned char)(on && loc);
    ecf_clear(c);
}

/* >>TURN name [file-name] ... CHECKING {ON [WITH LOCATION] | OFF} (2023 7.3.25) */
static void apply_turn(Tok *d)
{
    char buf[512]; snprintf(buf, sizeof buf, "%s", d->s);
    char *w[64]; int nw = 0;
    for (char *t = strtok(buf, " \t"); t && nw < 64; t = strtok(NULL, " \t")) w[nw++] = t;
    int k = 1, names[64], files[64], nn = 0;       /* w[0] is TURN */
    while (k < nw && strcasecmp(w[k], "checking")) {
        if (strncasecmp(w[k], "ec-", 3)) {
            /* a file-name after an exception-name (rule 1: a word not EC-) */
            if (!nn) die_at(d->line, ">>TURN: '%s' is not an exception-name", w[k]);
            char lw[64]; int q = 0; for (; w[k][q] && q < 63; q++) lw[q] = (char)tolower((unsigned char)w[k][q]); lw[q] = 0;
            File *f = file_find(lw);
            if (!f) die_at(d->line, ">>TURN: '%s' is not a file-name", w[k]);
            const char *en = ec_name(names[nn - 1]);
            if (strncmp(en, "EC-I-O", 6)) die_at(d->line, ">>TURN: a file-name follows only an EC-I-O exception-name (2023 7.3.25.3 rule 4)");
            if (files[nn - 1] >= 0) { if (nn == 64) die_at(d->line, ">>TURN: too many names"); names[nn] = names[nn - 1]; nn++; }
            files[nn - 1] = (int)(f - g_files); k++;
            continue;
        }
        int i = ec_find(w[k], d->line);
        if (i < 0) die_at(d->line, ">>TURN: '%s' is not an exception-name", w[k]);
        if (nn == 64) die_at(d->line, ">>TURN: too many names");
        names[nn] = i; files[nn] = -1; nn++; k++;
    }
    if (!nn || k >= nw) die_at(d->line, ">>TURN needs exception-names and CHECKING ON or OFF");
    for (int a = 0; a < nn; a++)
        for (int b = a + 1; b < nn; b++)
            if (names[a] == names[b] && files[a] == files[b])
                die_at(d->line, ">>TURN names %s%s%s twice (2023 7.3.25.3 rule 3)", ec_name(names[a]),
                       files[a] >= 0 ? " for " : "", files[a] >= 0 ? g_files[files[a]].name : "");
    k++;
    int on = 1, loc = 0;
    if (k < nw && !strcasecmp(w[k], "off")) { on = 0; k++; }
    else {
        if (k < nw && !strcasecmp(w[k], "on")) k++;
        if (k < nw && !strcasecmp(w[k], "with")) k++;
        if (k < nw && !strcasecmp(w[k], "location")) { loc = 1; k++; }
    }
    if (k < nw) die_at(d->line, ">>TURN: unexpected '%s'", w[k]);
    for (int j = 0; j < nn; j++) {
        int i = names[j];
        for (int c = 0; c < NEC + g_necu; c++) if (ec_covers(i, c)) ec_turn_c(c, files[j], on, loc);
        if (files[j] < 0 && ec_covers_later_users(i)) { g_ecs.user_on = on; g_ecs.user_loc = on && loc; }
    }
}

static int unit_use_own_from(void);

/* the directives the parser has reached */
static void apply_dirs(void)
{
    while (g_ndir_done < g_ndir && g_dir[g_ndir_done].pos <= g_tp) apply_turn(&g_dir[g_ndir_done++].tok);
}

/* the unit's declarative sections: each USE names files or open modes.
 * After an I/O statement the compiler emits the choice: this unit's USE
 * for the file, then this unit's for the open mode, then outward through
 * the containing programs' GLOBAL ones (X3.23-1985 USE general rules). */
typedef struct { int sec, unit, global, mode; File *file; int ec; } UseEntry;   /* ec: an exception-name's index (USE AFTER EXCEPTION CONDITION), else -1 */
static UseEntry g_use[64]; static int g_nuse;
static struct { int unit, sec; int rep; } g_rwuse[16]; static int g_nrwuse;   /* USE BEFORE REPORTING sections: their report, for SUPPRESS */
static int g_in_decl;

/* USE [GLOBAL] AFTER [STANDARD] {ERROR|EXCEPTION} PROCEDURE [ON] {file... | INPUT | OUTPUT | I-O | EXTEND} */
static void parse_use(void)
{
    int line = cur()->line;
    if (!g_in_decl) die_at(line, "USE belongs in a DECLARATIVES section");
    /* immediately after the section header, a sentence by itself
     * (X3.23-1985 USE rule 1; 2023 14.9.49.3 rule 1) */
    if (g_cur_sec_id < 0 || g_tp < 4 || g_tok[g_tp - 2].kind != T_PERIOD || !is_word(&g_tok[g_tp - 3], "section"))   /* USE itself is g_tp - 1 */
        die_at(line, "USE immediately follows its section header (2023 14.9.49.3 rule 1)");
    int global = accept_word("global");
    if (accept_word("before")) {
        expect_word("reporting");
        if (cur()->kind != T_WORD) die_at(line, "USE BEFORE REPORTING needs a report group name");
        RGroup *g = NULL; Report *r = NULL;
        for (int i = g_report_base; i < g_nreport && !g; i++)
            for (int k = 0; k < g_reports[i].ng; k++)
                if (g_reports[i].g[k].name[0] && !strcmp(g_reports[i].g[k].name, cur()->s)) { r = &g_reports[i]; g = &g_reports[i].g[k]; break; }
        if (!g) die_at(line, "'%s' is not a report group", cur()->s);
        if (g->use_sec >= 0) die_at(line, "two USE BEFORE REPORTING procedures for '%s'", cur()->s);
        advance();
        g->use_sec = g_cur_sec_id;
        if (g_nrwuse == 16) die_at(line, "too many USE BEFORE REPORTING sections");
        g_rwuse[g_nrwuse].unit = g_unit; g_rwuse[g_nrwuse].sec = g_cur_sec_id; g_rwuse[g_nrwuse].rep = (int)(r - g_reports);
        g_nrwuse++;
        (void)global;
        if (cur()->kind != T_PERIOD) die_at(cur()->line, "the USE statement is a sentence by itself (2023 14.9.49.3 rule 1)");
        return;
    }
    if (at_word("for") && is_word(cur() + 1, "debugging")) {
        /* the section's uses of the module's special register stay quiet */
        static const char *dbg[] = { "debug-item", "debug-line", "debug-name", "debug-sub-1", "debug-sub-2",
                                     "debug-sub-3", "debug-contents", NULL };
        for (int i = 0; dbg[i] && g_npoison < 64; i++) snprintf(g_poison[g_npoison++], sizeof g_poison[0], "%s", dbg[i]);
        die_at(line, "USE FOR DEBUGGING is the Debug module, obsolete in COBOL 85 (item 18) and not implemented here");
    }
    expect_word("after");
    if ((at_word("exception") && is_word(cur() + 1, "condition")) || at_word("ec")) {
        /* USE AFTER EXCEPTION CONDITION exception-name ... (2023 14.9.49 format 3) */
        if (g_std < 2002) die_at(line, "USE AFTER EXCEPTION CONDITION is COBOL 2002; compile with -std=2002");
        if (global) die_at(line, "USE GLOBAL is not allowed with EXCEPTION CONDITION (2023 14.9.49.2, format 3 has no GLOBAL)");
        if (!accept_word("ec")) { advance(); advance(); }
        int any = 0;
        while (cur()->kind == T_WORD && !strncmp(cur()->s, "ec-", 3)) {
            int i = ec_find(cur()->s, cur()->line);
            if (i < 0) die_at(cur()->line, "'%s' is not an exception-name", cur()->s);
            advance();
            if (at_word("file")) die_at(cur()->line, "USE AFTER EXCEPTION CONDITION ... FILE is not implemented yet");
            /* the same name in two USE statements is allowed: the first
             * in the source is the one selected (14.9.49.4 rule 3) */
            if (g_nuse == 64) die_at(line, "too many USE procedures");
            g_use[g_nuse].sec = g_cur_sec_id; g_use[g_nuse].unit = g_unit; g_use[g_nuse].global = 0;
            g_use[g_nuse].mode = 0; g_use[g_nuse].file = NULL; g_use[g_nuse].ec = i;
            g_nuse++; any = 1;
        }
        if (!any) die_at(line, "USE AFTER EXCEPTION CONDITION needs an exception-name");
        if (cur()->kind != T_PERIOD) die_at(cur()->line, "the USE statement is a sentence by itself (2023 14.9.49.3 rule 1)");
        return;
    }
    if (at_word("exception") && is_word(cur() + 1, "object")) die_at(line, "USE AFTER EXCEPTION OBJECT is object orientation, not implemented");
    accept_word("standard");
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
            if (f->org == COB_ORG_SORT)
                die_at(line, "'%s' is a sort or merge file and takes no USE procedure (2023 14.9.49.3 rule 2)", f->name);
        }
        for (int i = 0; i < g_nuse; i++)
            if (g_use[i].unit == g_unit && g_use[i].mode == mode && g_use[i].file == f)
                die_at(line, mode ? "two USE procedures for the same open mode (2023 14.9.49.3 rule 7)" : "two USE procedures for file '%s' (2023 14.9.49.3 rule 8)", f ? f->name : "");
        if (g_nuse == 64) die_at(line, "too many USE procedures");
        g_use[g_nuse].sec = sec; g_use[g_nuse].unit = g_unit; g_use[g_nuse].global = global; g_use[g_nuse].mode = mode; g_use[g_nuse].file = f; g_use[g_nuse].ec = -1;
        g_nuse++; any = 1;
    }
    if (!any) die_at(line, "USE AFTER ERROR PROCEDURE needs a file-name or INPUT/OUTPUT/I-O/EXTEND");
    if (cur()->kind != T_PERIOD) die_at(cur()->line, "the USE statement is a sentence by itself (2023 14.9.49.3 rule 1)");
}

/* after an I/O statement, with its result in SLOT_C: if the condition is
 * not handled by the statement's own clause and a USE procedure applies,
 * perform that section (the runtime picks it: the file's, else the open
 * mode's), then continue with the next statement */
static void unit_use_range(int level, int *from, int *to);
static int unit_use_own_from(void);                     /* where this unit's own USE entries begin */
static int ecp_target(int i, int fidx, Ecp **ep, int *resume);

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
                if (u->ec >= 0) continue;                 /* an exception-name's, not a file's */
                if (pass == 0 ? u->file != f : !u->mode) continue;
                if (u->mode) any_mode = 1;
                c[nc++] = u;
            }
    }
    /* SLOT_C after the statement: 0 fine, 1 the statement's own condition,
     * 2 an error with a FILE STATUS to record it, 3 an error nothing but a
     * USE procedure can take -- the run stops if none does */
    int Ldone = new_label();
    /* EC-I-O (cobol ISSUES-58): with checking on, the condition the I-O
     * status names (2023 9.1.13) -- after the statement's own phrase and
     * the file's and the open mode's USE AFTER ERROR procedures, before
     * the run stops for want of one (USE general rule 3) */
    int fidx = (int)(f - g_files);
    int warn = ec_on_io("EC-I-O-WARNING", fidx), Lwarn = warn ? new_label() : 0;
    emit("\tldw r13, sp+%d", SLOT_C);
    emit("\tbeq r13, r0, .L%d", warn ? Lwarn : Ldone);
    if (has_clause) { emit_li("r2", 1); emit("\tbeq r13, r2, .L%d", Ldone); }
    static const struct { const char *name; int digit; } ecio[] = {
        { "EC-I-O-AT-END", 1 }, { "EC-I-O-INVALID-KEY", 2 }, { "EC-I-O-PERMANENT-ERROR", 3 },
        { "EC-I-O-LOGIC-ERROR", 4 }, { "EC-I-O-RECORD-OPERATION", 5 }, { "EC-I-O-FILE-SHARING", 6 },
        { "EC-I-O-RECORD-CONTENT", 7 }, { "EC-I-O-IMP", 9 }, { NULL, 0 } };
    /* inside imperative-statement-1 of an exception-checking PERFORM, a
     * condition a WHEN phrase takes goes there, and a USE procedure that
     * would match is ignored (14.9.28 rules 17, 18; cobol ISSUES-94 E8) */
    int taken[16] = { 0 }, any_taken = 0;
    for (int k = 0; g_necp && ecio[k].name; k++) {
        Ecp *e; int r, i = ec_find(ecio[k].name, 0);
        if (ec_on_io(ecio[k].name, fidx) && ecp_target(i, fidx, &e, &r) >= 0) { taken[k] = 1; any_taken = 1; }
    }
    if (any_taken) {
        emit_call("cob_io_class");                  /* the status's first digit; r13 survives */
        for (int k = 0; ecio[k].name; k++) {
            if (!taken[k]) continue;
            int Lnext = new_label();
            emit_li("r2", ecio[k].digit);
            emit("\tbne r1, r2, .L%d", Lnext);
            g_ec_file = f->oname; g_ec_fidx = fidx;
            emit_ec_raise(ec_find(ecio[k].name, 0));
            g_ec_file = NULL; g_ec_fidx = -1;
            emit_jump(Ldone);
            emit_label(Lnext);
        }
    }
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
    int any = 0;
    for (int k = 0; ecio[k].name; k++) if (ec_on_io(ecio[k].name, fidx) && !taken[k]) any = 1;
    if (any) {
        emit_call("cob_io_class");                  /* the status's first digit; r13 survives */
        for (int k = 0; ecio[k].name; k++) {
            if (!ec_on_io(ecio[k].name, fidx) || taken[k]) continue;
            int Lnext = new_label();
            emit_li("r2", ecio[k].digit);
            emit("\tbne r1, r2, .L%d", Lnext);
            g_ec_file = f->oname; g_ec_fidx = fidx;
            emit_ec_raise(ec_find(ecio[k].name, 0));      /* a fatal one ends the run here */
            g_ec_file = NULL; g_ec_fidx = -1;
            emit_jump(Ldone);
            emit_label(Lnext);
        }
    }
    emit_li("r2", 3);
    emit("\tbne r13, r2, .L%d", Ldone);
    emit_file_addr("r3", f);
    emit_call("cob_io_unhandled");
    if (warn) {
        /* a successful statement whose status is not 00: EC-I-O-WARNING,
         * only when turned on by its own name (7.3.25 rule 4) */
        emit_jump(Ldone);
        emit_label(Lwarn);
        emit_call("cob_io_class");
        emit("\tbne r1, r0, .L%d", Ldone);
        g_ec_file = f->oname; g_ec_fidx = fidx;
        emit_ec_raise(ec_find("EC-I-O-WARNING", 0));
        g_ec_file = NULL; g_ec_fidx = -1;
    }
    emit_label(Ldone);
}

/* ---- PERFORM ---------------------------------------------------------- */

static int g_ncnt;      /* TIMES counters */
static int *g_cnt_unit; static int g_cnt_cap;   /* the unit each counter belongs to */

typedef struct { Para *from, *thru; int inline_body, Lexit; } Body;   /* Lexit: after END-PERFORM, for EXIT PERFORM */

/* the inline PERFORMs being compiled, innermost last: where EXIT PERFORM
 * goes, and EXIT PERFORM CYCLE (-1: not allowed) (2023 14.9.14.4 rules
 * 4-5; cobol ISSUES-90) */
static struct { int Lexit, Lcycle; } g_pstk[64]; static int g_npstk;
static void pstk_push(int lexit, int lcycle)
{
    if (g_npstk == 64) die_at(cur()->line, "inline PERFORMs nested more than 64 deep");
    g_pstk[g_npstk].Lexit = lexit; g_pstk[g_npstk].Lcycle = lcycle; g_npstk++;
}
/* EXIT PARAGRAPH and EXIT SECTION: the current paragraph's and section's
 * end, made when an EXIT asks for it (rules 6-7) */
static int g_exit_par_label = -1, g_exit_sec_label = -1;
static void end_par_label(void) { if (g_exit_par_label >= 0) { emit_label(g_exit_par_label); g_exit_par_label = -1; } }
static void end_sec_label(void) { if (g_exit_sec_label >= 0) { emit_label(g_exit_sec_label); g_exit_sec_label = -1; } }

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
static int g_is_merge;    /* parse_sort is parsing MERGE: USING of two or more files, no INPUT PROCEDURE;
                             each is already in key order and joins the merge as presorted runs (cob_merge_using) */

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
               !at_word("with") && !at_word("collating") && !at_word("sequence") && !at_word("using") && !at_word("input") &&
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
    int coll = -1;                          /* [COLLATING] SEQUENCE [IS] alphabet-name: the keys compare by its ranks */
    if (accept_word("collating") || at_word("sequence")) {
        expect_word("sequence"); accept_word("is");
        if (cur()->kind != T_WORD) die_at(cur()->line, "SORT COLLATING SEQUENCE needs an alphabet-name");
        for (int i = 0; i < g_nalphabet; i++) if (!strcmp(g_alphabet[i].name, cur()->s)) coll = i;
        if (coll < 0) die_at(cur()->line, "SORT COLLATING SEQUENCE: '%s' is not an alphabet-name", cur()->s);
        if (g_alphabet[coll].native) coll = -1;
        else g_alphabet[coll].used = 1;
        advance();
    }
    char tab[32]; snprintf(tab, sizeof tab, ".Lsk%d_%d", g_unit, t->id);
    emit_file_addr("r3", sd); emit_la("r4", tab); emit_li("r5", t->nk); emit_li("r6", dups);
    if (coll >= 0) { char al[32]; snprintf(al, sizeof al, ".Lalph%d_%d", g_unit, coll); emit_la("r7", al); } else emit_li("r7", 0);
    emit_call("cob_sort_begin");
    if (accept_word("using")) {
        int n = 0;
        while (cur()->kind == T_WORD && !at_word("giving") && !at_word("output")) {
            File *in = expect_file();
            if (in->org == COB_ORG_SORT) die_at(line, "%s USING names a sort file", verb);
            emit_file_addr("r3", sd); emit_file_addr("r4", in); emit_call(g_is_merge ? "cob_merge_using" : "cob_sort_using"); n++;
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
        /* an inline body: EXIT PERFORM CYCLE comes to its end, EXIT PERFORM
         * past END-PERFORM */
        if (b->Lexit < 0) { parse_statements(); return; }
        int Lcycle = new_label();
        pstk_push(b->Lexit, Lcycle);
        parse_statements();
        g_npstk--;
        emit_label(Lcycle);
    }
}

static void emit_add_to_ref(Opnd *by, Ref *var)
{
    Opnd ops[1] = { *by }; Ref rs[1] = { *var };
    int hot = opnd_hot_int(by) && ref_hot_store(var, 0, ops_all_nonneg(ops, 1));
    int rd[1] = { 0 };
    if (hot) emit_hot_sum(ops, 1);
    else emit_push(by);
    emit_store_receivers(rs, rd, 1, hot, 0, 0, 0, ops_sum_mag(ops, 1), ops_all_nonneg(ops, 1));
}

typedef struct { Ref var; Opnd from, by; Cond *until; } Vary;

/* an induction variable to its FROM value; an index-name set from an
 * identifier that is not positive is EC-RANGE-PERFORM-VARYING (2023
 * 14.9.28.4 rule 3) */
static void emit_vary_init(Vary *x)
{
    if (x->var.sym->is_index && x->from.kind == O_REF && ec_on_name("EC-RANGE-PERFORM-VARYING")) {
        int Lok = new_label();
        Arg a[2] = { arg_ref(&x->from.ref), arg_desc(sym_desc(x->from.ref.sym)) };
        emit_args(a, 2); emit_call("cob_load_int");
        emit("\tblt r0, r1, .L%d", Lok);
        emit_ec_raise(ec_find("EC-RANGE-PERFORM-VARYING", 0));
        emit_label(Lok);
    }
    emit_move(&x->from, &x->var);
}

static void emit_varying(Vary *v, int nv, int level, Body *body, int test_after)
{
    Vary *x = &v[level];
    emit_vary_init(x);
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
    if (level > 0) emit_vary_init(x);
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
    for (int k = 0; k < nv; k++) emit_vary_init(&v[k]);
    int Ltop = new_label();
    emit_label(Ltop);
    emit_body(body);
    for (int k = nv - 1; k >= 0; k--) {
        int Ldone = new_label();
        cond_jump_true(v[k].until, Ldone);
        for (int j = k + 1; j < nv; j++) emit_vary_init(&v[j]);
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

/* The WHEN phrases of the inline PERFORM at the cursor, read ahead from
 * the tokens -- their names are turned on before imperative-statement-1
 * is compiled (14.9.28 rule 14).  Returns 1 when the PERFORM ends in WHEN
 * ... EXCEPTION or FINALLY at its own level, an exception-checking
 * PERFORM; with e NULL it only answers that. */
static int g_in_finally;            /* inside a FINALLY phrase: no transfer out of the PERFORM (14.9.28.4 rule 16) */
static int ecp_scan(Ecp *e)
{
    int depth = 0, found = 0;
    for (int k = g_tp; k < g_ntok; k++) {
        Tok *t = &g_tok[k];
        if (t->kind == T_PERIOD || t->kind == T_EOF) break;
        if (t->kind != T_WORD) continue;
        if (!strcmp(t->s, "perform") && !(k > 0 && is_word(&g_tok[k - 1], "exit"))) {   /* EXIT PERFORM opens nothing */
            Tok *n = &g_tok[k + 1];
            if (!(at_para_name(n) && para_find(n->s))) depth++;       /* inline: closed by END-PERFORM */
            continue;
        }
        if (!strcmp(t->s, "end-perform")) { if (depth-- == 0) break; continue; }
        if (depth) continue;
        if (!strcmp(t->s, "finally")) { if (!e) return 1; found = 1; continue; }
        if (strcmp(t->s, "when")) continue;
        Tok *a = &g_tok[k + 1], *b = &g_tok[k + 2];
        int other = is_word(a, "other") && is_word(b, "exception"), common = is_word(a, "common") && is_word(b, "exception");
        if (!is_word(a, "exception") && !other && !common) continue;
        if (!e) return 1;
        found = 1;
        if (other) { e->Lother = new_label(); continue; }
        if (common) { e->Lcommon = new_label(); continue; }
        if (e->nw == e->wcap) { e->wcap = e->wcap ? 2 * e->wcap : 8; e->w = xrealloc(e->w, (size_t)e->wcap * sizeof *e->w); }
        EcpWhen *w = &e->w[e->nw++]; memset(w, 0, sizeof *w); w->label = new_label();
        for (int q = k + 2; q < g_ntok && g_tok[q].kind == T_WORD && !is_verb(g_tok[q].s); q++) {
            Tok *x = &g_tok[q];
            if (strncmp(x->s, "ec-", 3))
                die_at(x->line, "WHEN EXCEPTION with a file-name or an open mode is not implemented yet (exception-names, and name FILE file-name, are)");
            int i = ec_find(x->s, x->line);
            if (i < 0) die_at(x->line, "'%s' is not an exception-name", x->s);
            int file = -1;
            if (is_word(&g_tok[q + 1], "file")) {
                File *f = file_find(g_tok[q + 2].s);
                if (!f) die_at(x->line, "WHEN %s FILE: '%s' is not a file-name", ec_name(i), g_tok[q + 2].s);
                if (strncmp(ec_name(i), "EC-I-O", 6)) die_at(x->line, "FILE follows only an EC-I-O exception-name (2023 14.9.28.3 rule 16)");
                file = (int)(f - g_files); q += 2;
            }
            for (int v = 0; v < e->nw; v++) for (int u = 0; u < e->w[v].n; u++)
                if (e->w[v].ec[u] == i && e->w[v].file[u] == file)
                    die_at(x->line, "%s appears twice in the WHEN phrases (2023 14.9.28.3 rule 15)", ec_name(i));
            if (w->n == w->cap) { w->cap = w->cap ? 2 * w->cap : 8; w->ec = xrealloc(w->ec, (size_t)w->cap * sizeof *w->ec); w->file = xrealloc(w->file, (size_t)w->cap * sizeof *w->file); }
            w->ec[w->n] = i; w->file[w->n] = file; w->n++;
        }
        if (!w->n) die_at(t->line, "WHEN EXCEPTION needs an exception-name");
    }
    return found;
}
static int perform_is_ecp(void) { return ecp_scan(NULL); }

/* the implicit TURN before imperative-statement-1 (rule 14): each
 * condition a WHEN name covers whose checking is not already enabled --
 * for all files, or for the WHEN's file -- turned on, with LOCATION when
 * the PERFORM has it; a condition already enabled keeps its setting */
static void ecp_implicit_turn(int i, int file, int loc)
{
    for (int c = 0; c < NEC + g_necu; c++) {
        if (!ec_covers(i, c)) continue;
        if (file >= 0 ? ec_on_file(c, file, NULL) : ec_on_all(c)) continue;
        ec_turn_c(c, file, 1, loc);
    }
    if (file < 0 && ec_covers_later_users(i) && !g_ecs.user_on) { g_ecs.user_on = 1; g_ecs.user_loc = loc; }
}

/* PERFORM [WITH LOCATION] imperative-statement-1 {WHEN EXCEPTION ...}...
 * [WHEN OTHER EXCEPTION ...] [WHEN COMMON EXCEPTION ...] [FINALLY ...]
 * END-PERFORM (2023 14.9.28 format 3) */
static void parse_perform_ecp(void)
{
    static int ecp_ids;
    int line = cur()->line, loc = 0, start = g_tp;
    if (at_word("with") && is_word(peek(1), "location")) { advance(); advance(); loc = 1; }
    Ecp *e = xmalloc(sizeof *e); memset(e, 0, sizeof *e);
    e->id = ecp_ids++; e->Lother = e->Lcommon = -1; e->Lend = new_label();
    ecp_scan(e);
    /* the checking before the PERFORM: after END-PERFORM it is back as it
     * was, for no TURN can be inside it (7.3.25.3 rule 5), and whatever
     * the implicit TURN enabled is not enabled any more (rule 22) */
    EcState pre; memset(&pre, 0, sizeof pre); ecs_copy(&pre, &g_ecs);
    int necu0 = g_necu;
    for (int w = 0; w < e->nw; w++) for (int q = 0; q < e->w[w].n; q++) ecp_implicit_turn(e->w[w].ec[q], e->w[w].file[q], loc);
    /* imperative-statement-1, a statement at a time: a raise resumes after
     * the statement it occurred in (rule 20) */
    int Lafter = new_label();
    pstk_push(e->Lend, -1);                       /* EXIT PERFORM: to FINALLY or END-PERFORM, no CYCLE (rules 4, 8) */
    if (g_necp == g_ecp_cap) { g_ecp_cap = g_ecp_cap ? 2 * g_ecp_cap : 8; g_ecp = xrealloc(g_ecp, (size_t)g_ecp_cap * sizeof *g_ecp); }
    g_ecp[g_necp++] = e;
    while (!at_word("when") && !at_word("finally") && !at_word("end-perform") && !at_scope_end()) {
        e->resume = new_label();
        parse_statement();
        emit_label(e->resume);
    }
    g_necp--;
    emit_jump(e->Lend);
    /* the phrases: checking off inside them (the implicit PUSH ALL and
     * TURN OFF ALL, rule 14), no WHEN of this PERFORM for their raises (21) */
    memset(g_ecs.on, 0, sizeof g_ecs.on); memset(g_ecs.loc, 0, sizeof g_ecs.loc);
    g_ecs.nf = 0; g_ecs.user_on = g_ecs.user_loc = 0;
    g_ecp_handler++;
    int wi = 0;
    for (;;) {
        int is_common = 0;
        if (at_word("when") && is_word(peek(1), "exception")) {
            advance(); advance();
            while (cur()->kind == T_WORD && !is_verb(cur()->s)) advance();     /* the names, read already */
            emit_label(e->w[wi++].label);
        } else if (at_word("when") && is_word(peek(1), "other")) { advance(); advance(); expect_word("exception"); emit_label(e->Lother); }
        else if (at_word("when") && is_word(peek(1), "common")) { advance(); advance(); expect_word("exception"); emit_label(e->Lcommon); is_common = 1; }
        else break;
        parse_statements();
        /* a WHEN phrase goes on to WHEN COMMON (17-19); the last of them
         * returns where the raise left its resume point -- after the
         * statement for a nonfatal condition; a fatal one ends the run
         * there (20; 14.6.13.1.3 rule 4) */
        if (!is_common && e->Lcommon >= 0) { emit_jump(e->Lcommon); continue; }
        emit_li("r3", e->id);
        emit("\tadd r4, sp, r0");
        emit_call("cob_ecp_pop");
        emit("\tjalr r0, r1, 0");
    }
    emit_label(e->Lend);
    /* what a phrase left by EXIT PERFORM: dropped, and a fatal condition
     * ends the run all the same */
    emit_li("r3", e->id);
    emit("\tadd r4, sp, r0");
    emit_call("cob_ecp_drop");
    g_npstk--;
    if (accept_word("finally")) {                   /* in FINALLY: EXIT PERFORM goes past END-PERFORM (16) */
        pstk_push(Lafter, -1); g_in_finally++; parse_statements(); g_in_finally--; g_npstk--;
    }
    g_ecp_handler--;
    if (!accept_word("end-perform")) die_at(cur()->line, "expected END-PERFORM to end the exception-checking PERFORM, found %s", tok_desc(cur()));
    emit_label(Lafter);
    for (int k = 0; k < g_ndir; k++)
        if (g_dir[k].pos >= start && g_dir[k].pos < g_tp)
            die_at(g_dir[k].tok.line, "a TURN directive inside an exception-checking PERFORM (2023 7.3.25.3 rule 5)");
    ecs_copy(&g_ecs, &pre);
    for (int c = NEC + necu0; c < NEC + g_necu; c++) { g_ecs.on[c] = (unsigned char)pre.user_on; g_ecs.loc[c] = (unsigned char)pre.user_loc; }
    free(pre.f);
    (void)line;
}

static void decl_ref_check(const Para *p, int is_perform, int line);
/* the declarative section a paragraph or section is in, or -1 */
static int para_decl_sec(const Para *p)
{
    int sec = p->is_section ? p->id : p->section;    /* ids from 1; 0: in no section */
    if (!sec) return -1;
    for (int u = 0; u < g_nuse; u++) if (g_use[u].unit == g_unit && g_use[u].sec == sec) return sec;
    for (int u = 0; u < g_nrwuse; u++) if (g_rwuse[u].unit == g_unit && g_rwuse[u].sec == sec) return sec;
    return -1;
}

/* VARYING or AFTER with an index-name (2023 14.9.28.3 rules 4-6; X3.23-1985
 * PERFORM rules 3-4): the other operands integers, FROM a positive and BY
 * a nonzero literal; BY is never zero */
static void varying_rules(const Ref *var, const Opnd *from, const Opnd *by)
{
    if (by->kind == O_NUM && numlit_is_zero(&by->num))
        die_at(by->line, "the BY literal of PERFORM VARYING shall not be zero (2023 14.9.28.3 rule 6)");
    if (var->sym->is_index) {
        if (from->kind == O_REF && !is_int_item(from->ref.sym))
            die_at(from->line, "VARYING an index-name: FROM '%s' must be an integer item (2023 14.9.28.3 rule 4a)", from->ref.sym->name);
        if (from->kind == O_NUM && (!numlit_is_int(&from->num) || from->num.neg || numlit_is_zero(&from->num)))
            die_at(from->line, "VARYING an index-name: the FROM literal must be a positive integer (2023 14.9.28.3 rule 4b)");
        if (by->kind == O_REF && !is_int_item(by->ref.sym))
            die_at(by->line, "VARYING an index-name: BY '%s' must be an integer item (2023 14.9.28.3 rule 4a)", by->ref.sym->name);
        if (by->kind == O_NUM && !numlit_is_int(&by->num))
            die_at(by->line, "VARYING an index-name: the BY literal must be a nonzero integer (2023 14.9.28.3 rule 4c)");
    }
    if (from->kind == O_REF && from->ref.sym->is_index) {
        if (!is_int_item(var->sym))
            die_at(var->line, "FROM an index-name: the VARYING item '%s' must be an integer (2023 14.9.28.3 rule 5a)", var->sym->name);
        if (by->kind == O_REF && !is_int_item(by->ref.sym))
            die_at(by->line, "FROM an index-name: BY '%s' must be an integer item (2023 14.9.28.3 rule 5b)", by->ref.sym->name);
        if (by->kind == O_NUM && !numlit_is_int(&by->num))
            die_at(by->line, "FROM an index-name: the BY literal must be an integer (2023 14.9.28.3 rule 5c)");
    }
}

static void parse_perform(void)
{
    Body body; memset(&body, 0, sizeof body);
    if (g_std >= 2002 && ((at_word("with") && is_word(peek(1), "location")) ||
        (!(at_para_name(cur()) && para_find(cur()->s)) && perform_is_ecp()))) { parse_perform_ecp(); return; }
    if (at_para_name(cur()) && para_find(cur()->s)) {
        body.from = expect_para();
        decl_ref_check(body.from, 1, cur()->line);
        if (accept_word("thru") || accept_word("through")) {
            body.thru = expect_para();
            decl_ref_check(body.thru, 1, cur()->line);
            /* a range into or out of the declaratives stays in one
             * declarative section (X3.23-1985 PERFORM rule 5; 2023
             * 14.9.28.3 rule 11) */
            int d1 = para_decl_sec(body.from), d2 = para_decl_sec(body.thru);
            if ((d1 >= 0 || d2 >= 0) && d1 != d2)
                die_at(cur()->line, "PERFORM %s THRU %s: a range that names a declarative procedure stays in one declarative section (2023 14.9.28.3 rule 11)",
                       body.from->oname, body.thru->oname);
        }
    } else {
        /* a name that is no statement, loop phrase or TIMES count can only
         * have meant a paragraph */
        Tok *t = cur(), *n = peek(1);
        if (t->kind == T_WORD && !is_verb(t->s) && !is_terminator(t->s) && !is_word(t, "with") && !is_word(t, "test") &&
            !is_word(t, "until") && !is_word(t, "varying") && !is_word(n, "times") && !is_word(n, "of") && !is_word(n, "in") &&
            n->kind != T_LP)
            die_at(t->line, "'%s' is not a paragraph or section", t->s);
        body.inline_body = 1;
        body.Lexit = g_std >= 2002 ? new_label() : -1;     /* EXIT PERFORM is 2002: -std=85 output keeps its labels */
    }

    int test_after = 0, test_given = 0;
    if (accept_word("with")) { expect_word("test"); test_given = 1; if (accept_word("after")) test_after = 1; else expect_word("before"); }
    else if (accept_word("test")) { test_given = 1; if (accept_word("after")) test_after = 1; else expect_word("before"); }

    if (accept_word("until") && g_std >= 2002 && accept_word("exit")) {
        if (test_given) die_at(g_tok[g_tp - 1].line, "UNTIL EXIT takes no WITH TEST phrase (2023 14.9.28.3 rule 8)");
        /* UNTIL EXIT: a condition that never holds (14.9.28.4 rule 11); an
         * EXIT PERFORM, a GOBACK or a STOP leaves it (cobol ISSUES-90) */
        int Ltop = new_label();
        emit_label(Ltop);
        emit_body(&body);
        emit_jump(Ltop);
    } else if (g_tok[g_tp - 1].kind == T_WORD && !strcmp(g_tok[g_tp - 1].s, "until")) {
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
            expect_word("from");
            /* an AFTER's FROM is evaluated at every reset and BY at every
             * step, not where they are parsed: a user function there waits
             * for a deferred evaluation like a condition's */
            if (nv > 0) g_ufn_forbid = "the FROM phrase of PERFORM ... AFTER";
            parse_operand(&v[nv].from); check_numeric_opnd(&v[nv].from);
            g_ufn_forbid = NULL;
            if (nv > 0 && v[nv].from.kind == O_REF)          /* BP-M1: the 74/85 reset order shows here */
                for (int k = 0; k < nv; k++)
                    if (v[k].var.sym == v[nv].from.ref.sym) { bp(BP_M1_VARYING_AFTER, v[nv].from.line); break; }
            expect_word("by");
            g_ufn_forbid = "the BY phrase of PERFORM VARYING";
            parse_operand(&v[nv].by); check_numeric_opnd(&v[nv].by);
            g_ufn_forbid = NULL;
            varying_rules(&v[nv].var, &v[nv].from, &v[nv].by);
            expect_word("until");
            if (at_word("exit"))
                die_at(cur()->line, "UNTIL EXIT is not a VARYING or AFTER phrase's condition (2023 14.9.28.3 rule 8)");
            v[nv].until = parse_cond();
            nv++;
            if (!accept_word("after")) break;
        }
        if (g_std < 2002 && body.inline_body && nv > 1)
            die_at(v[1].var.line, "an in-line PERFORM VARYING takes no AFTER phrase in COBOL 85 (X3.23-1985 PERFORM syntax rule 2)");
        if (test_after && nv > 1) emit_varying_test_after(v, nv, &body);
        else emit_varying(v, nv, 0, &body, test_after);
    } else if (at_operand() && times_follows()) {
        Opnd n; parse_operand(&n); check_numeric_opnd(&n);
        if ((n.kind == O_REF && !is_int_item(n.ref.sym)) || (n.kind == O_NUM && !numlit_is_int(&n.num)))
            die_at(n.line, "PERFORM ... TIMES takes an integer (2023 14.9.28.3 rule 2)");
        expect_word("times");
        if (g_ncnt == g_cnt_cap) { g_cnt_cap = g_cnt_cap ? 2 * g_cnt_cap : 64; g_cnt_unit = realloc(g_cnt_unit, (size_t)g_cnt_cap * sizeof *g_cnt_unit); }
        g_cnt_unit[g_ncnt] = g_unit;
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
    if (body.inline_body) { if (body.Lexit >= 0) emit_label(body.Lexit); expect_word("end-perform"); }
    /* an out-of-line PERFORM has no END-PERFORM: the next one belongs to
     * whatever inline PERFORM encloses this statement */
}

/* ---- GO TO, SET ------------------------------------------------------- */

/* declaratives and the rest meet only by PERFORM (2023 14.9.49.3 rules
 * 3-4; X3.23-1985 USE rules 3-4): a declarative procedure names no
 * nondeclarative one, and a declarative one is named from outside its
 * section only by PERFORM */
static void decl_ref_check(const Para *p, int is_perform, int line)
{
    if (g_in_decl && !p->in_decl)
        die_at(line, "a declarative procedure refers to '%s', which is not in the declaratives (2023 14.9.49.3 rule 3)", p->oname);
    if (!is_perform && p->in_decl) {
        int here = g_cur_sec_id, there = p->is_section ? p->id : p->section;
        if (!g_in_decl || here != there)
            die_at(line, "'%s' is in a declarative section: it is named from outside that section only by PERFORM (2023 14.9.49.3 rule 4)", p->oname);
    }
}

static void parse_goto(void)
{
    if (g_in_finally) die_at(cur()->line, "GO TO in a FINALLY phrase: no statement there transfers control out of the PERFORM (2023 14.9.28.4 rule 16)");
    accept_word("to");
    Para *ps[64]; int n = 0;
    while (at_para_name(cur()) && !at_word("depending") && !(cur()->kind == T_WORD && (is_verb(cur()->s) || is_terminator(cur()->s))) && para_find(cur()->s)) {
        if (n >= 64) die_at(cur()->line, "too many GO TO targets");
        ps[n++] = expect_para();
        decl_ref_check(ps[n - 1], 0, cur()->line);
    }
    int altered = g_cur_para && is_altered_para(g_cur_para->name);
    if (!n && !altered && at_para_name(cur()) && !(cur()->kind == T_WORD && (is_verb(cur()->s) || is_terminator(cur()->s))) &&
        !at_word("depending"))
        die_at(cur()->line, "'%s' is not a paragraph or section", cur()->s);
    if (!n && !altered) die_at(cur()->line, "GO TO without a procedure-name: the paragraph is not named in any ALTER");
    if (altered && n <= 1 && !at_word("depending")) {
        /* through the paragraph's cell, which ALTER rewrites */
        if (g_naltcell == 64) die_at(cur()->line, "too many altered paragraphs");
        g_altcell[g_naltcell].para = g_cur_para->id; g_altcell[g_naltcell].target = n ? ps[0]->id : -1; g_naltcell++;
        char lab[32]; snprintf(lab, sizeof lab, ".Lalt%d_%d", g_unit, g_cur_para->id);
        emit_la("r1", lab);
        emit("\tldw r1, r1+0");
        emit("\tjalr r0, r1, 0");
        return;
    }
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

/* How well a WHEN (or USE) exception-name w, for file wf (-1: none),
 * matches condition i raised on file fidx: the order of USE general rule
 * 3c-3g (14.9.49.4), which 14.9.28 rule 17 points at -- the name with its
 * file, its group with the file, the name, the group, EC-ALL.  0 best;
 * -1 no match. */
static int ec_match_rank(int w, int wf, int i, int fidx)
{
    int g = ec_group(i);
    if (wf >= 0) {
        if (wf != fidx) return -1;
        return w == i ? 0 : w == g ? 1 : -1;
    }
    return w == i ? 2 : w == g ? 3 : w == ec_find("EC-ALL", 0) ? 4 : -1;
}

/* the WHEN phrase of the innermost exception-checking PERFORM that takes
 * condition i here, or -1; *resume set to where its return goes.  A fatal
 * condition goes to a WHEN that names it or its hierarchy only, never to
 * WHEN OTHER (14.6.13.1.3 rule 4); a nonfatal one to WHEN OTHER when no
 * WHEN names it (14.9.28 rule 18). */
static int ecp_target(int i, int fidx, Ecp **ep, int *resume)
{
    for (int k = g_necp - 1; k >= 0; k--) {
        Ecp *e = g_ecp[k];
        int best = -1, rank = 5;
        for (int w = 0; w < e->nw; w++)
            for (int q = 0; q < e->w[w].n; q++) {
                int r = ec_match_rank(e->w[w].ec[q], e->w[w].file[q], i, fidx);
                if (r >= 0 && r < rank) { rank = r; best = e->w[w].label; }
            }
        *ep = e; *resume = e->resume;
        if (best >= 0) return best;
        if (e->Lother >= 0 && !ec_fatal(i)) { *resume = e->Lend; return e->Lother; }
    }
    return -1;
}

/* a raise inside imperative-statement-1 that a WHEN takes: the resume
 * point and fatality onto libcob's stack of them (a recursive activation's
 * raise pushes its own; cobol ISSUES-94 E9), then the phrase; returns 1
 * when a WHEN took it */
static int ecp_dispatch(int i)
{
    Ecp *e; int resume;
    int target = ecp_target(i, g_ec_fidx, &e, &resume);
    if (target < 0) return 0;
    char lab[32]; snprintf(lab, sizeof lab, ".L%d", resume);
    emit_li("r3", e->id);
    emit_la("r4", lab);
    emit_li("r5", ec_fatal(i));
    emit("\tadd r6, sp, r0");                   /* the activation's frame */
    emit_call("cob_ecp_push");
    emit_jump(target);
    return 1;
}

/* after exception condition i is raised: a WHEN of the exception-checking
 * PERFORM around it, else the declarative that applies -- this program's
 * USE for the name, else its group's, else EC-ALL's (2023 14.6.13.1.3-4)
 * -- then stop the run if i is fatal */
static void emit_ec_dispatch(int i)
{
    if (g_necp && ecp_dispatch(i)) return;          /* the WHEN takes it; USE does not (17) */
    int cand[3] = { i, ec_group(i), ec_find("EC-ALL", 0) }, sec = -1;
    for (int c = 0; c < 3 && sec < 0; c++)
        for (int u = unit_use_own_from(); u < g_nuse; u++)
            if (g_use[u].unit == g_unit && g_use[u].ec >= 0 && g_use[u].ec == cand[c]) { sec = g_use[u].sec; break; }
    if (sec >= 0) {
        int Lret = new_label();
        char lab[32]; snprintf(lab, sizeof lab, ".L%d", Lret);
        emit_li("r3", sec);
        emit_la("r4", lab);
        emit_call("cob_use_push");                  /* EC-FLOW-USE when it is active already (E14) */
        emit("\tjal r0, .Lp%d_%d", g_unit, sec);
        emit_label(Lret);
    }
    if (ec_fatal(i)) emit_call("cob_ec_abort");     /* abnormal run unit termination (14.6.12) */
}

/* EC-SIZE (cobol ISSUES-55): with checking on for any of the conditions a
 * statement's arithmetic can meet, the statement is compiled as if it had
 * a SIZE ERROR phrase, and the phrase's place raises the condition libcob
 * saw -- EC-SIZE-ZERO-DIVIDE, -OVERFLOW (the 18-digit intermediate), or
 * -TRUNCATION (a result too large for its receiver), 14.7.5 */
static int ec_size_on(void)
{
    if (g_std < 2002) return 0;
    static const char *n[] = { "EC-SIZE-ZERO-DIVIDE", "EC-SIZE-OVERFLOW", "EC-SIZE-TRUNCATION" };
    for (int k = 0; k < 3; k++) if (g_ecs.on[ec_find(n[k], 0)]) return 1;
    return 0;
}

static void emit_ec_size(void)
{
    static const char *n[] = { "EC-SIZE-ZERO-DIVIDE", "EC-SIZE-OVERFLOW", "EC-SIZE-TRUNCATION" };
    int Ldone = new_label();
    emit_call("cob_size_kind");
    emit("\tadd r13, r1, r0");
    for (int k = 0; k < 3; k++) {
        int i = ec_find(n[k], 0);
        if (!g_ecs.on[i]) continue;
        int Lnext = new_label();
        emit_li("r2", k + 1);
        emit("\tbne r13, r2, .L%d", Lnext);
        emit_ec_raise(i);
        emit_jump(Ldone);
        emit_label(Lnext);
    }
    emit_label(Ldone);
}

static int ec_on_name(const char *name) { return g_std >= 2002 && g_ecs.on[ec_find(name, 0)]; }
/* an EC-I-O condition for one file: its TURN for that file, else for all */
static int ec_on_io(const char *name, int file) { return g_std >= 2002 && ec_on_file(ec_find(name, 0), file, NULL); }

/* EXCEPTION-LOCATION's string (2002 15.25.2 rule 2b), known here: the
 * program-name; the paragraph, OF its section, or the section; the line.
 * The line is implementor-defined: its number, and the copybook's name
 * before it when the statement came from one. */
static void ec_location(char *b, size_t n)
{
    int k = snprintf(b, n, "%s; ", g_progid_orig);
    Para *p = g_cur_para;
    if (p && !p->is_section && p->section > 0)
        k += snprintf(b + k, n - (size_t)k, "%s OF %s; ", p->oname, g_para[p->section - 1].oname);
    else if (p) k += snprintf(b + k, n - (size_t)k, "%s; ", p->oname);
    else k += snprintf(b + k, n - (size_t)k, "; ");
    const Tok *t = g_stmt_tok;
    if (t && t->file && g_ntok && g_tok[0].file && strcmp(t->file, g_tok[0].file)) {
        const char *base = strrchr(t->file, '/');
        snprintf(b + k, n - (size_t)k, "%s:%d", base ? base + 1 : t->file, t->line);
    } else snprintf(b + k, n - (size_t)k, "%d", t ? t->line : 0);
}

/* raise condition i here: the last exception status, the statement's
 * name when WITH LOCATION turned it on, then the declarative and fatality */
static void emit_ec_raise(int i)
{
    char nm[64]; snprintf(nm, sizeof nm, "%s", ec_name(i));
    emit_la("r3", lit_label((const unsigned char *)nm, (int)strlen(nm) + 1));
    int loc = g_ecs.loc[i];
    if (g_ec_fidx >= 0) ec_on_file(i, g_ec_fidx, &loc);
    if (loc && g_cur_stmt[0]) emit_la("r4", lit_label((const unsigned char *)g_cur_stmt, (int)strlen(g_cur_stmt) + 1));
    else emit_li("r4", 0);
    if (loc) {
        char loc[256]; ec_location(loc, sizeof loc);
        emit_la("r5", lit_label((const unsigned char *)loc, (int)strlen(loc) + 1));
    } else emit_li("r5", 0);
    if (g_ec_file) emit_la("r6", lit_label((const unsigned char *)g_ec_file, (int)strlen(g_ec_file) + 1));
    else emit_li("r6", 0);
    emit_call("cob_ec_raise");
    emit_ec_dispatch(i);
}

/* RAISE EXCEPTION exception-name (2023 14.9.29).  Everything is known here:
 * whether checking is on at this statement, the declarative that applies
 * (the name's own USE, its group's, EC-ALL's), and whether the condition
 * is fatal.  Checking off: the statement does nothing. */
static void parse_raise(void)
{
    int line = cur()->line;
    if (!accept_word("exception")) die_at(line, "RAISE of an exception object is object orientation, not implemented");
    if (cur()->kind != T_WORD) die_at(line, "RAISE EXCEPTION needs an exception-name");
    int i = ec_find(cur()->s, line);
    if (i < 0) die_at(line, "'%s' is not an exception-name", cur()->s);
    if (ec_level(i) != 3) die_at(line, "RAISE needs a level-3 exception-name, not %s", ec_name(i));
    if (g_ecp_handler) die_at(line, "RAISE in a WHEN or FINALLY phrase of an exception-checking PERFORM (2023 14.9.29.3 rule 4)");
    advance();
    if (!g_ecs.on[i]) return;
    emit_ec_raise(i);
}

static void parse_set(void)
{
    Ref rs[MAXOPS]; int nr = 0;
    if (g_std >= 2002 && at_word("last") && is_word(peek(1), "exception")) {
        /* SET LAST EXCEPTION TO OFF (2023 14.9.39): no exception condition exists */
        advance(); advance(); expect_word("to"); expect_word("off");
        emit_call("cob_ec_clear");
        return;
    }
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
    int raddr[MAXOPS], nptr = 0;
    while (at_operand()) {
        if (nr >= MAXOPS) die_at(cur()->line, "too many items in SET");
        raddr[nr] = 0;
        if (at_word("address") && is_word(peek(1), "of") && !sym_lookup_quiet("address")) {
            /* SET ADDRESS OF data-name (format 7): a based entry's implicit
             * pointer (2023 14.9.39.3 rule 18); a LINKAGE record's cell
             * likewise, as IBM and GnuCOBOL allow */
            int line = cur()->line;
            if (g_std < 2002) die_at(line, "ADDRESS OF is COBOL 2002; compile with -std=2002");
            advance(); advance();
            parse_ref(&rs[nr]);
            Sym *x = rs[nr].sym;
            if (rs[nr].nsub || rs[nr].rm || x->parent >= 0 || !(x->is_based || x->is_linkage))
                die_at(line, "SET ADDRESS OF '%s': it is a BASED entry, or a LINKAGE record at level 01 or 77 (2023 14.9.39.3 rule 18)", x->name);
            raddr[nr] = 1;
        } else parse_ref(&rs[nr]);
        if (raddr[nr] || (!rs[nr].sym->is_group && rs[nr].sym->usage == U_POINTER)) nptr++;
        nr++;
    }
    if (!nr) die_at(cur()->line, "SET needs an item");
    if (nptr && nptr != nr) die_at(rs[0].line, "SET: data-pointer receivers are not mixed with others");
    if (nptr && accept_word("to")) {
        /* format 7: the value once, then each receiver in order */
        Opnd v; parse_operand(&v);
        if (!opnd_is_ptr(&v))
            die_at(v.line, "SET of a data pointer takes ADDRESS OF, a pointer item or NULL (2023 14.9.39.3 rule 17)");
        emit_ptr_value(&v, "r1");
        emit("\tstw sp+%d, r1", SLOT_A);
        for (int i = 0; i < nr; i++) {
            if (raddr[i]) emit_la("r3", g_sym[rs[i].sym->record].label);
            else emit_ref_addr(&rs[i], "r3");
            emit("\tldw r1, sp+%d", SLOT_A);
            emit("\tstw r3+0, r1");
        }
        return;
    }
    if (nptr) {
        /* format 10: SET pointer UP|DOWN BY n, in bytes */
        int down = 0;
        if (accept_word("up")) down = 0; else if (accept_word("down")) down = 1;
        else die_at(cur()->line, "expected TO, UP BY or DOWN BY in SET");
        expect_word("by");
        Opnd v; parse_operand(&v); check_numeric_opnd(&v);
        for (int i = 0; i < nr; i++) {
            if (raddr[i]) die_at(rs[i].line, "SET ADDRESS OF ... UP or DOWN: set a pointer item instead (2023 14.9.39 format 10)");
            emit_push(&v); emit_call("cob_pop_int");
            emit("\tstw sp+%d, r1", SLOT_A);
            emit_ref_addr(&rs[i], "r3");
            emit("\tldw r2, r3+0");
            if (ec_on_name("EC-DATA-PTR-NULL")) {
                int Lok = new_label();
                emit("\tbne r2, r0, .L%d", Lok);
                emit_ec_raise(ec_find("EC-DATA-PTR-NULL", 0));
                emit_label(Lok);
                emit_ref_addr(&rs[i], "r3");
                emit("\tldw r2, r3+0");
            }
            emit("\tldw r1, sp+%d", SLOT_A);
            emit("\t%s r2, r2, r1", down ? "sub" : "add");
            emit("\tstw r3+0, r2");
        }
        return;
    }
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
        int hot = opnd_hot_int(&v) && ref_hot_store(&rs[i], down, ops_all_nonneg(ops, 1));
        int rd[1] = { 0 };
        if (hot) emit_hot_sum(ops, 1); else emit_push(&v);
        emit_store_receivers(&rs[i], rd, 1, hot, 0, down, 0, ops_sum_mag(ops, 1), ops_all_nonneg(ops, 1));
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
            int reversed = 0;
            if (accept_word("with")) { accept_word("no"); accept_word("rewind"); accept_word("lock"); }
            if (at_word("reversed")) bp(BP_O4_REVERSED, cur()->line);
            if (accept_word("reversed")) {          /* obsolete: read from the last record back (SQ303M, SQ401M) */
                if (mode != COB_OPEN_INPUT) die_at(cur()->line, "REVERSED goes with OPEN INPUT");
                reversed = 8;
            }
            emit_file_addr("r3", f); emit_li("r4", mode | reversed); emit_call("cob_open");
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
    int before = 0, after = 0, after_kw = 0; Opnd n; int dyn = 0, adv = 0;
    if (at_word("before") || at_word("after")) {
        adv = 1;
        after_kw = accept_word("after"); if (!after_kw) accept_word("before");
        accept_word("advancing");
        if (accept_word("page") || (cur()->kind == T_WORD && mnemonic_kind(cur()->s) == 3 && (advance(), 1))) {
            /* a form feed before (AFTER PAGE) or after (BEFORE PAGE) the record */
            if (after_kw) before = -1; else after = -1;
            accept_word("line"); accept_word("lines");
            goto advancing_done;
        }
        parse_operand(&n);
        if (n.kind == O_FIG && !strncmp(n.tok->s, "zero", 4)) { n.kind = O_NUM; numlit_zero(&n.num); }   /* ADVANCING ZERO (SQ101M) */
        if (n.kind == O_NUM) {
            long v = (long)numlit_int(&n.num);
            /* the runtime's counts are the newlines beyond the record's own; PAGE is -1, ZERO lines -2 (no advance at all) */
            if (f->linage) { if (after_kw) before = (int)v; else after = (int)v; }
            else if (after_kw) before = v ? (int)v - 1 : -2; else after = v ? (int)v - 1 : -2;
        }
        else if (n.kind == O_REF && is_int_item(n.ref.sym)) dyn = 1;
        else die_at(n.line, "ADVANCING needs an integer");
        accept_word("line"); accept_word("lines");
    }
advancing_done:;
    /* a BEFORE phrase on a print file (not LINAGE, which counts its own):
     * before = -3 marks it, so BEFORE 1 is not taken for AFTER 1 -- the
     * runtime's printer needs to know which side of the record the move
     * falls on (libcob.c; cobol ISSUES-46) */
    if (adv && !after_kw && !f->linage) before = -3;
    /* a file written WITH ADVANCING and no ORGANIZATION clause is a print
     * file: its records are lines (GnuCOBOL's "line advancing" file).  The
     * phrase decides, not its count: AFTER 1 is zero newlines beyond the
     * record's own, so testing the counts left a file written only AFTER 1
     * a plain sequential file with no line breaks at all (CCVS-85 NC113M;
     * cobol ISSUES-44) */
    if (adv && f->org == COB_ORG_SEQ && !f->org_given && !f->varying) f->org = COB_ORG_LINESEQ;
    /* (a LINAGE file took the line counts themselves above, not n-1: AFTER n
     * in r4, BEFORE n in r5, -1 for PAGE, 0/0 for no ADVANCING) */
    int keyed_org = f->org == COB_ORG_INDEXED || f->org == COB_ORG_RELATIVE;
    if (keyed_org && (before || after || dyn)) die_at(rec.line, "ADVANCING is not valid on an %s file", f->org == COB_ORG_INDEXED ? "INDEXED" : "RELATIVE");
    if (!keyed_org && at_word("invalid")) die_at(cur()->line, "INVALID KEY needs an INDEXED or RELATIVE file");
    if (dyn) {
        if (is_hot_int(n.ref.sym)) emit_hot_value(&n);
        else { Arg a[2] = { arg_ref(&n.ref), arg_desc(sym_desc(n.ref.sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
        /* the runtime's counts are n-1, and zero lines is -2: a zero in the
         * item must not become -1, which is PAGE (SQ101M's LONG-ZERO) */
        if (!f->linage) { emit("\tseq r2, r1, r0"); emit("\taddi r1, r1, -1"); emit("\tsub r1, r1, r2"); }
        emit("\tstw sp+%d, r1", SLOT_C);
        emit_file_addr("r3", f);
        if (after_kw) { emit("\tldw r4, sp+%d", SLOT_C); emit_li("r5", 0); }
        else { emit_li("r4", f->linage ? 0 : -3); emit("\tldw r5, sp+%d", SLOT_C); }
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

static void parse_string_1(void);
static void parse_string(void)
{
    parse_string_1();
}
static void parse_string_1(void)
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
    /* the receiver: not edited, not JUSTIFIED (X3.23 6.24.2); a group is alphanumeric */
    if (!dst.sym->is_group && (dst.sym->pi.category == PIC_NUMERIC || dst.sym->pi.edited || dst.sym->just))
        die_at(dst.line, "the STRING receiver must be an alphanumeric item, not edited or JUSTIFIED");
    if (dst.user_rm) die_at(dst.line, "the STRING receiver shall not be reference-modified (2023 14.9.43.3 rule 4; X3.23-1985 STRING syntax rule 3)");
    if (dst.sym->strong) die_at(dst.line, "a strongly-typed group is not a STRING receiver (2023 14.9.43.3 rule 6)");
    /* national operands (cobol ISSUES-69): characters of two bytes throughout */
    int nat = ref_is_national(&dst);
    static const char *srule = "14.9.43.3 rule 1";
    for (int i = 0; i < n; i++) { no_bits(&srcs[i], "STRING"); no_bits(&delims[i], "STRING"); }
    { Opnd dq; memset(&dq, 0, sizeof dq); dq.kind = O_REF; dq.ref = dst; dq.line = dst.line; no_bits(&dq, "STRING"); }
    for (int i = 0; i < n; i++) {
        nat_class_check(&srcs[i], nat, "STRING", srule);
        if (delims[i].kind != O_ALL) nat_class_check(&delims[i], nat, "STRING", srule);   /* O_ALL: SIZE */
    }
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
    emit_call(nat ? "cob_str_begin_nat" : "cob_str_begin");

    for (int i = 0; i < n; i++) {
        Arg a[4]; Arg dd;
        if (srcs[i].kind == O_FIG) fig_char_args(&srcs[i], nat, &a[0], &a[1]);   /* SPACE, ZERO, ...: one character */
        else if (srcs[i].kind == O_ALL) {
            a[0] = arg_label(lit_label((unsigned char *)srcs[i].tok->s, srcs[i].tok->len)); a[1] = arg_imm(srcs[i].tok->len);
        } else {
            opnd_args(&srcs[i], &a[0], &dd, 0, 0);
            a[1] = arg_len(&srcs[i]);
        }
        Opnd *d = &delims[i];
        if (d->kind == O_ALL) { a[2] = arg_imm(0); a[3] = arg_imm(0); }
        else if (d->kind == O_FIG) fig_char_args(d, nat, &a[2], &a[3]);
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
static void parse_unstring_1(void);
static void parse_unstring(void)
{
    parse_unstring_1();
}
static void parse_unstring_1(void)
{
    int line = cur()->line;
    Opnd src; parse_operand(&src);
    if (src.kind != O_REF) die_at(src.line, "UNSTRING needs a data item to take apart");
    if (g_std < 2002 && src.ref.user_rm)                /* 2023 dropped the rule */
        die_at(src.line, "the UNSTRING sending item shall not be reference-modified in COBOL 85 (X3.23-1985 UNSTRING syntax rule 7)");
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
        if (rcv[n].sym->strong)                   /* its category is its type (8.5.2.1) */
            die_at(rcv[n].line, "the strongly-typed group '%s' is not an UNSTRING receiver (2023 14.9.48.3 rule 4)", rcv[n].sym->name);
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
    /* national operands (cobol ISSUES-69): the source, the delimiters, the
     * receivers and DELIMITER IN items all national, or none */
    int nat = opnd_is_national(&src);
    static const char *urule = "14.9.48.3 rule 3";
    no_bits(&src, "UNSTRING");
    for (int i = 0; i < n; i++) { Opnd rq; memset(&rq, 0, sizeof rq); rq.kind = O_REF; rq.ref = rcv[i]; rq.line = rcv[i].line; no_bits(&rq, "UNSTRING"); }
    for (int i = 0; i < nd; i++) nat_class_check(&delims[i], nat, "UNSTRING", urule);
    for (int i = 0; i < n; i++) {
        Opnd ro; memset(&ro, 0, sizeof ro); ro.kind = O_REF; ro.ref = rcv[i]; ro.line = rcv[i].line;
        if (nat && is_numeric_sym(rcv[i].sym)) {
            if (rcv[i].sym->usage != U_NATIONAL)
                die_at(rcv[i].line, "UNSTRING: a numeric receiver of national data must be USAGE NATIONAL (2023 14.9.48.3 rule 4)");
        } else nat_class_check(&ro, nat, "UNSTRING", urule);
        if (nat && rcv[i].sym->pi.edited)
            die_at(rcv[i].line, "UNSTRING: a national-edited receiver is not allowed (2023 14.9.48.3 rule 4)");
        if (has_d[i]) { ro.ref = dlm[i]; ro.line = dlm[i].line; nat_class_check(&ro, nat, "UNSTRING", urule); }
    }

    /* begin: the source, its length, the pointer */
    if (has_ptr) {
        Arg a[2] = { arg_ref(&ptr), arg_desc(sym_desc(ptr.sym)) }; emit_args(a, 2); emit_call("cob_load_int");
        emit("\tstw sp+%d, r1", SLOT_C);
    }
    { Arg a[2], dd; opnd_args(&src, &a[0], &dd, 0, 0); a[1] = arg_len(&src); emit_args(a, 2); }
    if (has_ptr) emit("\tldw r5, sp+%d", SLOT_C); else emit_li("r5", 0);
    emit_call(nat ? "cob_unstr_begin_nat" : "cob_unstr_begin");
    for (int i = 0; i < nd; i++) {
        Arg a[3];
        if (delims[i].kind == O_FIG) fig_char_args(&delims[i], nat, &a[0], &a[1]);
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
            if (o->kind == O_ADDR) {
                /* the address, a word, BY VALUE; by reference or content,
                 * the unique data item ADDRESS OF creates (2023 8.4.3.11
                 * GR 1): a compiler-made pointer record holding it */
                if (mode == 2) { a[n++] = arg_value(o); continue; }
                FDesc fd; memset(&fd, 0, sizeof fd); fd.size = 4; fd.usage = U_POINTER; snprintf(fd.pic, sizeof fd.pic, "-");
                Sym *t = ftemp_new(&fd, o->line);
                Ref tr = ftemp_ref(t, o->line);
                emit_ptr_value(o, "r1");
                emit("\tstw sp+%d, r1", SLOT_A);
                emit_ref_addr(&tr, "r3");
                emit("\tldw r1, sp+%d", SLOT_A);
                emit("\tstw r3+0, r1");
                memset(o, 0, sizeof *o); o->kind = O_REF; o->ref = tr; o->line = tr.line;
                if (mode == 1) { a[n] = arg_content(o); ncontent++; } else a[n] = arg_ref(&o->ref);
                n++;
                continue;
            }
            if (mode == 1) {
                if (o->kind == O_REF && o->ref.sym->is_cond) die_at(o->line, "a condition-name cannot be passed");
                if (o->kind == O_REF && o->ref.rm && o->ref.rm_bit) die_at(o->line, "BY CONTENT of a reference-modified bit item is not implemented (its bits would need moving to a byte)");
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
                if (o->kind == O_REF) {
                    if (o->ref.sym->is_cond) die_at(o->line, "a condition-name cannot be passed");
                    if (sym_bitlike(o->ref.sym)) bit_arg_check(&o->ref);
                    a[n] = arg_ref(&o->ref);
                }
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
    /* EC-PROGRAM-NOT-FOUND (2023 14.9.4 general rule 3b; cobol ISSUES-59):
     * with checking on and no ON EXCEPTION phrase, the CALL resolves at run
     * time, and a missing program raises the condition (fatal) */
    int on_phrase = at_word("on") || at_word("exception") || at_word("overflow");
    int ecnf = !on_phrase && ec_on_name("EC-PROGRAM-NOT-FOUND");
    int Lcall = new_label(), Lafter = new_label();
    if (dynamic || has_clause || ecnf) {
        if (dynamic) { emit_ref_addr(&target, "r3"); emit_li("r4", target.sym->size); }
        else { emit_la("r3", lit_label((const unsigned char *)t->s, t->len)); emit_li("r4", t->len); }
        emit_li("r5", !has_clause && !ecnf);            /* no clause: the runtime stops on a missing program */
        emit_call("cob_resolve");
        emit("\tadd r12, r0, r1");                      /* callee-saved; the compiler uses no other of r12-r28 */
        if (has_clause || ecnf) {
            emit("\tbne r12, r0, .L%d", Lcall);
            if (ecnf) emit_ec_raise(ec_find("EC-PROGRAM-NOT-FOUND", 0));
            emit_li("r1", 1); emit("\tstw sp+%d, r1", SLOT_C);
            emit_jump(Lafter);
            emit_label(Lcall);
        }
    }
    if (ec_on_name("EC-PROGRAM-RECURSIVE-CALL")) {
        /* the called program active and not RECURSIVE (14.9.4 general rule
         * 3f): known here from its registered descriptor, before the call */
        int Lok = new_label();
        if (dynamic) { emit_ref_addr(&target, "r3"); emit_li("r4", target.sym->size); }
        else { emit_la("r3", lit_label((const unsigned char *)t->s, t->len)); emit_li("r4", t->len); }
        emit_call("cob_program_busy");
        emit("\tbeq r1, r0, .L%d", Lok);
        emit_ec_raise(ec_find("EC-PROGRAM-RECURSIVE-CALL", 0));
        emit_label(Lok);
    }
    emit_args(a, n);
    if (dynamic || has_clause || ecnf) emit("\tjalr r31, r12, 0");
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
    if (ecnf && !has_clause) emit_label(Lafter);    /* reached only past a raise that returned, which a fatal one never does */
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

/* a report field's columns: a national one's character positions (cobol ISSUES-92) */
static int rfield_cols(const RField *f) { return f->pi.category == PIC_NATIONAL ? f->pi.bytes / 2 : f->pi.bytes; }
static int rfield_is_nat(const RField *f) { return f->pi.category == PIC_NATIONAL || f->usage_nat; }

static int rfield_desc(RField *f)
{
    Desc d; memset(&d, 0, sizeof d);
    switch (f->pi.category) {
    case PIC_NATIONAL: d.cat = COB_NATIONAL; break;
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
    if (f->usage_nat) { d.usage = COB_U_NATIONAL; d.size = 2 * f->pi.bytes; }
    return desc_add(&d);
}

static void emit_report_group(Report *r, RGroup *g);

/* ---- resolution at first use (INITIATE/GENERATE/TERMINATE) ----------- */

static Sym *rw_ref_sym(int tp, int line)
{
    Ref rr; int save_tp = g_tp;
    g_tp = tp; parse_ref(&rr); g_tp = save_tp;
    if (rr.nsub || rr.rm) die_at(line, "a report control or SUM operand is a plain data-name");
    return rr.sym;
}

static int rw_ctl_level_of(Report *r, Sym *sym, int line)
{
    for (int i = 0; i < r->nctl; i++) if (r->ctl_sym[i] == sym_idx(sym)) return i + 1;
    die_at(line, "'%s' is not in RD %s's CONTROL clause", sym->name, r->name);
    return 0;
}

static int rw_sym_is_counter(Report *r, int symidx)
{
    for (int gi = 0; gi < r->ng; gi++)
        for (int li = 0; li < r->g[gi].nl; li++)
            for (int fi = 0; fi < r->g[gi].l[li].nf; fi++)
                if (r->g[gi].l[li].f[fi].ctr_sym == symidx) return 1;
    return 0;
}

static void rw_resolve(Report *r)
{
    if (r->resolved) return;
    r->resolved = 1;
    for (int gi = 0; gi < r->ng; gi++) {
        RGroup *g = &r->g[gi];
        g->ctl_level = -1;
        if (g->type == RG_CONTROL_HEADING || g->type == RG_CONTROL_FOOTING) {
            if (!g->ctl_tp) {
                if (!r->ctl_final) die_at(g->line, "TYPE CONTROL %s FINAL needs CONTROL FINAL in the RD", g->type == RG_CONTROL_FOOTING ? "FOOTING" : "HEADING");
                g->ctl_level = 0;
            } else g->ctl_level = rw_ctl_level_of(r, rw_ref_sym(g->ctl_tp, g->line), g->line);
        }
        for (int li = 0; li < g->nl; li++)
            for (int fi = 0; fi < g->l[li].nf; fi++) {
                RField *f = &g->l[li].f[fi];
                if (!f->has_sum) continue;
                for (int k = 0; k < f->nsum; k++) {
                    Sym *op = rw_ref_sym(f->sum_tp[k], f->line);
                    f->sum_sym[k] = sym_idx(op);
                    f->sum_is_ctr[k] = rw_sym_is_counter(r, f->sum_sym[k]);
                }
                for (int k = 0; k < f->nupon; k++) {
                    Tok *nt = &g_tok[f->upon_tp[k]];
                    int found = -1;
                    for (int gj = 0; gj < r->ng; gj++)
                        if (r->g[gj].type == RG_DETAIL && r->g[gj].name[0] && !strcmp(r->g[gj].name, nt->s)) found = gj;
                    if (found < 0) die_at(f->line, "UPON '%s' is not a DETAIL group of RD %s", nt->s, r->name);
                    f->upon_g[k] = found;
                }
                if (f->reset_final) f->reset_lvl = 0;
                else if (f->reset_tp) f->reset_lvl = rw_ctl_level_of(r, rw_ref_sym(f->reset_tp, f->line), f->line);
                else f->reset_lvl = g->ctl_level;   /* its own footing's level (FINAL = 0) */
            }
    }
}

/* ---- small emission helpers ------------------------------------------- */

static void emit_rw_ldw(Report *r, int off, const char *reg)
{
    emit_report_addr("r1", r);
    emit("\tldw %s, r1+%d", reg, off);
}

static void emit_rw_stw_imm(Report *r, int off, int v)
{
    emit_report_addr("r1", r);
    emit_li("r2", v);
    emit("\tstw r1+%d, r2", off);
}

static void emit_rw_move_sym(int from, int to)
{
    emit_item_addr("r3", &g_sym[from], g_sym[from].offset);
    emit_desc_addr("r4", sym_desc(&g_sym[from]));
    emit_item_addr("r5", &g_sym[to], g_sym[to].offset);
    emit_desc_addr("r6", sym_desc(&g_sym[to]));
    emit_call("cob_move");
}

/* counter += source (both plain items): through the numeric stack */
static void emit_rw_add_into(int src, int ctr)
{
    emit_item_addr("r3", &g_sym[src], g_sym[src].offset);
    emit_desc_addr("r4", sym_desc(&g_sym[src]));
    emit_call("cob_push");
    emit_item_addr("r3", &g_sym[ctr], g_sym[ctr].offset);
    emit_desc_addr("r4", sym_desc(&g_sym[ctr]));
    emit_li("r5", 0);
    emit_call("cob_top_addto");
    emit_call("cob_drop");
}

static void emit_rw_zero(int ctr)
{
    emit_li("r3", 0); emit_li("r4", 0); emit_li("r5", 0);
    emit_call("cob_push_lit");
    emit_item_addr("r3", &g_sym[ctr], g_sym[ctr].offset);
    emit_desc_addr("r4", sym_desc(&g_sym[ctr]));
    emit_li("r5", 0);
    emit_call("cob_top_store");
    emit_call("cob_drop");
}

/* the counter arithmetic a control level L owes when it breaks: every
 * counter summing a counter that resets at L takes its value (rolling
 * forward, crossfooting), in the order the entries stand; then, after
 * the footing presents, the counters resetting at L go to zero */
static void emit_rw_rolls(Report *r, int level)
{
    for (int gi = 0; gi < r->ng; gi++)
        for (int li = 0; li < r->g[gi].nl; li++)
            for (int fi = 0; fi < r->g[gi].l[li].nf; fi++) {
                RField *f = &r->g[gi].l[li].f[fi];
                if (!f->has_sum) continue;
                for (int k = 0; k < f->nsum; k++)
                    if (f->sum_is_ctr[k]) {
                        RField *sf = NULL;
                        for (int gj = 0; gj < r->ng && !sf; gj++)
                            for (int lj = 0; lj < r->g[gj].nl && !sf; lj++)
                                for (int fj = 0; fj < r->g[gj].l[lj].nf; fj++)
                                    if (r->g[gj].l[lj].f[fj].ctr_sym == f->sum_sym[k]) { sf = &r->g[gj].l[lj].f[fj]; break; }
                        if (sf && sf->reset_lvl == level) emit_rw_add_into(f->sum_sym[k], f->ctr_sym);
                    }
            }
}

static void emit_rw_resets(Report *r, int level)
{
    for (int gi = 0; gi < r->ng; gi++)
        for (int li = 0; li < r->g[gi].nl; li++)
            for (int fi = 0; fi < r->g[gi].l[li].nf; fi++) {
                RField *f = &r->g[gi].l[li].f[fi];
                if (f->has_sum && f->reset_lvl == level) emit_rw_zero(f->ctr_sym);
            }
}

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
 * would pass its bound spills onto a new page first.  The body groups
 * are DETAIL, CONTROL HEADING and CONTROL FOOTING (X3.23 VIII); a
 * CONTROL FOOTING's bound is the RD FOOTING line, the others' LAST
 * DETAIL -- the runtime reads the kind from is_body (1 or 2). */
static void emit_report_group(Report *r, RGroup *g)
{
    int is_body = g->type == RG_DETAIL || g->type == RG_CONTROL_HEADING ? 1
                : g->type == RG_CONTROL_FOOTING ? 2 : 0;
    int Lsupp = -1;
    if (g->type == RG_REPORT_FOOTING && g->nl) {
        emit_report_addr("r3", r);
        emit_li("r4", g->l[0].abs); emit_li("r5", g->l[0].plus);
        emit_call("cob_rw_rf_begin");
    }
    if (g->use_sec >= 0) {
        int Lret = new_label();
        char lab[32]; snprintf(lab, sizeof lab, ".L%d", Lret);
        emit_li("r3", g->use_sec);
        emit_la("r4", lab);
        emit_call("cob_perform_push");
        emit("\tjal r0, .Lp%d_%d", g_unit, g->use_sec);
        emit_label(Lret);
        Lsupp = new_label();
        emit_rw_ldw(r, RW_OFF_SUPPRESS, "r2");
        int Lrender = new_label();
        emit("\tbeq r2, r0, .L%d", Lrender);
        emit_rw_stw_imm(r, RW_OFF_SUPPRESS, 0);
        emit_jump(Lsupp);
        emit_label(Lrender);
    }
    for (int i = 0; i < g->nl; i++) {
        RLine *ln = &g->l[i];
        if (ln->np && is_body) {                    /* LINE ... NEXT PAGE */
            emit_page_advance(r);
        }
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
            int Lgi = -1;
            if (f->gi && g->type == RG_DETAIL) {    /* GROUP INDICATE: spaces except first after a page or break */
                Lgi = new_label();
                emit_rw_ldw(r, RW_OFF_GI, "r2");
                emit("\tandi r2, r2, %d", 1 << (int)(g - r->g));
                emit("\tbeq r2, r0, .L%d", Lgi);
            }
            Arg a[4];
            a[0] = arg_imm(f->column);
            a[1] = arg_desc(rfield_desc(f));
            if (f->ctr_sym) {                       /* a SUM entry prints its counter */
                Ref *rf = xmalloc(sizeof *rf);
                memset(rf, 0, sizeof *rf);
                rf->sym = &g_sym[f->ctr_sym]; rf->line = f->line;
                a[2] = arg_ref(rf); a[3] = arg_desc(sym_desc(rf->sym));
            } else if (f->has_source) {
                Ref *rf = xmalloc(sizeof *rf);
                int save_tp = g_tp;
                g_tp = f->source_tp; parse_ref(rf); g_tp = save_tp;
                if (rf->sym->is_cond) die_at(f->line, "SOURCE '%s' is a condition-name", rf->sym->name);
                if (sym_is_national(rf->sym) && f->pi.category != PIC_NATIONAL)
                    die_at(f->line, "SOURCE '%s' is national: it goes to a national field (PICTURE N), not this one (2023 14.9.25.3 rule 3)", rf->sym->name);
                a[2] = arg_ref(rf); a[3] = arg_desc(sym_desc(rf->sym));
            } else if (f->value->kind == T_STR) {
                a[2] = arg_label(lit_label((unsigned char *)f->value->s, f->value->len));
                a[3] = arg_desc(f->value->nat ? nat_desc(f->value->len) : str_desc(f->value->len));
            } else {
                NumLit n; numlit_parse(f->value, &n);
                int d; a[2] = arg_label(num_lit_label(&n, &d)); a[3] = arg_desc(d);
            }
            emit_args(a, 4);
            emit_call("cob_rw_field");
            if (Lgi >= 0) emit_label(Lgi);
        }
        emit_report_addr("r3", r);
        emit_li("r4", is_body);
        emit_call("cob_rw_line_write");
    }
    if (g->type == RG_DETAIL) {
        int gi_any = 0;
        for (int i = 0; i < g->nl; i++) for (int k = 0; k < g->l[i].nf; k++) if (g->l[i].f[k].gi) gi_any = 1;
        if (gi_any) {                               /* this group's GROUP INDICATE fields wait for the next page or break */
            emit_report_addr("r1", r);
            emit("\tldw r2, r1+%d", RW_OFF_GI);
            emit_li("r3", ~(1 << (int)(g - r->g)));
            emit("\tand r2, r2, r3");
            emit("\tstw r1+%d, r2", RW_OFF_GI);
        }
    }
    if (g->next_kind) {
        int Lskip = -1;
        if (g->type == RG_CONTROL_FOOTING) {        /* NEXT GROUP on a CF applies only at its own break level (VIII 2.15.4(3)) */
            Lskip = new_label();
            emit_rw_ldw(r, RW_OFF_BRK, "r2");
            emit_li("r3", g->ctl_level);
            emit("\tbne r2, r3, .L%d", Lskip);
        }
        emit_report_addr("r3", r);
        emit_li("r4", g->next_kind);
        emit_li("r5", g->next_n);
        emit_call("cob_rw_next_group");
        if (Lskip >= 0) emit_label(Lskip);
    }
    if (Lsupp >= 0) emit_label(Lsupp);
}

/* a CODE on one report of a file is on each report of it (X3.23-1985 XIII
 * 3.6.3 rule 2; 2023 13.18.12.3 rule 3) */
static void rw_check_code(Report *r)
{
    int any = 0, all = 1;
    for (int i = g_report_base; i < g_nreport; i++) {
        if (g_reports[i].file != r->file) continue;
        int has = g_reports[i].code_lit || g_reports[i].code_tp;
        any |= has; all &= has;
    }
    if (any && !all) die_at(r->line, "CODE is on one report of the file '%s' but not on each (2023 13.18.12.3 rule 3)", g_files[r->file].name);
}

/* a report's CODE for the runtime: the literal once, at INITIATE; an
 * identifier's value at each body group's start (GENERATE) */
static void emit_rw_code(Report *r)
{
    if (r->code_lit) {
        Arg a[3] = { arg_imm(0), arg_label(lit_label((unsigned char *)r->code_lit->s, r->code_lit->len)), arg_imm(r->code_lit->len) };
        emit_args(a + 1, 2);
        emit("\tadd r5, r4, r0"); emit("\tadd r4, r3, r0");
        emit_report_addr("r3", r);
        emit_call("cob_rw_code");
    } else if (r->code_tp) {
        int save = g_tp; g_tp = r->code_tp;
        Ref cr; parse_ref(&cr);
        g_tp = save;
        if (cr.sym->is_group || (cr.sym->pi.category != PIC_ALPHANUMERIC))
            die_at(cr.line, "CODE: '%s' is not an alphanumeric data item (2023 13.18.12.3 rule 2)", cr.sym->name);
        Arg a[2] = { arg_ref(&cr), arg_imm(cr.sym->size) };
        emit_args(a, 2);
        emit("\tadd r5, r4, r0"); emit("\tadd r4, r3, r0");
        emit_report_addr("r3", r);
        emit_call("cob_rw_code");
    }
}

/* no GENERATE, INITIATE or TERMINATE in a USE BEFORE REPORTING procedure
 * (X3.23-1985 USE rule 7; 2023 14.9.49.3 rule 10) */
static void rw_not_in_use(const char *verb, int line)
{
    if (!g_in_decl) return;
    for (int i = 0; i < g_nrwuse; i++)
        if (g_rwuse[i].unit == g_unit && g_rwuse[i].sec == g_cur_sec_id)
            die_at(line, "%s in a USE BEFORE REPORTING procedure (2023 14.9.49.3 rule 10)", verb);
}

static void parse_initiate(void)
{
    rw_not_in_use("INITIATE", cur()->line);
    /* INITIATE report-name ... (X3.23-1985 XIII 4.2) */
    do {
        Report *r = expect_report();
        rw_check_code(r);
        emit_report_addr("r3", r);
        emit_call("cob_rw_initiate");
        if (r->code_lit) emit_rw_code(r);
    } while (cur()->kind == T_WORD && report_find(cur()->s));
}

/* the counters a GENERATE subtotals: plain (non-counter) sources, the
 * UPON list honoured -- at GENERATE report-name only unrestricted
 * counters take their sources (X3.23 VIII 2.21.4(11)) */
static void emit_rw_subtotals(Report *r, RGroup *det)
{
    for (int gi = 0; gi < r->ng; gi++)
        for (int li = 0; li < r->g[gi].nl; li++)
            for (int fi = 0; fi < r->g[gi].l[li].nf; fi++) {
                RField *f = &r->g[gi].l[li].f[fi];
                if (!f->has_sum) continue;
                if (f->nupon) {
                    int hit = 0;
                    for (int k = 0; k < f->nupon; k++) if (det && &r->g[f->upon_g[k]] == det) hit = 1;
                    if (!hit) continue;
                }
                for (int k = 0; k < f->nsum; k++)
                    if (!f->sum_is_ctr[k]) emit_rw_add_into(f->sum_sym[k], f->ctr_sym);
            }
}

/* the control footing sequence for every level from the most minor up
 * to `to_level` (1 = most major, 0 = FINAL too): the rolls, the group,
 * the resets -- a level with no footing group still rolls and resets */
static void emit_rw_cf_level(Report *r, int L)
{
    emit_rw_rolls(r, L);
    for (int k = 0; k < r->ng; k++)
        if (r->g[k].type == RG_CONTROL_FOOTING && r->g[k].ctl_level == L) emit_report_group(r, &r->g[k]);
    emit_rw_resets(r, L);
}

static void emit_rw_generate(Report *r, RGroup *det)
{
    rw_resolve(r);
    int Lsense = new_label(), Lbody = new_label();
    emit_rw_ldw(r, RW_OFF_FIRST_GEN, "r2");
    emit("\tbne r2, r0, .L%d", Lsense);

    /* the first GENERATE: the controls remembered, the REPORT HEADING,
     * the first page, CONTROL HEADINGs FINAL then major to minor */
    for (int L = 0; L < r->nctl; L++) emit_rw_move_sym(r->ctl_sym[L], r->ctl_clone[L]);
    emit_rw_stw_imm(r, RW_OFF_FIRST_GEN, 1);
    for (int k = 0; k < r->ng; k++) if (r->g[k].type == RG_REPORT_HEADING) emit_report_group(r, &r->g[k]);
    {
        int Lpg = new_label();
        emit_rw_ldw(r, 52, "r2");                   /* next_page: an RH that kept the page to itself */
        emit("\tbne r2, r0, .L%d", Lpg);
        emit_report_addr("r3", r);
        emit_call("cob_rw_first_page");
        for (int k = 0; k < r->ng; k++) if (r->g[k].type == RG_PAGE_HEADING) emit_report_group(r, &r->g[k]);
        emit_label(Lpg);
    }
    for (int L = 0; L <= r->nctl; L++)
        for (int k = 0; k < r->ng; k++)
            if (r->g[k].type == RG_CONTROL_HEADING && r->g[k].ctl_level == L) emit_report_group(r, &r->g[k]);
    emit_jump(Lbody);

    emit_label(Lsense);
    if (r->nctl) {
        /* sense: the most major control whose value moved */
        int Lfound = new_label();
        emit_rw_stw_imm(r, RW_OFF_BRK, 0);
        for (int L = 1; L <= r->nctl; L++) {
            Sym *it = &g_sym[r->ctl_sym[L - 1]], *cl = &g_sym[r->ctl_clone[L - 1]];
            emit_item_addr("r3", it, it->offset); emit_desc_addr("r4", sym_desc(it));
            emit_item_addr("r5", cl, cl->offset); emit_desc_addr("r6", sym_desc(cl));
            emit_call("cob_cmp");
            int Lnx = new_label();
            emit("\tbeq r1, r0, .L%d", Lnx);
            emit_report_addr("r1", r); emit_li("r2", L);
            emit("\tstw r1+%d, r2", RW_OFF_BRK);
            emit_jump(Lfound);
            emit_label(Lnx);
        }
        emit_jump(Lbody);
        emit_label(Lfound);
        /* CONTROL FOOTINGs, most minor up to the break level.  During
         * them the control items themselves hold the prior values (VIII
         * 2.21.4(13)): the new values wait aside, the clones move in --
         * so a USE BEFORE REPORTING procedure sees what the footing sees */
        for (int L = 0; L < r->nctl; L++) {
            emit_rw_move_sym(r->ctl_sym[L], r->ctl_held[L]);
            emit_rw_move_sym(r->ctl_clone[L], r->ctl_sym[L]);
        }
        for (int L = r->nctl; L >= 1; L--) {
            int Lskip = new_label();
            emit_rw_ldw(r, RW_OFF_BRK, "r2");
            emit_li("r3", L);
            emit("\tblt r3, r2, .L%d", Lskip);      /* L < brk: this level did not break */
            emit_rw_cf_level(r, L);
            emit_label(Lskip);
        }
        for (int L = 0; L < r->nctl; L++) {
            emit_rw_move_sym(r->ctl_held[L], r->ctl_sym[L]);
            emit_rw_move_sym(r->ctl_sym[L], r->ctl_clone[L]);
        }
        emit_report_addr("r1", r);
        emit_li("r2", -1);
        emit("\tstw r1+%d, r2", RW_OFF_GI);
        for (int L = 1; L <= r->nctl; L++) {
            int Lskip = new_label();
            emit_rw_ldw(r, RW_OFF_BRK, "r2");
            emit_li("r3", L);
            emit("\tblt r3, r2, .L%d", Lskip);
            for (int k = 0; k < r->ng; k++)
                if (r->g[k].type == RG_CONTROL_HEADING && r->g[k].ctl_level == L) emit_report_group(r, &r->g[k]);
            emit_label(Lskip);
        }
    }
    emit_label(Lbody);
    emit_rw_subtotals(r, det);
    if (det) {
        int last_printing = 0;
        for (int i = 0; i < det->nl; i++) if (det->l[i].nf) last_printing = i;
        int height = 0;
        for (int i = 1; i <= last_printing; i++) height += det->l[i].plus;
        emit_report_addr("r3", r);
        emit_li("r4", det->l[0].abs); emit_li("r5", det->l[0].plus); emit_li("r6", height);
        emit_call("cob_rw_fit");
        int Lfits = new_label();
        emit("\tbeq r1, r0, .L%d", Lfits);
        emit_page_advance(r);
        emit_label(Lfits);
        emit_report_group(r, det);
    }
}

static void parse_terminate_1(Report *r);
static void parse_terminate(void)
{
    rw_not_in_use("TERMINATE", cur()->line);
    /* TERMINATE report-name ... (X3.23-1985 XIII 4.4) */
    do parse_terminate_1(expect_report());
    while (cur()->kind == T_WORD && report_find(cur()->s));
}
static void parse_terminate_1(Report *r)
{
    rw_resolve(r);
    int Lend = new_label();
    emit_rw_ldw(r, RW_OFF_FIRST_GEN, "r2");
    emit("\tbeq r2, r0, .L%d", Lend);              /* no GENERATE ran: TERMINATE presents nothing */
    /* a break in the most major control, FINAL included, then the
     * REPORT FOOTING; the footings read the last GENERATE's values */
    emit_rw_stw_imm(r, RW_OFF_BRK, 1);
    for (int L = 0; L < r->nctl; L++) {         /* the footings read the last GENERATE's values */
        emit_rw_move_sym(r->ctl_sym[L], r->ctl_held[L]);
        emit_rw_move_sym(r->ctl_clone[L], r->ctl_sym[L]);
    }
    for (int L = r->nctl; L >= 1; L--) emit_rw_cf_level(r, L);
    if (r->ctl_final || r->nctl == 0) emit_rw_cf_level(r, 0);
    emit_page_footing(r);                       /* the page's last group, before the REPORT FOOTING (VIII 3.4.4) */
    for (int k = 0; k < r->ng; k++) if (r->g[k].type == RG_REPORT_FOOTING) emit_report_group(r, &r->g[k]);
    for (int L = 0; L < r->nctl; L++) emit_rw_move_sym(r->ctl_held[L], r->ctl_sym[L]);
    emit_report_addr("r3", r);
    emit_call("cob_rw_terminate");
    emit_rw_stw_imm(r, RW_OFF_FIRST_GEN, 0);
    emit_label(Lend);
}

static void parse_generate(void)
{
    rw_not_in_use("GENERATE", cur()->line);
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a report group after GENERATE");
    Report *r = NULL; RGroup *g = NULL;
    for (int i = g_report_base; i < g_nreport && !g; i++)
        for (int k = 0; k < g_reports[i].ng; k++)
            if (g_reports[i].g[k].name[0] && !strcmp(g_reports[i].g[k].name, t->s)) { r = &g_reports[i]; g = &g_reports[i].g[k]; break; }
    if (!g) {
        r = report_find(t->s);
        if (!r) die_at(t->line, "'%s' is not a report group", t->s);
        advance();
        if (r->code_tp) emit_rw_code(r);
        emit_rw_generate(r, NULL);                  /* GENERATE report-name: summary reporting */
        return;
    }
    if (g->type != RG_DETAIL) die_at(t->line, "GENERATE needs a DETAIL group or the report-name");
    advance();
    if (r->code_tp) emit_rw_code(r);
    emit_rw_generate(r, g);
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

static int g_insp_nat;                  /* the inspected item is national (cobol ISSUES-68) */

/* a pattern operand: address and length as Args.  Beside a national item
 * every operand is national, and a figurative constant is one national
 * character (2023 14.9.22.3 rules 3 and 4) */
static void pattern_args(Opnd *o, Arg *addr, Arg *len)
{
    if (o->kind == O_FIG && g_insp_nat) {
        unsigned u = nat_fig(o->tok->s); unsigned char two[2] = { (unsigned char)(u >> 8), (unsigned char)u };
        *addr = arg_label(lit_label(two, 2)); *len = arg_imm(2); return;
    }
    if (o->kind == O_FIG) { unsigned char c = (unsigned char)fig_byte(o->tok->s); *addr = arg_label(lit_label(&c, 1)); *len = arg_imm(1); return; }
    if (opnd_is_national(o) != g_insp_nat)
        die_at(o->line, g_insp_nat ? "INSPECT of a national item: every operand must be national (2023 14.9.22.3 rule 4)"
                                   : "INSPECT of an item that is not national: a national operand is not allowed (2023 14.9.22.3 rule 4)");
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

static void parse_inspect_1(void);
static void parse_inspect(void)
{
    parse_inspect_1(); g_insp_nat = 0;
}
static void parse_inspect_1(void)
{
    Ref item; parse_ref(&item);
    if (item.sym->is_cond) die_at(item.line, "INSPECT of a condition-name");
    /* a numeric USAGE NATIONAL item's characters are national too */
    { Opnd io; memset(&io, 0, sizeof io); io.kind = O_REF; io.ref = item; io.line = item.line; no_bits(&io, "INSPECT"); }
    if (item.sym->strong) die_at(item.line, "INSPECT of a strongly-typed group (2023 14.9.22.3 rule 1)");
    g_insp_nat = sym_is_national(item.sym) || (!item.sym->is_group && item.sym->usage == U_NATIONAL);
    int w = g_insp_nat ? 2 : 1;             /* a character's bytes */
    Opnd itemo = ref_opnd(&item);
    operand_odo_length(&itemo);             /* a group over an ODO table is inspected at its current length */
    /* the phrases are registered with the runtime, which makes the one pass
     * the text describes (cob_inspect_run); then each tally is added.  A
     * statement with both TALLYING and REPLACING is two statements, the
     * tallying pass first (X3.23 general rule): two begin/run rounds. */
    { Arg a[3] = { arg_ref(&itemo.ref), arg_len(&itemo), itemo.ref.rm ? (g_insp_nat ? arg_desc(nat_desc(2)) : arg_imm(0)) : arg_desc(sym_desc(item.sym)) }; emit_args(a, 3); emit_call("cob_inspect_begin"); }
    Ref tallies[32]; int tally_ph[32], nt = 0, np = 0, any = 0;
    if (accept_word("converting")) {
        Opnd from, to; parse_operand(&from); expect_word("to"); parse_operand(&to);
        int fl = from.kind == O_FIG ? w : opnd_size(&from), tl = to.kind == O_FIG ? w : opnd_size(&to);
        if (fl > 0 && tl > 0 && fl != tl && to.kind != O_FIG) die_at(to.line, "INSPECT CONVERTING: the two operands must be the same length");
        parse_inspect_range();
        Arg a[3], x;
        if (to.kind == O_FIG && fl > w) {
            /* CONVERTING "abc" TO SPACE: the figurative is as long as the other */
            unsigned char *f = xmalloc((size_t)fl);
            if (g_insp_nat) { unsigned u = nat_fig(to.tok->s); for (int i = 0; i + 1 < fl; i += 2) { f[i] = (unsigned char)(u >> 8); f[i + 1] = (unsigned char)u; } }
            else memset(f, fig_byte(to.tok->s), (size_t)fl);
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
        Arg a[3] = { arg_ref(&itemo.ref), arg_len(&itemo), itemo.ref.rm ? (g_insp_nat ? arg_desc(nat_desc(2)) : arg_imm(0)) : arg_desc(sym_desc(item.sym)) }; emit_args(a, 3); emit_call("cob_inspect_begin");
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
                    int pl = pat.kind == O_FIG ? w : opnd_size(&pat), rl = rep.kind == O_FIG ? w : opnd_size(&rep);
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

/* INITIALIZE ... REPLACING category DATA BY value: every elementary item
 * of that category below the receiver (index items, condition-names,
 * REDEFINES items and elementary FILLERs left alone, X3.23 6.16) takes
 * the value by the MOVE rules -- every occurrence of a table, the
 * receiver's own subscripts leading, the rest unrolled at compile time */
static void init_replace_walk(Sym *s, const Ref *base, int cat, Opnd *value, long *sub, int nsub, int line, int bits_only)
{
    if (s->is_cond || s->is_index || s->redefines >= 0) return;
    if (s->is_group) {
        for (int c = s->child; c >= 0; c = g_sym[c].sibling) {
            Sym *k = &g_sym[c];
            if (k->occurs) {
                /* one more dimension: every occurrence (a bit array's
                 * elements too, their bits found by ref_resolve_bits) */
                if (nsub >= MAXDIM) die_at(line, "INITIALIZE REPLACING: too many dimensions");
                for (long i = 1; i <= k->occurs; i++) { sub[nsub] = i; init_replace_walk(k, base, cat, value, sub, nsub + 1, line, bits_only); }
            } else init_replace_walk(k, base, cat, value, sub, nsub, line, bits_only);
        }
        return;
    }
    if (s->is_filler || s->pi.category != cat) return;
    if (bits_only && s->usage != U_BIT) return;
    Ref r = *base; r.sym = s; r.nsub = nsub; r.rm = 0; r.user_rm = 0; r.rm_bit = 0; r.bitsub = 0;
    for (int i = 0; i < nsub; i++) { if (i < base->nsub) r.sub[i] = base->sub[i]; else { r.sub[i].sym = NULL; r.sub[i].lit = sub[i]; r.sub[i].adj = 0; } }
    if (nsub != s->ndims) die_at(line, "INITIALIZE REPLACING: '%s' needs %d subscripts", s->name, s->ndims);
    ref_resolve_bits(&r);                       /* a bit array's element: its own bits (cobol ISSUES-94 B2) */
    emit_move(value, &r);
}

/* the bytes INITIALIZE sets from the template: those of the elementary
 * items it initializes -- not FILLERs, index items, or REDEFINES items
 * and their subordinates (X3.23 6.16; the item a REDEFINES redefines is
 * initialized, cobol ISSUES-94), every occurrence.  Bit items share bytes
 * with their neighbours and are set by MOVE instead (ISSUES-94 B1). */
static void init_cover(Sym *s, int top_off, int disp, unsigned char *cover, int limit, int is_top)
{
    if (s->is_cond || s->is_index) return;
    if (!is_top && s->redefines >= 0) return;
    if (s->bitgroup || (!s->is_group && s->usage == U_BIT)) return;
    int reps = (!is_top && s->occurs) ? s->occurs : 1;
    for (int i = 0; i < reps; i++) {
        int d = disp + i * s->size;
        if (s->is_group) { for (int c = s->child; c >= 0; c = g_sym[c].sibling) init_cover(&g_sym[c], top_off, d, cover, limit, 0); }
        else if (!s->is_filler) { int a = s->offset - top_off + d; for (int k = a; k < a + s->size && k < limit; k++) if (k >= 0) cover[k] = 1; }
    }
}

static void parse_initialize(void)
{
    Ref rs[MAXOPS]; int n = 0;
    static Tok tok_zero = { T_WORD, 0, "zero", 4, NULL, 0, 0, 0, 0, 0, 0 };
    static Tok tok_space = { T_WORD, 0, "spaces", 6, NULL, 0, 0, 0, 0, 0, 0 };
    Opnd fig_zero, fig_space; memset(&fig_zero, 0, sizeof fig_zero); memset(&fig_space, 0, sizeof fig_space);
    fig_zero.kind = O_FIG; fig_zero.tok = &tok_zero; fig_space.kind = O_FIG; fig_space.tok = &tok_space;
    while (at_operand()) {
        if (n >= MAXOPS) die_at(cur()->line, "too many items in INITIALIZE");
        Ref *r = &rs[n]; parse_ref(r);
        if (r->sym->is_cond) die_at(r->line, "INITIALIZE of a condition-name");
        n++;
    }
    if (!n) die_at(cur()->line, "INITIALIZE needs an item");
    if (!at_word("replacing")) {
        /* no REPLACING: every elementary item to its category's default --
         * the template image copied in runs around the bytes left alone,
         * then the edited items by MOVE (ZERO or SPACES through the edit) */
        for (int i = 0; i < n; i++) {
            Ref *r = &rs[i]; Sym *t = r->sym;
            if (r->user_rm) {
                /* a reference-modified item is an elementary item of its
                 * part's category: alphanumeric (national, boolean), set to
                 * spaces (national spaces, zeros) (X3.23 6.16; 2023 8.4.2.4;
                 * cobol ISSUES-94) */
                emit_move(r->rm_bit || t->pi.category == PIC_BOOLEAN ? &fig_zero : &fig_space, r);
                continue;
            }
            if (r->rm) { emit_move(&fig_zero, r); continue; }      /* a bit array's element */
            Sym tmp; memset(&tmp, 0, sizeof tmp);
            tmp.image = xmalloc(t->size); tmp.image_size = t->size;
            g_no_values = 1;
            init_one(&tmp, sym_idx(t), 0, 1);
            g_no_values = 0;
            unsigned char *cover = xmalloc((size_t)t->size + 1); memset(cover, 0, (size_t)t->size + 1);
            init_cover(t, t->offset, 0, cover, t->size, 1);
            for (int a = 0; a < t->size; ) {
                if (!cover[a]) { a++; continue; }
                int b = a; while (b < t->size && cover[b]) b++;
                Ref part = *r;
                if (a) { part.rm = 1; part.rm_start = a + 1; part.rm_len = b - a; part.rm_l0 = -1; part.rm_nat = 0; }   /* bytes */
                Arg args[3] = { arg_ref(&part), arg_label(lit_label(tmp.image + a, b - a)), arg_imm(b - a) };
                emit_args(args, 3);
                emit_call("memcpy");
                a = b;
            }
            free(cover); free(tmp.image);
            long sub[MAXDIM];
            if (t->is_group) {
                init_replace_walk(t, r, PIC_NUMERIC_EDITED, &fig_zero, sub, r->nsub, r->line, 0);
                init_replace_walk(t, r, PIC_ALPHANUMERIC_EDITED, &fig_space, sub, r->nsub, r->line, 0);
                init_replace_walk(t, r, PIC_BOOLEAN, &fig_zero, sub, r->nsub, r->line, 1);   /* bit items, a MOVE each */
            } else if (t->pi.category == PIC_NUMERIC_EDITED) emit_move(&fig_zero, r);
            else if (t->pi.category == PIC_ALPHANUMERIC_EDITED) emit_move(&fig_space, r);
            else if (t->usage == U_BIT) emit_move(&fig_zero, r);
        }
    }
    if (accept_word("replacing")) {
        for (;;) {
            int line = cur()->line, cat;
            if (accept_word("alphabetic")) cat = PIC_ALPHABETIC;
            else if (accept_word("alphanumeric")) cat = PIC_ALPHANUMERIC;
            else if (accept_word("numeric")) cat = PIC_NUMERIC;
            else if (accept_word("alphanumeric-edited")) cat = PIC_ALPHANUMERIC_EDITED;
            else if (accept_word("numeric-edited")) cat = PIC_NUMERIC_EDITED;
            else if (g_std >= 2002 && accept_word("national")) cat = PIC_NATIONAL;
            else if (g_std >= 2002 && accept_word("boolean")) cat = PIC_BOOLEAN;
            else die_at(line, "INITIALIZE REPLACING: expected ALPHABETIC, ALPHANUMERIC, NUMERIC, ALPHANUMERIC-EDITED, NUMERIC-EDITED%s",
                        g_std >= 2002 ? " or NATIONAL" : "");
            accept_word("data"); expect_word("by");
            Opnd value; parse_operand(&value);
            if (value.kind != O_REF && value.kind != O_STR && value.kind != O_NUM && value.kind != O_FIG)
                die_at(line, "INITIALIZE REPLACING ... BY needs an item or a literal");
            for (int i = 0; i < n; i++) {
                Sym *t = rs[i].sym;
                long sub[MAXDIM];
                for (int k = 0; k < rs[i].nsub && k < MAXDIM; k++) sub[k] = 0;
                int rcat = rs[i].user_rm ? (rs[i].rm_bit || t->pi.category == PIC_BOOLEAN ? PIC_BOOLEAN : rs[i].rm_nat ? PIC_NATIONAL : PIC_ALPHANUMERIC)
                                         : t->pi.category;      /* a part is of its part's category */
                if (!t->is_group || rs[i].user_rm) {
                    if (!t->is_filler && rcat == cat) { Ref r = rs[i]; emit_move(&value, &r); }
                } else init_replace_walk(t, &rs[i], cat, &value, sub, rs[i].nsub, line, 0);
            }
            if (!(at_word("alphabetic") || at_word("alphanumeric") || at_word("numeric") || at_word("alphanumeric-edited") ||
                  at_word("numeric-edited") || (g_std >= 2002 && (at_word("national") || at_word("boolean"))))) break;
        }
    }
    if (at_word("with") || at_word("default") || at_word("all") || (at_word("to") && is_word(peek(1), "value")))
        die_at(cur()->line, g_std < 2002 ? "INITIALIZE WITH FILLER / ALL / DEFAULT is COBOL 2002; compile with -std=2002"
                                         : "INITIALIZE WITH FILLER, ALL ... TO VALUE and THEN TO DEFAULT are not implemented");
}

/* ---- SEARCH ------------------------------------------------------------ */

/* SEARCH table [VARYING id] [AT END s] {WHEN cond s}... [END-SEARCH]
 * walks the table's first index from its current value, a serial scan.
 * SEARCH ALL is a binary search over the table's KEYs when its WHEN has
 * the form the standard gives it -- key (index) = value, joined by AND,
 * the keys a leading run of the OCCURS KEY list -- and a scan from 1
 * otherwise (a scan finds the same entry when the keys are unique, and
 * any entry when they are not, which the standard allows).  The bound is
 * the OCCURS count, or the DEPENDING ON item. */

/* SEARCH ALL: is o the table's key k, subscripted by exactly the index? */
static int sa_key_of(const Opnd *o, Sym *tbl, Sym *ix)
{
    if (o->kind != O_REF || o->ref.rm || o->ref.nsub != 1 || o->ref.sub[0].sym != ix || o->ref.sub[0].adj != 0) return -1;   /* ix, not ix + n or ix - n (rule 8) */
    const Sym *x = o->ref.sym;
    int inside = 0;
    for (const Sym *a = x; a; a = a->parent >= 0 ? &g_sym[a->parent] : NULL) if (a == tbl) { inside = 1; break; }
    if (!inside) return -1;
    for (int k = 0; k < tbl->nokey; k++) if (!strcmp(tbl->okey[k], x->name)) return k;
    return -1;
}
/* does the operand depend on the index (then it is no search argument)? */
static int sa_uses_index(const Opnd *o, const Sym *ix)
{
    if (o->kind == O_REF) { for (int i = 0; i < o->ref.nsub; i++) if (o->ref.sub[i].sym == ix) return 1; return o->ref.sym == ix || o->ref.rm; }
    if (o->kind == O_EXPR) { for (int t = o->e_start; t < o->e_end; t++) if (g_tok[t].kind == T_WORD && !strcmp(g_tok[t].s, ix->name)) return 1; return 0; }
    return o->kind == O_FUNC || o->kind == O_BEXPR || o->kind == O_ADDR;
}
/* collect c's key = value relations; 0 when c has another shape */
static int sa_collect(Cond *c, Sym *tbl, Sym *ix, Cond **rel, int *n)
{
    if (c->kind == C_AND) return sa_collect(c->a, tbl, ix, rel, n) && sa_collect(c->b, tbl, ix, rel, n);
    if (c->kind != C_REL || c->op != R_EQ || c->neg || c->bstack || c->ptr || *n >= 8) return 0;
    int kx = sa_key_of(&c->x, tbl, ix), ky = sa_key_of(&c->y, tbl, ix);
    if ((kx < 0) == (ky < 0)) return 0;
    if (ky >= 0) { Opnd t = c->x; c->x = c->y; c->y = t; kx = ky; }     /* the key on the left */
    if (sa_uses_index(&c->y, ix)) return 0;
    for (int i = 0; i < *n; i++) if (sa_key_of(&rel[i]->x, tbl, ix) == kx) return 0;
    rel[(*n)++] = c;
    return 1;
}

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

    /* the phrases first, no code: AT END's statements and each WHEN's
     * body are emitted after the loop, which holds only the tests */
    int save_atend = -1, atend_start = -1;
    if (at_word("at") || at_word("end")) {          /* [AT] END: AT is optional (NC237A writes SEARCH ALL t END GO TO ...) */
        accept_word("at"); expect_word("end");
        atend_start = g_tp;
        g_noemit++; parse_statements(); g_noemit--;
        save_atend = g_tp;
    }
    Cond *wc[16]; int when_start[16], when_body_end[16], nwhen = 0;
    while (at_word("when")) {
        if (nwhen >= 16) die_at(cur()->line, "too many WHENs in SEARCH");
        advance();
        wc[nwhen] = parse_cond();
        when_start[nwhen] = g_tp;
        g_noemit++;
        if (at_word("next")) { advance(); expect_word("sentence"); } else parse_statements();
        g_noemit--;
        when_body_end[nwhen] = g_tp;
        nwhen++;
    }
    if (!nwhen) die_at(t.line, "SEARCH needs at least one WHEN");

    int Lend = new_label(), Latend = new_label(), Lwhen[16];
    for (int i = 0; i < nwhen; i++) Lwhen[i] = new_label();
    Cond *rel[8]; int nrel = 0;
    int binary = all && nwhen == 1 && tbl->ndims == 1 && tbl->nokey > 0 && !(wc[0]->uc1 > wc[0]->uc0) &&
                 sa_collect(wc[0], tbl, ix, rel, &nrel) && nrel > 0 && is_hot_int(ix);
    if (binary) {
        /* the keys used must be the first ones declared (2023 14.9.37.3
         * rule 8); in declared order they steer the search */
        Cond *ord[8]; int no = 0;
        for (int k = 0; k < tbl->nokey && no < nrel; k++) {
            int f = -1;
            for (int i = 0; i < nrel; i++) if (sa_key_of(&rel[i]->x, tbl, ix) == k) f = i;
            if (f < 0) break;
            ord[no++] = rel[f];
        }
        if (no != nrel) binary = 0;
        else {
            /* lo, hi and the middle in slots of their own: the key tests
             * use SLOT_A and the staging slots above g_slot_base */
            int base = g_slot_base; g_slot_base += 3;
            if (g_slot_base > NSLOTS) die_at(t.line, "internal: too many staged operands");
            int lo = SLOT(base), hi = SLOT(base + 1), mid = SLOT(base + 2);
            int Ltop = new_label(), Lup = new_label(), Ldown = new_label();
            emit_li("r1", 1); emit("\tstw sp+%d, r1", lo);
            if (tbl->odo_dep_sym) {
                Opnd d; memset(&d, 0, sizeof d); d.kind = O_REF; d.ref.sym = tbl->odo_dep_sym; d.ref.line = t.line;
                if (is_hot_int(tbl->odo_dep_sym)) emit_hot_value(&d);
                else { Arg a[2] = { arg_ref(&d.ref), arg_desc(sym_desc(tbl->odo_dep_sym)) }; emit_args(a, 2); emit_call("cob_load_int"); }
            } else emit_li("r1", tbl->occurs);
            emit("\tstw sp+%d, r1", hi);
            emit_label(Ltop);
            emit("\tldw r1, sp+%d", lo); emit("\tldw r2, sp+%d", hi);
            emit("\tslt r3, r2, r1");                        /* hi < lo: not there */
            emit("\tbne r3, r0, .L%d", Latend);
            emit("\tadd r1, r1, r2"); emit("\tsrli r1, r1, 1");
            emit("\tstw sp+%d, r1", mid);
            emit_ref_addr(&ixr, "r3");
            emit("\tldw r1, sp+%d", mid);
            emit_store_int(ix, "r3", "r1");                  /* the index at the middle entry */
            for (int i = 0; i < no; i++) {
                /* key below the argument: the entry sought lies after the
                 * middle for an ascending key, before it for a descending one */
                int desc = tbl->okey_desc[sa_key_of(&ord[i]->x, tbl, ix)];
                Cond *lt = cond_new(C_REL); *lt = *ord[i]; lt->op = R_LT;
                Cond *gt = cond_new(C_REL); *gt = *ord[i]; gt->op = R_GT;
                cond_jump_true(lt, desc ? Ldown : Lup);
                cond_jump_true(gt, desc ? Lup : Ldown);
            }
            emit_jump(Lwhen[0]);                             /* every key equal: found */
            emit_label(Lup);                                 /* lo = mid + 1 */
            emit("\tldw r1, sp+%d", mid); emit("\taddi r1, r1, 1"); emit("\tstw sp+%d, r1", lo);
            emit_jump(Ltop);
            emit_label(Ldown);                               /* hi = mid - 1 */
            emit("\tldw r1, sp+%d", mid); emit("\taddi r1, r1, -1"); emit("\tstw sp+%d, r1", hi);
            emit_jump(Ltop);
            g_slot_base = base;
        }
    }
    if (!binary) {
        int Ltop = new_label();
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
        for (int i = 0; i < nwhen; i++) cond_jump_true(wc[i], Lwhen[i]);
        /* no WHEN held: step and go round */
        Opnd step; memset(&step, 0, sizeof step); step.kind = O_NUM; numlit_from_int(&step.num, 1); step.line = t.line;
        emit_add_to_ref(&step, &ixr);
        if (has_vary && vary.sym != ixr.sym) emit_add_to_ref(&step, &vary);   /* VARYING the table's own index: once */
        emit_jump(Ltop);
    }

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

static void parse_raise(void);
static void parse_statement_1(void);

/* a statement, then the EC-DATA-CONVERSION its conversion functions noted */
static int g_para_body_tp = -1;     /* where the current paragraph's first sentence begins */
static int cur_use_is_global(void)
{
    for (int u = 0; u < g_nuse; u++)
        if (g_use[u].unit == g_unit && g_use[u].sec == g_cur_sec_id && g_use[u].global) return 1;
    return 0;
}
static void parse_statement(void)
{
    int outer = g_stmt_convcheck;
    char stmt[16]; memcpy(stmt, g_cur_stmt, sizeof stmt);
    const Tok *stok = g_stmt_tok;
    g_stmt_convcheck = 0;
    g_stmt_tok = cur();
    parse_statement_1();
    if (g_stmt_convcheck) {
        int Lok = new_label();
        emit_call("cob_fn_conv_bad");
        emit("\tbeq r1, r0, .L%d", Lok);
        emit_ec_raise(ec_find("EC-DATA-CONVERSION", 0));
        emit_label(Lok);
    }
    g_stmt_convcheck = outer;
    memcpy(g_cur_stmt, stmt, sizeof stmt); g_stmt_tok = stok;
}

/* ALLOCATE {arithmetic-expression CHARACTERS | data-name-1} [INITIALIZED]
 * [RETURNING data-name-2] (2002 14.8.3) */
static void parse_allocate(void)
{
    Ref based; int has_based = 0, line = cur()->line;
    long fixed = -1;
    const Sym *first = cur()->kind == T_WORD ? sym_lookup_quiet(cur()->s) : NULL;
    if (first && !first->is_based && !is_word(peek(1), "characters") && peek(1)->kind != T_OP && peek(1)->kind != T_LP)
        die_at(line, "ALLOCATE '%s': it is not a BASED entry (2002 14.8.3 rule 1)", first->name);
    if (first && first->is_based) {
        parse_ref(&based); has_based = 1;
        if (based.nsub || based.rm || based.sym->parent >= 0 || !based.sym->is_based)
            die_at(based.line, "ALLOCATE '%s': it is not a BASED entry (2002 14.8.3 rule 1)", based.sym->name);
        fixed = based.sym->size;                 /* an ODO table at its maximum (GR 3), as laid out */
    } else {
        int e0 = g_tp; g_noemit++; parse_expr(); g_noemit--; int e1 = g_tp;
        expect_word("characters");
        emit_expr_tokens(e0, e1); emit_call("cob_pop_alloc_size");
    }
    int init = accept_word("initialized");
    if (init && has_based)
        die_at(line, "ALLOCATE data-name INITIALIZED is not implemented (it is INITIALIZE ... ALL TO VALUE THEN TO DEFAULT)");
    Ref ret; int has_ret = 0;
    if (accept_word("returning")) {
        parse_ref(&ret); has_ret = 1;
        if (ret.sym->is_group || ret.sym->usage != U_POINTER)
            die_at(ret.line, "ALLOCATE RETURNING '%s': a data-pointer item (2002 14.8.3 rule 3)", ret.sym->name);
    }
    if (!has_based && !has_ret) die_at(line, "ALLOCATE of a number of characters needs RETURNING (2002 14.8.3 rule 2)");
    /* the storage comes zeroed, which INITIALIZED asks of characters
     * (GR 6) and leaves pointers NULL (GR 9) */
    if (fixed >= 0) emit_li("r3", fixed); else emit("\tadd r3, r1, r0");
    emit("\tstw sp+%d, r3", SLOT_B);
    emit_call("cob_allocate");
    emit("\tstw sp+%d, r1", SLOT_A);
    if (ec_on_name("EC-STORAGE-NOT-AVAIL")) {
        /* none to be had (GR 5c); a count of 0 or less is NULL, no exception (GR 2) */
        int Lok = new_label();
        emit("\tbne r1, r0, .L%d", Lok);
        emit("\tldw r2, sp+%d", SLOT_B);
        emit("\tbge r0, r2, .L%d", Lok);
        emit_ec_raise(ec_find("EC-STORAGE-NOT-AVAIL", 0));
        emit_label(Lok);
    }
    if (has_based) { emit_la("r3", g_sym[based.sym->record].label); emit("\tldw r1, sp+%d", SLOT_A); emit("\tstw r3+0, r1"); }
    if (has_ret) { emit_ref_addr(&ret, "r3"); emit("\tldw r1, sp+%d", SLOT_A); emit("\tstw r3+0, r1"); }
}

/* FREE {data-name-1}... (2002 14.8.14): each pointer's storage released
 * and the pointer NULL; NULL is left alone; anything else is
 * EC-STORAGE-NOT-ALLOC, the pointer unchanged */
static void parse_free(void)
{
    int n = 0;
    while (at_operand()) {
        Ref r; parse_ref(&r);
        if (r.sym->is_group || r.sym->usage != U_POINTER)
            die_at(r.line, "FREE '%s': a data-pointer item (2002 14.8.14 rule 1)", r.sym->name);
        emit_ref_addr(&r, "r3");
        emit("\tldw r3, r3+0");
        emit_call("cob_free");
        int Lnot = new_label(), Ldone = new_label();
        emit("\tbne r1, r0, .L%d", Lnot);
        emit_ref_addr(&r, "r3");
        emit("\tstw r3+0, r0");
        emit("\tjal r0, .L%d", Ldone);
        emit_label(Lnot);
        if (ec_on_name("EC-STORAGE-NOT-ALLOC")) {
            emit_li("r2", 1);
            emit("\tbne r1, r2, .L%d", Ldone);
            emit_ec_raise(ec_find("EC-STORAGE-NOT-ALLOC", 0));
        }
        emit_label(Ldone);
        n++;
    }
    if (!n) die_at(cur()->line, "FREE needs a pointer item");
}

static void parse_statement_1(void)
{
    apply_dirs();                           /* a >>TURN before this statement */
    Tok *t = cur();
    if (t->kind != T_WORD) die_at(t->line, "expected a statement, found %s", tok_desc(t));
    const char *v = t->s;
    { int k = 0; for (; v[k] && k < 15; k++) g_cur_stmt[k] = (char)toupper((unsigned char)v[k]); g_cur_stmt[k] = 0; }

    if (g_std >= 2002 && !strcmp(v, "raise")) { advance(); parse_raise(); return; }
    if (g_std >= 2002 && !strcmp(v, "validate"))
        die_at(t->line, "VALIDATE is not implemented: an obsolete facility no COBOL provider has implemented (2023 Annex D.22, Annex E; docs/standards.md)");
    if (!strcmp(v, "raise")) die_at(t->line, "RAISE is COBOL 2002; compile with -std=2002");
    if (g_std >= 2002 && !strcmp(v, "resume"))
        die_at(t->line, "RESUME is not implemented (COBOL 2014 made it optional)");

    if (g_std >= 2002 && !strcmp(v, "allocate")) { advance(); parse_allocate(); return; }
    if (g_std >= 2002 && !strcmp(v, "free")) { advance(); parse_free(); return; }
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
    if (!strcmp(v, "suppress")) {
        advance(); accept_word("printing");
        int rep = -1;
        for (int i = 0; i < g_nrwuse; i++)
            if (g_rwuse[i].unit == g_unit && g_rwuse[i].sec == g_cur_sec_id) rep = g_rwuse[i].rep;
        if (rep < 0) die_at(t->line, "SUPPRESS belongs in a USE BEFORE REPORTING section");
        emit_report_addr("r1", &g_reports[rep]);
        emit_li("r2", 1);
        emit("\tstw r1+%d, r2", RW_OFF_SUPPRESS);
        return;
    }
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
    if (!strcmp(v, "unlock")) {
        /* UNLOCK file [RECORD|RECORDS|ALL RECORDS]: RM/COBOL's record
         * locking, released.  One user here: nothing was locked. */
        advance();
        if (cur()->kind != T_WORD) die_at(t->line, "UNLOCK needs a file-name");
        advance();
        accept_word("all"); accept_word("record"); accept_word("records");
        return;
    }
    if (!strcmp(v, "stop")) {
        advance();
        if (accept_word("run")) {
            /* STOP RUN [RETURNING] {integer | identifier}: the process exit
             * status.  Neither form is in the 1985 text (RETURNING is 2002; the
             * bare identifier is RM/COBOL, the Open Systems suite's SJCLCODE
             * copybook ends every program with STOP RUN JCL-CODE), and both are
             * one operand on the exit path that exists (GitHub #35). */
            accept_word("returning");
            if (cur()->kind == T_PERIOD || cur()->kind == T_EOF || !at_operand()) { emit_li("r3", 0); emit_call("cob_stop_run"); return; }
            { Opnd n; parse_operand(&n); check_numeric_opnd(&n);
              if (opnd_hot_int(&n)) emit_hot_value(&n);
              else {
                  if (n.kind != O_REF) die_at(t->line, "STOP RUN needs an integer or a numeric identifier");
                  Arg a[2] = { arg_ref(&n.ref), arg_desc(sym_desc(n.ref.sym)) };
                  emit_args(a, 2); emit_call("cob_load_int");
              }
              emit("\tadd r3, r0, r1"); emit_call("cob_stop_run"); return; }
        }
        /* STOP literal (obsolete): the literal to the operator, who would
         * resume the run -- displayed, and the run goes on */
        if (cur()->kind != T_STR && cur()->kind != T_NUM) die_at(t->line, "STOP needs RUN or a literal");
        bp(BP_O3_STOP_LITERAL, t->line);
        { Arg a[2] = { arg_label(lit_label((unsigned char *)cur()->s, cur()->len)), arg_imm(cur()->len) }; emit_args(a, 2); emit_call("cob_display"); emit_call("cob_display_nl"); }
        advance();
        return;
    }
    if (!strcmp(v, "goback")) { advance(); emit("\tjal r0, .Lgb%d", g_unit); return; }
    if (!strcmp(v, "continue")) { advance(); return; }
    if (!strcmp(v, "exit")) {
        int exit_tp = g_tp;
        advance();
        if (accept_word("program")) {
            if (at_word("raising"))
                die_at(t->line, "EXIT PROGRAM RAISING is not implemented yet (exception propagation to the caller)");
            if (g_is_function)
                die_at(t->line, "EXIT PROGRAM is only in a program's procedure division, not a function's (2023 14.9.14.3 rule 7)");
            if (g_in_decl && cur_use_is_global())
                die_at(t->line, "EXIT PROGRAM in a declarative procedure whose USE is GLOBAL (X3.23-1985 EXIT PROGRAM rule 2; 2023 14.9.14.3 rule 2)");
            if (g_std < 2002 && cur()->kind == T_WORD && is_verb(cur()->s))
                die_at(t->line, "EXIT PROGRAM must be the last of the imperative statements in its sentence (X3.23-1985 EXIT PROGRAM syntax rule 1)");
            /* a program no calling program controls continues past it
             * (X3.23-1985 EXIT PROGRAM general rule 1; 2023 14.9.14.4 rule 2) */
            emit_call("cob_called");
            emit("\tbne r1, r0, .Lgb%d", g_unit);
            return;
        }
        if (g_std >= 2002 && accept_word("perform")) {
            /* 2023 14.9.14 format 3 (cobol ISSUES-90) */
            int cycle = accept_word("cycle");
            if (!g_npstk) die_at(t->line, "EXIT PERFORM is only in an inline or exception-checking PERFORM (2023 14.9.14.3 rule 8)");
            if (cycle && g_pstk[g_npstk - 1].Lcycle < 0)
                die_at(t->line, "EXIT PERFORM CYCLE is not allowed in an exception-checking PERFORM (2023 14.9.14.3 rule 8)");
            emit_jump(cycle ? g_pstk[g_npstk - 1].Lcycle : g_pstk[g_npstk - 1].Lexit);
            return;
        }
        if (g_in_finally && (at_word("paragraph") || at_word("section")))
            die_at(t->line, "EXIT %s in a FINALLY phrase: no statement there transfers control out of the PERFORM (2023 14.9.28.4 rule 16)",
                   at_word("paragraph") ? "PARAGRAPH" : "SECTION");
        if (g_std >= 2002 && accept_word("paragraph")) {
            if (!g_cur_para || g_cur_para->is_section) die_at(t->line, "EXIT PARAGRAPH is only in a paragraph (2023 14.9.14.3 rule 10)");
            if (g_exit_par_label < 0) g_exit_par_label = new_label();
            emit_jump(g_exit_par_label);
            return;
        }
        if (g_std >= 2002 && accept_word("section")) {
            if (g_cur_sec_id < 0) die_at(t->line, "EXIT SECTION is only in a section (2023 14.9.14.3 rule 9)");
            if (g_exit_sec_label < 0) g_exit_sec_label = new_label();
            emit_jump(g_exit_sec_label);
            return;
        }
        if (at_word("perform") || at_word("paragraph") || at_word("section"))
            die_at(t->line, "EXIT %s is COBOL 2002; compile with -std=2002",
                   at_word("perform") ? "PERFORM" : at_word("paragraph") ? "PARAGRAPH" : "SECTION");
        /* EXIT alone: a sentence by itself, the only one in its paragraph
         * (X3.23-1985 EXIT syntax rules 1-2; 2023 14.9.14.3 rule 1) */
        int alone = exit_tp == g_para_body_tp && cur()->kind == T_PERIOD;
        if (alone) {
            Tok *n = peek(1);
            alone = n->kind == T_EOF || is_word(n, "end") || is_word(n, "identification") || is_word(n, "id") ||
                    (at_para_name(n) && (peek(2)->kind == T_PERIOD || is_word(peek(2), "section")));
        }
        if (!alone) die_at(t->line, "EXIT must be a sentence by itself, the only one in its paragraph (X3.23-1985 EXIT syntax rule 1; 2023 14.9.14.3 rule 1)");
        return;
    }
    if (!strcmp(v, "next")) die_at(t->line, "NEXT SENTENCE is only valid inside IF (or SEARCH)");
    if (!strcmp(v, "alter")) {
        /* ALTER p1 TO [PROCEED TO] p2 ...: p1's GO TO now goes to p2 */
        bp(BP_O1_ALTER, t->line);
        advance();
        for (;;) {
            Para *p1 = expect_para();
            decl_ref_check(p1, 0, cur()->line);
            if (p1->is_section) die_at(t->line, "ALTER names a paragraph, not a section");
            if (!is_altered_para(p1->name)) die_at(t->line, "internal: '%s' was not seen by the ALTER prescan", p1->name);
            expect_word("to");
            if (accept_word("proceed")) expect_word("to");
            Para *p2 = expect_para();
            decl_ref_check(p2, 0, cur()->line);
            char cell[32], tgt[32];
            snprintf(cell, sizeof cell, ".Lalt%d_%d", g_unit, p1->id);
            snprintf(tgt, sizeof tgt, ".Lp%d_%d", g_unit, p2->id);
            emit_la("r2", tgt); emit_la("r1", cell); emit("\tstw r1+0, r2");
            if (!(cur()->kind == T_WORD && !is_verb(cur()->s) && !is_terminator(cur()->s) && para_find(cur()->s))) break;
        }
        return;
    }
    if (!strcmp(v, "enter") || !strcmp(v, "disable") || !strcmp(v, "enable") ||
        !strcmp(v, "purge") || !strcmp(v, "receive") || !strcmp(v, "send"))
        die_at(t->line, "%s is not supported (the Communication module is deliberately out)", v);
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
static int g_recursive;             /* PROGRAM-ID ... IS RECURSIVE, or contained in such a program (COBOL 2002) */
static int g_std = 85;              /* -std=85 (the default) or -std=2002: Stage B, docs/standards.md */

/* everything a unit keeps in globals, saved while a contained program is compiled */
struct UnitSave {
    int unit, sym_base, sym_end, file_base, file_end, para_base, para_end, use_end;
    char progid[64], progid_orig[64];
    int nreport, report_base, nscreen, screen_base, nclass, nswitch, nalphabet, nmnemonic, last_item, nsame_groups, collate, lowval, highval, cur_fd, in_linkage;
    char collate_name[64];
    char crtname[64];
    int nuse, in_decl, cur_sec_id, saw_end, initial, recursive, nsorttab;
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
static void emit_act_desc(void);
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
    memcpy(u->progid, g_progid, sizeof u->progid); memcpy(u->progid_orig, g_progid_orig, sizeof u->progid_orig);
    u->nreport = g_nreport; u->report_base = g_report_base; u->nscreen = g_nscreen; u->screen_base = g_screen_base; u->nclass = g_nclass; u->nswitch = g_nswitch; u->nalphabet = g_nalphabet;
    u->nmnemonic = g_nmnemonic; u->last_item = g_last_item; u->nsame_groups = g_nsame_groups; u->collate = g_collate;
    u->lowval = g_lowval; u->highval = g_highval; u->cur_fd = g_cur_fd; u->in_linkage = g_in_linkage;
    memcpy(u->collate_name, g_collate_name, sizeof u->collate_name);
    memcpy(u->crtname, g_crt_status_name, sizeof u->crtname);
    u->nuse = g_nuse; memcpy(u->use, g_use, sizeof u->use); u->in_decl = g_in_decl; u->cur_sec_id = g_cur_sec_id;
    u->saw_end = g_saw_end_program; u->initial = g_initial; u->recursive = g_recursive; u->io_file = g_io_file;
    memcpy(u->cls, g_class, sizeof u->cls); memcpy(u->sw, g_switch, sizeof u->sw); memcpy(u->alph, g_alphabet, sizeof u->alph);
    memcpy(u->mn, g_mnemonic, sizeof u->mn); memcpy(u->same, g_same, sizeof u->same); memcpy(u->nsame, g_nsame, sizeof u->nsame);
    u->nsorttab = g_nsorttab; u->sorttab = xmalloc((size_t)(g_nsorttab + 1) * sizeof *g_sorttab);
    memcpy(u->sorttab, g_sorttab, (size_t)g_nsorttab * sizeof *g_sorttab);
    g_ustack[g_udepth++] = u;

    g_unit = ++g_unit_counter;
    g_sym_base = g_nsym; g_file_base = g_nfile; g_para_base = g_npara;
    /* the contained unit's own USE entries follow every enclosing unit's */
    g_report_base = g_nreport; g_screen_base = g_nscreen; g_nclass = 0; g_nswitch = 0; g_nalphabet = 0; g_nmnemonic = 0; g_last_item = -1;
    g_nsame_groups = 0; g_npoison = 0; g_collate = -1; g_collate_name[0] = 0; g_crt_status_name[0] = 0; g_lowval = 0x00; g_highval = 0xFF; g_cur_fd = -1; g_in_linkage = 0;
    g_nsorttab = 0; g_initial = 0;
    /* a program contained in a recursive program is recursive (2023 11.10.4 rule 4) */
    g_recursive = u->recursive;
    int in_proc = g_in_proc; char cur_stmt[16]; memcpy(cur_stmt, g_cur_stmt, sizeof cur_stmt);
    g_in_proc = 0;
    parse_identification_division();
    parse_environment_division();
    parse_data_division();
    if (!at_word("procedure")) die_at(cur()->line, "expected PROCEDURE DIVISION, found %s", tok_desc(cur()));
    parse_procedure_division();
    g_in_proc = in_proc; memcpy(g_cur_stmt, cur_stmt, sizeof cur_stmt);
    emit_unit_data();
    if (!g_saw_end_program) die_at(cur()->line, "a contained program needs its END PROGRAM");

    g_udepth--;
    g_unit = u->unit; g_sym_base = u->sym_base; g_nsym = u->sym_end; g_file_base = u->file_base; g_nfile = u->file_end;
    g_para_base = u->para_base; g_npara = u->para_end;
    memcpy(g_progid, u->progid, sizeof g_progid); memcpy(g_progid_orig, u->progid_orig, sizeof g_progid_orig);
    g_nreport = u->nreport; g_report_base = u->report_base; g_nscreen = u->nscreen; g_screen_base = u->screen_base; g_nclass = u->nclass; g_nswitch = u->nswitch; g_nalphabet = u->nalphabet;
    g_nmnemonic = u->nmnemonic; g_last_item = u->last_item; g_nsame_groups = u->nsame_groups; g_collate = u->collate;
    g_lowval = u->lowval; g_highval = u->highval; g_cur_fd = u->cur_fd; g_in_linkage = u->in_linkage;
    memcpy(g_collate_name, u->collate_name, sizeof g_collate_name);
    memcpy(g_crt_status_name, u->crtname, sizeof g_crt_status_name);
    g_nuse = u->nuse; memcpy(g_use, u->use, sizeof g_use); g_in_decl = u->in_decl; g_cur_sec_id = u->cur_sec_id;
    g_saw_end_program = u->saw_end; g_initial = u->initial; g_recursive = u->recursive; g_io_file = u->io_file;
    memcpy(g_class, u->cls, sizeof g_class); memcpy(g_switch, u->sw, sizeof g_switch); memcpy(g_alphabet, u->alph, sizeof g_alphabet);
    memcpy(g_mnemonic, u->mn, sizeof g_mnemonic); memcpy(g_same, u->same, sizeof g_same); memcpy(g_nsame, u->nsame, sizeof g_nsame);
    g_nsorttab = u->nsorttab;
    if (g_nsorttab > g_sorttabcap) { g_sorttabcap = g_nsorttab; g_sorttab = realloc(g_sorttab, (size_t)g_sorttabcap * sizeof *g_sorttab); }
    memcpy(g_sorttab, u->sorttab, (size_t)g_nsorttab * sizeof *g_sorttab);
    free(u->sorttab); free(u);
}

/* Where a parse resumes after an error in a sentence: past its period,
 * unless a paragraph or section header, or the end of the program or of
 * DECLARATIVES, comes first (the period was left off). */
static void resync_sentence(int start)
{
    if (g_tp > start && g_tok[g_tp - 1].kind == T_PERIOD) return;
    if (g_tp == start) advance();
    while (cur()->kind != T_PERIOD && cur()->kind != T_EOF) {
        Tok *t = cur(), *n = peek(1);
        if (t->kind == T_WORD && g_tok[g_tp - 1].line != t->line &&
            ((n->kind == T_PERIOD && para_find(t->s)) || (is_word(n, "section") && peek(2)->kind == T_PERIOD))) return;
        if (is_word(t, "end") && (is_word(n, "program") || is_word(n, "declaratives"))) return;
        if ((is_word(t, "identification") || is_word(t, "id")) && is_word(n, "division")) return;
        advance();
    }
    if (cur()->kind == T_PERIOD) advance();
}

/* -fnsig: past this unit's procedure division to its END PROGRAM or END
 * FUNCTION (a contained program's END names another), which is consumed */
static void skip_unit_body(void)
{
    const char *kind = g_is_function ? "function" : "program";
    while (cur()->kind != T_EOF) {
        if (at_word("end") && is_word(peek(1), kind) &&
            (peek(2)->kind == T_PERIOD || peek(2)->kind == T_EOF || is_word(peek(2), g_progid))) {
            advance(); advance();
            if (cur()->kind == T_WORD) advance();
            if (cur()->kind == T_PERIOD) advance();
            g_saw_end_program = 1;
            return;
        }
        advance();
    }
    g_saw_end_program = 0;
}

static void parse_procedure_division(void)
{
    expect_word("procedure"); expect_word("division");
    g_cur_stmt[0] = 0; g_in_proc = 1;
    Sym *using[8]; int nusing = 0;
    if (accept_word("using")) {
        while (cur()->kind == T_WORD && !at_word("returning")) {
            if (g_std >= 2002) {
                if (accept_word("by")) {
                    if (at_word("value")) die_at(cur()->line, "BY VALUE parameters are not implemented yet (the C-ABI CALL has them)");
                    expect_word("reference");
                }
                if (at_word("optional")) die_at(cur()->line, "OPTIONAL parameters are not implemented yet");
                if (cur()->kind != T_WORD || at_word("returning")) break;
            }
            if (nusing >= (g_is_function ? 7 : 8)) die_at(cur()->line, "more than %d USING items (stack arguments) are not implemented yet", g_is_function ? 7 : 8);
            Sym *u = sym_lookup(cur()->s, NULL, 0, cur()->line);
            if (!g_sym[u->record].is_linkage || u->parent >= 0)
                die_at(cur()->line, "USING '%s' must be a level 01 or 77 item of the LINKAGE SECTION", u->name);
            using[nusing++] = u;
            advance();
        }
    }
    if (at_word("returning") && !g_is_function)
        die_at(cur()->line, "PROCEDURE DIVISION RETURNING is COBOL 2002, and for a program not implemented yet; make the result the last USING item (docs/functions.md)");
    if (g_is_function) {
        /* the function's result: a level 01 or 77 item; the caller passes
         * the address of its temporary after the arguments */
        if (!accept_word("returning")) die_at(cur()->line, "a function needs PROCEDURE DIVISION ... RETURNING its result");
        Sym *r = sym_lookup(cur()->s, NULL, 0, cur()->line);
        if (!r->is_linkage || r->parent >= 0 || r->level == 66 || r->is_cond || r->redefines >= 0)
            die_at(cur()->line, "RETURNING '%s' must be a level 01 or 77 item of the LINKAGE SECTION, without REDEFINES (2023 14.2.2 rule 5)", r->name);
        g_returning = r;
        advance();
        if (g_nfnsig == 128) die_at(cur()->line, "more than 128 user-defined functions");
        FnSig *f = &g_fnsig[g_nfnsig++];
        memset(f, 0, sizeof *f);
        snprintf(f->name, sizeof f->name, "%s", g_progid);
        snprintf(f->link, sizeof f->link, "%s", link_name(g_progid));
        f->nparam = nusing;
        for (int k = 0; k < nusing; k++) fdesc_of(&f->param[k], using[k]);
        fdesc_of(&f->ret, r);
        fnsig_write(f);
    }
    expect_period();
    if (g_fnsig_only) { skip_unit_body(); return; }
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
    emit("\tstw sp+%d, r12", SLOT_R12);
    emit("\tstw sp+%d, r13", SLOT_R13);
    /* the caller's addresses go into the LINKAGE cells, first: every call
     * below clobbers the argument registers (a USING program with DECIMAL-
     * POINT IS COMMA, CURRENCY SIGN, a COLLATING SEQUENCE or IS INITIAL
     * used to take its addresses from what those calls left there) */
    if (g_std >= 2002) {
        /* the arguments wait in the frame while cob_act_enter saves the
         * cells they are about to overwrite (a RECURSIVE caller's own) */
        for (int i = 0; i < nusing; i++) emit("\tstw sp+%d, %s", SLOT(i), argreg(i));
        if (g_is_function) emit("\tstw sp+%d, %s", SLOT_RET, argreg(nusing));   /* where the result goes */
        char lab[32]; snprintf(lab, sizeof lab, ".Lact%d", g_unit);
        emit_la("r3", lab); emit_call("cob_act_enter"); emit("\tstw sp+%d, r1", SLOT_ACT);
        for (int i = 0; i < nusing; i++) {
            emit_la("r1", g_sym[using[i]->record].label);
            emit("\tldw r2, sp+%d", SLOT(i));
            emit("\tstw r1+0, r2");
        }
        if (g_is_function) {
            /* the LINKAGE result is the caller's temporary itself */
            emit_la("r1", g_returning->label);
            emit("\tldw r2, sp+%d", SLOT_RET);
            emit("\tstw r1+0, r2");
        }
    } else
        for (int i = 0; i < nusing; i++) {
            emit_la("r1", g_sym[using[i]->record].label);
            emit("\tstw r1+0, %s", argreg(i));
        }
    emit_call("cob_perform_enter"); emit("\tstw sp+%d, r1", SLOT_PBASE);   /* this activation's PERFORM frames */
    if (g_collate >= 0) {       /* PROGRAM COLLATING SEQUENCE: this unit's table, the caller's kept */
        char lab[32]; snprintf(lab, sizeof lab, ".Lcoll%d", g_unit);
        emit_la("r3", lab); emit_call("cob_set_collating"); emit("\tstw sp+%d, r1", SLOT_COLL);
    }
    if (g_dp_comma) { emit("\taddi r3, r0, 1"); emit_call("cob_set_decimal_point"); emit("\tstw sp+%d, r1", SLOT_DP); }
    if (g_currency && g_currency != '$') { emit_li("r3", g_currency); emit_call("cob_set_currency"); emit("\tstw sp+%d, r1", SLOT_CUR); }
    if (g_initial) { char cl[32]; snprintf(cl, sizeof cl, ".Lcan%d", g_unit); emit_call(cl); }   /* INITIAL: as after CANCEL */
    /* a FILE STATUS item in the LINKAGE SECTION (or EXTERNAL): the image
     * takes its address now that the cell is filled (status is at 16) */
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        if (!f->status_sym) continue;
        Sym *rec = &g_sym[f->status_sym->record];
        if (!rec_indirect(rec)) continue;
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
    g_cur_para = NULL;
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
        if (is_word(t, "end") && (is_word(peek(1), "program") || is_word(peek(1), "function"))) break;
        if ((is_word(t, "identification") || is_word(t, "id")) && is_word(peek(1), "division")) {
            /* a contained program: from here to END PROGRAM the text is nested
             * programs; the containing program's flow ends as at its last line */
            if (cur_par >= 0) { end_par_label(); emit_exit_check(cur_par); }
            if (cur_sec >= 0) { end_sec_label(); emit_exit_check(cur_sec); }
            cur_par = -1; cur_sec = -1; g_cur_sec_id = -1;
            emit("\tjal r0, .Lgb%d", g_unit);
            compile_nested_unit();
            emit("\t.text");                   /* the contained unit's data left the section */
            continue;
        }
        if (is_word(t, "end") && is_word(peek(1), "declaratives")) {
            if (!g_in_decl) die_at(t->line, "END DECLARATIVES without DECLARATIVES");
            if (cur_par >= 0) { end_par_label(); emit_exit_check(cur_par); }
            if (cur_sec >= 0) { end_sec_label(); emit_exit_check(cur_sec); }
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
            if (cur_par >= 0) { end_par_label(); emit_exit_check(cur_par); }
            if (p->is_section && cur_sec >= 0) { end_sec_label(); emit_exit_check(cur_sec); }
            emit_para_label(p);
            g_cur_para = p;
            if (p->is_section) { cur_sec = p->id; cur_par = -1; g_cur_sec_id = p->id; } else cur_par = p->id;
            advance(); if (p->is_section) advance();
            expect_period();
            g_para_body_tp = g_tp;
            continue;
        }
        if (t->kind == T_WORD && !is_verb(t->s) && peek(1)->kind == T_NUM && is_word(peek(2), "section"))
            die_at(t->line, "section segment numbers are obsolete in COBOL 85; not supported");

        /* a sentence; after an error in it, the next one (ISSUES-41) */
        g_sentence_label = -1;
        jmp_buf jb, *outer = g_recover;
        int start = g_tp, noemit = g_noemit, slot = g_slot_base, cdepth = g_cond_depth, merge = g_is_merge, fdepth = g_fn_depth;
        /* the state a statement may leave half-changed when it fails: the
         * checking (an exception-checking PERFORM's implicit TURN), the
         * PERFORMs open around it (cobol ISSUES-94 E10) */
        static EcState ecs0;
        ecs_copy(&ecs0, &g_ecs);
        int necp = g_necp, ecp_handler = g_ecp_handler, npstk = g_npstk, necu = g_necu, in_finally = g_in_finally;
        if (setjmp(jb)) {
            g_recover = outer;
            g_noemit = noemit; g_slot_base = slot; g_cond_depth = cdepth; g_is_merge = merge; g_fn_depth = fdepth;
            ecs_copy(&g_ecs, &ecs0);
            for (int c = NEC + necu; c < NEC + g_necu; c++) { g_ecs.on[c] = (unsigned char)g_ecs.user_on; g_ecs.loc[c] = (unsigned char)g_ecs.user_loc; }
            g_necp = necp; g_ecp_handler = ecp_handler; g_npstk = npstk; g_in_finally = in_finally;
            g_abbr_op = -1; g_sentence_label = -1; g_ufn_forbid = NULL;
            resync_sentence(start);
            continue;
        }
        g_recover = &jb;
        for (;;) {
            parse_statement();
            if (cur()->kind == T_PERIOD) { advance(); break; }
            if (cur()->kind == T_EOF) die_at(cur()->line, "missing '.' at the end of the last sentence");
            if (at_scope_end()) die_at(cur()->line, "'%s' without a matching statement", cur()->s);
        }
        g_recover = outer;
        if (g_sentence_label >= 0) emit_label(g_sentence_label);
    }
    if (cur_par >= 0) { end_par_label(); emit_exit_check(cur_par); }
    if (cur_sec >= 0) { end_sec_label(); emit_exit_check(cur_sec); }

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
    emit("\tldw r3, sp+%d", SLOT_PBASE); emit_call("cob_perform_leave");
    if (g_std >= 2002) {
        char lab[32]; snprintf(lab, sizeof lab, ".Lact%d", g_unit);
        emit_la("r3", lab); emit("\tldw r4, sp+%d", SLOT_ACT); emit_call("cob_act_leave");
    }
    if (g_collate >= 0) { emit("\tldw r3, sp+%d", SLOT_COLL); emit_call("cob_set_collating"); }
    if (g_dp_comma) { emit("\tldw r3, sp+%d", SLOT_DP); emit_call("cob_set_decimal_point"); }
    if (g_currency && g_currency != '$') { emit("\tldw r3, sp+%d", SLOT_CUR); emit_call("cob_set_currency"); }
    emit("\taddi r1, r0, 0");
    emit("\tldw r13, sp+%d", SLOT_R13);
    emit("\tldw r12, sp+%d", SLOT_R12);
    emit("\tldw r11, sp+4");
    emit("\tldw lr, sp+0");
    emit("\taddi sp, sp, %d", FRAME);
    emit("\tjalr r0, r31, 0");

    /* the unit joins the program registry at start-up (CALL identifier);
     * a function is invoked, never CALLed, and does not */
    if (!g_is_function) {
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
            if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0 || rec_indirect(s)) continue;
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
        if (g_std >= 2002) {                   /* and its activation descriptor, for EC-PROGRAM-RECURSIVE-CALL */
            char al[32]; snprintf(al, sizeof al, ".Lact%d", g_unit);
            emit_la("r3", nlab); emit_la("r4", al);
            emit_call("cob_register_act");
        }
        emit("\tldw lr, sp+0");
        emit("\taddi sp, sp, 8");
        emit("\tjalr r0, r31, 0");
        emit("\t.section .init_array");
        emit("\t.p2align 2");
        emit("\t.word .Lreg%d", g_unit);
        emit("\t.text");
    }

    if (!g_is_function && !g_main_done && !g_module && !g_udepth) {
        /* the first program of an executable is the main program (functions
         * defined ahead of it, as REPOSITORY requires, are not) */
        g_main_done = 1;
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
    if (g_is_function) {
        if (!(at_word("end") && is_word(peek(1), "function"))) die_at(cur()->line, "a function ends with END FUNCTION %s", g_progid);
        advance(); advance();
        if (cur()->kind != T_WORD || strcmp(cur()->s, g_progid))
            die_at(cur()->line, "END FUNCTION names '%s' but the function is '%s'", cur()->s, g_progid);
        advance();
        if (cur()->kind != T_EOF) expect_period();
        g_saw_end_program = 1;
    } else if (accept_word("end")) {
        expect_word("program");
        if (cur()->kind == T_PERIOD || cur()->kind == T_EOF) {
            /* a bare END PROGRAM. -- RM/COBOL; the Open Systems AP and IN
             * modules end every program that way (PA's CRPACHK without
             * even the period, as the last line of the file) */
        } else {
            if (cur()->kind != T_WORD || strcmp(cur()->s, g_progid))
                die_at(cur()->line, "END PROGRAM names '%s' but the program is '%s'", cur()->s, g_progid);
            advance();
        }
        if (cur()->kind != T_EOF) expect_period();
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
    g_is_function = 0; g_returning = NULL; g_nrepo_fn = 0; g_repo_all_intrinsic = 0;
    if (at_word("function-id")) {
        /* COBOL 2002 11.5: a user-defined function, always recursive */
        if (g_std < 2002) die_at(cur()->line, "FUNCTION-ID is COBOL 2002; compile with -std=2002 (docs/standards.md, Stage B)");
        if (g_udepth) die_at(cur()->line, "a function definition cannot be contained in a program");
        g_is_function = 1; g_recursive = 1;
    } else expect_word("program-id");
    if (g_is_function) advance();
    expect_period();
    if (cur()->kind != T_WORD) die_at(cur()->line, "expected a program-name");
    snprintf(g_progid, sizeof g_progid, "%s", cur()->s);
    snprintf(g_progid_orig, sizeof g_progid_orig, "%s", tok_orig(cur()));
    advance();
    if (g_is_function) {
        if (accept_word("as")) die_at(cur()->line, "FUNCTION-ID ... AS literal is not implemented yet");
        if (accept_word("is") || at_word("prototype")) {
            if (at_word("prototype")) die_at(cur()->line, "function prototypes (IS PROTOTYPE) are not implemented yet; the caller finds the definition's signature file");
            die_at(cur()->line, "expected '.' after the function name, found %s", tok_desc(cur()));
        }
    }
    accept_word("is");
    for (;;) {
        int line = cur()->line;
        if (accept_word("initial")) {
            g_initial = 1;                                       /* fresh WORKING-STORAGE on every CALL */
            if (g_recursive)
                die_at(line, "INITIAL: a program that is, or is contained in, a RECURSIVE program cannot be INITIAL (2023 11.10.3 rule 5)");
        }
        else if (accept_word("common")) { }                      /* callable by the siblings too: every program here is */
        else if (accept_word("recursive")) {
            if (g_std < 2002) die_at(line, "RECURSIVE is COBOL 2002; compile with -std=2002 (docs/standards.md, Stage B)");
            for (int k = 0; k < g_udepth; k++)
                if (g_ustack[k]->initial) die_at(line, "RECURSIVE: a program contained in an INITIAL program cannot be RECURSIVE (2023 11.10.3 rule 6)");
            g_recursive = 1;
        }
        else break;
    }
    if (g_initial && g_recursive) die_at(cur()->line, "a program cannot be both INITIAL and RECURSIVE");
    accept_word("program");
    expect_period();

    static const char *paras[] = { "author", "installation", "date-written",
        "date-compiled", "security", "remarks", NULL };
    while (!at_division() && cur()->kind != T_EOF) {
        Tok *t = cur();
        int known = 0;
        for (int i = 0; paras[i]; i++) if (is_word(t, paras[i])) known = 1;
        if (!known) die_at(t->line, "unexpected %s in the IDENTIFICATION DIVISION", tok_desc(t));
        bp(BP_O2_COMMENT_ENTRY, t->line);
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
    user_word(cur()->s, line, "a file");
    snprintf(f->name, sizeof f->name, "%s", cur()->s);
    snprintf(f->oname, sizeof f->oname, "%s", tok_orig(cur()));
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
                /* RM/COBOL's device word before the name -- ASSIGN TO RANDOM
                 * "GLMAST.GLDATA", ASSIGN TO PRINT "PRINTER" -- says nothing on
                 * this machine: accepted and ignored, as SHARING is (GitHub #34).
                 * A device word with nothing after it is still refused. */
                static const char *devs[] = { "random", "print", "printer", "disk", "input", "output", "input-output",
                                              "display", "keyboard", "tape", "cassette", NULL };
                static const char *clauses[] = { "organization", "organisation", "access", "record", "status", "file",
                                                 "sequential", "indexed", "relative", "line", "lock", "sharing", "key",
                                                 "alternate", "reserve", "padding", "data", "block", NULL };
                int dev = 0, clause_next = peek(1)->kind != T_STR && peek(1)->kind != T_WORD;
                for (int i = 0; devs[i]; i++) if (at_word(devs[i])) dev = 1;
                for (int i = 0; clauses[i]; i++) if (is_word(peek(1), clauses[i])) clause_next = 1;
                if (dev && !clause_next) advance();
                else if (dev)
                    die_at(t->line, "ASSIGN TO %s (a device) is not supported; name a file", cur()->s);
                if (cur()->kind == T_STR) { f->assign_lit = cur(); advance(); }
                else if (cur()->kind == T_WORD) { snprintf(f->assign_name, sizeof f->assign_name, "%s", cur()->s); advance(); }
                else die_at(t->line, "expected a literal or data-name after ASSIGN TO");
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

/* REPOSITORY (COBOL 2002 12.3.8): FUNCTION name ... makes user functions
 * invocable without the word FUNCTION; FUNCTION ALL INTRINSIC and
 * FUNCTION name ... INTRINSIC do the same for intrinsics. */
static void parse_repository(void)
{
    advance(); expect_period();
    while (at_word("function")) {
        int line = cur()->line;
        advance();
        if (accept_word("all")) { expect_word("intrinsic"); g_repo_all_intrinsic = 1; continue; }
        int first = g_nrepo_fn;
        while (cur()->kind == T_WORD && !at_word("function") && !at_word("intrinsic") && !at_division() &&
               !at_word("input-output") && !at_word("special-names")) {
            if (at_word("as")) die_at(cur()->line, "REPOSITORY FUNCTION ... AS literal is not implemented yet");
            if (g_nrepo_fn == 32) die_at(cur()->line, "more than 32 functions in REPOSITORY");
            snprintf(g_repo_fn[g_nrepo_fn++], sizeof g_repo_fn[0], "%s", cur()->s);
            advance();
        }
        if (g_nrepo_fn == first) die_at(line, "REPOSITORY FUNCTION needs a function name, or ALL INTRINSIC");
        if (accept_word("intrinsic")) {
            /* intrinsics named individually: invocable without FUNCTION, like ALL INTRINSIC does for all */
            for (int k = first; k < g_nrepo_fn; k++) if (!fn89_known(g_repo_fn[k]))
                die_at(line, "'%s' is not an intrinsic function", g_repo_fn[k]);
            g_repo_all_intrinsic = 1;         /* narrower in the text; the names are checked, the rest is harmless */
            g_nrepo_fn = first;
        }
    }
    for (;;) {
        if (at_word("class") || at_word("interface") || at_word("program") || at_word("property"))
            die_at(cur()->line, "REPOSITORY %s is object orientation or a program prototype, not implemented", cur()->s);
        break;
    }
    if (cur()->kind == T_PERIOD) advance();
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
                while ((cur()->kind == T_WORD || cur()->kind == T_NUM) && !at_word("special-names") && !at_word("input-output") && !at_word("repository") &&
                       !at_word("source-computer") && !at_word("object-computer") && !at_division()) {   /* MEMORY SIZE 64000 CHARACTERS: obsolete, no effect */
                    if (at_word("memory")) bp(BP_O5_MEMORY_SIZE, cur()->line);
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
                        user_word(cur()->s, cur()->line, "a class");
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
                    if (accept_word("symbolic")) {
                        /* SYMBOLIC [CHARACTERS] {name... {IS|ARE} integer...}... [IN alphabet-name] */
                        accept_word("characters");
                        for (;;) {
                            char names[32][64]; int nn = 0;
                            while (cur()->kind == T_WORD && !at_word("is") && !at_word("are") && !at_word("in")) {
                                if (nn == 32) die_at(cur()->line, "SYMBOLIC CHARACTERS: too many names in one list");
                                user_word(cur()->s, cur()->line, "a symbolic character");
                                snprintf(names[nn++], 64, "%s", cur()->s); advance();
                            }
                            if (!nn) die_at(cur()->line, "SYMBOLIC CHARACTERS: expected a name");
                            if (!accept_word("is")) accept_word("are");
                            int ni = 0;
                            while (cur()->kind == T_NUM) {
                                if (ni >= nn) die_at(cur()->line, "SYMBOLIC CHARACTERS: more integers than names");
                                int ord = atoi(cur()->s);
                                if (ord < 1 || ord > 256) die_at(cur()->line, "SYMBOLIC CHARACTERS: the ordinal position is 1 through 256");
                                if (g_nsymch == 32) die_at(cur()->line, "too many SYMBOLIC CHARACTERS");
                                if (symch_find(names[ni]) >= 0) die_at(cur()->line, "SYMBOLIC CHARACTERS: '%s' is named twice", names[ni]);
                                memcpy(g_symch[g_nsymch].name, names[ni], sizeof g_symch[0].name);
                                g_symch[g_nsymch].byte = ord - 1; g_nsymch++;
                                ni++; advance();
                            }
                            if (ni != nn) die_at(cur()->line, "SYMBOLIC CHARACTERS: %d names but %d integers", nn, ni);
                            if (accept_word("in")) {
                                /* IN alphabet-name: the native sequence is the one there is */
                                if (cur()->kind != T_WORD) die_at(cur()->line, "SYMBOLIC CHARACTERS IN needs an alphabet-name");
                                advance(); break;
                            }
                            if (cur()->kind != T_WORD || at_word("class") || at_word("currency") || at_word("decimal-point") || at_word("alphabet") || at_word("symbolic") || switch_find(cur()->s)) break;
                            if (mnemonic_kind(cur()->s) >= 0 || !strncmp(cur()->s, "switch-", 7) || at_word("sysin") || at_word("sysout") || at_word("console") || at_word("syserr") || at_word("formfeed")) break;
                        }
                        continue;
                    }
                    if (at_word("crt") && is_word(peek(1), "status")) {
                        advance(); advance(); accept_word("is");
                        if (cur()->kind != T_WORD) die_at(cur()->line, "CRT STATUS IS needs a data-name");
                        snprintf(g_crt_status_name, sizeof g_crt_status_name, "%s", cur()->s);
                        advance(); continue;
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
                        user_word(cur()->s, cur()->line, "an alphabet");
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
                            user_word(cur()->s, cur()->line, "a mnemonic");
                            Mnemonic *m = &g_mnemonic[g_nmnemonic++];
                            snprintf(m->name, sizeof m->name, "%s", cur()->s); m->kind = mk; advance();
                            continue;
                        }
                    }
                    if (at_division() || at_word("input-output") || at_word("repository")) break;
                    die_at(cur()->line, "SPECIAL-NAMES clause '%s' is not implemented yet (CLASS, SWITCH-n, ALPHABET and the device names are)", cur()->s);
                }
                continue;
            }
            if (at_word("repository")) {
                if (g_std < 2002) die_at(cur()->line, "REPOSITORY is COBOL 2002; compile with -std=2002, or rewrite user-defined functions as CALL (docs/functions.md)");
                parse_repository();
                continue;
            }
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
                if (at_word("rerun")) bp(BP_O10_RERUN, cur()->line);
                if (at_word("multiple") && is_word(cur() + 1, "file")) bp(BP_O11_MULTIPLE_FILE, cur()->line);
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
        if (at_word("label")) bp(BP_O6_LABEL_RECORDS, cur()->line);
        if (accept_word("label")) { accept_word("record"); accept_word("records"); accept_word("is"); accept_word("are"); accept_word("standard"); accept_word("omitted"); continue; }
        if (at_word("data")) bp(BP_O8_DATA_RECORDS, cur()->line);
        if (accept_word("data")) { accept_word("record"); accept_word("records"); accept_word("is"); accept_word("are"); while (cur()->kind == T_WORD && !at_word("block") && !at_word("record") && !at_word("label") && !at_word("report") && !at_word("value")) advance(); continue; }
        if (accept_word("report") || accept_word("reports")) {
            accept_word("is"); accept_word("are");
            if (cur()->kind != T_WORD) die_at(t->line, "expected a report-name");
            snprintf(f->report_name, sizeof f->report_name, "%s", cur()->s); advance();
            /* REPORTS ARE r1 r2 ...: several reports to one file (X3.23-1985
             * XIII 2.2; each told apart by its CODE, cobol ISSUES-94) */
            while (cur()->kind == T_WORD && !is_verb(cur()->s) && !at_word("label") && !at_word("block") && !at_word("record") &&
                   !at_word("records") && !at_word("data") && !at_word("value") && !at_word("recording") && !at_word("code-set") &&
                   !at_word("linage") && !at_word("external") && !at_word("global") && !at_word("is")) {
                f->report_more = xrealloc(f->report_more, (size_t)(f->nreport_more + 1) * sizeof *f->report_more);
                snprintf(f->report_more[f->nreport_more++], 64, "%s", cur()->s); advance();
            }
            continue;
        }
        if (accept_word("recording")) {
            accept_word("mode"); accept_word("is");
            if (accept_word("f")) { f->varying = 0; continue; }
            if (accept_word("v")) { f->varying = 1; continue; }
            die_at(t->line, "RECORDING MODE %s is refused (U and S are tapemgr's business; docs/framing.md)", cur()->s);
        }
        if (at_word("value")) bp(BP_O7_VALUE_OF, cur()->line);
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
        if (accept_word("code-set")) {
            accept_word("is");
            if (cur()->kind != T_WORD) die_at(t->line, "CODE-SET needs an alphabet-name");
            int found = -1;
            for (int i = 0; i < g_nalphabet; i++) if (!strcmp(g_alphabet[i].name, cur()->s)) found = i;
            if (found < 0) die_at(t->line, "CODE-SET: '%s' is not an alphabet-name", cur()->s);
            if (!g_alphabet[found].native) die_at(t->line, "CODE-SET %s: only the native (STANDARD-1) character set is available", cur()->s);
            advance(); continue;
        }
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
    for (int i = g_file_base; i < g_nfile; i++) {
        if (!strcmp(g_files[i].report_name, r->name)) r->file = i;
        for (int k = 0; k < g_files[i].nreport_more; k++) if (!strcmp(g_files[i].report_more[k], r->name)) r->file = i;
    }
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
        if (accept_word("control") || accept_word("controls")) {
            accept_word("is"); accept_word("are");
            if (accept_word("final")) r->ctl_final = 1;
            while (cur()->kind == T_WORD && !at_word("page") && !at_word("heading") && !at_word("first") && !at_word("last") && !at_word("footing") && !at_word("code")) {
                if (r->nctl == 8) die_at(t->line, "more than 8 control levels");
                Sym *c = sym_lookup(cur()->s, NULL, 0, t->line); advance();
                r->ctl_sym[r->nctl] = sym_idx(c);
                /* the prior values a CONTROL FOOTING prints: a hidden clone
                 * of the item, sensed against and refreshed at each break */
                Sym *cl = sym_new();
                snprintf(cl->name, sizeof cl->name, "*prior-%.50s-%d", c->name, r->nctl);
                cl->line = t->line; cl->level = 77;
                cl->has_pic = c->has_pic; memcpy(cl->pic, c->pic, sizeof cl->pic); cl->pi = c->pi;
                cl->usage = c->usage; cl->has_usage = c->has_usage;
                r->ctl_clone[r->nctl] = sym_idx(cl);
                Sym *hd = sym_new();
                snprintf(hd->name, sizeof hd->name, "*held-%.51s-%d", c->name, r->nctl);
                hd->line = t->line; hd->level = 77;
                hd->has_pic = c->has_pic; memcpy(hd->pic, c->pic, sizeof hd->pic); hd->pi = c->pi;
                hd->usage = c->usage; hd->has_usage = c->has_usage;
                r->ctl_held[r->nctl] = sym_idx(hd);
                r = &g_reports[g_nreport - 1];                       /* sym_new may move nothing, but be safe */
                r->nctl++;
            }
            if (!r->nctl && !r->ctl_final) die_at(t->line, "CONTROL needs FINAL or data-names");
            continue;
        }
        if (accept_word("code")) {
            /* CODE (X3.23-1985 XIII 3.6; 2023 13.18.12): the characters
             * each record of this report begins with, outside the lines'
             * columns.  85: a two-character literal; 2023 also an
             * identifier, evaluated at the start of each body group. */
            accept_word("is");
            if (cur()->kind == T_STR) {
                if (g_std < 2002 && cur()->len != 2) die_at(t->line, "CODE takes a two-character literal (X3.23-1985 XIII 3.6.3 rule 1)");
                r->code_lit = cur(); advance();
            } else if (g_std >= 2002 && cur()->kind == T_WORD) {
                r->code_tp = g_tp; advance();
                while ((at_word("of") || at_word("in")) && peek(1)->kind == T_WORD) { advance(); advance(); }
            } else die_at(t->line, "CODE takes %s", g_std >= 2002 ? "an alphanumeric literal or identifier" : "a two-character literal");
            continue;
        }
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
        g->use_sec = -1; g->ctl_level = -1;
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
            char entry_name[64] = "";
            if (cur()->kind == T_WORD) {
                int is_clause = 0;
                for (int k = 0; clause_words[k]; k++) if (at_word(clause_words[k])) is_clause = 1;
                if (!is_clause) {
                    if (first) snprintf(g->name, sizeof g->name, "%s", cur()->s);
                    else snprintf(entry_name, sizeof entry_name, "%s", cur()->s);
                    advance();
                }   /* a name */
            }
            /* the entry's clauses */
            int has_line = 0, labs = 0, lplus = 0, is_field = 0, lnp = 0;
            RField fd; memset(&fd, 0, sizeof fd); fd.line = eline;
            int usage_disp = 0;         /* DISPLAY written: a PICTURE of N refuses it (13.18.60.3 rule 20) */
            snprintf(fd.ename, sizeof fd.ename, "%s", entry_name);
            while (cur()->kind != T_PERIOD) {
                Tok *t = cur();
                if (accept_word("type")) {
                    if (!first) die_at(t->line, "TYPE belongs on the 01 of report group '%s'", g->name);
                    accept_word("is");
                    if (accept_word("page")) { if (accept_word("heading")) g->type = RG_PAGE_HEADING; else if (accept_word("footing")) g->type = RG_PAGE_FOOTING; else die_at(t->line, "TYPE PAGE: HEADING or FOOTING"); }
                    else if (accept_word("ph")) g->type = RG_PAGE_HEADING;
                    else if (accept_word("pf")) g->type = RG_PAGE_FOOTING;
                    else if (accept_word("detail") || accept_word("de")) g->type = RG_DETAIL;
                    else if (accept_word("report")) { if (accept_word("heading")) g->type = RG_REPORT_HEADING; else if (accept_word("footing")) g->type = RG_REPORT_FOOTING; else die_at(t->line, "TYPE REPORT: HEADING or FOOTING"); }
                    else if (accept_word("rh")) g->type = RG_REPORT_HEADING;
                    else if (accept_word("rf")) g->type = RG_REPORT_FOOTING;
                    else if (at_word("control") || at_word("ch") || at_word("cf")) {
                        int foot = at_word("cf");
                        if (accept_word("control")) { if (accept_word("footing")) foot = 1; else if (!accept_word("heading")) die_at(t->line, "TYPE CONTROL: HEADING or FOOTING"); }
                        else advance();
                        g->type = foot ? RG_CONTROL_FOOTING : RG_CONTROL_HEADING;
                        if (accept_word("final")) g->ctl_tp = 0;
                        else if (cur()->kind == T_WORD) { g->ctl_tp = g_tp; advance(); while (at_word("of") || at_word("in")) { advance(); if (cur()->kind == T_WORD) advance(); } }
                        else die_at(t->line, "TYPE CONTROL %s needs a control data-name or FINAL", foot ? "FOOTING" : "HEADING");
                    }
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
                    } else if (accept_word("next")) { expect_word("page"); lnp = 1; }
                    else die_at(t->line, "expected a line number after LINE");
                    if (!lnp && !labs && !lplus) die_at(t->line, "LINE needs a number");
                    if (r->page_limit && labs > r->page_limit) die_at(t->line, "LINE %d is past PAGE LIMIT %d", labs, r->page_limit);
                    has_line = 1;
                    continue;
                }
                if (accept_word("next")) {
                    expect_word("group"); accept_word("is");
                    if (!first) die_at(t->line, "NEXT GROUP belongs on the 01 of report group '%s'", g->name);
                    if (accept_word("plus")) { if (cur()->kind != T_NUM) die_at(t->line, "expected a number after NEXT GROUP PLUS"); g->next_kind = 2; g->next_n = atoi(cur()->s); advance(); }
                    else if (accept_word("next")) { expect_word("page"); g->next_kind = 3; }
                    else if (cur()->kind == T_NUM) {
                        if (cur()->s[0] == '+') { g->next_kind = 2; g->next_n = atoi(cur()->s + 1); }
                        else { g->next_kind = 1; g->next_n = atoi(cur()->s); }
                        advance();
                    }
                    else die_at(t->line, "NEXT GROUP takes an integer, PLUS integer, or NEXT PAGE");
                    continue;
                }
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
                    snprintf(fd.pic, sizeof fd.pic, "%s", cur()->s); pic_len_check(fd.pic, t->line);
                    if (nat_picture(fd.pic, &fd.pi, t->line)) { advance(); is_field = 1; continue; }
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
                if (accept_word("usage")) {
                    accept_word("is");
                    if (accept_word("national")) { if (g_std < 2002) die_at(t->line, "USAGE NATIONAL is COBOL 2002; compile with -std=2002"); fd.usage_nat = 1; }
                    else if (accept_word("display")) usage_disp = 1;
                    else die_at(cur()->line, "a report group item takes only USAGE DISPLAY or NATIONAL, not %s (2023 13.18.60.3 rule 7)", tok_desc(cur()));
                    continue;
                }
                if (accept_word("display")) { usage_disp = 1; continue; }
                if (g_std >= 2002 && accept_word("national")) { fd.usage_nat = 1; continue; }
                if (accept_word("sum")) {
                    fd.has_sum = 1; is_field = 1;
                    for (;;) {
                        accept_word("of");
                        if (cur()->kind != T_WORD) die_at(t->line, "SUM needs data-names");
                        if (fd.nsum == 8) die_at(t->line, "more than 8 SUM operands");
                        fd.sum_tp[fd.nsum++] = g_tp; advance();
                        while (at_word("of") || at_word("in")) { advance(); if (cur()->kind == T_WORD) advance(); }
                        if (at_word("upon") || at_word("reset") || cur()->kind == T_PERIOD || at_word("sum")) {
                            if (accept_word("sum")) continue;
                            break;
                        }
                    }
                    if (accept_word("upon")) {
                        while (cur()->kind == T_WORD && !at_word("reset")) {
                            if (fd.nupon == 4) die_at(t->line, "more than 4 UPON details");
                            fd.upon_tp[fd.nupon++] = g_tp; advance();
                        }
                        if (!fd.nupon) die_at(t->line, "UPON needs a DETAIL group name");
                    }
                    if (accept_word("reset")) {
                        accept_word("on");
                        if (accept_word("final")) fd.reset_final = 1;
                        else if (cur()->kind == T_WORD) { fd.reset_tp = g_tp; advance(); }
                        else die_at(t->line, "RESET ON needs a control data-name or FINAL");
                    }
                    continue;
                }
                if (accept_word("group")) { accept_word("indicate"); fd.gi = 1; is_field = 1; continue; }
                die_at(t->line, "unexpected %s in report group '%s'", tok_desc(t), g->name);
            }
            expect_period();
            if (first && !has_type) die_at(g->line, "report group '%s' needs a TYPE", g->name);
            if (has_line) {
                if (g->nl == g->lcap) { g->lcap = g->lcap ? g->lcap * 2 : 4; g->l = realloc(g->l, g->lcap * sizeof *g->l); }
                RLine *ln = &g->l[g->nl++];
                memset(ln, 0, sizeof *ln);
                ln->line = eline; ln->abs = labs; ln->plus = lplus; ln->np = lnp;
            }
            if (is_field) {
                if (!g->nl) die_at(eline, "a printable entry of report group '%s' before any LINE", g->name);
                RLine *ln = &g->l[g->nl - 1];
                if (fd.has_sum) {
                    /* the sum counter: a signed item sized by the entry's
                     * PICTURE, named by the entry's data-name when it has
                     * one (X3.23 VIII 2.20), zeroed by INITIATE */
                    if (g->type != RG_CONTROL_FOOTING) die_at(eline, "SUM belongs in a CONTROL FOOTING group");
                    if (!fd.has_pic) die_at(eline, "a SUM entry needs a PICTURE");
                    Sym *ctr = sym_new();
                    if (fd.ename[0]) snprintf(ctr->name, sizeof ctr->name, "%s", fd.ename);
                    else snprintf(ctr->name, sizeof ctr->name, "*sum-%d-%d", (int)(r - g_reports), r->ng * 100 + g->nl);
                    ctr->line = eline; ctr->level = 77;
                    ctr->has_pic = 1;
                    int idig = fd.pi.digits - fd.pi.scale;
                    if (fd.pi.scale > 0 && idig > 0) snprintf(ctr->pic, sizeof ctr->pic, "s9(%d)v9(%d)", idig, fd.pi.scale);
                    else if (fd.pi.scale > 0) snprintf(ctr->pic, sizeof ctr->pic, "sv9(%d)", fd.pi.scale);
                    else snprintf(ctr->pic, sizeof ctr->pic, "s9(%d)", fd.pi.digits > 0 ? fd.pi.digits : 1);
                    pic_analyse(ctr->pic, &ctr->pi);
                    fd.ctr_sym = sym_idx(ctr);
                    r = &g_reports[g_nreport - 1]; g = &r->g[r->ng - 1]; ln = &g->l[g->nl - 1];
                }
                if (!fd.has_pic && fd.value && fd.value->kind == T_STR) {
                    /* VALUE without PICTURE: an alphanumeric of the literal's width */
                    fd.has_pic = 1;
                    if (fd.value->nat) {
                        /* a national literal: national, as many positions as it
                         * has characters or takes columns, so all of it shows */
                        int nu = fd.value->len / 2, nc = nat_lit_cols((const unsigned char *)fd.value->s, fd.value->len);
                        snprintf(fd.pic, sizeof fd.pic, "n(%d)", nc > nu ? nc : nu > 0 ? nu : 1);
                        nat_picture(fd.pic, &fd.pi, eline);
                    } else {
                        snprintf(fd.pic, sizeof fd.pic, "x(%d)", fd.value->len > 0 ? fd.value->len : 1);
                        if (pic_analyse(fd.pic, &fd.pi) < 0) die_at(eline, "report field: %s", fd.pi.err);
                    }
                }
                if (fd.blank_zero) bwz_check("the report field", &fd.pi, 0, eline);
                if (usage_disp && fd.pi.category == PIC_NATIONAL)
                    die_at(eline, "a report group item with a PICTURE of N takes only USAGE NATIONAL (2023 13.18.60.3 rule 20)");
                if (fd.usage_nat) {
                    if (fd.pi.category == PIC_NATIONAL) fd.usage_nat = 0;     /* PICTURE N is national usage already */
                    else if (fd.pi.category != PIC_NUMERIC && fd.pi.category != PIC_NUMERIC_EDITED)
                        die_at(eline, "USAGE NATIONAL takes a PICTURE N, or a numeric or numeric-edited one (2023 13.18.60.3 rule 12)");
                }
                if (fd.value && fd.value->kind == T_STR && fd.value->nat && !rfield_is_nat(&fd))
                    die_at(eline, "a national VALUE goes to a national field (PICTURE N)");
                if (!fd.has_pic) die_at(eline, "a report field needs a PICTURE");
                if (fd.has_source + !!fd.value + fd.has_sum != 1) die_at(eline, "a report field needs exactly one of SOURCE, VALUE and SUM");
                if (!fd.column) fd.column = ln->nf ? ln->f[ln->nf - 1].column + rfield_cols(&ln->f[ln->nf - 1]) : 1;
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
        /* nested groups: a stack of the enclosing entries.  Each carries
         * the composed look (flags, colours) its children inherit, and
         * the group's LINE/COLUMN, which anchor its first child. */
        struct { int level, flags, fg, bg, line, col, subidx, usage; } gstk[16];
        int gdepth = 0;
        while (cur()->kind == T_NUM && strcmp(cur()->s, "01")) {
            int fl = parse_level(); int fline = cur()->line; advance();
            if (fl <= 1 || fl > 49) die_at(fline, "bad level %d in a screen", fl);
            while (gdepth && gstk[gdepth - 1].level >= fl) {
                int si = gstk[--gdepth].subidx;
                if (si >= 0) sc->sub[si].count = sc->nf - sc->sub[si].first;
            }
            char ename[64] = "";
            int susage = 0;             /* USAGE: 1 DISPLAY, 2 NATIONAL; a group's reaches its children */
            if (cur()->kind == T_WORD && !at_word("blank") && !at_word("usage") && !at_word("line") && !at_word("column") && !at_word("col") &&
                !at_word("value") && !at_word("pic") && !at_word("picture") && !at_word("highlight") && !at_word("underline") &&
                !at_word("auto") && !at_word("auto-skip") && !at_word("reverse-video") && !at_word("from") && !at_word("to") && !at_word("using") &&
                !at_word("secure") && !at_word("required") && !at_word("full") && !at_word("lowlight") && !at_word("blink") && !at_word("bell") &&
                !at_word("beep") && !at_word("erase") && !at_word("foreground-color") && !at_word("background-color")) {
                snprintf(ename, sizeof ename, "%s", cur()->s);
                advance();                                       /* a name on the entry */
            }
            if (sc->nf == sc->fcap) { sc->fcap = sc->fcap ? sc->fcap * 2 : 16; sc->f = realloc(sc->f, sc->fcap * sizeof *sc->f); }
            SField *f = &sc->f[sc->nf];
            SField *prev = sc->nf ? &sc->f[sc->nf - 1] : NULL;
            memset(f, 0, sizeof *f);
            f->srcline = fline; f->kind = -1; f->fg = 255; f->bg = 255;
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
                    if (accept_word("plus") || accept_word("+")) {       /* relative to the previous slot's line */
                        int n = 1;
                        if (cur()->kind == T_NUM) { n = atoi(cur()->s); advance(); }
                        f->line = (prev ? prev->line : 0) + n; continue;
                    }
                    if (cur()->kind != T_NUM) die_at(t->line, "expected a number after LINE");
                    f->line = atoi(cur()->s); advance(); continue;
                }
                if (accept_word("column") || accept_word("col")) {
                    accept_word("number"); accept_word("is");
                    if (accept_word("plus") || accept_word("+")) {       /* from the position after the previous slot, as GnuCOBOL counts */
                        int n = 1;
                        if (cur()->kind == T_NUM) { n = atoi(cur()->s); advance(); }
                        f->col = (prev && (!f->line || f->line == prev->line) ? prev->col + prev->width : 0) + n; continue;
                    }
                    if (cur()->kind != T_NUM) die_at(t->line, "expected a number after COLUMN");
                    f->col = atoi(cur()->s); advance(); continue;
                }
                if (accept_word("value")) {
                    accept_word("is");
                    if (cur()->kind != T_STR) die_at(t->line, "a screen VALUE needs a nonnumeric literal");
                    f->value = cur(); f->natlit = cur()->nat; advance(); f->kind = COB_SCR_VALUE; continue;
                }
                if (accept_word("pic") || accept_word("picture")) {
                    if (cur()->kind != T_PIC) die_at(t->line, "expected a PICTURE character-string");
                    f->has_pic = 1;
                    snprintf(f->pic, sizeof f->pic, "%s", cur()->s);
                    pic_len_check(f->pic, t->line);
                    if (nat_picture(f->pic, &f->pi, t->line)) { advance(); continue; }
                    if (pic_analyse(f->pic, &f->pi) < 0) die_at(t->line, "screen field: %s", f->pi.err);
                    advance(); continue;
                }
                if (at_word("from") || at_word("to") || at_word("using")) {
                    int kind = at_word("from") ? COB_SCR_FROM : at_word("to") ? COB_SCR_TO : COB_SCR_USING;
                    advance();
                    /* the reference's tokens are recorded and skipped, as
                     * Report Writer records SOURCE: the table dimensions do
                     * not exist yet, so it is resolved at first use
                     * (sfield_resolve) and re-parsed at every ACCEPT/DISPLAY
                     * when its address is not static */
                    f->ref_tp = g_tp;
                    if (cur()->kind != T_WORD) die_at(t->line, "expected a data-name");
                    advance();
                    while ((at_word("of") || at_word("in")) && peek(1)->kind == T_WORD) { advance(); advance(); }
                    if (cur()->kind == T_LP) {
                        int d = 0;
                        do { if (cur()->kind == T_LP) d++; else if (cur()->kind == T_RP) d--; advance(); }
                        while (d && cur()->kind != T_PERIOD);
                    }
                    if (cur()->kind == T_LP) die_at(t->line, "reference modification in a screen item is not implemented");
                    f->kind = kind; continue;
                }
                if (accept_word("highlight")) { f->flags |= COB_SF_HIGHLIGHT; continue; }
                if (accept_word("underline")) { f->flags |= COB_SF_UNDERLINE; continue; }
                if (accept_word("auto") || accept_word("auto-skip")) { f->flags |= COB_SF_AUTO; continue; }
                if (accept_word("reverse-video")) { f->flags |= COB_SF_REVERSE; continue; }
                if (accept_word("bell") || accept_word("beep") || accept_word("blink")) continue;   /* no bell, no blink: painted plain */
                if (accept_word("erase")) { accept_word("eol"); accept_word("eos"); continue; }
                if (accept_word("foreground-color") || accept_word("foreground-colour") || accept_word("background-color") || accept_word("background-colour")) {
                    int bg = t->s[0] == 'b';
                    accept_word("is");
                    if (cur()->kind != T_NUM) die_at(t->line, "expected a colour number 0-7 after %s", t->s);
                    int c = atoi(cur()->s); advance();
                    if (c < 0 || c > 7) die_at(t->line, "a screen colour is 0-7 (black, blue, green, cyan, red, magenta, yellow, white)");
                    if (bg) f->bg = c; else f->fg = c;
                    continue;
                }
                if (accept_word("secure")) { f->flags |= COB_SF_SECURE; continue; }
                if (accept_word("required")) { f->flags |= COB_SF_REQUIRED; continue; }
                if (accept_word("full")) { f->flags |= COB_SF_FULL; continue; }
                if (accept_word("lowlight")) { f->flags |= COB_SF_LOWLIGHT; continue; }
                if (accept_word("usage")) {
                    accept_word("is");
                    if (accept_word("national")) susage = 2;
                    else if (accept_word("display")) susage = 1;
                    else die_at(cur()->line, "a screen item takes only USAGE DISPLAY or NATIONAL, not %s (2023 13.18.60.3 rule 17)", tok_desc(cur()));
                    continue;
                }
                die_at(t->line, "unexpected %s in screen '%s'", tok_desc(t), sc->name);
            }
            expect_period();
            if (!susage && gdepth) susage = gstk[gdepth - 1].usage;
            if (f->has_pic && f->blank_zero) bwz_check("the screen field", &f->pi, 0, fline);
            if (f->has_pic && susage == 1 && f->pi.category == PIC_NATIONAL)
                die_at(fline, "a screen item with a PICTURE of N takes only USAGE NATIONAL (2023 13.18.60.3 rule 20)");
            if (f->has_pic && susage == 2 && f->pi.category != PIC_NATIONAL)
                die_at(fline, "USAGE NATIONAL on a screen item whose PICTURE is not N is not implemented");
            if (blank_screen_entry && f->kind < 0 && !f->has_pic) continue;   /* just BLANK SCREEN */
            if (f->kind < 0 && !f->has_pic) {
                /* a group: its look composes over the enclosing one and its
                 * children inherit it; its position anchors the first child */
                if (gdepth == 16) die_at(fline, "screen groups nested more than 16 deep");
                int pf = gdepth ? gstk[gdepth - 1].flags : 0;
                int pfg = gdepth ? gstk[gdepth - 1].fg : 255, pbg = gdepth ? gstk[gdepth - 1].bg : 255;
                gstk[gdepth].level = fl;
                gstk[gdepth].flags = pf | f->flags;
                gstk[gdepth].fg = f->fg != 255 ? f->fg : pfg;
                gstk[gdepth].bg = f->bg != 255 ? f->bg : pbg;
                gstk[gdepth].line = f->line; gstk[gdepth].col = f->col;
                gstk[gdepth].subidx = -1;
                gstk[gdepth].usage = susage;
                if (ename[0] && strcmp(ename, "filler")) {
                    if (sym_lookup_quiet(ename)) die_at(fline, "'%s' is both a data item and a screen group", ename);
                    char dummy[40]; int d1, d2;
                    if (screen_ref(ename, dummy, sizeof dummy, &d1, &d2)) die_at(fline, "screen group '%s' is already a screen or group name", ename);
                    if (sc->nsub == sc->subcap) { sc->subcap = sc->subcap ? sc->subcap * 2 : 4; sc->sub = realloc(sc->sub, sc->subcap * sizeof *sc->sub); }
                    SGroup *g = &sc->sub[sc->nsub];
                    snprintf(g->name, sizeof g->name, "%s", ename);
                    g->first = sc->nf; g->count = 0;
                    gstk[gdepth].subidx = sc->nsub++;
                }
                gdepth++;
                continue;
            }
            if (f->kind < 0) die_at(fline, "a screen slot needs VALUE, or PIC with FROM, TO or USING");
            if (gdepth) {
                /* inherit the enclosing look; the input-only clauses reach
                 * only the fields that take input */
                int gf = gstk[gdepth - 1].flags;
                if (f->kind != COB_SCR_TO && f->kind != COB_SCR_USING)
                    gf &= ~(COB_SF_AUTO | COB_SF_SECURE | COB_SF_REQUIRED | COB_SF_FULL);
                f->flags |= gf;
                if (f->fg == 255) f->fg = gstk[gdepth - 1].fg;
                if (f->bg == 255) f->bg = gstk[gdepth - 1].bg;
                if (!f->line && gstk[gdepth - 1].line) f->line = gstk[gdepth - 1].line;
                if (!f->col && gstk[gdepth - 1].col) f->col = gstk[gdepth - 1].col;
                gstk[gdepth - 1].line = 0; gstk[gdepth - 1].col = 0;    /* the anchor is the first child's */
            }
            if (f->kind == COB_SCR_VALUE) {
                if (f->has_pic) die_at(fline, "a VALUE slot takes no PICTURE");
                f->width = f->natlit ? nat_lit_cols((const unsigned char *)f->value->s, f->value->len) : f->value->len;
            }
            else { if (!f->has_pic) die_at(fline, "a FROM/TO/USING slot needs a PICTURE"); f->width = sfield_cols(f); }
            if (!f->line) f->line = prev ? prev->line : 1;        /* no LINE: the previous slot's line */
            if (!f->col) f->col = prev && prev->line == f->line ? prev->col + prev->width : 1;   /* no COLUMN: right after it */
            if ((f->flags & (COB_SF_SECURE | COB_SF_REQUIRED | COB_SF_FULL)) && f->kind != COB_SCR_TO && f->kind != COB_SCR_USING)
                die_at(fline, "SECURE, REQUIRED and FULL belong to an input field (TO or USING)");
            sc->nf++;
        }
        while (gdepth) {
            int si = gstk[--gdepth].subidx;
            if (si >= 0) sc->sub[si].count = sc->nf - sc->sub[si].first;
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
        if (at_word("local-storage") && is_word(peek(1), "section")) {
            /* COBOL 2002: automatic data, a fresh copy for every activation
             * (2023 8.6.4), reached through a cell like a LINKAGE record */
            if (g_std < 2002) die_at(cur()->line, "the LOCAL-STORAGE SECTION is COBOL 2002; compile with -std=2002 (docs/standards.md, Stage B)");
            advance(); advance(); expect_period();
            g_in_local = 1;
            while (cur()->kind == T_NUM) parse_data_item();
            g_in_local = 0;
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

/* The activation descriptor (-std=2002; cobol ISSUES-49).  cob_act_enter
 * reads it at every entry: the active count, whether the program is
 * RECURSIVE, its name for the EC-PROGRAM-RECURSIVE-CALL message, the words
 * an activation owns but which live in static cells -- LINKAGE and
 * LOCAL-STORAGE cells, FILE STATUS pointers into them, TIMES counters --
 * saved on entry and restored on return, and each LOCAL-STORAGE record's
 * cell, initial image and size, a fresh copy per activation (2023 8.6.4).
 * Only a RECURSIVE program can be re-entered, so only its words are saved. */
static void emit_act_desc(void)
{
    char words[256][48]; int nw = 0;
    if (g_recursive) {
        for (int i = g_sym_base; i < g_nsym; i++) {
            Sym *s = &g_sym[i];
            if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0) continue;
            if ((s->is_linkage || s->is_local) && nw < 256) snprintf(words[nw++], sizeof words[0], "%s", s->label);
        }
        for (int i = g_file_base; i < g_nfile; i++) {
            File *f = &g_files[i];
            if (f->status_sym && (g_sym[f->status_sym->record].is_linkage || g_sym[f->status_sym->record].is_local) && nw < 256)
                snprintf(words[nw++], sizeof words[0], ".Lf%d_%d+16", f->unit, i);
        }
        for (int k = 0; k < g_ncnt; k++)
            if (g_cnt_unit[k] == g_unit && nw < 256) snprintf(words[nw++], sizeof words[0], ".Lcnt%d", k);
        if (nw == 256) die_at(0, "internal: more than 256 per-activation words in '%s'", g_progid);
    }
    int nl = 0;
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (!(s->is_cond || s->parent >= 0 || s->redefines >= 0) && s->is_local) nl++;
    }
    char nm[80]; snprintf(nm, sizeof nm, "%s", g_progid);
    const char *nlab = lit_label((const unsigned char *)nm, (int)strlen(nm) + 1);
    emit("\t.p2align 2");
    emit(".Lact%d:\t# activation descriptor", g_unit);
    emit("\t.word 0");                              /* active instances */
    emit("\t.word %d", g_recursive);
    emit("\t.word %s", nlab);
    emit("\t.word %d", nw);
    for (int k = 0; k < nw; k++) emit("\t.word %s", words[k]);
    emit("\t.word %d", nl);
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || !s->is_local) continue;
        emit("\t.word %s", s->label); emit("\t.word %s_i", s->label); emit("\t.word %d", s->image_size);
    }
}

static void emit_unit_data(void)
{
    emit("");
    emit("\t.data");
    for (int i = g_sym_base; i < g_nsym; i++) {
        Sym *s = &g_sym[i];
        if (s->is_cond || s->parent >= 0 || s->redefines >= 0 || s->lin_file >= 0 || s->rep_ctr >= 0) continue;
        if (s->is_local) {
            /* a cell for the activation's copy, and the copy's initial state
             * (cob_act_enter makes a fresh one on every entry) */
            emit("\t.p2align 2");
            emit("%s:\t# local-storage %02d %s (%d bytes, the activation's)", s->label, s->level, s->name, s->image_size);
            emit("\t.word 0");
            emit("\t.section .rodata");
            emit("\t.p2align 3");
            emit("%s_i:", s->label);
            emit_bytes(s->image, s->image_size);
            emit("\t.data");
            continue;
        }
        if (s->is_based && !s->is_linkage) {
            emit("\t.p2align 2");
            emit("%s:\t# based %02d %s (%d bytes, wherever SET ADDRESS OF puts it)", s->label, s->level, s->name, s->image_size);
            emit("\t.word 0");
            continue;
        }
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
    if (g_std >= 2002) emit_act_desc();
    for (int i = g_file_base; i < g_nfile; i++) {
        File *f = &g_files[i];
        /* a line sequential file of national records holds UTF-8 text:
         * the runtime converts, told by varying = 2 (cobol ISSUES-74) */
        int varying = f->varying;
        if (f->org == COB_ORG_LINESEQ) {
            int nrec = 0, arec = 0;
            for (int k = g_sym_base; k < g_nsym; k++)
                if (g_sym[k].level == 1 && g_sym[k].fd == i) { if (sym_is_national(&g_sym[k])) nrec = k + 1; else arec = k + 1; }
            if (nrec && arec)
                die_at(f->line, "the line sequential file '%s' has national and alphanumeric records; they are all one or the other here", f->name);
            if (nrec) varying = 2;
            if (g_std >= 2002) varying |= 4;    /* 2023 14.9.30 rule 15: 06, the rest for the next READ (cobol ISSUES-94 N8) */
        }
        emit("\t.p2align 2");
        emit(".Lf%d_%d:\t# %s", f->unit, i, f->name);
        emit("\t.byte %d,%d,%d,0", f->org, f->access, f->optional);
        emit("\t.word 0");
        if (f->rec >= 0 && !f->external) emit("\t.word %s", g_sym[g_sym[f->rec].record].label); else emit("\t.word 0");   /* an EXTERNAL file's record area is set at entry */
        emit("\t.word %d", f->recsize);
        if (f->status_sym && !rec_indirect(&g_sym[f->status_sym->record]))
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
        emit("\t.word %d", varying);
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
        emit("\t.word 0");                                 /* reversed (OPEN INPUT ... REVERSED) */
        emit("\t.word 0");                                 /* pr_state (the print file's cursor) */
        emit("\t.word 0");                                 /* rbuf, rpos, rlen: the runtime's line-sequential read buffer */
        emit("\t.word 0");
        emit("\t.word 0");
        if (f->external) { emit(".Lfx%d_%d:\t# the shared connector of EXTERNAL %s", f->unit, i, f->name); emit("\t.word 0"); }
    }
    emit("\t.p2align 2");
    emit(".Luse%d:\t# USE sections by open mode", g_unit);
    for (int m = 0; m < 5; m++) emit("\t.word 0");
    if (g_collate >= 0) {
        emit(".Lcoll%d:\t# PROGRAM COLLATING SEQUENCE %s: rank of each character", g_unit, g_alphabet[g_collate].name);
        emit_bytes(g_alphabet[g_collate].rank, 256);
    }
    for (int i = 0; i < g_nalphabet; i++)
        if (g_alphabet[i].used) {
            emit(".Lalph%d_%d:\t# ALPHABET %s: rank of each character, for SORT", g_unit, i, g_alphabet[i].name);
            emit_bytes(g_alphabet[i].rank, 256);
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
    for (int i = g_screen_base; i < g_nscreen; i++) {
        Screen *sc = &g_screens[i];
        emit("\t.p2align 2");
        emit(".Lscrf%d_%d:\t# screen %s slots", g_unit, i, sc->name);
        for (int k = 0; k < sc->nf; k++) {
            SField *f = &sc->f[k];
            sfield_resolve(f);
            emit("\t.byte %d,%d", f->kind | (f->dyn ? 0x80 : 0), f->flags);
            emit("\t.short %d", f->line); emit("\t.short %d", f->col);
            emit("\t.byte %d,%d", f->fg, f->bg);   /* FOREGROUND-COLOR, BACKGROUND-COLOR (255: not given) */
            emit("\t.word %d", f->width);
            if (f->kind == COB_SCR_VALUE) emit("\t.word %s", lit_label((unsigned char *)f->value->s, f->value->len)); else emit("\t.word 0");
            if (f->kind == COB_SCR_VALUE && f->natlit) emit("\t.word .Ld%d", nat_desc(f->value->len));   /* painted as national text */
            else if (f->has_pic) {
                Desc d; memset(&d, 0, sizeof d);
                switch (f->pi.category) {
                case PIC_NATIONAL: d.cat = COB_NATIONAL; break;
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
            if (f->item && f->dyn) { emit("\t.word .Lsdyn%d_%d_%d", g_unit, i, k); emit("\t.word .Ld%d", sym_desc(f->item)); }
            else if (f->item) { emit("\t.word %s+%ld", g_sym[f->item->record].label, f->stat_off); emit("\t.word .Ld%d", sym_desc(f->item)); }
            else { emit("\t.word 0"); emit("\t.word 0"); }
            emit("\t.byte %d,%d", f->ext, f->prompt ? f->prompt : '_'); emit("\t.short 0");   /* ext, prompt, rsv */
        }
        emit(".Lscr%d_%d:\t# screen %s", g_unit, i, sc->name);
        emit("\t.word %d", sc->nf);
        emit("\t.word %d", sc->blank_screen);
        emit("\t.word .Lscrf%d_%d", g_unit, i);
        for (int j = 0; j < sc->nsub; j++) {                  /* a named group: a window into the same slots */
            emit(".Lscrg%d_%d_%d:\t# screen %s group %s", g_unit, i, j, sc->name, sc->sub[j].name);
            emit("\t.word %d", sc->sub[j].count);
            emit("\t.word 0");
            emit("\t.word .Lscrf%d_%d+%d", g_unit, i, sc->sub[j].first * SCRF_SIZE);
        }
        for (int k = 0; k < sc->nf; k++)
            if (sc->f[k].dyn) { emit(".Lsdyn%d_%d_%d:\t# %s: the address, computed at ACCEPT/DISPLAY", g_unit, i, k, sc->f[k].item->name); emit("\t.word 0"); }
    }
    for (int i = 0; i < g_naltcell; i++) {
        emit("\t.p2align 2");
        emit(".Lalt%d_%d:\t# ALTERed paragraph's GO TO target", g_unit, g_altcell[i].para);
        if (g_altcell[i].target >= 0) emit("\t.word .Lp%d_%d", g_unit, g_altcell[i].target); else emit("\t.word 0");
    }
    g_naltcell = 0; g_naltname = 0;
    for (int i = g_report_base; i < g_nreport; i++) {
        Report *r = &g_reports[i];
        emit("\t.p2align 2");
        emit(".Lrpt%d_%d:\t# report %s", g_unit, i, r->name);
        emit("\t.word .Lf%d_%d", g_files[r->file].unit, r->file);
        emit("\t.word %d", r->page_limit); emit("\t.word %d", r->heading);
        emit("\t.word %d", r->first_detail); emit("\t.word %d", r->last_detail);
        emit("\t.word 0"); emit("\t.word 0"); emit("\t.word 0");    /* line_counter (20), page_counter (24), body_seen */
        emit("\t.word %d", r->footing); emit("\t.word 0");           /* footing, page_started */
        for (int w = 0; w < 6; w++) emit("\t.word 0");               /* first_gen brk next_line next_page suppress gi_pending */
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
        "  -I dir   where COPY looks for copybooks (repeatable)\n"
        "  -std=85  X3.23-1985 and the 1989 intrinsics; the default\n"
        "  -std=2002 add the COBOL 2002 modules landed so far (docs/standards.md, Stage B)\n"
        "  -fnsig   only write the user functions' .s32fn signature files (docs/functions.md)\n"
        "  -fixed-columns=bytes  count reference-format columns in bytes, not characters (UTF-8 source)\n"
        "  -warn-74 warn where a COBOL 74 program needs updating (docs/behavior-points.md)\n", VERSION);
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
        else if (!strcmp(argv[i], "-warn-74")) g_warn74 = 1;
        else if (!strcmp(argv[i], "-fnsig")) g_fnsig_only = 1;
        else if (!strcmp(argv[i], "-fixed-columns=bytes")) g_col_bytes = 1;
        else if (!strcmp(argv[i], "-fixed-columns=chars")) g_col_bytes = 0;
        else if (!strcmp(argv[i], "-std=85") || !strcmp(argv[i], "-std=cobol85")) g_std = 85;
        else if (!strcmp(argv[i], "-std=2002") || !strcmp(argv[i], "-std=cobol2002")) g_std = 2002;
        else if (!strcmp(argv[i], "-std=74") || !strcmp(argv[i], "-std=cobol74")) {
            fprintf(stderr, "s32-cobc: there is no -std=74: 74 programs compile as 85, and -warn-74 flags where their "
                            "meaning changed; full COBOL 74 is cobc370's job (docs/standards.md)\n");
            return 2;
        }
        else if (!strncmp(argv[i], "-std=", 5)) {
            fprintf(stderr, "s32-cobc: %s is not implemented; -std=85 (the default) and -std=2002 "
                            "(COBOL 2002, Stage B of docs/standards.md, as its modules land)\n", argv[i]);
            return 2;
        }
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

    {   /* the external repository: signature files go beside the output */
        static char od[1024]; snprintf(od, sizeof od, "%s", out);
        char *sl = strrchr(od, '/'); if (sl) { *sl = 0; g_outdir = od; } else g_outdir = ".";
    }
    read_source(in);
    tokenize();
    expand_types();

    if (g_fnsig_only) g_noemit = 1;         /* signatures only: no code, no output file */
    else {
        g_out = fopen(out, "w");
        if (!g_out) { fprintf(stderr, "s32-cobc: cannot write %s\n", out); return 1; }
        g_out_path = out;
    }
    emit("\t.file\t\"%s\"", in);
    emit("# s32-cobc %s", VERSION);

    for (;;) {
        /* one program unit; a source file may hold several, each closed
         * by END PROGRAM */
        g_nsym = 0; g_nfile = 0; g_npara = 0; g_nreport = 0; g_report_base = 0; g_nscreen = 0; g_screen_base = 0; g_nclass = 0; g_nswitch = 0; g_nalphabet = 0; g_nmnemonic = 0; g_last_item = -1;
        g_nsame_groups = 0; g_collate = -1; g_collate_name[0] = 0; g_lowval = 0x00; g_highval = 0xFF; g_cur_fd = -1; g_in_linkage = 0;
        g_sym_base = g_file_base = g_para_base = 0; g_udepth = 0; g_nuse = 0; g_initial = 0; g_recursive = 0; g_nsymch = 0;
        g_in_proc = 0;
        parse_identification_division();
        parse_environment_division();
        parse_data_division();
        if (!at_word("procedure")) die_at(cur()->line, "expected PROCEDURE DIVISION, found %s", tok_desc(cur()));
        parse_procedure_division();
        if (!g_nerrors && !g_fnsig_only) emit_unit_data();   /* nothing is generated once anything has failed */
        if (cur()->kind == T_EOF) break;
        if (!g_saw_end_program) die_at(cur()->line, "unexpected %s after the program (a further program needs END PROGRAM before it)", tok_desc(cur()));
        g_unit = ++g_unit_counter;
    }
    if (g_nerrors) fail();
    if (g_fnsig_only) return 0;
    emit_rodata();
    relax_branches();
    fclose(g_out);
    return 0;
}
