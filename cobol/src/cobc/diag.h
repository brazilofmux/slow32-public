/* s32-cobc: diagnostics.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

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
typedef struct { int offset, desc, descending, size; } SortKey;
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
typedef struct { char name[64]; int native, used, ebcdic; unsigned char rank[256]; } Alphabet;   /* ebcdic: ALPHABET ... IS EBCDIC, rank = the CP037 code */
/* ISO 8859-1 to EBCDIC code page 037 (US/Canada), IBM's reference EBCDIC:
 * ALPHABET ... IS EBCDIC collates by it and CODE-SET converts by it.
 * Generated from Python's cp037 codec; a bijection. */
static const unsigned char g_cp037[256] = {
    0x00, 0x01, 0x02, 0x03, 0x37, 0x2D, 0x2E, 0x2F, 0x16, 0x05, 0x25, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
    0x10, 0x11, 0x12, 0x13, 0x3C, 0x3D, 0x32, 0x26, 0x18, 0x19, 0x3F, 0x27, 0x1C, 0x1D, 0x1E, 0x1F,
    0x40, 0x5A, 0x7F, 0x7B, 0x5B, 0x6C, 0x50, 0x7D, 0x4D, 0x5D, 0x5C, 0x4E, 0x6B, 0x60, 0x4B, 0x61,
    0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0x7A, 0x5E, 0x4C, 0x7E, 0x6E, 0x6F,
    0x7C, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6,
    0xD7, 0xD8, 0xD9, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xBA, 0xE0, 0xBB, 0xB0, 0x6D,
    0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96,
    0x97, 0x98, 0x99, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xC0, 0x4F, 0xD0, 0xA1, 0x07,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x15, 0x06, 0x17, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x09, 0x0A, 0x1B,
    0x30, 0x31, 0x1A, 0x33, 0x34, 0x35, 0x36, 0x08, 0x38, 0x39, 0x3A, 0x3B, 0x04, 0x14, 0x3E, 0xFF,
    0x41, 0xAA, 0x4A, 0xB1, 0x9F, 0xB2, 0x6A, 0xB5, 0xBD, 0xB4, 0x9A, 0x8A, 0x5F, 0xCA, 0xAF, 0xBC,
    0x90, 0x8F, 0xEA, 0xFA, 0xBE, 0xA0, 0xB6, 0xB3, 0x9D, 0xDA, 0x9B, 0x8B, 0xB7, 0xB8, 0xB9, 0xAB,
    0x64, 0x65, 0x62, 0x66, 0x63, 0x67, 0x9E, 0x68, 0x74, 0x71, 0x72, 0x73, 0x78, 0x75, 0x76, 0x77,
    0xAC, 0x69, 0xED, 0xEE, 0xEB, 0xEF, 0xEC, 0xBF, 0x80, 0xFD, 0xFE, 0xFB, 0xFC, 0xAD, 0xAE, 0x59,
    0x44, 0x45, 0x42, 0x46, 0x43, 0x47, 0x9C, 0x48, 0x54, 0x51, 0x52, 0x53, 0x58, 0x55, 0x56, 0x57,
    0x8C, 0x49, 0xCD, 0xCE, 0xCB, 0xCF, 0xCC, 0xE1, 0x70, 0xDD, 0xDE, 0xDB, 0xDC, 0x8D, 0x8E, 0xDF,
};   /* rank: the collating position of each character; used: a SORT names it, its table is emitted */
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
 * kind 1 the console for ACCEPT, 2 the console for DISPLAY, 3 a page,
 * 4 the error stream for DISPLAY (SYSERR, STDERR) */
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

/* a warning, always shown: something the program says is not what it
 * gets (a literal cut to its item's length) */
static void warn_at(int line, const char *fmt, ...)
{
    va_list ap;
    fprintf(stderr, "%s:%d: warning: ", diag_file(line), line);
    va_start(ap, fmt); vfprintf(stderr, fmt, ap); va_end(ap);
    fputc('\n', stderr);
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
       BP_E1_RETURN_CODE, BP_E2_GOBACK, BP_E3_COMP_N, BP_E4_VENDOR_BINARY, BP_E5_BINARY_2002,
       BP_E6_STOP_RUN_VALUE, BP_E7_POSITIONED_IO, BP_E8_HEX_LITERAL, BP_E9_CALL_VALUE,
       BP_E10_SCREEN_SECTION, BP_E11_FREE_FORMAT, BP_E12_LINE_SEQUENTIAL, BP_E13_UNDERSCORE, BP_E14_COMPOSITE,
       BP_E15_INIT_ODO, BP_E16_NUMERIC_KEY, BP_E17_NUMERIC_STATUS, BP_E18_NO_ATEND, BP_E19_LINESEQ_CLAUSES,
       BP_E20_LONG_LITERAL, BP_E21_EXIT_PROGRAM_NOT_LAST, BP_E22_SEPARATOR_SPACE, BP_E23_CONDNAME_GROUP,
       BP_E24_COMMENT_ENTRY_2002, BP_E25_CONSTANT_NO_AS, BP_E26_LEVEL_78, BP_E27_TRIM, BP_E28_ANY_LENGTH_OUTER, BP_E29_ROUNDED_MODE, BP_E30_DOLLAR_SET,
       BP_D1_MF_NO_FILE_CONTROL, BP_D2_MF_SPLIT_KEY, BP_D3_MF_STOP_NOT_LAST, BP_D4_MF_EXIT_NOT_ALONE, BP_D5_MF_NO_FILE_SECTION, BP_D6_MF_ASSIGN_IMPLICIT, BP_D7_MF_VALUE_TRUNCATED, BP_E31_ENVIRONMENT, BP_E32_SCREEN_DIMS,
       BP_COUNT };
static const struct { const char *id; char cls; const char *msg; } g_bp[BP_COUNT] = {
    { "BP-M1", 'M', "this AFTER item's FROM reads an outer VARYING item: COBOL 85 augments the outer item before "
                    "resetting this one, COBOL 74 did the reverse, so a 74 program's loop bounds change here" },
    { "BP-M2", 'M', "the receiving group holds an OCCURS DEPENDING ON table whose DEPENDING ON item is inside it, "
                    "and takes its maximum length (COBOL 85); COBOL 74 used the current length" },
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
    { "BP-E1", 'E', "RETURN-CODE is an IBM and Micro Focus special register, not standard COBOL; "
                    "a standard program returns a value through PROCEDURE DIVISION RETURNING (2002)" },
    { "BP-E2", 'E', "GOBACK is COBOL 2002; in a COBOL 85 program the standard ends with EXIT PROGRAM or STOP RUN" },
    { "BP-E3", 'E', "COMP-3, COMP-5 and COMP-1 are implementors' usages, not standard COBOL; "
                    "the standard's are PACKED-DECIMAL and BINARY" },
    { "BP-E4", 'E', "SIGNED-INT, UNSIGNED-INT, SIGNED-SHORT and UNSIGNED-SHORT are GnuCOBOL's; "
                    "the standard's are BINARY-LONG and BINARY-SHORT [SIGNED | UNSIGNED] (2002)" },
    { "BP-E5", 'E', "BINARY-CHAR, BINARY-SHORT, BINARY-LONG and POINTER are COBOL 2002; an extension in a COBOL 85 program" },
    { "BP-E6", 'E', "STOP RUN with an identifier or RETURNING is RM/COBOL and GnuCOBOL; "
                    "the standard form is STOP RUN WITH ERROR | NORMAL STATUS (2002)" },
    { "BP-E7", 'E', "positioned DISPLAY and ACCEPT (LINE, POSITION, AT) are RM/COBOL and Micro Focus; "
                    "the standard positions a screen item in the SCREEN SECTION" },
    { "BP-E8", 'E', "hexadecimal literals (X\"...\") are COBOL 2002; an extension in a COBOL 85 program" },
    { "BP-E9", 'E', "CALL ... BY VALUE and RETURNING are COBOL 2002; an extension in a COBOL 85 program" },
    { "BP-E10", 'E', "the SCREEN SECTION is COBOL 2002; an extension in a COBOL 85 program" },
    { "BP-E11", 'E', "free-form source is COBOL 2002; an extension in a COBOL 85 program" },
    { "BP-E12", 'E', "ORGANIZATION LINE SEQUENTIAL is not in COBOL 85 or 2002 (COBOL 2023 adds it)" },
    { "BP-E13", 'E', "an underscore in a user-defined word is an implementor's extension; the standard's words take letters, digits and hyphens" },
    { "BP-E14", 'E', "the composite of operands is more than 18 digits, which X3.23-1985 forbids; taken here, but the "
                     "arithmetic holds 18 digits, so a value past that would overflow" },
    { "BP-E15", 'E', "INITIALIZE of an item that is or contains an OCCURS DEPENDING ON table, which X3.23-1985 forbids "
                     "(INITIALIZE syntax rule 4); COBOL 2002 allows it, and it is taken" },
    { "BP-E16", 'E', "a RECORD KEY or ALTERNATE RECORD KEY that is not alphanumeric (or national); the standard's keys are, "
                     "and here a numeric key is taken, ordered by its bytes" },
    { "BP-E17", 'E', "a FILE STATUS item that is not alphanumeric; the standard's is PIC XX, and here a two-digit numeric one is taken" },
    { "BP-E18", 'E', "no AT END or INVALID KEY phrase and no USE procedure for the file, which X3.23-1985 requires; "
                     "the condition goes to the FILE STATUS, or stops the run" },
    { "BP-E19", 'E', "RESERVE, BLOCK CONTAINS or RECORD CONTAINS on a LINE SEQUENTIAL file, which 2023 excludes "
                     "(12.4.5.2 rule 12, 13.4.5.3 rule 4); taken, with no effect on the lines" },
    { "BP-E20", 'E', "a literal of more than 160 character positions: X3.23-1985 and 2002 allow 1 through 160 "
                     "(2014 and 2023 allow 8,191); taken" },
    { "BP-E21", 'E', "EXIT PROGRAM followed by more statements in its sentence: X3.23-1985 makes it the last "
                     "(EXIT PROGRAM syntax rule 1), 2002 does not; taken, as 2002 runs it" },
    { "BP-E22", 'E', "a separator comma or semicolon not followed by a space: the standard's separators are followed "
                     "by one (1985 and 2023 reference format); taken as a separator" },
    { "BP-E23", 'E', "a condition-name on a group holding items of a usage other than DISPLAY, or JUSTIFIED or "
                     "SYNCHRONIZED ones (X3.23-1985 VI-21 general rule 2c; 2023 13.16.3 rule 24c and d); taken, the "
                     "group compared as its bytes, as an 88 VALUE HIGH-VALUES end-of-file flag is written" },
    { "BP-E24", 'E', "comment-entries (AUTHOR, DATE-WRITTEN, ...) were deleted by COBOL 2002 (ISO/IEC 1989:2002 F.1); "
                     "taken as comments, as COBOL 85 takes them" },
    { "BP-E25", 'E', "a constant entry without AS (01 name CONSTANT literal) is GnuCOBOL's; the standard writes "
                     "CONSTANT AS literal (2002 13.9; 2023 13.10)" },
    { "BP-E26", 'E', "a level 78 entry is Micro Focus's constant-name; the standard's constant entry is "
                     "01 name CONSTANT AS (2002 13.9; 2023 13.10)" },
    { "BP-E27", 'E', "FUNCTION TRIM is COBOL 2014 (2023 15.96), beyond 1985 and 2002; IBM, Micro Focus and "
                     "GnuCOBOL all have it, and it is taken" },
    { "BP-E28", 'E', "ANY LENGTH in an outermost program, which 2023 13.18.2.3 rule 2 excludes (a plain CALL need not carry "
                     "lengths); Micro Focus and GnuCOBOL take it, and so does this compiler's CALL" },
    { "BP-E29", 'E', "ROUNDED MODE is COBOL 2014 (2023 14.7.4), beyond 1985 and 2002; taken" },
    { "BP-E30", 'E', "a $SET line is Micro Focus's compiler-directive line; SOURCEFORMAT is taken as >>SOURCE FORMAT is, "
                     "listing directives have no effect, and any other is refused" },
    { "BP-D1", 'D', "a file-control entry without the FILE-CONTROL paragraph header (or the INPUT-OUTPUT SECTION header "
                    "above it): Micro Focus's; the standard writes both (2002 12.3, 12.3.3)" },
    { "BP-D2", 'D', "a split key written RECORD KEY IS name = data-name ..., Micro Focus's spelling of 2002's "
                    "SOURCE IS (12.3.4.12), its parts of any category" },
    { "BP-D3", 'D', "STOP RUN followed by more statements of its sentence (X3.23-1985 STOP syntax rule 2; 2002 14.8.38.2 "
                    "rule 1): Micro Focus does not enforce the rule, and what follows it never runs" },
    { "BP-D4", 'D', "EXIT not a sentence by itself, alone in its paragraph (X3.23-1985 EXIT syntax rules 1-2; 2023 14.9.14.3 "
                    "rule 1): Micro Focus does not enforce the rules; such an EXIT does nothing" },
    { "BP-D5", 'D', "file description entries without the FILE SECTION header, first in the DATA DIVISION: Micro Focus "
                    "practice; the standard writes the header (2002 13.3)" },
    { "BP-D6", 'D', "ASSIGN TO a data-name declared nowhere: Micro Focus declares it implicitly, alphanumeric and long "
                    "enough for a file name (its SELECT rule 4); the standard's data-name is declared" },
    { "BP-D7", 'D', "a VALUE literal longer than its alphanumeric item, cut on the right to the item; the "
                    "standard refuses it (X3.23-1985 VALUE syntax rule 3; 2023 13.18.63.3 rule 4), as Micro Focus's reference does" },
    { "BP-E31", 'E', "ENVIRONMENT-NAME, ENVIRONMENT-VALUE and ACCEPT ... FROM ENVIRONMENT are X/Open's and Micro "
                     "Focus's, and SET ENVIRONMENT GnuCOBOL's, not standard COBOL (the standard names devices through SPECIAL-NAMES)" },
    { "BP-E32", 'E', "ACCEPT ... FROM LINES and FROM COLUMNS, the terminal's size, are X/Open's, not standard COBOL" },
};
static int g_warn74;                 /* -warn-74: say where a 74-era program needs updating */
static int g_warn_ext;               /* -warn-extensions: say where a program leaves the standard (class E) */
static int g_dialect_mf;             /* -dialect=mf: Micro Focus's own forms (class D points) are taken */
static void bp(int point, int line)
{
    static int last_point = -1, last_line = -1;
    /* an element COBOL 2002 deleted (its F.1 list): under -std=2002 it is
     * no longer the language, whatever the warnings asked for */
    /* (not BP-O9: 2023 14.9.25.3 rule 5 permits an ALL literal of digits
     * to an integer item again, as an obsolete feature) */
    if (g_std >= 2002 && point >= BP_O1_ALTER && point <= BP_O11_MULTIPLE_FILE && point != BP_O9_ALL_NUMERIC)
        die_at(line, "[%s] %s (ISO/IEC 1989:2002 F.1); under -std=2002 it is refused -- compile with -std=85", g_bp[point].id, g_bp[point].msg);
    /* class D: a dialect's own, taken only under its switch (-dialect=mf) */
    if (g_bp[point].cls == 'D' && !g_dialect_mf)
        die_at(line, "[%s] %s -- compile with -dialect=mf", g_bp[point].id, g_bp[point].msg);
    if (g_bp[point].cls == 'E' || g_bp[point].cls == 'D' ? !g_warn_ext : !g_warn74) return;
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
    if (strchr(w, '_')) bp(BP_E13_UNDERSCORE, line);
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
