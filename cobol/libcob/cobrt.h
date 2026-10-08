/* cobrt.h -- the descriptor the compiler builds and the runtime reads.
 *
 * Included by both s32-cobc.c (host, to emit the bytes in this order) and
 * libcob.c (guest, to read them).  cobc370's COBSTR idea: the runtime works
 * in bytes and pictures and knows nothing about the statement that called
 * it.  The struct is laid out for the 32-bit guest: 12 bytes of scalars,
 * then a pointer to the PICTURE symbol string (the edit descriptor).
 */
#ifndef COBRT_H
#define COBRT_H

/* cat */
enum { COB_ALNUM = 0, COB_ALPHA = 1, COB_ALNUM_ED = 2, COB_NUM = 3, COB_NUM_ED = 4, COB_GROUP = 5,
       COB_NATIONAL = 6,     /* national: UTF-16 code units, big-endian (cobol ISSUES-62) */
       COB_BOOLEAN = 7 };    /* boolean: a character 0 or 1 per boolean position (cobol ISSUES-76) */

/* usage (runtime view: COMP-5 and the C-ABI types are BINARY + NOTRUNC) */
enum { COB_U_DISPLAY = 0, COB_U_BINARY = 1, COB_U_PACKED = 2,
       COB_U_NATIONAL = 3,   /* numeric, numeric-edited and boolean USAGE NATIONAL (cobol ISSUES-72) */
       COB_U_BIT = 4,        /* boolean USAGE BIT: size the bits, scale the first bit's place (cobol ISSUES-78) */
       COB_U_FLOAT = 5,      /* COMP-1 / COMP-2, FLOAT-SHORT/-LONG, FLOAT-BINARY-32/64: IEEE single or double, size 4 or 8, the machine's byte order unless COB_F2_BIGEND (docs/usage.md) */
       COB_U_SFLOAT = 6 };   /* the software floating-point formats (2014): IEEE decimal64 (size 8) or decimal128 (16), BID unless COB_F2_DPD; binary128 (16) with COB_F2_FBIN; computed on the wide decimal stack */

/* flags */
enum {
    COB_F_SIGNED   = 1,   /* S in the picture, or a signed native type */
    COB_F_SEPLEAD  = 2,   /* sign is a separate leading character (literals) */
    COB_F_SEPTRAIL = 4,   /* SIGN TRAILING SEPARATE */
    COB_F_JUST     = 8,   /* JUSTIFIED RIGHT */
    COB_F_BLANKZ   = 16,  /* BLANK WHEN ZERO */
    COB_F_NOTRUNC  = 32,  /* COMP-5 / C types: full binary capacity, no decimal truncation */
    COB_F_LEAD     = 64,  /* SIGN LEADING (not separate): overpunch on the first digit */
    COB_F_INTFN    = 128  /* an integer function's result: DISPLAY shows no leading zeros (cobol ISSUES-81) */
};
/* flags2 */
enum {
    COB_F2_BIGEND  = 1,   /* COB_U_BINARY stored big-endian: COMP, BINARY, COMP-X (docs/usage.md) */
    COB_F2_NOSIGN  = 2,   /* COB_U_PACKED with no sign nibble: unsigned COMP-6 */
    COB_F2_TWOSC   = 4,   /* a MOVE's store: a negative value in two's complement, though unsigned (MF: COMP-X) */
    COB_F2_SIZEDIG = 8,   /* a capacity-limited binary whose size error is by its picture's digits (MF: 9(n) COMP-X) */
    COB_F2_DPD     = 16,  /* a COB_U_SFLOAT decimal in the densely-packed-decimal encoding (DECIMAL-ENCODING); BID otherwise */
    COB_F2_FBIN    = 32   /* a COB_U_SFLOAT that is binary128 (FLOAT-BINARY-128), not a decimal format */
};

/* a file, as SELECT/FD described it; built by the compiler in .data */
enum { COB_ORG_LINESEQ = 0, COB_ORG_SEQ = 1, COB_ORG_INDEXED = 2, COB_ORG_RELATIVE = 3, COB_ORG_SORT = 4 };

/* an ALTERNATE RECORD KEY, as the compiler lays a file's key table out in .data */
typedef struct {
    unsigned int offset;      /* in the record */
    unsigned int len;
    unsigned int dups;        /* WITH DUPLICATES */
} cob_altkey;

/* a SORT key, as the compiler lays the statement's key table out in .data */
typedef struct {
    unsigned int offset;      /* in the SD record */
    const void *desc;         /* the key item's cob_desc */
    unsigned int descending;
} cob_sort_key;
enum { COB_OPEN_INPUT = 1, COB_OPEN_OUTPUT = 2, COB_OPEN_IO = 3, COB_OPEN_EXTEND = 4 };

typedef struct {
    unsigned char org, access, optional, open_mode;   /* open_mode: 0 closed */
    void *fp;                 /* FILE* while open */
    char *record;             /* the record area (the FD's first 01) */
    unsigned int recsize;     /* the largest 01 under the FD */
    char *status;             /* FILE STATUS item (2 bytes) or 0 */
    const char *assign;       /* literal name, NUL-terminated, or 0 */
    char *assign_item;        /* ASSIGN TO data-name: its bytes ... */
    unsigned int assign_len;  /* ... and length */
    unsigned int at_eof;
    unsigned int last_len;    /* bytes the last READ delivered */
    unsigned int keyoff, keylen;   /* RECORD KEY: offset in the record, length */
    void *idx;                /* indexed: the in-memory key table while open */
    unsigned int varying;     /* sequential: records carry an IBM RDW (mode V) */
    unsigned int minlen;      /* RECORD CONTAINS m TO n / VARYING FROM m TO n */
    void *dep_item;           /* RECORD IS VARYING ... DEPENDING ON item, or 0 */
    const void *dep_desc;
    void *rel_key;            /* relative: the RELATIVE KEY item ... */
    const void *rel_key_desc; /* ... and its descriptor, or 0 (sequential access may omit it) */
    unsigned int rel_pos;     /* relative: the next record number for READ NEXT / sequential WRITE */
    unsigned int rel_last;    /* relative: record number of the last successful READ, 0 = none */
    int use_para;             /* DECLARATIVES: the USE section for this file (a paragraph id), 0 none */
    const int *use_modes;     /* the unit's USE sections by open mode, indexed by COB_OPEN_ */
    unsigned int open_try;    /* the mode the last OPEN asked for (it may have failed) */
    unsigned int locked;      /* CLOSE WITH LOCK: no further OPEN (38) */
    unsigned int eof_seen;    /* the AT END condition was already reported once (the next READ is 46) */
    unsigned int fpos;        /* sequential: the byte position after the last READ/WRITE (the libc's
                                 buffered stream cannot tell it back reliably) */
    const cob_altkey *altkeys;/* indexed: the ALTERNATE RECORD KEYs ... */
    unsigned int naltkeys;    /* ... and how many */
    const void *linage;       /* FD LINAGE: four of (literal, item, descriptor) -- lines, footing, top, bottom */
    unsigned int lin_lines, lin_foot, lin_top, lin_bot;   /* their values, taken at OPEN and at each new page */
    unsigned int lin_counter; /* LINAGE-COUNTER (offset 136: the compiler reads it as a data item) */
    unsigned int lin_eop;     /* the last WRITE met the footing or overflowed the page */
    unsigned int lin_needs_top;   /* the top margin has not been written yet */
    char *saved_status;       /* EXTERNAL: the entering program's own image keeps the shared connector's previous status item here */
    unsigned int reversed;    /* OPEN INPUT ... REVERSED: fixed-length records read from the last back */
    unsigned int pr_state;    /* line sequential: the printer's cursor (libcob.c, PR_TOP..PR_INK) */
    char *rbuf;               /* line sequential input: the runtime's read buffer ... */
    unsigned int rpos, rlen;  /* ... the next byte in it, and how many it holds */
    const unsigned char *code_out;  /* FD CODE-SET: native to the medium's code (256 bytes), or 0 */
    const unsigned char *code_in;   /* ... and the medium's code back to native */
    const unsigned int *split;      /* indexed: Micro Focus split keys (-dialect=mf), or 0 -- the number of
                                       keys, then for each: its place in the record area's tail, its number
                                       of parts, and each part's offset and length */
    /* The runtime's: what READ and WRITE found out about this file the
     * first time, so that the next four million do not ask again
     * (libcob.c, "the short entries").  All zero while the file is closed.
     * fast_r: a fixed-length sequential file open for input, its records
     * coming out of rbuf with nothing to translate; fast_r1: and they are
     * one byte long.  fast_w, fast_w1: the same for output, the records
     * going into the stream's own buffer.  fast_r, fast_w = 2: a plain
     * line sequential file, whose lines take cob_read_n's and
     * cob_write_n's short paths (2026-10-08). */
    unsigned char fast_r1, fast_r, fast_w1, fast_w;
    unsigned int started;     /* sequential: a START positioned the file (FIRST, LAST): the next READ, NEXT or PREVIOUS, reads the record at fpos (14.9.41 GR 20-21) */
    unsigned int last_st;     /* the last I-O status of this connector, 0x10000 | its two characters; 0 never accessed (FUNCTION EXCEPTION-FILE (file-name), 2023 15.28.4 rule 2) */
    unsigned int share_lock;  /* the file control entry's SHARING (bits 0-3: 0 none, 1 ALL OTHER, 2 NO OTHER, 3 READ ONLY) and LOCK MODE (bits 4-7: 0 none, 1 MANUAL, 2 AUTOMATIC; bit 8 MULTIPLE) -- 2023 12.4.5.15, 12.4.5.9 */
    unsigned int lk_state;    /* the runtime's: the sharing mode this opening took (bits 0-3), the physical file's slot + 1 (bits 8-) */
} cob_file;

/* A dynamic-capacity table (2023 13.18.38 format 4, 8.5.1.9): in its
 * record the entry is this 8-byte slot -- the elements' address and the
 * current capacity -- and the elements live on the heap, one after
 * another, so an element's address is elems + (n - 1) * elem.  A slot
 * whose elems is 0 with a capacity has that many elements still to be
 * made (the initial state: the minimum capacity); cob_dyn_elem makes
 * them.  The descriptor is the compiler's, read-only; busy counts the
 * SEARCH statements under way on the table (EC-FLOW-SEARCH). */
typedef struct { unsigned char *elems; unsigned cap; } cob_dyn;
typedef struct {
    unsigned elem;               /* an element's bytes */
    unsigned min, expected;      /* FROM, TO (0: none) */
    unsigned flags;              /* COB_DYN_INITIALIZED */
    const unsigned char *image;  /* a new element's initial state: INITIALIZE WITH FILLER ALL TO VALUE THEN TO DEFAULT (8.5.1.9.5) */
    const unsigned char *image0; /* ... and the categories' defaults alone, what an INITIALIZE without phrases gives */
    const char *name;            /* for messages */
} cob_dyn_desc;
enum { COB_DYN_INITIALIZED = 1 };
#define COB_DYN_MAX 16777215     /* the implementor's maximum capacity (13.18.38.3 rule 29; A.3 item 60) */
/* cob_dyn_status after cob_dyn_elem or cob_dyn_set: the condition met */
enum { COB_DYN_OK = 0, COB_DYN_SUBSCRIPT = 1, COB_DYN_OVERFLOW = 2, COB_DYN_LIMIT = 3, COB_DYN_SET = 5 };
/* A dynamic-length elementary item (2023 13.18.19, 8.5.1.10): the same
 * slot (cob_dyn: the characters' address, the current length in
 * characters), the characters on the heap; a slot with no address and a
 * length has its VALUE still to be laid down (the initial state). */
typedef struct {
    unsigned limit;              /* LIMIT, in characters (the implementor's maximum when none) */
    unsigned nat;                /* PIC N: two bytes a character, big-endian UTF-16 */
    const unsigned char *value;  /* the VALUE clause's content, or 0 */
    unsigned vlen;               /* ... its bytes */
    const char *name;
} cob_dynl_desc;
#define COB_DYNL_MAX 16777215    /* the implementor's maximum length, characters (13.18.19.4 rule 2) */
/* the phrases of one I-O statement (2023 14.9.30 formats, 14.7.9 RETRY,
 * 14.9.27 SHARING): set by the compiler just before the call, read and
 * cleared by it (cob_io_set) */
enum { COB_IO_LOCK = 1, COB_IO_NOLOCK = 2, COB_IO_IGNORE_LOCK = 4, COB_IO_ADV_LOCK = 8, COB_IO_SHARE_SHIFT = 8 };


/* FUNCTION: the numeric intrinsics cob_fn_num computes over the top n
 * of the numeric stack.  Integer-class results have scale 0, the rest
 * scale 9; every result is a sign and 18 digits (SIGN LEADING SEPARATE). */
enum {
    COB_FN_MAX = 1, COB_FN_MIN, COB_FN_ORD_MAX, COB_FN_ORD_MIN, COB_FN_SUM,
    COB_FN_RANGE, COB_FN_MIDRANGE, COB_FN_MEAN, COB_FN_MEDIAN,
    COB_FN_VARIANCE, COB_FN_STDDEV, COB_FN_MOD, COB_FN_REM,
    COB_FN_INTEGER, COB_FN_INTEGER_PART, COB_FN_FACTORIAL,
    COB_FN_SQRT, COB_FN_LOG, COB_FN_LOG10, COB_FN_SIN, COB_FN_COS,
    COB_FN_TAN, COB_FN_ASIN, COB_FN_ACOS, COB_FN_ATAN,
    COB_FN_ANNUITY, COB_FN_PRESENT_VALUE, COB_FN_RANDOM,
    /* COBOL 2002 */
    COB_FN_ABS, COB_FN_EXP, COB_FN_EXP10, COB_FN_PI, COB_FN_SIGN, COB_FN_FRACTION_PART,
    COB_FN_YEAR_TO_YYYY, COB_FN_DATE_TO_YYYYMMDD, COB_FN_DAY_TO_YYYYDDD,
    COB_FN_TEST_DATE_YYYYMMDD, COB_FN_TEST_DAY_YYYYDDD, COB_FN_E,
    /* COBOL 2014 */
    COB_FN_COMBINED_DATETIME, COB_FN_SECONDS_PAST_MIDNIGHT
};

/* a report (RD), as the compiler described it; the counters are the
 * runtime's.  Lines are rendered into a buffer and written through the
 * report's print file, one line-sequential record per physical line. */
typedef struct {
    cob_file *file;
    int page_limit, heading, first_detail, last_detail;
    int line_counter, page_counter;
    int body_seen;            /* a body group has been presented on this page */
    int footing;              /* RD FOOTING: the last line a body group may use (= LAST DETAIL when absent) */
    int page_started;         /* the first GENERATE has begun a page (PAGE-COUNTER is 1 from INITIATE) */
    int first_gen;            /* (40) the first GENERATE has run: controls saved, RH presented */
    int brk;                  /* (44) the level of the current control break; 0 none, 1 most major */
    int next_line;            /* (48) NEXT GROUP integer saved for the next page's body group */
    int next_page;            /* (52) NEXT GROUP NEXT PAGE (or an integer that did not fit) */
    int suppress;             /* (56) SUPPRESS PRINTING from a USE BEFORE REPORTING procedure */
    int gi_pending;           /* (60) GROUP INDICATE: bit k set = group k presents its indicated fields */
    int active;               /* (64) INITIATEd and not yet TERMINATEd (EC-REPORT-ACTIVE, -INACTIVE, -NOT-TERMINATED) */
} cob_report;

/* a SCREEN SECTION 01: a table of slots (docs/screen.md).  kind: 0 VALUE,
 * 1 FROM, 2 TO, 3 USING.  flags: 1 HIGHLIGHT, 2 UNDERLINE, 4 AUTO,
 * 8 REVERSE-VIDEO, 16 SECURE, 32 REQUIRED, 64 FULL, 128 LOWLIGHT.
 * fg, bg: FOREGROUND-COLOR / BACKGROUND-COLOR 0-7, 255 when not given. */
enum { COB_SCR_VALUE = 0, COB_SCR_FROM = 1, COB_SCR_TO = 2, COB_SCR_USING = 3 };
enum { COB_SF_HIGHLIGHT = 1, COB_SF_UNDERLINE = 2, COB_SF_AUTO = 4, COB_SF_REVERSE = 8,
       COB_SF_SECURE = 16, COB_SF_REQUIRED = 32, COB_SF_FULL = 64, COB_SF_LOWLIGHT = 128 };

/* ext: RM/COBOL's positioned DISPLAY/ACCEPT (GitHub #32/#33), lowered to a
 * one-statement screen.  POS: line 0 is the line after the last positioned
 * statement, col 0 is column 1; CONT: this slot follows the one painted
 * before it; PROMPT: an input slot shows `prompt` where it holds a space;
 * ERASE_*: clear before painting; NOBEEP: no bell on a rejected key. */
enum { COB_SX_POS = 1, COB_SX_PROMPT = 2, COB_SX_ERASE_EOS = 4, COB_SX_ERASE_EOL = 8,
       COB_SX_ERASE_ALL = 16, COB_SX_NOBEEP = 32, COB_SX_CONT = 64 };
enum { COB_SR_DYNLEN = 1, COB_SR_DISPVAL = 2, COB_SR_BLINK = 4, COB_SR_BELL = 8, COB_SR_DYNSIZE = 16,
       COB_SR_FROMTO = 32, COB_SR_BLANK_LINE = 64, COB_SR_BLANK_SCREEN = 128 };   /* FROMTO: a TO slot whose initial content is the FROM slot's before it (FROM x TO y, 13.17.2); BLANK_LINE: the line cleared before the field is painted on a DISPLAY (13.18.7.3 rule 1); BLANK_SCREEN: the entry that carries BLANK SCREEN, its colours the screen's defaults (rules 3-4) */   /* DISPVAL: a FROM slot shows its item as a plain DISPLAY would; BLINK, BELL: those clauses; DYNSIZE: DYNLEN under SIZE -- the width is SIZE's, the part's length in the value word */

typedef struct {
    unsigned char kind, flags;
    unsigned short line, col;
    unsigned char fg, bg;
    unsigned int width;          /* characters painted */
    const char *value;           /* VALUE literal (width bytes) */
    const void *pic;             /* cob_desc of the PICTURE, or 0 */
    void *item;                  /* the FROM/TO/USING item */
    const void *item_desc;
    unsigned char ext, prompt;   /* COB_SX_* bits; the PROMPT character */
    unsigned short rsv;          /* COB_SR_DYNLEN: a part of computed length, its bytes the field (width set by the statement) */
} cob_scr_field;                 /* 32 bytes on the guest: the compiler lays slots out by that */

void cob_scr_at(cob_scr_field *f, int rrcc);   /* AT rrcc from an identifier: line rr, column cc */

typedef struct {
    unsigned int nfields;
    unsigned int blank_screen;   /* BLANK SCREEN: 1 clears on a DISPLAY; 2 (a dialect's count, -dialect=gnucobol/mf) on an ACCEPT as well -- 2023 13.18.7.3 rule 5 ignores it there */
    cob_scr_field *fields;
    int line_off, col_off;       /* ACCEPT/DISPLAY screen-name AT LINE l COLUMN c (14.9.1.2 format 4, 14.9.11.2 format 2): the screen record placed at (l, c), as offsets from (1, 1); the statement stores them */
} cob_screen;
/* what a screen statement found (2023 9.2.x; EC-SCREEN): bits the compiler
 * raises as conditions when they are checked, and DISPLAY's ON EXCEPTION */
enum { COB_SCR_E_STARTING_COLUMN = 1, COB_SCR_E_LINE_NUMBER = 2, COB_SCR_E_FIELD_OVERLAP = 4, COB_SCR_E_ITEM_VALUE = 8 };

typedef struct {
    unsigned char cat;
    unsigned char usage;
    unsigned char digits;
    signed char   scale;
    unsigned char flags;
    unsigned char flags2;    /* COB_F2_*: the flags byte is full */
    unsigned char pad[2];
    unsigned int  size;      /* bytes in storage */
    const char   *pic;       /* flattened PICTURE symbols, or 0 */
} cob_desc;

/* SPECIAL-NAMES DECIMAL-POINT IS COMMA: editing and DISPLAY swap '.' and ',' */
extern int cob_dp_comma;
int cob_set_decimal_point(int comma);
extern int cob_currency;
extern char cob_currency_str[32]; extern int cob_currency_len;   /* CURRENCY SIGN IS literal WITH PICTURE SYMBOL: the string (2023 12.3.7) */
int cob_set_currency(int c);

#endif
