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
enum { COB_ALNUM = 0, COB_ALPHA = 1, COB_ALNUM_ED = 2, COB_NUM = 3, COB_NUM_ED = 4, COB_GROUP = 5 };

/* usage (runtime view: COMP-5 and the C-ABI types are BINARY + NOTRUNC) */
enum { COB_U_DISPLAY = 0, COB_U_BINARY = 1, COB_U_PACKED = 2 };

/* flags */
enum {
    COB_F_SIGNED   = 1,   /* S in the picture, or a signed native type */
    COB_F_SEPLEAD  = 2,   /* sign is a separate leading character (literals) */
    COB_F_SEPTRAIL = 4,   /* SIGN TRAILING SEPARATE */
    COB_F_JUST     = 8,   /* JUSTIFIED RIGHT */
    COB_F_BLANKZ   = 16,  /* BLANK WHEN ZERO */
    COB_F_NOTRUNC  = 32,  /* COMP-5 / C types: full binary capacity, no decimal truncation */
    COB_F_LEAD     = 64   /* SIGN LEADING (not separate): overpunch on the first digit */
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
    unsigned int nl_pending;  /* line sequential: the last record went out without its newline (BEFORE ADVANCING ZERO) */
    char *rbuf;               /* line sequential input: the runtime's read buffer ... */
    unsigned int rpos, rlen;  /* ... the next byte in it, and how many it holds */
} cob_file;

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
} cob_report;

/* a SCREEN SECTION 01: a table of slots (docs/screen.md).  kind: 0 VALUE,
 * 1 FROM, 2 TO, 3 USING.  flags: 1 HIGHLIGHT, 2 UNDERLINE, 4 AUTO,
 * 8 REVERSE-VIDEO. */
enum { COB_SCR_VALUE = 0, COB_SCR_FROM = 1, COB_SCR_TO = 2, COB_SCR_USING = 3 };
enum { COB_SF_HIGHLIGHT = 1, COB_SF_UNDERLINE = 2, COB_SF_AUTO = 4, COB_SF_REVERSE = 8 };

typedef struct {
    unsigned char kind, flags;
    unsigned short line, col;
    unsigned int width;          /* characters painted */
    const char *value;           /* VALUE literal (width bytes) */
    const void *pic;             /* cob_desc of the PICTURE, or 0 */
    void *item;                  /* the FROM/TO/USING item */
    const void *item_desc;
} cob_scr_field;

typedef struct {
    unsigned int nfields;
    unsigned int blank_screen;
    cob_scr_field *fields;
} cob_screen;

typedef struct {
    unsigned char cat;
    unsigned char usage;
    unsigned char digits;
    signed char   scale;
    unsigned char flags;
    unsigned char pad[3];
    unsigned int  size;      /* bytes in storage */
    const char   *pic;       /* flattened PICTURE symbols, or 0 */
} cob_desc;

/* SPECIAL-NAMES DECIMAL-POINT IS COMMA: editing and DISPLAY swap '.' and ',' */
extern int cob_dp_comma;
int cob_set_decimal_point(int comma);
extern int cob_currency;
int cob_set_currency(int c);

#endif
