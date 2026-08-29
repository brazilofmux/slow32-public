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
    COB_F_NOTRUNC  = 32   /* COMP-5 / C types: full binary capacity, no decimal truncation */
};

/* a file, as SELECT/FD described it; built by the compiler in .data */
enum { COB_ORG_LINESEQ = 0, COB_ORG_SEQ = 1, COB_ORG_INDEXED = 2, COB_ORG_RELATIVE = 3 };
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
} cob_file;

/* a report (RD), as the compiler described it; the counters are the
 * runtime's.  Lines are rendered into a buffer and written through the
 * report's print file, one line-sequential record per physical line. */
typedef struct {
    cob_file *file;
    int page_limit, heading, first_detail, last_detail;
    int line_counter, page_counter;
    int body_seen;            /* a body group has been presented on this page */
} cob_report;

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

#endif
