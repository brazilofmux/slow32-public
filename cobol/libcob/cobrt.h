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
