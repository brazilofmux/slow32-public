/* COBOL PICTURE: scanner interface and the analysed result.
 *
 * The scanner (picture_scan.c, from picture.rl) tokenises.  pic_analyse
 * assigns meaning: category, digit count, scale, sign, byte width, and the
 * flattened symbol string that is the *software edit descriptor* -- the
 * runtime edits by walking it, where cobc370 handed the S/370 ED
 * instruction a mask.  No target bytes live in here.
 */
#ifndef PICTURE_H
#define PICTURE_H
#include <ctype.h>

typedef struct { char sym; int rep; } PicItem;

#define PIC_MAXITEM 64
#define PIC_MAXPAT  256

enum {
    PIC_ALPHABETIC,           /* A only                       */
    PIC_ALPHANUMERIC,         /* X, or A and X                */
    PIC_ALPHANUMERIC_EDITED,  /* A/X with B, 0, /             */
    PIC_NUMERIC,              /* 9, S, V, P                   */
    PIC_NUMERIC_EDITED        /* anything else numeric        */
};

typedef struct {
    int  category;    /* PIC_* above */
    int  digits;      /* digit positions (P counts, V does not) */
    int  scale;       /* digits right of the assumed/actual point;
                         negative for trailing P */
    int  is_signed;   /* S, or a sign symbol in an edited picture */
    int  edited;      /* category is one of the *_EDITED ones */
    int  bytes;       /* character positions the item occupies (DISPLAY) */
    char floating;    /* '+', '-' or '$' when a floating string is present */
    char pat[PIC_MAXPAT]; /* flattened symbols: the edit descriptor.  V, S
                             and P are kept so the editor knows where the
                             point is; CR/DB are 'C'/'D'. */
    int  patlen;
    char err[128];    /* set when the picture cannot be handled */
} PicInfo;

int pic_scan(const char *s, PicItem *out, int max, int *errpos);
int pic_analyse(const char *s, PicInfo *info);   /* 0 ok, -1 with info->err */
const char *pic_category_name(int category);

#endif
