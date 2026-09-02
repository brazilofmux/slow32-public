/* picture.rl -- COBOL PICTURE scanner, Ragel -G2, feeding a hand-written
 * analyser.
 *
 * Re-hosted from ~/cobc370/src/picture.rl (COBOL 74, S/370).  The PICTURE
 * character-string language barely moved between 1974 and 1985, so the
 * tokeniser travels as-is; pic_analyse does not (it emitted ED masks in
 * CP037) and is rewritten in picture.c with a software edit descriptor.
 *
 * The machine only tokenises.  Meaning is assigned in picture.c, so the
 * intricate part -- floating insertion strings, where n sign symbols give
 * n-1 digit positions -- is written in C where it can be read.
 *
 * Build: ./gen_picture.sh   (ragel -G2 -o picture_scan.c picture.rl)
 */
#include <stdlib.h>
#include <string.h>
#include "picture.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
#pragma GCC diagnostic ignored "-Wunused-const-variable"
#endif

%%{
    machine picscan;
    write data;
}%%

/* Tokenise a PICTURE into (symbol, repeat) pairs.
 * Returns the number of items, or -1 with *errpos set to the offending byte.
 * CR and DB collapse to the single symbols 'C' and 'D'. */
int pic_scan(const char *s, PicItem *out, int max, int *errpos)
{
    const char *p = s, *pe = s + strlen(s), *eof = pe;
    const char *ts, *te;
    int cs, act, count = 0;

    *errpos = -1;

    %%{
        picsym = '9' | 'Z' | 'z' | 'X' | 'x' | 'A' | 'a' | 'V' | 'v'
               | 'P' | 'p'
               | 'S' | 's' | '*' | ',' | '.' | '/' | 'B' | 'b' | '0'
               | '+' | '$' | '-';

        action emit_rep {
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = (char)toupper((unsigned char)ts[0]);
            out[count].rep = (int)strtol(ts + 2, NULL, 10);
            if (out[count].rep < 1) { *errpos = (int)(ts - s); return -1; }
            count++;
        }
        action emit_one {
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = (char)toupper((unsigned char)ts[0]);
            out[count].rep = 1;
            count++;
        }
        action emit_cr {
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = 'C'; out[count].rep = 1; count++;
        }
        action emit_db {
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = 'D'; out[count].rep = 1; count++;
        }

        main := |*
            picsym '(' digit+ ')'  => emit_rep;
            ('CR' | 'cr')          => emit_cr;
            ('DB' | 'db')          => emit_db;
            picsym                 => emit_one;
        *|;
    }%%

    %% write init;
    %% write exec;

    (void)act; (void)eof; (void)te;
    if (cs == picscan_error) { *errpos = (int)(p - s); return -1; }
    return count;
}
