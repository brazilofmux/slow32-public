
#line 1 "/Users/sdennis/slow-32/cobol/src/picture.rl"
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


#line 23 "/Users/sdennis/slow-32/cobol/src/picture_scan.c"
static const int picscan_start = 7;
static const int picscan_first_final = 7;
static const int picscan_error = 0;

static const int picscan_en_main = 7;


#line 22 "/Users/sdennis/slow-32/cobol/src/picture.rl"


/* Tokenise a PICTURE into (symbol, repeat) pairs.
 * Returns the number of items, or -1 with *errpos set to the offending byte.
 * CR and DB collapse to the single symbols 'C' and 'D'. */
int pic_scan(const char *s, PicItem *out, int max, int *errpos)
{
    const char *p = s, *pe = s + strlen(s), *eof = pe;
    const char *ts, *te;
    int cs, act, count = 0;

    *errpos = -1;

    
#line 69 "/Users/sdennis/slow-32/cobol/src/picture.rl"


    
#line 50 "/Users/sdennis/slow-32/cobol/src/picture_scan.c"
	{
	cs = picscan_start;
	ts = 0;
	te = 0;
	act = 0;
	}

#line 72 "/Users/sdennis/slow-32/cobol/src/picture.rl"
    
#line 60 "/Users/sdennis/slow-32/cobol/src/picture_scan.c"
	{
	if ( p == pe )
		goto _test_eof;
	switch ( cs )
	{
tr0:
#line 48 "/Users/sdennis/slow-32/cobol/src/picture.rl"
	{{p = ((te))-1;}{
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = (char)toupper((unsigned char)ts[0]);
            out[count].rep = 1;
            count++;
        }}
	goto st7;
tr2:
#line 41 "/Users/sdennis/slow-32/cobol/src/picture.rl"
	{te = p+1;{
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = (char)toupper((unsigned char)ts[0]);
            out[count].rep = (int)strtol(ts + 2, NULL, 10);
            if (out[count].rep < 1) { *errpos = (int)(ts - s); return -1; }
            count++;
        }}
	goto st7;
tr3:
#line 54 "/Users/sdennis/slow-32/cobol/src/picture.rl"
	{te = p+1;{
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = 'C'; out[count].rep = 1; count++;
        }}
	goto st7;
tr5:
#line 58 "/Users/sdennis/slow-32/cobol/src/picture.rl"
	{te = p+1;{
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = 'D'; out[count].rep = 1; count++;
        }}
	goto st7;
tr11:
#line 48 "/Users/sdennis/slow-32/cobol/src/picture.rl"
	{te = p;p--;{
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = (char)toupper((unsigned char)ts[0]);
            out[count].rep = 1;
            count++;
        }}
	goto st7;
st7:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof7;
case 7:
#line 1 "NONE"
	{ts = p;}
#line 116 "/Users/sdennis/slow-32/cobol/src/picture_scan.c"
	switch( (*p) ) {
		case 36: goto tr6;
		case 57: goto tr6;
		case 67: goto st3;
		case 68: goto st4;
		case 80: goto tr6;
		case 83: goto tr6;
		case 86: goto tr6;
		case 88: goto tr6;
		case 90: goto tr6;
		case 99: goto st5;
		case 100: goto st6;
		case 112: goto tr6;
		case 115: goto tr6;
		case 118: goto tr6;
		case 120: goto tr6;
		case 122: goto tr6;
	}
	if ( (*p) < 65 ) {
		if ( 42 <= (*p) && (*p) <= 48 )
			goto tr6;
	} else if ( (*p) > 66 ) {
		if ( 97 <= (*p) && (*p) <= 98 )
			goto tr6;
	} else
		goto tr6;
	goto st0;
st0:
cs = 0;
	goto _out;
tr6:
#line 1 "NONE"
	{te = p+1;}
	goto st8;
st8:
	if ( ++p == pe )
		goto _test_eof8;
case 8:
#line 155 "/Users/sdennis/slow-32/cobol/src/picture_scan.c"
	if ( (*p) == 40 )
		goto st1;
	goto tr11;
st1:
	if ( ++p == pe )
		goto _test_eof1;
case 1:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st2;
	goto tr0;
st2:
	if ( ++p == pe )
		goto _test_eof2;
case 2:
	if ( (*p) == 41 )
		goto tr2;
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st2;
	goto tr0;
st3:
	if ( ++p == pe )
		goto _test_eof3;
case 3:
	if ( (*p) == 82 )
		goto tr3;
	goto st0;
st4:
	if ( ++p == pe )
		goto _test_eof4;
case 4:
	if ( (*p) == 66 )
		goto tr5;
	goto st0;
st5:
	if ( ++p == pe )
		goto _test_eof5;
case 5:
	if ( (*p) == 114 )
		goto tr3;
	goto st0;
st6:
	if ( ++p == pe )
		goto _test_eof6;
case 6:
	if ( (*p) == 98 )
		goto tr5;
	goto st0;
	}
	_test_eof7: cs = 7; goto _test_eof; 
	_test_eof8: cs = 8; goto _test_eof; 
	_test_eof1: cs = 1; goto _test_eof; 
	_test_eof2: cs = 2; goto _test_eof; 
	_test_eof3: cs = 3; goto _test_eof; 
	_test_eof4: cs = 4; goto _test_eof; 
	_test_eof5: cs = 5; goto _test_eof; 
	_test_eof6: cs = 6; goto _test_eof; 

	_test_eof: {}
	if ( p == eof )
	{
	switch ( cs ) {
	case 8: goto tr11;
	case 1: goto tr0;
	case 2: goto tr0;
	}
	}

	_out: {}
	}

#line 73 "/Users/sdennis/slow-32/cobol/src/picture.rl"

    (void)act; (void)eof; (void)te;
    if (cs == picscan_error) { *errpos = (int)(p - s); return -1; }
    return count;
}
