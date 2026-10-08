
#line 1 "picture.rl"
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


#line 28 "picture_scan.c"
static const int picscan_start = 7;
static const int picscan_first_final = 7;
static const int picscan_error = 0;

static const int picscan_en_main = 7;


#line 34 "picture.rl"


/* Tokenise a PICTURE into (symbol, repeat) pairs.
 * Returns the number of items, or -1 with *errpos set to the offending byte.
 * CR and DB collapse to the single symbols 'C' and 'D'. */
int pic_scan(const char *s0, PicItem *out, int max, int *errpos)
{
    /* the interface is char, as its callers' strings are; the machine reads bytes */
    const unsigned char *s = (const unsigned char *)s0;
    const unsigned char *p = s, *pe = s + strlen(s0), *eof = pe;
    const unsigned char *ts, *te;
    int cs, act, count = 0;

    *errpos = -1;

    
#line 83 "picture.rl"


    
#line 57 "picture_scan.c"
	{
	cs = picscan_start;
	ts = 0;
	te = 0;
	act = 0;
	}

#line 86 "picture.rl"
    
#line 67 "picture_scan.c"
	{
	if ( p == pe )
		goto _test_eof;
	switch ( cs )
	{
tr0:
#line 62 "picture.rl"
	{{p = ((te))-1;}{
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = (char)toupper(ts[0]);
            out[count].rep = 1;
            count++;
        }}
	goto st7;
tr2:
#line 55 "picture.rl"
	{te = p+1;{
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = (char)toupper(ts[0]);
            out[count].rep = (int)strtol((const char *)ts + 2, NULL, 10);
            if (out[count].rep < 1) { *errpos = (int)(ts - s); return -1; }
            count++;
        }}
	goto st7;
tr3:
#line 68 "picture.rl"
	{te = p+1;{
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = 'C'; out[count].rep = 1; count++;
        }}
	goto st7;
tr5:
#line 72 "picture.rl"
	{te = p+1;{
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = 'D'; out[count].rep = 1; count++;
        }}
	goto st7;
tr11:
#line 62 "picture.rl"
	{te = p;p--;{
            if (count >= max) { *errpos = (int)(ts - s); return -1; }
            out[count].sym = (char)toupper(ts[0]);
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
#line 123 "picture_scan.c"
	switch( (*p) ) {
		case 36u: goto tr6;
		case 57u: goto tr6;
		case 67u: goto st3;
		case 68u: goto st4;
		case 80u: goto tr6;
		case 83u: goto tr6;
		case 86u: goto tr6;
		case 88u: goto tr6;
		case 90u: goto tr6;
		case 99u: goto st5;
		case 100u: goto st6;
		case 112u: goto tr6;
		case 115u: goto tr6;
		case 118u: goto tr6;
		case 120u: goto tr6;
		case 122u: goto tr6;
	}
	if ( (*p) < 65u ) {
		if ( 42u <= (*p) && (*p) <= 48u )
			goto tr6;
	} else if ( (*p) > 66u ) {
		if ( 97u <= (*p) && (*p) <= 98u )
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
#line 162 "picture_scan.c"
	if ( (*p) == 40u )
		goto st1;
	goto tr11;
st1:
	if ( ++p == pe )
		goto _test_eof1;
case 1:
	if ( 48u <= (*p) && (*p) <= 57u )
		goto st2;
	goto tr0;
st2:
	if ( ++p == pe )
		goto _test_eof2;
case 2:
	if ( (*p) == 41u )
		goto tr2;
	if ( 48u <= (*p) && (*p) <= 57u )
		goto st2;
	goto tr0;
st3:
	if ( ++p == pe )
		goto _test_eof3;
case 3:
	if ( (*p) == 82u )
		goto tr3;
	goto st0;
st4:
	if ( ++p == pe )
		goto _test_eof4;
case 4:
	if ( (*p) == 66u )
		goto tr5;
	goto st0;
st5:
	if ( ++p == pe )
		goto _test_eof5;
case 5:
	if ( (*p) == 114u )
		goto tr3;
	goto st0;
st6:
	if ( ++p == pe )
		goto _test_eof6;
case 6:
	if ( (*p) == 98u )
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

#line 87 "picture.rl"

    (void)act; (void)eof; (void)te;
    if (cs == picscan_error) { *errpos = (int)(p - s); return -1; }
    return count;
}
