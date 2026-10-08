
#line 1 "lex.rl"
/* lex.rl -- the COBOL token scanner, Ragel -G2 (docs/plans/standard-queue.md
 * item 15, ruled 2026-10-06): one grammar for 8.3's lexical elements that
 * both the text-word scanner of copy.h (COPY and REPLACE work on
 * text-words, 7.2) and the token scanner of tokenizer.h read through.
 * Until then the two scanned by hand, each its own copy of the rules.
 *
 * The machine knows one line of source at a time -- the reference format,
 * continuation and comment lines are reader.h's -- and returns one lexeme
 * from a given position: the finest division (a word, a number with its
 * sign and exponent, a literal with its prefix, each punctuation).  What
 * is context -- a PICTURE after PIC, EXEC SQL text, the boundary that
 * lets a sign or a leading point begin a number, DECIMAL-POINT IS
 * COMMA's swap -- stays with the callers, as the ruling has it.  The
 * callers differ in how they assemble: the text-word scanner runs
 * adjacent lexemes together between separators, the tokenizer keeps them
 * apart.
 *
 * Build: ./gen_lex.sh   (ragel -G2 -o lex_scan.c lex.rl); the output is
 * checked in, so the build needs no ragel. */
#include <string.h>
#include "lex.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
#pragma GCC diagnostic ignored "-Wunused-const-variable"
#endif


#line 32 "lex_scan.c"
static const int lexscan_start = 8;
static const int lexscan_first_final = 8;
static const int lexscan_error = -1;

static const int lexscan_en_main = 8;


#line 31 "lex.rl"


/* is the text at q a separator's tail: the line's end, a space, a tab, or
 * a closing pseudo-text delimiter (==... PIC 9(5).==) */
static int lx_sep_tail(const char *q, const char *pe)
{
    return q >= pe || *q == ' ' || *q == '\t' || (q + 1 < pe && q[0] == '=' && q[1] == '=');
}

/* the lexeme at p (p < pe): kind, text and length; the literal's prefix
 * length; whether a number carries an exponent.  Every byte is some
 * lexeme (LX_OTHER when nothing else), so the callers always advance. */
int lx_next(const char *p0, const char *pe, Lexeme *out)
{
    const char *p = p0, *eof = pe;
    const char *ts, *te;
    int cs, act;
    memset(out, 0, sizeof *out);
    out->s = p0; out->len = 1; out->kind = LX_OTHER;

    
#line 124 "lex.rl"


    
#line 66 "lex_scan.c"
	{
	cs = lexscan_start;
	ts = 0;
	te = 0;
	act = 0;
	}

#line 127 "lex.rl"
    
#line 76 "lex_scan.c"
	{
	if ( p == pe )
		goto _test_eof;
	switch ( cs )
	{
tr0:
#line 1 "NONE"
	{	switch( act ) {
	case 11:
	{{p = ((te))-1;}
            out->kind = LX_NUM; out->len = (int)(te - ts);
            for (const char *q = ts; q < te; q++) if (*q == 'e' || *q == 'E') { out->exp = 1; break; }
            {p++; cs = 8; goto _out;}
        }
	break;
	case 12:
	{{p = ((te))-1;} out->kind = LX_WORD; out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }
	break;
	case 13:
	{{p = ((te))-1;} out->kind = LX_OP;   out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }
	break;
	case 14:
	{{p = ((te))-1;} out->kind = LX_OTHER; out->len = 1; {p++; cs = 8; goto _out;} }
	break;
	}
	}
	goto st8;
tr4:
#line 86 "lex.rl"
	{{p = ((te))-1;}{
            out->kind = LX_LIT; out->len = (int)(te - ts);
            const char *q = ts; while (*q != '"' && *q != '\'') q++;
            out->prefix = (int)(q - ts);
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr10:
#line 98 "lex.rl"
	{{p = ((te))-1;}{
            out->kind = LX_NUM; out->len = (int)(te - ts);
            for (const char *q = ts; q < te; q++) if (*q == 'e' || *q == 'E') { out->exp = 1; break; }
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr13:
#line 105 "lex.rl"
	{te = p+1;{ out->kind = LX_OTHER; out->len = 1; {p++; cs = 8; goto _out;} }}
	goto st8;
tr19:
#line 104 "lex.rl"
	{te = p+1;{ out->kind = LX_OP;   out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }}
	goto st8;
tr21:
#line 69 "lex.rl"
	{te = p+1;{ out->kind = LX_LP;    out->len = 1; {p++; cs = 8; goto _out;} }}
	goto st8;
tr22:
#line 70 "lex.rl"
	{te = p+1;{ out->kind = LX_RP;    out->len = 1; {p++; cs = 8; goto _out;} }}
	goto st8;
tr25:
#line 81 "lex.rl"
	{te = p+1;{
            /* a comma or semicolon separates before a space; otherwise it
             * is tight to what follows (the tokenizer says what that means) */
            out->kind = lx_sep_tail(te, pe) ? LX_SEP : LX_COMMA; out->len = 1; {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr28:
#line 71 "lex.rl"
	{te = p+1;{ out->kind = LX_COLON; out->len = 1; {p++; cs = 8; goto _out;} }}
	goto st8;
tr34:
#line 105 "lex.rl"
	{te = p;p--;{ out->kind = LX_OTHER; out->len = 1; {p++; cs = 8; goto _out;} }}
	goto st8;
tr35:
#line 103 "lex.rl"
	{te = p;p--;{ out->kind = LX_WORD; out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }}
	goto st8;
tr37:
#line 66 "lex.rl"
	{te = p;p--;{ out->kind = LX_SPACE;   out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }}
	goto st8;
tr38:
#line 92 "lex.rl"
	{te = p;p--;{
            out->kind = LX_LIT; out->len = (int)(te - ts); out->bad = 1;
            const char *q = ts; while (*q != '"' && *q != '\'') q++;
            out->prefix = (int)(q - ts);
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr39:
#line 86 "lex.rl"
	{te = p;p--;{
            out->kind = LX_LIT; out->len = (int)(te - ts);
            const char *q = ts; while (*q != '"' && *q != '\'') q++;
            out->prefix = (int)(q - ts);
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr40:
#line 104 "lex.rl"
	{te = p;p--;{ out->kind = LX_OP;   out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }}
	goto st8;
tr42:
#line 67 "lex.rl"
	{te = p;p--;{ out->kind = LX_COMMENT; out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }}
	goto st8;
tr45:
#line 98 "lex.rl"
	{te = p;p--;{
            out->kind = LX_NUM; out->len = (int)(te - ts);
            for (const char *q = ts; q < te; q++) if (*q == 'e' || *q == 'E') { out->exp = 1; break; }
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr47:
#line 72 "lex.rl"
	{te = p;p--;{
            /* a period separates before a space, the line's end or ==; a
             * doubled one is one separator (RM's reader let "12370121.."
             * through); otherwise it is a period inside something */
            if (lx_sep_tail(te, pe)) { out->kind = LX_PERIOD; out->len = (int)(te - ts); }
            else if (te - ts == 2 && lx_sep_tail(ts + 1, pe)) { out->kind = LX_PERIOD; out->len = 1; }
            else { out->kind = LX_DOT; out->len = 1; }
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr48:
#line 72 "lex.rl"
	{te = p+1;{
            /* a period separates before a space, the line's end or ==; a
             * doubled one is one separator (RM's reader let "12370121.."
             * through); otherwise it is a period inside something */
            if (lx_sep_tail(te, pe)) { out->kind = LX_PERIOD; out->len = (int)(te - ts); }
            else if (te - ts == 2 && lx_sep_tail(ts + 1, pe)) { out->kind = LX_PERIOD; out->len = 1; }
            else { out->kind = LX_DOT; out->len = 1; }
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr49:
#line 68 "lex.rl"
	{te = p+1;{ out->kind = LX_PDELIM;  out->len = 2; {p++; cs = 8; goto _out;} }}
	goto st8;
st8:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof8;
case 8:
#line 1 "NONE"
	{ts = p;}
#line 231 "lex_scan.c"
	switch( (*p) ) {
		case 9: goto st13;
		case 32: goto st13;
		case 34: goto st14;
		case 39: goto st16;
		case 40: goto tr21;
		case 41: goto tr22;
		case 42: goto st18;
		case 43: goto tr24;
		case 44: goto tr25;
		case 45: goto tr24;
		case 46: goto st23;
		case 58: goto tr28;
		case 59: goto tr25;
		case 60: goto st25;
		case 61: goto st26;
		case 62: goto st27;
		case 66: goto tr32;
		case 71: goto tr32;
		case 78: goto tr32;
		case 85: goto tr33;
		case 88: goto tr33;
		case 90: goto tr33;
		case 98: goto tr32;
		case 103: goto tr32;
		case 110: goto tr32;
		case 117: goto tr33;
		case 120: goto tr33;
		case 122: goto tr33;
	}
	if ( (*p) < 38 ) {
		if ( (*p) < -32 ) {
			if ( -62 <= (*p) && (*p) <= -33 )
				goto st9;
		} else if ( (*p) > -17 ) {
			if ( -16 <= (*p) && (*p) <= -12 )
				goto tr16;
		} else
			goto tr15;
	} else if ( (*p) > 47 ) {
		if ( (*p) < 65 ) {
			if ( 48 <= (*p) && (*p) <= 57 )
				goto tr27;
		} else if ( (*p) > 89 ) {
			if ( 97 <= (*p) && (*p) <= 121 )
				goto tr1;
		} else
			goto tr1;
	} else
		goto tr19;
	goto tr13;
st9:
	if ( ++p == pe )
		goto _test_eof9;
case 9:
	if ( (*p) <= -65 )
		goto tr1;
	goto tr34;
tr1:
#line 1 "NONE"
	{te = p+1;}
#line 103 "lex.rl"
	{act = 12;}
	goto st10;
st10:
	if ( ++p == pe )
		goto _test_eof10;
case 10:
#line 300 "lex_scan.c"
	switch( (*p) ) {
		case 45: goto tr1;
		case 95: goto tr1;
	}
	if ( (*p) < -16 ) {
		if ( (*p) > -33 ) {
			if ( -32 <= (*p) && (*p) <= -17 )
				goto st1;
		} else if ( (*p) >= -62 )
			goto st0;
	} else if ( (*p) > -12 ) {
		if ( (*p) < 65 ) {
			if ( 48 <= (*p) && (*p) <= 57 )
				goto tr1;
		} else if ( (*p) > 90 ) {
			if ( 97 <= (*p) && (*p) <= 122 )
				goto tr1;
		} else
			goto tr1;
	} else
		goto st2;
	goto tr35;
st0:
	if ( ++p == pe )
		goto _test_eof0;
case 0:
	if ( (*p) <= -65 )
		goto tr1;
	goto tr0;
st1:
	if ( ++p == pe )
		goto _test_eof1;
case 1:
	if ( (*p) <= -65 )
		goto st0;
	goto tr0;
st2:
	if ( ++p == pe )
		goto _test_eof2;
case 2:
	if ( (*p) <= -65 )
		goto st1;
	goto tr0;
tr15:
#line 1 "NONE"
	{te = p+1;}
#line 105 "lex.rl"
	{act = 14;}
	goto st11;
st11:
	if ( ++p == pe )
		goto _test_eof11;
case 11:
#line 354 "lex_scan.c"
	if ( (*p) <= -65 )
		goto st0;
	goto tr34;
tr16:
#line 1 "NONE"
	{te = p+1;}
#line 105 "lex.rl"
	{act = 14;}
	goto st12;
st12:
	if ( ++p == pe )
		goto _test_eof12;
case 12:
#line 368 "lex_scan.c"
	if ( (*p) <= -65 )
		goto st1;
	goto tr34;
st13:
	if ( ++p == pe )
		goto _test_eof13;
case 13:
	switch( (*p) ) {
		case 9: goto st13;
		case 32: goto st13;
	}
	goto tr37;
st14:
	if ( ++p == pe )
		goto _test_eof14;
case 14:
	if ( (*p) == 34 )
		goto tr6;
	goto st14;
tr6:
#line 1 "NONE"
	{te = p+1;}
	goto st15;
st15:
	if ( ++p == pe )
		goto _test_eof15;
case 15:
#line 396 "lex_scan.c"
	if ( (*p) == 34 )
		goto st3;
	goto tr39;
st3:
	if ( ++p == pe )
		goto _test_eof3;
case 3:
	if ( (*p) == 34 )
		goto tr6;
	goto st3;
st16:
	if ( ++p == pe )
		goto _test_eof16;
case 16:
	if ( (*p) == 39 )
		goto tr8;
	goto st16;
tr8:
#line 1 "NONE"
	{te = p+1;}
	goto st17;
st17:
	if ( ++p == pe )
		goto _test_eof17;
case 17:
#line 422 "lex_scan.c"
	if ( (*p) == 39 )
		goto st4;
	goto tr39;
st4:
	if ( ++p == pe )
		goto _test_eof4;
case 4:
	if ( (*p) == 39 )
		goto tr8;
	goto st4;
st18:
	if ( ++p == pe )
		goto _test_eof18;
case 18:
	switch( (*p) ) {
		case 42: goto tr19;
		case 62: goto st19;
	}
	goto tr40;
st19:
	if ( ++p == pe )
		goto _test_eof19;
case 19:
	goto st19;
tr24:
#line 1 "NONE"
	{te = p+1;}
#line 104 "lex.rl"
	{act = 13;}
	goto st20;
tr44:
#line 1 "NONE"
	{te = p+1;}
#line 98 "lex.rl"
	{act = 11;}
	goto st20;
st20:
	if ( ++p == pe )
		goto _test_eof20;
case 20:
#line 463 "lex_scan.c"
	if ( (*p) == 46 )
		goto st5;
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr44;
	goto tr0;
st5:
	if ( ++p == pe )
		goto _test_eof5;
case 5:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr9;
	goto tr0;
tr9:
#line 1 "NONE"
	{te = p+1;}
	goto st21;
st21:
	if ( ++p == pe )
		goto _test_eof21;
case 21:
#line 484 "lex_scan.c"
	switch( (*p) ) {
		case 69: goto st6;
		case 101: goto st6;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr9;
	goto tr45;
st6:
	if ( ++p == pe )
		goto _test_eof6;
case 6:
	switch( (*p) ) {
		case 43: goto st7;
		case 45: goto st7;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st22;
	goto tr10;
st7:
	if ( ++p == pe )
		goto _test_eof7;
case 7:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st22;
	goto tr10;
st22:
	if ( ++p == pe )
		goto _test_eof22;
case 22:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st22;
	goto tr45;
st23:
	if ( ++p == pe )
		goto _test_eof23;
case 23:
	if ( (*p) == 46 )
		goto tr48;
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr9;
	goto tr47;
tr27:
#line 1 "NONE"
	{te = p+1;}
#line 98 "lex.rl"
	{act = 11;}
	goto st24;
st24:
	if ( ++p == pe )
		goto _test_eof24;
case 24:
#line 536 "lex_scan.c"
	switch( (*p) ) {
		case 45: goto tr1;
		case 46: goto st5;
		case 95: goto tr1;
	}
	if ( (*p) < -16 ) {
		if ( (*p) > -33 ) {
			if ( -32 <= (*p) && (*p) <= -17 )
				goto st1;
		} else if ( (*p) >= -62 )
			goto st0;
	} else if ( (*p) > -12 ) {
		if ( (*p) < 65 ) {
			if ( 48 <= (*p) && (*p) <= 57 )
				goto tr27;
		} else if ( (*p) > 90 ) {
			if ( 97 <= (*p) && (*p) <= 122 )
				goto tr1;
		} else
			goto tr1;
	} else
		goto st2;
	goto tr45;
st25:
	if ( ++p == pe )
		goto _test_eof25;
case 25:
	if ( 61 <= (*p) && (*p) <= 62 )
		goto tr19;
	goto tr40;
st26:
	if ( ++p == pe )
		goto _test_eof26;
case 26:
	if ( (*p) == 61 )
		goto tr49;
	goto tr40;
st27:
	if ( ++p == pe )
		goto _test_eof27;
case 27:
	if ( (*p) == 61 )
		goto tr19;
	goto tr40;
tr32:
#line 1 "NONE"
	{te = p+1;}
#line 103 "lex.rl"
	{act = 12;}
	goto st28;
st28:
	if ( ++p == pe )
		goto _test_eof28;
case 28:
#line 591 "lex_scan.c"
	switch( (*p) ) {
		case 34: goto st14;
		case 39: goto st16;
		case 45: goto tr1;
		case 88: goto tr33;
		case 95: goto tr1;
		case 120: goto tr33;
	}
	if ( (*p) < -16 ) {
		if ( (*p) > -33 ) {
			if ( -32 <= (*p) && (*p) <= -17 )
				goto st1;
		} else if ( (*p) >= -62 )
			goto st0;
	} else if ( (*p) > -12 ) {
		if ( (*p) < 65 ) {
			if ( 48 <= (*p) && (*p) <= 57 )
				goto tr1;
		} else if ( (*p) > 90 ) {
			if ( 97 <= (*p) && (*p) <= 122 )
				goto tr1;
		} else
			goto tr1;
	} else
		goto st2;
	goto tr35;
tr33:
#line 1 "NONE"
	{te = p+1;}
#line 103 "lex.rl"
	{act = 12;}
	goto st29;
st29:
	if ( ++p == pe )
		goto _test_eof29;
case 29:
#line 628 "lex_scan.c"
	switch( (*p) ) {
		case 34: goto st14;
		case 39: goto st16;
		case 45: goto tr1;
		case 95: goto tr1;
	}
	if ( (*p) < -16 ) {
		if ( (*p) > -33 ) {
			if ( -32 <= (*p) && (*p) <= -17 )
				goto st1;
		} else if ( (*p) >= -62 )
			goto st0;
	} else if ( (*p) > -12 ) {
		if ( (*p) < 65 ) {
			if ( 48 <= (*p) && (*p) <= 57 )
				goto tr1;
		} else if ( (*p) > 90 ) {
			if ( 97 <= (*p) && (*p) <= 122 )
				goto tr1;
		} else
			goto tr1;
	} else
		goto st2;
	goto tr35;
	}
	_test_eof8: cs = 8; goto _test_eof; 
	_test_eof9: cs = 9; goto _test_eof; 
	_test_eof10: cs = 10; goto _test_eof; 
	_test_eof0: cs = 0; goto _test_eof; 
	_test_eof1: cs = 1; goto _test_eof; 
	_test_eof2: cs = 2; goto _test_eof; 
	_test_eof11: cs = 11; goto _test_eof; 
	_test_eof12: cs = 12; goto _test_eof; 
	_test_eof13: cs = 13; goto _test_eof; 
	_test_eof14: cs = 14; goto _test_eof; 
	_test_eof15: cs = 15; goto _test_eof; 
	_test_eof3: cs = 3; goto _test_eof; 
	_test_eof16: cs = 16; goto _test_eof; 
	_test_eof17: cs = 17; goto _test_eof; 
	_test_eof4: cs = 4; goto _test_eof; 
	_test_eof18: cs = 18; goto _test_eof; 
	_test_eof19: cs = 19; goto _test_eof; 
	_test_eof20: cs = 20; goto _test_eof; 
	_test_eof5: cs = 5; goto _test_eof; 
	_test_eof21: cs = 21; goto _test_eof; 
	_test_eof6: cs = 6; goto _test_eof; 
	_test_eof7: cs = 7; goto _test_eof; 
	_test_eof22: cs = 22; goto _test_eof; 
	_test_eof23: cs = 23; goto _test_eof; 
	_test_eof24: cs = 24; goto _test_eof; 
	_test_eof25: cs = 25; goto _test_eof; 
	_test_eof26: cs = 26; goto _test_eof; 
	_test_eof27: cs = 27; goto _test_eof; 
	_test_eof28: cs = 28; goto _test_eof; 
	_test_eof29: cs = 29; goto _test_eof; 

	_test_eof: {}
	if ( p == eof )
	{
	switch ( cs ) {
	case 9: goto tr34;
	case 10: goto tr35;
	case 0: goto tr0;
	case 1: goto tr0;
	case 2: goto tr0;
	case 11: goto tr34;
	case 12: goto tr34;
	case 13: goto tr37;
	case 14: goto tr38;
	case 15: goto tr39;
	case 3: goto tr4;
	case 16: goto tr38;
	case 17: goto tr39;
	case 4: goto tr4;
	case 18: goto tr40;
	case 19: goto tr42;
	case 20: goto tr0;
	case 5: goto tr0;
	case 21: goto tr45;
	case 6: goto tr10;
	case 7: goto tr10;
	case 22: goto tr45;
	case 23: goto tr47;
	case 24: goto tr45;
	case 25: goto tr40;
	case 26: goto tr40;
	case 27: goto tr40;
	case 28: goto tr35;
	case 29: goto tr35;
	}
	}

	_out: {}
	}

#line 128 "lex.rl"

    (void)act; (void)eof; (void)cs;
    return out->kind;
}
