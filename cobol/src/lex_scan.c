
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
static const int lexscan_start = 5;
static const int lexscan_first_final = 5;
static const int lexscan_error = -1;

static const int lexscan_en_main = 5;


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

    
#line 120 "lex.rl"


    
#line 66 "lex_scan.c"
	{
	cs = lexscan_start;
	ts = 0;
	te = 0;
	act = 0;
	}

#line 123 "lex.rl"
    
#line 76 "lex_scan.c"
	{
	if ( p == pe )
		goto _test_eof;
	switch ( cs )
	{
tr0:
#line 82 "lex.rl"
	{{p = ((te))-1;}{
            out->kind = LX_LIT; out->len = (int)(te - ts);
            const char *q = ts; while (*q != '"' && *q != '\'') q++;
            out->prefix = (int)(q - ts);
            {p++; cs = 5; goto _out;}
        }}
	goto st5;
tr5:
#line 1 "NONE"
	{	switch( act ) {
	case 11:
	{{p = ((te))-1;}
            out->kind = LX_NUM; out->len = (int)(te - ts);
            for (const char *q = ts; q < te; q++) if (*q == 'e' || *q == 'E') { out->exp = 1; break; }
            {p++; cs = 5; goto _out;}
        }
	break;
	case 13:
	{{p = ((te))-1;} out->kind = LX_OP;   out->len = (int)(te - ts); {p++; cs = 5; goto _out;} }
	break;
	}
	}
	goto st5;
tr7:
#line 94 "lex.rl"
	{{p = ((te))-1;}{
            out->kind = LX_NUM; out->len = (int)(te - ts);
            for (const char *q = ts; q < te; q++) if (*q == 'e' || *q == 'E') { out->exp = 1; break; }
            {p++; cs = 5; goto _out;}
        }}
	goto st5;
tr10:
#line 101 "lex.rl"
	{te = p+1;{ out->kind = LX_OTHER; out->len = 1; {p++; cs = 5; goto _out;} }}
	goto st5;
tr13:
#line 100 "lex.rl"
	{te = p+1;{ out->kind = LX_OP;   out->len = (int)(te - ts); {p++; cs = 5; goto _out;} }}
	goto st5;
tr15:
#line 65 "lex.rl"
	{te = p+1;{ out->kind = LX_LP;    out->len = 1; {p++; cs = 5; goto _out;} }}
	goto st5;
tr16:
#line 66 "lex.rl"
	{te = p+1;{ out->kind = LX_RP;    out->len = 1; {p++; cs = 5; goto _out;} }}
	goto st5;
tr19:
#line 77 "lex.rl"
	{te = p+1;{
            /* a comma or semicolon separates before a space; otherwise it
             * is tight to what follows (the tokenizer says what that means) */
            out->kind = lx_sep_tail(te, pe) ? LX_SEP : LX_COMMA; out->len = 1; {p++; cs = 5; goto _out;}
        }}
	goto st5;
tr22:
#line 67 "lex.rl"
	{te = p+1;{ out->kind = LX_COLON; out->len = 1; {p++; cs = 5; goto _out;} }}
	goto st5;
tr29:
#line 62 "lex.rl"
	{te = p;p--;{ out->kind = LX_SPACE;   out->len = (int)(te - ts); {p++; cs = 5; goto _out;} }}
	goto st5;
tr30:
#line 88 "lex.rl"
	{te = p;p--;{
            out->kind = LX_LIT; out->len = (int)(te - ts); out->bad = 1;
            const char *q = ts; while (*q != '"' && *q != '\'') q++;
            out->prefix = (int)(q - ts);
            {p++; cs = 5; goto _out;}
        }}
	goto st5;
tr31:
#line 82 "lex.rl"
	{te = p;p--;{
            out->kind = LX_LIT; out->len = (int)(te - ts);
            const char *q = ts; while (*q != '"' && *q != '\'') q++;
            out->prefix = (int)(q - ts);
            {p++; cs = 5; goto _out;}
        }}
	goto st5;
tr32:
#line 100 "lex.rl"
	{te = p;p--;{ out->kind = LX_OP;   out->len = (int)(te - ts); {p++; cs = 5; goto _out;} }}
	goto st5;
tr34:
#line 63 "lex.rl"
	{te = p;p--;{ out->kind = LX_COMMENT; out->len = (int)(te - ts); {p++; cs = 5; goto _out;} }}
	goto st5;
tr37:
#line 94 "lex.rl"
	{te = p;p--;{
            out->kind = LX_NUM; out->len = (int)(te - ts);
            for (const char *q = ts; q < te; q++) if (*q == 'e' || *q == 'E') { out->exp = 1; break; }
            {p++; cs = 5; goto _out;}
        }}
	goto st5;
tr39:
#line 68 "lex.rl"
	{te = p;p--;{
            /* a period separates before a space, the line's end or ==; a
             * doubled one is one separator (RM's reader let "12370121.."
             * through); otherwise it is a period inside something */
            if (lx_sep_tail(te, pe)) { out->kind = LX_PERIOD; out->len = (int)(te - ts); }
            else if (te - ts == 2 && lx_sep_tail(ts + 1, pe)) { out->kind = LX_PERIOD; out->len = 1; }
            else { out->kind = LX_DOT; out->len = 1; }
            {p++; cs = 5; goto _out;}
        }}
	goto st5;
tr40:
#line 68 "lex.rl"
	{te = p+1;{
            /* a period separates before a space, the line's end or ==; a
             * doubled one is one separator (RM's reader let "12370121.."
             * through); otherwise it is a period inside something */
            if (lx_sep_tail(te, pe)) { out->kind = LX_PERIOD; out->len = (int)(te - ts); }
            else if (te - ts == 2 && lx_sep_tail(ts + 1, pe)) { out->kind = LX_PERIOD; out->len = 1; }
            else { out->kind = LX_DOT; out->len = 1; }
            {p++; cs = 5; goto _out;}
        }}
	goto st5;
tr41:
#line 99 "lex.rl"
	{te = p;p--;{ out->kind = LX_WORD; out->len = (int)(te - ts); {p++; cs = 5; goto _out;} }}
	goto st5;
tr42:
#line 64 "lex.rl"
	{te = p+1;{ out->kind = LX_PDELIM;  out->len = 2; {p++; cs = 5; goto _out;} }}
	goto st5;
st5:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof5;
case 5:
#line 1 "NONE"
	{ts = p;}
#line 221 "lex_scan.c"
	switch( (*p) ) {
		case 9: goto st6;
		case 32: goto st6;
		case 34: goto st7;
		case 39: goto st9;
		case 40: goto tr15;
		case 41: goto tr16;
		case 42: goto st11;
		case 43: goto tr18;
		case 44: goto tr19;
		case 45: goto tr18;
		case 46: goto st16;
		case 58: goto tr22;
		case 59: goto tr19;
		case 60: goto st19;
		case 61: goto st20;
		case 62: goto st21;
		case 66: goto st22;
		case 71: goto st22;
		case 78: goto st22;
		case 85: goto st23;
		case 88: goto st23;
		case 90: goto st23;
		case 98: goto st22;
		case 103: goto st22;
		case 110: goto st22;
		case 117: goto st23;
		case 120: goto st23;
		case 122: goto st23;
	}
	if ( (*p) < 48 ) {
		if ( 38 <= (*p) && (*p) <= 47 )
			goto tr13;
	} else if ( (*p) > 57 ) {
		if ( (*p) > 89 ) {
			if ( 97 <= (*p) && (*p) <= 121 )
				goto st18;
		} else if ( (*p) >= 65 )
			goto st18;
	} else
		goto tr21;
	goto tr10;
st6:
	if ( ++p == pe )
		goto _test_eof6;
case 6:
	switch( (*p) ) {
		case 9: goto st6;
		case 32: goto st6;
	}
	goto tr29;
st7:
	if ( ++p == pe )
		goto _test_eof7;
case 7:
	if ( (*p) == 34 )
		goto tr2;
	goto st7;
tr2:
#line 1 "NONE"
	{te = p+1;}
	goto st8;
st8:
	if ( ++p == pe )
		goto _test_eof8;
case 8:
#line 288 "lex_scan.c"
	if ( (*p) == 34 )
		goto st0;
	goto tr31;
st0:
	if ( ++p == pe )
		goto _test_eof0;
case 0:
	if ( (*p) == 34 )
		goto tr2;
	goto st0;
st9:
	if ( ++p == pe )
		goto _test_eof9;
case 9:
	if ( (*p) == 39 )
		goto tr4;
	goto st9;
tr4:
#line 1 "NONE"
	{te = p+1;}
	goto st10;
st10:
	if ( ++p == pe )
		goto _test_eof10;
case 10:
#line 314 "lex_scan.c"
	if ( (*p) == 39 )
		goto st1;
	goto tr31;
st1:
	if ( ++p == pe )
		goto _test_eof1;
case 1:
	if ( (*p) == 39 )
		goto tr4;
	goto st1;
st11:
	if ( ++p == pe )
		goto _test_eof11;
case 11:
	switch( (*p) ) {
		case 42: goto tr13;
		case 62: goto st12;
	}
	goto tr32;
st12:
	if ( ++p == pe )
		goto _test_eof12;
case 12:
	goto st12;
tr18:
#line 1 "NONE"
	{te = p+1;}
#line 100 "lex.rl"
	{act = 13;}
	goto st13;
tr36:
#line 1 "NONE"
	{te = p+1;}
#line 94 "lex.rl"
	{act = 11;}
	goto st13;
st13:
	if ( ++p == pe )
		goto _test_eof13;
case 13:
#line 355 "lex_scan.c"
	if ( (*p) == 46 )
		goto st2;
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr36;
	goto tr5;
st2:
	if ( ++p == pe )
		goto _test_eof2;
case 2:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr6;
	goto tr5;
tr6:
#line 1 "NONE"
	{te = p+1;}
	goto st14;
st14:
	if ( ++p == pe )
		goto _test_eof14;
case 14:
#line 376 "lex_scan.c"
	switch( (*p) ) {
		case 69: goto st3;
		case 101: goto st3;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr6;
	goto tr37;
st3:
	if ( ++p == pe )
		goto _test_eof3;
case 3:
	switch( (*p) ) {
		case 43: goto st4;
		case 45: goto st4;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st15;
	goto tr7;
st4:
	if ( ++p == pe )
		goto _test_eof4;
case 4:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st15;
	goto tr7;
st15:
	if ( ++p == pe )
		goto _test_eof15;
case 15:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st15;
	goto tr37;
st16:
	if ( ++p == pe )
		goto _test_eof16;
case 16:
	if ( (*p) == 46 )
		goto tr40;
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr6;
	goto tr39;
tr21:
#line 1 "NONE"
	{te = p+1;}
#line 94 "lex.rl"
	{act = 11;}
	goto st17;
st17:
	if ( ++p == pe )
		goto _test_eof17;
case 17:
#line 428 "lex_scan.c"
	switch( (*p) ) {
		case 45: goto st18;
		case 46: goto st2;
		case 95: goto st18;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr21;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st18;
	} else
		goto st18;
	goto tr37;
st18:
	if ( ++p == pe )
		goto _test_eof18;
case 18:
	switch( (*p) ) {
		case 45: goto st18;
		case 95: goto st18;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st18;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st18;
	} else
		goto st18;
	goto tr41;
st19:
	if ( ++p == pe )
		goto _test_eof19;
case 19:
	if ( 61 <= (*p) && (*p) <= 62 )
		goto tr13;
	goto tr32;
st20:
	if ( ++p == pe )
		goto _test_eof20;
case 20:
	if ( (*p) == 61 )
		goto tr42;
	goto tr32;
st21:
	if ( ++p == pe )
		goto _test_eof21;
case 21:
	if ( (*p) == 61 )
		goto tr13;
	goto tr32;
st22:
	if ( ++p == pe )
		goto _test_eof22;
case 22:
	switch( (*p) ) {
		case 34: goto st7;
		case 39: goto st9;
		case 45: goto st18;
		case 88: goto st23;
		case 95: goto st18;
		case 120: goto st23;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st18;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st18;
	} else
		goto st18;
	goto tr41;
st23:
	if ( ++p == pe )
		goto _test_eof23;
case 23:
	switch( (*p) ) {
		case 34: goto st7;
		case 39: goto st9;
		case 45: goto st18;
		case 95: goto st18;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st18;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st18;
	} else
		goto st18;
	goto tr41;
	}
	_test_eof5: cs = 5; goto _test_eof; 
	_test_eof6: cs = 6; goto _test_eof; 
	_test_eof7: cs = 7; goto _test_eof; 
	_test_eof8: cs = 8; goto _test_eof; 
	_test_eof0: cs = 0; goto _test_eof; 
	_test_eof9: cs = 9; goto _test_eof; 
	_test_eof10: cs = 10; goto _test_eof; 
	_test_eof1: cs = 1; goto _test_eof; 
	_test_eof11: cs = 11; goto _test_eof; 
	_test_eof12: cs = 12; goto _test_eof; 
	_test_eof13: cs = 13; goto _test_eof; 
	_test_eof2: cs = 2; goto _test_eof; 
	_test_eof14: cs = 14; goto _test_eof; 
	_test_eof3: cs = 3; goto _test_eof; 
	_test_eof4: cs = 4; goto _test_eof; 
	_test_eof15: cs = 15; goto _test_eof; 
	_test_eof16: cs = 16; goto _test_eof; 
	_test_eof17: cs = 17; goto _test_eof; 
	_test_eof18: cs = 18; goto _test_eof; 
	_test_eof19: cs = 19; goto _test_eof; 
	_test_eof20: cs = 20; goto _test_eof; 
	_test_eof21: cs = 21; goto _test_eof; 
	_test_eof22: cs = 22; goto _test_eof; 
	_test_eof23: cs = 23; goto _test_eof; 

	_test_eof: {}
	if ( p == eof )
	{
	switch ( cs ) {
	case 6: goto tr29;
	case 7: goto tr30;
	case 8: goto tr31;
	case 0: goto tr0;
	case 9: goto tr30;
	case 10: goto tr31;
	case 1: goto tr0;
	case 11: goto tr32;
	case 12: goto tr34;
	case 13: goto tr5;
	case 2: goto tr5;
	case 14: goto tr37;
	case 3: goto tr7;
	case 4: goto tr7;
	case 15: goto tr37;
	case 16: goto tr39;
	case 17: goto tr37;
	case 18: goto tr41;
	case 19: goto tr32;
	case 20: goto tr32;
	case 21: goto tr32;
	case 22: goto tr41;
	case 23: goto tr41;
	}
	}

	_out: {}
	}

#line 124 "lex.rl"

    (void)act; (void)eof; (void)cs;
    return out->kind;
}
