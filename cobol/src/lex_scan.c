
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


#line 38 "lex.rl"


/* is the text at q a separator's tail: the line's end, a space, a tab, or
 * a closing pseudo-text delimiter (==... PIC 9(5).==) */
static int lx_sep_tail(const unsigned char *q, const unsigned char *pe)
{
    return q >= pe || *q == ' ' || *q == '\t' || (q + 1 < pe && q[0] == '=' && q[1] == '=');
}

/* the lexeme at p (p < pe): kind, text and length; the literal's prefix
 * length; whether a number carries an exponent.  Every byte is some
 * lexeme (LX_OTHER when nothing else), so the callers always advance. */
int lx_next(const char *p0, const char *pe0, Lexeme *out)
{
    /* the interface is char, as its callers' buffers are; the machine reads
     * bytes (alphtype unsigned char above) */
    const unsigned char *p = (const unsigned char *)p0;
    const unsigned char *pe = (const unsigned char *)pe0, *eof = pe;
    const unsigned char *ts, *te;
    int cs, act;
    memset(out, 0, sizeof *out);
    out->s = p0; out->len = 1; out->kind = LX_OTHER;

    
#line 134 "lex.rl"


    
#line 69 "lex_scan.c"
	{
	cs = lexscan_start;
	ts = 0;
	te = 0;
	act = 0;
	}

#line 137 "lex.rl"
    
#line 79 "lex_scan.c"
	{
	if ( p == pe )
		goto _test_eof;
	switch ( cs )
	{
tr0:
#line 96 "lex.rl"
	{{p = ((te))-1;}{
            out->kind = LX_LIT; out->len = (int)(te - ts);
            const unsigned char *q = ts; while (*q != '"' && *q != '\'') q++;
            out->prefix = (int)(q - ts);
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr5:
#line 1 "NONE"
	{	switch( act ) {
	case 11:
	{{p = ((te))-1;}
            out->kind = LX_NUM; out->len = (int)(te - ts);
            for (const unsigned char *q = ts; q < te; q++) if (*q == 'e' || *q == 'E') { out->exp = 1; break; }
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
tr7:
#line 108 "lex.rl"
	{{p = ((te))-1;}{
            out->kind = LX_NUM; out->len = (int)(te - ts);
            for (const unsigned char *q = ts; q < te; q++) if (*q == 'e' || *q == 'E') { out->exp = 1; break; }
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr13:
#line 115 "lex.rl"
	{te = p+1;{ out->kind = LX_OTHER; out->len = 1; {p++; cs = 8; goto _out;} }}
	goto st8;
tr16:
#line 114 "lex.rl"
	{te = p+1;{ out->kind = LX_OP;   out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }}
	goto st8;
tr18:
#line 79 "lex.rl"
	{te = p+1;{ out->kind = LX_LP;    out->len = 1; {p++; cs = 8; goto _out;} }}
	goto st8;
tr19:
#line 80 "lex.rl"
	{te = p+1;{ out->kind = LX_RP;    out->len = 1; {p++; cs = 8; goto _out;} }}
	goto st8;
tr22:
#line 91 "lex.rl"
	{te = p+1;{
            /* a comma or semicolon separates before a space; otherwise it
             * is tight to what follows (the tokenizer says what that means) */
            out->kind = lx_sep_tail(te, pe) ? LX_SEP : LX_COMMA; out->len = 1; {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr25:
#line 81 "lex.rl"
	{te = p+1;{ out->kind = LX_COLON; out->len = 1; {p++; cs = 8; goto _out;} }}
	goto st8;
tr34:
#line 76 "lex.rl"
	{te = p;p--;{ out->kind = LX_SPACE;   out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }}
	goto st8;
tr35:
#line 102 "lex.rl"
	{te = p;p--;{
            out->kind = LX_LIT; out->len = (int)(te - ts); out->bad = 1;
            const unsigned char *q = ts; while (*q != '"' && *q != '\'') q++;
            out->prefix = (int)(q - ts);
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr36:
#line 96 "lex.rl"
	{te = p;p--;{
            out->kind = LX_LIT; out->len = (int)(te - ts);
            const unsigned char *q = ts; while (*q != '"' && *q != '\'') q++;
            out->prefix = (int)(q - ts);
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr37:
#line 114 "lex.rl"
	{te = p;p--;{ out->kind = LX_OP;   out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }}
	goto st8;
tr39:
#line 77 "lex.rl"
	{te = p;p--;{ out->kind = LX_COMMENT; out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }}
	goto st8;
tr42:
#line 108 "lex.rl"
	{te = p;p--;{
            out->kind = LX_NUM; out->len = (int)(te - ts);
            for (const unsigned char *q = ts; q < te; q++) if (*q == 'e' || *q == 'E') { out->exp = 1; break; }
            {p++; cs = 8; goto _out;}
        }}
	goto st8;
tr44:
#line 82 "lex.rl"
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
tr45:
#line 82 "lex.rl"
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
tr47:
#line 113 "lex.rl"
	{te = p;p--;{ out->kind = LX_WORD; out->len = (int)(te - ts); {p++; cs = 8; goto _out;} }}
	goto st8;
tr48:
#line 78 "lex.rl"
	{te = p+1;{ out->kind = LX_PDELIM;  out->len = 2; {p++; cs = 8; goto _out;} }}
	goto st8;
tr49:
#line 115 "lex.rl"
	{te = p;p--;{ out->kind = LX_OTHER; out->len = 1; {p++; cs = 8; goto _out;} }}
	goto st8;
st8:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof8;
case 8:
#line 1 "NONE"
	{ts = p;}
#line 234 "lex_scan.c"
	switch( (*p) ) {
		case 9u: goto st9;
		case 32u: goto st9;
		case 34u: goto st10;
		case 39u: goto st12;
		case 40u: goto tr18;
		case 41u: goto tr19;
		case 42u: goto st14;
		case 43u: goto tr21;
		case 44u: goto tr22;
		case 45u: goto tr21;
		case 46u: goto st19;
		case 58u: goto tr25;
		case 59u: goto tr22;
		case 60u: goto st22;
		case 61u: goto st23;
		case 62u: goto st24;
		case 66u: goto tr29;
		case 71u: goto tr29;
		case 78u: goto tr29;
		case 85u: goto tr30;
		case 88u: goto tr30;
		case 90u: goto tr30;
		case 98u: goto tr29;
		case 103u: goto tr29;
		case 110u: goto tr29;
		case 117u: goto tr30;
		case 120u: goto tr30;
		case 122u: goto tr30;
	}
	if ( (*p) < 97u ) {
		if ( (*p) < 48u ) {
			if ( 38u <= (*p) && (*p) <= 47u )
				goto tr16;
		} else if ( (*p) > 57u ) {
			if ( 65u <= (*p) && (*p) <= 89u )
				goto tr10;
		} else
			goto tr24;
	} else if ( (*p) > 121u ) {
		if ( (*p) < 224u ) {
			if ( 194u <= (*p) && (*p) <= 223u )
				goto st27;
		} else if ( (*p) > 239u ) {
			if ( 240u <= (*p) && (*p) <= 244u )
				goto tr33;
		} else
			goto tr32;
	} else
		goto tr10;
	goto tr13;
st9:
	if ( ++p == pe )
		goto _test_eof9;
case 9:
	switch( (*p) ) {
		case 9u: goto st9;
		case 32u: goto st9;
	}
	goto tr34;
st10:
	if ( ++p == pe )
		goto _test_eof10;
case 10:
	if ( (*p) == 34u )
		goto tr2;
	goto st10;
tr2:
#line 1 "NONE"
	{te = p+1;}
	goto st11;
st11:
	if ( ++p == pe )
		goto _test_eof11;
case 11:
#line 310 "lex_scan.c"
	if ( (*p) == 34u )
		goto st0;
	goto tr36;
st0:
	if ( ++p == pe )
		goto _test_eof0;
case 0:
	if ( (*p) == 34u )
		goto tr2;
	goto st0;
st12:
	if ( ++p == pe )
		goto _test_eof12;
case 12:
	if ( (*p) == 39u )
		goto tr4;
	goto st12;
tr4:
#line 1 "NONE"
	{te = p+1;}
	goto st13;
st13:
	if ( ++p == pe )
		goto _test_eof13;
case 13:
#line 336 "lex_scan.c"
	if ( (*p) == 39u )
		goto st1;
	goto tr36;
st1:
	if ( ++p == pe )
		goto _test_eof1;
case 1:
	if ( (*p) == 39u )
		goto tr4;
	goto st1;
st14:
	if ( ++p == pe )
		goto _test_eof14;
case 14:
	switch( (*p) ) {
		case 42u: goto tr16;
		case 62u: goto st15;
	}
	goto tr37;
st15:
	if ( ++p == pe )
		goto _test_eof15;
case 15:
	goto st15;
tr21:
#line 1 "NONE"
	{te = p+1;}
#line 114 "lex.rl"
	{act = 13;}
	goto st16;
tr41:
#line 1 "NONE"
	{te = p+1;}
#line 108 "lex.rl"
	{act = 11;}
	goto st16;
st16:
	if ( ++p == pe )
		goto _test_eof16;
case 16:
#line 377 "lex_scan.c"
	if ( (*p) == 46u )
		goto st2;
	if ( 48u <= (*p) && (*p) <= 57u )
		goto tr41;
	goto tr5;
st2:
	if ( ++p == pe )
		goto _test_eof2;
case 2:
	if ( 48u <= (*p) && (*p) <= 57u )
		goto tr6;
	goto tr5;
tr6:
#line 1 "NONE"
	{te = p+1;}
	goto st17;
st17:
	if ( ++p == pe )
		goto _test_eof17;
case 17:
#line 398 "lex_scan.c"
	switch( (*p) ) {
		case 69u: goto st3;
		case 101u: goto st3;
	}
	if ( 48u <= (*p) && (*p) <= 57u )
		goto tr6;
	goto tr42;
st3:
	if ( ++p == pe )
		goto _test_eof3;
case 3:
	switch( (*p) ) {
		case 43u: goto st4;
		case 45u: goto st4;
	}
	if ( 48u <= (*p) && (*p) <= 57u )
		goto st18;
	goto tr7;
st4:
	if ( ++p == pe )
		goto _test_eof4;
case 4:
	if ( 48u <= (*p) && (*p) <= 57u )
		goto st18;
	goto tr7;
st18:
	if ( ++p == pe )
		goto _test_eof18;
case 18:
	if ( 48u <= (*p) && (*p) <= 57u )
		goto st18;
	goto tr42;
st19:
	if ( ++p == pe )
		goto _test_eof19;
case 19:
	if ( (*p) == 46u )
		goto tr45;
	if ( 48u <= (*p) && (*p) <= 57u )
		goto tr6;
	goto tr44;
tr24:
#line 1 "NONE"
	{te = p+1;}
#line 108 "lex.rl"
	{act = 11;}
	goto st20;
st20:
	if ( ++p == pe )
		goto _test_eof20;
case 20:
#line 450 "lex_scan.c"
	switch( (*p) ) {
		case 45u: goto tr10;
		case 46u: goto st2;
		case 95u: goto tr10;
	}
	if ( (*p) < 97u ) {
		if ( (*p) > 57u ) {
			if ( 65u <= (*p) && (*p) <= 90u )
				goto tr10;
		} else if ( (*p) >= 48u )
			goto tr24;
	} else if ( (*p) > 122u ) {
		if ( (*p) < 224u ) {
			if ( 194u <= (*p) && (*p) <= 223u )
				goto st5;
		} else if ( (*p) > 239u ) {
			if ( 240u <= (*p) && (*p) <= 244u )
				goto st7;
		} else
			goto st6;
	} else
		goto tr10;
	goto tr42;
tr10:
#line 1 "NONE"
	{te = p+1;}
#line 113 "lex.rl"
	{act = 12;}
	goto st21;
st21:
	if ( ++p == pe )
		goto _test_eof21;
case 21:
#line 484 "lex_scan.c"
	switch( (*p) ) {
		case 45u: goto tr10;
		case 95u: goto tr10;
	}
	if ( (*p) < 97u ) {
		if ( (*p) > 57u ) {
			if ( 65u <= (*p) && (*p) <= 90u )
				goto tr10;
		} else if ( (*p) >= 48u )
			goto tr10;
	} else if ( (*p) > 122u ) {
		if ( (*p) < 224u ) {
			if ( 194u <= (*p) && (*p) <= 223u )
				goto st5;
		} else if ( (*p) > 239u ) {
			if ( 240u <= (*p) && (*p) <= 244u )
				goto st7;
		} else
			goto st6;
	} else
		goto tr10;
	goto tr47;
st5:
	if ( ++p == pe )
		goto _test_eof5;
case 5:
	if ( 128u <= (*p) && (*p) <= 191u )
		goto tr10;
	goto tr5;
st6:
	if ( ++p == pe )
		goto _test_eof6;
case 6:
	if ( 128u <= (*p) && (*p) <= 191u )
		goto st5;
	goto tr5;
st7:
	if ( ++p == pe )
		goto _test_eof7;
case 7:
	if ( 128u <= (*p) && (*p) <= 191u )
		goto st6;
	goto tr5;
st22:
	if ( ++p == pe )
		goto _test_eof22;
case 22:
	if ( 61u <= (*p) && (*p) <= 62u )
		goto tr16;
	goto tr37;
st23:
	if ( ++p == pe )
		goto _test_eof23;
case 23:
	if ( (*p) == 61u )
		goto tr48;
	goto tr37;
st24:
	if ( ++p == pe )
		goto _test_eof24;
case 24:
	if ( (*p) == 61u )
		goto tr16;
	goto tr37;
tr29:
#line 1 "NONE"
	{te = p+1;}
#line 113 "lex.rl"
	{act = 12;}
	goto st25;
st25:
	if ( ++p == pe )
		goto _test_eof25;
case 25:
#line 559 "lex_scan.c"
	switch( (*p) ) {
		case 34u: goto st10;
		case 39u: goto st12;
		case 45u: goto tr10;
		case 88u: goto tr30;
		case 95u: goto tr10;
		case 120u: goto tr30;
	}
	if ( (*p) < 97u ) {
		if ( (*p) > 57u ) {
			if ( 65u <= (*p) && (*p) <= 90u )
				goto tr10;
		} else if ( (*p) >= 48u )
			goto tr10;
	} else if ( (*p) > 122u ) {
		if ( (*p) < 224u ) {
			if ( 194u <= (*p) && (*p) <= 223u )
				goto st5;
		} else if ( (*p) > 239u ) {
			if ( 240u <= (*p) && (*p) <= 244u )
				goto st7;
		} else
			goto st6;
	} else
		goto tr10;
	goto tr47;
tr30:
#line 1 "NONE"
	{te = p+1;}
#line 113 "lex.rl"
	{act = 12;}
	goto st26;
st26:
	if ( ++p == pe )
		goto _test_eof26;
case 26:
#line 596 "lex_scan.c"
	switch( (*p) ) {
		case 34u: goto st10;
		case 39u: goto st12;
		case 45u: goto tr10;
		case 95u: goto tr10;
	}
	if ( (*p) < 97u ) {
		if ( (*p) > 57u ) {
			if ( 65u <= (*p) && (*p) <= 90u )
				goto tr10;
		} else if ( (*p) >= 48u )
			goto tr10;
	} else if ( (*p) > 122u ) {
		if ( (*p) < 224u ) {
			if ( 194u <= (*p) && (*p) <= 223u )
				goto st5;
		} else if ( (*p) > 239u ) {
			if ( 240u <= (*p) && (*p) <= 244u )
				goto st7;
		} else
			goto st6;
	} else
		goto tr10;
	goto tr47;
st27:
	if ( ++p == pe )
		goto _test_eof27;
case 27:
	if ( 128u <= (*p) && (*p) <= 191u )
		goto tr10;
	goto tr49;
tr32:
#line 1 "NONE"
	{te = p+1;}
#line 115 "lex.rl"
	{act = 14;}
	goto st28;
st28:
	if ( ++p == pe )
		goto _test_eof28;
case 28:
#line 638 "lex_scan.c"
	if ( 128u <= (*p) && (*p) <= 191u )
		goto st5;
	goto tr49;
tr33:
#line 1 "NONE"
	{te = p+1;}
#line 115 "lex.rl"
	{act = 14;}
	goto st29;
st29:
	if ( ++p == pe )
		goto _test_eof29;
case 29:
#line 652 "lex_scan.c"
	if ( 128u <= (*p) && (*p) <= 191u )
		goto st6;
	goto tr49;
	}
	_test_eof8: cs = 8; goto _test_eof; 
	_test_eof9: cs = 9; goto _test_eof; 
	_test_eof10: cs = 10; goto _test_eof; 
	_test_eof11: cs = 11; goto _test_eof; 
	_test_eof0: cs = 0; goto _test_eof; 
	_test_eof12: cs = 12; goto _test_eof; 
	_test_eof13: cs = 13; goto _test_eof; 
	_test_eof1: cs = 1; goto _test_eof; 
	_test_eof14: cs = 14; goto _test_eof; 
	_test_eof15: cs = 15; goto _test_eof; 
	_test_eof16: cs = 16; goto _test_eof; 
	_test_eof2: cs = 2; goto _test_eof; 
	_test_eof17: cs = 17; goto _test_eof; 
	_test_eof3: cs = 3; goto _test_eof; 
	_test_eof4: cs = 4; goto _test_eof; 
	_test_eof18: cs = 18; goto _test_eof; 
	_test_eof19: cs = 19; goto _test_eof; 
	_test_eof20: cs = 20; goto _test_eof; 
	_test_eof21: cs = 21; goto _test_eof; 
	_test_eof5: cs = 5; goto _test_eof; 
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
	case 11: goto tr36;
	case 0: goto tr0;
	case 12: goto tr35;
	case 13: goto tr36;
	case 1: goto tr0;
	case 14: goto tr37;
	case 15: goto tr39;
	case 16: goto tr5;
	case 2: goto tr5;
	case 17: goto tr42;
	case 3: goto tr7;
	case 4: goto tr7;
	case 18: goto tr42;
	case 19: goto tr44;
	case 20: goto tr42;
	case 21: goto tr47;
	case 5: goto tr5;
	case 6: goto tr5;
	case 7: goto tr5;
	case 22: goto tr37;
	case 23: goto tr37;
	case 24: goto tr37;
	case 25: goto tr47;
	case 26: goto tr47;
	case 27: goto tr49;
	case 28: goto tr49;
	case 29: goto tr49;
	}
	}

	_out: {}
	}

#line 138 "lex.rl"

    (void)act; (void)eof; (void)cs;
    return out->kind;
}
