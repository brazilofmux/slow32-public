
#line 1 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
/* f77_lexer.rl -- Ragel -G2 token scanner for Fortran 77.
 *
 * Build:  bash gen_lexer.sh   ->  f77_lexer_gen.c
 *
 * Runs over one ASSEMBLED statement at a time (see f77_card.h, which
 * handles the card-image layer: columns, continuations, blank squeezing
 * and Hollerith counts).  By the time the scanner sees text, blanks are
 * gone and case is folded, so the token grammar is ordinary.
 *
 * Two Fortran-specific hazards are handled here:
 *
 *   1. `1.EQ.2` -- longest-match would take `1.` as a REAL constant and
 *      leave `EQ.2`.  The dotted operators are matched at higher
 *      priority, and f77_number() additionally backs off the trailing
 *      dot when what follows is a dotted operator, so the scanner
 *      resumes at the `.` and yields `1` `.EQ.` `2`.
 *
 *   2. There are NO reserved words.  The scanner never classifies a
 *      keyword: `IF`, `DO` and `FORMAT` come back as T_NAME and the
 *      parser decides from context.  This is what lets `DO10I=1.10` be
 *      an assignment while `DO 10 I=1,10` is a loop.
 */

/* === Token kinds === */

#define T_EOF     0
#define T_NAME    1
#define T_ICON    2   /* integer constant          -> lex_ival */
#define T_RCON    3   /* REAL constant             -> lex_dval */
#define T_DCON    4   /* DOUBLE PRECISION constant -> lex_dval */
#define T_SCON    5   /* character / Hollerith     -> lex_sidx, lex_slen */
#define T_LP      6
#define T_RP      7
#define T_COMMA   8
#define T_ASSIGN  9
#define T_PLUS   10
#define T_MINUS  11
#define T_STAR   12
#define T_SLASH  13
#define T_POWER  14   /* ** */
#define T_CONCAT 15   /* // */
#define T_COLON  16
#define T_EQ     17
#define T_NE     18
#define T_LT     19
#define T_LE     20
#define T_GT     21
#define T_GE     22
#define T_AND    23
#define T_OR     24
#define T_NOT    25
#define T_EQV    26
#define T_NEQV   27
#define T_TRUE   28
#define T_FALSE  29
#define T_PCT    30   /* % (Fortran 90: a component of a structure) */
#define T_DOTOP  31   /* .NAME. -- Fortran 90's defined operator -> lex_name */

/* === Current token === */

static int    lx_t;                    /* token kind */
static char   lex_name[F77_MAX_NAME];  /* T_NAME text (upper case) */
static int    lex_namelen;
static int    lex_ival;                /* T_ICON value */
static double lex_dval;                /* T_RCON / T_DCON value */
static char   lex_kind[F77_MAX_NAME];  /* a number's _kind suffix (Fortran 90), "" for none */
/* The parser's: a number with a kind suffix, resolved to its type as it
 * is scanned (f77_kind_lit).  None in the token dump. */
static void (*lx_kind_hook)(void);
static int    lex_sidx;                /* T_SCON: string-pool index */
static int    lex_slen;                /* T_SCON: length in bytes */

/* Scanner state, saved between calls (one statement at a time). */
static char *lx_rp;
static char *lx_rpe;
static int   lx_rcs;
static int   lx_ract;
static char *lx_rts;
static char *lx_rte;

/* === Helpers === */

static void f77_name_tok(char *ts, char *te) {
    int n;
    n = (int)(te - ts);
    if (n > F77_MAX_NAME - 1) n = F77_MAX_NAME - 1;
    memcpy(lex_name, ts, n);
    lex_name[n] = 0;
    lex_namelen = n;
    lx_t = T_NAME;
}

/* A character constant, already normalised by the card reader so that
 * '' is the only escape.  Interned into the shared string pool. */
static void f77_string_tok(char *ts, char *te) {
    char buf[F77_MAX_STMT];
    char *p;
    int n;
    p = ts + 1;          /* skip opening quote */
    n = 0;
    while (p < te - 1) {
        if (*p == '\'' && p + 1 < te - 1 && p[1] == '\'') p = p + 1;
        buf[n] = *p;
        n = n + 1;
        p = p + 1;
    }
    lex_sidx = f77_intern_str(buf, n);
    lex_slen = n;
    lx_t = T_SCON;
}

static int f77_digits_val(char *s, char *e) {
    int v;
    v = 0;
    while (s < e && *s >= '0' && *s <= '9') { v = v * 10 + (*s - '0'); s = s + 1; }
    return v;
}

/* Is `s` the start of a dotted operator?  Used to decide whether a
 * trailing '.' on a numeric literal really belongs to the number. */
static int f77_is_dotop(char *s, char *e) {
    int n;
    if (s >= e || *s != '.') return 0;
    n = 1;
    while (s + n < e && ((s[n] >= 'A' && s[n] <= 'Z'))) n = n + 1;
    if (n == 1) return 0;
    if (s + n >= e || s[n] != '.') return 0;
    return 1;
}

/* Scan a numeric literal.  Returns the number of characters actually
 * consumed, which may be FEWER than Ragel matched: `1.EQ.2` matches
 * `1.` but must consume only `1`, leaving the dot to start .EQ. */
static int f77_number(char *ts, char *te, char *pe) {
    char buf[128];
    char *p;
    int n;
    int isreal;
    int isdbl;

    /* Back off a trailing dot that introduces a dotted operator. */
    if (te > ts && te[-1] == '.' && f77_is_dotop(te - 1, pe)) te = te - 1;
    /* A kind suffix (Fortran 90): 1.0_WP, 0._SP, 7_4.  Kept in lex_kind,
     * and the token's length takes it in. */
    lex_kind[0] = 0;
    if (te + 1 < pe && *te == '_' && (lx_isalpha(te[1]) || lx_isdigit(te[1]))) {
        char *q;
        int k;
        q = te + 1;
        k = 0;
        while (q < pe && (lx_isalpha(*q) || lx_isdigit(*q) || *q == '_')) {
            if (k < F77_MAX_NAME - 1) lex_kind[k++] = *q;
            q = q + 1;
        }
        lex_kind[k] = 0;
    }

    isreal = 0;
    isdbl = 0;
    n = 0;
    p = ts;
    while (p < te && n < 127) {
        if (*p == '.') isreal = 1;
        if (*p == 'D' || *p == 'd') { isdbl = 1; isreal = 1; buf[n] = 'E'; }
        else if (*p == 'E' || *p == 'e') { isreal = 1; buf[n] = 'E'; }
        else buf[n] = *p;
        n = n + 1;
        p = p + 1;
    }
    buf[n] = 0;

    if (!isreal) {
        lex_ival = f77_digits_val(ts, te);
        lx_t = T_ICON;
    } else {
        lex_dval = strtod(buf, NULL);
        lx_t = isdbl ? T_DCON : T_RCON;
    }
    if (lex_kind[0]) return (int)(te - ts) + 1 + (int)strlen(lex_kind);
    return (int)(te - ts);
}


#line 261 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"



#line 191 "/Users/sdennis/slow-32/fortran/src/f77_lexer_gen.c"
static const int f77_lexer_start = 39;
static const int f77_lexer_first_final = 39;
static const int f77_lexer_error = 0;

static const int f77_lexer_en_main = 39;


#line 264 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"

/* Point the scanner at the statement the card reader just assembled. */
static void f77_lex_stmt_init(void) {
    int cs;
    int act;
    char *ts;
    char *te;
    lx_rp = lx_stmt;
    lx_rpe = lx_stmt + lx_stmt_len;
    
#line 210 "/Users/sdennis/slow-32/fortran/src/f77_lexer_gen.c"
	{
	cs = f77_lexer_start;
	ts = 0;
	te = 0;
	act = 0;
	}

#line 274 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
    lx_rcs = cs;
    lx_ract = act;
    lx_rts = ts;
    lx_rte = te;
    lx_t = T_EOF;
}

/* Scan one token from the current statement. */
static void f77_tok(void) {
    char *p;
    char *pe;
    char *eof;
    char *ts;
    char *te;
    int cs;
    int act;

    p = lx_rp;
    pe = lx_rpe;
    eof = pe;
    cs = lx_rcs;
    act = lx_ract;
    ts = lx_rts;
    te = lx_rte;

    lx_t = T_EOF;
    if (p >= pe) return;

    
#line 248 "/Users/sdennis/slow-32/fortran/src/f77_lexer_gen.c"
	{
	if ( p == pe )
		goto _test_eof;
	switch ( cs )
	{
tr0:
#line 1 "NONE"
	{	switch( act ) {
	case 0:
	{{goto st0;}}
	break;
	case 19:
	{{p = ((te))-1;} f77_string_tok(ts, te); {p++; cs = 39; goto _out;} }
	break;
	}
	}
	goto st39;
tr14:
#line 216 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{{p = ((te))-1;}{
            p = ts + f77_number(ts, te, pe) - 1;
            {p++; cs = 39; goto _out;}
        }}
	goto st39;
tr17:
#line 209 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ f77_name_tok(ts + 1, te - 1); lx_t = T_DOTOP; {p++; cs = 39; goto _out;} }}
	goto st39;
tr20:
#line 200 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_AND;   {p++; cs = 39; goto _out;} }}
	goto st39;
tr22:
#line 194 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_EQ;    {p++; cs = 39; goto _out;} }}
	goto st39;
tr24:
#line 203 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_EQV;   {p++; cs = 39; goto _out;} }}
	goto st39;
tr29:
#line 206 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_FALSE; {p++; cs = 39; goto _out;} }}
	goto st39;
tr32:
#line 199 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_GE;    {p++; cs = 39; goto _out;} }}
	goto st39;
tr33:
#line 198 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_GT;    {p++; cs = 39; goto _out;} }}
	goto st39;
tr36:
#line 197 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_LE;    {p++; cs = 39; goto _out;} }}
	goto st39;
tr37:
#line 196 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_LT;    {p++; cs = 39; goto _out;} }}
	goto st39;
tr40:
#line 195 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_NE;    {p++; cs = 39; goto _out;} }}
	goto st39;
tr43:
#line 204 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_NEQV;  {p++; cs = 39; goto _out;} }}
	goto st39;
tr45:
#line 202 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_NOT;   {p++; cs = 39; goto _out;} }}
	goto st39;
tr47:
#line 201 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_OR;    {p++; cs = 39; goto _out;} }}
	goto st39;
tr51:
#line 205 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_TRUE;  {p++; cs = 39; goto _out;} }}
	goto st39;
tr52:
#line 212 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{{p = ((te))-1;}{
            p = ts + f77_number(ts, te, pe) - 1;
            {p++; cs = 39; goto _out;}
        }}
	goto st39;
tr55:
#line 224 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{{p = ((te))-1;}{
            p = ts + f77_number(ts, te, pe) - 1;
            {p++; cs = 39; goto _out;}
        }}
	goto st39;
tr60:
#line 254 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_PCT;    {p++; cs = 39; goto _out;} }}
	goto st39;
tr61:
#line 238 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_LP;     {p++; cs = 39; goto _out;} }}
	goto st39;
tr62:
#line 239 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_RP;     {p++; cs = 39; goto _out;} }}
	goto st39;
tr64:
#line 249 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_PLUS;   {p++; cs = 39; goto _out;} }}
	goto st39;
tr65:
#line 240 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_COMMA;  {p++; cs = 39; goto _out;} }}
	goto st39;
tr66:
#line 250 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_MINUS;  {p++; cs = 39; goto _out;} }}
	goto st39;
tr70:
#line 253 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_COLON;  {p++; cs = 39; goto _out;} }}
	goto st39;
tr74:
#line 258 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{ /* skip */ }}
	goto st39;
tr75:
#line 233 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{ f77_name_tok(ts, te); {p++; cs = 39; goto _out;} }}
	goto st39;
tr76:
#line 230 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{ f77_string_tok(ts, te); {p++; cs = 39; goto _out;} }}
	goto st39;
tr77:
#line 251 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{ lx_t = T_STAR;   {p++; cs = 39; goto _out;} }}
	goto st39;
tr78:
#line 236 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_POWER;  {p++; cs = 39; goto _out;} }}
	goto st39;
tr79:
#line 216 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{
            p = ts + f77_number(ts, te, pe) - 1;
            {p++; cs = 39; goto _out;}
        }}
	goto st39;
tr81:
#line 252 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{ lx_t = T_SLASH;  {p++; cs = 39; goto _out;} }}
	goto st39;
tr82:
#line 237 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_CONCAT; {p++; cs = 39; goto _out;} }}
	goto st39;
tr83:
#line 244 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_NE;     {p++; cs = 39; goto _out;} }}
	goto st39;
tr84:
#line 224 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{
            p = ts + f77_number(ts, te, pe) - 1;
            {p++; cs = 39; goto _out;}
        }}
	goto st39;
tr87:
#line 212 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{
            p = ts + f77_number(ts, te, pe) - 1;
            {p++; cs = 39; goto _out;}
        }}
	goto st39;
tr89:
#line 220 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{
            p = ts + f77_number(ts, te, pe) - 1;
            {p++; cs = 39; goto _out;}
        }}
	goto st39;
tr90:
#line 245 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{ lx_t = T_LT;     {p++; cs = 39; goto _out;} }}
	goto st39;
tr91:
#line 246 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_LE;     {p++; cs = 39; goto _out;} }}
	goto st39;
tr92:
#line 241 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{ lx_t = T_ASSIGN; {p++; cs = 39; goto _out;} }}
	goto st39;
tr93:
#line 243 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_EQ;     {p++; cs = 39; goto _out;} }}
	goto st39;
tr94:
#line 247 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p;p--;{ lx_t = T_GT;     {p++; cs = 39; goto _out;} }}
	goto st39;
tr95:
#line 248 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{te = p+1;{ lx_t = T_GE;     {p++; cs = 39; goto _out;} }}
	goto st39;
st39:
#line 1 "NONE"
	{ts = 0;}
#line 1 "NONE"
	{act = 0;}
	if ( ++p == pe )
		goto _test_eof39;
case 39:
#line 1 "NONE"
	{ts = p;}
#line 465 "/Users/sdennis/slow-32/fortran/src/f77_lexer_gen.c"
	switch( (*p) ) {
		case 9: goto st40;
		case 32: goto st40;
		case 36: goto st41;
		case 37: goto tr60;
		case 39: goto st1;
		case 40: goto tr61;
		case 41: goto tr62;
		case 42: goto st43;
		case 43: goto tr64;
		case 44: goto tr65;
		case 45: goto tr66;
		case 46: goto st2;
		case 47: goto st46;
		case 58: goto tr70;
		case 60: goto st51;
		case 61: goto st52;
		case 62: goto st53;
		case 95: goto st41;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr69;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st41;
	} else
		goto st41;
	goto st0;
st0:
cs = 0;
	goto _out;
st40:
	if ( ++p == pe )
		goto _test_eof40;
case 40:
	switch( (*p) ) {
		case 9: goto st40;
		case 32: goto st40;
	}
	goto tr74;
st41:
	if ( ++p == pe )
		goto _test_eof41;
case 41:
	switch( (*p) ) {
		case 36: goto st41;
		case 95: goto st41;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st41;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st41;
	} else
		goto st41;
	goto tr75;
st1:
	if ( ++p == pe )
		goto _test_eof1;
case 1:
	if ( (*p) == 39 )
		goto tr2;
	goto st1;
tr2:
#line 1 "NONE"
	{te = p+1;}
#line 230 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"
	{act = 19;}
	goto st42;
st42:
	if ( ++p == pe )
		goto _test_eof42;
case 42:
#line 541 "/Users/sdennis/slow-32/fortran/src/f77_lexer_gen.c"
	if ( (*p) == 39 )
		goto st1;
	goto tr76;
st43:
	if ( ++p == pe )
		goto _test_eof43;
case 43:
	if ( (*p) == 42 )
		goto tr78;
	goto tr77;
st2:
	if ( ++p == pe )
		goto _test_eof2;
case 2:
	switch( (*p) ) {
		case 65: goto st5;
		case 69: goto st9;
		case 70: goto st12;
		case 71: goto st17;
		case 76: goto st20;
		case 78: goto st23;
		case 79: goto st29;
		case 84: goto st31;
	}
	if ( (*p) < 66 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr3;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else
		goto st6;
	goto st0;
tr3:
#line 1 "NONE"
	{te = p+1;}
	goto st44;
st44:
	if ( ++p == pe )
		goto _test_eof44;
case 44:
#line 583 "/Users/sdennis/slow-32/fortran/src/f77_lexer_gen.c"
	if ( (*p) < 68 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr3;
	} else if ( (*p) > 69 ) {
		if ( 100 <= (*p) && (*p) <= 101 )
			goto st3;
	} else
		goto st3;
	goto tr79;
st3:
	if ( ++p == pe )
		goto _test_eof3;
case 3:
	switch( (*p) ) {
		case 43: goto st4;
		case 45: goto st4;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st45;
	goto tr14;
st4:
	if ( ++p == pe )
		goto _test_eof4;
case 4:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st45;
	goto tr14;
st45:
	if ( ++p == pe )
		goto _test_eof45;
case 45:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st45;
	goto tr79;
st5:
	if ( ++p == pe )
		goto _test_eof5;
case 5:
	switch( (*p) ) {
		case 46: goto tr17;
		case 78: goto st7;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st6:
	if ( ++p == pe )
		goto _test_eof6;
case 6:
	if ( (*p) == 46 )
		goto tr17;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st7:
	if ( ++p == pe )
		goto _test_eof7;
case 7:
	switch( (*p) ) {
		case 46: goto tr17;
		case 68: goto st8;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st8:
	if ( ++p == pe )
		goto _test_eof8;
case 8:
	if ( (*p) == 46 )
		goto tr20;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st9:
	if ( ++p == pe )
		goto _test_eof9;
case 9:
	switch( (*p) ) {
		case 46: goto tr17;
		case 81: goto st10;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st10:
	if ( ++p == pe )
		goto _test_eof10;
case 10:
	switch( (*p) ) {
		case 46: goto tr22;
		case 86: goto st11;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st11:
	if ( ++p == pe )
		goto _test_eof11;
case 11:
	if ( (*p) == 46 )
		goto tr24;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st12:
	if ( ++p == pe )
		goto _test_eof12;
case 12:
	switch( (*p) ) {
		case 46: goto tr17;
		case 65: goto st13;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 66 )
		goto st6;
	goto st0;
st13:
	if ( ++p == pe )
		goto _test_eof13;
case 13:
	switch( (*p) ) {
		case 46: goto tr17;
		case 76: goto st14;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st14:
	if ( ++p == pe )
		goto _test_eof14;
case 14:
	switch( (*p) ) {
		case 46: goto tr17;
		case 83: goto st15;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st15:
	if ( ++p == pe )
		goto _test_eof15;
case 15:
	switch( (*p) ) {
		case 46: goto tr17;
		case 69: goto st16;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st16:
	if ( ++p == pe )
		goto _test_eof16;
case 16:
	if ( (*p) == 46 )
		goto tr29;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st17:
	if ( ++p == pe )
		goto _test_eof17;
case 17:
	switch( (*p) ) {
		case 46: goto tr17;
		case 69: goto st18;
		case 84: goto st19;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st18:
	if ( ++p == pe )
		goto _test_eof18;
case 18:
	if ( (*p) == 46 )
		goto tr32;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st19:
	if ( ++p == pe )
		goto _test_eof19;
case 19:
	if ( (*p) == 46 )
		goto tr33;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st20:
	if ( ++p == pe )
		goto _test_eof20;
case 20:
	switch( (*p) ) {
		case 46: goto tr17;
		case 69: goto st21;
		case 84: goto st22;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st21:
	if ( ++p == pe )
		goto _test_eof21;
case 21:
	if ( (*p) == 46 )
		goto tr36;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st22:
	if ( ++p == pe )
		goto _test_eof22;
case 22:
	if ( (*p) == 46 )
		goto tr37;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st23:
	if ( ++p == pe )
		goto _test_eof23;
case 23:
	switch( (*p) ) {
		case 46: goto tr17;
		case 69: goto st24;
		case 79: goto st27;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st24:
	if ( ++p == pe )
		goto _test_eof24;
case 24:
	switch( (*p) ) {
		case 46: goto tr40;
		case 81: goto st25;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st25:
	if ( ++p == pe )
		goto _test_eof25;
case 25:
	switch( (*p) ) {
		case 46: goto tr17;
		case 86: goto st26;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st26:
	if ( ++p == pe )
		goto _test_eof26;
case 26:
	if ( (*p) == 46 )
		goto tr43;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st27:
	if ( ++p == pe )
		goto _test_eof27;
case 27:
	switch( (*p) ) {
		case 46: goto tr17;
		case 84: goto st28;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st28:
	if ( ++p == pe )
		goto _test_eof28;
case 28:
	if ( (*p) == 46 )
		goto tr45;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st29:
	if ( ++p == pe )
		goto _test_eof29;
case 29:
	switch( (*p) ) {
		case 46: goto tr17;
		case 82: goto st30;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st30:
	if ( ++p == pe )
		goto _test_eof30;
case 30:
	if ( (*p) == 46 )
		goto tr47;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st31:
	if ( ++p == pe )
		goto _test_eof31;
case 31:
	switch( (*p) ) {
		case 46: goto tr17;
		case 82: goto st32;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st32:
	if ( ++p == pe )
		goto _test_eof32;
case 32:
	switch( (*p) ) {
		case 46: goto tr17;
		case 85: goto st33;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st33:
	if ( ++p == pe )
		goto _test_eof33;
case 33:
	switch( (*p) ) {
		case 46: goto tr17;
		case 69: goto st34;
	}
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st34:
	if ( ++p == pe )
		goto _test_eof34;
case 34:
	if ( (*p) == 46 )
		goto tr51;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st6;
	} else if ( (*p) >= 65 )
		goto st6;
	goto st0;
st46:
	if ( ++p == pe )
		goto _test_eof46;
case 46:
	switch( (*p) ) {
		case 47: goto tr82;
		case 61: goto tr83;
	}
	goto tr81;
tr69:
#line 1 "NONE"
	{te = p+1;}
	goto st47;
st47:
	if ( ++p == pe )
		goto _test_eof47;
case 47:
#line 1034 "/Users/sdennis/slow-32/fortran/src/f77_lexer_gen.c"
	if ( (*p) == 46 )
		goto tr85;
	if ( (*p) < 68 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr69;
	} else if ( (*p) > 69 ) {
		if ( 100 <= (*p) && (*p) <= 101 )
			goto st37;
	} else
		goto st37;
	goto tr84;
tr85:
#line 1 "NONE"
	{te = p+1;}
	goto st48;
st48:
	if ( ++p == pe )
		goto _test_eof48;
case 48:
#line 1054 "/Users/sdennis/slow-32/fortran/src/f77_lexer_gen.c"
	if ( (*p) < 68 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr85;
	} else if ( (*p) > 69 ) {
		if ( 100 <= (*p) && (*p) <= 101 )
			goto st35;
	} else
		goto st35;
	goto tr87;
st35:
	if ( ++p == pe )
		goto _test_eof35;
case 35:
	switch( (*p) ) {
		case 43: goto st36;
		case 45: goto st36;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st49;
	goto tr52;
st36:
	if ( ++p == pe )
		goto _test_eof36;
case 36:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st49;
	goto tr52;
st49:
	if ( ++p == pe )
		goto _test_eof49;
case 49:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st49;
	goto tr87;
st37:
	if ( ++p == pe )
		goto _test_eof37;
case 37:
	switch( (*p) ) {
		case 43: goto st38;
		case 45: goto st38;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st50;
	goto tr55;
st38:
	if ( ++p == pe )
		goto _test_eof38;
case 38:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st50;
	goto tr55;
st50:
	if ( ++p == pe )
		goto _test_eof50;
case 50:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st50;
	goto tr89;
st51:
	if ( ++p == pe )
		goto _test_eof51;
case 51:
	if ( (*p) == 61 )
		goto tr91;
	goto tr90;
st52:
	if ( ++p == pe )
		goto _test_eof52;
case 52:
	if ( (*p) == 61 )
		goto tr93;
	goto tr92;
st53:
	if ( ++p == pe )
		goto _test_eof53;
case 53:
	if ( (*p) == 61 )
		goto tr95;
	goto tr94;
	}
	_test_eof39: cs = 39; goto _test_eof; 
	_test_eof40: cs = 40; goto _test_eof; 
	_test_eof41: cs = 41; goto _test_eof; 
	_test_eof1: cs = 1; goto _test_eof; 
	_test_eof42: cs = 42; goto _test_eof; 
	_test_eof43: cs = 43; goto _test_eof; 
	_test_eof2: cs = 2; goto _test_eof; 
	_test_eof44: cs = 44; goto _test_eof; 
	_test_eof3: cs = 3; goto _test_eof; 
	_test_eof4: cs = 4; goto _test_eof; 
	_test_eof45: cs = 45; goto _test_eof; 
	_test_eof5: cs = 5; goto _test_eof; 
	_test_eof6: cs = 6; goto _test_eof; 
	_test_eof7: cs = 7; goto _test_eof; 
	_test_eof8: cs = 8; goto _test_eof; 
	_test_eof9: cs = 9; goto _test_eof; 
	_test_eof10: cs = 10; goto _test_eof; 
	_test_eof11: cs = 11; goto _test_eof; 
	_test_eof12: cs = 12; goto _test_eof; 
	_test_eof13: cs = 13; goto _test_eof; 
	_test_eof14: cs = 14; goto _test_eof; 
	_test_eof15: cs = 15; goto _test_eof; 
	_test_eof16: cs = 16; goto _test_eof; 
	_test_eof17: cs = 17; goto _test_eof; 
	_test_eof18: cs = 18; goto _test_eof; 
	_test_eof19: cs = 19; goto _test_eof; 
	_test_eof20: cs = 20; goto _test_eof; 
	_test_eof21: cs = 21; goto _test_eof; 
	_test_eof22: cs = 22; goto _test_eof; 
	_test_eof23: cs = 23; goto _test_eof; 
	_test_eof24: cs = 24; goto _test_eof; 
	_test_eof25: cs = 25; goto _test_eof; 
	_test_eof26: cs = 26; goto _test_eof; 
	_test_eof27: cs = 27; goto _test_eof; 
	_test_eof28: cs = 28; goto _test_eof; 
	_test_eof29: cs = 29; goto _test_eof; 
	_test_eof30: cs = 30; goto _test_eof; 
	_test_eof31: cs = 31; goto _test_eof; 
	_test_eof32: cs = 32; goto _test_eof; 
	_test_eof33: cs = 33; goto _test_eof; 
	_test_eof34: cs = 34; goto _test_eof; 
	_test_eof46: cs = 46; goto _test_eof; 
	_test_eof47: cs = 47; goto _test_eof; 
	_test_eof48: cs = 48; goto _test_eof; 
	_test_eof35: cs = 35; goto _test_eof; 
	_test_eof36: cs = 36; goto _test_eof; 
	_test_eof49: cs = 49; goto _test_eof; 
	_test_eof37: cs = 37; goto _test_eof; 
	_test_eof38: cs = 38; goto _test_eof; 
	_test_eof50: cs = 50; goto _test_eof; 
	_test_eof51: cs = 51; goto _test_eof; 
	_test_eof52: cs = 52; goto _test_eof; 
	_test_eof53: cs = 53; goto _test_eof; 

	_test_eof: {}
	if ( p == eof )
	{
	switch ( cs ) {
	case 40: goto tr74;
	case 41: goto tr75;
	case 1: goto tr0;
	case 42: goto tr76;
	case 43: goto tr77;
	case 44: goto tr79;
	case 3: goto tr14;
	case 4: goto tr14;
	case 45: goto tr79;
	case 46: goto tr81;
	case 47: goto tr84;
	case 48: goto tr87;
	case 35: goto tr52;
	case 36: goto tr52;
	case 49: goto tr87;
	case 37: goto tr55;
	case 38: goto tr55;
	case 50: goto tr89;
	case 51: goto tr90;
	case 52: goto tr92;
	case 53: goto tr94;
	}
	}

	_out: {}
	}

#line 303 "/Users/sdennis/slow-32/fortran/src/f77_lexer.rl"

    lx_rp = p;
    lx_rcs = cs;
    lx_ract = act;
    lx_rts = ts;
    lx_rte = te;
    if ((lx_t == T_ICON || lx_t == T_RCON || lx_t == T_DCON) && lex_kind[0] && lx_kind_hook) lx_kind_hook();
}
