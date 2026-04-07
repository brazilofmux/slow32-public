
#line 1 "c_lexer.rl"
/* c_lexer.rl -- Ragel -G2 C lexer for SLOW-32 s32-cc compiler (stage18)
 *
 * Build:   bash gen_lexer.sh
 * Output:  c_lexer_gen.c (post-processed for s32-cc compatibility)
 *
 * Interface matches s32cc_lex.h API used by s32cc_parse.h.
 */

/* === Libc prototypes === */
int strcmp(char *a, char *b);
int strncmp(char *a, char *b, int n);
int strlen(char *s);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);

int fdputs(char *s, int f);
int fdputc(int c, int f);
void fdputuint(int f, int v);
void exit(int status);

#define NULL 0

/* === Token constants (same numbering as s32cc_lex.h) === */

#define TK_EOF        0
#define TK_NUM        1
#define TK_STRING     2
#define TK_CHARLIT    3
#define TK_IDENT      4

#define TK_AUTO       10
#define TK_BREAK      11
#define TK_CASE       12
#define TK_CHAR       13
#define TK_CONST      14
#define TK_CONTINUE   15
#define TK_DEFAULT    16
#define TK_DO         17
#define TK_DOUBLE     18
#define TK_ELSE       19
#define TK_ENUM       20
#define TK_EXTERN     21
#define TK_FLOAT      22
#define TK_FOR        23
#define TK_GOTO       24
#define TK_IF         25
#define TK_INLINE     26
#define TK_INT        27
#define TK_LONG       28
#define TK_REGISTER   29
#define TK_RESTRICT   30
#define TK_RETURN     31
#define TK_SHORT      32
#define TK_SIGNED     33
#define TK_SIZEOF     34
#define TK_STATIC     35
#define TK_STRUCT     36
#define TK_SWITCH     37
#define TK_TYPEDEF    38
#define TK_UNION      39
#define TK_UNSIGNED   40
#define TK_VOID       41
#define TK_VOLATILE   42
#define TK_WHILE      43

#define TK_LPAREN     50
#define TK_RPAREN     51
#define TK_LBRACK     52
#define TK_RBRACK     53
#define TK_LBRACE     54
#define TK_RBRACE     55
#define TK_SEMI       56
#define TK_COMMA      57
#define TK_DOT        58
#define TK_ARROW      59
#define TK_ELLIPSIS   60

#define TK_PLUS       70
#define TK_MINUS      71
#define TK_STAR       72
#define TK_SLASH      73
#define TK_PERCENT    74
#define TK_AMP        75
#define TK_PIPE       76
#define TK_CARET      77
#define TK_TILDE      78
#define TK_BANG       79
#define TK_LSHIFT     80
#define TK_RSHIFT     81

#define TK_ASSIGN     90
#define TK_PLUSEQ     91
#define TK_MINUSEQ    92
#define TK_STAREQ     93
#define TK_SLASHEQ    94
#define TK_PERCENTEQ  95
#define TK_AMPEQ      96
#define TK_PIPEEQ     97
#define TK_CARETEQ    98
#define TK_LSHIFTEQ   99
#define TK_RSHIFTEQ  100

#define TK_EQ        110
#define TK_NE        111
#define TK_LT        112
#define TK_GT        113
#define TK_LE        114
#define TK_GE        115

#define TK_LAND      120
#define TK_LOR       121
#define TK_INC       122
#define TK_DEC       123
#define TK_QMARK     124
#define TK_COLON     125
#define TK_HASH      126
#define TK_HASHHASH  127

/* === Lexer globals === */

#define LEX_SRC_SZ   1100000
#define LEX_STR_SZ   256
#define LEX_POOL_SZ  524288
#define LEX_POOL_MAX 16384

static char lex_src[LEX_SRC_SZ];
static int  lex_len;
static int  lex_line;
static int  lex_col;
static int  lex_tok;
static int  lex_val;
static char lex_str[LEX_STR_SZ];
static int  lex_slen;

/* For lex_pos compatibility (parser uses it for #include save/restore) */
static int  lex_pos;

/* String pool */
static char lex_strpool[LEX_POOL_SZ];
static int  lex_strpool_len;
static int  lex_str_off[LEX_POOL_MAX];
static int  lex_str_len[LEX_POOL_MAX];
static int  lex_str_count;

/* Ragel persistent state */
static char *lex_rp;    /* scan cursor (Ragel p) */
static char *lex_rpe;   /* end pointer (Ragel pe) */
static int   lex_rcs;   /* current state */
static int   lex_ract;  /* scanner action */
static char *lex_rts;   /* token start */
static char *lex_rte;   /* token end */

/* === Keyword lookup === */

int lex_kw_lookup(char *name) {
    int c;
    c = name[0];
    if (c == 97) {
        if (strcmp(name, "auto") == 0) return TK_AUTO;
        return 0;
    }
    if (c == 98) {
        if (strcmp(name, "break") == 0) return TK_BREAK;
        return 0;
    }
    if (c == 99) {
        if (strcmp(name, "case") == 0) return TK_CASE;
        if (strcmp(name, "char") == 0) return TK_CHAR;
        if (strcmp(name, "const") == 0) return TK_CONST;
        if (strcmp(name, "continue") == 0) return TK_CONTINUE;
        return 0;
    }
    if (c == 100) {
        if (strcmp(name, "default") == 0) return TK_DEFAULT;
        if (strcmp(name, "do") == 0) return TK_DO;
        if (strcmp(name, "double") == 0) return TK_DOUBLE;
        return 0;
    }
    if (c == 101) {
        if (strcmp(name, "else") == 0) return TK_ELSE;
        if (strcmp(name, "enum") == 0) return TK_ENUM;
        if (strcmp(name, "extern") == 0) return TK_EXTERN;
        return 0;
    }
    if (c == 102) {
        if (strcmp(name, "float") == 0) return TK_FLOAT;
        if (strcmp(name, "for") == 0) return TK_FOR;
        return 0;
    }
    if (c == 103) {
        if (strcmp(name, "goto") == 0) return TK_GOTO;
        return 0;
    }
    if (c == 105) {
        if (strcmp(name, "if") == 0) return TK_IF;
        if (strcmp(name, "inline") == 0) return TK_INLINE;
        if (strcmp(name, "int") == 0) return TK_INT;
        return 0;
    }
    if (c == 108) {
        if (strcmp(name, "long") == 0) return TK_LONG;
        return 0;
    }
    if (c == 114) {
        if (strcmp(name, "register") == 0) return TK_REGISTER;
        if (strcmp(name, "restrict") == 0) return TK_RESTRICT;
        if (strcmp(name, "return") == 0) return TK_RETURN;
        return 0;
    }
    if (c == 115) {
        if (strcmp(name, "short") == 0) return TK_SHORT;
        if (strcmp(name, "signed") == 0) return TK_SIGNED;
        if (strcmp(name, "sizeof") == 0) return TK_SIZEOF;
        if (strcmp(name, "static") == 0) return TK_STATIC;
        if (strcmp(name, "struct") == 0) return TK_STRUCT;
        if (strcmp(name, "switch") == 0) return TK_SWITCH;
        return 0;
    }
    if (c == 116) {
        if (strcmp(name, "typedef") == 0) return TK_TYPEDEF;
        return 0;
    }
    if (c == 117) {
        if (strcmp(name, "union") == 0) return TK_UNION;
        if (strcmp(name, "unsigned") == 0) return TK_UNSIGNED;
        return 0;
    }
    if (c == 118) {
        if (strcmp(name, "void") == 0) return TK_VOID;
        if (strcmp(name, "volatile") == 0) return TK_VOLATILE;
        return 0;
    }
    if (c == 119) {
        if (strcmp(name, "while") == 0) return TK_WHILE;
        return 0;
    }
    return 0;
}

/* === Escape sequence parser (shared by string and char actions) === */

static int lex_parse_esc(char *s, int *posout) {
    int ch;
    int val;
    int i;
    int pos;
    pos = *posout;
    ch = s[pos] & 255;
    pos = pos + 1;
    if (ch == 110) { *posout = pos; return 10; }
    if (ch == 116) { *posout = pos; return 9; }
    if (ch == 114) { *posout = pos; return 13; }
    if (ch == 48) {
        ch = s[pos] & 255;
        if (ch >= 49 && ch <= 55) {
            val = 0; i = 0;
            while (i < 3) {
                ch = s[pos] & 255;
                if (ch < 48 || ch > 55) break;
                val = val * 8 + (ch - 48);
                pos = pos + 1; i = i + 1;
            }
            *posout = pos; return val;
        }
        *posout = pos; return 0;
    }
    if (ch == 97)  { *posout = pos; return 7; }
    if (ch == 98)  { *posout = pos; return 8; }
    if (ch == 102) { *posout = pos; return 12; }
    if (ch == 118) { *posout = pos; return 11; }
    if (ch == 92)  { *posout = pos; return 92; }
    if (ch == 39)  { *posout = pos; return 39; }
    if (ch == 34)  { *posout = pos; return 34; }
    if (ch == 120) {
        val = 0; i = 0;
        while (i < 2) {
            ch = s[pos] & 255;
            if (ch >= 48 && ch <= 57) { val = val * 16 + (ch - 48); }
            else if (ch >= 97 && ch <= 102) { val = val * 16 + (ch - 87); }
            else if (ch >= 65 && ch <= 70) { val = val * 16 + (ch - 55); }
            else break;
            pos = pos + 1; i = i + 1;
        }
        *posout = pos; return val;
    }
    if (ch >= 49 && ch <= 55) {
        val = ch - 48; i = 0;
        while (i < 2) {
            ch = s[pos] & 255;
            if (ch < 48 || ch > 55) break;
            val = val * 8 + (ch - 48);
            pos = pos + 1; i = i + 1;
        }
        *posout = pos; return val;
    }
    *posout = pos; return ch;
}

/* === Helper: count newlines in range === */

static void lex_count_nl(char *from, char *to) {
    char *cp;
    cp = from;
    while (cp < to) {
        if (*cp == 10) lex_line = lex_line + 1;
        cp = cp + 1;
    }
}

/* === Helper: parse number from ts..te range === */

static void lex_parse_num(char *ts, char *te) {
    int val;
    int ch;
    char *np;
    val = 0;
    np = ts;
    ch = *np & 255;
    if (ch == 48) {
        np = np + 1;
        if (np < te) {
            ch = *np & 255;
            if (ch == 120 || ch == 88) {
                np = np + 1;
                while (np < te) {
                    ch = *np & 255;
                    if (ch >= 48 && ch <= 57) val = val * 16 + (ch - 48);
                    else if (ch >= 97 && ch <= 102) val = val * 16 + (ch - 87);
                    else if (ch >= 65 && ch <= 70) val = val * 16 + (ch - 55);
                    else break;
                    np = np + 1;
                }
            } else if (ch >= 48 && ch <= 55) {
                while (np < te) {
                    ch = *np & 255;
                    if (ch < 48 || ch > 55) break;
                    val = val * 8 + (ch - 48);
                    np = np + 1;
                }
            }
        }
    } else {
        while (np < te) {
            ch = *np & 255;
            if (ch < 48 || ch > 57) break;
            val = val * 10 + (ch - 48);
            np = np + 1;
        }
    }
    lex_tok = TK_NUM;
    lex_val = val;
}

/* === Helper: parse string literal from ts..te (includes quotes) === */

static void lex_parse_str(char *ts, char *te) {
    int pool_start;
    int slen;
    int ch;
    int pos;
    int end;
    pool_start = lex_strpool_len;
    slen = 0;
    pos = 1;  /* skip opening quote */
    end = (int)(te - ts) - 1;  /* before closing quote */
    while (pos < end) {
        ch = ts[pos] & 255;
        if (ch == 92) {
            pos = pos + 1;
            ch = lex_parse_esc(ts, &pos);
        } else {
            pos = pos + 1;
        }
        if (lex_strpool_len < LEX_POOL_SZ - 1) {
            lex_strpool[lex_strpool_len] = ch;
            lex_strpool_len = lex_strpool_len + 1;
        }
        if (slen < LEX_STR_SZ - 1) {
            lex_str[slen] = ch;
            slen = slen + 1;
        }
    }
    if (lex_strpool_len < LEX_POOL_SZ) {
        lex_strpool[lex_strpool_len] = 0;
        lex_strpool_len = lex_strpool_len + 1;
    }
    lex_str[slen] = 0;
    lex_slen = slen;
    if (lex_str_count < LEX_POOL_MAX) {
        lex_str_off[lex_str_count] = pool_start;
        lex_str_len[lex_str_count] = slen;
        lex_val = lex_str_count;
        lex_str_count = lex_str_count + 1;
    }
    lex_tok = TK_STRING;
}

/* === Helper: parse char literal from ts..te (includes quotes) === */

static void lex_parse_chr(char *ts, char *te) {
    int ch;
    int pos;
    pos = 1;  /* skip opening quote */
    ch = ts[pos] & 255;
    if (ch == 92) {
        pos = pos + 1;
        ch = lex_parse_esc(ts, &pos);
    }
    lex_tok = TK_CHARLIT;
    lex_val = ch;
}

/* === Helper: copy ident from ts..te, do keyword lookup === */

static void lex_parse_id(char *ts, char *te) {
    int len;
    int kw;
    len = (int)(te - ts);
    if (len > LEX_STR_SZ - 1) len = LEX_STR_SZ - 1;
    memcpy(lex_str, ts, len);
    lex_str[len] = 0;
    lex_slen = len;
    kw = lex_kw_lookup(lex_str);
    if (kw != 0) lex_tok = kw;
    else lex_tok = TK_IDENT;
}

/* ================================================================
 * Ragel machine definition
 * ================================================================ */


#line 535 "c_lexer.rl"


/* === Ragel data tables === */


#line 438 "c_lexer_gen.c"
static const int c_lexer_start = 9;
static const int c_lexer_error = 0;

static const int c_lexer_en_main = 9;


#line 540 "c_lexer.rl"

/* === lex_init === */

void lex_init(char *src, int len) {
    int i;
    int cs;
    char *ts;
    char *te;
    int act;
    i = 0;
    while (i < len && i < LEX_SRC_SZ - 1) {
        lex_src[i] = src[i];
        i = i + 1;
    }
    lex_src[i] = 0;
    lex_len = i;
    lex_line = 1;
    lex_col = 1;
    lex_tok = TK_EOF;
    lex_val = 0;
    lex_slen = 0;
    lex_str[0] = 0;
    lex_pos = 0;

    lex_strpool_len = 0;
    lex_str_count = 0;

    lex_rp = lex_src;
    lex_rpe = lex_src + lex_len;

    
#line 473 "c_lexer_gen.c"
	{
	cs = c_lexer_start;
	ts = 0;
	te = 0;
	act = 0;
	}

#line 571 "c_lexer.rl"
    lex_rcs = cs;
    lex_ract = 0;
    lex_rts = 0;
    lex_rte = 0;
}

/* === lex_next -- scan one token === */

void lex_next(void) {
    char *p;
    char *pe;
    char *eof;
    char *ts;
    char *te;
    int cs;
    int act;

    p = lex_rp;
    pe = lex_rpe;
    eof = pe;
    cs = lex_rcs;
    act = lex_ract;
    ts = lex_rts;
    te = lex_rte;

    lex_tok = TK_EOF;

    if (p >= pe) return;

    
#line 508 "c_lexer_gen.c"
	{
	if ( p == pe )
		goto _test_eof;
	switch ( cs )
	{
tr2:
#line 465 "c_lexer.rl"
	{te = p+1;{
            lex_parse_str(ts, te);
            {p++; cs = 9; goto _out;}
        }}
	goto st9;
tr5:
#line 471 "c_lexer.rl"
	{te = p+1;{
            lex_parse_chr(ts, te);
            {p++; cs = 9; goto _out;}
        }}
	goto st9;
tr7:
#line 516 "c_lexer.rl"
	{{p = ((te))-1;}{ lex_tok = TK_DOT; {p++; cs = 9; goto _out;} }}
	goto st9;
tr8:
#line 483 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_ELLIPSIS; {p++; cs = 9; goto _out;} }}
	goto st9;
tr9:
#line 524 "c_lexer.rl"
	{{p = ((te))-1;}{ lex_tok = TK_SLASH; {p++; cs = 9; goto _out;} }}
	goto st9;
tr12:
#line 446 "c_lexer.rl"
	{te = p+1;{
            lex_count_nl(ts, te);
        }}
	goto st9;
tr13:
#line 459 "c_lexer.rl"
	{{p = ((te))-1;}{
            lex_parse_num(ts, te);
            {p++; cs = 9; goto _out;}
        }}
	goto st9;
tr16:
#line 440 "c_lexer.rl"
	{te = p+1;{ lex_line = lex_line + 1; }}
	goto st9;
tr21:
#line 508 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LPAREN; {p++; cs = 9; goto _out;} }}
	goto st9;
tr22:
#line 509 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RPAREN; {p++; cs = 9; goto _out;} }}
	goto st9;
tr25:
#line 515 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_COMMA; {p++; cs = 9; goto _out;} }}
	goto st9;
tr31:
#line 517 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_COLON; {p++; cs = 9; goto _out;} }}
	goto st9;
tr32:
#line 514 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_SEMI; {p++; cs = 9; goto _out;} }}
	goto st9;
tr36:
#line 518 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_QMARK; {p++; cs = 9; goto _out;} }}
	goto st9;
tr38:
#line 510 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LBRACK; {p++; cs = 9; goto _out;} }}
	goto st9;
tr39:
#line 511 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RBRACK; {p++; cs = 9; goto _out;} }}
	goto st9;
tr41:
#line 512 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LBRACE; {p++; cs = 9; goto _out;} }}
	goto st9;
tr43:
#line 513 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RBRACE; {p++; cs = 9; goto _out;} }}
	goto st9;
tr44:
#line 519 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_TILDE; {p++; cs = 9; goto _out;} }}
	goto st9;
tr45:
#line 439 "c_lexer.rl"
	{te = p;p--;{ /* skip */ }}
	goto st9;
tr46:
#line 529 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_BANG; {p++; cs = 9; goto _out;} }}
	goto st9;
tr47:
#line 496 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_NE; {p++; cs = 9; goto _out;} }}
	goto st9;
tr48:
#line 520 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_HASH; {p++; cs = 9; goto _out;} }}
	goto st9;
tr49:
#line 486 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_HASHHASH; {p++; cs = 9; goto _out;} }}
	goto st9;
tr50:
#line 525 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_PERCENT; {p++; cs = 9; goto _out;} }}
	goto st9;
tr51:
#line 491 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_PERCENTEQ; {p++; cs = 9; goto _out;} }}
	goto st9;
tr52:
#line 526 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_AMP; {p++; cs = 9; goto _out;} }}
	goto st9;
tr53:
#line 501 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LAND; {p++; cs = 9; goto _out;} }}
	goto st9;
tr54:
#line 492 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_AMPEQ; {p++; cs = 9; goto _out;} }}
	goto st9;
tr55:
#line 523 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_STAR; {p++; cs = 9; goto _out;} }}
	goto st9;
tr56:
#line 489 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_STAREQ; {p++; cs = 9; goto _out;} }}
	goto st9;
tr57:
#line 521 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_PLUS; {p++; cs = 9; goto _out;} }}
	goto st9;
tr58:
#line 503 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_INC; {p++; cs = 9; goto _out;} }}
	goto st9;
tr59:
#line 487 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_PLUSEQ; {p++; cs = 9; goto _out;} }}
	goto st9;
tr60:
#line 522 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_MINUS; {p++; cs = 9; goto _out;} }}
	goto st9;
tr61:
#line 504 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_DEC; {p++; cs = 9; goto _out;} }}
	goto st9;
tr62:
#line 488 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_MINUSEQ; {p++; cs = 9; goto _out;} }}
	goto st9;
tr63:
#line 505 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_ARROW; {p++; cs = 9; goto _out;} }}
	goto st9;
tr64:
#line 516 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_DOT; {p++; cs = 9; goto _out;} }}
	goto st9;
tr66:
#line 524 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_SLASH; {p++; cs = 9; goto _out;} }}
	goto st9;
tr68:
#line 490 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_SLASHEQ; {p++; cs = 9; goto _out;} }}
	goto st9;
tr69:
#line 443 "c_lexer.rl"
	{te = p;p--;{ /* skip */ }}
	goto st9;
tr70:
#line 459 "c_lexer.rl"
	{te = p;p--;{
            lex_parse_num(ts, te);
            {p++; cs = 9; goto _out;}
        }}
	goto st9;
tr74:
#line 455 "c_lexer.rl"
	{te = p;p--;{
            lex_parse_num(ts, te);
            {p++; cs = 9; goto _out;}
        }}
	goto st9;
tr76:
#line 451 "c_lexer.rl"
	{te = p;p--;{
            lex_parse_num(ts, te);
            {p++; cs = 9; goto _out;}
        }}
	goto st9;
tr78:
#line 531 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_LT; {p++; cs = 9; goto _out;} }}
	goto st9;
tr80:
#line 497 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LE; {p++; cs = 9; goto _out;} }}
	goto st9;
tr81:
#line 499 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_LSHIFT; {p++; cs = 9; goto _out;} }}
	goto st9;
tr82:
#line 484 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LSHIFTEQ; {p++; cs = 9; goto _out;} }}
	goto st9;
tr83:
#line 530 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_ASSIGN; {p++; cs = 9; goto _out;} }}
	goto st9;
tr84:
#line 495 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_EQ; {p++; cs = 9; goto _out;} }}
	goto st9;
tr85:
#line 532 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_GT; {p++; cs = 9; goto _out;} }}
	goto st9;
tr86:
#line 498 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_GE; {p++; cs = 9; goto _out;} }}
	goto st9;
tr88:
#line 500 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_RSHIFT; {p++; cs = 9; goto _out;} }}
	goto st9;
tr89:
#line 485 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RSHIFTEQ; {p++; cs = 9; goto _out;} }}
	goto st9;
tr90:
#line 477 "c_lexer.rl"
	{te = p;p--;{
            lex_parse_id(ts, te);
            {p++; cs = 9; goto _out;}
        }}
	goto st9;
tr91:
#line 528 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_CARET; {p++; cs = 9; goto _out;} }}
	goto st9;
tr92:
#line 494 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_CARETEQ; {p++; cs = 9; goto _out;} }}
	goto st9;
tr93:
#line 527 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_PIPE; {p++; cs = 9; goto _out;} }}
	goto st9;
tr94:
#line 493 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_PIPEEQ; {p++; cs = 9; goto _out;} }}
	goto st9;
tr95:
#line 502 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LOR; {p++; cs = 9; goto _out;} }}
	goto st9;
st9:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof9;
case 9:
#line 1 "NONE"
	{ts = p;}
#line 725 "c_lexer_gen.c"
	switch( (*p) ) {
		case 10: goto tr16;
		case 32: goto st10;
		case 33: goto st11;
		case 34: goto st1;
		case 35: goto st12;
		case 37: goto st13;
		case 38: goto st14;
		case 39: goto st3;
		case 40: goto tr21;
		case 41: goto tr22;
		case 42: goto st15;
		case 43: goto st16;
		case 44: goto tr25;
		case 45: goto st17;
		case 46: goto tr27;
		case 47: goto tr28;
		case 48: goto tr29;
		case 58: goto tr31;
		case 59: goto tr32;
		case 60: goto st28;
		case 61: goto st30;
		case 62: goto st31;
		case 63: goto tr36;
		case 91: goto tr38;
		case 93: goto tr39;
		case 94: goto st34;
		case 95: goto st33;
		case 123: goto tr41;
		case 124: goto st35;
		case 125: goto tr43;
		case 126: goto tr44;
	}
	if ( (*p) < 49 ) {
		if ( 9 <= (*p) && (*p) <= 13 )
			goto st10;
	} else if ( (*p) > 57 ) {
		if ( (*p) > 90 ) {
			if ( 97 <= (*p) && (*p) <= 122 )
				goto st33;
		} else if ( (*p) >= 65 )
			goto st33;
	} else
		goto st23;
	goto st0;
st0:
cs = 0;
	goto _out;
st10:
	if ( ++p == pe )
		goto _test_eof10;
case 10:
	switch( (*p) ) {
		case 9: goto st10;
		case 32: goto st10;
	}
	if ( 11 <= (*p) && (*p) <= 13 )
		goto st10;
	goto tr45;
st11:
	if ( ++p == pe )
		goto _test_eof11;
case 11:
	if ( (*p) == 61 )
		goto tr47;
	goto tr46;
st1:
	if ( ++p == pe )
		goto _test_eof1;
case 1:
	switch( (*p) ) {
		case 10: goto st0;
		case 34: goto tr2;
		case 92: goto st2;
	}
	goto st1;
st2:
	if ( ++p == pe )
		goto _test_eof2;
case 2:
	goto st1;
st12:
	if ( ++p == pe )
		goto _test_eof12;
case 12:
	if ( (*p) == 35 )
		goto tr49;
	goto tr48;
st13:
	if ( ++p == pe )
		goto _test_eof13;
case 13:
	if ( (*p) == 61 )
		goto tr51;
	goto tr50;
st14:
	if ( ++p == pe )
		goto _test_eof14;
case 14:
	switch( (*p) ) {
		case 38: goto tr53;
		case 61: goto tr54;
	}
	goto tr52;
st3:
	if ( ++p == pe )
		goto _test_eof3;
case 3:
	switch( (*p) ) {
		case 10: goto st0;
		case 39: goto tr5;
		case 92: goto st4;
	}
	goto st3;
st4:
	if ( ++p == pe )
		goto _test_eof4;
case 4:
	goto st3;
st15:
	if ( ++p == pe )
		goto _test_eof15;
case 15:
	if ( (*p) == 61 )
		goto tr56;
	goto tr55;
st16:
	if ( ++p == pe )
		goto _test_eof16;
case 16:
	switch( (*p) ) {
		case 43: goto tr58;
		case 61: goto tr59;
	}
	goto tr57;
st17:
	if ( ++p == pe )
		goto _test_eof17;
case 17:
	switch( (*p) ) {
		case 45: goto tr61;
		case 61: goto tr62;
		case 62: goto tr63;
	}
	goto tr60;
tr27:
#line 1 "NONE"
	{te = p+1;}
	goto st18;
st18:
	if ( ++p == pe )
		goto _test_eof18;
case 18:
#line 877 "c_lexer_gen.c"
	if ( (*p) == 46 )
		goto st5;
	goto tr64;
st5:
	if ( ++p == pe )
		goto _test_eof5;
case 5:
	if ( (*p) == 46 )
		goto tr8;
	goto tr7;
tr28:
#line 1 "NONE"
	{te = p+1;}
	goto st19;
st19:
	if ( ++p == pe )
		goto _test_eof19;
case 19:
#line 894 "c_lexer_gen.c"
	switch( (*p) ) {
		case 42: goto st6;
		case 47: goto st20;
		case 61: goto tr68;
	}
	goto tr66;
st6:
	if ( ++p == pe )
		goto _test_eof6;
case 6:
	if ( (*p) == 42 )
		goto st7;
	goto st6;
st7:
	if ( ++p == pe )
		goto _test_eof7;
case 7:
	switch( (*p) ) {
		case 42: goto st7;
		case 47: goto tr12;
	}
	goto st6;
st20:
	if ( ++p == pe )
		goto _test_eof20;
case 20:
	if ( (*p) == 10 )
		goto tr69;
	goto st20;
tr29:
#line 1 "NONE"
	{te = p+1;}
	goto st21;
st21:
	if ( ++p == pe )
		goto _test_eof21;
case 21:
#line 930 "c_lexer_gen.c"
	switch( (*p) ) {
		case 76: goto st24;
		case 85: goto st24;
		case 88: goto st8;
		case 108: goto st24;
		case 117: goto st24;
		case 120: goto st8;
	}
	if ( (*p) > 55 ) {
		if ( 56 <= (*p) && (*p) <= 57 )
			goto st23;
	} else if ( (*p) >= 48 )
		goto st22;
	goto tr70;
st22:
	if ( ++p == pe )
		goto _test_eof22;
case 22:
	switch( (*p) ) {
		case 76: goto st25;
		case 85: goto st25;
		case 108: goto st25;
		case 117: goto st25;
	}
	if ( (*p) > 55 ) {
		if ( 56 <= (*p) && (*p) <= 57 )
			goto st23;
	} else if ( (*p) >= 48 )
		goto st22;
	goto tr74;
st23:
	if ( ++p == pe )
		goto _test_eof23;
case 23:
	switch( (*p) ) {
		case 76: goto st24;
		case 85: goto st24;
		case 108: goto st24;
		case 117: goto st24;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st23;
	goto tr70;
st24:
	if ( ++p == pe )
		goto _test_eof24;
case 24:
	switch( (*p) ) {
		case 76: goto st24;
		case 85: goto st24;
		case 108: goto st24;
		case 117: goto st24;
	}
	goto tr70;
st25:
	if ( ++p == pe )
		goto _test_eof25;
case 25:
	switch( (*p) ) {
		case 76: goto st25;
		case 85: goto st25;
		case 108: goto st25;
		case 117: goto st25;
	}
	goto tr74;
st8:
	if ( ++p == pe )
		goto _test_eof8;
case 8:
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st26;
	} else if ( (*p) > 70 ) {
		if ( 97 <= (*p) && (*p) <= 102 )
			goto st26;
	} else
		goto st26;
	goto tr13;
st26:
	if ( ++p == pe )
		goto _test_eof26;
case 26:
	switch( (*p) ) {
		case 76: goto st27;
		case 85: goto st27;
		case 108: goto st27;
		case 117: goto st27;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st26;
	} else if ( (*p) > 70 ) {
		if ( 97 <= (*p) && (*p) <= 102 )
			goto st26;
	} else
		goto st26;
	goto tr76;
st27:
	if ( ++p == pe )
		goto _test_eof27;
case 27:
	switch( (*p) ) {
		case 76: goto st27;
		case 85: goto st27;
		case 108: goto st27;
		case 117: goto st27;
	}
	goto tr76;
st28:
	if ( ++p == pe )
		goto _test_eof28;
case 28:
	switch( (*p) ) {
		case 60: goto st29;
		case 61: goto tr80;
	}
	goto tr78;
st29:
	if ( ++p == pe )
		goto _test_eof29;
case 29:
	if ( (*p) == 61 )
		goto tr82;
	goto tr81;
st30:
	if ( ++p == pe )
		goto _test_eof30;
case 30:
	if ( (*p) == 61 )
		goto tr84;
	goto tr83;
st31:
	if ( ++p == pe )
		goto _test_eof31;
case 31:
	switch( (*p) ) {
		case 61: goto tr86;
		case 62: goto st32;
	}
	goto tr85;
st32:
	if ( ++p == pe )
		goto _test_eof32;
case 32:
	if ( (*p) == 61 )
		goto tr89;
	goto tr88;
st33:
	if ( ++p == pe )
		goto _test_eof33;
case 33:
	if ( (*p) == 95 )
		goto st33;
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st33;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st33;
	} else
		goto st33;
	goto tr90;
st34:
	if ( ++p == pe )
		goto _test_eof34;
case 34:
	if ( (*p) == 61 )
		goto tr92;
	goto tr91;
st35:
	if ( ++p == pe )
		goto _test_eof35;
case 35:
	switch( (*p) ) {
		case 61: goto tr94;
		case 124: goto tr95;
	}
	goto tr93;
	}
	_test_eof9: cs = 9; goto _test_eof; 
	_test_eof10: cs = 10; goto _test_eof; 
	_test_eof11: cs = 11; goto _test_eof; 
	_test_eof1: cs = 1; goto _test_eof; 
	_test_eof2: cs = 2; goto _test_eof; 
	_test_eof12: cs = 12; goto _test_eof; 
	_test_eof13: cs = 13; goto _test_eof; 
	_test_eof14: cs = 14; goto _test_eof; 
	_test_eof3: cs = 3; goto _test_eof; 
	_test_eof4: cs = 4; goto _test_eof; 
	_test_eof15: cs = 15; goto _test_eof; 
	_test_eof16: cs = 16; goto _test_eof; 
	_test_eof17: cs = 17; goto _test_eof; 
	_test_eof18: cs = 18; goto _test_eof; 
	_test_eof5: cs = 5; goto _test_eof; 
	_test_eof19: cs = 19; goto _test_eof; 
	_test_eof6: cs = 6; goto _test_eof; 
	_test_eof7: cs = 7; goto _test_eof; 
	_test_eof20: cs = 20; goto _test_eof; 
	_test_eof21: cs = 21; goto _test_eof; 
	_test_eof22: cs = 22; goto _test_eof; 
	_test_eof23: cs = 23; goto _test_eof; 
	_test_eof24: cs = 24; goto _test_eof; 
	_test_eof25: cs = 25; goto _test_eof; 
	_test_eof8: cs = 8; goto _test_eof; 
	_test_eof26: cs = 26; goto _test_eof; 
	_test_eof27: cs = 27; goto _test_eof; 
	_test_eof28: cs = 28; goto _test_eof; 
	_test_eof29: cs = 29; goto _test_eof; 
	_test_eof30: cs = 30; goto _test_eof; 
	_test_eof31: cs = 31; goto _test_eof; 
	_test_eof32: cs = 32; goto _test_eof; 
	_test_eof33: cs = 33; goto _test_eof; 
	_test_eof34: cs = 34; goto _test_eof; 
	_test_eof35: cs = 35; goto _test_eof; 

	_test_eof: {}
	if ( p == eof )
	{
	switch ( cs ) {
	case 10: goto tr45;
	case 11: goto tr46;
	case 12: goto tr48;
	case 13: goto tr50;
	case 14: goto tr52;
	case 15: goto tr55;
	case 16: goto tr57;
	case 17: goto tr60;
	case 18: goto tr64;
	case 5: goto tr7;
	case 19: goto tr66;
	case 6: goto tr9;
	case 7: goto tr9;
	case 20: goto tr69;
	case 21: goto tr70;
	case 22: goto tr74;
	case 23: goto tr70;
	case 24: goto tr70;
	case 25: goto tr74;
	case 8: goto tr13;
	case 26: goto tr76;
	case 27: goto tr76;
	case 28: goto tr78;
	case 29: goto tr81;
	case 30: goto tr83;
	case 31: goto tr85;
	case 32: goto tr88;
	case 33: goto tr90;
	case 34: goto tr91;
	case 35: goto tr93;
	}
	}

	_out: {}
	}

#line 601 "c_lexer.rl"

    lex_rp = p;
    lex_rcs = cs;
    lex_ract = act;
    lex_rts = ts;
    lex_rte = te;

    /* Update lex_pos for compatibility */
    lex_pos = (int)(p - lex_src);
}
