
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
#define TK_FNUM       5   /* float/double literal */

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
#define TK_OFFSETOF   44  /* __builtin_offsetof */
#define TK_STATIC_ASSERT 45  /* _Static_assert */

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
static int  lex_fval_hi;   /* high 32 bits for double literals */
static int  lex_fty;       /* TY_FLOAT or TY_DOUBLE for float literals */
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
    if (c == 95) {  /* '_' */
        if (strcmp(name, "__builtin_offsetof") == 0) return TK_OFFSETOF;
        if (strcmp(name, "_Static_assert") == 0) return TK_STATIC_ASSERT;
        return 0;
    }
    if (c == 111) {  /* 'o' */
        if (strcmp(name, "offsetof") == 0) return TK_OFFSETOF;
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

/* === Helper: decimal to IEEE 754 binary32 (32-bit int only) === */
/* sig: decimal significand (up to 9 digits, fits int)
 * exp10: decimal exponent (value = sig * 10^exp10)
 * neg: sign (0=positive, 1=negative)
 * Returns: IEEE 754 binary32 bits packed in int */

static int lex_soft_f32(int sig, int exp10, int neg) {
    int m;
    int e2;
    int biased;

    if (sig == 0) return neg ? (1 << 31) : 0;

    m = sig;
    e2 = 0;

    /* Normalize m to [2^20, 2^27) for precision headroom.
     * Max after *5 is ~2^29.3, still fits 32-bit signed int. */
    while (m < 0 || m >= (1 << 27)) { m = (m >> 1) & 0x7FFFFFFF; e2 = e2 + 1; }
    while (m < (1 << 20)) { m = m << 1; e2 = e2 - 1; }

    /* Apply 10^exp10 = 2^exp10 * 5^exp10 */
    while (exp10 > 0) {
        m = m * 5;
        e2 = e2 + 1;
        while (m < 0 || m >= (1 << 27)) { m = (m >> 1) & 0x7FFFFFFF; e2 = e2 + 1; }
        exp10 = exp10 - 1;
    }
    while (exp10 < 0) {
        while (m < (1 << 24)) { m = m << 1; e2 = e2 - 1; }
        m = (m + 2) / 5;  /* rounded division */
        e2 = e2 - 1;
        exp10 = exp10 + 1;
    }

    /* Normalize to [2^23, 2^24) for f32 mantissa */
    while (m >= (1 << 24)) {
        m = (m + 1) >> 1;  /* round */
        e2 = e2 + 1;
    }
    while (m > 0 && m < (1 << 23)) { m = m << 1; e2 = e2 - 1; }

    /* value = m * 2^e2, m in [2^23, 2^24)
     * IEEE: 1.frac * 2^(biased-127), biased = e2+150 */
    biased = e2 + 150;

    if (biased >= 255) return (neg << 31) | 0x7F800000;  /* infinity */
    if (biased <= 0) {
        if (biased < -23) return neg << 31;  /* zero */
        m = m >> (1 - biased);
        biased = 0;
    }

    return (neg << 31) | (biased << 23) | (m & 0x7FFFFF);
}

/* === Helper: decimal to IEEE 754 binary64 (32-bit int only) === */
/* Computes f32 first, then promotes to f64 by adjusting exponent and
 * extending mantissa.  Precision limited to ~24 bits (f32 level),
 * sufficient for bootstrap. */
/* Returns lo 32 bits; lex_fval_hi receives hi 32 bits */

static int lex_soft_f64(int sig, int exp10, int neg) {
    int f32bits;
    int s;
    int exp8;
    int mant23;
    int exp11;
    int hi;
    int lo;

    if (sig == 0) {
        lex_fval_hi = neg ? (1 << 31) : 0;
        return 0;
    }

    /* Compute f32 representation first */
    f32bits = lex_soft_f32(sig, exp10, neg);

    s = (f32bits >> 31) & 1;
    exp8 = (f32bits >> 23) & 0xFF;
    mant23 = f32bits & 0x7FFFFF;

    if (exp8 == 0) {
        /* Zero or denormal → f64 zero */
        lex_fval_hi = s << 31;
        return 0;
    }
    if (exp8 == 255) {
        /* Infinity or NaN */
        lex_fval_hi = (s << 31) | 0x7FF00000;
        if (mant23) lex_fval_hi = lex_fval_hi | 0x80000;  /* NaN */
        return 0;
    }

    /* Normal: f64_exp = f32_exp - 127 + 1023 = f32_exp + 896 */
    exp11 = exp8 + 896;

    /* f64 mantissa = f32 mantissa << 29 (52-23 = 29 extra bits, all zero)
     * Split into hi (top 20 bits) and lo (bottom 32 bits):
     *   f64_mant[51:32] = mant23 >> 3  (top 20 bits of 23-bit mantissa)
     *   f64_mant[31:0]  = (mant23 & 7) << 29  (bottom 3 bits shifted up) */
    hi = (s << 31) | (exp11 << 20) | (mant23 >> 3);
    lo = (mant23 & 7) << 29;

    lex_fval_hi = hi;
    return lo;
}

/* === Helper: parse float literal from ts..te === */

static void lex_parse_fnum(char *ts, char *te) {
    int sig;
    int exp10;
    int neg;
    int is_float;
    int ch;
    char *np;
    int saw_dot;
    int frac_digits;
    int eneg;
    int eval;

    sig = 0;
    exp10 = 0;
    neg = 0;
    is_float = 0;  /* 0=double, 1=float */
    saw_dot = 0;
    frac_digits = 0;
    np = ts;

    /* Parse sign (unlikely in literal, but handle) */
    if (np < te && *np == 45) { neg = 1; np = np + 1; }
    else if (np < te && *np == 43) { np = np + 1; }

    /* Parse integer and fractional parts */
    while (np < te) {
        ch = *np & 255;
        if (ch == 46) { /* '.' */
            saw_dot = 1;
            np = np + 1;
        } else if (ch == 101 || ch == 69) { /* 'e' or 'E' */
            break;
        } else if (ch == 102 || ch == 70) { /* 'f' or 'F' */
            is_float = 1;
            np = np + 1;
        } else if (ch == 108 || ch == 76) { /* 'l' or 'L' */
            np = np + 1;  /* skip suffix */
        } else if (ch >= 48 && ch <= 57) {
            /* Accumulate up to 9 significant digits (999999999 < 2^30) */
            if (sig < 100000000) {
                sig = sig * 10 + (ch - 48);
                if (saw_dot) frac_digits = frac_digits + 1;
            } else {
                /* Overflow: just adjust exponent for excess digits */
                if (!saw_dot) exp10 = exp10 + 1;
            }
            np = np + 1;
        } else {
            break;
        }
    }
    exp10 = exp10 - frac_digits;

    /* Parse exponent */
    if (np < te && (*np == 101 || *np == 69)) {
        np = np + 1;
        eneg = 0;
        eval = 0;
        if (np < te && *np == 45) { eneg = 1; np = np + 1; }
        else if (np < te && *np == 43) { np = np + 1; }
        while (np < te) {
            ch = *np & 255;
            if (ch >= 48 && ch <= 57) {
                eval = eval * 10 + (ch - 48);
                np = np + 1;
            } else {
                break;
            }
        }
        if (eneg) exp10 = exp10 - eval;
        else exp10 = exp10 + eval;
    }

    /* Check trailing suffix */
    while (np < te) {
        ch = *np & 255;
        if (ch == 102 || ch == 70) { is_float = 1; np = np + 1; }
        else if (ch == 108 || ch == 76) { np = np + 1; }
        else { break; }
    }

    /* Convert to IEEE 754 bits */
    if (is_float) {
        lex_val = lex_soft_f32(sig, exp10, neg);
        lex_fval_hi = 0;
        lex_fty = 5;  /* TY_FLOAT */
    } else {
        lex_val = lex_soft_f64(sig, exp10, neg);
        lex_fty = 6;  /* TY_DOUBLE */
    }
    lex_tok = TK_FNUM;
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


#line 756 "c_lexer.rl"


/* === Ragel data tables === */


#line 645 "c_lexer_gen.c"
static const int c_lexer_start = 15;
static const int c_lexer_error = 0;

static const int c_lexer_en_main = 15;


#line 761 "c_lexer.rl"

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

    
#line 680 "c_lexer_gen.c"
	{
	cs = c_lexer_start;
	ts = 0;
	te = 0;
	act = 0;
	}

#line 792 "c_lexer.rl"
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

    
#line 715 "c_lexer_gen.c"
	{
	if ( p == pe )
		goto _test_eof;
	switch ( cs )
	{
tr2:
#line 686 "c_lexer.rl"
	{te = p+1;{
            lex_parse_str(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr5:
#line 692 "c_lexer.rl"
	{te = p+1;{
            lex_parse_chr(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr7:
#line 737 "c_lexer.rl"
	{{p = ((te))-1;}{ lex_tok = TK_DOT; {p++; cs = 15; goto _out;} }}
	goto st15;
tr8:
#line 704 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_ELLIPSIS; {p++; cs = 15; goto _out;} }}
	goto st15;
tr9:
#line 662 "c_lexer.rl"
	{{p = ((te))-1;}{
            lex_parse_fnum(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr12:
#line 745 "c_lexer.rl"
	{{p = ((te))-1;}{ lex_tok = TK_SLASH; {p++; cs = 15; goto _out;} }}
	goto st15;
tr15:
#line 653 "c_lexer.rl"
	{te = p+1;{
            lex_count_nl(ts, te);
        }}
	goto st15;
tr16:
#line 658 "c_lexer.rl"
	{{p = ((te))-1;}{
            lex_parse_fnum(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr19:
#line 1 "NONE"
	{	switch( act ) {
	case 9:
	{{p = ((te))-1;}
            lex_parse_num(ts, te);
            {p++; cs = 15; goto _out;}
        }
	break;
	case 10:
	{{p = ((te))-1;}
            lex_parse_num(ts, te);
            {p++; cs = 15; goto _out;}
        }
	break;
	}
	}
	goto st15;
tr22:
#line 680 "c_lexer.rl"
	{{p = ((te))-1;}{
            lex_parse_num(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr25:
#line 647 "c_lexer.rl"
	{te = p+1;{ lex_line = lex_line + 1; }}
	goto st15;
tr30:
#line 729 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LPAREN; {p++; cs = 15; goto _out;} }}
	goto st15;
tr31:
#line 730 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RPAREN; {p++; cs = 15; goto _out;} }}
	goto st15;
tr34:
#line 736 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_COMMA; {p++; cs = 15; goto _out;} }}
	goto st15;
tr40:
#line 738 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_COLON; {p++; cs = 15; goto _out;} }}
	goto st15;
tr41:
#line 735 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_SEMI; {p++; cs = 15; goto _out;} }}
	goto st15;
tr45:
#line 739 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_QMARK; {p++; cs = 15; goto _out;} }}
	goto st15;
tr47:
#line 731 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LBRACK; {p++; cs = 15; goto _out;} }}
	goto st15;
tr48:
#line 732 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RBRACK; {p++; cs = 15; goto _out;} }}
	goto st15;
tr50:
#line 733 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LBRACE; {p++; cs = 15; goto _out;} }}
	goto st15;
tr52:
#line 734 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RBRACE; {p++; cs = 15; goto _out;} }}
	goto st15;
tr53:
#line 740 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_TILDE; {p++; cs = 15; goto _out;} }}
	goto st15;
tr54:
#line 646 "c_lexer.rl"
	{te = p;p--;{ /* skip */ }}
	goto st15;
tr55:
#line 750 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_BANG; {p++; cs = 15; goto _out;} }}
	goto st15;
tr56:
#line 717 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_NE; {p++; cs = 15; goto _out;} }}
	goto st15;
tr57:
#line 741 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_HASH; {p++; cs = 15; goto _out;} }}
	goto st15;
tr58:
#line 707 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_HASHHASH; {p++; cs = 15; goto _out;} }}
	goto st15;
tr59:
#line 746 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_PERCENT; {p++; cs = 15; goto _out;} }}
	goto st15;
tr60:
#line 712 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_PERCENTEQ; {p++; cs = 15; goto _out;} }}
	goto st15;
tr61:
#line 747 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_AMP; {p++; cs = 15; goto _out;} }}
	goto st15;
tr62:
#line 722 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LAND; {p++; cs = 15; goto _out;} }}
	goto st15;
tr63:
#line 713 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_AMPEQ; {p++; cs = 15; goto _out;} }}
	goto st15;
tr64:
#line 744 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_STAR; {p++; cs = 15; goto _out;} }}
	goto st15;
tr65:
#line 710 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_STAREQ; {p++; cs = 15; goto _out;} }}
	goto st15;
tr66:
#line 742 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_PLUS; {p++; cs = 15; goto _out;} }}
	goto st15;
tr67:
#line 724 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_INC; {p++; cs = 15; goto _out;} }}
	goto st15;
tr68:
#line 708 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_PLUSEQ; {p++; cs = 15; goto _out;} }}
	goto st15;
tr69:
#line 743 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_MINUS; {p++; cs = 15; goto _out;} }}
	goto st15;
tr70:
#line 725 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_DEC; {p++; cs = 15; goto _out;} }}
	goto st15;
tr71:
#line 709 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_MINUSEQ; {p++; cs = 15; goto _out;} }}
	goto st15;
tr72:
#line 726 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_ARROW; {p++; cs = 15; goto _out;} }}
	goto st15;
tr73:
#line 737 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_DOT; {p++; cs = 15; goto _out;} }}
	goto st15;
tr76:
#line 662 "c_lexer.rl"
	{te = p;p--;{
            lex_parse_fnum(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr79:
#line 745 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_SLASH; {p++; cs = 15; goto _out;} }}
	goto st15;
tr81:
#line 711 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_SLASHEQ; {p++; cs = 15; goto _out;} }}
	goto st15;
tr82:
#line 650 "c_lexer.rl"
	{te = p;p--;{ /* skip */ }}
	goto st15;
tr83:
#line 680 "c_lexer.rl"
	{te = p;p--;{
            lex_parse_num(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr89:
#line 658 "c_lexer.rl"
	{te = p;p--;{
            lex_parse_fnum(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr92:
#line 676 "c_lexer.rl"
	{te = p;p--;{
            lex_parse_num(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr94:
#line 666 "c_lexer.rl"
	{te = p;p--;{
            lex_parse_fnum(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr96:
#line 672 "c_lexer.rl"
	{te = p;p--;{
            lex_parse_num(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr98:
#line 752 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_LT; {p++; cs = 15; goto _out;} }}
	goto st15;
tr100:
#line 718 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LE; {p++; cs = 15; goto _out;} }}
	goto st15;
tr101:
#line 720 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_LSHIFT; {p++; cs = 15; goto _out;} }}
	goto st15;
tr102:
#line 705 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LSHIFTEQ; {p++; cs = 15; goto _out;} }}
	goto st15;
tr103:
#line 751 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_ASSIGN; {p++; cs = 15; goto _out;} }}
	goto st15;
tr104:
#line 716 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_EQ; {p++; cs = 15; goto _out;} }}
	goto st15;
tr105:
#line 753 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_GT; {p++; cs = 15; goto _out;} }}
	goto st15;
tr106:
#line 719 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_GE; {p++; cs = 15; goto _out;} }}
	goto st15;
tr108:
#line 721 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_RSHIFT; {p++; cs = 15; goto _out;} }}
	goto st15;
tr109:
#line 706 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RSHIFTEQ; {p++; cs = 15; goto _out;} }}
	goto st15;
tr110:
#line 698 "c_lexer.rl"
	{te = p;p--;{
            lex_parse_id(ts, te);
            {p++; cs = 15; goto _out;}
        }}
	goto st15;
tr111:
#line 749 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_CARET; {p++; cs = 15; goto _out;} }}
	goto st15;
tr112:
#line 715 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_CARETEQ; {p++; cs = 15; goto _out;} }}
	goto st15;
tr113:
#line 748 "c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_PIPE; {p++; cs = 15; goto _out;} }}
	goto st15;
tr114:
#line 714 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_PIPEEQ; {p++; cs = 15; goto _out;} }}
	goto st15;
tr115:
#line 723 "c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LOR; {p++; cs = 15; goto _out;} }}
	goto st15;
st15:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof15;
case 15:
#line 1 "NONE"
	{ts = p;}
#line 979 "c_lexer_gen.c"
	switch( (*p) ) {
		case 10: goto tr25;
		case 32: goto st16;
		case 33: goto st17;
		case 34: goto st1;
		case 35: goto st18;
		case 37: goto st19;
		case 38: goto st20;
		case 39: goto st3;
		case 40: goto tr30;
		case 41: goto tr31;
		case 42: goto st21;
		case 43: goto st22;
		case 44: goto tr34;
		case 45: goto st23;
		case 46: goto tr36;
		case 47: goto tr37;
		case 48: goto tr38;
		case 58: goto tr40;
		case 59: goto tr41;
		case 60: goto st42;
		case 61: goto st44;
		case 62: goto st45;
		case 63: goto tr45;
		case 91: goto tr47;
		case 93: goto tr48;
		case 94: goto st48;
		case 95: goto st47;
		case 123: goto tr50;
		case 124: goto st49;
		case 125: goto tr52;
		case 126: goto tr53;
	}
	if ( (*p) < 49 ) {
		if ( 9 <= (*p) && (*p) <= 13 )
			goto st16;
	} else if ( (*p) > 57 ) {
		if ( (*p) > 90 ) {
			if ( 97 <= (*p) && (*p) <= 122 )
				goto st47;
		} else if ( (*p) >= 65 )
			goto st47;
	} else
		goto tr39;
	goto st0;
st0:
cs = 0;
	goto _out;
st16:
	if ( ++p == pe )
		goto _test_eof16;
case 16:
	switch( (*p) ) {
		case 9: goto st16;
		case 32: goto st16;
	}
	if ( 11 <= (*p) && (*p) <= 13 )
		goto st16;
	goto tr54;
st17:
	if ( ++p == pe )
		goto _test_eof17;
case 17:
	if ( (*p) == 61 )
		goto tr56;
	goto tr55;
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
st18:
	if ( ++p == pe )
		goto _test_eof18;
case 18:
	if ( (*p) == 35 )
		goto tr58;
	goto tr57;
st19:
	if ( ++p == pe )
		goto _test_eof19;
case 19:
	if ( (*p) == 61 )
		goto tr60;
	goto tr59;
st20:
	if ( ++p == pe )
		goto _test_eof20;
case 20:
	switch( (*p) ) {
		case 38: goto tr62;
		case 61: goto tr63;
	}
	goto tr61;
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
st21:
	if ( ++p == pe )
		goto _test_eof21;
case 21:
	if ( (*p) == 61 )
		goto tr65;
	goto tr64;
st22:
	if ( ++p == pe )
		goto _test_eof22;
case 22:
	switch( (*p) ) {
		case 43: goto tr67;
		case 61: goto tr68;
	}
	goto tr66;
st23:
	if ( ++p == pe )
		goto _test_eof23;
case 23:
	switch( (*p) ) {
		case 45: goto tr70;
		case 61: goto tr71;
		case 62: goto tr72;
	}
	goto tr69;
tr36:
#line 1 "NONE"
	{te = p+1;}
	goto st24;
st24:
	if ( ++p == pe )
		goto _test_eof24;
case 24:
#line 1131 "c_lexer_gen.c"
	if ( (*p) == 46 )
		goto st5;
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr75;
	goto tr73;
st5:
	if ( ++p == pe )
		goto _test_eof5;
case 5:
	if ( (*p) == 46 )
		goto tr8;
	goto tr7;
tr75:
#line 1 "NONE"
	{te = p+1;}
	goto st25;
st25:
	if ( ++p == pe )
		goto _test_eof25;
case 25:
#line 1150 "c_lexer_gen.c"
	switch( (*p) ) {
		case 69: goto st6;
		case 70: goto st27;
		case 76: goto st27;
		case 101: goto st6;
		case 102: goto st27;
		case 108: goto st27;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr75;
	goto tr76;
st6:
	if ( ++p == pe )
		goto _test_eof6;
case 6:
	switch( (*p) ) {
		case 43: goto st7;
		case 45: goto st7;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st26;
	goto tr9;
st7:
	if ( ++p == pe )
		goto _test_eof7;
case 7:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st26;
	goto tr9;
st26:
	if ( ++p == pe )
		goto _test_eof26;
case 26:
	switch( (*p) ) {
		case 70: goto st27;
		case 76: goto st27;
		case 102: goto st27;
		case 108: goto st27;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st26;
	goto tr76;
st27:
	if ( ++p == pe )
		goto _test_eof27;
case 27:
	switch( (*p) ) {
		case 70: goto st27;
		case 76: goto st27;
		case 102: goto st27;
		case 108: goto st27;
	}
	goto tr76;
tr37:
#line 1 "NONE"
	{te = p+1;}
	goto st28;
st28:
	if ( ++p == pe )
		goto _test_eof28;
case 28:
#line 1210 "c_lexer_gen.c"
	switch( (*p) ) {
		case 42: goto st8;
		case 47: goto st29;
		case 61: goto tr81;
	}
	goto tr79;
st8:
	if ( ++p == pe )
		goto _test_eof8;
case 8:
	if ( (*p) == 42 )
		goto st9;
	goto st8;
st9:
	if ( ++p == pe )
		goto _test_eof9;
case 9:
	switch( (*p) ) {
		case 42: goto st9;
		case 47: goto tr15;
	}
	goto st8;
st29:
	if ( ++p == pe )
		goto _test_eof29;
case 29:
	if ( (*p) == 10 )
		goto tr82;
	goto st29;
tr38:
#line 1 "NONE"
	{te = p+1;}
#line 680 "c_lexer.rl"
	{act = 10;}
	goto st30;
st30:
	if ( ++p == pe )
		goto _test_eof30;
case 30:
#line 1247 "c_lexer_gen.c"
	switch( (*p) ) {
		case 46: goto tr84;
		case 69: goto st12;
		case 76: goto st38;
		case 85: goto st38;
		case 88: goto st14;
		case 101: goto st12;
		case 108: goto st38;
		case 117: goto st38;
		case 120: goto st14;
	}
	if ( (*p) > 55 ) {
		if ( 56 <= (*p) && (*p) <= 57 )
			goto tr39;
	} else if ( (*p) >= 48 )
		goto tr85;
	goto tr83;
tr84:
#line 1 "NONE"
	{te = p+1;}
	goto st31;
st31:
	if ( ++p == pe )
		goto _test_eof31;
case 31:
#line 1271 "c_lexer_gen.c"
	switch( (*p) ) {
		case 69: goto st10;
		case 70: goto st33;
		case 76: goto st33;
		case 101: goto st10;
		case 102: goto st33;
		case 108: goto st33;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr84;
	goto tr89;
st10:
	if ( ++p == pe )
		goto _test_eof10;
case 10:
	switch( (*p) ) {
		case 43: goto st11;
		case 45: goto st11;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st32;
	goto tr16;
st11:
	if ( ++p == pe )
		goto _test_eof11;
case 11:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st32;
	goto tr16;
st32:
	if ( ++p == pe )
		goto _test_eof32;
case 32:
	switch( (*p) ) {
		case 70: goto st33;
		case 76: goto st33;
		case 102: goto st33;
		case 108: goto st33;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st32;
	goto tr89;
st33:
	if ( ++p == pe )
		goto _test_eof33;
case 33:
	switch( (*p) ) {
		case 70: goto st33;
		case 76: goto st33;
		case 102: goto st33;
		case 108: goto st33;
	}
	goto tr89;
tr85:
#line 1 "NONE"
	{te = p+1;}
#line 676 "c_lexer.rl"
	{act = 9;}
	goto st34;
st34:
	if ( ++p == pe )
		goto _test_eof34;
case 34:
#line 1332 "c_lexer_gen.c"
	switch( (*p) ) {
		case 46: goto tr84;
		case 69: goto st12;
		case 76: goto st39;
		case 85: goto st39;
		case 101: goto st12;
		case 108: goto st39;
		case 117: goto st39;
	}
	if ( (*p) > 55 ) {
		if ( 56 <= (*p) && (*p) <= 57 )
			goto tr39;
	} else if ( (*p) >= 48 )
		goto tr85;
	goto tr92;
tr39:
#line 1 "NONE"
	{te = p+1;}
#line 680 "c_lexer.rl"
	{act = 10;}
	goto st35;
st35:
	if ( ++p == pe )
		goto _test_eof35;
case 35:
#line 1355 "c_lexer_gen.c"
	switch( (*p) ) {
		case 46: goto tr84;
		case 69: goto st12;
		case 76: goto st38;
		case 85: goto st38;
		case 101: goto st12;
		case 108: goto st38;
		case 117: goto st38;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr39;
	goto tr83;
st12:
	if ( ++p == pe )
		goto _test_eof12;
case 12:
	switch( (*p) ) {
		case 43: goto st13;
		case 45: goto st13;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st36;
	goto tr19;
st13:
	if ( ++p == pe )
		goto _test_eof13;
case 13:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st36;
	goto tr19;
st36:
	if ( ++p == pe )
		goto _test_eof36;
case 36:
	switch( (*p) ) {
		case 70: goto st37;
		case 76: goto st37;
		case 102: goto st37;
		case 108: goto st37;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st36;
	goto tr94;
st37:
	if ( ++p == pe )
		goto _test_eof37;
case 37:
	switch( (*p) ) {
		case 70: goto st37;
		case 76: goto st37;
		case 102: goto st37;
		case 108: goto st37;
	}
	goto tr94;
st38:
	if ( ++p == pe )
		goto _test_eof38;
case 38:
	switch( (*p) ) {
		case 76: goto st38;
		case 85: goto st38;
		case 108: goto st38;
		case 117: goto st38;
	}
	goto tr83;
st39:
	if ( ++p == pe )
		goto _test_eof39;
case 39:
	switch( (*p) ) {
		case 76: goto st39;
		case 85: goto st39;
		case 108: goto st39;
		case 117: goto st39;
	}
	goto tr92;
st14:
	if ( ++p == pe )
		goto _test_eof14;
case 14:
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st40;
	} else if ( (*p) > 70 ) {
		if ( 97 <= (*p) && (*p) <= 102 )
			goto st40;
	} else
		goto st40;
	goto tr22;
st40:
	if ( ++p == pe )
		goto _test_eof40;
case 40:
	switch( (*p) ) {
		case 76: goto st41;
		case 85: goto st41;
		case 108: goto st41;
		case 117: goto st41;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st40;
	} else if ( (*p) > 70 ) {
		if ( 97 <= (*p) && (*p) <= 102 )
			goto st40;
	} else
		goto st40;
	goto tr96;
st41:
	if ( ++p == pe )
		goto _test_eof41;
case 41:
	switch( (*p) ) {
		case 76: goto st41;
		case 85: goto st41;
		case 108: goto st41;
		case 117: goto st41;
	}
	goto tr96;
st42:
	if ( ++p == pe )
		goto _test_eof42;
case 42:
	switch( (*p) ) {
		case 60: goto st43;
		case 61: goto tr100;
	}
	goto tr98;
st43:
	if ( ++p == pe )
		goto _test_eof43;
case 43:
	if ( (*p) == 61 )
		goto tr102;
	goto tr101;
st44:
	if ( ++p == pe )
		goto _test_eof44;
case 44:
	if ( (*p) == 61 )
		goto tr104;
	goto tr103;
st45:
	if ( ++p == pe )
		goto _test_eof45;
case 45:
	switch( (*p) ) {
		case 61: goto tr106;
		case 62: goto st46;
	}
	goto tr105;
st46:
	if ( ++p == pe )
		goto _test_eof46;
case 46:
	if ( (*p) == 61 )
		goto tr109;
	goto tr108;
st47:
	if ( ++p == pe )
		goto _test_eof47;
case 47:
	if ( (*p) == 95 )
		goto st47;
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st47;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st47;
	} else
		goto st47;
	goto tr110;
st48:
	if ( ++p == pe )
		goto _test_eof48;
case 48:
	if ( (*p) == 61 )
		goto tr112;
	goto tr111;
st49:
	if ( ++p == pe )
		goto _test_eof49;
case 49:
	switch( (*p) ) {
		case 61: goto tr114;
		case 124: goto tr115;
	}
	goto tr113;
	}
	_test_eof15: cs = 15; goto _test_eof; 
	_test_eof16: cs = 16; goto _test_eof; 
	_test_eof17: cs = 17; goto _test_eof; 
	_test_eof1: cs = 1; goto _test_eof; 
	_test_eof2: cs = 2; goto _test_eof; 
	_test_eof18: cs = 18; goto _test_eof; 
	_test_eof19: cs = 19; goto _test_eof; 
	_test_eof20: cs = 20; goto _test_eof; 
	_test_eof3: cs = 3; goto _test_eof; 
	_test_eof4: cs = 4; goto _test_eof; 
	_test_eof21: cs = 21; goto _test_eof; 
	_test_eof22: cs = 22; goto _test_eof; 
	_test_eof23: cs = 23; goto _test_eof; 
	_test_eof24: cs = 24; goto _test_eof; 
	_test_eof5: cs = 5; goto _test_eof; 
	_test_eof25: cs = 25; goto _test_eof; 
	_test_eof6: cs = 6; goto _test_eof; 
	_test_eof7: cs = 7; goto _test_eof; 
	_test_eof26: cs = 26; goto _test_eof; 
	_test_eof27: cs = 27; goto _test_eof; 
	_test_eof28: cs = 28; goto _test_eof; 
	_test_eof8: cs = 8; goto _test_eof; 
	_test_eof9: cs = 9; goto _test_eof; 
	_test_eof29: cs = 29; goto _test_eof; 
	_test_eof30: cs = 30; goto _test_eof; 
	_test_eof31: cs = 31; goto _test_eof; 
	_test_eof10: cs = 10; goto _test_eof; 
	_test_eof11: cs = 11; goto _test_eof; 
	_test_eof32: cs = 32; goto _test_eof; 
	_test_eof33: cs = 33; goto _test_eof; 
	_test_eof34: cs = 34; goto _test_eof; 
	_test_eof35: cs = 35; goto _test_eof; 
	_test_eof12: cs = 12; goto _test_eof; 
	_test_eof13: cs = 13; goto _test_eof; 
	_test_eof36: cs = 36; goto _test_eof; 
	_test_eof37: cs = 37; goto _test_eof; 
	_test_eof38: cs = 38; goto _test_eof; 
	_test_eof39: cs = 39; goto _test_eof; 
	_test_eof14: cs = 14; goto _test_eof; 
	_test_eof40: cs = 40; goto _test_eof; 
	_test_eof41: cs = 41; goto _test_eof; 
	_test_eof42: cs = 42; goto _test_eof; 
	_test_eof43: cs = 43; goto _test_eof; 
	_test_eof44: cs = 44; goto _test_eof; 
	_test_eof45: cs = 45; goto _test_eof; 
	_test_eof46: cs = 46; goto _test_eof; 
	_test_eof47: cs = 47; goto _test_eof; 
	_test_eof48: cs = 48; goto _test_eof; 
	_test_eof49: cs = 49; goto _test_eof; 

	_test_eof: {}
	if ( p == eof )
	{
	switch ( cs ) {
	case 16: goto tr54;
	case 17: goto tr55;
	case 18: goto tr57;
	case 19: goto tr59;
	case 20: goto tr61;
	case 21: goto tr64;
	case 22: goto tr66;
	case 23: goto tr69;
	case 24: goto tr73;
	case 5: goto tr7;
	case 25: goto tr76;
	case 6: goto tr9;
	case 7: goto tr9;
	case 26: goto tr76;
	case 27: goto tr76;
	case 28: goto tr79;
	case 8: goto tr12;
	case 9: goto tr12;
	case 29: goto tr82;
	case 30: goto tr83;
	case 31: goto tr89;
	case 10: goto tr16;
	case 11: goto tr16;
	case 32: goto tr89;
	case 33: goto tr89;
	case 34: goto tr92;
	case 35: goto tr83;
	case 12: goto tr19;
	case 13: goto tr19;
	case 36: goto tr94;
	case 37: goto tr94;
	case 38: goto tr83;
	case 39: goto tr92;
	case 14: goto tr22;
	case 40: goto tr96;
	case 41: goto tr96;
	case 42: goto tr98;
	case 43: goto tr101;
	case 44: goto tr103;
	case 45: goto tr105;
	case 46: goto tr108;
	case 47: goto tr110;
	case 48: goto tr111;
	case 49: goto tr113;
	}
	}

	_out: {}
	}

#line 822 "c_lexer.rl"

    lex_rp = p;
    lex_rcs = cs;
    lex_ract = act;
    lex_rts = ts;
    lex_rte = te;

    /* Update lex_pos for compatibility */
    lex_pos = (int)(p - lex_src);
}
