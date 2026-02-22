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

int fputs(char *s, int f);
int fputc(int c, int f);
void fput_uint(int f, int v);
void exit(int status);

int stderr;
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

%%{
    machine c_lexer;

    main := |*

        # --- Whitespace ---
        [ \t\r\f\v]+ => { /* skip */ };
        '\n' => { lex_line = lex_line + 1; };

        # --- Line comment ---
        '//' [^\n]* => { /* skip */ };

        # --- Block comment ---
        '/*' any* :>> '*/' => {
            lex_count_nl(ts, te);
        };

        # --- Float literals (before integers for longest-match priority) ---
        [0-9]+ '.' [0-9]* ([eE] [+\-]? [0-9]+)? [fFlL]* => {
            lex_parse_fnum(ts, te);
            fbreak;
        };
        '.' [0-9]+ ([eE] [+\-]? [0-9]+)? [fFlL]* => {
            lex_parse_fnum(ts, te);
            fbreak;
        };
        [0-9]+ [eE] [+\-]? [0-9]+ [fFlL]* => {
            lex_parse_fnum(ts, te);
            fbreak;
        };

        # --- Numeric literals ---
        '0' [xX] [0-9a-fA-F]+ [uUlL]* => {
            lex_parse_num(ts, te);
            fbreak;
        };
        '0' [0-7]+ [uUlL]* => {
            lex_parse_num(ts, te);
            fbreak;
        };
        [0-9]+ [uUlL]* => {
            lex_parse_num(ts, te);
            fbreak;
        };

        # --- String literal ---
        '"' ( [^"\\\n] | '\\' any )* '"' => {
            lex_parse_str(ts, te);
            fbreak;
        };

        # --- Char literal ---
        "'" ( [^'\\\n] | '\\' any )* "'" => {
            lex_parse_chr(ts, te);
            fbreak;
        };

        # --- Identifier / keyword ---
        [a-zA-Z_][a-zA-Z0-9_]* => {
            lex_parse_id(ts, te);
            fbreak;
        };

        # --- Multi-char operators (longest match) ---
        '...' => { lex_tok = TK_ELLIPSIS; fbreak; };
        '<<=' => { lex_tok = TK_LSHIFTEQ; fbreak; };
        '>>=' => { lex_tok = TK_RSHIFTEQ; fbreak; };
        '##'  => { lex_tok = TK_HASHHASH; fbreak; };
        '+='  => { lex_tok = TK_PLUSEQ; fbreak; };
        '-='  => { lex_tok = TK_MINUSEQ; fbreak; };
        '*='  => { lex_tok = TK_STAREQ; fbreak; };
        '/='  => { lex_tok = TK_SLASHEQ; fbreak; };
        '%='  => { lex_tok = TK_PERCENTEQ; fbreak; };
        '&='  => { lex_tok = TK_AMPEQ; fbreak; };
        '|='  => { lex_tok = TK_PIPEEQ; fbreak; };
        '^='  => { lex_tok = TK_CARETEQ; fbreak; };
        '=='  => { lex_tok = TK_EQ; fbreak; };
        '!='  => { lex_tok = TK_NE; fbreak; };
        '<='  => { lex_tok = TK_LE; fbreak; };
        '>='  => { lex_tok = TK_GE; fbreak; };
        '<<'  => { lex_tok = TK_LSHIFT; fbreak; };
        '>>'  => { lex_tok = TK_RSHIFT; fbreak; };
        '&&'  => { lex_tok = TK_LAND; fbreak; };
        '||'  => { lex_tok = TK_LOR; fbreak; };
        '++'  => { lex_tok = TK_INC; fbreak; };
        '--'  => { lex_tok = TK_DEC; fbreak; };
        '->'  => { lex_tok = TK_ARROW; fbreak; };

        # --- Single-char operators ---
        '(' => { lex_tok = TK_LPAREN; fbreak; };
        ')' => { lex_tok = TK_RPAREN; fbreak; };
        '[' => { lex_tok = TK_LBRACK; fbreak; };
        ']' => { lex_tok = TK_RBRACK; fbreak; };
        '{' => { lex_tok = TK_LBRACE; fbreak; };
        '}' => { lex_tok = TK_RBRACE; fbreak; };
        ';' => { lex_tok = TK_SEMI; fbreak; };
        ',' => { lex_tok = TK_COMMA; fbreak; };
        '.' => { lex_tok = TK_DOT; fbreak; };
        ':' => { lex_tok = TK_COLON; fbreak; };
        '?' => { lex_tok = TK_QMARK; fbreak; };
        '~' => { lex_tok = TK_TILDE; fbreak; };
        '#' => { lex_tok = TK_HASH; fbreak; };
        '+' => { lex_tok = TK_PLUS; fbreak; };
        '-' => { lex_tok = TK_MINUS; fbreak; };
        '*' => { lex_tok = TK_STAR; fbreak; };
        '/' => { lex_tok = TK_SLASH; fbreak; };
        '%' => { lex_tok = TK_PERCENT; fbreak; };
        '&' => { lex_tok = TK_AMP; fbreak; };
        '|' => { lex_tok = TK_PIPE; fbreak; };
        '^' => { lex_tok = TK_CARET; fbreak; };
        '!' => { lex_tok = TK_BANG; fbreak; };
        '=' => { lex_tok = TK_ASSIGN; fbreak; };
        '<' => { lex_tok = TK_LT; fbreak; };
        '>' => { lex_tok = TK_GT; fbreak; };

    *|;
}%%

/* === Ragel data tables === */

%% write data nofinal;

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

    %% write init;
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

    %% write exec;

    lex_rp = p;
    lex_rcs = cs;
    lex_ract = act;
    lex_rts = ts;
    lex_rte = te;

    /* Update lex_pos for compatibility */
    lex_pos = (int)(p - lex_src);
}
