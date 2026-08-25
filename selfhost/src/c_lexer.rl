/* c_lexer.rl -- Ragel -G2 C lexer for SLOW-32 s32-cc compiler (stage18)
 *
 * Build:   bash gen_lexer.sh
 * Output:  c_lexer_gen.c (post-processed for s32-cc compatibility)
 *
 * Interface matches s32cc_lex.h API used by s32cc_parse.h.
 */

/* === Libc prototypes ===
 * Dialect decls for the SLOW-32 self-host path (s12cc / cc-x64 / cc-a64
 * compiling this file have no system headers). Hosted builds (GCC/Clang
 * compiling the cross drivers) define S12CC_HOSTED in the driver prologue
 * and include the real headers instead — redeclaring strlen as
 * `int strlen(char *)` there is a GCC 14 hard error. See ISSUES #57. */
#ifndef S12CC_HOSTED
int strcmp(char *a, char *b);
int strncmp(char *a, char *b, int n);
int strlen(char *s);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);
void exit(int status);
#endif

int fdputs(char *s, int f);
int fdputc(int c, int f);
void fdputuint(int f, int v);

#ifndef NULL
#define NULL 0
#endif

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
static int  lex_val_hi;    /* high 32 bits for 64-bit integer literals */
static int  lex_val_ll;    /* 1 if the literal had an LL/ll suffix */
static int  lex_val_u;     /* 1 if the literal had a U/u suffix */
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
    long long val;     /* 64-bit accumulator — preserves literals > 2^32 */
    int ch;
    char *np;
    int n_l;
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
    /* Suffix: count L's (need 2 for llong) and any U for unsigned.  Order
     * is loose — `LLU`, `ULL`, `LUL`, `lLu` all accepted. */
    n_l = 0;
    lex_val_u = 0;
    while (np < te) {
        ch = *np & 255;
        if (ch == 'L' || ch == 'l') n_l = n_l + 1;
        else if (ch == 'U' || ch == 'u') lex_val_u = 1;
        else break;
        np = np + 1;
    }
    lex_tok = TK_NUM;
    lex_val = (int)val;
    lex_val_hi = (int)(val >> 32);
    lex_val_ll = (n_l >= 2) ? 1 : 0;
}

/* === Decimal -> IEEE 754 binary64 via native doubles ===
 * The compiler's own doubles are IEEE binary64 on every host that
 * builds it (the stage07-built bootstrap lowers them to the __fp64
 * pair libcalls -- HW FP behind one call -- and the cross compilers
 * use host hardware FP), so decimal->binary conversion is done in
 * double arithmetic instead of the old 24-bit integer approximation.
 * The significand is accumulated exactly in a long long (<= 18
 * digits) and converted with one rounding; the power-of-ten table is
 * BUILT by exact multiplication because an FP literal in this very
 * file would be parsed by the previous stage's lexer.  10^k is
 * exactly representable for k <= 22, so a literal with <= 15-16
 * significant digits and |exp10| <= 22 rounds exactly once --
 * bit-identical to a correctly-rounded strtod (clang's parse).
 * Larger |exp10| takes one rounding per 10^22 ladder step (a few ulp
 * near 1e+-300). */

static double lex_p10[23];
static double lex_p5[23];     /* 5^k, exact for k <= 22 */
static double lex_dd_split;   /* 2^27 + 1, Dekker splitter */
static double lex_tp_err;     /* twoProd error out-param */
static int lex_p10_ready;

static void lex_p10_build(void) {
    int i;
    double p;
    p = 1.0;
    i = 0;
    while (i < 23) {
        lex_p10[i] = p;
        p = p * 10.0;
        i = i + 1;
    }
    p = 1.0;
    i = 0;
    while (i < 23) {
        lex_p5[i] = p;
        p = p * 5.0;
        i = i + 1;
    }
    /* built arithmetically: 134217728.0 (= 2^27) parses exactly even
     * under the previous stage's 24-bit lexer; 134217729.0 would not */
    lex_dd_split = 134217728.0 + 1.0;
    lex_p10_ready = 1;
}

/* Dekker twoProd: lex_tp_x = fl(a*b), lex_tp_err = a*b - fl(a*b).
 * Operands and results travel through file-scope statics because the
 * STAGE07 bootstrap compiler cannot pass or return doubles by value
 * (the reason the old converter was integer-only).  Near the overflow
 * threshold the 2^27 split itself overflows; there the error term is
 * surrendered (plain rounding) rather than NaN -- the 5^e ladder never
 * reaches that band, so the guards are pure insurance. */
static double lex_tp_a;
static double lex_tp_b;
static double lex_tp_x;

static void lex_two_prod(void) {
    double t;
    double ah;
    double al;
    double bh;
    double bl;

    lex_tp_x = lex_tp_a * lex_tp_b;
    t = lex_tp_a * lex_dd_split;
    if (t * 0.5 == t && t != 0.0) { lex_tp_err = 0.0; return; }
    ah = t - (t - lex_tp_a);
    al = lex_tp_a - ah;
    t = lex_tp_b * lex_dd_split;
    if (t * 0.5 == t && t != 0.0) { lex_tp_err = 0.0; return; }
    bh = t - (t - lex_tp_b);
    bl = lex_tp_b - bh;
    lex_tp_err = ((ah * bh - lex_tp_x) + ah * bl + al * bh) + al * bl;
}

/* === Helper: parse float literal from ts..te === */

static void lex_parse_fnum(char *ts, char *te) {
    long long sig;
    double d;
    int ndig;
    int exp10;
    int neg;
    int is_float;
    int ch;
    char *np;
    int saw_dot;
    int frac_digits;
    int eneg;
    int eval;
    int e;
    int *pw;

    sig = 0;
    ndig = 0;
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
            if (ndig < 18 && (sig != 0 || ch != 48)) {
                /* significant digit (leading zeros only scale) */
                sig = sig * 10 + (ch - 48);
                ndig = ndig + 1;
                if (saw_dot) frac_digits = frac_digits + 1;
            } else if (sig == 0) {
                /* leading zero: contributes only to the scale */
                if (saw_dot) frac_digits = frac_digits + 1;
            } else {
                /* excess significant digits: drop, keep the scale */
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
                if (eval < 100000000) eval = eval * 10 + (ch - 48);
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

    /* Convert: d = sig * 10^exp10 in double arithmetic.  Clamp the
     * exponent well past the representable range so absurd literals
     * saturate to inf/0 without an absurd loop.
     *
     * Fast path: sig <= 2^53 (exact in double) and |exp10| <= 22
     * (10^e exact) is ONE rounding -- bit-identical to a correctly
     * rounded strtod.  Otherwise the scale 10^|exp10| is accumulated
     * as a double-double (sh + sl) with twoProd-compensated ladder
     * steps and sig's conversion residue rides along, so the result
     * stays correct to the last bit except within ~2^-50 ulp of a
     * rounding halfway point, in the split-overflow band above
     * ~6.7e300, and deep in the denormal range. */
    if (!lex_p10_ready) lex_p10_build();
    if (exp10 > 400) exp10 = 400;
    if (exp10 < -400) exp10 = -400;
    d = (double)sig;
    {
        double sigerr;
        double b;
        double sh;
        double sl;
        double ph;
        double pe;
        double q;
        double resid;
        sigerr = (double)(sig - (long long)d);
        if (exp10 == 0) {
            /* d is the single-rounded conversion of sig: done */
        } else if (exp10 > 0 && exp10 <= 22 && sigerr == 0.0) {
            d = d * lex_p10[exp10];
        } else if (exp10 < 0 && exp10 >= -22 && sigerr == 0.0) {
            d = d / lex_p10[0 - exp10];
        } else {
            /* 10^e = 5^e * 2^e.  5^|e| stays in the normal range for
             * |e| <= 400 (5^400 ~ 1.5e280), so the compensated ladder
             * never saturates and the Dekker splits never overflow;
             * the final scale by 2^e is EXACT, so the one rounding
             * that matters already happened in the combine below.
             * (Results that land in the denormal range round a second
             * time there -- the one remaining corner.) */
            sh = 1.0;
            sl = 0.0;
            e = exp10;
            if (e < 0) e = 0 - e;
            while (e > 0) {
                if (e > 22) { b = lex_p5[22]; e = e - 22; }
                else        { b = lex_p5[e];  e = 0; }
                lex_tp_a = sh;
                lex_tp_b = b;
                lex_two_prod();
                ph = lex_tp_x;
                pe = lex_tp_err + sl * b;
                sh = ph + pe;
                sl = pe - (sh - ph);
            }
            if (exp10 > 0) {
                /* v = (d + sigerr) * (sh + sl) */
                lex_tp_a = d;
                lex_tp_b = sh;
                lex_two_prod();
                ph = lex_tp_x;
                d = ph + (lex_tp_err + (d * sl + sigerr * sh));
            } else {
                /* v = (d + sigerr) / (sh + sl): one Newton correction
                 * of the plain quotient using the exact residual */
                q = d / sh;
                lex_tp_a = q;
                lex_tp_b = sh;
                lex_two_prod();
                ph = lex_tp_x;
                resid = ((d - ph) - lex_tp_err) + (sigerr - q * sl);
                d = q + resid / sh;
            }
            /* exact scale by 2^exp10, assembled from exponent bits
             * (inlined: stage07 cannot return a double) */
            {
                double p2;
                int *p2w;
                p2w = (int *)&p2;
                p2w[0] = 0;
                p2w[1] = (exp10 + 1023) << 20;
                d = d * p2;
            }
        }
    }

    /* Emit IEEE bits (sign applied by bit flip so -0.0 works) */
    if (is_float) {
        float fv;
        fv = (float)d;
        pw = (int *)&fv;
        lex_val = pw[0];
        if (neg) lex_val = lex_val ^ (1 << 31);
        lex_fval_hi = 0;
        lex_fty = 5;  /* TY_FLOAT */
    } else {
        pw = (int *)&d;
        lex_val = pw[0];
        lex_fval_hi = pw[1];
        if (neg) lex_fval_hi = lex_fval_hi ^ (1 << 31);
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
