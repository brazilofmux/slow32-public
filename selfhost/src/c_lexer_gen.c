
#line 1 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
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
/* The pointers as the library has them (void *, const): the compiler's
 * own memcpy(&d, w, 8) and strdup(name) were 6.5.16.1 violations --
 * pointer types that do not convert, and an implicit int malloc
 * assigned to a pointer -- which S5's checks find in its own source. */
int strcmp(const char *a, const char *b);
int strncmp(const char *a, const char *b, int n);
int strlen(const char *s);
void *memcpy(void *dst, const void *src, int n);
void *memset(void *dst, int c, int n);
void *malloc(unsigned long n);
void *calloc(unsigned long n, unsigned long size);
void *realloc(void *p, unsigned long n);
void free(void *p);
char *strdup(const char *s);
char *getenv(const char *name);
int atoi(const char *nptr);
char *strchr(const char *s, int c);
char *strcpy(char *dst, const char *src);
char *strncpy(char *dst, const char *src, int n);
int write(int fd, const void *buf, int n);
int rename(const char *from, const char *to);
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

#define LEX_SRC_SZ   12000000   /* was 1100000 */
#define LEX_STR_SZ   256
#define LEX_POOL_SZ  4194304   /* was 524288 */
#define LEX_POOL_MAX 65536   /* was 16384 */

static char lex_src[LEX_SRC_SZ];
static int  lex_len;
static int  lex_line;
static int  lex_col;
static int  lex_tok;
static int  lex_val;
static int  lex_val_hi;    /* high 32 bits for 64-bit integer literals */
static int  lex_val_ll;    /* 1 if the literal had an LL/ll suffix */
static int  lex_val_l;     /* 1 if the literal had a single L suffix: long, not int */
static int  lex_val_u;     /* 1 if the literal had a U/u suffix */
static int  lex_val_hexoct; /* 1 if the literal was hex or octal (C11 6.4.4.1) */
static int  lex_val_macro;  /* 1 if TK_NUM was injected from a #define / __LINE__ */
static int  lex_fval_hi;   /* high 32 bits for double literals */
static int  lex_fty;       /* TY_FLOAT or TY_DOUBLE for float literals */
static char lex_str[LEX_STR_SZ];
static int  lex_slen;
static int  lex_bol;       /* 1: no token yet on this line -- a # here begins a directive (6.10p2) */
static int  lex_painted;
static char *lex_err;      /* a malformed token's message, for next() to report where it is */
static char *lex_warn;     /* ... and a warning's, once per token: lex_warn_ts is the last one warned */
static char *lex_warn_ts;   /* 1: the identifier was painted (pp_rescan_region), never to be replaced */

/* For lex_pos compatibility (parser uses it for #include save/restore) */
static int  lex_pos;

/* String pool */
static char lex_strpool[LEX_POOL_SZ];
static int  lex_strpool_len;
static int  lex_str_off[LEX_POOL_MAX];
static int  lex_str_len[LEX_POOL_MAX];
/* bytes per element: 1 for "..." and u8"...", 2 for u"...", 4 for L"..."
 * and U"..." (6.4.5).  A wide string's code units are pooled little-
 * endian, its length width*n + width-1 so the emitter's closing 0 byte
 * completes the terminating unit -- and sizeof, length + 1, is right. */
static int  lex_str_width[LEX_POOL_MAX];
static int  lex_str_pfx[LEX_POOL_MAX];    /* its prefix: 0, 'L', 'u', 'U', or 8 for u8 */
static int  lex_chr_prefix;    /* L, u or U before a character constant, 0 for none */
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

static void p_error(char *msg);   /* parser.h; the lexer is included first */

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
    if (ch == 97)  { *posout = pos; return 7; }
    if (ch == 98)  { *posout = pos; return 8; }
    if (ch == 102) { *posout = pos; return 12; }
    if (ch == 118) { *posout = pos; return 11; }
    if (ch == 92)  { *posout = pos; return 92; }
    if (ch == 39)  { *posout = pos; return 39; }
    if (ch == 34)  { *posout = pos; return 34; }
    if (ch == 120) {
        /* C90 6.1.3.4: a hex escape takes every hex digit that follows,
         * and a value a char cannot hold is a constraint violation.  So
         * "\x0041" is "A", and "\xC3\xA4b" is an error (the b is a digit),
         * not the 0xC3 0xA4 'b' a two-digit reading would make of it; clang
         * and gcc reject it, and a compiler that accepted it would let a
         * program through that no other compiler builds. */
        val = 0; i = 0;
        while (1) {
            ch = s[pos] & 255;
            if (ch >= 48 && ch <= 57) { val = val * 16 + (ch - 48); }
            else if (ch >= 97 && ch <= 102) { val = val * 16 + (ch - 87); }
            else if (ch >= 65 && ch <= 70) { val = val * 16 + (ch - 55); }
            else break;
            if (val > 255) val = 256;   /* saturate; keep consuming */
            pos = pos + 1; i = i + 1;
        }
        if (i == 0) p_error("\\x used with no following hex digits");
        if (val > 255) p_error("hex escape sequence out of range");
        *posout = pos; return val;
    }
    /* An octal escape is one to three octal digits, 0 among them
     * (6.4.4.4): "\001" is 0x01.  \0 had a branch of its own that
     * stopped after the 0 unless a 1-7 followed, and then took three
     * more -- "\001" was NUL '0' '1', "\0123" four digits. */
    if (ch >= 48 && ch <= 55) {
        val = ch - 48; i = 0;
        while (i < 2) {
            ch = s[pos] & 255;
            if (ch < 48 || ch > 55) break;
            val = val * 8 + (ch - 48);
            pos = pos + 1; i = i + 1;
        }
        if (val > 255) p_error("octal escape sequence out of range");
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

/* Has val * base + d left 64 bits? */
static int lex_num_overflows(unsigned long long val, int base, int d) {
    return val > (18446744073709551615ULL - (unsigned long long)d) / (unsigned long long)base;
}

static void lex_parse_num(char *ts, char *te) {
    unsigned long long val;  /* 64-bit accumulator; unsigned since 64-bit masks overflow signed (UB) */
    int ch;
    int d;
    int big;
    char *np;
    int n_l;
    val = 0;
    big = 0;
    lex_val_hexoct = 0;
    lex_val_macro = 0;
    np = ts;
    ch = *np & 255;
    if (ch == 48) {
        np = np + 1;
        if (np < te) {
            ch = *np & 255;
            if (ch == 120 || ch == 88) {
                lex_val_hexoct = 1;
                np = np + 1;
                while (np < te) {
                    ch = *np & 255;
                    if (ch >= 48 && ch <= 57) d = ch - 48;
                    else if (ch >= 97 && ch <= 102) d = ch - 87;
                    else if (ch >= 65 && ch <= 70) d = ch - 55;
                    else break;
                    if (lex_num_overflows(val, 16, d)) big = 1;
                    val = val * 16 + d;
                    np = np + 1;
                }
                /* 0x1e+1 is one pp-number, not 0x1e + 1 (6.4.8) */
                if ((te[-1] == 101 || te[-1] == 69) && (*te == 43 || *te == 45))
                    lex_err = "invalid suffix on integer constant";
            } else if (ch >= 48 && ch <= 55) {
                lex_val_hexoct = 1;
                while (np < te) {
                    ch = *np & 255;
                    if (ch < 48 || ch > 55) break;
                    if (lex_num_overflows(val, 8, ch - 48)) big = 1;
                    val = val * 8 + (ch - 48);
                    np = np + 1;
                }
            }
        }
    } else {
        while (np < te) {
            ch = *np & 255;
            if (ch < 48 || ch > 57) break;
            if (lex_num_overflows(val, 10, ch - 48)) big = 1;
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
    /* digits the constant's base has none of: 09, 08 (6.4.4.1) */
    if (np < te) lex_err = "invalid digit in integer constant";
    if (big) lex_err = "integer literal is too large to be represented in any integer type";
    lex_tok = TK_NUM;
    lex_val = (int)val;
    lex_val_hi = (int)(val >> 32);
    lex_val_ll = (n_l >= 2) ? 1 : 0;
    lex_val_l = (n_l == 1) ? 1 : 0;
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

/* A hexadecimal floating constant, 0x1.8p1 (6.4.4.2): hex digits and a
 * binary exponent, the value exact -- the mantissa taken to 53 bits (a
 * double's, so it converts without rounding; further digits are
 * dropped), then scaled by powers of two built from exponent bits,
 * each product exact.  It was the constant 0x1 and the identifier p4. */
static void lex_parse_hexfloat(char *ts, char *te) {
    unsigned long long m;
    double d;
    double p2;
    int *p2w;
    int *pw;
    int e;
    int frac;
    int ch;
    int v;
    int pe;
    int psign;
    int step;
    int is_float;
    char *p;
    m = 0;
    e = 0;
    frac = 0;
    p = ts + 2;
    while (p < te) {
        ch = *p & 255;
        if (ch == 46) { frac = 1; p = p + 1; continue; }
        if (ch >= 48 && ch <= 57) v = ch - 48;
        else if (ch >= 97 && ch <= 102) v = ch - 87;
        else if (ch >= 65 && ch <= 70) v = ch - 55;
        else break;
        if ((m >> 49) == 0) {
            m = m * 16 + (unsigned long long)v;
            if (frac) e = e - 4;
        } else if (!frac) {
            e = e + 4;
        }
        p = p + 1;
    }
    p = p + 1;                              /* the p or P */
    psign = 1;
    if (*p == 43) p = p + 1;
    else if (*p == 45) { psign = -1; p = p + 1; }
    pe = 0;
    while (p < te && *p >= 48 && *p <= 57) {
        if (pe < 100000) pe = pe * 10 + (*p - 48);
        p = p + 1;
    }
    e = e + psign * pe;
    is_float = p < te && (*p == 102 || *p == 70);
    d = (double)(int)(m >> 26) * 67108864.0 + (double)(int)(m & 67108863);
    while (e > 0 && d != 0.0) {
        step = e > 60 ? 60 : e;
        p2w = (int *)&p2;
        p2w[0] = 0;
        p2w[1] = (step + 1023) << 20;
        d = d * p2;
        e = e - step;
    }
    while (e < 0 && d != 0.0) {
        step = 0 - e > 60 ? 60 : 0 - e;
        p2w = (int *)&p2;
        p2w[0] = 0;
        p2w[1] = (1023 - step) << 20;
        d = d * p2;
        e = e + step;
    }
    if (is_float) {
        float fv;
        fv = (float)d;
        pw = (int *)&fv;
        lex_val = pw[0];
        lex_fval_hi = 0;
        lex_fty = 5;  /* TY_FLOAT */
    } else {
        pw = (int *)&d;
        lex_val = pw[0];
        lex_fval_hi = pw[1];
        lex_fty = 6;  /* TY_DOUBLE */
    }
    lex_tok = TK_FNUM;
}

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

/* Identical string literals share one pool entry (one .rodata copy, one
 * address), as clang and gcc do.  Returns the existing index, or -1.
 * Only for literals that were not truncated at LEX_STR_SZ, so equal
 * recorded lengths mean equal literals. */
static int lex_str_intern(int pool_start, int slen) {
    int k;
    int j;
    int a;
    int b;
    if (slen >= LEX_STR_SZ - 1) return -1;
    k = 0;
    while (k < lex_str_count) {
        if (lex_str_len[k] == slen) {
            a = lex_str_off[k];
            b = pool_start;
            j = 0;
            while (j < slen && lex_strpool[a + j] == lex_strpool[b + j]) j = j + 1;
            if (j == slen) return k;
        }
        k = k + 1;
    }
    return -1;
}

/* A source character from UTF-8, for a wide literal: the code point,
 * *posout advanced past it.  A malformed byte is taken as itself. */
static int lex_utf8_decode(char *s, int *posout) {
    int pos;
    int c;
    int n;
    int cp;
    pos = *posout;
    c = s[pos] & 255;
    pos = pos + 1;
    n = 0;
    cp = c;
    if (c >= 240 && c < 248) { n = 3; cp = c & 7; }
    else if (c >= 224 && c < 240) { n = 2; cp = c & 15; }
    else if (c >= 192 && c < 224) { n = 1; cp = c & 31; }
    while (n > 0 && (s[pos] & 192) == 128) {
        cp = (cp << 6) | (s[pos] & 63);
        pos = pos + 1;
        n = n - 1;
    }
    *posout = pos;
    return cp;
}

/* The hex digits of a universal character name (6.4.3), ndig of them
 * (4 after \u, 8 after \U): the code point.  Fewer is an error. */
static int lex_parse_ucn(char *s, int *posout, int ndig) {
    int pos;
    int i;
    int ch;
    int v;
    pos = *posout;
    v = 0;
    i = 0;
    while (i < ndig) {
        ch = s[pos] & 255;
        if (ch >= 48 && ch <= 57) v = v * 16 + (ch - 48);
        else if (ch >= 97 && ch <= 102) v = v * 16 + (ch - 87);
        else if (ch >= 65 && ch <= 70) v = v * 16 + (ch - 55);
        else { lex_err = "incomplete universal character name"; break; }
        pos = pos + 1;
        i = i + 1;
    }
    *posout = pos;
    return v;
}

/* An escape in a wide literal, the cursor after the backslash: a hex
 * escape takes the digits a code unit holds, a universal character
 * name its code point; the rest as in a narrow literal. */
static int lex_parse_esc_wide(char *s, int *posout) {
    int pos;
    int ch;
    int v;
    pos = *posout;
    ch = s[pos] & 255;
    if (ch == 117 || ch == 85) {
        pos = pos + 1;
        v = lex_parse_ucn(s, &pos, ch == 117 ? 4 : 8);
        *posout = pos;
        return v;
    }
    if (ch == 120) {
        pos = pos + 1;
        v = 0;
        while (1) {
            ch = s[pos] & 255;
            if (ch >= 48 && ch <= 57) v = v * 16 + (ch - 48);
            else if (ch >= 97 && ch <= 102) v = v * 16 + (ch - 87);
            else if (ch >= 65 && ch <= 70) v = v * 16 + (ch - 55);
            else break;
            pos = pos + 1;
        }
        *posout = pos;
        return v;
    }
    return lex_parse_esc(s, posout);
}

/* Code point cp onto the pool as a narrow string has it, UTF-8
 * (5.2.1.2) -- for a universal character name, which was the letter u
 * and its digits.  Returns the bytes written. */
static int lex_pool_utf8(int cp) {
    int n;
    if (lex_strpool_len + 4 >= LEX_POOL_SZ) return 0;
    if (cp < 128) {
        lex_strpool[lex_strpool_len] = cp;
        n = 1;
    } else if (cp < 2048) {
        lex_strpool[lex_strpool_len] = 192 | (cp >> 6);
        lex_strpool[lex_strpool_len + 1] = 128 | (cp & 63);
        n = 2;
    } else if (cp < 65536) {
        lex_strpool[lex_strpool_len] = 224 | (cp >> 12);
        lex_strpool[lex_strpool_len + 1] = 128 | ((cp >> 6) & 63);
        lex_strpool[lex_strpool_len + 2] = 128 | (cp & 63);
        n = 3;
    } else {
        lex_strpool[lex_strpool_len] = 240 | (cp >> 18);
        lex_strpool[lex_strpool_len + 1] = 128 | ((cp >> 12) & 63);
        lex_strpool[lex_strpool_len + 2] = 128 | ((cp >> 6) & 63);
        lex_strpool[lex_strpool_len + 3] = 128 | (cp & 63);
        n = 4;
    }
    lex_strpool_len = lex_strpool_len + n;
    return n;
}

/* Code unit u onto the pool, width bytes little-endian. */
static void lex_pool_unit(int u, int width) {
    int i;
    i = 0;
    while (i < width && lex_strpool_len < LEX_POOL_SZ - 1) {
        lex_strpool[lex_strpool_len] = u & 255;
        lex_strpool_len = lex_strpool_len + 1;
        u = u >> 8;
        i = i + 1;
    }
}

/* A wide string literal, L"" u"" U"" (6.4.5p5): its code units from
 * UTF-8 source and escapes, u's beyond U+FFFF as surrogate pairs.  Not
 * interned: a narrow string of the same bytes is another array. */
static void lex_parse_wide_str(char *ts, char *te, int width) {
    int pool_start;
    int pos;
    int end;
    int cp;
    int units;
    pool_start = lex_strpool_len;
    pos = 0;
    while (ts[pos] != 34) pos = pos + 1;
    pos = pos + 1;
    end = (int)(te - ts) - 1;
    units = 0;
    while (pos < end) {
        if ((ts[pos] & 255) == 92) {
            pos = pos + 1;
            cp = lex_parse_esc_wide(ts, &pos);
        } else {
            cp = lex_utf8_decode(ts, &pos);
        }
        if (width == 2 && cp > 65535) {
            cp = cp - 65536;
            lex_pool_unit(55296 + (cp >> 10), 2);
            lex_pool_unit(56320 + (cp & 1023), 2);
            units = units + 2;
        } else {
            lex_pool_unit(cp, width);
            units = units + 1;
        }
    }
    lex_pool_unit(0, width - 1);    /* the terminator, less the byte the emitter adds */
    lex_strpool[lex_strpool_len] = 0;
    lex_strpool_len = lex_strpool_len + 1;
    lex_str[0] = 0;
    lex_slen = 0;
    if (lex_str_count < LEX_POOL_MAX) {
        lex_str_off[lex_str_count] = pool_start;
        lex_str_len[lex_str_count] = units * width + width - 1;
        lex_str_width[lex_str_count] = width;
        lex_str_pfx[lex_str_count] = ts[0];
        lex_val = lex_str_count;
        lex_str_count = lex_str_count + 1;
    }
    lex_tok = TK_STRING;
}

static void lex_parse_str(char *ts, char *te) {
    int ucn_n;
    int ucn_k;
    int pool_start;
    int slen;
    int ch;
    int pos;
    int end;
    int dup;
    if (ts[0] == 76 || ts[0] == 85) { lex_parse_wide_str(ts, te, 4); return; }   /* L"" U"" */
    if (ts[0] == 117 && ts[1] != 56) { lex_parse_wide_str(ts, te, 2); return; }  /* u"" */
    pool_start = lex_strpool_len;
    slen = 0;
    pos = 0;
    while (ts[pos] != 34) pos = pos + 1;   /* past a u8 prefix */
    pos = pos + 1;  /* skip opening quote */
    end = (int)(te - ts) - 1;  /* before closing quote */
    while (pos < end) {
        ch = ts[pos] & 255;
        if (ch == 92 && (ts[pos + 1] == 117 || ts[pos + 1] == 85)) {
            /* a universal character name: its code point in UTF-8 */
            pos = pos + 2;
            ch = lex_parse_ucn(ts, &pos, ts[pos - 1] == 117 ? 4 : 8);
            ucn_n = lex_pool_utf8(ch);
            ucn_k = 0;
            while (ucn_k < ucn_n && slen < LEX_STR_SZ - 1) {
                lex_str[slen] = lex_strpool[lex_strpool_len - ucn_n + ucn_k];
                slen = slen + 1;
                ucn_k = ucn_k + 1;
            }
            continue;
        }
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
    dup = ts[0] == 117 ? -1 : lex_str_intern(pool_start, slen);   /* a u8 one keeps its own entry, and its prefix */
    if (dup >= 0) {
        lex_strpool_len = pool_start;   /* give the bytes back */
        lex_val = dup;
        lex_tok = TK_STRING;
        return;
    }
    if (lex_str_count < LEX_POOL_MAX) {
        lex_str_off[lex_str_count] = pool_start;
        lex_str_len[lex_str_count] = slen;
        lex_str_width[lex_str_count] = 1;
        lex_str_pfx[lex_str_count] = ts[0] == 117 ? 8 : 0;
        lex_val = lex_str_count;
        lex_str_count = lex_str_count + 1;
    }
    lex_tok = TK_STRING;
}

/* === Helper: parse char literal from ts..te (includes quotes) === */

static void lex_parse_chr(char *ts, char *te) {
    int ch;
    int pos;
    int end;
    int n;
    int v;
    lex_chr_prefix = 0;
    pos = 1;  /* skip opening quote */
    if (ts[0] != 39) {
        /* L'x', u'x', U'x' (6.4.4.4p2): the code point, unconverted,
         * of the type wchar_t, char16_t or char32_t is (the parser types
         * it).  They were the identifier L and a constant. */
        lex_chr_prefix = ts[0];
        pos = 2;
        if (te - ts == 3) lex_err = "empty character constant";
        if ((ts[pos] & 255) == 92) {
            pos = pos + 1;
            ch = lex_parse_esc_wide(ts, &pos);
        } else {
            ch = lex_utf8_decode(ts, &pos);
        }
        if (lex_chr_prefix == 117) ch = ch & 65535;
        lex_tok = TK_CHARLIT;
        lex_val = ch;
        return;
    }
    if (te - ts == 2) lex_err = "empty character constant";   /* '' (6.4.4.4p1) */
    end = (int)(te - ts) - 1;
    n = 0;
    v = 0;
    while (pos < end) {
        ch = ts[pos] & 255;
        if (ch == 92) {
            pos = pos + 1;
            ch = lex_parse_esc(ts, &pos);
        } else {
            pos = pos + 1;
        }
        v = (v << 8) | (ch & 255);
        n = n + 1;
    }
    /* C90 6.1.3.4: the constant's value is the byte read as a char, and
     * char is signed here (ldb), as under clang: '\xff' is -1, not 255.
     * More than one character is implementation-defined (6.4.4.4p10):
     * clang's int, the first in the high byte -- 'ab' is 0x6162.  Only
     * the first was taken. */
    if (n == 1 && v > 127) v = v - 256;
    /* implementation-defined, and clang warns (-Wmultichar); a token
     * read again after a parser's rewind is not warned twice */
    if (n > 1 && ts != lex_warn_ts) {
        lex_warn = "multi-character character constant";
        lex_warn_ts = ts;
    }
    lex_tok = TK_CHARLIT;
    lex_val = v;
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
    lex_painted = 0;
    kw = lex_kw_lookup(lex_str);
    if (kw != 0) lex_tok = kw;
    else lex_tok = TK_IDENT;
}

/* ================================================================
 * Ragel machine definition
 * ================================================================ */


#line 1286 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"


/* === Ragel data tables === */


#line 1160 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
static const int c_lexer_start = 21;
static const int c_lexer_error = 0;

static const int c_lexer_en_main = 21;


#line 1291 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"

/* === lex_init === */

/* Universal character names in identifiers (6.4.2.1, 6.4.3): outside
 * literals, \u hhhh and \U hhhhhhhh are rewritten in place as _u hhhh
 * and _U hhhhhhhh -- the same length, one name however it is spelled,
 * and a label the assembler takes.  Done to the text, before the
 * scanner: a rule for them made the scanner more than stage07 can
 * compile.  lex_src[from..to). */
static void lex_ucn_idents(int from, int to) {
    int v;
    int k;
    int q;
    int n;
    int i;
    int ok;
    k = from;
    while (k < to) {
        q = lex_src[k];
        if (q == 34 || q == 39) {
            k = k + 1;
            while (k < to && lex_src[k] != q && lex_src[k] != 10) {
                if (lex_src[k] == 92) k = k + 1;
                k = k + 1;
            }
            k = k + 1;
            continue;
        }
        if (q == 47 && lex_src[k + 1] == 42) {
            k = k + 2;
            while (k + 1 < to && !(lex_src[k] == 42 && lex_src[k + 1] == 47)) k = k + 1;
            k = k + 2;
            continue;
        }
        if (q == 47 && lex_src[k + 1] == 47) {
            while (k < to && lex_src[k] != 10) k = k + 1;
            continue;
        }
        if (q == 92 && (lex_src[k + 1] == 117 || lex_src[k + 1] == 85)) {
            n = lex_src[k + 1] == 117 ? 4 : 8;
            ok = 1;
            v = 0;
            i = 0;
            while (i < n) {
                q = lex_src[k + 2 + i];
                if (q >= 48 && q <= 57) v = v * 16 + (q - 48);
                else if (q >= 97 && q <= 102) v = v * 16 + (q - 87);
                else if (q >= 65 && q <= 70) v = v * 16 + (q - 55);
                else ok = 0;
                i = i + 1;
            }
            /* not a basic character, nor a surrogate (6.4.3p2): left as
             * written, the backslash is refused */
            if ((v < 160 && v != 36 && v != 64 && v != 96) || (v >= 55296 && v <= 57343)) ok = 0;
            if (ok) lex_src[k] = 95;
            k = k + 2;
            continue;
        }
        k = k + 1;
    }
}

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
    lex_ucn_idents(0, lex_len);
    lex_line = 1;
    lex_bol = 1;
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

    
#line 1260 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	{
	cs = c_lexer_start;
	ts = 0;
	te = 0;
	act = 0;
	}

#line 1383 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
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

    
#line 1299 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	{
	if ( p == pe )
		goto _test_eof;
	switch ( cs )
	{
tr2:
#line 1 "NONE"
	{	switch( act ) {
	case 0:
	{{goto st0;}}
	break;
	case 9:
	{{p = ((te))-1;}
            lex_parse_num(ts, te);
            {p++; cs = 21; goto _out;}
        }
	break;
	case 10:
	{{p = ((te))-1;}
            lex_parse_num(ts, te);
            {p++; cs = 21; goto _out;}
        }
	break;
	case 11:
	{{p = ((te))-1;}
            lex_parse_num(ts, te);
            {p++; cs = 21; goto _out;}
        }
	break;
	case 15:
	{{p = ((te))-1;}
            lex_parse_id(ts, te);
            {p++; cs = 21; goto _out;}
        }
	break;
	}
	}
	goto st21;
tr4:
#line 1202 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{
            lex_parse_str(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr6:
#line 1233 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{{p = ((te))-1;}{ lex_tok = TK_HASH; {p++; cs = 21; goto _out;} }}
	goto st21;
tr7:
#line 1232 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_HASHHASH; {p++; cs = 21; goto _out;} }}
	goto st21;
tr9:
#line 1208 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{
            lex_parse_chr(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr11:
#line 1267 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{{p = ((te))-1;}{ lex_tok = TK_DOT; {p++; cs = 21; goto _out;} }}
	goto st21;
tr12:
#line 1227 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_ELLIPSIS; {p++; cs = 21; goto _out;} }}
	goto st21;
tr13:
#line 1172 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{{p = ((te))-1;}{
            lex_parse_fnum(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr16:
#line 1275 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{{p = ((te))-1;}{ lex_tok = TK_SLASH; {p++; cs = 21; goto _out;} }}
	goto st21;
tr19:
#line 1163 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{
            lex_count_nl(ts, te);
        }}
	goto st21;
tr20:
#line 1168 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{{p = ((te))-1;}{
            lex_parse_fnum(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr25:
#line 1196 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{{p = ((te))-1;}{
            lex_parse_num(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr34:
#line 1157 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_line = lex_line + 1; lex_bol = 1; }}
	goto st21;
tr39:
#line 1259 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LPAREN; {p++; cs = 21; goto _out;} }}
	goto st21;
tr40:
#line 1260 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RPAREN; {p++; cs = 21; goto _out;} }}
	goto st21;
tr43:
#line 1266 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_COMMA; {p++; cs = 21; goto _out;} }}
	goto st21;
tr50:
#line 1265 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_SEMI; {p++; cs = 21; goto _out;} }}
	goto st21;
tr54:
#line 1269 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_QMARK; {p++; cs = 21; goto _out;} }}
	goto st21;
tr57:
#line 1261 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LBRACK; {p++; cs = 21; goto _out;} }}
	goto st21;
tr58:
#line 1262 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RBRACK; {p++; cs = 21; goto _out;} }}
	goto st21;
tr61:
#line 1263 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LBRACE; {p++; cs = 21; goto _out;} }}
	goto st21;
tr63:
#line 1264 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RBRACE; {p++; cs = 21; goto _out;} }}
	goto st21;
tr64:
#line 1270 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_TILDE; {p++; cs = 21; goto _out;} }}
	goto st21;
tr65:
#line 1214 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{
            lex_parse_id(ts + 1, te);
            lex_painted = 1;
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr66:
#line 1156 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ /* skip */ }}
	goto st21;
tr67:
#line 1280 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_BANG; {p++; cs = 21; goto _out;} }}
	goto st21;
tr68:
#line 1247 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_NE; {p++; cs = 21; goto _out;} }}
	goto st21;
tr69:
#line 1271 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_HASH; {p++; cs = 21; goto _out;} }}
	goto st21;
tr70:
#line 1230 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_HASHHASH; {p++; cs = 21; goto _out;} }}
	goto st21;
tr71:
#line 1276 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_PERCENT; {p++; cs = 21; goto _out;} }}
	goto st21;
tr73:
#line 1242 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_PERCENTEQ; {p++; cs = 21; goto _out;} }}
	goto st21;
tr74:
#line 1237 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RBRACE; {p++; cs = 21; goto _out;} }}
	goto st21;
tr75:
#line 1233 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_HASH; {p++; cs = 21; goto _out;} }}
	goto st21;
tr77:
#line 1277 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_AMP; {p++; cs = 21; goto _out;} }}
	goto st21;
tr78:
#line 1252 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LAND; {p++; cs = 21; goto _out;} }}
	goto st21;
tr79:
#line 1243 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_AMPEQ; {p++; cs = 21; goto _out;} }}
	goto st21;
tr80:
#line 1274 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_STAR; {p++; cs = 21; goto _out;} }}
	goto st21;
tr81:
#line 1240 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_STAREQ; {p++; cs = 21; goto _out;} }}
	goto st21;
tr82:
#line 1272 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_PLUS; {p++; cs = 21; goto _out;} }}
	goto st21;
tr83:
#line 1254 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_INC; {p++; cs = 21; goto _out;} }}
	goto st21;
tr84:
#line 1238 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_PLUSEQ; {p++; cs = 21; goto _out;} }}
	goto st21;
tr85:
#line 1273 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_MINUS; {p++; cs = 21; goto _out;} }}
	goto st21;
tr86:
#line 1255 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_DEC; {p++; cs = 21; goto _out;} }}
	goto st21;
tr87:
#line 1239 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_MINUSEQ; {p++; cs = 21; goto _out;} }}
	goto st21;
tr88:
#line 1256 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_ARROW; {p++; cs = 21; goto _out;} }}
	goto st21;
tr89:
#line 1267 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_DOT; {p++; cs = 21; goto _out;} }}
	goto st21;
tr92:
#line 1172 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{
            lex_parse_fnum(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr95:
#line 1275 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_SLASH; {p++; cs = 21; goto _out;} }}
	goto st21;
tr97:
#line 1241 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_SLASHEQ; {p++; cs = 21; goto _out;} }}
	goto st21;
tr98:
#line 1160 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ /* skip */ }}
	goto st21;
tr99:
#line 1196 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{
            lex_parse_num(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr105:
#line 1168 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{
            lex_parse_fnum(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr108:
#line 1192 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{
            lex_parse_num(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr110:
#line 1176 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{
            lex_parse_fnum(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr112:
#line 1182 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{
            lex_parse_hexfloat(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr113:
#line 1182 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{
            lex_parse_hexfloat(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr114:
#line 1188 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{
            lex_parse_num(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr116:
#line 1268 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_COLON; {p++; cs = 21; goto _out;} }}
	goto st21;
tr117:
#line 1235 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RBRACK; {p++; cs = 21; goto _out;} }}
	goto st21;
tr118:
#line 1282 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_LT; {p++; cs = 21; goto _out;} }}
	goto st21;
tr119:
#line 1236 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LBRACE; {p++; cs = 21; goto _out;} }}
	goto st21;
tr120:
#line 1234 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LBRACK; {p++; cs = 21; goto _out;} }}
	goto st21;
tr122:
#line 1248 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LE; {p++; cs = 21; goto _out;} }}
	goto st21;
tr123:
#line 1250 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_LSHIFT; {p++; cs = 21; goto _out;} }}
	goto st21;
tr124:
#line 1228 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LSHIFTEQ; {p++; cs = 21; goto _out;} }}
	goto st21;
tr125:
#line 1281 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_ASSIGN; {p++; cs = 21; goto _out;} }}
	goto st21;
tr126:
#line 1246 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_EQ; {p++; cs = 21; goto _out;} }}
	goto st21;
tr127:
#line 1283 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_GT; {p++; cs = 21; goto _out;} }}
	goto st21;
tr128:
#line 1249 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_GE; {p++; cs = 21; goto _out;} }}
	goto st21;
tr130:
#line 1251 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_RSHIFT; {p++; cs = 21; goto _out;} }}
	goto st21;
tr131:
#line 1229 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_RSHIFTEQ; {p++; cs = 21; goto _out;} }}
	goto st21;
tr132:
#line 1221 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{
            lex_parse_id(ts, te);
            {p++; cs = 21; goto _out;}
        }}
	goto st21;
tr133:
#line 1279 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_CARET; {p++; cs = 21; goto _out;} }}
	goto st21;
tr134:
#line 1245 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_CARETEQ; {p++; cs = 21; goto _out;} }}
	goto st21;
tr136:
#line 1278 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p;p--;{ lex_tok = TK_PIPE; {p++; cs = 21; goto _out;} }}
	goto st21;
tr137:
#line 1244 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_PIPEEQ; {p++; cs = 21; goto _out;} }}
	goto st21;
tr138:
#line 1253 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{te = p+1;{ lex_tok = TK_LOR; {p++; cs = 21; goto _out;} }}
	goto st21;
st21:
#line 1 "NONE"
	{ts = 0;}
#line 1 "NONE"
	{act = 0;}
	if ( ++p == pe )
		goto _test_eof21;
case 21:
#line 1 "NONE"
	{ts = p;}
#line 1700 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	switch( (*p) ) {
		case 1: goto st1;
		case 10: goto tr34;
		case 32: goto st23;
		case 33: goto st24;
		case 34: goto st2;
		case 35: goto st25;
		case 37: goto st26;
		case 38: goto st28;
		case 39: goto st5;
		case 40: goto tr39;
		case 41: goto tr40;
		case 42: goto st29;
		case 43: goto st30;
		case 44: goto tr43;
		case 45: goto st31;
		case 46: goto tr45;
		case 47: goto tr46;
		case 48: goto tr47;
		case 58: goto st51;
		case 59: goto tr50;
		case 60: goto st52;
		case 61: goto st54;
		case 62: goto st55;
		case 63: goto tr54;
		case 76: goto tr56;
		case 85: goto tr56;
		case 91: goto tr57;
		case 93: goto tr58;
		case 94: goto st59;
		case 95: goto st57;
		case 117: goto tr60;
		case 123: goto tr61;
		case 124: goto st62;
		case 125: goto tr63;
		case 126: goto tr64;
	}
	if ( (*p) < 49 ) {
		if ( 9 <= (*p) && (*p) <= 13 )
			goto st23;
	} else if ( (*p) > 57 ) {
		if ( (*p) > 90 ) {
			if ( 97 <= (*p) && (*p) <= 122 )
				goto st57;
		} else if ( (*p) >= 65 )
			goto st57;
	} else
		goto tr48;
	goto st0;
st0:
cs = 0;
	goto _out;
st1:
	if ( ++p == pe )
		goto _test_eof1;
case 1:
	if ( (*p) == 95 )
		goto st22;
	if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st22;
	} else if ( (*p) >= 65 )
		goto st22;
	goto st0;
st22:
	if ( ++p == pe )
		goto _test_eof22;
case 22:
	if ( (*p) == 95 )
		goto st22;
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st22;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st22;
	} else
		goto st22;
	goto tr65;
st23:
	if ( ++p == pe )
		goto _test_eof23;
case 23:
	switch( (*p) ) {
		case 9: goto st23;
		case 32: goto st23;
	}
	if ( 11 <= (*p) && (*p) <= 13 )
		goto st23;
	goto tr66;
st24:
	if ( ++p == pe )
		goto _test_eof24;
case 24:
	if ( (*p) == 61 )
		goto tr68;
	goto tr67;
st2:
	if ( ++p == pe )
		goto _test_eof2;
case 2:
	switch( (*p) ) {
		case 10: goto tr2;
		case 34: goto tr4;
		case 92: goto st3;
	}
	goto st2;
st3:
	if ( ++p == pe )
		goto _test_eof3;
case 3:
	goto st2;
st25:
	if ( ++p == pe )
		goto _test_eof25;
case 25:
	if ( (*p) == 35 )
		goto tr70;
	goto tr69;
st26:
	if ( ++p == pe )
		goto _test_eof26;
case 26:
	switch( (*p) ) {
		case 58: goto tr72;
		case 61: goto tr73;
		case 62: goto tr74;
	}
	goto tr71;
tr72:
#line 1 "NONE"
	{te = p+1;}
	goto st27;
st27:
	if ( ++p == pe )
		goto _test_eof27;
case 27:
#line 1838 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	if ( (*p) == 37 )
		goto st4;
	goto tr75;
st4:
	if ( ++p == pe )
		goto _test_eof4;
case 4:
	if ( (*p) == 58 )
		goto tr7;
	goto tr6;
st28:
	if ( ++p == pe )
		goto _test_eof28;
case 28:
	switch( (*p) ) {
		case 38: goto tr78;
		case 61: goto tr79;
	}
	goto tr77;
st5:
	if ( ++p == pe )
		goto _test_eof5;
case 5:
	switch( (*p) ) {
		case 10: goto tr2;
		case 39: goto tr9;
		case 92: goto st6;
	}
	goto st5;
st6:
	if ( ++p == pe )
		goto _test_eof6;
case 6:
	goto st5;
st29:
	if ( ++p == pe )
		goto _test_eof29;
case 29:
	if ( (*p) == 61 )
		goto tr81;
	goto tr80;
st30:
	if ( ++p == pe )
		goto _test_eof30;
case 30:
	switch( (*p) ) {
		case 43: goto tr83;
		case 61: goto tr84;
	}
	goto tr82;
st31:
	if ( ++p == pe )
		goto _test_eof31;
case 31:
	switch( (*p) ) {
		case 45: goto tr86;
		case 61: goto tr87;
		case 62: goto tr88;
	}
	goto tr85;
tr45:
#line 1 "NONE"
	{te = p+1;}
	goto st32;
st32:
	if ( ++p == pe )
		goto _test_eof32;
case 32:
#line 1907 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	if ( (*p) == 46 )
		goto st7;
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr91;
	goto tr89;
st7:
	if ( ++p == pe )
		goto _test_eof7;
case 7:
	if ( (*p) == 46 )
		goto tr12;
	goto tr11;
tr91:
#line 1 "NONE"
	{te = p+1;}
	goto st33;
st33:
	if ( ++p == pe )
		goto _test_eof33;
case 33:
#line 1928 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	switch( (*p) ) {
		case 69: goto st8;
		case 70: goto st35;
		case 76: goto st35;
		case 101: goto st8;
		case 102: goto st35;
		case 108: goto st35;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr91;
	goto tr92;
st8:
	if ( ++p == pe )
		goto _test_eof8;
case 8:
	switch( (*p) ) {
		case 43: goto st9;
		case 45: goto st9;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st34;
	goto tr13;
st9:
	if ( ++p == pe )
		goto _test_eof9;
case 9:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st34;
	goto tr13;
st34:
	if ( ++p == pe )
		goto _test_eof34;
case 34:
	switch( (*p) ) {
		case 70: goto st35;
		case 76: goto st35;
		case 102: goto st35;
		case 108: goto st35;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st34;
	goto tr92;
st35:
	if ( ++p == pe )
		goto _test_eof35;
case 35:
	switch( (*p) ) {
		case 70: goto st35;
		case 76: goto st35;
		case 102: goto st35;
		case 108: goto st35;
	}
	goto tr92;
tr46:
#line 1 "NONE"
	{te = p+1;}
	goto st36;
st36:
	if ( ++p == pe )
		goto _test_eof36;
case 36:
#line 1990 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	switch( (*p) ) {
		case 42: goto st10;
		case 47: goto st37;
		case 61: goto tr97;
	}
	goto tr95;
st10:
	if ( ++p == pe )
		goto _test_eof10;
case 10:
	if ( (*p) == 42 )
		goto st11;
	goto st10;
st11:
	if ( ++p == pe )
		goto _test_eof11;
case 11:
	switch( (*p) ) {
		case 42: goto st11;
		case 47: goto tr19;
	}
	goto st10;
st37:
	if ( ++p == pe )
		goto _test_eof37;
case 37:
	if ( (*p) == 10 )
		goto tr98;
	goto st37;
tr47:
#line 1 "NONE"
	{te = p+1;}
#line 1196 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{act = 11;}
	goto st38;
st38:
	if ( ++p == pe )
		goto _test_eof38;
case 38:
#line 2030 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	switch( (*p) ) {
		case 46: goto tr100;
		case 69: goto st14;
		case 76: goto st46;
		case 85: goto st46;
		case 88: goto st16;
		case 101: goto st14;
		case 108: goto st46;
		case 117: goto st46;
		case 120: goto st16;
	}
	if ( (*p) > 55 ) {
		if ( 56 <= (*p) && (*p) <= 57 )
			goto tr48;
	} else if ( (*p) >= 48 )
		goto tr101;
	goto tr99;
tr100:
#line 1 "NONE"
	{te = p+1;}
	goto st39;
st39:
	if ( ++p == pe )
		goto _test_eof39;
case 39:
#line 2056 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	switch( (*p) ) {
		case 69: goto st12;
		case 70: goto st41;
		case 76: goto st41;
		case 101: goto st12;
		case 102: goto st41;
		case 108: goto st41;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr100;
	goto tr105;
st12:
	if ( ++p == pe )
		goto _test_eof12;
case 12:
	switch( (*p) ) {
		case 43: goto st13;
		case 45: goto st13;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st40;
	goto tr20;
st13:
	if ( ++p == pe )
		goto _test_eof13;
case 13:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st40;
	goto tr20;
st40:
	if ( ++p == pe )
		goto _test_eof40;
case 40:
	switch( (*p) ) {
		case 70: goto st41;
		case 76: goto st41;
		case 102: goto st41;
		case 108: goto st41;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st40;
	goto tr105;
st41:
	if ( ++p == pe )
		goto _test_eof41;
case 41:
	switch( (*p) ) {
		case 70: goto st41;
		case 76: goto st41;
		case 102: goto st41;
		case 108: goto st41;
	}
	goto tr105;
tr101:
#line 1 "NONE"
	{te = p+1;}
#line 1192 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{act = 10;}
	goto st42;
st42:
	if ( ++p == pe )
		goto _test_eof42;
case 42:
#line 2120 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	switch( (*p) ) {
		case 46: goto tr100;
		case 69: goto st14;
		case 76: goto st47;
		case 85: goto st47;
		case 101: goto st14;
		case 108: goto st47;
		case 117: goto st47;
	}
	if ( (*p) > 55 ) {
		if ( 56 <= (*p) && (*p) <= 57 )
			goto tr48;
	} else if ( (*p) >= 48 )
		goto tr101;
	goto tr108;
tr48:
#line 1 "NONE"
	{te = p+1;}
#line 1196 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{act = 11;}
	goto st43;
st43:
	if ( ++p == pe )
		goto _test_eof43;
case 43:
#line 2146 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	switch( (*p) ) {
		case 46: goto tr100;
		case 69: goto st14;
		case 76: goto st46;
		case 85: goto st46;
		case 101: goto st14;
		case 108: goto st46;
		case 117: goto st46;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto tr48;
	goto tr99;
st14:
	if ( ++p == pe )
		goto _test_eof14;
case 14:
	switch( (*p) ) {
		case 43: goto st15;
		case 45: goto st15;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st44;
	goto tr2;
st15:
	if ( ++p == pe )
		goto _test_eof15;
case 15:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st44;
	goto tr2;
st44:
	if ( ++p == pe )
		goto _test_eof44;
case 44:
	switch( (*p) ) {
		case 70: goto st45;
		case 76: goto st45;
		case 102: goto st45;
		case 108: goto st45;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st44;
	goto tr110;
st45:
	if ( ++p == pe )
		goto _test_eof45;
case 45:
	switch( (*p) ) {
		case 70: goto st45;
		case 76: goto st45;
		case 102: goto st45;
		case 108: goto st45;
	}
	goto tr110;
st46:
	if ( ++p == pe )
		goto _test_eof46;
case 46:
	switch( (*p) ) {
		case 76: goto st46;
		case 85: goto st46;
		case 108: goto st46;
		case 117: goto st46;
	}
	goto tr99;
st47:
	if ( ++p == pe )
		goto _test_eof47;
case 47:
	switch( (*p) ) {
		case 76: goto st47;
		case 85: goto st47;
		case 108: goto st47;
		case 117: goto st47;
	}
	goto tr108;
st16:
	if ( ++p == pe )
		goto _test_eof16;
case 16:
	if ( (*p) == 46 )
		goto st17;
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr27;
	} else if ( (*p) > 70 ) {
		if ( 97 <= (*p) && (*p) <= 102 )
			goto tr27;
	} else
		goto tr27;
	goto tr25;
st17:
	if ( ++p == pe )
		goto _test_eof17;
case 17:
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st18;
	} else if ( (*p) > 70 ) {
		if ( 97 <= (*p) && (*p) <= 102 )
			goto st18;
	} else
		goto st18;
	goto tr25;
st18:
	if ( ++p == pe )
		goto _test_eof18;
case 18:
	switch( (*p) ) {
		case 80: goto st19;
		case 112: goto st19;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st18;
	} else if ( (*p) > 70 ) {
		if ( 97 <= (*p) && (*p) <= 102 )
			goto st18;
	} else
		goto st18;
	goto tr2;
st19:
	if ( ++p == pe )
		goto _test_eof19;
case 19:
	switch( (*p) ) {
		case 43: goto st20;
		case 45: goto st20;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st48;
	goto tr2;
st20:
	if ( ++p == pe )
		goto _test_eof20;
case 20:
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st48;
	goto tr2;
st48:
	if ( ++p == pe )
		goto _test_eof48;
case 48:
	switch( (*p) ) {
		case 70: goto tr113;
		case 76: goto tr113;
		case 102: goto tr113;
		case 108: goto tr113;
	}
	if ( 48 <= (*p) && (*p) <= 57 )
		goto st48;
	goto tr112;
tr27:
#line 1 "NONE"
	{te = p+1;}
#line 1188 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{act = 9;}
	goto st49;
st49:
	if ( ++p == pe )
		goto _test_eof49;
case 49:
#line 2309 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	switch( (*p) ) {
		case 46: goto st18;
		case 76: goto st50;
		case 80: goto st19;
		case 85: goto st50;
		case 108: goto st50;
		case 112: goto st19;
		case 117: goto st50;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto tr27;
	} else if ( (*p) > 70 ) {
		if ( 97 <= (*p) && (*p) <= 102 )
			goto tr27;
	} else
		goto tr27;
	goto tr114;
st50:
	if ( ++p == pe )
		goto _test_eof50;
case 50:
	switch( (*p) ) {
		case 76: goto st50;
		case 85: goto st50;
		case 108: goto st50;
		case 117: goto st50;
	}
	goto tr114;
st51:
	if ( ++p == pe )
		goto _test_eof51;
case 51:
	if ( (*p) == 62 )
		goto tr117;
	goto tr116;
st52:
	if ( ++p == pe )
		goto _test_eof52;
case 52:
	switch( (*p) ) {
		case 37: goto tr119;
		case 58: goto tr120;
		case 60: goto st53;
		case 61: goto tr122;
	}
	goto tr118;
st53:
	if ( ++p == pe )
		goto _test_eof53;
case 53:
	if ( (*p) == 61 )
		goto tr124;
	goto tr123;
st54:
	if ( ++p == pe )
		goto _test_eof54;
case 54:
	if ( (*p) == 61 )
		goto tr126;
	goto tr125;
st55:
	if ( ++p == pe )
		goto _test_eof55;
case 55:
	switch( (*p) ) {
		case 61: goto tr128;
		case 62: goto st56;
	}
	goto tr127;
st56:
	if ( ++p == pe )
		goto _test_eof56;
case 56:
	if ( (*p) == 61 )
		goto tr131;
	goto tr130;
st57:
	if ( ++p == pe )
		goto _test_eof57;
case 57:
	if ( (*p) == 95 )
		goto st57;
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st57;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st57;
	} else
		goto st57;
	goto tr132;
tr56:
#line 1 "NONE"
	{te = p+1;}
#line 1221 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{act = 15;}
	goto st58;
st58:
	if ( ++p == pe )
		goto _test_eof58;
case 58:
#line 2412 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	switch( (*p) ) {
		case 34: goto st2;
		case 39: goto st5;
		case 95: goto st57;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st57;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st57;
	} else
		goto st57;
	goto tr132;
st59:
	if ( ++p == pe )
		goto _test_eof59;
case 59:
	if ( (*p) == 61 )
		goto tr134;
	goto tr133;
tr60:
#line 1 "NONE"
	{te = p+1;}
#line 1221 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{act = 15;}
	goto st60;
st60:
	if ( ++p == pe )
		goto _test_eof60;
case 60:
#line 2444 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	switch( (*p) ) {
		case 34: goto st2;
		case 39: goto st5;
		case 56: goto tr135;
		case 95: goto st57;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st57;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st57;
	} else
		goto st57;
	goto tr132;
tr135:
#line 1 "NONE"
	{te = p+1;}
#line 1221 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"
	{act = 15;}
	goto st61;
st61:
	if ( ++p == pe )
		goto _test_eof61;
case 61:
#line 2470 "/Users/sdennis/slow-32/selfhost/src/c_lexer_gen.c"
	switch( (*p) ) {
		case 34: goto st2;
		case 95: goto st57;
	}
	if ( (*p) < 65 ) {
		if ( 48 <= (*p) && (*p) <= 57 )
			goto st57;
	} else if ( (*p) > 90 ) {
		if ( 97 <= (*p) && (*p) <= 122 )
			goto st57;
	} else
		goto st57;
	goto tr132;
st62:
	if ( ++p == pe )
		goto _test_eof62;
case 62:
	switch( (*p) ) {
		case 61: goto tr137;
		case 124: goto tr138;
	}
	goto tr136;
	}
	_test_eof21: cs = 21; goto _test_eof; 
	_test_eof1: cs = 1; goto _test_eof; 
	_test_eof22: cs = 22; goto _test_eof; 
	_test_eof23: cs = 23; goto _test_eof; 
	_test_eof24: cs = 24; goto _test_eof; 
	_test_eof2: cs = 2; goto _test_eof; 
	_test_eof3: cs = 3; goto _test_eof; 
	_test_eof25: cs = 25; goto _test_eof; 
	_test_eof26: cs = 26; goto _test_eof; 
	_test_eof27: cs = 27; goto _test_eof; 
	_test_eof4: cs = 4; goto _test_eof; 
	_test_eof28: cs = 28; goto _test_eof; 
	_test_eof5: cs = 5; goto _test_eof; 
	_test_eof6: cs = 6; goto _test_eof; 
	_test_eof29: cs = 29; goto _test_eof; 
	_test_eof30: cs = 30; goto _test_eof; 
	_test_eof31: cs = 31; goto _test_eof; 
	_test_eof32: cs = 32; goto _test_eof; 
	_test_eof7: cs = 7; goto _test_eof; 
	_test_eof33: cs = 33; goto _test_eof; 
	_test_eof8: cs = 8; goto _test_eof; 
	_test_eof9: cs = 9; goto _test_eof; 
	_test_eof34: cs = 34; goto _test_eof; 
	_test_eof35: cs = 35; goto _test_eof; 
	_test_eof36: cs = 36; goto _test_eof; 
	_test_eof10: cs = 10; goto _test_eof; 
	_test_eof11: cs = 11; goto _test_eof; 
	_test_eof37: cs = 37; goto _test_eof; 
	_test_eof38: cs = 38; goto _test_eof; 
	_test_eof39: cs = 39; goto _test_eof; 
	_test_eof12: cs = 12; goto _test_eof; 
	_test_eof13: cs = 13; goto _test_eof; 
	_test_eof40: cs = 40; goto _test_eof; 
	_test_eof41: cs = 41; goto _test_eof; 
	_test_eof42: cs = 42; goto _test_eof; 
	_test_eof43: cs = 43; goto _test_eof; 
	_test_eof14: cs = 14; goto _test_eof; 
	_test_eof15: cs = 15; goto _test_eof; 
	_test_eof44: cs = 44; goto _test_eof; 
	_test_eof45: cs = 45; goto _test_eof; 
	_test_eof46: cs = 46; goto _test_eof; 
	_test_eof47: cs = 47; goto _test_eof; 
	_test_eof16: cs = 16; goto _test_eof; 
	_test_eof17: cs = 17; goto _test_eof; 
	_test_eof18: cs = 18; goto _test_eof; 
	_test_eof19: cs = 19; goto _test_eof; 
	_test_eof20: cs = 20; goto _test_eof; 
	_test_eof48: cs = 48; goto _test_eof; 
	_test_eof49: cs = 49; goto _test_eof; 
	_test_eof50: cs = 50; goto _test_eof; 
	_test_eof51: cs = 51; goto _test_eof; 
	_test_eof52: cs = 52; goto _test_eof; 
	_test_eof53: cs = 53; goto _test_eof; 
	_test_eof54: cs = 54; goto _test_eof; 
	_test_eof55: cs = 55; goto _test_eof; 
	_test_eof56: cs = 56; goto _test_eof; 
	_test_eof57: cs = 57; goto _test_eof; 
	_test_eof58: cs = 58; goto _test_eof; 
	_test_eof59: cs = 59; goto _test_eof; 
	_test_eof60: cs = 60; goto _test_eof; 
	_test_eof61: cs = 61; goto _test_eof; 
	_test_eof62: cs = 62; goto _test_eof; 

	_test_eof: {}
	if ( p == eof )
	{
	switch ( cs ) {
	case 22: goto tr65;
	case 23: goto tr66;
	case 24: goto tr67;
	case 2: goto tr2;
	case 3: goto tr2;
	case 25: goto tr69;
	case 26: goto tr71;
	case 27: goto tr75;
	case 4: goto tr6;
	case 28: goto tr77;
	case 5: goto tr2;
	case 6: goto tr2;
	case 29: goto tr80;
	case 30: goto tr82;
	case 31: goto tr85;
	case 32: goto tr89;
	case 7: goto tr11;
	case 33: goto tr92;
	case 8: goto tr13;
	case 9: goto tr13;
	case 34: goto tr92;
	case 35: goto tr92;
	case 36: goto tr95;
	case 10: goto tr16;
	case 11: goto tr16;
	case 37: goto tr98;
	case 38: goto tr99;
	case 39: goto tr105;
	case 12: goto tr20;
	case 13: goto tr20;
	case 40: goto tr105;
	case 41: goto tr105;
	case 42: goto tr108;
	case 43: goto tr99;
	case 14: goto tr2;
	case 15: goto tr2;
	case 44: goto tr110;
	case 45: goto tr110;
	case 46: goto tr99;
	case 47: goto tr108;
	case 16: goto tr25;
	case 17: goto tr25;
	case 18: goto tr2;
	case 19: goto tr2;
	case 20: goto tr2;
	case 48: goto tr112;
	case 49: goto tr114;
	case 50: goto tr114;
	case 51: goto tr116;
	case 52: goto tr118;
	case 53: goto tr123;
	case 54: goto tr125;
	case 55: goto tr127;
	case 56: goto tr130;
	case 57: goto tr132;
	case 58: goto tr132;
	case 59: goto tr133;
	case 60: goto tr132;
	case 61: goto tr132;
	case 62: goto tr136;
	}
	}

	_out: {}
	}

#line 1413 "/Users/sdennis/slow-32/selfhost/src/c_lexer.rl"

    lex_rp = p;
    lex_rcs = cs;
    lex_ract = act;
    lex_rts = ts;
    lex_rte = te;

    /* Update lex_pos for compatibility */
    lex_pos = (int)(p - lex_src);
}
