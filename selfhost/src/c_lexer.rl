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

/* grown tables, for every module after this one */
#include "tables.h"

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

#define LEX_STR_SZ   256
/* The string-literal pool and the table of literals grow (tables.h):
 * they were 4MB and 65536, and their writers dropped what did not fit
 * without a word. */

/* The translation unit as the preprocessor rewrites it -- includes
 * spliced in, macros expanded in place -- grown (tables.h, TAB_LEX_SRC;
 * lex_src_room).  It was 12MB: a longer source or #include was cut to
 * what fit.  Past lex_len are scratch areas (pp_preexpand's argument
 * text, pp_ev_from_text's parking), within lex_src_cap. */
static char *lex_src;
static int  lex_src_cap;
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
static char *lex_strpool;          /* grown (tables.h, TAB_LEX_POOL) */
static int lex_strpool_cap;
static int  lex_strpool_len;
static int *lex_str_off;
static int *lex_str_len;
/* bytes per element: 1 for "..." and u8"...", 2 for u"...", 4 for L"..."
 * and U"..." (6.4.5).  A wide string's code units are pooled little-
 * endian, its length width*n + width-1 so the emitter's closing 0 byte
 * completes the terminating unit -- and sizeof, length + 1, is right. */
static int *lex_str_width;
static int *lex_str_pfx;    /* its prefix: 0, 'L', 'u', 'U', or 8 for u8 */
static int  lex_chr_prefix;    /* L, u or U before a character constant, 0 for none */
static int  lex_str_count;
static int lex_str_count_cap;

/* room for need entries (tables.h, TAB_LEX_STR) */
/* the interning hash's chain and each literal's hash (lex_str_intern) */
static int *lex_str_hnext;
static unsigned *lex_str_hval;
/* 1 once the code or data refers to it: only those are emitted (a piece
 * of a concatenation never is, nor a literal only sizeof'd) */
static char *lex_str_used;

static void lex_str_room(int need) {
    if (need <= lex_str_count_cap) return;
    if (lex_str_count_cap == 0) {
        tab_add((char **)&lex_str_hnext, (int)sizeof(int), TAB_LEX_STR, 0);
        tab_add(&lex_str_used, 1, TAB_LEX_STR, 0);
        tab_add((char **)&lex_str_hval, (int)sizeof(unsigned), TAB_LEX_STR, 0);
        tab_add((char **)&lex_str_off, (int)sizeof(int), TAB_LEX_STR, 0);
        tab_add((char **)&lex_str_len, (int)sizeof(int), TAB_LEX_STR, 0);
        tab_add((char **)&lex_str_width, (int)sizeof(int), TAB_LEX_STR, 0);
        tab_add((char **)&lex_str_pfx, (int)sizeof(int), TAB_LEX_STR, 0);
    }
    tab_grow(TAB_LEX_STR, &lex_str_count_cap, 1024, need);
}

/* room in the pool for n more bytes */
static void lex_pool_room(int n) {
    tab_room1(&lex_strpool, 1, TAB_LEX_POOL, &lex_strpool_cap, 65536, lex_strpool_len + n);
}

static void lex_pool_put(int c) {
    lex_pool_room(1);
    lex_strpool[lex_strpool_len] = c;
    lex_strpool_len = lex_strpool_len + 1;
}


/* Ragel persistent state */
static char *lex_rp;    /* scan cursor (Ragel p) */
static char *lex_rpe;   /* end pointer (Ragel pe) */
static int   lex_rcs;   /* current state */
static int   lex_ract;  /* scanner action */
static char *lex_rts;   /* token start */
static char *lex_rte;   /* token end */

/* Growing lex_src moves it, and these follow it: the scanner's
 * pointers above, lex_warn_ts, and the parser's lexer marks.  Nothing
 * else keeps a pointer into it across a call that can grow it -- the
 * preprocessor works in offsets, and the scanner's own locals (lex_next)
 * never call the preprocessor. */
static void ps_mk_rebase(char *old, int oldcap, char *nw);   /* parser.h */

static char *lex_rebase(char *q, char *old, int oldcap, char *nw) {
    if (q == 0 || q < old || q > old + oldcap) return q;
    return nw + (q - old);
}

/* S12CC_SRCMOVE=1: every request for room past lex_len moves lex_src
 * to a new block, so a pointer left behind shows up at once (a test
 * knob, as S12CC_TABDBG is; test_src_moves) */
static int lex_src_moves = -1;

/* room for need bytes in lex_src */
static void lex_src_room(int need) {
    char *old;
    int oldcap;
    if (lex_src_moves < 0) lex_src_moves = getenv("S12CC_SRCMOVE") != 0;
    if (need <= lex_src_cap && !(lex_src_moves && need > lex_len + 1)) return;
    old = lex_src;
    oldcap = lex_src_cap;
    if (lex_src_moves && old != 0) {
        char *nb;
        int nc;
        nc = need > oldcap ? need : oldcap;
        nb = (char *)malloc((unsigned long)nc);
        if (!nb) {
            fdputs("s12cc: out of memory (S12CC_SRCMOVE)\n", 2);
            exit(1);
        }
        memcpy(nb, old, (unsigned long)oldcap);
        if (nc > oldcap) memset(nb + oldcap, 0, (unsigned long)(nc - oldcap));
        memset(old, 85, (unsigned long)oldcap);     /* what is left behind is garbage */
        free(old);
        lex_src = nb;
        lex_src_cap = nc;
    } else {
        tab_room1(&lex_src, 1, TAB_LEX_SRC, &lex_src_cap, 1 << 20, need);
    }
    if (old == 0 || lex_src == old) return;
    lex_rp = lex_rebase(lex_rp, old, oldcap, lex_src);
    lex_rpe = lex_rebase(lex_rpe, old, oldcap, lex_src);
    lex_rts = lex_rebase(lex_rts, old, oldcap, lex_src);
    lex_rte = lex_rebase(lex_rte, old, oldcap, lex_src);
    lex_warn_ts = lex_rebase(lex_warn_ts, old, oldcap, lex_src);
    ps_mk_rebase(old, oldcap, lex_src);
}

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
 * Off that path the double-double ladder below comes within a fraction
 * of an ulp, and lex_round_exact settles the last bit against the
 * halfway points exactly: every decimal constant, double or float, is
 * the nearest one, ties to even (selfhost ISSUES-108). */

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

/* === Correct rounding: the decimal against the halfway points ===
 *
 * The conversion below is exact on its fast path and otherwise within a
 * small fraction of an ulp -- which is one ulp wrong when the decimal
 * lies just across a halfway point: 1e23 came out 0x44b52d02c7e14af7,
 * strtod's is ...af6.  So, as David Gay's strtod corrects its first
 * approximation: with the candidate in hand, the decimal is compared,
 * exactly and in big integers, with the halfway points either side of
 * it, and the candidate steps across (or stays: a tie goes to the even
 * one).  A float literal is rounded this way at float's precision,
 * once -- (float) of the double rounded twice.
 *
 * Gay's strtod itself (runtime/dtoa.c) is in the library programs link,
 * not in this compiler's: stage07 builds the compiler and its library,
 * and cannot compile it (it returns doubles).  Hence 16-bit limbs in
 * file-scope arrays, values through statics, as the rest of this file. */
#define LEX_BN 700                    /* limbs: 11,200 bits */
#define LEX_DEC_MAXDIG 780            /* digits kept; past them, a sticky 1 */
static int lex_bnd[LEX_BN];           /* the decimal's digits, an integer */
static int lex_bnd_n;
static int lex_bnx[LEX_BN];
static int lex_bnx_n;
static int lex_bny[LEX_BN];
static int lex_bny_n;
static int lex_dec_e;                 /* the decimal is lex_bnd * 10^lex_dec_e */
static int lex_dec_mag;               /* its magnitude: about 10^lex_dec_mag */
static double lex_fix_d;              /* the candidate in, a double result out */
static int lex_fix_fbits;             /* a float result, its bits */

/* w = w * m + a, m and a below 2^16 */
static void lex_bn_muladd(int *w, int *n, int m, int a) {
    long long carry;
    long long t;
    int i;
    carry = a;
    i = 0;
    while (i < *n) {
        t = (long long)w[i] * m + carry;
        w[i] = (int)(t & 65535);
        carry = t >> 16;
        i = i + 1;
    }
    while (carry != 0 && *n < LEX_BN) {
        w[*n] = (int)(carry & 65535);
        carry = carry >> 16;
        *n = *n + 1;
    }
}

static void lex_bn_mulpow5(int *w, int *n, int e) {
    int m;
    while (e >= 6) {
        lex_bn_muladd(w, n, 15625, 0);      /* 5^6 */
        e = e - 6;
    }
    m = 1;
    while (e > 0) {
        m = m * 5;
        e = e - 1;
    }
    if (m > 1) lex_bn_muladd(w, n, m, 0);
}

static void lex_bn_shl(int *w, int *n, int s) {
    int words;
    int bits;
    int carry;
    int v;
    int i;
    if (*n == 0) return;
    words = s / 16;
    bits = s % 16;
    if (bits) {
        carry = 0;
        i = 0;
        while (i < *n) {
            v = w[i];
            w[i] = ((v << bits) | carry) & 65535;
            carry = v >> (16 - bits);
            i = i + 1;
        }
        if (carry && *n < LEX_BN) {
            w[*n] = carry;
            *n = *n + 1;
        }
    }
    if (words) {
        if (*n + words > LEX_BN) words = LEX_BN - *n;
        i = *n - 1;
        while (i >= 0) {
            w[i + words] = w[i];
            i = i - 1;
        }
        i = 0;
        while (i < words) {
            w[i] = 0;
            i = i + 1;
        }
        *n = *n + words;
    }
}

static int lex_bn_cmp(int *x, int xn, int *y, int yn) {
    int i;
    while (xn > 0 && x[xn - 1] == 0) xn = xn - 1;
    while (yn > 0 && y[yn - 1] == 0) yn = yn - 1;
    if (xn != yn) return xn < yn ? -1 : 1;
    i = xn - 1;
    while (i >= 0) {
        if (x[i] != y[i]) return x[i] < y[i] ? -1 : 1;
        i = i - 1;
    }
    return 0;
}

/* The decimal floating constant in ts..te, exactly: lex_bnd, lex_dec_e,
 * lex_dec_mag (the sign is not read: the caller has it) */
static void lex_dec_scan(char *ts, char *te) {
    char *p;
    int ch;
    int nd;
    int frac;
    int ex;
    int eneg;
    int saw_dot;
    int sticky;
    p = ts;
    nd = 0;
    frac = 0;
    ex = 0;
    saw_dot = 0;
    sticky = 0;
    lex_bnd_n = 0;
    lex_dec_e = 0;
    if (p < te && (*p == 45 || *p == 43)) p = p + 1;
    while (p < te) {
        ch = *p & 255;
        if (ch == 46) {
            saw_dot = 1;
        } else if (ch >= 48 && ch <= 57) {
            if (nd == 0 && ch == 48) {
                if (saw_dot) frac = frac + 1;       /* a leading zero: scale only */
            } else if (nd < LEX_DEC_MAXDIG) {
                lex_bn_muladd(lex_bnd, &lex_bnd_n, 10, ch - 48);
                nd = nd + 1;
                if (saw_dot) frac = frac + 1;
            } else {
                if (ch != 48) sticky = 1;
                if (!saw_dot) lex_dec_e = lex_dec_e + 1;
            }
        } else {
            break;
        }
        p = p + 1;
    }
    if (p < te && (*p == 101 || *p == 69)) {
        p = p + 1;
        eneg = 0;
        if (p < te && *p == 45) { eneg = 1; p = p + 1; }
        else if (p < te && *p == 43) { p = p + 1; }
        while (p < te && *p >= 48 && *p <= 57) {
            if (ex < 100000000) ex = ex * 10 + (*p - 48);
            p = p + 1;
        }
        if (eneg) ex = 0 - ex;
    }
    if (sticky) {
        /* digits past the kept ones, not all zero: a 1 after the last
         * kept stands for them -- above the truncation, below the next */
        lex_bn_muladd(lex_bnd, &lex_bnd_n, 10, 1);
        nd = nd + 1;
        lex_dec_e = lex_dec_e - 1;
    }
    lex_dec_e = lex_dec_e + ex - frac;
    lex_dec_mag = nd + lex_dec_e - 1;
}

/* the sign of (the decimal) - M * 2^K */
static int lex_dec_cmp(long long m, int k) {
    int i;
    int ax;
    int ay;
    i = 0;
    while (i < lex_bnd_n) {
        lex_bnx[i] = lex_bnd[i];
        i = i + 1;
    }
    lex_bnx_n = lex_bnd_n;
    lex_bny_n = 0;
    while (m != 0) {
        lex_bny[lex_bny_n] = (int)(m & 65535);
        lex_bny_n = lex_bny_n + 1;
        m = m >> 16;
    }
    if (lex_dec_e >= 0) {
        lex_bn_mulpow5(lex_bnx, &lex_bnx_n, lex_dec_e);
        ax = lex_dec_e;
        ay = k;
    } else {
        lex_bn_mulpow5(lex_bny, &lex_bny_n, 0 - lex_dec_e);
        ax = 0;
        ay = k - lex_dec_e;
    }
    if (ax > ay) lex_bn_shl(lex_bnx, &lex_bnx_n, ax - ay);
    else if (ay > ax) lex_bn_shl(lex_bny, &lex_bny_n, ay - ax);
    return lex_bn_cmp(lex_bnx, lex_bnx_n, lex_bny, lex_bny_n);
}

/* lex_fix_d: the correctly rounded value of the decimal in ts..te, a
 * double or (is_float) a float, from the candidate lex_fix_d holds --
 * positive, a close approximation, inf when it overflowed.  The value is
 * sig * 2^e2 at precision p (a normal one has sig >= 2^(p-1)).  A float
 * is made from the double's bits and handed back as bits: stage07's
 * conversions between float and double lose subnormals. */
static void lex_round_exact(char *ts, char *te, int is_float) {
    int p;
    int emin;
    int emax;
    int bias;
    int mbits;
    int e2;
    int biased;
    int inf;
    int steps;
    int c;
    int w[2];
    int drop;
    long long sig;
    long long top;
    long long mant;
    lex_dec_scan(ts, te);
    lex_fix_fbits = 0;
    if (lex_bnd_n == 0) { lex_fix_d = 0.0; return; }
    p = is_float ? 24 : 53;
    mbits = p - 1;
    bias = is_float ? 127 : 1023;
    emin = 2 - bias - p;                  /* -1074, -149: a subnormal's */
    emax = bias - mbits;                  /* 971, 104: the largest finite's */
    top = 1;
    top = top << mbits;                   /* 2^(p-1) */
    inf = 0;
    if (lex_dec_mag > 310) inf = 1;
    if (lex_dec_mag < -330) { lex_fix_d = 0.0; return; }
    /* the candidate, as sig and e2: the double's, and for a float the
     * double's significand cut to 24 bits (within an ulp: the steps
     * below finish it) */
    memcpy(w, &lex_fix_d, 8);
    biased = (w[1] >> 20) & 2047;
    mant = (long long)(w[1] & 0xFFFFF);
    mant = (mant << 16) | (long long)((w[0] >> 16) & 65535);
    mant = (mant << 16) | (long long)(w[0] & 65535);
    if (biased == 2047) inf = 1;
    if (biased == 0) {
        sig = mant;
        e2 = -1074;
    } else {
        sig = 1;
        sig = (sig << 52) | mant;
        e2 = biased - 1075;
    }
    if (is_float && !inf && sig != 0) {
        drop = 29;
        if (e2 + drop < emin) drop = emin - e2;
        if (drop > 62) {
            sig = 0;
            e2 = emin;
        } else {
            sig = sig >> drop;
            e2 = e2 + drop;
        }
        if (sig == 0) e2 = emin;
        if (e2 > emax) inf = 1;
    }
    if (inf) {                            /* the largest finite, to be tested */
        sig = top + top - 1;
        e2 = emax;
    }
    steps = 0;
    while (steps < 8) {
        steps = steps + 1;
        /* above the halfway point to the next one up: step up */
        c = lex_dec_cmp(sig + sig + 1, e2 - 1);
        if (c > 0 || (c == 0 && (sig & 1))) {
            sig = sig + 1;
            if (sig == top + top) {
                sig = top;
                e2 = e2 + 1;
            }
            if (e2 > emax) { inf = 1; break; }
            inf = 0;
            continue;
        }
        inf = 0;
        if (sig == 0) break;
        /* below the halfway point to the next one down: step down */
        if (sig == top && e2 > emin) c = lex_dec_cmp(sig * 4 - 1, e2 - 2);
        else c = lex_dec_cmp(sig + sig - 1, e2 - 1);
        if (c < 0 || (c == 0 && (sig & 1))) {
            if (sig == top && e2 > emin) {
                sig = top + top - 1;
                e2 = e2 - 1;
            } else {
                sig = sig - 1;
            }
            continue;
        }
        break;
    }
    /* the result's bits */
    mant = sig & (top - 1);
    if (inf) {
        biased = bias + bias + 1;
        mant = 0;
    } else if (sig >= top) {
        biased = e2 + bias + mbits;
    } else {
        biased = 0;
    }
    if (is_float) {
        lex_fix_fbits = (biased << 23) | (int)mant;
        return;
    }
    w[1] = (biased << 20) | (int)((mant >> 32) & 0xFFFFF);
    w[0] = (int)((mant >> 16) & 65535);
    w[0] = (w[0] << 16) | (int)(mant & 65535);
    memcpy(&lex_fix_d, w, 8);
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
    int once;       /* the conversion rounded exactly once: no correction */
    int dropped;    /* a nonzero digit past the 18 kept */

    once = 0;
    dropped = 0;
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
                if (ch != 48) dropped = 1;
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
     * steps and sig's conversion residue rides along, so the result is
     * within a fraction of an ulp -- off by one only near a halfway
     * point, in the split-overflow band above ~6.7e300, and deep in the
     * denormal range, which lex_round_exact then corrects. */
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
            once = 1;
        } else if (exp10 > 0 && exp10 <= 22 && sigerr == 0.0) {
            d = d * lex_p10[exp10];
            once = 1;
        } else if (exp10 < 0 && exp10 >= -22 && sigerr == 0.0) {
            d = d / lex_p10[0 - exp10];
            once = 1;
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

    /* Off the one-rounding path, or a float (whose (float)d would round
     * a second time): the decimal against the halfway points */
    if (is_float || dropped || !once) {
        lex_fix_d = d;
        lex_round_exact(ts, te, is_float);
        d = lex_fix_d;
    }

    /* Emit IEEE bits (sign applied by bit flip so -0.0 works) */
    if (is_float) {
        lex_val = lex_fix_fbits;      /* lex_round_exact's, always for a float */
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
 * address), as clang and gcc do.  Returns the existing index, the
 * lowest that matches, or -1.  Lengths are the literals' own, the
 * bytes the pool's.  A hash of chains (lex_str_hhead, by length and bytes)
 * that catches up on the literals registered since the last lookup,
 * whichever path registered them: it was a scan of every literal, and
 * 70,000 of them took most of a minute. */
static int *lex_str_hhead;
static int lex_str_hcap;        /* buckets, a power of two */
static int lex_str_hashed;      /* literals in the hash */

static unsigned lex_str_hash(int off, int slen) {
    unsigned h;
    int j;
    h = 2166136261u ^ (unsigned)slen;
    j = 0;
    while (j < slen) {
        h = (h ^ (unsigned)(lex_strpool[off + j] & 255)) * 16777619u;
        j = j + 1;
    }
    return h;
}

static void lex_str_hash_add(int k) {
    int b;
    b = (int)(lex_str_hval[k] & (unsigned)(lex_str_hcap - 1));
    lex_str_hnext[k] = lex_str_hhead[b];
    lex_str_hhead[b] = k;
}

static void lex_str_hash_sync(void) {
    int k;
    if (lex_str_count > lex_str_hcap) {
        k = lex_str_hcap ? lex_str_hcap : 1024;
        while (k < lex_str_count) k = k * 2;
        if (lex_str_hhead) free(lex_str_hhead);
        lex_str_hhead = (int *)malloc((unsigned long)k * sizeof(int));
        if (!lex_str_hhead) {
            fdputs("s12cc: out of memory (string literals)\n", 2);
            exit(1);
        }
        lex_str_hcap = k;
        k = 0;
        while (k < lex_str_hcap) { lex_str_hhead[k] = -1; k = k + 1; }
        k = 0;
        while (k < lex_str_hashed) { lex_str_hash_add(k); k = k + 1; }
    }
    while (lex_str_hashed < lex_str_count) {
        lex_str_hval[lex_str_hashed] = lex_str_hash(lex_str_off[lex_str_hashed], lex_str_len[lex_str_hashed]);
        lex_str_hash_add(lex_str_hashed);
        lex_str_hashed = lex_str_hashed + 1;
    }
}

static int lex_str_intern(int pool_start, int slen) {
    int k;
    int j;
    int a;
    int b;
    int best;
    unsigned h;
    if (lex_str_count == 0) return -1;
    lex_str_hash_sync();
    h = lex_str_hash(pool_start, slen);
    best = -1;
    k = lex_str_hhead[(int)(h & (unsigned)(lex_str_hcap - 1))];
    while (k >= 0) {
        if (lex_str_hval[k] == h && lex_str_len[k] == slen && (best < 0 || k < best)) {
            a = lex_str_off[k];
            b = pool_start;
            j = 0;
            while (j < slen && lex_strpool[a + j] == lex_strpool[b + j]) j = j + 1;
            if (j == slen) best = k;
        }
        k = lex_str_hnext[k];
    }
    return best;
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
    lex_pool_room(5);
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
    while (i < width) {
        lex_pool_put(u & 255);
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
    lex_pool_put(0);
    lex_str[0] = 0;
    lex_slen = 0;
    lex_str_room(lex_str_count + 1);
    lex_str_off[lex_str_count] = pool_start;
    lex_str_len[lex_str_count] = units * width + width - 1;
    lex_str_width[lex_str_count] = width;
    lex_str_pfx[lex_str_count] = ts[0];
    lex_val = lex_str_count;
    lex_str_count = lex_str_count + 1;
    lex_tok = TK_STRING;
}

static void lex_parse_str(char *ts, char *te) {
    int ucn_n;
    int ucn_k;
    int pool_start;
    int slen;
    int total;      /* the literal's length: slen is lex_str's, at most LEX_STR_SZ - 1 */
    int ch;
    int pos;
    int end;
    int dup;
    if (ts[0] == 76 || ts[0] == 85) { lex_parse_wide_str(ts, te, 4); return; }   /* L"" U"" */
    if (ts[0] == 117 && ts[1] != 56) { lex_parse_wide_str(ts, te, 2); return; }  /* u"" */
    pool_start = lex_strpool_len;
    slen = 0;
    total = 0;
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
            total = total + ucn_n;
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
        lex_pool_put(ch);
        total = total + 1;
        if (slen < LEX_STR_SZ - 1) {
            lex_str[slen] = ch;
            slen = slen + 1;
        }
    }
    lex_pool_put(0);
    lex_str[slen] = 0;
    lex_slen = slen;
    dup = ts[0] == 117 ? -1 : lex_str_intern(pool_start, total);   /* a u8 one keeps its own entry, and its prefix */
    if (dup >= 0) {
        lex_strpool_len = pool_start;   /* give the bytes back */
        lex_val = dup;
        lex_tok = TK_STRING;
        return;
    }
    lex_str_room(lex_str_count + 1);
    lex_str_off[lex_str_count] = pool_start;
    lex_str_len[lex_str_count] = total;    /* (it was slen: a literal past 255 was cut there) */
    lex_str_width[lex_str_count] = 1;
    lex_str_pfx[lex_str_count] = ts[0] == 117 ? 8 : 0;
    lex_val = lex_str_count;
    lex_str_count = lex_str_count + 1;
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

%%{
    machine c_lexer;

    main := |*

        # --- Whitespace ---
        [ \t\r\f\v]+ => { /* skip */ };
        '\n' => { lex_line = lex_line + 1; lex_bol = 1; };

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

        # --- Hexadecimal floating constant (6.4.4.2) ---
        '0' [xX] ( [0-9a-fA-F]* '.' [0-9a-fA-F]+ | [0-9a-fA-F]+ '.'? ) [pP] [+\-]? [0-9]+ [fFlL]? => {
            lex_parse_hexfloat(ts, te);
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

        # --- String literal (u8 L u U prefixes: 6.4.5) ---
        ( 'u8' | 'L' | 'u' | 'U' )? '"' ( [^"\\\n] | '\\' any )* '"' => {
            lex_parse_str(ts, te);
            fbreak;
        };

        # --- Char literal (L u U prefixes: 6.4.4.4) ---
        ( 'L' | 'u' | 'U' )? "'" ( [^'\\\n] | '\\' any )* "'" => {
            lex_parse_chr(ts, te);
            fbreak;
        };

        # --- Painted identifier: 0x01 then the name (pp_rescan_region) ---
        0x01 [a-zA-Z_][a-zA-Z0-9_]* => {
            lex_parse_id(ts + 1, te);
            lex_painted = 1;
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
        # --- Digraphs (6.4.6p3): the same tokens, other spellings ---
        '%:%:' => { lex_tok = TK_HASHHASH; fbreak; };
        '%:'  => { lex_tok = TK_HASH; fbreak; };
        '<:'  => { lex_tok = TK_LBRACK; fbreak; };
        ':>'  => { lex_tok = TK_RBRACK; fbreak; };
        '<%'  => { lex_tok = TK_LBRACE; fbreak; };
        '%>'  => { lex_tok = TK_RBRACE; fbreak; };
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
    lex_src_room(len + 1);
    i = 0;
    while (i < len) {
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
    lex_str_hashed = 0;            /* the interning hash starts over */
    if (lex_str_hhead) free(lex_str_hhead);
    lex_str_hhead = 0;
    lex_str_hcap = 0;

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
