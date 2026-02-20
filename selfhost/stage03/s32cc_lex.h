/* s32cc_lex.h -- C lexer for s32-cc (stage03)
 *
 * Single includable file: token constants + full lexer implementation.
 * cc-min compatible: no switch, no postfix++, no goto, no do-while.
 */

/* === Libc prototypes (no #include nesting) === */
int strcmp(char *a, char *b);
int strncmp(char *a, char *b, int n);
int strlen(char *s);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);

int fputs(char *s, int f);
int fputc(int c, int f);
void fput_uint(int f, int v);
void exit(int status);

#define stderr 2
#define NULL 0

/* ========================================================
 * Token type constants
 * ======================================================== */

/* Special */
#define TK_EOF        0

/* Literals */
#define TK_NUM        1
#define TK_STRING     2
#define TK_CHARLIT    3

/* Identifier */
#define TK_IDENT      4

/* Keywords (C89/C99) */
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

/* Operators and punctuation */
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

/* ========================================================
 * Lexer state (globals)
 * ======================================================== */

#define LEX_SRC_SZ   262144
#define LEX_STR_SZ   256
#define LEX_POOL_SZ  65536
#define LEX_POOL_MAX 2048

static char lex_src[LEX_SRC_SZ];
static int  lex_len;
static int  lex_pos;
static int  lex_line;
static int  lex_col;
static int  lex_tok;
static int  lex_val;
static char lex_str[LEX_STR_SZ];
static int  lex_slen;

/* String pool for string literals */
static char lex_strpool[LEX_POOL_SZ];
static int  lex_strpool_len;
static int  lex_str_off[LEX_POOL_MAX];
static int  lex_str_len[LEX_POOL_MAX];
static int  lex_str_count;

/* ========================================================
 * Keyword lookup (if/else chain with strcmp)
 * ======================================================== */

int lex_kw_lookup(char *name) {
    int c;
    c = name[0];
    if (c == 97) {  /* a */
        if (strcmp(name, "auto") == 0) return TK_AUTO;
        return 0;
    }
    if (c == 98) {  /* b */
        if (strcmp(name, "break") == 0) return TK_BREAK;
        return 0;
    }
    if (c == 99) {  /* c */
        if (strcmp(name, "case") == 0) return TK_CASE;
        if (strcmp(name, "char") == 0) return TK_CHAR;
        if (strcmp(name, "const") == 0) return TK_CONST;
        if (strcmp(name, "continue") == 0) return TK_CONTINUE;
        return 0;
    }
    if (c == 100) {  /* d */
        if (strcmp(name, "default") == 0) return TK_DEFAULT;
        if (strcmp(name, "do") == 0) return TK_DO;
        if (strcmp(name, "double") == 0) return TK_DOUBLE;
        return 0;
    }
    if (c == 101) {  /* e */
        if (strcmp(name, "else") == 0) return TK_ELSE;
        if (strcmp(name, "enum") == 0) return TK_ENUM;
        if (strcmp(name, "extern") == 0) return TK_EXTERN;
        return 0;
    }
    if (c == 102) {  /* f */
        if (strcmp(name, "float") == 0) return TK_FLOAT;
        if (strcmp(name, "for") == 0) return TK_FOR;
        return 0;
    }
    if (c == 103) {  /* g */
        if (strcmp(name, "goto") == 0) return TK_GOTO;
        return 0;
    }
    if (c == 105) {  /* i */
        if (strcmp(name, "if") == 0) return TK_IF;
        if (strcmp(name, "inline") == 0) return TK_INLINE;
        if (strcmp(name, "int") == 0) return TK_INT;
        return 0;
    }
    if (c == 108) {  /* l */
        if (strcmp(name, "long") == 0) return TK_LONG;
        return 0;
    }
    if (c == 114) {  /* r */
        if (strcmp(name, "register") == 0) return TK_REGISTER;
        if (strcmp(name, "restrict") == 0) return TK_RESTRICT;
        if (strcmp(name, "return") == 0) return TK_RETURN;
        return 0;
    }
    if (c == 115) {  /* s */
        if (strcmp(name, "short") == 0) return TK_SHORT;
        if (strcmp(name, "signed") == 0) return TK_SIGNED;
        if (strcmp(name, "sizeof") == 0) return TK_SIZEOF;
        if (strcmp(name, "static") == 0) return TK_STATIC;
        if (strcmp(name, "struct") == 0) return TK_STRUCT;
        if (strcmp(name, "switch") == 0) return TK_SWITCH;
        return 0;
    }
    if (c == 116) {  /* t */
        if (strcmp(name, "typedef") == 0) return TK_TYPEDEF;
        return 0;
    }
    if (c == 117) {  /* u */
        if (strcmp(name, "union") == 0) return TK_UNION;
        if (strcmp(name, "unsigned") == 0) return TK_UNSIGNED;
        return 0;
    }
    if (c == 118) {  /* v */
        if (strcmp(name, "void") == 0) return TK_VOID;
        if (strcmp(name, "volatile") == 0) return TK_VOLATILE;
        return 0;
    }
    if (c == 119) {  /* w */
        if (strcmp(name, "while") == 0) return TK_WHILE;
        return 0;
    }
    return 0;
}

/* ========================================================
 * Character classification helpers
 * ======================================================== */

int lex_is_space(int ch) {
    if (ch == 32) return 1;   /* space */
    if (ch == 9) return 1;    /* tab */
    if (ch == 10) return 1;   /* newline */
    if (ch == 13) return 1;   /* carriage return */
    if (ch == 12) return 1;   /* form feed */
    if (ch == 11) return 1;   /* vertical tab */
    return 0;
}

int lex_is_alpha(int ch) {
    if (ch >= 97 && ch <= 122) return 1;  /* a-z */
    if (ch >= 65 && ch <= 90)  return 1;  /* A-Z */
    if (ch == 95) return 1;               /* _ */
    return 0;
}

int lex_is_digit(int ch) {
    if (ch >= 48 && ch <= 57) return 1;  /* 0-9 */
    return 0;
}

int lex_is_hex(int ch) {
    if (ch >= 48 && ch <= 57) return 1;   /* 0-9 */
    if (ch >= 97 && ch <= 102) return 1;  /* a-f */
    if (ch >= 65 && ch <= 70)  return 1;  /* A-F */
    return 0;
}

int lex_is_octal(int ch) {
    if (ch >= 48 && ch <= 55) return 1;  /* 0-7 */
    return 0;
}

int lex_is_alnum(int ch) {
    if (lex_is_alpha(ch)) return 1;
    if (lex_is_digit(ch)) return 1;
    return 0;
}

/* ========================================================
 * Lexer core
 * ======================================================== */

void lex_init(char *src, int len) {
    int i;

    /* Copy source into buffer */
    i = 0;
    while (i < len && i < LEX_SRC_SZ - 1) {
        lex_src[i] = src[i];
        i = i + 1;
    }
    lex_src[i] = 0;
    lex_len = i;
    lex_pos = 0;
    lex_line = 1;
    lex_col = 1;
    lex_tok = TK_EOF;
    lex_val = 0;
    lex_slen = 0;
    lex_str[0] = 0;

    lex_strpool_len = 0;
    lex_str_count = 0;
}

int lex_peek(void) {
    if (lex_pos >= lex_len) return -1;
    return lex_src[lex_pos] & 255;
}

int lex_peek_at(int offset) {
    int p;
    p = lex_pos + offset;
    if (p >= lex_len) return -1;
    return lex_src[p] & 255;
}

int lex_advance(void) {
    int ch;
    if (lex_pos >= lex_len) return -1;
    ch = lex_src[lex_pos] & 255;
    lex_pos = lex_pos + 1;
    if (ch == 10) {
        lex_line = lex_line + 1;
        lex_col = 1;
    } else {
        lex_col = lex_col + 1;
    }
    return ch;
}

void lex_skip_ws(void) {
    int ch;
    int ch2;

    while (lex_pos < lex_len) {
        ch = lex_src[lex_pos] & 255;

        /* Whitespace */
        if (lex_is_space(ch)) {
            lex_advance();
            continue;
        }

        /* Comments */
        if (ch == 47 && lex_pos + 1 < lex_len) {
            ch2 = lex_src[lex_pos + 1] & 255;

            /* // line comment */
            if (ch2 == 47) {
                lex_advance();
                lex_advance();
                while (lex_pos < lex_len) {
                    ch = lex_src[lex_pos] & 255;
                    if (ch == 10) break;
                    lex_advance();
                }
                continue;
            }

            /* block comment */
            if (ch2 == 42) {
                lex_advance();
                lex_advance();
                while (lex_pos < lex_len) {
                    ch = lex_src[lex_pos] & 255;
                    if (ch == 42 && lex_pos + 1 < lex_len) {
                        ch2 = lex_src[lex_pos + 1] & 255;
                        if (ch2 == 47) {
                            lex_advance();
                            lex_advance();
                            break;
                        }
                    }
                    lex_advance();
                }
                continue;
            }
        }

        /* Not whitespace or comment */
        break;
    }
}

/* Scan escape sequence after backslash. Returns char value. */
int lex_scan_escape(void) {
    int ch;
    int val;
    int i;

    ch = lex_advance();
    if (ch < 0) return 0;

    if (ch == 110) return 10;   /* \n */
    if (ch == 116) return 9;    /* \t */
    if (ch == 114) return 13;   /* \r */
    if (ch == 48) {             /* \0 — could be octal */
        /* Check if next char is octal digit */
        ch = lex_peek();
        if (ch >= 49 && ch <= 55) {
            /* Octal escape: \0nn */
            val = 0;
            i = 0;
            while (i < 3 && lex_is_octal(lex_peek())) {
                val = val * 8 + (lex_advance() - 48);
                i = i + 1;
            }
            return val;
        }
        return 0;  /* just \0 */
    }
    if (ch == 97)  return 7;    /* \a */
    if (ch == 98)  return 8;    /* \b */
    if (ch == 102) return 12;   /* \f */
    if (ch == 118) return 11;   /* \v */
    if (ch == 92)  return 92;   /* \\ */
    if (ch == 39)  return 39;   /* \' */
    if (ch == 34)  return 34;   /* \" */

    if (ch == 120) {            /* \x hex */
        val = 0;
        i = 0;
        while (i < 2 && lex_is_hex(lex_peek())) {
            ch = lex_advance();
            if (ch >= 48 && ch <= 57) {
                val = val * 16 + (ch - 48);
            } else if (ch >= 97 && ch <= 102) {
                val = val * 16 + (ch - 97 + 10);
            } else if (ch >= 65 && ch <= 70) {
                val = val * 16 + (ch - 65 + 10);
            }
            i = i + 1;
        }
        return val;
    }

    /* Octal escape: \1-7 followed by up to 2 more octal digits */
    if (ch >= 49 && ch <= 55) {
        val = ch - 48;
        i = 0;
        while (i < 2 && lex_is_octal(lex_peek())) {
            val = val * 8 + (lex_advance() - 48);
            i = i + 1;
        }
        return val;
    }

    /* Unknown escape — return literal char */
    return ch;
}

/* Scan identifier or keyword */
void lex_scan_ident(void) {
    int ch;
    int kw;

    lex_slen = 0;
    while (lex_pos < lex_len) {
        ch = lex_src[lex_pos] & 255;
        if (!lex_is_alnum(ch)) break;
        if (lex_slen < LEX_STR_SZ - 1) {
            lex_str[lex_slen] = ch;
            lex_slen = lex_slen + 1;
        }
        lex_advance();
    }
    lex_str[lex_slen] = 0;

    /* Check if it's a keyword */
    kw = lex_kw_lookup(lex_str);
    if (kw != 0) {
        lex_tok = kw;
    } else {
        lex_tok = TK_IDENT;
    }
}

/* Scan numeric literal (decimal, hex, octal) */
void lex_scan_number(void) {
    int ch;
    int val;
    int ch2;

    val = 0;
    ch = lex_src[lex_pos] & 255;

    if (ch == 48) {  /* starts with 0 */
        lex_advance();
        ch2 = lex_peek();

        if (ch2 == 120 || ch2 == 88) {  /* 0x or 0X */
            lex_advance();
            while (lex_is_hex(lex_peek())) {
                ch = lex_advance();
                if (ch >= 48 && ch <= 57) {
                    val = val * 16 + (ch - 48);
                } else if (ch >= 97 && ch <= 102) {
                    val = val * 16 + (ch - 97 + 10);
                } else if (ch >= 65 && ch <= 70) {
                    val = val * 16 + (ch - 65 + 10);
                }
            }
        } else if (lex_is_octal(ch2)) {  /* Octal */
            while (lex_is_octal(lex_peek())) {
                ch = lex_advance();
                val = val * 8 + (ch - 48);
            }
        }
        /* else just 0 */
    } else {
        /* Decimal */
        while (lex_is_digit(lex_peek())) {
            ch = lex_advance();
            val = val * 10 + (ch - 48);
        }
    }

    /* Skip optional suffixes: u/U, l/L, combinations */
    ch = lex_peek();
    if (ch == 117 || ch == 85) {  /* u/U */
        lex_advance();
        ch = lex_peek();
    }
    if (ch == 108 || ch == 76) {  /* l/L */
        lex_advance();
        ch = lex_peek();
        if (ch == 108 || ch == 76) {  /* ll/LL */
            lex_advance();
        }
    }
    /* Check for trailing U after L */
    ch = lex_peek();
    if (ch == 117 || ch == 85) {  /* u/U */
        lex_advance();
    }

    lex_tok = TK_NUM;
    lex_val = val;
}

/* Scan string literal (opening " already peeked but not consumed) */
void lex_scan_string(void) {
    int ch;
    int pool_start;
    int slen;

    lex_advance(); /* consume opening " */

    pool_start = lex_strpool_len;
    slen = 0;

    while (lex_pos < lex_len) {
        ch = lex_src[lex_pos] & 255;
        if (ch == 34) {  /* closing " */
            lex_advance();
            break;
        }
        if (ch == 10) {  /* newline in string — error, just stop */
            break;
        }
        if (ch == 92) {  /* backslash escape */
            lex_advance();
            ch = lex_scan_escape();
        } else {
            ch = lex_advance();
        }

        /* Store in string pool */
        if (lex_strpool_len < LEX_POOL_SZ - 1) {
            lex_strpool[lex_strpool_len] = ch;
            lex_strpool_len = lex_strpool_len + 1;
        }
        /* Also in lex_str for convenience */
        if (slen < LEX_STR_SZ - 1) {
            lex_str[slen] = ch;
            slen = slen + 1;
        }
    }

    /* NUL-terminate pool entry */
    if (lex_strpool_len < LEX_POOL_SZ) {
        lex_strpool[lex_strpool_len] = 0;
        lex_strpool_len = lex_strpool_len + 1;
    }
    lex_str[slen] = 0;
    lex_slen = slen;

    /* Record in string table */
    if (lex_str_count < LEX_POOL_MAX) {
        lex_str_off[lex_str_count] = pool_start;
        lex_str_len[lex_str_count] = slen;
        lex_val = lex_str_count;
        lex_str_count = lex_str_count + 1;
    }

    lex_tok = TK_STRING;
}

/* Scan character literal (opening ' already peeked but not consumed) */
void lex_scan_char(void) {
    int ch;
    int val;

    lex_advance(); /* consume opening ' */

    ch = lex_peek();
    if (ch == 92) {  /* backslash escape */
        lex_advance();
        val = lex_scan_escape();
    } else {
        val = lex_advance();
    }

    /* Consume closing ' */
    ch = lex_peek();
    if (ch == 39) {
        lex_advance();
    }

    lex_tok = TK_CHARLIT;
    lex_val = val;
}

/* ========================================================
 * Main lexer: lex_next()
 * ======================================================== */

void lex_next(void) {
    int ch;
    int ch2;
    int ch3;

    lex_skip_ws();

    if (lex_pos >= lex_len) {
        lex_tok = TK_EOF;
        return;
    }

    ch = lex_src[lex_pos] & 255;

    /* Identifier or keyword */
    if (lex_is_alpha(ch)) {
        lex_scan_ident();
        return;
    }

    /* Numeric literal */
    if (lex_is_digit(ch)) {
        lex_scan_number();
        return;
    }

    /* String literal */
    if (ch == 34) {
        lex_scan_string();
        return;
    }

    /* Character literal */
    if (ch == 39) {
        lex_scan_char();
        return;
    }

    /* Operators and punctuation */

    /* ( */
    if (ch == 40) {
        lex_advance();
        lex_tok = TK_LPAREN;
        return;
    }
    /* ) */
    if (ch == 41) {
        lex_advance();
        lex_tok = TK_RPAREN;
        return;
    }
    /* [ */
    if (ch == 91) {
        lex_advance();
        lex_tok = TK_LBRACK;
        return;
    }
    /* ] */
    if (ch == 93) {
        lex_advance();
        lex_tok = TK_RBRACK;
        return;
    }
    /* { */
    if (ch == 123) {
        lex_advance();
        lex_tok = TK_LBRACE;
        return;
    }
    /* } */
    if (ch == 125) {
        lex_advance();
        lex_tok = TK_RBRACE;
        return;
    }
    /* ; */
    if (ch == 59) {
        lex_advance();
        lex_tok = TK_SEMI;
        return;
    }
    /* , */
    if (ch == 44) {
        lex_advance();
        lex_tok = TK_COMMA;
        return;
    }
    /* ~ */
    if (ch == 126) {
        lex_advance();
        lex_tok = TK_TILDE;
        return;
    }
    /* ? */
    if (ch == 63) {
        lex_advance();
        lex_tok = TK_QMARK;
        return;
    }
    /* : */
    if (ch == 58) {
        lex_advance();
        lex_tok = TK_COLON;
        return;
    }

    /* . or ... */
    if (ch == 46) {
        ch2 = lex_peek_at(1);
        ch3 = lex_peek_at(2);
        if (ch2 == 46 && ch3 == 46) {
            lex_advance();
            lex_advance();
            lex_advance();
            lex_tok = TK_ELLIPSIS;
            return;
        }
        lex_advance();
        lex_tok = TK_DOT;
        return;
    }

    /* + += ++ */
    if (ch == 43) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_PLUSEQ;
            return;
        }
        if (ch2 == 43) {
            lex_advance();
            lex_tok = TK_INC;
            return;
        }
        lex_tok = TK_PLUS;
        return;
    }

    /* - -= -- -> */
    if (ch == 45) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_MINUSEQ;
            return;
        }
        if (ch2 == 45) {
            lex_advance();
            lex_tok = TK_DEC;
            return;
        }
        if (ch2 == 62) {
            lex_advance();
            lex_tok = TK_ARROW;
            return;
        }
        lex_tok = TK_MINUS;
        return;
    }

    /* * *= */
    if (ch == 42) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_STAREQ;
            return;
        }
        lex_tok = TK_STAR;
        return;
    }

    /* / /= (comments already handled in skip_ws) */
    if (ch == 47) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_SLASHEQ;
            return;
        }
        lex_tok = TK_SLASH;
        return;
    }

    /* % %= */
    if (ch == 37) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_PERCENTEQ;
            return;
        }
        lex_tok = TK_PERCENT;
        return;
    }

    /* & &= && */
    if (ch == 38) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_AMPEQ;
            return;
        }
        if (ch2 == 38) {
            lex_advance();
            lex_tok = TK_LAND;
            return;
        }
        lex_tok = TK_AMP;
        return;
    }

    /* | |= || */
    if (ch == 124) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_PIPEEQ;
            return;
        }
        if (ch2 == 124) {
            lex_advance();
            lex_tok = TK_LOR;
            return;
        }
        lex_tok = TK_PIPE;
        return;
    }

    /* ^ ^= */
    if (ch == 94) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_CARETEQ;
            return;
        }
        lex_tok = TK_CARET;
        return;
    }

    /* ! != */
    if (ch == 33) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_NE;
            return;
        }
        lex_tok = TK_BANG;
        return;
    }

    /* = == */
    if (ch == 61) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_EQ;
            return;
        }
        lex_tok = TK_ASSIGN;
        return;
    }

    /* < <= << <<= */
    if (ch == 60) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_LE;
            return;
        }
        if (ch2 == 60) {
            lex_advance();
            ch3 = lex_peek();
            if (ch3 == 61) {
                lex_advance();
                lex_tok = TK_LSHIFTEQ;
                return;
            }
            lex_tok = TK_LSHIFT;
            return;
        }
        lex_tok = TK_LT;
        return;
    }

    /* > >= >> >>= */
    if (ch == 62) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 61) {
            lex_advance();
            lex_tok = TK_GE;
            return;
        }
        if (ch2 == 62) {
            lex_advance();
            ch3 = lex_peek();
            if (ch3 == 61) {
                lex_advance();
                lex_tok = TK_RSHIFTEQ;
                return;
            }
            lex_tok = TK_RSHIFT;
            return;
        }
        lex_tok = TK_GT;
        return;
    }

    /* # ## */
    if (ch == 35) {
        lex_advance();
        ch2 = lex_peek();
        if (ch2 == 35) {
            lex_advance();
            lex_tok = TK_HASHHASH;
            return;
        }
        lex_tok = TK_HASH;
        return;
    }

    /* Unknown character — skip it and return EOF-like */
    lex_advance();
    lex_tok = TK_EOF;
}
