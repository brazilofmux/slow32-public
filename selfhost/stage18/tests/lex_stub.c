/* lex_stub.c -- Lexer globals + helpers + STUB lex_next (no Ragel exec block).
 * If this compiles: issue is in the Ragel exec block.
 * If this crashes: issue is in the helpers/globals.
 */

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

#define TK_EOF 0
#define TK_NUM 1
#define TK_STRING 2
#define TK_CHARLIT 3
#define TK_IDENT 4
#define TK_INT 27
#define TK_SEMI 56
#define TK_ASSIGN 90

#define LEX_SRC_SZ 262144
#define LEX_STR_SZ 256
#define LEX_POOL_SZ 65536
#define LEX_POOL_MAX 2048

static char lex_src[LEX_SRC_SZ];
static int lex_len;
static int lex_line;
static int lex_col;
static int lex_tok;
static int lex_val;
static char lex_str[LEX_STR_SZ];
static int lex_slen;
static int lex_pos;

static char lex_strpool[LEX_POOL_SZ];
static int lex_strpool_len;
static int lex_str_off[LEX_POOL_MAX];
static int lex_str_len[LEX_POOL_MAX];
static int lex_str_count;

static char *lex_rp;
static char *lex_rpe;
static int lex_rcs;
static int lex_ract;
static char *lex_rts;
static char *lex_rte;

int lex_kw_lookup(char *name) {
    if (strcmp(name, "int") == 0) return TK_INT;
    return 0;
}

static int lex_parse_esc(char *s, int *posout) {
    int ch; int pos;
    pos = *posout; ch = s[pos] & 255; pos = pos + 1;
    if (ch == 110) { *posout = pos; return 10; }
    if (ch == 48)  { *posout = pos; return 0; }
    *posout = pos; return ch;
}

static void lex_count_nl(char *from, char *to) {
    char *cp;
    cp = from;
    while (cp < to) {
        if (*cp == 10) lex_line = lex_line + 1;
        cp = cp + 1;
    }
}

static void lex_parse_num(char *ts, char *te) {
    int val; int ch; char *np;
    val = 0; np = ts;
    while (np < te) {
        ch = *np & 255;
        if (ch < 48 || ch > 57) break;
        val = val * 10 + (ch - 48);
        np = np + 1;
    }
    lex_tok = TK_NUM;
    lex_val = val;
}

static void lex_parse_str(char *ts, char *te) {
    lex_tok = TK_STRING;
    lex_val = 0;
}

static void lex_parse_chr(char *ts, char *te) {
    lex_tok = TK_CHARLIT;
    lex_val = ts[1] & 255;
}

static void lex_parse_id(char *ts, char *te) {
    int len; int kw;
    len = (int)(te - ts);
    if (len > LEX_STR_SZ - 1) len = LEX_STR_SZ - 1;
    memcpy(lex_str, ts, len);
    lex_str[len] = 0;
    lex_slen = len;
    kw = lex_kw_lookup(lex_str);
    if (kw != 0) lex_tok = kw;
    else lex_tok = TK_IDENT;
}

static const int c_lexer_start = 9;
static const int c_lexer_error = 0;
static const int c_lexer_en_main = 9;

void lex_init(char *src, int len) {
    int i;
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
    lex_rcs = c_lexer_start;
    lex_ract = 0;
    lex_rts = 0;
    lex_rte = 0;
}

/* STUB lex_next -- no Ragel, just return EOF */
void lex_next(void) {
    lex_tok = TK_EOF;
}

int main(void) {
    char *src;
    src = "int x = 42;";
    lex_init(src, strlen(src));
    lex_next();
    if (lex_tok != TK_EOF) {
        fputs("FAIL: stub should return EOF\n", stderr);
        return 1;
    }
    fputs("OK: stub lexer compiled fine\n", stderr);
    return 0;
}
