/* lex_minimal.c -- Minimal test: just the globals + lex_init + stub lex_next.
 * Tests whether the generated lexer code up to (but not including)
 * the Ragel exec block compiles OK with s32-cc.
 */

/* Libc prototypes */
int strcmp(char *a, char *b);
int strlen(char *s);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);
int fputs(char *s, int f);
int fputc(int c, int f);
void fput_uint(int f, int v);
void exit(int status);

#define stderr 2
#define NULL 0

/* Token constants */
#define TK_EOF 0
#define TK_NUM 1
#define TK_IDENT 4
#define TK_INT 27

/* Globals */
#define LEX_SRC_SZ 262144
#define LEX_STR_SZ 256
#define LEX_POOL_SZ 65536
#define LEX_POOL_MAX 2048

static char lex_src[LEX_SRC_SZ];
static int lex_len;
static int lex_line;
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

/* Test: switch with goto labels before first case (Duff's device pattern) */
int test_switch_with_labels(int cs) {
    int r;
    char *p;
    char *pe;
    char *ts;
    char *te;
    int act;

    r = 0;
    p = lex_src;
    pe = lex_src + 10;
    ts = 0;
    te = 0;
    act = 0;

    switch (cs) {
    tr0:
        r = r + 1;
        goto _done;
    tr1:
        r = r + 2;
        goto _done;
    case 1:
        goto tr0;
    case 2:
        goto tr1;
    case 3:
        {ts = p;}
        switch (*p) {
            case 65: goto tr0;
            case 66: goto tr1;
        }
        goto _done;
    }
_done:
    return r;
}

int main(void) {
    int v;

    lex_src[0] = 65;  /* 'A' */

    v = test_switch_with_labels(1);
    if (v != 1) {
        fputs("FAIL: case1 expected 1 got ", stderr);
        fput_uint(stderr, v);
        fputc(10, stderr);
        return 1;
    }

    v = test_switch_with_labels(2);
    if (v != 2) {
        fputs("FAIL: case2 expected 2 got ", stderr);
        fput_uint(stderr, v);
        fputc(10, stderr);
        return 1;
    }

    v = test_switch_with_labels(3);
    if (v != 1) {
        fputs("FAIL: case3 expected 1 got ", stderr);
        fput_uint(stderr, v);
        fputc(10, stderr);
        return 1;
    }

    fputs("OK: switch+labels test passed\n", stderr);
    return 0;
}
