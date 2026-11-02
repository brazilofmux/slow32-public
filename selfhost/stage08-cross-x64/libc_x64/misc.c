/* misc.c — Miscellaneous libc functions */

int write(int fd, char *buf, int len);
void exit(int code);
int strlen(char *s);
int strncmp(char *a, char *b, int n);
char *malloc(int size);
char *memcpy(char *dst, char *src, int n);

/* ---- Environment ---- */

static char **saved_envp;
char **environ;     /* POSIX global; set in lockstep with saved_envp */

void file_sys_init(void);

void __save_envp(char **envp) {
    saved_envp = envp;
    environ = envp;
    /* Initialise stdio FILEs eagerly so that fprintf(stderr, ...) — which
     * binds the parameter before its body's lazy init — sees a non-NULL
     * stderr on the very first call. dbt's usage()/error paths hit this
     * before any printf would have triggered the lazy path. */
    file_sys_init();
}

char *getenv(char *name) {
    int nlen;
    char **ep;
    if (!saved_envp) return 0;
    nlen = strlen(name);
    ep = saved_envp;
    while (*ep) {
        if (strncmp(*ep, name, nlen) == 0 && (*ep)[nlen] == '=')
            return *ep + nlen + 1;
        ep = ep + 1;
    }
    return 0;
}

/* ---- Utility ---- */

void abort(void) {
    write(2, "abort\n", 6);
    exit(134);
}

int atoi(char *s) {
    int n;
    int neg;
    n = 0;
    neg = 0;
    while (*s == ' ' || *s == '\t' || *s == '\n') s = s + 1;
    if (*s == '-') { neg = 1; s = s + 1; }
    else if (*s == '+') s = s + 1;
    while (*s >= '0' && *s <= '9') {
        n = n * 10 + (*s - '0');
        s = s + 1;
    }
    if (neg) return 0 - n;
    return n;
}

int abs(int x) {
    if (x < 0) return 0 - x;
    return x;
}

/* ---- Character classification ---- */

int isspace(int c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

int isdigit(int c) {
    return c >= '0' && c <= '9';
}

int isalpha(int c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

int toupper(int c) {
    if (c >= 'a' && c <= 'z') return c - 32;
    return c;
}

int tolower(int c) {
    if (c >= 'A' && c <= 'Z') return c + 32;
    return c;
}

/* __assert_fail — what <assert.h>'s assert() calls on a false condition.
 * tools/dbt/block_cache.c asserts; the self-hosted DBT would not link
 * without it (GitHub issue 82).  "assertion failed: EXPR (FILE:LINE)"
 * on fd 2, then abort(). */
static void af_puts(char *s) {
    int n = 0;
    while (s[n]) n = n + 1;
    write(2, s, n);
}
void __assert_fail(char *expr, char *file, int line) {
    char digits[16];
    int i = 15;
    digits[i] = 0;
    if (line == 0) { i = i - 1; digits[i] = 48; }
    while (line > 0) {
        i = i - 1;
        digits[i] = 48 + line % 10;
        line = line / 10;
    }
    af_puts("assertion failed: ");
    af_puts(expr);
    af_puts(" (");
    af_puts(file);
    af_puts(":");
    af_puts(digits + i);
    af_puts(")\n");
    abort();
}

/* What the self-hosted <assert.h> and <math.h> call (C library clause 7,
 * 2026-11): assert() reports through __s32_assert_func, fpclassify and
 * the classification macros through __fpclassify/__fpclassifyf, so the
 * cross DBTs -- tools/dbt built by this compiler -- need them here.
 * Values as math.h numbers them: FP_NAN 0, FP_INFINITE 1, FP_ZERO 2,
 * FP_SUBNORMAL 3, FP_NORMAL 4. */
void __s32_assert_func(char *expr, char *file, int line, char *func) {
    (void)func;
    __assert_fail(expr, file, line);
}

int __fpclassify(double x) {
    unsigned int w[2];
    unsigned int hi;
    unsigned int lo;
    unsigned int e;
    memcpy((char *)w, (char *)&x, 8);
    lo = w[0];
    hi = w[1];
    e = (hi >> 20) & 0x7FFu;
    if (e == 0) return ((hi & 0x7FFFFFFFu) | lo) ? 3 : 2;
    if (e == 0x7FFu) return ((hi & 0x000FFFFFu) | lo) ? 0 : 1;
    return 4;
}

int __fpclassifyf(float x) {
    unsigned int u;
    unsigned int e;
    memcpy((char *)&u, (char *)&x, 4);
    e = (u >> 23) & 0xFFu;
    if (e == 0) return (u & 0x7FFFFFFFu) ? 3 : 2;
    if (e == 0xFFu) return (u & 0x007FFFFFu) ? 0 : 1;
    return 4;
}
