/* extras.c — extra libc bits needed by tools/dbt sources.
 *
 *   - qsort: textbook recursive quicksort over arbitrary record sizes.
 *   - system: minimal stub (returns -1).  dbt only uses system() for
 *     optional objdump diagnostics; treating it as unavailable is fine.
 *   - __builtin_popcount: gcc auto-inlines this; cc-x64 does not, so
 *     provide a real function.
 *   - snprintf: fixed-buffer subset that handles the format specifiers
 *     dbt uses for stats/dump output (%d/%u/%x/%s/%c with width/zero-
 *     pad and an %llu/%lld for 64-bit counters). */

int strlen(char *s);
char *memcpy(char *dst, char *src, int n);
int write(int fd, char *buf, int len);
long __syscall();

/* ---- memmove: like memcpy but safe for overlap ---- */
char *memmove(char *dst, char *src, int n) {
    int i;
    if (dst == src || n <= 0) return dst;
    if (dst < src) {
        i = 0;
        while (i < n) { dst[i] = src[i]; i = i + 1; }
    } else {
        i = n;
        while (i > 0) { i = i - 1; dst[i] = src[i]; }
    }
    return dst;
}

/* ---- memchr ---- */
char *memchr(char *s, int c, int n) {
    int i;
    unsigned char ch;
    ch = (unsigned char)c;
    i = 0;
    while (i < n) {
        if ((unsigned char)s[i] == ch) return s + i;
        i = i + 1;
    }
    return 0;
}

/* ---- strtoul: like strtol but unsigned ---- */
unsigned long strtoul(char *s, char **endp, int base) {
    unsigned long v;
    int neg;
    int c;
    int d;
    /* Skip whitespace */
    while (*s == ' ' || *s == '\t' || *s == '\n') s = s + 1;
    neg = 0;
    if (*s == '+') s = s + 1;
    else if (*s == '-') { neg = 1; s = s + 1; }
    /* Auto-detect base */
    if (base == 0) {
        if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
            base = 16; s = s + 2;
        } else if (s[0] == '0') {
            base = 8; s = s + 1;
        } else {
            base = 10;
        }
    } else if (base == 16 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        s = s + 2;
    }
    v = 0;
    while (*s) {
        c = *s;
        if (c >= '0' && c <= '9') d = c - '0';
        else if (c >= 'a' && c <= 'z') d = c - 'a' + 10;
        else if (c >= 'A' && c <= 'Z') d = c - 'A' + 10;
        else break;
        if (d >= base) break;
        v = v * base + d;
        s = s + 1;
    }
    if (endp) *endp = s;
    if (neg) return 0UL - v;
    return v;
}

/* ---- mmap / munmap / clock_gettime — direct syscalls ---- */

void *mmap(void *addr, unsigned long len, int prot, int flags, int fd,
           long offset) {
    return (void *)(long)__syscall(9, addr, len, prot, flags, fd, offset);
}

int munmap(void *addr, unsigned long len) {
    return __syscall(11, addr, len, 0, 0, 0, 0);
}

int clock_gettime(int clk_id, void *ts) {
    return __syscall(228, clk_id, ts, 0, 0, 0, 0);
}

int mprotect(void *addr, unsigned long len, int prot) {
    return __syscall(10, addr, len, prot, 0, 0, 0);
}

/* Minimal sysconf — only _SC_PAGESIZE (= 30 in Linux) is interesting to dbt. */
long sysconf(int name) {
    if (name == 30) return 4096;
    return -1;
}

/* setitimer / signal / sigaction / sigemptyset — stubs.  dbt's
 * probe-timer and SIGSEGV-handler features are optional; returning
 * failure cleanly disables them without affecting the hot path. */
int setitimer(int which, void *new_val, void *old_val) { return -1; }
void *signal(int sig, void *handler) { return (void *)(long)-1; }
int sigaction(int sig, void *act, void *oldact) { return -1; }
int sigemptyset(void *set) {
    /* signal.h defines sigset_t as `unsigned long` (8 bytes). The real
     * Linux kernel sigset_t is 128 bytes, but our sigaction stub is a
     * no-op so the kernel layout doesn't matter — only the in-process
     * struct layout does. Writing 128 bytes here overflows the
     * `struct sigaction` local on the stack and corrupts the return
     * address. Match the cc-x64 sigset_t size instead. */
    if (set) *(long *)set = 0;
    return 0;
}

/* _exit — direct syscall (no atexit handlers, no stdio flush). */
void _exit(int status) {
    __syscall(231, status, 0, 0, 0, 0, 0);  /* exit_group */
    __syscall(60, status, 0, 0, 0, 0, 0);   /* fallback: exit */
    while (1) { /* unreachable */ }
}

/* IEEE-754 classification — bit-pattern checks via memcpy.  Avoids any
 * dependence on libm and works under cc-x64's f64-as-(lo,hi) lowering. */
int isnan(double x) {
    unsigned long long u;
    unsigned int hi;
    unsigned int lo;
    memcpy((char *)&u, (char *)&x, 8);
    hi = (unsigned int)(u >> 32);
    lo = (unsigned int)u;
    /* exponent all 1s AND mantissa nonzero */
    if ((hi & 0x7FF00000u) != 0x7FF00000u) return 0;
    if ((hi & 0x000FFFFFu) != 0) return 1;
    if (lo != 0) return 1;
    return 0;
}

int isinf(double x) {
    unsigned long long u;
    unsigned int hi;
    unsigned int lo;
    memcpy((char *)&u, (char *)&x, 8);
    hi = (unsigned int)(u >> 32);
    lo = (unsigned int)u;
    if ((hi & 0x7FF00000u) != 0x7FF00000u) return 0;
    if ((hi & 0x000FFFFFu) != 0) return 0;
    if (lo != 0) return 0;
    return 1;
}

int isfinite(double x) {
    unsigned long long u;
    unsigned int hi;
    memcpy((char *)&u, (char *)&x, 8);
    hi = (unsigned int)(u >> 32);
    return (hi & 0x7FF00000u) != 0x7FF00000u;
}

/* strtoull — same shape as strtoul but for long long. */
unsigned long long strtoull(char *s, char **endp, int base) {
    unsigned long long v;
    int neg;
    int c;
    int d;
    while (*s == ' ' || *s == '\t' || *s == '\n') s = s + 1;
    neg = 0;
    if (*s == '+') s = s + 1;
    else if (*s == '-') { neg = 1; s = s + 1; }
    if (base == 0) {
        if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
            base = 16; s = s + 2;
        } else if (s[0] == '0') {
            base = 8; s = s + 1;
        } else {
            base = 10;
        }
    } else if (base == 16 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        s = s + 2;
    }
    v = 0ULL;
    while (*s) {
        c = *s;
        if (c >= '0' && c <= '9') d = c - '0';
        else if (c >= 'a' && c <= 'z') d = c - 'a' + 10;
        else if (c >= 'A' && c <= 'Z') d = c - 'A' + 10;
        else break;
        if (d >= base) break;
        v = v * (unsigned long long)base + (unsigned long long)d;
        s = s + 1;
    }
    if (endp) *endp = s;
    if (neg) return 0ULL - v;
    return v;
}

/* strtok_r — thread-safe tokenizer.  *saveptr tracks state; pass NULL
 * as `s` for subsequent calls. */
char *strtok_r(char *s, char *delim, char **saveptr) {
    char *start;
    char *p;
    char *d;
    int matched;
    if (s == 0) s = *saveptr;
    if (s == 0) return 0;
    /* Skip leading delimiters */
    while (*s) {
        d = delim;
        matched = 0;
        while (*d) { if (*s == *d) { matched = 1; break; } d = d + 1; }
        if (!matched) break;
        s = s + 1;
    }
    if (*s == 0) { *saveptr = 0; return 0; }
    start = s;
    /* Find next delimiter */
    p = s;
    while (*p) {
        d = delim;
        matched = 0;
        while (*d) { if (*p == *d) { matched = 1; break; } d = d + 1; }
        if (matched) break;
        p = p + 1;
    }
    if (*p == 0) {
        *saveptr = 0;
    } else {
        *p = 0;
        *saveptr = p + 1;
    }
    return start;
}

/* ---- qsort: recursive quicksort, median-of-three pivot ---- */

static void qs_swap(char *a, char *b, int sz) {
    int i; char tmp;
    i = 0;
    while (i < sz) {
        tmp = a[i];
        a[i] = b[i];
        b[i] = tmp;
        i = i + 1;
    }
}

static void qsort_inner(char *base, int n, int sz,
                        int (*cmp)(char *, char *)) {
    char *piv;
    int lo; int hi;
    int mid;
    if (n < 2) return;
    /* Median-of-three pivot */
    mid = n / 2;
    if (cmp(base + 0 * sz, base + mid * sz) > 0)
        qs_swap(base + 0 * sz, base + mid * sz, sz);
    if (cmp(base + 0 * sz, base + (n - 1) * sz) > 0)
        qs_swap(base + 0 * sz, base + (n - 1) * sz, sz);
    if (cmp(base + mid * sz, base + (n - 1) * sz) > 0)
        qs_swap(base + mid * sz, base + (n - 1) * sz, sz);
    /* Move pivot to end-1 */
    qs_swap(base + mid * sz, base + (n - 2) * sz, sz);
    piv = base + (n - 2) * sz;
    /* Partition */
    lo = 0;
    hi = n - 2;
    while (1) {
        do { lo = lo + 1; } while (cmp(base + lo * sz, piv) < 0);
        do { hi = hi - 1; } while (hi >= 0 && cmp(base + hi * sz, piv) > 0);
        if (lo >= hi) break;
        qs_swap(base + lo * sz, base + hi * sz, sz);
    }
    qs_swap(base + lo * sz, piv, sz);
    qsort_inner(base, lo, sz, cmp);
    qsort_inner(base + (lo + 1) * sz, n - lo - 1, sz, cmp);
}

void qsort(char *base, int nmemb, int sz,
           int (*cmp)(char *, char *)) {
    qsort_inner(base, nmemb, sz, cmp);
}

/* ---- system: stub ---- */

int system(char *cmd) {
    /* dbt only invokes system() for optional diagnostic shell-outs
     * (e.g. piping to objdump for human-readable disassembly).  Returning
     * -1 reports "command processor unavailable" without crashing. */
    return -1;
}

/* ---- __builtin_popcount ---- */

int __builtin_popcount(unsigned int x) {
    int n;
    n = 0;
    while (x) {
        n = n + (x & 1);
        x = x >> 1;
    }
    return n;
}

/* ---- snprintf: minimal subset ----
 *
 * Supports: %d, %u, %x, %X, %s, %c, %p, %lld, %llu, with optional
 *   - leading '0' for zero-fill
 *   - decimal width
 *   - '%%' escape
 * Anything else falls through as a literal char.  Always NUL-terminates
 * the buffer (assuming size > 0).  Returns the number of bytes that
 * WOULD have been written (excluding NUL), matching the C99 contract. */

static int sn_emit(char *buf, int sz, int pos, int c) {
    if (pos + 1 < sz) buf[pos] = c;
    return pos + 1;
}

static int sn_putuint(char *buf, int sz, int pos, unsigned int v,
                      int base, int width, int zero, int upper) {
    char tmp[16];
    int n; int i;
    char *digits;
    digits = upper ? "0123456789ABCDEF" : "0123456789abcdef";
    n = 0;
    if (v == 0) { tmp[0] = '0'; n = 1; }
    while (v != 0 && n < 16) {
        tmp[n] = digits[v % base];
        v = v / base;
        n = n + 1;
    }
    while (n < width) {
        pos = sn_emit(buf, sz, pos, zero ? '0' : ' ');
        n = n + 1;
        width = width - 1;
    }
    while (n > 0) {
        n = n - 1;
        pos = sn_emit(buf, sz, pos, tmp[n]);
    }
    return pos;
}

static int sn_putint(char *buf, int sz, int pos, int v,
                     int width, int zero) {
    unsigned int u;
    if (v < 0) {
        pos = sn_emit(buf, sz, pos, '-');
        u = (unsigned int)(-v);
        if (width > 0) width = width - 1;
    } else {
        u = (unsigned int)v;
    }
    return sn_putuint(buf, sz, pos, u, 10, width, zero, 0);
}

/* 64-bit helpers — single divide loop for %lld/%llu */
static int sn_putu64(char *buf, int sz, int pos, unsigned long long v,
                     int width, int zero) {
    char tmp[24];
    int n;
    n = 0;
    if (v == 0) { tmp[0] = '0'; n = 1; }
    while (v != 0 && n < 24) {
        tmp[n] = '0' + (int)(v % 10ULL);
        v = v / 10ULL;
        n = n + 1;
    }
    while (n < width) {
        pos = sn_emit(buf, sz, pos, zero ? '0' : ' ');
        n = n + 1;
        width = width - 1;
    }
    while (n > 0) {
        n = n - 1;
        pos = sn_emit(buf, sz, pos, tmp[n]);
    }
    return pos;
}

static int sn_putl64(char *buf, int sz, int pos, long long v,
                     int width, int zero) {
    unsigned long long u;
    if (v < 0) {
        pos = sn_emit(buf, sz, pos, '-');
        u = (unsigned long long)(-v);
        if (width > 0) width = width - 1;
    } else {
        u = (unsigned long long)v;
    }
    return sn_putu64(buf, sz, pos, u, width, zero);
}

int vsnprintf(char *buf, int sz, char *fmt, char *ap);

int snprintf(char *buf, int sz, char *fmt, ...) {
    char *ap;
    int r;
    /* SysV varargs: skip the four named args (buf, sz, fmt) — args after
     * fmt start at ap = &fmt + 1.  cc-x64 lays variadics out so &fmt + 1
     * gives a contiguous spill area. */
    ap = (char *)(&fmt) + 8;
    r = vsnprintf(buf, sz, fmt, ap);
    return r;
}

int vsnprintf(char *buf, int sz, char *fmt, char *ap) {
    int pos;
    int i;
    int width;
    int zero;
    int is_ll;
    int v;
    unsigned int uv;
    long long lv;
    unsigned long long ulv;
    char *s;

    pos = 0;
    i = 0;
    while (fmt[i] != 0) {
        if (fmt[i] != '%') {
            pos = sn_emit(buf, sz, pos, fmt[i]);
            i = i + 1;
            continue;
        }
        i = i + 1;  /* skip '%' */
        zero = 0;
        width = 0;
        is_ll = 0;
        if (fmt[i] == '0') { zero = 1; i = i + 1; }
        while (fmt[i] >= '0' && fmt[i] <= '9') {
            width = width * 10 + (fmt[i] - '0');
            i = i + 1;
        }
        if (fmt[i] == 'l') {
            i = i + 1;
            if (fmt[i] == 'l') { is_ll = 1; i = i + 1; }
        } else if (fmt[i] == 'z') {
            i = i + 1;  /* size_t — same width as int on this build */
        }
        if (fmt[i] == 'd' || fmt[i] == 'i') {
            if (is_ll) {
                lv = *(long long *)ap;
                ap = ap + 8;
                pos = sn_putl64(buf, sz, pos, lv, width, zero);
            } else {
                v = *(int *)ap;
                ap = ap + 8;
                pos = sn_putint(buf, sz, pos, v, width, zero);
            }
        } else if (fmt[i] == 'u') {
            if (is_ll) {
                ulv = *(unsigned long long *)ap;
                ap = ap + 8;
                pos = sn_putu64(buf, sz, pos, ulv, width, zero);
            } else {
                uv = *(unsigned int *)ap;
                ap = ap + 8;
                pos = sn_putuint(buf, sz, pos, uv, 10, width, zero, 0);
            }
        } else if (fmt[i] == 'x') {
            uv = *(unsigned int *)ap;
            ap = ap + 8;
            pos = sn_putuint(buf, sz, pos, uv, 16, width, zero, 0);
        } else if (fmt[i] == 'X') {
            uv = *(unsigned int *)ap;
            ap = ap + 8;
            pos = sn_putuint(buf, sz, pos, uv, 16, width, zero, 1);
        } else if (fmt[i] == 's') {
            s = *(char **)ap;
            ap = ap + 8;
            if (s == 0) s = "(null)";
            while (*s) {
                pos = sn_emit(buf, sz, pos, *s);
                s = s + 1;
            }
        } else if (fmt[i] == 'c') {
            v = *(int *)ap;
            ap = ap + 8;
            pos = sn_emit(buf, sz, pos, v);
        } else if (fmt[i] == 'p') {
            ulv = *(unsigned long long *)ap;
            ap = ap + 8;
            pos = sn_emit(buf, sz, pos, '0');
            pos = sn_emit(buf, sz, pos, 'x');
            pos = sn_putu64(buf, sz, pos, ulv, 0, 0);
        } else if (fmt[i] == '%') {
            pos = sn_emit(buf, sz, pos, '%');
        } else {
            /* Unknown — emit literal */
            pos = sn_emit(buf, sz, pos, '%');
            pos = sn_emit(buf, sz, pos, fmt[i]);
        }
        i = i + 1;
    }
    if (sz > 0) {
        if (pos < sz) buf[pos] = 0;
        else buf[sz - 1] = 0;
    }
    return pos;
}
