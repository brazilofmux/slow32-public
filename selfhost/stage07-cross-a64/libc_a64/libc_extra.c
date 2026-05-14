/* libc_extra.c — additional libc functions used by tools/dbt.
 *
 * Naming kept generic so this file can grow as more callers land.
 * Most math functions here are *symbol-only* stubs: dbt.c uses them
 * as host pointers in its math intercept table but never actually
 * calls them on the AArch64 self-host (the table feeds into trampoline
 * generation that runs in JIT'd guest code; intercepts that fire would
 * be a runtime issue separate from getting things to link).
 */

int strlen(char *s);
char *malloc(int size);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);

/* ============================================================================
 * Memory functions
 * ============================================================================ */

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

char *memchr(char *s, int c, int n) {
    int i;
    unsigned char target;
    target = (unsigned char)c;
    i = 0;
    while (i < n) {
        if ((unsigned char)s[i] == target) return s + i;
        i = i + 1;
    }
    return 0;
}

/* ============================================================================
 * String → integer
 *
 * Subset of strtoul/strtoull: handles base 0 (auto: 0x → 16, 0 → 8, else 10),
 * base 8/10/16. Skips leading whitespace and optional sign. `endptr` records
 * where parsing stopped if non-NULL.
 * ============================================================================ */

static int parse_digit(int c, int base) {
    int v;
    if (c >= '0' && c <= '9') v = c - '0';
    else if (c >= 'a' && c <= 'z') v = c - 'a' + 10;
    else if (c >= 'A' && c <= 'Z') v = c - 'A' + 10;
    else return -1;
    if (v >= base) return -1;
    return v;
}

unsigned long strtoul(char *s, char **endptr, int base) {
    unsigned long v;
    int neg;
    int d;

    while (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') s = s + 1;
    neg = 0;
    if (*s == '-') { neg = 1; s = s + 1; }
    else if (*s == '+') s = s + 1;

    if ((base == 0 || base == 16) && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        s = s + 2;
        base = 16;
    } else if (base == 0 && s[0] == '0') {
        base = 8;
    } else if (base == 0) {
        base = 10;
    }

    v = 0;
    while (1) {
        d = parse_digit(*s, base);
        if (d < 0) break;
        v = v * (unsigned long)base + (unsigned long)d;
        s = s + 1;
    }
    if (endptr) *endptr = s;
    if (neg) v = (unsigned long)(0 - (long)v);
    return v;
}

unsigned long long strtoull(char *s, char **endptr, int base) {
    /* Same logic, 64-bit accumulator. Identical body — strtoul already
     * accumulates as unsigned long, which is 64-bit on AArch64. */
    return (unsigned long long)strtoul(s, endptr, base);
}

/* ============================================================================
 * strtok_r — destructive tokenizer with caller-supplied saveptr.
 * ============================================================================ */

static int is_delim(int c, char *delim) {
    while (*delim) {
        if (c == *delim) return 1;
        delim = delim + 1;
    }
    return 0;
}

char *strtok_r(char *s, char *delim, char **saveptr) {
    char *start;
    if (s == 0) s = *saveptr;
    if (s == 0) return 0;

    while (*s && is_delim(*s, delim)) s = s + 1;
    if (*s == 0) { *saveptr = 0; return 0; }

    start = s;
    while (*s && !is_delim(*s, delim)) s = s + 1;
    if (*s) {
        *s = 0;
        *saveptr = s + 1;
    } else {
        *saveptr = 0;
    }
    return start;
}

/* ============================================================================
 * qsort — Lomuto-partition quicksort. Recursive (depth ~log2 N).
 * ============================================================================ */

static void qs_swap(char *a, char *b, int size) {
    int i;
    char tmp;
    i = 0;
    while (i < size) {
        tmp = a[i];
        a[i] = b[i];
        b[i] = tmp;
        i = i + 1;
    }
}

static int qs_call_cmp(int (*cmp)(), char *a, char *b) {
    return cmp(a, b);
}

static void qs_run(char *base, int lo, int hi, int size, int (*cmp)()) {
    int i; int j;
    char *pivot;
    if (lo >= hi) return;
    pivot = base + hi * size;
    i = lo - 1;
    j = lo;
    while (j < hi) {
        if (qs_call_cmp(cmp, base + j * size, pivot) <= 0) {
            i = i + 1;
            if (i != j) qs_swap(base + i * size, base + j * size, size);
        }
        j = j + 1;
    }
    i = i + 1;
    if (i != hi) qs_swap(base + i * size, base + hi * size, size);
    qs_run(base, lo, i - 1, size, cmp);
    qs_run(base, i + 1, hi, size, cmp);
}

void qsort(char *base, int n, int size, int (*cmp)()) {
    if (n <= 1) return;
    qs_run(base, 0, n - 1, size, cmp);
}

/* ============================================================================
 * snprintf — format into a fixed-size buffer.
 *
 * Reuses the formatting style of stdio.c's fmt_core (kept self-contained
 * here so we don't have to refactor stdio.c). Uses a fixed args[] read
 * via the pointer-width-args trick.
 * ============================================================================ */

static void snp_putc(int ch, char **ctx) {
    /* ctx[0] = current cursor, ctx[1] = end cursor (one past last writable) */
    char *cur;
    cur = ctx[0];
    if (cur < ctx[1]) {
        *cur = (char)ch;
        ctx[0] = cur + 1;
    }
}

static void snp_emit_int(int val, int is_unsigned, int base, int width,
                         int zero_pad, char **ctx) {
    char buf[24];
    int i; int neg;
    unsigned int uval;
    int len; int pad; int d;

    neg = 0;
    if (!is_unsigned && val < 0) {
        neg = 1;
        uval = (unsigned int)(0 - val);
    } else {
        uval = (unsigned int)val;
    }
    i = 0;
    if (uval == 0) { buf[i] = '0'; i = i + 1; }
    else {
        while (uval > 0) {
            d = (int)(uval % (unsigned int)base);
            if (d < 10) buf[i] = '0' + d;
            else buf[i] = 'a' + d - 10;
            uval = uval / (unsigned int)base;
            i = i + 1;
        }
    }
    len = i;
    if (neg) len = len + 1;
    pad = width - len;
    if (pad < 0) pad = 0;
    if (!zero_pad) while (pad > 0) { snp_putc(' ', ctx); pad = pad - 1; }
    if (neg) snp_putc('-', ctx);
    if (zero_pad) while (pad > 0) { snp_putc('0', ctx); pad = pad - 1; }
    while (i > 0) { i = i - 1; snp_putc(buf[i], ctx); }
}

static void snp_emit_hex(unsigned int val, int width, int zero_pad, int upper,
                         char **ctx) {
    char buf[16];
    int i; int len; int pad;
    char *digits;
    digits = upper ? "0123456789ABCDEF" : "0123456789abcdef";
    i = 0;
    if (val == 0) { buf[i] = '0'; i = i + 1; }
    else {
        while (val != 0) {
            buf[i] = digits[val & 15];
            val = val >> 4;
            i = i + 1;
        }
    }
    len = i;
    pad = width - len;
    if (pad < 0) pad = 0;
    if (!zero_pad) while (pad > 0) { snp_putc(' ', ctx); pad = pad - 1; }
    if (zero_pad) while (pad > 0) { snp_putc('0', ctx); pad = pad - 1; }
    while (i > 0) { i = i - 1; snp_putc(buf[i], ctx); }
}

/* 64-bit integer emit — mirrors fmt_int64 in stdio.c.  Single base
 * parameter (10 or 16); upper selects hex case (ignored for decimal). */
static void snp_emit_int64(long long val, int is_unsigned, int base,
                           int width, int zero_pad, int upper, char **ctx) {
    char buf[24];
    int i; int neg;
    unsigned long long uval;
    int len; int pad; int d;
    char *digits;

    digits = upper ? "0123456789ABCDEF" : "0123456789abcdef";
    neg = 0;
    if (!is_unsigned && val < 0) {
        neg = 1;
        uval = (unsigned long long)(0 - val);
    } else {
        uval = (unsigned long long)val;
    }
    i = 0;
    if (uval == 0) { buf[i] = '0'; i = i + 1; }
    else {
        while (uval > 0) {
            d = (int)(uval % (unsigned long long)base);
            buf[i] = digits[d];
            uval = uval / (unsigned long long)base;
            i = i + 1;
        }
    }
    len = i;
    if (neg) len = len + 1;
    pad = width - len;
    if (pad < 0) pad = 0;
    if (!zero_pad) while (pad > 0) { snp_putc(' ', ctx); pad = pad - 1; }
    if (neg) snp_putc('-', ctx);
    if (zero_pad) while (pad > 0) { snp_putc('0', ctx); pad = pad - 1; }
    while (i > 0) { i = i - 1; snp_putc(buf[i], ctx); }
}

/* Sibling of stdio.c's fmt_core.  See the comment there for the rules
 * on `ll`, `.N`, and the `%f` placeholder — same logic, same caveats
 * (variadic doubles live in V regs and never reach the X-class args[]
 * array, so `%f` prints '?' without consuming an arg). */
static int snp_core(char *fmt, char **args, char *buf, int size) {
    char *ctx[2];
    int written;
    int ai;
    int width; int zero_pad;
    int is_ll;
    int precision;
    int has_prec;
    char *cur_start;

    if (size <= 0) return 0;
    cur_start = buf;
    ctx[0] = buf;
    ctx[1] = buf + size - 1;  /* leave room for terminator */
    ai = 0;

    while (*fmt) {
        if (*fmt != '%') {
            snp_putc(*fmt, ctx);
            fmt = fmt + 1;
            continue;
        }
        fmt = fmt + 1;
        if (*fmt == '%') {
            snp_putc('%', ctx);
            fmt = fmt + 1;
            continue;
        }
        zero_pad = 0;
        if (*fmt == '0') { zero_pad = 1; fmt = fmt + 1; }
        width = 0;
        while (*fmt >= '0' && *fmt <= '9') {
            width = width * 10 + (*fmt - '0');
            fmt = fmt + 1;
        }
        /* Optional .N precision — parsed but only consumed by the %f
         * placeholder, which doesn't currently use it. */
        precision = 0;
        has_prec = 0;
        if (*fmt == '.') {
            has_prec = 1;
            fmt = fmt + 1;
            while (*fmt >= '0' && *fmt <= '9') {
                precision = precision * 10 + (*fmt - '0');
                fmt = fmt + 1;
            }
        }
        (void)precision;
        (void)has_prec;
        /* Length modifier: ll for 64-bit; bare l treated as no-modifier
         * (long == int on the s12cc subset).  z/j/t skipped as size_t /
         * intmax_t / ptrdiff_t are 32-bit here. */
        is_ll = 0;
        if (*fmt == 'l') {
            fmt = fmt + 1;
            if (*fmt == 'l') { is_ll = 1; fmt = fmt + 1; }
        } else if (*fmt == 'z' || *fmt == 'j' || *fmt == 't') {
            fmt = fmt + 1;
        }
        if (*fmt == 'd' || *fmt == 'i') {
            if (is_ll) snp_emit_int64((long long)args[ai], 0, 10, width, zero_pad, 0, ctx);
            else       snp_emit_int((int)(long)args[ai], 0, 10, width, zero_pad, ctx);
            ai = ai + 1;
        } else if (*fmt == 'u') {
            if (is_ll) snp_emit_int64((long long)args[ai], 1, 10, width, zero_pad, 0, ctx);
            else       snp_emit_int((int)(long)args[ai], 1, 10, width, zero_pad, ctx);
            ai = ai + 1;
        } else if (*fmt == 'x') {
            if (is_ll) snp_emit_int64((long long)args[ai], 1, 16, width, zero_pad, 0, ctx);
            else       snp_emit_hex((unsigned int)(long)args[ai], width, zero_pad, 0, ctx);
            ai = ai + 1;
        } else if (*fmt == 'X') {
            if (is_ll) snp_emit_int64((long long)args[ai], 1, 16, width, zero_pad, 1, ctx);
            else       snp_emit_hex((unsigned int)(long)args[ai], width, zero_pad, 1, ctx);
            ai = ai + 1;
        } else if (*fmt == 'p') {
            snp_putc('0', ctx); snp_putc('x', ctx);
            snp_emit_int64((long long)args[ai], 1, 16, 0, 0, 0, ctx);
            ai = ai + 1;
        } else if (*fmt == 'c') {
            snp_putc((int)(long)args[ai], ctx);
            ai = ai + 1;
        } else if (*fmt == 's') {
            char *s; s = args[ai];
            if (!s) s = "(null)";
            while (*s) { snp_putc(*s, ctx); s = s + 1; }
            ai = ai + 1;
        } else if (*fmt == 'f' || *fmt == 'F' ||
                   *fmt == 'g' || *fmt == 'G' ||
                   *fmt == 'e' || *fmt == 'E') {
            /* FP placeholder — see fmt_core for the rationale.  Doubles
             * pass in V regs and never enter the X-class args[] array,
             * so we can't read the value; print '?' and don't advance
             * the arg index. */
            snp_putc('?', ctx);
        } else {
            snp_putc('%', ctx);
            snp_putc(*fmt, ctx);
        }
        fmt = fmt + 1;
    }
    *(ctx[0]) = 0;  /* terminator */
    written = (int)(ctx[0] - cur_start);
    return written;
}

int snprintf(char *buf, int size, char *fmt, char *a0, char *a1, char *a2,
             char *a3, char *a4, char *a5, char *a6, char *a7) {
    char *args[8];
    args[0] = a0; args[1] = a1; args[2] = a2; args[3] = a3;
    args[4] = a4; args[5] = a5; args[6] = a6; args[7] = a7;
    return snp_core(fmt, args, buf, size);
}

/* ============================================================================
 * FP classification — bit-twiddle since dbt's host_isnan/isinf/isfinite
 * wrappers want the IEEE 754 classification of a double.
 * ============================================================================ */

int isnan(double x) {
    long bits; unsigned long exp; unsigned long mant;
    memcpy((char *)&bits, (char *)&x, 8);
    exp = ((unsigned long)bits >> 52) & 0x7FFu;
    mant = (unsigned long)bits & 0xFFFFFFFFFFFFFul;
    return (exp == 0x7FFu) && (mant != 0);
}

int isinf(double x) {
    long bits; unsigned long exp; unsigned long mant;
    memcpy((char *)&bits, (char *)&x, 8);
    exp = ((unsigned long)bits >> 52) & 0x7FFu;
    mant = (unsigned long)bits & 0xFFFFFFFFFFFFFul;
    if ((exp == 0x7FFu) && (mant == 0)) {
        if ((unsigned long)bits & 0x8000000000000000ul) return -1;
        return 1;
    }
    return 0;
}

int isfinite(double x) {
    long bits; unsigned long exp;
    memcpy((char *)&bits, (char *)&x, 8);
    exp = ((unsigned long)bits >> 52) & 0x7FFu;
    return exp != 0x7FFu;
}

/* ============================================================================
 * Compiler builtins: __builtin_ctz / __builtin_popcount.
 * cc-a64 doesn't recognise these as intrinsics yet, so they link as
 * regular calls. Plain-C implementations; AArch64 has CLZ (and RBIT+CLZ
 * for ctz, CNT for popcount) — these are upgrade candidates.
 * ============================================================================ */

int __builtin_ctz(unsigned int x) {
    int n;
    if (x == 0) return 32;  /* C says result is undefined; pick a value */
    n = 0;
    while ((x & 1) == 0) { x = x >> 1; n = n + 1; }
    return n;
}

int __builtin_popcount(unsigned int x) {
    int n;
    n = 0;
    while (x) {
        x = x & (x - 1);
        n = n + 1;
    }
    return n;
}

/* ============================================================================
 * Process: system() — running an external command isn't part of dbt's
 * critical path on a64 (block_cache.c calls it for an optional disasm
 * dump). Stub returning -1 is sufficient.
 * ============================================================================ */

int system(char *cmd) {
    return -1;
}

/* ============================================================================
 * Math symbol stubs.
 *
 * dbt.c registers these in math_intercepts[] as `(void *)(uintptr_t)fn`
 * — host pointers it'll patch into trampolines for guest calls. The
 * symbols MUST resolve at link time but the implementations don't
 * actually run on the AArch64 self-host (we never replay a workload
 * that calls libm here yet). Each returns 0/x to keep behavior
 * predictable if ever invoked.
 * ============================================================================ */

double fabs(double x)         { if (x < 0) return 0 - x; return x; }
double ceil(double x)         { return x; }
double floor(double x)        { return x; }
double round(double x)        { return x; }
double trunc(double x)        { return x; }
double sin(double x)          { return 0; }
double cos(double x)          { return 0; }
double tan(double x)          { return 0; }
double asin(double x)         { return 0; }
double acos(double x)         { return 0; }
double atan(double x)         { return 0; }
double atan2(double y, double x) { return 0; }
double sinh(double x)         { return 0; }
double cosh(double x)         { return 0; }
double tanh(double x)         { return 0; }
double exp(double x)          { return 0; }
double log(double x)          { return 0; }
double log10(double x)        { return 0; }
double pow(double a, double b) { return 0; }
double fmod(double a, double b) { return 0; }
double frexp(double x, int *e) { if (e) *e = 0; return x; }
double ldexp(double x, int e) { return x; }
double modf(double x, double *ip) { if (ip) *ip = x; return 0; }
double copysign(double x, double y) {
    if (y < 0) { if (x > 0) return 0 - x; return x; }
    if (x < 0) return 0 - x;
    return x;
}

float fabsf(float x)          { if (x < 0) return 0 - x; return x; }
float ceilf(float x)          { return x; }
float floorf(float x)         { return x; }
float roundf(float x)         { return x; }
float truncf(float x)         { return x; }
float sinf(float x)           { return 0; }
float cosf(float x)           { return 0; }
float tanf(float x)           { return 0; }
float asinf(float x)          { return 0; }
float acosf(float x)          { return 0; }
float atanf(float x)          { return 0; }
float atan2f(float y, float x) { return 0; }
float sinhf(float x)          { return 0; }
float coshf(float x)          { return 0; }
float tanhf(float x)          { return 0; }
float expf(float x)           { return 0; }
float logf(float x)           { return 0; }
float log10f(float x)         { return 0; }
float powf(float a, float b)  { return 0; }
float fmodf(float a, float b) { return 0; }
float frexpf(float x, int *e) { if (e) *e = 0; return x; }
float ldexpf(float x, int e)  { return x; }
float modff(float x, float *ip) { if (ip) *ip = x; return 0; }
float copysignf(float x, float y) {
    if (y < 0) { if (x > 0) return 0 - x; return x; }
    if (x < 0) return 0 - x;
    return x;
}
