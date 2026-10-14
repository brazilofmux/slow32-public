/* Variadic doubles among ints.  cc-x64 passes every argument in the GP
 * sequence (rdi..r9, then the stack), a double as its bits; its
 * va_arg(double) read the SysV XMM save area instead, which no caller
 * filled -- right only when XMM0 happened to hold the value, and every
 * argument after it shifted by one (dbt-x64's -Q probe line).  Doubles
 * first, last, between ints, past the six registers, and after a named
 * double; and a printf-style walk driven by a format string. */
#include <stdarg.h>
#include <stdio.h>

static double mix(int n, ...) {
    va_list ap;
    double s = 0.0;
    int i;
    va_start(ap, n);
    for (i = 0; i < n; i = i + 1) {
        if (i % 2 == 0) s = s * 3.0 + va_arg(ap, double);
        else s = s * 3.0 + (double)va_arg(ap, int);
    }
    va_end(ap);
    return s;
}

static double after_named(double scale, int n, ...) {
    va_list ap;
    double s = 0.0;
    int i;
    va_start(ap, n);
    for (i = 0; i < n; i = i + 1) s = s + scale * va_arg(ap, double);
    va_end(ap);
    return s;
}

/* d = double, i = int, s = string, l = long long */
static long long walk(const char *fmt, ...) {
    va_list ap;
    long long h = 7;
    va_start(ap, fmt);
    while (*fmt) {
        if (*fmt == 'd') h = h * 31 + (long long)(va_arg(ap, double) * 1000.0);
        else if (*fmt == 'i') h = h * 31 + va_arg(ap, int);
        else if (*fmt == 'l') h = h * 31 + va_arg(ap, long long);
        else if (*fmt == 's') { const char *s = va_arg(ap, const char *); while (*s) h = h * 31 + *s++; }
        fmt++;
    }
    va_end(ap);
    return h;
}

int main(void) {
    double a = mix(9, 1.5, 2, 2.25, 3, 3.125, 4, 4.0625, 5, 5.5);
    double b = after_named(0.5, 7, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0);
    long long c = walk("didsidli", 1.25, 7, 2.5, "ab", 9, -3.75, 1234567890123LL, 11);
    long long d = walk("dddddddd", 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0);
    printf("mix %.4f\n", a);
    printf("named %.4f\n", b);
    printf("walk %lld %lld\n", c, d);
    printf("[%7.1fs] %08X %08X %s|\n", 1.0, 0x11u, 0x22u, "end");
    return (int)((long long)a + (long long)b + c + d) & 127;
}
