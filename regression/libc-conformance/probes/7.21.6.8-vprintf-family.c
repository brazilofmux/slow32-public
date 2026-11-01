/* 7.21.6.8-13 vfprintf vprintf vsnprintf vsprintf: the va_list forms behave as their variadic counterparts */
#include <stdio.h>
#include <stdarg.h>

static int both(char *buf, size_t n, const char *fmt, ...) {
    va_list ap, aq;
    va_start(ap, fmt);
    va_copy(aq, ap);
    int a = vsnprintf(buf, n, fmt, ap);
    va_end(ap);
    int b = vprintf(fmt, aq);
    va_end(aq);
    printf("|%d %d [%s]\n", a, b, buf);
    return a;
}

static int to_stream(FILE *f, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int r = vfprintf(f, fmt, ap);
    va_end(ap);
    return r;
}

static int to_buf(char *b, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int r = vsprintf(b, fmt, ap);
    va_end(ap);
    return r;
}

int main(void) {
    char b[32];
    both(b, sizeof b, "%d %s %.2f %lld %c", -1, "two", 3.0, 4LL, '5');
    both(b, 6, "%s%s", "abcd", "efgh");
    both(b, 0 + 1, "%x", 255u);
    int r = to_stream(stdout, "%5s|%-3d|%e\n", "ab", 9, 0.5);
    printf("%d\n", r);
    r = to_buf(b, "%o-%X-%%", 8u, 171u);
    printf("%d [%s]\n", r, b);
    return 0;
}
