/* 7.16p3: a va_list may be passed to another function that applies va_arg, including the v-forms of printf */
#include <stdarg.h>
#include <stdio.h>

static int vsum(int n, va_list ap) {
    int s = 0;
    while (n--) s += va_arg(ap, int);
    return s;
}

static int sum(int n, ...) {
    va_list ap;
    int r;
    va_start(ap, n);
    r = vsum(n, ap);
    va_end(ap);
    return r;
}

static int fmt(char *buf, size_t size, const char *f, ...) {
    va_list ap;
    int r;
    va_start(ap, f);
    r = vsnprintf(buf, size, f, ap);
    va_end(ap);
    return r;
}

static void say(const char *f, ...) {
    va_list ap;
    va_start(ap, f);
    vprintf(f, ap);
    va_end(ap);
}

int main(void) {
    char buf[64];
    int n = fmt(buf, sizeof buf, "%d-%s-%lld-%.2f", 7, "x", 1LL << 35, 0.5);
    printf("%d %s %d\n", sum(4, 1, 20, 300, 4000), buf, n);
    say("%s %d %c\n", "vprintf", -9, 'z');
    return 0;
}
