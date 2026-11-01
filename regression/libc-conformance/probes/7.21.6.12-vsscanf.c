/* 7.21.6.12 vsscanf: the va_list form of sscanf assigns through the pointers and returns the count */
#include <stdio.h>
#include <stdarg.h>

static int scan(const char *s, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int r = vsscanf(s, fmt, ap);
    va_end(ap);
    return r;
}

int main(void) {
    int a = 0;
    char w[8] = "";
    double d = 0;
    int r = scan("17 abc 2.5", "%d %7s %lf", &a, w, &d);
    printf("%d: %d %s %g\n", r, a, w, d);
    printf("%d\n", scan("", "%d", &a) == EOF);
    return 0;
}
