/* 7.16p3: a pointer to a va_list may be passed to another function, and the caller goes on with va_arg after it returns */
#include <stdarg.h>
#include <stdio.h>

static int take(va_list *app) {
    int a = va_arg(*app, int);
    return a + va_arg(*app, int);
}

static void run(int n, ...) {
    va_list ap;
    int first, rest;
    va_start(ap, n);
    first = take(&ap);
    rest = va_arg(ap, int);
    printf("%d %d %g\n", first, rest, va_arg(ap, double));
    va_end(ap);
}

int main(void) {
    run(0, 3, 4, 50, 2.5);
    return 0;
}
