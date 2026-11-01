/* 7.16.1.1: va_arg fetches a structure passed through the ellipsis, and the argument after it */
#include <stdarg.h>
#include <stdio.h>

struct trio { int a, b, c; };

static void show(int n, ...) {
    va_list ap;
    struct trio t;
    int after;
    va_start(ap, n);
    t = va_arg(ap, struct trio);
    after = va_arg(ap, int);
    va_end(ap);
    printf("%d %d,%d,%d %d\n", n, t.a, t.b, t.c, after);
}

int main(void) {
    struct trio t = { 7, 8, 9 };
    show(1, t, 42);
    return 0;
}
