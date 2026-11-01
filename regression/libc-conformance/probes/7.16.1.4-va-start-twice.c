/* 7.16.1.3p2, 7.16.1.4: after va_end, va_start again restarts the traversal from the first variable argument */
#include <stdarg.h>
#include <stdio.h>

static int sum2(int n, ...) {
    va_list ap;
    int i, s = 0;
    va_start(ap, n);
    for (i = 0; i < n; i++) s += va_arg(ap, int);
    va_end(ap);
    va_start(ap, n);
    for (i = 0; i < n; i++) s += va_arg(ap, int) * 100;
    va_end(ap);
    return s;
}

int main(void) {
    printf("%d\n", sum2(3, 1, 2, 3));
    return 0;
}
