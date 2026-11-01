/* 7.16.1.2: va_copy duplicates the traversal state; copy and original then advance independently */
#include <stdarg.h>
#include <stdio.h>

static void twice(int n, ...) {
    va_list ap, cp;
    int i;
    va_start(ap, n);
    printf("%d ", va_arg(ap, int));
    va_copy(cp, ap);
    for (i = 1; i < n; i++) printf("%d ", va_arg(ap, int));
    for (i = 1; i < n; i++) printf("%d ", va_arg(cp, int) * 10);
    va_end(cp);
    va_end(ap);
    printf("\n");
}

int main(void) {
    twice(4, 1, 2, 3, 4);
    return 0;
}
