/* 7.16.1p1: va_start, va_arg, va_end and va_copy are macros */
#include <stdarg.h>
#include <stdio.h>

int main(void) {
    int a = 0, b = 0, c = 0, d = 0;
#ifdef va_start
    a = 1;
#endif
#ifdef va_arg
    b = 1;
#endif
#ifdef va_end
    c = 1;
#endif
#ifdef va_copy
    d = 1;
#endif
    printf("%d %d %d %d\n", a, b, c, d);
    return 0;
}
