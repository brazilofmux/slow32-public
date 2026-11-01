/* 7.16.1.1: va_arg walks a long argument list mixing 4- and 8-byte types after named parameters */
#include <stdarg.h>
#include <stdio.h>

static long long walk(int a, int b, int c, int n, ...) {
    va_list ap;
    long long sum = a + b + c;
    int i;
    va_start(ap, n);
    for (i = 0; i < n; i++) {
        switch (i % 3) {
        case 0: sum += va_arg(ap, int); break;
        case 1: sum += va_arg(ap, long long); break;
        default: sum += (long long)(va_arg(ap, double) * 4); break;
        }
    }
    va_end(ap);
    return sum;
}

int main(void) {
    printf("%lld\n", walk(1, 2, 3, 12, 10, 100000000000LL, 0.25, 20, 200000000000LL, 0.5,
                          30, 300000000000LL, 0.75, 40, 400000000000LL, 1.0));
    printf("%lld\n", walk(0, 0, 0, 0));
    return 0;
}
