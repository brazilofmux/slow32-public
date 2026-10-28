// 6.9.1p8  a definition with an ellipsis reads its trailing arguments with va_arg
#include <stdio.h>
#include <stdarg.h>
static long long sum(int n, ...) {
    va_list ap;
    long long s = 0;
    va_start(ap, n);
    for (int i = 0; i < n; i++) {
        switch (va_arg(ap, int)) {
        case 0: s += va_arg(ap, int); break;
        case 1: s += va_arg(ap, long long); break;
        default: s += (long long)va_arg(ap, double);
        }
    }
    va_end(ap);
    return s;
}
int main(void) {
    printf("%lld\n", sum(4, 0, 5, 1, 1LL << 35, 2, 2.5, 0, -1));
    return 0;
}
