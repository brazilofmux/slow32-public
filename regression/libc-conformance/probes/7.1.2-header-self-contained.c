/* 7.1.2 standard headers: <inttypes.h> alone supplies <stdint.h>'s types and macros (7.8p1); <setjmp.h> and <stdarg.h> need no other header */
#include <inttypes.h>
#include <setjmp.h>
#include <stdarg.h>

int printf(const char *, ...);

static int first(int n, ...) {
    va_list ap;
    int r;
    va_start(ap, n);
    r = va_arg(ap, int);
    va_end(ap);
    return r;
}

int main(void) {
    uint64_t u = UINT64_MAX;
    int16_t s = INT16_MIN;
    intmax_t m = INTMAX_MAX;
    jmp_buf env;
    (void)env;
    printf("%d %d %d %d\n", u == 18446744073709551615u, s == -32768, m >= 9223372036854775807, first(1, 42));
    return 0;
}
