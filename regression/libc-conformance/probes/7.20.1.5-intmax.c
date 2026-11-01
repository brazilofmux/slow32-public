/* 7.20.1.5, 7.20.2.5: intmax_t and uintmax_t are at least as wide as any integer type, and their limit macros match */
#include <limits.h>
#include <stdint.h>
#include <stdio.h>

int main(void) {
    printf("%d %d %d\n", INTMAX_MAX >= LLONG_MAX, INTMAX_MIN <= LLONG_MIN, UINTMAX_MAX >= ULLONG_MAX);
    printf("%d %d\n", (uintmax_t)-1 == UINTMAX_MAX, (intmax_t)INTMAX_MAX == INTMAX_MAX);
    printf("%d %d\n", sizeof(intmax_t) >= sizeof(long long), sizeof(uintmax_t) >= sizeof(unsigned long long));
    return 0;
}
