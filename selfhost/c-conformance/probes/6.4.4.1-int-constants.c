// 6.4.4.1p1-p4  decimal, octal, hex constants with u/l/ll suffixes in any case and order
#include <stdio.h>
int main(void) {
    long long v = 0x7fffffffffffffffLL;
    printf("%d %d %d %u %ld %lu %lld %llu %llu %lld %d\n", 017, 0x1F, 0XaB, 10u, 20L, 30UL, 40ll, 50uLL, 60LLU,
           v, 0);
    printf("%d %d %d\n", (int)sizeof 1, (int)sizeof 1L, (int)sizeof 1LL);
    return 0;
}
