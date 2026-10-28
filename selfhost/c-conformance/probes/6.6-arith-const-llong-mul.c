// 6.6p8  64-bit multiply and divide fold in a static initializer
#include <stdio.h>
static long long a = 2LL * 3000000000u;
static long long b = 9000000000LL / 3;
int main(void) { printf("%lld %lld\n", a, b); return 0; }
