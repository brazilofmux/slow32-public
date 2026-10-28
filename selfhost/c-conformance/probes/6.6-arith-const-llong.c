// 6.6p8 (6.3.1.8)  static initializer constants use the usual conversions: int * long long is computed in long long
#include <stdio.h>
static long long a = 0x7FFFFFFF * 2LL;
static long long b = 1LL << 40;
static unsigned long long c = 0xFFFFFFFFu + 1ULL;
int main(void) { printf("%lld %lld %llu\n", a, b, c); return 0; }
