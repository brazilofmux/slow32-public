// 6.7.2p2  type specifiers may appear in any order (int signed, int long long, short unsigned int)
#include <stdio.h>
int main(void) {
    int signed e = -3; int long long k = 9; short unsigned int l = 10; int unsigned long u = 7;
    printf("%d %lld %u %lu\n", e, k, l, u);
    return 0;
}
