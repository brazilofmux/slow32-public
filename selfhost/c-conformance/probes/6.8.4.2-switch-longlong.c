// 6.8.4.2p5  switch on long long / unsigned long long with cases differing only in the high word
#include <stdio.h>
static int f(long long v) {
    switch (v) {
    case 1: return 1;
    case 0x100000001LL: return 2;
    case -1: return 3;
    case 0xFFFFFFFFLL: return 4;
    case -0x7FFFFFFFFFFFFFFFLL - 1: return 5;
    case 0x7FFFFFFFFFFFFFFFLL: return 6;
    default: return 0;
    }
}
static int g(unsigned long long v) {
    switch (v) { case 0x8000000000000000ULL: return 1; case 2: return 2; case 3: return 3; case 4: return 4; case 5: return 5; case 6: return 6; default: return 0; }
}
int main(void) {
    long long t[] = { 1, 0x100000001LL, -1, 0xFFFFFFFFLL, -0x7FFFFFFFFFFFFFFFLL - 1, 0x7FFFFFFFFFFFFFFFLL, 0x200000001LL, 0x1FFFFFFFFLL };
    for (int i = 0; i < 8; i++) printf("%d", f(t[i]));
    printf(" %d%d%d%d\n", g(0x8000000000000000ULL), g(2), g(0x100000002ULL), g(6));
    return 0;
}
