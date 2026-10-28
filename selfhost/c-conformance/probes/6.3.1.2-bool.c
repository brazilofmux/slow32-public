// 6.3.1.2p1  conversion to _Bool yields 0 only for 0, otherwise 1 (incl. large ints, fractions, pointers)
#include <stdio.h>
int main(void) {
    int big = 256;
    double half = 0.5;
    long long ll = 0x100000000LL;
    int x;
    _Bool b1 = big, b2 = half, b3 = ll, b4 = &x, b5 = 0, b6 = -1;
    _Bool b7 = (_Bool)2;
    _Bool b8 = 1; b8 += 1;
    printf("%d %d %d %d %d %d %d %d %d\n", b1, b2, b3, b4, b5, b6, b7, b8, (int)sizeof(_Bool));
    return 0;
}
