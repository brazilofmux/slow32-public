// 6.3.1.3p3  out-of-range to signed is implementation-defined; clang wraps; checks stage08 agrees
#include <stdio.h>
int main(void) {
    unsigned u = 0x80000001u;
    long long ll = 0x1FFFFFFFFLL;
    signed char sc = (signed char)200;
    short s = (short)40000;
    printf("%d %d %d %d\n", (int)u, (int)ll, sc, s);
    return 0;
}
