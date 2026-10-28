// 6.7.6.2p5  a VLA typedef's size is evaluated once, at the typedef
#include <stdio.h>
int main(void) {
    int n = 4;
    typedef int T[n];
    n = 9;
    T x;
    printf("%u\n", (unsigned)(sizeof x / sizeof x[0]));
    return 0;
}
