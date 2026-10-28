// 6.4.4.2p4  L suffix: long double (size matches clang's ABI for SLOW-32)
#include <stdio.h>
int main(void) {
    long double x = 1.25L;
    printf("%d %.2f\n", (int)sizeof x, (double)(x + x));
    return 0;
}
