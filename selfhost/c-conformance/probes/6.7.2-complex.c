// 6.7.2p2  _Complex types (conditional feature; clang supports it)
// optional: __STDC_NO_COMPLEX__
#include <stdio.h>
int main(void) {
    double _Complex z = 1.0;
    printf("%u\n", (unsigned)sizeof z);
    return 0;
}
