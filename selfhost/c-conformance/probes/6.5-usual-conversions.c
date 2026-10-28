// 6.5p1-6.3.1.8  usual arithmetic conversions in binary operators (signed/unsigned mix)
#include <stdio.h>
int main(void) {
    int neg = -1;
    unsigned u = 1;
    long long ll = -1;
    unsigned char uc = 200;
    signed char sc = -100;
    printf("%d %d %d\n", neg < u, ll < u, (int)sizeof(uc + sc));
    printf("%d %u\n", uc + sc, (unsigned)(neg + u));
    unsigned short us = 65535;
    printf("%d %d\n", us + 1, (int)sizeof(us + us));
    return 0;
}
