// 6.8.4.1p2-3  if on scalars (ints, pointers, doubles, ll); else binds to the nearest if
#include <stdio.h>
int main(void) {
    int r = 0;
    int *np = 0, v = 1, *p = &v;
    double d0 = 0.0, dh = 0.5, dn = -0.0;
    long long big = 1LL << 40;
    if (np) r |= 1;
    if (p) r |= 2;
    if (d0) r |= 4;
    if (dh) r |= 8;
    if (dn) r |= 16;
    if (big) r |= 32;
    if ((int)big) r |= 64;
    if (v)
        if (!v) r |= 128;
        else r |= 256;
    if (!v)
        if (v) r |= 512;
        else r |= 1024;
    printf("%d\n", r);
    return 0;
}
