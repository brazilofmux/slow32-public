/* 7.21.6.2 fscanf: the hh h l ll j z t length modifiers select the type of the receiving object */
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>

int main(void) {
    signed char hh[2] = { 0x55, 0x55 };
    unsigned char uhh[2] = { 0x55, 0x55 };
    short h[2] = { 0x5555, 0x5555 };
    long l = 0;
    long long ll = 0;
    unsigned long long ull = 0;
    intmax_t j = 0;
    size_t z = 0;
    ptrdiff_t t = 0;
    int r = sscanf("-7 200 -300", "%hhd %hhu %hd", &hh[0], &uhh[0], &h[0]);
    printf("%d: %d %d %d %d %d %d\n", r, hh[0], hh[1], uhh[0], uhh[1], h[0], h[1]);
    r = sscanf("-123456 -9000000000 18000000000000000000 -5000000000 4000000000 -77",
               "%ld %lld %llu %jd %zu %td", &l, &ll, &ull, &j, &z, &t);
    printf("%d: %ld %lld %llu %jd %zu %td\n", r, l, ll, ull, j, z, t);
    r = sscanf("ff 0x7fffffffffffffff", "%hhx %lli", &uhh[0], &ll);
    printf("%d: %d %lld\n", r, uhh[0], ll);
    return 0;
}
