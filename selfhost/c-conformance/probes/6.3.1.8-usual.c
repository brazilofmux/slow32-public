// 6.3.1.8p1  usual arithmetic conversions: signed/unsigned mixes, long long, floating
#include <stdio.h>
int main(void) {
    int neg = -1;
    unsigned one = 1;
    long long ll = -1;
    unsigned long long ull = 1;
    int r1 = neg < one;               /* -1 -> UINT_MAX: 0 */
    int r2 = ll < one;                /* unsigned int -> long long: 1 */
    int r3 = neg < ull;               /* 0 */
    long long r4 = neg + 0u;          /* unsigned: 4294967295 */
    long long r5 = ll + one;          /* long long: 0 */
    double r6 = 7 / 2 + 7 / 2.0;      /* 3 + 3.5 */
    volatile float f = 0.1f;
    int r7 = (f + 1.0) - 1.0 == (double)f;   /* computed in double */
    int r8 = f * 3 == (float)(f * 3);
    unsigned long ul = 3;
    long r9 = -5L / (long)ul;
    unsigned long r10 = -5L + ul;     /* long and unsigned long same rank -> unsigned long */
    printf("%d %d %d %lld %lld %.1f %d %d %ld %lu\n", r1, r2, r3, r4, r5, r6, r7, r8, r9, r10);
    return 0;
}
