/* 7.7, 5.2.4.2.2p11-p13: EPSILON, MIN, MAX, MAX_10_EXP and MIN_10_EXP agree with the arithmetic of float, double and long double */
#include <float.h>
#include <stdio.h>

#define CHECK(T, P)                                                              \
    do {                                                                         \
        volatile T one = 1, e = 1, x, b = FLT_RADIX;                              \
        int i, n = 0;                                                            \
        for (;;) {                                                               \
            x = one + e / b;                                                     \
            if (x == one) break;                                                 \
            e = e / b;                                                           \
            n++;                                                                 \
        }                                                                        \
        printf(#P "_EPSILON %d MANT_DIG %d", e == P##_EPSILON, n + 1 == P##_MANT_DIG); \
        x = one;                                                                 \
        for (i = P##_MIN_EXP; i < 1; i++) x = x / b;                            \
        printf(" MIN %d", x == P##_MIN);                                         \
        x = one - P##_EPSILON / b;                                               \
        for (i = 0; i < P##_MAX_EXP; i++) x = x * b;                             \
        printf(" MAX %d", x == P##_MAX);                                         \
        x = one;                                                                 \
        for (i = 0; i < P##_MAX_10_EXP; i++) x = x * 10;                         \
        printf(" MAX_10_EXP %d", x <= P##_MAX && x > P##_MAX / 10);              \
        x = one;                                                                 \
        for (i = 0; i > P##_MIN_10_EXP; i--) x = x / 10;                         \
        printf(" MIN_10_EXP %d\n", x >= P##_MIN && x / 10 < P##_MIN);            \
    } while (0)

int main(void) {
    CHECK(float, FLT);
    CHECK(double, DBL);
    CHECK(long double, LDBL);
    return 0;
}
