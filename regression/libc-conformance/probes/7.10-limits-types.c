/* 7.10, 5.2.4.2.1p1: each <limits.h> macro has the type of an object of its type after the integer promotions */
#include <limits.h>
#include <stdio.h>

#define T(m) _Generic((m), int: "int", unsigned: "unsigned", long: "long", unsigned long: "unsigned long", \
                      long long: "long long", unsigned long long: "unsigned long long", default: "other")
#define SAME(m, t) (sizeof(m) == sizeof(+(t)0) && ((m) * 0 - 1 < 0) == (+(t)0 - 1 < 0))

int main(void) {
    printf("char-sized %d %d %d %d %d\n", SAME(CHAR_MAX, char), SAME(CHAR_MIN, char), SAME(SCHAR_MAX, signed char),
           SAME(SCHAR_MIN, signed char), SAME(UCHAR_MAX, unsigned char));
    printf("short %d %d %d\n", SAME(SHRT_MAX, short), SAME(SHRT_MIN, short), SAME(USHRT_MAX, unsigned short));
    printf("%s %s %s\n", T(INT_MIN), T(INT_MAX), T(UINT_MAX));
    printf("%s %s %s\n", T(LONG_MIN), T(LONG_MAX), T(ULONG_MAX));
    printf("%s %s %s\n", T(LLONG_MIN), T(LLONG_MAX), T(ULLONG_MAX));
    return 0;
}
