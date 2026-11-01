/* 7.10, 5.2.4.2.1p1-p2, 6.3.1.3p2: the unsigned maxima are the converted -1, CHAR_BIT counts unsigned char's bits, CHAR_MIN/MAX follow char's signedness */
#include <limits.h>
#include <stdio.h>

int main(void) {
    unsigned char uc = (unsigned char)-1;
    int bits = 0;
    while (uc) { bits++; uc >>= 1; }
    printf("CHAR_BIT %d\n", bits == CHAR_BIT);
    printf("unsigned %d %d %d %d %d\n", (unsigned char)-1 == UCHAR_MAX, (unsigned short)-1 == USHRT_MAX,
           (unsigned)-1 == UINT_MAX, (unsigned long)-1 == ULONG_MAX, (unsigned long long)-1 == ULLONG_MAX);
    if ((char)-1 < 0)
        printf("char %d\n", CHAR_MIN == SCHAR_MIN && CHAR_MAX == SCHAR_MAX);
    else
        printf("char %d\n", CHAR_MIN == 0 && CHAR_MAX == UCHAR_MAX);
    printf("signed max fits %d %d %d %d %d\n", (signed char)SCHAR_MAX == SCHAR_MAX, (short)SHRT_MAX == SHRT_MAX,
           (int)INT_MAX == INT_MAX, (long)LONG_MAX == LONG_MAX, (long long)LLONG_MAX == LLONG_MAX);
    printf("ranks %d %d %d %d\n", SCHAR_MAX <= SHRT_MAX && SHRT_MAX <= INT_MAX && INT_MAX <= LONG_MAX && LONG_MAX <= LLONG_MAX,
           UCHAR_MAX <= USHRT_MAX && USHRT_MAX <= UINT_MAX && UINT_MAX <= ULONG_MAX && ULONG_MAX <= ULLONG_MAX,
           INT_MAX <= UINT_MAX, LLONG_MAX <= ULLONG_MAX);
    return 0;
}
