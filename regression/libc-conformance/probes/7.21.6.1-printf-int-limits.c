/* 7.21.6.1 fprintf: the extreme values of each signed and unsigned integer type */
#include <stdio.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>

int main(void) {
    printf("%d %d %u %x %o\n", INT_MIN, INT_MAX, UINT_MAX, UINT_MAX, UINT_MAX);
    char a[32], b[32];
    snprintf(a, sizeof a, "%ld %ld %lu", LONG_MIN, LONG_MAX, ULONG_MAX);
    snprintf(b, sizeof b, "%lld %lld %llu", (long long)LONG_MIN, (long long)LONG_MAX, (unsigned long long)ULONG_MAX);
    printf("long limits match long long: %d\n", strcmp(a, b) == 0);
    printf("%lld %lld %llu %llx %llo\n", LLONG_MIN, LLONG_MAX, ULLONG_MAX, ULLONG_MAX, ULLONG_MAX);
    printf("%jd %ju\n", INTMAX_MIN, UINTMAX_MAX);
    printf("%+lld %.25lld %-22lld|\n", LLONG_MAX, LLONG_MIN, LLONG_MIN);
    printf("%hd %hu %hhd %hhu\n", SHRT_MIN, USHRT_MAX, SCHAR_MIN, UCHAR_MAX);
    return 0;
}
