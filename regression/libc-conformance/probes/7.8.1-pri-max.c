/* 7.8.1p2-p3: the PRI macros for intmax_t and uintmax_t */
#include <inttypes.h>
#include <stdio.h>

int main(void) {
    intmax_t m = INTMAX_MIN;
    uintmax_t u = UINTMAX_MAX;
    printf("%d\n", m < -9223372036854775807 && u >= 18446744073709551615u);
    printf("%" PRIdMAX " %" PRIiMAX "\n", (intmax_t)-9223372036854775807 - 1, (intmax_t)123456789012345);
    printf("%" PRIuMAX " %" PRIxMAX " %" PRIoMAX " %" PRIXMAX "\n", (uintmax_t)18446744073709551615u,
           (uintmax_t)0xfedcba9876543210u, (uintmax_t)8, (uintmax_t)0xabcdef);
    return 0;
}
