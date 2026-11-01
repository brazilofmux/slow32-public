/* 7.24.4.4 strncmp: n is only an upper bound -- with n of SIZE_MAX or above INT_MAX, the strings' nulls still end the comparison */
#include <stdio.h>
#include <string.h>

static int sgn(int v) { return (v > 0) - (v < 0); }

int main(void) {
    printf("%d %d %d ", sgn(strncmp("abc", "abc", (size_t)-1)), sgn(strncmp("abc", "abd", (size_t)-1)), sgn(strncmp("b", "a", (size_t)-1)));
    printf("%d %d\n", sgn(strncmp("abc", "abd", (size_t)0x80000000u)), sgn(strncmp("ab", "abc", (size_t)0x90000000u)));
    return 0;
}
