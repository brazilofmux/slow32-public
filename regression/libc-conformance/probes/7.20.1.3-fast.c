/* 7.20.1.3: int_fastN_t and uint_fastN_t hold at least N bits, and their limit macros match them */
#include <stdint.h>
#include <stdio.h>

int main(void) {
    printf("umax %d %d %d %d\n", (uint_fast8_t)-1 == UINT_FAST8_MAX, (uint_fast16_t)-1 == UINT_FAST16_MAX,
           (uint_fast32_t)-1 == UINT_FAST32_MAX, (uint_fast64_t)-1 == UINT_FAST64_MAX);
    printf("wide enough %d %d %d %d\n", UINT_FAST8_MAX >= 255, UINT_FAST16_MAX >= 65535,
           UINT_FAST32_MAX >= 4294967295u, UINT_FAST64_MAX >= 18446744073709551615u);
    printf("signed %d %d %d %d\n", INT_FAST8_MIN <= -127 && INT_FAST8_MAX >= 127,
           INT_FAST16_MIN <= -32767 && INT_FAST16_MAX >= 32767,
           INT_FAST32_MIN <= -2147483647 && INT_FAST32_MAX >= 2147483647,
           INT_FAST64_MIN <= -9223372036854775807 && INT_FAST64_MAX >= 9223372036854775807);
    printf("hold %d %d\n", (int_fast8_t)INT_FAST8_MIN == INT_FAST8_MIN, (int_fast64_t)INT_FAST64_MAX == INT_FAST64_MAX);
    return 0;
}
