/* 7.20.1.2: int_leastN_t and uint_leastN_t hold at least N bits, and their limit macros match them */
#include <stdint.h>
#include <stdio.h>

int main(void) {
    printf("umax %d %d %d %d\n", (uint_least8_t)-1 == UINT_LEAST8_MAX, (uint_least16_t)-1 == UINT_LEAST16_MAX,
           (uint_least32_t)-1 == UINT_LEAST32_MAX, (uint_least64_t)-1 == UINT_LEAST64_MAX);
    printf("wide enough %d %d %d %d\n", UINT_LEAST8_MAX >= 255, UINT_LEAST16_MAX >= 65535,
           UINT_LEAST32_MAX >= 4294967295u, UINT_LEAST64_MAX >= 18446744073709551615u);
    printf("signed %d %d %d %d\n", INT_LEAST8_MIN <= -127 && INT_LEAST8_MAX >= 127,
           INT_LEAST16_MIN <= -32767 && INT_LEAST16_MAX >= 32767,
           INT_LEAST32_MIN <= -2147483647 && INT_LEAST32_MAX >= 2147483647,
           INT_LEAST64_MIN <= -9223372036854775807 && INT_LEAST64_MAX >= 9223372036854775807);
    printf("hold %d %d\n", (int_least8_t)INT_LEAST8_MIN == INT_LEAST8_MIN, (int_least64_t)INT_LEAST64_MAX == INT_LEAST64_MAX);
    return 0;
}
