/* 7.20.1.1: intN_t and uintN_t have exactly N bits, no padding, and the signed ones two's complement */
#include <limits.h>
#include <stdint.h>
#include <stdio.h>

#define W(T, N) (sizeof(T) * CHAR_BIT == N)

int main(void) {
    printf("widths %d %d %d %d %d %d %d %d\n", W(int8_t, 8), W(uint8_t, 8), W(int16_t, 16), W(uint16_t, 16),
           W(int32_t, 32), W(uint32_t, 32), W(int64_t, 64), W(uint64_t, 64));
    printf("umax %d %d %d %d\n", (uint8_t)-1 == 255, (uint16_t)-1 == 65535, (uint32_t)-1 == 4294967295u,
           (uint64_t)-1 == 18446744073709551615u);
    printf("signed %d %d %d %d\n", (int8_t)-1 < 0, (int16_t)-1 < 0, (int32_t)-1 < 0, (int64_t)-1 < 0);
    printf("min %d %d %d %d\n", INT8_MIN == -128, INT16_MIN == -32768, INT32_MIN == -2147483647 - 1,
           INT64_MIN == -9223372036854775807 - 1);
    printf("max %d %d %d %d\n", INT8_MAX == 127, INT16_MAX == 32767, INT32_MAX == 2147483647,
           INT64_MAX == 9223372036854775807);
    printf("umacro %d %d %d %d\n", UINT8_MAX == 255, UINT16_MAX == 65535, UINT32_MAX == 4294967295u,
           UINT64_MAX == 18446744073709551615u);
    return 0;
}
