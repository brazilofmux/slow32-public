/* 7.20.2p2: the exact-width and greatest-width limit macros are usable in #if with their exact values */
#include <stdint.h>
#include <stdio.h>

#if INT8_MIN == -128 && INT8_MAX == 127 && UINT8_MAX == 255 && INT16_MIN == -32768 && INT16_MAX == 32767 \
    && UINT16_MAX == 65535 && INT32_MIN == -2147483647 - 1 && INT32_MAX == 2147483647 && UINT32_MAX == 4294967295 \
    && INT64_MIN == -9223372036854775807 - 1 && INT64_MAX == 9223372036854775807 \
    && UINT64_MAX == 18446744073709551615u && INTMAX_MAX >= INT64_MAX && UINTMAX_MAX >= UINT64_MAX
#define OK 1
#else
#define OK 0
#endif

int main(void) {
    printf("%d\n", OK);
    return 0;
}
