/* 7.20.4.1, 7.20.4.2: INTN_C, UINTN_C, INTMAX_C and UINTMAX_C give constants of the promoted least types, usable in #if */
#include <stdint.h>
#include <stdio.h>

#define SAME(m, t) (sizeof(m) == sizeof(+(t)0) && ((m) * 0 - 1 < 0) == (+(t)0 - 1 < 0))

#if INT64_C(1) << 40 == 1099511627776 && UINT64_C(18446744073709551615) == 18446744073709551615u \
    && INTMAX_C(9223372036854775807) > 0 && UINT32_C(4294967295) == 4294967295
#define PP 1
#else
#define PP 0
#endif

int main(void) {
    printf("#if %d\n", PP);
    printf("values %d %d %d %d\n", INT8_C(127) == 127, UINT16_C(65535) == 65535, INT32_C(2147483647) == 2147483647,
           UINT64_C(0xFFFFFFFFFFFFFFFF) == 18446744073709551615u);
    printf("small %d %d %d %d\n", SAME(INT8_C(1), int_least8_t), SAME(UINT8_C(1), uint_least8_t),
           SAME(INT16_C(1), int_least16_t), SAME(UINT16_C(1), uint_least16_t));
    printf("32 %d %d\n", SAME(INT32_C(1), int_least32_t), SAME(UINT32_C(1), uint_least32_t));
    printf("64 %d %d\n", _Generic(INT64_C(1), int_least64_t: 1, default: 0), _Generic(UINT64_C(1), uint_least64_t: 1, default: 0));
    printf("max %d %d\n", _Generic(INTMAX_C(1), intmax_t: 1, default: 0), _Generic(UINTMAX_C(1), uintmax_t: 1, default: 0));
    printf("big %d\n", INT64_C(4000000000) * 4 == 16000000000);
    return 0;
}
