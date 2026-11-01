/* 7.20.2p2: each limit macro has the type of an object of its type after the integer promotions */
#include <stdint.h>
#include <stdio.h>

#define SAME(m, t) (sizeof(m) == sizeof(+(t)0) && ((m) * 0 - 1 < 0) == (+(t)0 - 1 < 0))

int main(void) {
    printf("8 %d %d %d\n", SAME(INT8_MIN, int8_t), SAME(INT8_MAX, int8_t), SAME(UINT8_MAX, uint8_t));
    printf("16 %d %d %d\n", SAME(INT16_MIN, int16_t), SAME(INT16_MAX, int16_t), SAME(UINT16_MAX, uint16_t));
    printf("32 %d %d %d\n", _Generic(INT32_MIN, int32_t: 1, default: 0), _Generic(INT32_MAX, int32_t: 1, default: 0),
           _Generic(UINT32_MAX, uint32_t: 1, default: 0));
    printf("64 %d %d %d\n", _Generic(INT64_MIN, int64_t: 1, default: 0), _Generic(INT64_MAX, int64_t: 1, default: 0),
           _Generic(UINT64_MAX, uint64_t: 1, default: 0));
    printf("max %d %d %d\n", _Generic(INTMAX_MIN, intmax_t: 1, default: 0), _Generic(INTMAX_MAX, intmax_t: 1, default: 0),
           _Generic(UINTMAX_MAX, uintmax_t: 1, default: 0));
    return 0;
}
