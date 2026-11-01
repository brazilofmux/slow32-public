/* 7.8.1p2-p3: the d, u and x PRI macros for the exact-width types print each width's extremes */
#include <inttypes.h>
#include <stdio.h>

int main(void) {
    int8_t a = INT8_MIN; int16_t b = INT16_MIN; int32_t c = INT32_MIN; int64_t d = INT64_MIN;
    uint8_t e = UINT8_MAX; uint16_t f = UINT16_MAX; uint32_t g = UINT32_MAX; uint64_t h = UINT64_MAX;
    printf("%" PRId8 " %" PRId16 " %" PRId32 " %" PRId64 "\n", a, b, c, d);
    printf("%" PRId8 " %" PRId16 " %" PRId32 " %" PRId64 "\n", (int8_t)INT8_MAX, (int16_t)INT16_MAX, (int32_t)INT32_MAX, (int64_t)INT64_MAX);
    printf("%" PRIu8 " %" PRIu16 " %" PRIu32 " %" PRIu64 "\n", e, f, g, h);
    printf("%" PRIx8 " %" PRIx16 " %" PRIx32 " %" PRIx64 "\n", e, f, g, h);
    printf("%" PRIx64 " %" PRId32 "|\n", (uint64_t)0x123456789abcdef0u, (int32_t)-7);
    return 0;
}
