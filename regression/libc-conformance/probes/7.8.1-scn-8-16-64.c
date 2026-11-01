/* 7.8.1p4-p5: the SCN macros for 8, 16 and 64 bits, and the i and o forms, store only into an object of their type */
#include <inttypes.h>
#include <stdio.h>

int main(void) {
    struct { int8_t a; int8_t guard1; } s8 = { 0, 99 };
    struct { uint16_t a; uint16_t guard2; } s16 = { 0, 999 };
    int64_t c = 0;
    uint64_t d = 0;
    int32_t i = 0;
    uint32_t o = 0;
    int n = sscanf("-100 65535 -9223372036854775807 ffffffffffffffff 0x1f 777", "%" SCNd8 " %" SCNu16 " %" SCNd64
                   " %" SCNx64 " %" SCNi32 " %" SCNo32, &s8.a, &s16.a, &c, &d, &i, &o);
    printf("%d %d %d %u %u %lld %llu %ld %lu\n", n, s8.a, s8.guard1, (unsigned)s16.a, (unsigned)s16.guard2,
           (long long)c, (unsigned long long)d, (long)i, (unsigned long)o);
    return 0;
}
