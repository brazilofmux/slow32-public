/* 7.8.1p2-p3: the i, o and X PRI macros for every exact width */
#include <inttypes.h>
#include <stdio.h>

int main(void) {
    printf("%" PRIi8 " %" PRIi16 " %" PRIi32 " %" PRIi64 "\n", (int8_t)-5, (int16_t)-300, (int32_t)-70000, (int64_t)-5000000000);
    printf("%" PRIo8 " %" PRIo16 " %" PRIo32 " %" PRIo64 "\n", (uint8_t)0377, (uint16_t)0177777, (uint32_t)037777777777, (uint64_t)01777777777777777777777);
    printf("%" PRIX8 " %" PRIX16 " %" PRIX32 " %" PRIX64 "\n", (uint8_t)0xab, (uint16_t)0xabcd, (uint32_t)0xabcdef12, (uint64_t)0xabcdef1234567890);
    return 0;
}
