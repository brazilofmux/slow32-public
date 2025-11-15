#include <stdint.h>
#include <stdio.h>

extern uint64_t __udivdi3(uint64_t, uint64_t);

int main(void) {
    union {
        uint64_t ll;
        struct {
            uint32_t lo;
            uint32_t hi;
        } parts;
    } q = {0};

    q.ll = __udivdi3(1234ULL, 10ULL);
    printf("lo=%u hi=%u\n", q.parts.lo, q.parts.hi);
    return 0;
}
