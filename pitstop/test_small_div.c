#include <stdint.h>
#include <stdio.h>

extern uint64_t __udivdi3(uint64_t, uint64_t);
extern uint64_t __umoddi3(uint64_t, uint64_t);

int main(void) {
    uint64_t value = 1234ULL;
    uint64_t q = __udivdi3(value, 10ULL);
    uint64_t r = __umoddi3(value, 10ULL);
    printf("q=%llu r=%llu\n", (unsigned long long)q, (unsigned long long)r);
    return 0;
}
