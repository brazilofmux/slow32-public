// 6.10.1p4  #if arithmetic is done in intmax_t/uintmax_t
#include <stdio.h>
int main(void) {
    int r = 0;
#if 0x7FFFFFFF + 1 > 0          /* no 32-bit overflow: intmax_t is 64 bits */
    r |= 1;
#endif
#if 0xFFFFFFFFFFFFFFFF == -1    /* both unsigned max */
    r |= 4;
#endif
#if (1 << 40) == 0x10000000000
    r |= 8;
#endif
#if (7 / 2 == 3) && (-7 % 3 == -1) && (5 ^ 3) == 6 && (~0 == -1)
    r |= 16;
#endif
#if (1 ? 2 : 3) == 2 && (0 ? 2 : 3) == 3
    r |= 32;
#endif
#if 9223372036854775807 > 0 && -9223372036854775807 - 1 < 0
    r |= 64;
#endif
    printf("%d\n", r);
    return 0;
}
