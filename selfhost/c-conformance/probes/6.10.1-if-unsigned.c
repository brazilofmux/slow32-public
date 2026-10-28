// 6.10.1p4  #if follows the usual conversions in uintmax_t: -1 < 0u is false
#include <stdio.h>
int main(void) {
    int r = 0;
#if -1 < 0u
    r |= 1;
#endif
#if -1 > 0u
    r |= 2;
#endif
#if (0u - 1) > 0
    r |= 4;
#endif
#if 0xFFFFFFFFFFFFFFFF / 2 == 0x7FFFFFFFFFFFFFFF
    r |= 8;
#endif
#if (-1 >> 1) == -1 && (0xFFFFFFFFFFFFFFFF >> 63) == 1
    r |= 16;
#endif
    printf("%d\n", r);
    return 0;
}
