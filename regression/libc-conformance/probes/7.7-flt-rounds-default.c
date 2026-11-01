/* 7.7, 5.2.4.2.2p8: FLT_ROUNDS is 1 (to nearest) in the default rounding mode of an IEC 60559 implementation (F.8.3) */
#include <float.h>
#include <stdio.h>

int main(void) {
    int r = FLT_ROUNDS;
    printf("FLT_ROUNDS == 1: %d\n", r == 1);
    return 0;
}
