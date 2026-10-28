// 6.10.8.3p1  without __STDC_NO_ATOMICS__, _Atomic objects work
#include <stdio.h>
int main(void) {
#ifndef __STDC_NO_ATOMICS__
    _Atomic int a = 5;
    a += 2;
    printf("%d\n", (int)a);
#else
    printf("%d\n", 7);
#endif
    return 0;
}
