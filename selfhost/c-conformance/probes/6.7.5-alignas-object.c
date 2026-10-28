// 6.7.5p2,p6  _Alignas on objects (by constant and by type) raises alignment
#include <stdio.h>
#include <stdint.h>
_Alignas(16) static char buf[3];
_Alignas(double) static char dbl[8];
int main(void) {
    _Alignas(32) int local = 5;
    printf("%d %d %d %d\n", (int)((uintptr_t)buf % 16), (int)((uintptr_t)dbl % 8),
           (int)((uintptr_t)&local % 32), local);
    return 0;
}
