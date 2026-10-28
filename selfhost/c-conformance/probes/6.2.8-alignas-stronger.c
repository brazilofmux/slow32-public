// 6.2.8p3  an extended/stronger alignment from _Alignas is honoured for objects
#include <stdio.h>
#include <stdint.h>
_Alignas(16) static char g[3];
struct w { char c; _Alignas(8) int i; };
int main(void) {
    _Alignas(32) char l[5];
    printf("%d %d %d %d\n", (int)((uintptr_t)g % 16), (int)((uintptr_t)l % 32),
           (int)_Alignof(struct w), (int)sizeof(struct w));
    return 0;
}
