// 6.4.1p1  _Alignas and _Alignof keywords recognized (semantics in 6.2.8 probes)
#include <stdio.h>
int main(void) {
    _Alignas(int) char c[4] = { 1, 2, 3, 4 };
    printf("%d %d\n", (int)_Alignof(char), c[3]);
    return 0;
}
