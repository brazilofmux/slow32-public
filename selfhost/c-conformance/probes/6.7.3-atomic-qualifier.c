// 6.7.3p3,p5  _Atomic as a qualifier: atomic int and pointer-to-atomic
#include <stdio.h>
_Atomic int a = 1;
int main(void) {
    _Atomic int *p = &a;
    *p += 4; a++;
    int _Atomic b = a * 3;
    printf("%d %d\n", (int)a, (int)b);
    return 0;
}
