// 6.7.1p3  _Thread_local at file scope and with static at block scope
#include <stdio.h>
_Thread_local int tl = 7;
static _Thread_local int tl2;
int bump(void) { static _Thread_local int k = 1; return k++; }
int main(void) {
    tl += 3; tl2 = 2;
    int a = bump(); int b = bump();
    printf("%d %d %d %d\n", tl, tl2, a, b);
    return 0;
}
