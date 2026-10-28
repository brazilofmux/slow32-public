// 6.2.4p4  _Thread_local object has thread storage duration (single-threaded program)
#include <stdio.h>
_Thread_local int tl = 3;
static _Thread_local int tl2;
int main(void) {
    tl += 4; tl2 = tl * 2;
    printf("%d %d\n", tl, tl2);
    return 0;
}
