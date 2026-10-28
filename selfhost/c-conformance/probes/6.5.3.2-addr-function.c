// 6.5.3.2p1,3  & applied to a function designator yields a pointer to the function
#include <stdio.h>
static int f(int x) { return x * 2; }
int main(void) {
    int (*fp)(int) = &f;
    printf("%d %d\n", fp(4), (*fp)(5));
    return 0;
}
