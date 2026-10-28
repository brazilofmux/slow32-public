// 6.3.2.1p4  a function designator decays to a pointer, except as operand of sizeof/& ; *f is f again
#include <stdio.h>
static int twice(int x) { return 2 * x; }
int main(void) {
    int (*fp)(int) = twice;
    int (*fp3)(int) = *twice;
    int (*fp4)(int) = **twice;
    printf("%d %d %d %d\n", fp(3), (*fp3)(5), fp4(6), (*twice)(1));
    return 0;
}
