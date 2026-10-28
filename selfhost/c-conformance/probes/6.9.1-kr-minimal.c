// 6.9.1p6  the smallest old-style definition: one int parameter declared in the declaration list
#include <stdio.h>
int twice(x) int x; { return 2 * x; }
int main(void) {
    printf("%d\n", twice(21));
    return 0;
}
