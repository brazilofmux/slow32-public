// 6.5.3p1  unary operators nest: -~!x, *&*p, sizeof -x, - -x
#include <stdio.h>
int main(void) {
    int x = 5; int *p = &x;
    printf("%d %d %d %d %d\n", -~!x, *&*p, (int)sizeof -x, - -x, !!x);
    return 0;
}
