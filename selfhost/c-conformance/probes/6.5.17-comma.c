// 6.5.17p2  comma: left evaluated as void then sequenced; result is right operand's type and value, not an lvalue
#include <stdio.h>
static int g;
static int f(int x) { g = x; return x; }
int main(void) {
    int a = 1, b;
    b = (a++, a += 10, a * 2);
    int r = (f(3), g + 1);
    printf("%d %d %d %d\n", a, b, r, (int)sizeof(0, (char)1));
    char arr[10];
    printf("%d\n", (int)sizeof(0, arr));   /* array converts to pointer */
    return 0;
}
