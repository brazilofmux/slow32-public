// 6.2.4p8  a function call's non-lvalue struct result with an array member: temporary lifetime
#include <stdio.h>
struct S { int a[3]; };
static struct S mk(int x) { struct S s = { { x, x + 1, x + 2 } }; return s; }
int main(void) {
    int v = mk(5).a[2];
    printf("%d\n", v);
    return 0;
}
