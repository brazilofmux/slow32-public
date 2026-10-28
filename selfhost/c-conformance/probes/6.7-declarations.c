// 6.7p1-7  multi-declarator declarations, tentative-then-real definition
#include <stdio.h>
int t; int t = 3;
int a = 1, *pa = &a, arr[2] = { 5, 6 };
int f(void) { return 12; }
int main(void) {
    int x = 2, y = x * 3, *px = &x;
    printf("%d %d %d %d %d\n", t, *pa + arr[1], f(), y, *px);
    return 0;
}
