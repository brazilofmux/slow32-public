// 6.3.2.1p4  &function_designator is a pointer to the function (block scope and static init)
#include <stdio.h>
static int twice(int x) { return 2 * x; }
static int (*g)(int) = &twice;
int main(void) {
    int (*fp)(int) = &twice;
    printf("%d %d %d\n", fp(4), g(5), fp == g);
    return 0;
}
