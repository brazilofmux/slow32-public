// 6.10.3p10  a function-like macro name not followed by ( is not an invocation
#include <stdio.h>
#define twice(x) ((x) * 2)
static int (twice)(int x) { return x + 1000; }   /* parenthesized: not expanded */
int main(void) {
    int (*fp)(int) = twice;                       /* no ( follows: the function */
    printf("%d %d %d\n", twice(3), (twice)(3), fp(3));
    return 0;
}
