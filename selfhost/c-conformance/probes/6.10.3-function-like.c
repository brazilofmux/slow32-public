// 6.10.3p10,p11  function-like macros: nested parens, commas in parens, newlines in the call
#include <stdio.h>
#define ADD(a, b) ((a) + (b))
#define FIRST(a, b) a
#define CALL(f, x) f(x)
static int dbl(int v) { return 2 * v; }
static int pair(int a, int b) { return a * 10 + b; }
int main(void) {
    int x = ADD((1, 2), pair(3, 4));      /* (1,2) is one argument */
    int y = ADD(
                5,
                6
               );
    int z = FIRST((7, 8), 9);
    int w = CALL(dbl, ADD(1, 2));
    printf("%d %d %d %d\n", x, y, z, w);
    return 0;
}
