// 6.5.8p6  a relational operator's result has type int, whatever its operands: it does not make the arithmetic around it unsigned
#include <stdio.h>
int main(void) {
    volatile unsigned u = 1, v = 2;
    volatile int n = -5;
    int a = (n + (u < v)) < 0;          /* -5 + 1 = -4: negative */
    int b = (u < v) > -1;               /* int 1 > int -1 */
    int c = (u == v) - 1 < 0;           /* 0 - 1 = -1 */
    printf("%d %d %d\n", a, b, c);
    return 0;
}
