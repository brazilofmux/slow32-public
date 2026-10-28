// 6.10.3.1p1  arguments are fully expanded before substitution, except next to ##
#include <stdio.h>
#define VAL 42
#define CAT(a, b) a ## b
#define XCAT(a, b) CAT(a, b)
#define VAL_1 7
#define ID(x) x
#define TWICE(x) ((x) + (x))
int main(void) {
    int VAL1 = 5;
    printf("%d %d\n", CAT(VAL, 1), XCAT(VAL_, 1));   /* VAL1 -> 5 ; VAL_1 -> 7 */
    printf("%d %d\n", ID(ID(ID(VAL))), TWICE(TWICE(VAL)));
    return 0;
}
