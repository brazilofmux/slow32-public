// 6.10.3p4  an argument with no tokens is valid; a plain parameter then vanishes
#include <stdio.h>
#define OR0(a) (a + 0)
#define MID(a, b, c) (a b c)
int main(void) {
    printf("%d %d %d\n", OR0(), MID(1, +, 2), MID(, 3, ));
    return 0;
}
