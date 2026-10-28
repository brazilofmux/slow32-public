// 6.10.3.3p3  ## can build a punctuator from two (+ ## = gives +=)
#include <stdio.h>
#define CAT(a, b) a ## b
int main(void) {
    int k = 3;
    k CAT(+, =) 4;
    k CAT(<, <=) 1;
    printf("%d\n", k);
    return 0;
}
