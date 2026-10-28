// 6.8.5p3  register is a permitted storage class in a for clause-1 declaration
#include <stdio.h>
int main(void) {
    int sum = 0;
    for (register int r = 0; r < 4; r++) sum += r;
    printf("%d\n", sum);
    return 0;
}
