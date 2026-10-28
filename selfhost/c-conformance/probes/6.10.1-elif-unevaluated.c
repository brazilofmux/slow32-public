// 6.10.1p6  after a true group, later #elif expressions are not evaluated
#include <stdio.h>
int main(void) {
    int r = 0;
#if 1
    r = 1;
#elif 1/0
    r = 2;
#endif
    printf("%d\n", r);
    return 0;
}
