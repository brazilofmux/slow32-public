// 6.4.4.4p10  multi-character constant value is implementation-defined; checks agreement with clang
#include <stdio.h>
int main(void) {
#pragma clang diagnostic ignored "-Wmultichar"
    int m = 'ab';
    printf("%d\n", m);
    return 0;
}
