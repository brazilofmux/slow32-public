// 6.2.1p3  a label name has function scope: goto forward into a later block
#include <stdio.h>
int main(void) {
    int n = 0;
    goto later;
    n = 99;
    {
    later:
        n += 5;
    }
    printf("%d\n", n);
    return 0;
}
