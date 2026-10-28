// 6.10.3p10  the ( of an invocation may come on a later line
#include <stdio.h>
#define SQ(x) ((x) * (x))
int main(void) {
    int v = SQ
        (7);
    printf("%d\n", v);
    return 0;
}
