// 6.8.6.1p3  jumping into the middle of a loop body then iterating normally (cf. the standard's example)
#include <stdio.h>
int main(void) {
    int i = 0, s = 0;
    goto mid;
    while (i < 5) {
        s += 100;
    mid:
        s += 1;
        i++;
    }
    printf("%d %d\n", i, s);
    return 0;
}
