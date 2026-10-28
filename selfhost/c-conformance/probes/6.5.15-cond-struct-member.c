// 6.5.15p5  a ?: with struct arms yields a struct value whose member may be selected
#include <stdio.h>
struct P { int x, y; };
int main(void) {
    volatile int t = 1;
    struct P a = {1, 2}, b = {3, 4};
    printf("%d %d\n", (t ? a : b).y, (!t ? a : b).x);
    return 0;
}
