// 6.8.4.2p3  an enclosed switch has its own case/default labels; break leaves only the inner one
#include <stdio.h>
static int f(int a, int b) {
    int r = 0;
    switch (a) {
    case 1:
        switch (b) { case 1: r += 1; break; default: r += 2; }
        r += 10;
        break;
    default:
        switch (b) { case 1: r += 100; default: r += 200; }
    case 2:
        r += 1000;
    }
    return r;
}
int main(void) {
    for (int a = 0; a < 3; a++) for (int b = 0; b < 3; b++) printf("%d ", f(a, b));
    printf("\n");
    return 0;
}
