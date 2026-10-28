// 6.8.4.2p4-5  jump to matching case, fall-through, default anywhere, no match and no default runs nothing
#include <stdio.h>
static int f(int x) {
    int r = 0;
    switch (x) {
    case 1: r += 1;
    default: r += 10;
    case 2: r += 100; break;
    case -5: r += 1000;
    case 'A': r += 5;
    }
    return r;
}
static int g(int x) {
    int r = 7;
    switch (x) { case 1: r = 1; break; case 2: r = 2; }
    switch (x) { }
    return r;
}
int main(void) {
    for (int i = -6; i <= 66; i++) printf("%d ", f(i));
    printf("\n%d %d %d\n", g(1), g(2), g(9));
    return 0;
}
