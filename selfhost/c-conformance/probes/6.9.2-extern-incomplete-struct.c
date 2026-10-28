// 6.9.2p1-2  an extern declaration of an object of a not-yet-complete struct type, defined after completion
#include <stdio.h>
struct pt;
extern struct pt pv;
static struct pt *addr(void) { return &pv; }
struct pt { int x, y; };
struct pt pv = { 3, 4 };
int main(void) {
    printf("%d\n", addr()->x + pv.y);
    return 0;
}
