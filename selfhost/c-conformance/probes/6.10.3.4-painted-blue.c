// 6.10.3.4p2  a name left unreplaced stays unreplaceable when rescanned later
#include <stdio.h>
static int g(int x) { return x + 1000; }
#define g(x) (2 * g(x))
#define ID(x) x
int main(void) {
    /* g(x) produces a g token that must never expand again, even via ID */
    printf("%d %d\n", g(3), ID(g(3)));
    return 0;
}
