// 6.3.1.2p1/6.7.9p10  a static _Bool initialized from nonzero values holds 1: integers, floating, an address
#include <stdio.h>
static int g;
_Bool b1 = 2, b2 = 0.5, b4 = 0;
static _Bool ba[4] = { 3, 0, 0.25, &g };
struct s { char c; _Bool b; } sv = { 1, 256 };
int main(void) {
    static _Bool lb = 7;
    static struct s ls = { 0, 9 };
    printf("%d %d %d %d %d %d %d %d %d\n", b1, b2, b4, ba[0], ba[2], ba[3], sv.b, lb, ls.b);
    return 0;
}
