// 6.7.9p9,17/6.7.2.1  a brace initializer for a struct containing named bit-fields
#include <stdio.h>
struct b { unsigned a : 3; int s : 5; int y; };
static struct b g = { 5, -2, 7 };
int main(void) {
    struct b l = { 3, 4, 9 }, z = { 0 }, d = { .s = -1 };
    printf("%u %d %d %u %d %d %d %d\n", g.a, g.s, g.y, l.a, l.s + l.y, z.s, d.s, (int)d.a);
    return 0;
}
