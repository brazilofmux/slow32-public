// 6.7.9p6-7,17-19  designated initializers: array index, member, nested, override, continuation
#include <stdio.h>
struct p { int x, y, z; };
struct q { struct p a[3]; int t; };
int main(void) {
    int a[6] = { [4] = 40, 50, [1] = 10, [4] = 44 };
    struct p p = { .z = 3, .x = 1 };
    struct q q = { .a[1].y = 7, .a[2] = { 8, 9 }, .t = 99, .a[0].x = 5 };
    int b[] = { [9] = 1 };
    printf("%d %d %d %d %d %d\n", a[0], a[1], a[4], a[5], p.x + p.z, p.y);
    printf("%d %d %d %d %d %u\n", q.a[1].y, q.a[2].x, q.a[2].y, q.t, q.a[0].x, (unsigned)(sizeof b / sizeof b[0]));
    return 0;
}
