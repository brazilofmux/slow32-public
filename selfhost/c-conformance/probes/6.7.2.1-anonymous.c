// 6.7.2.1p13  anonymous struct and union members are members of the containing object
#include <stdio.h>
struct v { int tag; union { int i; float f; }; struct { short x, y; }; };
int main(void) {
    struct v a = { .tag = 1, .i = 42, .x = 3, .y = 4 };
    struct v b; b.tag = 2; b.f = 1.5f; b.x = -1; b.y = 9;
    printf("%d %d %d %d\n", a.tag, a.i, a.x, a.y);
    printf("%d %d %d %d %u\n", b.tag, (int)(b.f * 2.0f), b.x, b.y, (unsigned)sizeof(struct v));
    return 0;
}
