// 6.5.2.3p3-6  . and -> on structs/unions, qualifiers propagate, common initial sequence
#include <stdio.h>
struct A { int tag; int a; };
struct B { int tag; double b; };
union U { struct A a; struct B b; };
struct In { int x; struct { int y; } s; };
int main(void) {
    union U u;
    u.a.tag = 42;
    struct In in = { 1, { 2 } };
    struct In *p = &in;
    p->s.y += 5;
    const struct In *cp = &in;
    printf("%d %d %d %d\n", u.b.tag, p->x, in.s.y, (int)sizeof cp->x);
    return 0;
}
