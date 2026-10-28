// 6.2.3p1  labels, tags, members and ordinary identifiers are separate name spaces
#include <stdio.h>
struct t { int t; };
int main(void) {
    struct t t;
    t.t = 3;
    int s = 0;
    goto t;
t:  s += t.t;
    union u { int u; } u = { 4 };
    enum e { E1 = 2 } e = E1;
    struct e2 { int e; } q = { 5 };
    printf("%d %d %d %d\n", s, u.u, (int)e, q.e);
    return 0;
}
