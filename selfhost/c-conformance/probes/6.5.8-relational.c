// 6.5.8p5-6  relational operators yield int 0/1; pointers within an array; mixed types
#include <stdio.h>
int main(void) {
    int a[5]; int *p = &a[1], *q = &a[3];
    double d = 1.5; long long ll = -1;
    printf("%d %d %d %d\n", p < q, q <= p, p >= p, (int)sizeof(p < q));
    printf("%d %d %d\n", d > 1, ll < 0, -1 < 0u);
    struct { int x, y; } s; printf("%d\n", &s.x < &s.y);
    return 0;
}
