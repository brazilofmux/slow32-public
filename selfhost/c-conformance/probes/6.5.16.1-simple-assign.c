// 6.5.16.1p2  simple assignment converts; struct copy; void* round trip; adding qualifiers
#include <stdio.h>
struct B { int a[5]; char c; };
int main(void) {
    struct B s = {{1, 2, 3, 4, 5}, 'z'}, t;
    t = s;
    s.a[0] = 99;
    int x = 0;
    void *v = &x; int *ip = v; const int *cip = ip;
    char ch; ch = 0x141;
    double d; d = 3; int i; i = 2.9;
    printf("%d %d %c %d %d %.1f %d\n", t.a[0], t.a[4], t.c, cip == &x, ch, d, i);
    return 0;
}
