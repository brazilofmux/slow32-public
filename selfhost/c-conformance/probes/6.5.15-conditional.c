// 6.5.15p4-6  ?: evaluates one arm; arithmetic arms convert; null/void* arms; struct arms; lvalue-ness
#include <stdio.h>
struct P { int x, y; };
int main(void) {
    volatile int t = 1, f = 0;
    int a = 0, b = 0;
    t ? a++ : b++;
    double r = t ? 1 : 2.5;
    printf("%d %d %.1f %d\n", a, b, r, (int)sizeof(f ? 'a' : 1.0f));
    int x = 4; int *p = f ? 0 : &x; void *vp = &x;
    printf("%d %d\n", *p, *(int *)(t ? vp : p));
    struct P s1 = {1, 2}, s2 = {3, 4};
    struct P s3 = f ? s1 : s2;
    printf("%d\n", s3.x);
    long long ll = t ? 1u : -1LL; printf("%lld\n", ll);
    return 0;
}
