// 6.8.6.4p3  the returned value is converted as if by assignment to the function's return type
#include <stdio.h>
static unsigned char uc(int x) { return x; }
static signed char sc(int x) { return x; }
static int fromd(double d) { return d; }
static double fromi(int i) { return i; }
static float fromd2f(double d) { return d; }
static long long fromu(unsigned u) { return u; }
static unsigned short us(long long v) { return v; }
static const void *vp(int *p) { return p; }
struct pt { int x, y; char tag[5]; };
static struct pt mk(int a) { struct pt p = { a, a * 2, "abc" }; return p; }
static void nothing(void) { return; }
int main(void) {
    int v = 3;
    struct pt p = mk(21);
    nothing();
    printf("%d %d %d %.1f %d %lld %u %d\n", uc(300), sc(200), fromd(-7.9), fromi(5),
           fromd2f(0.1) == 0.1f, fromu(0xFFFFFFFFu), us(0x123456789LL),
           vp(&v) == (void *)&v);
    printf("%d %d %s\n", p.x, p.y, p.tag);
    return 0;
}
