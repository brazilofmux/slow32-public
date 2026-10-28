// 6.4.6p1  all punctuators: compound assignment, shifts, ternary, ellipsis, ->, ...
#include <stdio.h>
#include <stdarg.h>
struct P { int x; };
static int sum(int n, ...) { va_list ap; va_start(ap, n); int s = 0; while (n--) s += va_arg(ap, int); va_end(ap); return s; }
int main(void) {
    int a = 100;
    struct P p = { 3 }, *pp = &p;
    a += 1; a -= 2; a *= 3; a /= 2; a %= 50; a <<= 2; a >>= 1; a &= 0xff; a |= 0x100; a ^= 0x3;
    int b = !a + ~a + (a ? 1 : 2) + (a != 0) + (a == 0) + (a <= 1) + (a >= 1) + (a < 2) + (a > 2);
    int c = (a && b) + (a || b) + (a & 1) + (a | 1) + (a ^ 1);
    printf("%d %d %d %d %d\n", a, b, c, pp->x, sum(3, 1, 2, 3));
    return 0;
}
