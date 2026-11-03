/* A function-pointer signature with no parameters took no entry of the
 * parameter-type pool, so it shared its base with the next signature,
 * and the return type and variadic flag kept by base were that one's:
 * `double (*f)(void)` then `int (*g)(int)` made f() an int call, and
 * this printed garbage. */
#include <stdio.h>
static double half(void) { return 0.5; }
static int twice(int x) { return 2 * x; }
struct ops { double (*f)(void); int (*g)(int); };
int main(void) {
    double (*f)(void) = half;
    int (*g)(int) = twice;
    struct ops o;
    o.f = half; o.g = twice;
    printf("%g %d %g %d\n", f(), g(3), o.f(), o.g(4));
    return f() == 0.5 && o.f() == 0.5 ? 0 : 1;
}
