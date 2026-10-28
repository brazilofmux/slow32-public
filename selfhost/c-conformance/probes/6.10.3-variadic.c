// 6.10.3p12,6.10.3.1p2  variadic macros: __VA_ARGS__ merges trailing args with their commas
#include <stdio.h>
#define COUNT_(a, b, c, d, n, ...) n
#define COUNT(...) COUNT_(__VA_ARGS__, 4, 3, 2, 1, 0)
#define PR(fmt, ...) printf(fmt, __VA_ARGS__)
#define FIRST(x, ...) x
static int sum3(int a, int b, int c) { return a + b + c; }
#define CALL3(f, ...) f(__VA_ARGS__)
#define ONLY(...) __VA_ARGS__
int main(void) {
    PR("%d %d %d\n", COUNT(a), COUNT(a, b), COUNT(a, (b, c), d));
    PR("%d %d %d\n", FIRST(9, 8, 7), CALL3(sum3, 1, 2, 3), ONLY(4));
    return 0;
}
