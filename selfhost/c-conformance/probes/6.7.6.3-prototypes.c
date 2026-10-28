// 6.7.6.3p7-15  prototypes: array param adjustment, varargs, unnamed params, function returning fn pointer
#include <stdio.h>
#include <stdarg.h>
static int twice(int x) { return 2 * x; }
static int first(int a[10]) { return (int)sizeof a + a[0]; }
static int count(int n, ...) { va_list ap; va_start(ap, n); int s = 0; for (int i = 0; i < n; i++) s += va_arg(ap, int); va_end(ap); return s; }
static int unnamed(int, int b);
static int unnamed(int a, int b) { return a - b; }
static int (*pick(int w))(int) { return w ? twice : 0; }
static int apply(int (*f)(int), int x) { return f(x); }
int main(void) {
    int arr[2] = { 40, 1 };
    printf("%d %d %d %d %d\n", apply(twice, 21), first(arr), count(3, 1, 2, 3), unnamed(9, 4), pick(1)(8));
    return 0;
}
