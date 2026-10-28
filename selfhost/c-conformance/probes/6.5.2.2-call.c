// 6.5.2.2p4-7,10  argument conversion, default argument promotions in varargs, call via pointer, recursion
#include <stdio.h>
#include <stdarg.h>
static double half(double x) { return x / 2; }
static int sumv(int n, ...) {
    va_list ap; va_start(ap, n);
    int s = 0;
    for (int i = 0; i < n; i++) s += va_arg(ap, int);
    va_end(ap); return s;
}
static double dv(int n, ...) { va_list ap; va_start(ap, n); double d = va_arg(ap, double); va_end(ap); return d * n; }
int main(void) {
    char c = 3; short s = 4;
    double (*fp)(double) = half;
    float f = 1.5f;
    printf("%.2f %.2f\n", half(5), (*fp)(7));   /* int converted to double by prototype */
    printf("%d %.2f\n", sumv(3, c, s, 'a'), dv(2, f)); /* char/short->int, float->double */
    return 0;
}
