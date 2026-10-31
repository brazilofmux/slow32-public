// 6.5.4p2, 6.7.7  a cast to a pointer to an array: subscripts and steps move by whole rows
#include <stdio.h>
#include <stdarg.h>
int flat[24];
static int rows(int n, ...) {
    va_list ap;
    int s;
    va_start(ap, n);
    s = va_arg(ap, int (*)[3])[1][2];
    s = s + va_arg(ap, int (*)[2][3])[1][0][1];
    va_end(ap);
    return s;
}
int main(void) {
    int i;
    void *v = flat;
    for (i = 0; i < 24; i++) flat[i] = i * 3;
    printf("%d %d %d\n", ((int (*)[4])flat)[2][1], ((int (*)[2][3])v)[3][1][2], (*((int (*)[4])flat + 3))[2]);
    printf("%u %u %d\n", (unsigned)sizeof *(int (*)[5])v, (unsigned)sizeof ((int (*)[2][3])v)[1],
           (int)(((int (*)[4])flat + 2) - (int (*)[4])flat));
    printf("%d\n", rows(0, (int (*)[3])flat, (int (*)[2][3])flat));
    return 0;
}
