// 6.7.2.1p9  a member of array-of-array type: layout, subscripts, initializers at file and block scope
#include <stdio.h>
struct T { int t; short m[2][2][3]; int u; };
struct T gt = { 5, { { {1, 2, 3}, {4, 5, 6} }, { {7, 8, 9}, {10, 11, 12} } }, 99 };
int main(void) {
    struct T lt = { 6, { { {21, 22, 23} }, { {31}, {41, 42} } }, 77 };
    struct { char k; short m2[2][3]; } l2 = { 'x', { {1, 2}, {3} } };
    printf("%d %d %d %d %u %u\n", gt.m[1][0][2], gt.m[1][1][0], gt.u, lt.m[1][1][1], (unsigned)sizeof gt, (unsigned)sizeof gt.m[1]);
    printf("%d %d %d %d %d\n", lt.m[0][0][1], lt.m[1][0][0], lt.u, l2.m2[0][1], l2.m2[1][0]);
    return 0;
}
