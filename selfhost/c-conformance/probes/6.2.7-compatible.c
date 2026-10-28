// 6.2.7p3  composite type: a later declaration completes an array size; array params adjust
#include <stdio.h>
extern int a[];
extern int a[3];
int f(int p[], double);
int f(int p[5], double y);
int f(int *p, double y) { return p[1] + (int)y; }
int (*pf)(int *, double) = f;
int a[] = { 1, 2, 3 };
int main(void) {
    printf("%d %d\n", (int)sizeof a, pf(a, 3.5));
    return 0;
}
