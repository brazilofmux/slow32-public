// 6.5.13p3-4 6.5.14p3-4  && and || yield int 0/1, short-circuit, accept pointers and doubles
#include <stdio.h>
int main(void) {
    int cnt = 0; int *np = 0; int x; double d = 0.5;
    int r1 = np && (cnt++, 1);
    int r2 = &x || (cnt++, 1);
    int r3 = d && 2.0;
    int r4 = 0.0 || np;
    printf("%d %d %d %d %d %d\n", r1, r2, r3, r4, cnt, (int)sizeof(d && d));
    printf("%d\n", 5 && 7);
    return 0;
}
