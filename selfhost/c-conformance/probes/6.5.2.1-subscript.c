// 6.5.2.1p2-4  E1[E2] is *((E1)+(E2)); string-literal subscript; multidimensional arrays row-major
#include <stdio.h>
int main(void) {
    int a[3][4];
    for (int i = 0; i < 3; i++) for (int j = 0; j < 4; j++) a[i][j] = i * 10 + j;
    int *p = &a[0][0];
    printf("%d %d %d\n", a[2][3], p[7], (int)sizeof a[1]);
    printf("%c\n", 1["abc"]);
    return 0;
}
