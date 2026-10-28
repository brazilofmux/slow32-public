// 6.5.6p8  integer + pointer (integer on the left) scales by the element size like pointer + integer
#include <stdio.h>
int main(void) {
    int ia[4] = {5, 6, 7, 8}; double da[4];
    int *q = 3 + ia; double *r = 2 + da;
    printf("%d %d %d\n", *q, (int)(q - ia), (int)(r - da));
    return 0;
}
