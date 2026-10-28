// 6.5.6p8-9  pointer + integer, pointer - pointer (ptrdiff_t), one-past-end
#include <stdio.h>
#include <stddef.h>
struct T { int a; char b[6]; };
int main(void) {
    struct T arr[5];
    struct T *p = arr, *e = arr + 5;
    ptrdiff_t d = e - p;
    double da[4]; double *q = da + 3;
    printf("%d %d %d\n", (int)d, (int)(q - da), (int)(&arr[4] - &arr[1]));
    printf("%d %d\n", (int)sizeof(e - p), (int)((char *)e - (char *)p));
    unsigned u = 3; printf("%u %d\n", u - 5u > 0, 5 - 7);
    return 0;
}
