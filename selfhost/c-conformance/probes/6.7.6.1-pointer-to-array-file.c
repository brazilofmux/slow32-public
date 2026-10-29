// 6.7.6.1p1  file-scope pointers to arrays, declared alone and in a list with other declarators
#include <stdio.h>
static int m[3][3] = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };
int (*pa)[3], x = 2, (*pb)[3];
int main(void) {
    pa = m + 1;
    pb = &m[2];
    pa++;
    printf("%d %d %u\n", pa[0][x], (*pb)[0], (unsigned)sizeof *pa);
    return 0;
}
