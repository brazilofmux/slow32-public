// 6.7.6.1p1/6.7.6p3  pointer-to-array declarators: object, and function returning one
#include <stdio.h>
static int m[2][3] = { { 1, 2, 3 }, { 4, 5, 6 } };
static int (*row(int i))[3] { return &m[i]; }
int main(void) {
    int (*pa)[3] = m;
    pa++;
    printf("%d %u %d\n", (*pa)[2], (unsigned)sizeof *pa, (*row(0))[1]);
    return 0;
}
