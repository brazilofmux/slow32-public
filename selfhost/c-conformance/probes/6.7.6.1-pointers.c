// 6.7.6.1p1-3  pointer declarators: pointer to pointer, const pointer, pointer to volatile/const
#include <stdio.h>
int main(void) {
    int a[3] = { 4, 5, 6 };
    int *p = a, **pp = &p;
    int *const *cpp = &p;
    const int *volatile vp = a + 1;
    int *const fixed = a + 2;
    printf("%d %d %d %d\n", **pp, **cpp, *vp, *fixed);
    return 0;
}
