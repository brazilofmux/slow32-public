// 6.2.5p26-p28  qualified versions share representation/size; pointer-to-qualified vs qualified-pointer
#include <stdio.h>
#include <stddef.h>
int main(void) {
    int x = 1, y = 2;
    const int *pc = &x;      /* pointer to const */
    int *const cp = &x;      /* const pointer */
    pc = &y;
    *cp = 9;
    volatile int vi = 4;
    printf("%d %d %d %d %d\n", *pc, x, (int)(sizeof(const int) == sizeof(int)),
           (int)(sizeof(volatile long long) == sizeof(long long)), vi);
    return 0;
}
