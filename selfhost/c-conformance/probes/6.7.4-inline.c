// 6.7.4p6-7  static inline and inline-with-extern-definition functions
#include <stdio.h>
static inline int sq(int x) { return x * x; }
inline int cube(int x) { return x * x * x; }
extern int cube(int x);    /* makes this TU's definition external */
int main(void) {
    int (*fp)(int) = sq;
    printf("%d %d %d\n", sq(7), cube(3), fp(5));
    return 0;
}
