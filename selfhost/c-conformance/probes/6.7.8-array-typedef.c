// 6.7.8p3/6.7.3p9  an array typedef used with an initializer; a qualifier on it applies to the elements
#include <stdio.h>
typedef int A[3];
A plain = { 1, 2, 3 };
const A ca = { 4, 5, 6 };
int main(void) {
    const int *p = ca;
    printf("%d %d %d\n", plain[2], p[0] + p[2], (int)(sizeof ca / sizeof ca[0]));
    return 0;
}
