// 6.3.1.7p1  real to complex: imaginary part zero (complex is conditional, 6.10.8.3 __STDC_NO_COMPLEX__)
// optional: __STDC_NO_COMPLEX__
#include <stdio.h>
int main(void) {
#ifdef __STDC_NO_COMPLEX__
    printf("no complex\n");
#else
    double _Complex z = 2.5;
    double re = (double)z;
    printf("%.1f %d\n", re, z == 2.5);
#endif
    return 0;
}
