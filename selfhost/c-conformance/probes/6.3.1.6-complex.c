// 6.3.1.6p1  complex-to-complex conversion (complex types are conditional: __STDC_NO_COMPLEX__)
// optional: __STDC_NO_COMPLEX__
#include <stdio.h>
int main(void) {
#ifdef __STDC_NO_COMPLEX__
    printf("no complex\n");
#else
    double _Complex z = 1.0;
    float _Complex fz = z;
    printf("%d\n", (int)sizeof fz);
#endif
    return 0;
}
