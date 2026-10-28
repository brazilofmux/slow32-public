// 6.10.8.1p1  __STDC_VERSION__ is a long constant 201112L under C11
#include <stdio.h>
int main(void) {
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    printf("%ld %d\n", __STDC_VERSION__, (int)sizeof(__STDC_VERSION__));
#else
    puts("no C11 __STDC_VERSION__");
#endif
    return 0;
}
