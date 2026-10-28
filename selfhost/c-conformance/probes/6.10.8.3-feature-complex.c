// 6.10.8.3p1,p2  without __STDC_NO_COMPLEX__, _Complex works; never both NO_COMPLEX and IEC_559_COMPLEX
#include <stdio.h>
#if defined(__STDC_NO_COMPLEX__) && defined(__STDC_IEC_559_COMPLEX__)
#error both __STDC_NO_COMPLEX__ and __STDC_IEC_559_COMPLEX__
#endif
int main(void) {
#ifndef __STDC_NO_COMPLEX__
    double _Complex z = 3.0;
    double _Complex w = z * z;
    printf("%d\n", (int)__real__ w);
#else
    printf("%d\n", 9);
#endif
    return 0;
}
