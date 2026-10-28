// 6.10.8.1p1  __STDC_HOSTED__ is defined, to 0 or 1
#include <stdio.h>
int main(void) {
#if defined(__STDC_HOSTED__) && (__STDC_HOSTED__ == 0 || __STDC_HOSTED__ == 1)
    puts("defined");
#else
    puts("__STDC_HOSTED__ missing");
#endif
    return 0;
}
