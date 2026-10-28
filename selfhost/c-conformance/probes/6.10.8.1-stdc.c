// 6.10.8.1p1  __STDC__ is 1 and usable in #if
#include <stdio.h>
int main(void) {
#if __STDC__ == 1
    printf("%d\n", __STDC__);
#else
    puts("__STDC__ missing or not 1");
#endif
    return 0;
}
