// 6.10.1p4  character constants in #if
#include <stdio.h>
int main(void) {
    int r = 0;
#if 'A' == 65 && 'a' - 'A' == 32
    r |= 1;
#endif
#if '\n' == 10 && '\0' == 0 && '\x41' == 'A' && '\101' == 'A'
    r |= 2;
#endif
#if '\'' == 39 && '\\' == 92
    r |= 4;
#endif
    printf("%d\n", r);
    return 0;
}
