// 6.10.1p4,6.6  unevaluated division by zero in #if is fine (&&, ||, ?:)
#include <stdio.h>
int main(void) {
    int r = 0;
#if 0 && (1 / 0)
#else
    r |= 1;
#endif
#if 1 || (1 / 0)
    r |= 2;
#endif
#if (1 ? 5 : 1 / 0) == 5
    r |= 4;
#endif
    printf("%d\n", r);
    return 0;
}
