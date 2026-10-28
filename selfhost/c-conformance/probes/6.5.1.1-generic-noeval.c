// 6.5.1.1p3  the controlling expression is not evaluated; only the chosen association is
#include <stdio.h>
int main(void) {
    int i = 0, j = 0;
    int r = _Generic(i++, int: j += 10, default: j += 100);
    printf("%d %d %d\n", i, j, r);
    return 0;
}
