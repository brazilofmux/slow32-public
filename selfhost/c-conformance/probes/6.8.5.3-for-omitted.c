// 6.8.5.3p2  clause-1 and expr-3 may be omitted; an omitted expr-2 is a nonzero constant
#include <stdio.h>
int main(void) {
    int i = 0, n = 0;
    for (;;) { if (++i > 5) break; n += i; }
    for (; i < 8;) i++;
    int k;
    for (k = 0; ; k += 2) if (k > 10) break;
    printf("%d %d %d\n", i, n, k);
    return 0;
}
