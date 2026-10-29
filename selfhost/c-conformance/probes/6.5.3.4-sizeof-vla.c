// 6.5.3.4p2  sizeof of a VLA is computed at run time and its operand is evaluated
// optional: __STDC_NO_VLA__
#include <stdio.h>
static size_t fsize(int n) { char b[n + 3]; return sizeof b; }
int main(void) {
    int n = 4;
    int v[n][3];
    int k = 0;
    size_t s2 = sizeof(int[++k]);
    printf("%d %d %d %d\n", (int)sizeof v, (int)sizeof v[0], (int)fsize(10), (int)s2 + k * 100);
    return 0;
}
