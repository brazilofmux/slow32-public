// 6.10.8.3p1  without __STDC_NO_VLA__, variable length arrays must work
// optional: __STDC_NO_VLA__
#include <stdio.h>
static int sum(int n) {
#ifndef __STDC_NO_VLA__
    int a[n];
    int i, s = 0;
    for (i = 0; i < n; i++) a[i] = i * i;
    for (i = 0; i < n; i++) s += a[i];
    return s + (int)sizeof a;
#else
    (void)n; return 50;
#endif
}
int main(void) { printf("%d\n", sum(4)); return 0; }
