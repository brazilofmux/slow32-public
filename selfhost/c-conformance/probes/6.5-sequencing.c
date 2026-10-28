// 6.5p2-3  side effects sequenced by && || ?: and comma; operand order of evaluation
#include <stdio.h>
static int trace[8], n;
static int t(int v) { trace[n++] = v; return v; }
int main(void) {
    int i = 0;
    (void)(t(1) && t(2));
    (void)(t(0) && t(3));
    (void)(t(4) || t(5));
    (void)(t(0) ? t(6) : t(7));
    i = (i++, i++, i);
    for (int k = 0; k < n; k++) printf("%d ", trace[k]);
    printf("| %d\n", i);
    return 0;
}
