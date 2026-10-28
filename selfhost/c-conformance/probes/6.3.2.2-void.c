// 6.3.2.2p1  a void expression may be evaluated for side effects and cast-discarded
#include <stdio.h>
static int n;
static void bump(void) { n++; }
int main(void) {
    (void)bump;
    (void)(n += 3);
    bump();
    (void)0, bump();
    int z = (bump(), n);
    printf("%d %d\n", n, z);
    return 0;
}
