// 6.4.3p1  universal character names in identifiers name the same identifier as the UCN spelled the same
#include <stdio.h>
int main(void) {
    int caf\u00e9 = 7;
    caf\u00e9 += 1;
    printf("%d\n", caf\u00e9);
    return 0;
}
