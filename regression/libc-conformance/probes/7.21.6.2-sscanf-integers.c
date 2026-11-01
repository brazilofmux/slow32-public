/* 7.21.6.2 fscanf: %d %i %o %u %x %X match the subject sequences of strtol and strtoul with base 10, 0, 8, 10, 16 */
#include <stdio.h>

int main(void) {
    int d = 0, i1 = 0, i2 = 0, i3 = 0;
    unsigned o = 0, u = 0, x = 0, X = 0;
    int r = sscanf("-42 0x1f 017 99", "%d %i %i %i", &d, &i1, &i2, &i3);
    printf("%d: %d %d %d %d\n", r, d, i1, i2, i3);
    r = sscanf("777 +123 0xFF ff Ab", "%o %u %x %x %X", &o, &u, &x, &X, &X);
    printf("%d: %o %u %x %X\n", r, o, u, x, X);
    r = sscanf("-1 -0x10 +017", "%u %i %i", &u, &i1, &i2);
    printf("%d: %u %d %d\n", r, u, i1, i2);
    r = sscanf("  \t\n  12", "%d", &d);
    printf("%d: %d\n", r, d);
    return 0;
}
