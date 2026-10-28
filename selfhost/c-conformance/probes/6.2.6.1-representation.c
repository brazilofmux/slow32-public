// 6.2.6.1p4  objects are copyable as unsigned char[n]; memcpy round-trip; memcmp of equal ints
#include <stdio.h>
#include <string.h>
int main(void) {
    double d = 3.25, e;
    unsigned char buf[sizeof d];
    memcpy(buf, &d, sizeof d);
    memcpy(&e, buf, sizeof e);
    unsigned u = 0x11223344u;
    unsigned char *p = (unsigned char *)&u;
    int a = 77, b = 77;
    printf("%g %02x%02x%02x%02x %d\n", e, p[0], p[1], p[2], p[3], memcmp(&a, &b, sizeof a));
    return 0;
}
