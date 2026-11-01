/* 7.9: the eleven <iso646.h> macros spell the corresponding operators */
#include <iso646.h>
#include <stdio.h>

int main(void) {
    unsigned a = 12, b = 10;
    int t = 1, f = 0;
    printf("%d %d %d %d\n", t and f, t or f, not f, t not_eq f);
    printf("%u %u %u %u\n", a bitand b, a bitor b, a xor b, compl a & 0xffu);
    a and_eq 6u;
    printf("%u", a);
    a or_eq 1u;
    printf(" %u", a);
    a xor_eq 3u;
    printf(" %u\n", a);
    return 0;
}
