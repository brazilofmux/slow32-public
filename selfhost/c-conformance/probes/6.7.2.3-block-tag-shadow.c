// 6.7.2.3p4,8/6.2.1p4  a struct defined in a block with an outer tag's name declares a new type there
#include <stdio.h>
struct t { int a; };
int main(void) {
    { struct t { double x; char y; } inner = { 1.0, 'z' }; printf("%u %c\n", (unsigned)sizeof inner, inner.y); }
    struct t o = { 3 }; printf("%d %u\n", o.a, (unsigned)sizeof o);
    return 0;
}
