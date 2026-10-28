// 6.7.9p10,17  union initialization: first member by default, any member by designator
#include <stdio.h>
union u { int i; char c[4]; double d; };
int main(void) {
    union u a = { 0x41424344 }, b = { .d = 2.5 }, c = { .c = { 'w', 'x' } };
    printf("%x %d %c%c\n", a.i, (int)(b.d * 2.0), c.c[0], c.c[1]);
    static union u z; printf("%d\n", z.i);
    return 0;
}
