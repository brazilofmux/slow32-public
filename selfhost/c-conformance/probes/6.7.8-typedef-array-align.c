// 6.7.8p3  a typedef names the array type itself, so a member of it is aligned as its elements are
#include <stdio.h>
#include <stddef.h>
typedef unsigned char mask16[16];
typedef short pair[2];
struct m { char c; mask16 a; char d; pair p; };
union u { mask16 a; char c; };
int main(void) {
    union u v;
    printf("%u %u %u %u\n", (unsigned)offsetof(struct m, a), (unsigned)offsetof(struct m, d),
           (unsigned)offsetof(struct m, p), (unsigned)sizeof(struct m));
    v.a[15] = 'z'; v.a[0] = 'y';
    printf("%u %c %c\n", (unsigned)sizeof(union u), v.c, v.a[15]);
    return 0;
}
