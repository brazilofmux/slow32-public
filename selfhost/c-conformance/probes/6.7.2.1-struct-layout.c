// 6.7.2.1p15  members in increasing address order; padding; union members overlap
#include <stdio.h>
#include <stddef.h>
struct s { char c; int i; short h; double d; char t; };
union u { char c; int i; double d; };
int main(void) {
    struct s v; union u w;
    printf("%u %u %u %u %u %u\n", (unsigned)offsetof(struct s, c), (unsigned)offsetof(struct s, i),
           (unsigned)offsetof(struct s, h), (unsigned)offsetof(struct s, d),
           (unsigned)offsetof(struct s, t), (unsigned)sizeof(struct s));
    printf("%u %d\n", (unsigned)sizeof(union u), (void *)&w.c == (void *)&w.d);
    printf("%d\n", (void *)&v == (void *)&v.c);
    return 0;
}
