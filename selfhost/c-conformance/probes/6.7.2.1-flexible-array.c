// 6.7.2.1p18  flexible array member: size excludes it, access through malloc'd object
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
struct fa { int n; double d[]; };
struct fc { short n; char c[]; };
int main(void) {
    struct fa *p = malloc(sizeof(struct fa) + 4 * sizeof(double));
    p->n = 4;
    for (int i = 0; i < 4; i++) p->d[i] = i * 1.5;
    double s = 0; for (int i = 0; i < p->n; i++) s += p->d[i];
    printf("%u %u %d\n", (unsigned)sizeof(struct fa), (unsigned)offsetof(struct fa, d), (int)(s * 10));
    printf("%u %u\n", (unsigned)sizeof(struct fc), (unsigned)offsetof(struct fc, c));
    free(p);
    return 0;
}
