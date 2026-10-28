// 6.2.8p1  alignment requirements observed via offsetof padding (no _Alignof)
#include <stdio.h>
#include <stddef.h>
struct ac { char c; char t; };
struct as { char c; short t; };
struct ai { char c; int t; };
struct al { char c; long long t; };
struct ad { char c; double t; };
struct ap { char c; void *t; };
struct sd { char c; struct ad t; };
int main(void) {
    printf("%d %d %d %d %d %d %d %d\n", (int)offsetof(struct ac, t), (int)offsetof(struct as, t),
           (int)offsetof(struct ai, t), (int)offsetof(struct al, t), (int)offsetof(struct ad, t),
           (int)offsetof(struct ap, t), (int)offsetof(struct sd, t), (int)sizeof(struct ad));
    return 0;
}
