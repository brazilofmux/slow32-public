// 6.5.2.5p5  a compound literal at file scope has static storage and constant initializers
#include <stdio.h>
static int *gp = (int[]){4, 5, 6};
struct P { int x, y; };
static struct P *pp = &(struct P){11, 12};
int main(void) {
    gp[1] += 100;
    printf("%d %d %d\n", gp[1], pp->x, pp->y);
    return 0;
}
