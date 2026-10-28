// 6.10.3.5p7  (EXAMPLE 5 shape) x ## y ## z over every pattern of empty arguments
#include <stdio.h>
#define glue3(a,b,c) a ## b ## c
int main(void) {
    int j[] = { glue3(2,0,1), glue3(,3,4), glue3(5,,6), glue3(7,8,),
                glue3(9,,), glue3(,13,), glue3(,,14), glue3(,,) };
    int k;
    printf("%d:", (int)(sizeof j / sizeof j[0]));
    for (k = 0; k < (int)(sizeof j / sizeof j[0]); k++) printf(" %d", j[k]);
    printf("\n");
    return 0;
}
