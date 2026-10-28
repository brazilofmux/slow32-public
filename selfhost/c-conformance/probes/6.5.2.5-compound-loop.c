// 6.5.2.5p16  a compound literal at block scope is one object for its block, initialized again each time it is evaluated
#include <stdio.h>
struct box { int n; };
int main(void) {
    struct box *cur = 0, *prev;
    int round = 10, laps = 0;
redo:
    prev = cur;
    cur = &((struct box){ round++ });   /* same block every time: the same object */
    if (++laps < 3) goto redo;
    printf("%d %d %d\n", cur == prev, cur->n, prev->n);
    int sum = 0;
    for (int k = 1; k <= 3; k++) { int *pair = (int[2]){ k, 10 * k }; pair[1] += 1; sum += pair[0] + pair[1]; }
    printf("%d\n", sum);
    return 0;
}
