// 6.8.5.3p1  continue in a for loop evaluates expression-3 before the test
#include <stdio.h>
int main(void) {
    int evals = 0, sum = 0, i;
    for (i = 0; i < 10; i++, evals++) {
        if (i & 1) continue;
        sum += i;
    }
    int j = 0;
    while (j < 10) { j++; if (j < 5) continue; sum += 100; }
    printf("%d %d %d\n", evals, sum, i);
    return 0;
}
