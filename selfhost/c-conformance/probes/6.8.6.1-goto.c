// 6.8.6.1p2  goto forward out of nested loops, and backward to form a loop
#include <stdio.h>
int main(void) {
    int i, j, n = 0;
    for (i = 0; i < 10; i++)
        for (j = 0; j < 10; j++)
            if (i * j == 42) goto found;
    i = j = -1;
found:
    printf("%d %d\n", i, j);
again:
    if (++n < 4) goto again;
    printf("%d\n", n);
    return 0;
}
