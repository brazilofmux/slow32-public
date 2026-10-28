// 6.8.6.3p2  break ends only the smallest enclosing switch or loop
#include <stdio.h>
int main(void) {
    int s = 0, i, j;
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 5; j++) {
            if (j == 2) break;
            s += 1;
        }
        while (1) { s += 10; break; }
        do { s += 100; break; s += 9999; } while (1);
        if (i == 3) break;
    }
    printf("%d %d %d\n", s, i, j);
    return 0;
}
