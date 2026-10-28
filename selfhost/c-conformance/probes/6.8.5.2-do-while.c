// 6.8.5.2p1  do body runs before the first test; continue in do goes to the test
#include <stdio.h>
int main(void) {
    int n = 0, i = 0, c = 0;
    do n++; while (0);
    do { i++; if (i % 2) continue; c += i; } while (i < 9);
    int k = 0;
    do { k++; if (k == 3) break; } while (1);
    unsigned u = 3;
    do ; while (u--);
    printf("%d %d %d %d %u\n", n, i, c, k, u);
    return 0;
}
