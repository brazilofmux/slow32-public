// 6.8.6.1p2  goto may jump into a nested block (no VLA in scope); the code skipped is not run
#include <stdio.h>
int main(void) {
    int hits = 0;
    goto into;
    {
        hits += 1000;
    into:
        hits += 7;
    }
    printf("%d\n", hits);
    return 0;
}
