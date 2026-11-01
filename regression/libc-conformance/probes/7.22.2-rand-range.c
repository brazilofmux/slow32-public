/* 7.22.2.1 rand: every result lies in 0..RAND_MAX, and RAND_MAX is at least 32767 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    int i, ok = 1, distinct = 0, first = rand();
    for (i = 0; i < 20000; i++) {
        int r = rand();
        if (r < 0 || r > RAND_MAX) ok = 0;
        if (r != first) distinct = 1;
    }
    printf("in range: %d\nRAND_MAX >= 32767: %d\nvaries: %d\n", ok, RAND_MAX >= 32767, distinct);
    return 0;
}
