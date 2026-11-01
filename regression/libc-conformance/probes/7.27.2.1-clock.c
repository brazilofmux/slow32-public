/* 7.27.2.1 clock: returns (clock_t)-1 when unavailable, otherwise a processor time that does not run backwards */
#include <stdio.h>
#include <time.h>

int main(void) {
    clock_t a = clock(), b;
    volatile unsigned long spin = 0;
    unsigned long i;
    if (a == (clock_t)-1) {
        printf("clock: unavailable, and says so with (clock_t)-1\n");
        return 0;
    }
    for (i = 0; i < 200000; i++) spin += i;
    b = clock();
    printf("second call available: %d\n", b != (clock_t)-1);
    printf("not earlier than the first: %d\n", b >= a);
    printf("seconds elapsed is small: %d\n", (double)(b - a) / CLOCKS_PER_SEC < 30.0);
    return 0;
}
