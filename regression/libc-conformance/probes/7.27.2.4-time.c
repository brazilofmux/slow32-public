/* 7.27.2.4 time: the result is also stored through a non-null argument; (time_t)-1 only when unavailable */
#include <stdio.h>
#include <time.h>

int main(void) {
    time_t a, b = 0, c;
    a = time(NULL);
    c = time(&b);
    printf("available: %d\n", a != (time_t)-1);
    printf("returned value stored: %d\n", c == b);
    printf("not earlier: %d\n", difftime(c, a) >= 0.0 && difftime(c, a) < 10.0);
    return 0;
}
