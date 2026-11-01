/* 7.27.2.5 timespec_get: TIME_UTC is a positive constant; a TIME_UTC call returns the base and a nanosecond count in range */
#include <stdio.h>
#include <time.h>

int main(void) {
    struct timespec ts;
    int r;
    printf("TIME_UTC > 0: %d\n", TIME_UTC > 0);
    ts.tv_nsec = -1;
    r = timespec_get(&ts, TIME_UTC);
    printf("returns TIME_UTC: %d\n", r == TIME_UTC);
    printf("nsec in range: %d\n", ts.tv_nsec >= 0 && ts.tv_nsec <= 999999999L);
    printf("agrees with time(): %d\n", difftime(time(NULL), ts.tv_sec) >= 0.0 && difftime(time(NULL), ts.tv_sec) < 10.0);
    return 0;
}
