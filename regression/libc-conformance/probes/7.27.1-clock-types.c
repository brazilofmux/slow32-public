/* 7.27.1 components: CLOCKS_PER_SEC has type clock_t and is positive; clock_t and time_t are real types; struct tm and timespec members */
#include <stdio.h>
#include <stddef.h>
#include <time.h>

int main(void) {
    struct tm t;
    struct timespec ts;
    /* real types: a floating value converts to them */
    clock_t c = (clock_t)2.5;
    time_t tt = (time_t)7.0;
    printf("CLOCKS_PER_SEC same size as clock_t: %d\n", sizeof(CLOCKS_PER_SEC) == sizeof(clock_t));
    printf("CLOCKS_PER_SEC > 0: %d\n", CLOCKS_PER_SEC > 0);
    printf("clock_t from 2.5: %d, time_t from 7.0: %d\n", (int)c, (int)tt);
    t.tm_sec = 1; t.tm_min = 2; t.tm_hour = 3; t.tm_mday = 4; t.tm_mon = 5; t.tm_year = 6;
    t.tm_wday = 0; t.tm_yday = 7; t.tm_isdst = -1;
    ts.tv_sec = 8; ts.tv_nsec = 999999999L;
    printf("tm members: %d %d %d %d %d %d %d %d %d\n", t.tm_sec, t.tm_min, t.tm_hour, t.tm_mday, t.tm_mon,
           t.tm_year, t.tm_wday, t.tm_yday, t.tm_isdst);
    printf("timespec: %d %ld, tv_nsec is long: %d\n", (int)ts.tv_sec, ts.tv_nsec, sizeof ts.tv_nsec == sizeof(long));
    printf("NULL from time.h: %d\n", (void *)NULL == (void *)0);
    return 0;
}
