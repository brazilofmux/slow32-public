/* 7.27.2.3 mktime: out-of-range members are brought into range, and wday/yday are filled in */
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {
    struct tm t;
    memset(&t, 0, sizeof t);
    t.tm_year = 124; t.tm_mon = 1; t.tm_mday = 30; t.tm_hour = 12; t.tm_isdst = -1;
    time_t r = mktime(&t);
    printf("%d %d-%02d-%02d wday=%d yday=%d\n", r != (time_t)-1, t.tm_year + 1900, t.tm_mon + 1,
           t.tm_mday, t.tm_wday, t.tm_yday);
    return 0;
}
