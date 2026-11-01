/* 7.27.3.5p3 strftime: %z and %Z are determined by tm_isdst -- a time built from the standard members alone */
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {
    struct tm t;
    char buf[64];
    int k;
    for (k = 0; k <= 1; k++) {
        /* every member C names, set; nothing else, so any further member is zero */
        memset(&t, 0, sizeof t);
        t.tm_year = 125; t.tm_mon = 6 * k; t.tm_mday = 15; t.tm_hour = 12; t.tm_wday = 3; t.tm_yday = 14 + 181 * k;
        t.tm_isdst = k;
        strftime(buf, sizeof buf, "%z", &t);
        printf("isdst=%d: %%z [%s]\n", k, buf);
    }
    return 0;
}
