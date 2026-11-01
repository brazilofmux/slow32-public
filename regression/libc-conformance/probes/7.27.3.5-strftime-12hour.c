/* 7.27.3.5 strftime: %I is 01-12 and %p AM/PM across the day, %H 00-23, %r in the "C" locale */
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {
    struct tm t;
    char buf[64];
    int h;
    memset(&t, 0, sizeof t);
    t.tm_year = 125; t.tm_mday = 1; t.tm_wday = 3; t.tm_min = 30; t.tm_sec = 15;
    for (h = 0; h < 24; h++) {
        t.tm_hour = h;
        strftime(buf, sizeof buf, "%H %I %p|%r", &t);
        printf("%s\n", buf);
    }
    return 0;
}
