/* 7.27.3.5 strftime: %d zero-pads, %e space-pads, %j is three digits, %m two, %S allows 60 */
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {
    static const int mdays[] = {1, 9, 10, 31};
    static const int ydays[] = {0, 8, 9, 98, 99, 364, 365};
    struct tm t;
    char buf[64];
    int i;
    memset(&t, 0, sizeof t);
    t.tm_year = 124;
    for (i = 0; i < 4; i++) {
        t.tm_mday = mdays[i];
        strftime(buf, sizeof buf, "[%d][%e]", &t);
        printf("%s\n", buf);
    }
    for (i = 0; i < 7; i++) {
        t.tm_yday = ydays[i];
        strftime(buf, sizeof buf, "[%j]", &t);
        printf("%s\n", buf);
    }
    t.tm_mon = 0; t.tm_sec = 60; t.tm_min = 0; t.tm_hour = 0;
    strftime(buf, sizeof buf, "[%m][%S][%M][%H]", &t);
    printf("%s\n", buf);
    t.tm_mon = 11; t.tm_sec = 0;
    strftime(buf, sizeof buf, "[%m][%S]", &t);
    printf("%s\n", buf);
    return 0;
}
