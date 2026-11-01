/* 7.27.3.5 strftime: %Y, %C (year/100 truncated) and %y (last two digits) for years from 0 to beyond 9999 (negative tm_year included) */
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {
    static const long long years[] = {2026, 1999, 1900, 1850, 1000, 999, 100, 99, 10, 9, 1, 0,
                                      10000, 12345};
    struct tm t;
    char buf[64];
    int i;
    memset(&t, 0, sizeof t);
    t.tm_mday = 1;
    for (i = 0; i < (int)(sizeof years / sizeof years[0]); i++) {
        t.tm_year = (int)(years[i] - 1900);
        strftime(buf, sizeof buf, "Y[%Y] C[%C] y[%y]", &t);
        printf("%lld: %s\n", years[i], buf);
    }
    return 0;
}
