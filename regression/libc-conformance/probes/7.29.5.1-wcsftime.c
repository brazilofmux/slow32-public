/* 7.29.5.1 wcsftime: strftime's conversions into a wide array */
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <wchar.h>

int main(void) {
    struct tm t;
    wchar_t w[64];
    size_t n, i;
    memset(&t, 0, sizeof t);
    t.tm_year = 126; t.tm_mon = 10; t.tm_mday = 1; t.tm_hour = 9; t.tm_min = 8; t.tm_sec = 7; t.tm_wday = 0;
    t.tm_yday = 304;
    n = wcsftime(w, 64, L"%a %B %d %Y %T", &t);
    printf("%u [", (unsigned)n);
    for (i = 0; i < n; i++) putchar((int)w[i]);
    printf("]\n");
    return 0;
}
