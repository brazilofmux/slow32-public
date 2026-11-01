/* 7.27.3.1 asctime: 25 characters and a newline -- names, a space-padded 3-wide day field, zero-padded time, the year */
#include <stdio.h>
#include <string.h>
#include <time.h>

static void one(int y, int mo, int d, int h, int mi, int s, int wd) {
    struct tm t;
    char *p;
    memset(&t, 0, sizeof t);
    t.tm_year = y - 1900; t.tm_mon = mo; t.tm_mday = d; t.tm_hour = h; t.tm_min = mi; t.tm_sec = s;
    t.tm_wday = wd;
    p = asctime(&t);
    printf("[%s] len=%d\n", p, (int)strlen(p));
}

int main(void) {
    int i;
    one(1973, 8, 16, 1, 3, 52, 0);
    one(2025, 0, 1, 0, 0, 0, 3);       /* a one-digit day */
    one(2025, 0, 9, 9, 9, 9, 4);
    one(2025, 0, 10, 10, 10, 10, 5);
    one(2025, 0, 31, 23, 59, 60, 5);   /* a leap second is in range */
    one(1000, 0, 1, 0, 0, 0, 3);
    one(9999, 11, 31, 23, 59, 59, 5);
    for (i = 0; i < 12; i++) one(2024, i, 15, 12, 0, 0, i % 7);
    return 0;
}
