/* 7.27.3.5p8 strftime: the count without the terminator when it fits in maxsize, else 0; ordinary characters copied */
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {
    struct tm t;
    char buf[32];
    size_t n;
    memset(&t, 0, sizeof t);
    t.tm_year = 124; t.tm_mon = 1; t.tm_mday = 29; t.tm_wday = 4; t.tm_yday = 59;
    n = strftime(buf, 11, "%Y-%m-%d", &t);
    printf("exact fit: %u [%s]\n", (unsigned)n, buf);
    printf("one short: %u\n", (unsigned)strftime(buf, 10, "%Y-%m-%d", &t));
    printf("maxsize 1: %u\n", (unsigned)strftime(buf, 1, "%Y", &t));
    printf("maxsize 0: %u\n", (unsigned)strftime(buf, 0, "x", &t));
    memset(buf, 'z', sizeof buf);
    n = strftime(buf, 1, "", &t);
    printf("empty format: %u, terminator placed: %d\n", (unsigned)n, buf[0] == '\0');
    n = strftime(buf, sizeof buf, "plain text, no %%s", &t);
    printf("ordinary: %u [%s]\n", (unsigned)n, buf);
    n = strftime(buf, sizeof buf, "%A %B", &t);
    printf("names: %u [%s]\n", (unsigned)n, buf);
    printf("names, one short: %u\n", (unsigned)strftime(buf, 17, "%A %B", &t));
    memset(buf, 'z', sizeof buf);
    n = strftime(buf, 6, "%n%t%%%n", &t);
    printf("specials: %u %d %d %d %d %d\n", (unsigned)n, buf[0], buf[1], buf[2], buf[3], buf[4]);
    return 0;
}
