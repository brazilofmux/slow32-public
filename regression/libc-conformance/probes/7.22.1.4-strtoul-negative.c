/* 7.22.1.4 strtoul: a minus sign negates in the unsigned type; overflow before negation gives ULONG_MAX and ERANGE */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

int main(void) {
    char b[64];
    char *e;
    unsigned long u;
    errno = 0; u = strtoul("-1", &e, 10); printf("%d %d\n", u == ULONG_MAX, errno == 0);
    errno = 0; u = strtoul("-0x10", &e, 16); printf("%d %d\n", u == (unsigned long)0 - 16, errno == 0);
    errno = 0; u = strtoul("-0", &e, 10); printf("%lu %d\n", u, errno == 0);
    sprintf(b, "%lu", ULONG_MAX);
    errno = 0; u = strtoul(b, &e, 10); printf("%d %d\n", u == ULONG_MAX, errno == 0);
    b[strlen(b) - 1]++;                       /* ULONG_MAX + 1 (ends in 5) */
    errno = 0; u = strtoul(b, &e, 10); printf("%d %d %d\n", u == ULONG_MAX, errno == ERANGE, *e == 0);
    sprintf(b, "-%lu", ULONG_MAX);
    errno = 0; u = strtoul(b, &e, 10); printf("%lu %d\n", u, errno == 0);
    b[strlen(b) - 1]++;
    errno = 0; u = strtoul(b, &e, 10); printf("%d %d\n", u == ULONG_MAX, errno == ERANGE);
    return 0;
}
