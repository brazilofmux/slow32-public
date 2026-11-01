/* 7.5p3: no library function sets errno to zero; successful calls leave a nonzero errno nonzero */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int main(void) {
    char buf[16];
    errno = EDOM;
    long v = strtol("123", 0, 10);
    double d = strtod("2.5", 0);
    snprintf(buf, sizeof buf, "%ld", v);
    (void)strlen(buf);
    (void)isalpha('a');
    void *m = malloc(10);
    free(m);
    printf("%s %g %d\n", buf, d, errno != 0);
    return 0;
}
