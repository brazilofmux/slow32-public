/* 7.24.6.2 strerror: EDOM, ERANGE and EILSEQ map to distinct non-empty messages; no other library function overwrites the string */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

int main(void) {
    char a[256], b[256], c[256];
    char *p;
    strncpy(a, strerror(EDOM), 255); a[255] = 0;
    strncpy(b, strerror(ERANGE), 255); b[255] = 0;
    strncpy(c, strerror(EILSEQ), 255); c[255] = 0;
    printf("non-empty %d %d %d\n", a[0] != 0, b[0] != 0, c[0] != 0);
    printf("distinct %d %d %d\n", strcmp(a, b) != 0, strcmp(a, c) != 0, strcmp(b, c) != 0);
    printf("stable %d\n", strcmp(strerror(EDOM), a) == 0);
    p = strerror(ERANGE);
    errno = EDOM;
    perror("probe");
    strtol("99999999999999999999999", NULL, 10);
    printf("untouched by perror and strtol %d\n", strcmp(p, b) == 0);
    printf("unknown number gives a string %d\n", strerror(12345) != NULL);
    return 0;
}
