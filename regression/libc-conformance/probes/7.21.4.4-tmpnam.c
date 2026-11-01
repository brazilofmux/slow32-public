/* 7.21.4.4 tmpnam: makes a name that is not an existing file and differs from earlier ones; TMP_MAX is at least 25 and L_tmpnam bounds the name */
#include <stdio.h>
#include <string.h>

int main(void) {
    char a[L_tmpnam], b[L_tmpnam];
    printf("%d\n", TMP_MAX >= 25);
    char *p = tmpnam(a);
    char *q = tmpnam(b);
    printf("%d %d %d\n", p == a, q == b, strcmp(a, b) != 0);
    printf("%d %d\n", strlen(a) < L_tmpnam, fopen(a, "r") == NULL);
    char *s = tmpnam(NULL);
    printf("%d\n", s != NULL && strcmp(s, a) != 0 && strcmp(s, b) != 0);
    return 0;
}
