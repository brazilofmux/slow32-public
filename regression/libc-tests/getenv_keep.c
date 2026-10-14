/* getenv: an answer stays good after another getenv.  The standard
 * lets a later call overwrite it (C11 7.22.4.6) and so does POSIX;
 * glibc does not, and programs written there count on it.  Both
 * libraries here keep each name's answer, so this has a .expect rather
 * than a host leg.  HOME and PATH are in every environment the harness
 * runs in; their values differ by machine, so only comparisons print. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
    char home[512];
    char *h;
    char *p;
    char *h2;
    int i;
    int same;

    h = getenv("HOME");
    if (h == 0) { printf("HOME not set\n"); return 1; }
    if (strlen(h) >= sizeof home) { printf("HOME too long\n"); return 1; }
    strcpy(home, h);
    p = getenv("PATH");
    if (p == 0) { printf("PATH not set\n"); return 1; }
    printf("HOME differs from PATH: %d\n", strcmp(home, p) != 0);
    printf("HOME's answer intact after PATH's: %d\n", strcmp(h, home) == 0);
    h2 = getenv("HOME");
    printf("HOME asked again, same text: %d\n", strcmp(h2, home) == 0);
    same = 1;
    for (i = 0; i < 1000; i++)
        if (getenv("S32_LIBC_TEST_NO_SUCH_NAME") != 0) same = 0;
    printf("a name not set, asked 1000 times: %s\n", same ? "null" : "SET?");
    printf("a name with '=': %s\n", getenv("HOME=") ? "SET?" : "null");
    printf("the empty name: %s\n", getenv("") ? "SET?" : "null");
    printf("HOME still intact: %d\n", strcmp(h, home) == 0);
    return 0;
}
