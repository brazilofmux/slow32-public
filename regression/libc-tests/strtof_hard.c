/* HOSTLEG -- strtof rounds once.  strtof_hard.in holds 3000 inputs, most
 * of them the exact decimal midpoint between two adjacent floats, as is,
 * or nudged up or down far past double precision -- where "(float)strtod"
 * rounds twice: the old runtime was wrong 540 times in 3000, and its
 * ERANGE 342 times -- plus random decimal and hexadecimal constants.  Each
 * line prints the float's bits, whether errno is ERANGE, and where parsing
 * stopped.  (The inputs come from a seeded generator; any change to them
 * is a change to this test.) */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
    static char line[4096];
    while (fgets(line, sizeof line, stdin)) {
        char *end;
        float f;
        unsigned u;
        line[strcspn(line, "\n")] = 0;
        errno = 0;
        f = strtof(line, &end);
        memcpy(&u, &f, 4);
        printf("%08x %d %d\n", u, errno == ERANGE, (int)(end - line));
    }
    return 0;
}
