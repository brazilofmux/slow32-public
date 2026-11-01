/* 7.21.10.4 perror: writes the string, a colon and a space, a message, and a newline to stderr; with an empty or null string only the message */
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main(void) {
    if (!freopen("err.txt", "w", stderr)) { printf("freopen failed\n"); return 1; }
    errno = EDOM;
    perror("tag");
    errno = ERANGE;
    perror("");
    errno = EDOM;
    perror(NULL);
    fclose(stderr);
    FILE *f = fopen("err.txt", "r");
    char line[128];
    int n = 0;
    while (fgets(line, sizeof line, f)) {
        size_t len = strlen(line);
        n++;
        printf("%d: nl=%d tagged=%d nonempty=%d same_as_strerror=", n, line[len - 1] == '\n',
               strncmp(line, "tag: ", 5) == 0, len > 1);
        line[len - 1] = 0;
        const char *msg = strncmp(line, "tag: ", 5) == 0 ? line + 5 : line;
        printf("%d\n", strcmp(msg, strerror(n == 2 ? ERANGE : EDOM)) == 0);
    }
    fclose(f);
    remove("err.txt");
    return 0;
}
