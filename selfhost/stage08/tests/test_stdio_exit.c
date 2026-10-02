/* What is still in a stream's buffer when the program ends is written
 * (libc/stdio.c: exit sends it, and returning from main is exit).  And
 * exit's status is the one given: it used to be whatever the last call
 * had returned.
 *
 *   test_stdio_exit w FILE   write FILE through a FILE and through a
 *                            descriptor, close neither, leave by exit(0)
 *                            from inside a function
 *   test_stdio_exit r FILE   0 when FILE holds what w wrote
 *   test_stdio_exit o        a line with no newline on stdout, then return
 *   test_stdio_exit s        exit(37)
 * run-tests.sh runs the four. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void leave(void) {
    exit(0);
}

int main(int argc, char **argv) {
    FILE *f; int fd; char b[80]; int r; int i;
    if (argc < 2) return 90;
    if (argv[1][0] == 'w' && argc > 2) {
        f = fopen(argv[2], "w");
        if (!f) return 91;
        fputs("through the FILE", f);
        for (i = 0; i < 5000; i++) fputc('.', f);       /* past one buffer, into the next */
        fd = fdopen_path("stdio_exit_fd.tmp", "w");
        if (fd < 0) return 92;
        fdputs("through the descriptor", fd);
        leave();
        return 93;                                       /* not reached */
    }
    if (argv[1][0] == 'r' && argc > 2) {
        f = fopen(argv[2], "r");
        if (!f) return 94;
        r = (int)fread(b, 1, 16, f); b[r] = 0;
        if (strcmp(b, "through the FILE") != 0) return 95;
        for (i = 0; i < 5000; i++) if (fgetc(f) != '.') return 96;
        if (fgetc(f) != EOF) return 97;
        fclose(f);
        f = fopen("stdio_exit_fd.tmp", "r");
        if (!f) return 98;
        r = (int)fread(b, 1, 79, f); b[r] = 0;
        fclose(f);
        remove("stdio_exit_fd.tmp");
        remove(argv[2]);
        if (strcmp(b, "through the descriptor") != 0) return 99;
        return 0;
    }
    if (argv[1][0] == 'o') {
        printf("a line with no end");
        return 0;
    }
    if (argv[1][0] == 's') exit(37);
    return 90;
}
