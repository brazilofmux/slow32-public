/* A C program's buffered output is written when it ends: stdout's last
 * line though it has no newline, and what a file's stream still holds
 * though nobody closed it.  runtime/exit_mmio.c used to go to the host
 * with all of that still in the buffers.
 *
 * What exit does cannot be read back by the program that exits, so the
 * unclosed file is checked through fflush(NULL), which sends the same
 * streams by the same list.  stdout is made fully buffered, so every
 * line printed after that flush is still in its buffer when main
 * returns: that they appear at all is exit's own doing. */
#include <stdio.h>
#include <string.h>

int main(void) {
    char b[64];
    setvbuf(stdout, NULL, _IOFBF, 0);
    FILE *f = fopen("exit_flush_a.dat", "w");
    FILE *g = fopen("exit_flush_b.dat", "w");
    if (!f || !g) { printf("fopen failed\n"); return 1; }
    fputs("held in a's buffer", f);
    fputs("held in b's buffer", g);

    FILE *r = fopen("exit_flush_a.dat", "r");
    size_t n = fread(b, 1, 63, r); b[n] = 0; fclose(r);
    printf("before the flush, a holds [%s]\n", b);

    fflush(NULL);
    r = fopen("exit_flush_a.dat", "r");
    n = fread(b, 1, 63, r); b[n] = 0; fclose(r);
    printf("after fflush(NULL), a holds [%s]\n", b);
    r = fopen("exit_flush_b.dat", "r");
    n = fread(b, 1, 63, r); b[n] = 0; fclose(r);
    printf("and b holds [%s]\n", b);

    fclose(f);                          /* a closed stream leaves the list; b stays open to the end */
    remove("exit_flush_a.dat");
    remove("exit_flush_b.dat");
    printf("the last line, in stdout's buffer until exit\n");
    return 0;
}
