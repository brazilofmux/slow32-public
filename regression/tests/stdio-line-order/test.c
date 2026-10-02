/* stdout is line buffered and stderr is not: with both on one device, a
 * line written to stdout is out before the stderr line written after it,
 * and a partial line waits for its newline.  The short entries of fwrite
 * and fputc (runtime/stdio.c) must leave a line-buffered stream to the
 * general routine, which is what looks for the newline -- this is the
 * test that says so (the harness reads both streams as one). */
#include <stdio.h>
int main(void) {
    printf("one\n"); fprintf(stderr, "two\n");
    fputs("three\n", stdout); fputs("four\n", stderr);
    fwrite("five\n", 1, 5, stdout); fwrite("six\n", 1, 4, stderr);
    putchar('7'); putchar('\n'); fputc('8', stderr); fputc('\n', stderr);
    fwrite("t", 1, 1, stdout); fwrite("\n", 1, 1, stdout); fwrite("u", 1, 1, stderr); fwrite("\n", 1, 1, stderr);
    printf("partial "); fprintf(stderr, "nine\n"); printf("line\n");
    return 0;
}
