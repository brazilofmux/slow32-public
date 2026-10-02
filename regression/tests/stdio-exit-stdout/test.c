/* A program that only ever writes to stdout: what stdout holds when the
 * program ends is written by exit (see stdio-exit-flush; here no file is
 * ever opened, so it is stdout alone that tells exit there is something
 * to send).  stdout is made fully buffered, so the line waits for exit
 * though it is complete -- an unfinished last line would test the same
 * thing, but an engine's own closing banner then lands on that line and
 * the differential harness cannot tell them apart. */
#include <stdio.h>
int main(void) {
    setvbuf(stdout, NULL, _IOFBF, 0);
    printf("no file was opened; this line waited for exit\n");
    return 0;
}
