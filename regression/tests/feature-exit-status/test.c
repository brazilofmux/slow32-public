/* runtime ISSUES-13: main's return value must become the emulator's exit
 * status.  exit() in the DEBUG libc used to discard it (every program
 * exited 0), and the suite only compared stdout, so nothing noticed.  The
 * runner checks expected_exit.txt against the emulator's status. */
#include <stdio.h>
static int bits(void) { volatile int v = 0x28; return v | 2; }
int main(void) {
    int rc = bits();                /* 42 */
    printf("returning %d\n", rc);
    return rc;
}
