/* system: there is no command processor -- system(0) says so, and a
 * command fails.  A file of its own, so that it is an archive member of
 * its own: a program that brings a system() with it links, and uses its
 * own (runtime/system.c is the clang runtime's, for the same reason).
 * Built in phase 2 only. */
#include <stdlib.h>

int system(const char *command) {
    if (!command) return 0;
    return -1;
}
