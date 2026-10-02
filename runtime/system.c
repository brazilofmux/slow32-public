/* system: there is no command processor -- system(NULL) says so, and a
 * command fails.
 *
 * A file of its own, so that it is an archive member of its own: a
 * program that brings a system() with it (SQLite's shell port did, when
 * the library had none) links, and uses its own.
 */
#include <stdlib.h>

int system(const char *command) {
    if (!command) return 0;
    return -1;
}
