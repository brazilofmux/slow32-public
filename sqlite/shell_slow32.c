/* What the SQLite shell needs from a POSIX libc that the guest's does not
 * have.  There is no shell to run a command in, no symbolic links, one user,
 * one process. */
#include <stddef.h>
#include <errno.h>
#include "compat/shell_slow32.h"
int system(const char *cmd) { (void)cmd; return -1; }
int chmod(const char *path, unsigned mode) { (void)path; (void)mode; return 0; }
int symlink(const char *target, const char *path) { (void)target; (void)path; errno = EPERM; return -1; }
long readlink(const char *path, char *buf, size_t n) { (void)path; (void)buf; (void)n; errno = EINVAL; return -1; }
int getuid(void) { return 0; }
int getpid(void) { return 1; }
static void (*at_fn[8])(void); static int at_n;
int atexit(void (*fn)(void)) { if (at_n < 8) { at_fn[at_n++] = fn; return 0; } return -1; }
void slow32_shell_run_atexit(void) { while (at_n > 0) at_fn[--at_n](); }
#include <stdio.h>
#include <unistd.h>
/* the guest has no pipes to a shell */
FILE *popen(const char *cmd, const char *mode) { (void)cmd; (void)mode; errno = ENOSYS; return 0; }
int pclose(FILE *f) { (void)f; return -1; }
/* not a terminal: the shell reads its standard input as a script, no
 * prompts -- how it is driven on the emulator */
int isatty(int fd) { (void)fd; return 0; }
