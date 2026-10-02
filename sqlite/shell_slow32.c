/* What the SQLite shell needs from a POSIX libc that the guest's does not
 * have.  There is no shell to run a command in, no symbolic links, one user,
 * one process. */
#include <stddef.h>
#include <errno.h>
#include "compat/shell_slow32.h"
int chmod(const char *path, unsigned mode) { (void)path; (void)mode; return 0; }
int symlink(const char *target, const char *path) { (void)target; (void)path; errno = EPERM; return -1; }
long readlink(const char *path, char *buf, size_t n) { (void)path; (void)buf; (void)n; errno = EINVAL; return -1; }
int getuid(void) { return 0; }
int getpid(void) { return 1; }
/* system and atexit were here until both C libraries had their own
 * (2026-10, selfhost ISSUES-75).  This atexit kept what was registered
 * and nothing ever ran it; the library's runs it at exit. */
#include <stdio.h>
#include <unistd.h>
/* the guest has no pipes to a shell */
FILE *popen(const char *cmd, const char *mode) { (void)cmd; (void)mode; errno = ENOSYS; return 0; }
int pclose(FILE *f) { (void)f; return -1; }
/* not a terminal: the shell reads its standard input as a script, no
 * prompts -- how it is driven on the emulator */
int isatty(int fd) { (void)fd; return 0; }
