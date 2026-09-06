/* Forced into shell.c: what it calls that the guest libc does not declare.
 * The definitions are in shell_slow32.c (stubs: no shell, no links, no
 * users on this machine). */
#ifndef SLOW32_SHELL_COMPAT_H
#define SLOW32_SHELL_COMPAT_H
#include <stddef.h>
int system(const char *cmd);
int chmod(const char *path, unsigned mode);
int symlink(const char *target, const char *path);
long readlink(const char *path, char *buf, size_t n);
int getuid(void);
int getpid(void);
int atexit(void (*fn)(void));
int isatty(int fd);
#endif
