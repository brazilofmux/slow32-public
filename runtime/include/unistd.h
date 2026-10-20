#ifndef SLOW32_UNISTD_H
#define SLOW32_UNISTD_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// Access mode constants for access()
#define F_OK 0  // File exists
#define X_OK 1  // Execute permission
#define W_OK 2  // Write permission
#define R_OK 4  // Read permission

// The end, here and now: no atexit functions, nothing flushed
void _exit(int status);

// Sleep functions
unsigned int sleep(unsigned int seconds);
int usleep(unsigned int usec);

// File/directory operations
int access(const char *pathname, int mode);
int chdir(const char *path);
char *getcwd(char *buf, size_t size);
int unlink(const char *pathname);
int rmdir(const char *pathname);
int mkdir(const char *pathname, unsigned int mode);
int open(const char *pathname, int flags, ...);
int close(int fd);
int read(int fd, void *buf, size_t count);
int write(int fd, const void *buf, size_t count);
int lseek(int fd, int offset, int whence);
int ftruncate(int fd, int length);
int truncate(const char *path, long long length);

/* Links: the machine has no service to make or read one.  link and
 * symlink fail with ENOSYS; readlink says EINVAL for a path that is not
 * a link, ENOSYS for one that is (posix_extra.c). */
int link(const char *oldpath, const char *newpath);
int symlink(const char *target, const char *linkpath);
long readlink(const char *path, char *buf, size_t size);

/* pathconf/fpathconf: _PC_PATH_MAX and _PC_NAME_MAX */
#define _PC_NAME_MAX 3
#define _PC_PATH_MAX 4
long pathconf(const char *path, int name);
long fpathconf(int fd, int name);

/* Run another .s32x via the host emulator and wait. Returns the
 * child's exit code, or -1 on failure. argv[0] is ignored; path is
 * the image. Extra arguments start at argv[1]. argv may be NULL. */
int s32_execv(const char *path, char *const argv[]);
/* Like s32_execv, but child's stdin/stdout/stderr become guest_fd
 * (a socket or file). guest_fd < 0 means inherit the parent's stdio. */
int s32_execv_fd(const char *path, char *const argv[], int guest_fd);

/* The machine has no way to ask whether a descriptor is a terminal, so
 * isatty() says it is not: 0, with errno ENOTTY. */
int isatty(int fd);

/* Command-line options (runtime/getopt.c); <getopt.h> has the long ones */
extern char *optarg;
extern int optind, opterr, optopt;
int getopt(int argc, char *const argv[], const char *optstring);

#ifdef __cplusplus
}
#endif

#endif // SLOW32_UNISTD_H
