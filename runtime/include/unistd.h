#ifndef SLOW32_UNISTD_H
#define SLOW32_UNISTD_H

#include <stddef.h>
#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

/* The environment, filled in at start for a program that names it */
extern char **environ;

/* A second descriptor for the same file or socket (FCNTL F_DUPFD) */
int dup(int fd);
int dup2(int fd, int fd2);

// Access mode constants for access()
// clock_gettime (CLOCK_REALTIME, CLOCK_MONOTONIC) and nanosleep are here;
// the timer_* calls are not.  libc++ looks for this to use clock_gettime
// for its clocks, steady_clock among them.
#define _POSIX_TIMERS 200809L

#define F_OK 0  // File exists
#define X_OK 1  // Execute permission
#define W_OK 2  // Write permission
#define R_OK 4  // Read permission

// The end, here and now: no atexit functions, nothing flushed
__attribute__((noreturn)) void _exit(int status);

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

/* One program runs on the machine at a time: it is process 1, with no
 * parent, and makes no others.  fork and the exec family fail with
 * ENOSYS (s32_execv above runs a program and waits for it), and so a
 * program has no children to wait for (<sys/wait.h>).  process.c. */
pid_t getpid(void);
pid_t getppid(void);
pid_t fork(void);
int execv(const char *path, char *const argv[]);
int execve(const char *path, char *const argv[], char *const envp[]);
int execvp(const char *file, char *const argv[]);

/* sysconf: what the machine can say.  _SC_OPEN_MAX is the host's table
 * of guest descriptors (docs/SPEC.md); the machine has no pages, and
 * _SC_PAGESIZE is the unit an allocator may round to; one processor.
 * Any other name fails with EINVAL.  Linux's numbers. */
#define _SC_OPEN_MAX           4
#define _SC_PAGESIZE           30
#define _SC_PAGE_SIZE          _SC_PAGESIZE
#define _SC_NPROCESSORS_CONF   83
#define _SC_NPROCESSORS_ONLN   84
long sysconf(int name);
int getdtablesize(void);

/* 1 when fd is a terminal, else 0 with errno ENOTTY: the term service's
 * GET_LFLAG says (<termios.h>).  0 in the DEBUG library, and on a host
 * whose term service predates the request. */
int isatty(int fd);

/* Command-line options (runtime/getopt.c); <getopt.h> has the long ones */
extern char *optarg;
extern int optind, opterr, optopt;
int getopt(int argc, char *const argv[], const char *optstring);

#ifdef __cplusplus
}
#endif

#endif // SLOW32_UNISTD_H
