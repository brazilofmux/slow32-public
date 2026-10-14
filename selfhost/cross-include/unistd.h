/* unistd.h -- POSIX calls over Linux syscalls (cross builds). */
#ifndef _UNISTD_H
#define _UNISTD_H
#include <sys/types.h>

#ifndef NULL
#define NULL 0
#endif

#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define _SC_PAGESIZE     30
#define _SC_PAGE_SIZE    _SC_PAGESIZE

int   read(int fd, char *buf, int len);
int   write(int fd, char *buf, int len);
int   close(int fd);
long  lseek(int fd, long offset, int whence);
void  _exit(int code);
long  sysconf(int name);
int   getpid(void);
int   getuid(void);
int   isatty(int fd);
int   unlink(const char *path);
int   rmdir(const char *path);
int   access(const char *path, int mode);
int   ftruncate(int fd, long length);
int   chdir(const char *path);
char *getcwd(char *buf, unsigned long size);
int   fsync(int fd);
int   dup2(int oldfd, int newfd);
int   pipe(int fds[2]);
pid_t fork(void);
int   execv(const char *path, char *const argv[]);
unsigned int sleep(unsigned int seconds);
int   usleep(unsigned int usec);
#endif
