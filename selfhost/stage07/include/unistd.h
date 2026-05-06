/* unistd.h -- s12cc-compatible stub
 *
 * Just the bits of POSIX unistd that dbt and other tools actually use.
 */
#ifndef _UNISTD_H
#define _UNISTD_H

#ifndef NULL
#define NULL 0
#endif

#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

#define _SC_PAGESIZE     30
#define _SC_PAGE_SIZE    _SC_PAGESIZE

int   read(int fd, char *buf, int len);
int   write(int fd, char *buf, int len);
int   close(int fd);
int   lseek(int fd, int offset, int whence);
void  _exit(int code);
long  sysconf(int name);
int   getpid(void);
int   isatty(int fd);
int   unlink(char *path);

#endif
