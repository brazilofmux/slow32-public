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
unsigned int sleep(unsigned int seconds);
int   usleep(unsigned int usec);

int access(const char *path, int mode);
int ftruncate(int fd, int length);
/* access() modes, as the MMIO host takes them */
#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4
int chdir(const char *path);
char *getcwd(char *buf, unsigned long size);   /* size_t */
int fsync(int fd);
int getpid(void);
int getuid(void);
int isatty(int fd);

/* Command-line options (runtime/getopt.c); <getopt.h> has the long ones */
extern char *optarg;
extern int optind, opterr, optopt;
int getopt(int argc, char *const argv[], const char *optstring);
#endif
