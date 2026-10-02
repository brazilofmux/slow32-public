/* stdio.h -- the self-hosted library's (also the cross compilers', whose
 * libraries define a subset of it) */
#ifndef _STDIO_H
#define _STDIO_H

#include <stddef.h>

#ifndef NULL
#define NULL 0
#endif

#define EOF (-1)

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

typedef struct _file FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2
#define BUFSIZ 1024

FILE *fopen(const char *path, const char *mode);
FILE *fdopen(int fd, const char *mode);
FILE *freopen(const char *path, const char *mode, FILE *f);
FILE *tmpfile(void);
int   fclose(FILE *f);
int   fflush(FILE *f);
int   setvbuf(FILE *f, char *buf, int mode, size_t size);
void  setbuf(FILE *f, char *buf);
int   fileno(FILE *f);

size_t fread(void *ptr, size_t size, size_t count, FILE *f);
size_t fwrite(const void *ptr, size_t size, size_t count, FILE *f);
int   fgetc(FILE *f);
int   getc(FILE *f);
int   getchar(void);
int   ungetc(int c, FILE *f);
char *fgets(char *buf, int n, FILE *f);
int   fputc(int c, FILE *f);
int   putc(int c, FILE *f);
int   putchar(int c);
int   fputs(const char *s, FILE *f);
int   puts(const char *s);

/* offsets are int here and long in the standard: one type on SLOW-32,
 * and the cross compilers' libraries, which share this header, define
 * these two with int on machines where long is wider */
int   fseek(FILE *f, int offset, int whence);
int   ftell(FILE *f);
void  rewind(FILE *f);
typedef long fpos_t;
int   fgetpos(FILE *f, fpos_t *pos);
int   fsetpos(FILE *f, const fpos_t *pos);
int   feof(FILE *f);
int   ferror(FILE *f);
void  clearerr(FILE *f);
void  perror(const char *s);

int   remove(const char *path);
int   rename(const char *oldpath, const char *newpath);

int   printf(const char *fmt, ...);
int   fprintf(FILE *f, const char *fmt, ...);
int   sprintf(char *str, const char *fmt, ...);
int   snprintf(char *buf, size_t size, const char *fmt, ...);
int   sscanf(const char *str, const char *fmt, ...);

#include <stdarg.h>
int   vprintf(const char *fmt, va_list ap);
int   vfprintf(FILE *f, const char *fmt, va_list ap);
int   vsprintf(char *str, const char *fmt, va_list ap);
int   vsnprintf(char *buf, size_t size, const char *fmt, va_list ap);
int   vsscanf(const char *str, const char *fmt, va_list ap);

/* POSIX: a whole line, the buffer grown to fit */
#ifndef _SSIZE_T_DEFINED
#define _SSIZE_T_DEFINED
typedef int ssize_t;
#endif
ssize_t getline(char **lineptr, size_t *n, FILE *f);
ssize_t getdelim(char **lineptr, size_t *n, int delim, FILE *f);

/* The fd-named functions: the stream functions, reached by descriptor
 * (libc/stdio.c has the history).  The compiler's own diagnostics still
 * use them.  fdseek has LSEEK semantics: it returns the resulting
 * offset, not fseek's 0-on-success.  Test `< 0` for failure. */
int   fdopen_path(const char *path, const char *mode);
int   fdclose(int fd);
int   fdgetc(int fd);
char *fdgets(char *buf, int n, int fd);
int   fdread(const char *buf, int sz, int count, int fd);
int   fdwrite(const char *buf, int sz, int count, int fd);
int   fdseek(int fd, int off, int whence);
int   fdtell(int fd);
int   fdputc(int c, int fd);
int   fdputs(const char *s, int fd);
void  fdputuint(int fd, unsigned int v);

#define FILENAME_MAX 4096
#endif
