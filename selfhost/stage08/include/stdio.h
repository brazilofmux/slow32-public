/* stdio.h -- s12cc-compatible stub
 *
 * Declares the subset of <stdio.h> implemented by libc_a64/stdio.c.
 * Signatures match the dialect (no const/restrict, void*→char* for
 * byte buffers).  Used when porting third-party C to compile under
 * cc-a64; gcc still picks up its own stdio.h via the system include
 * path when cc-a64 isn't in use.
 */
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

int   fclose(FILE *f);
int   fflush(FILE *f);
int   fputc(int c, FILE *f);
int   fputs(char *s, FILE *f);
int   fgetc(FILE *f);
int   fread(char *ptr, int size, int count, FILE *f);
int   fwrite(char *ptr, int size, int count, FILE *f);
int   fseek(FILE *f, int offset, int whence);
int   ftell(FILE *f);
int   putchar(int c);

/* True variadic on cc-x64 and cc-a64 via the callee-side va_*
 * lowering (ISSUES.md #48).  Floating-point args still print as '?'
 * (#49 — no V-reg save area in the GP-only va_list path). */
int   printf(char *fmt, ...);
int   fprintf(FILE *f, char *fmt, ...);
int   snprintf(char *buf, int size, char *fmt, ...);
void  perror(char *s);

/* --- Declarations that were missing -------------------------------
 *
 * These are all DEFINED in libc/stdio.c but were never declared here,
 * so every caller got an implicit declaration returning int.  On a
 * 32-bit target an int happens to hold a pointer, which is why
 * fopen() and fgets() appeared to work -- and is exactly the hazard
 * that produced the fdseek bug: fdseek returns lseek's offset, callers
 * assumed fseek's 0-on-success, and nothing type-checked the gap.
 * An audit found 51 of 107 libc functions undeclared; these are the
 * public ones. */
FILE *fopen(const char *path, const char *mode);
FILE *tmpfile(void);
int   puts(const char *s);
int   getc(FILE *fp);
int   putc(int c, FILE *fp);
int   ungetc(int c, FILE *fp);
void  rewind(FILE *fp);
int   remove(const char *path);
int   rename(const char *oldpath, const char *newpath);
int   setvbuf(FILE *fp, char *buf, int mode, unsigned int size);
int   feof(FILE *fp);
int   ferror(FILE *fp);
void  clearerr(FILE *fp);
int   fileno(FILE *fp);
char *fgets(char *buf, int n, FILE *fp);
int   sprintf(char *str, const char *format, ...);

/* The fd-based file layer the self-hosted tools are built on.
 * fdseek has LSEEK semantics: it returns the resulting offset, not
 * fseek's 0-on-success.  Test `< 0` for failure. */
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

#endif
