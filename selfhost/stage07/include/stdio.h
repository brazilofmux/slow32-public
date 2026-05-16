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

#endif
