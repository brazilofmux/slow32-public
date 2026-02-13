#ifndef _STDIO_H
#define _STDIO_H

#include <stddef.h>
#include <stdarg.h>

#define EOF (-1)
#define BUFSIZ 1024
#define FILENAME_MAX 256

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2

#define L_tmpnam 20

typedef struct FILE {
    // Fields used by stdio_buffered.c (line-buffered output via flush callback)
    char *buffer;
    char *ptr;
    size_t count;
    size_t size;
    int mode;
    int fd;
    int flags;
    void (*flush_fn)(struct FILE *);
    // Fields used by stdio.c (MMIO buffered I/O for files)
    int error;
    int eof;
    size_t buf_size;
    size_t buf_pos;
    size_t buf_len;
} FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

FILE *fopen(const char *pathname, const char *mode);
FILE *freopen(const char *pathname, const char *mode, FILE *stream);
int fclose(FILE *stream);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
int fgetc(FILE *stream);
int getc(FILE *stream);
int getchar(void);
int fputc(int c, FILE *stream);
int putc(int c, FILE *stream);
int putchar(int c);
char *fgets(char *s, int size, FILE *stream);
int fputs(const char *s, FILE *stream);
int puts(const char *s);
int printf(const char *format, ...);
int fprintf(FILE *stream, const char *format, ...);
int sprintf(char *str, const char *format, ...);
int snprintf(char *str, size_t size, const char *format, ...);
int vprintf(const char *format, va_list ap);
int vfprintf(FILE *stream, const char *format, va_list ap);
int vsprintf(char *str, const char *format, va_list ap);
int vsnprintf(char *str, size_t size, const char *format, va_list ap);
int scanf(const char *format, ...);
int fscanf(FILE *stream, const char *format, ...);
int sscanf(const char *str, const char *format, ...);
int fseek(FILE *stream, long offset, int whence);
long ftell(FILE *stream);
void rewind(FILE *stream);
int feof(FILE *stream);
int ferror(FILE *stream);
void clearerr(FILE *stream);
int fflush(FILE *stream);
void perror(const char *s);
int fileno(FILE *stream);
int remove(const char *pathname);
int rename(const char *oldpath, const char *newpath);

int ungetc(int c, FILE *stream);
int setvbuf(FILE *stream, char *buf, int mode, size_t size);
FILE *tmpfile(void);

#endif