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
    int ungetc_char;   /* -1 = empty, otherwise pushed-back character */
    // Memory-stream bookkeeping (memstream.c); NULL for ordinary files
    void *mem_cookie;
    // The next open stream (stdio.c): exit sends what each still holds
    struct FILE *next_open;
} FILE;

/* For a caller that writes very many very small records and cannot afford
 * a call for each (the COBOL runtime's WRITE of a one-byte record, four
 * million times in one program): the stream's own buffer, to store into.
 *
 * __s32_out_plain: this stream takes that -- fully buffered, its buffer
 * allocated, a file and not a memory stream, nothing read ahead in the
 * buffer.  That is settled when the stream is opened and stays so for a
 * stream opened for output only ("w", "a") that setvbuf is not called on,
 * so it is asked once.
 *
 * __s32_out_room: where the next n bytes of such a stream go, the stream
 * moved past them -- or NULL when the buffer has not the room, and the
 * caller calls fwrite, which is where a buffer is emptied and where a
 * write fails.  __s32_out_byte: one byte stored, or 0 for the same reason.
 * The tests are fwrite's own (stdio.c), so the buffer is emptied at the
 * same byte either way.
 *
 * The two values are stdio.c's, which asserts they are. */
#define __S32_STREAM_ROOM 1
#define __S32_MODE_FULL 2          /* FILE.mode of a fully buffered stream */
#define __S32_FLAG_MEM  0x80       /* FILE.flags of a memory stream */
static inline int __s32_out_plain(const FILE *s)
{
    return s->mode == __S32_MODE_FULL && s->buffer != 0 && s->buf_len == 0 &&
           !(s->flags & __S32_FLAG_MEM) && s->buf_pos <= s->buf_size;
}
static inline char *__s32_out_room(FILE *s, size_t n)
{
    size_t pos = s->buf_pos;
    if (n < s->buf_size - pos) { s->buf_pos = pos + n; return s->buffer + pos; }
    return 0;
}
static inline int __s32_out_byte(FILE *s, int c)
{
    size_t pos = s->buf_pos;
    if (pos + 1 < s->buf_size) { s->buffer[pos] = (char)c; s->buf_pos = pos + 1; return 1; }
    return 0;
}

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

FILE *fopen(const char *pathname, const char *mode);
FILE *fdopen(int fd, const char *mode);
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
int vsscanf(const char *str, const char *format, va_list ap);
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

typedef long fpos_t;
int fgetpos(FILE *stream, fpos_t *pos);
int fsetpos(FILE *stream, const fpos_t *pos);
void setbuf(FILE *stream, char *buf);

// POSIX line input (MMIO libc)
ssize_t getline(char **lineptr, size_t *n, FILE *stream);
ssize_t getdelim(char **lineptr, size_t *n, int delim, FILE *stream);

// POSIX memory streams (MMIO libc, memstream.c). See that file for the
// exact subset of semantics supported.
FILE *fmemopen(void *buf, size_t size, const char *mode);
FILE *open_memstream(char **bufp, size_t *sizep);

#endif