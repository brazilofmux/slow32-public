// Minimal stdio functions needed for basic operation

#include <stddef.h>

extern int putchar(int c);

// Minimal FILE type for debug runtime
typedef struct {
    int fd;
} FILE;

// Static stream objects
static FILE _stdout = {1};
static FILE _stderr = {2};

FILE *stdout = &_stdout;
FILE *stderr = &_stderr;
FILE *stdin = (FILE *)0;  // Not supported in debug runtime

int puts(const char *s) {
    while (*s) {
        putchar(*s++);
    }
    putchar('\n');
    return 0;
}

int fputc(int c, FILE *stream) {
    // Debug runtime just outputs everything via DEBUG instruction
    (void)stream;
    putchar(c);
    return c;
}

int putc(int c, FILE *stream) {
    return fputc(c, stream);
}

int fputs(const char *s, FILE *stream) {
    (void)stream;
    while (*s) {
        putchar(*s++);
    }
    return 0;
}

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
    // Debug runtime outputs everything character by character
    (void)stream;
    const unsigned char *p = (const unsigned char *)ptr;
    size_t total = size * nmemb;
    for (size_t i = 0; i < total; i++) {
        putchar(p[i]);
    }
    return nmemb;
}

int fflush(FILE *stream) {
    (void)stream;
    return 0;  // Nothing to flush in debug mode
}