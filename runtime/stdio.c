#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "mmio_ring.h"

#define FLAG_READ     0x01
#define FLAG_WRITE    0x02
#define FLAG_APPEND   0x04
#define FLAG_CREATE   0x08
#define FLAG_TRUNC    0x10

static FILE _stdin = {0, FLAG_READ, 0, 0, NULL, 0, 0, 0};
static FILE _stdout = {1, FLAG_WRITE, 0, 0, NULL, 0, 0, 0};
static FILE _stderr = {2, FLAG_WRITE, 0, 0, NULL, 0, 0, 0};

FILE *stdin = &_stdin;
FILE *stdout = &_stdout;
FILE *stderr = &_stderr;

int putchar(int c) {
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    data_buffer[0] = (unsigned char)c;
    s32_mmio_request(S32_MMIO_OP_PUTCHAR, 1u, 0u, 0u);
    return c;
}

int getchar(void) {
    unsigned int result = (unsigned int)s32_mmio_request(S32_MMIO_OP_GETCHAR, 0u, 0u, 0u);
    if (result == 0xFFFFFFFF) return EOF;
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    return (int)data_buffer[0];
}

int puts(const char *s) {
    while (*s) {
        putchar(*s++);
    }
    putchar('\n');
    return 0;
}

FILE *fopen(const char *pathname, const char *mode) {
    FILE *f = malloc(sizeof(FILE));
    if (!f) return NULL;
    
    f->flags = 0;
    if (strchr(mode, 'r')) f->flags |= FLAG_READ;
    if (strchr(mode, 'w')) f->flags |= FLAG_WRITE | FLAG_CREATE | FLAG_TRUNC;
    if (strchr(mode, 'a')) f->flags |= FLAG_WRITE | FLAG_APPEND;
    if (strchr(mode, '+')) f->flags |= FLAG_READ | FLAG_WRITE;
    
    size_t len = strlen(pathname);
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, pathname, len + 1);
    
    f->fd = s32_mmio_request(S32_MMIO_OP_OPEN, len + 1u, 0u, f->flags);
    if (f->fd < 0) {
        free(f);
        return NULL;
    }
    
    f->error = 0;
    f->eof = 0;
    f->buffer = NULL;
    f->buf_size = 0;
    f->buf_pos = 0;
    f->buf_len = 0;
    
    return f;
}

int fclose(FILE *stream) {
    if (!stream) return EOF;
    
    if (stream->buffer) free(stream->buffer);
    
    int result = s32_mmio_request(S32_MMIO_OP_CLOSE, 0u, 0u, stream->fd);
    
    if (stream != stdin && stream != stdout && stream != stderr) {
        free(stream);
    }
    
    return (result < 0) ? EOF : 0;
}

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if (!stream || !ptr) return 0;
    
    size_t total = size * nmemb;
    if (total == 0) return 0;
    if (total > S32_MMIO_DATA_CAPACITY) {
        total = S32_MMIO_DATA_CAPACITY;
    }
    
    if (stream == stdout || stream == stderr) {
        const unsigned char *p = ptr;
        for (size_t i = 0; i < total; i++) {
            putchar(p[i]);
        }
        return nmemb;
    }
    
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, ptr, total);
    unsigned int written = (unsigned int)s32_mmio_request(S32_MMIO_OP_WRITE, total, 0u, stream->fd);
    
    if (written != total) {
        stream->error = 1;
        // Avoid division - return 0 for partial write
        return 0;
    }
    
    return nmemb;
}

size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if (!stream || !ptr) return 0;
    
    size_t total = size * nmemb;
    if (total == 0) return 0;
    if (total > S32_MMIO_DATA_CAPACITY) {
        total = S32_MMIO_DATA_CAPACITY;
    }
    
    if (stream == stdin) {
        unsigned char *p = ptr;
        for (size_t i = 0; i < total; i++) {
            int c = getchar();
            if (c == EOF) {
                stream->eof = 1;
                // Avoid division - return 0 for partial read
                return 0;
            }
            p[i] = (unsigned char)c;
        }
        return nmemb;
    }
    
    unsigned int bytes_read = (unsigned int)s32_mmio_request(S32_MMIO_OP_READ, total, 0u, stream->fd);

    if (bytes_read == S32_MMIO_STATUS_ERR || bytes_read == S32_MMIO_STATUS_EINTR) {
        stream->error = 1;
        return 0;
    }
    
    if (bytes_read == 0) {
        stream->eof = 1;
        return 0;
    }
    
    if (bytes_read < total) {
        stream->eof = 1;
    }
    
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy(ptr, (void *)data_buffer, bytes_read);
    // Return the number of fully read elements (may be partial at EOF)
    return bytes_read / size;
}

int fgetc(FILE *stream) {
    unsigned char c;
    if (fread(&c, 1, 1, stream) != 1) return EOF;
    return c;
}

int getc(FILE *stream) {
    return fgetc(stream);
}

int fputc(int c, FILE *stream) {
    unsigned char ch = c;
    if (fwrite(&ch, 1, 1, stream) != 1) return EOF;
    return c;
}

int putc(int c, FILE *stream) {
    return fputc(c, stream);
}

char *fgets(char *s, int size, FILE *stream) {
    if (!s || size <= 0) return NULL;
    
    int i = 0;
    while (i < size - 1) {
        int c = fgetc(stream);
        if (c == EOF) {
            if (i == 0) return NULL;
            break;
        }
        s[i++] = c;
        if (c == '\n') break;
    }
    s[i] = '\0';
    return s;
}

int fputs(const char *s, FILE *stream) {
    size_t len = strlen(s);
    if (fwrite(s, 1, len, stream) != len) return EOF;
    return 0;
}

int fseek(FILE *stream, long offset, int whence) {
    if (!stream) return -1;
    
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    data_buffer[0] = (unsigned char)whence;
    *(long *)(void *)(data_buffer + 4) = offset;
    
    int result = s32_mmio_request(S32_MMIO_OP_SEEK, 8u, 0u, stream->fd);
    if (result < 0) {
        stream->error = 1;
        return -1;
    }
    
    stream->eof = 0;
    return 0;
}

long ftell(FILE *stream) {
    return fseek(stream, 0, SEEK_CUR);
}

void rewind(FILE *stream) {
    fseek(stream, 0, SEEK_SET);
    clearerr(stream);
}

int feof(FILE *stream) {
    return stream ? stream->eof : 0;
}

int ferror(FILE *stream) {
    return stream ? stream->error : 0;
}

void clearerr(FILE *stream) {
    if (stream) {
        stream->error = 0;
        stream->eof = 0;
    }
}

int fflush(FILE *stream) {
    return 0;
}

void perror(const char *s) {
    if (s && *s) {
        fputs(s, stderr);
        fputs(": ", stderr);
    }
    fputs("error\n", stderr);
}
