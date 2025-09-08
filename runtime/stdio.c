#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

extern void yield(void);

#define MMIO_BASE     0x10000000
#define REQ_HEAD      (*(volatile unsigned int*)(MMIO_BASE + 0x0000))
#define REQ_TAIL      (*(volatile unsigned int*)(MMIO_BASE + 0x0004))
#define REQ_RING      ((volatile unsigned int*)(MMIO_BASE + 0x1000))
#define RESP_HEAD     (*(volatile unsigned int*)(MMIO_BASE + 0x2000))
#define RESP_TAIL     (*(volatile unsigned int*)(MMIO_BASE + 0x2004))
#define RESP_RING     ((volatile unsigned int*)(MMIO_BASE + 0x3000))
#define DATA_BUFFER   ((volatile unsigned char*)(MMIO_BASE + 0x4000))

#define OP_PUTCHAR    0x01
#define OP_GETCHAR    0x02
#define OP_WRITE      0x03
#define OP_READ       0x04
#define OP_OPEN       0x05
#define OP_CLOSE      0x06
#define OP_SEEK       0x07

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

static unsigned int mmio_request(unsigned int opcode, unsigned int length, 
                                  unsigned int offset, unsigned int status) {
    unsigned int req_head = REQ_HEAD;
    unsigned int req_tail = REQ_TAIL;
    
    while (((req_head + 1) % 256) == req_tail) {
        yield();
        req_tail = REQ_TAIL;
    }
    
    unsigned int idx = req_head * 4;
    REQ_RING[idx + 0] = opcode;
    REQ_RING[idx + 1] = length;
    REQ_RING[idx + 2] = offset;
    REQ_RING[idx + 3] = status;
    
    REQ_HEAD = (req_head + 1) % 256;
    
    unsigned int resp_head = RESP_HEAD;
    unsigned int resp_tail = RESP_TAIL;
    
    while (resp_head == resp_tail) {
        yield();
        resp_tail = RESP_TAIL;
    }
    
    idx = resp_head * 4;
    unsigned int result = RESP_RING[idx + 3];
    RESP_HEAD = (resp_head + 1) % 256;
    
    return result;
}

int putchar(int c) {
    DATA_BUFFER[0] = (unsigned char)c;
    mmio_request(OP_PUTCHAR, 1, 0, 0);
    return c;
}

int getchar(void) {
    unsigned int result = mmio_request(OP_GETCHAR, 0, 0, 0);
    if (result == 0xFFFFFFFF) return EOF;
    return (int)DATA_BUFFER[0];
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
    memcpy((void *)DATA_BUFFER, pathname, len + 1);
    
    f->fd = mmio_request(OP_OPEN, len + 1, 0, f->flags);
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
    
    int result = mmio_request(OP_CLOSE, 0, 0, stream->fd);
    
    if (stream != stdin && stream != stdout && stream != stderr) {
        free(stream);
    }
    
    return (result < 0) ? EOF : 0;
}

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if (!stream || !ptr) return 0;
    
    size_t total = size * nmemb;
    if (total == 0) return 0;
    
    if (stream == stdout || stream == stderr) {
        const unsigned char *p = ptr;
        for (size_t i = 0; i < total; i++) {
            putchar(p[i]);
        }
        return nmemb;
    }
    
    memcpy((void *)DATA_BUFFER, ptr, total);
    unsigned int written = mmio_request(OP_WRITE, total, 0, stream->fd);
    
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
    
    unsigned int bytes_read = mmio_request(OP_READ, total, 0, stream->fd);
    
    if (bytes_read == 0) {
        stream->eof = 1;
        return 0;
    }
    
    if (bytes_read < total) {
        stream->eof = 1;
    }
    
    memcpy(ptr, (void *)DATA_BUFFER, bytes_read);
    // Avoid division - only return full reads
    return (bytes_read == total) ? nmemb : 0;
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
    
    DATA_BUFFER[0] = whence;
    *(long *)(DATA_BUFFER + 4) = offset;
    
    int result = mmio_request(OP_SEEK, 8, 0, stream->fd);
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