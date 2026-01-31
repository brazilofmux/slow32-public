#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>
#include <unistd.h>

#include "mmio_ring.h"

#define FLAG_READ     0x01
#define FLAG_WRITE    0x02
#define FLAG_APPEND   0x04
#define FLAG_CREATE   0x08
#define FLAG_TRUNC    0x10

#define _IONBF 0
#define _IOLBF 1
#define _IOFBF 2

#define STDIO_BUF_SIZE 4096

// Initializers must match struct FILE layout in stdio.h
static FILE _stdin  = { .fd = 0, .flags = FLAG_READ, .mode = _IONBF };
static FILE _stdout = { .fd = 1, .flags = FLAG_WRITE, .mode = _IOLBF };
static FILE _stderr = { .fd = 2, .flags = FLAG_WRITE, .mode = _IONBF };

FILE *stdin = &_stdin;
FILE *stdout = &_stdout;
FILE *stderr = &_stderr;

// Whether the host supports READ_DIRECT (auto-detected on first use)
static int has_direct_read = 1;

int fflush(FILE *stream);

// Internal: Flush write buffer
static int internal_flush(FILE *stream) {
    if (stream->buf_pos > 0 && stream->buf_len == 0) {
        size_t len = stream->buf_pos;
        size_t total = 0;
        unsigned char *p = (unsigned char *)stream->buffer;
        
        while (total < len) {
             size_t chunk = len - total;
             if (chunk > S32_MMIO_DATA_CAPACITY) chunk = S32_MMIO_DATA_CAPACITY;
             
             volatile unsigned char *db = S32_MMIO_DATA_BUFFER;
             memcpy((void*)db, p + total, chunk);
             unsigned int res = s32_mmio_request(S32_MMIO_OP_WRITE, chunk, 0, stream->fd);
             
             if (res == S32_MMIO_STATUS_ERR) {
                 stream->error = 1;
                 stream->buf_pos = 0;
                 return EOF;
             }
             
             total += res;
             if (res < chunk) break;
        }
        stream->buf_pos = 0;
        
        if (total < len) {
             stream->error = 1;
             return EOF;
        }
    }
    return 0;
}

int fflush(FILE *stream) {
    if (!stream) return 0;
    return internal_flush(stream);
}

int fclose(FILE *stream) {
    if (!stream) return EOF;
    
    fflush(stream);
    
    if (stream->buffer) free(stream->buffer);
    
    int result = s32_mmio_request(S32_MMIO_OP_CLOSE, 0u, 0u, stream->fd);
    
    if (stream != stdin && stream != stdout && stream != stderr) {
        free(stream);
    }
    
    return (result < 0) ? EOF : 0;
}

FILE *fopen(const char *pathname, const char *mode) {
    FILE *f = calloc(1, sizeof(FILE));
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
    
    f->mode = _IOFBF;
    f->buffer = malloc(STDIO_BUF_SIZE);
    f->buf_size = f->buffer ? STDIO_BUF_SIZE : 0;
    if (!f->buffer) f->mode = _IONBF;
    
    return f;
}

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if (!stream || !ptr) return 0;
    size_t total_bytes = size * nmemb;
    if (total_bytes == 0) return 0;
    
    // Lazy alloc for stdout
    if (stream == stdout && !stream->buffer && stream->mode != _IONBF) {
        stream->buffer = malloc(STDIO_BUF_SIZE);
        stream->buf_size = stream->buffer ? STDIO_BUF_SIZE : 0;
        if (!stream->buffer) stream->mode = _IONBF;
    }
    
    if (stream->mode == _IONBF || !stream->buffer) {
         size_t bytes_written = 0;
         const unsigned char *src = ptr;
         volatile unsigned char *db = S32_MMIO_DATA_BUFFER;
         
         while (bytes_written < total_bytes) {
             size_t chunk = total_bytes - bytes_written;
             if (chunk > S32_MMIO_DATA_CAPACITY) chunk = S32_MMIO_DATA_CAPACITY;
             
             memcpy((void*)db, src + bytes_written, chunk);
             unsigned int res = s32_mmio_request(S32_MMIO_OP_WRITE, chunk, 0, stream->fd);
             
             if (res == S32_MMIO_STATUS_ERR) {
                 stream->error = 1;
                 break;
             }
             bytes_written += res;
             if (res < chunk) {
                 stream->error = 1;
                 break;
             }
         }
         return bytes_written / size;
    }
    
    const unsigned char *src = ptr;
    size_t bytes_processed = 0;
    
    while (bytes_processed < total_bytes) {
        size_t avail = stream->buf_size - stream->buf_pos;
        size_t chunk = total_bytes - bytes_processed;
        
        if (chunk > avail) chunk = avail;
        
        memcpy(stream->buffer + stream->buf_pos, src + bytes_processed, chunk);
        stream->buf_pos += chunk;
        bytes_processed += chunk;
        
        if (stream->buf_pos == stream->buf_size) {
            if (internal_flush(stream) == EOF) break;
        }
    }
    
    if (stream->mode == _IOLBF) {
        if (memchr(ptr, '\n', total_bytes)) {
             fflush(stream);
        }
    }
    
    return bytes_processed / size;
}

static size_t fread_fill_buffer(FILE *stream) {
    if (!stream->buffer || stream->buf_size == 0) return 0;

    size_t to_read = stream->buf_size;
    unsigned int bytes_read;
    
    if (has_direct_read) {
        bytes_read = (unsigned int)s32_mmio_request(
            S32_MMIO_OP_READ_DIRECT, to_read, (uint32_t)stream->buffer, stream->fd);
        if (bytes_read == S32_MMIO_STATUS_ERR) {
            has_direct_read = 0;
        }
    }
    
    if (!has_direct_read) {
        if (to_read > S32_MMIO_DATA_CAPACITY) to_read = S32_MMIO_DATA_CAPACITY;
        bytes_read = (unsigned int)s32_mmio_request(
            S32_MMIO_OP_READ, to_read, 0u, stream->fd);
    }

    if (bytes_read == S32_MMIO_STATUS_ERR || bytes_read == S32_MMIO_STATUS_EINTR) {
        stream->error = 1;
        return 0;
    }

    if (bytes_read == 0) {
        stream->eof = 1;
        return 0;
    }

    if (!has_direct_read) {
        volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
        memcpy(stream->buffer, (void *)data_buffer, bytes_read);
    }
    
    stream->buf_pos = 0;
    stream->buf_len = bytes_read;

    return bytes_read;
}

int getchar(void) {
    unsigned int result = (unsigned int)s32_mmio_request(S32_MMIO_OP_GETCHAR, 0u, 0u, 0u);
    if (result == 0xFFFFFFFF) return EOF;
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    return (int)data_buffer[0];
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
                return i / size;
            }
            p[i] = (unsigned char)c;
        }
        return nmemb;
    }
    
    if (stream->buf_len == 0 && stream->buf_pos > 0) {
        fflush(stream);
    }

    unsigned char *dest = ptr;
    size_t remaining = total;
    size_t bytes_copied = 0;

    while (remaining > 0) {
        size_t avail = stream->buf_len - stream->buf_pos;

        if (avail > 0) {
            size_t to_copy = (avail < remaining) ? avail : remaining;
            memcpy(dest, stream->buffer + stream->buf_pos, to_copy);
            stream->buf_pos += to_copy;
            dest += to_copy;
            remaining -= to_copy;
            bytes_copied += to_copy;
        } else if (stream->buffer && stream->buf_size > 0 && remaining < stream->buf_size) {
            // Request fits in buffer or is small, fill buffer first
            if (fread_fill_buffer(stream) == 0) break;
        } else {
            // Direct read optimization for large or unbuffered reads
            if (has_direct_read) {
                unsigned int dres = (unsigned int)s32_mmio_request(
                    S32_MMIO_OP_READ_DIRECT, remaining, (uint32_t)dest, stream->fd);
                
                if (dres != S32_MMIO_STATUS_ERR) {
                    if (dres == S32_MMIO_STATUS_EINTR) continue;
                    if (dres == 0) { stream->eof = 1; break; }
                    dest += dres;
                    remaining -= dres;
                    bytes_copied += dres;
                    continue;
                }
                has_direct_read = 0; // Not supported by host
            }

            // Fallback: chunked read via MMIO buffer
            size_t chunk = remaining;
            if (chunk > S32_MMIO_DATA_CAPACITY) chunk = S32_MMIO_DATA_CAPACITY;

            unsigned int bytes_read = (unsigned int)s32_mmio_request(
                S32_MMIO_OP_READ, chunk, 0u, stream->fd);

            if (bytes_read == S32_MMIO_STATUS_ERR || bytes_read == S32_MMIO_STATUS_EINTR) {
                stream->error = 1;
                break;
            }

            if (bytes_read == 0) {
                stream->eof = 1;
                break;
            }

            volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
            memcpy(dest, (void *)data_buffer, bytes_read);
            dest += bytes_read;
            remaining -= bytes_read;
            bytes_copied += bytes_read;

            if (bytes_read < chunk) {
                stream->eof = 1;
                break;
            }
        }
    }

    return bytes_copied / size;
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

int putchar(int c) {
    return fputc(c, stdout);
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

int puts(const char *s) {
    if (fputs(s, stdout) == EOF) return EOF;
    if (putchar('\n') == EOF) return EOF;
    return 0;
}

int fseek(FILE *stream, long offset, int whence) {
    if (!stream) return -1;

    fflush(stream);
    stream->buf_pos = 0;
    stream->buf_len = 0;

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
    if (!stream) return -1L;
    
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    data_buffer[0] = (unsigned char)SEEK_CUR;
    *(long *)(void *)(data_buffer + 4) = 0;

    int result = s32_mmio_request(S32_MMIO_OP_SEEK, 8u, 0u, stream->fd);
    if (result < 0) {
        stream->error = 1;
        return -1L;
    }
    
    long pos = (long)result;
    
    if (stream->buf_len == 0 && stream->buf_pos > 0) {
        pos += stream->buf_pos;
    } else if (stream->buf_len > 0) {
        pos -= (stream->buf_len - stream->buf_pos);
    }
    
    return pos;
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

void perror(const char *s) {
    if (s && *s) {
        fputs(s, stderr);
        fputs(": ", stderr);
    }
    fputs("error\n", stderr);
}
