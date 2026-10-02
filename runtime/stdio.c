#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>

#include "mmio_ring.h"
#include "stdio_impl.h"

#define _IONBF 0
#define _IOLBF 1
#define _IOFBF 2

/* <stdio.h> tells a caller that stores into the buffer itself
 * (__s32_out_plain) what these two are */
_Static_assert(_IOFBF == __S32_MODE_FULL, "stdio.h: __S32_MODE_FULL");
_Static_assert(FLAG_MEMSTREAM == __S32_FLAG_MEM, "stdio.h: __S32_FLAG_MEM");

#define STDIO_BUF_SIZE 4096

// Initializers must match struct FILE layout in stdio.h
static FILE _stdin  = { .fd = 0, .flags = FLAG_READ, .mode = _IONBF, .ungetc_char = -1 };
static FILE _stdout = { .fd = 1, .flags = FLAG_WRITE, .mode = _IOLBF, .ungetc_char = -1 };
static FILE _stderr = { .fd = 2, .flags = FLAG_WRITE, .mode = _IONBF, .ungetc_char = -1 };

FILE *stdin = &_stdin;
FILE *stdout = &_stdout;
FILE *stderr = &_stderr;

// Whether the host supports READ_DIRECT (auto-detected on first use)
static int has_direct_read = 1;

int fflush(FILE *stream);
int fseek(FILE *stream, long offset, int whence);

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

/* The streams fopen made and fclose has not closed, and what the end of
 * the program does with them: a C program's buffered output is written
 * when it exits, closed or not.  It was not -- exit went to the host with
 * stdout's last partial line and every unclosed file's last block still
 * here (the COBOL runtime closes its files at STOP RUN for this reason).
 * exit_mmio.c calls through __stdio_exit_hook, set the first time a
 * stream could be holding something. */
static FILE *open_list;
extern void (*__stdio_exit_hook)(void);

static void stdio_at_exit(void) {
    internal_flush(stdout);
    for (FILE *f = open_list; f; f = f->next_open) internal_flush(f);
}

/* Output waiting in stdout is sent before input is asked of stdin, so a
 * prompt with no newline is seen before its answer is awaited. */
static void flush_before_input(void) {
    if (stdout->buf_pos > 0 && stdout->buf_len == 0) internal_flush(stdout);
}

int fflush(FILE *stream) {
    if (!stream) { stdio_at_exit(); return 0; }
    if (stream->flags & FLAG_MEMSTREAM) return __memstream_flush(stream);
    return internal_flush(stream);
}

int fclose(FILE *stream) {
    if (!stream) return EOF;
    if (stream->flags & FLAG_MEMSTREAM) return __memstream_close(stream);

    /* C: fclose returns EOF if any error was detected -- the flush of the
     * last buffered bytes included.  It ignored the flush, so a device
     * full at close reported success (cobol/tests/fault). */
    int flushed = fflush(stream);
    
    if (stream->buffer) free(stream->buffer);
    for (FILE **pp = &open_list; *pp; pp = &(*pp)->next_open)
        if (*pp == stream) { *pp = stream->next_open; break; }
    
    int result = s32_mmio_request(S32_MMIO_OP_CLOSE, 0u, 0u, stream->fd);
    
    if (stream != stdin && stream != stdout && stream != stderr) {
        free(stream);
    } else {
        /* a standard stream stays what it is, closed: its buffer is gone,
         * and a later write must not go into where it was */
        stream->buffer = NULL;
        stream->buf_size = stream->buf_pos = stream->buf_len = 0;
        stream->mode = _IONBF;
        stream->fd = -1;
    }
    
    return (result < 0 || flushed == EOF) ? EOF : 0;
}

static int mode_to_flags(const char *mode) {
    int flags = 0;
    if (strchr(mode, 'r')) flags |= FLAG_READ;
    if (strchr(mode, 'w')) flags |= FLAG_WRITE | FLAG_CREATE | FLAG_TRUNC;
    if (strchr(mode, 'a')) flags |= FLAG_WRITE | FLAG_APPEND | FLAG_CREATE;
    if (strchr(mode, '+')) flags |= FLAG_READ | FLAG_WRITE;
    return flags;
}

/* Wrap an already-open guest fd in a buffered FILE. */
FILE *fdopen(int fd, const char *mode) {
    FILE *f;
    if (fd < 0 || !mode) return NULL;
    f = calloc(1, sizeof(FILE));
    if (!f) return NULL;

    f->ungetc_char = -1;
    f->flags = mode_to_flags(mode);
    f->fd = fd;
    f->mode = _IOFBF;
    f->buffer = malloc(STDIO_BUF_SIZE);
    f->buf_size = f->buffer ? STDIO_BUF_SIZE : 0;
    if (!f->buffer) f->mode = _IONBF;
    f->next_open = open_list;
    open_list = f;
    __stdio_exit_hook = stdio_at_exit;
    return f;
}

FILE *fopen(const char *pathname, const char *mode) {
    int flags = mode_to_flags(mode);
    int fd;
    FILE *f;

    size_t len = strlen(pathname);
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, pathname, len + 1);

    fd = s32_mmio_request(S32_MMIO_OP_OPEN, len + 1u, 0u, flags);
    if (fd < 0) return NULL;

    f = fdopen(fd, mode);
    if (!f) { s32_mmio_request(S32_MMIO_OP_CLOSE, 0u, 0u, fd); return NULL; }
    /* "a": every write goes to the end, so that is where the stream is.
     * The host appends whatever its position says, but ftell counts from
     * that position, and reported the bytes written since the open as if
     * the file had been empty.  ("a+" reads from the beginning: left.) */
    if ((flags & FLAG_APPEND) && !(flags & FLAG_READ)) fseek(f, 0, SEEK_END);
    return f;
}

/* The same stream on another file: what it held is sent, its file is
 * closed, and the stream -- the same FILE, so stdout stays stdout --
 * begins again on the new one.  When the new file does not open the
 * stream is closed.  (Until 2026-10 this closed the stream and returned
 * a new one from fopen: freopen(..., stdout) left printf writing where
 * it had been.) */
FILE *freopen(const char *pathname, const char *mode, FILE *stream) {
    if (!stream) return NULL;
    if (pathname == NULL) {
        /* Per C standard, pathname==NULL means change mode of existing stream.
           On bare metal, just return the stream unchanged. */
        return stream;
    }
    if (stream->flags & FLAG_MEMSTREAM) {
        fclose(stream);
        return fopen(pathname, mode);
    }

    int standard = (stream == stdin || stream == stdout || stream == stderr);
    fflush(stream);
    if (stream->fd >= 0) s32_mmio_request(S32_MMIO_OP_CLOSE, 0u, 0u, stream->fd);

    int flags = mode_to_flags(mode);
    size_t len = strlen(pathname);
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, pathname, len + 1);
    int fd = s32_mmio_request(S32_MMIO_OP_OPEN, len + 1u, 0u, flags);

    if (fd < 0) {
        for (FILE **pp = &open_list; *pp; pp = &(*pp)->next_open)
            if (*pp == stream) { *pp = stream->next_open; break; }
        if (stream->buffer) free(stream->buffer);
        stream->buffer = NULL;
        stream->buf_size = stream->buf_pos = stream->buf_len = 0;
        if (standard) {
            stream->fd = -1;
            stream->flags = 0;
            stream->mode = _IONBF;
        } else {
            free(stream);
        }
        return NULL;
    }

    stream->fd = fd;
    stream->flags = flags;
    stream->error = 0;
    stream->eof = 0;
    stream->buf_pos = 0;
    stream->buf_len = 0;
    stream->ungetc_char = -1;
    if (!stream->buffer) {
        stream->buffer = malloc(STDIO_BUF_SIZE);
        stream->buf_size = stream->buffer ? STDIO_BUF_SIZE : 0;
    }
    stream->mode = stream->buffer ? _IOFBF : _IONBF;
    int listed = 0;
    for (FILE *f = open_list; f; f = f->next_open) if (f == stream) listed = 1;
    if (!listed) {
        stream->next_open = open_list;
        open_list = stream;
    }
    __stdio_exit_hook = stdio_at_exit;
    if ((flags & FLAG_APPEND) && !(flags & FLAG_READ)) fseek(stream, 0, SEEK_END);
    return stream;
}

/* fwrite, fread and fputc are short entries in front of the general
 * routines, as fgetc is.  A byte to or from a fully buffered stream whose
 * buffer has the room, or the byte, is a store and a count: the entry
 * does that and nothing else, so it needs no registers saved (a function
 * pays for its whole frame on every path, and the general fwrite was 91
 * instructions for one byte).  A few bytes are a memcpy and a count, one
 * call further on (fwrite_more, fread_more).  Everything else -- an
 * unbuffered or line-buffered stream, a memory stream, a buffer about to
 * fill, a stream with read-ahead in its buffer, an element count whose
 * product could overflow -- is the general routine's, unchanged.  The
 * short paths leave the buffer short of full, so the flush stays in one
 * place. */
static size_t fwrite_general(const void *ptr, size_t size, size_t nmemb, FILE *stream);

static __attribute__((noinline)) size_t fwrite_more(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if (stream && ptr && stream->mode == _IOFBF && stream->buffer && stream->buf_len == 0 &&
        !(stream->flags & FLAG_MEMSTREAM) && (size == 1 || nmemb == 1)) {
        size_t total = size == 1 ? nmemb : size, pos = stream->buf_pos;
        if (total != 0 && pos < stream->buf_size && total < stream->buf_size - pos) {
            memcpy(stream->buffer + pos, ptr, total);
            stream->buf_pos = pos + total;
            return nmemb;
        }
    }
    return fwrite_general(ptr, size, nmemb, stream);
}

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if (size == 1 && nmemb == 1 && stream && ptr && stream->mode == _IOFBF && stream->buffer &&
        stream->buf_len == 0 && !(stream->flags & FLAG_MEMSTREAM)) {
        size_t pos = stream->buf_pos;
        if (pos + 1 < stream->buf_size) {
            stream->buffer[pos] = *(const char *)ptr;
            stream->buf_pos = pos + 1;
            return 1;
        }
    }
    return fwrite_more(ptr, size, nmemb, stream);
}

static size_t fwrite_general(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if (!stream || !ptr) return 0;
    size_t total_bytes = size * nmemb;
    if (total_bytes == 0) return 0;
    if (stream->flags & FLAG_MEMSTREAM)
        return __memstream_write(stream, ptr, total_bytes) / size;
    
    // Lazy alloc for stdout
    if (stream == stdout && !stream->buffer && stream->mode != _IONBF) {
        __stdio_exit_hook = stdio_at_exit;
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
         return size == 1 ? bytes_written : bytes_written / size;
    }
    
    if (stream->buf_len > 0) {
        /* The buffer holds read-ahead, and this is a write.  C allows output
         * directly after input that met end-of-file, and there the bytes
         * went into the reader's buffer and were never flushed: lost.  The
         * buffer becomes the writer's, and the host goes back over what was
         * read ahead but not read (none, at end-of-file), so the write
         * lands where the program is. */
        if (stream->buf_len != stream->buf_pos || stream->ungetc_char >= 0) fseek(stream, 0, SEEK_CUR);
        else { stream->buf_pos = 0; stream->buf_len = 0; }
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
            /* The bytes just put in the buffer were in the write that
             * failed: they are not written.  (Counted, a request that
             * filled the buffer exactly reported all of itself written
             * while the device took none of it -- a program writing one
             * byte at a time to a full device lost a buffer of them and
             * was told nothing: runtime ISSUES-28.) */
            if (internal_flush(stream) == EOF) { bytes_processed -= chunk; break; }
        }
    }
    
    if (stream->mode == _IOLBF) {
        if (memchr(ptr, '\n', total_bytes)) {
             fflush(stream);
        }
    }
    
    return size == 1 ? bytes_processed : bytes_processed / size;
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
    flush_before_input();
    unsigned int result = (unsigned int)s32_mmio_request(S32_MMIO_OP_GETCHAR, 0u, 0u, 0u);
    if (result == 0xFFFFFFFF) return EOF;
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    return (int)data_buffer[0];
}

static size_t fread_general(void *ptr, size_t size, size_t nmemb, FILE *stream);

/* the bytes are in the buffer: copy them (see fwrite) */
static __attribute__((noinline)) size_t fread_more(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if (stream && ptr && stream->ungetc_char < 0 && stream->buf_pos < stream->buf_len &&
        !(stream->flags & FLAG_MEMSTREAM) && (size == 1 || nmemb == 1)) {
        size_t total = size == 1 ? nmemb : size, pos = stream->buf_pos;
        if (total != 0 && total <= stream->buf_len - pos) {
            memcpy(ptr, stream->buffer + pos, total);
            stream->buf_pos = pos + total;
            return nmemb;
        }
    }
    return fread_general(ptr, size, nmemb, stream);
}

size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if (size == 1 && nmemb == 1 && stream && ptr && stream->ungetc_char < 0 &&
        stream->buf_pos < stream->buf_len && !(stream->flags & FLAG_MEMSTREAM)) {
        *(char *)ptr = stream->buffer[stream->buf_pos++];
        return 1;
    }
    return fread_more(ptr, size, nmemb, stream);
}

static size_t fread_general(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if (!stream || !ptr) return 0;
    size_t total = size * nmemb;
    if (total == 0) return 0;
    if (stream->flags & FLAG_MEMSTREAM)
        return __memstream_read(stream, ptr, total) / size;

    unsigned char *dest = ptr;
    size_t remaining = total;
    size_t bytes_copied = 0;

    /* as-if-by-fgetc: a pending ungetc is the first byte */
    if (stream->ungetc_char >= 0) {
        *dest++ = (unsigned char)stream->ungetc_char;
        stream->ungetc_char = -1;
        remaining--;
        bytes_copied++;
        if (remaining == 0)
            return size == 1 ? bytes_copied : bytes_copied / size;
    }

    if (stream == stdin) {
        for (size_t i = 0; i < remaining; i++) {
            int c = getchar();
            if (c == EOF) {
                stream->eof = 1;
                return (bytes_copied + i) / size;
            }
            dest[i] = (unsigned char)c;
        }
        return nmemb;
    }
    
    if (stream->buf_len == 0 && stream->buf_pos > 0) {
        fflush(stream);
    }

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

    return size == 1 ? bytes_copied : bytes_copied / size;
}

static __attribute__((noinline)) int fgetc_slow(FILE *stream) {
    if (stream->ungetc_char >= 0) {
        int c = stream->ungetc_char;
        stream->ungetc_char = -1;
        stream->eof = 0;
        return c;
    }
    unsigned char c;
    if (fread(&c, 1, 1, stream) != 1) return EOF;
    return c;
}

int fgetc(FILE *stream) {
    /* the buffered byte is the common case: no fread, no memcpy, no divide,
     * and no frame -- the slow path is a separate function */
    if (stream->ungetc_char < 0 && stream->buf_pos < stream->buf_len && !(stream->flags & FLAG_MEMSTREAM))
        return (unsigned char)stream->buffer[stream->buf_pos++];
    return fgetc_slow(stream);
}

int getc(FILE *stream) {
    return fgetc(stream);
}

static __attribute__((noinline)) int fputc_general(int c, FILE *stream) {
    unsigned char ch = c;
    if (fwrite_general(&ch, 1, 1, stream) != 1) return EOF;
    return c;
}

int fputc(int c, FILE *stream) {
    /* a byte into a buffer with room for more than one (see fwrite) */
    if (stream->mode == _IOFBF && stream->buffer && stream->buf_len == 0 && !(stream->flags & FLAG_MEMSTREAM)) {
        size_t pos = stream->buf_pos;
        if (pos + 1 < stream->buf_size) {
            stream->buffer[pos] = (char)c;
            stream->buf_pos = pos + 1;
            return c;
        }
    }
    return fputc_general(c, stream);
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
    if (stream->flags & FLAG_MEMSTREAM)
        return __memstream_seek(stream, offset, whence);

    /* SEEK_CUR counts from where the program is, and the host is further
     * on by whatever was read ahead into the buffer (and a character put
     * back is one the program has not read): fseek(f, 0, SEEK_CUR) after a
     * buffered read landed at the end of the buffer, not at the reader */
    if (whence == SEEK_CUR) {
        if (stream->buf_len > 0) offset -= (long)(stream->buf_len - stream->buf_pos);
        if (stream->ungetc_char >= 0) offset -= 1;
    }
    fflush(stream);
    stream->buf_pos = 0;
    stream->buf_len = 0;
    stream->ungetc_char = -1;

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
    if (stream->flags & FLAG_MEMSTREAM)
        return __memstream_tell(stream);

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
    if (stream->ungetc_char >= 0) {
        pos -= 1;
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

int fileno(FILE *stream) {
    if (!stream) return -1;
    return stream->fd;
}

void perror(const char *s) {
    if (s && *s) {
        fputs(s, stderr);
        fputs(": ", stderr);
    }
    fputs(strerror(errno), stderr);
    fputc('\n', stderr);
}

int ungetc(int c, FILE *stream) {
    if (c == EOF || !stream) return EOF;
    stream->ungetc_char = (unsigned char)c;
    stream->eof = 0;
    return (unsigned char)c;
}

int setvbuf(FILE *stream, char *buf, int mode, size_t size) {
    /* The public header speaks POSIX (_IOFBF 0, _IOLBF 1, _IONBF 2);
     * this file's internal constants are historically reversed.
     * Translate; custom buffers are not supported (ignored). */
    (void)buf; (void)size;
    if (!stream) {
        return -1;
    }
    switch (mode) {
        case 0: stream->mode = _IOFBF; break;  /* public _IOFBF */
        case 1: stream->mode = _IOLBF; break;  /* public _IOLBF */
        case 2: stream->mode = _IONBF; break;  /* public _IONBF */
        default: return -1;
    }
    return 0;
}

/* A file for reading and writing that has no name: it is made under a
 * name nothing else has, opened, and unlinked at once, so it is gone
 * when it is closed or the run ends.  (The host keeps an unlinked file
 * as long as it is open; one that does not leaves the file behind.) */
FILE *tmpfile(void) {
    static unsigned int serial;
    char name[64];

    for (int tries = 0; tries < 16; tries++) {
        struct timespec ts = {0, 0};
        clock_gettime(CLOCK_REALTIME, &ts);
        serial++;
        snprintf(name, sizeof name, "%ss32tmp-%08lx%08lx-%u", tries < 8 ? "/tmp/" : "",
                 (unsigned long)ts.tv_sec, (unsigned long)ts.tv_nsec, serial);
        if (access(name, F_OK) == 0) continue;          /* someone's: another name */
        FILE *fp = fopen(name, "w+");
        if (fp) {
            unlink(name);
            return fp;
        }
    }
    return NULL;
}

/* A whole line, however long, its delimiter with it: the buffer is the
 * caller's to free, and is made or grown here as the line needs. */
ssize_t getdelim(char **lineptr, size_t *n, int delim, FILE *stream) {
    size_t used = 0;
    int c;

    if (!lineptr || !n || !stream) return -1;
    if (!*lineptr || *n == 0) {
        *n = 128;
        *lineptr = malloc(*n);
        if (!*lineptr) return -1;
    }

    while ((c = fgetc(stream)) != EOF) {
        if (used + 2 > *n) {
            size_t ncap = *n * 2;
            char *nbuf = realloc(*lineptr, ncap);
            if (!nbuf) return -1;
            *lineptr = nbuf;
            *n = ncap;
        }
        (*lineptr)[used++] = (char)c;
        if (c == delim) break;
    }

    if (used == 0) return -1;   /* EOF (or error) before any byte */
    (*lineptr)[used] = '\0';
    return (ssize_t)used;
}

ssize_t getline(char **lineptr, size_t *n, FILE *stream) {
    return getdelim(lineptr, n, '\n', stream);
}

int fgetpos(FILE *stream, fpos_t *pos) {
    long at = ftell(stream);
    if (at < 0) return -1;
    *pos = at;
    return 0;
}

int fsetpos(FILE *stream, const fpos_t *pos) {
    return fseek(stream, *pos, SEEK_SET);
}

/* setvbuf with the two choices there were before it */
void setbuf(FILE *stream, char *buf) {
    /* the public numbers (see setvbuf): 0 full, 2 none */
    if (buf) setvbuf(stream, buf, 0, BUFSIZ);
    else setvbuf(stream, NULL, 2, 0);
}
