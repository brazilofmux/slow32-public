/*
 * memstream.c — POSIX memory streams for the SLOW-32 MMIO libc
 *
 * fmemopen(buf, size, mode) and open_memstream(bufp, sizep) return FILE *
 * streams backed by guest memory instead of an MMIO fd. stdio.c dispatches
 * to the __memstream_* functions below whenever FLAG_MEMSTREAM is set, so
 * fgetc/fgets/getline (via fread) and fputс/fputs/fprintf (via fwrite) work
 * on these streams unchanged.
 *
 * Supported subset:
 *   fmemopen        "r"/"rb": read the caller's buffer, size = data length.
 *                   "w"/"wb", "r+"/"w+": write (and read) within the FIXED
 *                   size given; a memstream never grows a caller's buffer.
 *                   buf == NULL: the stream allocates (and owns) the buffer.
 *                   "a"/append modes are not supported (returns NULL).
 *   open_memstream  growable write stream. *bufp/*sizep are updated on every
 *                   fflush and on fclose; the final buffer is NUL-terminated
 *                   and OWNED BY THE CALLER after fclose (POSIX semantics).
 *   fseek/ftell     SEEK_SET/CUR/END within [0, data length]. Seeking past
 *                   the end (POSIX zero-fill) is not supported.
 *
 * FILE field roles for memstreams:
 *   buffer   data bytes
 *   buf_size capacity
 *   buf_len  data length (read limit / write high-water mark)
 *   buf_pos  current position
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "stdio_impl.h"

typedef struct {
    char **user_bufp;   /* open_memstream: caller's pointer to sync */
    size_t *user_sizep; /* open_memstream: caller's length to sync */
    int growable;       /* open_memstream: capacity may grow */
    int owns_buffer;    /* free the data buffer on fclose */
} memstream_cookie;

/* Grow to hold at least `needed` data bytes plus a terminating NUL. */
static int memstream_reserve(FILE *f, memstream_cookie *ck, size_t needed) {
    size_t want = needed + 1;
    size_t cap;
    char *nbuf;

    if (want <= f->buf_size) return 0;
    if (!ck->growable) return -1;
    cap = f->buf_size ? f->buf_size : 64;
    while (cap < want) cap *= 2;
    nbuf = realloc(f->buffer, cap);
    if (!nbuf) return -1;
    f->buffer = nbuf;
    f->buf_size = cap;
    return 0;
}

static void memstream_sync(FILE *f, memstream_cookie *ck) {
    if (ck->user_bufp) {
        f->buffer[f->buf_len] = '\0';
        *ck->user_bufp = f->buffer;
        *ck->user_sizep = f->buf_len;
    }
}

size_t __memstream_read(FILE *f, void *p, size_t n) {
    size_t avail = f->buf_len - f->buf_pos;
    if (!(f->flags & FLAG_READ)) return 0;
    if (avail == 0) {
        f->eof = 1;
        return 0;
    }
    if (n > avail) n = avail;
    memcpy(p, f->buffer + f->buf_pos, n);
    f->buf_pos += n;
    return n;
}

size_t __memstream_write(FILE *f, const void *p, size_t n) {
    memstream_cookie *ck = f->mem_cookie;
    size_t end = f->buf_pos + n;

    if (!(f->flags & FLAG_WRITE) || n == 0) return 0;
    if (memstream_reserve(f, ck, end) != 0) {
        /* Fixed-size fmemopen buffer (or OOM): take what fits. POSIX lets a
         * fixed stream write up to size and error beyond it. */
        if (f->buf_pos >= f->buf_size ? 1 : 0) {
            f->error = 1;
            return 0;
        }
        /* Capacity includes the NUL slot only for growable streams; a fixed
         * buffer may be filled completely. */
        end = f->buf_size;
        if (end <= f->buf_pos) {
            f->error = 1;
            return 0;
        }
        n = end - f->buf_pos;
        f->error = 1;
    }
    memcpy(f->buffer + f->buf_pos, p, n);
    f->buf_pos = end <= f->buf_pos + n ? end : f->buf_pos + n;
    if (f->buf_pos > f->buf_len) f->buf_len = f->buf_pos;
    return n;
}

int __memstream_seek(FILE *f, long offset, int whence) {
    long base;
    long npos;

    switch (whence) {
    case SEEK_SET: base = 0; break;
    case SEEK_CUR: base = (long)f->buf_pos; break;
    case SEEK_END: base = (long)f->buf_len; break;
    default: return -1;
    }
    npos = base + offset;
    if (npos < 0 || (size_t)npos > f->buf_len) {
        f->error = 1;
        return -1;
    }
    f->buf_pos = (size_t)npos;
    f->eof = 0;
    return 0;
}

long __memstream_tell(FILE *f) {
    return (long)f->buf_pos;
}

int __memstream_flush(FILE *f) {
    memstream_sync(f, (memstream_cookie *)f->mem_cookie);
    return 0;
}

int __memstream_close(FILE *f) {
    memstream_cookie *ck = f->mem_cookie;
    memstream_sync(f, ck);
    if (ck->owns_buffer) free(f->buffer);
    free(ck);
    free(f);
    return 0;
}

static FILE *memstream_new(int flags) {
    FILE *f = calloc(1, sizeof(FILE));
    memstream_cookie *ck = calloc(1, sizeof(memstream_cookie));
    if (!f || !ck) {
        free(f);
        free(ck);
        return NULL;
    }
    f->flags = flags | FLAG_MEMSTREAM;
    f->fd = -1;
    f->ungetc_char = -1;
    f->mem_cookie = ck;
    return f;
}

FILE *fmemopen(void *buf, size_t size, const char *mode) {
    FILE *f;
    memstream_cookie *ck;
    int flags = 0;

    if (!mode || strchr(mode, 'a')) return NULL;
    if (strchr(mode, 'r')) flags |= FLAG_READ;
    if (strchr(mode, 'w')) flags |= FLAG_WRITE;
    if (strchr(mode, '+')) flags |= FLAG_READ | FLAG_WRITE;
    if (!flags) return NULL;

    f = memstream_new(flags);
    if (!f) return NULL;
    ck = f->mem_cookie;

    if (buf) {
        f->buffer = buf;
    } else {
        f->buffer = malloc(size ? size : 1);
        if (!f->buffer) {
            free(ck);
            free(f);
            return NULL;
        }
        memset(f->buffer, 0, size ? size : 1);
        ck->owns_buffer = 1;
    }
    f->buf_size = size;
    /* "r": the whole buffer is data. "w": starts empty (and truncates). */
    f->buf_len = (flags & FLAG_WRITE) && !strchr(mode, '+') ? 0
               : (strchr(mode, 'w') ? 0 : size);
    return f;
}

FILE *open_memstream(char **bufp, size_t *sizep) {
    FILE *f;
    memstream_cookie *ck;

    if (!bufp || !sizep) return NULL;
    f = memstream_new(FLAG_WRITE);
    if (!f) return NULL;
    ck = f->mem_cookie;

    f->buffer = malloc(64);
    if (!f->buffer) {
        free(ck);
        free(f);
        return NULL;
    }
    f->buf_size = 64;
    f->buffer[0] = '\0';
    ck->user_bufp = bufp;
    ck->user_sizep = sizep;
    ck->growable = 1;
    /* The caller owns the buffer after fclose; never free it here. */
    memstream_sync(f, ck);
    return f;
}
