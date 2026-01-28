// SLOW-32 Buffered stdio implementation
// This file is shared between DEBUG and MMIO versions of libc

#include <stdio.h>

// Buffer size for stdio streams
#define STDIO_BUFFER_SIZE 256

// Buffering modes
#define _IONBF 0  // No buffering
#define _IOLBF 1  // Line buffering
#define _IOFBF 2  // Full buffering

// File flags
#define _IOEOF  0x01  // End of file reached
#define _IOERR  0x02  // Error occurred
#define _IOREAD 0x04  // File open for reading
#define _IOWRITE 0x08 // File open for writing

// Static buffers for standard streams
static char stdout_buffer[STDIO_BUFFER_SIZE];
static char stderr_buffer[STDIO_BUFFER_SIZE];

// External flush functions (provided by flush_debug.c or flush_mmio.c)
extern void __flush_debug(FILE *stream);
extern void __flush_mmio(FILE *stream);

// Standard streams
FILE __stdout = {
    .buffer = stdout_buffer,
    .ptr = stdout_buffer,
    .count = 0,
    .size = STDIO_BUFFER_SIZE,
    .mode = _IOLBF,  // Line buffered
    .fd = 1,
    .flags = _IOWRITE,
#ifdef USE_MMIO
    .flush_fn = __flush_mmio
#else
    .flush_fn = __flush_debug
#endif
};

FILE __stderr = {
    .buffer = stderr_buffer,
    .ptr = stderr_buffer,
    .count = 0,
    .size = STDIO_BUFFER_SIZE,
    .mode = _IONBF,  // Unbuffered
    .fd = 2,
    .flags = _IOWRITE,
#ifdef USE_MMIO
    .flush_fn = __flush_mmio
#else
    .flush_fn = __flush_debug
#endif
};

FILE *stdout = &__stdout;
FILE *stderr = &__stderr;
FILE *stdin = NULL;  // Not implemented yet

// Flush a stream
int fflush(FILE *stream) {
    if (!stream || !(stream->flags & _IOWRITE)) {
        return -1;
    }
    
    if (stream->count > 0 && stream->flush_fn) {
        stream->flush_fn(stream);
        stream->ptr = stream->buffer;
        stream->count = 0;
    }
    
    return 0;
}

// Write a character to a stream
int fputc(int c, FILE *stream) {
    if (!stream || !(stream->flags & _IOWRITE)) {
        return -1;
    }
    
    // Unbuffered mode - flush immediately
    if (stream->mode == _IONBF) {
        char ch = (char)c;
        stream->buffer[0] = ch;
        stream->ptr = stream->buffer;
        stream->count = 1;
        stream->flush_fn(stream);
        stream->count = 0;
        return c;
    }
    
    // Add to buffer
    *stream->ptr++ = (char)c;
    stream->count++;
    
    // Flush if buffer full or newline in line-buffered mode
    if (stream->count >= stream->size || 
        (stream->mode == _IOLBF && c == '\n')) {
        fflush(stream);
    }
    
    return c;
}

// Write a string to a stream
int fputs(const char *s, FILE *stream) {
    if (!s || !stream) {
        return -1;
    }
    
    while (*s) {
        if (fputc(*s++, stream) == -1) {
            return -1;
        }
    }
    
    return 0;
}

// Write binary data to a stream
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
    if (!ptr || !stream || !(stream->flags & _IOWRITE)) {
        return 0;
    }
    
    const char *data = (const char *)ptr;
    size_t total = size * nmemb;
    size_t written = 0;
    
    while (written < total) {
        if (fputc(data[written++], stream) == -1) {
            break;
        }
    }
    
    return written / size;
}

// Convenience functions for stdout
__attribute__((used))
int putchar(int c) {
    return fputc(c, stdout);
}

__attribute__((used))
int puts(const char *s) {
    if (fputs(s, stdout) == -1) {
        return -1;
    }
    // Add newline as per standard puts
    if (fputc('\n', stdout) == -1) {
        return -1;
    }
    return 0;
}

// Initialize stdio (called from crt0)
void __stdio_init(void) {
    // Reset stream pointers
    stdout->ptr = stdout->buffer;
    stdout->count = 0;
    stderr->ptr = stderr->buffer;
    stderr->count = 0;
}

// Cleanup stdio (called at exit)
void __stdio_cleanup(void) {
    fflush(stdout);
    fflush(stderr);
}