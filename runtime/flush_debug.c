// SLOW-32 DEBUG instruction flush implementation
// This is the backend for the DEBUG version of libc

#include <stddef.h>

// FILE structure (must match stdio_buffered.c)
typedef struct FILE {
    char *buffer;
    char *ptr;
    size_t count;
    size_t size;
    int mode;
    int fd;
    int flags;
    void (*flush_fn)(struct FILE *);
} FILE;

// Flush buffer using DEBUG instruction
void __flush_debug(FILE *stream) {
    if (!stream || stream->count == 0) {
        return;
    }
    
    // Output each character using DEBUG instruction
    for (size_t i = 0; i < stream->count; i++) {
        char c = stream->buffer[i];
        __asm__ volatile("DEBUG %0, r0, r0" : : "r"(c));
    }
    
    // Reset buffer
    stream->ptr = stream->buffer;
    stream->count = 0;
}

// Direct putchar for compatibility (bypasses buffering)
int putchar_direct(int c) {
    __asm__ volatile("DEBUG %0, r0, r0" : : "r"(c));
    return c;
}