/*
 * stdio_impl.h — private FILE flag bits shared by the MMIO stdio family
 * (stdio.c, memstream.c). Not installed; runtime-internal.
 */
#ifndef _SLOW32_STDIO_IMPL_H_
#define _SLOW32_STDIO_IMPL_H_

#include <stdio.h>

#define FLAG_READ     0x01
#define FLAG_WRITE    0x02
#define FLAG_APPEND   0x04
#define FLAG_CREATE   0x08
#define FLAG_TRUNC    0x10
/* Stream is backed by guest memory, not an MMIO fd (memstream.c). Every
 * stdio entry point that would touch the fd must dispatch on this first. */
#define FLAG_MEMSTREAM 0x80

/* memstream.c internals, called from stdio.c's dispatch */
size_t __memstream_read(FILE *f, void *p, size_t n);
size_t __memstream_write(FILE *f, const void *p, size_t n);
int __memstream_seek(FILE *f, long offset, int whence);
long __memstream_tell(FILE *f);
int __memstream_flush(FILE *f);
int __memstream_close(FILE *f);

#endif
