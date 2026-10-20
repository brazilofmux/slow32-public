#ifndef SLOW32_SYS_MMAN_H
#define SLOW32_SYS_MMAN_H

#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

#define PROT_NONE  0
#define PROT_READ  1
#define PROT_WRITE 2
#define PROT_EXEC  4

#define MAP_SHARED  1
#define MAP_PRIVATE 2
#define MAP_FIXED   0x10
#define MAP_ANONYMOUS 0x20

#define MAP_FAILED ((void *)-1)

/* The machine has no MMU (mman.c): a mapping is anonymous memory from the
 * heap, page-aligned and zero-filled.  Executable memory there is none --
 * code runs only from the code segment, W^X -- so PROT_EXEC is EACCES; a
 * file cannot be mapped (ENODEV), nor an address chosen (MAP_FIXED,
 * ENOMEM).  munmap takes back a whole mapping; part of one is EINVAL. */
#define MAP_ANON MAP_ANONYMOUS
void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset);
int munmap(void *addr, size_t length);

#ifdef __cplusplus
}
#endif
#endif
