#ifndef SLOW32_SYS_MMAN_H
#define SLOW32_SYS_MMAN_H

#include <stddef.h>
#include <stdint.h>

#define PROT_NONE  0
#define PROT_READ  1
#define PROT_WRITE 2
#define PROT_EXEC  4

#define MAP_SHARED  1
#define MAP_PRIVATE 2
#define MAP_FIXED   0x10
#define MAP_ANONYMOUS 0x20

#define MAP_FAILED ((void *)-1)

void *mmap(void *addr, size_t length, int prot, int flags, int fd, uint64_t offset);
int munmap(void *addr, size_t length);

#endif
