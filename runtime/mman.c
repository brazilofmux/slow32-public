/* mmap and munmap for a machine with no MMU (<sys/mman.h>).
 *
 * What can be had is memory: an anonymous mapping is a page-aligned,
 * zero-filled block from the heap, private or "shared" alike (there is no
 * other process to share it with).  What cannot is said so: executable
 * memory (the code segment is the only one that runs, and it is not
 * writable) is EACCES, a file mapping ENODEV, a fixed address ENOMEM.
 * munmap frees a whole mapping; part of one is EINVAL, and a range that
 * holds no mapping is not an error (Linux's rule). */
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#define PAGE 4096u

struct mapping { void *base; size_t len; };
static struct mapping *maps;
static int nmaps, maps_cap;

void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset) {
    (void)addr;
    if (length == 0 || offset < 0) { errno = EINVAL; return MAP_FAILED; }
    if (prot & PROT_EXEC) { errno = EACCES; return MAP_FAILED; }
    if (!(flags & MAP_ANONYMOUS) || fd != -1) { errno = ENODEV; return MAP_FAILED; }
    if (flags & MAP_FIXED) { errno = ENOMEM; return MAP_FAILED; }
    if (!(flags & (MAP_PRIVATE | MAP_SHARED))) { errno = EINVAL; return MAP_FAILED; }
    if (length > (size_t)-1 - PAGE) { errno = ENOMEM; return MAP_FAILED; }
    size_t len = (length + PAGE - 1) & ~(size_t)(PAGE - 1);
    if (nmaps == maps_cap) {
        int ncap = maps_cap ? maps_cap * 2 : 16;
        struct mapping *n = realloc(maps, (size_t)ncap * sizeof *n);
        if (!n) { errno = ENOMEM; return MAP_FAILED; }
        maps = n;
        maps_cap = ncap;
    }
    void *p = aligned_alloc(PAGE, len);
    if (!p) { errno = ENOMEM; return MAP_FAILED; }
    memset(p, 0, len);
    maps[nmaps].base = p;
    maps[nmaps].len = len;
    nmaps++;
    return p;
}

int munmap(void *addr, size_t length) {
    if (length == 0 || ((size_t)addr & (PAGE - 1))) { errno = EINVAL; return -1; }
    size_t len = (length + PAGE - 1) & ~(size_t)(PAGE - 1);
    char *lo = (char *)addr, *hi = lo + len;
    for (int i = 0; i < nmaps; i++) {
        char *b = (char *)maps[i].base, *e = b + maps[i].len;
        if (hi <= b || lo >= e) continue;               /* no overlap */
        if (lo != b || len != maps[i].len) { errno = EINVAL; return -1; }
        free(maps[i].base);
        maps[i] = maps[--nmaps];
        return 0;
    }
    return 0;
}
