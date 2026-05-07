/* malloc.c -- Simple bump allocator via brk syscall */

char *sys_brk(char *addr);
char *memset(char *dst, int c, int n);
char *memcpy(char *dst, char *src, int n);

static char *heap_cur;
static char *heap_end;

static void heap_init(void) {
    if (heap_cur) return;
    heap_cur = sys_brk(0);
    heap_end = heap_cur;
}

char *malloc(int size) {
    char *p;
    char *new_end;
    int grow;
    heap_init();

    /* Align to 16 bytes + 16-byte header */
    size = (size + 15) & ~15;
    size = size + 16;

    if (heap_cur + size > heap_end) {
        grow = size;
        if (grow < 65536) grow = 65536;
        new_end = sys_brk(heap_end + grow);
        if (new_end == heap_end) return 0;
        heap_end = new_end;
    }

    p = heap_cur;
    *(int *)p = size;
    heap_cur = heap_cur + size;
    return p + 16;
}

void free(char *ptr) {
    /* bump allocator: no-op */
}

char *calloc(int n, int size) {
    /* Our bump allocator never reuses memory: brk grows monotonically,
     * free is a no-op, and the kernel guarantees fresh brk pages are
     * zero-filled.  An explicit memset would eagerly touch every page --
     * disastrous when calloc is used for 256MB-sized buffers (the
     * SLOW-32 emulator's `mem` array is one such case), causing 67K
     * page faults on startup.  Skip it; the pages are already zero. */
    return malloc(n * size);
}

char *realloc(char *ptr, int size) {
    char *p;
    int old_size;
    if (!ptr) return malloc(size);

    old_size = *(int *)(ptr - 16) - 16;
    if (size <= old_size) return ptr;
    p = malloc(size);
    if (p) memcpy(p, ptr, old_size);
    return p;
}
