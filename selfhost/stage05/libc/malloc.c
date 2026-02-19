/* malloc.c -- heap allocator for selfhost libc
 * Ported from runtime/malloc.c for cc-min/s32-cc compilation.
 * Uses a static heap array (no linker-provided symbols needed).
 * Single free-list with boundary tags and coalescing.
 *
 * Block layout:
 *   [size:4] [payload ...] [footer_size:4]
 * size field: total block size, LSB = allocated flag.
 * Free blocks additionally store next/prev pointers at offset 4,8.
 */

char *memset(char *dst, int c, int n);
char *memcpy(char *dst, char *src, int n);

#define HEAP_SIZE 1048576
static char heap_mem[HEAP_SIZE];

#define ALIGNMENT 8
#define ALLOC_BIT 1
#define SIZE_MASK -8
#define HDR_SIZE 8
#define FTR_SIZE 4
#define MIN_BLK 24

/* Free node: overlaid on free blocks at offset 0 */
struct fnode {
    int size;
    struct fnode *next;
    struct fnode *prev;
};

static struct fnode free_head;
static int heap_ready;

static int align_up(int v) {
    return (v + ALIGNMENT - 1) & SIZE_MASK;
}

static int blk_size(char *blk) {
    return *((int *)blk) & SIZE_MASK;
}

static int blk_alloc(char *blk) {
    return *((int *)blk) & ALLOC_BIT;
}

static void set_footer(char *blk, int sz) {
    *((int *)(blk + sz - FTR_SIZE)) = sz;
}

static void list_add(char *blk) {
    struct fnode *node;
    struct fnode *head;
    node = (struct fnode *)blk;
    head = &free_head;
    node->next = head->next;
    node->prev = head;
    head->next->prev = node;
    head->next = node;
}

static void list_remove(char *blk) {
    struct fnode *node;
    node = (struct fnode *)blk;
    node->prev->next = node->next;
    node->next->prev = node->prev;
}

static void heap_init(void) {
    int start;
    int astart;
    char *first;
    int sz;
    if (heap_ready) return;
    free_head.next = &free_head;
    free_head.prev = &free_head;
    free_head.size = 0;
    start = (int)heap_mem;
    astart = align_up(start);
    if (astart + MIN_BLK > (int)heap_mem + HEAP_SIZE) return;
    first = (char *)astart;
    sz = ((int)heap_mem + HEAP_SIZE - astart) & SIZE_MASK;
    if (sz < MIN_BLK) return;
    *((int *)first) = sz;
    set_footer(first, sz);
    list_add(first);
    heap_ready = 1;
}

char *malloc(int size) {
    int need;
    struct fnode *cur;
    struct fnode *sent;
    char *blk;
    int bsz;
    int rem;
    char *nb;
    if (size <= 0) return (char *)0;
    if (!heap_ready) heap_init();
    need = align_up(HDR_SIZE + align_up(size) + FTR_SIZE);
    if (need < MIN_BLK) need = MIN_BLK;
    sent = &free_head;
    cur = sent->next;
    while (cur != sent) {
        blk = (char *)cur;
        bsz = blk_size(blk);
        if (bsz >= need) {
            if (bsz >= need + MIN_BLK) {
                rem = bsz - need;
                list_remove(blk);
                *((int *)blk) = need | ALLOC_BIT;
                set_footer(blk, need);
                nb = blk + need;
                *((int *)nb) = rem;
                set_footer(nb, rem);
                list_add(nb);
            } else {
                list_remove(blk);
                *((int *)blk) = *((int *)blk) | ALLOC_BIT;
            }
            return blk + HDR_SIZE;
        }
        cur = cur->next;
    }
    return (char *)0;
}

void free(char *ptr) {
    char *blk;
    int sz;
    char *nblk;
    char *heap_end;
    int *pftr;
    int psz;
    char *pblk;
    if (!ptr) return;
    heap_end = heap_mem + HEAP_SIZE;
    blk = ptr - HDR_SIZE;
    sz = blk_size(blk);
    *((int *)blk) = *((int *)blk) & ~ALLOC_BIT;
    /* coalesce next */
    nblk = blk + sz;
    if (nblk < heap_end) {
        if (!blk_alloc(nblk)) {
            list_remove(nblk);
            sz = sz + blk_size(nblk);
            *((int *)blk) = sz;
            set_footer(blk, sz);
        }
    }
    /* coalesce prev */
    if (blk > (char *)heap_mem) {
        pftr = (int *)(blk - FTR_SIZE);
        if ((int)pftr >= (int)heap_mem) {
            psz = *pftr & SIZE_MASK;
            if (psz > 0) {
                pblk = blk - psz;
                if (!blk_alloc(pblk)) {
                    list_remove(pblk);
                    sz = blk_size(pblk) + sz;
                    *((int *)pblk) = sz;
                    set_footer(pblk, sz);
                    blk = pblk;
                }
            }
        }
    }
    list_add(blk);
}

char *calloc(int nmemb, int size) {
    int total;
    char *ptr;
    total = nmemb * size;
    if (nmemb != 0 && total / nmemb != size) return (char *)0;
    ptr = malloc(total);
    if (ptr) memset(ptr, 0, total);
    return ptr;
}

char *realloc(char *ptr, int size) {
    char *blk;
    int cur_sz;
    int payload;
    int need;
    char *nblk;
    int nsz;
    int ntot;
    int rem;
    char *sb;
    char *np;
    int cp;
    char *heap_end;
    if (!ptr) return malloc(size);
    if (size <= 0) { free(ptr); return (char *)0; }
    heap_end = heap_mem + HEAP_SIZE;
    blk = ptr - HDR_SIZE;
    cur_sz = blk_size(blk);
    payload = cur_sz - HDR_SIZE - FTR_SIZE;
    if (payload >= size) return ptr;
    need = align_up(HDR_SIZE + align_up(size) + FTR_SIZE);
    nblk = blk + cur_sz;
    if (nblk < heap_end && !blk_alloc(nblk)) {
        nsz = blk_size(nblk);
        if (cur_sz + nsz >= need) {
            list_remove(nblk);
            ntot = cur_sz + nsz;
            *((int *)blk) = ntot | ALLOC_BIT;
            set_footer(blk, ntot);
            if (ntot >= need + MIN_BLK) {
                rem = ntot - need;
                *((int *)blk) = need | ALLOC_BIT;
                set_footer(blk, need);
                sb = blk + need;
                *((int *)sb) = rem;
                set_footer(sb, rem);
                list_add(sb);
            }
            return ptr;
        }
    }
    np = malloc(size);
    if (np) {
        cp = payload;
        if (size < cp) cp = size;
        memcpy(np, ptr, cp);
        free(ptr);
    }
    return np;
}
