/* btree.h -- the indexed file's key tables as B+trees in one paged file.
 *
 * One file, `<data>.key`, of 4K pages.  Page 0 is the header; each key
 * (the RECORD KEY and every ALTERNATE RECORD KEY) is its own tree in the
 * same file.  A tree entry is fixed width -- the key's bytes, a 4-byte
 * big-endian arrival number, a 4-byte pointer -- and entries are ordered
 * by (key, arrival), so every entry is distinct, duplicates need no
 * special case anywhere, and equal keys come back in arrival order, which
 * is what READ NEXT under WITH DUPLICATES must deliver.  In a leaf the
 * pointer is the record's slot in the data file; in an internal node it
 * is the child page, and entry 0's key is "minus infinity" (its bytes are
 * ignored), so an internal node with n entries has n children.  Leaves
 * are linked both ways.  With 4K pages and a 20-byte key a page holds 145
 * entries: three levels cover 1.7 million records, four cover 200 million.
 * No key compression, no rebalancing on delete beyond freeing a page that
 * empties -- a leaf may sit half full; the tree never grows past what the
 * key count needs and the file's pages are reused through a free chain.
 *
 * A cursor is a lower bound, the (key, arrival) of the next entry to
 * deliver, not a position: inserts and deletes between two READ NEXTs
 * cannot invalidate it.  The page cache is a small LRU with pin counts,
 * sized from the heap the program was linked with (or S32_INDEX_CACHE
 * pages); pages go to the file through lseek/read/write, not stdio's
 * 1K-buffered streams.  A bitmap of live slots in the data file lets a
 * deleted slot be reused by a later WRITE.
 *
 * Single translation unit: include from the runtime (or a utility); the
 * fatal callback is the only way out.  Host-testable: no SLOW-32 in it. */

#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BT_PAGE      4096u
#define BT_MAGIC     "S32BT001"
#define BT_MAXKEYS   17u          /* the prime key and up to sixteen alternates */
#define BT_HDR       16u          /* page header: type, count, next, prev */
#define BT_MINCACHE  16u
#define BT_KEYMAX    255u

enum { BT_FREE = 0, BT_LEAF = 1, BT_NODE = 2, BT_BITMAP = 3 };

/* header (page 0) layout */
enum { BH_MAGIC = 0, BH_PAGESIZE = 8, BH_RECSIZE = 12, BH_KEYOFF = 16, BH_KEYLEN = 20,
       BH_NSLOTS = 24, BH_SEQ = 28, BH_NPAGES = 32, BH_FREE = 36, BH_NKEYS = 40, BH_BMFIRST = 44,
       BH_KEYS = 48, BH_KEYSZ = 24 };   /* per key: off, klen, dups, root, count, extra */
#define BT_BMBITS ((BT_PAGE - BT_HDR) * 8u)   /* live-slot bits per bitmap page */

typedef struct { unsigned off, klen, dups, root, count, extra; } btkey;   /* extra: payload bytes after the pointer, leaves only */

typedef struct {
    int fd;
    unsigned npages, free_head, nslots, seq, nkeys, bm_first;
    unsigned recsize, keyoff, keylen;
    btkey k[BT_MAXKEYS];
    int hdr_dirty;
    /* the cache */
    unsigned char *pages;         /* ncache pages */
    unsigned *pno;                /* page number in each frame, ~0u empty */
    unsigned *tick;               /* last use */
    unsigned char *dirty, *pin;
    unsigned ncache, clock;
    unsigned bm_hint;             /* no free slot below this (not persisted) */
    unsigned short *hmap;         /* pno -> frame guess, direct-mapped */
    void (*fatal)(const char *);
} btf;
#define BT_HMAP 1024u

static void bt_die(btf *b, const char *m) { b->fatal(m); exit(3); }

static void bt_put32(unsigned char *p, unsigned v) { p[0] = (unsigned char)v; p[1] = (unsigned char)(v >> 8); p[2] = (unsigned char)(v >> 16); p[3] = (unsigned char)(v >> 24); }
static unsigned bt_get32(const unsigned char *p) { return p[0] | (p[1] << 8) | (p[2] << 16) | ((unsigned)p[3] << 24); }
static void bt_putbe(unsigned char *p, unsigned v) { p[0] = (unsigned char)(v >> 24); p[1] = (unsigned char)(v >> 16); p[2] = (unsigned char)(v >> 8); p[3] = (unsigned char)v; }
static unsigned bt_getbe(const unsigned char *p) { return ((unsigned)p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3]; }

/* ---- pages: a pinned LRU cache over lseek/read/write ---- */

static int bt_frame_of(btf *b, unsigned pno)
{
    unsigned g = b->hmap[pno % BT_HMAP];
    if (g < b->ncache && b->pno[g] == pno) return (int)g;
    for (unsigned i = 0; i < b->ncache; i++) if (b->pno[i] == pno) { b->hmap[pno % BT_HMAP] = (unsigned short)i; return (int)i; }
    return -1;
}

static void bt_write_frame(btf *b, unsigned i)
{
    if (!b->dirty[i]) return;
    if (lseek(b->fd, (int)(b->pno[i] * BT_PAGE), SEEK_SET) < 0 ||
        write(b->fd, b->pages + (size_t)i * BT_PAGE, BT_PAGE) != (int)BT_PAGE) bt_die(b, "index: write failed");
    b->dirty[i] = 0;
}

static unsigned bt_victim(btf *b)
{
    int best = -1;
    for (unsigned i = 0; i < b->ncache; i++) {
        if (b->pno[i] == ~0u) return i;
        if (b->pin[i]) continue;
        if (best < 0 || b->tick[i] < b->tick[best]) best = (int)i;
    }
    if (best < 0) bt_die(b, "index: page cache exhausted (S32_INDEX_CACHE)");
    bt_write_frame(b, (unsigned)best);
    b->pno[best] = ~0u;
    return (unsigned)best;
}

/* the page, pinned; bt_unpin when done with the pointer */
static unsigned char *bt_pin(btf *b, unsigned pno)
{
    int i = bt_frame_of(b, pno);
    if (i < 0) {
        i = (int)bt_victim(b);
        unsigned char *p = b->pages + (size_t)i * BT_PAGE;
        if (pno < b->npages) {
            if (lseek(b->fd, (int)(pno * BT_PAGE), SEEK_SET) < 0) bt_die(b, "index: seek failed");
            int n = read(b->fd, p, BT_PAGE);
            if (n < 0) bt_die(b, "index: read failed");
            if ((unsigned)n < BT_PAGE) memset(p + n, 0, BT_PAGE - (unsigned)n);
        } else memset(p, 0, BT_PAGE);
        b->pno[i] = pno; b->dirty[i] = 0; b->pin[i] = 0;
        b->hmap[pno % BT_HMAP] = (unsigned short)i;
    }
    b->tick[i] = ++b->clock;
    b->pin[i]++;
    return b->pages + (size_t)i * BT_PAGE;
}

static void bt_unpin(btf *b, unsigned pno) { int i = bt_frame_of(b, pno); if (i >= 0 && b->pin[i]) b->pin[i]--; }
static void bt_dirty(btf *b, unsigned pno) { int i = bt_frame_of(b, pno); if (i >= 0) b->dirty[i] = 1; }

static unsigned bt_alloc_page(btf *b)
{
    unsigned pno;
    if (b->free_head) {
        pno = b->free_head;
        unsigned char *p = bt_pin(b, pno);
        b->free_head = bt_get32(p + 4);
        memset(p, 0, BT_PAGE);
        bt_dirty(b, pno); bt_unpin(b, pno);
    } else {
        pno = b->npages++;
        unsigned char *p = bt_pin(b, pno);
        memset(p, 0, BT_PAGE);
        bt_dirty(b, pno); bt_unpin(b, pno);
    }
    b->hdr_dirty = 1;
    return pno;
}

static void bt_free_page(btf *b, unsigned pno)
{
    unsigned char *p = bt_pin(b, pno);
    memset(p, 0, BT_HDR);
    p[0] = BT_FREE; bt_put32(p + 4, b->free_head);
    b->free_head = pno; b->hdr_dirty = 1;
    bt_dirty(b, pno); bt_unpin(b, pno);
}

/* ---- the header ---- */

static void bt_hdr_write(btf *b)
{
    unsigned char *h = bt_pin(b, 0);
    memset(h, 0, BT_PAGE);
    memcpy(h + BH_MAGIC, BT_MAGIC, 8);
    bt_put32(h + BH_PAGESIZE, BT_PAGE); bt_put32(h + BH_RECSIZE, b->recsize); bt_put32(h + BH_KEYOFF, b->keyoff); bt_put32(h + BH_KEYLEN, b->keylen);
    bt_put32(h + BH_NSLOTS, b->nslots); bt_put32(h + BH_SEQ, b->seq); bt_put32(h + BH_NPAGES, b->npages); bt_put32(h + BH_FREE, b->free_head);
    bt_put32(h + BH_NKEYS, b->nkeys); bt_put32(h + BH_BMFIRST, b->bm_first);
    for (unsigned i = 0; i < b->nkeys; i++) {
        unsigned char *k = h + BH_KEYS + i * BH_KEYSZ;
        bt_put32(k, b->k[i].off); bt_put32(k + 4, b->k[i].klen); bt_put32(k + 8, b->k[i].dups); bt_put32(k + 12, b->k[i].root); bt_put32(k + 16, b->k[i].count); bt_put32(k + 20, b->k[i].extra);
    }
    bt_dirty(b, 0); bt_unpin(b, 0);
    b->hdr_dirty = 0;
}

static int bt_hdr_read(btf *b)
{
    unsigned char *h = bt_pin(b, 0);
    int ok = !memcmp(h + BH_MAGIC, BT_MAGIC, 8) && bt_get32(h + BH_PAGESIZE) == BT_PAGE;
    if (ok) {
        b->recsize = bt_get32(h + BH_RECSIZE); b->keyoff = bt_get32(h + BH_KEYOFF); b->keylen = bt_get32(h + BH_KEYLEN);
        b->nslots = bt_get32(h + BH_NSLOTS); b->seq = bt_get32(h + BH_SEQ); b->npages = bt_get32(h + BH_NPAGES); b->free_head = bt_get32(h + BH_FREE);
        b->nkeys = bt_get32(h + BH_NKEYS); b->bm_first = bt_get32(h + BH_BMFIRST);
        if (b->nkeys > BT_MAXKEYS) ok = 0;
        for (unsigned i = 0; ok && i < b->nkeys; i++) {
            const unsigned char *k = h + BH_KEYS + i * BH_KEYSZ;
            b->k[i].off = bt_get32(k); b->k[i].klen = bt_get32(k + 4); b->k[i].dups = bt_get32(k + 8); b->k[i].root = bt_get32(k + 12); b->k[i].count = bt_get32(k + 16); b->k[i].extra = bt_get32(k + 20);
            if (!b->k[i].klen || b->k[i].klen > BT_KEYMAX) ok = 0;
        }
    }
    bt_unpin(b, 0);
    return ok;
}

/* ---- open / close ---- */

static void bt_cache_alloc(btf *b, unsigned ncache)
{
    if (ncache < BT_MINCACHE) ncache = BT_MINCACHE;
    for (;;) {
        b->pages = malloc((size_t)ncache * BT_PAGE);
        b->pno = malloc(ncache * sizeof *b->pno); b->tick = malloc(ncache * sizeof *b->tick);
        b->dirty = malloc(ncache); b->pin = malloc(ncache); b->hmap = malloc(BT_HMAP * sizeof *b->hmap);
        if (b->pages && b->pno && b->tick && b->dirty && b->pin && b->hmap) break;
        free(b->pages); free(b->pno); free(b->tick); free(b->dirty); free(b->pin); free(b->hmap);
        if (ncache <= BT_MINCACHE) bt_die(b, "index: out of memory for the page cache");
        ncache /= 2; if (ncache < BT_MINCACHE) ncache = BT_MINCACHE;
    }
    b->ncache = ncache;
    for (unsigned i = 0; i < ncache; i++) { b->pno[i] = ~0u; b->tick[i] = 0; b->dirty[i] = 0; b->pin[i] = 0; }
    for (unsigned i = 0; i < BT_HMAP; i++) b->hmap[i] = 0xFFFF;
    b->clock = 0; b->bm_hint = 0;
}

/* a new, empty key file: keys[] describe the trees; each gets an empty root leaf */
static int bt_create(btf *b, const char *path, unsigned ncache, unsigned recsize, unsigned keyoff, unsigned keylen,
                     const btkey *keys, unsigned nkeys, void (*fatal)(const char *))
{
    memset(b, 0, sizeof *b);
    b->fatal = fatal;
    b->fd = open(path, O_RDWR | O_CREAT | O_TRUNC, 0644);
    if (b->fd < 0) return 0;
    bt_cache_alloc(b, ncache);
    b->recsize = recsize; b->keyoff = keyoff; b->keylen = keylen; b->nkeys = nkeys;
    b->npages = 1;                                   /* the header */
    for (unsigned i = 0; i < nkeys; i++) {
        b->k[i] = keys[i]; b->k[i].count = 0;
        if (b->k[i].klen + 8 + b->k[i].extra > (BT_PAGE - BT_HDR) / 4) bt_die(b, "index: key too wide for a page");
        b->k[i].root = bt_alloc_page(b);
        unsigned char *p = bt_pin(b, b->k[i].root); p[0] = BT_LEAF; bt_dirty(b, b->k[i].root); bt_unpin(b, b->k[i].root);
    }
    b->hdr_dirty = 1;
    return 1;
}

/* an existing key file; 0 if it is not one of ours (the caller decides) */
static int bt_open(btf *b, const char *path, int rdonly, unsigned ncache, void (*fatal)(const char *))
{
    memset(b, 0, sizeof *b);
    b->fatal = fatal;
    b->fd = open(path, rdonly ? O_RDONLY : O_RDWR);
    if (b->fd < 0) return 0;
    bt_cache_alloc(b, ncache);
    b->npages = 1;                                   /* enough to read page 0 */
    if (!bt_hdr_read(b)) { close(b->fd); free(b->pages); free(b->pno); free(b->tick); free(b->dirty); free(b->pin); free(b->hmap); memset(b, 0, sizeof *b); b->fd = -1; return 0; }
    return 1;
}

static void bt_flush(btf *b)
{
    if (b->hdr_dirty) bt_hdr_write(b);
    for (unsigned i = 0; i < b->ncache; i++) if (b->pno[i] != ~0u) bt_write_frame(b, i);
}

static void bt_close(btf *b, int save)
{
    if (b->fd < 0) return;
    if (save) bt_flush(b);
    close(b->fd);
    free(b->pages); free(b->pno); free(b->tick); free(b->dirty); free(b->pin); free(b->hmap);
    memset(b, 0, sizeof *b); b->fd = -1;
}

/* ---- entries ---- */

static unsigned bt_esize(const btf *b, unsigned ki) { return b->k[ki].klen + 8; }                      /* a node entry */
static unsigned bt_lsize(const btf *b, unsigned ki) { return b->k[ki].klen + 8 + b->k[ki].extra; }    /* a leaf entry */
static unsigned bt_psize(const btf *b, unsigned ki, const unsigned char *p) { return p[0] == BT_LEAF ? bt_lsize(b, ki) : bt_esize(b, ki); }
static unsigned bt_cap_of(const btf *b, unsigned ki, const unsigned char *p) { return (BT_PAGE - BT_HDR) / bt_psize(b, ki, p); }
static unsigned bt_count(const unsigned char *p) { return p[2] | (p[3] << 8); }
static void bt_setcount(unsigned char *p, unsigned n) { p[2] = (unsigned char)n; p[3] = (unsigned char)(n >> 8); }
static unsigned char *bt_ent(unsigned char *p, unsigned es, unsigned i) { return p + BT_HDR + (size_t)i * es; }

/* the composite (key, arrival) is klen + 4 bytes; compare on n of them */
static int bt_cmp(const unsigned char *a, const unsigned char *b, unsigned n) { return memcmp(a, b, n); }

/* first entry in the page whose (key, arrival) >= target (on n bytes) */
static unsigned bt_lower(unsigned char *p, unsigned es, const unsigned char *target, unsigned n)
{
    unsigned lo = 0, hi = bt_count(p);
    while (lo < hi) {
        unsigned mid = (lo + hi) / 2;
        if (bt_cmp(bt_ent(p, es, mid), target, n) < 0) lo = mid + 1; else hi = mid;
    }
    return lo;
}

/* internal node: the child covering target = the last entry whose key <= target; entry 0 is -inf */
static unsigned bt_child_ix(unsigned char *p, unsigned es, const unsigned char *target, unsigned n)
{
    unsigned lo = 1, hi = bt_count(p);            /* first entry > target, in [1, count] */
    while (lo < hi) {
        unsigned mid = (lo + hi) / 2;
        if (bt_cmp(bt_ent(p, es, mid), target, n) <= 0) lo = mid + 1; else hi = mid;
    }
    return lo - 1;
}

#define BT_MAXDEPTH 16
typedef struct { unsigned page[BT_MAXDEPTH], ix[BT_MAXDEPTH]; unsigned depth; } btpath;

/* root to the leaf that would hold target; the path's pages are NOT pinned on return */
static unsigned bt_descend(btf *b, unsigned ki, const unsigned char *target, unsigned n, btpath *path)
{
    unsigned es = bt_esize(b, ki), pno = b->k[ki].root;
    path->depth = 0;
    for (;;) {
        unsigned char *p = bt_pin(b, pno);
        if (p[0] == BT_LEAF) { bt_unpin(b, pno); return pno; }
        if (path->depth >= BT_MAXDEPTH) bt_die(b, "index: tree too deep");
        unsigned ix = bt_child_ix(p, es, target, n);
        unsigned child = bt_get32(bt_ent(p, es, ix) + b->k[ki].klen + 4);
        path->page[path->depth] = pno; path->ix[path->depth] = ix; path->depth++;
        bt_unpin(b, pno);
        pno = child;
    }
}

/* ---- scanning: (page, index) positions along the leaf chain ---- */

/* the first entry >= target (n bytes of (key, arrival)); 0 if none; page and ix out. */
static int bt_first_ge(btf *b, unsigned ki, const unsigned char *target, unsigned n, unsigned *page, unsigned *ix)
{
    btpath path;
    unsigned es = bt_lsize(b, ki), pno = bt_descend(b, ki, target, n, &path);
    unsigned char *p = bt_pin(b, pno);
    unsigned i = bt_lower(p, es, target, n);
    while (i >= bt_count(p)) {                    /* past this leaf (or an empty one): the next */
        unsigned nx = bt_get32(p + 4);
        bt_unpin(b, pno);
        if (!nx) return 0;
        pno = nx; p = bt_pin(b, pno); i = 0;
    }
    bt_unpin(b, pno);
    *page = pno; *ix = i;
    return 1;
}

/* the entry after (page, ix); 0 at the end */
static int bt_step(btf *b, unsigned *page, unsigned *ix)
{
    unsigned pno = *page, i = *ix + 1;
    unsigned char *p = bt_pin(b, pno);
    while (i >= bt_count(p)) {
        unsigned nx = bt_get32(p + 4);
        bt_unpin(b, pno);
        if (!nx) return 0;
        pno = nx; p = bt_pin(b, pno); i = 0;
    }
    bt_unpin(b, pno);
    *page = pno; *ix = i;
    return 1;
}

/* the entry before (page, ix); 0 at the front.  ix may be count (one past the end). */
static int bt_back(btf *b, unsigned *page, unsigned *ix)
{
    unsigned pno = *page, i = *ix;
    while (i == 0) {
        unsigned char *p = bt_pin(b, pno);
        unsigned pv = bt_get32(p + 8);
        bt_unpin(b, pno);
        if (!pv) return 0;
        pno = pv; p = bt_pin(b, pno); i = bt_count(p); bt_unpin(b, pno);
    }
    *page = pno; *ix = i - 1;
    return 1;
}

/* the last entry of the tree; 0 if empty */
static int bt_last(btf *b, unsigned ki, unsigned *page, unsigned *ix)
{
    unsigned es = bt_esize(b, ki), pno = b->k[ki].root;    /* node entries on the way down */
    for (;;) {
        unsigned char *p = bt_pin(b, pno);
        if (p[0] == BT_LEAF) { unsigned c = bt_count(p); bt_unpin(b, pno); *page = pno; *ix = c; return bt_back(b, page, ix); }
        unsigned c = bt_count(p);
        unsigned child = bt_get32(bt_ent(p, es, c - 1) + b->k[ki].klen + 4);
        bt_unpin(b, pno); pno = child;
    }
}

/* read the entry at (page, ix): key+arrival into ka (klen+4 bytes), the pointer returned */
static unsigned bt_read(btf *b, unsigned ki, unsigned page, unsigned ix, unsigned char *ka)
{
    unsigned es = bt_lsize(b, ki), kl = b->k[ki].klen;
    unsigned char *p = bt_pin(b, page);
    unsigned char *e = bt_ent(p, es, ix);
    if (ka) memcpy(ka, e, kl + 4);
    unsigned ptr = bt_get32(e + kl + 4);
    bt_unpin(b, page);
    return ptr;
}

/* the leaf entry's extra payload, read or written */
static void bt_extra_get(btf *b, unsigned ki, unsigned page, unsigned ix, unsigned char *out)
{
    unsigned es = bt_lsize(b, ki), kl = b->k[ki].klen;
    unsigned char *p = bt_pin(b, page);
    memcpy(out, bt_ent(p, es, ix) + kl + 8, b->k[ki].extra);
    bt_unpin(b, page);
}
static void bt_extra_set(btf *b, unsigned ki, unsigned page, unsigned ix, const unsigned char *in)
{
    unsigned es = bt_lsize(b, ki), kl = b->k[ki].klen;
    unsigned char *p = bt_pin(b, page);
    memcpy(bt_ent(p, es, ix) + kl + 8, in, b->k[ki].extra);
    bt_dirty(b, page); bt_unpin(b, page);
}

/* ---- insert ---- */

/* put (ka, ptr) into page at ix, shifting; the page must have room */
static void bt_put_at(btf *b, unsigned ki, unsigned page, unsigned ix, const unsigned char *ka, unsigned ptr, const unsigned char *extra)
{
    unsigned kl = b->k[ki].klen;
    unsigned char *p = bt_pin(b, page);
    unsigned es = bt_psize(b, ki, p), c = bt_count(p);
    memmove(bt_ent(p, es, ix + 1), bt_ent(p, es, ix), (size_t)(c - ix) * es);
    unsigned char *e = bt_ent(p, es, ix);
    memcpy(e, ka, kl + 4); bt_put32(e + kl + 4, ptr);
    if (p[0] == BT_LEAF && b->k[ki].extra) { if (extra) memcpy(e + kl + 8, extra, b->k[ki].extra); else memset(e + kl + 8, 0, b->k[ki].extra); }
    bt_setcount(p, c + 1);
    bt_dirty(b, page); bt_unpin(b, page);
}

/* a full page splits: the upper half moves to a new sibling; the sibling's
 * first (key, arrival) goes up as its separator.  Returns the new page and
 * fills sep. */
static unsigned bt_split(btf *b, unsigned ki, unsigned page, unsigned char *sep)
{
    unsigned kl = b->k[ki].klen;
    unsigned npno = bt_alloc_page(b);
    unsigned char *p = bt_pin(b, page), *q = bt_pin(b, npno);
    unsigned es = bt_psize(b, ki, p), c = bt_count(p), half = c / 2, moved = c - half;
    q[0] = p[0];
    memcpy(bt_ent(q, es, 0), bt_ent(p, es, half), (size_t)moved * es);
    bt_setcount(q, moved); bt_setcount(p, half);
    memcpy(sep, bt_ent(q, es, 0), kl + 4);
    if (p[0] == BT_LEAF) {                        /* link: p <-> q <-> old next */
        unsigned nx = bt_get32(p + 4);
        bt_put32(q + 4, nx); bt_put32(q + 8, page); bt_put32(p + 4, npno);
        if (nx) { unsigned char *r = bt_pin(b, nx); bt_put32(r + 8, npno); bt_dirty(b, nx); bt_unpin(b, nx); }
    }
    bt_dirty(b, page); bt_dirty(b, npno);
    bt_unpin(b, page); bt_unpin(b, npno);
    return npno;
}

static void bt_insert_x(btf *b, unsigned ki, const unsigned char *key, unsigned arrival, unsigned slot, const unsigned char *extra)
{
    unsigned kl = b->k[ki].klen, es = bt_esize(b, ki), ncap = (BT_PAGE - BT_HDR) / es;
    unsigned char ka[BT_KEYMAX + 4], sep[BT_KEYMAX + 4];
    memcpy(ka, key, kl); bt_putbe(ka + kl, arrival);
    btpath path;
    unsigned leaf = bt_descend(b, ki, ka, kl + 4, &path);
    unsigned char *p = bt_pin(b, leaf);
    unsigned ix = bt_lower(p, bt_lsize(b, ki), ka, kl + 4), c = bt_count(p), lcap = bt_cap_of(b, ki, p);
    bt_unpin(b, leaf);
    bt_put_at(b, ki, leaf, ix, ka, slot, extra);
    b->k[ki].count++; b->hdr_dirty = 1;
    if (c + 1 < lcap) return;
    /* split up the path */
    unsigned page = leaf, child = bt_split(b, ki, page, sep);
    for (;;) {
        if (!path.depth) {                        /* the root split: a new root over the two */
            unsigned root = bt_alloc_page(b);
            unsigned char *r = bt_pin(b, root);
            r[0] = BT_NODE;
            memset(bt_ent(r, es, 0), 0, kl + 4); bt_put32(bt_ent(r, es, 0) + kl + 4, page);
            memcpy(bt_ent(r, es, 1), sep, kl + 4); bt_put32(bt_ent(r, es, 1) + kl + 4, child);
            bt_setcount(r, 2);
            bt_dirty(b, root); bt_unpin(b, root);
            b->k[ki].root = root; b->hdr_dirty = 1;
            return;
        }
        path.depth--;
        unsigned parent = path.page[path.depth], pix = path.ix[path.depth] + 1;
        unsigned char *q = bt_pin(b, parent);
        unsigned pc = bt_count(q);
        bt_unpin(b, parent);
        bt_put_at(b, ki, parent, pix, sep, child, 0);
        if (pc + 1 < ncap) return;
        page = parent; child = bt_split(b, ki, page, sep);
    }
}
static void bt_insert(btf *b, unsigned ki, const unsigned char *key, unsigned arrival, unsigned slot) { bt_insert_x(b, ki, key, arrival, slot, 0); }

/* ---- remove ---- */

static void bt_remove_at(btf *b, unsigned ki, unsigned page, unsigned ix)
{
    unsigned char *p = bt_pin(b, page);
    unsigned es = bt_psize(b, ki, p), c = bt_count(p);
    memmove(bt_ent(p, es, ix), bt_ent(p, es, ix + 1), (size_t)(c - ix - 1) * es);
    bt_setcount(p, c - 1);
    bt_dirty(b, page); bt_unpin(b, page);
}

/* a page along the path emptied: unlink it, free it, take its separator out of
 * the parent; a parent that empties goes the same way; a root left with one
 * child collapses onto it */
static void bt_collapse(btf *b, unsigned ki, btpath *path, unsigned page)
{
    unsigned es = bt_esize(b, ki), kl = b->k[ki].klen;
    for (;;) {
        unsigned char *p = bt_pin(b, page);
        int leaf = p[0] == BT_LEAF;
        if (leaf) {
            unsigned nx = bt_get32(p + 4), pv = bt_get32(p + 8);
            if (nx) { unsigned char *r = bt_pin(b, nx); bt_put32(r + 8, pv); bt_dirty(b, nx); bt_unpin(b, nx); }
            if (pv) { unsigned char *r = bt_pin(b, pv); bt_put32(r + 4, nx); bt_dirty(b, pv); bt_unpin(b, pv); }
        }
        bt_unpin(b, page);
        if (!path->depth) {                       /* the root itself emptied: keep it as an empty leaf */
            unsigned char *r = bt_pin(b, page);
            memset(r, 0, BT_PAGE); r[0] = BT_LEAF;
            bt_dirty(b, page); bt_unpin(b, page);
            return;
        }
        bt_free_page(b, page);
        path->depth--;
        unsigned parent = path->page[path->depth], pix = path->ix[path->depth];
        bt_remove_at(b, ki, parent, pix);
        unsigned char *q = bt_pin(b, parent);
        unsigned pc = bt_count(q);
        if (pc && pix == 0) memset(bt_ent(q, es, 0), 0, kl + 4), bt_dirty(b, parent);   /* the new first child is -inf */
        bt_unpin(b, parent);
        if (pc == 0) { page = parent; continue; }
        if (pc == 1 && path->depth == 0) {        /* a root with one child: that child is the root */
            unsigned char *r = bt_pin(b, parent);
            unsigned only = bt_get32(bt_ent(r, es, 0) + kl + 4);
            bt_unpin(b, parent);
            bt_free_page(b, parent);
            b->k[ki].root = only; b->hdr_dirty = 1;
        }
        return;
    }
}

/* remove the entry with this key that points at slot; 1 if found */
static int bt_remove(btf *b, unsigned ki, const unsigned char *key, unsigned slot)
{
    unsigned kl = b->k[ki].klen, es = bt_lsize(b, ki);
    unsigned char ka[BT_KEYMAX + 4];
    memcpy(ka, key, kl); bt_putbe(ka + kl, 0);
    btpath path;
    unsigned page = bt_descend(b, ki, ka, kl + 4, &path);
    unsigned char *p = bt_pin(b, page);
    unsigned ix = bt_lower(p, es, ka, kl + 4);
    bt_unpin(b, page);
    /* forward over the equal-key run, which may cross leaves */
    for (;;) {
        p = bt_pin(b, page);
        if (ix >= bt_count(p)) {
            unsigned nx = bt_get32(p + 4);
            bt_unpin(b, page);
            if (!nx) return 0;
            /* a later leaf: descend again to have a path for a possible collapse */
            unsigned char first[BT_KEYMAX + 4];
            unsigned char *q = bt_pin(b, nx); if (!bt_count(q)) { unsigned nn = bt_get32(q + 4); bt_unpin(b, nx); if (!nn) return 0; page = nx; ix = 0; continue; }
            memcpy(first, bt_ent(q, es, 0), kl + 4); bt_unpin(b, nx);
            page = bt_descend(b, ki, first, kl + 4, &path); ix = 0;
            continue;
        }
        unsigned char *e = bt_ent(p, es, ix);
        if (memcmp(e, key, kl) != 0) { bt_unpin(b, page); return 0; }
        if (bt_get32(e + kl + 4) == slot) { bt_unpin(b, page); break; }
        bt_unpin(b, page); ix++;
    }
    bt_remove_at(b, ki, page, ix);
    b->k[ki].count--; b->hdr_dirty = 1;
    p = bt_pin(b, page);
    unsigned c = bt_count(p);
    bt_unpin(b, page);
    if (!c) bt_collapse(b, ki, &path, page);
    return 1;
}

/* remove the entry (key, arrival) exactly; 1 if it was there */
static int bt_remove_exact(btf *b, unsigned ki, const unsigned char *key, unsigned arrival)
{
    unsigned kl = b->k[ki].klen, es = bt_lsize(b, ki);
    unsigned char ka[BT_KEYMAX + 4];
    memcpy(ka, key, kl); bt_putbe(ka + kl, arrival);
    btpath path;
    unsigned page = bt_descend(b, ki, ka, kl + 4, &path);
    unsigned char *p = bt_pin(b, page);
    unsigned ix = bt_lower(p, es, ka, kl + 4), c = bt_count(p);
    int hit = ix < c && !memcmp(bt_ent(p, es, ix), ka, kl + 4);
    bt_unpin(b, page);
    if (!hit) return 0;
    bt_remove_at(b, ki, page, ix);
    b->k[ki].count--; b->hdr_dirty = 1;
    if (c == 1) bt_collapse(b, ki, &path, page);
    return 1;
}

/* ---- the live-slot bitmap ---- */

static unsigned bt_bm_page(btf *b, unsigned slot, int create)
{
    unsigned want = slot / BT_BMBITS, pno = b->bm_first, prev = 0;
    for (unsigned i = 0; ; i++) {
        if (!pno) {
            if (!create) return 0;
            pno = bt_alloc_page(b);
            unsigned char *p = bt_pin(b, pno); p[0] = BT_BITMAP; bt_dirty(b, pno); bt_unpin(b, pno);
            if (prev) { unsigned char *q = bt_pin(b, prev); bt_put32(q + 4, pno); bt_dirty(b, prev); bt_unpin(b, prev); }
            else { b->bm_first = pno; b->hdr_dirty = 1; }
        }
        if (i == want) return pno;
        unsigned char *p = bt_pin(b, pno);
        unsigned nx = bt_get32(p + 4);
        bt_unpin(b, pno);
        prev = pno; pno = nx;
    }
}

static void bt_slot_set(btf *b, unsigned slot, int live)
{
    unsigned pno = bt_bm_page(b, slot, 1), bit = slot % BT_BMBITS;
    unsigned char *p = bt_pin(b, pno);
    if (live) p[BT_HDR + bit / 8] |= (unsigned char)(1u << (bit % 8));
    else { p[BT_HDR + bit / 8] &= (unsigned char)~(1u << (bit % 8)); if (slot < b->bm_hint) b->bm_hint = slot; }
    bt_dirty(b, pno); bt_unpin(b, pno);
}

static int bt_slot_live(btf *b, unsigned slot)
{
    unsigned pno = bt_bm_page(b, slot, 0);
    if (!pno) return 0;
    unsigned bit = slot % BT_BMBITS;
    unsigned char *p = bt_pin(b, pno);
    int r = (p[BT_HDR + bit / 8] >> (bit % 8)) & 1;
    bt_unpin(b, pno);
    return r;
}

/* a slot for a new record: the lowest free one below nslots, else a new one */
static unsigned bt_slot_alloc(btf *b)
{
    unsigned pno = b->bm_first, base = 0;
    while (pno && base < b->nslots) {
        if (base + BT_BMBITS <= b->bm_hint) {          /* this page is known full below the hint */
            unsigned char *p = bt_pin(b, pno); unsigned nx = bt_get32(p + 4); bt_unpin(b, pno);
            pno = nx; base += BT_BMBITS; continue;
        }
        unsigned char *p = bt_pin(b, pno);
        unsigned lim = b->nslots - base < BT_BMBITS ? b->nslots - base : BT_BMBITS;
        unsigned from = b->bm_hint > base ? (b->bm_hint - base) / 8 : 0;
        for (unsigned byte = from; byte * 8 < lim; byte++) {
            if (p[BT_HDR + byte] == 0xFF) continue;
            for (unsigned bit = 0; bit < 8 && byte * 8 + bit < lim; bit++)
                if (!((p[BT_HDR + byte] >> bit) & 1)) { bt_unpin(b, pno); unsigned s = base + byte * 8 + bit; bt_slot_set(b, s, 1); b->bm_hint = s + 1; return s; }
        }
        unsigned nx = bt_get32(p + 4);
        bt_unpin(b, pno);
        pno = nx; base += BT_BMBITS;
    }
    unsigned s = b->nslots++;
    b->hdr_dirty = 1;
    bt_slot_set(b, s, 1);
    b->bm_hint = s + 1;
    return s;
}
