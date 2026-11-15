#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "index.h"
#include "util.h"
#include "date.h"
#include "ast.h"

static void page_free(ndx_page_t *p);
static int page_flush(index_t *idx, int page_no);
static void hash_add(index_t *idx, ndx_page_t *p);
static void hash_remove(index_t *idx, ndx_page_t *p);
static ndx_page_t *hash_lookup(index_t *idx, int page_no);
static ndx_page_t *page_get(index_t *idx, int page_no);
static void page_put(ndx_page_t *p);

/* Page cache capacity.  Work-area index slots are zeroed, and index_init() is
 * not on the open path, so this was 0 there: cache_evict() then freed every
 * unpinned page after each fetch, and a SEEK re-read its whole root-to-leaf
 * path from disk on every call (a build likewise).  Applied at open and build. */
#define INDEX_CACHE_PAGES 512
static void index_cache_setup(index_t *idx) {
    if (idx->cache_capacity <= 0) idx->cache_capacity = INDEX_CACHE_PAGES;
}

void index_clear_key_ast(index_t *idx) {
    if (idx->key_ast) {
        ast_free(idx->key_ast);
        idx->key_ast = NULL;
    }
}

int index_compile_key_ast(index_t *idx, struct memvar_store *store) {
    const char *err = NULL;
    index_clear_key_ast(idx);
    idx->key_has_macro = (strchr(idx->key_expr, '&') != NULL);
    if (idx->key_has_macro || !idx->key_expr[0])
        return 0;
    idx->key_ast = ast_compile(idx->key_expr, store, &err);
    return idx->key_ast ? 0 : -1;
}

static void encode_sortable_double(double n, char *out) {
    static const char hex[] = "0123456789ABCDEF";
    uint64_t bits;
    int i;

    /* Normalize NaN to zero for deterministic key generation. */
    if (n != n) n = 0.0;

    memcpy(&bits, &n, sizeof(bits));
    if (bits & 0x8000000000000000ULL)
        bits = ~bits;
    else
        bits ^= 0x8000000000000000ULL;

    for (i = 15; i >= 0; i--) {
        out[i] = hex[bits & 0x0F];
        bits >>= 4;
    }
    out[16] = '\0';
}

static double decode_sortable_double(const char *in) {
    uint64_t bits = 0;
    double n;
    int i;
    for (i = 0; i < 16; i++) {
        char c = in[i];
        bits = (bits << 4) | (uint64_t)(c >= 'A' ? c - 'A' + 10 : c - '0');
    }
    if (bits & 0x8000000000000000ULL)
        bits ^= 0x8000000000000000ULL;
    else
        bits = ~bits;
    memcpy(&n, &bits, sizeof(n));
    return n;
}

/* A numeric or date key (type 1): the double dBASE keeps, a date as its
 * Julian day, as sixteen characters that sort as it does */
void index_format_key_value(int key_type, const value_t *val, char *buf, int size) {
    if (size <= 0) return;
    buf[0] = '\0';

    if (key_type == 1) {
        char tmp[17];
        double n = 0.0;
        if (val->type == VAL_NUM) n = val->num;
        else if (val->type == VAL_DATE) n = (double)val->date;
        else if (val->type == VAL_CHAR) n = atof(val->str);
        encode_sortable_double(n, tmp);
        str_copy(buf, tmp, size);
        return;
    }

    val_to_string(val, buf, size);
}

/* A key as the file holds it, and back */
static void key_to_disk(const index_t *idx, const char *mem, unsigned char *disk) {
    if (idx->key_type == 1) {
        double n = decode_sortable_double(mem);
        memcpy(disk, &n, 8);
    } else {
        memcpy(disk, mem, idx->disk_key_len);
    }
}

static void key_from_disk(const index_t *idx, const unsigned char *disk, char *mem) {
    if (idx->key_type == 1) {
        double n;
        char tmp[17];
        memcpy(&n, disk, 8);
        encode_sortable_double(n, tmp);
        memcpy(mem, tmp, 16);
    } else {
        memcpy(mem, disk, idx->disk_key_len);
    }
}

/* The leaf chain the file does not keep */
static int leaf_link_room(index_t *idx, int page_no) {
    if (page_no < idx->leaf_link_cap) return 0;
    {
        int cap = idx->leaf_link_cap ? idx->leaf_link_cap : 64;
        int *nl;
        while (cap <= page_no) cap *= 2;
        nl = (int *)realloc(idx->leaf_link, (size_t)cap * 2 * sizeof(int));
        if (!nl) return -1;
        memset(nl + 2 * idx->leaf_link_cap, 0, (size_t)(cap - idx->leaf_link_cap) * 2 * sizeof(int));
        idx->leaf_link = nl;
        idx->leaf_link_cap = cap;
    }
    return 0;
}

static void leaf_link_set(index_t *idx, int page_no, int next, int prev) {
    if (leaf_link_room(idx, page_no) < 0) return;
    idx->leaf_link[2 * page_no] = next;
    idx->leaf_link[2 * page_no + 1] = prev;
}

/* ---- Key comparison ---- */
static int key_cmp(const char *a, const char *b, int key_len) {
    return memcmp(a, b, key_len);
}

static char *page_key(index_t *idx, ndx_page_t *page, int i) {
    return page->keys + i * idx->key_len;
}

/* ---- LRU cache helpers ---- */
static void cache_remove(index_t *idx, ndx_page_t *p) {
    if (!p) return;
    if (p->lru_prev) p->lru_prev->lru_next = p->lru_next;
    if (p->lru_next) p->lru_next->lru_prev = p->lru_prev;
    if (idx->lru_head == p) idx->lru_head = p->lru_next;
    if (idx->lru_tail == p) idx->lru_tail = p->lru_prev;
    p->lru_prev = NULL;
    p->lru_next = NULL;
    if (idx->cache_size > 0) idx->cache_size--;
}

static void cache_add(index_t *idx, ndx_page_t *p) {
    if (!p) return;
    p->lru_prev = NULL;
    p->lru_next = idx->lru_head;
    if (idx->lru_head) idx->lru_head->lru_prev = p;
    idx->lru_head = p;
    if (!idx->lru_tail) idx->lru_tail = p;
    idx->cache_size++;
}

static void cache_touch(index_t *idx, ndx_page_t *p) {
    if (!p || idx->lru_head == p) return;
    cache_remove(idx, p);
    cache_add(idx, p);
}

static void cache_evict(index_t *idx) {
    ndx_page_t *p;
    if (!idx->fp) return; /* don't evict if we can't flush */
    while (idx->cache_size > idx->cache_capacity && idx->lru_tail) {
        p = idx->lru_tail;
        while (p && p->pin_count > 0)
            p = p->lru_prev;
        if (!p) return; /* all pages pinned */
        if (p->dirty) page_flush(idx, p->page_no);
        cache_remove(idx, p);
        hash_remove(idx, p);
        page_free(p);
    }
}

/* ---- Page Hash Table (Sparse Page Table) ---- */
static int page_hash_idx(int page_no) {
    return (unsigned int)page_no % PAGE_HASH_SIZE;
}

static ndx_page_t *hash_lookup(index_t *idx, int page_no) {
    int h = page_hash_idx(page_no);
    ndx_page_t *p = idx->page_hash[h];
    while (p) {
        if (p->page_no == page_no) return p;
        p = p->hash_next;
    }
    return NULL;
}

static void hash_add(index_t *idx, ndx_page_t *p) {
    int h = page_hash_idx(p->page_no);
    p->hash_next = idx->page_hash[h];
    idx->page_hash[h] = p;
}

static void hash_remove(index_t *idx, ndx_page_t *p) {
    int h = page_hash_idx(p->page_no);
    ndx_page_t **curr = &idx->page_hash[h];
    while (*curr) {
        if (*curr == p) {
            *curr = p->hash_next;
            p->hash_next = NULL;
            return;
        }
        curr = &(*curr)->hash_next;
    }
}

static void page_reclaim(index_t *idx, ndx_page_t *p) {
    p->type = PAGE_FREE;
    p->nkeys = 0;
    if (p->children) {
        memset(p->children, 0, (idx->alloc_keys + 1) * sizeof(int));
    } else {
        p->children = (int *)calloc(idx->alloc_keys + 1, sizeof(int));
    }
    p->children[0] = idx->free_page_head;
    idx->free_page_head = p->page_no;
    p->dirty = 1;
}

/* ---- Page allocation ---- */
/* A page's arrays for its type, room for one key past the most a page
 * holds (an insert overflows before the split).  alloc_keys covers the
 * file's own pages too, which may be fuller than ours. */
static int page_arrays(index_t *idx, ndx_page_t *p) {
    p->keys = (char *)calloc(idx->alloc_keys + 1, idx->key_len);
    if (p->type == PAGE_LEAF) {
        p->recnos = (uint32_t *)calloc(idx->alloc_keys + 1, sizeof(uint32_t));
        return p->keys && p->recnos ? 0 : -1;
    }
    p->children = (int *)calloc(idx->alloc_keys + 2, sizeof(int));
    return p->keys && p->children ? 0 : -1;
}

static ndx_page_t *page_new(index_t *idx, int type) {
    ndx_page_t *p;
    int pg_no;

    if (idx->free_page_head > 0) {
        pg_no = idx->free_page_head;
        p = page_get(idx, pg_no);
        if (p) {
            idx->free_page_head = p->children[0];
            /* Re-initialize reuse page.  Free all arrays and reallocate
               for the target type to avoid buffer size mismatches when a
               leaf page is recycled as internal (or vice versa). */
            p->type = type;
            p->nkeys = 0;
            p->dirty = 1;
            p->next_leaf = 0;
            p->prev_leaf = 0;
            free(p->keys);      p->keys = NULL;
            free(p->recnos);    p->recnos = NULL;
            free(p->children);  p->children = NULL;

            if (page_arrays(idx, p) < 0) { page_free(p); return NULL; }
            return p;
        }
    }

    pg_no = idx->num_pages;
    idx->num_pages++;

    p = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
    if (!p) return NULL;

    p->page_no = pg_no;
    p->type = type;
    p->nkeys = 0;
    p->dirty = 1;
    p->next_leaf = 0;
    p->prev_leaf = 0;
    p->pin_count = 1;
    p->lru_prev = NULL;
    p->lru_next = NULL;

    if (page_arrays(idx, p) < 0) { page_free(p); return NULL; }

    hash_add(idx, p);
    cache_add(idx, p);
    cache_evict(idx);
    return p;
}

static void page_free(ndx_page_t *p) {
    if (!p) return;
    if (p->keys) free(p->keys);
    if (p->recnos) free(p->recnos);
    if (p->children) free(p->children);
    free(p);
}

/* ---- Page I/O ---- */
static void put16(unsigned char *b, int v) { b[0] = (unsigned char)v; b[1] = (unsigned char)(v >> 8); }
static void put32(unsigned char *b, uint32_t v) {
    b[0] = (unsigned char)v; b[1] = (unsigned char)(v >> 8);
    b[2] = (unsigned char)(v >> 16); b[3] = (unsigned char)(v >> 24);
}
static int get16(const unsigned char *b) { return b[0] | (b[1] << 8); }
static uint32_t get32(const unsigned char *b) {
    return (uint32_t)b[0] | ((uint32_t)b[1] << 8) | ((uint32_t)b[2] << 16) | ((uint32_t)b[3] << 24);
}

/* the header: dBASE III's fields, and ours where dBASE leaves zeros */
static void header_image(const index_t *idx, unsigned char *buf) {
    int klen = (int)strlen(idx->key_expr);
    put32(buf + 0, (uint32_t)idx->root_page);
    put32(buf + 4, (uint32_t)idx->num_pages);
    put16(buf + 12, idx->disk_key_len);
    put16(buf + 14, idx->keys_per_page);
    put16(buf + 16, idx->key_type == 1 ? 1 : 0);
    put16(buf + 18, idx->item_size);
    buf[22] = (unsigned char)((idx->descending ? NDX_FLAG_DESC : 0) |
                              (idx->for_expr[0] ? NDX_FLAG_FOR : 0));
    buf[23] = (unsigned char)(idx->unique ? 1 : 0);
    memcpy(buf + 24, idx->key_expr, (size_t)klen);   /* index_build kept both in NDX_EXPR_ROOM */
    if (idx->for_expr[0])
        memcpy(buf + 24 + klen + 1, idx->for_expr, strlen(idx->for_expr));
}

static int page_flush(index_t *idx, int page_no) {
    unsigned char buf[NDX_PAGE];
    ndx_page_t *p;
    int i;

    if (!idx->fp) return -1;
    p = hash_lookup(idx, page_no);
    if (page_no != 0 && !p) return -1;

    memset(buf, 0, NDX_PAGE);

    if (page_no == 0) {
        header_image(idx, buf);
    } else if (p->type == PAGE_FREE) {
        /* nothing points here: dBASE keeps no free list, nor does the file */
    } else if (p->type == PAGE_INTERNAL) {
        unsigned char *it = buf + 4;
        put32(buf, (uint32_t)p->nkeys);
        for (i = 0; i < p->nkeys; i++, it += idx->item_size) {
            put32(it, (uint32_t)p->children[i]);
            key_to_disk(idx, page_key(idx, p, i), it + 8);
        }
        put32(it, (uint32_t)p->children[p->nkeys]);   /* the rightmost page: a pointer only */
    } else {
        unsigned char *it = buf + 4;
        put32(buf, (uint32_t)p->nkeys);
        for (i = 0; i < p->nkeys; i++, it += idx->item_size) {
            put32(it + 4, p->recnos[i]);
            key_to_disk(idx, page_key(idx, p, i), it + 8);
        }
        leaf_link_set(idx, page_no, p->next_leaf, p->prev_leaf);
    }

    fseek(idx->fp, (long)page_no * NDX_PAGE, SEEK_SET);
    if (fwrite(buf, NDX_PAGE, 1, idx->fp) != 1)
        return -1;
    fflush(idx->fp);
    if (p) p->dirty = 0;
    return 0;
}

/* A page from the file.  A leaf's first item has no left page; a page
 * with no keys is an empty leaf unless its one pointer names another
 * page (dBASE leaves an emptied root's old pointer behind). */
static ndx_page_t *page_read(index_t *idx, int page_no, unsigned char *buf) {
    ndx_page_t *p;
    int i, n, cap;
    uint32_t left0;
    const unsigned char *it = buf + 4;

    if (page_no == 0) return NULL; /* header page handled separately */

    /* the count is a 32-bit field, read as dBASE and Quicksilver read it,
     * its low 16 bits: an empty page of Teacher's Pet's has 0x80000000 */
    n = get16(buf);
    cap = (NDX_PAGE - 4) / idx->item_size;   /* the most items a page can hold */
    if (n < 0 || n > cap) return NULL;       /* not a page of this index */
    left0 = get32(it);

    p = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
    if (!p) return NULL;
    p->page_no = page_no;
    p->nkeys = n;
    p->type = (left0 == 0 || (n == 0 && (left0 >= (uint32_t)idx->num_pages || left0 == (uint32_t)page_no)))
              ? PAGE_LEAF : PAGE_INTERNAL;
    if (page_arrays(idx, p) < 0) { page_free(p); return NULL; }

    for (i = 0; i < n; i++, it += idx->item_size) {
        if (p->type == PAGE_INTERNAL) p->children[i] = (int)get32(it);
        else p->recnos[i] = get32(it + 4);
        key_from_disk(idx, it + 8, page_key(idx, p, i));
    }
    if (p->type == PAGE_INTERNAL) {
        if (4 + n * idx->item_size + 4 > NDX_PAGE) { page_free(p); return NULL; }
        p->children[n] = (int)get32(it);
        for (i = 0; i <= n; i++)
            if (p->children[i] <= 0 || p->children[i] >= idx->num_pages) { page_free(p); return NULL; }
    } else if (page_no < idx->leaf_link_cap) {
        p->next_leaf = idx->leaf_link[2 * page_no];
        p->prev_leaf = idx->leaf_link[2 * page_no + 1];
    }
    return p;
}

static ndx_page_t *page_get(index_t *idx, int page_no) {
    ndx_page_t *p;
    unsigned char buf[NDX_PAGE];

    if (page_no < 0 || page_no >= idx->num_pages) return NULL;
    p = hash_lookup(idx, page_no);
    if (p) {
        p->pin_count++;
        cache_touch(idx, p);
        return p;
    }

    if (!idx->fp) return NULL;

    fseek(idx->fp, (long)page_no * NDX_PAGE, SEEK_SET);
    if (fread(buf, NDX_PAGE, 1, idx->fp) != 1)
        return NULL;

    p = page_read(idx, page_no, buf);
    if (!p) return NULL;
    p->pin_count = 1;
    hash_add(idx, p);
    cache_add(idx, p);
    cache_evict(idx);
    return p;
}

static void page_put(ndx_page_t *p) {
    if (!p) return;
    if (p->pin_count > 0) p->pin_count--;
}

/* ---- Free all tree pages ---- */
static void free_all_pages(index_t *idx) {
    int i;
    for (i = 0; i < PAGE_HASH_SIZE; i++) {
        ndx_page_t *p = idx->page_hash[i];
        while (p) {
            ndx_page_t *next = p->hash_next;
            page_free(p);
            p = next;
        }
        idx->page_hash[i] = NULL;
    }
    idx->num_pages = 0;
    idx->cache_size = 0;
    idx->lru_head = NULL;
    idx->lru_tail = NULL;
    idx->free_page_head = 0;
    if (idx->leaf_link)
        memset(idx->leaf_link, 0, (size_t)idx->leaf_link_cap * 2 * sizeof(int));
}

/* ---- Init ---- */
void index_init(index_t *idx) {
    memset(idx, 0, sizeof(index_t));
    idx->iter_page = -1;
    idx->iter_pos = -1;
    idx->cache_capacity = INDEX_CACHE_PAGES;
    idx->key_ast = NULL;
    idx->key_has_macro = 0;
    idx->for_expr[0] = '\0';
    idx->for_ast = NULL;
    idx->descending = 0;
}

/* ---- Close ---- */
void index_close(index_t *idx) {
    free_all_pages(idx);
    free(idx->leaf_link);
    idx->leaf_link = NULL;
    idx->leaf_link_cap = 0;
    index_clear_key_ast(idx);
    if (idx->for_ast) { ast_free(idx->for_ast); idx->for_ast = NULL; }
    if (idx->fp) {
        fclose(idx->fp);
        idx->fp = NULL;
    }
    idx->nentries = 0;
    idx->root_page = 0;
    idx->first_leaf = 0;
    idx->iter_page = -1;
    idx->iter_pos = -1;
    idx->active = 0;
    idx->filename[0] = '\0';
    idx->key_expr[0] = '\0';
    idx->key_has_macro = 0;
    idx->for_expr[0] = '\0';
    idx->descending = 0;
}

/* ---- Page geometry from the key: dBASE III's ---- */
/* An item is the left page, the record number and the key rounded up to
 * four bytes; a page is a count and as many items as fit.  An internal
 * page needs one item more than its keys, for the rightmost page. */
static void compute_fanout(index_t *idx) {
    idx->disk_key_len = idx->key_type == 1 ? 8 : idx->key_len;
    idx->item_size = 8 + ((idx->disk_key_len + 3) & ~3);
    idx->keys_per_page = (NDX_PAGE - 4) / idx->item_size;
    idx->max_keys_leaf = idx->keys_per_page;
    idx->max_keys_internal = idx->keys_per_page - 1;
    idx->alloc_keys = (NDX_PAGE - 4) / idx->item_size + 1;
}

/* ---- Binary search in a page ---- */
/* Returns index of first key >= search_key (for leaves: where to insert) */
static int page_lower_bound(index_t *idx, ndx_page_t *page, const char *key) {
    int lo = 0, hi = page->nkeys;
    while (lo < hi) {
        int mid = (lo + hi) / 2;
        if (key_cmp(page_key(idx, page, mid), key, idx->key_len) < 0)
            lo = mid + 1;
        else
            hi = mid;
    }
    return lo;
}

/* ---- Binary search: upper bound ---- */
/* Returns index of first key > search_key (for internal routing) */
static int page_upper_bound(index_t *idx, ndx_page_t *page, const char *key) {
    int lo = 0, hi = page->nkeys;
    while (lo < hi) {
        int mid = (lo + hi) / 2;
        if (key_cmp(page_key(idx, page, mid), key, idx->key_len) <= 0)
            lo = mid + 1;
        else
            hi = mid;
    }
    return lo;
}

/* Whether the last entry under page_no is at or past (key, recno): a
 * greater key, or the key with a greater record number.  An insert goes
 * into the first child, of those a run of equal keys spans, for which
 * this holds -- equal keys stay in record order when an older record
 * takes a key already there (a REPLACE), not only when one is appended. */
static int last_entry_ge(index_t *idx, int page_no, const char *key, uint32_t recno) {
    ndx_page_t *p = page_get(idx, page_no);
    int ge = 0;
    while (p && p->type == PAGE_INTERNAL) {
        int child = p->children[p->nkeys];
        page_put(p);
        p = page_get(idx, child);
    }
    if (!p) return 0;
    if (p->nkeys > 0) {
        int c = key_cmp(page_key(idx, p, p->nkeys - 1), key, idx->key_len);
        ge = c > 0 || (c == 0 && p->recnos[p->nkeys - 1] >= recno);
    }
    page_put(p);
    return ge;
}

/* ---- Search: find leaf and position for key ---- */
/* The FIRST leaf that can hold key: the first child whose key -- the
 * last key under it -- is not below it.  A run of equal keys a split
 * divided starts there (routing past it found the run part-way, and
 * Teacher's Pet's grade report lost a student's first grades).  A search
 * that ends past a leaf's last key goes on into the next. */
static ndx_page_t *bt_find_leaf(index_t *idx, const char *key) {
    ndx_page_t *node;
    if (idx->num_pages <= 1) return NULL;
    node = page_get(idx, idx->root_page);
    while (node && node->type == PAGE_INTERNAL) {
        int pos = page_lower_bound(idx, node, key);
        int child_no = node->children[pos];
        page_put(node);
        /* descend to children[pos] */
        node = page_get(idx, child_no);
    }
    return node; /* pinned */
}

/* ---- Key existence check (for UNIQUE validation) ---- */
int index_key_exists(index_t *idx, const char *key, uint32_t exclude_recno) {
    char padded[MAX_INDEX_KEY];
    ndx_page_t *leaf;
    int pos;

    if (!idx->active || idx->nentries == 0) return 0;

    memset(padded, ' ', idx->key_len);
    {
        int len = strlen(key);
        if (len > idx->key_len) len = idx->key_len;
        memcpy(padded, key, len);
    }

    leaf = bt_find_leaf(idx, padded);
    if (!leaf) return 0;

    /* the run of equal keys, across leaves */
    pos = page_lower_bound(idx, leaf, padded);
    for (;;) {
        if (pos >= leaf->nkeys) {
            int next = leaf->next_leaf;
            page_put(leaf);
            if (!next || !(leaf = page_get(idx, next))) return 0;
            pos = 0;
            continue;
        }
        if (key_cmp(page_key(idx, leaf, pos), padded, idx->key_len) != 0) break;
        if (leaf->recnos[pos] != exclude_recno) {
            page_put(leaf);
            return 1; /* duplicate exists */
        }
        pos++;
    }
    page_put(leaf);
    return 0;
}

/* ---- Seek ---- */
int index_seek(index_t *idx, const char *key, int match_len) {
    ndx_page_t *leaf;
    char padded[MAX_INDEX_KEY];
    int pos;
    int cmp_len;

    if (!idx->active || idx->nentries == 0) {
        idx->iter_page = -1;
        idx->iter_pos = -1;
        return 0;
    }

    /* Pad key to key_len */
    memset(padded, ' ', idx->key_len);
    {
        int len = strlen(key);
        if (len > idx->key_len) len = idx->key_len;
        memcpy(padded, key, len);
    }

    /* For match comparison: use match_len if specified, else full key_len */
    cmp_len = (match_len > 0 && match_len < idx->key_len) ? match_len : idx->key_len;

    leaf = bt_find_leaf(idx, padded);
    if (!leaf) {
        idx->iter_page = -1;
        idx->iter_pos = -1;
        return 0;
    }

    pos = page_lower_bound(idx, leaf, padded);

    if (pos < leaf->nkeys) {
        idx->iter_page = leaf->page_no;
        idx->iter_pos = pos;
        if (key_cmp(page_key(idx, leaf, pos), padded, cmp_len) == 0) {
            page_put(leaf);
            return 1; /* match */
        }
        page_put(leaf);
        return 0;
    }

    /* Past end of this leaf -- try next leaf */
    if (leaf->next_leaf) {
        ndx_page_t *next = page_get(idx, leaf->next_leaf);
        idx->iter_page = leaf->next_leaf;
        idx->iter_pos = 0;
        if (next && next->nkeys > 0 &&
            key_cmp(page_key(idx, next, 0), padded, cmp_len) == 0) {
            page_put(next);
            page_put(leaf);
            return 1; /* match at boundary */
        }
        page_put(next);
    } else {
        /* Past end of entire index */
        idx->iter_page = leaf->page_no;
        idx->iter_pos = leaf->nkeys;
    }
    page_put(leaf);
    return 0;
}

/* ---- Current record number ---- */
uint32_t index_current_recno(const index_t *idx) {
    ndx_page_t *page;
    if (idx->iter_page < 0 || idx->iter_page >= idx->num_pages)
        return 0;
    page = page_get((index_t *)idx, idx->iter_page);
    if (!page || page->type != PAGE_LEAF) {
        if (page) page_put(page);
        return 0;
    }
    if (idx->iter_pos < 0 || idx->iter_pos >= page->nkeys)
    {
        page_put(page);
        return 0;
    }
    {
        uint32_t rec = page->recnos[idx->iter_pos];
        page_put(page);
        return rec;
    }
}

/* ---- Navigation (ascending-order primitives) ---- */
static void index_top_asc(index_t *idx) {
    if (!idx->active || idx->nentries == 0 || idx->first_leaf == 0) {
        idx->iter_page = -1;
        idx->iter_pos = -1;
        return;
    }
    idx->iter_page = idx->first_leaf;
    idx->iter_pos = 0;
}

static void index_bottom_asc(index_t *idx) {
    int pg;
    if (!idx->active || idx->nentries == 0 || idx->first_leaf == 0) {
        idx->iter_page = -1;
        idx->iter_pos = -1;
        return;
    }
    /* Follow leaf chain to last leaf */
    pg = idx->first_leaf;
    for (;;) {
        ndx_page_t *page = page_get(idx, pg);
        if (!page) break;
        if (!page->next_leaf) {
            idx->iter_page = pg;
            idx->iter_pos = page->nkeys - 1;
            page_put(page);
            return;
        }
        pg = page->next_leaf;
        page_put(page);
    }
}

static int index_next_asc(index_t *idx) {
    ndx_page_t *page;
    if (idx->iter_page < 0 || idx->iter_page >= idx->num_pages)
        return -1;
    page = page_get(idx, idx->iter_page);
    if (!page || page->type != PAGE_LEAF) {
        if (page) page_put(page);
        return -1;
    }

    idx->iter_pos++;
    if (idx->iter_pos >= page->nkeys) {
        if (!page->next_leaf) {
            page_put(page);
            return -1; /* EOF */
        }
        idx->iter_page = page->next_leaf;
        idx->iter_pos = 0;
    }
    page_put(page);
    return 0;
}

static int index_prev_asc(index_t *idx) {
    ndx_page_t *page;
    if (idx->iter_page < 0 || idx->iter_page >= idx->num_pages)
        return -1;
    page = page_get(idx, idx->iter_page);
    if (!page || page->type != PAGE_LEAF) {
        if (page) page_put(page);
        return -1;
    }

    idx->iter_pos--;
    if (idx->iter_pos < 0) {
        if (!page->prev_leaf) {
            page_put(page);
            return -1; /* BOF */
        }
        idx->iter_page = page->prev_leaf;
        {
            ndx_page_t *prev = page_get(idx, idx->iter_page);
            if (!prev) {
                page_put(page);
                return -1;
            }
            idx->iter_pos = prev->nkeys - 1;
            page_put(prev);
        }
    }
    page_put(page);
    return 0;
}

/* ---- Public navigation: descending flag reverses direction ---- */
void index_top(index_t *idx) {
    if (idx->descending) index_bottom_asc(idx);
    else                 index_top_asc(idx);
}

void index_bottom(index_t *idx) {
    if (idx->descending) index_top_asc(idx);
    else                 index_bottom_asc(idx);
}

int index_next(index_t *idx) {
    if (idx->descending) return index_prev_asc(idx);
    else                 return index_next_asc(idx);
}

int index_prev(index_t *idx) {
    if (idx->descending) return index_next_asc(idx);
    else                 return index_prev_asc(idx);
}

/* ---- Leaf insert at position (assumes not full) ---- */
static void leaf_insert_at(index_t *idx, ndx_page_t *leaf, int pos,
                           const char *key, uint32_t recno) {
    int i;
    /* Shift right */
    for (i = leaf->nkeys; i > pos; i--) {
        memcpy(page_key(idx, leaf, i), page_key(idx, leaf, i - 1), idx->key_len);
        leaf->recnos[i] = leaf->recnos[i - 1];
    }
    memcpy(page_key(idx, leaf, pos), key, idx->key_len);
    leaf->recnos[pos] = recno;
    leaf->nkeys++;
    leaf->dirty = 1;
}

/* ---- Split a leaf page ---- */
/* Splits 'leaf' in two. The right half goes into a new page.
   Returns the new page. Sets *split_key to the last key left behind: an
   internal key is the last key under its left page, as dBASE keeps it. */
static ndx_page_t *leaf_split(index_t *idx, ndx_page_t *leaf, char *split_key) {
    ndx_page_t *right;
    int mid, i;

    mid = leaf->nkeys / 2;
    right = page_new(idx, PAGE_LEAF);
    if (!right) return NULL;

    /* Copy right half to new page */
    for (i = mid; i < leaf->nkeys; i++) {
        int j = i - mid;
        memcpy(page_key(idx, right, j), page_key(idx, leaf, i), idx->key_len);
        right->recnos[j] = leaf->recnos[i];
    }
    right->nkeys = leaf->nkeys - mid;
    leaf->nkeys = mid;

    /* Update leaf chain */
    right->next_leaf = leaf->next_leaf;
    right->prev_leaf = leaf->page_no;
    leaf->next_leaf = right->page_no;
    if (right->next_leaf) {
        ndx_page_t *next = page_get(idx, right->next_leaf);
        if (next) {
            next->prev_leaf = right->page_no;
            next->dirty = 1;
            page_put(next);
        }
    }

    leaf->dirty = 1;
    right->dirty = 1;

    memcpy(split_key, page_key(idx, leaf, leaf->nkeys - 1), idx->key_len);

    return right;
}

/* ---- Internal node: insert key and right child at position ---- */
static void internal_insert_at(index_t *idx, ndx_page_t *node, int pos,
                                const char *key, int right_child) {
    int i;
    /* Shift keys and children right */
    for (i = node->nkeys; i > pos; i--) {
        memcpy(page_key(idx, node, i), page_key(idx, node, i - 1), idx->key_len);
        node->children[i + 1] = node->children[i];
    }
    memcpy(page_key(idx, node, pos), key, idx->key_len);
    node->children[pos + 1] = right_child;
    node->nkeys++;
    node->dirty = 1;
}

/* ---- Split an internal node ---- */
static ndx_page_t *internal_split(index_t *idx, ndx_page_t *node, char *promote_key) {
    ndx_page_t *right;
    int mid, i;

    mid = node->nkeys / 2;
    right = page_new(idx, PAGE_INTERNAL);
    if (!right) return NULL;

    /* The middle key gets promoted to parent */
    memcpy(promote_key, page_key(idx, node, mid), idx->key_len);

    /* Copy right half (after mid) to new node */
    for (i = mid + 1; i < node->nkeys; i++) {
        int j = i - mid - 1;
        memcpy(page_key(idx, right, j), page_key(idx, node, i), idx->key_len);
    }
    /* Copy children from mid+1 onwards */
    for (i = mid + 1; i <= node->nkeys; i++) {
        right->children[i - mid - 1] = node->children[i];
    }
    right->nkeys = node->nkeys - mid - 1;
    node->nkeys = mid;

    node->dirty = 1;
    right->dirty = 1;

    return right;
}

/* ---- Insert ---- */
int index_insert(index_t *idx, const char *key, uint32_t recno) {
    char padded[MAX_INDEX_KEY];
    ndx_page_t *node;
    int path[64];      /* stack of page numbers on descent path */
    int path_pos[64];  /* child index taken at each level */
    int depth = 0;
    int pos;
    char split_key[MAX_INDEX_KEY];

    if (!idx->active) return -1;

    /* Pad key */
    memset(padded, ' ', idx->key_len);
    {
        int len = strlen(key);
        if (len > idx->key_len) len = idx->key_len;
        memcpy(padded, key, len);
    }

    /* Handle empty tree */
    if (idx->num_pages <= 1) {
        /* Create root leaf (page 1, since page 0 is header) */
        ndx_page_t *leaf = page_new(idx, PAGE_LEAF);
        if (!leaf) return -1;
        leaf_insert_at(idx, leaf, 0, padded, recno);
        idx->root_page = leaf->page_no;
        idx->first_leaf = leaf->page_no;
        idx->nentries = 1;
        page_put(leaf);
        return 0;
    }

    /* Walk down tree to find leaf: among the children a run of equal keys
     * spans (lo to hi), the first whose last entry is at or past (key,
     * recno) -- see last_entry_ge; a unique key goes where its equal
     * would be */
    node = page_get(idx, idx->root_page);
    while (node->type == PAGE_INTERNAL) {
        int hi = page_upper_bound(idx, node, padded);
        pos = page_lower_bound(idx, node, padded);
        if (!idx->unique)
            while (pos < hi && !last_entry_ge(idx, node->children[pos], padded, recno))
                pos++;
        path[depth] = node->page_no;
        path_pos[depth] = pos;
        depth++;
        {
            int child_no = node->children[pos];
            page_put(node);
            node = page_get(idx, child_no);
        }
    }

    /* node is now a leaf. Insert into it: among equal keys, in record order
     * (an appended record after the ones already there) */
    pos = page_lower_bound(idx, node, padded);
    if (!idx->unique)
        while (pos < node->nkeys && node->recnos[pos] < recno &&
               key_cmp(page_key(idx, node, pos), padded, idx->key_len) == 0)
            pos++;
    if (idx->unique) {
        if (pos < node->nkeys &&
            key_cmp(page_key(idx, node, pos), padded, idx->key_len) == 0) {
            /* Duplicate key in unique index - reject */
            page_put(node);
            return 1;
        }
        /* its equal may stand just across a leaf boundary, either way */
        {
            int nb = pos == 0 ? node->prev_leaf : pos == node->nkeys ? node->next_leaf : 0;
            ndx_page_t *other = nb ? page_get(idx, nb) : NULL;
            int dup = other && other->nkeys > 0 &&
                key_cmp(page_key(idx, other, pos == 0 ? other->nkeys - 1 : 0), padded, idx->key_len) == 0;
            if (other) page_put(other);
            if (dup) { page_put(node); return 1; }
        }
    }
    leaf_insert_at(idx, node, pos, padded, recno);
    idx->nentries++;

    /* If leaf is over capacity, split upward */
    while (node->nkeys > (node->type == PAGE_LEAF ? idx->max_keys_leaf : idx->max_keys_internal)) {
        ndx_page_t *right;

        if (node->type == PAGE_LEAF) {
            right = leaf_split(idx, node, split_key);
        } else {
            right = internal_split(idx, node, split_key);
        }
        if (!right) return -1;

        if (depth == 0) {
            /* node is root: create new root */
            ndx_page_t *new_root = page_new(idx, PAGE_INTERNAL);
            if (!new_root) return -1;
            new_root->children[0] = node->page_no;
            internal_insert_at(idx, new_root, 0, split_key, right->page_no);
            idx->root_page = new_root->page_no;
            page_put(new_root);
            page_put(right);
            page_put(node);
            node = NULL;
            break;
        }

        /* Insert split_key into parent */
        depth--;
        {
            ndx_page_t *parent = page_get(idx, path[depth]);
            page_put(node);
            node = parent;
        }
        internal_insert_at(idx, node, path_pos[depth], split_key, right->page_no);
        page_put(right);
        /* Loop continues if parent also overflows */
    }

    /* Update first_leaf if needed */
    {
        ndx_page_t *fl = (idx->first_leaf > 0) ? page_get(idx, idx->first_leaf) : NULL;
        if (fl && fl->prev_leaf) {
            /* first_leaf is wrong; walk back */
            while (fl->prev_leaf) {
                int prev_no = fl->prev_leaf;
                page_put(fl);
                fl = page_get(idx, prev_no);
                if (!fl) break;
            }
            if (fl) idx->first_leaf = fl->page_no;
        }
        page_put(fl);
    }

    if (node) page_put(node);
    return 0;
}

/* ---- Remove ---- */

/* Remove entry from leaf at position pos */
static void leaf_remove_at(index_t *idx, ndx_page_t *leaf, int pos) {
    int i;
    for (i = pos; i < leaf->nkeys - 1; i++) {
        memcpy(page_key(idx, leaf, i), page_key(idx, leaf, i + 1), idx->key_len);
        leaf->recnos[i] = leaf->recnos[i + 1];
    }
    leaf->nkeys--;
    leaf->dirty = 1;
}

static void internal_remove_at(index_t *idx, ndx_page_t *node, int pos) {
    int i;
    for (i = pos; i < node->nkeys - 1; i++) {
        memcpy(page_key(idx, node, i), page_key(idx, node, i + 1), idx->key_len);
        node->children[i + 1] = node->children[i + 2];
    }
    node->nkeys--;
    node->dirty = 1;
}

static int bt_remove(index_t *idx, int page_no, const char *key, uint32_t recno) {
    ndx_page_t *node = page_get(idx, page_no);
    if (!node) return 0;

    if (node->type == PAGE_INTERNAL) {
        /* every child whose range can hold key: a run of equal keys may
         * span several (routing to the last alone missed an entry in an
         * earlier one, and left it in the index) */
        int lo = page_lower_bound(idx, node, key), hi = page_upper_bound(idx, node, key);
        int pos, removed = 0;
        for (pos = lo; pos <= hi && !removed; pos++)
            removed = bt_remove(idx, node->children[pos], key, recno);
        if (!removed) {
            page_put(node);
            return 0;
        }
        pos--;   /* the child that held it */
        
        /* Check for underflow in the child we just returned from */
        ndx_page_t *child = page_get(idx, node->children[pos]);
        int min_keys = (child->type == PAGE_LEAF) ? ((idx->max_keys_leaf + 1) / 2) : ((idx->max_keys_internal + 1) / 2);
        
        if (child->nkeys < min_keys && child->page_no != idx->root_page) {
            /* Try to borrow from siblings */
            ndx_page_t *left = NULL, *right = NULL;
            if (pos > 0) left = page_get(idx, node->children[pos-1]);
            if (pos < node->nkeys) right = page_get(idx, node->children[pos+1]);

            if (left && left->nkeys > min_keys) {
                /* Borrow from left */
                if (child->type == PAGE_LEAF) {
                    leaf_insert_at(idx, child, 0, page_key(idx, left, left->nkeys - 1), left->recnos[left->nkeys - 1]);
                    left->nkeys--;
                    left->dirty = 1;
                    /* the left page's key: its new last */
                    memcpy(page_key(idx, node, pos - 1), page_key(idx, left, left->nkeys - 1), idx->key_len);
                    node->dirty = 1;
                } else {
                    /* Internal borrow: rotate through parent */
                    for (int i = child->nkeys; i > 0; i--) {
                        memcpy(page_key(idx, child, i), page_key(idx, child, i - 1), idx->key_len);
                        child->children[i + 1] = child->children[i];
                    }
                    child->children[1] = child->children[0];
                    memcpy(page_key(idx, child, 0), page_key(idx, node, pos - 1), idx->key_len);
                    child->children[0] = left->children[left->nkeys];
                    child->nkeys++;
                    child->dirty = 1;
                    memcpy(page_key(idx, node, pos - 1), page_key(idx, left, left->nkeys - 1), idx->key_len);
                    left->nkeys--;
                    left->dirty = 1;
                    node->dirty = 1;
                }
            } else if (right && right->nkeys > min_keys) {
                /* Borrow from right */
                if (child->type == PAGE_LEAF) {
                    leaf_insert_at(idx, child, child->nkeys, page_key(idx, right, 0), right->recnos[0]);
                    leaf_remove_at(idx, right, 0);
                    /* the child's key: the entry it took, now its last */
                    memcpy(page_key(idx, node, pos), page_key(idx, child, child->nkeys - 1), idx->key_len);
                    node->dirty = 1;
                } else {
                    /* Internal borrow: rotate through parent */
                    memcpy(page_key(idx, child, child->nkeys), page_key(idx, node, pos), idx->key_len);
                    child->children[child->nkeys + 1] = right->children[0];
                    child->nkeys++;
                    child->dirty = 1;
                    memcpy(page_key(idx, node, pos), page_key(idx, right, 0), idx->key_len);
                    right->children[0] = right->children[1];
                    internal_remove_at(idx, right, 0);
                    node->dirty = 1;
                }
            } else {
                /* Must merge */
                if (left) {
                    /* Merge child into left */
                    if (child->type == PAGE_LEAF) {
                        for (int i = 0; i < child->nkeys; i++) {
                            leaf_insert_at(idx, left, left->nkeys, page_key(idx, child, i), child->recnos[i]);
                        }
                        if (idx->first_leaf == child->page_no) idx->first_leaf = left->page_no;
                        left->next_leaf = child->next_leaf;
                        if (child->next_leaf) {
                            ndx_page_t *next = page_get(idx, child->next_leaf);
                            if (next) { next->prev_leaf = left->page_no; next->dirty = 1; page_put(next); }
                        }
                    } else {
                        /* Internal merge: pull down parent separator */
                        memcpy(page_key(idx, left, left->nkeys), page_key(idx, node, pos - 1), idx->key_len);
                        left->children[left->nkeys + 1] = child->children[0];
                        left->nkeys++;
                        for (int i = 0; i < child->nkeys; i++) {
                            memcpy(page_key(idx, left, left->nkeys), page_key(idx, child, i), idx->key_len);
                            left->children[left->nkeys + 1] = child->children[i + 1];
                            left->nkeys++;
                        }
                    }
                    left->dirty = 1;
                    internal_remove_at(idx, node, pos - 1);
                    page_reclaim(idx, child);
                } else if (right) {
                    /* Merge right into child */
                    if (child->type == PAGE_LEAF) {
                        for (int i = 0; i < right->nkeys; i++) {
                            leaf_insert_at(idx, child, child->nkeys, page_key(idx, right, i), right->recnos[i]);
                        }
                        if (idx->first_leaf == right->page_no) idx->first_leaf = child->page_no;
                        child->next_leaf = right->next_leaf;
                        if (right->next_leaf) {
                            ndx_page_t *next = page_get(idx, right->next_leaf);
                            if (next) { next->prev_leaf = child->page_no; next->dirty = 1; page_put(next); }
                        }
                    } else {
                        /* Internal merge: pull down parent separator */
                        memcpy(page_key(idx, child, child->nkeys), page_key(idx, node, pos), idx->key_len);
                        child->children[child->nkeys + 1] = right->children[0];
                        child->nkeys++;
                        for (int i = 0; i < right->nkeys; i++) {
                            memcpy(page_key(idx, child, child->nkeys), page_key(idx, right, i), idx->key_len);
                            child->children[child->nkeys + 1] = right->children[i + 1];
                            child->nkeys++;
                        }
                    }
                    child->dirty = 1;
                    internal_remove_at(idx, node, pos);
                    page_reclaim(idx, right);
                }
            }
            if (left) page_put(left);
            if (right) page_put(right);
        }
        page_put(child);
        page_put(node);
        return 1;
    } else {
        /* Leaf: find exact key+recno and remove */
        int found = -1;
        for (int i = 0; i < node->nkeys; i++) {
            if (key_cmp(page_key(idx, node, i), key, idx->key_len) == 0 && node->recnos[i] == recno) {
                found = i;
                break;
            }
        }
        if (found >= 0) {
            leaf_remove_at(idx, node, found);
            idx->nentries--;
            page_put(node);
            return 1;
        }
    }
    page_put(node);
    return 0;
}

int index_remove(index_t *idx, const char *key, uint32_t recno) {
    char padded[MAX_INDEX_KEY];
    int removed;

    if (!idx->active || idx->nentries == 0) return -1;

    memset(padded, ' ', idx->key_len);
    {
        int len = strlen(key);
        if (len > idx->key_len) len = idx->key_len;
        memcpy(padded, key, len);
    }

    removed = bt_remove(idx, idx->root_page, padded, recno);
    if (!removed) return -1;

    /* Check if root needs to be collapsed */
    ndx_page_t *root = page_get(idx, idx->root_page);
    if (root && root->type == PAGE_INTERNAL && root->nkeys == 0) {
        int new_root = root->children[0];
        page_reclaim(idx, root);
        idx->root_page = new_root;
        /* Also update first_leaf if we collapsed to nothing */
        if (idx->nentries == 0) idx->first_leaf = 0;
    }
    if (root) page_put(root);

    if (idx->nentries == 0 && idx->root_page != 0) {
        /* Tree is entirely empty, but might have a single leaf left as root */
        ndx_page_t *r = page_get(idx, idx->root_page);
        if (r && r->nkeys == 0) {
            /* Keep it as empty leaf root or just let it be. 
               Standard dBase behavior is often to keep an empty root page. */
        }
        if (r) page_put(r);
    }

    return 0;
}

/* ---- Clear (for ZAP) ---- */
void index_clear(index_t *idx) {
    free_all_pages(idx);

    /* Reinitialize with header placeholder */
    {
        ndx_page_t *hdr = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
        hdr->page_no = 0;
        hdr->pin_count = 1;
        hash_add(idx, hdr);
    }
    idx->num_pages = 1;
    idx->free_page_head = 0;
    idx->nentries = 0;
    idx->iter_page = -1;
    idx->iter_pos = -1;
    /* an empty index is an empty root leaf: the file has no other way */
    {
        ndx_page_t *leaf = page_new(idx, PAGE_LEAF);
        idx->root_page = leaf ? leaf->page_no : 0;
        idx->first_leaf = idx->root_page;
        page_put(leaf);
    }
}

typedef struct {
    char key[MAX_INDEX_KEY];
    uint32_t recno;
} key_entry_t;

static int key_entry_cmp(const void *a, const void *b, void *userdata) {
    index_t *idx = (index_t *)userdata;
    const key_entry_t *ka = (const key_entry_t *)a;
    const key_entry_t *kb = (const key_entry_t *)b;
    int cmp = key_cmp(ka->key, kb->key, idx->key_len);
    if (cmp) return cmp;
    /* equal keys in record order, as dBASE's and Clipper's indexes keep
     * them (qsort is not stable: they came out scrambled, and Teacher's
     * Pet's grade report, walking a student's grades by an index on the
     * student alone, lost the date order its copy had) */
    return ka->recno < kb->recno ? -1 : ka->recno > kb->recno;
}

/* The last key under a page of a tree being built (its pages all cached) */
static const char *subtree_last_key(index_t *idx, ndx_page_t *p) {
    while (p && p->type == PAGE_INTERNAL)
        p = hash_lookup(idx, p->children[p->nkeys]);
    return p && p->nkeys > 0 ? page_key(idx, p, p->nkeys - 1) : NULL;
}

/* A tree from n sorted entries, bottom up, as full as its pages go: the
 * header page, the leaves, then each level of internal pages, whose
 * keys are the last key under each child but the rightmost.  No entries
 * is one empty root leaf.  The pages stay in the cache for index_write. */
static int bulk_load(index_t *idx, const key_entry_t *entries, int n) {
    ndx_page_t **level = NULL;
    int nlevel, i;

    free_all_pages(idx);
    idx->nentries = 0;
    {
        ndx_page_t *hdr = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
        if (!hdr) return -1;
        hdr->page_no = 0;
        hdr->pin_count = 1;
        hash_add(idx, hdr);
        idx->num_pages = 1;
    }

    nlevel = n > 0 ? (n + idx->max_keys_leaf - 1) / idx->max_keys_leaf : 1;
    level = (ndx_page_t **)malloc((size_t)nlevel * sizeof(ndx_page_t *));
    if (!level) return -1;
    for (i = 0; i < nlevel; i++) {
        ndx_page_t *leaf = page_new(idx, PAGE_LEAF);
        int j, count = n - i * idx->max_keys_leaf;
        if (!leaf) { free(level); return -1; }
        if (count > idx->max_keys_leaf) count = idx->max_keys_leaf;
        if (count < 0) count = 0;
        for (j = 0; j < count; j++) {
            const key_entry_t *e = &entries[i * idx->max_keys_leaf + j];
            memcpy(page_key(idx, leaf, j), e->key, idx->key_len);
            leaf->recnos[j] = e->recno;
        }
        leaf->nkeys = count;
        idx->nentries += count;
        if (i > 0) {
            leaf->prev_leaf = level[i - 1]->page_no;
            level[i - 1]->next_leaf = leaf->page_no;
        }
        level[i] = leaf;
    }
    idx->first_leaf = level[0]->page_no;

    while (nlevel > 1) {
        /* children shared out evenly: a last page with one child alone
         * would have no key, and a reader could take it for a leaf */
        int per = idx->max_keys_internal + 1;   /* children a page takes */
        int nup = (nlevel + per - 1) / per, c = 0;
        ndx_page_t **up = (ndx_page_t **)malloc((size_t)nup * sizeof(ndx_page_t *));
        if (!up) goto fail;
        for (i = 0; i < nup; i++) {
            ndx_page_t *parent = page_new(idx, PAGE_INTERNAL);
            int j, kids = nlevel / nup + (i < nlevel % nup);
            if (!parent) { free(up); goto fail; }
            for (j = 0; j < kids; j++, c++) {
                parent->children[j] = level[c]->page_no;
                if (j < kids - 1) {
                    const char *last = subtree_last_key(idx, level[c]);
                    if (!last) { free(up); goto fail; }
                    memcpy(page_key(idx, parent, j), last, idx->key_len);
                }
            }
            parent->nkeys = kids - 1;
            up[i] = parent;
        }
        for (i = 0; i < nlevel; i++) page_put(level[i]);
        free(level);
        level = up;
        nlevel = nup;
    }
    idx->root_page = level[0]->page_no;
    page_put(level[0]);
    free(level);
    return 0;

fail:
    for (i = 0; i < nlevel; i++) page_put(level[i]);
    free(level);
    return -1;
}

/* ---- Determine key type/length from DBF field definitions (no records needed) ---- */
static void index_probe_key_from_schema(index_t *idx, dbf_t *db, const char *key_expr) {
    /* Simple field reference? */
    int fi = dbf_find_field(db, key_expr);
    if (fi >= 0) {
        switch (db->fields[fi].type) {
        case 'N': case 'F':
            idx->key_type = 1;
            idx->key_len = 16;
            break;
        case 'D':
            idx->key_type = 1;   /* a date's key is its Julian day */
            idx->key_len = 16;
            break;
        default:
            idx->key_type = 0;
            idx->key_len = db->fields[fi].length;
            break;
        }
        return;
    }

    /* Compound expression — sum field widths from '+' separated parts */
    {
        char buf[256];
        const char *p;
        int total_len = 0;
        int all_fields = 1;

        str_copy(buf, key_expr, sizeof(buf));
        p = buf;
        while (*p) {
            char part[64];
            const char *end = strchr(p, '+');
            int plen = end ? (int)(end - p) : (int)strlen(p);
            int i, start;

            /* Trim whitespace */
            if (plen >= (int)sizeof(part)) plen = (int)sizeof(part) - 1;
            memcpy(part, p, plen);
            part[plen] = '\0';
            start = 0;
            while (part[start] == ' ') start++;
            i = plen - 1;
            while (i > start && part[i] == ' ') part[i--] = '\0';

            fi = dbf_find_field(db, part + start);
            if (fi >= 0) {
                total_len += db->fields[fi].length;
            } else {
                all_fields = 0;
                break;
            }
            p = end ? end + 1 : p + plen;
        }

        if (all_fields && total_len > 0) {
            idx->key_type = 0;
            if (total_len > MAX_INDEX_KEY) total_len = MAX_INDEX_KEY;
            idx->key_len = total_len;
            return;
        }
    }

    /* Fallback: expression with function calls, etc. — use a reasonable default */
    idx->key_type = 0;
    idx->key_len = 10;
}

/* ---- Probe key type and length without storing entries (O(1) memory) ---- */
static void index_probe_key_info(index_t *idx, dbf_t *db, expr_ctx_t *ctx,
                                  const char *key_expr) {
    uint32_t i;
    int max_len = 0;
    int key_type_set = 0;

    idx->key_len = 10;
    idx->key_type = 0;

    for (i = 1; i <= db->record_count; i++) {
        value_t val;
        char keybuf[MAX_INDEX_KEY + 1];
        int len;

        dbf_read_record(db, i);
        if (db->record_buf[0] == '*') continue;

        if (idx->for_ast) {
            value_t fcond;
            if (ast_eval(idx->for_ast, ctx, &fcond) != 0) continue;
            if (fcond.type == VAL_LOGIC && !fcond.logic) continue;
            if (fcond.type == VAL_NUM && fcond.num == 0.0) continue;
        }

        if (idx->key_ast) {
            if (ast_eval(idx->key_ast, ctx, &val) != 0) continue;
        } else {
            if (ast_eval_dynamic(key_expr, ctx, &val) != 0) continue;
        }

        if (!key_type_set) {
            idx->key_type = (val.type == VAL_NUM || val.type == VAL_DATE) ? 1 : 0;
            key_type_set = 1;
        }

        /* Numeric and date have fixed key lengths — no need to scan further */
        if (idx->key_type == 1) break;

        index_format_key_value(idx->key_type, &val, keybuf, sizeof(keybuf));
        len = strlen(keybuf);
        if (len > max_len) max_len = len;
    }

    if (!key_type_set) {
        /* No records — fall back to schema-based probe */
        index_probe_key_from_schema(idx, db, key_expr);
    } else if (idx->key_type == 1) {
        idx->key_len = 16;
    } else {
        if (max_len < 1) max_len = 1;
        if (max_len > MAX_INDEX_KEY) max_len = MAX_INDEX_KEY;
        idx->key_len = max_len;
    }
}

/* ---- Incremental build: insert records one-by-one (O(1) extra memory) ---- */
static void index_build_incremental(index_t *idx, dbf_t *db, expr_ctx_t *ctx,
                                     const char *key_expr) {
    uint32_t i;
    for (i = 1; i <= db->record_count; i++) {
        value_t val;
        char keybuf[MAX_INDEX_KEY + 1];

        dbf_read_record(db, i);
        if (db->record_buf[0] == '*') continue;

        if (idx->for_ast) {
            value_t fcond;
            if (ast_eval(idx->for_ast, ctx, &fcond) != 0) continue;
            if (fcond.type == VAL_LOGIC && !fcond.logic) continue;
            if (fcond.type == VAL_NUM && fcond.num == 0.0) continue;
        }

        if (idx->key_ast) {
            if (ast_eval(idx->key_ast, ctx, &val) != 0) continue;
        } else {
            if (ast_eval_dynamic(key_expr, ctx, &val) != 0) continue;
        }

        index_format_key_value(idx->key_type, &val, keybuf, sizeof(keybuf));
        index_insert(idx, keybuf, i);
    }
}

/* ---- Build index from database ---- */
int index_build(index_t *idx, dbf_t *db, expr_ctx_t *ctx, const char *key_expr, const char *filename,
                const char *for_expr, int descending) {
    index_cache_setup(idx);
    uint32_t i;
    dbf_t *saved_db;
    int max_len = 0;
    int key_type_set = 0;
    key_entry_t *entries = NULL;
    int nentries = 0;
    int status = -1;   /* -2: the key or FOR expression failed, ctx->error says why */

    if (!dbf_is_open(db)) return -1;

    /* Close any previous state */
    free_all_pages(idx);
    if (idx->fp) { fclose(idx->fp); idx->fp = NULL; }

    char norm_path[64];
    str_copy(norm_path, filename, sizeof(norm_path));
    path_normalize(norm_path);

    str_copy(idx->key_expr, key_expr, sizeof(idx->key_expr));
    str_copy(idx->filename, norm_path, sizeof(idx->filename));

    /* Store FOR/DESCENDING */
    if (for_expr && for_expr[0]) {
        str_copy(idx->for_expr, for_expr, sizeof(idx->for_expr));
        const char *ast_err;
        if (idx->for_ast) { ast_free(idx->for_ast); idx->for_ast = NULL; }
        idx->for_ast = ast_compile(for_expr, ctx->vars, &ast_err);
    } else {
        idx->for_expr[0] = '\0';
        if (idx->for_ast) { ast_free(idx->for_ast); idx->for_ast = NULL; }
    }
    idx->descending = descending;
    /* the key expression and the FOR, each NUL-ended, go in the header */
    if (strlen(idx->key_expr) + 1 + (idx->for_expr[0] ? strlen(idx->for_expr) + 1 : 0) > NDX_EXPR_ROOM) {
        ctx->error = "Index expression and FOR condition too long for an index file";
        idx->for_expr[0] = '\0';
        return -2;
    }

    if (index_compile_key_ast(idx, ctx->vars) < 0) {
        /* Keep dynamic fallback if AST compile fails. */
        idx->key_ast = NULL;
    }

    saved_db = ctx->db;
    ctx->db = db;

    /* Try to allocate the bulk entries array */
    if (db->record_count > 0) {
        entries = (key_entry_t *)malloc(db->record_count * sizeof(key_entry_t));
    }

    if (!entries && db->record_count > 0) {
        /* ---- INCREMENTAL PATH: bounded memory ---- */
        index_probe_key_info(idx, db, ctx, key_expr);
        compute_fanout(idx);

        if (bulk_load(idx, NULL, 0) < 0) {   /* an empty root leaf */
            free_all_pages(idx);
            ctx->db = saved_db;
            return -1;
        }

        /* Open file before inserts so cache_evict can flush dirty pages */
        idx->fp = fopen(norm_path, "w+b");
        if (!idx->fp) {
            free_all_pages(idx);
            ctx->db = saved_db;
            return -1;
        }
        idx->active = 1;

        index_build_incremental(idx, db, ctx, key_expr);

        ctx->db = saved_db;
        if (idx->nentries > 0) {
            index_top(idx);
        } else {
            idx->iter_page = -1;
            idx->iter_pos = -1;
        }
        return 0;
    }

    /* ---- BULK PATH: fast sort+pack ---- */
    idx->key_len = 10;
    idx->key_type = 0;

    for (i = 1; i <= db->record_count; i++) {
        value_t val;
        char keybuf[MAX_INDEX_KEY + 1];

        dbf_read_record(db, i);
        if (db->record_buf[0] == '*') continue;

        /* Apply FOR filter */
        if (idx->for_ast) {
            value_t fcond;
            if (ast_eval(idx->for_ast, ctx, &fcond) != 0) { status = -2; goto fail; }
            if (fcond.type == VAL_LOGIC && !fcond.logic) continue;
            if (fcond.type == VAL_NUM && fcond.num == 0.0) continue;
        }

        if (idx->key_ast) {
            if (ast_eval(idx->key_ast, ctx, &val) != 0) { status = -2; goto fail; }
        } else {
            if (ast_eval_dynamic(key_expr, ctx, &val) != 0) { status = -2; goto fail; }
        }

        if (!key_type_set) {
            idx->key_type = (val.type == VAL_NUM || val.type == VAL_DATE) ? 1 : 0;
            key_type_set = 1;
        }

        index_format_key_value(idx->key_type, &val, keybuf, sizeof(keybuf));
        {
            int len = strlen(keybuf);
            if (idx->key_type == 0 && len > max_len) max_len = len;

            /* Store entry for sorting */
            memset(entries[nentries].key, ' ', MAX_INDEX_KEY);
            if (len > MAX_INDEX_KEY) len = MAX_INDEX_KEY;
            memcpy(entries[nentries].key, keybuf, len);
            entries[nentries].recno = i;
            nentries++;
        }
    }

    if (!key_type_set) {
        /* No records — fall back to schema-based probe */
        index_probe_key_from_schema(idx, db, key_expr);
    } else if (idx->key_type == 1) {
        idx->key_len = 16; /* sortable transformed IEEE-754 hex */
    } else {
        if (max_len < 1) max_len = 1;
        if (max_len > MAX_INDEX_KEY) max_len = MAX_INDEX_KEY;
        idx->key_len = max_len;
    }

    compute_fanout(idx);

    /* Sort entries */
    if (nentries > 0) {
        qsort_r(entries, nentries, sizeof(key_entry_t), key_entry_cmp, idx);
    }

    if (idx->unique && nentries > 1) {
        int out = 0;
        int j;
        for (i = 0; i < (uint32_t)nentries;) {
            uint32_t recno = entries[i].recno;
            j = (int)i + 1;
            while (j < nentries &&
                   key_cmp(entries[i].key, entries[j].key, idx->key_len) == 0) {
                if (entries[j].recno < recno) recno = entries[j].recno;
                j++;
            }
            if (out != (int)i) memcpy(&entries[out], &entries[i], sizeof(entries[0]));
            entries[out].recno = recno;
            out++;
            i = (uint32_t)j;
        }
        nentries = out;
    }

    if (bulk_load(idx, entries, nentries) < 0) goto fail;

    free(entries);
    ctx->db = saved_db;

    /* Position at first entry */
    if (idx->nentries > 0) {
        index_top(idx);
    } else {
        idx->iter_page = -1;
        idx->iter_pos = -1;
    }

    idx->active = 1;
    return 0;

fail:
    index_clear_key_ast(idx);
    if (entries) free(entries);
    free_all_pages(idx);
    ctx->db = saved_db;
    return status;
}

/* ---- Write index to file ---- */
int index_write(index_t *idx) {
    int i;

    if (idx->fp) {
        fclose(idx->fp);
        idx->fp = NULL;
    }

    idx->fp = fopen(idx->filename, "wb");
    if (!idx->fp) return -1;

    /* Write all pages */
    for (i = 0; i < idx->num_pages; i++) {
        if (page_flush(idx, i) < 0) {
            fclose(idx->fp);
            idx->fp = NULL;
            return -1;
        }
    }

    /* Keep file open for incremental writes */
    return 0;
}

/* ---- Public: Flush dirty pages to disk ---- */
void index_flush(index_t *idx) {
    ndx_page_t *p;
    if (!idx->fp) {
        idx->fp = fopen(idx->filename, "r+b");
        if (!idx->fp) return;
    }
    /* Always write header */
    page_flush(idx, 0);
    for (p = idx->lru_head; p; p = p->lru_next) {
        if (p->dirty)
            page_flush(idx, p->page_no);
    }
    fflush(idx->fp);
}

/* ---- Open: the leaf chain and entry count the file does not keep ---- */
/* Every internal page is read, level by level from the root, its
 * children in order; the leaves are the last level (a B-tree's leaves
 * are all at one depth), and of each only its count is read.  A page
 * named twice, or out of the file, is not an index of ours to open. */
static int index_walk(index_t *idx) {
    int *level, *next, nlevel = 1, depth = 0, i;
    unsigned char cnt[4];
    ndx_page_t *p;

    /* the depth, down the leftmost pages */
    for (i = idx->root_page;;) {
        p = page_get(idx, i);
        if (!p) return -1;
        if (p->type == PAGE_LEAF) { page_put(p); break; }
        i = p->children[0];
        page_put(p);
        if (++depth > 32) return -1;
    }

    level = (int *)malloc(sizeof(int));
    if (!level) return -1;
    level[0] = idx->root_page;
    while (depth-- > 0) {
        int n = 0, cap = nlevel * 8 + 8;
        next = (int *)malloc((size_t)cap * sizeof(int));
        if (!next) { free(level); return -1; }
        for (i = 0; i < nlevel; i++) {
            int j;
            p = page_get(idx, level[i]);
            if (!p || p->type != PAGE_INTERNAL) { if (p) page_put(p); free(next); free(level); return -1; }
            for (j = 0; j <= p->nkeys; j++) {
                if (n == cap) {
                    int *g = (int *)realloc(next, (size_t)(cap *= 2) * sizeof(int));
                    if (!g) { page_put(p); free(next); free(level); return -1; }
                    next = g;
                }
                next[n++] = p->children[j];
            }
            page_put(p);
        }
        free(level);
        level = next;
        nlevel = n;
        if (nlevel > idx->num_pages) { free(level); return -1; }
    }

    if (leaf_link_room(idx, idx->num_pages) < 0) { free(level); return -1; }
    idx->nentries = 0;
    for (i = 0; i < nlevel; i++) {
        int pg = level[i];
        if (idx->leaf_link[2 * pg] || idx->leaf_link[2 * pg + 1] || (i > 0 && pg == level[0])) {
            free(level); return -1;   /* named twice */
        }
        leaf_link_set(idx, pg, i + 1 < nlevel ? level[i + 1] : 0, i > 0 ? level[i - 1] : 0);
        fseek(idx->fp, (long)pg * NDX_PAGE, SEEK_SET);
        if (fread(cnt, 4, 1, idx->fp) != 1) { free(level); return -1; }
        idx->nentries += get16(cnt);   /* as page_read reads it */
    }
    idx->first_leaf = level[0];
    free(level);
    /* the leaf the depth was found through was read before its links */
    for (i = 0; i < PAGE_HASH_SIZE; i++)
        for (p = idx->page_hash[i]; p; p = p->hash_next)
            if (p->type == PAGE_LEAF && p->page_no < idx->leaf_link_cap) {
                p->next_leaf = idx->leaf_link[2 * p->page_no];
                p->prev_leaf = idx->leaf_link[2 * p->page_no + 1];
            }
    return 0;
}

/* dBASE III's header, checked as far as it can be */
static int read_dbase_header(index_t *idx, const unsigned char *buf, long file_size) {
    int klen, isz, kpp, type, explen;
    idx->root_page = (int)get32(buf + 0);
    idx->num_pages = (int)get32(buf + 4);
    klen = get16(buf + 12);
    kpp  = get16(buf + 14);
    type = get16(buf + 16);
    isz  = get16(buf + 18);
    if (idx->num_pages < 2 || (long)idx->num_pages * NDX_PAGE > file_size + NDX_PAGE - 1 ||
        idx->root_page < 1 || idx->root_page >= idx->num_pages ||
        type > 1 || klen < 1 || klen > MAX_INDEX_KEY || (type == 1 && klen != 8) ||
        isz < 8 + klen || kpp < 2 || 4 + kpp * isz > NDX_PAGE)
        return -1;
    idx->key_type = type;
    idx->disk_key_len = klen;
    idx->key_len = type == 1 ? 16 : klen;
    idx->item_size = isz;
    idx->keys_per_page = kpp;
    idx->max_keys_leaf = kpp;
    idx->max_keys_internal = kpp - 1;
    idx->alloc_keys = (NDX_PAGE - 4) / isz + 1;
    idx->unique = buf[23] ? 1 : 0;
    idx->descending = (buf[22] & NDX_FLAG_DESC) ? 1 : 0;
    explen = (int)strnlen((const char *)buf + 24, NDX_EXPR_ROOM);
    if (explen == 0 || explen >= (int)sizeof(idx->key_expr)) return -1;
    memcpy(idx->key_expr, buf + 24, (size_t)explen);
    idx->key_expr[explen] = '\0';
    trim_right(idx->key_expr);   /* dBASE pads it with a space */
    idx->for_expr[0] = '\0';
    if (buf[22] & NDX_FLAG_FOR) {
        int room = NDX_EXPR_ROOM - explen - 1;
        int flen = room > 0 ? (int)strnlen((const char *)buf + 24 + explen + 1, (size_t)room) : 0;
        if (flen >= (int)sizeof(idx->for_expr)) return -1;
        memcpy(idx->for_expr, buf + 24 + explen + 1, (size_t)flen);
        idx->for_expr[flen] = '\0';
    }
    idx->free_page_head = 0;
    return 0;
}

/* ---- This interpreter's formats before dBASE's: entries, in order ---- */
typedef struct { key_entry_t *e; int n, cap; } entry_list_t;

static int entry_add(entry_list_t *l, const char *key, int key_len, uint32_t recno) {
    if (l->n == l->cap) {
        int cap = l->cap ? l->cap * 2 : 256;
        key_entry_t *g = (key_entry_t *)realloc(l->e, (size_t)cap * sizeof(key_entry_t));
        if (!g) return -1;
        l->e = g;
        l->cap = cap;
    }
    memset(l->e[l->n].key, ' ', MAX_INDEX_KEY);
    memcpy(l->e[l->n].key, key, (size_t)key_len);
    l->e[l->n].recno = recno;
    l->n++;
    return 0;
}

/* NDX2: 4 KB pages, its leaves chained from the header's first leaf;
 * a date's key was its YYYYMMDD, now its Julian day as a number's */
static int read_ndx2_entries(index_t *idx, FILE *fp, entry_list_t *l) {
    unsigned char buf[NDX2_PAGE_SIZE];
    int pg, guard = 0, old_type, old_len, npages;
    if (fseek(fp, 0, SEEK_SET) != 0 || fread(buf, NDX2_PAGE_SIZE, 1, fp) != 1) return -1;
    pg = (int)get32(buf + 12);
    npages = (int)get32(buf + 16);
    old_len = (int)get32(buf + 24);
    old_type = (int)get32(buf + 28);
    idx->unique = (int)get32(buf + 36) ? 1 : 0;
    memcpy(idx->key_expr, buf + 40, 255); idx->key_expr[255] = '\0';
    memcpy(idx->for_expr, buf + 296, 255); idx->for_expr[255] = '\0';
    idx->descending = (int)get32(buf + 552) ? 1 : 0;
    if (old_len < 1 || old_len > MAX_INDEX_KEY || old_type < 0 || old_type > 2) return -1;
    idx->key_type = old_type ? 1 : 0;
    idx->key_len = old_type ? 16 : old_len;
    while (pg > 0) {
        int n, i;
        if (pg >= npages || ++guard > npages) return -1;
        if (fseek(fp, (long)pg * NDX2_PAGE_SIZE, SEEK_SET) != 0 || fread(buf, NDX2_PAGE_SIZE, 1, fp) != 1) return -1;
        if (get16(buf) != PAGE_LEAF) return -1;
        n = get16(buf + 2);
        if (12 + n * (old_len + 4) > NDX2_PAGE_SIZE) return -1;
        for (i = 0; i < n; i++) {
            const unsigned char *it = buf + 12 + i * (old_len + 4);
            char key[MAX_INDEX_KEY + 1];
            if (old_type == 2) {
                char ymd[9];
                memcpy(ymd, it, 8); ymd[8] = '\0';
                encode_sortable_double((double)date_from_dbf(ymd), key);
            } else {
                memcpy(key, it, (size_t)old_len);
            }
            if (entry_add(l, key, idx->key_len, get32(it + old_len)) < 0) return -1;
        }
        pg = (int)get32(buf + 4);
    }
    return 0;
}

/* NDX1: a flat array of character keys */
static int read_ndx1_entries(index_t *idx, FILE *fp, entry_list_t *l) {
    uint32_t n, klen, i;
    char expr[256];
    if (fseek(fp, 4, SEEK_SET) != 0 || fread(&n, 4, 1, fp) != 1 || fread(&klen, 4, 1, fp) != 1 ||
        fread(expr, 256, 1, fp) != 1 || klen < 1 || klen > MAX_INDEX_KEY)
        return -1;
    expr[255] = '\0';
    str_copy(idx->key_expr, expr, sizeof(idx->key_expr));
    idx->key_type = 0;
    idx->key_len = (int)klen;
    idx->unique = 0;
    for (i = 0; i < n; i++) {
        char key[MAX_INDEX_KEY];
        uint32_t rec;
        if (fread(key, klen, 1, fp) != 1 || fread(&rec, 4, 1, fp) != 1) break;
        if (entry_add(l, key, (int)klen, rec) < 0) return -1;
    }
    return 0;
}

/* ---- Read index from file ---- */
int index_read(index_t *idx, const char *filename) {
    FILE *fp;
    unsigned char buf[NDX_PAGE];
    char norm_path[64];
    long size;
    uint32_t magic;

    index_cache_setup(idx);
    free_all_pages(idx);

    str_copy(norm_path, filename, sizeof(norm_path));
    path_normalize(norm_path);

    fp = fopen(norm_path, "r+b");
    if (!fp) fp = fopen(norm_path, "rb");   /* read-only media: opened, not kept up */
    if (!fp) return -1;
    if (fseek(fp, 0, SEEK_END) != 0 || (size = ftell(fp)) < NDX_PAGE ||
        fseek(fp, 0, SEEK_SET) != 0 || fread(buf, NDX_PAGE, 1, fp) != 1) {
        fclose(fp);
        return -1;
    }
    str_copy(idx->filename, norm_path, sizeof(idx->filename));
    magic = get32(buf);

    if (magic == NDX2_MAGIC || magic == NDX1_MAGIC) {
        /* converted: rebuilt as dBASE's from its entries, in its place */
        entry_list_t l = { NULL, 0, 0 };
        int rc = magic == NDX2_MAGIC ? read_ndx2_entries(idx, fp, &l) : read_ndx1_entries(idx, fp, &l);
        fclose(fp);
        if (rc == 0 && strlen(idx->key_expr) + 1 +
                       (idx->for_expr[0] ? strlen(idx->for_expr) + 1 : 0) > NDX_EXPR_ROOM)
            rc = -1;
        if (rc == 0) {
            compute_fanout(idx);
            rc = bulk_load(idx, l.e, l.n);
        }
        free(l.e);
        if (rc == 0) rc = index_write(idx);
        if (rc < 0) { free_all_pages(idx); return -1; }
    } else {
        idx->fp = fp;
        if (read_dbase_header(idx, buf, size) < 0 || index_walk(idx) < 0) {
            free_all_pages(idx);
            fclose(fp);
            idx->fp = NULL;
            return -1;
        }
        {   /* page 0 = header (dummy) */
            ndx_page_t *hdr = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
            if (hdr) { hdr->page_no = 0; hdr->pin_count = 1; hash_add(idx, hdr); }
        }
    }

    idx->active = 1;
    if (idx->nentries > 0) {
        index_top(idx);
    } else {
        idx->iter_page = -1;
        idx->iter_pos = -1;
    }
    return 0;
}
