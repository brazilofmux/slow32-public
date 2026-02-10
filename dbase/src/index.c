#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "index.h"
#include "util.h"

/* ---- Key comparison ---- */
static int key_cmp(const char *a, const char *b, int key_len) {
    return memcmp(a, b, key_len);
}

static char *page_key(index_t *idx, ndx_page_t *page, int i) {
    return page->keys + i * idx->key_len;
}

/* ---- Page allocation ---- */
static ndx_page_t *page_new(index_t *idx, int type) {
    ndx_page_t *p;
    int max_keys;
    int pg_no;

    /* Grow pages array if needed */
    if (idx->num_pages >= idx->pages_capacity) {
        int newcap = idx->pages_capacity ? idx->pages_capacity * 2 : 16;
        ndx_page_t **newarr = (ndx_page_t **)realloc(idx->pages, newcap * sizeof(ndx_page_t *));
        if (!newarr) return NULL;
        idx->pages = newarr;
        idx->pages_capacity = newcap;
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

    if (type == PAGE_LEAF) {
        max_keys = idx->max_keys_leaf;
        p->keys = (char *)calloc(max_keys, idx->key_len);
        p->recnos = (uint32_t *)calloc(max_keys, sizeof(uint32_t));
        p->children = NULL;
    } else {
        max_keys = idx->max_keys_internal;
        p->keys = (char *)calloc(max_keys, idx->key_len);
        p->children = (int *)calloc(max_keys + 1, sizeof(int));
        p->recnos = NULL;
    }

    idx->pages[pg_no] = p;
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
static int page_flush(index_t *idx, int page_no) {
    unsigned char buf[NDX_PAGE_SIZE];
    ndx_page_t *p;
    int i, off;

    if (!idx->fp) return -1;
    p = idx->pages[page_no];
    if (!p) return -1;

    memset(buf, 0, NDX_PAGE_SIZE);

    if (page_no == 0) {
        /* Header page */
        uint32_t tmp;
        tmp = NDX2_MAGIC;        memcpy(buf + 0, &tmp, 4);
        tmp = 1;                 memcpy(buf + 4, &tmp, 4);  /* version */
        tmp = idx->root_page;    memcpy(buf + 8, &tmp, 4);
        tmp = idx->first_leaf;   memcpy(buf + 12, &tmp, 4);
        tmp = idx->num_pages;    memcpy(buf + 16, &tmp, 4);
        tmp = idx->nentries;     memcpy(buf + 20, &tmp, 4);
        tmp = idx->key_len;      memcpy(buf + 24, &tmp, 4);
        tmp = idx->key_type;     memcpy(buf + 28, &tmp, 4);
        memcpy(buf + 32, idx->key_expr, 256);
    } else if (p->type == PAGE_INTERNAL) {
        uint16_t t16;
        t16 = PAGE_INTERNAL;    memcpy(buf + 0, &t16, 2);
        t16 = (uint16_t)p->nkeys; memcpy(buf + 2, &t16, 2);
        off = 4;
        /* Child pointers: n+1 */
        for (i = 0; i <= p->nkeys; i++) {
            uint32_t child = (uint32_t)p->children[i];
            memcpy(buf + off, &child, 4);
            off += 4;
        }
        /* Keys: n */
        for (i = 0; i < p->nkeys; i++) {
            memcpy(buf + off, page_key(idx, p, i), idx->key_len);
            off += idx->key_len;
        }
    } else {
        /* Leaf */
        uint16_t t16;
        uint32_t t32;
        t16 = PAGE_LEAF;       memcpy(buf + 0, &t16, 2);
        t16 = (uint16_t)p->nkeys; memcpy(buf + 2, &t16, 2);
        t32 = (uint32_t)p->next_leaf; memcpy(buf + 4, &t32, 4);
        t32 = (uint32_t)p->prev_leaf; memcpy(buf + 8, &t32, 4);
        off = 12;
        for (i = 0; i < p->nkeys; i++) {
            memcpy(buf + off, page_key(idx, p, i), idx->key_len);
            off += idx->key_len;
            memcpy(buf + off, &p->recnos[i], 4);
            off += 4;
        }
    }

    fseek(idx->fp, (long)page_no * NDX_PAGE_SIZE, SEEK_SET);
    if (fwrite(buf, NDX_PAGE_SIZE, 1, idx->fp) != 1)
        return -1;
    fflush(idx->fp);
    p->dirty = 0;
    return 0;
}

static int page_read(index_t *idx, int page_no, unsigned char *buf) {
    ndx_page_t *p;
    uint16_t t16;
    int i, off;

    if (page_no == 0) return 0; /* header page handled separately */

    memcpy(&t16, buf + 0, 2);

    if (t16 == PAGE_INTERNAL) {
        int max_keys = idx->max_keys_internal;
        p = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
        p->page_no = page_no;
        p->type = PAGE_INTERNAL;
        memcpy(&t16, buf + 2, 2);
        p->nkeys = t16;
        p->keys = (char *)calloc(max_keys, idx->key_len);
        p->children = (int *)calloc(max_keys + 1, sizeof(int));
        p->recnos = NULL;
        p->dirty = 0;

        off = 4;
        for (i = 0; i <= p->nkeys; i++) {
            uint32_t child;
            memcpy(&child, buf + off, 4);
            p->children[i] = (int)child;
            off += 4;
        }
        for (i = 0; i < p->nkeys; i++) {
            memcpy(page_key(idx, p, i), buf + off, idx->key_len);
            off += idx->key_len;
        }
    } else {
        /* Leaf */
        int max_keys = idx->max_keys_leaf;
        uint32_t t32;
        p = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
        p->page_no = page_no;
        p->type = PAGE_LEAF;
        memcpy(&t16, buf + 2, 2);
        p->nkeys = t16;
        memcpy(&t32, buf + 4, 4); p->next_leaf = (int)t32;
        memcpy(&t32, buf + 8, 4); p->prev_leaf = (int)t32;
        p->keys = (char *)calloc(max_keys, idx->key_len);
        p->recnos = (uint32_t *)calloc(max_keys, sizeof(uint32_t));
        p->children = NULL;
        p->dirty = 0;

        off = 12;
        for (i = 0; i < p->nkeys; i++) {
            memcpy(page_key(idx, p, i), buf + off, idx->key_len);
            off += idx->key_len;
            memcpy(&p->recnos[i], buf + off, 4);
            off += 4;
        }
    }

    idx->pages[page_no] = p;
    return 0;
}

/* ---- Free all tree pages ---- */
static void free_all_pages(index_t *idx) {
    int i;
    if (idx->pages) {
        for (i = 0; i < idx->num_pages; i++) {
            if (idx->pages[i]) {
                page_free(idx->pages[i]);
                idx->pages[i] = NULL;
            }
        }
        free(idx->pages);
        idx->pages = NULL;
    }
    idx->num_pages = 0;
    idx->pages_capacity = 0;
}

/* ---- Init ---- */
void index_init(index_t *idx) {
    memset(idx, 0, sizeof(index_t));
    idx->iter_page = -1;
    idx->iter_pos = -1;
}

/* ---- Close ---- */
void index_close(index_t *idx) {
    free_all_pages(idx);
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
}

/* ---- Compute max keys from key_len ---- */
static void compute_fanout(index_t *idx) {
    /* Internal: header(4) + (n+1)*4 child ptrs + n*key_len <= 4096
       4 + 4*(n+1) + n*key_len <= 4096
       n*(4+key_len) <= 4096 - 8
       n <= (4088) / (4 + key_len) */
    idx->max_keys_internal = (NDX_PAGE_SIZE - 8) / (4 + idx->key_len);
    /* Leaf: header(12) + n*(key_len+4) <= 4096
       n <= (4084) / (key_len + 4) */
    idx->max_keys_leaf = (NDX_PAGE_SIZE - 12) / (idx->key_len + 4);

    if (idx->max_keys_internal < 3) idx->max_keys_internal = 3;
    if (idx->max_keys_leaf < 3) idx->max_keys_leaf = 3;
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

/* ---- Search: find leaf and position for key ---- */
static ndx_page_t *bt_find_leaf(index_t *idx, const char *key) {
    ndx_page_t *node;
    if (idx->num_pages <= 1) return NULL;
    node = idx->pages[idx->root_page];
    while (node && node->type == PAGE_INTERNAL) {
        int pos = page_lower_bound(idx, node, key);
        /* descend to children[pos] */
        node = idx->pages[node->children[pos]];
    }
    return node;
}

/* ---- Seek ---- */
int index_seek(index_t *idx, const char *key) {
    ndx_page_t *leaf;
    char padded[MAX_INDEX_KEY];
    int pos;

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
        if (key_cmp(page_key(idx, leaf, pos), padded, idx->key_len) == 0)
            return 1; /* exact match */
        return 0;
    }

    /* Past end of this leaf -- try next leaf */
    if (leaf->next_leaf) {
        idx->iter_page = leaf->next_leaf;
        idx->iter_pos = 0;
    } else {
        /* Past end of entire index */
        idx->iter_page = leaf->page_no;
        idx->iter_pos = leaf->nkeys;
    }
    return 0;
}

/* ---- Current record number ---- */
uint32_t index_current_recno(const index_t *idx) {
    ndx_page_t *page;
    if (idx->iter_page < 0 || idx->iter_page >= idx->num_pages)
        return 0;
    page = idx->pages[idx->iter_page];
    if (!page || page->type != PAGE_LEAF)
        return 0;
    if (idx->iter_pos < 0 || idx->iter_pos >= page->nkeys)
        return 0;
    return page->recnos[idx->iter_pos];
}

/* ---- Navigation ---- */
void index_top(index_t *idx) {
    if (!idx->active || idx->nentries == 0 || idx->first_leaf == 0) {
        idx->iter_page = -1;
        idx->iter_pos = -1;
        return;
    }
    idx->iter_page = idx->first_leaf;
    idx->iter_pos = 0;
}

void index_bottom(index_t *idx) {
    int pg;
    if (!idx->active || idx->nentries == 0 || idx->first_leaf == 0) {
        idx->iter_page = -1;
        idx->iter_pos = -1;
        return;
    }
    /* Follow leaf chain to last leaf */
    pg = idx->first_leaf;
    while (idx->pages[pg]->next_leaf)
        pg = idx->pages[pg]->next_leaf;
    idx->iter_page = pg;
    idx->iter_pos = idx->pages[pg]->nkeys - 1;
}

int index_next(index_t *idx) {
    ndx_page_t *page;
    if (idx->iter_page < 0 || idx->iter_page >= idx->num_pages)
        return -1;
    page = idx->pages[idx->iter_page];
    if (!page || page->type != PAGE_LEAF)
        return -1;

    idx->iter_pos++;
    if (idx->iter_pos >= page->nkeys) {
        if (!page->next_leaf) return -1; /* EOF */
        idx->iter_page = page->next_leaf;
        idx->iter_pos = 0;
    }
    return 0;
}

int index_prev(index_t *idx) {
    ndx_page_t *page;
    if (idx->iter_page < 0 || idx->iter_page >= idx->num_pages)
        return -1;
    page = idx->pages[idx->iter_page];
    if (!page || page->type != PAGE_LEAF)
        return -1;

    idx->iter_pos--;
    if (idx->iter_pos < 0) {
        if (!page->prev_leaf) return -1; /* BOF */
        idx->iter_page = page->prev_leaf;
        idx->iter_pos = idx->pages[idx->iter_page]->nkeys - 1;
    }
    return 0;
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
   Returns the new page. Sets *split_key to the first key of the new page. */
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
        idx->pages[right->next_leaf]->prev_leaf = right->page_no;
        idx->pages[right->next_leaf]->dirty = 1;
    }

    leaf->dirty = 1;
    right->dirty = 1;

    /* Split key = first key of right page */
    memcpy(split_key, page_key(idx, right, 0), idx->key_len);

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
        return 0;
    }

    /* Walk down tree to find leaf */
    node = idx->pages[idx->root_page];
    while (node->type == PAGE_INTERNAL) {
        pos = page_lower_bound(idx, node, padded);
        path[depth] = node->page_no;
        path_pos[depth] = pos;
        depth++;
        node = idx->pages[node->children[pos]];
    }

    /* node is now a leaf. Insert into it. */
    pos = page_lower_bound(idx, node, padded);
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
            break;
        }

        /* Insert split_key into parent */
        depth--;
        node = idx->pages[path[depth]];
        internal_insert_at(idx, node, path_pos[depth], split_key, right->page_no);
        /* Loop continues if parent also overflows */
    }

    /* Update first_leaf if needed */
    {
        ndx_page_t *fl = idx->pages[idx->first_leaf];
        if (fl->prev_leaf) {
            /* first_leaf is wrong; walk back */
            while (fl->prev_leaf)
                fl = idx->pages[fl->prev_leaf];
            idx->first_leaf = fl->page_no;
        }
    }

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

/* Find the position of key+recno in an internal node's key array for update purposes */
/* Update the separator key in an internal node if needed */
static void update_separator(index_t *idx, int *path, int *path_pos, int depth, ndx_page_t *leaf) {
    /* After removal/merge, update parent separators if the first key changed.
       For simplicity, we only update if the leaf chain is intact. */
    (void)idx; (void)path; (void)path_pos; (void)depth; (void)leaf;
    /* B+ tree separators are just routing hints -- they don't need to exactly
       match leaf keys. A stale separator still routes correctly as long as
       the tree structure is valid. We only need to handle the case where
       a merge collapses a level. */
}

int index_remove(index_t *idx, const char *key, uint32_t recno) {
    char padded[MAX_INDEX_KEY];
    ndx_page_t *leaf;
    int i, found;

    if (!idx->active || idx->nentries == 0) return -1;

    /* Pad key */
    memset(padded, ' ', idx->key_len);
    {
        int len = strlen(key);
        if (len > idx->key_len) len = idx->key_len;
        memcpy(padded, key, len);
    }

    /* Find the leaf */
    leaf = bt_find_leaf(idx, padded);
    if (!leaf) return -1;

    /* Find exact key+recno */
    found = -1;
    for (i = 0; i < leaf->nkeys; i++) {
        if (key_cmp(page_key(idx, leaf, i), padded, idx->key_len) == 0 &&
            leaf->recnos[i] == recno) {
            found = i;
            break;
        }
    }
    if (found < 0) {
        /* Key might be in next leaf if it was at boundary */
        ndx_page_t *next = leaf->next_leaf ? idx->pages[leaf->next_leaf] : NULL;
        if (next) {
            for (i = 0; i < next->nkeys; i++) {
                if (key_cmp(page_key(idx, next, i), padded, idx->key_len) == 0 &&
                    next->recnos[i] == recno) {
                    leaf = next;
                    found = i;
                    break;
                }
            }
        }
        if (found < 0) return -1;
    }

    leaf_remove_at(idx, leaf, found);
    idx->nentries--;

    /* Handle empty leaf */
    if (leaf->nkeys == 0 && idx->nentries > 0) {
        /* Unlink from leaf chain */
        if (leaf->prev_leaf) {
            idx->pages[leaf->prev_leaf]->next_leaf = leaf->next_leaf;
            idx->pages[leaf->prev_leaf]->dirty = 1;
        }
        if (leaf->next_leaf) {
            idx->pages[leaf->next_leaf]->prev_leaf = leaf->prev_leaf;
            idx->pages[leaf->next_leaf]->dirty = 1;
        }
        if (idx->first_leaf == leaf->page_no) {
            idx->first_leaf = leaf->next_leaf;
        }
        /* Note: We don't actually remove the page from the array or update
           parent internal nodes. The empty page becomes dead space.
           For dBase workloads this is fine -- REINDEX cleans up. */
    }

    /* Handle completely empty tree */
    if (idx->nentries == 0) {
        free_all_pages(idx);
        /* Reinitialize with just header placeholder */
        idx->pages = NULL;
        idx->pages_capacity = 0;
        idx->num_pages = 0;
        /* Allocate header page placeholder */
        {
            ndx_page_t *hdr;
            idx->pages = (ndx_page_t **)calloc(1, sizeof(ndx_page_t *));
            idx->pages_capacity = 1;
            hdr = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
            hdr->page_no = 0;
            idx->pages[0] = hdr;
            idx->num_pages = 1;
        }
        idx->root_page = 0;
        idx->first_leaf = 0;
    }

    return 0;
}

/* ---- Clear (for ZAP) ---- */
void index_clear(index_t *idx) {
    free_all_pages(idx);

    /* Reinitialize with header placeholder */
    idx->pages = (ndx_page_t **)calloc(1, sizeof(ndx_page_t *));
    idx->pages_capacity = 1;
    {
        ndx_page_t *hdr = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
        hdr->page_no = 0;
        idx->pages[0] = hdr;
    }
    idx->num_pages = 1;
    idx->root_page = 0;
    idx->first_leaf = 0;
    idx->nentries = 0;
    idx->iter_page = -1;
    idx->iter_pos = -1;
}

/* ---- Build index from database ---- */
int index_build(index_t *idx, dbf_t *db, expr_ctx_t *ctx, const char *key_expr, const char *filename) {
    uint32_t i;
    dbf_t *saved_db;

    if (!dbf_is_open(db)) return -1;

    /* Close any previous state */
    free_all_pages(idx);
    if (idx->fp) { fclose(idx->fp); idx->fp = NULL; }

    str_copy(idx->key_expr, key_expr, sizeof(idx->key_expr));
    str_copy(idx->filename, filename, sizeof(idx->filename));

    /* Determine key length by evaluating expression for first record */
    idx->key_len = 10; /* default */
    idx->key_type = 0; /* char */

    saved_db = ctx->db;
    ctx->db = db;

    if (db->record_count > 0) {
        value_t val;
        char keybuf[MAX_INDEX_KEY + 1];
        dbf_read_record(db, 1);
        if (expr_eval_str(ctx, key_expr, &val) == 0) {
            val_to_string(&val, keybuf, sizeof(keybuf));
            /* Don't trim -- measure natural width */
            idx->key_len = strlen(keybuf);
            if (idx->key_len < 1) idx->key_len = 1;
            if (idx->key_len > MAX_INDEX_KEY) idx->key_len = MAX_INDEX_KEY;
            if (val.type == VAL_NUM) idx->key_type = 1;
            else if (val.type == VAL_DATE) idx->key_type = 2;
        }
    }

    compute_fanout(idx);

    /* Initialize with header placeholder (page 0) */
    idx->pages = NULL;
    idx->pages_capacity = 0;
    idx->num_pages = 0;
    idx->nentries = 0;
    idx->root_page = 0;
    idx->first_leaf = 0;
    {
        /* Page 0 = header (not a tree node) */
        ndx_page_t *hdr;
        idx->pages = (ndx_page_t **)calloc(1, sizeof(ndx_page_t *));
        idx->pages_capacity = 1;
        hdr = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
        hdr->page_no = 0;
        idx->pages[0] = hdr;
        idx->num_pages = 1;
    }

    idx->active = 1;

    /* Insert each non-deleted record */
    for (i = 1; i <= db->record_count; i++) {
        value_t val;
        char keybuf[MAX_INDEX_KEY + 1];

        dbf_read_record(db, i);
        if (db->record_buf[0] == '*') continue;

        if (expr_eval_str(ctx, key_expr, &val) != 0) {
            if (ctx->error) printf("Index key error at record %d: %s\n", (int)i, ctx->error);
            continue;
        }

        val_to_string(&val, keybuf, sizeof(keybuf));
        /* Pad to key_len with spaces */
        {
            int len = strlen(keybuf);
            if (len < idx->key_len)
                memset(keybuf + len, ' ', idx->key_len - len);
            keybuf[idx->key_len] = '\0';
        }

        index_insert(idx, keybuf, i);
    }

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
    int i;
    if (!idx->fp) {
        idx->fp = fopen(idx->filename, "r+b");
        if (!idx->fp) return;
    }
    /* Always write header */
    page_flush(idx, 0);
    for (i = 1; i < idx->num_pages; i++) {
        if (idx->pages[i] && idx->pages[i]->dirty)
            page_flush(idx, i);
    }
    fflush(idx->fp);
}

/* ---- Read NDX2 format ---- */
static int read_ndx2(index_t *idx, FILE *fp) {
    unsigned char buf[NDX_PAGE_SIZE];
    uint32_t tmp;
    int i;

    /* Parse header (already read first 4 bytes for magic) */
    fseek(fp, 0, SEEK_SET);
    if (fread(buf, NDX_PAGE_SIZE, 1, fp) != 1) return -1;

    memcpy(&tmp, buf + 4, 4);  /* version */
    memcpy(&tmp, buf + 8, 4);  idx->root_page = (int)tmp;
    memcpy(&tmp, buf + 12, 4); idx->first_leaf = (int)tmp;
    memcpy(&tmp, buf + 16, 4); idx->num_pages = (int)tmp;
    memcpy(&tmp, buf + 20, 4); idx->nentries = (int)tmp;
    memcpy(&tmp, buf + 24, 4); idx->key_len = (int)tmp;
    memcpy(&tmp, buf + 28, 4); idx->key_type = (int)tmp;
    memcpy(idx->key_expr, buf + 32, 256);
    idx->key_expr[255] = '\0';

    compute_fanout(idx);

    /* Allocate page cache */
    idx->pages_capacity = idx->num_pages;
    idx->pages = (ndx_page_t **)calloc(idx->pages_capacity, sizeof(ndx_page_t *));
    if (!idx->pages) return -1;

    /* Page 0 = header (dummy) */
    {
        ndx_page_t *hdr = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
        hdr->page_no = 0;
        idx->pages[0] = hdr;
    }

    /* Read remaining pages */
    for (i = 1; i < idx->num_pages; i++) {
        if (fread(buf, NDX_PAGE_SIZE, 1, fp) != 1) return -1;
        if (page_read(idx, i, buf) < 0) return -1;
    }

    return 0;
}

/* ---- Read NDX1 (old flat format) ---- */
static int read_ndx1(index_t *idx, FILE *fp) {
    uint32_t nentries, keylen;
    int i;
    char *keys;
    uint32_t *recnos;

    /* Header: magic(4) already read, nentries(4), keylen(4), expr(256) */
    fseek(fp, 4, SEEK_SET);
    if (fread(&nentries, 4, 1, fp) != 1) return -1;
    if (fread(&keylen, 4, 1, fp) != 1) return -1;

    {
        char expr_buf[256];
        if (fread(expr_buf, 256, 1, fp) != 1) return -1;
        str_copy(idx->key_expr, expr_buf, sizeof(idx->key_expr));
    }

    /* Read flat entries */
    keys = (char *)malloc(nentries * keylen);
    recnos = (uint32_t *)malloc(nentries * sizeof(uint32_t));
    if (!keys || !recnos) {
        if (keys) free(keys);
        if (recnos) free(recnos);
        return -1;
    }

    for (i = 0; i < (int)nentries; i++) {
        if (fread(keys + i * keylen, keylen, 1, fp) != 1) break;
        if (fread(&recnos[i], 4, 1, fp) != 1) break;
    }
    nentries = i;

    /* Determine actual key_len from old data */
    idx->key_len = (int)keylen;
    if (idx->key_len > MAX_INDEX_KEY) idx->key_len = MAX_INDEX_KEY;
    idx->key_type = 0;
    compute_fanout(idx);

    /* Initialize empty tree */
    idx->pages = (ndx_page_t **)calloc(1, sizeof(ndx_page_t *));
    idx->pages_capacity = 1;
    {
        ndx_page_t *hdr = (ndx_page_t *)calloc(1, sizeof(ndx_page_t));
        hdr->page_no = 0;
        idx->pages[0] = hdr;
    }
    idx->num_pages = 1;
    idx->root_page = 0;
    idx->first_leaf = 0;
    idx->nentries = 0;

    /* Bulk insert (data is already sorted) */
    for (i = 0; i < (int)nentries; i++) {
        char keybuf[MAX_INDEX_KEY + 1];
        memset(keybuf, ' ', idx->key_len);
        memcpy(keybuf, keys + i * keylen, idx->key_len);
        keybuf[idx->key_len] = '\0';
        index_insert(idx, keybuf, recnos[i]);
    }

    free(keys);
    free(recnos);
    return 0;
}

/* ---- Read index from file ---- */
int index_read(index_t *idx, const char *filename) {
    FILE *fp;
    uint32_t magic;

    fp = fopen(filename, "rb");
    if (!fp) return -1;

    if (fread(&magic, 4, 1, fp) != 1) {
        fclose(fp);
        return -1;
    }

    str_copy(idx->filename, filename, sizeof(idx->filename));

    if (magic == NDX2_MAGIC) {
        if (read_ndx2(idx, fp) < 0) {
            fclose(fp);
            return -1;
        }
    } else if (magic == NDX1_MAGIC) {
        if (read_ndx1(idx, fp) < 0) {
            fclose(fp);
            return -1;
        }
    } else {
        fclose(fp);
        return -1;
    }

    fclose(fp);

    /* Open for incremental writes */
    idx->fp = fopen(filename, "r+b");

    /* Position at first entry */
    if (idx->nentries > 0) {
        index_top(idx);
    } else {
        idx->iter_page = -1;
        idx->iter_pos = -1;
    }

    idx->active = 1;
    return 0;
}
