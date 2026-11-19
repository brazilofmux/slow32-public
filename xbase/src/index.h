#ifndef INDEX_H
#define INDEX_H

#include <stdio.h>
#include <stdint.h>
#include "dbf.h"
#include "expr.h"

/* An .NDX file is dBASE III's: 512-byte pages, page 0 the header (root
 * page, page count, key length, keys per page, key type 0 character or
 * 1 numeric/date, item size, unique flag, key expression).  A page is a
 * key count and items of (left page, record number, key); a leaf's left
 * pages are 0, an internal page's key is the last key under its left
 * page, and its last item holds only the rightmost page.  Numeric and
 * date keys are IEEE doubles, a date its Julian day.  This interpreter's
 * own formats before it, NDX2 and NDX1, are converted when opened. */
#define NDX_PAGE        512
#define NDX_EXPR_ROOM   488         /* header bytes 24..511 */
#define NDX_FLAG_DESC   0x01        /* header byte 22, ours (dBASE leaves it 0): DESCENDING */
#define NDX_FLAG_FOR    0x02        /* ... a FOR condition follows the key expression's NUL */
#define NDX2_MAGIC      0x4E445832  /* "NDX2" */
#define NDX1_MAGIC      0x4E445831  /* "NDX1" */
#define NDX2_PAGE_SIZE  4096
#define MAX_INDEX_KEY   100
#define MAX_INDEXES     7

#define PAGE_INTERNAL   0x01
#define PAGE_LEAF       0x02
#define PAGE_FREE       0x00

#define ND_MAX_PAGES    1000000     /* Reasonable limit for 4GB index */
#define PAGE_HASH_SIZE  1024

typedef struct ndx_page {
    int page_no;        /* page number in file */
    int type;           /* PAGE_INTERNAL or PAGE_LEAF */
    int nkeys;          /* number of keys in this page */
    int dirty;          /* needs writing to disk */
    int pin_count;      /* in-use pin count (not evictable if >0) */

    /* Internal node: child page numbers */
    int *children;      /* malloc'd [max_keys+1] -- internal only */

    /* Leaf node: doubly-linked chain (in memory only: index_t's leaf_link
     * keeps it for pages out of the cache; the file has no such links) */
    int next_leaf;      /* next leaf page number (0 = none) */
    int prev_leaf;      /* prev leaf page number (0 = none) */

    /* Common: keys and record numbers */
    uint32_t *recnos;   /* malloc'd [max_keys] -- leaf only */
    char *keys;         /* malloc'd [max_keys * key_len] */

    /* LRU cache links */
    struct ndx_page *lru_prev;
    struct ndx_page *lru_next;

    /* Hash table links (sparse page table) */
    struct ndx_page *hash_next;
} ndx_page_t;

typedef struct {
    char filename[64];
    char key_expr[256];
    struct ast_node *key_ast; /* cached key expression AST (NULL if dynamic) */
    int key_has_macro;        /* expression contains &macro; must eval dynamically */
    int key_len;            /* key length in memory: a numeric/date key is 16
                               characters that sort as the double does */
    int key_type;           /* 0=char, 1=numeric or date */
    int codepage;           /* its table's mark (codepage.h), 0 for none -- the
                               program's: a key is held in the table's codepage */
    int disk_key_len;       /* key length in the file (8 for numeric/date) */
    int item_size;          /* bytes per item in a page */
    int keys_per_page;      /* the header's keys per page */
    int alloc_keys;         /* key slots a page's arrays hold */
    int max_keys_leaf;      /* max keys per leaf page */
    int max_keys_internal;  /* max keys per internal page */
    int *leaf_link;         /* [2*page]: next and previous leaf of a leaf page */
    int leaf_link_cap;      /* pages leaf_link has room for */

    /* Tree structure */
    int root_page;          /* page number of root */
    int first_leaf;         /* page number of first leaf (for GO TOP) */
    int num_pages;          /* total pages in file */
    int nentries;           /* total entries */

    /* Page table (Hash) + LRU cache */
    ndx_page_t *page_hash[PAGE_HASH_SIZE];
    int cache_capacity;     /* max pages to keep in memory */
    int cache_size;         /* current cached pages */
    ndx_page_t *lru_head;   /* most recently used */
    ndx_page_t *lru_tail;   /* least recently used */

    /* File handle for incremental page writes */
    FILE *fp;               /* open file, or NULL if not yet opened */

    /* Iterator */
    int iter_page;          /* current leaf page number (-1 = not positioned) */
    int iter_pos;           /* position within leaf */

    int active;
    int unique;             /* 1 = unique index */

    /* Conditional / descending index */
    char for_expr[256];           /* FOR condition (empty = no filter) */
    struct ast_node *for_ast;     /* compiled FOR AST */
    int descending;               /* 1 = descending key order */
} index_t;

/* Initialize an index structure */
void index_init(index_t *idx);

/* Build index from database using key expression */
/* 0, or -1 on a file error, or -2 when the key or FOR expression failed on
 * a record (ctx->error holds its message; nothing is built) */
int index_build(index_t *idx, dbf_t *db, expr_ctx_t *ctx, const char *key_expr, const char *filename,
                const char *for_expr, int descending);

/* Format an evaluated expression value into a canonical index key string
   for the given key type (0=char, 1=numeric or date). */
void index_format_key_value(int key_type, const value_t *val, char *buf, int size);

/* Write index to .NDX file */
int index_write(index_t *idx);

/* Read index from .NDX file */
int index_read(index_t *idx, const char *filename);

/* Close and free index */
void index_close(index_t *idx);

/* Binary search for key. Positions iterator. Returns 1 if exact match, 0 if not.
   match_len: if >0, compare only first match_len bytes (prefix match for partial keys).
              if 0, compare full key_len bytes (exact match). */
int index_seek(index_t *idx, const char *key, int match_len);

/* Get record number at current position. Returns 0 if not positioned. */
uint32_t index_current_recno(const index_t *idx);

/* Move to next/previous in index order. Returns 0 on success, -1 if at end. */
int index_next(index_t *idx);
int index_prev(index_t *idx);

/* Move to first/last entry */
void index_top(index_t *idx);
void index_bottom(index_t *idx);

/* Check if key exists in index (excluding a specific recno).
   Returns 1 if a duplicate exists, 0 if not. */
int index_key_exists(index_t *idx, const char *key, uint32_t exclude_recno);

/* Insert a key+recno into the index.
   Returns 0 on success, 1 if rejected (UNIQUE duplicate), -1 on error. */
int index_insert(index_t *idx, const char *key, uint32_t recno);

/* Remove a key+recno from the index */
int index_remove(index_t *idx, const char *key, uint32_t recno);

/* Clear all entries (for ZAP) */
void index_clear(index_t *idx);

/* Flush dirty pages to disk */
void index_flush(index_t *idx);

/* Compile/clear cached key AST for index key_expr. */
int index_compile_key_ast(index_t *idx, struct memvar_store *store);
void index_clear_key_ast(index_t *idx);

#endif
