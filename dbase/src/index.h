#ifndef INDEX_H
#define INDEX_H

#include <stdint.h>
#include "dbf.h"
#include "expr.h"

#define MAX_INDEX_KEY     100
#define MAX_INDEX_ENTRIES 4000
#define MAX_INDEXES       7     /* max open indexes per work area */

typedef struct {
    char key[MAX_INDEX_KEY];
    uint32_t recno;
} index_entry_t;

typedef struct {
    char filename[64];
    char key_expr[256];
    index_entry_t *entries;
    int nentries;
    int capacity;
    int current;            /* current position in index (-1 = not positioned) */
    int active;
} index_t;

/* .NDX file magic */
#define NDX_MAGIC 0x4E445831  /* "NDX1" */

/* Initialize an index structure */
void index_init(index_t *idx);

/* Build index from database using key expression */
int index_build(index_t *idx, dbf_t *db, expr_ctx_t *ctx, const char *key_expr, const char *filename);

/* Write index to .NDX file */
int index_write(const index_t *idx);

/* Read index from .NDX file */
int index_read(index_t *idx, const char *filename);

/* Close and free index */
void index_close(index_t *idx);

/* Binary search for key. Sets idx->current. Returns 1 if exact match, 0 if not. */
int index_seek(index_t *idx, const char *key);

/* Get record number at current position. Returns 0 if not positioned. */
uint32_t index_current_recno(const index_t *idx);

/* Move to next/previous in index order. Returns 0 on success, -1 if at end. */
int index_next(index_t *idx);
int index_prev(index_t *idx);

/* Move to first/last entry */
void index_top(index_t *idx);
void index_bottom(index_t *idx);

#endif
