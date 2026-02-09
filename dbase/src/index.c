#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "index.h"
#include "util.h"

void index_init(index_t *idx) {
    memset(idx, 0, sizeof(index_t));
    idx->current = -1;
}

void index_close(index_t *idx) {
    if (idx->entries) {
        free(idx->entries);
        idx->entries = NULL;
    }
    idx->nentries = 0;
    idx->capacity = 0;
    idx->current = -1;
    idx->active = 0;
    idx->filename[0] = '\0';
    idx->key_expr[0] = '\0';
}

/* Compare function for qsort */
static int entry_compare(const void *a, const void *b) {
    const index_entry_t *ea = (const index_entry_t *)a;
    const index_entry_t *eb = (const index_entry_t *)b;
    int cmp = strcmp(ea->key, eb->key);
    if (cmp == 0)
        return (int)ea->recno - (int)eb->recno; /* stable by recno */
    return cmp;
}

/* Build index from database */
int index_build(index_t *idx, dbf_t *db, expr_ctx_t *ctx, const char *key_expr, const char *filename) {
    uint32_t i;
    dbf_t *saved_db;

    if (!dbf_is_open(db)) return -1;

    /* Clean up any previous data */
    if (idx->entries) free(idx->entries);
    idx->nentries = 0;
    idx->capacity = (db->record_count > 0) ? (int)db->record_count : 64;
    if (idx->capacity > MAX_INDEX_ENTRIES) idx->capacity = MAX_INDEX_ENTRIES;
    idx->entries = (index_entry_t *)malloc(idx->capacity * sizeof(index_entry_t));
    if (!idx->entries) return -1;

    str_copy(idx->key_expr, key_expr, sizeof(idx->key_expr));
    str_copy(idx->filename, filename, sizeof(idx->filename));

    saved_db = ctx->db;
    ctx->db = db;

    for (i = 1; i <= db->record_count; i++) {
        value_t val;
        char keybuf[MAX_INDEX_KEY];

        if (idx->nentries >= MAX_INDEX_ENTRIES) {
            printf("Index overflow (max %d entries).\n", MAX_INDEX_ENTRIES);
            break;
        }

        dbf_read_record(db, i);

        /* Skip deleted records */
        if (db->record_buf[0] == '*') continue;

        if (expr_eval_str(ctx, key_expr, &val) != 0) {
            if (ctx->error) printf("Index key error at record %d: %s\n", (int)i, ctx->error);
            continue;
        }

        val_to_string(&val, keybuf, sizeof(keybuf));
        trim_right(keybuf);

        /* Grow if needed */
        if (idx->nentries >= idx->capacity) {
            int newcap = idx->capacity * 2;
            index_entry_t *newent;
            if (newcap > MAX_INDEX_ENTRIES) newcap = MAX_INDEX_ENTRIES;
            newent = (index_entry_t *)realloc(idx->entries, newcap * sizeof(index_entry_t));
            if (!newent) break;
            idx->entries = newent;
            idx->capacity = newcap;
        }

        str_copy(idx->entries[idx->nentries].key, keybuf, MAX_INDEX_KEY);
        idx->entries[idx->nentries].recno = i;
        idx->nentries++;
    }

    ctx->db = saved_db;

    /* Sort */
    if (idx->nentries > 0)
        qsort(idx->entries, idx->nentries, sizeof(index_entry_t), entry_compare);

    idx->current = (idx->nentries > 0) ? 0 : -1;
    idx->active = 1;

    return 0;
}

/* Write index to .NDX file */
int index_write(const index_t *idx) {
    FILE *fp;
    uint32_t magic = NDX_MAGIC;
    uint32_t nentries = idx->nentries;
    uint32_t keylen = MAX_INDEX_KEY;
    int i;

    fp = fopen(idx->filename, "wb");
    if (!fp) return -1;

    /* Header */
    fwrite(&magic, 4, 1, fp);
    fwrite(&nentries, 4, 1, fp);
    fwrite(&keylen, 4, 1, fp);

    /* Key expression (256 bytes) */
    {
        char expr_buf[256];
        memset(expr_buf, 0, 256);
        str_copy(expr_buf, idx->key_expr, 256);
        fwrite(expr_buf, 256, 1, fp);
    }

    /* Entries */
    for (i = 0; i < idx->nentries; i++) {
        fwrite(idx->entries[i].key, keylen, 1, fp);
        fwrite(&idx->entries[i].recno, 4, 1, fp);
    }

    fclose(fp);
    return 0;
}

/* Read index from .NDX file */
int index_read(index_t *idx, const char *filename) {
    FILE *fp;
    uint32_t magic, nentries, keylen;
    int i;

    fp = fopen(filename, "rb");
    if (!fp) return -1;

    fread(&magic, 4, 1, fp);
    if (magic != NDX_MAGIC) {
        fclose(fp);
        return -1;
    }

    fread(&nentries, 4, 1, fp);
    fread(&keylen, 4, 1, fp);

    /* Read key expression */
    {
        char expr_buf[256];
        fread(expr_buf, 256, 1, fp);
        str_copy(idx->key_expr, expr_buf, sizeof(idx->key_expr));
    }

    /* Allocate entries */
    if (idx->entries) free(idx->entries);
    idx->nentries = (int)nentries;
    idx->capacity = idx->nentries > 0 ? idx->nentries : 1;
    idx->entries = (index_entry_t *)malloc(idx->capacity * sizeof(index_entry_t));
    if (!idx->entries) {
        fclose(fp);
        return -1;
    }

    str_copy(idx->filename, filename, sizeof(idx->filename));

    /* Read entries */
    for (i = 0; i < idx->nentries; i++) {
        memset(idx->entries[i].key, 0, MAX_INDEX_KEY);
        fread(idx->entries[i].key, keylen > MAX_INDEX_KEY ? MAX_INDEX_KEY : keylen, 1, fp);
        fread(&idx->entries[i].recno, 4, 1, fp);
    }

    fclose(fp);

    idx->current = (idx->nentries > 0) ? 0 : -1;
    idx->active = 1;

    return 0;
}

/* Binary search for key */
int index_seek(index_t *idx, const char *key) {
    int lo = 0, hi = idx->nentries - 1;
    int best = idx->nentries; /* position for "not found" */
    char search[MAX_INDEX_KEY];

    str_copy(search, key, MAX_INDEX_KEY);
    trim_right(search);

    while (lo <= hi) {
        int mid = (lo + hi) / 2;
        int cmp = strcmp(idx->entries[mid].key, search);
        if (cmp < 0) {
            lo = mid + 1;
        } else if (cmp > 0) {
            best = mid;
            hi = mid - 1;
        } else {
            /* Exact match - find first occurrence */
            best = mid;
            hi = mid - 1;
        }
    }

    if (best < idx->nentries) {
        idx->current = best;
        /* Check if it's an exact match */
        if (strcmp(idx->entries[best].key, search) == 0)
            return 1;
        return 0;
    }

    idx->current = idx->nentries; /* past end */
    return 0;
}

/* Get record number at current position */
uint32_t index_current_recno(const index_t *idx) {
    if (idx->current < 0 || idx->current >= idx->nentries)
        return 0;
    return idx->entries[idx->current].recno;
}

/* Move to next entry */
int index_next(index_t *idx) {
    if (idx->current < 0) idx->current = 0;
    else idx->current++;
    if (idx->current >= idx->nentries) return -1;
    return 0;
}

/* Move to previous entry */
int index_prev(index_t *idx) {
    if (idx->current <= 0) return -1;
    idx->current--;
    return 0;
}

/* Move to first entry */
void index_top(index_t *idx) {
    idx->current = (idx->nentries > 0) ? 0 : -1;
}

/* Move to last entry */
void index_bottom(index_t *idx) {
    idx->current = (idx->nentries > 0) ? idx->nentries - 1 : -1;
}
