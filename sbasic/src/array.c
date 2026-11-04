#include "array.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* The arrays, each its own allocation (a pointer to one stays good while
 * the table grows); a slot an erased array left is used again */
static sb_array_t **arrays;
static int array_count = 0, array_cap = 0;
unsigned array_generation = 0;

/* --- Scopes: QuickBASIC's arrays of a SUB or FUNCTION -------------------
 *
 * In a procedure an array is its own unless the procedure SHAREs it or
 * the main program DIMs it SHARED; a STATIC one lives on between calls.
 * Each activation is a scope: a name is looked up as its key -- the
 * main program's, NAME; this activation's own, "id:NAME", erased when it
 * returns; the procedure's STATIC, "sP:NAME".  A key has a ':', so a
 * key is its own key (arr->name passed back in is found again). */
typedef struct { unsigned id; int proc; array_mode_fn mode; void *ctx; } array_scope_t;
static array_scope_t *scopes;
static int nscopes, cap_scopes;
static unsigned scope_next = 1;

const char *array_key_mode(const char *name, int mode, char *buf, size_t cap) {
    if (!nscopes || mode == ARRAY_MODULE || strchr(name, ':')) return name;
    if (mode == ARRAY_STATIC) snprintf(buf, cap, "s%d:%s", scopes[nscopes - 1].proc, name);
    else snprintf(buf, cap, "%u:%s", scopes[nscopes - 1].id, name);
    return buf;
}

static const char *array_key(const char *name, char *buf, size_t cap) {
    array_scope_t *t;
    if (!nscopes || strchr(name, ':')) return name;
    t = &scopes[nscopes - 1];
    return array_key_mode(name, t->mode ? t->mode(name, t->ctx) : ARRAY_MODULE, buf, cap);
}

/* a procedure's activation begins (proc: which, for its STATICs; mode
 * says how its names are scoped, NULL for names given as keys) */
error_t array_scope_push(int proc, array_mode_fn mode, void *ctx) {
    if (nscopes == cap_scopes) {
        int cap = cap_scopes ? cap_scopes * 2 : 16;
        array_scope_t *t = realloc(scopes, (size_t)cap * sizeof(*t));
        if (!t) return ERR_OUT_OF_MEMORY;
        scopes = t;
        cap_scopes = cap;
    }
    scopes[nscopes].id = scope_next++;
    scopes[nscopes].proc = proc;
    scopes[nscopes].mode = mode;
    scopes[nscopes].ctx = ctx;
    nscopes++;
    array_generation++;
    return ERR_NONE;
}

/* ... and returns: its own arrays erased */
void array_scope_pop(void) {
    char pre[16];
    size_t n;
    if (!nscopes) return;
    n = (size_t)snprintf(pre, sizeof pre, "%u:", scopes[nscopes - 1].id);
    for (int i = 0; i < array_count; i++)
        if (arrays[i]->active && strncmp(arrays[i]->name, pre, n) == 0) {
            for (int j = 0; j < arrays[i]->total; j++)
                val_clear(&arrays[i]->data[j]);
            free(arrays[i]->data);
            arrays[i]->active = 0;
        }
    nscopes--;
    array_generation++;
}

void array_clear_all(void) {
    array_generation++;
    for (int i = 0; i < array_count; i++) {
        if (arrays[i]->active) {
            for (int j = 0; j < arrays[i]->total; j++)
                val_clear(&arrays[i]->data[j]);
            free(arrays[i]->data);
            arrays[i]->active = 0;
        }
    }
    array_count = 0;
    nscopes = 0;
}

/* A name is looked up from the same place (the same AST string) again
 * and again: a small cache by the name's address and the scope, good
 * while array_generation holds, each hit checked by one comparison */
#define FIND_CACHE 64
static struct { const char *name; unsigned gen, scope; sb_array_t *arr; } find_cache[FIND_CACHE];

sb_array_t *array_find(const char *name) {
    unsigned h = (unsigned)((unsigned long)name >> 2) % FIND_CACHE;
    unsigned sc = nscopes ? scopes[nscopes - 1].id : 0;
    char kb[ARRAY_KEY_MAX];
    const char *key;
    if (find_cache[h].name == name && find_cache[h].gen == array_generation &&
        find_cache[h].scope == sc)
        return find_cache[h].arr;
    key = array_key(name, kb, sizeof kb);
    for (int i = 0; i < array_count; i++) {
        if (arrays[i]->active && strcmp(arrays[i]->name, key) == 0) {
            find_cache[h].name = name;          /* hits only are kept */
            find_cache[h].gen = array_generation;
            find_cache[h].scope = sc;
            find_cache[h].arr = arrays[i];
            return arrays[i];
        }
    }
    return NULL;
}

error_t array_dim(const char *name, val_type_t type,
                  int *sizes, int ndims, int base) {
    char kb[ARRAY_KEY_MAX];
    if (array_find(name))
        return ERR_DUPLICATE_DIM;
    name = array_key(name, kb, sizeof kb);
    array_generation++;

    /* Reuse an inactive slot before growing the table (#6 fix) */
    int slot = -1;
    for (int i = 0; i < array_count; i++) {
        if (!arrays[i]->active) { slot = i; break; }
    }
    if (slot < 0) {
        if (array_count == array_cap) {
            int cap = array_cap ? array_cap * 2 : 64;
            sb_array_t **t = realloc(arrays, (size_t)cap * sizeof(*t));
            if (!t) return ERR_OUT_OF_MEMORY;
            arrays = t;
            for (int i = array_cap; i < cap; i++) arrays[i] = NULL;
            array_cap = cap;
        }
        if (!arrays[array_count] && !(arrays[array_count] = malloc(sizeof(sb_array_t))))
            return ERR_OUT_OF_MEMORY;
        slot = array_count++;
    }

    int total = 1;
    for (int i = 0; i < ndims; i++) {
        if (sizes[i] <= 0) return ERR_ILLEGAL_FUNCTION_CALL;
        total *= sizes[i];
    }

    sb_array_t *arr = arrays[slot];
    memset(arr, 0, sizeof(sb_array_t));
    strncpy(arr->name, name, sizeof arr->name - 1);
    arr->elem_type = type;
    arr->ndims = ndims;
    for (int i = 0; i < ndims; i++)
        arr->dims[i] = sizes[i];
    arr->base = base;
    arr->total = total;
    arr->data = calloc(total, sizeof(value_t));
    if (!arr->data) return ERR_OUT_OF_MEMORY;
    arr->active = 1;

    for (int i = 0; i < total; i++)
        arr->data[i] = val_default(type);

    return ERR_NONE;
}

static int calc_flat_index(sb_array_t *arr, int *indices, int nindices) {
    if (nindices != arr->ndims) return -1;
    int flat = 0;
    int multiplier = 1;
    for (int d = nindices - 1; d >= 0; d--) {
        int idx = indices[d] - arr->base;
        if (idx < 0 || idx >= arr->dims[d]) return -1;
        flat += idx * multiplier;
        multiplier *= arr->dims[d];
    }
    return flat;
}

error_t array_get(const char *name, int *indices, int nindices,
                  value_t **out) {
    sb_array_t *arr = array_find(name);
    if (!arr) return ERR_UNDEFINED_VAR;
    int flat = calc_flat_index(arr, indices, nindices);
    if (flat < 0) return nindices != arr->ndims ? ERR_WRONG_DIMS : ERR_SUBSCRIPT_OUT_OF_RANGE;
    *out = &arr->data[flat];
    return ERR_NONE;
}

error_t array_set(const char *name, int *indices, int nindices,
                  const value_t *val) {
    sb_array_t *arr = array_find(name);
    if (!arr) return ERR_UNDEFINED_VAR;
    int flat = calc_flat_index(arr, indices, nindices);
    if (flat < 0) return nindices != arr->ndims ? ERR_WRONG_DIMS : ERR_SUBSCRIPT_OUT_OF_RANGE;
    val_assign(&arr->data[flat], val);
    return ERR_NONE;
}

void array_erase(const char *name) {
    sb_array_t *arr = array_find(name);
    array_generation++;
    if (arr) {
        for (int j = 0; j < arr->total; j++)
            val_clear(&arr->data[j]);
        free(arr->data);
        arr->active = 0;
    }
}

error_t array_redim(const char *name, val_type_t type,
                    int *sizes, int ndims, int base, int preserve) {
    sb_array_t *arr = array_find(name);
    if (!arr)
        return array_dim(name, type, sizes, ndims, base);
    array_generation++;

    int new_total = 1;
    for (int i = 0; i < ndims; i++) {
        if (sizes[i] <= 0) return ERR_ILLEGAL_FUNCTION_CALL;
        new_total *= sizes[i];
    }

    if (preserve) {
        /* Dimension count must match for PRESERVE to be meaningful */
        if (ndims != arr->ndims)
            return ERR_ILLEGAL_FUNCTION_CALL;

        value_t *new_data = calloc(new_total, sizeof(value_t));
        if (!new_data) return ERR_OUT_OF_MEMORY;

        for (int i = 0; i < new_total; i++)
            new_data[i] = val_default(type);

        /* Coordinate-aware copy through shared bounds (#3 fix).
         * For each coordinate within min(old,new) of every dimension,
         * compute separate old/new flat indices and copy the value. */
        int shared[MAX_ARRAY_DIMS];
        for (int d = 0; d < ndims; d++)
            shared[d] = arr->dims[d] < sizes[d] ? arr->dims[d] : sizes[d];

        int coord[MAX_ARRAY_DIMS];
        memset(coord, 0, sizeof(coord));
        for (;;) {
            /* Old flat index (row-major, old dims) */
            int old_flat = 0, old_mult = 1;
            for (int d = ndims - 1; d >= 0; d--) {
                old_flat += coord[d] * old_mult;
                old_mult *= arr->dims[d];
            }
            /* New flat index (row-major, new dims) */
            int new_flat = 0, new_mult = 1;
            for (int d = ndims - 1; d >= 0; d--) {
                new_flat += coord[d] * new_mult;
                new_mult *= sizes[d];
            }
            val_clear(&new_data[new_flat]);
            new_data[new_flat] = val_copy(&arr->data[old_flat]);

            /* Increment coordinate (rightmost dimension first) */
            int d = ndims - 1;
            while (d >= 0) {
                if (++coord[d] < shared[d]) break;
                coord[d] = 0;
                d--;
            }
            if (d < 0) break;
        }

        for (int i = 0; i < arr->total; i++)
            val_clear(&arr->data[i]);
        free(arr->data);
        arr->data = new_data;
    } else {
        for (int i = 0; i < arr->total; i++)
            val_clear(&arr->data[i]);
        free(arr->data);
        arr->data = calloc(new_total, sizeof(value_t));
        if (!arr->data) return ERR_OUT_OF_MEMORY;
        for (int i = 0; i < new_total; i++)
            arr->data[i] = val_default(type);
    }

    arr->elem_type = type;
    arr->ndims = ndims;
    for (int i = 0; i < ndims; i++)
        arr->dims[i] = sizes[i];
    arr->base = base;
    arr->total = new_total;

    return ERR_NONE;
}
