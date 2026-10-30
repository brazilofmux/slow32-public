#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "memvar.h"
#include "gc.h"
#include "program.h"
#include "util.h"

unsigned memvar_array_gen;

/* Arrays are shared by reference and not collected: "freeing" one (its
 * variable released or overwritten) leaves it to whoever else holds it.
 * The generation still moves, as name(...) may now parse differently. */
static void array_free(array_t *arr) {
    (void)arr;
    memvar_array_gen++;
}

array_t *array_new(int len) {
    array_t *a = calloc(1, sizeof(array_t));
    int i;
    if (!a) return NULL;
    if (len < 0) len = 0;
    a->cap = len > 4 ? len : 4;
    a->el = calloc((size_t)a->cap, sizeof(value_t));
    if (!a->el) { free(a); return NULL; }
    gc_register(a, GC_ARRAY, sizeof(array_t) + (size_t)a->cap * sizeof(value_t));
    for (i = 0; i < len; i++) a->el[i] = val_nil();
    a->len = len;
    return a;
}

array_t *array_new_dims(int rows, int cols) {
    array_t *a = array_new(rows);
    int i;
    if (!a || cols <= 0) return a;
    for (i = 0; i < rows; i++) {
        array_t *row = array_new(cols);
        if (!row) return NULL;
        a->el[i] = val_array(row);
    }
    return a;
}

int array_resize(array_t *a, int len) {
    int i;
    if (len < 0) len = 0;
    if (len > a->cap) {
        int cap = a->cap * 2 > len ? a->cap * 2 : len;
        value_t *n = realloc(a->el, (size_t)cap * sizeof(value_t));
        if (!n) return -1;
        a->el = n;
        a->cap = cap;
    }
    for (i = a->len; i < len; i++) a->el[i] = val_nil();
    a->len = len;
    return 0;
}

int array_add(array_t *a, const value_t *v) {
    if (array_resize(a, a->len + 1) != 0) return -1;
    a->el[a->len - 1] = *v;
    return 0;
}

value_t val_array(array_t *a) {
    value_t v = val_nil();
    v.type = VAL_ARRAY;
    v.array = a;
    return v;
}

/* FNV-1a over the upper-case name.  Slots are ~300 bytes, so a lookup that
 * walked all 256 with a case-insensitive compare touched 77KB; now it walks
 * the live prefix and compares one word per slot. */
static unsigned name_hash(const char *s) {
    unsigned h = 2166136261u;
    while (*s) { h ^= (unsigned char)*s++; h *= 16777619u; }
    return h;
}

static void slot_link(memvar_store_t *store, int i) {
    int b = (int)(store->vars[i].hash & (MEMVAR_BUCKETS - 1));
    store->next[i] = store->bucket[b];
    store->bucket[b] = (short)i;
}

/* Slot i leaves use: off its chain, then marked free */
static void slot_free(memvar_store_t *store, int i) {
    short *pp = &store->bucket[store->vars[i].hash & (MEMVAR_BUCKETS - 1)];
    while (*pp >= 0) {
        if (*pp == i) { *pp = store->next[i]; break; }
        pp = &store->next[*pp];
    }
    store->vars[i].used = 0;
}

static void hw_grow(memvar_store_t *store, int i) { if (i + 1 > store->hw) store->hw = i + 1; }
static void hw_shrink(memvar_store_t *store) { while (store->hw > 0 && !store->vars[store->hw - 1].used) store->hw--; }

void memvar_init(memvar_store_t *store) {
    int i;
    for (i = 0; i < MEMVAR_MAX; i++)
        store->vars[i].used = 0;
    for (i = 0; i < MEMVAR_BUCKETS; i++)
        store->bucket[i] = -1;
    store->count = 0;
    store->hw = 0;
}

/* The slot an upper-cased name resolves to at the current depth: a LOCAL
 * made at this depth shadows everything of that name; a LOCAL of any other
 * depth is invisible; otherwise the first PRIVATE/PUBLIC.  -1 if none.
 * (LOCAL used to release the variable it shadowed, so a callee saw nothing
 * and the caller's value was gone on return: GitHub issue 113.) */
static int memvar_slot(const memvar_store_t *store, const char *uname, unsigned h) {
    int i, best = -1, local = -1;
    /* The chain is in no particular order, so "first" is the lowest slot */
    for (i = store->bucket[h & (MEMVAR_BUCKETS - 1)]; i >= 0; i = store->next[i]) {
        if (store->vars[i].hash == h && strcmp(store->vars[i].name, uname) == 0) {
            if (store->vars[i].is_local) {
                if (store->vars[i].scope_depth == store->current_depth &&
                    (local < 0 || i < local)) local = i;
                continue;
            }
            if (best < 0 || i < best) best = i;
        }
    }
    return local >= 0 ? local : best;
}

int memvar_find(const memvar_store_t *store, const char *name, value_t *val) {
    int i;
    char uname[64];
    /* Stored names are upper-case (memvar_set, memvar_set_local): one
     * upper-casing of the query and a first-byte reject replace a
     * case-insensitive compare against every used slot. */
    unsigned h;
    str_copy(uname, name, sizeof(uname));
    str_upper(uname);
    h = name_hash(uname);
    i = memvar_slot(store, uname, h);
    if (i < 0) return -1;
    *val = store->vars[i].val;
    return 0;
}

int memvar_set(memvar_store_t *store, const char *name, const value_t *val) {
    int i;
    char uname[MEMVAR_NAMELEN];

    str_copy(uname, name, MEMVAR_NAMELEN);
    str_upper(uname);

    unsigned h = name_hash(uname);

    /* Update existing: the LOCAL of this depth if there is one, else the PRIVATE/PUBLIC */
    i = memvar_slot(store, uname, h);
    if (i >= 0) {
        store->vars[i].val = *val;
        return 0;
    }

    /* Find free slot */
    for (i = 0; i < MEMVAR_MAX; i++) {
        if (!store->vars[i].used) {
            str_copy(store->vars[i].name, uname, MEMVAR_NAMELEN);
            store->vars[i].hash = name_hash(store->vars[i].name);
            store->vars[i].val = *val;
            store->vars[i].used = 1;
            slot_link(store, i);
            store->vars[i].scope_depth = store->current_depth;
            store->vars[i].is_local = 0;
            store->count++;
            hw_grow(store, i);
            return 0;
        }
    }

    prog_error(ERR_TOO_MANY_VARS, "Too many memory variables");
    return -1;
}

int memvar_set_local(memvar_store_t *store, const char *name, const value_t *val) {
    int i;
    char uname[MEMVAR_NAMELEN];

    str_copy(uname, name, MEMVAR_NAMELEN);
    str_upper(uname);

    /* A second LOCAL of the name at this depth resets it; a PRIVATE or
     * PUBLIC of the name is left alone, shadowed until memvar_release_locals. */
    i = memvar_slot(store, uname, name_hash(uname));
    if (i >= 0 && store->vars[i].is_local) {
        store->vars[i].val = *val;
        return 0;
    }

    /* Find free slot */
    for (i = 0; i < MEMVAR_MAX; i++) {
        if (!store->vars[i].used) {
            str_copy(store->vars[i].name, uname, MEMVAR_NAMELEN);
            store->vars[i].hash = name_hash(store->vars[i].name);
            store->vars[i].val = *val;
            store->vars[i].used = 1;
            slot_link(store, i);
            store->vars[i].scope_depth = store->current_depth;
            store->vars[i].is_local = 1;
            store->count++;
            hw_grow(store, i);
            return 0;
        }
    }

    prog_error(ERR_TOO_MANY_VARS, "Too many memory variables");
    return -1;
}

int memvar_release(memvar_store_t *store, const char *name) {
    int i;
    char uname[64];
    str_copy(uname, name, sizeof(uname));
    str_upper(uname);
    i = memvar_slot(store, uname, name_hash(uname));
    if (i < 0) return -1;
    slot_free(store, i);
    store->count--;
    hw_shrink(store);
    return 0;
}

/* The LOCALs of a scope, on its RETURN. */
void memvar_release_locals(memvar_store_t *store, int depth) {
    int i;
    for (i = 0; i < store->hw; i++) {
        if (!store->vars[i].used || !store->vars[i].is_local ||
            store->vars[i].scope_depth != depth) continue;
        slot_free(store, i);
        store->count--;
    }
    hw_shrink(store);
}

void memvar_release_all(memvar_store_t *store) {
    int i;
    for (i = 0; i < MEMVAR_MAX; i++) {
        if (!store->vars[i].used) continue;
        /* Only release variables created at or below current scope */
        if (store->vars[i].scope_depth < store->current_depth) continue;
        slot_free(store, i);
        store->count--;
    }
    hw_shrink(store);
}

/* Case-insensitive wildcard match: '*' and '?' */
static int pattern_match(const char *name, const char *pat) {
    return str_like(name, pat);
}

int memvar_release_matching(memvar_store_t *store, const char *pattern, int like) {
    int i, count = 0;
    for (i = 0; i < MEMVAR_MAX; i++) {
        if (!store->vars[i].used) continue;
        /* Only release variables created at or below current scope */
        if (store->vars[i].scope_depth < store->current_depth) continue;
        int matches = pattern_match(store->vars[i].name, pattern);
        if ((like && matches) || (!like && !matches)) {
            slot_free(store, i);
            store->count--;
            count++;
        }
    }
    hw_shrink(store);
    return count;
}

int memvar_declare_array(memvar_store_t *store, const char *name, int rows, int cols) {
    value_t v;
    array_t *arr;

    if (rows < 1 || rows > 65536 || cols > 65536) return -1;
    arr = array_new_dims(rows, cols);
    if (!arr) return -1;
    v = val_array(arr);
    memvar_array_gen++;
    return memvar_set(store, name, &v) < 0 ? -1 : 0;
}

/* The element row (1-based) of the array name, or with col >= 0, element
 * col of that element (a nested array): name[row] / name[row,col]; col
 * < 0 means no second index */
static value_t *elem_ref(const memvar_store_t *store, const char *name, int row, int col, int *err) {
    value_t mv;
    array_t *arr;
    *err = -1;
    if (memvar_find(store, name, &mv) != 0 || mv.type != VAL_ARRAY || !mv.array) return NULL;
    arr = mv.array;
    *err = -2;
    if (row < 1 || row > arr->len) return NULL;
    if (col < 0) return &arr->el[row - 1];
    if (arr->el[row - 1].type != VAL_ARRAY || !arr->el[row - 1].array) return NULL;
    arr = arr->el[row - 1].array;
    if (col < 1 || col > arr->len) return NULL;
    return &arr->el[col - 1];
}

int memvar_set_elem(memvar_store_t *store, const char *name, int row, int col, const value_t *val) {
    int err;
    value_t *p = elem_ref(store, name, row, col, &err);
    if (!p) return err;
    *p = *val;
    return 0;
}

int memvar_get_elem(const memvar_store_t *store, const char *name, int row, int col, value_t *val) {
    int err;
    value_t *p = elem_ref(store, name, row, col, &err);
    if (!p) return err;
    *val = *p;
    return 0;
}

void memvar_display(const memvar_store_t *store) {
    int i;
    char buf[256];
    int shown = 0;

    for (i = 0; i < MEMVAR_MAX; i++) {
        if (!store->vars[i].used) continue;
        val_to_string(&store->vars[i].val, buf, sizeof(buf));
        switch (store->vars[i].val.type) {
        case VAL_NUM:
            printf("%-10s  N  %s\n", store->vars[i].name, buf);
            break;
        case VAL_CHAR:
            printf("%-10s  C  \"%s\"\n", store->vars[i].name, buf);
            break;
        case VAL_DATE:
            printf("%-10s  D  {%s}\n", store->vars[i].name, buf);
            break;
        case VAL_LOGIC:
            printf("%-10s  L  %s\n", store->vars[i].name, buf);
            break;
        case VAL_ARRAY: {
            array_t *arr = store->vars[i].val.array;
            if (arr->len > 0 && arr->el[0].type == VAL_ARRAY && arr->el[0].array)
                printf("%-10s  A  [%d,%d]\n", store->vars[i].name, arr->len, arr->el[0].array->len);
            else
                printf("%-10s  A  [%d]\n", store->vars[i].name, arr->len);
            break;
        }
        default:
            printf("%-10s  U  %s\n", store->vars[i].name, buf);
            break;
        }
        shown++;
    }
    printf("%d variable(s) defined.\n", shown);
}
