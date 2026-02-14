// s32_hashtable.h — Dynamic arrays and hash tables for assembler/linker
// Self-contained, header-only. Uses FNV-1a hashing with open-addressing linear probing.

#ifndef S32_HASHTABLE_H
#define S32_HASHTABLE_H

#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// ── Dynamic array push ──────────────────────────────────────────────────────
// Ensures arr[cnt] is valid.  Grows geometrically (doubles, starting at 64).
// After the macro, the caller may safely access arr[cnt] and then increment cnt.
// NOTE: realloc may move the buffer.  If you hold pointers into it (e.g. hash
// table keys), detect the move and rebuild.
#define DARR_PUSH(arr, cnt, cap, type) do {                     \
    if ((cnt) >= (cap)) {                                       \
        int _new = (cap) ? (cap) * 2 : 64;                     \
        (arr) = (type *)realloc((arr), (size_t)_new * sizeof(type)); \
        memset((arr) + (cap), 0, (size_t)(_new - (cap)) * sizeof(type)); \
        (cap) = _new;                                           \
    }                                                           \
} while (0)

// ── Hash table ──────────────────────────────────────────────────────────────
// Open-addressing, linear probing, FNV-1a 32-bit hash, grows at 70% load.
// Keys are const char* (not owned — must point to stable storage, typically
// the name[] field inside a dynamic array element).
// Values are int (typically an index into the corresponding array).
// "Not found" is signalled by returning -1, so -1 must not be a valid value.

typedef struct {
    const char **keys;   // NULL = empty slot
    int         *values;
    int          capacity;  // always a power of 2
    int          count;
} s32_hashmap_t;

static inline uint32_t s32_fnv1a(const char *key) {
    uint32_t h = 0x811c9dc5u;
    for (const unsigned char *p = (const unsigned char *)key; *p; p++) {
        h ^= *p;
        h *= 0x01000193u;
    }
    return h;
}

static inline void s32_hashmap_init(s32_hashmap_t *m, int initial_cap) {
    // Round up to power of 2
    int cap = 16;
    while (cap < initial_cap) cap <<= 1;
    m->keys     = (const char **)calloc((size_t)cap, sizeof(const char *));
    m->values   = (int *)malloc((size_t)cap * sizeof(int));
    m->capacity = cap;
    m->count    = 0;
}

static inline void s32_hashmap_free(s32_hashmap_t *m) {
    free(m->keys);
    free(m->values);
    m->keys = NULL;
    m->values = NULL;
    m->capacity = m->count = 0;
}

// Returns -1 if not found.
static inline int s32_hashmap_get(const s32_hashmap_t *m, const char *key) {
    if (!m->keys || m->count == 0) return -1;
    uint32_t mask = (uint32_t)(m->capacity - 1);
    uint32_t idx  = s32_fnv1a(key) & mask;
    for (;;) {
        if (!m->keys[idx]) return -1;
        if (strcmp(m->keys[idx], key) == 0) return m->values[idx];
        idx = (idx + 1) & mask;
    }
}

static inline void s32_hashmap_grow(s32_hashmap_t *m);

static inline void s32_hashmap_put(s32_hashmap_t *m, const char *key, int value) {
    // Grow at 70% load
    if (m->count * 10 >= m->capacity * 7) {
        s32_hashmap_grow(m);
    }
    uint32_t mask = (uint32_t)(m->capacity - 1);
    uint32_t idx  = s32_fnv1a(key) & mask;
    for (;;) {
        if (!m->keys[idx]) {
            m->keys[idx]   = key;
            m->values[idx] = value;
            m->count++;
            return;
        }
        if (strcmp(m->keys[idx], key) == 0) {
            m->values[idx] = value;  // update existing
            return;
        }
        idx = (idx + 1) & mask;
    }
}

static inline void s32_hashmap_grow(s32_hashmap_t *m) {
    int old_cap = m->capacity;
    const char **old_keys   = m->keys;
    int         *old_values = m->values;

    int new_cap = old_cap * 2;
    m->keys     = (const char **)calloc((size_t)new_cap, sizeof(const char *));
    m->values   = (int *)malloc((size_t)new_cap * sizeof(int));
    m->capacity = new_cap;
    m->count    = 0;

    for (int i = 0; i < old_cap; i++) {
        if (old_keys[i]) {
            s32_hashmap_put(m, old_keys[i], old_values[i]);
        }
    }
    free(old_keys);
    free(old_values);
}

// Reset count to 0, keep allocation.  Used after a realloc moves the backing
// array — caller must re-insert all entries with the new key pointers.
static inline void s32_hashmap_clear(s32_hashmap_t *m) {
    memset(m->keys, 0, (size_t)m->capacity * sizeof(const char *));
    m->count = 0;
}

#endif // S32_HASHTABLE_H
