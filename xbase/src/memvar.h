#ifndef MEMVAR_H
#define MEMVAR_H

#include "expr.h"

#define MEMVAR_MAX     256
#define MEMVAR_NAMELEN  33
#define MEMVAR_SIGNIFICANT 10   /* Clipper keeps ten characters of a name, and programs lean on it (RL's frm_pagehdr is its .MEM's FRM_PAGEHD) */

/* An array, as Clipper has them: one dimension, any values (arrays too, so
 * a[i][j] and DECLARE a[r,c] nest), resizable, shared by reference --
 * copying a value_t copies the reference.  Arrays live until the program
 * ends (no collector yet: a known debt). */
typedef struct array_s {
    int len;
    int cap;
    value_t *el;
} array_t;

array_t *array_new(int len);                  /* len elements, NIL */
array_t *array_new_dims(int rows, int cols);  /* cols > 0: rows arrays of cols */
int  array_resize(array_t *a, int len);       /* grow with NIL, or shrink */
int  array_add(array_t *a, const value_t *v);
value_t val_array(array_t *a);

typedef struct {
    char name[MEMVAR_NAMELEN];
    value_t val;
    int used;
    unsigned hash;      /* of the (upper-case) name: rejects a slot without a string compare */
    int scope_depth;    /* call_depth at which this var was created */
    int is_local;       /* 1 = LOCAL variable, only visible at exact scope */
} memvar_t;

#define MEMVAR_BUCKETS 512   /* power of two */

typedef struct memvar_store {
    memvar_t vars[MEMVAR_MAX];
    /* The used slots chained by name hash: a lookup visits the slots whose
     * names share a bucket, not every live slot */
    short bucket[MEMVAR_BUCKETS];
    short next[MEMVAR_MAX];
    int count;
    int hw;             /* slots at or above this index are unused: a lookup walks hw slots, not 256 */
    int current_depth;  /* updated on push/pop frame */
    int live_max;       /* the deepest live frame: a block is evaluated at its
                           maker's depth, so a call made from it takes a depth
                           above every live frame, not maker + 1 (which was a
                           live frame's own, and leaving released its LOCALs) */
} memvar_store_t;

/* Bumped whenever an array is declared or freed: name(...) resolves to an array
 * or a function call at compile time, so cached expression trees are dropped. */
extern unsigned memvar_array_gen;

void memvar_init(memvar_store_t *store);
/* Detached locals (memvar.c): which call is now at a depth; whether name
 * is a LOCAL of the current one; the cell the blocks of the current call
 * share for that LOCAL (NULL: none to be had) */
unsigned memvar_frame_serial(int depth);
int memvar_local_here(const memvar_store_t *store, const char *name);
array_t *memvar_capture_cell(memvar_store_t *store, const char *name, const value_t *now);
int  memvar_find(const memvar_store_t *store, const char *name, value_t *val);
int  memvar_set(memvar_store_t *store, const char *name, const value_t *val);
int  memvar_declare_array(memvar_store_t *store, const char *name, int rows, int cols);
int  memvar_set_elem(memvar_store_t *store, const char *name, int row, int col, const value_t *val);
int  memvar_get_elem(const memvar_store_t *store, const char *name, int row, int col, value_t *val);
int  memvar_set_local(memvar_store_t *store, const char *name, const value_t *val);
int  memvar_release(memvar_store_t *store, const char *name);
void memvar_release_locals(memvar_store_t *store, int depth);
void memvar_release_all(memvar_store_t *store);
int  memvar_release_matching(memvar_store_t *store, const char *pattern, int like);
int  memvar_pattern_match(const char *name, const char *pat);   /* * and ? in pat */
void memvar_norm(char *dst, const char *name);   /* upper case, ten characters; dst holds MEMVAR_NAMELEN */
void memvar_display(const memvar_store_t *store);

#endif
