#ifndef SBASIC_ARRAY_H
#define SBASIC_ARRAY_H

#include "value.h"
#include "error.h"

#define MAX_ARRAY_DIMS 8
#define ARRAY_KEY_MAX 96            /* a name with its scope: "id:NAME" */

typedef struct {
    char name[ARRAY_KEY_MAX];
    val_type_t elem_type;
    int ndims;
    int dims[MAX_ARRAY_DIMS];   /* size of each dimension */
    int lbound[MAX_ARRAY_DIMS];  /* each dimension's lower bound (lo TO hi, or OPTION BASE) */
    int total;                   /* total number of elements */
    value_t *data;              /* flat row-major storage */
    int active;                  /* 1 if allocated */
    int fixlen;                  /* AS STRING * n: n; else 0 */
} sb_array_t;

/* Bumped whenever an array is made, erased or re-dimensioned: a pointer
 * to an array (and to its data) stays good while it is unchanged
 * (compiled programs cache them) */
extern unsigned array_generation;

/* QuickBASIC's arrays of a procedure (array.c): how a name is scoped in
 * one -- the main program's, the activation's own, the procedure's STATIC */
enum { ARRAY_MODULE, ARRAY_LOCAL, ARRAY_STATIC };
typedef int (*array_mode_fn)(const char *name, void *ctx);
error_t array_scope_push(int proc, array_mode_fn mode, void *ctx);
void array_scope_pop(void);
/* name's key in the current scope by mode (for names compiled code scopes) */
const char *array_key_mode(const char *name, int mode, char *buf, size_t cap);

/* Clear all arrays (call at program start) */
void array_clear_all(void);

/* Create an array. sizes[] are dimension sizes (not upper bounds). */
error_t array_dim(const char *name, val_type_t type,
                  int *sizes, int ndims, const int *lbounds);

/* Find an array by name (NULL if not found) */
sb_array_t *array_find(const char *name);

/* Get pointer to element at indices (does NOT copy) */
error_t array_get(const char *name, int *indices, int nindices,
                  value_t **out);

/* Set element at indices */
error_t array_set(const char *name, int *indices, int nindices,
                  const value_t *val);

/* Erase a single array */
void array_erase(const char *name);

/* Redimension (preserve=1 keeps data) */
error_t array_redim(const char *name, val_type_t type,
                    int *sizes, int ndims, const int *lbounds, int preserve);

#endif
