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
    int fixed;                   /* QuickBASIC's static array: ERASE and CLEAR zero it, its bounds stay */
    int wiped;                   /* ... and they have: a DIM of other bounds may make it anew */
    int erased;                  /* a dynamic array ERASE took away (active 0): until it is
                                  * made again a use is out of range, not an implicit array */
} sb_array_t;

/* Bumped whenever an array is made, erased or re-dimensioned: a pointer
 * to an array (and to its data) stays good while it is unchanged
 * (compiled programs cache them) */
extern unsigned array_generation;
extern unsigned array_dim_generation;
/* the activation going on, never used again by another (0: the main
 * program): what a procedure's name means holds within it */
extern unsigned array_scope_now;

/* QuickBASIC's arrays of a procedure (array.c): how a name is scoped in
 * one -- the main program's, the activation's own, the procedure's STATIC */
enum { ARRAY_MODULE, ARRAY_LOCAL, ARRAY_STATIC };
typedef int (*array_mode_fn)(const char *name, void *ctx);
error_t array_scope_push(int proc, array_mode_fn mode, void *ctx);
/* name (or a key) is a procedure's STATIC array */
int array_is_static(const char *name);
/* array name's key where it is now, made to name it from anywhere (a
 * main program's ":NAME"): what an array passed as an argument is */
const char *array_ref_key(const char *name, char *buf, size_t cap);
/* the activation going on names the array at key name (an array
 * parameter, name()) */
error_t array_scope_alias(const char *name, const char *key);
void array_scope_pop(void);
/* name's key in the current scope by mode (for names compiled code scopes) */
const char *array_key_mode(const char *name, int mode, char *buf, size_t cap);

/* Clear all arrays (call at program start) */
void array_clear_all(void);
/* The CLEAR statement: static arrays zeroed, dynamic ones erased */
void array_clear(void);
/* name is a static array (made by a main-program DIM of constant bounds) */
void array_mark_fixed(const char *name);
/* an ERASE took name away and nothing has made it since */
int array_gone(const char *name);
/* name, gone with nothing left behind (SPLIT makes it again, whatever it was) */
void array_remove(const char *name);
/* name is there, and a static DIM of it only declares it */
int array_standing(const char *name);

/* Create an array. sizes[] are dimension sizes (not upper bounds). */
error_t array_dim(const char *name, val_type_t type,
                  int *sizes, int ndims, const int *lbounds);

/* Find an array by name (NULL if not found) */
sb_array_t *array_find(const char *name);

/* the element of an array in hand (its name, a main program's key, is
 * no key in a procedure that has it as a parameter) */
error_t array_elem_of(sb_array_t *arr, int *indices, int nindices, value_t **out);

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
