#ifndef CLIPFN_H
#define CLIPFN_H

/* The rest of Clipper 5.2's function set (the language guide's list):
 * the DB*() functional forms of the database commands, field and memvar
 * blocks, index and work-area queries, the binary conversions, the
 * memo-line functions, files, environment and the interactive few.  One
 * table, registered by func.c beside its own. */

#include "expr.h"

typedef struct {
    const char *name;
    int (*fn)(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);
} clipfn_entry_t;

extern const clipfn_entry_t clipfn_table[];   /* ends with a NULL name */

#endif
