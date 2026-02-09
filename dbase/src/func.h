#ifndef FUNC_H
#define FUNC_H

#include "expr.h"

/* Call a built-in function by name. Returns 0 on success, -1 on error. */
int func_call(expr_ctx_t *ctx, const char *name, value_t *args, int nargs, value_t *result);

#endif
