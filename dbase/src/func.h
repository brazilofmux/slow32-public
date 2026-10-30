#ifndef FUNC_H
#define FUNC_H

#include "expr.h"

/* Call a built-in function by name. Returns 0 on success, -1 on error. */
int func_call(expr_ctx_t *ctx, const char *name, value_t *args, int nargs, value_t *result);
/* 1 when a built-in function of this name exists (any case) */
int func_is_builtin(const char *name);
/* A program's function that takes a built-in's name (asked first from then on) */
void func_register_override(const char *name);
int func_is_overridden(const char *upper_name);   /* a program function of the name replaces the built-in */
/* name(@x, ...): refs[i] is the variable argument i was passed by
 * reference from (NULL or "" for a value); a user function writes its
 * parameter's final value back */
int func_call_refs(expr_ctx_t *ctx, const char *name, value_t *args, int nargs,
                   const char *const *refs, value_t *result);
/* For the UDF handlers: the references of the call being made (once) */
const char *const *func_take_refs(void);

/* User-defined function callback. Set by program.c. */
typedef int (*udf_callback_t)(const char *name, value_t *args, int nargs, value_t *result);
void func_set_udf_callback(udf_callback_t cb);
udf_callback_t func_get_udf_callback(void);

/* Cleanup low-level file handles */
void ll_close_all(void);

#endif
