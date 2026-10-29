#ifndef CLIPRT_H
#define CLIPRT_H

#include "expr.h"
#include "memvar.h"

void clip_init(void);
void clip_fini(void);
void clip_let(const char *name, const char *expr);
int  clip_truth(const char *expr);
void clip_q(const char *expr, int newline);
void clip_cmd(const char *line);

/* Compiled FUNCTION / PROCEDURE */
#define CLIP_SCOPE_MAX 32

typedef int (*clip_fn_t)(value_t *args, int nargs, value_t *result);

/* A DO ... WITH call: which arguments came from variables (by reference),
 * and the callee's final values of those parameters, for the write-back */
typedef struct {
    int nargs;
    char (*ref_names)[MEMVAR_NAMELEN];
    value_t out[MAX_FUNC_ARGS];
    int have[MAX_FUNC_ARGS];
} clip_call_t;

typedef struct {
    char names[CLIP_SCOPE_MAX][MEMVAR_NAMELEN];
    value_t saved[CLIP_SCOPE_MAX];
    int valid[CLIP_SCOPE_MAX];
    int count;
    const char *at_file;   /* the caller's location, back on leave */
    int at_line;
    clip_call_t *call;     /* a DO ... WITH with by-reference arguments, or NULL */
    char pnames[MAX_FUNC_ARGS][MEMVAR_NAMELEN];  /* parameter names by position */
    int npnames;
} clip_scope_t;

/* Where the program is: the file and line the next statement came from */
void clip_at(const char *file, int line);

void clip_register_fn(const char *name, clip_fn_t fn);
void clip_do(const char *name);
void clip_enter(clip_scope_t *s);
void clip_param(clip_scope_t *s, const char *name, value_t *args, int nargs, int idx);
void clip_private(clip_scope_t *s, const char *name);
void clip_local(clip_scope_t *s, const char *name);
void clip_leave(clip_scope_t *s);
void clip_return(clip_scope_t *s, const char *expr, value_t *result);
void clip_end(clip_scope_t *s, value_t *result);

#endif
