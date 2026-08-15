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

typedef struct {
    char names[CLIP_SCOPE_MAX][MEMVAR_NAMELEN];
    value_t saved[CLIP_SCOPE_MAX];
    int valid[CLIP_SCOPE_MAX];
    int count;
} clip_scope_t;

void clip_register_fn(const char *name, clip_fn_t fn);
void clip_enter(clip_scope_t *s);
void clip_param(clip_scope_t *s, const char *name, value_t *args, int nargs, int idx);
void clip_private(clip_scope_t *s, const char *name);
void clip_leave(clip_scope_t *s);
void clip_return(clip_scope_t *s, const char *expr, value_t *result);
void clip_end(clip_scope_t *s, value_t *result);

#endif
