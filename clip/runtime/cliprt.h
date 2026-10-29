#ifndef CLIPRT_H
#define CLIPRT_H

#include <setjmp.h>
#include "expr.h"
#include "program.h"
#include "ast.h"
#include "command.h"
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

typedef struct clip_scope clip_scope_t;

struct clip_scope {
    char names[CLIP_SCOPE_MAX][MEMVAR_NAMELEN];
    value_t saved[CLIP_SCOPE_MAX];
    int valid[CLIP_SCOPE_MAX];
    int count;
    const char *at_file;   /* the caller's location, back on leave */
    int at_line;
    clip_call_t *call;     /* a DO ... WITH with by-reference arguments, or NULL */
    char pnames[MAX_FUNC_ARGS][MEMVAR_NAMELEN];  /* parameter names by position */
    int npnames;
    clip_scope_t *prev;    /* the scope that called this one */
};

/* FOR var = start TO end [STEP step] ... NEXT */
typedef struct {
    char var[MEMVAR_NAMELEN];
    double end, step;
} clip_for_t;
int clip_for_init(clip_for_t *f, const char *arg);
int clip_for_next(clip_for_t *f);

/* BEGIN SEQUENCE ... [RECOVER ...] END: BREAK, or an error inside, comes
 * back to the setjmp with the scopes entered since unwound */
typedef struct clip_seq {
    jmp_buf jb;
    struct clip_seq *prev;
    clip_scope_t *scope;
} clip_seq_t;
jmp_buf *clip_seq_begin(clip_seq_t *q);
void clip_seq_end(void);
void clip_break(void);

/* Compiled expressions: the context for a statement, a failure reported as
 * the interpreter reports one, a condition's truth, an assignment, a call
 * (an array element when name is an array, as the parser decides), and ?'s
 * output of one value. */
expr_ctx_t *clip_ctx(void);
void clip_fail(expr_ctx_t *c);
int  clip_cond(expr_ctx_t *c, int failed, const value_t *v);
void clip_setv(const char *name, value_t *v);
typedef struct {
    unsigned gen;       /* memvar_array_gen + 1 when decided; 0, never */
    int is_array;
} clip_site_t;
int  clip_call(expr_ctx_t *c, const char *name, value_t *args, int nargs, value_t *result,
               clip_site_t *site);
void clip_qv(const value_t *v, int first);
void clip_qnl(void);
void clip_return_v(clip_scope_t *s, const value_t *v, value_t *result);

/* A line of a TEXT ... ENDTEXT block, printed as it stands */
void clip_text(const char *line);

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
