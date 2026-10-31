#ifndef CLIPRT_H
#define CLIPRT_H

#include <setjmp.h>
#include "expr.h"
#include "program.h"
#include "ast.h"
#include "command.h"
#include "func.h"
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
void clip_exit_proc(clip_fn_t fn);   /* an EXIT PROCEDURE, run by clip_fini */

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
    int prev_depth, prev_live_max;   /* the store's depths on entry, back on leave */
    int nargs;             /* PCOUNT() */
    const char *name;      /* PROCNAME() */
    int nstatics;          /* STATICs bound in this call, written back on return */
    const char *static_names[16];
    value_t *static_vals[16];
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
/* A compiled code block being evaluated: PROCNAME()'s "(b)NAME" level,
 * between the scope it runs under and what it calls */
typedef struct clip_bframe {
    struct clip_bframe *prev;
    clip_scope_t *under;   /* the scope that was innermost when it began */
    const char *name;      /* "(b)" and the function it is written in */
    int def_line;          /* the line it is written on */
    int call_line;         /* the line it was evaluated from */
} clip_bframe_t;
void clip_bframe_push(clip_bframe_t *f, const char *name, int def_line);
void clip_bframe_pop(clip_bframe_t *f);

typedef struct clip_seq {
    jmp_buf jb;
    struct clip_seq *prev;
    clip_scope_t *scope;
    clip_bframe_t *bframe;
    int depth;             /* the LOCALs' depth: a BREAK out of a block (an
                              error handler's) skips its evaluation's reset */
    int locals_hidden;     /* likewise memvar_locals_hidden, out of a macro */
} clip_seq_t;
jmp_buf *clip_seq_begin(clip_seq_t *q);
void clip_seq_end(void);
void clip_break(void);
/* BREAK with a value (NULL: none), the value from expression text (a
 * macro), and RECOVER USING's variable given what the BREAK carried */
void clip_break_v(const value_t *v);
void clip_break_s(const char *expr);
void clip_recover_using(const char *name);

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
/* STATIC name: its storage bound as a LOCAL of this call (s NULL: the
 * main program), and kept on return */
void clip_static_bind(clip_scope_t *s, const char *name, value_t *storage);
/* The main program's name for PROCNAME(): its file's, upper case */
void clip_main_name(const char *name);

void clip_register_fn(const char *name, clip_fn_t fn);
void clip_startup(void);
int clip_errorlevel(void);   /* ERRORLEVEL(): the program's exit status */   /* after the functions are registered: ErrorSys() when the program has one */
void clip_do(const char *name);
void clip_enter(clip_scope_t *s);
void clip_param(clip_scope_t *s, const char *name, value_t *args, int nargs, int idx);
void clip_private(clip_scope_t *s, const char *name);
void clip_local(clip_scope_t *s, const char *name);
void clip_leave(clip_scope_t *s);
void clip_return(clip_scope_t *s, const char *expr, value_t *result);
void clip_end(clip_scope_t *s, value_t *result);

#endif
