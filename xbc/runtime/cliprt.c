#include "cliprt.h"
#include "report.h"
int clip_errorlevel(void);

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dbf.h"
#include "command.h"
#include "program.h"
#include "screen.h"
#include "expr.h"
#include "gc.h"
#include "ast.h"
#include "memvar.h"
#include "func.h"
#include "util.h"

#define CLIP_MAX_FNS 2048   /* DBU alone has 152 functions */

static dbf_t clip_db;

static struct {
    char name[MEMVAR_NAMELEN];
    clip_fn_t fn;
} clip_fns[CLIP_MAX_FNS];
static int clip_nfn;
static udf_callback_t clip_prev_udf;
static clip_call_t *clip_next_call;   /* the DO ... WITH about to enter */
static clip_scope_t *clip_scope_top;  /* the compiled function running */
static clip_seq_t *clip_seq_top;      /* the innermost BEGIN SEQUENCE */

static int clip_recover(void);

static const char *clip_main;

void clip_main_name(const char *name) {
    clip_main = name;
}

/* PROCNAME(level) / PROCLINE(level) for compiled code: the scopes from the
 * innermost, then the main program */
static clip_bframe_t *clip_bframe_top;

void clip_bframe_push(clip_bframe_t *f, const char *name, int def_line) {
    const char *file;
    f->prev = clip_bframe_top;
    f->under = clip_scope_top;
    f->name = name;
    f->def_line = def_line;
    prog_get_compiled_loc(&file, &f->call_line);
    clip_bframe_top = f;
}

void clip_bframe_pop(clip_bframe_t *f) {
    clip_bframe_top = f->prev;
}

/* Level n of the call stack: the scopes and the blocks evaluated under
 * them, innermost first; a scope's line is where it called the level
 * inside it, a block's the line it is written on (Clipper's "(b)NAME") */
static const char *clip_procname(int level, int *line) {
    clip_scope_t *s = clip_scope_top;
    clip_bframe_t *b = clip_bframe_top;
    const char *file;
    int pending, main_done = 0;
    prog_get_compiled_loc(&file, &pending);
    for (;;) {
        const char *name;
        int ln, next;
        if (b && b->under == s) {
            name = b->name;
            ln = b->def_line;
            next = b->call_line;
            b = b->prev;
        } else if (s) {
            name = s->name ? s->name : "";
            ln = pending;
            next = s->at_line;
            s = s->prev;
        } else if (!main_done && clip_main) {
            name = clip_main;
            ln = pending;
            next = 0;
            main_done = 1;
        } else {
            *line = 0;
            return "";
        }
        if (level == 0) {
            *line = ln;
            return name;
        }
        level--;
        pending = next;
    }
}

static int clip_pcount(void) {
    return clip_scope_top ? clip_scope_top->nargs : 0;
}

static void name_upper(char *dst, const char *src, int n) {
    int i;
    for (i = 0; src[i] && i < n - 1; i++) {
        dst[i] = (char)toupper((unsigned char)src[i]);
    }
    dst[i] = '\0';
}

static int clip_udf_dispatch(const char *name, value_t *args, int nargs, value_t *result) {
    int i;
    for (i = 0; i < clip_nfn; i++) {
        if (strcmp(clip_fns[i].name, name) == 0) {
            const char *const *refs = func_take_refs();
            char ref_names[MAX_FUNC_ARGS][MEMVAR_NAMELEN];
            clip_call_t call;
            int k, rc;
            if (!refs) {
                return clip_fns[i].fn(args, nargs, result);
            }
            /* f(@x): the parameters' final values back to the caller's */
            memset(&call, 0, sizeof(call));
            memset(ref_names, 0, sizeof(ref_names));
            for (k = 0; k < nargs && k < MAX_FUNC_ARGS; k++) {
                if (refs[k] && refs[k][0]) {
                    str_copy(ref_names[k], refs[k], MEMVAR_NAMELEN);
                    str_upper(ref_names[k]);
                }
            }
            call.nargs = nargs;
            call.ref_names = ref_names;
            clip_next_call = &call;
            rc = clip_fns[i].fn(args, nargs, result);
            clip_next_call = NULL;
            for (k = 0; k < nargs && k < MAX_FUNC_ARGS; k++) {
                if (call.have[k]) {
                    memvar_set(cmd_get_memvar_store(), ref_names[k], &call.out[k]);
                }
            }
            return rc;
        }
    }
    if (clip_prev_udf) {
        return clip_prev_udf(name, args, nargs, result);
    }
    return 1;
}

/* Run an inlined PROCEDURE by name as a command (DO <name>, no WITH args).
 * The compiler emits this for a bare DO whose target was compiled in, since
 * such procedures are reachable only through the UDF registry, not the on-disk
 * program loader that clip_cmd("DO ...") would use. Name match is
 * case-insensitive, like the interpreter. Falls back to clip_cmd so a name
 * that turns out not to be registered still gets the normal DO handling. */
void clip_do(const char *name) {
    char upper[MEMVAR_NAMELEN];
    value_t result;
    int i;
    name_upper(upper, name, (int)sizeof(upper));
    for (i = 0; i < clip_nfn; i++) {
        if (strcmp(clip_fns[i].name, upper) == 0) {
            clip_fns[i].fn(NULL, 0, &result);
            return;
        }
    }
    {
        char line[MEMVAR_NAMELEN + 4];
        snprintf(line, sizeof(line), "DO %s", name);
        clip_cmd(line);
    }
}

/* The interpreter's DO, for a procedure compiled in: run it, then copy each
 * by-reference parameter's final value back to the caller's variable. */
static int clip_do_hook(const char *name, value_t *args, int nargs,
                        char ref_names[][MEMVAR_NAMELEN]) {
    char upper[MEMVAR_NAMELEN];
    clip_call_t call;
    value_t result;
    int i;

    name_upper(upper, name, (int)sizeof(upper));
    for (i = 0; i < clip_nfn; i++) {
        if (strcmp(clip_fns[i].name, upper) == 0) {
            break;
        }
    }
    if (i == clip_nfn) {
        return 0;
    }
    memset(&call, 0, sizeof(call));
    call.nargs = nargs;
    call.ref_names = ref_names;
    clip_next_call = &call;
    clip_fns[i].fn(args, nargs, &result);
    clip_next_call = NULL;
    for (i = 0; i < nargs && i < MAX_FUNC_ARGS; i++) {
        if (call.have[i]) {
            memvar_set(cmd_get_memvar_store(), ref_names[i], &call.out[i]);
        }
    }
    return 1;
}

/* Clipper runs ErrorSys() before the program (CA's ERRORSYS.PRG:
 * "automatically executes at startup"); a program that supplies its own
 * gets its error block posted this way */
void clip_startup(void) {
    int i;
    for (i = 0; i < clip_nfn; i++) {
        if (strcmp(clip_fns[i].name, "ERRORSYS") == 0) {
            value_t r;
            clip_fns[i].fn(NULL, 0, &r);
            return;
        }
    }
}

void clip_register_fn(const char *name, clip_fn_t fn) {
    char upper[MEMVAR_NAMELEN];
    int i;
    name_upper(upper, name, (int)sizeof(upper));
    for (i = 0; i < clip_nfn; i++) {
        if (strcmp(clip_fns[i].name, upper) == 0) {
            clip_fns[i].fn = fn;
            return;
        }
    }
    if (clip_nfn >= CLIP_MAX_FNS) {
        return;
    }
    func_register_override(upper);   /* a built-in's name: the program's wins */
    str_copy(clip_fns[clip_nfn].name, upper, MEMVAR_NAMELEN);
    clip_fns[clip_nfn].fn = fn;
    clip_nfn++;
}

void clip_init(void) {
    char line[32];
    gc_set_stack_base(line + sizeof(line));   /* the collector scans the stack up to here (main holds no values) */
    dbf_init(&clip_db);
    prog_init();
    clip_prev_udf = func_get_udf_callback();
    func_set_udf_callback(clip_udf_dispatch);
    prog_set_do_hook(clip_do_hook);
    prog_set_compiled_recover(clip_recover);
    prog_set_pcount_hook(clip_pcount);
    prog_set_procname_hook(clip_procname);
    strcpy(line, "SET TALK OFF");
    cmd_execute(&clip_db, line);
    report_set_clipper_layout(1);   /* REPORT FORM prints CA-Clipper's layout */
}

void clip_fini(void) {
    cmd_close_all();
    screen_shutdown();
}

static int clip_trace_on(void);

void clip_cmd(const char *line) {
    char buf[2048];   /* s32-xbc's joined-statement limit (RL's PRIVATE list runs to 700) */
    strncpy(buf, line, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    if (clip_trace_on()) fprintf(stderr, "  cmd: %s\n", buf);
    /* cmd_execute returns 1 on QUIT (including QUIT inside DO file). */
    if (cmd_execute(&clip_db, buf)) {
        clip_fini();
        exit(clip_errorlevel());
    }
}

void clip_static_bind(clip_scope_t *s, const char *name, value_t *storage) {
    memvar_store_t *store = cmd_get_memvar_store();
    if (!s) {
        memvar_set(store, name, storage);
        return;
    }
    memvar_set_local(store, name, storage);
    if (s->nstatics < 16) {
        s->static_names[s->nstatics] = name;
        s->static_vals[s->nstatics] = storage;
        s->nstatics++;
    }
}

/* XBC_TRACE=1 in the environment: every statement (its function and
 * source line) and every command handed to the interpreter, on stderr --
 * the way to see where a compiled program went before it stopped. */
static int clip_trace_on(void) {
    static int on = -1;
    if (on < 0) {
        const char *e = getenv("XBC_TRACE");
        on = e && *e && *e != '0';
    }
    return on;
}

void clip_at(const char *file, int line) {
    prog_set_compiled_loc(file, line);
    gc_safepoint();   /* a statement boundary: collect when due */
    if (clip_trace_on())
        fprintf(stderr, "[%s %d]\n", clip_scope_top && clip_scope_top->name ? clip_scope_top->name : "-", line);
}

/* An expression error, reported as the interpreter reports one */
static void expr_failed(expr_ctx_t *ctx) {
    const char *msg = ctx->error ? ctx->error : "expression error";
    prog_error(expr_error_code(msg), msg);
}

static int eval_expr(const char *expr, value_t *out) {
    expr_ctx_t *ctx;
    cmd_ctx_setup();
    ctx = cmd_get_expr_ctx();
    if (ast_eval_dynamic(expr, ctx, out) != 0) {
        expr_failed(ctx);
        return -1;
    }
    return 0;
}

void clip_let(const char *name, const char *expr) {
    value_t v;
    if (eval_expr(expr, &v) != 0) {
        return;
    }
    memvar_set(cmd_get_memvar_store(), name, &v);
}

int clip_truth(const char *expr) {
    value_t v;
    if (eval_expr(expr, &v) != 0) {
        return 0;
    }
    /* Interpreter IF/WHILE/CASE require VAL_LOGIC. */
    if (v.type == VAL_LOGIC) {
        return v.logic != 0;
    }
    return 0;
}

void clip_qv(const value_t *v, int first);

void clip_q(const char *expr, int newline) {
    const char *p = expr;
    expr_ctx_t *ctx;
    value_t vals[MAX_FUNC_ARGS];
    int n = 0, i;

    cmd_ctx_setup();
    ctx = cmd_get_expr_ctx();
    /* QOUT(): the arguments first, then the new line and the output */
    while (*p == ' ' || *p == '\t') p++;
    while (*p && n < MAX_FUNC_ARGS) {
        if (ast_eval_adv(ctx, &p, &vals[n]) != 0) {
            expr_failed(ctx);
            return;
        }
        n++;
        while (*p == ' ' || *p == '\t') p++;
        if (*p != ',') {
            break;
        }
        p++;
        while (*p == ' ' || *p == '\t') p++;
    }
    if (newline) {
        cmd_output("\n");
    }
    for (i = 0; i < n; i++) {
        clip_qv(&vals[i], i == 0);
    }
}

static void scope_save(clip_scope_t *s, const char *name) {
    memvar_store_t *store = cmd_get_memvar_store();
    value_t val;
    char uname[MEMVAR_NAMELEN];
    int i;

    name_upper(uname, name, (int)sizeof(uname));
    for (i = 0; i < s->count; i++) {
        if (str_icmp(s->names[i], uname) == 0) {
            return;
        }
    }
    if (s->count >= CLIP_SCOPE_MAX) {
        return;
    }
    str_copy(s->names[s->count], uname, MEMVAR_NAMELEN);
    if (memvar_find(store, uname, &val) == 0) {
        s->saved[s->count] = val;
        s->valid[s->count] = 1;
    } else {
        s->valid[s->count] = 0;
    }
    s->count++;
}

void clip_enter(clip_scope_t *s) {
    memvar_store_t *store = cmd_get_memvar_store();
    s->count = 0;
    s->npnames = 0;
    s->nargs = 0;
    s->name = NULL;
    s->nstatics = 0;
    s->call = clip_next_call;
    clip_next_call = NULL;
    s->prev = clip_scope_top;
    clip_scope_top = s;
    prog_get_compiled_loc(&s->at_file, &s->at_line);
    /* A fresh depth above every live frame (see memvar_store_t.live_max) */
    s->prev_depth = store->current_depth;
    s->prev_live_max = store->live_max;
    if (store->live_max < store->current_depth) store->live_max = store->current_depth;
    store->current_depth = ++store->live_max;
}

void clip_param(clip_scope_t *s, const char *name, value_t *args, int nargs, int idx) {
    memvar_store_t *store = cmd_get_memvar_store();
    scope_save(s, name);
    if (idx >= 0 && idx < MAX_FUNC_ARGS) {
        name_upper(s->pnames[idx], name, MEMVAR_NAMELEN);
        if (idx >= s->npnames) {
            s->npnames = idx + 1;
        }
    }
    if (idx >= 0 && idx < nargs) {
        memvar_set(store, name, &args[idx]);
    } else {
        value_t nil = val_nil();
        memvar_set(store, name, &nil);
    }
}

void clip_private(clip_scope_t *s, const char *name) {
    scope_save(s, name);
    memvar_release(cmd_get_memvar_store(), name);
}

void clip_local(clip_scope_t *s, const char *name) {
    /* A LOCAL shadows an outer variable of the name without touching it,
     * and clip_leave releases it; nothing to save. */
    value_t nil = val_nil();
    (void)s;
    memvar_set_local(cmd_get_memvar_store(), name, &nil);
}

void clip_leave(clip_scope_t *s) {
    memvar_store_t *store = cmd_get_memvar_store();
    int i;
    /* By-reference parameters: their values now, before the unwinding */
    if (s->call) {
        for (i = 0; i < s->npnames && i < s->call->nargs; i++) {
            if (s->call->ref_names[i][0] && s->pnames[i][0] &&
                memvar_find(store, s->pnames[i], &s->call->out[i]) == 0) {
                s->call->have[i] = 1;
            }
        }
        s->call = NULL;
    }
    /* STATICs keep what this call made of them */
    for (i = 0; i < s->nstatics; i++) {
        memvar_find(store, s->static_names[i], s->static_vals[i]);
    }
    /* A sequence begun in this call and still open ends with it (RETURN) */
    while (clip_seq_top && clip_seq_top->scope == s) {
        clip_seq_top = clip_seq_top->prev;
    }
    clip_scope_top = s->prev;
    /* LOCALs first, so a PRIVATE restore lands on what it shadowed */
    memvar_release_locals(store, store->current_depth);
    for (i = 0; i < s->count; i++) {
        if (s->valid[i]) {
            memvar_set(store, s->names[i], &s->saved[i]);
        } else {
            memvar_release(store, s->names[i]);
        }
    }
    s->count = 0;
    store->current_depth = s->prev_depth;
    store->live_max = s->prev_live_max;
    prog_set_compiled_loc(s->at_file, s->at_line);
}

void clip_return(clip_scope_t *s, const char *expr, value_t *result) {
    if (expr && *expr) {
        if (eval_expr(expr, result) != 0) {
            *result = val_nil();
        }
    } else {
        *result = val_nil();
    }
    clip_leave(s);
}

void clip_end(clip_scope_t *s, value_t *result) {
    clip_leave(s);
    *result = val_nil();
}

/* ---- FOR ... NEXT, as the interpreter's prog_for / prog_next ---- */

int clip_for_init(clip_for_t *f, const char *arg) {
    const char *p = arg;
    value_t start_val, end_val, step_val;
    expr_ctx_t *ctx;
    int i = 0;

    cmd_ctx_setup();
    ctx = cmd_get_expr_ctx();
    while (*p == ' ' || *p == '\t') p++;
    while ((isalnum((unsigned char)*p) || *p == '_') && i < MEMVAR_NAMELEN - 1)
        f->var[i++] = *p++;
    f->var[i] = '\0';
    while (*p == ' ' || *p == '\t') p++;
    if (p[0] == ':' && p[1] == '=')   /* Clipper's FOR i := ... */
        p++;
    if (*p != '=') {
        printf("Syntax error in FOR.\n");
        return 0;
    }
    p++;
    while (*p == ' ' || *p == '\t') p++;
    if (ast_eval_adv(ctx, &p, &start_val) != 0 || start_val.type != VAL_NUM) {
        printf("FOR requires numeric expressions.\n");
        return 0;
    }
    while (*p == ' ' || *p == '\t') p++;
    if (!str_imatch(p, "TO")) {
        printf("Missing TO in FOR.\n");
        return 0;
    }
    p += 2;
    while (*p == ' ' || *p == '\t') p++;
    if (ast_eval_adv(ctx, &p, &end_val) != 0 || end_val.type != VAL_NUM) {
        printf("FOR requires numeric expressions.\n");
        return 0;
    }
    step_val = val_num(1.0);
    while (*p == ' ' || *p == '\t') p++;
    if (str_imatch(p, "STEP")) {
        p += 4;
        while (*p == ' ' || *p == '\t') p++;
        if (ast_eval_adv(ctx, &p, &step_val) != 0 || step_val.type != VAL_NUM) {
            printf("STEP requires numeric expression.\n");
            return 0;
        }
    }
    memvar_set(cmd_get_memvar_store(), f->var, &start_val);
    f->end = end_val.num;
    f->step = step_val.num;
    return !((f->step > 0 && start_val.num > f->end) ||
             (f->step < 0 && start_val.num < f->end));
}

int clip_for_next(clip_for_t *f) {
    memvar_store_t *store = cmd_get_memvar_store();
    value_t val;
    if (memvar_find(store, f->var, &val) != 0 || val.type != VAL_NUM) {
        return 0;
    }
    val.num += f->step;
    memvar_set(store, f->var, &val);
    if (f->step > 0) {
        return !(val.num > f->end);
    }
    if (f->step < 0) {
        return !(val.num < f->end);
    }
    return 1; /* STEP 0 loops forever, as in the interpreter */
}

/* ---- BEGIN SEQUENCE ---- */

jmp_buf *clip_seq_begin(clip_seq_t *q) {
    q->prev = clip_seq_top;
    q->scope = clip_scope_top;
    q->bframe = clip_bframe_top;
    clip_seq_top = q;
    return &q->jb;
}

void clip_seq_end(void) {
    if (clip_seq_top) {
        clip_seq_top = clip_seq_top->prev;
    }
}

/* Back to the innermost sequence's RECOVER, leaving the calls made since */
static void clip_seq_jump(void) {
    clip_seq_t *q = clip_seq_top;
    clip_seq_top = q->prev;
    while (clip_scope_top && clip_scope_top != q->scope) {
        clip_leave(clip_scope_top);
    }
    clip_bframe_top = q->bframe;   /* the blocks a BREAK left */
    prog_break_landed();
    longjmp(q->jb, 1);
}

void clip_break(void) {
    if (!clip_seq_top) {
        prog_break_landed();
        prog_error(ERR_SYNTAX, "BREAK without BEGIN SEQUENCE");
        return;
    }
    clip_seq_jump();
}

void clip_break_v(const value_t *v) {
    prog_set_break_value(v);
    clip_break();
}

static int eval_expr(const char *expr, value_t *out);

void clip_break_s(const char *expr) {
    value_t v;
    if (eval_expr(expr, &v) != 0) {
        return;
    }
    clip_break_v(&v);
}

void clip_recover_using(const char *name) {
    value_t v = prog_break_value();
    memvar_set(cmd_get_memvar_store(), name, &v);
}

static int clip_recover(void) {
    if (!clip_seq_top) {
        return 0;
    }
    clip_seq_jump();
    return 1;
}

void clip_text(const char *line) {
    cmd_output("\n");   /* as ? writes: the new line first (Clipper's TEXT is QOut) */
    printf("%s", line);
}

/* ---- Compiled expressions ---- */

expr_ctx_t *clip_ctx(void) {
    cmd_ctx_setup();
    return cmd_get_expr_ctx();
}

void clip_fail(expr_ctx_t *c) {
    expr_failed(c);
}

int clip_cond(expr_ctx_t *c, int failed, const value_t *v) {
    if (failed) {
        expr_failed(c);
        return 0;
    }
    /* Interpreter IF/WHILE/CASE require VAL_LOGIC. */
    return v->type == VAL_LOGIC && v->logic != 0;
}

/* name = value (an array variable is replaced, as Clipper's assignment
 * replaces it) */
void clip_setv(const char *name, value_t *v) {
    memvar_set(cmd_get_memvar_store(), name, v);
}

/* name(args): an element when name is an array, else a call.  The
 * interpreter decides when it parses and keeps the tree until an array is
 * declared or freed (memvar_array_gen); the call site's cache does the same. */
int clip_call(expr_ctx_t *c, const char *name, value_t *args, int nargs, value_t *result,
              clip_site_t *site) {
    if (site->gen != memvar_array_gen + 1u) {
        value_t mv;
        site->is_array = (nargs == 1 || nargs == 2) && c->vars &&
                         memvar_find(c->vars, name, &mv) == 0 && mv.type == VAL_ARRAY;
        site->gen = memvar_array_gen + 1u;
    }
    if (site->is_array) {
        return ast_eval_elem(c, name, &args[0], nargs == 2 ? &args[1] : NULL, result);
    }
    return func_call(c, name, args, nargs, result);
}

void clip_qv(const value_t *v, int first) {
    char buf[256];
    if (!first) {
        cmd_output(" ");
    }
    if (v->type == VAL_CHAR) {   /* the whole text, however long */
        cmd_output(val_text(v));
        return;
    }
    val_to_display(v, buf, sizeof(buf));
    cmd_output(buf);
}

void clip_qnl(void) {
    cmd_output("\n");
}

void clip_return_v(clip_scope_t *s, const value_t *v, value_t *result) {
    *result = *v;
    clip_leave(s);
}
