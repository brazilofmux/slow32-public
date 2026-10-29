#include "cliprt.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dbf.h"
#include "command.h"
#include "program.h"
#include "screen.h"
#include "expr.h"
#include "memvar.h"
#include "func.h"
#include "util.h"

#define CLIP_MAX_FNS 64

static dbf_t clip_db;

static struct {
    char name[MEMVAR_NAMELEN];
    clip_fn_t fn;
} clip_fns[CLIP_MAX_FNS];
static int clip_nfn;
static udf_callback_t clip_prev_udf;
static clip_call_t *clip_next_call;   /* the DO ... WITH about to enter */

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
            return clip_fns[i].fn(args, nargs, result);
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
    str_copy(clip_fns[clip_nfn].name, upper, MEMVAR_NAMELEN);
    clip_fns[clip_nfn].fn = fn;
    clip_nfn++;
}

void clip_init(void) {
    char line[32];
    dbf_init(&clip_db);
    prog_init();
    clip_prev_udf = func_get_udf_callback();
    func_set_udf_callback(clip_udf_dispatch);
    prog_set_do_hook(clip_do_hook);
    strcpy(line, "SET TALK OFF");
    cmd_execute(&clip_db, line);
}

void clip_fini(void) {
    cmd_close_all();
    screen_shutdown();
}

void clip_cmd(const char *line) {
    char buf[512];
    strncpy(buf, line, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    /* cmd_execute returns 1 on QUIT (including QUIT inside DO file). */
    if (cmd_execute(&clip_db, buf)) {
        clip_fini();
        exit(0);
    }
}

void clip_at(const char *file, int line) {
    prog_set_compiled_loc(file, line);
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
    if (expr_eval_str(ctx, expr, out) != 0) {
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

void clip_q(const char *expr, int newline) {
    const char *p = expr;
    int first = 1;
    expr_ctx_t *ctx;

    cmd_ctx_setup();
    ctx = cmd_get_expr_ctx();
    while (*p) {
        value_t val;
        char buf[256];
        while (*p == ' ' || *p == '\t') {
            p++;
        }
        if (*p == '\0') {
            break;
        }
        if (expr_eval(ctx, &p, &val) != 0) {
            expr_failed(ctx);
            return;
        }
        val_to_string(&val, buf, sizeof(buf));
        if (!first) {
            cmd_output(" ");
        }
        cmd_output(buf);
        first = 0;
        while (*p == ' ' || *p == '\t') {
            p++;
        }
        if (*p == ',') {
            p++;
        }
    }
    if (newline) {
        cmd_output("\n");
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
    s->call = clip_next_call;
    clip_next_call = NULL;
    prog_get_compiled_loc(&s->at_file, &s->at_line);
    store->current_depth++;
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
    if (store->current_depth > 0) {
        store->current_depth--;
    }
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
