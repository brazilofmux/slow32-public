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

static int eval_expr(const char *expr, value_t *out) {
    expr_ctx_t *ctx;
    cmd_ctx_setup();
    ctx = cmd_get_expr_ctx();
    if (expr_eval_str(ctx, expr, out) != 0) {
        printf("*** %s\n", ctx->error ? ctx->error : "expression error");
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
    switch (v.type) {
    case VAL_LOGIC:
        return v.logic != 0;
    case VAL_NUM:
        return v.num != 0.0;
    case VAL_CHAR:
        return v.str[0] != '\0';
    default:
        return 0;
    }
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
            printf("*** %s\n", ctx->error ? ctx->error : "expression error");
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
    s->count = 0;
}

void clip_param(clip_scope_t *s, const char *name, value_t *args, int nargs, int idx) {
    memvar_store_t *store = cmd_get_memvar_store();
    scope_save(s, name);
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

void clip_leave(clip_scope_t *s) {
    memvar_store_t *store = cmd_get_memvar_store();
    int i;
    for (i = 0; i < s->count; i++) {
        if (s->valid[i]) {
            memvar_set(store, s->names[i], &s->saved[i]);
        } else {
            memvar_release(store, s->names[i]);
        }
    }
    s->count = 0;
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
