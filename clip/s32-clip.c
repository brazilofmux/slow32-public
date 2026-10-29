/*
 * s32-clip — Clipper experiment: .prg to C.
 *
 * Control flow becomes C (IF / ELSEIF / DO WHILE / DO CASE / EXIT / LOOP).
 * FUNCTION / PROCEDURE bodies become C functions registered with the
 * expression evaluator (PAD_ZERO() in INDEX/SEEK hits C).
 * SET PROCEDURE TO <file> inlines <file>.prg from the source directory
 * when it exists; otherwise the command rides clip_cmd() at runtime.
 * Other commands we do not lower become clip_cmd() (cmd_execute).
 */

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"   /* dbase's own parser: expressions are parsed here, at compile time */

#define MAX_LINE  512
#define MAX_NEST  32
#define MAX_LINES 4000
#define MAX_FUNCS 64
#define MAX_FILES 16
#define MAX_PARAMS 16
#define MAX_NAME  32

typedef struct {
    char text[MAX_LINE];
    char src[256];
    int lineno;     /* the interpreter's: logical lines, ; continuations joined */
    int is_text;    /* a line of a TEXT block, kept as it stands */
} src_line_t;

typedef struct {
    char name[MAX_NAME];
    int header;
    int start;
    int end;
    char params[MAX_PARAMS][MAX_NAME];
    int nparams;
} func_t;

typedef struct {
    FILE *out;
    const char *inpath;
    int lineno;
    int depth;
    char nest[MAX_NEST];
    int in_fn;
    int param_i;
    int ended;
} emit_t;

typedef struct {
    char *buf;
    int len, cap;
} sb_t;

static src_line_t g_lines[MAX_LINES];
static int g_nlines;
static char g_loaded[MAX_FILES][256];
static int g_nloaded;
static char g_inlined[MAX_FILES][MAX_NAME];
static int g_ninlined;

/* Names of every FUNCTION/PROCEDURE compiled into this translation unit, so a
 * bare DO can be lowered to a direct registry call (clip_do) instead of the
 * on-disk program loader that clip_cmd would use. Distinct from g_inlined,
 * which tracks SET PROCEDURE *files*, not the procedures they contain. */
static char g_compiled_fn[MAX_FUNCS][MAX_NAME];
static int g_ncompiled_fn;

static int icmp_n(const char *a, const char *b, int n) {
    int i;
    for (i = 0; i < n; i++) {
        int ca = toupper((unsigned char)a[i]);
        int cb = toupper((unsigned char)b[i]);
        if (ca != cb) {
            return ca - cb;
        }
        if (a[i] == '\0' || b[i] == '\0') {
            break;
        }
    }
    return 0;
}

static int is_kw(const char *p, const char *kw) {
    int n = (int)strlen(kw);
    if (icmp_n(p, kw, n) != 0) {
        return 0;
    }
    return p[n] == '\0' || isspace((unsigned char)p[n]) || p[n] == '(';
}

static const char *skip_ws(const char *p) {
    while (*p && isspace((unsigned char)*p)) {
        p++;
    }
    return p;
}

static void strip_comment(char *line) {
    char *p = line;
    int in_str = 0;
    char q = 0;
    if (*skip_ws(line) == '*') {
        *line = '\0';
        return;
    }
    if (is_kw(skip_ws(line), "NOTE")) {
        *line = '\0';
        return;
    }
    while (*p) {
        if (!in_str && p[0] == '&' && p[1] == '&') {
            *p = '\0';
            return;
        }
        if (!in_str && (*p == '"' || *p == '\'')) {
            in_str = 1;
            q = *p;
        } else if (in_str && *p == q) {
            in_str = 0;
        }
        p++;
    }
}

static void rtrim(char *s) {
    int n = (int)strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1])) {
        s[--n] = '\0';
    }
}

static void c_escape(FILE *out, const char *s) {
    fputc('"', out);
    while (*s) {
        if (*s == '\\' || *s == '"') {
            fputc('\\', out);
        }
        fputc(*s, out);
        s++;
    }
    fputc('"', out);
}

static void emit_indent(FILE *out, int depth) {
    int i;
    for (i = 0; i <= depth; i++) {
        fputs("    ", out);
    }
}

static void emit_cmd(FILE *out, int depth, const char *p) {
    emit_indent(out, depth);
    fprintf(out, "clip_cmd(");
    c_escape(out, p);
    fprintf(out, "); /* interp */\n");
}

/* The program name the interpreter would report: the file's base name, as
 * DO would have it, with .PRG */
static void file_label(const char *path, char *buf, int n) {
    const char *b = strrchr(path, '/');
    const char *dot;
    int len;
    b = b ? b + 1 : path;
    dot = strrchr(b, '.');
    len = dot ? (int)(dot - b) : (int)strlen(b);
    if (len > n - 5) {
        len = n - 5;
    }
    memcpy(buf, b, (size_t)len);
    strcpy(buf + len, ".PRG");
}

/* clip_at(file, line) as an expression, for a condition's front */
static void emit_at_expr(emit_t *e) {
    char label[64];
    file_label(e->inpath, label, (int)sizeof(label));
    fprintf(e->out, "clip_at(");
    c_escape(e->out, label);
    fprintf(e->out, ", %d)", e->lineno);
}

static ast_node_t *parse_expr(const char *text);
static int gen_block(sb_t *b, ast_node_t **nodes, int nnodes, int *results);

/* A condition: "<pre>(clip_at(...), <truth>)<post>", the truth compiled
 * when the expression parses, else clip_truth("expr") at run time */
static void emit_cond(emit_t *e, int depth, const char *pre, const char *expr, const char *post) {
    ast_node_t *n = parse_expr(expr);
    emit_indent(e->out, depth);
    fprintf(e->out, "%s(", pre);
    emit_at_expr(e);
    if (n) {
        sb_t b = {0};
        int r;
        gen_block(&b, &n, 1, &r);
        fprintf(e->out, ", ({ %sclip_cond(c, e, &t%d); }))%s\n", b.buf, r, post);
        free(b.buf);
        ast_free(n);
        return;
    }
    fprintf(e->out, ", clip_truth(");
    c_escape(e->out, expr);
    fprintf(e->out, "))%s\n", post);
}

/* ---- Expressions compiled: dbase's parser at compile time, and C that
 * applies the interpreter's own per-node operations (ast_eval_*) ---- */

static void sb_printf(sb_t *b, const char *fmt, ...) {
    va_list ap;
    int n;
    for (;;) {
        va_start(ap, fmt);
        n = vsnprintf(b->buf ? b->buf + b->len : NULL, b->buf ? (size_t)(b->cap - b->len) : 0, fmt, ap);
        va_end(ap);
        if (b->buf && n < b->cap - b->len) {
            b->len += n;
            return;
        }
        b->cap = b->cap * 2 + n + 256;
        b->buf = realloc(b->buf, (size_t)b->cap);
        if (!b->buf) {
            fprintf(stderr, "out of memory\n");
            exit(1);
        }
    }
}

static void sb_cstr(sb_t *b, const char *s) {
    sb_printf(b, "\"");
    for (; *s; s++) {
        if (*s == '\\' || *s == '"') {
            sb_printf(b, "\\%c", *s);
        } else if ((unsigned char)*s < 32 || (unsigned char)*s >= 127) {
            sb_printf(b, "\\%03o", (unsigned char)*s);
        } else {
            sb_printf(b, "%c", *s);
        }
    }
    sb_printf(b, "\"");
}

static const char *op_name(ast_op_t op) {
    static const char *names[] = {
        "OP_ADD", "OP_SUB", "OP_MUL", "OP_DIV", "OP_POWER",
        "OP_EQ", "OP_EXACT_EQ", "OP_NE", "OP_LT", "OP_GT", "OP_LE", "OP_GE", "OP_SUBSTR",
        "OP_AND", "OP_OR", "OP_NOT", "OP_NEGATE"
    };
    return names[op];
}

/* Code that leaves node's value in temporary t<n> (returned); `e` becomes
 * nonzero at the first failing operation, and the rest are skipped. */
static int gen_expr(sb_t *b, int *ntmp, ast_node_t *node) {
    int t = (*ntmp)++;
    int i, a, l, r;
    switch (node->type) {
    case AST_LITERAL:
        switch (node->literal.type) {
        case VAL_NUM:
            sb_printf(b, "t%d = val_num(%.17g); ", t, node->literal.num);
            break;
        case VAL_CHAR:
            sb_printf(b, "t%d = val_str(", t);
            sb_cstr(b, node->literal.str);
            sb_printf(b, "); ");
            break;
        case VAL_LOGIC:
            sb_printf(b, "t%d = val_logic(%d); ", t, node->literal.logic ? 1 : 0);
            break;
        case VAL_DATE:
            sb_printf(b, "t%d = val_date(%ld); ", t, (long)node->literal.date);
            break;
        default:
            sb_printf(b, "t%d = val_nil(); ", t);
            break;
        }
        return t;
    case AST_FIELD:
        sb_printf(b, "e = e || ast_eval_field(c, ");
        sb_cstr(b, node->name);
        sb_printf(b, ", &t%d); ", t);
        return t;
    case AST_MEMVAR:
        sb_printf(b, "e = e || ast_eval_name(c, ");
        sb_cstr(b, node->name);
        sb_printf(b, ", &t%d); ", t);
        return t;
    case AST_ALIAS_FIELD:
        sb_printf(b, "e = e || ast_eval_alias_field(c, ");
        sb_cstr(b, node->alias_field.alias);
        sb_printf(b, ", ");
        sb_cstr(b, node->alias_field.field);
        sb_printf(b, ", &t%d); ", t);
        return t;
    case AST_ARRAY_ACCESS:
        l = gen_expr(b, ntmp, node->array.index1);
        r = node->array.index2 ? gen_expr(b, ntmp, node->array.index2) : -1;
        sb_printf(b, "e = e || ast_eval_elem(c, ");
        sb_cstr(b, node->array.name);
        if (r >= 0) {
            sb_printf(b, ", &t%d, &t%d, &t%d); ", l, r, t);
        } else {
            sb_printf(b, ", &t%d, NULL, &t%d); ", l, t);
        }
        return t;
    case AST_FUNC_CALL:
        a = -1;
        if (node->call.nargs > 0) {
            int args[MAX_FUNC_ARGS];
            for (i = 0; i < node->call.nargs; i++) {
                args[i] = gen_expr(b, ntmp, node->call.args[i]);
            }
            a = (*ntmp)++;
            sb_printf(b, "{ value_t *av = a%d; ", a);
            for (i = 0; i < node->call.nargs; i++) {
                sb_printf(b, "av[%d] = t%d; ", i, args[i]);
            }
            sb_printf(b, "} ");
        }
        sb_printf(b, "e = e || clip_call(c, ");
        sb_cstr(b, node->call.name);
        if (a >= 0) {
            sb_printf(b, ", a%d, %d, &t%d); ", a, node->call.nargs, t);
        } else {
            sb_printf(b, ", NULL, 0, &t%d); ", t);
        }
        return t;
    case AST_UNOP:
        l = gen_expr(b, ntmp, node->unop.child);
        sb_printf(b, "e = e || ast_eval_unop(c, %s, &t%d, &t%d); ", op_name(node->unop.op), l, t);
        return t;
    case AST_BINOP:
        l = gen_expr(b, ntmp, node->binop.left);
        r = gen_expr(b, ntmp, node->binop.right);
        sb_printf(b, "e = e || ast_eval_binop(c, %s, &t%d, &t%d, &t%d); ",
                  op_name(node->binop.op), l, r, t);
        return t;
    }
    return t;
}

/* Temporaries t0..t<n-1> (and argument arrays where calls need them) */
static void sb_decls(sb_t *b, ast_node_t *node, int *ntmp, int *arrays) {
    int t = (*ntmp)++;
    int i;
    (void)t;
    switch (node->type) {
    case AST_ARRAY_ACCESS:
        sb_decls(b, node->array.index1, ntmp, arrays);
        if (node->array.index2) {
            sb_decls(b, node->array.index2, ntmp, arrays);
        }
        break;
    case AST_FUNC_CALL:
        for (i = 0; i < node->call.nargs; i++) {
            sb_decls(b, node->call.args[i], ntmp, arrays);
        }
        if (node->call.nargs > 0) {
            sb_printf(b, "value_t a%d[%d]; ", (*ntmp)++, node->call.nargs);
        }
        break;
    case AST_UNOP:
        sb_decls(b, node->unop.child, ntmp, arrays);
        break;
    case AST_BINOP:
        sb_decls(b, node->binop.left, ntmp, arrays);
        sb_decls(b, node->binop.right, ntmp, arrays);
        break;
    default:
        break;
    }
}

/* The expression text compiled, or NULL: a macro (&) or text the parser
 * does not take whole stays with the run-time evaluator. */
static ast_node_t *parse_expr(const char *text) {
    const char *pp = text;
    const char *err = NULL;
    ast_node_t *n;
    if (strchr(text, '&')) {
        return NULL;
    }
    n = ast_compile_adv(&pp, NULL, &err);
    if (!n) {
        return NULL;
    }
    if (*skip_ws(pp) != '\0') {
        ast_free(n);
        return NULL;
    }
    return n;
}

/* An expression's code in a block: the context, temporaries, code; leaves
 * the value in t<return>. */
static int gen_block(sb_t *b, ast_node_t **nodes, int nnodes, int *results) {
    int ntmp = 0, i, dummy = 0;
    sb_t code = {0};
    sb_printf(b, "expr_ctx_t *c = clip_ctx(); int e = 0; ");
    for (i = 0; i < nnodes; i++) {
        sb_decls(b, nodes[i], &dummy, NULL);
    }
    for (i = 0; i < nnodes; i++) {
        results[i] = gen_expr(&code, &ntmp, nodes[i]);
    }
    if (ntmp > 0) {
        sb_printf(b, "value_t");
        for (i = 0; i < ntmp; i++) {
            sb_printf(b, "%s t%d", i ? "," : "", i);
        }
        sb_printf(b, "; ");
    }
    if (code.buf) {
        sb_printf(b, "%s", code.buf);
        free(code.buf);
    }
    return ntmp;
}

/* name = expr, compiled when expr parses; 0 when it does not */
static int emit_assign(emit_t *e, const char *name, const char *expr) {
    ast_node_t *n = parse_expr(expr);
    sb_t b = {0};
    int r;
    if (!n) {
        return 0;
    }
    gen_block(&b, &n, 1, &r);
    emit_indent(e->out, e->depth);
    fprintf(e->out, "{ %sif (e) clip_fail(c); else clip_setv(", b.buf);
    c_escape(e->out, name);
    fprintf(e->out, ", &t%d); }\n", r);
    free(b.buf);
    ast_free(n);
    return 1;
}

/* ? / ?? a, b, ...: each value printed as it is computed, as clip_q does;
 * 0 when an item does not parse (the whole list goes to clip_q) */
static int emit_print(emit_t *e, const char *list, int newline) {
    ast_node_t *nodes[MAX_FUNC_ARGS];
    int res[MAX_FUNC_ARGS];
    int n = 0, i;
    const char *pp = skip_ws(list);
    sb_t b = {0};

    if (strchr(list, '&')) {
        return 0;
    }
    while (*pp) {
        const char *err = NULL;
        if (n >= MAX_FUNC_ARGS) {
            goto refuse;
        }
        nodes[n] = ast_compile_adv(&pp, NULL, &err);
        if (!nodes[n]) {
            goto refuse;
        }
        n++;
        pp = skip_ws(pp);
        if (*pp == ',') {
            pp = skip_ws(pp + 1);
        } else if (*pp) {
            goto refuse;
        }
    }
    {
        /* Declarations for all items, then each item's code and its output */
        int ntmp = 0, dummy = 0;
        sb_printf(&b, "expr_ctx_t *c = clip_ctx(); int e = 0; ");
        for (i = 0; i < n; i++) {
            sb_decls(&b, nodes[i], &dummy, NULL);
        }
        {
            sb_t code = {0};
            for (i = 0; i < n; i++) {
                res[i] = gen_expr(&code, &ntmp, nodes[i]);
                sb_printf(&code, "if (!e) clip_qv(&t%d, %d); ", res[i], i == 0);
            }
            if (ntmp > 0) {
                sb_printf(&b, "value_t");
                for (i = 0; i < ntmp; i++) {
                    sb_printf(&b, "%s t%d", i ? "," : "", i);
                }
                sb_printf(&b, "; ");
            }
            if (code.buf) {
                sb_printf(&b, "%s", code.buf);
                free(code.buf);
            }
        }
    }
    emit_indent(e->out, e->depth);
    fprintf(e->out, "{ %sif (e) clip_fail(c);%s }\n", b.buf ? b.buf : "",
            newline ? " else clip_qnl();" : "");
    free(b.buf);
    for (i = 0; i < n; i++) {
        ast_free(nodes[i]);
    }
    return 1;
refuse:
    for (i = 0; i < n; i++) {
        ast_free(nodes[i]);
    }
    return 0;
}

/* RETURN expr in a function, compiled when expr parses */
static int emit_return(emit_t *e, const char *expr) {
    ast_node_t *n = parse_expr(expr);
    sb_t b = {0};
    int r;
    if (!n) {
        return 0;
    }
    gen_block(&b, &n, 1, &r);
    emit_indent(e->out, e->depth);
    fprintf(e->out, "{ %sif (e) { clip_fail(c); t%d = val_nil(); } clip_return_v(&scope, &t%d, result); return 0; }\n",
            b.buf, r, r);
    free(b.buf);
    ast_free(n);
    return 1;
}

static int find_assign(const char *p, char *name, int nsz, const char **expr) {
    const char *s;
    int n;
    p = skip_ws(p);
    if (!isalpha((unsigned char)*p) && *p != '_') {
        return 0;
    }
    s = p;
    while (isalnum((unsigned char)*s) || *s == '_') {
        s++;
    }
    n = (int)(s - p);
    if (n <= 0 || n >= nsz) {
        return 0;
    }
    s = skip_ws(s);
    if (*s != '=') {
        return 0;
    }
    if (s[1] == '=') {
        return 0;
    }
    memcpy(name, p, (size_t)n);
    name[n] = '\0';
    *expr = skip_ws(s + 1);
    return 1;
}

static int find_store(const char *p, char *name, int nsz, char *expr, int esz) {
    const char *to;
    const char *q;
    int elen;
    p = skip_ws(p);
    if (!is_kw(p, "STORE")) {
        return 0;
    }
    p = skip_ws(p + 5);
    to = NULL;
    for (q = p; *q; q++) {
        if ((q == p || isspace((unsigned char)q[-1])) &&
            is_kw(q, "TO") && (q[2] == '\0' || isspace((unsigned char)q[2]))) {
            to = q;
        }
    }
    if (!to) {
        return 0;
    }
    elen = (int)(to - p);
    while (elen > 0 && isspace((unsigned char)p[elen - 1])) {
        elen--;
    }
    if (elen <= 0 || elen >= esz) {
        return 0;
    }
    memcpy(expr, p, (size_t)elen);
    expr[elen] = '\0';
    to = skip_ws(to + 2);
    q = to;
    while (isalnum((unsigned char)*q) || *q == '_') {
        q++;
    }
    elen = (int)(q - to);
    if (elen <= 0 || elen >= nsz) {
        return 0;
    }
    memcpy(name, to, (size_t)elen);
    name[elen] = '\0';
    return 1;
}


static int parse_namelist(const char *p, char names[][MAX_NAME], int maxn) {
    int n = 0;
    p = skip_ws(p);
    while (*p && n < maxn) {
        int i = 0;
        if (!isalpha((unsigned char)*p) && *p != '_') {
            break;
        }
        while (isalnum((unsigned char)*p) || *p == '_') {
            if (i < MAX_NAME - 1) {
                names[n][i++] = *p;
            }
            p++;
        }
        names[n][i] = '\0';
        n++;
        p = skip_ws(p);
        if (*p == ',') {
            p = skip_ws(p + 1);
            continue;
        }
        break;
    }
    return n;
}

static int parse_func_header(const char *p, char *name, int nsz,
                             char params[][MAX_NAME], int *nparams) {
    int i = 0;
    *nparams = 0;
    if (is_kw(p, "FUNCTION")) {
        p = skip_ws(p + 8);
    } else if (is_kw(p, "PROCEDURE")) {
        p = skip_ws(p + 9);
    } else {
        return 0;
    }
    if (!isalpha((unsigned char)*p) && *p != '_') {
        return 0;
    }
    while (isalnum((unsigned char)*p) || *p == '_') {
        if (i < nsz - 1) {
            name[i++] = (char)toupper((unsigned char)*p);
        }
        p++;
    }
    name[i] = '\0';
    p = skip_ws(p);
    if (*p == '(') {
        char inner[MAX_LINE];
        int k = 0;
        p++;
        while (*p && *p != ')' && k < MAX_LINE - 1) {
            inner[k++] = *p++;
        }
        inner[k] = '\0';
        *nparams = parse_namelist(inner, params, MAX_PARAMS);
    }
    return name[0] != '\0';
}

static int parse_set_proc(const char *p, char *name, int nsz) {
    const char *q;
    int i = 0;
    if (!is_kw(p, "SET")) {
        return 0;
    }
    q = skip_ws(p + 3);
    if (is_kw(q, "PROCEDURE")) {
        q = skip_ws(q + 9);
    } else if (is_kw(q, "PROC")) {
        q = skip_ws(q + 4);
    } else {
        return 0;
    }
    if (!is_kw(q, "TO")) {
        return 0;
    }
    q = skip_ws(q + 2);
    if (*q == '"' || *q == '\'') {
        char quote = *q++;
        while (*q && *q != quote && i < nsz - 1) {
            name[i++] = *q++;
        }
    } else {
        while (*q && !isspace((unsigned char)*q) && i < nsz - 1) {
            name[i++] = *q++;
        }
    }
    name[i] = '\0';
    if (i >= 4 && icmp_n(name + i - 4, ".PRG", 4) == 0) {
        name[i - 4] = '\0';
    }
    return name[0] != '\0';
}

static int was_inlined(const char *name) {
    int i;
    for (i = 0; i < g_ninlined; i++) {
        if (icmp_n(g_inlined[i], name, MAX_NAME) == 0 &&
            (int)strlen(g_inlined[i]) == (int)strlen(name)) {
            return 1;
        }
    }
    return 0;
}

static void mark_inlined(const char *name) {
    if (g_ninlined >= MAX_FILES || was_inlined(name)) {
        return;
    }
    strncpy(g_inlined[g_ninlined], name, MAX_NAME - 1);
    g_inlined[g_ninlined][MAX_NAME - 1] = '\0';
    g_ninlined++;
}

static int was_compiled_fn(const char *name) {
    int i;
    for (i = 0; i < g_ncompiled_fn; i++) {
        if (icmp_n(g_compiled_fn[i], name, MAX_NAME) == 0 &&
            (int)strlen(g_compiled_fn[i]) == (int)strlen(name)) {
            return 1;
        }
    }
    return 0;
}

static void mark_compiled_fn(const char *name) {
    if (g_ncompiled_fn >= MAX_FUNCS || was_compiled_fn(name)) {
        return;
    }
    strncpy(g_compiled_fn[g_ncompiled_fn], name, MAX_NAME - 1);
    g_compiled_fn[g_ncompiled_fn][MAX_NAME - 1] = '\0';
    g_ncompiled_fn++;
}

static int already_loaded(const char *path) {
    int i;
    for (i = 0; i < g_nloaded; i++) {
        if (strcmp(g_loaded[i], path) == 0) {
            return 1;
        }
    }
    return 0;
}

static void mark_loaded(const char *path) {
    if (g_nloaded >= MAX_FILES || already_loaded(path)) {
        return;
    }
    strncpy(g_loaded[g_nloaded], path, 255);
    g_loaded[g_nloaded][255] = '\0';
    g_nloaded++;
}

static void path_dir(const char *path, char *dir, int n) {
    const char *slash = strrchr(path, '/');
    int len;
    if (!slash) {
        strncpy(dir, ".", (size_t)n - 1);
        dir[n - 1] = '\0';
        return;
    }
    len = (int)(slash - path);
    if (len <= 0) {
        strncpy(dir, "/", (size_t)n - 1);
        dir[n - 1] = '\0';
        return;
    }
    if (len >= n) {
        len = n - 1;
    }
    memcpy(dir, path, (size_t)len);
    dir[len] = '\0';
}

static int store_line(const char *path, const char *text, int lineno, int is_text) {
    if (g_nlines >= MAX_LINES) {
        fprintf(stderr, "%s: too many lines\n", path);
        return -1;
    }
    strncpy(g_lines[g_nlines].text, text, MAX_LINE - 1);
    g_lines[g_nlines].text[MAX_LINE - 1] = '\0';
    strncpy(g_lines[g_nlines].src, path, 255);
    g_lines[g_nlines].src[255] = '\0';
    g_lines[g_nlines].lineno = lineno;
    g_lines[g_nlines].is_text = is_text;
    g_nlines++;
    return 0;
}

/* Lines as the interpreter's prog_load reads them: trailing blanks off, a
 * line ending in ; joined to the next, numbered by logical line. */
static int load_prg(const char *path) {
    FILE *in;
    char buf[MAX_LINE];
    char next[MAX_LINE];
    int lineno = 0;
    int in_text = 0;

    if (already_loaded(path)) {
        return 0;
    }
    in = fopen(path, "r");
    if (!in) {
        perror(path);
        return -1;
    }
    mark_loaded(path);
    while (fgets(buf, sizeof(buf), in)) {
        const char *p;
        int rc;
        rtrim(buf);
        while (buf[0] && buf[strlen(buf) - 1] == ';') {
            buf[strlen(buf) - 1] = '\0';
            if (!fgets(next, sizeof(next), in)) {
                break;
            }
            rtrim(next);
            if (strlen(buf) + strlen(skip_ws(next)) < MAX_LINE - 1) {
                strcat(buf, skip_ws(next));
            }
        }
        lineno++;
        if (in_text) {
            p = skip_ws(buf);
            if (is_kw(p, "ENDTEXT")) {
                in_text = 0;
                rc = store_line(path, "ENDTEXT", lineno, 0);
            } else {
                rc = store_line(path, buf, lineno, 1);
            }
            if (rc != 0) {
                fclose(in);
                return -1;
            }
            continue;
        }
        strip_comment(buf);
        p = skip_ws(buf);
        if (*p == '\0') {
            continue;
        }
        if (store_line(path, p, lineno, 0) != 0) {
            fclose(in);
            return -1;
        }
        if (is_kw(p, "TEXT")) {
            in_text = 1;
        }
    }
    fclose(in);
    return 0;
}

static int try_load_proc(const char *from_src, const char *name) {
    char dir[256];
    char path[512];
    const char *exts[] = {".prg", ".PRG", "", NULL};
    int i;

    path_dir(from_src, dir, (int)sizeof(dir));
    for (i = 0; exts[i]; i++) {
        FILE *fp;
        snprintf(path, sizeof(path), "%s/%s%s", dir, name, exts[i]);
        fp = fopen(path, "r");
        if (!fp) {
            continue;
        }
        fclose(fp);
        if (already_loaded(path)) {
            mark_inlined(name);
            return 0;
        }
        if (load_prg(path) != 0) {
            return -1;
        }
        mark_inlined(name);
        return 0;
    }
    return 0;
}

static int line_in_func(int idx, const func_t *fns, int nfns) {
    int i;
    for (i = 0; i < nfns; i++) {
        if (idx >= fns[i].header && idx < fns[i].end) {
            return 1;
        }
    }
    return 0;
}

static int open_block(emit_t *e, char kind) {
    if (e->depth >= MAX_NEST) {
        fprintf(stderr, "%s:%d: nesting too deep\n", e->inpath, e->lineno);
        return -1;
    }
    e->nest[e->depth++] = kind;
    return 0;
}

static int close_block(emit_t *e, char kind, const char *msg) {
    if (e->depth < 1 || e->nest[e->depth - 1] != kind) {
        fprintf(stderr, "%s:%d: %s\n", e->inpath, e->lineno, msg);
        return -1;
    }
    e->depth--;
    return 0;
}

static int emit_stmt(emit_t *e, const char *p) {
    char name[MAX_NAME];
    char expr[MAX_LINE];
    char names[MAX_PARAMS][MAX_NAME];
    int n, i;

    if (is_kw(p, "PARAMETERS") || is_kw(p, "PARAMETER")) {
        if (!e->in_fn) {
            fprintf(stderr, "%s:%d: PARAMETERS outside FUNCTION\n",
                    e->inpath, e->lineno);
            return -1;
        }
        p = skip_ws(p + (is_kw(p, "PARAMETERS") ? 10 : 9));
        n = parse_namelist(p, names, MAX_PARAMS);
        for (i = 0; i < n; i++) {
            emit_indent(e->out, e->depth);
            fprintf(e->out, "clip_param(&scope, ");
            c_escape(e->out, names[i]);
            fprintf(e->out, ", args, nargs, %d);\n", e->param_i++);
        }
        return 0;
    }
    if (e->in_fn && is_kw(p, "PRIVATE")) {
        p = skip_ws(p + 7);
        n = parse_namelist(p, names, MAX_PARAMS);
        for (i = 0; i < n; i++) {
            emit_indent(e->out, e->depth);
            fprintf(e->out, "clip_private(&scope, ");
            c_escape(e->out, names[i]);
            fprintf(e->out, ");\n");
        }
        return 0;
    }
    if (is_kw(p, "LOCAL")) {
        p = skip_ws(p + 5);
        n = parse_namelist(p, names, MAX_PARAMS);
        for (i = 0; i < n; i++) {
            emit_indent(e->out, e->depth);
            fprintf(e->out, e->in_fn ? "clip_local(&scope, " : "clip_local(NULL, ");
            c_escape(e->out, names[i]);
            fprintf(e->out, ");\n");
        }
        return 0;
    }
    if (is_kw(p, "RETURN")) {
        const char *q = skip_ws(p + 6);
        emit_indent(e->out, e->depth);
        emit_at_expr(e);
        fprintf(e->out, ";\n");
        if (e->in_fn && *q && emit_return(e, q)) {
            return 0;
        }
        if (e->in_fn) {
            emit_indent(e->out, e->depth);
            fprintf(e->out, "clip_return(&scope, ");
            c_escape(e->out, q);
            fprintf(e->out, ", result);\n");
            emit_indent(e->out, e->depth);
            fprintf(e->out, "return 0;\n");
            return 0;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_fini();\n");
        emit_indent(e->out, e->depth);
        fprintf(e->out, "return 0;\n");
        if (e->depth == 0) {
            e->ended = 1;
        }
        return 0;
    }
    if (is_kw(p, "QUIT")) {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_fini();\n");
        emit_indent(e->out, e->depth);
        if (e->in_fn) {
            fprintf(e->out, "exit(0);\n");
        } else {
            fprintf(e->out, "return 0;\n");
            if (e->depth == 0) {
                e->ended = 1;
            }
        }
        return 0;
    }
    if (is_kw(p, "EXIT") || is_kw(p, "LOOP")) {
        int d;
        for (d = e->depth - 1; d >= 0; d--) {
            char k = e->nest[d];
            if (k == 'W' || k == 'F' || k == 'S') {
                break;
            }
            if (k == 'Q') {   /* leaving a BEGIN SEQUENCE body */
                emit_indent(e->out, e->depth);
                fprintf(e->out, "clip_seq_end();\n");
            }
        }
        if (d < 0) {
            fprintf(stderr, "%s:%d: %s outside a loop\n", e->inpath, e->lineno,
                    is_kw(p, "EXIT") ? "EXIT" : "LOOP");
            return -1;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, is_kw(p, "EXIT") ? "break;\n" : "continue;\n");
        return 0;
    }
    if (is_kw(p, "FOR")) {
        if (open_block(e, 'F') != 0) {
            return -1;
        }
        emit_indent(e->out, e->depth - 1);
        fprintf(e->out, "{ clip_for_t clip_f%d; if ((", e->depth - 1);
        emit_at_expr(e);
        fprintf(e->out, ", clip_for_init(&clip_f%d, ", e->depth - 1);
        c_escape(e->out, skip_ws(p + 3));
        fprintf(e->out, "))) do {\n");
        return 0;
    }
    if (is_kw(p, "NEXT")) {
        if (close_block(e, 'F', "NEXT without FOR") != 0) {
            return -1;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, "} while (clip_for_next(&clip_f%d)); }\n", e->depth);
        return 0;
    }
    if (is_kw(p, "SCAN")) {
        if (open_block(e, 'S') != 0) {
            return -1;
        }
        emit_indent(e->out, e->depth - 1);
        fprintf(e->out, "{ prog_scan_t clip_s%d; if ((", e->depth - 1);
        emit_at_expr(e);
        fprintf(e->out, ", prog_scan_open(&clip_s%d, ", e->depth - 1);
        c_escape(e->out, skip_ws(p + 4));
        fprintf(e->out, "))) do {\n");
        return 0;
    }
    if (is_kw(p, "ENDSCAN")) {
        if (close_block(e, 'S', "ENDSCAN without SCAN") != 0) {
            return -1;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, "} while (prog_scan_next(&clip_s%d)); prog_scan_close(&clip_s%d); }\n",
                e->depth, e->depth);
        return 0;
    }
    if (is_kw(p, "TEXT")) {
        if (open_block(e, 'T') != 0) {
            return -1;
        }
        emit_indent(e->out, e->depth - 1);
        fprintf(e->out, "{ /* TEXT */\n");
        return 0;
    }
    if (is_kw(p, "ENDTEXT")) {
        if (close_block(e, 'T', "ENDTEXT without TEXT") != 0) {
            return -1;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, "}\n");
        return 0;
    }
    if (is_kw(p, "BEGIN") && is_kw(skip_ws(p + 5), "SEQUENCE")) {
        if (open_block(e, 'Q') != 0) {
            return -1;
        }
        emit_indent(e->out, e->depth - 1);
        fprintf(e->out, "{ clip_seq_t clip_q%d; if (setjmp(*clip_seq_begin(&clip_q%d)) == 0) {\n",
                e->depth - 1, e->depth - 1);
        return 0;
    }
    if (is_kw(p, "RECOVER")) {
        if (e->depth < 1 || e->nest[e->depth - 1] != 'Q') {
            fprintf(stderr, "%s:%d: RECOVER without BEGIN SEQUENCE\n", e->inpath, e->lineno);
            return -1;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_seq_end();\n");
        emit_indent(e->out, e->depth - 1);
        fprintf(e->out, "} else {\n");
        e->nest[e->depth - 1] = 'R';
        return 0;
    }
    if (is_kw(p, "END") && (*skip_ws(p + 3) == '\0' || is_kw(skip_ws(p + 3), "SEQUENCE"))) {
        if (e->depth < 1 || (e->nest[e->depth - 1] != 'Q' && e->nest[e->depth - 1] != 'R')) {
            fprintf(stderr, "%s:%d: END without BEGIN SEQUENCE\n", e->inpath, e->lineno);
            return -1;
        }
        if (e->nest[e->depth - 1] == 'Q') {
            emit_indent(e->out, e->depth);
            fprintf(e->out, "clip_seq_end();\n");
        }
        e->depth--;
        emit_indent(e->out, e->depth);
        fprintf(e->out, "} }\n");
        return 0;
    }
    if (is_kw(p, "BREAK")) {
        emit_indent(e->out, e->depth);
        emit_at_expr(e);
        fprintf(e->out, ";\n");
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_break();\n");
        return 0;
    }
    if (is_kw(p, "ENDDO")) {
        if (e->depth < 1 || e->nest[e->depth - 1] != 'W') {
            fprintf(stderr, "%s:%d: ENDDO without DO WHILE\n", e->inpath, e->lineno);
            return -1;
        }
        e->depth--;
        emit_indent(e->out, e->depth);
        fprintf(e->out, "}\n");
        return 0;
    }
    if (is_kw(p, "ENDIF")) {
        if (e->depth < 1 || e->nest[e->depth - 1] != 'I') {
            fprintf(stderr, "%s:%d: ENDIF without IF\n", e->inpath, e->lineno);
            return -1;
        }
        e->depth--;
        emit_indent(e->out, e->depth);
        fprintf(e->out, "}\n");
        return 0;
    }
    if (is_kw(p, "ENDCASE")) {
        if (e->depth < 1 ||
            (e->nest[e->depth - 1] != 'C' && e->nest[e->depth - 1] != 'K' &&
             e->nest[e->depth - 1] != 'O')) {
            fprintf(stderr, "%s:%d: ENDCASE without DO CASE\n", e->inpath, e->lineno);
            return -1;
        }
        if (e->nest[e->depth - 1] == 'K' || e->nest[e->depth - 1] == 'O') {
            e->depth--;
            emit_indent(e->out, e->depth);
            fprintf(e->out, "}\n");
        } else {
            e->depth--;
        }
        return 0;
    }
    if (is_kw(p, "ELSEIF") ||
        (is_kw(p, "ELSE") && is_kw(skip_ws(p + 4), "IF"))) {
        const char *q;
        if (e->depth < 1 || e->nest[e->depth - 1] != 'I') {
            fprintf(stderr, "%s:%d: ELSEIF without IF\n", e->inpath, e->lineno);
            return -1;
        }
        if (is_kw(p, "ELSEIF")) {
            q = skip_ws(p + 6);
        } else {
            q = skip_ws(skip_ws(p + 4) + 2);
        }
        emit_cond(e, e->depth - 1, "} else if ", q, " {");
        return 0;
    }
    if (is_kw(p, "ELSE")) {
        if (e->depth < 1 || e->nest[e->depth - 1] != 'I') {
            fprintf(stderr, "%s:%d: ELSE without IF\n", e->inpath, e->lineno);
            return -1;
        }
        emit_indent(e->out, e->depth - 1);
        fprintf(e->out, "} else {\n");
        return 0;
    }
    if (is_kw(p, "CASE")) {
        const char *q = skip_ws(p + 4);
        if (e->depth < 1) {
            fprintf(stderr, "%s:%d: CASE without DO CASE\n", e->inpath, e->lineno);
            return -1;
        }
        if (e->nest[e->depth - 1] == 'C') {
            emit_cond(e, e->depth - 1, "if ", q, " {");
            e->nest[e->depth - 1] = 'K';
            return 0;
        }
        if (e->nest[e->depth - 1] == 'K') {
            emit_cond(e, e->depth - 1, "} else if ", q, " {");
            return 0;
        }
        fprintf(stderr, "%s:%d: CASE after OTHERWISE\n", e->inpath, e->lineno);
        return -1;
    }
    if (is_kw(p, "OTHERWISE")) {
        if (e->depth < 1 || e->nest[e->depth - 1] != 'K') {
            fprintf(stderr, "%s:%d: OTHERWISE without CASE\n", e->inpath, e->lineno);
            return -1;
        }
        emit_indent(e->out, e->depth - 1);
        fprintf(e->out, "} else {\n");
        e->nest[e->depth - 1] = 'O';
        return 0;
    }
    if (is_kw(p, "DO")) {
        const char *q = skip_ws(p + 2);
        if (is_kw(q, "WHILE")) {
            q = skip_ws(q + 5);
            if (e->depth >= MAX_NEST) {
                fprintf(stderr, "%s:%d: nesting too deep\n", e->inpath, e->lineno);
                return -1;
            }
            emit_cond(e, e->depth, "while ", q, " {");
            e->nest[e->depth++] = 'W';
            return 0;
        }
        if (is_kw(q, "CASE")) {
            if (e->depth >= MAX_NEST) {
                fprintf(stderr, "%s:%d: nesting too deep\n", e->inpath, e->lineno);
                return -1;
            }
            e->nest[e->depth++] = 'C';
            return 0;
        }
        emit_indent(e->out, e->depth);
        emit_at_expr(e);
        fprintf(e->out, ";\n");
        /* DO <name> targeting a compiled-in procedure. SET PROCEDURE TO is
         * dropped when its file is inlined, so clip_cmd("DO name") would find
         * neither an on-disk .prg nor a procedure file and raise File-not-
         * found. Inlined procedures live only in the UDF registry, so route
         * the call through clip_do(). WITH-argument marshalling is not done,
         * so an inlined target with WITH is an explicit compile error rather
         * than a silent miscompile. */
        {
            char name[MAX_NAME];
            int i = 0;
            const char *r = q;
            while (*r && (isalnum((unsigned char)*r) || *r == '_') &&
                   i < MAX_NAME - 1) {
                name[i++] = *r++;
            }
            name[i] = '\0';
            r = skip_ws(r);
            /* With WITH arguments, the interpreter's DO parses them (and
             * which are by reference) and its hook calls the compiled code */
            if (name[0] && was_compiled_fn(name) && *r == '\0') {
                emit_indent(e->out, e->depth);
                fprintf(e->out, "clip_do(");
                c_escape(e->out, name);
                fprintf(e->out, ");\n");
                return 0;
            }
        }
        emit_cmd(e->out, e->depth, p);
        return 0;
    }
    if (is_kw(p, "IF")) {
        const char *q = skip_ws(p + 2);
        if (e->depth >= MAX_NEST) {
            fprintf(stderr, "%s:%d: nesting too deep\n", e->inpath, e->lineno);
            return -1;
        }
        emit_cond(e, e->depth, "if ", q, " {");
        e->nest[e->depth++] = 'I';
        return 0;
    }
    if (is_kw(p, "SUSPEND")) {
        fprintf(stderr, "%s:%d: SUSPEND: a compiled program has no interactive mode\n",
                e->inpath, e->lineno);
        return -1;
    }
    /* What remains can fail at run time: say where it is first */
    emit_indent(e->out, e->depth);
    emit_at_expr(e);
    fprintf(e->out, ";\n");

    if (is_kw(p, "SET")) {
        char proc[MAX_NAME];
        if (parse_set_proc(p, proc, MAX_NAME) && was_inlined(proc)) {
            emit_indent(e->out, e->depth);
            fprintf(e->out, "/* SET PROCEDURE TO %s — compiled in */\n", proc);
            return 0;
        }
        emit_cmd(e->out, e->depth, p);
        return 0;
    }
    if (p[0] == '?' && p[1] == '?') {
        if (emit_print(e, skip_ws(p + 2), 0)) {
            return 0;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_q(");
        c_escape(e->out, skip_ws(p + 2));
        fprintf(e->out, ", 0);\n");
        return 0;
    }
    if (p[0] == '?') {
        if (*skip_ws(p + 1) && emit_print(e, skip_ws(p + 1), 1)) {
            return 0;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_q(");
        c_escape(e->out, skip_ws(p + 1));
        fprintf(e->out, ", 1);\n");
        return 0;
    }
    if (find_store(p, name, (int)sizeof(name), expr, (int)sizeof(expr))) {
        if (emit_assign(e, name, expr)) {
            return 0;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_let(");
        c_escape(e->out, name);
        fprintf(e->out, ", ");
        c_escape(e->out, expr);
        fprintf(e->out, ");\n");
        return 0;
    }
    if (find_assign(p, name, (int)sizeof(name), &p)) {
        if (emit_assign(e, name, p)) {
            return 0;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_let(");
        c_escape(e->out, name);
        fprintf(e->out, ", ");
        c_escape(e->out, p);
        fprintf(e->out, ");\n");
        return 0;
    }

    emit_cmd(e->out, e->depth, p);
    return 0;
}

static int emit_line(emit_t *e, const src_line_t *ln) {
    if (ln->is_text) {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_text(");
        c_escape(e->out, ln->text);
        fprintf(e->out, ");\n");
        return 0;
    }
    return emit_stmt(e, ln->text);
}

static int emit_function(FILE *out, const func_t *fn) {
    emit_t e;
    int i;

    memset(&e, 0, sizeof(e));
    e.out = out;
    e.in_fn = 1;
    e.param_i = 0;

    fprintf(out, "static int clip_fn_%s(value_t *args, int nargs, value_t *result) {\n",
            fn->name);
    fprintf(out, "    clip_scope_t scope;\n");
    fprintf(out, "    clip_enter(&scope);\n");
    for (i = 0; i < fn->nparams; i++) {
        fprintf(out, "    clip_param(&scope, ");
        c_escape(out, fn->params[i]);
        fprintf(out, ", args, nargs, %d);\n", e.param_i++);
    }
    for (i = fn->start; i < fn->end; i++) {
        e.inpath = g_lines[i].src;
        e.lineno = g_lines[i].lineno;
        if (emit_line(&e, &g_lines[i]) != 0) {
            return -1;
        }
    }
    if (e.depth != 0) {
        fprintf(stderr, "%s:%d: unclosed block in FUNCTION %s\n",
                g_lines[fn->header].src, g_lines[fn->header].lineno, fn->name);
        return -1;
    }
    fprintf(out, "    clip_end(&scope, result);\n");
    fprintf(out, "    return 0;\n");
    fprintf(out, "}\n\n");
    return 0;
}

int main(int argc, char **argv) {
    const char *outpath = NULL;
    const char *inputs[MAX_FILES];
    int ninputs = 0;
    FILE *out;
    func_t fns[MAX_FUNCS];
    int nfns = 0;
    emit_t e;
    int i, j;
    int scan;

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            outpath = argv[++i];
        } else if (argv[i][0] != '-') {
            if (ninputs >= MAX_FILES) {
                fprintf(stderr, "too many input files\n");
                return 1;
            }
            inputs[ninputs++] = argv[i];
        }
    }
    if (ninputs < 1 || !outpath) {
        fprintf(stderr, "Usage: s32-clip input.prg [more.prg ...] -o output.c\n");
        return 1;
    }

    for (i = 0; i < ninputs; i++) {
        if (load_prg(inputs[i]) != 0) {
            return 1;
        }
    }

    /* Pull in SET PROCEDURE files sitting next to any loaded source. */
    for (scan = 0; scan < g_nlines; scan++) {
        char proc[MAX_NAME];
        if (parse_set_proc(g_lines[scan].text, proc, MAX_NAME)) {
            if (try_load_proc(g_lines[scan].src, proc) != 0) {
                return 1;
            }
        }
    }

    i = 0;
    while (i < g_nlines) {
        if (!g_lines[i].is_text &&
            (is_kw(g_lines[i].text, "FUNCTION") ||
             is_kw(g_lines[i].text, "PROCEDURE"))) {
            if (nfns >= MAX_FUNCS) {
                fprintf(stderr, "%s:%d: too many functions\n",
                        g_lines[i].src, g_lines[i].lineno);
                return 1;
            }
            if (!parse_func_header(g_lines[i].text, fns[nfns].name, MAX_NAME,
                                   fns[nfns].params, &fns[nfns].nparams)) {
                fprintf(stderr, "%s:%d: bad FUNCTION/PROCEDURE header\n",
                        g_lines[i].src, g_lines[i].lineno);
                return 1;
            }
            fns[nfns].header = i;
            fns[nfns].start = i + 1;
            j = i + 1;
            while (j < g_nlines &&
                   (g_lines[j].is_text ||
                    (!is_kw(g_lines[j].text, "FUNCTION") &&
                     !is_kw(g_lines[j].text, "PROCEDURE")))) {
                j++;
            }
            fns[nfns].end = j;
            mark_compiled_fn(fns[nfns].name);
            nfns++;
            i = j;
            continue;
        }
        i++;
    }

    out = fopen(outpath, "w");
    if (!out) {
        perror(outpath);
        return 1;
    }

    fprintf(out, "/* generated by s32-clip from %s */\n", inputs[0]);
    fprintf(out, "#include \"cliprt.h\"\n");
    fprintf(out, "#include <stdlib.h>\n\n");

    for (i = 0; i < nfns; i++) {
        if (emit_function(out, &fns[i]) != 0) {
            fclose(out);
            return 1;
        }
    }

    fprintf(out, "int main(void) {\n");
    fprintf(out, "    clip_init();\n");
    for (i = 0; i < nfns; i++) {
        fprintf(out, "    clip_register_fn(\"%s\", clip_fn_%s);\n",
                fns[i].name, fns[i].name);
    }

    memset(&e, 0, sizeof(e));
    e.out = out;
    for (i = 0; i < g_nlines; i++) {
        if (line_in_func(i, fns, nfns)) {
            continue;
        }
        e.inpath = g_lines[i].src;
        e.lineno = g_lines[i].lineno;
        if (emit_line(&e, &g_lines[i]) != 0) {
            fclose(out);
            return 1;
        }
    }
    if (e.depth != 0) {
        fprintf(stderr, "%s: unclosed block\n", inputs[0]);
        fclose(out);
        return 1;
    }
    if (!e.ended) {
        fprintf(out, "    clip_fini();\n");
        fprintf(out, "    return 0;\n");
    }
    fprintf(out, "}\n");
    fclose(out);
    return 0;
}
