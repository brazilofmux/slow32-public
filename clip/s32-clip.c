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

#include "lex.h"
#include "command.h"
#include "ast.h"   /* dbase's own parser: expressions are parsed here, at compile time */

#define MAX_LINE  512
#define MAX_NEST  32
#define MAX_LINES 4000
#define MAX_FUNCS 64
#define MAX_FILES 64
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
        "OP_AND", "OP_OR", "OP_NOT", "OP_NEGATE", "OP_MOD", "OP_NONE"
    };
    return names[op];
}

/* Declarations an expression's code needs beyond its temporaries (argument
 * arrays), collected as the code is generated */
static sb_t *g_decl;

/* Code blocks: each body a C function, written after everything else and
 * reached through clip_blocks[] (declared at the top of the output) */
static sb_t g_blockfuns;
static int g_nblocks;

static int gen_expr(sb_t *b, int *ntmp, ast_node_t *node);

/* The current value of an assignment target, into t<cur>; r and c are its
 * element indexes (already computed) when it is an element */
static void gen_target_read(sb_t *b, ast_node_t *t, int r, int c, int cur) {
    switch (t->type) {
    case AST_INDEX:
        sb_printf(b, "e = e || ast_eval_index(c, &t%d, &t%d, &t%d); ", r, c, cur);
        break;
    case AST_SEND:
        sb_printf(b, "e = e || ast_eval_send(c, &t%d, ", r);
        sb_cstr(b, t->send.name);
        sb_printf(b, ", NULL, 0, 0, &t%d); ", cur);
        break;
    case AST_MEMVAR:
        sb_printf(b, "e = e || ast_eval_name(c, ");
        sb_cstr(b, t->name);
        sb_printf(b, ", &t%d); ", cur);
        break;
    case AST_FIELD:
        sb_printf(b, "e = e || ast_eval_field(c, ");
        sb_cstr(b, t->name);
        sb_printf(b, ", &t%d); ", cur);
        break;
    case AST_ALIAS_FIELD:
        sb_printf(b, "e = e || ast_eval_alias_field(c, ");
        sb_cstr(b, t->alias_field.alias);
        sb_printf(b, ", ");
        sb_cstr(b, t->alias_field.field);
        sb_printf(b, ", &t%d); ", cur);
        break;
    default: /* AST_ARRAY_ACCESS */
        sb_printf(b, "e = e || ast_eval_elem(c, ");
        sb_cstr(b, t->array.name);
        if (c >= 0) sb_printf(b, ", &t%d, &t%d, &t%d); ", r, c, cur);
        else sb_printf(b, ", &t%d, NULL, &t%d); ", r, cur);
        break;
    }
}

static void gen_target_store(sb_t *b, ast_node_t *t, int r, int c, int val) {
    switch (t->type) {
    case AST_INDEX:
        sb_printf(b, "e = e || ast_store_index(c, &t%d, &t%d, &t%d); ", r, c, val);
        break;
    case AST_SEND:
        sb_printf(b, "e = e || ast_store_send(c, &t%d, ", r);
        sb_cstr(b, t->send.name);
        sb_printf(b, ", &t%d); ", val);
        break;
    case AST_MEMVAR:
        sb_printf(b, "e = e || ast_store_name(c, ");
        sb_cstr(b, t->name);
        sb_printf(b, ", &t%d); ", val);
        break;
    case AST_FIELD:
        sb_printf(b, "e = e || ast_store_alias_field(c, \"FIELD\", ");
        sb_cstr(b, t->name);
        sb_printf(b, ", &t%d); ", val);
        break;
    case AST_ALIAS_FIELD:
        sb_printf(b, "e = e || ast_store_alias_field(c, ");
        sb_cstr(b, t->alias_field.alias);
        sb_printf(b, ", ");
        sb_cstr(b, t->alias_field.field);
        sb_printf(b, ", &t%d); ", val);
        break;
    default:
        sb_printf(b, "e = e || ast_store_elem(c, ");
        sb_cstr(b, t->array.name);
        if (c >= 0) sb_printf(b, ", &t%d, &t%d, &t%d); ", r, c, val);
        else sb_printf(b, ", &t%d, NULL, &t%d); ", r, val);
        break;
    }
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
            sb_printf(b, "t%d = val_num_dec(%.17g, %d, %d); ", t, node->literal.num,
                      node->literal.ndec, node->literal.nwidth);
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
    case AST_REF:
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
        if (node->call.nargs == 3 &&
            (strcasecmp(node->call.name, "IIF") == 0 || strcasecmp(node->call.name, "IF") == 0)) {
            /* one branch only, as ast.c evaluates it */
            l = gen_expr(b, ntmp, node->call.args[0]);
            sb_printf(b, "if (!e) { if ((t%d.type == VAL_LOGIC && t%d.logic) || (t%d.type == VAL_NUM && t%d.num != 0.0)) { ",
                      l, l, l, l);
            r = gen_expr(b, ntmp, node->call.args[1]);
            sb_printf(b, "if (!e) t%d = t%d; } else { ", t, r);
            r = gen_expr(b, ntmp, node->call.args[2]);
            sb_printf(b, "if (!e) t%d = t%d; } } ", t, r);
            return t;
        }
        a = -1;
        if (node->call.nargs > 0) {
            int args[MAX_FUNC_ARGS];
            for (i = 0; i < node->call.nargs; i++) {
                args[i] = gen_expr(b, ntmp, node->call.args[i]);
            }
            a = (*ntmp)++;
            sb_printf(g_decl, "value_t a%d[%d]; ", a, node->call.nargs);
            sb_printf(b, "{ value_t *av = a%d; ", a);
            for (i = 0; i < node->call.nargs; i++) {
                sb_printf(b, "av[%d] = t%d; ", i, args[i]);
            }
            sb_printf(b, "} ");
        }
        for (i = 0; i < node->call.nargs && node->call.args[i]->type != AST_REF; i++) {}
        if (i < node->call.nargs) {   /* f(@x, ...): by reference */
            int k = (*ntmp)++;
            sb_printf(b, "{ static const char *const rf%d[] = {", k);
            for (i = 0; i < node->call.nargs; i++) {
                sb_printf(b, "%s", i ? ", " : "");
                if (node->call.args[i]->type == AST_REF) sb_cstr(b, node->call.args[i]->name);
                else sb_printf(b, "0");
            }
            sb_printf(b, "}; e = e || func_call_refs(c, ");
            sb_cstr(b, node->call.name);
            sb_printf(b, ", a%d, %d, rf%d, &t%d); } ", a, node->call.nargs, k, t);
            return t;
        }
        sb_printf(b, "{ static clip_site_t site; e = e || clip_call(c, ");
        sb_cstr(b, node->call.name);
        if (a >= 0) {
            sb_printf(b, ", a%d, %d, &t%d, &site); } ", a, node->call.nargs, t);
        } else {
            sb_printf(b, ", NULL, 0, &t%d, &site); } ", t);
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
    case AST_BLOCK: {
        int k = g_nblocks++, i2, ntmp2 = 0, last = -1;
        sb_t code = {0}, decl = {0};
        sb_t *outer_decl = g_decl;
        g_decl = &decl;
        for (i2 = 0; i2 < node->block.n; i2++) {
            last = gen_expr(&code, &ntmp2, node->block.exprs[i2]);
        }
        g_decl = outer_decl;
        sb_printf(&g_blockfuns, "static int clip_blk_%d(expr_ctx_t *c, value_t *result) {\n    int e = 0; %s", k,
                  decl.buf ? decl.buf : "");
        if (ntmp2 > 0) {
            sb_printf(&g_blockfuns, "value_t");
            for (i2 = 0; i2 < ntmp2; i2++) sb_printf(&g_blockfuns, "%s t%d", i2 ? "," : "", i2);
            sb_printf(&g_blockfuns, ";");
        }
        sb_printf(&g_blockfuns, "\n    %s\n    if (!e) *result = t%d;\n    return e ? -1 : 0;\n}\n\n",
                  code.buf ? code.buf : "", last);
        free(code.buf);
        free(decl.buf);
        {
            char frees[MAX_BLOCK_CAPTURES][MEMVAR_NAMELEN];
            int nf = ast_block_free_names(node, frees);
            sb_printf(b, "{ static const char *const bp%d[] = {", k);
            for (i2 = 0; i2 < node->block.nparams; i2++) {
                sb_printf(b, "%s", i2 ? ", " : "");
                sb_cstr(b, node->block.params[i2]);
            }
            sb_printf(b, "%s0}; static const char *const bf%d[] = {", node->block.nparams ? ", " : "", k);
            for (i2 = 0; i2 < nf; i2++) {
                sb_printf(b, "%s", i2 ? ", " : "");
                sb_cstr(b, frees[i2]);
            }
            sb_printf(b, "%s0}; e = e || ast_make_block(c, clip_blocks[%d], %d, bp%d, %d, bf%d, &t%d); } ",
                      nf ? ", " : "", k, node->block.nparams, k, nf, k, t);
        }
        return t;
    }
    case AST_ARRAY_LIT:
        if (node->list.n > 0) {
            int vals[4096];
            int k;
            for (k = 0; k < node->list.n && k < 4096; k++) {
                vals[k] = gen_expr(b, ntmp, node->list.elems[k]);
            }
            a = (*ntmp)++;
            sb_printf(g_decl, "value_t a%d[%d]; ", a, node->list.n);
            sb_printf(b, "{ value_t *av = a%d; ", a);
            for (k = 0; k < node->list.n; k++) {
                sb_printf(b, "av[%d] = t%d; ", k, vals[k]);
            }
            sb_printf(b, "} e = e || ast_make_array(c, a%d, %d, &t%d); ", a, node->list.n, t);
        } else {
            sb_printf(b, "e = e || ast_make_array(c, NULL, 0, &t%d); ", t);
        }
        return t;
    case AST_INDEX:
        l = gen_expr(b, ntmp, node->index.base);
        r = gen_expr(b, ntmp, node->index.index);
        sb_printf(b, "e = e || ast_eval_index(c, &t%d, &t%d, &t%d); ", l, r, t);
        return t;
    case AST_SEND:
        l = gen_expr(b, ntmp, node->send.base);
        a = -1;
        if (node->send.nargs > 0) {
            int args[MAX_FUNC_ARGS];
            for (i = 0; i < node->send.nargs; i++) {
                args[i] = gen_expr(b, ntmp, node->send.args[i]);
            }
            a = (*ntmp)++;
            sb_printf(g_decl, "value_t a%d[%d]; ", a, node->send.nargs);
            sb_printf(b, "{ value_t *av = a%d; ", a);
            for (i = 0; i < node->send.nargs; i++) {
                sb_printf(b, "av[%d] = t%d; ", i, args[i]);
            }
            sb_printf(b, "} ");
        }
        sb_printf(b, "e = e || ast_eval_send(c, &t%d, ", l);
        sb_cstr(b, node->send.name);
        if (a >= 0) sb_printf(b, ", a%d, %d, %d, &t%d); ", a, node->send.nargs, node->send.is_call, t);
        else sb_printf(b, ", NULL, 0, %d, &t%d); ", node->send.is_call, t);
        return t;
    case AST_ASSIGN:
    case AST_INCDEC: {
        /* as ast.c's eval_store: indexes once, then the current value
         * (compound and ++/--), the right side, the operation, the store */
        ast_node_t *tg = node->type == AST_ASSIGN ? node->assign.target : node->incdec.target;
        int ri = -1, ci = -1, cur = -1, nv;
        if (tg->type == AST_ARRAY_ACCESS) {
            ri = gen_expr(b, ntmp, tg->array.index1);
            if (tg->array.index2) ci = gen_expr(b, ntmp, tg->array.index2);
        } else if (tg->type == AST_INDEX) {   /* ri: the array, ci: the index */
            ri = gen_expr(b, ntmp, tg->index.base);
            ci = gen_expr(b, ntmp, tg->index.index);
        } else if (tg->type == AST_SEND) {   /* ri: the object */
            ri = gen_expr(b, ntmp, tg->send.base);
        }
        if (node->type == AST_ASSIGN && node->assign.op == OP_NONE) {
            nv = gen_expr(b, ntmp, node->assign.value);
        } else {
            cur = (*ntmp)++;
            gen_target_read(b, tg, ri, ci, cur);
            nv = (*ntmp)++;
            if (node->type == AST_ASSIGN) {
                int rhs = gen_expr(b, ntmp, node->assign.value);
                sb_printf(b, "e = e || ast_eval_binop(c, %s, &t%d, &t%d, &t%d); ",
                          op_name(node->assign.op), cur, rhs, nv);
            } else {
                int one = (*ntmp)++;
                sb_printf(b, "t%d = val_num_dec(1, 0, 0); e = e || ast_eval_binop(c, %s, &t%d, &t%d, &t%d); ",
                          one, node->incdec.delta > 0 ? "OP_ADD" : "OP_SUB", cur, one, nv);
            }
        }
        gen_target_store(b, tg, ri, ci, nv);
        sb_printf(b, "if (!e) t%d = t%d; ", t,
                  (node->type == AST_INCDEC && !node->incdec.prefix) ? cur : nv);
        return t;
    }
    }
    return t;
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
    int ntmp = 0, i;
    sb_t code = {0}, decl = {0};
    g_decl = &decl;
    sb_printf(b, "expr_ctx_t *c = clip_ctx(); int e = 0; ");
    for (i = 0; i < nnodes; i++) {
        results[i] = gen_expr(&code, &ntmp, nodes[i]);
    }
    if (decl.buf) {
        sb_printf(b, "%s", decl.buf);
        free(decl.buf);
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
        int ntmp = 0;
        sb_printf(&b, "expr_ctx_t *c = clip_ctx(); int e = 0; ");
        {
            sb_t code = {0}, decl = {0};
            g_decl = &decl;
            /* QOUT(): every argument first, then the new line and the output */
            for (i = 0; i < n; i++) {
                res[i] = gen_expr(&code, &ntmp, nodes[i]);
            }
            sb_printf(&code, "if (!e) { %s", newline ? "clip_qnl(); " : "");
            for (i = 0; i < n; i++) {
                sb_printf(&code, "clip_qv(&t%d, %d); ", res[i], i == 0);
            }
            sb_printf(&code, "} ");
            if (decl.buf) {
                sb_printf(&b, "%s", decl.buf);
                free(decl.buf);
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
    fprintf(e->out, "{ %sif (e) clip_fail(c); }\n", b.buf ? b.buf : "");
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

/* The argument text of a command as its handler would get it (after the
 * keyword), or NULL when it holds a macro */
static const char *cmd_arg(const char *p, int kwlen) {
    const char *a = skip_ws(p + kwlen);
    return strchr(a, '&') ? NULL : a;
}

/* REPLACE f WITH e [, f WITH e ...] with no scope, FOR or WHILE */
static int emit_replace(emit_t *e, const char *args) {
    char fields[MAX_FUNC_ARGS][MAX_NAME];
    ast_node_t *nodes[MAX_FUNC_ARGS];
    int n = 0, i, ntmp = 0;
    const char *pp = args;
    sb_t b = {0}, code = {0};

    while (*pp) {
        const char *err = NULL;
        int k = 0;
        if (n >= MAX_FUNC_ARGS || (!isalpha((unsigned char)*pp) && *pp != '_')) {
            goto refuse;
        }
        while ((isalnum((unsigned char)*pp) || *pp == '_') && k < MAX_NAME - 1) {
            fields[n][k++] = *pp++;
        }
        fields[n][k] = '\0';
        pp = skip_ws(pp);
        if (!is_kw(pp, "WITH")) {
            goto refuse;
        }
        pp = skip_ws(pp + 4);
        nodes[n] = ast_compile_adv(&pp, NULL, &err);
        if (!nodes[n]) {
            goto refuse;
        }
        n++;
        pp = skip_ws(pp);
        if (*pp == ',') {
            pp = skip_ws(pp + 1);
        } else if (*pp) {
            goto refuse;   /* a scope, FOR, WHILE: the interpreter's REPLACE */
        }
    }
    if (n == 0) {
        return 0;
    }
    for (i = 0; i < n; i++) {
        if (lex_is_reserved(fields[i])) {
            goto refuse;
        }
    }
    sb_printf(&b, "{ static const char *const rf[] = {");
    for (i = 0; i < n; i++) {
        sb_printf(&b, "%s", i ? ", " : "");
        sb_cstr(&b, fields[i]);
    }
    sb_printf(&b, "}; cmd_replace_state_t *rs = cmd_api_replace_begin(rf, %d); if (rs) { "
                  "expr_ctx_t *c = clip_ctx(); int e = 0; ", n);
    {
        sb_t decl = {0};
        g_decl = &decl;
        for (i = 0; i < n; i++) {
            int r = gen_expr(&code, &ntmp, nodes[i]);
            sb_printf(&code, "if (!e) cmd_api_replace_set(rs, %d, &t%d); ", i, r);
        }
        if (decl.buf) {
            sb_printf(&b, "%s", decl.buf);
            free(decl.buf);
        }
    }
    sb_printf(&b, "value_t");
    for (i = 0; i < ntmp; i++) {
        sb_printf(&b, "%s t%d", i ? "," : "", i);
    }
    sb_printf(&b, "; %s", code.buf ? code.buf : "");
    sb_printf(&b, "if (e) { clip_fail(c); cmd_api_replace_abort(rs); } else cmd_api_replace_end(rs); } }");
    emit_indent(e->out, e->depth);
    fprintf(e->out, "%s\n", b.buf);
    free(b.buf);
    free(code.buf);
    for (i = 0; i < n; i++) {
        ast_free(nodes[i]);
    }
    return 1;
refuse:
    for (i = 0; i < n; i++) {
        ast_free(nodes[i]);
    }
    free(code.buf);
    return 0;
}

/* Clipper 5: an expression as a statement -- x := e, x += e, x++, ++x,
 * f(args) (a name that is no command), M->x := e; 0 when the line is not */
static int emit_expr_stmt(emit_t *e, const char *p) {
    const char *q = p;
    char word[MAX_NAME];
    int k = 0;
    ast_node_t *n;
    sb_t b = {0};
    int r;

    if (!((q[0] == '+' && q[1] == '+') || (q[0] == '-' && q[1] == '-'))) {
        if (!isalpha((unsigned char)*q) && *q != '_') {
            return 0;
        }
        while ((isalnum((unsigned char)*q) || *q == '_') && k < MAX_NAME - 1) {
            word[k++] = *q++;
        }
        word[k] = '\0';
        q = skip_ws(q);
        if (*q == '(') {
            if (cmd_is_command_word(word)) {
                return 0;
            }
        } else if (!(q[0] == ':' && q[1] == '=') && !(q[1] == '=' && strchr("+-*/%^", q[0])) &&
                   !(q[0] == '+' && q[1] == '+') && !(q[0] == '-' && q[1] == '-') &&
                   !(q[0] == '-' && q[1] == '>') && q[0] != '[' &&
                   !(q[0] == ':' && q[1] != '=')) {   /* o:ivar := e, o:method() */
            return 0;
        }
    }
    n = parse_expr(p);
    if (!n) {
        return 0;
    }
    if (n->type != AST_ASSIGN && n->type != AST_INCDEC && n->type != AST_FUNC_CALL &&
        !(n->type == AST_SEND && n->send.is_call)) {
        ast_free(n);
        return 0;
    }
    gen_block(&b, &n, 1, &r);
    emit_indent(e->out, e->depth);
    fprintf(e->out, "{ %sif (e) clip_fail(c); }\n", b.buf);
    free(b.buf);
    ast_free(n);
    return 1;
}

/* LOCAL / PRIVATE / PUBLIC name [:= value] [, ...]: each declared, then
 * assigned its value; 0 leaves the statement as it was (an array
 * dimension, say) */
static int emit_decl(emit_t *e, const char *kw, const char *list) {
    char items[MAX_PARAMS][MAX_LINE];
    int n = 0, depth = 0, i, k = 0;
    char quote = 0;
    const char *p;

    for (p = list; ; p++) {   /* split at the top-level commas */
        if (*p == '\0' || (*p == ',' && depth == 0 && !quote)) {
            if (n >= MAX_PARAMS) {
                return 0;
            }
            items[n][k] = '\0';
            n++;
            k = 0;
            if (*p == '\0') {
                break;
            }
            continue;
        }
        if (quote) {
            if (*p == quote) quote = 0;
        } else if (*p == '"' || *p == '\'') {
            quote = *p;
        } else if (*p == '(' || *p == '[' || *p == '{') {
            depth++;
        } else if (*p == ')' || *p == ']' || *p == '}') {
            depth--;
        }
        if (k < MAX_LINE - 1) {
            items[n][k++] = *p;
        }
    }
    for (i = 0; i < n; i++) {   /* names, name[dims], := initializers */
        const char *q = skip_ws(items[i]);
        if (!isalpha((unsigned char)*q) && *q != '_') {
            return 0;
        }
        while (isalnum((unsigned char)*q) || *q == '_') q++;
        q = skip_ws(q);
        if (*q && !(q[0] == ':' && q[1] == '=') && q[0] != '[') {
            return 0;
        }
    }
    for (i = 0; i < n; i++) {
        char name[MAX_NAME];
        const char *q = skip_ws(items[i]);
        int m = 0;
        while ((isalnum((unsigned char)*q) || *q == '_') && m < MAX_NAME - 1) {
            name[m++] = *q++;
        }
        name[m] = '\0';
        q = skip_ws(q);
        if (q[0] == '[') {   /* name[d1, d2]: name := ARRAY(d1, d2) */
            const char *close = strrchr(q, ']');
            char *r;
            if (!close) {
                return 0;
            }
            {
                char tmp[MAX_LINE];
                snprintf(tmp, sizeof(tmp), ":= ARRAY(%.*s)", (int)(close - q - 1), q + 1);
                r = items[i] + (q - items[i]);
                snprintf(r, (size_t)(MAX_LINE - (r - items[i])), "%s", tmp);
                q = r;
            }
        }
        emit_indent(e->out, e->depth);
        if (!strcmp(kw, "LOCAL")) {
            fprintf(e->out, e->in_fn ? "clip_local(&scope, " : "clip_local(NULL, ");
            c_escape(e->out, name);
            fprintf(e->out, ");\n");
        } else if (!strcmp(kw, "PRIVATE") && e->in_fn) {
            fprintf(e->out, "clip_private(&scope, ");
            c_escape(e->out, name);
            fprintf(e->out, ");\n");
        } else {
            char line[MAX_LINE];
            snprintf(line, sizeof(line), "%s %s", kw, name);
            fprintf(e->out, "clip_cmd(");
            c_escape(e->out, line);
            fprintf(e->out, ");\n");
        }
        if (q[0] == ':') {
            char asg[MAX_LINE + MAX_NAME];
            snprintf(asg, sizeof(asg), "%s %s", name, q);
            if (!emit_expr_stmt(e, asg)) {
                fprintf(stderr, "%s:%d: initializer of %s does not compile\n", e->inpath, e->lineno, name);
                return -1;
            }
        }
    }
    return 1;
}

/* STATIC name [:= value] [, ...]: C static storage per variable,
 * initialized the first time through, bound as a LOCAL of each call */
static int g_nstatic;

static int emit_static(emit_t *e, const char *list) {
    const char *p = list;
    while (*(p = skip_ws(p))) {
        char name[MAX_NAME];
        int m = 0, k = g_nstatic++;
        while ((isalnum((unsigned char)*p) || *p == '_') && m < MAX_NAME - 1) {
            name[m++] = *p++;
        }
        name[m] = '\0';
        if (!name[0]) {
            fprintf(stderr, "%s:%d: STATIC: a name expected\n", e->inpath, e->lineno);
            return -1;
        }
        p = skip_ws(p);
        emit_indent(e->out, e->depth);
        fprintf(e->out, "{ static value_t sv%d; static int si%d; if (!si%d) { si%d = 1; sv%d = val_nil();", k, k, k, k, k);
        if (p[0] == ':' && p[1] == '=') {
            /* the initializer: up to the next top-level comma */
            const char *q = p + 2;
            int depth = 0;
            char quote = 0, init[MAX_LINE];
            ast_node_t *n;
            sb_t b = {0};
            int r;
            while (*q && (depth > 0 || quote || *q != ',')) {
                if (quote) { if (*q == quote) quote = 0; }
                else if (*q == '"' || *q == '\'') quote = *q;
                else if (*q == '(' || *q == '[' || *q == '{') depth++;
                else if (*q == ')' || *q == ']' || *q == '}') depth--;
                q++;
            }
            snprintf(init, sizeof(init), "%.*s", (int)(q - (p + 2)), p + 2);
            n = parse_expr(init);
            if (!n) {
                fprintf(stderr, "%s:%d: STATIC %s: the initializer does not compile\n", e->inpath, e->lineno, name);
                return -1;
            }
            gen_block(&b, &n, 1, &r);
            fprintf(e->out, " { %sif (e) clip_fail(c); else sv%d = t%d; }", b.buf, k, r);
            free(b.buf);
            ast_free(n);
            p = q;
        }
        fprintf(e->out, " } clip_static_bind(%s, ", e->in_fn ? "&scope" : "NULL");
        c_escape(e->out, name);
        fprintf(e->out, ", &sv%d); }\n", k);
        p = skip_ws(p);
        if (*p == ',') {
            p++;
        }
    }
    return 0;
}

/* Commands compiled to direct calls; 0 leaves the line to clip_cmd */
static int emit_command(emit_t *e, const char *p) {
    const char *a;
    if (is_kw(p, "SELECT") && (a = cmd_arg(p, 6)) && !strchr(a, '(')) {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "cmd_api_select(");
        c_escape(e->out, a);
        fprintf(e->out, ");\n");
        return 1;
    }
    if (is_kw(p, "SKIP") && (a = cmd_arg(p, 4))) {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "cmd_api_skip(");
        c_escape(e->out, a);
        fprintf(e->out, ");\n");
        return 1;
    }
    if (is_kw(p, "GO") && (a = cmd_arg(p, 2))) {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "cmd_api_go(");
        c_escape(e->out, a);
        fprintf(e->out, ");\n");
        return 1;
    }
    if (is_kw(p, "APPEND") && is_kw(skip_ws(p + 6), "BLANK") && *skip_ws(skip_ws(p + 6) + 5) == '\0') {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "cmd_api_append_blank();\n");
        return 1;
    }
    if (is_kw(p, "SEEK") && (a = cmd_arg(p, 4)) && *a) {
        ast_node_t *n = parse_expr(a);
        sb_t b = {0};
        int r;
        if (!n) {
            return 0;
        }
        gen_block(&b, &n, 1, &r);
        emit_indent(e->out, e->depth);
        fprintf(e->out, "if (cmd_api_seek_ready()) { %sif (e) clip_fail(c); else cmd_api_seek_value(&t%d); }\n",
                b.buf, r);
        free(b.buf);
        ast_free(n);
        return 1;
    }
    if (is_kw(p, "REPLACE") && (a = cmd_arg(p, 7)) && *a) {
        return emit_replace(e, a);
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
    if (*skip_ws(q) == '[' || *skip_ws(q) == '(') {
        return 0;   /* an element: STORE's own code assigns it */
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

/* ---- The preprocessor (Clipper's, the part programs lean on):
 * #define (constants, case-sensitive, and NAME(a, b) pseudofunctions),
 * #undef, #ifdef / #ifndef / #else / #endif, #include, #error, #stdout,
 * #command / #xcommand (a statement's shape: literal words and <name>
 * markers that take the text up to the next literal) and #translate /
 * #xtranslate (the same anywhere in a line).  Lines are expanded as they
 * are loaded, until nothing changes. ---- */

#define PP_MAX_DEFS 512
#define PP_MAX_RULES 256
typedef struct {
    char name[64];
    int nparams;          /* -1: a constant, else a pseudofunction */
    char params[8][32];
    char body[MAX_LINE];
} pp_define_t;
typedef struct {
    int anywhere;         /* #translate: not only at the start of a line */
    char pattern[MAX_LINE];
    char result[MAX_LINE];
} pp_rule_t;
static pp_define_t g_defs[PP_MAX_DEFS];
static int g_ndefs;
static pp_rule_t g_rules[PP_MAX_RULES];
static int g_nrules;

static int load_prg(const char *path);

static int pp_find_def(const char *name, int n) {
    int i;
    for (i = g_ndefs - 1; i >= 0; i--) {
        if ((int)strlen(g_defs[i].name) == n && strncmp(g_defs[i].name, name, (size_t)n) == 0) {
            return i;
        }
    }
    return -1;
}

static const char *pp_word(const char *p, char *w, int n) {
    int i = 0;
    p = skip_ws(p);
    while ((isalnum((unsigned char)*p) || *p == '_') && i < n - 1) {
        w[i++] = *p++;
    }
    w[i] = '\0';
    return p;
}

/* #define NAME body / NAME(a, b) body */
static int pp_define(const char *p) {
    pp_define_t *d;
    if (g_ndefs >= PP_MAX_DEFS) {
        return -1;
    }
    d = &g_defs[g_ndefs];
    memset(d, 0, sizeof(*d));
    p = pp_word(p, d->name, (int)sizeof(d->name));
    if (!d->name[0]) {
        return -1;
    }
    d->nparams = -1;
    if (*p == '(') {   /* no space before (: a pseudofunction */
        d->nparams = 0;
        p++;
        while (*(p = skip_ws(p)) && *p != ')') {
            if (d->nparams >= 8) {
                return -1;
            }
            p = pp_word(p, d->params[d->nparams++], 32);
            p = skip_ws(p);
            if (*p == ',') p++;
        }
        if (*p == ')') p++;
    }
    snprintf(d->body, sizeof(d->body), "%s", skip_ws(p));
    rtrim(d->body);
    g_ndefs++;
    return 0;
}

/* Identifiers named by #define, replaced outside strings; 1 if anything was */
static int pp_apply_defines(char *line) {
    char out[MAX_LINE];
    const char *p = line;
    int o = 0, changed = 0;
    char quote = 0;
    while (*p && o < MAX_LINE - 1) {
        if (quote) {
            if (*p == quote) quote = 0;
            out[o++] = *p++;
            continue;
        }
        if (*p == '"' || *p == '\'') {
            quote = *p;
            out[o++] = *p++;
            continue;
        }
        if ((isalpha((unsigned char)*p) || *p == '_') && (p == line || !(isalnum((unsigned char)p[-1]) || p[-1] == '_'))) {
            const char *s = p;
            int d;
            while (isalnum((unsigned char)*p) || *p == '_') p++;
            d = pp_find_def(s, (int)(p - s));
            if (d >= 0 && g_defs[d].nparams < 0) {
                o += snprintf(out + o, (size_t)(MAX_LINE - o), "%s", g_defs[d].body);
                changed = 1;
            } else if (d >= 0 && *skip_ws(p) == '(') {
                /* a pseudofunction: its arguments, top-level commas */
                char args[8][MAX_LINE];
                int na = 0, depth = 0, k = 0;
                const char *q = skip_ws(p) + 1, *b;
                char aq = 0;
                memset(args, 0, sizeof(args));
                while (*q && (depth > 0 || aq || *q != ')')) {
                    if (aq) { if (*q == aq) aq = 0; }
                    else if (*q == '"' || *q == '\'') aq = *q;
                    else if (*q == '(' || *q == '[' || *q == '{') depth++;
                    else if (*q == ')' || *q == ']' || *q == '}') depth--;
                    if (!aq && depth == 0 && *q == ',') {
                        na++;
                        k = 0;
                        q++;
                        continue;
                    }
                    if (na < 8 && k < MAX_LINE - 1) args[na][k++] = *q;
                    q++;
                }
                if (*q == ')') q++;
                if (k > 0 || na > 0) na++;
                for (b = g_defs[d].body; *b && o < MAX_LINE - 1;) {
                    if (isalpha((unsigned char)*b) || *b == '_') {
                        const char *w = b;
                        int pi, done = 0;
                        while (isalnum((unsigned char)*b) || *b == '_') b++;
                        for (pi = 0; pi < g_defs[d].nparams; pi++) {
                            if ((int)strlen(g_defs[d].params[pi]) == (int)(b - w) &&
                                !strncmp(g_defs[d].params[pi], w, (size_t)(b - w))) {
                                char a[MAX_LINE];
                                snprintf(a, sizeof(a), "%s", skip_ws(pi < na ? args[pi] : ""));
                                rtrim(a);
                                o += snprintf(out + o, (size_t)(MAX_LINE - o), "%s", a);
                                done = 1;
                                break;
                            }
                        }
                        if (!done) o += snprintf(out + o, (size_t)(MAX_LINE - o), "%.*s", (int)(b - w), w);
                    } else {
                        out[o++] = *b++;
                    }
                }
                p = q;
                changed = 1;
            } else {
                o += snprintf(out + o, (size_t)(MAX_LINE - o), "%.*s", (int)(p - s), s);
            }
            continue;
        }
        out[o++] = *p++;
    }
    out[o < MAX_LINE ? o : MAX_LINE - 1] = '\0';
    if (changed) strcpy(line, out);
    return changed;
}

/* A #command / #translate pattern against text: literal words (any case),
 * other characters as they are, <name> markers taking the text up to the
 * next literal (or the end).  The matched markers' text into vals. */
#define PP_MAX_MARKERS 16
static int pp_match(const char *pat, const char *text, char names[][32], char vals[][MAX_LINE],
                    int *nm, const char **end) {
    const char *p = skip_ws(pat), *t = skip_ws(text);
    *nm = 0;
    while (*p) {
        if (*p == '<') {   /* a marker */
            const char *close = strchr(p, '>');
            const char *lit;
            char litw[64];
            int li = 0, depth = 0;
            char q = 0;
            const char *v = t;
            if (!close || *nm >= PP_MAX_MARKERS) return 0;
            snprintf(names[*nm], 32, "%.*s", (int)(close - p - 1), p + 1);
            p = skip_ws(close + 1);
            lit = p;   /* the next literal, if any */
            while ((isalnum((unsigned char)*lit) || *lit == '_') && li < 63) litw[li++] = *lit++;
            litw[li] = '\0';
            if (!li && *p && *p != '<') { litw[0] = *p; litw[1] = '\0'; li = 1; }
            while (*t) {
                if (!q && depth == 0 && li) {
                    if (isalpha((unsigned char)litw[0])) {
                        if ((t == v || !(isalnum((unsigned char)t[-1]) || t[-1] == '_')) &&
                            !strncasecmp(t, litw, (size_t)li) && !(isalnum((unsigned char)t[li]) || t[li] == '_'))
                            break;
                    } else if (*t == litw[0]) {
                        break;
                    }
                }
                if (q) { if (*t == q) q = 0; }
                else if (*t == '"' || *t == '\'') q = *t;
                else if (*t == '(' || *t == '[' || *t == '{') depth++;
                else if (*t == ')' || *t == ']' || *t == '}') depth--;
                t++;
            }
            snprintf(vals[*nm], MAX_LINE, "%.*s", (int)(t - v), v);
            rtrim(vals[*nm]);
            if (!vals[*nm][0]) return 0;
            (*nm)++;
            continue;
        }
        if (isalpha((unsigned char)*p) || *p == '_') {   /* a literal word */
            const char *w = p;
            int n;
            while (isalnum((unsigned char)*p) || *p == '_') p++;
            n = (int)(p - w);
            if (strncasecmp(t, w, (size_t)n) != 0 || isalnum((unsigned char)t[n]) || t[n] == '_') return 0;
            t = skip_ws(t + n);
            p = skip_ws(p);
            continue;
        }
        if (*t != *p) return 0;   /* punctuation */
        t = skip_ws(t + 1);
        p = skip_ws(p + 1);
    }
    *end = t;
    return 1;
}

static void pp_result(const char *res, char names[][32], char vals[][MAX_LINE], int nm, char *out, int size) {
    int o = 0;
    while (*res && o < size - 1) {
        int stringify = 0, i;
        const char *r = res;
        if (r[0] == '<' && r[1] == '"') { stringify = 1; r += 2; }
        else if (r[0] == '#' && r[1] == '<') { stringify = 1; r += 2; }
        else if (r[0] == '<') { r += 1; }
        else { out[o++] = *res++; continue; }
        for (i = 0; i < nm; i++) {
            int n = (int)strlen(names[i]);
            if (!strncmp(r, names[i], (size_t)n) && (r[n] == '>' || (stringify && r[n] == '"' && r[n + 1] == '>'))) {
                o += snprintf(out + o, (size_t)(size - o), stringify ? "\"%s\"" : "%s", vals[i]);
                res = r + n + (r[n] == '"' ? 2 : 1);
                break;
            }
        }
        if (i == nm) out[o++] = *res++;
    }
    out[o < size ? o : size - 1] = '\0';
}

/* #command rules at the start of the line, #translate rules anywhere */
static int pp_apply_rules(char *line) {
    int i;
    for (i = g_nrules - 1; i >= 0; i--) {
        char names[PP_MAX_MARKERS][32];
        char vals[PP_MAX_MARKERS][MAX_LINE];
        int nm;
        const char *end;
        const char *at = skip_ws(line);
        for (;;) {
            if (pp_match(g_rules[i].pattern, at, names, vals, &nm, &end) &&
                (g_rules[i].anywhere || *skip_ws(end) == '\0')) {
                char res[MAX_LINE], out[MAX_LINE];
                pp_result(g_rules[i].result, names, vals, nm, res, (int)sizeof(res));
                snprintf(out, sizeof(out), "%.*s%s%s", (int)(at - line), line, res, end);
                strcpy(line, out);
                return 1;
            }
            if (!g_rules[i].anywhere || !*at) break;
            at++;
        }
    }
    return 0;
}

static void pp_expand(char *line) {
    int n;
    for (n = 0; n < 32 && (pp_apply_defines(line) | pp_apply_rules(line)); n++) {}
}

/* A directive line (#...); cond/depth: the #if stack of this file */
static int pp_directive(const char *path, int lineno, const char *p, int *cond, int *depth) {
    char w[32];
    int active = 1, i;
    for (i = 0; i < *depth; i++) active = active && cond[i];
    p = pp_word(p + 1, w, (int)sizeof(w));
    for (i = 0; w[i]; i++) w[i] = (char)tolower((unsigned char)w[i]);
    if (!strcmp(w, "ifdef") || !strcmp(w, "ifndef")) {
        char name[64];
        pp_word(p, name, (int)sizeof(name));
        if (*depth >= 32) return -1;
        cond[(*depth)++] = (pp_find_def(name, (int)strlen(name)) >= 0) == (w[2] == 'd');
        return 0;
    }
    if (!strcmp(w, "else")) {
        if (*depth < 1) { fprintf(stderr, "%s:%d: #else without #ifdef\n", path, lineno); return -1; }
        cond[*depth - 1] = !cond[*depth - 1];
        return 0;
    }
    if (!strcmp(w, "endif")) {
        if (*depth < 1) { fprintf(stderr, "%s:%d: #endif without #ifdef\n", path, lineno); return -1; }
        (*depth)--;
        return 0;
    }
    if (!active) return 0;
    if (!strcmp(w, "define")) {
        if (pp_define(p) != 0) { fprintf(stderr, "%s:%d: bad #define\n", path, lineno); return -1; }
        return 0;
    }
    if (!strcmp(w, "undef")) {
        char name[64];
        int d;
        pp_word(p, name, (int)sizeof(name));
        while ((d = pp_find_def(name, (int)strlen(name))) >= 0) {
            g_defs[d] = g_defs[--g_ndefs];
        }
        return 0;
    }
    if (!strcmp(w, "include")) {
        char file[256], dir[256], full[600];
        const char *q = skip_ws(p);
        char close = *q == '<' ? '>' : '"';
        int k = 0;
        if (*q == '"' || *q == '<') q++;
        while (*q && *q != close && k < 255) file[k++] = *q++;
        file[k] = '\0';
        path_dir(path, dir, (int)sizeof(dir));
        snprintf(full, sizeof(full), "%s/%s", dir, file);
        if (load_prg(full) != 0) {
            fprintf(stderr, "%s:%d: #include %s: not found\n", path, lineno, file);
            return -1;
        }
        return 0;
    }
    if (!strcmp(w, "error")) {
        fprintf(stderr, "%s:%d: #error %s\n", path, lineno, skip_ws(p));
        return -1;
    }
    if (!strcmp(w, "stdout")) {
        printf("%s\n", skip_ws(p));
        return 0;
    }
    if (!strcmp(w, "command") || !strcmp(w, "xcommand") || !strcmp(w, "translate") || !strcmp(w, "xtranslate")) {
        const char *arrow = strstr(p, "=>");
        if (!arrow || g_nrules >= PP_MAX_RULES) { fprintf(stderr, "%s:%d: bad #%s\n", path, lineno, w); return -1; }
        g_rules[g_nrules].anywhere = strstr(w, "translate") != NULL;
        p = skip_ws(p);
        snprintf(g_rules[g_nrules].pattern, MAX_LINE, "%.*s", (int)(arrow - p), p);
        rtrim(g_rules[g_nrules].pattern);
        snprintf(g_rules[g_nrules].result, MAX_LINE, "%s", skip_ws(arrow + 2));
        g_nrules++;
        return 0;
    }
    fprintf(stderr, "%s:%d: unknown directive #%s\n", path, lineno, w);
    return -1;
}

/* Clipper's comments: // to the end of the line, and block comments, which
 * may span lines (*in_block carries one over); quotes respected */
static void strip_clipper_comments(char *line, int *in_block) {
    char *r = line, *w = line;
    char quote = 0;
    while (*r) {
        if (*in_block) {
            if (r[0] == '*' && r[1] == '/') { *in_block = 0; r += 2; }
            else r++;
            continue;
        }
        if (quote) {
            if (*r == quote) quote = 0;
            *w++ = *r++;
            continue;
        }
        if (*r == '"' || *r == '\'') { quote = *r; *w++ = *r++; continue; }
        if (r[0] == '/' && r[1] == '/') break;
        if (r[0] == '/' && r[1] == '*') { *in_block = 1; r += 2; continue; }
        *w++ = *r++;
    }
    *w = '\0';
}

/* Lines as the interpreter's prog_load reads them: trailing blanks off, a
 * line ending in ; joined to the next, numbered by logical line. */
static int load_prg(const char *path) {
    FILE *in;
    char buf[MAX_LINE];
    char next[MAX_LINE];
    int lineno = 0;
    int in_text = 0;
    int in_block = 0;
    int cond[32], depth = 0, active, k;

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
        strip_clipper_comments(buf, &in_block);
        p = skip_ws(buf);
        if (*p == '#') {
            if (pp_directive(path, lineno, p, cond, &depth) != 0) {
                fclose(in);
                return -1;
            }
            continue;
        }
        for (active = 1, k = 0; k < depth; k++) active = active && cond[k];
        if (!active) {
            continue;
        }
        strip_comment(buf);
        pp_expand(buf);
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

/* Program files reached by DO name: each compiles to a procedure NAME
 * whose body is the file's top-level code, as Clipper linked them */
typedef struct {
    char name[MAX_NAME];
    int start, end;     /* its lines in g_lines */
} progfile_t;
static progfile_t g_progs[MAX_FILES];
static int g_nprogs;

static int try_load_prog(const char *from_src, const char *name) {
    char dir[256];
    char path[512];
    const char *exts[] = {".prg", ".PRG", NULL};
    int i, k;

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
            return 0;
        }
        if (g_nprogs >= MAX_FILES) {
            fprintf(stderr, "%s: too many program files\n", path);
            return -1;
        }
        g_progs[g_nprogs].start = g_nlines;
        if (load_prg(path) != 0) {
            return -1;
        }
        g_progs[g_nprogs].end = g_nlines;
        for (k = 0; name[k] && k < MAX_NAME - 1; k++) {
            g_progs[g_nprogs].name[k] = (char)toupper((unsigned char)name[k]);
        }
        g_progs[g_nprogs].name[k] = '\0';
        g_nprogs++;
        return 0;
    }
    return 0;
}

/* DO <name>: the name, when the line is a DO of a program or procedure */
static int parse_do_target(const char *p, char *name, int nsz) {
    int i = 0;
    if (!is_kw(p, "DO")) {
        return 0;
    }
    p = skip_ws(p + 2);
    if (is_kw(p, "WHILE") || is_kw(p, "CASE")) {
        return 0;
    }
    while ((isalnum((unsigned char)*p) || *p == '_') && i < nsz - 1) {
        name[i++] = *p++;
    }
    name[i] = '\0';
    return name[0] != '\0';
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
    if (is_kw(p, "STATIC")) {   /* STATIC name [:= value] [, ...] */
        return emit_static(e, skip_ws(p + 6));
    }
    if (is_kw(p, "LOCAL") || is_kw(p, "PRIVATE") || is_kw(p, "PUBLIC")) {
        const char *kw = is_kw(p, "LOCAL") ? "LOCAL" : is_kw(p, "PRIVATE") ? "PRIVATE" : "PUBLIC";
        if (strstr(p, ":=") || strchr(p, '[')) {
            int rc = emit_decl(e, kw, skip_ws(p + strlen(kw)));
            if (rc != 0) {
                return rc < 0 ? -1 : 0;
            }
        }
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
        p = skip_ws(p + 7);
        if (is_kw(p, "USING")) {   /* the variable given what BREAK carried */
            char name[MEMVAR_NAMELEN];
            int i = 0;
            p = skip_ws(p + 5);
            while ((isalnum((unsigned char)*p) || *p == '_') && i < MEMVAR_NAMELEN - 1) {
                name[i++] = (char)toupper((unsigned char)*p++);
            }
            name[i] = '\0';
            emit_indent(e->out, e->depth);
            fprintf(e->out, "clip_recover_using(\"%s\");\n", name);
        }
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
    if (is_kw(p, "BREAK")) {   /* BREAK [value] */
        const char *a = skip_ws(p + 5);
        emit_indent(e->out, e->depth);
        emit_at_expr(e);
        fprintf(e->out, ";\n");
        emit_indent(e->out, e->depth);
        if (*a == '\0') {
            fprintf(e->out, "clip_break_v(NULL);\n");
        } else {
            ast_node_t *n = parse_expr(a);
            sb_t b = {0};
            int r;
            if (n) {
                gen_block(&b, &n, 1, &r);
                fprintf(e->out, "{ %sif (e) clip_fail(c); else clip_break_v(&t%d); }\n", b.buf, r);
                ast_free(n);
            } else {
                sb_cstr(&b, a);
                fprintf(e->out, "clip_break_s(%s);\n", b.buf);
            }
            free(b.buf);
        }
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
    /* MEMVAR / FIELD: declarations, nothing at run time */
    if ((is_kw(p, "MEMVAR") || is_kw(p, "FIELD")) && skip_ws(p + (is_kw(p, "MEMVAR") ? 6 : 5))[0] != '-') {
        return 0;
    }

    /* What remains can fail at run time: say where it is first */
    emit_indent(e->out, e->depth);
    emit_at_expr(e);
    fprintf(e->out, ";\n");

    if (emit_expr_stmt(e, p)) {
        return 0;
    }
    if (emit_command(e, p)) {
        return 0;
    }

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
    {
        const char *whole = p;
        if (find_assign(p, name, (int)sizeof(name), &p) && lex_is_reserved(name)) {
            emit_cmd(e->out, e->depth, whole);   /* the interpreter refuses it, and says so */
            return 0;
        }
        p = whole;
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

/* A line that declares and runs nothing: what Clipper allows before the
 * first procedure of a file */
static int is_declaration(const char *text) {
    static const char *const words[] = { "STATIC", "LOCAL", "PRIVATE", "PUBLIC", "MEMVAR", "FIELD",
                                         "PARAMETERS", "DECLARE", "ANNOUNCE", "REQUEST", "EXTERNAL", NULL };
    const char *p = skip_ws(text);
    int i;
    if (*p == '\0' || *p == '*' || is_kw(p, "NOTE")) return 1;
    for (i = 0; words[i]; i++) {
        if (is_kw(p, words[i])) return 1;
    }
    return 0;
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
    fprintf(out, "    scope.nargs = nargs;\n");
    fprintf(out, "    scope.name = \"%s\";\n", fn->name);
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
    int top_stmts;
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

    /* Pull in SET PROCEDURE files, and the program files DO reaches,
     * sitting next to the source that names them (lines appended as they
     * load are scanned in turn). */
    for (scan = 0; scan < g_nlines; scan++) {
        char proc[MAX_NAME];
        if (g_lines[scan].is_text) {
            continue;
        }
        if (parse_set_proc(g_lines[scan].text, proc, MAX_NAME)) {
            if (try_load_proc(g_lines[scan].src, proc) != 0) {
                return 1;
            }
        } else if (parse_do_target(g_lines[scan].text, proc, MAX_NAME)) {
            if (try_load_prog(g_lines[scan].src, proc) != 0) {
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
            while (j < g_nlines && strcmp(g_lines[j].src, g_lines[i].src) == 0 &&
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

    /* Each program file: a procedure of its top-level lines */
    for (i = 0; i < g_nprogs; i++) {
        int k = g_progs[i].start;
        if (was_compiled_fn(g_progs[i].name)) {
            continue;   /* a procedure of that name is found first, as DO finds it */
        }
        if (nfns >= MAX_FUNCS) {
            fprintf(stderr, "too many functions\n");
            return 1;
        }
        while (k < g_progs[i].end &&
               (g_lines[k].is_text || (!is_kw(g_lines[k].text, "FUNCTION") &&
                                       !is_kw(g_lines[k].text, "PROCEDURE")))) {
            k++;
        }
        strcpy(fns[nfns].name, g_progs[i].name);
        fns[nfns].nparams = 0;
        fns[nfns].header = g_progs[i].start;
        fns[nfns].start = g_progs[i].start;
        fns[nfns].end = k;
        mark_compiled_fn(fns[nfns].name);
        nfns++;
    }

    out = fopen(outpath, "w");
    if (!out) {
        perror(outpath);
        return 1;
    }

    fprintf(out, "/* generated by s32-clip from %s */\n", inputs[0]);
    fprintf(out, "#include \"cliprt.h\"\n");
    fprintf(out, "#include <stdlib.h>\n\n");
    fprintf(out, "extern int (*const clip_blocks[])(expr_ctx_t *, value_t *);\n\n");

    for (i = 0; i < nfns; i++) {
        if (emit_function(out, &fns[i]) != 0) {
            fclose(out);
            return 1;
        }
    }

    fprintf(out, "int main(void) {\n");
    fprintf(out, "    clip_init();\n");
    {
        char label[64];
        int k;
        file_label(inputs[0], label, (int)sizeof(label));
        label[strlen(label) - 4] = '\0';   /* no .PRG */
        for (k = 0; label[k]; k++) label[k] = (char)toupper((unsigned char)label[k]);
        fprintf(out, "    clip_main_name(\"%s\");\n", label);
    }
    for (i = 0; i < nfns; i++) {
        fprintf(out, "    clip_register_fn(\"%s\", clip_fn_%s);\n",
                fns[i].name, fns[i].name);
    }

    memset(&e, 0, sizeof(e));
    e.out = out;
    top_stmts = 0;
    for (i = 0; i < g_nlines; i++) {
        if (line_in_func(i, fns, nfns)) {
            continue;
        }
        if (!is_declaration(g_lines[i].text)) {
            top_stmts++;
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
    /* Clipper's entry point: the main file's first procedure, when the
     * top level is declarations only (top-level statements are the dBASE
     * form of a main program, and run as one) */
    if (top_stmts == 0 && !e.ended) {
        int first = -1;
        for (i = 0; i < nfns; i++) {
            if (strcmp(g_lines[fns[i].header].src, inputs[0]) == 0 &&
                (first < 0 || fns[i].header < fns[first].header)) {
                first = i;
            }
        }
        if (first >= 0) {
            fprintf(out, "    { expr_ctx_t *c = clip_ctx(); value_t r; static clip_site_t site; "
                         "if (clip_call(c, \"%s\", NULL, 0, &r, &site)) clip_fail(c); }\n", fns[first].name);
        }
    }
    if (!e.ended) {
        fprintf(out, "    clip_fini();\n");
        fprintf(out, "    return 0;\n");
    }
    fprintf(out, "}\n");
    if (g_nblocks > 0) {
        fprintf(out, "\n%s", g_blockfuns.buf);
        fprintf(out, "int (*const clip_blocks[])(expr_ctx_t *, value_t *) = {");
        for (i = 0; i < g_nblocks; i++) {
            fprintf(out, "%s clip_blk_%d", i ? "," : "", i);
        }
        fprintf(out, " };\n");
    }
    fclose(out);
    return 0;
}
