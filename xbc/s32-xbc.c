/*
 * s32-xbc — the SLOW-32 xBase compiler: .prg to C.
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
#include "func.h"
#include "program.h"
#include "ast.h"   /* xbase's own parser: expressions are parsed here, at compile time */

#define MAX_LINE  2048   /* a statement with its continuations joined (RL's PRIVATE runs to 700 characters) */
#define MAX_NEST  32
#define MAX_LINES 4000   /* the first allocation; the table grows */
#define MAX_FUNCS 512
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
    int nest_line[MAX_NEST];   /* where each open block began (for the unclosed-block message) */
    int in_fn;
    int param_i;
    int ended;
} emit_t;

typedef struct {
    char *buf;
    int len, cap;
} sb_t;

static src_line_t *g_lines;
static int g_lines_cap;
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

/* The program name the interpreter reports: the file's own name, the
 * one DO opens -- its base name and its extension as they are on disk
 * (ui.prg is ui.prg, as the interpreter names it wherever case counts),
 * .PRG added only when it has none */
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
    if (dot && strlen(dot) == 4) strcpy(buf + len, dot);
    else strcpy(buf + len, ".PRG");
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

/* ---- Expressions compiled: xbase's parser at compile time, and C that
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
static char g_blk_fn[MAX_NAME + 4] = "";   /* the function a block is written in, for PROCNAME()'s "(b)NAME" */
static int g_blk_line;                      /* and its line */

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
        if (node->binop.op == OP_AND || node->binop.op == OP_OR) {
            /* short-circuit: the right side only when the left leaves the
             * answer open (the temporaries are declared up front, so the
             * right side's code may sit inside the else) */
            int is_and = node->binop.op == OP_AND;
            sb_printf(b, "if (!e && t%d.type == VAL_LOGIC && %st%d.logic) { t%d = t%d; } else { ",
                      l, is_and ? "!" : "", l, t, l);
            r = gen_expr(b, ntmp, node->binop.right);
            sb_printf(b, "e = e || ast_eval_binop(c, %s, &t%d, &t%d, &t%d); } ",
                      op_name(node->binop.op), l, r, t);
            return t;
        }
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
        sb_printf(&g_blockfuns, "\n    clip_bframe_t bfr; clip_bframe_push(&bfr, \"(b)%s\", %d);", g_blk_fn, g_blk_line);
        sb_printf(&g_blockfuns, "\n    %s\n    clip_bframe_pop(&bfr);\n    if (!e) *result = t%d;\n    return e ? -1 : 0;\n}\n\n",
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
    case AST_MACRO:   /* &(expr): the string it gives, compiled at run time */
        l = gen_expr(b, ntmp, node->macro.expr);
        sb_printf(b, "e = e || ast_eval_macro_value(c, &t%d, &t%d); ", l, t);
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
        {
            int no_blank = *q == '(';   /* SET(...) is the function, SET (...) the command */
            q = skip_ws(q);
            if (*q == '(') {
                if (cmd_is_command_word(word) && !(no_blank && func_is_builtin(word))) {
                    return 0;
                }
            }
        }
        if (*q == '(') {
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
    if (*skip_ws(q) == '[' || *skip_ws(q) == '(' || *skip_ws(q) == ',') {
        return 0;   /* an element, or a list of names: STORE's own code assigns them */
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
    if (g_nlines >= g_lines_cap) {   /* grown on demand: DBU is 6,000 lines */
        int cap = g_lines_cap ? g_lines_cap * 2 : MAX_LINES;
        src_line_t *g = realloc(g_lines, (size_t)cap * sizeof(src_line_t));
        if (!g) {
            fprintf(stderr, "%s: out of memory\n", path);
            return -1;
        }
        g_lines = g;
        g_lines_cap = cap;
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
/* A #command / #translate rule, parsed: the match pattern and the result
 * as item lists.  Match items: a literal word (four letters match under
 * #command, exact under #xcommand), punctuation, a marker -- regular
 * <x>, list <x,...>, restricted <x:A,B>, wild <*x*>, extended <(x)> --
 * or an optional group [ ... ], which may come in any order with its
 * neighbours.  Result items: text, a marker -- regular <x>, dumb #<x>,
 * normal <"x">, smart <(x)>, blockify <{x}>, logify <.x.> -- or a group
 * [ ... ] written only when a marker in it matched. */
typedef enum { PI_WORD, PI_PUNCT, PI_MARKER, PI_GROUP, PI_TEXT } pp_kind_t;
typedef enum { MK_REGULAR, MK_LIST, MK_RESTRICTED, MK_WILD, MK_EXTENDED,
               RK_REGULAR, RK_DUMB, RK_NORMAL, RK_SMART, RK_BLOCK, RK_LOGIFY } pp_marker_t;
typedef struct pp_item {
    pp_kind_t kind;
    pp_marker_t mk;
    char text[MAX_LINE];        /* word, punctuation, or result text */
    char name[32];
    char words[8][32];          /* a restricted marker's choices */
    int nwords;
    struct pp_item *sub;        /* a group's items */
    int nsub;
} pp_item_t;
typedef struct {
    int anywhere;         /* #translate: not only at the start of a line */
    int exact;            /* #xcommand / #xtranslate: whole keywords */
    pp_item_t *pat;
    int npat;
    pp_item_t *res;
    int nres;
} pp_rule_t;
static pp_define_t g_defs[PP_MAX_DEFS];
static int g_ndefs;
static pp_rule_t g_rules[PP_MAX_RULES];
static int g_nrules;

static int load_prg(const char *path);

/* #include's search: the including file's directory, then -I directories,
 * then the compiler's own include/ (beside the s32-xbc binary) */
#define MAX_INCDIRS 16
static const char *g_incdirs[MAX_INCDIRS];
static int g_nincdirs;

static int file_exists(const char *path) {
    FILE *f = fopen(path, "r");
    if (!f) return 0;
    fclose(f);
    return 1;
}

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

/* ---- parsing a rule's two sides ---- */

static pp_item_t *pp_items_grow(pp_item_t **items, int *n) {
    pp_item_t *g = realloc(*items, (size_t)(*n + 1) * sizeof(pp_item_t));
    if (!g) return NULL;
    *items = g;
    memset(&g[*n], 0, sizeof(pp_item_t));
    return &g[(*n)++];
}

/* <...>: the marker's kind and name; p at '<', returns past '>' */
static const char *pp_parse_marker(const char *p, pp_item_t *it, int in_result) {
    const char *close;
    char inner[MAX_LINE];
    int n;
    it->kind = PI_MARKER;
    if (in_result && *p == '#') { it->mk = RK_DUMB; p++; }
    close = strchr(p, '>');
    if (!close) return NULL;
    /* <"x"> and <.x.> and <{x}> and <(x)> close with the matching mark before > */
    n = (int)(close - p - 1);
    snprintf(inner, sizeof(inner), "%.*s", n, p + 1);
    rtrim(inner);
    if (in_result) {
        if (it->mk != RK_DUMB) it->mk = RK_REGULAR;
        if (inner[0] == '"' && inner[strlen(inner) - 1] == '"') { it->mk = RK_NORMAL; inner[strlen(inner) - 1] = 0; memmove(inner, inner + 1, strlen(inner)); }
        else if (inner[0] == '(' && inner[strlen(inner) - 1] == ')') { it->mk = RK_SMART; inner[strlen(inner) - 1] = 0; memmove(inner, inner + 1, strlen(inner)); }
        else if (inner[0] == '{' && inner[strlen(inner) - 1] == '}') { it->mk = RK_BLOCK; inner[strlen(inner) - 1] = 0; memmove(inner, inner + 1, strlen(inner)); }
        else if (inner[0] == '.' && inner[strlen(inner) - 1] == '.') { it->mk = RK_LOGIFY; inner[strlen(inner) - 1] = 0; memmove(inner, inner + 1, strlen(inner)); }
        snprintf(it->name, sizeof(it->name), "%s", skip_ws(inner));
        rtrim(it->name);
    } else {
        char *c;
        it->mk = MK_REGULAR;
        if (inner[0] == '*' && inner[strlen(inner) - 1] == '*') { it->mk = MK_WILD; inner[strlen(inner) - 1] = 0; memmove(inner, inner + 1, strlen(inner)); }
        else if (inner[0] == '(' && inner[strlen(inner) - 1] == ')') { it->mk = MK_EXTENDED; inner[strlen(inner) - 1] = 0; memmove(inner, inner + 1, strlen(inner)); }
        else if ((c = strstr(inner, ",...")) != NULL) { it->mk = MK_LIST; *c = 0; }
        else if ((c = strchr(inner, ':')) != NULL) {
            char *w;
            it->mk = MK_RESTRICTED;
            *c++ = 0;
            for (w = strtok(c, ", "); w && it->nwords < 8; w = strtok(NULL, ", ")) {
                snprintf(it->words[it->nwords++], 32, "%s", w);
            }
        }
        snprintf(it->name, sizeof(it->name), "%s", skip_ws(inner));
        rtrim(it->name);
    }
    return close + 1;
}

static const char *pp_parse_items(const char *p, pp_item_t **items, int *n, int in_result, int depth);

/* The items of a pattern or result up to the end or a closing ] */
static const char *pp_parse_items(const char *p, pp_item_t **items, int *n, int in_result, int depth) {
    while (*p) {
        pp_item_t *it;
        if (!in_result) p = skip_ws(p);
        if (!*p) break;
        if (*p == ']' && depth > 0) return p + 1;
        if (*p == '[') {
            it = pp_items_grow(items, n);
            if (!it) return NULL;
            it->kind = PI_GROUP;
            p = pp_parse_items(p + 1, &it->sub, &it->nsub, in_result, depth + 1);
            if (!p) return NULL;
            continue;
        }
        if (*p == '<' || (in_result && *p == '#' && p[1] == '<')) {
            it = pp_items_grow(items, n);
            if (!it) return NULL;
            p = pp_parse_marker(p, it, in_result);
            if (!p) return NULL;
            continue;
        }
        if (in_result) {   /* text up to the next marker or bracket */
            int k = 0;
            it = pp_items_grow(items, n);
            if (!it) return NULL;
            it->kind = PI_TEXT;
            while (*p && *p != '<' && !(*p == '#' && p[1] == '<') && *p != '[' && !(*p == ']' && depth > 0) && k < MAX_LINE - 1) {
                it->text[k++] = *p++;
            }
            it->text[k] = 0;
            continue;
        }
        it = pp_items_grow(items, n);
        if (!it) return NULL;
        if (isalpha((unsigned char)*p) || *p == '_') {
            int k = 0;
            it->kind = PI_WORD;
            while ((isalnum((unsigned char)*p) || *p == '_') && k < 63) it->text[k++] = (char)toupper((unsigned char)*p++);
            it->text[k] = 0;
        } else {
            it->kind = PI_PUNCT;
            it->text[0] = *p++;
            it->text[1] = 0;
        }
    }
    return p;
}

/* ---- matching ---- */

#define PP_MAX_BIND 32
typedef struct {
    char name[32];
    int matched;
    int nvals;
    char vals[16][MAX_LINE];   /* one, or a list's elements */
} pp_bind_t;
typedef struct {
    pp_bind_t b[PP_MAX_BIND];
    int n;
} pp_binds_t;

static pp_bind_t *pp_bind(pp_binds_t *bs, const char *name) {
    int i;
    for (i = 0; i < bs->n; i++) if (strcasecmp(bs->b[i].name, name) == 0) return &bs->b[i];
    if (bs->n >= PP_MAX_BIND) return NULL;
    snprintf(bs->b[bs->n].name, 32, "%s", name);
    bs->b[bs->n].matched = 0;
    bs->b[bs->n].nvals = 0;
    return &bs->b[bs->n++];
}

/* A keyword of the text against a pattern word: whole, or its first four
 * letters or more under #command */
static int pp_word_at(const char *t, const char *word, int exact, int *len) {
    int n = 0;
    while (isalnum((unsigned char)t[n]) || t[n] == '_') n++;
    if (n == 0) return 0;
    *len = n;
    if (n == (int)strlen(word) && strncasecmp(t, word, (size_t)n) == 0) return 1;
    if (!exact && n >= 4 && n < (int)strlen(word) && strncasecmp(t, word, (size_t)n) == 0) return 1;
    return 0;
}

/* The words and punctuation that can follow item i: the next required
 * item's, and the first of each optional group before it */
static int pp_stops(const pp_item_t *items, int n, int i, const char **stops, int *nstops, int max) {
    for (; i < n && *nstops < max; i++) {
        const pp_item_t *it = &items[i];
        if (it->kind == PI_WORD || it->kind == PI_PUNCT) { stops[(*nstops)++] = it->text; return 1; }
        if (it->kind == PI_GROUP) {
            if (it->nsub > 0 && (it->sub[0].kind == PI_WORD || it->sub[0].kind == PI_PUNCT)) stops[(*nstops)++] = it->sub[0].text;
            else if (it->nsub > 0 && it->sub[0].kind == PI_MARKER && it->sub[0].mk == MK_RESTRICTED) {
                int k;
                for (k = 0; k < it->sub[0].nwords && *nstops < max; k++) stops[(*nstops)++] = it->sub[0].words[k];
            }
            continue;   /* optional: what follows it can follow too */
        }
        return 0;   /* a marker: no stop of its own */
    }
    return 0;
}

static int pp_at_stop(const char *t, const char *start, const char **stops, int nstops, int exact) {
    int k, len;
    for (k = 0; k < nstops; k++) {
        if (isalpha((unsigned char)stops[k][0])) {
            if ((t == start || !(isalnum((unsigned char)t[-1]) || t[-1] == '_')) && pp_word_at(t, stops[k], exact, &len)) return 1;
        } else if (*t == stops[k][0]) {
            return 1;
        }
    }
    return 0;
}

/* Text from t to the first top-level stop (or a top-level comma when
 * comma_stops), quotes and brackets respected */
static const char *pp_scan_expr(const char *t, const char *start, const char **stops, int nstops, int exact, int comma_stops) {
    int depth = 0;
    char q = 0;
    while (*t) {
        if (!q && depth == 0) {
            if (comma_stops && *t == ',') break;
            if (pp_at_stop(t, start, stops, nstops, exact)) break;
        }
        if (q) { if (*t == q) q = 0; }
        else if (*t == '"' || *t == '\'') q = *t;
        else if (*t == '(' || *t == '[' || *t == '{') depth++;
        else if (*t == ')' || *t == ']' || *t == '}') depth--;
        t++;
    }
    return t;
}

/* outer: the stops of the enclosing context -- a marker that runs to the
 * end of its group still stops at the other groups' keywords and at what
 * follows the run */
static int pp_match_items(const pp_item_t *items, int n, const char **tp, pp_binds_t *bs, int exact,
                          const char **outer, int nouter);

static int pp_match_items(const pp_item_t *items, int n, const char **tp, pp_binds_t *bs, int exact,
                          const char **outer, int nouter) {
    const char *t = *tp;
    int i;
    for (i = 0; i < n; i++) {
        const pp_item_t *it = &items[i];
        int len;
        t = skip_ws(t);
        switch (it->kind) {
        case PI_WORD:
            if (!pp_word_at(t, it->text, exact, &len)) return 0;
            t += len;
            break;
        case PI_PUNCT:
            if (*t != it->text[0]) return 0;
            t++;
            break;
        case PI_MARKER: {
            pp_bind_t *b = pp_bind(bs, it->name);
            const char *stops[32];
            int nstops = 0, k;
            if (!b) return 0;
            if (!pp_stops(items, n, i + 1, stops, &nstops, 32)) {
                for (k = 0; k < nouter && nstops < 32; k++) stops[nstops++] = outer[k];
            }
            if (it->mk == MK_WILD) {
                snprintf(b->vals[0], MAX_LINE, "%s", t);
                rtrim(b->vals[0]);
                b->nvals = 1;
                b->matched = 1;
                t += strlen(t);
                break;
            }
            if (it->mk == MK_RESTRICTED) {
                for (k = 0; k < it->nwords; k++) {
                    if (pp_word_at(t, it->words[k], exact, &len)) break;
                }
                if (k == it->nwords) return 0;
                snprintf(b->vals[0], MAX_LINE, "%s", it->words[k]);
                b->nvals = 1;
                b->matched = 1;
                t += len;
                break;
            }
            if (it->mk == MK_LIST) {
                b->nvals = 0;
                for (;;) {
                    const char *e = pp_scan_expr(t, t, stops, nstops, exact, 1);
                    if (e == t) return 0;
                    if (b->nvals < 16) {
                        snprintf(b->vals[b->nvals], MAX_LINE, "%.*s", (int)(e - t), t);
                        rtrim(b->vals[b->nvals]);
                        b->nvals++;
                    }
                    t = skip_ws(e);
                    if (*t != ',') break;
                    t = skip_ws(t + 1);
                }
                b->matched = 1;
                break;
            }
            {   /* regular and extended */
                const char *e = pp_scan_expr(t, t, stops, nstops, exact, 0);
                if (e == t) return 0;
                snprintf(b->vals[0], MAX_LINE, "%.*s", (int)(e - t), t);
                rtrim(b->vals[0]);
                b->nvals = 1;
                b->matched = 1;
                t = e;
            }
            break;
        }
        case PI_GROUP: {
            /* a run of optional groups: each may match, in any order */
            int j = i, used[64] = {0}, any, k;
            const char *gstops[64];
            int ngstops = 0;
            while (j < n && items[j].kind == PI_GROUP) j++;
            /* what a group's marker may run into: the run's keywords, then what follows the run */
            for (k = i; k < j; k++) pp_stops(items, n, k, gstops, &ngstops, 48);
            if (!pp_stops(items, n, j, gstops, &ngstops, 56)) {
                for (k = 0; k < nouter && ngstops < 64; k++) gstops[ngstops++] = outer[k];
            }
            do {
                any = 0;
                for (k = i; k < j; k++) {
                    const char *tt = t;
                    pp_binds_t saved = *bs;
                    if (used[k - i]) continue;
                    if (pp_match_items(items[k].sub, items[k].nsub, &tt, bs, exact, gstops, ngstops)) {
                        t = tt;
                        used[k - i] = 1;
                        any = 1;
                        break;
                    }
                    *bs = saved;
                }
            } while (any);
            i = j - 1;
            break;
        }
        default:
            break;
        }
    }
    *tp = t;
    return 1;
}

/* ---- the result ---- */

static void pp_out(char *out, int size, const char *s) {
    size_t have = strlen(out);
    snprintf(out + have, (size_t)size - have, "%s", s);
}

static void pp_quote(char *dst, int size, const char *s) {
    if (strchr(s, '"')) snprintf(dst, (size_t)size, "'%s'", s);
    else snprintf(dst, (size_t)size, "\"%s\"", s);
}

static int pp_group_matched(const pp_item_t *items, int n, const pp_binds_t *bs) {
    int i, k;
    for (i = 0; i < n; i++) {
        if (items[i].kind == PI_MARKER) {
            for (k = 0; k < bs->n; k++) {
                if (strcasecmp(bs->b[k].name, items[i].name) == 0 && bs->b[k].matched) return 1;
            }
        } else if (items[i].kind == PI_GROUP && pp_group_matched(items[i].sub, items[i].nsub, bs)) {
            return 1;
        }
    }
    return 0;
}

static void pp_emit(const pp_item_t *items, int n, const pp_binds_t *bs, char *out, int size) {
    int i, k, v;
    for (i = 0; i < n; i++) {
        const pp_item_t *it = &items[i];
        const pp_bind_t *b = NULL;
        if (it->kind == PI_TEXT) { pp_out(out, size, it->text); continue; }
        if (it->kind == PI_GROUP) {
            if (pp_group_matched(it->sub, it->nsub, bs)) pp_emit(it->sub, it->nsub, bs, out, size);
            continue;
        }
        if (it->kind != PI_MARKER) continue;
        for (k = 0; k < bs->n; k++) if (strcasecmp(bs->b[k].name, it->name) == 0) b = &bs->b[k];
        if (it->mk == RK_LOGIFY) { pp_out(out, size, b && b->matched ? ".T." : ".F."); continue; }
        if (!b || !b->matched) continue;
        for (v = 0; v < b->nvals; v++) {
            char piece[MAX_LINE + 8];
            const char *s = b->vals[v];
            size_t len = strlen(s);
            if (v) pp_out(out, size, ", ");
            switch (it->mk) {
            case RK_DUMB: case RK_NORMAL:
                pp_quote(piece, sizeof(piece), s);
                break;
            case RK_SMART:   /* parenthesized or already a string: as it is */
                if (len >= 2 && s[0] == '(' && s[len - 1] == ')') snprintf(piece, sizeof(piece), "%s", s);
                else if (len >= 2 && (s[0] == '"' || s[0] == '\'') && s[len - 1] == s[0]) snprintf(piece, sizeof(piece), "%s", s);
                else pp_quote(piece, sizeof(piece), s);
                break;
            case RK_BLOCK:
                snprintf(piece, sizeof(piece), "{|| %s}", s);
                break;
            default:
                snprintf(piece, sizeof(piece), "%s", s);
                break;
            }
            pp_out(out, size, piece);
        }
    }
}

/* #command rules at the start of the line, #translate rules anywhere */
static int pp_apply_rules(char *line) {
    int i;
    for (i = g_nrules - 1; i >= 0; i--) {
        const pp_rule_t *r = &g_rules[i];
        const char *at = skip_ws(line);
        for (;;) {
            pp_binds_t bs;
            const char *end = at;
            bs.n = 0;
            if (pp_match_items(r->pat, r->npat, &end, &bs, r->exact, NULL, 0) &&
                (r->anywhere || *skip_ws(end) == '\0')) {
                char res[MAX_LINE], out[MAX_LINE];
                res[0] = 0;
                pp_emit(r->res, r->nres, &bs, res, (int)sizeof(res));
                snprintf(out, sizeof(out), "%.*s%s%s", (int)(at - line), line, res, end);
                strcpy(line, out);
                return 1;
            }
            if (!r->anywhere || !*at) break;
            at++;
        }
    }
    return 0;
}

/* The offset of a ; outside quotes, or -1: a rule's result writes several
 * statements that way */
static int pp_top_semicolon(const char *line) {
    char quote = 0;
    const char *q;
    for (q = line; *q; q++) {
        if (quote) { if (*q == quote) quote = 0; }
        else if (*q == '"' || *q == '\'') quote = *q;
        else if (*q == ';') return (int)(q - line);
    }
    return -1;
}

/* Defines and rules until nothing changes.  A result holding several
 * statements (; between them) is split first and each statement expanded
 * on its own: a later rule must not match across the ; (ERRORSYS.PRG's
 * "#command ? <list,...> => ?? Chr(13) + Chr(10) ; ?? <list>" had the ??
 * rule take "Chr(13) + Chr(10) ; ?? cMessage" as its list). */
static void pp_expand_depth(char *line, int depth) {
    int n;
    for (n = 0; n < 32; n++) {
        if (!(pp_apply_defines(line) | pp_apply_rules(line))) return;
        if (depth < 8 && pp_top_semicolon(line) >= 0) {
            char out[MAX_LINE], piece[MAX_LINE];
            const char *p = line;
            out[0] = '\0';
            for (;;) {
                int at = pp_top_semicolon(p);
                size_t len = at >= 0 ? (size_t)at : strlen(p);
                if (len >= sizeof(piece)) len = sizeof(piece) - 1;
                memcpy(piece, p, len);
                piece[len] = '\0';
                pp_expand_depth(piece, depth + 1);
                if (strlen(out) + strlen(piece) + 4 < sizeof(out)) {
                    if (out[0]) strcat(out, " ; ");
                    strcat(out, piece);
                }
                if (at < 0) break;
                p += at + 1;
            }
            strcpy(line, out);
            return;
        }
    }
}

static void pp_expand(char *line) {
    pp_expand_depth(line, 0);
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
        if (!file_exists(full)) {
            int d;
            for (d = 0; d < g_nincdirs; d++) {
                snprintf(full, sizeof(full), "%s/%s", g_incdirs[d], file);
                if (file_exists(full)) break;
            }
            if (d == g_nincdirs) {
                fprintf(stderr, "%s:%d: #include %s: not found\n", path, lineno, file);
                return -1;
            }
        }
        if (load_prg(full) != 0) {
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
        {
            pp_rule_t *r = &g_rules[g_nrules];
            char pat[MAX_LINE];
            memset(r, 0, sizeof(*r));
            r->anywhere = strstr(w, "translate") != NULL;
            r->exact = w[0] == 'x';
            p = skip_ws(p);
            snprintf(pat, sizeof(pat), "%.*s", (int)(arrow - p), p);
            rtrim(pat);
            if (!pp_parse_items(pat, &r->pat, &r->npat, 0, 0) ||
                !pp_parse_items(skip_ws(arrow + 2), &r->res, &r->nres, 1, 0)) {
                fprintf(stderr, "%s:%d: bad #%s\n", path, lineno, w);
                return -1;
            }
            g_nrules++;
        }
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
    prog_block_stack_t blocks = { { 0 }, 0 };   /* bare END, WHILE, STATIC FUNCTION (program.c) */

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
        while (prog_cut_continuation(buf)) {   /* a ; before a trailing comment continues too */
            if (!fgets(next, sizeof(next), in)) {
                break;
            }
            rtrim(next);
            if (strlen(buf) + 1 + strlen(skip_ws(next)) < MAX_LINE - 1) {
                size_t n = strlen(buf);
                if (n > 0 && buf[n - 1] != ' ' && buf[n - 1] != '\t') strcat(buf, " ");   /* the indentation separates the tokens */
                strcat(buf, skip_ws(next));
            } else {
                fprintf(stderr, "%s(%d): statement longer than %d characters; the rest is dropped\n", path, lineno + 1, MAX_LINE - 1);
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
        /* A rule's result may hold several statements, ; between them
         * (Clipper's preprocessor writes them so); each is a line here */
        {
            char *piece = buf, *q;
            char quote = 0;
            int more = 1;
            while (more) {
                char norm[MAX_LINE];
                for (q = piece; *q; q++) {
                    if (quote) { if (*q == quote) quote = 0; }
                    else if (*q == '"' || *q == '\'') quote = *q;
                    else if (*q == ';') break;
                }
                more = *q == ';';
                *q = '\0';
                p = skip_ws(prog_normalize_line(piece, &blocks, norm, (int)sizeof(norm)));
                if (*p) {
                    if (store_line(path, p, lineno, 0) != 0) {
                        fclose(in);
                        return -1;
                    }
                    if (is_kw(p, "TEXT")) {
                        in_text = 1;
                    }
                }
                piece = q + 1;
            }
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
    e->nest_line[e->depth] = e->lineno;
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
            fprintf(e->out, "exit(clip_errorlevel());\n");   /* ERRORSYS: ErrorLevel(1); QUIT */
        } else {
            fprintf(e->out, "return clip_errorlevel();\n");
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
    if (is_kw(p, "FOR") && (isalpha((unsigned char)*skip_ws(p + 3)) || *skip_ws(p + 3) == '_')) {   /* FOR = x assigns a variable named FOR */
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
    if (is_kw(p, "NEXT") && (*skip_ws(p + 4) != '=' && !(skip_ws(p + 4)[0] == ':' && skip_ws(p + 4)[1] == '='))) {   /* NEXT = x: a variable named NEXT */
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
    /* EXTERNAL / REQUEST / ANNOUNCE: for a linker; nothing here */
    if (is_kw(p, "EXTERNAL") || is_kw(p, "REQUEST") || is_kw(p, "ANNOUNCE")) {
        return 0;
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
    g_blk_line = ln->lineno;
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
    snprintf(g_blk_fn, sizeof(g_blk_fn), "%s", fn->name);

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
        {
            static const char *const kinds = "IWFSTQRC";
            static const char *const names[] = { "IF", "DO WHILE", "FOR", "SCAN", "TEXT", "BEGIN SEQUENCE", "RECOVER", "DO CASE" };
            int d;
            fprintf(stderr, "%s:%d: unclosed block in FUNCTION %s:", e.inpath, e.lineno, fn->name);
            for (d = 0; d < e.depth; d++) {
                const char *k = strchr(kinds, e.nest[d]);
                fprintf(stderr, " %s(line %d)", k ? names[k - kinds] : "block", e.nest_line[d]);
            }
            fprintf(stderr, "\n");
        }
        return -1;
    }
    fprintf(out, "    clip_end(&scope, result);\n");
    fprintf(out, "    return 0;\n");
    fprintf(out, "}\n\n");
    return 0;
}

/* The INIT PROCEDUREs, each given the command line's words: after the
 * top level's declarations (its STATICs are made as the program loads),
 * before its first statement or the entry procedure */
static void emit_inits(FILE *out, const func_t *fns, int nfns) {
    int i;
    for (i = 0; i < nfns; i++) {
        if (prog_routine_kind(fns[i].name) == 'I')
            fprintf(out, "    { value_t r, a[16]; int k, na = 0; "
                         "for (k = 1; k < argc && na < 16; k++) a[na++] = val_str(argv[k]); "
                         "clip_fn_%s(na ? a : NULL, na, &r); }\n", fns[i].name);
    }
}

int main(int argc, char **argv) {
    const char *outpath = NULL;
    const char *inputs[MAX_FILES];
    int ninputs = 0;
    FILE *out;
    static func_t fns[MAX_FUNCS];   /* 512 of them: off the stack */
    int nfns = 0;
    emit_t e;
    int top_stmts;
    int i, j;
    int scan;

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            outpath = argv[++i];
        } else if (strncmp(argv[i], "-I", 2) == 0) {
            const char *d = argv[i][2] ? argv[i] + 2 : (i + 1 < argc ? argv[++i] : NULL);
            if (d && g_nincdirs < MAX_INCDIRS - 1) g_incdirs[g_nincdirs++] = d;
        } else if (argv[i][0] != '-') {
            if (ninputs >= MAX_FILES) {
                fprintf(stderr, "too many input files\n");
                return 1;
            }
            inputs[ninputs++] = argv[i];
        }
    }
    {   /* the compiler's own headers, beside the binary; set.ch read first
         * for every program, as Clipper's std.ch included it */
        static char own[600], setch[700];
        char dir[512];
        const char *env = getenv("XBC_INCLUDE");   /* an installed compiler's headers elsewhere */
        path_dir(argv[0], dir, (int)sizeof(dir));
        if (env && *env) snprintf(own, sizeof(own), "%s", env);
        else snprintf(own, sizeof(own), "%s/include", dir);
        if (g_nincdirs < MAX_INCDIRS) g_incdirs[g_nincdirs++] = own;
        snprintf(setch, sizeof(setch), "%s/set.ch", own);
        if (file_exists(setch) && load_prg(setch) != 0) return 1;
    }
    if (ninputs < 1 || !outpath) {
        fprintf(stderr, "Usage: s32-xbc input.prg [more.prg ...] -o output.c\n");
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

    fprintf(out, "/* generated by s32-xbc from %s */\n", inputs[0]);
    fprintf(out, "#include \"cliprt.h\"\n");
    fprintf(out, "#include <stdlib.h>\n\n");
    fprintf(out, "extern int (*const clip_blocks[])(expr_ctx_t *, value_t *);\n\n");

    for (i = 0; i < nfns; i++) {
        if (emit_function(out, &fns[i]) != 0) {
            fclose(out);
            return 1;
        }
    }

    fprintf(out, "int main(int argc, char **argv) {\n");
    fprintf(out, "    (void)argc; (void)argv;\n");
    fprintf(out, "    clip_init();\n");
    {
        char label[64];
        int k;
        file_label(inputs[0], label, (int)sizeof(label));
        label[strlen(label) - 4] = '\0';   /* no .PRG */
        for (k = 0; label[k]; k++) label[k] = (char)toupper((unsigned char)label[k]);
        fprintf(out, "    clip_main_name(\"%s\");\n", label);
        snprintf(g_blk_fn, sizeof(g_blk_fn), "%s", label);
    }
    for (i = 0; i < nfns; i++) {
        fprintf(out, "    clip_register_fn(\"%s\", clip_fn_%s);\n",
                fns[i].name, fns[i].name);
    }
    fprintf(out, "    clip_startup();\n");   /* the program's ErrorSys(), as Clipper runs it */
    /* The EXIT PROCEDUREs, run as the program ends (the main file's first) */
    for (i = 0; i < nfns; i++) {
        if (prog_routine_kind(fns[i].name) == 'E')
            fprintf(out, "    clip_exit_proc(clip_fn_%s);\n", fns[i].name);
    }

    memset(&e, 0, sizeof(e));
    e.out = out;
    top_stmts = 0;
    for (i = 0; i < g_nlines; i++) {
        if (line_in_func(i, fns, nfns)) {
            continue;
        }
        if (!prog_is_declaration(g_lines[i].text)) {
            if (top_stmts++ == 0) emit_inits(out, fns, nfns);
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
    if (top_stmts == 0) emit_inits(out, fns, nfns);
    /* Clipper's entry point: the main file's first procedure, when the
     * top level is declarations only (top-level statements are the dBASE
     * form of a main program, and run as one) */
    if (top_stmts == 0 && !e.ended) {
        int first = -1;
        for (i = 0; i < nfns; i++) {
            if (strcmp(g_lines[fns[i].header].src, inputs[0]) == 0 && !prog_routine_kind(fns[i].name) &&
                (first < 0 || fns[i].header < fns[first].header)) {
                first = i;
            }
        }
        if (first >= 0) {
            fprintf(out, "    clip_main_name(NULL);   /* no main program below the first procedure */\n");
            /* the command line's words are its parameters, as Clipper passes them */
            fprintf(out, "    { expr_ctx_t *c = clip_ctx(); value_t r, a[16]; int k, na = 0; static clip_site_t site; "
                         "for (k = 1; k < argc && na < 16; k++) a[na++] = val_str(argv[k]); "
                         "if (clip_call(c, \"%s\", na ? a : NULL, na, &r, &site)) clip_fail(c); }\n", fns[first].name);
        }
    }
    if (!e.ended) {
        fprintf(out, "    clip_fini();\n");
        fprintf(out, "    return clip_errorlevel();\n");
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
