/* s32-sbc: SLOW BASIC to C (docs/plans/basic-compiler.md).
 *
 *   s32-sbc input.bas -o output.c
 *
 * The program is loaded, parsed and lowered by the interpreter's own
 * front end (sbasic/src: program.c, the parser, lower.c), and the main
 * unit's ops are written as C: each op a label, a jump a goto, the rest
 * run by the interpreter's executor (act_step), whose transfers of
 * control (act_transfer) come back to a switch over the labels.  The
 * source is embedded; at run time the runtime lowers it again, by the
 * same code, and the C checks it has the ops it was written from.
 *
 * Milestone 1: expressions, assignments, PRINT, IF conditions, GOTO and
 * GOSUB are C here, calling the interpreter's operations (eval.h); the
 * rest is still the executor's, op by op. */
#include "program.h"
#include "parser.h"
#include "lower.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage(void) {
    fprintf(stderr, "usage: s32-sbc input.bas -o output.c\n");
    exit(2);
}

/* the program text as a C string literal, a line at a time */
static void emit_source(FILE *out, const char *text) {
    fprintf(out, "static const char sbc_source[] =\n    \"");
    for (const char *p = text; *p; p++) {
        unsigned char c = (unsigned char)*p;
        if (c == '\n') {
            fprintf(out, "\\n\"");
            if (p[1]) fprintf(out, "\n    \"");
            continue;
        }
        if (c == '"' || c == '\\') fprintf(out, "\\%c", c);
        else if (c == '?') fprintf(out, "\\?");     /* no trigraphs */
        else if (c < 32 || c >= 127) fprintf(out, "\\%03o", c);
        else fputc(c, out);
    }
    if (!*text) fprintf(out, "\"");
    fprintf(out, ";\n\n");
}

/* the label of op t; the end of the unit is "done" */
static void emit_goto(FILE *out, const code_t *c, int t) {
    if (t >= c->nops || t < 0) fprintf(out, "goto done;");
    else fprintf(out, "goto L%d;", t);
}

/* --- Expressions: C on temporaries T[], calling the interpreter's
 * operations.  path is the C that reaches the node at run time (from
 * the op's statement), for what is not compiled: eval_expr on it. --- */

static int ntemp, maxtemp;
static int cur_op;

static int new_temp(void) {
    if (++ntemp > maxtemp) maxtemp = ntemp;
    return ntemp - 1;
}

/* a C string literal of n bytes */
static void emit_cstring(FILE *out, const char *p, int n) {
    fputc('"', out);
    for (int i = 0; i < n; i++) {
        unsigned char ch = (unsigned char)p[i];
        if (ch == '"' || ch == '\\') fprintf(out, "\\%c", ch);
        else if (ch == '?') fprintf(out, "\\?");
        else if (ch < 32 || ch >= 127) fprintf(out, "\\%03o", ch);
        else fputc(ch, out);
    }
    fputc('"', out);
}

#define CHECK "if (err != ERR_NONE) goto fail;"

static int emit_expr(FILE *out, const expr_t *e, const char *path);

/* name(args): an array read when eval_call_array says it is one */
static int emit_call(FILE *out, const expr_t *e, const char *path, int t) {
    fprintf(out, "    { sb_array_t *ar = eval_call_array(");
    emit_cstring(out, e->call.name, (int)strlen(e->call.name));
    fprintf(out, ");\n      if (ar) { int ix[8];\n");
    for (int i = 0; i < e->call.nargs; i++) {
        char sub[1024];
        snprintf(sub, sizeof sub, "%s->call.args[%d]", path, i);
        int u = emit_expr(out, e->call.args[i], sub);
        fprintf(out, "    err = val_to_integer(&T[%d], &ix[%d]); val_clear(&T[%d]); " CHECK "\n", u, i, u);
    }
    fprintf(out, "    err = eval_array_read(ar, %d, ix, &T[%d]); " CHECK " }\n", e->call.nargs, t);
    fprintf(out, "      else { err = eval_expr(env, %s, &T[%d]); " CHECK " } }\n", path, t);
    return t;
}

static int emit_expr(FILE *out, const expr_t *e, const char *path) {
    int t = new_temp();
    char l[1024], r[1024];
    switch (e->type) {
        case EXPR_LITERAL: {
            const value_t *v = &e->literal;
            if (v->type == VAL_INTEGER || v->type == VAL_LONG)
                fprintf(out, "    T[%d].type = %s; T[%d].ival = %d;\n", t,
                        v->type == VAL_LONG ? "VAL_LONG" : "VAL_INTEGER", t, v->ival);
            else if (v->type == VAL_SINGLE || v->type == VAL_DOUBLE)
                fprintf(out, "    T[%d].type = %s; T[%d].dval = %a;\n", t,
                        v->type == VAL_DOUBLE ? "VAL_DOUBLE" : "VAL_SINGLE", t, v->dval);
            else if (v->type == VAL_STRING) {
                fprintf(out, "    T[%d] = val_string(", t);
                emit_cstring(out, v->sval->data, v->sval->len);
                fprintf(out, ", %d);\n", v->sval->len);
            } else
                fprintf(out, "    err = eval_expr(env, %s, &T[%d]); " CHECK "\n", path, t);
            return t;
        }
        case EXPR_VARIABLE:
            fprintf(out, "    err = eval_variable(env, ");
            emit_cstring(out, e->var.name, (int)strlen(e->var.name));
            fprintf(out, ", &T[%d]); " CHECK "\n", t);
            return t;
        case EXPR_UNARY: {
            snprintf(l, sizeof l, "%s->unary.operand", path);
            int u = emit_expr(out, e->unary.operand, l);
            fprintf(out, "    err = eval_unary(%d, &T[%d], &T[%d]); val_clear(&T[%d]); " CHECK "\n",
                    (int)e->unary.op, u, t, u);
            return t;
        }
        case EXPR_BINARY:
        case EXPR_COMPARE: {
            int bin = e->type == EXPR_BINARY;
            snprintf(l, sizeof l, "%s->%s.left", path, bin ? "binary" : "compare");
            snprintf(r, sizeof r, "%s->%s.right", path, bin ? "binary" : "compare");
            int u = emit_expr(out, bin ? e->binary.left : e->compare.left, l);
            int w = emit_expr(out, bin ? e->binary.right : e->compare.right, r);
            fprintf(out, "    err = eval_%s(%d, &T[%d], &T[%d], &T[%d]); val_clear(&T[%d]); val_clear(&T[%d]); " CHECK "\n",
                    bin ? "binary" : "compare", bin ? (int)e->binary.op : (int)e->compare.op,
                    u, w, t, u, w);
            return t;
        }
        case EXPR_CALL:
            if (e->call.nargs >= 1 && e->call.nargs <= 8)
                return emit_call(out, e, path, t);
            /* fall through */
        default:
            fprintf(out, "    err = eval_expr(env, %s, &T[%d]); " CHECK "\n", path, t);
            return t;
    }
}

/* --- Statements --- */

static void emit_preamble(FILE *out, const op_t *op, int k) {
    fprintf(out, "    pc = %d; cur_basic_line = %d; if (trace_enabled) eval_trace(c->ops[%d].s);\n",
            k, op->basic_line, k);
}

/* a statement compiled here: 1, or 0 for the executor's */
static int emit_stmt(FILE *out, const code_t *c, const op_t *op, int k) {
    const stmt_t *s = op->s;
    char path[1024];
    switch (s->type) {
        case STMT_ASSIGN: {
            emit_preamble(out, op, k);
            fprintf(out, "    err = eval_assign_check(env, ");
            emit_cstring(out, s->assign.name, (int)strlen(s->assign.name));
            fprintf(out, "); " CHECK "\n");
            snprintf(path, sizeof path, "c->ops[%d].s->assign.value", k);
            int t = emit_expr(out, s->assign.value, path);
            fprintf(out, "    err = eval_assign_store(env, ");
            emit_cstring(out, s->assign.name, (int)strlen(s->assign.name));
            fprintf(out, ", &T[%d]); val_clear(&T[%d]); " CHECK "\n", t, t);
            return 1;
        }
        case STMT_ARRAY_ASSIGN: {
            int n = s->array_assign.nindices;
            emit_preamble(out, op, k);
            fprintf(out, "    { int ix[8];\n");
            for (int i = 0; i < n; i++) {
                snprintf(path, sizeof path, "c->ops[%d].s->array_assign.indices[%d]", k, i);
                int u = emit_expr(out, s->array_assign.indices[i], path);
                fprintf(out, "    err = val_to_integer(&T[%d], &ix[%d]); val_clear(&T[%d]); " CHECK "\n", u, i, u);
            }
            snprintf(path, sizeof path, "c->ops[%d].s->array_assign.value", k);
            int t = emit_expr(out, s->array_assign.value, path);
            fprintf(out, "    err = eval_array_store(");
            emit_cstring(out, s->array_assign.name, (int)strlen(s->array_assign.name));
            fprintf(out, ", %d, ix, &T[%d]); val_clear(&T[%d]); " CHECK " }\n", n, t, t);
            return 1;
        }
        case STMT_PRINT: {
            if (s->print.using_fmt || s->print.using_expr) return 0;
            int nl = 1;
            emit_preamble(out, op, k);
            for (int i = 0; i < s->print.nitems; i++) {
                const print_item_t *it = &s->print.items[i];
                if (it->expr) {
                    snprintf(path, sizeof path, "c->ops[%d].s->print.items[%d].expr", k, i);
                    int t = emit_expr(out, it->expr, path);
                    int tabspc = it->expr->type == EXPR_CALL &&
                        (strcmp(it->expr->call.name, "TAB") == 0 || strcmp(it->expr->call.name, "SPC") == 0);
                    fprintf(out, "    err = print_value(&T[%d], %d, console_width); val_clear(&T[%d]); " CHECK "\n",
                            t, tabspc, t);
                }
                if (it->sep == ';') nl = 0;
                else if (it->sep == ',') { fprintf(out, "    print_comma(console_width);\n"); nl = 0; }
                else nl = 1;
            }
            if (nl) fprintf(out, "    print_text(\"\\n\");\n");
            return 1;
        }
        case STMT_GOTO:
        case STMT_GOSUB: {
            int t = code_find_label(c, s->goto_stmt.label);
            if (t < 0) return 0;                /* the executor's error */
            emit_preamble(out, op, k);
            if (s->type == STMT_GOSUB)
                fprintf(out, "    err = act_gosub(&a, %d); " CHECK "\n", k + 1);
            fprintf(out, "    ");
            emit_goto(out, c, t);
            fprintf(out, "\n");
            return 1;
        }
        default:
            return 0;
    }
}

static void emit_main(FILE *out, const code_t *c) {
    char *body = NULL;
    size_t blen = 0;
    FILE *b = open_memstream(&body, &blen);
    ntemp = maxtemp = 0;
    for (int k = 0; k < c->nops; k++) {
        const op_t *op = &c->ops[k];
        cur_op = k;
        ntemp = 0;
        fprintf(b, "L%d:\n", k);
        if (op->kind == OP_JUMP) {
            fprintf(b, "    ");
            emit_goto(b, c, op->target);
            fprintf(b, "\n");
            continue;
        }
        if (op->kind == OP_JFALSE) {
            char path[64];
            snprintf(path, sizeof path, "c->ops[%d].cond", k);
            fprintf(b, "    pc = %d;\n", k);
            int t = emit_expr(b, op->cond, path);
            fprintf(b, "    { int y = val_is_true(&T[%d]); val_clear(&T[%d]);\n      if (y %s 0) ",
                    t, t, op->invert ? "==" : "!=");
            emit_goto(b, c, k + 1);
            fprintf(b, " else ");
            emit_goto(b, c, op->target);
            fprintf(b, " }\n");
            continue;
        }
        if (op->kind == OP_STMT && emit_stmt(b, c, op, k))
            continue;
        fprintf(b, "    pc = %d; if ((err = act_step(&a, &pc)) != ERR_NONE) goto fail;", k);
        /* a simple statement goes on to the next op; anything else to
         * whichever op it chose */
        if (op->kind != OP_STMT)
            fprintf(b, " if (pc != %d) goto dispatch;", k + 1);
        fprintf(b, "\n");
    }
    fclose(b);
    if (maxtemp == 0) maxtemp = 1;
    fprintf(out,
            "static error_t sbc_main(env_t *env, code_t *c) {\n"
            "    act_t a;\n"
            "    value_t T[%d];\n"
            "    error_t err, result = act_begin(&a, env, c, 1);\n"
            "    int pc = 0;\n"
            "    memset(T, 0, sizeof T);\n"
            "    if (result != ERR_NONE) return result;\n", maxtemp);
    fwrite(body, 1, blen, out);
    free(body);
    fprintf(out,
            "    goto done;\n"
            "fail:\n"
            "    for (int i = 0; i < %d; i++) val_clear(&T[i]);\n"
            "    if (!act_transfer(&a, err, &pc, &result)) goto done;\n"
            "dispatch:\n"
            "    switch (pc) {\n", maxtemp);
    for (int k = 0; k < c->nops; k++)
        fprintf(out, "    case %d: goto L%d;\n", k, k);
    fprintf(out,
            "    default: goto done;\n"
            "    }\n"
            "done:\n"
            "    act_end(&a);\n"
            "    return result;\n"
            "}\n\n");
}

int main(int argc, char **argv) {
    const char *in = NULL, *outpath = NULL;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) outpath = argv[++i];
        else if (!in) in = argv[i];
        else usage();
    }
    if (!in || !outpath) usage();

    if (program_load_file(in) < 0) {
        fprintf(stderr, "s32-sbc: cannot read %s\n", in);
        return 1;
    }
    char *text = program_get_text();
    if (!text) { fprintf(stderr, "s32-sbc: out of memory\n"); return 1; }

    parser_t parser;
    parser_init(&parser, text, 1);
    stmt_t *program = parser_parse(&parser);
    if (parser_had_error(&parser)) {
        if (parser.error_detail[0])
            fprintf(stderr, "%s: line %d: %s (%s)\n", in, parser.error_line,
                    error_message(parser.error), parser.error_detail);
        else
            fprintf(stderr, "%s: line %d: %s\n", in, parser.error_line,
                    error_message(parser.error));
        return 1;
    }
    error_t err;
    code_t *c = lower_stmts(program, &err);
    if (!c) {
        fprintf(stderr, "%s: %s\n", in, error_message(err));
        return 1;
    }

    FILE *out = fopen(outpath, "w");
    if (!out) { fprintf(stderr, "s32-sbc: cannot write %s\n", outpath); return 1; }
    fprintf(out, "/* generated by s32-sbc from %s */\n#include \"sbcrt.h\"\n#include <string.h>\n\n", in);
    emit_source(out, text);
    emit_main(out, c);
    fprintf(out,
            "int main(void) {\n"
            "    return sbcrt_main(sbc_source, %d, sbc_main);\n"
            "}\n", c->nops);
    if (fclose(out) != 0) { fprintf(stderr, "s32-sbc: cannot write %s\n", outpath); return 1; }
    code_free(c);
    stmt_free(program);
    free(text);
    return 0;
}
