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
#include <stdarg.h>

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
/* the jumps written while a unit's ops are (emit_unit splits the ops
 * into chunks, and a jump out of a chunk needs a stub there) */
static int gt_rec, *gt_from, *gt_to, ngt, capgt;
static int cur_op;

static void emit_goto(FILE *out, const code_t *c, int t) {
    if (t >= c->nops || t < 0) { fprintf(out, "goto done;"); return; }
    fprintf(out, "goto L%d;", t);
    if (gt_rec) {
        if (ngt == capgt) {
            capgt = capgt ? capgt * 2 : 256;
            gt_from = realloc(gt_from, capgt * sizeof *gt_from);
            gt_to = realloc(gt_to, capgt * sizeof *gt_to);
        }
        gt_from[ngt] = cur_op;
        gt_to[ngt] = t;
        ngt++;
    }
}

/* --- Expressions: C on temporaries T[], calling the interpreter's
 * operations.  path is the C that reaches the node at run time (from
 * the op's statement), for what is not compiled: eval_expr on it. --- */

static int ntemp, maxtemp;

/* the variables named, each with a cached pointer to its value (V[i],
 * good while VG[i] == env_generation) */
static char **vars;
static int nvars, capvars;

/* the names the program DIMs or assigns elements of: name(args) of one
 * of them may be read natively (another name -- ABS(X) -- never is) */
static char **anames;
static int nanames;

static void note_array_name(const char *name) {
    for (int i = 0; i < nanames; i++)
        if (strcmp(anames[i], name) == 0) return;
    anames = realloc(anames, (nanames + 1) * sizeof(char *));
    anames[nanames++] = strdup(name);
}

static int is_array_name(const char *name) {
    for (int i = 0; i < nanames; i++)
        if (strcmp(anames[i], name) == 0) return 1;
    return 0;
}

/* the program's SUBs and FUNCTIONs (top level), each with a cached
 * pointer to the interpreter's entry (P[j], for eval_call_proc) */
static struct { const char *name; int nparams, is_function; } procs[1024];
static int nprocs_known;
static const char *consts[256]; /* the names CONST defines anywhere: never */
static int nconsts;             /* stored natively (a store is the error) */
static const char *fixeds[256]; /* the STRING * n variables (made n characters */
static int nfixeds;             /* again after a call passed them by reference) */
static int nat_local;           /* writing a native procedure (see below) */
static const stmt_t *ldef;      /* that procedure */
static int native_ok_proc[1024];        /* procs[j] has a native version */
static const char *nat_valtype(int t);
static int n_of_val(val_type_t t);
static const stmt_t *cur_program;
static int nf_nnames[1024];     /* the names a native procedure's variables have */

static int proc_index(const char *name) {
    for (int i = 0; i < nprocs_known; i++)
        if (strcmp(procs[i].name, name) == 0) return i;
    return -1;
}

/* a name CONST defines somewhere (or too many CONSTs to tell) */
static int is_const_name(const char *name) {
    if (nconsts >= 256) return 1;
    for (int i = 0; i < nconsts; i++)
        if (strcasecmp(consts[i], name) == 0) return 1;
    return 0;
}

enum { N_NONE = -1, N_INT = 0, N_LONG, N_SINGLE, N_DOUBLE, N_STRING };

/* The program's DEF FNs, by name (one defined twice is never inlined),
 * and the ones being inlined, innermost last: their parameters are C
 * locals fp<id>_<i> (id -1 while only types are asked) */
static struct { const char *name; const stmt_t *def; int dup; } deffns[64];
static int ndeffns;
static struct { const stmt_t *def; int id; } fnctx[8];
static int nfnctx;

static void note_deffn(const stmt_t *s) {
    for (int i = 0; i < ndeffns; i++)
        if (strcmp(deffns[i].name, s->def_fn.name) == 0) { deffns[i].dup = 1; return; }
    if (ndeffns < 64) {
        deffns[ndeffns].name = s->def_fn.name;
        deffns[ndeffns].def = s;
        deffns[ndeffns].dup = 0;
        ndeffns++;
    }
}

static void scan_program(const stmt_t *s, int top) {
    for (; s; s = s->next) {
        if (s->type == STMT_DEF_FN) note_deffn(s);
        if (s->type == STMT_CONST && nconsts < 256) consts[nconsts++] = s->const_stmt.name;
        /* (a STRING * n is stored by the interpreter too, which cuts or pads) */
        if (s->type == STMT_DIM_SCALAR && s->dim_scalar.fixlen && nconsts < 256)
            consts[nconsts++] = s->dim_scalar.name;
        if (s->type == STMT_DIM_SCALAR && s->dim_scalar.fixlen && nfixeds < 256)
            fixeds[nfixeds++] = s->dim_scalar.name;
        if (top && (s->type == STMT_SUB_DEF || s->type == STMT_FUNC_DEF) && nprocs_known < 1024) {
            procs[nprocs_known].name = s->proc_def.name;
            procs[nprocs_known].nparams = s->proc_def.nparams;
            procs[nprocs_known].is_function = s->type == STMT_FUNC_DEF;
            nprocs_known++;
        }
        switch (s->type) {
            case STMT_IF: scan_program(s->if_stmt.then_body, 0); scan_program(s->if_stmt.else_body, 0); break;
            case STMT_WHILE: scan_program(s->while_stmt.body, 0); break;
            case STMT_DO_LOOP: scan_program(s->do_loop.body, 0); break;
            case STMT_SELECT:
                for (const case_clause_t *cl = s->select_stmt.clauses; cl; cl = cl->next)
                    scan_program(cl->body, 0);
                break;
            case STMT_SUB_DEF: case STMT_FUNC_DEF: scan_program(s->proc_def.body, 0); break;
            default: break;
        }
    }
}

/* name(args) that is a call of one of the program's FUNCTIONs, with the
 * right number of arguments (a wrong number is the executor's error,
 * before any argument is evaluated) */
/* arguments the C passes itself: no name(args) that is not in
 * parentheses and not a FUNCTION call -- an array element, passed by
 * reference, is the interpreter's to pass (it may be one only at run
 * time) */
static int is_function_call(const expr_t *e);
static int nat_builtin(const expr_t *e);
static int args_compilable(expr_t *const *args, int n) {
    for (int i = 0; i < n; i++)
        if (args[i]->type == EXPR_CALL && !args[i]->paren && !is_function_call(args[i]) &&
            !nat_builtin(args[i]))
            return 0;
    return 1;
}

static int is_function_call(const expr_t *e) {
    int j = proc_index(e->call.name);
    return j >= 0 && procs[j].is_function && procs[j].nparams == e->call.nargs &&
           e->call.nargs <= 16 && !is_array_name(e->call.name);
}

/* name(args) that is a call of a DEF FN to inline: one definition, as
 * many arguments as it takes, numbers in and out (types by their names:
 * the parser wrote DEFtype's into them), not recursive; in the main program or an env procedure,
 * where its body's other variables are the unit's (call_def_fn's scope
 * is the caller's env, its parameters bound in a child of it) */
static int is_const_name(const char *name);
static int predict_name(const char *name);
static int lazy_concat(const expr_t *e);
static int deffn_index(const expr_t *e) {
    if (e->type != EXPR_CALL || nat_local || nfnctx >= 8 ||
        is_array_name(e->call.name) || proc_index(e->call.name) >= 0)
        return -1;
    for (int i = 0; i < ndeffns; i++) {
        if (strcmp(deffns[i].name, e->call.name) != 0) continue;
        const stmt_t *d = deffns[i].def;
        if (deffns[i].dup || d->def_fn.nparams != e->call.nargs) return -1;
        for (int k = 0; k < nfnctx; k++) if (fnctx[k].def == d) return -1;
        int ft = predict_name(d->def_fn.name);
        if (ft == N_NONE || ft == N_STRING) return -1;
        for (int k = 0; k < d->def_fn.nparams; k++) {
            int pt = predict_name(d->def_fn.params[k]);
            if (pt == N_NONE || pt == N_STRING || is_const_name(d->def_fn.params[k]) ||
                strncmp(d->def_fn.params[k], "FN", 2) == 0)
                return -1;
        }
        return i;
    }
    return -1;
}

/* name a parameter of a DEF FN being inlined: its context and index */
static int fn_param(const char *name, int *ctx, int *idx) {
    for (int k = nfnctx - 1; k >= 0; k--)
        for (int i = 0; i < fnctx[k].def->def_fn.nparams; i++)
            if (strcmp(fnctx[k].def->def_fn.params[i], name) == 0) {
                *ctx = k; *idx = i;
                return 1;
            }
    return 0;
}

/* the arguments of a call into T, a variable not in parentheses as its
 * reference (R[i], if eval_arg_ref says it is passed so) */
static int emit_args(FILE *out, expr_t *const *args, int n, const char *base, int *ta,
                     const char *proc, int pj);
static int nrefs;               /* R<n> arrays: unique in the file */


static void scan_array_names(const stmt_t *s) {
    for (; s; s = s->next) {
        switch (s->type) {
            case STMT_DIM: note_array_name(s->dim_stmt.name); break;
            case STMT_ARRAY_ASSIGN: note_array_name(s->array_assign.name); break;
            case STMT_READ:
                for (int i = 0; i < s->read_stmt.nvars; i++)
                    if (s->read_stmt.targets && s->read_stmt.targets[i])
                        note_array_name(s->read_stmt.varnames[i]);
                break;
            case STMT_INPUT:
                for (int i = 0; i < s->input.nvars; i++)
                    if (s->input.targets && s->input.targets[i])
                        note_array_name(s->input.varnames[i]);
                break;
            case STMT_IF:
                scan_array_names(s->if_stmt.then_body);
                scan_array_names(s->if_stmt.else_body);
                break;
            case STMT_WHILE: scan_array_names(s->while_stmt.body); break;
            case STMT_DO_LOOP: scan_array_names(s->do_loop.body); break;
            case STMT_SELECT:
                for (const case_clause_t *cl = s->select_stmt.clauses; cl; cl = cl->next)
                    scan_array_names(cl->body);
                break;
            case STMT_SUB_DEF: case STMT_FUNC_DEF: scan_array_names(s->proc_def.body); break;
            default: break;
        }
    }
}

/* the arrays a unit reads natively, each with a cached pointer (AR[j],
 * good while AG[j] == array_generation) */
static char **arrs;
static int narrs, caparrs;

static int arr_index(const char *name) {
    for (int i = 0; i < narrs; i++)
        if (strcmp(arrs[i], name) == 0) return i;
    if (narrs == caparrs) {
        caparrs = caparrs ? caparrs * 2 : 16;
        arrs = realloc(arrs, caparrs * sizeof(char *));
    }
    arrs[narrs] = strdup(name);
    return narrs++;
}

static int var_index(const char *name) {
    for (int i = 0; i < nvars; i++)
        if (strcmp(vars[i], name) == 0) return i;
    if (nvars == capvars) {
        capvars = capvars ? capvars * 2 : 32;
        vars = realloc(vars, capvars * sizeof(char *));
    }
    vars[nvars] = strdup(name);
    return nvars++;
}

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

/* after a call: each STRING * n variable passed by reference cut or
 * padded again (eval_refix) */
static void emit_refix(FILE *out, expr_t *const *args, int n) {
    for (int i = 0; i < n; i++) {
        if (args[i]->type != EXPR_VARIABLE || args[i]->paren) continue;
        for (int j = 0; j < nfixeds; j++)
            if (strcasecmp(fixeds[j], args[i]->var.name) == 0) {
                fprintf(out, "    err = eval_refix(env, ");
                emit_cstring(out, args[i]->var.name, (int)strlen(args[i]->var.name));
                fprintf(out, "); " CHECK "\n");
                break;
            }
    }
}


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
        fprintf(out, "    err = val_to_integer(&T[%d], &ix[%d]); VAL_CLEAR(&T[%d]); " CHECK "\n", u, i, u);
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
        case EXPR_VARIABLE: {
            int v = var_index(e->var.name);
            fprintf(out, "    if (VG[%d] != env_generation || !V[%d]) { V[%d] = eval_var_slot(env, ", v, v, v);
            emit_cstring(out, e->var.name, (int)strlen(e->var.name));
            fprintf(out, "); VG[%d] = env_generation; }\n", v);
            fprintf(out, "    if (V[%d]) T[%d] = val_copy(V[%d]); else { err = eval_variable(env, ", v, t, v);
            emit_cstring(out, e->var.name, (int)strlen(e->var.name));
            fprintf(out, ", &T[%d]); " CHECK " }\n", t);
            return t;
        }
        case EXPR_UNARY: {
            snprintf(l, sizeof l, "%s->unary.operand", path);
            int u = emit_expr(out, e->unary.operand, l);
            fprintf(out, "    err = eval_unary(%d, &T[%d], &T[%d]); VAL_CLEAR(&T[%d]); " CHECK "\n",
                    (int)e->unary.op, u, t, u);
            return t;
        }
        case EXPR_BINARY:
        case EXPR_COMPARE: {
            int bin = e->type == EXPR_BINARY;
            snprintf(l, sizeof l, "%s->%s.left", path, bin ? "binary" : "compare");
            snprintf(r, sizeof r, "%s->%s.right", path, bin ? "binary" : "compare");
            int u, w;
            if (bin && lazy_concat(e)) {        /* (s$ read after e, as QBasic reads it) */
                w = emit_expr(out, e->binary.right, r);
                u = emit_expr(out, e->binary.left, l);
            } else {
                u = emit_expr(out, bin ? e->binary.left : e->compare.left, l);
                w = emit_expr(out, bin ? e->binary.right : e->compare.right, r);
            }
            fprintf(out, "    err = eval_%s(%d, &T[%d], &T[%d], &T[%d]); VAL_CLEAR(&T[%d]); VAL_CLEAR(&T[%d]); " CHECK "\n",
                    bin ? "binary" : "compare", bin ? (int)e->binary.op : (int)e->compare.op,
                    u, w, t, u, w);
            return t;
        }
        case EXPR_CALL:
            if (is_function_call(e) && args_compilable(e->call.args, e->call.nargs)) {
                /* the arguments here, then the call: as call_proc */
                int n = e->call.nargs, ta[16];
                char base[1024];
                snprintf(base, sizeof base, "%s->call.args", path);
                int rid = emit_args(out, e->call.args, n, base, ta, e->call.name, proc_index(e->call.name));
                int pj = proc_index(e->call.name);
                if (native_ok_proc[pj]) {
                    /* its native version when it may run: references as
                     * eval_arg_ref gave them, values converted to the
                     * parameters' types as call_proc_values converts them */
                    fprintf(out, "    if (!trace_enabled && (NFOK[%d] > 0 || (NFOK[%d] == 0 && (NFOK[%d] = eval_names_plain(NFN%d, (int)(sizeof NFN%d / sizeof NFN%d[0]) - 1) ? 1 : -1) > 0))) {\n",
                            pj, pj, pj, pj, pj, pj);
                    fprintf(out, "      value_t *pp[%d];\n", n ? n : 1);
                    for (int i = 0; i < n; i++) {
                        const stmt_t *pd = NULL;
                        for (const stmt_t *d = cur_program; d; d = d->next)
                            if ((d->type == STMT_FUNC_DEF || d->type == STMT_SUB_DEF) &&
                                strcmp(d->proc_def.name, e->call.name) == 0) { pd = d; break; }
                        fprintf(out, "      if (R%d[%d]) pp[%d] = R%d[%d]; else { err = val_coerce(&T[%d], %s); " CHECK " pp[%d] = &T[%d]; }\n",
                                rid, i, i, rid, i, ta[i], nat_valtype(n_of_val(pd->proc_def.param_types[i])), i, ta[i]);
                    }
                    fprintf(out, "      err = NF%d(pp, &T[%d]);\n", pj, t);
                    for (int i = 0; i < n; i++)
                        fprintf(out, "      VAL_CLEAR(&T[%d]);\n", ta[i]);
                    fprintf(out, "      " CHECK "\n    } else\n");
                }
                fprintf(out, "    { value_t av[%d];\n", n ? n : 1);
                for (int i = 0; i < n; i++)
                    fprintf(out, "      av[%d] = T[%d]; T[%d].type = VAL_INTEGER;\n", i, ta[i], ta[i]);
                fprintf(out, "      err = eval_call_proc(");
                emit_cstring(out, e->call.name, (int)strlen(e->call.name));
                fprintf(out, ", &P[%d], av, R%d, %d, &T[%d]); " CHECK " }\n", proc_index(e->call.name), rid, n, t);
                emit_refix(out, e->call.args, n);
                return t;
            }
            if (e->call.nargs >= 1 && e->call.nargs <= 8)
                return emit_call(out, e, path, t);
            /* fall through */
        default:
            fprintf(out, "    err = eval_expr(env, %s, &T[%d]); " CHECK "\n", path, t);
            return t;
    }
}

static int emit_args(FILE *out, expr_t *const *args, int n, const char *base, int *ta,
                     const char *proc, int pj) {
    int id = nrefs++;
    fprintf(out, "    value_t *R%d[%d];\n", id, n ? n : 1);
    for (int i = 0; i < n; i++) {
        char sub[1100];
        const expr_t *a = args[i];
        snprintf(sub, sizeof sub, "%s[%d]", base, i);
        if (a->type == EXPR_VARIABLE && !a->paren) {
            ta[i] = new_temp();
            fprintf(out, "    R%d[%d] = eval_arg_ref(env, ", id, i);
            emit_cstring(out, a->var.name, (int)strlen(a->var.name));
            fprintf(out, ");\n    if (!R%d[%d]) { err = eval_variable(env, ", id, i);
            emit_cstring(out, a->var.name, (int)strlen(a->var.name));
            fprintf(out, ", &T[%d]); " CHECK " }\n", ta[i]);
            fprintf(out, "    else { err = eval_arg_check(");
            emit_cstring(out, proc, (int)strlen(proc));
            fprintf(out, ", &P[%d], %d, R%d[%d]); " CHECK " }\n", pj, i, id, i);
        } else {
            ta[i] = emit_expr(out, a, sub);
            fprintf(out, "    R%d[%d] = NULL;\n", id, i);
        }
    }
    return id;
}

/* --- Native code (milestone 2): a numeric expression on C long long and
 * double, the interpreter's rules for each type written out (value.c).
 * A statement's native version runs when its guard holds -- every
 * variable it names cached, of the type its name predicts -- and its
 * generic version otherwise.  Types: */

/* s$ + e, s$ a string variable of the variable-length kind and e more
 * than a constant or a variable: QBasic reads s$ as it concatenates,
 * after e (a FUNCTION in e that changes s$ is seen) -- the interpreter's
 * EXPR_BINARY, which takes e first; reading s$ does nothing else */
static int lazy_concat(const expr_t *e) {
    const expr_t *l = e->binary.left, *r = e->binary.right;
    if (e->binary.op != OP_ADD || l->type != EXPR_VARIABLE || r->type == EXPR_LITERAL) return 0;
    if (r->type == EXPR_VARIABLE && proc_index(r->var.name) < 0) return 0;
    if (predict_name(l->var.name) != N_STRING) return 0;
    for (int j = 0; j < nfixeds; j++)
        if (strcasecmp(fixeds[j], l->var.name) == 0) return 0;
    return 1;
}

static int predict_name(const char *name) {
    size_t n = strlen(name);
    char last = n ? name[n - 1] : 0;
    if (strcmp(name, "ERR") == 0 || strcmp(name, "ERL") == 0) return N_NONE;
    if (last == '%') return N_INT;
    if (last == '&') return N_LONG;
    if (last == '#') return N_DOUBLE;
    if (last == '$') return N_STRING;
    return N_SINGLE;            /* (the guard checks it) */
}

static int nat_is_float(int t) { return t == N_SINGLE || t == N_DOUBLE; }

/* The numeric builtins written natively, as builtin.c computes them: a
 * name(x) the program does not make an array or a procedure is the
 * builtin (the interpreter's order: procedure, array, builtin) */
static const char *nat_builtins[] = {
    "ABS", "INT", "FIX", "SGN", "SQR", "SIN", "COS", "TAN", "ATN", "EXP", "LOG",
    "CINT", "CLNG", "CSNG", "CDBL", "RND", NULL
};

static int nat_builtin(const expr_t *e) {
    if (e->type != EXPR_CALL || e->call.nargs != 1 || is_array_name(e->call.name) ||
        proc_index(e->call.name) >= 0)
        return 0;
    for (int i = 0; nat_builtins[i]; i++)
        if (strcmp(nat_builtins[i], e->call.name) == 0) return 1;
    return 0;
}

static int nat_type(const expr_t *e);

/* its result's type, from its argument's (builtin_call's retag: a double
 * from a function is a single unless the argument was a double) */
static int nat_builtin_type(const expr_t *e) {
    const char *n = e->call.name;
    int a = nat_type(e->call.args[0]);
    if (a < 0) return -1;
    if (!strcmp(n, "ABS") || !strcmp(n, "INT") || !strcmp(n, "FIX")) return a;
    if (!strcmp(n, "SGN") || !strcmp(n, "CINT")) return N_INT;
    if (!strcmp(n, "CLNG")) return N_LONG;
    if (!strcmp(n, "CSNG")) return N_SINGLE;
    if (!strcmp(n, "CDBL")) return N_DOUBLE;
    if (!strcmp(n, "RND")) return N_SINGLE;     /* fn_rnd's single, whatever n is */
    return a == N_DOUBLE ? N_DOUBLE : N_SINGLE;
}

/* The builtins called natively through builtin_call itself (the tree
 * walk and the lookups skipped, the arguments evaluated in order as the
 * interpreter evaluates them): string functions, and LEN, ASC, INSTR,
 * whose values are INTEGERs (a string is at most 4,096 characters) */
static const struct { const char *name; int type; } rt_builtins[] = {
    { "MID$", N_STRING }, { "LEFT$", N_STRING }, { "RIGHT$", N_STRING },
    { "CHR$", N_STRING }, { "STR$", N_STRING }, { "UCASE$", N_STRING },
    { "LCASE$", N_STRING }, { "LTRIM$", N_STRING }, { "RTRIM$", N_STRING },
    { "SPACE$", N_STRING }, { "STRING$", N_STRING }, { "HEX$", N_STRING },
    { "OCT$", N_STRING }, { "LEN", N_INT }, { "ASC", N_INT }, { "INSTR", N_INT },
    { NULL, 0 }
};

static int rt_builtin_type(const expr_t *e) {
    if (e->type != EXPR_CALL || e->call.nargs < 1 || e->call.nargs > 4 ||
        is_array_name(e->call.name) || proc_index(e->call.name) >= 0)
        return N_NONE;
    for (int i = 0; rt_builtins[i].name; i++)
        if (strcmp(rt_builtins[i].name, e->call.name) == 0)
            return rt_builtins[i].type;
    return N_NONE;
}

/* LEN, ASC, CHR$, LEFT$, RIGHT$, MID$ called through their typed
 * entries: when the arguments are as many as they take, of the types
 * they take (otherwise builtin_call, which reports what is wrong) */
static int nat_type(const expr_t *e);
static int nat_strfn(const expr_t *e) {
    if (rt_builtin_type(e) == N_NONE) return 0;
    const char *fn = e->call.name;
    int n = e->call.nargs, str;
    if (!strcmp(fn, "LEN") || !strcmp(fn, "ASC")) { if (n != 1) return 0; str = 1; }
    else if (!strcmp(fn, "CHR$")) { if (n != 1) return 0; str = 0; }
    else if (!strcmp(fn, "LEFT$") || !strcmp(fn, "RIGHT$")) { if (n != 2) return 0; str = 1; }
    else if (!strcmp(fn, "MID$")) { if (n != 2 && n != 3) return 0; str = 1; }
    else return 0;
    for (int i = 0; i < n; i++) {
        int t = nat_type(e->call.args[i]);
        if (t == N_NONE || (t == N_STRING) != (str && i == 0)) return 0;
    }
    return 1;
}

/* a variable's native type: in a native procedure a parameter's is its
 * declaration's (AS, or its name's), anything else its name's */
static int var_ntype(const char *name) {
    int fc, fi;
    if (fn_param(name, &fc, &fi)) return predict_name(name);
    if (nat_local)
        for (int i = 0; i < ldef->proc_def.nparams; i++)
            if (strcmp(ldef->proc_def.params[i], name) == 0)
                return n_of_val(ldef->proc_def.param_types[i]);
    return predict_name(name);
}

/* the native type of e, or N_NONE when it is not done natively */
static int nat_type_(const expr_t *e);

/* (a native procedure has no strings: one there is not native) */
static int nat_type(const expr_t *e) {
    int t = nat_type_(e);
    return (nat_local && t == N_STRING) ? N_NONE : t;
}

static int nat_type_(const expr_t *e) {
    int l, r;
    switch (e->type) {
        case EXPR_LITERAL:
            switch (e->literal.type) {
                case VAL_INTEGER: return N_INT;
                case VAL_LONG:    return N_LONG;
                case VAL_SINGLE:  return N_SINGLE;
                case VAL_DOUBLE:  return N_DOUBLE;
                case VAL_STRING:  return N_STRING;
                default:          return N_NONE;
            }
        case EXPR_VARIABLE:
            return var_ntype(e->var.name);
        case EXPR_CALL: {
            int fx = deffn_index(e);
            if (fx >= 0) {
                for (int i = 0; i < e->call.nargs; i++) {
                    int at = nat_type(e->call.args[i]);
                    if (at == N_NONE || at == N_STRING) return N_NONE;
                }
                fnctx[nfnctx].def = deffns[fx].def; fnctx[nfnctx].id = -1; nfnctx++;
                int bt = nat_type(deffns[fx].def->def_fn.body);
                nfnctx--;
                if (bt == N_NONE || bt == N_STRING) return N_NONE;
                return predict_name(deffns[fx].def->def_fn.name);
            }
            }
            if (nat_builtin(e)) {
                if (nat_type(e->call.args[0]) == N_STRING) return N_NONE;
                return nat_builtin_type(e);
            }
            if (rt_builtin_type(e) != N_NONE) {
                for (int i = 0; i < e->call.nargs; i++)
                    if (nat_type(e->call.args[i]) == N_NONE) return N_NONE;
                return rt_builtin_type(e);
            }
            if (is_function_call(e)) {
                /* its value's type is its name's (DEFtype's written into it) */
                if (!args_compilable(e->call.args, e->call.nargs)) return N_NONE;
                for (int i = 0; i < e->call.nargs; i++)
                    if (nat_type(e->call.args[i]) == N_NONE || nat_type(e->call.args[i]) == N_STRING)
                        return N_NONE;
                return predict_name(e->call.name) == N_STRING ? N_NONE : predict_name(e->call.name);
            }
            if (nat_local || !is_array_name(e->call.name) || e->call.nargs < 1 || e->call.nargs > 8)
                return N_NONE;
            for (int i = 0; i < e->call.nargs; i++)
                if (nat_type(e->call.args[i]) == N_NONE || nat_type(e->call.args[i]) == N_STRING)
                    return N_NONE;
            return predict_name(e->call.name);
        case EXPR_UNARY:
            l = nat_type(e->unary.operand);
            if (l == N_NONE || l == N_STRING) return N_NONE;
            if (e->unary.op == OP_NOT && nat_is_float(l)) return N_NONE;
            return l;
        case EXPR_COMPARE:
            l = nat_type(e->compare.left);
            r = nat_type(e->compare.right);
            if (l == N_NONE || r == N_NONE) return N_NONE;
            if ((l == N_STRING) != (r == N_STRING)) return N_NONE;    /* Type mismatch: generic */
            return N_INT;
        case EXPR_BINARY:
            l = nat_type(e->binary.left);
            r = nat_type(e->binary.right);
            if (l == N_NONE || r == N_NONE) return N_NONE;
            if (l == N_STRING || r == N_STRING)
                return (e->binary.op == OP_ADD && l == N_STRING && r == N_STRING) ? N_STRING : N_NONE;
            switch (e->binary.op) {
                case OP_ADD: case OP_SUB: case OP_MUL:
                    if (nat_is_float(l) || nat_is_float(r))
                        return (l == N_DOUBLE || r == N_DOUBLE) ? N_DOUBLE : N_SINGLE;
                    return (l == N_LONG || r == N_LONG) ? N_LONG : N_INT;
                case OP_DIV: case OP_POW:
                    return (l == N_DOUBLE || r == N_DOUBLE) ? N_DOUBLE : N_SINGLE;
                case OP_IDIV: case OP_MOD:
                case OP_AND: case OP_OR: case OP_XOR: case OP_IMP: case OP_EQV:
                    /* on floats the result's type depends on the value */
                    if (nat_is_float(l) || nat_is_float(r)) return N_NONE;
                    return (l == N_LONG || r == N_LONG) ? N_LONG : N_INT;
                default:
                    return N_NONE;
            }
        default:
            return N_NONE;
    }
}

/* the variables and arrays a native statement uses, for its guard */
typedef struct {
    int v[64], nv;              /* variables (nv past 64: go generic) */
    int a[16], an[16], na;      /* arrays, with the subscripts they take */
    int f[16], nf;              /* DEF FNs inlined (deffns[]): defined yet? */
} uses_t;

static void nat_uses(const expr_t *e, uses_t *u);

static void nat_vars(const expr_t *e, int *list, int *n);

static void nat_uses(const expr_t *e, uses_t *u) {
    int fx = e->type == EXPR_CALL ? deffn_index(e) : -1;
    if (fx >= 0) {
        for (int i = 0; i < e->call.nargs; i++) nat_uses(e->call.args[i], u);
        int seen = 0;
        for (int i = 0; i < u->nf; i++) if (u->f[i] == fx) seen = 1;
        if (!seen) {
            if (u->nf == 16) { u->nv = 1000; return; }   /* too many: generic */
            u->f[u->nf++] = fx;
        }
        fnctx[nfnctx].def = deffns[fx].def; fnctx[nfnctx].id = -1; nfnctx++;
        nat_uses(deffns[fx].def->def_fn.body, u);
        nfnctx--;
        return;
    }
    if (e->type == EXPR_CALL && (is_function_call(e) || nat_builtin(e) || rt_builtin_type(e) != N_NONE)) {
        for (int i = 0; i < e->call.nargs; i++) nat_uses(e->call.args[i], u);
        return;
    }
    if (e->type == EXPR_CALL) {
        int j = arr_index(e->call.name);
        if (u->na < 16) { u->a[u->na] = j; u->an[u->na] = e->call.nargs; }
        u->na++;
        for (int i = 0; i < e->call.nargs; i++) nat_uses(e->call.args[i], u);
        return;
    }
    switch (e->type) {
        case EXPR_VARIABLE: nat_vars(e, u->v, &u->nv); return;
        case EXPR_UNARY: nat_uses(e->unary.operand, u); return;
        case EXPR_BINARY: nat_uses(e->binary.left, u); nat_uses(e->binary.right, u); return;
        case EXPR_COMPARE: nat_uses(e->compare.left, u); nat_uses(e->compare.right, u); return;
        default: return;
    }
}

/* the variables e names, for the guard */
static void nat_vars(const expr_t *e, int *list, int *n) {
    switch (e->type) {
        case EXPR_VARIABLE: {
            int fc, fi;
            if (fn_param(e->var.name, &fc, &fi)) return;    /* a C local */
            int v = var_index(e->var.name);
            for (int i = 0; i < *n; i++) if (list[i] == v) return;
            if (*n < 64) list[*n] = v;
            (*n)++;                     /* past 64: the caller goes generic */
            return;
        }
        case EXPR_UNARY: nat_vars(e->unary.operand, list, n); return;
        case EXPR_BINARY: nat_vars(e->binary.left, list, n); nat_vars(e->binary.right, list, n); return;
        case EXPR_COMPARE: nat_vars(e->compare.left, list, n); nat_vars(e->compare.right, list, n); return;
        default: return;
    }
}

static const char *nat_valtype(int t) {
    static const char *names[] = { "VAL_INTEGER", "VAL_LONG", "VAL_SINGLE", "VAL_DOUBLE", "VAL_STRING" };
    return names[t];
}

static int nnat;

/* Writing a native procedure (nat_local: its variables C locals, no
 * env): its parameters are p[i] (value_t *), its other variables L<n> */
static char *lnames[256];
static int nlnames;

static int local_index(const char *name) {
    for (int i = 0; i < nlnames; i++)
        if (strcmp(lnames[i], name) == 0) return i;
    if (nlnames < 256) lnames[nlnames] = strdup(name);
    return nlnames++;
}

/* the C expression of a variable's value_t * */
static const char *var_ptr(const char *name) {
    static char buf[8][48];
    static int k;
    char *b = buf[k++ & 7];
    int fc, fi;
    if (fn_param(name, &fc, &fi)) {
        sprintf(b, "(&fp%d_%d)", fnctx[fc].id, fi);
        return b;
    }
    if (nat_local) {
        for (int i = 0; i < ldef->proc_def.nparams; i++)
            if (strcmp(ldef->proc_def.params[i], name) == 0) {
                sprintf(b, "p[%d]", i);
                return b;
            }
        sprintf(b, "(&L%d)", local_index(name));
        return b;
    }
    sprintf(b, "V[%d]", var_index(name));
    return b;
}

static int n_of_val(val_type_t t) {
    switch (t) {
        case VAL_INTEGER: return N_INT;
        case VAL_LONG:    return N_LONG;
        case VAL_SINGLE:  return N_SINGLE;
        case VAL_DOUBLE:  return N_DOUBLE;
        default:          return N_NONE;
    }
}

/* an integer range check of a long long r for type t */
static void nat_range(FILE *out, const char *r, int t) {
    if (t == N_INT)
        fprintf(out, "    if (%s < -32768 || %s > 32767) { err = ERR_OVERFLOW; goto fail; }\n", r, r);
    else
        fprintf(out, "    if (%s < -2147483648LL || %s > 2147483647LL) { err = ERR_OVERFLOW; goto fail; }\n", r, r);
}

static void nat_index(FILE *out, int j, const char **ix, const int *ixt, int n, const char *flat);

/* subscript x of type t, converted as val_to_integer converts it -- at
 * once, as the interpreter does, before the next subscript is evaluated;
 * the integer's C name into conv */
static void nat_subscript(FILE *out, const char *x, int t, char *conv) {
    if (nat_is_float(t)) {
        fprintf(out, "    if (!(%s < 2147483647.5 && %s >= -2147483648.5)) { err = ERR_OVERFLOW; goto fail; }\n", x, x);
        sprintf(conv, "%s_c", x);
        fprintf(out, "    long long %s = (long long)val_rint(%s);\n", conv, x);
    } else {
        strcpy(conv, x);
    }
}

/* the uses of the native statement being written: after a call (which
 * may CLEAR, ERASE, REDIM) its cached pointers are fetched again and
 * the guard's condition checked again -- an internal error if it no
 * longer holds, never a stale pointer */
static const uses_t *cur_uses;

static void nat_revalidate(FILE *out) {
    const uses_t *u = cur_uses;
    fprintf(out, "    if (env_generation != sg || array_generation != sa) {\n");
    for (int i = 0; i < u->nv; i++) {
        fprintf(out, "      V[%d] = eval_var_slot(env, ", u->v[i]);
        emit_cstring(out, vars[u->v[i]], (int)strlen(vars[u->v[i]]));
        fprintf(out, "); VG[%d] = env_generation;\n", u->v[i]);
    }
    for (int i = 0; i < u->na; i++) {
        fprintf(out, "      AR[%d] = eval_call_array(", u->a[i]);
        emit_cstring(out, arrs[u->a[i]], (int)strlen(arrs[u->a[i]]));
        fprintf(out, "); AG[%d] = array_generation;\n", u->a[i]);
    }
    fprintf(out, "      if (!(1");
    for (int i = 0; i < u->nv; i++)
        fprintf(out, " && V[%d] && V[%d]->type == %s", u->v[i], u->v[i],
                nat_valtype(predict_name(vars[u->v[i]])));
    for (int i = 0; i < u->na; i++)
        fprintf(out, " && AR[%d] && AR[%d]->elem_type == %s && AR[%d]->ndims == %d",
                u->a[i], u->a[i], nat_valtype(predict_name(arrs[u->a[i]])), u->a[i], u->an[i]);
    fprintf(out, ")) { err = ERR_INTERNAL; goto fail; }\n"
                 "      sg = env_generation; sa = array_generation;\n    }\n");
}

static void nat_store(FILE *out, const char *slot, int st, const char *x, int t);

/* code computing e into a fresh C local; its name in *name */
static int nat_emit(FILE *out, const expr_t *e, char *name) {
    int t = nat_type(e), id = nnat++;
    char a[32], b[32];
    sprintf(name, "n%d", id);
    const char *ct = nat_is_float(t) ? "double" : "long long";
    /* a string is a value_t holding one reference, cleared by whoever
     * takes it */
    if (t == N_STRING) {
        switch (e->type) {
            case EXPR_LITERAL: {
                /* made once, kept for good */
                const sb_string_t *ls = e->literal.sval;
                fprintf(out, "    static sb_string_t *lit%d;\n    if (!lit%d) lit%d = string_alloc(", id, id, id);
                emit_cstring(out, ls->data, ls->len);
                fprintf(out, ", %d);\n", ls->len);
                fprintf(out, "    value_t %s; %s.type = VAL_STRING; %s.sval = lit%d; string_ref(lit%d);\n",
                        name, name, name, id, id);
                return t;
            }
            case EXPR_VARIABLE:
                fprintf(out, "    value_t %s = val_copy(%s);\n", name, var_ptr(e->var.name));
                return t;
            case EXPR_BINARY: {             /* + of two strings */
                if (lazy_concat(e)) {       /* (s$ read after e, as QBasic reads it) */
                    nat_emit(out, e->binary.right, b);
                    nat_emit(out, e->binary.left, a);
                } else {
                    nat_emit(out, e->binary.left, a);
                    nat_emit(out, e->binary.right, b);
                }
                fprintf(out, "    value_t %s; err = val_add(&%s, &%s, &%s); VAL_CLEAR(&%s); VAL_CLEAR(&%s); " CHECK "\n",
                        name, a, b, name, a, b);
                return t;
            }
            default:
                break;              /* an element, a builtin: below */
        }
    }
    switch (e->type) {
        case EXPR_LITERAL:
            if (nat_is_float(t)) fprintf(out, "    double %s = %a;\n", name, e->literal.dval);
            else fprintf(out, "    long long %s = %d;\n", name, e->literal.ival);
            return t;
        case EXPR_VARIABLE:
            fprintf(out, "    %s %s = %s->%s;\n", ct, name, var_ptr(e->var.name), nat_is_float(t) ? "dval" : "ival");
            return t;
        case EXPR_CALL:
            if (deffn_index(e) >= 0) {
                /* a DEF FN inlined, as call_def_fn runs it: each argument
                 * evaluated and stored into its parameter (a C local, of
                 * its name's type) in turn, the body evaluated with the
                 * parameters in scope, the value made the function's type
                 * as val_coerce makes it */
                int fx = deffn_index(e);
                const stmt_t *d = deffns[fx].def;
                for (int i = 0; i < e->call.nargs; i++) {
                    int pt = predict_name(d->def_fn.params[i]);
                    int at = nat_emit(out, e->call.args[i], a);
                    char slot[40];
                    sprintf(slot, "(&fp%d_%d)", id, i);
                    fprintf(out, "    value_t fp%d_%d; fp%d_%d.type = %s;\n", id, i, id, i, nat_valtype(pt));
                    nat_store(out, slot, pt, a, at);
                }
                fnctx[nfnctx].def = d; fnctx[nfnctx].id = id; nfnctx++;
                int bt = nat_emit(out, d->def_fn.body, b);
                nfnctx--;
                if (t == N_DOUBLE && bt == N_DOUBLE)
                    fprintf(out, "    double %s = %s;\n", name, b);
                else {
                    char slot[40];
                    sprintf(slot, "(&fr%d)", id);
                    fprintf(out, "    value_t fr%d; fr%d.type = %s;\n", id, id, nat_valtype(t));
                    nat_store(out, slot, t, b, bt);
                    fprintf(out, "    %s %s = fr%d.%s;\n", ct, name, id, nat_is_float(t) ? "dval" : "ival");
                }
                return t;
            }
            if (nat_strfn(e)) {
                /* LEN, ASC, CHR$, LEFT$, RIGHT$, MID$ through their
                 * typed entries: the arguments evaluated in order, then
                 * converted in order as val_to_integer converts them */
                const char *fn = e->call.name;
                int n = e->call.nargs, at[3];
                char as[3][32];
                for (int i = 0; i < n; i++) at[i] = nat_emit(out, e->call.args[i], as[i]);
                int si = at[0] == N_STRING ? 0 : -1;     /* the string argument */
                fprintf(out, "    int i%d[3] = { 0, 0, 0 }; err = ERR_NONE;\n", id);
                for (int i = 0; i < n; i++) {
                    if (i == si) continue;
                    if (nat_is_float(at[i]))
                        fprintf(out, "    if (!err) err = val_int_of(%s, &i%d[%d]);\n", as[i], id, i);
                    else
                        fprintf(out, "    i%d[%d] = (int)%s;\n", id, i, as[i]);
                }
                if (!strcmp(fn, "LEN") || !strcmp(fn, "ASC")) {
                    if (!strcmp(fn, "LEN"))
                        fprintf(out, "    long long %s = %s.sval->len;\n", name, as[0]);
                    else
                        fprintf(out, "    if (%s.sval->len == 0) err = ERR_ILLEGAL_FUNCTION_CALL;\n"
                                     "    long long %s = (unsigned char)%s.sval->data[0];\n", as[0], name, as[0]);
                } else {
                    fprintf(out, "    value_t %s; %s.type = VAL_INTEGER;\n    if (!err) err = ", name, name);
                    if (!strcmp(fn, "CHR$")) fprintf(out, "sb_chr(i%d[0], &%s);\n", id, name);
                    else if (!strcmp(fn, "LEFT$")) fprintf(out, "sb_left(%s.sval, i%d[1], &%s);\n", as[0], id, name);
                    else if (!strcmp(fn, "RIGHT$")) fprintf(out, "sb_right(%s.sval, i%d[1], &%s);\n", as[0], id, name);
                    else fprintf(out, "sb_mid(%s.sval, i%d[1], i%d[2], %d, &%s);\n", as[0], id, id, n == 3, name);
                }
                if (si == 0) fprintf(out, "    VAL_CLEAR(&%s);\n", as[0]);
                fprintf(out, "    " CHECK "\n");
                return t;
            }
            if (rt_builtin_type(e) != N_NONE) {
                /* builtin_call itself, the arguments as values of
                 * their types; its value checked to be of the type
                 * expected (an internal error otherwise) */
                int n = e->call.nargs, at[4];
                char as[4][32];
                for (int i = 0; i < n; i++) at[i] = nat_emit(out, e->call.args[i], as[i]);
                fprintf(out, "    value_t av%d[%d], rv%d;\n", id, n, id);
                for (int i = 0; i < n; i++) {
                    if (at[i] == N_STRING)
                        fprintf(out, "    av%d[%d] = %s;\n", id, i, as[i]);
                    else
                        fprintf(out, "    av%d[%d].type = %s; av%d[%d].%s = %s;\n", id, i, nat_valtype(at[i]),
                                id, i, nat_is_float(at[i]) ? "dval" : "ival", as[i]);
                }
                fprintf(out, "    err = builtin_call(");
                emit_cstring(out, e->call.name, (int)strlen(e->call.name));
                fprintf(out, ", av%d, %d, &rv%d);\n", id, n, id);
                for (int i = 0; i < n; i++)
                    if (at[i] == N_STRING) fprintf(out, "    VAL_CLEAR(&av%d[%d]);\n", id, i);
                fprintf(out, "    " CHECK "\n    if (rv%d.type != %s) { VAL_CLEAR(&rv%d); err = ERR_INTERNAL; goto fail; }\n",
                        id, nat_valtype(t), id);
                if (t == N_STRING) fprintf(out, "    value_t %s = rv%d;\n", name, id);
                else fprintf(out, "    long long %s = rv%d.ival;\n", name, id);
                return t;
            }
            if (nat_builtin(e)) {
                const char *fn = e->call.name;
                int xt = nat_emit(out, e->call.args[0], a);
                if (!strcmp(fn, "ABS")) {
                    if (nat_is_float(xt)) fprintf(out, "    double %s = fabs(%s);\n", name, a);
                    else {
                        fprintf(out, "    long long %s = %s < 0 ? -%s : %s;\n", name, a, a, a);
                        nat_range(out, name, xt);
                    }
                } else if (!strcmp(fn, "INT") || !strcmp(fn, "FIX")) {
                    if (nat_is_float(xt))
                        fprintf(out, "    double %s = %s(%s);\n", name, !strcmp(fn, "INT") ? "floor" : "trunc", a);
                    else fprintf(out, "    long long %s = %s;\n", name, a);
                } else if (!strcmp(fn, "SGN")) {
                    fprintf(out, "    long long %s = (double)%s > 0 ? 1 : (double)%s < 0 ? -1 : 0;\n", name, a, a);
                } else if (!strcmp(fn, "CINT")) {
                    fprintf(out, "    double %s_r = val_rint((double)%s);\n", name, a);
                    fprintf(out, "    if (!(%s_r >= -32768 && %s_r <= 32767)) { err = ERR_OVERFLOW; goto fail; }\n", name, name);
                    fprintf(out, "    long long %s = (int)%s_r;\n", name, name);
                } else if (!strcmp(fn, "CLNG")) {
                    fprintf(out, "    double %s_r = val_rint((double)%s);\n", name, a);
                    fprintf(out, "    if (%s_r > 2147483647.0 || %s_r < -2147483648.0) { err = ERR_OVERFLOW; goto fail; }\n", name, name);
                    fprintf(out, "    long long %s = (int)%s_r;\n", name, name);
                } else if (!strcmp(fn, "CSNG")) {
                    fprintf(out, "    double %s = (double)(float)(double)%s;\n", name, a);
                } else if (!strcmp(fn, "RND")) {
                    fprintf(out, "    double %s = sb_rnd_of((double)%s, 1);\n", name, a);
                } else if (!strcmp(fn, "CDBL")) {
                    fprintf(out, "    double %s = (double)%s;\n", name, a);
                } else if (!strcmp(fn, "SQR")) {
                    fprintf(out, "    if ((double)%s < 0) { err = ERR_ILLEGAL_FUNCTION_CALL; goto fail; }\n", a);
                    fprintf(out, "    double %s = sqrt((double)%s);\n", name, a);
                } else if (!strcmp(fn, "LOG")) {
                    fprintf(out, "    if ((double)%s <= 0.0) { err = ERR_ILLEGAL_FUNCTION_CALL; goto fail; }\n", a);
                    fprintf(out, "    double %s = log((double)%s);\n", name, a);
                } else {
                    char lc[8];
                    for (int i = 0; fn[i] && i < 7; i++) { lc[i] = (char)(fn[i] - 'A' + 'a'); lc[i + 1] = 0; }
                    if (!strcmp(lc, "atn")) strcpy(lc, "atan");
                    fprintf(out, "    double %s = %s((double)%s);\n", name, lc, a);
                }
                return t;
            }
            if (is_function_call(e)) {
                int n = e->call.nargs, at[16];
                char as[16][32];
                for (int i = 0; i < n; i++) {
                    const expr_t *a = e->call.args[i];
                    at[i] = nat_emit(out, a, as[i]);
                    if (a->type == EXPR_VARIABLE && !a->paren) {
                        /* by reference: its type checked as it is reached */
                        fprintf(out, "    err = eval_arg_check(");
                        emit_cstring(out, e->call.name, (int)strlen(e->call.name));
                        fprintf(out, ", &P[%d], %d, %s); " CHECK "\n", proc_index(e->call.name), i,
                                var_ptr(a->var.name));
                    }
                }
                int pj = proc_index(e->call.name);
                fprintf(out, "    %s %s;\n", ct, name);
                if (native_ok_proc[pj]) {
                    /* its native version, when it may run (no TRACE, its
                     * variables' names no builtin's): arguments by
                     * reference as pointers, by value converted to the
                     * parameter's type as call_proc_values converts them */
                    const stmt_t *pd = NULL;
                    for (const stmt_t *d = cur_program; d; d = d->next)
                        if ((d->type == STMT_FUNC_DEF || d->type == STMT_SUB_DEF) &&
                            strcmp(d->proc_def.name, e->call.name) == 0) { pd = d; break; }
                    fprintf(out, "    if (!trace_enabled && (NFOK[%d] > 0 || (NFOK[%d] == 0 && (NFOK[%d] = eval_names_plain(NFN%d, (int)(sizeof NFN%d / sizeof NFN%d[0]) - 1) ? 1 : -1) > 0))) {\n",
                            pj, pj, pj, pj, pj, pj);
                    fprintf(out, "    value_t q%d[%d], *pp%d[%d], r%d;\n", id, n ? n : 1, id, n ? n : 1, id);
                    for (int i = 0; i < n; i++) {
                        const expr_t *a = e->call.args[i];
                        if (a->type == EXPR_VARIABLE && !a->paren) {
                            fprintf(out, "    pp%d[%d] = %s;\n", id, i, var_ptr(a->var.name));
                        } else {
                            int pt = n_of_val(pd->proc_def.param_types[i]);
                            char slot[48];
                            sprintf(slot, "(&q%d[%d])", id, i);
                            fprintf(out, "    q%d[%d].type = %s;\n", id, i, nat_valtype(pt));
                            nat_store(out, slot, pt, as[i], at[i]);
                            fprintf(out, "    pp%d[%d] = &q%d[%d];\n", id, i, id, i);
                        }
                    }
                    fprintf(out, "    err = NF%d(pp%d, &r%d); " CHECK "\n", pj, id, id);
                    fprintf(out, "    %s = r%d.%s;\n    } else {\n", name, id, nat_is_float(t) ? "dval" : "ival");
                }
                fprintf(out, "    value_t av%d[%d], rv%d, *rf%d[%d];\n", id, n ? n : 1, id, id, n ? n : 1);
                for (int i = 0; i < n; i++) {
                    const expr_t *a = e->call.args[i];
                    fprintf(out, "    av%d[%d].type = %s; av%d[%d].%s = %s;\n", id, i, nat_valtype(at[i]),
                            id, i, nat_is_float(at[i]) ? "dval" : "ival", as[i]);
                    /* a variable not in parentheses: by reference (the
                     * guard has it cached, of its type) */
                    if (a->type == EXPR_VARIABLE && !a->paren)
                        fprintf(out, "    rf%d[%d] = %s;\n", id, i, var_ptr(a->var.name));
                    else
                        fprintf(out, "    rf%d[%d] = NULL;\n", id, i);
                }
                fprintf(out, "    err = eval_call_proc(");
                emit_cstring(out, e->call.name, (int)strlen(e->call.name));
                fprintf(out, ", &P[%d], av%d, rf%d, %d, &rv%d); " CHECK "\n", proc_index(e->call.name), id, id, n, id);
                fprintf(out, "    if (rv%d.type != %s) { VAL_CLEAR(&rv%d); err = ERR_INTERNAL; goto fail; }\n",
                        id, nat_valtype(t), id);
                fprintf(out, "    %s = rv%d.%s;\n", name, id, nat_is_float(t) ? "dval" : "ival");
                if (native_ok_proc[pj]) fprintf(out, "    }\n");
                if (!nat_local) nat_revalidate(out);
                return t;
            }
            {
            int j = arr_index(e->call.name), n = e->call.nargs, ixt[8];
            char ixs[8][32], cvs[8][40], flat[32];
            const char *ix[8];
            for (int i = 0; i < n; i++) {
                int it = nat_emit(out, e->call.args[i], ixs[i]);
                nat_subscript(out, ixs[i], it, cvs[i]);
                ix[i] = cvs[i];
                ixt[i] = N_LONG;
            }
            sprintf(flat, "f%d", id);
            nat_index(out, j, ix, ixt, n, flat);
            if (t == N_STRING)
                fprintf(out, "    value_t %s = val_copy(&AR[%d]->data[%s]);\n", name, j, flat);
            else
                fprintf(out, "    %s %s = AR[%d]->data[%s].%s;\n", ct, name, j, flat, nat_is_float(t) ? "dval" : "ival");
            return t;
            }
        case EXPR_UNARY: {
            int lt = nat_emit(out, e->unary.operand, a);
            if (e->unary.op == OP_NEG) {
                fprintf(out, "    %s %s = -%s;\n", ct, name, a);
                if (!nat_is_float(lt)) nat_range(out, name, lt);
            } else {
                fprintf(out, "    long long %s = ~%s;\n", name, a);
            }
            return t;
        }
        case EXPR_COMPARE: {
            static const char *ops[] = { "==", "!=", "<", ">", "<=", ">=" };
            int lt = nat_emit(out, e->compare.left, a);
            nat_emit(out, e->compare.right, b);
            if (lt == N_STRING) {
                /* as val_compare: strcmp's sign (one string is equal to
                 * itself -- often so, the short ones being shared) */
                fprintf(out, "    int c%d = %s.sval == %s.sval ? 0 : strcmp(%s.sval->data, %s.sval->data); VAL_CLEAR(&%s); VAL_CLEAR(&%s);\n",
                        id, a, b, a, b, a, b);
                fprintf(out, "    long long %s = (c%d %s 0) ? -1 : 0;\n", name, id, ops[e->compare.op]);
                return t;
            }
            /* as val_compare: less, greater, or else equal -- so a NaN
             * compares equal to everything, as in the interpreter */
            fprintf(out, "    long long %s = (((double)%s < (double)%s) ? -1 : ((double)%s > (double)%s) ? 1 : 0) %s 0 ? -1 : 0;\n",
                    name, a, b, a, b, ops[e->compare.op]);
            return t;
        }
        case EXPR_BINARY: {
            int lt = nat_emit(out, e->binary.left, a);
            int rt = nat_emit(out, e->binary.right, b);
            switch (e->binary.op) {
                case OP_ADD: case OP_SUB: case OP_MUL: {
                    char op = e->binary.op == OP_ADD ? '+' : e->binary.op == OP_SUB ? '-' : '*';
                    if (nat_is_float(t))
                        fprintf(out, "    double %s = (double)%s %c (double)%s;\n", name, a, op, b);
                    else {
                        fprintf(out, "    long long %s = %s %c %s;\n", name, a, op, b);
                        nat_range(out, name, t);
                    }
                    return t;
                }
                case OP_DIV:
                    fprintf(out, "    if ((double)%s == 0.0) { err = ERR_DIVISION_BY_ZERO; goto fail; }\n", b);
                    fprintf(out, "    double %s = (double)%s / (double)%s;\n", name, a, b);
                    return t;
                case OP_POW:
                    fprintf(out, "    if (((double)%s == 0.0 && (double)%s < 0.0) || ((double)%s < 0.0 && (double)%s != floor((double)%s))) { err = ERR_ILLEGAL_FUNCTION_CALL; goto fail; }\n",
                            a, b, a, b, b);
                    fprintf(out, "    double %s = pow((double)%s, (double)%s);\n", name, a, b);
                    return t;
                case OP_IDIV: case OP_MOD:
                    fprintf(out, "    if (%s == 0) { err = ERR_DIVISION_BY_ZERO; goto fail; }\n", b);
                    /* 32-bit division (the operands are 32-bit integers);
                     * -1 apart, for INT_MIN \ -1 */
                    if (e->binary.op == OP_IDIV)
                        fprintf(out, "    long long %s = %s == -1 ? -%s : (long long)((int)%s / (int)%s);\n", name, b, a, a, b);
                    else
                        fprintf(out, "    long long %s = %s == -1 ? 0 : (long long)((int)%s %% (int)%s);\n", name, b, a, b);
                    nat_range(out, name, t);
                    return t;
                case OP_AND: fprintf(out, "    long long %s = (int)%s & (int)%s;\n", name, a, b); return t;
                case OP_OR:  fprintf(out, "    long long %s = (int)%s | (int)%s;\n", name, a, b); return t;
                case OP_XOR: fprintf(out, "    long long %s = (int)%s ^ (int)%s;\n", name, a, b); return t;
                case OP_IMP: fprintf(out, "    long long %s = ~(int)%s | (int)%s;\n", name, a, b); return t;
                case OP_EQV: fprintf(out, "    long long %s = ~((int)%s ^ (int)%s);\n", name, a, b); return t;
                default: break;
            }
            (void)lt; (void)rt;
            return t;
        }
        default:
            return t;
    }
}

/* a PRINT item TAB(n) or SPC(n), the builtin (name(args) is a FUNCTION
 * or an array before a builtin) */
static int print_tab_item(const expr_t *e) {
    return e->type == EXPR_CALL && e->call.nargs == 1 &&
           (strcmp(e->call.name, "TAB") == 0 || strcmp(e->call.name, "SPC") == 0) &&
           !is_array_name(e->call.name) && proc_index(e->call.name) < 0;
}

/* the guard: each variable cached and of its predicted type; each array
 * there (as eval_call_array finds it), of its predicted element type,
 * with as many dimensions as it is given subscripts */
static void nat_guard(FILE *out, const uses_t *u) {
    for (int i = 0; i < u->na; i++) {
        int j = u->a[i];
        fprintf(out, "    if (AG[%d] != array_generation) { AR[%d] = eval_call_array(", j, j);
        emit_cstring(out, arrs[j], (int)strlen(arrs[j]));
        fprintf(out, "); AG[%d] = array_generation; }\n", j);
    }
    /* the variables' caches fetched if stale (a lookup, nothing made:
     * a statement run once per call would never find them filled) */
    for (int i = 0; i < u->nv; i++) {
        fprintf(out, "    if (VG[%d] != env_generation || !V[%d]) { V[%d] = eval_var_slot(env, ",
                u->v[i], u->v[i], u->v[i]);
        emit_cstring(out, vars[u->v[i]], (int)strlen(vars[u->v[i]]));
        fprintf(out, "); VG[%d] = env_generation; }\n", u->v[i]);
    }
    for (int i = 0; i < u->nf; i++) {
        fprintf(out, "    if (FDG[%d] != array_generation + 1u) { FD[%d] = eval_def_fn_defined(", u->f[i], u->f[i]);
        emit_cstring(out, deffns[u->f[i]].name, (int)strlen(deffns[u->f[i]].name));
        fprintf(out, "); FDG[%d] = array_generation + 1u; }\n", u->f[i]);
    }
    fprintf(out, "    if (1");
    for (int i = 0; i < u->nf; i++)
        fprintf(out, " && FD[%d]", u->f[i]);
    for (int i = 0; i < u->nv; i++)
        fprintf(out, " && V[%d] && V[%d]->type == %s",
                u->v[i], u->v[i], nat_valtype(predict_name(vars[u->v[i]])));
    for (int i = 0; i < u->na; i++)
        fprintf(out, " && AR[%d] && AR[%d]->elem_type == %s && AR[%d]->ndims == %d",
                u->a[i], u->a[i], nat_valtype(predict_name(arrs[u->a[i]])), u->a[i], u->an[i]);
}

/* the element index of arr(subscripts) into C local flat, the
 * subscripts' natives converted as val_to_integer converts, the bounds
 * checked as calc_flat_index checks them */
static void nat_index(FILE *out, int j, const char **ix, const int *ixt, int n, const char *flat) {
    for (int i = 0; i < n; i++) {
        if (nat_is_float(ixt[i])) {
            fprintf(out, "    if (!(%s < 2147483647.5 && %s >= -2147483648.5)) { err = ERR_OVERFLOW; goto fail; }\n", ix[i], ix[i]);
            fprintf(out, "    int %s_i%d = (int)val_rint(%s);\n", flat, i, ix[i]);
        } else {
            fprintf(out, "    int %s_i%d = (int)%s;\n", flat, i, ix[i]);
        }
    }
    fprintf(out, "    int %s = 0;\n    { int mul = 1;\n", flat);
    for (int d = n - 1; d >= 0; d--)
        fprintf(out, "      { int x = %s_i%d - AR[%d]->lbound[%d]; if (x < 0 || x >= AR[%d]->dims[%d]) { err = ERR_SUBSCRIPT_OUT_OF_RANGE; goto fail; }\n"
                     "        %s += x * mul; mul *= AR[%d]->dims[%d]; }\n",
                flat, d, j, d, j, d, flat, j, d);
    fprintf(out, "    }\n");
}

/* storing native value x of type t into slot V[v], of type st, as
 * val_coerce and store_into do */
static void nat_store(FILE *out, const char *slot, int st, const char *x, int t) {
    if (st == N_STRING || t == N_STRING) {
        /* (both strings: the callers see to it) store_into's copy, the
         * reference taken released */
        fprintf(out, "    if (%s->type == VAL_STRING && %s.type == VAL_STRING) { sb_string_t *o_ = %s->sval; %s->sval = %s.sval; if (o_ && --o_->refcount <= 0) free(o_); }\n"
                     "    else { err = eval_store(%s, &%s); VAL_CLEAR(&%s); " CHECK " }\n",
                slot, x, slot, slot, x, slot, x, x);
        return;
    }
    if (st == N_INT || st == N_LONG) {
        if (nat_is_float(t)) {
            fprintf(out, "    if (!(%s < 2147483647.5 && %s >= -2147483648.5)) { err = ERR_OVERFLOW; goto fail; }\n", x, x);
            fprintf(out, "    { long long q = (long long)val_rint(%s);\n", x);
            if (st == N_INT) nat_range(out, "q", N_INT);
            fprintf(out, "    %s->ival = (int)q; }\n", slot);
        } else {
            if (st == N_INT && t != N_INT) nat_range(out, x, N_INT);
            fprintf(out, "    %s->ival = (int)%s;\n", slot, x);
        }
    } else if (st == N_SINGLE) {
        fprintf(out, "    { double q = (double)%s; if (!(fabs(q) <= 3.4028234663852886e38)) { err = ERR_OVERFLOW; goto fail; }\n"
                     "      %s->dval = (double)(float)q; }\n", x, slot);
    } else {
        fprintf(out, "    { double q = (double)%s; if (!(fabs(q) <= 1.7976931348623157e308)) { err = ERR_OVERFLOW; goto fail; }\n"
                     "      %s->dval = q; }\n", x, slot);
    }
}


/* --- Native procedures: a FUNCTION or SUB whose body is all native --
 * numeric assignments, conditions, FOR/NEXT, EXIT, calls -- gets a C
 * version with its variables in C (NF<j>), beside the env one.  Numeric
 * parameters, no GOTO, GOSUB, SHARED, STATIC,
 * arrays, strings, I/O.  Native call sites run it when they may (no
 * TRACE, none of its variables named as a builtin), the env version
 * otherwise; what it does is what the env version does. --- */

/* a name in a statement list, as a variable or an assignment's target */
typedef struct { const char *name; int seen; } name_seen_t;
static void seen_expr(expr_t *e, void *ctx) {
    name_seen_t *n = ctx;
    if (e->type == EXPR_VARIABLE && strcmp(e->var.name, n->name) == 0) n->seen = 1;
    if (e->type == EXPR_CALL && strcmp(e->call.name, n->name) == 0) n->seen = 1;
}
static void seen_top(expr_t *e, void *ctx) { expr_walk(e, seen_expr, ctx); }
static int names_used(const stmt_t *s, const char *name) {
    name_seen_t n = { name, 0 };
    for (; s && !n.seen; s = s->next) {
        if (s->type == STMT_ASSIGN && strcmp(s->assign.name, name) == 0) return 1;
        stmt_exprs((stmt_t *)s, seen_top, &n);
        switch (s->type) {
            case STMT_IF: if (names_used(s->if_stmt.then_body, name) || names_used(s->if_stmt.else_body, name)) return 1; break;
            case STMT_FOR: if (names_used(s->for_stmt.body, name)) return 1; break;
            case STMT_WHILE: if (names_used(s->while_stmt.body, name)) return 1; break;
            case STMT_DO_LOOP: if (names_used(s->do_loop.body, name)) return 1; break;
            case STMT_SELECT:
                for (const case_clause_t *cl = s->select_stmt.clauses; cl; cl = cl->next)
                    if (names_used(cl->body, name)) return 1;
                break;
            default: break;
        }
    }
    return n.seen;
}

/* an ON ERROR GOTO anywhere in s: a program that sets a handler has no
 * native procedures -- an error in one runs the handler with it still
 * there, and RESUME returns into it, which only the env version does */
static int sets_handler(const stmt_t *s) {
    for (; s; s = s->next) {
        switch (s->type) {
            case STMT_ON_ERROR: if (s->on_error.label[0]) return 1; break;
            case STMT_IF: if (sets_handler(s->if_stmt.then_body) || sets_handler(s->if_stmt.else_body)) return 1; break;
            case STMT_FOR: if (sets_handler(s->for_stmt.body)) return 1; break;
            case STMT_WHILE: if (sets_handler(s->while_stmt.body)) return 1; break;
            case STMT_DO_LOOP: if (sets_handler(s->do_loop.body)) return 1; break;
            case STMT_SELECT:
                for (const case_clause_t *cl = s->select_stmt.clauses; cl; cl = cl->next)
                    if (sets_handler(cl->body)) return 1;
                break;
            case STMT_SUB_DEF: case STMT_FUNC_DEF: if (sets_handler(s->proc_def.body)) return 1; break;
            default: break;
        }
    }
    return 0;
}

static int native_eligible(const stmt_t *def, const code_t *c) {
    int ok = 1;
    char bare[64];
    /* (FUNCTION f% whose body names f: f% too, which its C locals are not) */
    if (ast_name_bare(def->proc_def.name, bare) && names_used(def->proc_def.body, bare)) return 0;
    for (int i = 0; i < def->proc_def.nparams; i++)
        if (n_of_val(def->proc_def.param_types[i]) == N_NONE ||
            predict_name(def->proc_def.params[i]) == N_NONE ||
            strncmp(def->proc_def.params[i], "FN", 2) == 0)
            return 0;
    if (def->type == STMT_FUNC_DEF && (predict_name(def->proc_def.name) == N_NONE ||
                                       predict_name(def->proc_def.name) == N_STRING)) return 0;
    nat_local = 1;
    ldef = def;
    for (int k = 0; k < c->nops && ok; k++) {
        const op_t *op = &c->ops[k];
        const stmt_t *s = op->s;
        switch (op->kind) {
            case OP_JUMP: case OP_EXITFOR: case OP_NEXT:
                break;
            case OP_JFALSE:
                ok = nat_type(op->cond) != N_NONE;
                break;
            case OP_FOR:
                ok = op->target >= 0 && var_ntype(s->for_stmt.var_name) != N_NONE &&
                     strncmp(s->for_stmt.var_name, "FN", 2) != 0 &&
                     nat_type(s->for_stmt.start) != N_NONE && nat_type(s->for_stmt.end) != N_NONE &&
                     (!s->for_stmt.step || nat_type(s->for_stmt.step) != N_NONE);
                break;
            case OP_STMT:
                if (s->type == STMT_ASSIGN)
                    ok = var_ntype(s->assign.name) != N_NONE &&
                         strncmp(s->assign.name, "FN", 2) != 0 &&
                         nat_type(s->assign.value) != N_NONE;
                else if (s->type == STMT_EXIT)
                    ok = s->exit_stmt.what == EXIT_FUNCTION || s->exit_stmt.what == EXIT_SUB;
                else
                    ok = 0;
                break;
            default:
                ok = 0;
        }
    }
    nat_local = 0;
    return ok;
}

static void nat_goto(FILE *out, const code_t *c, int t) {
    if (t < 0 || t >= c->nops) fprintf(out, "goto done;");
    else fprintf(out, "goto NL%d;", t);
}

static void emit_native_proc(FILE *out, int j, const stmt_t *def, const code_t *c) {
    char *body = NULL;
    size_t blen = 0;
    FILE *b = open_memstream(&body, &blen);
    nat_local = 1;
    ldef = def;
    for (int i = 0; i < nlnames; i++) free(lnames[i]);
    nlnames = 0;
    if (def->type == STMT_FUNC_DEF) local_index(def->proc_def.name);   /* L0: its value */
    for (int k = 0; k < c->nops; k++) {
        const op_t *op = &c->ops[k];
        const stmt_t *s = op->s;
        char x[32], y[32], z[32];
        fprintf(b, "NL%d:;\n", k);
        switch (op->kind) {
            case OP_JUMP:
                fprintf(b, "    "); nat_goto(b, c, op->target); fprintf(b, "\n");
                break;
            case OP_EXITFOR:
                fprintf(b, "    "); nat_goto(b, c, op->target); fprintf(b, "\n");
                break;
            case OP_JFALSE:
                nat_emit(b, op->cond, x);
                fprintf(b, "    if ((%s != 0) %s 0) ", x, op->invert ? "==" : "!=");
                nat_goto(b, c, k + 1);
                fprintf(b, " else ");
                nat_goto(b, c, op->target);
                fprintf(b, "\n");
                break;
            case OP_STMT:
                if (s->type == STMT_ASSIGN) {
                    int t = nat_emit(b, s->assign.value, x);
                    nat_store(b, var_ptr(s->assign.name), var_ntype(s->assign.name), x, t);
                } else {
                    fprintf(b, "    goto done;\n");          /* EXIT FUNCTION/SUB */
                }
                break;
            case OP_FOR: {
                /* as for_start: start, limit, step evaluated, each as a
                 * double; the variable set (converted to its type) and
                 * read back; the test */
                const char *vp = var_ptr(s->for_stmt.var_name);
                int vt = var_ntype(s->for_stmt.var_name);
                int t0 = nat_emit(b, s->for_stmt.start, x);
                int t1 = nat_emit(b, s->for_stmt.end, y);
                int t2 = N_INT;
                if (s->for_stmt.step) t2 = nat_emit(b, s->for_stmt.step, z);
                else strcpy(z, "1");
                (void)t1; (void)t2;
                fprintf(b, "    FL%d = (double)%s; FS%d = (double)%s;\n", k, y, k, z);
                fprintf(b, "    { double st%d = (double)%s;\n", k, x);
                char sv[32];
                sprintf(sv, "st%d", k);
                nat_store(b, vp, vt, sv, N_DOUBLE);
                fprintf(b, "    double cnt = (double)%s->%s;\n", vp, nat_is_float(vt) ? "dval" : "ival");
                fprintf(b, "    if (!(FS%d >= 0 ? cnt <= FL%d : cnt >= FL%d)) ", k, k, k);
                nat_goto(b, c, op->target);
                fprintf(b, " }\n");
                (void)t0;
                break;
            }
            case OP_NEXT: {
                /* as for_next, by the variable's type */
                int f = op->slot;
                const stmt_t *fs = c->ops[f].s;
                const char *vp = var_ptr(fs->for_stmt.var_name);
                int vt = var_ntype(fs->for_stmt.var_name);
                fprintf(b, "    { double v;\n");
                if (vt == N_INT || vt == N_LONG) {
                    fprintf(b, "    v = (double)%s->ival + FS%d;\n", vp, f);
                    fprintf(b, "    if (v == (double)(int)v && v <= 2147483647.0 && v >= -2147483648.0%s) %s->ival = (int)v;\n",
                            vt == N_INT ? " && v >= -32768.0 && v <= 32767.0" : "", vp);
                    fprintf(b, "    else {\n");
                    nat_store(b, vp, vt, "v", N_DOUBLE);
                    fprintf(b, "    v = (double)%s->ival; }\n", vp);
                } else if (vt == N_SINGLE) {
                    fprintf(b, "    v = (double)(float)(%s->dval + FS%d); %s->dval = v;\n", vp, f, vp);
                } else {
                    fprintf(b, "    v = %s->dval + FS%d; %s->dval = v;\n", vp, f, vp);
                }
                fprintf(b, "    if (FS%d >= 0 ? v <= FL%d : v >= FL%d) ", f, f, f);
                nat_goto(b, c, f + 1);
                fprintf(b, " }\n");
                break;
            }
            default:
                break;
        }
    }
    fclose(b);
    fprintf(out, "static error_t NF%d(value_t **p, value_t *ret) {\n    error_t err = eval_enter();\n", j);
    fprintf(out, "    if (err != ERR_NONE) return err;\n");
    for (int i = 0; i < nlnames && i < 256; i++) {
        int t = predict_name(lnames[i]);
        fprintf(out, "    value_t L%d; L%d.type = %s; L%d.%s = 0;\n", i, i, nat_valtype(t), i,
                nat_is_float(t) ? "dval" : "ival");
    }
    for (int k = 0; k < c->nops; k++)
        if (c->ops[k].kind == OP_FOR) fprintf(out, "    double FL%d = 0, FS%d = 0;\n", k, k);
    fprintf(out, "    (void)p; (void)ret;\n");
    fwrite(body, 1, blen, out);
    free(body);
    fprintf(out, "done:\n");
    if (def->type == STMT_FUNC_DEF) fprintf(out, "    *ret = L0;\n");
    fprintf(out, "    eval_leave();\n    return ERR_NONE;\nfail:\n    eval_leave();\n    return err;\n}\n\n");
    nat_local = 0;
}

/* the names a native procedure's variables have, for eval_names_plain */
static void emit_native_names(FILE *out, int j, const stmt_t *def) {
    int n = 0;
    fprintf(out, "static const char *const NFN%d[] = {", j);
    for (int i = 0; i < def->proc_def.nparams; i++, n++) {
        emit_cstring(out, def->proc_def.params[i], (int)strlen(def->proc_def.params[i]));
        fprintf(out, ", ");
    }
    for (int i = 0; i < nlnames && i < 256; i++, n++) {
        emit_cstring(out, lnames[i], (int)strlen(lnames[i]));
        fprintf(out, ", ");
    }
    fprintf(out, "\"\" };\n");
    nf_nnames[j] = n;
}

/* --- Statements --- */

static void emit_preamble(FILE *out, const op_t *op, int k) {
    fprintf(out, "    pc = %d; cur_basic_line = %d; if (trace_enabled) eval_trace(c->ops[%d].s);\n",
            k, op->basic_line, k);
}

static void emit_generic_assign(FILE *out, const stmt_t *s, int k) {
    char path[1024];
    {
            /* the checks (CONST, OPTION EXPLICIT) before the value, as
             * the executor; a variable that passed them once (and is
             * kept) passes them for good -- but V[] is the reads' cache
             * too, and a CONST read is cached: a name some CONST defines
             * is checked every time */
            int v = var_index(s->assign.name);
            if (is_const_name(s->assign.name))
                fprintf(out, "    { err = eval_assign_check(env, ");
            else
                fprintf(out, "    if (VG[%d] != env_generation || !V[%d]) { err = eval_assign_check(env, ", v, v);
            emit_cstring(out, s->assign.name, (int)strlen(s->assign.name));
            fprintf(out, "); " CHECK " }\n");
            snprintf(path, sizeof path, "c->ops[%d].s->assign.value", k);
            int t = emit_expr(out, s->assign.value, path);
            if (is_const_name(s->assign.name)) {
                /* (V[] may hold it from a read: a CONST, a STRING * n,
                 * is stored by eval_assign_store every time) */
                fprintf(out, "    err = eval_assign_store(env, ");
                emit_cstring(out, s->assign.name, (int)strlen(s->assign.name));
                fprintf(out, ", &T[%d]);\n    VAL_CLEAR(&T[%d]); " CHECK "\n", t, t);
                return;
            }
            fprintf(out, "    if (VG[%d] != env_generation || !V[%d]) { V[%d] = eval_assign_slot(env, ", v, v, v);
            emit_cstring(out, s->assign.name, (int)strlen(s->assign.name));
            fprintf(out, "); VG[%d] = env_generation; }\n", v);
            fprintf(out, "    if (V[%d]) err = eval_store(V[%d], &T[%d]); else err = eval_assign_store(env, ", v, v, t);
            emit_cstring(out, s->assign.name, (int)strlen(s->assign.name));
            fprintf(out, ", &T[%d]);\n    VAL_CLEAR(&T[%d]); " CHECK "\n", t, t);
    }
}

static void emit_generic_array_assign(FILE *out, const stmt_t *s, int k) {
    char path[1024];
    {
            int n = s->array_assign.nindices;
            fprintf(out, "    { int ix[8];\n");
            /* the value, then the subscripts (QBasic's order, exec_array_assign) */
            snprintf(path, sizeof path, "c->ops[%d].s->array_assign.value", k);
            int t = emit_expr(out, s->array_assign.value, path);
            for (int i = 0; i < n; i++) {
                snprintf(path, sizeof path, "c->ops[%d].s->array_assign.indices[%d]", k, i);
                int u = emit_expr(out, s->array_assign.indices[i], path);
                fprintf(out, "    err = val_to_integer(&T[%d], &ix[%d]); VAL_CLEAR(&T[%d]); "
                        "if (err != ERR_NONE) { VAL_CLEAR(&T[%d]); } " CHECK "\n", u, i, u, t);
            }
            fprintf(out, "    err = eval_array_store(");
            emit_cstring(out, s->array_assign.name, (int)strlen(s->array_assign.name));
            fprintf(out, ", %d, ix, &T[%d]); VAL_CLEAR(&T[%d]); " CHECK " }\n", n, t, t);
    }
}

/* a statement compiled here: 1, or 0 for the executor's */
static int emit_stmt(FILE *out, const code_t *c, const op_t *op, int k) {
    const stmt_t *s = op->s;
    char path[1024];
    switch (s->type) {
        case STMT_ASSIGN: {
            int st = predict_name(s->assign.name), vt = nat_type(s->assign.value);
            if (st != N_NONE && vt != N_NONE && (st == N_STRING) == (vt == N_STRING) &&
                !is_const_name(s->assign.name)) {
                uses_t u;
                u.nv = u.na = u.nf = 0;
                u.v[u.nv++] = var_index(s->assign.name);
                nat_uses(s->assign.value, &u);
                if (u.nv <= 64 && u.na <= 16) {
                    char x[32], slot[32];
                    emit_preamble(out, op, k);
                    nat_guard(out, &u);
                    fprintf(out, ") {\n");
                    fprintf(out, "    unsigned sg = env_generation, sa = array_generation; (void)sg; (void)sa;\n");
                    cur_uses = &u;
                    int t = nat_emit(out, s->assign.value, x);
                    sprintf(slot, "V[%d]", u.v[0]);
                    nat_store(out, slot, st, x, t);
                    fprintf(out, "    goto L%d_done;\n    }\n", k);
                    emit_generic_assign(out, s, k);
                    fprintf(out, "L%d_done:;\n", k);
                    return 1;
                }
            }
            emit_preamble(out, op, k);
            emit_generic_assign(out, s, k);
            return 1;
        }

        case STMT_ARRAY_ASSIGN: {
            int n = s->array_assign.nindices;
            int vt0 = nat_type(s->array_assign.value);
            int st = predict_name(s->array_assign.name), native = st != N_NONE && n >= 1 && n <= 8 &&
                     vt0 != N_NONE && (st == N_STRING) == (vt0 == N_STRING);
            for (int i = 0; native && i < n; i++)
                if (nat_type(s->array_assign.indices[i]) == N_NONE ||
                    nat_type(s->array_assign.indices[i]) == N_STRING) native = 0;
            emit_preamble(out, op, k);
            if (native) {
                /* the value, then the subscripts, then the bounds, as
                 * exec_array_assign and eval_array_store */
                uses_t u;
                u.nv = u.na = u.nf = 0;
                int j = arr_index(s->array_assign.name);
                u.a[u.na] = j; u.an[u.na] = n; u.na++;
                for (int i = 0; i < n; i++) nat_uses(s->array_assign.indices[i], &u);
                nat_uses(s->array_assign.value, &u);
                if (u.nv <= 64 && u.na <= 16) {
                    char ixs[8][32], x[32], flat[32], slot[64];
                    const char *ix[8];
                    int ixt[8];
                    nat_guard(out, &u);
                    fprintf(out, ") {\n");
                    fprintf(out, "    unsigned sg = env_generation, sa = array_generation; (void)sg; (void)sa;\n");
                    cur_uses = &u;
                    /* the value, then each subscript converted as it is
                     * evaluated (QBasic's order, exec_array_assign) */
                    char cv[8][40];
                    const char *cix[8];
                    int cixt[8];
                    int t = nat_emit(out, s->array_assign.value, x);
                    for (int i = 0; i < n; i++) {
                        ixt[i] = nat_emit(out, s->array_assign.indices[i], ixs[i]);
                        ix[i] = ixs[i];
                        nat_subscript(out, ix[i], ixt[i], cv[i]);
                        cix[i] = cv[i];
                        cixt[i] = N_LONG;
                    }
                    sprintf(flat, "fa%d", k);
                    nat_index(out, j, cix, cixt, n, flat);
                    sprintf(slot, "(&AR[%d]->data[%s])", j, flat);
                    nat_store(out, slot, st, x, t);
                    fprintf(out, "    goto L%d_done;\n    }\n", k);
                    emit_generic_array_assign(out, s, k);
                    fprintf(out, "L%d_done:;\n", k);
                    return 1;
                }
            }
            emit_generic_array_assign(out, s, k);
            return 1;
        }

        case STMT_PRINT: {
            if (s->print.using_fmt || s->print.using_expr) return 0;
            int nl = 1;
            emit_preamble(out, op, k);
            /* native: every item's value has a native version; each item
             * evaluated and printed in turn, as exec_print */
            uses_t u;
            int native = s->print.nitems > 0;
            u.nv = u.na = u.nf = 0;
            for (int i = 0; native && i < s->print.nitems; i++) {
                const expr_t *x = s->print.items[i].expr;
                if (!x) continue;
                if (print_tab_item(x)) {
                    /* TAB(n), SPC(n): n native and numeric */
                    int at = nat_type(x->call.args[0]);
                    if (at == N_NONE || at == N_STRING) native = 0;
                    else nat_uses(x->call.args[0], &u);
                } else if (nat_type(x) == N_NONE) native = 0;
                else nat_uses(x, &u);
            }
            native = native && u.nv <= 64 && u.na <= 16;
            if (native) {
                nat_guard(out, &u);
                fprintf(out, ") {\n");
                fprintf(out, "    unsigned sg = env_generation, sa = array_generation; (void)sg; (void)sa;\n");
                cur_uses = &u;
                for (int i = 0; i < s->print.nitems; i++) {
                    const print_item_t *it = &s->print.items[i];
                    if (it->expr && print_tab_item(it->expr)) {
                        /* the argument converted as val_to_integer
                         * converts it, the spaces written */
                        char x[32];
                        int at = nat_emit(out, it->expr->call.args[0], x);
                        fprintf(out, "    { int tn;\n");
                        if (nat_is_float(at))
                            fprintf(out, "      err = val_int_of(%s, &tn); " CHECK "\n", x);
                        else
                            fprintf(out, "      tn = (int)%s;\n", x);
                        fprintf(out, "      print_tab_spc(%d, tn); }\n", it->expr->call.name[0] == 'S');
                    } else if (it->expr) {
                        char x[32];
                        int t = nat_emit(out, it->expr, x);
                        int tabspc = it->expr->type == EXPR_CALL &&
                            (strcmp(it->expr->call.name, "TAB") == 0 || strcmp(it->expr->call.name, "SPC") == 0);
                        if (t == N_STRING)
                            fprintf(out, "    err = print_value(&%s, %d, console_width); VAL_CLEAR(&%s); " CHECK "\n",
                                    x, tabspc, x);
                        else
                            fprintf(out, "    { value_t pv; pv.type = %s; pv.%s = %s;\n"
                                         "      err = print_value(&pv, 0, console_width); " CHECK " }\n",
                                    nat_valtype(t), nat_is_float(t) ? "dval" : "ival", x);
                    }
                    if (it->sep == ',') fprintf(out, "    print_comma(console_width);\n");
                }
                if (s->print.items[s->print.nitems - 1].sep != ';' && s->print.items[s->print.nitems - 1].sep != ',')
                    fprintf(out, "    print_text(\"\\n\");\n");
                fprintf(out, "    goto L%d_done;\n    }\n", k);
            }
            for (int i = 0; i < s->print.nitems; i++) {
                const print_item_t *it = &s->print.items[i];
                if (it->expr) {
                    snprintf(path, sizeof path, "c->ops[%d].s->print.items[%d].expr", k, i);
                    int t = emit_expr(out, it->expr, path);
                    int tabspc = it->expr->type == EXPR_CALL &&
                        (strcmp(it->expr->call.name, "TAB") == 0 || strcmp(it->expr->call.name, "SPC") == 0);
                    fprintf(out, "    err = print_value(&T[%d], %d, console_width); VAL_CLEAR(&T[%d]); " CHECK "\n",
                            t, tabspc, t);
                }
                if (it->sep == ';') nl = 0;
                else if (it->sep == ',') { fprintf(out, "    print_comma(console_width);\n"); nl = 0; }
                else nl = 1;
            }
            if (nl) fprintf(out, "    print_text(\"\\n\");\n");
            if (native) fprintf(out, "L%d_done:;\n", k);
            return 1;
        }
        case STMT_CALL: {
            /* CALL of one of the program's SUBs or FUNCTIONs, the
             * argument count right (else the executor's error first) */
            int j = proc_index(s->call_stmt.name), n = s->call_stmt.nargs, ta[16];
            if (j < 0 || procs[j].nparams != n || n > 16) return 0;
            if (!args_compilable(s->call_stmt.args, n)) return 0;
            emit_preamble(out, op, k);
            snprintf(path, sizeof path, "c->ops[%d].s->call_stmt.args", k);
            int rid = emit_args(out, s->call_stmt.args, n, path, ta, s->call_stmt.name, j);
            fprintf(out, "    { value_t av[%d];\n", n ? n : 1);
            for (int i = 0; i < n; i++)
                fprintf(out, "      av[%d] = T[%d]; T[%d].type = VAL_INTEGER;\n", i, ta[i], ta[i]);
            fprintf(out, "      err = eval_call_proc(");
            emit_cstring(out, s->call_stmt.name, (int)strlen(s->call_stmt.name));
            fprintf(out, ", &P[%d], av, R%d, %d, NULL); " CHECK " }\n", j, rid, n);
            emit_refix(out, s->call_stmt.args, n);
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
        case STMT_RETURN:
            /* a plain RETURN to its GOSUB's next op; anything else (a
             * label, no GOSUB open) the executor's, which follows */
            if (s->goto_stmt.label[0]) return 0;
            emit_preamble(out, op, k);
            fprintf(out, "    if (act_return(&a, &pc)) goto dispatch;\n");
            return 0;
        default:
            return 0;
    }
}

/* A FOR whose counter and bounds have native types: what for_start
 * does -- start, limit and step evaluated in turn, the start stored in
 * the counter as for_set stores it, the counter read back -- natively,
 * and act_for_enter's frame; the executor's when the guard fails (the
 * generic op follows) */
static void emit_native_for(FILE *out, const code_t *c, const op_t *op, int k) {
    const stmt_t *s = op->s;
    const char *var = s->for_stmt.var_name;
    int st = predict_name(var);
    if (st == N_NONE || st == N_STRING || is_const_name(var) || op->target < 0) return;
    const expr_t *ex[3] = { s->for_stmt.start, s->for_stmt.end, s->for_stmt.step };
    for (int i = 0; i < 3; i++)
        if (ex[i] && (nat_type(ex[i]) == N_NONE || nat_type(ex[i]) == N_STRING)) return;
    uses_t u;
    u.nv = u.na = u.nf = 0;
    u.v[u.nv++] = var_index(var);
    for (int i = 0; i < 3; i++)
        if (ex[i]) nat_uses(ex[i], &u);
    if (u.nv > 64 || u.na > 16) return;
    emit_preamble(out, op, k);
    nat_guard(out, &u);
    fprintf(out, ") {\n");
    fprintf(out, "    unsigned sg = env_generation, sa = array_generation; (void)sg; (void)sa;\n");
    cur_uses = &u;
    char x[3][32], slot[32];
    for (int i = 0; i < 3; i++)
        if (ex[i]) {
            nat_emit(out, ex[i], x[i]);
            fprintf(out, "    double fd%d_%d = (double)%s;\n", k, i, x[i]);
            sprintf(x[i], "fd%d_%d", k, i);
        }
    if (!ex[2]) strcpy(x[2], "1.0");
    sprintf(slot, "V[%d]", u.v[0]);
    nat_store(out, slot, st, x[0], N_DOUBLE);
    fprintf(out, st == N_INT || st == N_LONG ? "    { double fc = (double)V[%d]->ival;\n"
                                             : "    { double fc = V[%d]->dval;\n", u.v[0]);
    fprintf(out, "      int runs = %s >= 0 ? fc <= %s : fc >= %s;\n", x[2], x[1], x[1]);
    fprintf(out, "      err = act_for_enter(&a, c->ops[%d].s, runs, %s, %s, %d, V[%d]); " CHECK "\n",
            k, x[1], x[2], k + 1, u.v[0]);
    fprintf(out, "      if (!runs) ");
    emit_goto(out, c, op->target);
    fprintf(out, " else ");
    emit_goto(out, c, k + 1);
    fprintf(out, " }\n    }\n");
}

/* one unit, the main program or a SUB's or FUNCTION's body, as a C
 * function with the executor's signature (main_fn_t); its variable
 * caches are its own, per call (a recursive call has its own variables) */
static void emit_unit(FILE *out, const code_t *c, const char *fname, int is_main) {
    char *body = NULL;
    size_t blen = 0;
    FILE *b = open_memstream(&body, &blen);
    ntemp = maxtemp = 0;
    for (int i = 0; i < nvars; i++) free(vars[i]);
    nvars = 0;
    for (int i = 0; i < narrs; i++) free(arrs[i]);
    narrs = 0;
    long *off = malloc((size_t)(c->nops + 1) * sizeof *off);
    ngt = 0;
    gt_rec = 1;
    for (int k = 0; k < c->nops; k++) {
        const op_t *op = &c->ops[k];
        cur_op = k;
        ntemp = 0;
        fflush(b);
        off[k] = (long)blen;
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
            uses_t u;
            u.nv = u.na = u.nf = 0;
            int ct0 = nat_type(op->cond);
            if (ct0 != N_NONE && ct0 != N_STRING)
                nat_uses(op->cond, &u);
            if (ct0 != N_NONE && ct0 != N_STRING && u.nv <= 64 && u.na <= 16) {
                char x[32];
                nat_guard(b, &u);
                fprintf(b, ") {\n");
                fprintf(b, "    unsigned sg = env_generation, sa = array_generation; (void)sg; (void)sa;\n");
                cur_uses = &u;
                nat_emit(b, op->cond, x);
                fprintf(b, "    if ((%s != 0) %s 0) ", x, op->invert ? "==" : "!=");
                emit_goto(b, c, k + 1);
                fprintf(b, " else ");
                emit_goto(b, c, op->target);
                fprintf(b, "\n    }\n");
            }
            int t = emit_expr(b, op->cond, path);
            fprintf(b, "    { int y = val_is_true(&T[%d]); VAL_CLEAR(&T[%d]);\n      if (y %s 0) ",
                    t, t, op->invert ? "==" : "!=");
            emit_goto(b, c, k + 1);
            fprintf(b, " else ");
            emit_goto(b, c, op->target);
            fprintf(b, " }\n");
            continue;
        }
        if (op->kind == OP_STMT && emit_stmt(b, c, op, k))
            continue;
        if (op->kind == OP_FOR)
            emit_native_for(b, c, op, k);
        if (op->kind == OP_NEXT) {
            /* the loop on top is this NEXT's own FOR, its variable's
             * value cached: stepped here as for_next steps it, by the
             * variable's type; anything else is the executor's */
            fprintf(b, "    { for_slot_t *fr = a.nfors ? &a.fors[a.nfors - 1] : NULL;\n"
                       "      if (fr && fr->s == c->ops[%d].s && fr->var && fr->gen == env_generation) {\n"
                       "        value_t *cv = fr->var; double v; int go = 0;\n"
                       "        if (cv->type == VAL_INTEGER || cv->type == VAL_LONG) {\n"
                       "          v = (double)cv->ival + fr->step;\n"
                       "          if (v == (double)(int)v && v <= 2147483647.0 && v >= -2147483648.0 &&\n"
                       "              (cv->type == VAL_LONG || (v >= -32768.0 && v <= 32767.0))) { cv->ival = (int)v; go = 1; }\n"
                       "        } else if (cv->type == VAL_SINGLE) {\n"
                       "          v = (double)(float)(cv->dval + fr->step); cv->dval = v; go = 1;\n"
                       "        } else if (cv->type == VAL_DOUBLE) {\n"
                       "          v = cv->dval + fr->step; cv->dval = v; go = 1;\n"
                       "        }\n"
                       "        if (go) {\n"
                       "          if (fr->step >= 0 ? v <= fr->limit : v >= fr->limit) ", k);
            emit_goto(b, c, op->slot + 1);
            fprintf(b, "\n          a.nfors--; ");
            emit_goto(b, c, k + 1);
            fprintf(b, "\n        } } }\n");
        }
        fprintf(b, "    pc = %d; if ((err = act_step(&a, &pc)) != ERR_NONE) goto fail;", k);
        /* (which kind of op and statement the executor runs, for the
         * reader of the C) */
        fprintf(b, " /* op %d stmt %d */", (int)op->kind, op->s ? (int)op->s->type : -1);
        /* a simple statement goes on to the next op; anything else to
         * whichever op it chose */
        if (op->kind != OP_STMT)
            fprintf(b, " if (pc != %d) goto dispatch;", k + 1);
        fprintf(b, "\n");
    }
    fclose(b);
    gt_rec = 0;
    off[c->nops] = (long)blen;
    if (maxtemp == 0) maxtemp = 1;
    /* The ops, cut into chunks of about chunk_bytes of C, each its own
     * function: one function of a whole program's ops is more than a
     * compiler that keeps a table per instruction can hold (stage08 ran
     * out on the larger games).  The unit's locals are a frame the
     * chunks share (F, the names #defined over it, so the ops' C is as
     * it was); a jump within a chunk is a goto, one out of it a stub
     * that sets pc and returns to the unit's dispatch, which calls the
     * chunk holding pc.  S32SBC_CHUNK sets the size; 0, one chunk. */
    long chunk_bytes = 32768;
    { const char *e = getenv("S32SBC_CHUNK"); if (e) chunk_bytes = atol(e); }
    int *chunk = malloc((size_t)(c->nops ? c->nops : 1) * sizeof *chunk), nchunks = 0;
    int *cstart = malloc((size_t)(c->nops + 1) * sizeof *cstart);
    for (int k = 0; k < c->nops; k++) {
        if (k == 0 || (chunk_bytes > 0 && off[k] - off[cstart[nchunks - 1]] >= chunk_bytes))
            cstart[nchunks++] = k;
        chunk[k] = nchunks - 1;
    }
    if (nchunks == 0) cstart[nchunks++] = 0;  /* no ops: one empty chunk (an array needs one) */
    cstart[nchunks] = c->nops;
    fprintf(out, "struct %s_f {\n    act_t a;\n    value_t T[%d];\n    value_t *V[%d];\n    unsigned VG[%d];\n"
                 "    sb_array_t *AR[%d];\n    unsigned AG[%d];\n    env_t *env;\n    code_t *c;\n"
                 "    int pc;\n    error_t err, result;\n};\n",
            fname, maxtemp, nvars ? nvars : 1, nvars ? nvars : 1, narrs ? narrs : 1, narrs ? narrs : 1);
    fprintf(out, "#define a (F->a)\n#define T (F->T)\n#define V (F->V)\n#define VG (F->VG)\n#define AR (F->AR)\n"
                 "#define AG (F->AG)\n#define env (F->env)\n#define c (F->c)\n#define pc (F->pc)\n"
                 "#define err (F->err)\n#define result (F->result)\n");
    char *seen = calloc((size_t)(c->nops ? c->nops : 1), 1);
    for (int j = 0; j < nchunks; j++) {
        int k0 = cstart[j], k1 = cstart[j + 1];
        /* returns 0 done, 1 dispatch (pc set), 2 failed (err set) */
        fprintf(out, "static int %s_c%d(struct %s_f *F) {\n    switch (pc) {\n", fname, j, fname);
        for (int k = k0; k < k1; k++) fprintf(out, "    case %d: goto L%d;\n", k, k);
        fprintf(out, "    default: goto dispatch;\n    }\n");
        fwrite(body + off[k0], 1, (size_t)(off[k1] - off[k0]), out);
        if (k1 < c->nops) fprintf(out, "    pc = %d; goto dispatch;\n", k1);
        else fprintf(out, "    goto done;\n");
        memset(seen, 0, (size_t)c->nops);
        for (int g = 0; g < ngt; g++)
            if (gt_from[g] >= k0 && gt_from[g] < k1 && (gt_to[g] < k0 || gt_to[g] >= k1) && !seen[gt_to[g]]) {
                seen[gt_to[g]] = 1;
                fprintf(out, "L%d: pc = %d; goto dispatch;\n", gt_to[g], gt_to[g]);
            }
        fprintf(out, "fail: return 2;\ndispatch: return 1;\ndone: return 0;\n}\n");
    }
    fprintf(out, "#undef a\n#undef T\n#undef V\n#undef VG\n#undef AR\n#undef AG\n#undef env\n#undef c\n"
                 "#undef pc\n#undef err\n#undef result\n");
    fprintf(out, "static int (*const %s_cf[])(struct %s_f *) = {", fname, fname);
    for (int j = 0; j < nchunks; j++) fprintf(out, "%s %s_c%d", j ? "," : "", fname, j);
    fprintf(out, " };\nstatic const int %s_ck[] = {", fname);
    for (int k = 0; k < c->nops; k++) fprintf(out, "%s%s%d", k ? "," : "", k % 20 ? " " : "\n    ", chunk[k]);
    if (!c->nops) fprintf(out, " 0");
    fprintf(out, " };\n");
    fprintf(out,
            "static error_t %s(env_t *env, code_t *c) {\n"
            "    struct %s_f FR;\n"
            "    FR.env = env;\n"
            "    FR.c = c;\n"
            "    FR.result = act_begin(&FR.a, env, c, %d);\n"
            "    memset(FR.T, 0, sizeof FR.T);\n"
            "    memset(FR.V, 0, sizeof FR.V);\n"
            "    memset(FR.VG, 0, sizeof FR.VG);\n"
            "    memset(FR.AR, 0, sizeof FR.AR);\n"
            "    memset(FR.AG, 0xff, sizeof FR.AG);\n"
            "    if (FR.result != ERR_NONE) return FR.result;\n"
            "    FR.pc = 0;\n"
            "    while (FR.pc >= 0 && FR.pc < %d) {\n"
            "        int r = %s_cf[%s_ck[FR.pc]](&FR);\n"
            "        if (r == 0) break;\n"
            "        if (r == 2) {\n"
            "            for (int i = 0; i < %d; i++) VAL_CLEAR(&FR.T[i]);\n"
            "            if (!act_transfer(&FR.a, FR.err, &FR.pc, &FR.result)) break;\n"
            "        }\n"
            "    }\n"
            "    act_end(&FR.a);\n"
            "    return FR.result;\n"
            "}\n\n",
            fname, fname, is_main, c->nops, fname, fname, maxtemp);
    free(body);
    free(off);
    free(chunk);
    free(cstart);
    free(seen);
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
    scan_array_names(program);
    scan_program(program, 1);
    cur_program = program;
    code_t *c = lower_stmts(program, &err);
    if (!c) {
        fprintf(stderr, "%s: %s\n", in, error_message(err));
        return 1;
    }

    FILE *out = fopen(outpath, "w");
    if (!out) { fprintf(stderr, "s32-sbc: cannot write %s\n", outpath); return 1; }
    fprintf(out, "/* generated by s32-sbc from %s */\n#include \"sbcrt.h\"\n#include <string.h>\n#include <math.h>\n\n", in);
    emit_source(out, text);
    fprintf(out, "static void *P[%d];   /* each SUB's and FUNCTION's entry, found once */\n",
            nprocs_known ? nprocs_known : 1);
    fprintf(out, "static int NFOK[%d];  /* native version usable: 0 not known yet, 1, -1 */\n\n",
            nprocs_known ? nprocs_known : 1);
    fprintf(out, "static int FD[%d];        /* each DEF FN inlined: defined (as of FDG - 1) */\n"
                 "static unsigned FDG[%d];\n\n", ndeffns ? ndeffns : 1, ndeffns ? ndeffns : 1);
    /* which procedures have a native version; their names and prototypes,
     * written first (any unit may call them) */
    code_t *pcodes[1024];
    {
        int jx = 0, traps = sets_handler(program);
        for (stmt_t *d = program; d; d = d->next) {
            if (d->type != STMT_SUB_DEF && d->type != STMT_FUNC_DEF) continue;
            if (jx >= 1024) break;
            pcodes[jx] = lower_stmts(d->proc_def.body, &err);
            if (!pcodes[jx]) {
                fprintf(stderr, "%s: %s: %s\n", in, d->proc_def.name, error_message(err));
                return 1;
            }
            native_ok_proc[jx] = !traps && native_eligible(d, pcodes[jx]);
            jx++;
        }
        /* the native bodies: written to a buffer now (their names known
         * before any call site is written), emitted after the prototypes */
        jx = 0;
        char *nbody = NULL;
        size_t nlen = 0;
        FILE *nb = open_memstream(&nbody, &nlen);
        for (stmt_t *d = program; d; d = d->next) {
            if (d->type != STMT_SUB_DEF && d->type != STMT_FUNC_DEF) continue;
            if (jx >= 1024) break;
            if (native_ok_proc[jx]) {
                fprintf(out, "static error_t NF%d(value_t **p, value_t *ret);\n", jx);
                emit_native_proc(nb, jx, d, pcodes[jx]);
                emit_native_names(out, jx, d);
            }
            jx++;
        }
        fclose(nb);
        fprintf(out, "\n");
        fwrite(nbody, 1, nlen, out);
        free(nbody);
    }
    emit_unit(out, c, "sbc_main", 1);
    /* each SUB and FUNCTION: its body lowered as the runtime will */
    int nprocs = 0, procnops[1024];
    for (stmt_t *d = program; d; d = d->next) {
        if (d->type != STMT_SUB_DEF && d->type != STMT_FUNC_DEF) continue;
        code_t *pc = lower_stmts(d->proc_def.body, &err);
        if (!pc) {
            fprintf(stderr, "%s: %s: %s\n", in, d->proc_def.name, error_message(err));
            return 1;
        }
        char fname[32];
        snprintf(fname, sizeof fname, "sbc_proc_%d", nprocs);
        emit_unit(out, pc, fname, 0);
        if (nprocs < 1024) procnops[nprocs] = pc->nops;   /* for the registration */
        code_free(pc);
        nprocs++;
    }
    fprintf(out, "int main(void) {\n");
    nprocs = 0;
    for (stmt_t *d = program; d; d = d->next) {
        if (d->type != STMT_SUB_DEF && d->type != STMT_FUNC_DEF) continue;
        fprintf(out, "    eval_compiled_proc(");
        emit_cstring(out, d->proc_def.name, (int)strlen(d->proc_def.name));
        fprintf(out, ", sbc_proc_%d, %d);\n", nprocs, nprocs < 1024 ? procnops[nprocs] : -1);
        nprocs++;
    }
    fprintf(out,
            "    return sbcrt_main(sbc_source, %d, sbc_main);\n"
            "}\n", c->nops);
    if (fclose(out) != 0) { fprintf(stderr, "s32-sbc: cannot write %s\n", outpath); return 1; }
    code_free(c);
    stmt_free(program);
    free(text);
    return 0;
}
