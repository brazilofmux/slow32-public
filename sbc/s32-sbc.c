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
static void emit_goto(FILE *out, const code_t *c, int t) {
    if (t >= c->nops || t < 0) fprintf(out, "goto done;");
    else fprintf(out, "goto L%d;", t);
}

/* --- Expressions: C on temporaries T[], calling the interpreter's
 * operations.  path is the C that reaches the node at run time (from
 * the op's statement), for what is not compiled: eval_expr on it. --- */

static int ntemp, maxtemp;
static int cur_op;

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

static void scan_array_names(const stmt_t *s) {
    for (; s; s = s->next) {
        switch (s->type) {
            case STMT_DIM: note_array_name(s->dim_stmt.name); break;
            case STMT_ARRAY_ASSIGN: note_array_name(s->array_assign.name); break;
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
            int u = emit_expr(out, bin ? e->binary.left : e->compare.left, l);
            int w = emit_expr(out, bin ? e->binary.right : e->compare.right, r);
            fprintf(out, "    err = eval_%s(%d, &T[%d], &T[%d], &T[%d]); VAL_CLEAR(&T[%d]); VAL_CLEAR(&T[%d]); " CHECK "\n",
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

/* --- Native code (milestone 2): a numeric expression on C long long and
 * double, the interpreter's rules for each type written out (value.c).
 * A statement's native version runs when its guard holds -- every
 * variable it names cached, of the type its name predicts -- and its
 * generic version otherwise.  Types: */
enum { N_NONE = -1, N_INT = 0, N_LONG, N_SINGLE, N_DOUBLE };

static int predict_name(const char *name) {
    size_t n = strlen(name);
    char last = n ? name[n - 1] : 0;
    if (strcmp(name, "ERR") == 0 || strcmp(name, "ERL") == 0) return N_NONE;
    if (last == '%') return N_INT;
    if (last == '&') return N_LONG;
    if (last == '#') return N_DOUBLE;
    if (last == '$') return N_NONE;
    return N_SINGLE;            /* the guard checks: DEF* may say otherwise */
}

static int nat_is_float(int t) { return t == N_SINGLE || t == N_DOUBLE; }

/* the native type of e, or N_NONE when it is not done natively */
static int nat_type(const expr_t *e) {
    int l, r;
    switch (e->type) {
        case EXPR_LITERAL:
            switch (e->literal.type) {
                case VAL_INTEGER: return N_INT;
                case VAL_LONG:    return N_LONG;
                case VAL_SINGLE:  return N_SINGLE;
                case VAL_DOUBLE:  return N_DOUBLE;
                default:          return N_NONE;
            }
        case EXPR_VARIABLE:
            return predict_name(e->var.name);
        case EXPR_CALL:
            if (!is_array_name(e->call.name) || e->call.nargs < 1 || e->call.nargs > 8)
                return N_NONE;
            for (int i = 0; i < e->call.nargs; i++)
                if (nat_type(e->call.args[i]) == N_NONE) return N_NONE;
            return predict_name(e->call.name);
        case EXPR_UNARY:
            l = nat_type(e->unary.operand);
            if (l == N_NONE) return N_NONE;
            if (e->unary.op == OP_NOT && nat_is_float(l)) return N_NONE;
            return l;
        case EXPR_COMPARE:
            l = nat_type(e->compare.left);
            r = nat_type(e->compare.right);
            return (l == N_NONE || r == N_NONE) ? N_NONE : N_INT;
        case EXPR_BINARY:
            l = nat_type(e->binary.left);
            r = nat_type(e->binary.right);
            if (l == N_NONE || r == N_NONE) return N_NONE;
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
} uses_t;

static void nat_uses(const expr_t *e, uses_t *u);

static void nat_vars(const expr_t *e, int *list, int *n);

static void nat_uses(const expr_t *e, uses_t *u) {
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
    static const char *names[] = { "VAL_INTEGER", "VAL_LONG", "VAL_SINGLE", "VAL_DOUBLE" };
    return names[t];
}

static int nnat;

/* an integer range check of a long long r for type t */
static void nat_range(FILE *out, const char *r, int t) {
    if (t == N_INT)
        fprintf(out, "    if (%s < -32768 || %s > 32767) { err = ERR_OVERFLOW; goto fail; }\n", r, r);
    else
        fprintf(out, "    if (%s < -2147483648LL || %s > 2147483647LL) { err = ERR_OVERFLOW; goto fail; }\n", r, r);
}

static void nat_index(FILE *out, int j, const char **ix, const int *ixt, int n, const char *flat);

/* code computing e into a fresh C local; its name in *name */
static int nat_emit(FILE *out, const expr_t *e, char *name) {
    int t = nat_type(e), id = nnat++;
    char a[32], b[32];
    sprintf(name, "n%d", id);
    const char *ct = nat_is_float(t) ? "double" : "long long";
    switch (e->type) {
        case EXPR_LITERAL:
            if (nat_is_float(t)) fprintf(out, "    double %s = %a;\n", name, e->literal.dval);
            else fprintf(out, "    long long %s = %d;\n", name, e->literal.ival);
            return t;
        case EXPR_VARIABLE: {
            int v = var_index(e->var.name);
            fprintf(out, "    %s %s = V[%d]->%s;\n", ct, name, v, nat_is_float(t) ? "dval" : "ival");
            return t;
        }
        case EXPR_CALL: {
            int j = arr_index(e->call.name), n = e->call.nargs, ixt[8];
            char ixs[8][32], flat[32];
            const char *ix[8];
            for (int i = 0; i < n; i++) {
                ixt[i] = nat_emit(out, e->call.args[i], ixs[i]);
                ix[i] = ixs[i];
            }
            sprintf(flat, "f%d", id);
            nat_index(out, j, ix, ixt, n, flat);
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
            nat_emit(out, e->compare.left, a);
            nat_emit(out, e->compare.right, b);
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
    fprintf(out, "    if (1");
    for (int i = 0; i < u->nv; i++)
        fprintf(out, " && VG[%d] == env_generation && V[%d] && V[%d]->type == %s",
                u->v[i], u->v[i], u->v[i], nat_valtype(predict_name(vars[u->v[i]])));
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
            fprintf(out, "    int %s_i%d = (int)rint(%s);\n", flat, i, ix[i]);
        } else {
            fprintf(out, "    int %s_i%d = (int)%s;\n", flat, i, ix[i]);
        }
    }
    fprintf(out, "    int %s = 0;\n    { int mul = 1;\n", flat);
    for (int d = n - 1; d >= 0; d--)
        fprintf(out, "      { int x = %s_i%d - AR[%d]->base; if (x < 0 || x >= AR[%d]->dims[%d]) { err = ERR_SUBSCRIPT_OUT_OF_RANGE; goto fail; }\n"
                     "        %s += x * mul; mul *= AR[%d]->dims[%d]; }\n",
                flat, d, j, j, d, flat, j, d);
    fprintf(out, "    }\n");
}

/* storing native value x of type t into slot V[v], of type st, as
 * val_coerce and store_into do */
static void nat_store(FILE *out, const char *slot, int st, const char *x, int t) {
    if (st == N_INT || st == N_LONG) {
        if (nat_is_float(t)) {
            fprintf(out, "    if (!(%s < 2147483647.5 && %s >= -2147483648.5)) { err = ERR_OVERFLOW; goto fail; }\n", x, x);
            fprintf(out, "    { long long q = (long long)rint(%s);\n", x);
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
             * kept) passes them for good */
            int v = var_index(s->assign.name);
            fprintf(out, "    if (VG[%d] != env_generation || !V[%d]) { err = eval_assign_check(env, ", v, v);
            emit_cstring(out, s->assign.name, (int)strlen(s->assign.name));
            fprintf(out, "); " CHECK " }\n");
            snprintf(path, sizeof path, "c->ops[%d].s->assign.value", k);
            int t = emit_expr(out, s->assign.value, path);
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
            for (int i = 0; i < n; i++) {
                snprintf(path, sizeof path, "c->ops[%d].s->array_assign.indices[%d]", k, i);
                int u = emit_expr(out, s->array_assign.indices[i], path);
                fprintf(out, "    err = val_to_integer(&T[%d], &ix[%d]); VAL_CLEAR(&T[%d]); " CHECK "\n", u, i, u);
            }
            snprintf(path, sizeof path, "c->ops[%d].s->array_assign.value", k);
            int t = emit_expr(out, s->array_assign.value, path);
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
            int st = predict_name(s->assign.name);
            if (st != N_NONE && nat_type(s->assign.value) != N_NONE) {
                uses_t u;
                u.nv = u.na = 0;
                u.v[u.nv++] = var_index(s->assign.name);
                nat_uses(s->assign.value, &u);
                if (u.nv <= 64 && u.na <= 16) {
                    char x[32], slot[32];
                    emit_preamble(out, op, k);
                    nat_guard(out, &u);
                    fprintf(out, ") {\n");
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
            int st = predict_name(s->array_assign.name), native = st != N_NONE && n >= 1 && n <= 8 &&
                     nat_type(s->array_assign.value) != N_NONE;
            for (int i = 0; native && i < n; i++)
                if (nat_type(s->array_assign.indices[i]) == N_NONE) native = 0;
            emit_preamble(out, op, k);
            if (native) {
                /* the subscripts, then the value, then the bounds, as
                 * exec_array_assign and eval_array_store */
                uses_t u;
                u.nv = u.na = 0;
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
                    for (int i = 0; i < n; i++) {
                        ixt[i] = nat_emit(out, s->array_assign.indices[i], ixs[i]);
                        ix[i] = ixs[i];
                    }
                    /* the subscripts' conversions come before the value */
                    char cv[8][32];
                    const char *cix[8];
                    int cixt[8];
                    for (int i = 0; i < n; i++) {
                        if (nat_is_float(ixt[i])) {
                            fprintf(out, "    if (!(%s < 2147483647.5 && %s >= -2147483648.5)) { err = ERR_OVERFLOW; goto fail; }\n", ix[i], ix[i]);
                            sprintf(cv[i], "%s_c", ix[i]);
                            fprintf(out, "    long long %s = (long long)rint(%s);\n", cv[i], ix[i]);
                        } else {
                            strcpy(cv[i], ix[i]);
                        }
                        cix[i] = cv[i];
                        cixt[i] = N_LONG;
                    }
                    int t = nat_emit(out, s->array_assign.value, x);
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
            uses_t u;
            u.nv = u.na = 0;
            if (nat_type(op->cond) != N_NONE)
                nat_uses(op->cond, &u);
            if (nat_type(op->cond) != N_NONE && u.nv <= 64 && u.na <= 16) {
                char x[32];
                nat_guard(b, &u);
                fprintf(b, ") {\n");
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
        /* a simple statement goes on to the next op; anything else to
         * whichever op it chose */
        if (op->kind != OP_STMT)
            fprintf(b, " if (pc != %d) goto dispatch;", k + 1);
        fprintf(b, "\n");
    }
    fclose(b);
    if (maxtemp == 0) maxtemp = 1;
    fprintf(out,
            "static error_t %s(env_t *env, code_t *c) {\n"
            "    act_t a;\n"
            "    value_t T[%d];\n"
            "    value_t *V[%d];\n"
            "    unsigned VG[%d];\n"
            "    sb_array_t *AR[%d];\n"
            "    unsigned AG[%d];\n"
            "    error_t err, result = act_begin(&a, env, c, %d);\n"
            "    int pc = 0;\n"
            "    memset(T, 0, sizeof T);\n"
            "    memset(V, 0, sizeof V);\n"
            "    memset(VG, 0, sizeof VG);\n"
            "    memset(AR, 0, sizeof AR);\n"
            "    memset(AG, 0xff, sizeof AG);\n"
            "    if (result != ERR_NONE) return result;\n",
            fname, maxtemp, nvars ? nvars : 1, nvars ? nvars : 1,
            narrs ? narrs : 1, narrs ? narrs : 1, is_main);
    fwrite(body, 1, blen, out);
    free(body);
    fprintf(out,
            "    goto done;\n"
            "fail:\n"
            "    for (int i = 0; i < %d; i++) VAL_CLEAR(&T[i]);\n"
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
    scan_array_names(program);
    code_t *c = lower_stmts(program, &err);
    if (!c) {
        fprintf(stderr, "%s: %s\n", in, error_message(err));
        return 1;
    }

    FILE *out = fopen(outpath, "w");
    if (!out) { fprintf(stderr, "s32-sbc: cannot write %s\n", outpath); return 1; }
    fprintf(out, "/* generated by s32-sbc from %s */\n#include \"sbcrt.h\"\n#include <string.h>\n#include <math.h>\n\n", in);
    emit_source(out, text);
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
