/* The compilable dialect's checker: see strict.h. */
#include "strict.h"
#include "builtin.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* a name and where: labels, jumps, arrays, FN uses */
typedef struct {
    char name[64];
    int seq;            /* the statement's place in the text */
    int line;
    int kind;           /* jumps: 1 a GOTO-like transfer, 0 a reference only;
                         * DEF FNs and procedures, their parameters; FN uses, the arguments;
                         * DIMs, 1 after an ERASE */
    int n;              /* arrays: the subscripts (DIM, use) */
} ref_t;

typedef struct { ref_t *v; int n, cap; } refs_t;

typedef struct { int from, to; } range_t;   /* a FOR..NEXT, by seq */

typedef struct {
    refs_t labels, jumps;
    refs_t dims, uses;          /* arrays: DIMs, and uses */
    refs_t sdims;               /* the main program's DIMs with constant bounds (static arrays) */
    refs_t bounds;              /* arrays LBOUND or UBOUND names */
    refs_t redims;              /* arrays REDIMmed (which declares one too) */
    refs_t consts;              /* its CONSTs */
    refs_t vars;                /* names used as variables (and records) */
    refs_t asdims;              /* DIM x AS type's names */
    refs_t erased;              /* arrays ERASEd, which may be DIMmed again */
    refs_t options;             /* OPTION BASE statements */
    int base;                   /* the OPTION BASE in force */
    range_t *fors; int nfors, capfors;
} unit_t;

typedef struct {
    strict_report_fn report;
    void *rctx;
    int seq;
    int findings;
    int info[STRICT_NINFO];
    refs_t procs;               /* SUB and FUNCTION names */
    refs_t fndefs, fnuses;      /* DEF FN, program-wide */
    refs_t dimshared;           /* arrays the main program DIMs SHARED */
    refs_t modconsts;           /* the main program's CONSTs, every unit's */
    refs_t modjumps;            /* RESTORE's and ON ERROR GOTO's labels, anywhere: the main program's */
    refs_t staticdims;          /* arrays the main program's DIMs of constant bounds made, so far */
    unit_t *u;                  /* the unit being walked */
    stmt_t *proc;               /* its SUB or FUNCTION, NULL for the main program */
    struct open_for *st;        /* the FORs open in the text, the innermost last */
    int depth;                  /* statement lists open inside the unit's own */
    int nst, capst;
} check_t;

static const char *info_names[STRICT_NINFO] = {
    "gosub", "on-error", "resume", "resume-next", "erl", "clear", "goto-out-of-for",
    "goto-into-for", "fn-hoisted", "fn-args",
};

const char *strict_info_name(int i) { return i >= 0 && i < STRICT_NINFO ? info_names[i] : "?"; }

static void push(refs_t *r, const char *name, int seq, int line, int kind) {
    if (r->n == r->cap) {
        r->cap = r->cap ? r->cap * 2 : 16;
        r->v = realloc(r->v, (size_t)r->cap * sizeof(ref_t));
        if (!r->v) { fprintf(stderr, "sbasic: out of memory\n"); exit(1); }
    }
    ref_t *x = &r->v[r->n++];
    strncpy(x->name, name, 63);
    x->name[63] = '\0';
    x->seq = seq;
    x->line = line;
    x->kind = kind;
    x->n = 0;
}

static ref_t *find(refs_t *r, const char *name) {
    for (int i = 0; i < r->n; i++)
        if (strcmp(r->v[i].name, name) == 0) return &r->v[i];
    return NULL;
}

/* a SUB or FUNCTION by name, or by its name without a suffix (QB:
 * FUNCTION f% is f too) */
static ref_t *find_proc_ref(refs_t *procs, const char *name) {
    ref_t *p = find(procs, name);
    char bare[64];
    for (int i = 0; !p && i < procs->n; i++)
        if (ast_name_bare(procs->v[i].name, bare) && strcmp(bare, name) == 0) p = &procs->v[i];
    return p;
}

static void finding(check_t *c, int line, const char *code, const char *detail) {
    c->findings++;
    if (c->report) c->report(c->rctx, line, code, detail);
}

static void add_for(unit_t *u, int from, int to) {
    if (u->nfors == u->capfors) {
        u->capfors = u->capfors ? u->capfors * 2 : 16;
        u->fors = realloc(u->fors, (size_t)u->capfors * sizeof(range_t));
        if (!u->fors) { fprintf(stderr, "sbasic: out of memory\n"); exit(1); }
    }
    u->fors[u->nfors].from = from;
    u->fors[u->nfors].to = to;
    u->nfors++;
}

/* --- expressions: array uses, FN uses, ERL --- */

static int is_fn_name(const char *n) { return n[0] == 'F' && n[1] == 'N' && n[2]; }

/* the interpreter's own functions, no QuickBASIC's: a program may name a
 * variable MAX, as QuickBASIC lets it */
static int extension_name(const char *n) {
    static const char *const ext[] = {
        "ATN2", "ASIN", "ACOS", "SINH", "COSH", "TANH", "ASINH", "ACOSH", "ATANH", "LOG10",
        "LOG2", "ROUND", "MIN", "MAX", "FLOOR", "CEIL", "INSTRREV", "TRIM$", "BIN$",
        "REPLACE$", "REVERSE$", "DIR$", "CURDIR$", "FILEEXISTS", NULL };
    for (int i = 0; ext[i]; i++)
        if (strcmp(ext[i], n) == 0) return 1;
    return 0;
}

static void scan_expr(expr_t *e, void *vc) {
    check_t *c = vc;
    if (e->type == EXPR_VARIABLE) {
        int mn, mx;
        if (strcmp(e->var.name, "ERL") == 0) c->info[STRICT_ERL]++;
        if (builtin_arity(e->var.name, &mn, &mx) && mn > 0 && !extension_name(e->var.name)) {   /* TAN for TAN(X): QB "Expected: (" */
            char d[128];
            snprintf(d, sizeof d, "%s without its arguments", e->var.name);
            finding(c, e->line, "builtin-args", d);
        }
        if (is_fn_name(e->var.name)) push(&c->fnuses, e->var.name, c->seq, e->line, 0);
        else if (!find(&c->u->vars, e->var.name)) push(&c->u->vars, e->var.name, c->seq, e->line, 0);
    } else if (e->type == EXPR_FIELD_ACCESS) {
        /* (an element's: the element, an EXPR_CALL, is walked as an array's use) */
        if (!e->field.elem && !find(&c->u->vars, e->field.var_name))
            push(&c->u->vars, e->field.var_name, c->seq, e->line, 0);
    } else if (e->type == EXPR_CALL) {
        int mn, mx;
        if (is_fn_name(e->call.name)) push(&c->fnuses, e->call.name, c->seq, e->line, e->call.nargs);
        else if (builtin_arity(e->call.name, &mn, &mx)) {
            if (e->call.nargs < mn || (mx >= 0 && e->call.nargs > mx)) {
                char d[128];
                if (mx < 0) snprintf(d, sizeof d, "%s with %d arguments, at least %d", e->call.name, e->call.nargs, mn);
                else if (mn == mx) snprintf(d, sizeof d, "%s with %d arguments, not %d", e->call.name, e->call.nargs, mn);
                else snprintf(d, sizeof d, "%s with %d arguments, not %d to %d", e->call.name, e->call.nargs, mn, mx);
                finding(c, e->line, "builtin-args", d);
            }
        } else if (strcmp(e->call.name, "LBOUND") == 0 || strcmp(e->call.name, "UBOUND") == 0 ||
                   strcmp(e->call.name, "JOIN$") == 0) {
            /* the interpreter's own functions (eval.c), on an array named */
            expr_t *a = e->call.nargs ? e->call.args[0] : NULL;
            if (a && a->type == EXPR_VARIABLE) push(&c->u->bounds, a->var.name, c->seq, e->line, 0);
            if (a && a->type == EXPR_CALL && a->call.nargs == 0)
                push(&c->u->bounds, a->call.name, c->seq, e->line, 0);
        } else if (e->call.nargs > 0 && !find_proc_ref(&c->procs, e->call.name)) {
            push(&c->u->uses, e->call.name, c->seq, e->line, 0);
            c->u->uses.v[c->u->uses.n - 1].n = e->call.nargs;
        }
    }
}

static void scan_top(expr_t *e, void *vc) { expr_walk(e, scan_expr, vc); }

/* --- statements --- */

/* a FOR open in the text */
typedef struct open_for { char var[64]; int seq; int line; } open_for_t;

static void walk_unit(check_t *c, stmt_t *body, stmt_t *proc);

/* One statement list.  FOR and NEXT pair as QuickBASIC pairs them at load
 * time (QBasic 1.1 and QB64 agree, 2026-11-03): a NEXT closes the
 * innermost FOR open in its own block -- the one named, which must be
 * that one; NEXT K, J closes K then J -- and a block ends with none
 * open.  So `IF ... THEN PRINT X$;: NEXT Z` is "NEXT without FOR", and
 * so is a NEXT that would skip an inner FOR; a FOR left open is "FOR
 * without NEXT".  Jumps into and out of a FOR are allowed. */
/* name used as a variable here (an assignment's, FOR's, INPUT's target) */
static void var_use(check_t *c, const char *name, int line) {
    if (!find(&c->u->vars, name)) push(&c->u->vars, name, c->seq, line, 0);
}

/* DIM x AS type: a declaration, before x is used, once (QB: "Duplicate
 * definition" either way) */
static void as_dim(check_t *c, const char *name, int line) {
    char d[128];
    ref_t *r;
    if ((r = find(&c->u->asdims, name))) {
        snprintf(d, sizeof d, "DIM %s AS again (first at line %d)", name, r->line);
        finding(c, line, "dim-twice", d);
    } else if ((r = find(&c->u->vars, name))) {
        snprintf(d, sizeof d, "%s used at line %d before its DIM AS", name, r->line);
        finding(c, line, "dim-after-use", d);
    }
    push(&c->u->asdims, name, c->seq, line, 0);
}

/* name, a CONST of this unit or the main program's, given a value: QB
 * refuses it at load ("Duplicate definition", "Variable required") */
static void const_target(check_t *c, const char *name, int line) {
    char d[128];
    if (find(&c->u->consts, name) || find(&c->modconsts, name)) {
        snprintf(d, sizeof d, "%s is a CONST", name);
        finding(c, line, "const-assign", d);
    }
}

static const char *const_value(check_t *c, const expr_t *e);

/* a DIM's bounds all constant (a static array, QB's) -- and not an
 * array COMMON SHARED declares, which is dynamic */
static int dim_static(check_t *c, const stmt_t *s) {
    if (find(&c->dimshared, s->dim_stmt.name) && !s->dim_stmt.shared) return 0;
    for (int i = 0; i < s->dim_stmt.ndims; i++)
        if ((s->dim_stmt.dims[i] && const_value(c, s->dim_stmt.dims[i])) ||
            (s->dim_stmt.lower[i] && const_value(c, s->dim_stmt.lower[i])))
            return 0;
    return 1;
}

/* what a CONST's value may be (QB: "Invalid constant", and "Illegal
 * function call" for a string operator): literals and CONSTs, joined by
 * numeric operators and comparisons; why not, else NULL */
static const char *const_value(check_t *c, const expr_t *e) {
    const char *why;
    switch (e->type) {
    case EXPR_LITERAL:
        return NULL;
    case EXPR_VARIABLE:
        return find(&c->u->consts, e->var.name) || find(&c->modconsts, e->var.name)
               ? NULL : "names no CONST";
    case EXPR_UNARY:
        return const_value(c, e->unary.operand);
    case EXPR_COMPARE:
        if ((why = const_value(c, e->compare.left))) return why;
        return const_value(c, e->compare.right);
    case EXPR_BINARY:
        if (e->binary.op == OP_STRCAT) return "joins strings";
        if ((why = const_value(c, e->binary.left))) return why;
        if ((why = const_value(c, e->binary.right))) return why;
        if ((e->binary.left->type == EXPR_LITERAL && e->binary.left->literal.type == VAL_STRING) ||
            (e->binary.right->type == EXPR_LITERAL && e->binary.right->literal.type == VAL_STRING))
            return "joins strings";
        return NULL;
    default:
        return "calls a function";
    }
}

static void walk_list(check_t *c, stmt_t *s) {
    int base = c->nst;
    char d[128];
    c->depth++;
    for (; s; s = s->next) {
        int seq = ++c->seq;
        stmt_exprs(s, scan_top, c);
        switch (s->type) {
        case STMT_OPTION_EXPLICIT: case STMT_SPLIT: case STMT_TRACE:
            /* the interpreter's own; QuickBASIC will not load them */
            snprintf(d, sizeof d, "%s is no QuickBASIC statement",
                     s->type == STMT_SPLIT ? "SPLIT" : s->type == STMT_TRACE ? "TRACE" : "OPTION EXPLICIT");
            finding(c, s->line, "not-qb", d);
            break;
        case STMT_GFX_GET: case STMT_GFX_PUT:
            /* (the array named alone: one that must be DIMmed, as LBOUND's) */
            if (s->gfx.target && s->gfx.target->call.nargs == 0)
                push(&c->u->bounds, s->gfx.target->call.name, seq, s->line, 0);
            break;
        case STMT_TYPE_DEF:
            /* QuickBASIC's TYPEs hold fixed-length strings only */
            for (int i = 0; i < s->type_def.nfields; i++)
                if (s->type_def.fields[i].type == VAL_STRING && !s->type_def.fields[i].fixlen) {
                    snprintf(d, sizeof d, "TYPE %s's field %s a STRING without * its length",
                             s->type_def.name, s->type_def.fields[i].name);
                    finding(c, s->line, "not-qb", d);
                }
            break;
        case STMT_CONST: {
            const char *why = const_value(c, s->const_stmt.value);
            if (why) {
                snprintf(d, sizeof d, "CONST %s's value %s", s->const_stmt.name, why);
                finding(c, s->line, "const-value", d);
            }
            push(&c->u->consts, s->const_stmt.name, seq, s->line, 0);
            break;
        }
        case STMT_ASSIGN:
            const_target(c, s->assign.name, s->line);
            var_use(c, s->assign.name, s->line);
            break;
        case STMT_INPUT:
            for (int i = 0; i < s->input.nvars; i++) {
                const_target(c, s->input.varnames[i], s->line);
                if (!s->input.targets[i]) var_use(c, s->input.varnames[i], s->line);
            }
            break;
        case STMT_READ:
            for (int i = 0; i < s->read_stmt.nvars; i++) {
                const_target(c, s->read_stmt.varnames[i], s->line);
                if (!s->read_stmt.targets[i]) var_use(c, s->read_stmt.varnames[i], s->line);
            }
            break;
        case STMT_FIELD_ASSIGN:
            if (!s->field_assign.elem) var_use(c, s->field_assign.var_name, s->line);
            break;
        case STMT_DIM_SCALAR:
            as_dim(c, s->dim_scalar.name, s->line);
            break;
        case STMT_DIM_AS_TYPE:
            as_dim(c, s->dim_as_type.name, s->line);
            break;
        case STMT_LABEL:
            if (find(&c->u->labels, s->label.name)) {
                snprintf(d, sizeof d, "label %s defined twice", s->label.name);
                finding(c, s->line, "label-twice", d);
            } else
                push(&c->u->labels, s->label.name, seq, s->line, 0);
            break;
        case STMT_FOR:
            const_target(c, s->for_stmt.var_name, s->line);
            var_use(c, s->for_stmt.var_name, s->line);
            if (c->nst == c->capst) {
                c->capst = c->capst ? c->capst * 2 : 8;
                c->st = realloc(c->st, (size_t)c->capst * sizeof(open_for_t));
                if (!c->st) { fprintf(stderr, "sbasic: out of memory\n"); exit(1); }
            }
            strcpy(c->st[c->nst].var, s->for_stmt.var_name);
            c->st[c->nst].seq = seq;
            c->st[c->nst].line = s->line;
            c->nst++;
            if (s->for_stmt.body) walk_list(c, s->for_stmt.body);
            break;
        case STMT_NEXT: {
            int k = s->next_stmt.nvars ? s->next_stmt.nvars : 1;
            for (int i = 0; i < k; i++) {
                const char *v = s->next_stmt.nvars ? s->next_stmt.vars[i] : NULL;
                if (c->nst > base && (!v || strcmp(c->st[c->nst - 1].var, v) == 0)) {
                    c->nst--;
                    add_for(c->u, c->st[c->nst].seq, seq);
                    continue;
                }
                if (c->nst > base)
                    snprintf(d, sizeof d, "NEXT %s where FOR %s (line %d) is the one open",
                             v, c->st[c->nst - 1].var, c->st[c->nst - 1].line);
                else
                    snprintf(d, sizeof d, "NEXT %s closes no FOR of its own block", v ? v : "");
                finding(c, s->line, "next-without-for", d);
                break;
            }
            break;
        }
        case STMT_GOTO:
            push(&c->u->jumps, s->goto_stmt.label, seq, s->line, 1);
            break;
        case STMT_GOSUB:
            c->info[STRICT_GOSUB]++;
            push(&c->u->jumps, s->goto_stmt.label, seq, s->line, 1);
            break;
        case STMT_RETURN:               /* RETURN label: the main program's only */
            if (!s->goto_stmt.label[0]) break;
            if (c->proc) {
                snprintf(d, sizeof d, "RETURN %s in a SUB or FUNCTION", s->goto_stmt.label);
                finding(c, s->line, "not-in-proc", d);    /* (QB: Illegal in procedure or DEF FN) */
            }
            push(&c->u->jumps, s->goto_stmt.label, seq, s->line, 1);
            break;
        case STMT_ON_GOTO:
        case STMT_ON_GOSUB:
            if (s->type == STMT_ON_GOSUB) c->info[STRICT_GOSUB]++;
            for (int i = 0; i < s->on_branch.nlabels; i++)
                push(&c->u->jumps, s->on_branch.labels[i], seq, s->line, 1);
            break;
        case STMT_ON_ERROR:
            c->info[STRICT_ON_ERROR]++;
            if (s->on_error.label[0])   /* (its handler the module's, as RESTORE's label) */
                push(&c->modjumps, s->on_error.label, seq, s->line, 0);
            break;
        case STMT_RESUME:
            if (s->resume_stmt.resume_next == 0) c->info[STRICT_RESUME]++;
            else if (s->resume_stmt.resume_next == 1) c->info[STRICT_RESUME_NEXT]++;
            else push(&c->u->jumps, s->resume_stmt.label, seq, s->line, 1);
            break;
        case STMT_RESTORE:      /* (DATA is the module's: a SUB's RESTORE names its label) */
            if (s->restore_stmt.label[0])
                push(&c->modjumps, s->restore_stmt.label, seq, s->line, 0);
            break;
        case STMT_CLEAR:
            c->info[STRICT_CLEAR]++;
            break;
        case STMT_DIM:
            if (s->dim_stmt.as_missing) {   /* QB: "AS clause required", "Duplicate definition" */
                snprintf(d, sizeof d, "%s, typed AS in COMMON, DIMmed %s", s->dim_stmt.name,
                         s->dim_stmt.as_missing == 1 ? "without AS" : "AS another type");
                finding(c, s->line, s->dim_stmt.as_missing == 1 ? "as-required" : "common-type", d);
            }
            if (s->dim_stmt.is_redim) push(&c->u->redims, s->dim_stmt.name, seq, s->line, 0);
            if (!s->dim_stmt.is_redim) {
                ref_t *prev = find(&c->u->dims, s->dim_stmt.name), *sprev;
                /* (QB refuses a static array DIMmed again in the main program
                 * -- "Array already dimensioned" -- and loads the rest: in a
                 * SUB, or with bounds not constant, the second DIM fails, if
                 * it runs, when it runs) */
                int stat = !c->proc && dim_static(c, s);
                if (stat && (sprev = find(&c->u->sdims, s->dim_stmt.name))) {
                    snprintf(d, sizeof d, "DIM %s again (first at line %d)", s->dim_stmt.name, sprev->line);
                    finding(c, s->line, "dim-twice", d);
                }
                if (stat && !find(&c->u->sdims, s->dim_stmt.name))
                    push(&c->u->sdims, s->dim_stmt.name, seq, s->line, 0);
                if (!prev) {            /* kind 1: after an ERASE, uses before it were the old array's */
                    push(&c->u->dims, s->dim_stmt.name, seq, s->line,
                         find(&c->u->erased, s->dim_stmt.name) != NULL);
                    c->u->dims.v[c->u->dims.n - 1].n = s->dim_stmt.ndims;
                }
                for (int i = 0; i < s->dim_stmt.ndims; i++) {
                    /* (written as numbers: an upper bound below its lower, or below
                     * OPTION BASE where there is no lower) */
                    expr_t *b = s->dim_stmt.dims[i], *lo = s->dim_stmt.lower[i];
                    double v, l = c->u->base;
                    if (lo && !(lo->type == EXPR_LITERAL && lo->literal.type != VAL_STRING &&
                                val_to_double(&lo->literal, &l) == ERR_NONE))
                        continue;
                    if (b && b->type == EXPR_LITERAL && b->literal.type != VAL_STRING &&
                        val_to_double(&b->literal, &v) == ERR_NONE && v < l) {
                        if (lo) snprintf(d, sizeof d, "DIM %s with an upper bound below its lower", s->dim_stmt.name);
                        else snprintf(d, sizeof d, "DIM %s with an upper bound below OPTION BASE %d",
                                      s->dim_stmt.name, c->u->base);
                        finding(c, s->line, "dim-bound", d);
                        break;
                    }
                }
            }
            break;
        case STMT_ARRAY_ASSIGN:
            push(&c->u->uses, s->array_assign.name, seq, s->line, 0);
            c->u->uses.v[c->u->uses.n - 1].n = s->array_assign.nindices;
            break;
        case STMT_OPTION_BASE:
            push(&c->u->options, "OPTION BASE", seq, s->line, 0);
            c->u->base = s->option_base.base;
            break;
        case STMT_ERASE:                /* an erased array may be DIMmed again */
            for (int i = 0; i < s->erase_stmt.nnames; i++) {
                ref_t *dm = find(&c->u->dims, s->erase_stmt.names[i]);
                if (dm) {
                    dm->name[0] = '\0';
                    push(&c->u->erased, s->erase_stmt.names[i], seq, s->line, 0);
                }
            }
            break;
        case STMT_DEF_FN:
            if (c->nst > 0 || c->depth > 0) {   /* QB: "DEF FN not allowed in control statements" */
                snprintf(d, sizeof d, "DEF %s inside a %s", s->def_fn.name, c->nst > 0 ? "FOR" : "block");
                finding(c, s->line, "fn-in-block", d);
            }
            if (find(&c->fndefs, s->def_fn.name)) {
                snprintf(d, sizeof d, "DEF %s again", s->def_fn.name);
                finding(c, s->line, "fn-twice", d);
            } else
                push(&c->fndefs, s->def_fn.name, seq, s->line, s->def_fn.nparams);
            break;
        case STMT_SUB_DEF:
        case STMT_FUNC_DEF:
            walk_unit(c, s->proc_def.body, s);
            break;
        case STMT_CALL: {               /* CALL name, or name args alone */
            ref_t *p = find_proc_ref(&c->procs, s->call_stmt.name);
            if (!p && ast_machine_sub(s->call_stmt.name))
                break;                  /* (QB's own: BSAVE, BLOAD, CALL ABSOLUTE) */
            if (!p) {
                snprintf(d, sizeof d, "%s is no SUB of the program", s->call_stmt.name);
                finding(c, s->line, "sub-undefined", d);
            } else if (p->seq) {        /* QB: "Duplicate definition" */
                snprintf(d, sizeof d, "%s is a FUNCTION, not a SUB", s->call_stmt.name);
                finding(c, s->line, "call-function", d);
            } else if (p->kind != s->call_stmt.nargs) {
                snprintf(d, sizeof d, "%s with %d arguments, defined with %d",
                         s->call_stmt.name, s->call_stmt.nargs, p->kind);
                finding(c, s->line, "sub-args", d);
            }
            break;
        }
        case STMT_IF:
            walk_list(c, s->if_stmt.then_body);
            walk_list(c, s->if_stmt.else_body);
            break;
        case STMT_WHILE:
            walk_list(c, s->while_stmt.body);
            break;
        case STMT_DO_LOOP:
            walk_list(c, s->do_loop.body);
            break;
        case STMT_SELECT:
            for (case_clause_t *cl = s->select_stmt.clauses; cl; cl = cl->next)
                walk_list(c, cl->body);
            break;
        default:
            break;
        }
    }
    while (c->nst > base) {
        open_for_t *f = &c->st[--c->nst];
        snprintf(d, sizeof d, "FOR %s has no NEXT in its own block", f->var);
        finding(c, f->line, "for-without-next", d);
        add_for(c->u, f->seq, c->seq);      /* to the block's end, for goto-into-for */
    }
    c->depth--;
}

static void free_unit(unit_t *u) {
    free(u->labels.v); free(u->jumps.v); free(u->dims.v); free(u->uses.v); free(u->erased.v);
    free(u->bounds.v); free(u->redims.v); free(u->consts.v); free(u->vars.v); free(u->asdims.v);
    free(u->options.v); free(u->sdims.v); free(u->fors);
}

/* in proc (NULL: the main program), name is an array of the main
 * program's: SHARED name() there, or DIM SHARED */
static int array_shared(check_t *c, stmt_t *proc, const char *name) {
    char a[68];
    if (!proc) return 0;
    if (find(&c->dimshared, name)) return 1;
    for (int i = 0; i < proc->proc_def.nparams && i < 16; i++)     /* (an array parameter, name()) */
        if ((proc->proc_def.param_arrays >> i & 1) && strcmp(proc->proc_def.params[i], name) == 0)
            return 1;
    snprintf(a, sizeof a, "%s()", name);
    for (stmt_t *s = proc->proc_def.body; s; s = s->next)
        if (s->type == STMT_SHARED || s->type == STMT_STATIC)
            for (int i = 0; i < s->shared.nvars; i++)
                if (strcmp(s->shared.varnames[i], a) == 0) return 1;
    return 0;
}

/* the main program, or a SUB's or FUNCTION's body: its own labels and arrays */
static void walk_unit(check_t *c, stmt_t *body, stmt_t *proc) {
    unit_t u, *outer = c->u;
    stmt_t *outer_proc = c->proc;
    char d[160];
    int depth = c->depth;
    memset(&u, 0, sizeof u);
    c->u = &u;
    c->proc = proc;
    c->depth = -1;                      /* the unit's own list is depth 0 */
    walk_list(c, body);
    c->depth = depth;
    if (!proc)                          /* (every procedure walked by now: they are in its text) */
        for (int i = 0; i < c->modjumps.n; i++)
            if (!find(&u.labels, c->modjumps.v[i].name)) {
                snprintf(d, sizeof d, "label %s is not defined", c->modjumps.v[i].name);
                finding(c, c->modjumps.v[i].line, "label-undefined", d);
            }
    for (int i = 0; i < u.jumps.n; i++) {
        ref_t *j = &u.jumps.v[i];
        ref_t *l = find(&u.labels, j->name);
        if (!l) {
            snprintf(d, sizeof d, "label %s is not defined", j->name);
            finding(c, j->line, "label-undefined", d);
            continue;
        }
        if (!j->kind) continue;
        int into = 0, out = 0;
        for (int k = 0; k < u.nfors; k++) {
            int lin = l->seq > u.fors[k].from && l->seq <= u.fors[k].to;
            int jin = j->seq > u.fors[k].from && j->seq <= u.fors[k].to;
            if (lin && !jin) into = 1;
            if (jin && !lin) out = 1;
        }
        if (into) c->info[STRICT_GOTO_INTO_FOR]++;
        if (out) c->info[STRICT_GOTO_OUT_OF_FOR]++;
    }
    /* QB: "Array not defined" -- an array of a procedure that it does not
     * DIM (none are made by use there, nor for LBOUND and UBOUND) */
    for (int k = 0; k < u.uses.n; k++) {
        ref_t *use = &u.uses.v[k];
        if (!proc || find(&u.dims, use->name) || find(&u.redims, use->name) ||
            find(&u.erased, use->name) || array_shared(c, proc, use->name)) continue;
        int again = 0;
        for (int i = 0; i < k; i++) if (strcmp(u.uses.v[i].name, use->name) == 0) again = 1;
        if (again) continue;
        snprintf(d, sizeof d, "%s is no array of this procedure (DIM it, or SHARED %s())", use->name, use->name);
        finding(c, use->line, "array-undefined", d);
    }
    for (int k = 0; k < u.bounds.n; k++) {
        ref_t *b = &u.bounds.v[k];
        if (!proc || find(&u.dims, b->name) || find(&u.redims, b->name) ||
            find(&u.erased, b->name) || array_shared(c, proc, b->name)) continue;
        snprintf(d, sizeof d, "%s is no array of this procedure (DIM it, or SHARED %s())", b->name, b->name);
        finding(c, b->line, "array-undefined", d);
    }
    for (int i = 0; i < u.dims.n; i++) {
        ref_t *dm = &u.dims.v[i];
        if (dm->kind || !dm->name[0]) continue;
        for (int k = 0; k < u.uses.n; k++)
            if (u.uses.v[k].seq < dm->seq && strcmp(u.uses.v[k].name, dm->name) == 0) {
                snprintf(d, sizeof d, "%s used at line %d before its DIM", dm->name, u.uses.v[k].line);
                finding(c, dm->line, "dim-after-use", d);
                break;
            }
    }
    /* an array's subscripts: as many as its DIM's, or as its first use's
     * (QB: "Wrong number of dimensions"; NBS P074-P078) */
    for (int k = 0; k < u.uses.n; k++) {
        ref_t *use = &u.uses.v[k];
        int want = -1, at = 0;
        for (int i = 0; i < u.dims.n && want < 0; i++)
            if (strcmp(u.dims.v[i].name, use->name) == 0) { want = u.dims.v[i].n; at = u.dims.v[i].line; }
        for (int i = 0; i < k && want < 0; i++)
            if (strcmp(u.uses.v[i].name, use->name) == 0) { want = u.uses.v[i].n; at = u.uses.v[i].line; }
        if (want >= 0 && use->n != want) {
            snprintf(d, sizeof d, "%s with %d subscripts, %d at line %d", use->name, use->n, want, at);
            finding(c, use->line, "dims-mismatch", d);
        }
    }
    /* OPTION BASE: once, and before any array is DIMmed or used (QB:
     * "Array already dimensioned"; NBS P080-P082) */
    for (int i = 0; i < u.options.n; i++) {
        ref_t *o = &u.options.v[i];
        int first = 0;
        if (i > 0) {
            snprintf(d, sizeof d, "OPTION BASE again (first at line %d)", u.options.v[0].line);
            finding(c, o->line, "option-base", d);
            continue;
        }
        for (int k = 0; k < u.dims.n && !first; k++)
            if (u.dims.v[k].seq < o->seq) first = u.dims.v[k].line;
        for (int k = 0; k < u.uses.n && !first; k++)
            if (u.uses.v[k].seq < o->seq) first = u.uses.v[k].line;
        if (first) {
            snprintf(d, sizeof d, "OPTION BASE after an array at line %d", first);
            finding(c, o->line, "option-base", d);
        }
    }
    free_unit(&u);
    c->u = outer;
    c->proc = outer_proc;
}

/* a and b one name but for a type suffix */
static int same_but_suffix(const char *a, const char *b) {
    size_t la = strlen(a), lb = strlen(b);
    if (la && strchr("$%&#!", a[la - 1])) la--;
    if (lb && strchr("$%&#!", b[lb - 1])) lb--;
    return la == lb && strncmp(a, b, la) == 0;
}

/* --- types: QBasic's load-time Type mismatch ------------------------
 *
 * QBasic types a program as it loads it: every operator, comparison,
 * assignment, condition, argument and statement operand is a number or
 * a string, and a string where a number goes (or the reverse) is "Type
 * mismatch" before anything runs (probed, 2026-11-04).  Each expression
 * here is K_NUM, K_STR, or K_ANY where it cannot be told (a record, a
 * builtin of another dialect): only a definite mismatch is a finding. */

enum { K_ANY = 0, K_NUM, K_STR };

/* names declared without a suffix (DIM x AS, CONST, parameters): the
 * main program's first (nmod of them; shared: every procedure's -- DIM
 * SHARED, COMMON SHARED, CONST), then the procedure's being checked */
typedef struct { char name[64]; int k, shared, type; } kname_t;   /* (type: a val_type_t, -1 not known) */
static kname_t *kn;
static int nkn, capkn, nmod;
static int k_of(val_type_t t);
static stmt_t *ty_program, *ty_cur;     /* (ty_cur: the SUB or FUNCTION, NULL in the main program) */
static int ty_shared_decl;              /* declarations being noted are the module's shared ones */

static void kn_add(const char *name, int k) {
    if (nkn == capkn) {
        capkn = capkn ? capkn * 2 : 64;
        kn = realloc(kn, (size_t)capkn * sizeof *kn);
        if (!kn) { fprintf(stderr, "sbasic: out of memory\n"); exit(1); }
    }
    snprintf(kn[nkn].name, sizeof kn[nkn].name, "%s", name);
    kn[nkn].shared = ty_shared_decl;
    kn[nkn].type = -1;
    kn[nkn++].k = k;
}

/* the same, with its exact type */
static void kn_add_t(const char *name, val_type_t t) {
    kn_add(name, k_of(t));
    kn[nkn - 1].type = (int)t;
}

/* a SHARED list in body names name (name or name()): the procedure's
 * own SHARED, or the main program's (COMMON SHARED) */
static int ty_shares_in(stmt_t *body, const char *name) {
    char a[68];
    snprintf(a, sizeof a, "%s()", name);
    for (stmt_t *s = body; s; s = s->next)
        if (s->type == STMT_SHARED)
            for (int i = 0; i < s->shared.nvars; i++)
                if (strcmp(s->shared.varnames[i], name) == 0 || strcmp(s->shared.varnames[i], a) == 0)
                    return 1;
    return 0;
}

static int ty_proc_shares(const char *name) {
    return ty_cur && (ty_shares_in(ty_cur->proc_def.body, name) || ty_shares_in(ty_program, name));
}

static int k_of(val_type_t t) { return t == VAL_STRING ? K_STR : t == VAL_RECORD ? K_ANY : K_NUM; }

/* a name's kind: its suffix's, or as it was declared, else a SINGLE's
 * (the parser writes every other DEFtype into the name) */
static int name_k(const char *name) {
    size_t n = strlen(name);
    if (n && name[n - 1] == '$') return K_STR;
    if (n && strchr("%&!#", name[n - 1])) return K_NUM;
    for (int i = nkn - 1; i >= nmod; i--)           /* the procedure's own */
        if (strcmp(kn[i].name, name) == 0) return kn[i].k;
    for (int i = nmod - 1; i >= 0; i--)             /* the main program's, where seen */
        if (strcmp(kn[i].name, name) == 0 && (!ty_cur || kn[i].shared || ty_proc_shares(name)))
            return kn[i].k;
    return K_NUM;
}

/* QBasic's builtins, each its result and its arguments' kinds for a
 * number of them ('N', 'S', '*' either; 'L' LEN's: a string, or a
 * variable of any type) */
static const struct { const char *name; char res; const char *args; } qb_sigs[] = {
    {"ABS", 'N', "N"}, {"INT", 'N', "N"}, {"SGN", 'N', "N"}, {"SQR", 'N', "N"}, {"FIX", 'N', "N"},
    {"CINT", 'N', "N"}, {"CLNG", 'N', "N"}, {"CDBL", 'N', "N"}, {"CSNG", 'N', "N"},
    {"SIN", 'N', "N"}, {"COS", 'N', "N"}, {"TAN", 'N', "N"}, {"ATN", 'N', "N"},
    {"EXP", 'N', "N"}, {"LOG", 'N', "N"}, {"RND", 'N', ""}, {"RND", 'N', "N"},
    {"PEEK", 'N', "N"}, {"INP", 'N', "N"}, {"POS", 'N', "N"}, {"LOF", 'N', "N"}, {"LOC", 'N', "N"},
    {"EOF", 'N', "N"}, {"STICK", 'N', "N"}, {"STRIG", 'N', "N"}, {"PEN", 'N', "N"}, {"PLAY", 'N', "N"},
    {"FRE", 'N', "*"}, {"SCREEN", 'N', "NN"}, {"SCREEN", 'N', "NNN"}, {"POINT", 'N', "N"},
    {"POINT", 'N', "NN"}, {"PMAP", 'N', "NN"}, {"TIMER", 'N', ""}, {"CSRLIN", 'N', ""},
    {"FREEFILE", 'N', ""}, {"SADD", 'N', "S"}, {"VARPTR", 'N', "*"}, {"VARSEG", 'N', "*"},
    {"LEN", 'N', "L"}, {"VAL", 'N', "S"}, {"ASC", 'N', "S"}, {"INSTR", 'N', "SS"}, {"INSTR", 'N', "NSS"},
    {"CVI", 'N', "S"}, {"CVL", 'N', "S"}, {"CVS", 'N', "S"}, {"CVD", 'N', "S"},
    {"CHR$", 'S', "N"}, {"STR$", 'S', "N"}, {"HEX$", 'S', "N"}, {"OCT$", 'S', "N"}, {"SPACE$", 'S', "N"},
    {"STRING$", 'S', "N*"}, {"LEFT$", 'S', "SN"}, {"RIGHT$", 'S', "SN"}, {"MID$", 'S', "SN"},
    {"MID$", 'S', "SNN"}, {"LCASE$", 'S', "S"}, {"UCASE$", 'S', "S"}, {"LTRIM$", 'S', "S"},
    {"RTRIM$", 'S', "S"}, {"MKI$", 'S', "N"}, {"MKL$", 'S', "N"}, {"MKS$", 'S', "N"}, {"MKD$", 'S', "N"},
    {"INPUT$", 'S', "N"}, {"INPUT$", 'S', "NN"}, {"ENVIRON$", 'S', "*"}, {"INKEY$", 'S', ""},
    {"DATE$", 'S', ""}, {"TIME$", 'S', ""}, {"COMMAND$", 'S', ""}, {"TAB", 0, "N"}, {"SPC", 0, "N"},
    {"LBOUND", 'N', "*"}, {"LBOUND", 'N', "*N"}, {"UBOUND", 'N', "*"}, {"UBOUND", 'N', "*N"},
};

/* a SUB or FUNCTION by its name, or a FUNCTION f% by f (QuickBASIC) */
static stmt_t *ty_proc(const char *name) {
    char bare[64];
    for (stmt_t *s = ty_program; s; s = s->next)
        if ((s->type == STMT_SUB_DEF || s->type == STMT_FUNC_DEF) && strcmp(s->proc_def.name, name) == 0)
            return s;
    for (stmt_t *s = ty_program; s; s = s->next)
        if (s->type == STMT_FUNC_DEF && ast_name_bare(s->proc_def.name, bare) && strcmp(bare, name) == 0)
            return s;
    return NULL;
}

static stmt_t *ty_fn;           /* (DEF FNs anywhere: found by walking) */
static stmt_t *ty_deffn_in(stmt_t *s, const char *name);
static stmt_t *ty_deffn(const char *name) { return ty_deffn_in(ty_program, name); }

static int ty_report_once;      /* one Type mismatch a statement, as QBasic stops at the first */

static void ty_mismatch(check_t *c, int line, const char *what) {
    if (ty_report_once) return;
    ty_report_once = 1;
    finding(c, line, "type-mismatch", what);
}

static int ty_expr(check_t *c, expr_t *e);

/* e where a number (want K_NUM) or a string goes */
static void ty_want(check_t *c, expr_t *e, int want, const char *what) {
    int k;
    if (!e) return;
    k = ty_expr(c, e);
    if (k != K_ANY && k != want) ty_mismatch(c, e->line, what);
}

/* name's exact type: by its suffix, as it was declared, else a SINGLE; -1
 * when it cannot be told */
static int exact_t(const char *name) {
    size_t n = strlen(name);
    if (n) switch (name[n - 1]) {
        case '$': return VAL_STRING;
        case '%': return VAL_INTEGER;
        case '&': return VAL_LONG;
        case '!': return VAL_SINGLE;
        case '#': return VAL_DOUBLE;
        default: break;
    }
    for (int i = nkn - 1; i >= nmod; i--)
        if (strcmp(kn[i].name, name) == 0) return kn[i].type;
    for (int i = nmod - 1; i >= 0; i--)
        if (strcmp(kn[i].name, name) == 0 && (!ty_cur || kn[i].shared || ty_proc_shares(name)))
            return kn[i].type;
    return VAL_SINGLE;
}

/* an argument that names storage: a variable, or an array's element (not
 * a FUNCTION, a builtin, or a CONST: those are values) */
static int is_ref_arg(const expr_t *e) {
    const char *name;
    if (e->type == EXPR_VARIABLE) name = e->var.name;
    else if (e->type == EXPR_CALL && e->call.nargs > 0) name = e->call.name;
    else return 0;
    if (ty_proc(name) || strncmp(name, "FN", 2) == 0 || builtin_exists(name)) return 0;
    if (e->type == EXPR_VARIABLE && (strcmp(name, "ERR") == 0 || strcmp(name, "ERL") == 0)) return 0;
    for (int i = 0; i < nkn; i++)
        if (strcmp(kn[i].name, name) == 0 && kn[i].type == -2) return 0;
    return 1;
}

/* f's arguments against its parameters (an array parameter: passed over) */
static void ty_args(check_t *c, int line, stmt_t *def, expr_t **args, int nargs) {
    for (int i = 0; i < nargs; i++) {
        int want;
        if (!def || i >= def->proc_def.nparams || i >= 16 || (def->proc_def.param_arrays >> i & 1) ||
            (def->proc_def.param_recs && def->proc_def.param_recs[i][0])) {
            if (!(args[i]->type == EXPR_CALL && args[i]->call.nargs == 0)) ty_expr(c, args[i]);
            continue;
        }
        want = k_of(def->proc_def.param_types[i]);
        /* a variable or element goes by reference, so its type must be the
         * parameter's exactly (QB: "Parameter type mismatch"); (x) is a value */
        if (!args[i]->paren && (is_ref_arg(args[i]))) {
            int t = exact_t(args[i]->type == EXPR_VARIABLE ? args[i]->var.name : args[i]->call.name);
            if (t >= 0 && t != (int)def->proc_def.param_types[i])
                finding(c, args[i]->line, "param-mismatch", "a variable of another type than its parameter, by reference");
        }
        ty_want(c, args[i], want, "an argument of the other type than its parameter");
    }
    (void)line;
}

static int ty_call(check_t *c, expr_t *e) {
    const char *name = e->call.name;
    stmt_t *d;
    int matched = 0;
    if ((d = ty_proc(name)) && d->type == STMT_FUNC_DEF) {
        if (e->call.nargs == 0 && !e->paren && d->proc_def.nparams == 0)
            finding(c, e->line, "expected-expression", "a FUNCTION of no parameters called with ()");
        ty_args(c, e->line, d, e->call.args, e->call.nargs);
        return name_k(d->proc_def.name);
    }
    if (strncmp(name, "FN", 2) == 0) {
        if ((d = ty_deffn(name))) {
            if (e->call.nargs == 0 && d->def_fn.nparams == 0)
                finding(c, e->line, "expected-expression", "a DEF FN of no parameters called with ()");
            for (int i = 0; i < e->call.nargs && i < d->def_fn.nparams; i++)
                ty_want(c, e->call.args[i], k_of(d->def_fn.param_types[i]),
                        "an argument of the other type than its parameter");
        }
        return name_k(name);
    }
    for (size_t j = 0; j < sizeof qb_sigs / sizeof qb_sigs[0]; j++) {
        if (strcmp(qb_sigs[j].name, name) != 0) continue;
        matched = 1;
        if ((int)strlen(qb_sigs[j].args) != e->call.nargs) continue;
        if (e->call.nargs == 0)         /* RND(), TIMER() */
            finding(c, e->line, "expected-expression", "a builtin of no arguments called with ()");
        for (int i = 0; i < e->call.nargs; i++) {
            char a = qb_sigs[j].args[i];
            expr_t *x = e->call.args[i];
            if (a == '*' && (strcmp(name, "LBOUND") == 0 || strcmp(name, "UBOUND") == 0 ||
                             strcmp(name, "VARPTR") == 0 || strcmp(name, "VARSEG") == 0)) continue;
            if (a == 'L') {             /* LEN: a string, or any variable (its size) */
                int k = ty_expr(c, x);
                if (k == K_NUM && x->type != EXPR_VARIABLE && x->type != EXPR_CALL &&
                    x->type != EXPR_FIELD_ACCESS)
                    finding(c, x->line, "variable-required", "LEN of a number not a variable");
                continue;
            }
            if (a == '*') { ty_expr(c, x); continue; }
            ty_want(c, x, a == 'S' ? K_STR : K_NUM,
                    a == 'S' ? "a number where the builtin takes a string"
                             : "a string where the builtin takes a number");
        }
        return qb_sigs[j].res == 'S' ? K_STR : qb_sigs[j].res == 'N' ? K_NUM : K_ANY;
    }
    if (matched || builtin_exists(name)) {      /* (another dialect's, or a count QB has not) */
        for (int i = 0; i < e->call.nargs; i++) ty_expr(c, e->call.args[i]);
        return K_ANY;
    }
    /* an array's element: numbers for subscripts (a() alone, as GET
     * and PUT and calls take it, is no value to type) */
    if (e->call.nargs == 0) return K_ANY;
    for (int i = 0; i < e->call.nargs; i++)
        ty_want(c, e->call.args[i], K_NUM, "a string as a subscript");
    return name_k(name);
}

static int ty_expr(check_t *c, expr_t *e) {
    int a, b;
    if (!e) return K_ANY;
    switch (e->type) {
    case EXPR_LITERAL:
        return e->literal.type == VAL_STRING ? K_STR : K_NUM;
    case EXPR_VARIABLE:
        return name_k(e->var.name);
    case EXPR_FIELD_ACCESS:
        if (e->field.elem) ty_expr(c, e->field.elem);
        return K_ANY;
    case EXPR_UNARY:
        a = ty_expr(c, e->unary.operand);
        if (a == K_STR) ty_mismatch(c, e->line, e->unary.op == OP_NOT ? "NOT of a string" : "- of a string");
        return K_NUM;
    case EXPR_COMPARE:
        a = ty_expr(c, e->compare.left);
        b = ty_expr(c, e->compare.right);
        if (a != K_ANY && b != K_ANY && a != b) ty_mismatch(c, e->line, "a string compared with a number");
        return K_NUM;
    case EXPR_BINARY:
        a = ty_expr(c, e->binary.left);
        b = ty_expr(c, e->binary.right);
        if (e->binary.op == OP_ADD) {
            if (a != K_ANY && b != K_ANY && a != b) { ty_mismatch(c, e->line, "+ of a string and a number"); return K_ANY; }
            return a != K_ANY ? a : b;
        }
        if (e->binary.op == OP_STRCAT) return K_STR;
        if (a == K_STR || b == K_STR) { ty_mismatch(c, e->line, "a string in arithmetic"); return K_NUM; }
        return K_NUM;
    case EXPR_CALL:
        return ty_call(c, e);
    default:
        return K_ANY;
    }
}

static void ty_top(expr_t *e, void *vc) { ty_expr((check_t *)vc, e); }

static void ty_list(check_t *c, stmt_t *s);
static void ty_declare_proc(stmt_t *s);

static void ty_stmt(check_t *c, stmt_t *s) {
    int k;
    stmt_t *d;
    ty_report_once = 0;
    if (s->unsep)                       /* QB: "Expected: end-of-statement" */
        finding(c, s->line, "end-of-statement", "a statement right after another, no colon between");
    switch (s->type) {
    case STMT_ASSIGN:
        k = ty_expr(c, s->assign.value);
        if (k != K_ANY && k != name_k(s->assign.name))
            ty_mismatch(c, s->line, "a value of the other type than its variable");
        return;
    case STMT_ARRAY_ASSIGN:
        for (int i = 0; i < s->array_assign.nindices; i++)
            ty_want(c, s->array_assign.indices[i], K_NUM, "a string as a subscript");
        ty_want(c, s->array_assign.value, name_k(s->array_assign.name), "a value of the other type than its array");
        return;
    case STMT_IF:
        ty_want(c, s->if_stmt.condition, K_NUM, "a string as a condition");
        ty_list(c, s->if_stmt.then_body);
        ty_list(c, s->if_stmt.else_body);
        return;
    case STMT_WHILE:
        ty_want(c, s->while_stmt.condition, K_NUM, "a string as a condition");
        ty_list(c, s->while_stmt.body);
        return;
    case STMT_DO_LOOP:
        ty_want(c, s->do_loop.pre_cond, K_NUM, "a string as a condition");
        ty_want(c, s->do_loop.post_cond, K_NUM, "a string as a condition");
        ty_list(c, s->do_loop.body);
        return;
    case STMT_FOR:
        if (name_k(s->for_stmt.var_name) == K_STR) ty_mismatch(c, s->line, "a string as FOR's counter");
        ty_want(c, s->for_stmt.start, K_NUM, "a string in FOR");
        ty_want(c, s->for_stmt.end, K_NUM, "a string in FOR");
        ty_want(c, s->for_stmt.step, K_NUM, "a string in FOR");
        ty_list(c, s->for_stmt.body);
        return;
    case STMT_ON_GOTO: case STMT_ON_GOSUB:
        ty_want(c, s->on_branch.index, K_NUM, "a string as ON's index");
        return;
    case STMT_PRINT: case STMT_LPRINT:
        for (int i = 0; i < s->print.nitems; i++) ty_expr(c, s->print.items[i].expr);
        ty_want(c, s->print.using_expr, K_STR, "a number as PRINT USING's format");
        return;
    case STMT_LOCATE:
        ty_want(c, s->locate_stmt.row, K_NUM, "a string in LOCATE");
        ty_want(c, s->locate_stmt.col, K_NUM, "a string in LOCATE");
        for (int i = 0; i < 3; i++) ty_want(c, s->locate_stmt.rest[i], K_NUM, "a string in LOCATE");
        return;
    case STMT_COLOR:
        ty_want(c, s->color_stmt.fg, K_NUM, "a string in COLOR");
        ty_want(c, s->color_stmt.bg, K_NUM, "a string in COLOR");
        return;
    case STMT_SWAP: {
        int a = ty_expr(c, s->swap_stmt.lhs), b = ty_expr(c, s->swap_stmt.rhs);
        if (a != K_ANY && b != K_ANY && a != b) ty_mismatch(c, s->line, "SWAP of a string and a number");
        return;
    }
    case STMT_CALL:
        if ((d = ty_proc(s->call_stmt.name)) && d->type == STMT_SUB_DEF)
            ty_args(c, s->line, d, s->call_stmt.args, s->call_stmt.nargs);
        else
            for (int i = 0; i < s->call_stmt.nargs; i++)
                if (!(s->call_stmt.args[i]->type == EXPR_CALL && s->call_stmt.args[i]->call.nargs == 0))
                    ty_expr(c, s->call_stmt.args[i]);
        return;
    case STMT_DEF_FN:
        ty_want(c, s->def_fn.body, name_k(s->def_fn.name), "a DEF FN's value of the other type than its name");
        return;
    case STMT_SUB_DEF: case STMT_FUNC_DEF: {
        /* its own names, over the main program's */
        int keep = nkn;
        ty_cur = s;
        ty_declare_proc(s);
        ty_list(c, s->proc_def.body);
        nkn = keep;
        ty_cur = NULL;
        return;
    }
    case STMT_DIM:
        /* QB's "Array already dimensioned": a REDIM of what a DIM of
         * constant bounds made, ERASE or no, in a procedure through SHARED */
        if (!s->dim_stmt.is_redim && !ty_cur && dim_static(c, s))
            push(&c->staticdims, s->dim_stmt.name, 0, s->line, 0);
        else if (s->dim_stmt.is_redim && find(&c->staticdims, s->dim_stmt.name) &&
                 (!ty_cur || find(&c->dimshared, s->dim_stmt.name) || ty_proc_shares(s->dim_stmt.name))) {
            char d[128];
            snprintf(d, sizeof d, "REDIM %s, which a DIM of constant bounds made", s->dim_stmt.name);
            finding(c, s->line, "redim-static", d);
        }
        else if (!s->dim_stmt.is_redim && ty_cur && find(&c->staticdims, s->dim_stmt.name) &&
                 (find(&c->dimshared, s->dim_stmt.name) || ty_proc_shares(s->dim_stmt.name))) {
            char d[128];                /* (the same array: a SHARED name is the main program's) */
            snprintf(d, sizeof d, "DIM %s in a procedure, a shared array already made", s->dim_stmt.name);
            finding(c, s->line, "dim-twice", d);
        }
        stmt_exprs(s, ty_top, c);
        return;
    case STMT_SELECT:
        ty_expr(c, s->select_stmt.test_expr);
        for (case_clause_t *cl = s->select_stmt.clauses; cl; cl = cl->next) ty_list(c, cl->body);
        return;
    default:
        stmt_exprs(s, ty_top, c);
        return;
    }
}

static void ty_list(check_t *c, stmt_t *s) {
    for (; s; s = s->next) ty_stmt(c, s);
}

static stmt_t *ty_deffn_in(stmt_t *s, const char *name) {
    for (; s; s = s->next) {
        stmt_t *r = NULL;
        if (s->type == STMT_DEF_FN && strcmp(s->def_fn.name, name) == 0) return s;
        switch (s->type) {
        case STMT_IF: if (!(r = ty_deffn_in(s->if_stmt.then_body, name))) r = ty_deffn_in(s->if_stmt.else_body, name); break;
        case STMT_SUB_DEF: case STMT_FUNC_DEF: r = ty_deffn_in(s->proc_def.body, name); break;
        default: break;
        }
        if (r) return r;
    }
    return NULL;
}

/* the declarations that type a name with no suffix, anywhere */
static void ty_declare(stmt_t *s) {
    for (; s; s = s->next) {
        switch (s->type) {
        case STMT_DIM_SCALAR:
            ty_shared_decl = !ty_cur && s->dim_scalar.shared;
            kn_add_t(s->dim_scalar.name, s->dim_scalar.scalar_type);
            break;
        case STMT_DIM_AS_TYPE:
            ty_shared_decl = !ty_cur && s->dim_as_type.shared;
            kn_add(s->dim_as_type.name, K_ANY);
            break;
        case STMT_DIM:
            ty_shared_decl = !ty_cur && s->dim_stmt.shared;
            if (s->dim_stmt.typed) kn_add_t(s->dim_stmt.name, s->dim_stmt.elem_type);
            break;
        case STMT_CONST:
            ty_shared_decl = !ty_cur;
            if (s->const_stmt.value && s->const_stmt.value->type == EXPR_LITERAL)
                kn_add(s->const_stmt.name, s->const_stmt.value->literal.type == VAL_STRING ? K_STR : K_NUM);
            else kn_add(s->const_stmt.name, K_ANY);
            kn[nkn - 1].type = -2;      /* (a CONST: a value, passed so) */
            break;
        case STMT_SUB_DEF: case STMT_FUNC_DEF: {
            char bare[64];
            /* (FUNCTION f$ is f too, everywhere; its body is its own) */
            ty_shared_decl = 1;
            if (s->type == STMT_FUNC_DEF && ast_name_bare(s->proc_def.name, bare))
                kn_add(bare, name_k(s->proc_def.name));
            break;
        }
        case STMT_STATIC: case STMT_SHARED:
            ty_shared_decl = 0;
            for (int i = 0; i < s->shared.nvars && i < 16; i++)
                if (s->shared.astype[i]) kn_add(s->shared.varnames[i], k_of((val_type_t)(s->shared.astype[i] - 1)));
            break;
        case STMT_IF: ty_declare(s->if_stmt.then_body); ty_declare(s->if_stmt.else_body); break;
        case STMT_FOR: ty_declare(s->for_stmt.body); break;
        case STMT_WHILE: ty_declare(s->while_stmt.body); break;
        case STMT_DO_LOOP: ty_declare(s->do_loop.body); break;
        default: break;
        }
    }
}

/* a procedure's own: its parameters, its DIMs and STATICs */
static void ty_declare_proc(stmt_t *s) {
    for (int i = 0; i < s->proc_def.nparams; i++) {
        ty_shared_decl = 0;
        if (i < 16 && s->proc_def.param_recs && s->proc_def.param_recs[i][0]) kn_add(s->proc_def.params[i], K_ANY);
        else kn_add_t(s->proc_def.params[i], s->proc_def.param_types[i]);
    }
    ty_declare(s->proc_def.body);
}

static void ty_check(check_t *c, stmt_t *program) {
    unit_t none;                        /* (walk_unit's are gone: CONSTs are the main program's here) */
    memset(&none, 0, sizeof none);
    c->u = &none;
    ty_program = program;
    ty_cur = NULL;
    nkn = 0;
    ty_declare(program);
    nmod = nkn;
    ty_list(c, program);
    free(kn);
    kn = NULL;
    nkn = capkn = 0;
    c->u = NULL;
}

int strict_check(stmt_t *program, strict_report_fn report, void *rctx, int info[STRICT_NINFO]) {
    check_t c;
    char d[128];
    memset(&c, 0, sizeof c);
    c.report = report;
    c.rctx = rctx;
    for (stmt_t *s = program; s; s = s->next)
        if (s->type == STMT_SUB_DEF || s->type == STMT_FUNC_DEF) {
            /* one name, whatever its suffix (QB: "duplicate procedure
             * definition"); seq says which kind: 1 a FUNCTION */
            for (int i = 0; i < c.procs.n; i++)
                if (same_but_suffix(c.procs.v[i].name, s->proc_def.name)) {
                    snprintf(d, sizeof d, "%s again, as %s", s->proc_def.name, c.procs.v[i].name);
                    finding(&c, s->line, "proc-twice", d);
                    break;
                }
            push(&c.procs, s->proc_def.name, s->type == STMT_FUNC_DEF, s->line, s->proc_def.nparams);
        }
    for (stmt_t *s = program; s; s = s->next) {
        if (s->type == STMT_DIM && s->dim_stmt.shared)
            push(&c.dimshared, s->dim_stmt.name, 0, s->line, 0);
        if (s->type == STMT_SHARED)     /* (the module's own: COMMON SHARED a()) */
            for (int i = 0; i < s->shared.nvars; i++) {
                char nm[68];
                size_t n = strlen(s->shared.varnames[i]);
                if (n <= 2 || strcmp(s->shared.varnames[i] + n - 2, "()") != 0) continue;
                snprintf(nm, sizeof nm, "%.*s", (int)(n - 2), s->shared.varnames[i]);
                push(&c.dimshared, nm, 0, s->line, 0);
            }
        if (s->type == STMT_CONST)
            push(&c.modconsts, s->const_stmt.name, 0, s->line, 0);
    }
    walk_unit(&c, program, NULL);
    ty_check(&c, program);
    for (int i = 0; i < c.fnuses.n; i++) {
        ref_t *use = &c.fnuses.v[i];
        ref_t *def = find(&c.fndefs, use->name);
        /* QuickBASIC refuses this ("Function not defined"); a compiler
         * defines every DEF FN before the program runs (user, 2026-11-03:
         * QB was being lazy), so it is only counted */
        if (!def) {
            snprintf(d, sizeof d, "%s is not defined", use->name);
            finding(&c, use->line, "fn-undefined", d);
        } else {
            if (use->kind != def->kind) {   /* QB: "Argument-count mismatch" */
                snprintf(d, sizeof d, "%s with %d arguments, defined with %d", use->name, use->kind, def->kind);
                finding(&c, use->line, "fn-args", d);
            }
            if (use->seq <= def->seq)       /* before it, or in its own body */
                c.info[STRICT_FN_HOISTED]++;
        }
    }
    if (info) memcpy(info, c.info, sizeof c.info);
    free(c.procs.v); free(c.fndefs.v); free(c.fnuses.v); free(c.dimshared.v); free(c.modconsts.v); free(c.modjumps.v); free(c.staticdims.v); free(c.st);
    return c.findings;
}
