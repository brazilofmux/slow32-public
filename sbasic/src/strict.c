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
    int kind;           /* jumps: 1 a GOTO-like transfer, 0 a reference only */
} ref_t;

typedef struct { ref_t *v; int n, cap; } refs_t;

typedef struct { int from, to; } range_t;   /* a FOR..NEXT, by seq */

typedef struct {
    refs_t labels, jumps;
    refs_t dims, uses;          /* arrays: DIMs, and uses */
    refs_t erased;              /* arrays ERASEd, which may be DIMmed again */
    refs_t options;             /* OPTION BASE statements */
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
    unit_t *u;                  /* the unit being walked */
    struct open_for *st;        /* the FORs open in the text, the innermost last */
    int nst, capst;
} check_t;

static const char *info_names[STRICT_NINFO] = {
    "gosub", "on-error", "resume", "resume-next", "erl", "clear", "goto-out-of-for",
    "goto-into-for", "fn-hoisted",
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
}

static ref_t *find(refs_t *r, const char *name) {
    for (int i = 0; i < r->n; i++)
        if (strcmp(r->v[i].name, name) == 0) return &r->v[i];
    return NULL;
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

static void scan_expr(expr_t *e, void *vc) {
    check_t *c = vc;
    if (e->type == EXPR_VARIABLE) {
        if (strcmp(e->var.name, "ERL") == 0) c->info[STRICT_ERL]++;
        if (is_fn_name(e->var.name)) push(&c->fnuses, e->var.name, c->seq, e->line, 0);
    } else if (e->type == EXPR_CALL) {
        if (is_fn_name(e->call.name)) push(&c->fnuses, e->call.name, c->seq, e->line, 0);
        else if (e->call.nargs > 0 && !builtin_exists(e->call.name) && !find(&c->procs, e->call.name))
            push(&c->u->uses, e->call.name, c->seq, e->line, 0);
    }
}

static void scan_top(expr_t *e, void *vc) { expr_walk(e, scan_expr, vc); }

/* --- statements --- */

/* a FOR open in the text */
typedef struct open_for { char var[64]; int seq; int line; } open_for_t;

static void walk_unit(check_t *c, stmt_t *body);

/* One statement list.  FOR and NEXT pair as QuickBASIC pairs them at load
 * time (QBasic 1.1 and QB64 agree, 2026-11-03): a NEXT closes the
 * innermost FOR open in its own block -- the one named, which must be
 * that one; NEXT K, J closes K then J -- and a block ends with none
 * open.  So `IF ... THEN PRINT X$;: NEXT Z` is "NEXT without FOR", and
 * so is a NEXT that would skip an inner FOR; a FOR left open is "FOR
 * without NEXT".  Jumps into and out of a FOR are allowed. */
static void walk_list(check_t *c, stmt_t *s) {
    int base = c->nst;
    char d[128];
    for (; s; s = s->next) {
        int seq = ++c->seq;
        stmt_exprs(s, scan_top, c);
        switch (s->type) {
        case STMT_LABEL:
            if (find(&c->u->labels, s->label.name)) {
                snprintf(d, sizeof d, "label %s defined twice", s->label.name);
                finding(c, s->line, "label-twice", d);
            } else
                push(&c->u->labels, s->label.name, seq, s->line, 0);
            break;
        case STMT_FOR:
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
        case STMT_ON_GOTO:
        case STMT_ON_GOSUB:
            if (s->type == STMT_ON_GOSUB) c->info[STRICT_GOSUB]++;
            for (int i = 0; i < s->on_branch.nlabels; i++)
                push(&c->u->jumps, s->on_branch.labels[i], seq, s->line, 1);
            break;
        case STMT_ON_ERROR:
            c->info[STRICT_ON_ERROR]++;
            if (s->on_error.label[0])
                push(&c->u->jumps, s->on_error.label, seq, s->line, 0);
            break;
        case STMT_RESUME:
            if (s->resume_stmt.resume_next == 0) c->info[STRICT_RESUME]++;
            else if (s->resume_stmt.resume_next == 1) c->info[STRICT_RESUME_NEXT]++;
            else push(&c->u->jumps, s->resume_stmt.label, seq, s->line, 1);
            break;
        case STMT_RESTORE:
            if (s->restore_stmt.label[0])
                push(&c->u->jumps, s->restore_stmt.label, seq, s->line, 0);
            break;
        case STMT_CLEAR:
            c->info[STRICT_CLEAR]++;
            break;
        case STMT_DIM:
            if (!s->dim_stmt.is_redim) {
                ref_t *prev = find(&c->u->dims, s->dim_stmt.name);
                if (prev) {
                    snprintf(d, sizeof d, "DIM %s again (first at line %d)", s->dim_stmt.name, prev->line);
                    finding(c, s->line, "dim-twice", d);
                } else              /* kind 1: after an ERASE, uses before it were the old array's */
                    push(&c->u->dims, s->dim_stmt.name, seq, s->line,
                         find(&c->u->erased, s->dim_stmt.name) != NULL);
            }
            break;
        case STMT_ARRAY_ASSIGN:
            push(&c->u->uses, s->array_assign.name, seq, s->line, 0);
            break;
        case STMT_OPTION_BASE:
            push(&c->u->options, "OPTION BASE", seq, s->line, 0);
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
            if (find(&c->fndefs, s->def_fn.name)) {
                snprintf(d, sizeof d, "DEF %s again", s->def_fn.name);
                finding(c, s->line, "fn-twice", d);
            } else
                push(&c->fndefs, s->def_fn.name, seq, s->line, 0);
            break;
        case STMT_SUB_DEF:
        case STMT_FUNC_DEF:
            walk_unit(c, s->proc_def.body);
            break;
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
}

static void free_unit(unit_t *u) {
    free(u->labels.v); free(u->jumps.v); free(u->dims.v); free(u->uses.v); free(u->erased.v);
    free(u->options.v); free(u->fors);
}

/* the main program, or a SUB's or FUNCTION's body: its own labels and arrays */
static void walk_unit(check_t *c, stmt_t *body) {
    unit_t u, *outer = c->u;
    char d[160];
    memset(&u, 0, sizeof u);
    c->u = &u;
    walk_list(c, body);
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
}

int strict_check(stmt_t *program, strict_report_fn report, void *rctx, int info[STRICT_NINFO]) {
    check_t c;
    char d[128];
    memset(&c, 0, sizeof c);
    c.report = report;
    c.rctx = rctx;
    for (stmt_t *s = program; s; s = s->next)
        if (s->type == STMT_SUB_DEF || s->type == STMT_FUNC_DEF)
            push(&c.procs, s->proc_def.name, 0, s->line, 0);
    walk_unit(&c, program);
    for (int i = 0; i < c.fnuses.n; i++) {
        ref_t *use = &c.fnuses.v[i];
        ref_t *def = find(&c.fndefs, use->name);
        /* QuickBASIC refuses this ("Function not defined"); a compiler
         * defines every DEF FN before the program runs (user, 2026-11-03:
         * QB was being lazy), so it is only counted */
        if (!def) {
            snprintf(d, sizeof d, "%s is not defined", use->name);
            finding(&c, use->line, "fn-undefined", d);
        } else if (use->seq <= def->seq)    /* before it, or in its own body */
            c.info[STRICT_FN_HOISTED]++;
    }
    if (info) memcpy(info, c.info, sizeof c.info);
    free(c.procs.v); free(c.fndefs.v); free(c.fnuses.v); free(c.st);
    return c.findings;
}
