/* Statement trees to flat code: see lower.h. */
#include "lower.h"
#include <stdlib.h>
#include <string.h>

#define MAX_LOOP_DEPTH 64

typedef struct {
    code_t *c;
    error_t err;
    struct {
        exit_type_t kind;   /* EXIT_FOR, EXIT_WHILE or EXIT_DO */
        int *exits;         /* OP_JUMPs to patch to the loop's end */
        int nexits, cap;
    } loops[MAX_LOOP_DEPTH];
    int depth;
    int basic_line;     /* the last line number passed */
    int fors[MAX_LOOP_DEPTH];   /* the FORs open in the text (op indices) */
    int nfors;
} lower_t;

static int emit(lower_t *L, op_kind_t kind, stmt_t *s, int line) {
    code_t *c = L->c;
    if (c->nops == c->cap) {
        int cap = c->cap ? c->cap * 2 : 32;
        op_t *ops = realloc(c->ops, cap * sizeof(op_t));
        if (!ops) { L->err = ERR_OUT_OF_MEMORY; return 0; }
        c->ops = ops;
        c->cap = cap;
    }
    op_t *op = &c->ops[c->nops];
    memset(op, 0, sizeof(*op));
    op->kind = kind;
    op->s = s;
    op->line = line;
    op->basic_line = L->basic_line;
    op->target = -1;
    return c->nops++;
}

static unsigned label_hash(const char *s) {
    unsigned h = 5381;
    while (*s) h = h * 33 + (unsigned char)*s++;
    return h;
}

/* label i in the hash, which has room */
static void label_index(code_t *c, int i) {
    unsigned b = label_hash(c->labels[i].name) & (unsigned)(c->lhcap - 1);
    while (c->lhash[b]) b = (b + 1) & (unsigned)(c->lhcap - 1);
    c->lhash[b] = i + 1;
}

static void add_label(lower_t *L, const char *name) {
    code_t *c = L->c;
    if (code_find_label(c, name) >= 0) return;   /* the first one counts */
    if ((c->nlabels + 1) * 2 > c->lhcap) {
        int cap = c->lhcap ? c->lhcap * 2 : 64;
        int *h = calloc((size_t)cap, sizeof(int));
        if (!h) { L->err = ERR_OUT_OF_MEMORY; return; }
        free(c->lhash);
        c->lhash = h;
        c->lhcap = cap;
        for (int i = 0; i < c->nlabels; i++) label_index(c, i);
    }
    if (c->nlabels == c->lcap) {
        int cap = c->lcap ? c->lcap * 2 : 16;
        code_label_t *l = realloc(c->labels, cap * sizeof(code_label_t));
        if (!l) { L->err = ERR_OUT_OF_MEMORY; return; }
        c->labels = l;
        c->lcap = cap;
    }
    ast_name_copy(c->labels[c->nlabels].name, name);
    c->labels[c->nlabels].index = c->nops;
    label_index(c, c->nlabels);
    c->nlabels++;
}

static void loop_open(lower_t *L, exit_type_t kind) {
    if (L->depth >= MAX_LOOP_DEPTH) { L->err = ERR_STACK_OVERFLOW; return; }
    L->loops[L->depth].kind = kind;
    L->loops[L->depth].exits = NULL;
    L->loops[L->depth].nexits = 0;
    L->loops[L->depth].cap = 0;
    L->depth++;
}

/* the innermost loop's EXITs go to the next op */
static void loop_close(lower_t *L) {
    L->depth--;
    for (int i = 0; i < L->loops[L->depth].nexits; i++)
        L->c->ops[L->loops[L->depth].exits[i]].target = L->c->nops;
    free(L->loops[L->depth].exits);
}

/* EXIT FOR/WHILE/DO: a jump out of the innermost loop of its kind; with
 * none around it, the statement stays, and is an error when it runs */
static void lower_exit(lower_t *L, stmt_t *s) {
    exit_type_t k = s->exit_stmt.what;
    if (k == EXIT_FOR && L->nfors > 0) {
        /* to past the innermost open FOR's NEXT (set in match_fors) */
        int j = emit(L, OP_EXITFOR, s, s->line);
        L->c->ops[j].slot = L->fors[L->nfors - 1];
        return;
    }
    if (k == EXIT_WHILE || k == EXIT_DO) {
        for (int d = L->depth - 1; d >= 0; d--) {
            if (L->loops[d].kind != k) continue;
            int j = emit(L, OP_JUMP, s, s->line);
            if (L->loops[d].nexits == L->loops[d].cap) {
                int cap = L->loops[d].cap ? L->loops[d].cap * 2 : 4;
                int *e = realloc(L->loops[d].exits, cap * sizeof(int));
                if (!e) { L->err = ERR_OUT_OF_MEMORY; return; }
                L->loops[d].exits = e;
                L->loops[d].cap = cap;
            }
            L->loops[d].exits[L->loops[d].nexits++] = j;
            return;
        }
    }
    emit(L, OP_STMT, s, s->line);
}

static void lower_list(lower_t *L, stmt_t *s);

static void lower_stmt(lower_t *L, stmt_t *s) {
    code_t *c = L->c;
    int j, n, top;
    switch (s->type) {
        case STMT_LABEL: {
            const char *q = s->label.name;
            add_label(L, q);
            while (*q >= '0' && *q <= '9') q++;
            if (*q == '\0' && q != s->label.name)
                L->basic_line = atoi(s->label.name);
            return;
        }
        case STMT_REM:
        case STMT_SUB_DEF:
        case STMT_FUNC_DEF:
            return;
        case STMT_EXIT:
            lower_exit(L, s);
            return;
        case STMT_IF:
            j = emit(L, OP_JFALSE, s, s->line);
            c->ops[j].cond = s->if_stmt.condition;
            lower_list(L, s->if_stmt.then_body);
            if (s->if_stmt.else_body) {
                int over = emit(L, OP_JUMP, s, s->line);
                c->ops[j].target = c->nops;
                lower_list(L, s->if_stmt.else_body);
                c->ops[over].target = c->nops;
            } else {
                c->ops[j].target = c->nops;
            }
            return;
        case STMT_FOR:
            /* FOR and NEXT pair up in the text, wherever the NEXT stands
             * (IF c THEN ...: NEXT Z); the pairing says where a loop that
             * runs no times goes, and EXIT FOR.  Which loop a NEXT steps
             * is settled when it runs (run_code). */
            j = emit(L, OP_FOR, s, s->line);
            c->ops[j].slot = c->nfor++;
            if (L->nfors >= MAX_LOOP_DEPTH) { L->err = ERR_STACK_OVERFLOW; return; }
            L->fors[L->nfors++] = j;
            return;
        case STMT_NEXT:
            /* NEXT J, I is NEXT J then NEXT I; each takes the innermost
             * open FOR of its variable (closing any inside it), or the
             * innermost of all when it names none */
            for (int k = 0; k < (s->next_stmt.nvars ? s->next_stmt.nvars : 1); k++) {
                int f = -1;
                while (L->nfors > 0) {
                    int cand = L->fors[--L->nfors];
                    if (s->next_stmt.nvars == 0 ||
                        strcmp(c->ops[cand].s->for_stmt.var_name, s->next_stmt.vars[k]) == 0) {
                        f = cand;
                        break;
                    }
                }
                if (f < 0) { L->err = ERR_NEXT_WITHOUT_FOR; return; }
                n = emit(L, OP_NEXT, c->ops[f].s, s->line);
                c->ops[n].invert = s->next_stmt.nvars == 0;
                c->ops[n].slot = f;
                c->ops[f].target = n + 1;
            }
            return;
        case STMT_WHILE:
            top = c->nops;
            j = emit(L, OP_JFALSE, s, s->line);
            c->ops[j].cond = s->while_stmt.condition;
            loop_open(L, EXIT_WHILE);
            lower_list(L, s->while_stmt.body);
            n = emit(L, OP_JUMP, s, s->line);
            c->ops[n].target = top;
            c->ops[j].target = c->nops;
            loop_close(L);
            return;
        case STMT_DO_LOOP:
            /* DO [WHILE c | UNTIL c] body LOOP [WHILE c | UNTIL c] */
            top = c->nops;
            j = -1;
            if (s->do_loop.pre_cond) {
                j = emit(L, OP_JFALSE, s, s->line);
                c->ops[j].cond = s->do_loop.pre_cond;
                c->ops[j].invert = s->do_loop.pre_until;
            }
            loop_open(L, EXIT_DO);
            lower_list(L, s->do_loop.body);
            if (s->do_loop.post_cond) {
                /* back to the top while it holds */
                int b = emit(L, OP_JFALSE, s, s->line);
                c->ops[b].cond = s->do_loop.post_cond;
                c->ops[b].invert = !s->do_loop.post_until;
                c->ops[b].target = top;
            } else {
                n = emit(L, OP_JUMP, s, s->line);
                c->ops[n].target = top;
            }
            if (j >= 0) c->ops[j].target = c->nops;
            loop_close(L);
            return;
        case STMT_SELECT: {
            int k = 0;
            n = 0;
            for (case_clause_t *cl = s->select_stmt.clauses; cl; cl = cl->next) n++;
            int *targets = calloc(n + 1, sizeof(int));
            int *ends = calloc(n + 1, sizeof(int));
            if (!targets || !ends) { free(targets); free(ends); L->err = ERR_OUT_OF_MEMORY; return; }
            j = emit(L, OP_SELECT, s, s->line);
            c->ops[j].targets = targets;
            for (case_clause_t *cl = s->select_stmt.clauses; cl; cl = cl->next, k++) {
                c->ops[j].targets[k] = c->nops;
                lower_list(L, cl->body);
                ends[k] = emit(L, OP_JUMP, s, s->line);
            }
            for (k = 0; k < n; k++) c->ops[ends[k]].target = c->nops;
            c->ops[j].targets[n] = c->nops;
            free(ends);
            return;
        }
        default:
            emit(L, OP_STMT, s, s->line);
            return;
    }
}

static void lower_list(lower_t *L, stmt_t *s) {
    for (; s && L->err == ERR_NONE; s = s->next)
        lower_stmt(L, s);
}

/* A FOR the text left open (its loop closed by another FOR's NEXT, as in
 * awari) skips, when it runs no times, past the next NEXT of its
 * variable.  EXIT FOR goes where its FOR goes then: past the NEXT. */
static void match_exits(code_t *c) {
    for (int i = 0; i < c->nops; i++) {
        if (c->ops[i].kind != OP_FOR || c->ops[i].target >= 0) continue;
        for (int j = i + 1; j < c->nops; j++)
            if (c->ops[j].kind == OP_NEXT &&
                strcmp(c->ops[j].s->for_stmt.var_name, c->ops[i].s->for_stmt.var_name) == 0) {
                c->ops[i].target = j + 1;
                break;
            }
    }
    for (int i = 0; i < c->nops; i++)
        if (c->ops[i].kind == OP_EXITFOR)
            c->ops[i].target = c->ops[c->ops[i].slot].target;
}

static code_t *lower(stmt_t *s, int just_one, error_t *err) {
    lower_t L;
    memset(&L, 0, sizeof(L));
    L.c = calloc(1, sizeof(code_t));
    if (!L.c) { *err = ERR_OUT_OF_MEMORY; return NULL; }
    if (just_one) { if (s) lower_stmt(&L, s); }
    else lower_list(&L, s);
    while (L.depth > 0) loop_close(&L);
    /* a FOR still open at the end is an error only when no NEXT of its
     * variable comes after it at all (QBasic: awari's FOR at 230 leans
     * on another loop's NEXT and loads; P050's FOR with none does not) */
    for (int k = 0; k < L.nfors && L.err == ERR_NONE; k++) {
        int f = L.fors[k], found = 0;
        for (int j = f + 1; j < L.c->nops && !found; j++)
            found = L.c->ops[j].kind == OP_NEXT &&
                    strcmp(L.c->ops[j].s->for_stmt.var_name,
                           L.c->ops[f].s->for_stmt.var_name) == 0;
        if (!found) L.err = ERR_FOR_WITHOUT_NEXT;
    }
    match_exits(L.c);
    *err = L.err;
    if (L.err != ERR_NONE) { code_free(L.c); return NULL; }
    return L.c;
}

code_t *lower_stmts(stmt_t *list, error_t *err) { return lower(list, 0, err); }
code_t *lower_one(stmt_t *s, error_t *err) { return lower(s, 1, err); }

void code_free(code_t *c) {
    if (!c) return;
    for (int i = 0; i < c->nops; i++)
        if (c->ops[i].kind == OP_SELECT) free(c->ops[i].targets);
    free(c->ops);
    free(c->labels);
    free(c->lhash);
    free(c);
}

int code_find_label(const code_t *c, const char *name) {
    if (!c->lhcap) return -1;
    for (unsigned b = label_hash(name) & (unsigned)(c->lhcap - 1); c->lhash[b];
         b = (b + 1) & (unsigned)(c->lhcap - 1)) {
        const code_label_t *l = &c->labels[c->lhash[b] - 1];
        if (strcmp(l->name, name) == 0) return l->index;
    }
    return -1;
}
