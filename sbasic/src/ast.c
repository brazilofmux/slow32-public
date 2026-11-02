#include "ast.h"
#include <stdlib.h>
#include <string.h>

/* Safe copy into 64-byte AST name field (always null-terminates; the
 * rest of the field left as it is, unlike strncpy) */
void ast_name_copy(char *dst, const char *src) {
    int i = 0;
    while (i < 63 && src[i]) { dst[i] = src[i]; i++; }
    dst[i] = '\0';
}

/* Slot n of a list of names that holds cap, the list made at its first
 * name (a statement is allocated, and zeroed, once per statement of the
 * program: its lists are not carried in it); NULL past cap or when out
 * of memory */
static char *name_slot(char (**list)[64], int cap, int n) {
    if (n >= cap) return NULL;
    if (!*list && !(*list = calloc((size_t)cap, sizeof **list))) return NULL;
    return (*list)[n];
}

/* --- Expression constructors (unchanged from Stage 1) --- */

expr_t *expr_literal(value_t val, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_LITERAL;
    e->line = line;
    e->literal = val_copy(&val);
    return e;
}

expr_t *expr_variable(const char *name, val_type_t type, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_VARIABLE;
    e->line = line;
    ast_name_copy(e->var.name, name);
    e->var.var_type = type;
    return e;
}

expr_t *expr_unary(unaryop_t op, expr_t *operand, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_UNARY;
    e->line = line;
    e->unary.op = op;
    e->unary.operand = operand;
    return e;
}

expr_t *expr_binary(binop_t op, expr_t *left, expr_t *right, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_BINARY;
    e->line = line;
    e->binary.op = op;
    e->binary.left = left;
    e->binary.right = right;
    return e;
}

expr_t *expr_compare(cmpop_t op, expr_t *left, expr_t *right, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_COMPARE;
    e->line = line;
    e->compare.op = op;
    e->compare.left = left;
    e->compare.right = right;
    return e;
}

expr_t *expr_call(const char *name, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_CALL;
    e->line = line;
    ast_name_copy(e->call.name, name);
    e->call.nargs = 0;
    return e;
}

void expr_call_add_arg(expr_t *call, expr_t *arg) {
    if (call->call.nargs < 8)
        call->call.args[call->call.nargs++] = arg;
}

void expr_free(expr_t *e) {
    while (e) {
        expr_t *next = NULL;  /* for tail-call elimination */
        switch (e->type) {
        case EXPR_LITERAL:
            val_clear(&e->literal);
            break;
        case EXPR_VARIABLE:
        case EXPR_FIELD_ACCESS:
            break;
        case EXPR_UNARY:
            next = e->unary.operand;  /* iterate instead of recurse */
            break;
        case EXPR_BINARY:
            expr_free(e->binary.right);  /* recurse on short side */
            next = e->binary.left;       /* iterate on deep side */
            break;
        case EXPR_COMPARE:
            expr_free(e->compare.right);
            next = e->compare.left;
            break;
        case EXPR_CALL:
            for (int i = 0; i < e->call.nargs; i++)
                expr_free(e->call.args[i]);
            break;
        }
        free(e);
        e = next;
    }
}

/* --- Statement constructors --- */

stmt_t *stmt_alloc(stmt_type_t type, int line) {
    stmt_t *s = calloc(1, sizeof(stmt_t));
    if (!s) return NULL;
    s->type = type;
    s->line = line;
    return s;
}

stmt_t *stmt_print(int line) {
    return stmt_alloc(STMT_PRINT, line);
}

int stmt_print_add(stmt_t *s, expr_t *expr, char sep) {
    int n = s->print.nitems;
    print_item_t *tmp = realloc(s->print.items, (n + 1) * sizeof(print_item_t));
    if (!tmp) return -1;
    s->print.items = tmp;
    s->print.items[n].expr = expr;
    s->print.items[n].sep = sep;
    s->print.nitems = n + 1;
    return 0;
}

stmt_t *stmt_input(const char *prompt, int line) {
    stmt_t *s = stmt_alloc(STMT_INPUT, line);
    if (!s) return NULL;
    if (prompt) {
        s->input.prompt = strdup(prompt);
        if (!s->input.prompt) { free(s); return NULL; }
    }
    return s;
}

int stmt_input_add_var(stmt_t *s, const char *name, val_type_t type) {
    int n = s->input.nvars;
    if (n >= s->input.var_cap) {
        int newcap = s->input.var_cap ? s->input.var_cap * 2 : 4;
        void *tmp1 = realloc(s->input.varnames, newcap * sizeof(s->input.varnames[0]));
        if (!tmp1) return -1;
        s->input.varnames = tmp1;
        void *tmp2 = realloc(s->input.vartypes, newcap * sizeof(s->input.vartypes[0]));
        if (!tmp2) return -1;
        s->input.vartypes = tmp2;
        void *tmp3 = realloc(s->input.targets, newcap * sizeof(s->input.targets[0]));
        if (!tmp3) return -1;
        s->input.targets = tmp3;
        s->input.var_cap = newcap;
    }
    s->input.targets[n] = NULL;
    ast_name_copy(s->input.varnames[n], name);
    s->input.vartypes[n] = type;
    s->input.nvars = n + 1;
    return 0;
}

/* a name's type by its suffix (the parser's rule) */
static val_type_t suffix_type(const char *name) {
    int len = (int)strlen(name);
    if (len > 0 && name[len - 1] == '$') return VAL_STRING;
    if (len > 0 && name[len - 1] == '%') return VAL_INTEGER;
    if (len > 0 && name[len - 1] == '&') return VAL_LONG;
    if (len > 0 && name[len - 1] == '#') return VAL_DOUBLE;
    return VAL_SINGLE;
}

/* a READ or INPUT target: a variable, or an array element, whose
 * expression is kept (and owned) for the subscripts */
static int add_target(stmt_t *s, expr_t *e,
                      int (*add)(stmt_t *, const char *, val_type_t),
                      expr_t **(*targets)(stmt_t *), int *nvars) {
    const char *name = e->type == EXPR_VARIABLE ? e->var.name : e->call.name;
    if (add(s, name, e->type == EXPR_VARIABLE ? e->var.var_type : suffix_type(name)) < 0)
        return -1;
    if (e->type == EXPR_CALL)
        targets(s)[*nvars - 1] = e;
    else
        expr_free(e);
    return 0;
}

static expr_t **input_targets(stmt_t *s) { return s->input.targets; }
static expr_t **read_targets(stmt_t *s) { return s->read_stmt.targets; }

int stmt_input_add_target(stmt_t *s, expr_t *e) {
    return add_target(s, e, stmt_input_add_var, input_targets, &s->input.nvars);
}

int stmt_read_add_target(stmt_t *s, expr_t *e) {
    return add_target(s, e, stmt_read_add_var, read_targets, &s->read_stmt.nvars);
}

stmt_t *stmt_cls(int line) {
    return stmt_alloc(STMT_CLS, line);
}

stmt_t *stmt_locate(expr_t *row, expr_t *col, int line) {
    stmt_t *s = stmt_alloc(STMT_LOCATE, line);
    if (!s) return NULL;
    s->locate_stmt.row = row;
    s->locate_stmt.col = col;
    return s;
}

stmt_t *stmt_color(expr_t *fg, expr_t *bg, int has_fg, int has_bg, int line) {
    stmt_t *s = stmt_alloc(STMT_COLOR, line);
    if (!s) return NULL;
    s->color_stmt.fg = fg;
    s->color_stmt.bg = bg;
    s->color_stmt.has_fg = has_fg;
    s->color_stmt.has_bg = has_bg;
    return s;
}

stmt_t *stmt_assign(const char *name, val_type_t type, expr_t *value, int line) {
    stmt_t *s = stmt_alloc(STMT_ASSIGN, line);
    if (!s) return NULL;
    ast_name_copy(s->assign.name, name);
    s->assign.var_type = type;
    s->assign.value = value;
    return s;
}

stmt_t *stmt_if(expr_t *cond, stmt_t *then_body, stmt_t *else_body, int line) {
    stmt_t *s = stmt_alloc(STMT_IF, line);
    if (!s) return NULL;
    s->if_stmt.condition = cond;
    s->if_stmt.then_body = then_body;
    s->if_stmt.else_body = else_body;
    return s;
}

stmt_t *stmt_for(const char *var, val_type_t type,
                 expr_t *start, expr_t *end, expr_t *step,
                 stmt_t *body, int line) {
    stmt_t *s = stmt_alloc(STMT_FOR, line);
    if (!s) return NULL;
    ast_name_copy(s->for_stmt.var_name, var);
    s->for_stmt.var_type = type;
    s->for_stmt.start = start;
    s->for_stmt.end = end;
    s->for_stmt.step = step;
    s->for_stmt.body = body;
    return s;
}

stmt_t *stmt_while(expr_t *cond, stmt_t *body, int line) {
    stmt_t *s = stmt_alloc(STMT_WHILE, line);
    if (!s) return NULL;
    s->while_stmt.condition = cond;
    s->while_stmt.body = body;
    return s;
}

stmt_t *stmt_end(int line) {
    return stmt_alloc(STMT_END, line);
}

/* --- Stage 2 constructors --- */

stmt_t *stmt_do_loop(expr_t *pre_cond, int pre_until,
                     expr_t *post_cond, int post_until,
                     stmt_t *body, int line) {
    stmt_t *s = stmt_alloc(STMT_DO_LOOP, line);
    if (!s) return NULL;
    s->do_loop.pre_cond = pre_cond;
    s->do_loop.pre_until = pre_until;
    s->do_loop.post_cond = post_cond;
    s->do_loop.post_until = post_until;
    s->do_loop.body = body;
    return s;
}

stmt_t *stmt_select(expr_t *test, case_clause_t *clauses, int line) {
    stmt_t *s = stmt_alloc(STMT_SELECT, line);
    if (!s) return NULL;
    s->select_stmt.test_expr = test;
    s->select_stmt.clauses = clauses;
    return s;
}

stmt_t *stmt_goto(const char *label, int line) {
    stmt_t *s = stmt_alloc(STMT_GOTO, line);
    if (!s) return NULL;
    ast_name_copy(s->goto_stmt.label, label);
    return s;
}

stmt_t *stmt_gosub(const char *label, int line) {
    stmt_t *s = stmt_alloc(STMT_GOSUB, line);
    if (!s) return NULL;
    ast_name_copy(s->goto_stmt.label, label);
    return s;
}

stmt_t *stmt_return(const char *label, int line) {
    stmt_t *s = stmt_alloc(STMT_RETURN, line);
    if (!s) return NULL;
    if (label)
        strncpy(s->goto_stmt.label, label, 63);
    else
        s->goto_stmt.label[0] = '\0';
    return s;
}

stmt_t *stmt_label(const char *name, int line) {
    stmt_t *s = stmt_alloc(STMT_LABEL, line);
    if (!s) return NULL;
    ast_name_copy(s->label.name, name);
    return s;
}

stmt_t *stmt_proc_def(stmt_type_t type, const char *name, int line) {
    stmt_t *s = stmt_alloc(type, line);
    if (!s) return NULL;
    ast_name_copy(s->proc_def.name, name);
    s->proc_def.nparams = 0;
    s->proc_def.return_type = VAL_SINGLE;
    return s;
}

void stmt_proc_add_param(stmt_t *s, const char *name, val_type_t type) {
    int n = s->proc_def.nparams;
    char *slot = name_slot(&s->proc_def.params, 16, n);
    if (!slot) return;
    ast_name_copy(slot, name);
    s->proc_def.param_types[n] = type;
    s->proc_def.nparams = n + 1;
}

void stmt_proc_set_body(stmt_t *s, stmt_t *body) {
    s->proc_def.body = body;
}

stmt_t *stmt_call(const char *name, int line) {
    stmt_t *s = stmt_alloc(STMT_CALL, line);
    if (!s) return NULL;
    ast_name_copy(s->call_stmt.name, name);
    s->call_stmt.nargs = 0;
    return s;
}

void stmt_call_add_arg(stmt_t *s, expr_t *arg) {
    if (s->call_stmt.nargs < 16)
        s->call_stmt.args[s->call_stmt.nargs++] = arg;
}

stmt_t *stmt_next(int line) {
    return stmt_alloc(STMT_NEXT, line);
}

stmt_t *stmt_exit(exit_type_t what, int line) {
    stmt_t *s = stmt_alloc(STMT_EXIT, line);
    if (!s) return NULL;
    s->exit_stmt.what = what;
    return s;
}

stmt_t *stmt_const(const char *name, val_type_t type, expr_t *value, int line) {
    stmt_t *s = stmt_alloc(STMT_CONST, line);
    if (!s) return NULL;
    ast_name_copy(s->const_stmt.name, name);
    s->const_stmt.var_type = type;
    s->const_stmt.value = value;
    return s;
}

stmt_t *stmt_shared(int line) {
    stmt_t *s = stmt_alloc(STMT_SHARED, line);
    return s;
}

void stmt_shared_add(stmt_t *s, const char *name) {
    int n = s->shared.nvars;
    char *slot = name_slot(&s->shared.varnames, 16, n);
    if (!slot) return;
    ast_name_copy(slot, name);
    s->shared.nvars = n + 1;
}

stmt_t *stmt_static_decl(int line) {
    return stmt_alloc(STMT_STATIC, line);
}

void stmt_static_add(stmt_t *s, const char *name) {
    int n = s->shared.nvars;
    char *slot = name_slot(&s->shared.varnames, 16, n);
    if (!slot) return;
    ast_name_copy(slot, name);
    s->shared.nvars = n + 1;
}

/* --- Case clause --- */

case_clause_t *case_clause_alloc(void) {
    return calloc(1, sizeof(case_clause_t));
}

int case_clause_add_value(case_clause_t *c, expr_t *val) {
    int n = c->nmatches;
    case_match_t *tmp = realloc(c->matches, (n + 1) * sizeof(case_match_t));
    if (!tmp) return -1;
    c->matches = tmp;
    c->matches[n].match_type = 0;
    c->matches[n].expr1 = val;
    c->matches[n].expr2 = NULL;
    c->nmatches = n + 1;
    return 0;
}

int case_clause_add_range(case_clause_t *c, expr_t *lo, expr_t *hi) {
    int n = c->nmatches;
    case_match_t *tmp = realloc(c->matches, (n + 1) * sizeof(case_match_t));
    if (!tmp) return -1;
    c->matches = tmp;
    c->matches[n].match_type = 1;
    c->matches[n].expr1 = lo;
    c->matches[n].expr2 = hi;
    c->nmatches = n + 1;
    return 0;
}

int case_clause_add_is(case_clause_t *c, cmpop_t op, expr_t *val) {
    int n = c->nmatches;
    case_match_t *tmp = realloc(c->matches, (n + 1) * sizeof(case_match_t));
    if (!tmp) return -1;
    c->matches = tmp;
    c->matches[n].match_type = 2;
    c->matches[n].is_op = op;
    c->matches[n].expr1 = val;
    c->matches[n].expr2 = NULL;
    c->nmatches = n + 1;
    return 0;
}

void case_clause_free(case_clause_t *c) {
    while (c) {
        case_clause_t *next = c->next;
        for (int i = 0; i < c->nmatches; i++) {
            expr_free(c->matches[i].expr1);
            expr_free(c->matches[i].expr2);
        }
        free(c->matches);
        stmt_free(c->body);
        free(c);
        c = next;
    }
}

/* --- Statement destructor --- */

static void free_targets(expr_t **t, int n) {
    if (!t) return;
    for (int i = 0; i < n; i++) expr_free(t[i]);
    free(t);
}

void stmt_free(stmt_t *s) {
    while (s) {
        stmt_t *next = s->next;
        switch (s->type) {
            case STMT_NEXT:
                free(s->next_stmt.vars);
                break;
            case STMT_PRINT:
            case STMT_LPRINT:
                for (int i = 0; i < s->print.nitems; i++)
                    expr_free(s->print.items[i].expr);
                free(s->print.items);
                free(s->print.using_fmt);
                expr_free(s->print.using_expr);
                break;
            case STMT_INPUT:
                free(s->input.prompt);
                free(s->input.varnames);
                free(s->input.vartypes);
                free_targets(s->input.targets, s->input.nvars);
                break;
            case STMT_CLS:
                break;
            case STMT_LOCATE:
                expr_free(s->locate_stmt.row);
                expr_free(s->locate_stmt.col);
                break;
            case STMT_COLOR:
                expr_free(s->color_stmt.fg);
                expr_free(s->color_stmt.bg);
                break;
            case STMT_ASSIGN:
                expr_free(s->assign.value);
                break;
            case STMT_IF:
                expr_free(s->if_stmt.condition);
                stmt_free(s->if_stmt.then_body);
                stmt_free(s->if_stmt.else_body);
                break;
            case STMT_FOR:
                expr_free(s->for_stmt.start);
                expr_free(s->for_stmt.end);
                expr_free(s->for_stmt.step);
                stmt_free(s->for_stmt.body);
                break;
            case STMT_WHILE:
                expr_free(s->while_stmt.condition);
                stmt_free(s->while_stmt.body);
                break;
            case STMT_DO_LOOP:
                expr_free(s->do_loop.pre_cond);
                expr_free(s->do_loop.post_cond);
                stmt_free(s->do_loop.body);
                break;
            case STMT_SELECT:
                expr_free(s->select_stmt.test_expr);
                case_clause_free(s->select_stmt.clauses);
                break;
            case STMT_GOTO:
            case STMT_GOSUB:
            case STMT_RETURN:
            case STMT_LABEL:
            case STMT_END:
            case STMT_REM:
            case STMT_EXIT:
            case STMT_DECLARE:
                break;
            case STMT_SHARED:
            case STMT_STATIC:
                free(s->shared.varnames);
                break;
            case STMT_SUB_DEF:
            case STMT_FUNC_DEF:
                stmt_free(s->proc_def.body);
                free(s->proc_def.params);
                break;
            case STMT_CALL:
                for (int i = 0; i < s->call_stmt.nargs; i++)
                    expr_free(s->call_stmt.args[i]);
                break;
            case STMT_CONST:
                expr_free(s->const_stmt.value);
                break;
            case STMT_DIM:
                for (int i = 0; i < s->dim_stmt.ndims; i++)
                    expr_free(s->dim_stmt.dims[i]);
                break;
            case STMT_ARRAY_ASSIGN:
                for (int i = 0; i < s->array_assign.nindices; i++)
                    expr_free(s->array_assign.indices[i]);
                expr_free(s->array_assign.value);
                break;
            case STMT_DATA:
                for (int i = 0; i < s->data_stmt.nvalues; i++)
                    val_clear(&s->data_stmt.values[i]);
                free(s->data_stmt.values);
                free(s->data_stmt.quoted);
                break;
            case STMT_OPTION_BASE:
                break;
            case STMT_ERASE:
                free(s->erase_stmt.names);
                break;
            case STMT_RESTORE:
                break;
            case STMT_READ:
                free(s->read_stmt.varnames);
                free(s->read_stmt.vartypes);
                free_targets(s->read_stmt.targets, s->read_stmt.nvars);
                break;
            case STMT_SWAP:
                expr_free(s->swap_stmt.lhs);
                expr_free(s->swap_stmt.rhs);
                break;
            case STMT_RANDOMIZE:
                if (s->randomize.seed) expr_free(s->randomize.seed);
                break;
            case STMT_SLEEP:
                if (s->sleep_stmt.duration) expr_free(s->sleep_stmt.duration);
                break;
            case STMT_OPEN:
                expr_free(s->open_stmt.filename);
                expr_free(s->open_stmt.handle_num);
                expr_free(s->open_stmt.reclen);
                break;
            case STMT_CLOSE:
                if (s->close_stmt.handle_num) expr_free(s->close_stmt.handle_num);
                break;
            case STMT_PRINT_FILE:
            case STMT_WRITE_FILE:
                expr_free(s->print_file.handle_num);
                for (int i = 0; i < s->print_file.nitems; i++)
                    expr_free(s->print_file.items[i].expr);
                free(s->print_file.items);
                free(s->print_file.using_fmt);
                expr_free(s->print_file.using_expr);
                break;
            case STMT_INPUT_FILE:
            case STMT_LINE_INPUT:
                expr_free(s->input_file.handle_num);
                free(s->input_file.varnames);
                free(s->input_file.vartypes);
                break;
            case STMT_KILL:
                expr_free(s->kill_stmt.filename);
                break;
            case STMT_NAME:
                expr_free(s->name_stmt.oldname);
                expr_free(s->name_stmt.newname);
                break;
            case STMT_GET:
            case STMT_PUT:
                expr_free(s->get_put_stmt.handle_num);
                expr_free(s->get_put_stmt.position);
                break;
            case STMT_SEEK:
                expr_free(s->seek_stmt.seek_handle);
                expr_free(s->seek_stmt.seek_position);
                break;
            case STMT_TYPE_DEF:
                free(s->type_def.fields);
                break;
            case STMT_DIM_AS_TYPE:
            case STMT_ON_ERROR:
            case STMT_RESUME:
            case STMT_DEFTYPE:
                break;
            case STMT_FIELD_ASSIGN:
                expr_free(s->field_assign.value);
                break;
            case STMT_MID_ASSIGN:
                expr_free(s->mid_assign.start);
                expr_free(s->mid_assign.length);
                expr_free(s->mid_assign.value);
                break;
            case STMT_ERROR_RAISE:
                expr_free(s->error_raise.errnum);
                break;
            case STMT_ON_GOTO:
            case STMT_ON_GOSUB:
                expr_free(s->on_branch.index);
                free(s->on_branch.labels);
                break;
            case STMT_BEEP:
            case STMT_TRACE:
            case STMT_OPTION_EXPLICIT:
            case STMT_DIM_SCALAR:
                break;
            case STMT_SPLIT:
                expr_free(s->split.str_expr);
                expr_free(s->split.delim_expr);
                break;
            case STMT_LINE_INPUT_CONSOLE:
                free(s->input.prompt);
                free(s->input.varnames);
                free(s->input.vartypes);
                free_targets(s->input.targets, s->input.nvars);
                break;
            case STMT_WRITE_CONSOLE:
                for (int i = 0; i < s->print.nitems; i++)
                    expr_free(s->print.items[i].expr);
                free(s->print.items);
                break;
            case STMT_DEF_FN:
                expr_free(s->def_fn.body);
                free(s->def_fn.params);
                break;
            case STMT_SHELL:
            case STMT_CHDIR:
            case STMT_MKDIR:
            case STMT_RMDIR:
            case STMT_SCREEN:
            case STMT_DEF_SEG:
                expr_free(s->shell_stmt.command);
                break;
            case STMT_POKE:
                expr_free(s->poke_stmt.addr);
                expr_free(s->poke_stmt.value);
                break;
            case STMT_LSET:
            case STMT_RSET:
                expr_free(s->lrset.value);
                break;
            case STMT_WIDTH:
                expr_free(s->width_stmt.columns);
                break;
            case STMT_NOOP:
            case STMT_CLEAR:
                break;
            case STMT_PSET:
            case STMT_LINE_G:
            case STMT_CIRCLE:
            case STMT_PAINT:
            case STMT_PALETTE:
                expr_free(s->gfx.x1);
                expr_free(s->gfx.y1);
                expr_free(s->gfx.x2);
                expr_free(s->gfx.y2);
                expr_free(s->gfx.color);
                expr_free(s->gfx.aux);
                break;
        }
        free(s);
        s = next;
    }
}

/* --- Stage 3 constructors --- */

stmt_t *stmt_dim(const char *name, val_type_t type,
                 expr_t **dims, int ndims,
                 int is_redim, int preserve, int line) {
    stmt_t *s = stmt_alloc(STMT_DIM, line);
    if (!s) return NULL;
    ast_name_copy(s->dim_stmt.name, name);
    s->dim_stmt.elem_type = type;
    s->dim_stmt.ndims = ndims;
    for (int i = 0; i < ndims; i++)
        s->dim_stmt.dims[i] = dims[i];
    s->dim_stmt.is_redim = is_redim;
    s->dim_stmt.preserve = preserve;
    return s;
}

stmt_t *stmt_array_assign(const char *name, val_type_t type,
                          expr_t **indices, int nindices,
                          expr_t *value, int line) {
    stmt_t *s = stmt_alloc(STMT_ARRAY_ASSIGN, line);
    if (!s) return NULL;
    ast_name_copy(s->array_assign.name, name);
    s->array_assign.var_type = type;
    s->array_assign.nindices = nindices;
    for (int i = 0; i < nindices; i++)
        s->array_assign.indices[i] = indices[i];
    s->array_assign.value = value;
    return s;
}

stmt_t *stmt_option_base(int base, int line) {
    stmt_t *s = stmt_alloc(STMT_OPTION_BASE, line);
    if (!s) return NULL;
    s->option_base.base = base;
    return s;
}

stmt_t *stmt_erase(int line) {
    stmt_t *s = stmt_alloc(STMT_ERASE, line);
    return s;
}

void stmt_erase_add(stmt_t *s, const char *name) {
    int n = s->erase_stmt.nnames;
    char *slot = name_slot(&s->erase_stmt.names, 8, n);
    if (!slot) return;
    ast_name_copy(slot, name);
    s->erase_stmt.nnames = n + 1;
}

stmt_t *stmt_data(int line) {
    return stmt_alloc(STMT_DATA, line);
}

int stmt_data_add(stmt_t *s, value_t val, int quoted) {
    int n = s->data_stmt.nvalues;
    value_t *tmp = realloc(s->data_stmt.values, (n + 1) * sizeof(value_t));
    if (!tmp) return -1;
    s->data_stmt.values = tmp;
    unsigned char *q = realloc(s->data_stmt.quoted, n + 1);
    if (!q) return -1;
    s->data_stmt.quoted = q;
    q[n] = (unsigned char)quoted;
    s->data_stmt.values[n] = val_copy(&val);
    s->data_stmt.nvalues = n + 1;
    return 0;
}

stmt_t *stmt_read(int line) {
    return stmt_alloc(STMT_READ, line);
}

int stmt_read_add_var(stmt_t *s, const char *name, val_type_t type) {
    int n = s->read_stmt.nvars;
    if (n >= s->read_stmt.var_cap) {
        int newcap = s->read_stmt.var_cap ? s->read_stmt.var_cap * 2 : 4;
        void *tmp1 = realloc(s->read_stmt.varnames, newcap * sizeof(s->read_stmt.varnames[0]));
        if (!tmp1) return -1;
        s->read_stmt.varnames = tmp1;
        void *tmp2 = realloc(s->read_stmt.vartypes, newcap * sizeof(s->read_stmt.vartypes[0]));
        if (!tmp2) return -1;
        s->read_stmt.vartypes = tmp2;
        void *tmp3 = realloc(s->read_stmt.targets, newcap * sizeof(s->read_stmt.targets[0]));
        if (!tmp3) return -1;
        s->read_stmt.targets = tmp3;
        s->read_stmt.var_cap = newcap;
    }
    s->read_stmt.targets[n] = NULL;
    ast_name_copy(s->read_stmt.varnames[n], name);
    s->read_stmt.vartypes[n] = type;
    s->read_stmt.nvars = n + 1;
    return 0;
}

stmt_t *stmt_restore(const char *label, int line) {
    stmt_t *s = stmt_alloc(STMT_RESTORE, line);
    if (!s) return NULL;
    if (label && label[0])
        ast_name_copy(s->restore_stmt.label, label);
    return s;
}

/* Stage 4 */
stmt_t *stmt_swap(expr_t *lhs, expr_t *rhs, int line) {
    stmt_t *s = stmt_alloc(STMT_SWAP, line);
    if (!s) return NULL;
    s->swap_stmt.lhs = lhs;
    s->swap_stmt.rhs = rhs;
    return s;
}

stmt_t *stmt_randomize(expr_t *seed, int line) {
    stmt_t *s = stmt_alloc(STMT_RANDOMIZE, line);
    if (!s) return NULL;
    s->randomize.seed = seed;
    return s;
}

stmt_t *stmt_sleep(expr_t *duration, int line) {
    stmt_t *s = stmt_alloc(STMT_SLEEP, line);
    if (!s) return NULL;
    s->sleep_stmt.duration = duration;
    return s;
}

/* Stage 5 */
stmt_t *stmt_open(expr_t *filename, int mode, expr_t *handle,
                   expr_t *reclen, int line) {
    stmt_t *s = stmt_alloc(STMT_OPEN, line);
    if (!s) return NULL;
    s->open_stmt.filename = filename;
    s->open_stmt.mode = mode;
    s->open_stmt.handle_num = handle;
    s->open_stmt.reclen = reclen;
    return s;
}

stmt_t *stmt_close(expr_t *handle, int line) {
    stmt_t *s = stmt_alloc(STMT_CLOSE, line);
    if (!s) return NULL;
    s->close_stmt.handle_num = handle;
    return s;
}

stmt_t *stmt_print_file(expr_t *handle, int line) {
    stmt_t *s = stmt_alloc(STMT_PRINT_FILE, line);
    if (!s) return NULL;
    s->print_file.handle_num = handle;
    s->print_file.items = NULL;
    s->print_file.nitems = 0;
    s->print_file.using_fmt = NULL;
    s->print_file.using_expr = NULL;
    return s;
}

int stmt_print_file_add(stmt_t *s, expr_t *expr, char sep) {
    int n = s->print_file.nitems;
    print_item_t *tmp = realloc(s->print_file.items,
                                 (n + 1) * sizeof(print_item_t));
    if (!tmp) return -1;
    s->print_file.items = tmp;
    s->print_file.items[n].expr = expr;
    s->print_file.items[n].sep = sep;
    s->print_file.nitems = n + 1;
    return 0;
}

stmt_t *stmt_write_file(expr_t *handle, int line) {
    stmt_t *s = stmt_alloc(STMT_WRITE_FILE, line);
    if (!s) return NULL;
    s->print_file.handle_num = handle;
    s->print_file.items = NULL;
    s->print_file.nitems = 0;
    return s;
}

int stmt_write_file_add(stmt_t *s, expr_t *expr, char sep) {
    return stmt_print_file_add(s, expr, sep); /* same layout */
}

stmt_t *stmt_input_file(expr_t *handle, int line) {
    stmt_t *s = stmt_alloc(STMT_INPUT_FILE, line);
    if (!s) return NULL;
    s->input_file.handle_num = handle;
    s->input_file.nvars = 0;
    return s;
}

int stmt_input_file_add_var(stmt_t *s, const char *name, val_type_t type) {
    int n = s->input_file.nvars;
    if (n >= s->input_file.var_cap) {
        int newcap = s->input_file.var_cap ? s->input_file.var_cap * 2 : 4;
        void *tmp1 = realloc(s->input_file.varnames, newcap * sizeof(s->input_file.varnames[0]));
        if (!tmp1) return -1;
        s->input_file.varnames = tmp1;
        void *tmp2 = realloc(s->input_file.vartypes, newcap * sizeof(s->input_file.vartypes[0]));
        if (!tmp2) return -1;
        s->input_file.vartypes = tmp2;
        s->input_file.var_cap = newcap;
    }
    ast_name_copy(s->input_file.varnames[n], name);
    s->input_file.vartypes[n] = type;
    s->input_file.nvars = n + 1;
    return 0;
}

stmt_t *stmt_line_input(expr_t *handle, int line) {
    stmt_t *s = stmt_alloc(STMT_LINE_INPUT, line);
    if (!s) return NULL;
    s->input_file.handle_num = handle;
    s->input_file.nvars = 0;
    return s;
}

stmt_t *stmt_kill(expr_t *filename, int line) {
    stmt_t *s = stmt_alloc(STMT_KILL, line);
    if (!s) return NULL;
    s->kill_stmt.filename = filename;
    return s;
}

stmt_t *stmt_name(expr_t *oldname, expr_t *newname, int line) {
    stmt_t *s = stmt_alloc(STMT_NAME, line);
    if (!s) return NULL;
    s->name_stmt.oldname = oldname;
    s->name_stmt.newname = newname;
    return s;
}

stmt_t *stmt_get(expr_t *handle, expr_t *position,
                 const char *varname, val_type_t vartype, int line) {
    stmt_t *s = stmt_alloc(STMT_GET, line);
    if (!s) return NULL;
    s->get_put_stmt.handle_num = handle;
    s->get_put_stmt.position = position;
    ast_name_copy(s->get_put_stmt.varname, varname);
    s->get_put_stmt.vartype = vartype;
    return s;
}

stmt_t *stmt_put(expr_t *handle, expr_t *position,
                 const char *varname, val_type_t vartype, int line) {
    stmt_t *s = stmt_alloc(STMT_PUT, line);
    if (!s) return NULL;
    s->get_put_stmt.handle_num = handle;
    s->get_put_stmt.position = position;
    ast_name_copy(s->get_put_stmt.varname, varname);
    s->get_put_stmt.vartype = vartype;
    return s;
}

stmt_t *stmt_seek(expr_t *handle, expr_t *position, int line) {
    stmt_t *s = stmt_alloc(STMT_SEEK, line);
    if (!s) return NULL;
    s->seek_stmt.seek_handle = handle;
    s->seek_stmt.seek_position = position;
    return s;
}

/* Stage 6 */

expr_t *expr_field_access(const char *var, const char *field, int line) {
    expr_t *e = calloc(1, sizeof(expr_t));
    if (!e) return NULL;
    e->type = EXPR_FIELD_ACCESS;
    e->line = line;
    ast_name_copy(e->field.var_name, var);
    ast_name_copy(e->field.field_name, field);
    return e;
}

stmt_t *stmt_type_def(const char *name, int line) {
    stmt_t *s = stmt_alloc(STMT_TYPE_DEF, line);
    if (!s) return NULL;
    ast_name_copy(s->type_def.name, name);
    s->type_def.fields = NULL;
    s->type_def.nfields = 0;
    return s;
}

void stmt_type_def_add_field(stmt_t *s, const char *name, val_type_t type) {
    int n = s->type_def.nfields;
    struct type_field *f;
    if (n >= 32) return;
    f = realloc(s->type_def.fields, (n + 1) * sizeof(*f));
    if (!f) return;
    s->type_def.fields = f;
    ast_name_copy(f[n].name, name);
    f[n].type = type;
    s->type_def.nfields = n + 1;
}

stmt_t *stmt_dim_as_type(const char *name, const char *type_name, int line) {
    stmt_t *s = stmt_alloc(STMT_DIM_AS_TYPE, line);
    if (!s) return NULL;
    ast_name_copy(s->dim_as_type.name, name);
    ast_name_copy(s->dim_as_type.type_name, type_name);
    return s;
}

stmt_t *stmt_field_assign(const char *var, const char *field,
                          expr_t *value, int line) {
    stmt_t *s = stmt_alloc(STMT_FIELD_ASSIGN, line);
    if (!s) return NULL;
    ast_name_copy(s->field_assign.var_name, var);
    ast_name_copy(s->field_assign.field_name, field);
    s->field_assign.value = value;
    return s;
}

stmt_t *stmt_mid_assign(const char *var, expr_t *start, expr_t *length,
                         expr_t *value, int line) {
    stmt_t *s = stmt_alloc(STMT_MID_ASSIGN, line);
    if (!s) return NULL;
    ast_name_copy(s->mid_assign.var_name, var);
    s->mid_assign.start = start;
    s->mid_assign.length = length;
    s->mid_assign.value = value;
    return s;
}

stmt_t *stmt_on_error(const char *label, int line) {
    stmt_t *s = stmt_alloc(STMT_ON_ERROR, line);
    if (!s) return NULL;
    if (label)
        ast_name_copy(s->on_error.label, label);
    return s;
}

stmt_t *stmt_resume(int resume_next, const char *label, int line) {
    stmt_t *s = stmt_alloc(STMT_RESUME, line);
    if (!s) return NULL;
    s->resume_stmt.resume_next = resume_next;
    s->resume_stmt.label[0] = '\0';
    if (label) {
        strncpy(s->resume_stmt.label, label, 63);
        s->resume_stmt.label[63] = '\0';
    }
    return s;
}

stmt_t *stmt_error_raise(expr_t *errnum, int line) {
    stmt_t *s = stmt_alloc(STMT_ERROR_RAISE, line);
    if (!s) return NULL;
    s->error_raise.errnum = errnum;
    return s;
}

stmt_t *stmt_deftype(val_type_t type, char from, char to, int line) {
    stmt_t *s = stmt_alloc(STMT_DEFTYPE, line);
    if (!s) return NULL;
    s->deftype.def_type = type;
    s->deftype.from = from;
    s->deftype.to = to;
    return s;
}

stmt_t *stmt_on_goto(expr_t *index, int line) {
    stmt_t *s = stmt_alloc(STMT_ON_GOTO, line);
    if (!s) return NULL;
    s->on_branch.index = index;
    s->on_branch.nlabels = 0;
    return s;
}

stmt_t *stmt_on_gosub(expr_t *index, int line) {
    stmt_t *s = stmt_alloc(STMT_ON_GOSUB, line);
    if (!s) return NULL;
    s->on_branch.index = index;
    s->on_branch.nlabels = 0;
    return s;
}

void stmt_on_branch_add_label(stmt_t *s, const char *label) {
    int n = s->on_branch.nlabels;
    char *slot = name_slot(&s->on_branch.labels, 16, n);
    if (!slot) return;
    ast_name_copy(slot, label);
    s->on_branch.nlabels = n + 1;
}

int stmt_next_add_var(stmt_t *s, const char *name) {
    char *slot = name_slot(&s->next_stmt.vars, 8, s->next_stmt.nvars);
    if (!slot) return 0;
    ast_name_copy(slot, name);
    s->next_stmt.nvars++;
    return 1;
}

int stmt_def_fn_add_param(stmt_t *s, const char *name, val_type_t type) {
    int n = s->def_fn.nparams;
    char *slot = name_slot(&s->def_fn.params, 8, n);
    if (!slot) return 0;
    ast_name_copy(slot, name);
    s->def_fn.param_types[n] = type;
    s->def_fn.nparams = n + 1;
    return 1;
}

void stmt_append(stmt_t **head, stmt_t **tail, stmt_t *s) {
    if (!*head) {
        *head = s;
    } else {
        (*tail)->next = s;
    }
    /* Follow chain to find real tail (e.g., label + body) */
    while (s->next) s = s->next;
    *tail = s;
}
