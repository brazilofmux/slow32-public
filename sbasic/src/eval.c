#include "eval.h"
#include "builtin.h"
#include "array.h"
#include "fileio.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

/* --- Procedure and label tables --- */

#define MAX_PROCS 128
#define MAX_LABELS 256
#define MAX_GOSUB_DEPTH 256
#define MAX_DATA_VALUES 1024

typedef struct {
    char name[64];
    stmt_t *def;
} proc_entry_t;

typedef struct {
    char name[64];
    int index;          /* statement index for GOTO */
    int data_offset;    /* data pool offset for RESTORE */
} label_entry_t;

static proc_entry_t proc_table[MAX_PROCS];
static int proc_count = 0;

static label_entry_t label_table[MAX_LABELS];
static int label_count = 0;

static int gosub_stack[MAX_GOSUB_DEPTH];
static int gosub_top = 0;

static int goto_target = -1;

/* Pointer to global env for SHARED variable access */
static env_t *program_global_env = NULL;

/* OPTION BASE setting */
static int option_base = 0;

/* DATA pool */
static value_t data_pool[MAX_DATA_VALUES];
static int data_pool_count = 0;
static int data_read_ptr = 0;

/* TYPE record definitions */
#define MAX_TYPE_DEFS 32

typedef struct {
    char name[64];
    char field_names[32][64];
    val_type_t field_types[32];
    int nfields;
} type_def_t;

static type_def_t type_defs[MAX_TYPE_DEFS];
static int type_def_count = 0;

/* ON ERROR state */
static int on_error_label_idx = -1;  /* -1 = no handler */
static int on_error_active = 0;      /* currently handling an error? */
static int on_error_err_code = 0;    /* ERR value */
static int on_error_err_line = 0;    /* ERL value */
static int on_error_resume_pc = 0;   /* PC to RESUME to */
static int on_error_resume_next_pc = 0; /* PC for RESUME NEXT */
static int resume_is_next = 0;       /* set by RESUME stmt for eval_program */

/* Recursion depth limit (guards against C stack overflow) */
#define MAX_EVAL_DEPTH 32
static int eval_depth = 0;

/* Is this a real error (trappable by ON ERROR) or a flow-control signal? */
static int is_trappable_error(error_t err) {
    switch (err) {
    case ERR_NONE:
    case ERR_EXIT:
    case ERR_BREAK:
    case ERR_EXIT_FOR:
    case ERR_EXIT_WHILE:
    case ERR_EXIT_DO:
    case ERR_EXIT_SUB:
    case ERR_EXIT_FUNCTION:
    case ERR_GOTO:
    case ERR_GOSUB:
    case ERR_RETURN:
    case ERR_RESUME_WITHOUT_ERROR:
    case ERR_ON_ERROR_GOTO:
        return 0;
    default:
        return 1;
    }
}

/* DEFTYPE letter-to-type map: 26 letters A-Z, default VAL_DOUBLE */
static val_type_t deftype_map[26];

static val_type_t deftype_lookup(char first_letter) {
    if (first_letter >= 'A' && first_letter <= 'Z')
        return deftype_map[first_letter - 'A'];
    if (first_letter >= 'a' && first_letter <= 'z')
        return deftype_map[first_letter - 'a'];
    return VAL_DOUBLE;
}

static void deftype_init(void) {
    for (int i = 0; i < 26; i++)
        deftype_map[i] = VAL_DOUBLE;
    env_deftype_hook = deftype_lookup;
}

/* Resolve the effective type for a variable name.
   Explicit suffixes ($, %, #) take priority; bare names use deftype_map. */
static val_type_t resolve_var_type(const char *name) {
    int len = (int)strlen(name);
    if (len > 0) {
        char last = name[len - 1];
        if (last == '$') return VAL_STRING;
        if (last == '%') return VAL_INTEGER;
        if (last == '#') return VAL_DOUBLE;
    }
    return deftype_lookup(name[0]);
}

/* Find a TYPE definition by name */
static int find_type_def(const char *name) {
    for (int i = 0; i < type_def_count; i++) {
        if (strcmp(type_defs[i].name, name) == 0)
            return i;
    }
    return -1;
}

/* Find field index in a type def */
static int find_field_index(type_def_t *td, const char *field) {
    for (int i = 0; i < td->nfields; i++) {
        if (strcmp(td->field_names[i], field) == 0)
            return i;
    }
    return -1;
}

/* --- Lookup helpers --- */

static proc_entry_t *find_proc(const char *name) {
    for (int i = 0; i < proc_count; i++) {
        if (strcmp(proc_table[i].name, name) == 0)
            return &proc_table[i];
    }
    return NULL;
}

static int find_label(const char *name) {
    for (int i = 0; i < label_count; i++) {
        if (strcmp(label_table[i].name, name) == 0)
            return label_table[i].index;
    }
    return -1;
}

static int find_label_data_offset(const char *name) {
    for (int i = 0; i < label_count; i++) {
        if (strcmp(label_table[i].name, name) == 0)
            return label_table[i].data_offset;
    }
    return -1;
}

/* --- User-defined procedure call --- */

static error_t call_proc(env_t *caller_env, proc_entry_t *proc,
                          expr_t **arg_exprs, int nargs, value_t *out) {
    stmt_t *def = proc->def;
    int is_function = (def->type == STMT_FUNC_DEF);

    if (nargs != def->proc_def.nparams)
        return ERR_ILLEGAL_FUNCTION_CALL;

    value_t arg_vals[16];
    for (int i = 0; i < nargs; i++) {
        error_t err = eval_expr(caller_env, arg_exprs[i], &arg_vals[i]);
        if (err != ERR_NONE) {
            for (int j = 0; j < i; j++) val_clear(&arg_vals[j]);
            return err;
        }
    }

    env_t *local = env_create(NULL);

    for (int i = 0; i < nargs; i++) {
        env_set(local, def->proc_def.params[i], &arg_vals[i]);
        val_clear(&arg_vals[i]);
    }

    /* Collect and copy-in SHARED variables (pointers into AST, no copy) */
    const char *shared_names[32];
    int nshared = 0;
    for (stmt_t *s = def->proc_def.body; s && nshared < 32; s = s->next) {
        if (s->type == STMT_SHARED) {
            for (int i = 0; i < s->shared.nvars && nshared < 32; i++) {
                shared_names[nshared] = s->shared.varnames[i];
                nshared++;
            }
        }
    }
    for (int i = 0; i < nshared; i++) {
        value_t *gv = env_get(program_global_env, shared_names[i]);
        if (gv) env_set(local, shared_names[i], gv);
    }

    error_t err = eval_stmts(local, def->proc_def.body);

    if (err == ERR_EXIT_SUB && !is_function) err = ERR_NONE;
    if (err == ERR_EXIT_FUNCTION && is_function) err = ERR_NONE;

    if (is_function && out) {
        if (err == ERR_NONE) {
            value_t *retval = env_get(local, def->proc_def.name);
            if (retval)
                *out = val_copy(retval);
            else
                *out = val_double(0.0);
        }
    }

    for (int i = 0; i < nshared; i++) {
        value_t *lv = env_get(local, shared_names[i]);
        if (lv) env_set(program_global_env, shared_names[i], lv);
    }

    env_destroy(local);
    return err;
}

/* --- Expression evaluation --- */

static error_t eval_expr_impl(env_t *env, expr_t *e, value_t *out);

error_t eval_expr(env_t *env, expr_t *e, value_t *out) {
    if (++eval_depth > MAX_EVAL_DEPTH) {
        eval_depth--;
        return ERR_STACK_OVERFLOW;
    }
    error_t err = eval_expr_impl(env, e, out);
    eval_depth--;
    return err;
}

static error_t eval_expr_impl(env_t *env, expr_t *e, value_t *out) {
    if (!e) return ERR_INTERNAL;

    switch (e->type) {
        case EXPR_LITERAL:
            *out = val_copy(&e->literal);
            return ERR_NONE;

        case EXPR_VARIABLE: {
            /* ERR and ERL pseudo-variables */
            if (strcmp(e->var.name, "ERR") == 0) {
                *out = val_integer(on_error_err_code);
                return ERR_NONE;
            }
            if (strcmp(e->var.name, "ERL") == 0) {
                *out = val_integer(on_error_err_line);
                return ERR_NONE;
            }
            value_t *v = env_get(env, e->var.name);
            if (!v) return ERR_OUT_OF_MEMORY;
            *out = val_copy(v);
            return ERR_NONE;
        }

        case EXPR_UNARY: {
            value_t operand;
            EVAL_CHECK(eval_expr(env, e->unary.operand, &operand));
            error_t err;
            if (e->unary.op == OP_NEG) {
                err = val_neg(&operand, out);
            } else {
                *out = val_integer(val_is_true(&operand) ? 0 : -1);
                err = ERR_NONE;
            }
            val_clear(&operand);
            return err;
        }

        case EXPR_BINARY: {
            if (e->binary.op == OP_AND) {
                value_t left;
                EVAL_CHECK(eval_expr(env, e->binary.left, &left));
                if (!val_is_true(&left)) {
                    val_clear(&left);
                    *out = val_integer(0);
                    return ERR_NONE;
                }
                val_clear(&left);
                value_t right;
                EVAL_CHECK(eval_expr(env, e->binary.right, &right));
                *out = val_integer(val_is_true(&right) ? -1 : 0);
                val_clear(&right);
                return ERR_NONE;
            }
            if (e->binary.op == OP_OR) {
                value_t left;
                EVAL_CHECK(eval_expr(env, e->binary.left, &left));
                if (val_is_true(&left)) {
                    val_clear(&left);
                    *out = val_integer(-1);
                    return ERR_NONE;
                }
                val_clear(&left);
                value_t right;
                EVAL_CHECK(eval_expr(env, e->binary.right, &right));
                *out = val_integer(val_is_true(&right) ? -1 : 0);
                val_clear(&right);
                return ERR_NONE;
            }

            value_t left, right;
            EVAL_CHECK(eval_expr(env, e->binary.left, &left));
            error_t err = eval_expr(env, e->binary.right, &right);
            if (err != ERR_NONE) {
                val_clear(&left);
                return err;
            }

            switch (e->binary.op) {
                case OP_ADD:  err = val_add(&left, &right, out); break;
                case OP_SUB:  err = val_sub(&left, &right, out); break;
                case OP_MUL:  err = val_mul(&left, &right, out); break;
                case OP_DIV:  err = val_div(&left, &right, out); break;
                case OP_IDIV: err = val_idiv(&left, &right, out); break;
                case OP_MOD:  err = val_mod(&left, &right, out); break;
                case OP_POW:  err = val_pow(&left, &right, out); break;
                default:      err = ERR_INTERNAL; break;
            }
            val_clear(&left);
            val_clear(&right);
            return err;
        }

        case EXPR_COMPARE: {
            value_t left, right;
            EVAL_CHECK(eval_expr(env, e->compare.left, &left));
            error_t err = eval_expr(env, e->compare.right, &right);
            if (err != ERR_NONE) {
                val_clear(&left);
                return err;
            }
            int cmp;
            err = val_compare(&left, &right, &cmp);
            val_clear(&left);
            val_clear(&right);
            if (err != ERR_NONE) return err;

            int result;
            switch (e->compare.op) {
                case CMP_EQ: result = (cmp == 0); break;
                case CMP_NE: result = (cmp != 0); break;
                case CMP_LT: result = (cmp < 0); break;
                case CMP_GT: result = (cmp > 0); break;
                case CMP_LE: result = (cmp <= 0); break;
                case CMP_GE: result = (cmp >= 0); break;
                default:     result = 0; break;
            }
            *out = val_integer(result ? -1 : 0);
            return ERR_NONE;
        }

        case EXPR_CALL: {
            /* 1. Check for user-defined function */
            proc_entry_t *proc = find_proc(e->call.name);
            if (proc && proc->def->type == STMT_FUNC_DEF)
                return call_proc(env, proc, e->call.args, e->call.nargs, out);

            /* 2. Check for array element access */
            sb_array_t *arr = array_find(e->call.name);
            if (arr) {
                int indices[MAX_ARRAY_DIMS];
                if (e->call.nargs != arr->ndims)
                    return ERR_SUBSCRIPT_OUT_OF_RANGE;
                for (int i = 0; i < e->call.nargs; i++) {
                    value_t iv;
                    error_t err = eval_expr(env, e->call.args[i], &iv);
                    if (err != ERR_NONE) return err;
                    err = val_to_integer(&iv, &indices[i]);
                    val_clear(&iv);
                    if (err != ERR_NONE) return err;
                }
                value_t *elem;
                error_t err = array_get(e->call.name, indices,
                                        e->call.nargs, &elem);
                if (err != ERR_NONE) return err;
                *out = val_copy(elem);
                return ERR_NONE;
            }

            /* 3. LBOUND / UBOUND special handling */
            if (strcmp(e->call.name, "LBOUND") == 0 ||
                strcmp(e->call.name, "UBOUND") == 0) {
                if (e->call.nargs < 1) return ERR_ILLEGAL_FUNCTION_CALL;
                const char *arrname;
                if (e->call.args[0]->type == EXPR_VARIABLE)
                    arrname = e->call.args[0]->var.name;
                else
                    return ERR_ILLEGAL_FUNCTION_CALL;
                int dim = 1;
                if (e->call.nargs >= 2) {
                    value_t dv;
                    EVAL_CHECK(eval_expr(env, e->call.args[1], &dv));
                    error_t err = val_to_integer(&dv, &dim);
                    val_clear(&dv);
                    if (err != ERR_NONE) return err;
                }
                sb_array_t *a = array_find(arrname);
                if (!a) return ERR_UNDEFINED_VAR;
                if (dim < 1 || dim > a->ndims)
                    return ERR_ILLEGAL_FUNCTION_CALL;
                if (strcmp(e->call.name, "LBOUND") == 0)
                    *out = val_integer(a->base);
                else
                    *out = val_integer(a->base + a->dims[dim - 1] - 1);
                return ERR_NONE;
            }

            /* 4. Built-in functions */
            value_t args[8];
            int nargs = e->call.nargs;
            for (int i = 0; i < nargs; i++) {
                error_t err = eval_expr(env, e->call.args[i], &args[i]);
                if (err != ERR_NONE) {
                    for (int j = 0; j < i; j++) val_clear(&args[j]);
                    return err;
                }
            }
            /* Check for ERR and ERL pseudo-functions */
            if (strcmp(e->call.name, "ERR") == 0 && nargs == 0) {
                *out = val_integer(on_error_err_code);
                return ERR_NONE;
            }
            if (strcmp(e->call.name, "ERL") == 0 && nargs == 0) {
                *out = val_integer(on_error_err_line);
                return ERR_NONE;
            }

            error_t err = builtin_call(e->call.name, args, nargs, out);
            for (int i = 0; i < nargs; i++) val_clear(&args[i]);
            return err;
        }

        case EXPR_FIELD_ACCESS: {
            value_t *v = env_get(env, e->field.var_name);
            if (!v || v->type != VAL_RECORD) return ERR_UNDEFINED_VAR;
            type_def_t *td = &type_defs[v->rval->type_idx];
            int fi = find_field_index(td, e->field.field_name);
            if (fi < 0) return ERR_UNDEFINED_FIELD;
            *out = val_copy(&v->rval->fields[fi]);
            return ERR_NONE;
        }
    }

    return ERR_INTERNAL;
}

/* --- Statement execution --- */

/* PRINT USING format engine:
   Returns number of format chars consumed. Output goes to stdout.
   Uses snprintf with embedded precision (e.g. "%.2f") instead of %*.*f
   which has varargs issues on SLOW-32. */

/* --- Print column tracking --- */

int print_col = 0;

/* Print a string to stdout and update print_col */
static void col_puts(const char *s) {
    while (*s) {
        putchar(*s);
        if (*s == '\n' || *s == '\r')
            print_col = 0;
        else if (*s == '\t')
            print_col = (print_col + 8) & ~7;  /* tab stops every 8 */
        else
            print_col++;
        s++;
    }
}

/* Printf wrapper that tracks print_col */
static void col_printf(const char *fmt, ...) {
    char buf[1024];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    col_puts(buf);
}

static double fmt_fabs(double x) { return x < 0.0 ? -x : x; }

/* Format double into buf with exactly 'dec' decimal places, right-justified
   in 'width' characters. Returns strlen of result.
   Uses snprintf internally — handles any magnitude correctly. */
static int fmt_fixed(char *buf, int bufsz, double val, int width, int dec) {
    char tmp[512];
    char fmtstr[16];
    /* Build "%.Xf" format string (avoids %*.*f varargs issues on SLOW-32) */
    if (dec < 0) dec = 0;
    if (dec > 20) dec = 20;
    snprintf(fmtstr, sizeof(fmtstr), "%%.%df", dec);
    int len = snprintf(tmp, sizeof(tmp), fmtstr, val);
    if (len < 0) len = 0;
    if (len >= (int)sizeof(tmp)) len = (int)sizeof(tmp) - 1;
    /* Right-justify in width */
    int padding = width - len;
    if (padding < 0) padding = 0;
    int pos = 0;
    for (int i = 0; i < padding && pos < bufsz - 1; i++) buf[pos++] = ' ';
    for (int i = 0; i < len && pos < bufsz - 1; i++) buf[pos++] = tmp[i];
    buf[pos] = '\0';
    return pos;
}

static int format_using_num(const char *fmt, double val) {
    const char *start = fmt;
    int has_plus = 0, has_minus = 0, has_dollar = 0, has_stars = 0;
    int has_dot = 0, has_exp = 0;
    int width = 0, decimals = 0;

    const char *p = fmt;
    if (*p == '+') { has_plus = 1; p++; width++; }
    if (p[0] == '$' && p[1] == '$') { has_dollar = 1; p += 2; width += 2; }
    if (p[0] == '*' && p[1] == '*') { has_stars = 1; p += 2; width += 2; }
    while (*p == '#') { width++; p++; }
    if (*p == '.') { has_dot = 1; p++; width++; while (*p == '#') { decimals++; width++; p++; } }
    if (*p == '-') { has_minus = 1; p++; width++; }
    if (p[0] == '^' && p[1] == '^' && p[2] == '^' && p[3] == '^') { has_exp = 1; p += 4; width += 4; }

    if (width == 0) return 0;

    char buf[512];
    if (has_exp) {
        /* Scientific notation: manually format */
        int neg = (val < 0.0);
        double av = fmt_fabs(val);
        int exp_val = 0;
        if (av != 0.0) {
            while (av >= 10.0) { av /= 10.0; exp_val++; }
            while (av < 1.0) { av *= 10.0; exp_val--; }
        }
        if (neg) av = -av;
        /* Format mantissa without exponent part */
        int mw = width - 4; /* subtract ^^^^  */
        fmt_fixed(buf, sizeof(buf), av, mw, decimals);
        col_puts(buf);
        /* Exponent */
        col_printf("E%c%02d", exp_val >= 0 ? '+' : '-',
               exp_val >= 0 ? exp_val : -exp_val);
    } else {
        int fw = width - has_minus;
        fmt_fixed(buf, sizeof(buf), val, fw, decimals);
        int blen = (int)strlen(buf);
        if (has_stars) {
            for (int i = 0; i < blen; i++) {
                if (buf[i] == ' ') buf[i] = '*';
                else break;
            }
        }
        if (has_dollar && blen > 0) {
            int i = 0;
            while (i < blen && (buf[i] == ' ' || buf[i] == '*')) i++;
            if (i > 0) buf[i - 1] = '$';
        }
        col_puts(buf);
        if (has_minus) {
            if (val < 0.0) col_puts("-");
            else col_puts(" ");
        }
    }
    return (int)(p - start);
}

static int format_using_str(const char *fmt, const char *str, int slen) {
    if (*fmt == '!') {
        /* First character only */
        col_printf("%c", slen > 0 ? str[0] : ' ');
        return 1;
    }
    if (*fmt == '&') {
        /* Entire string */
        col_puts(str);
        return 1;
    }
    if (*fmt == '\\') {
        /* Fixed width: count chars between backslashes */
        const char *p = fmt + 1;
        int width = 2; /* the two backslashes count as positions */
        while (*p && *p != '\\') { width++; p++; }
        if (*p == '\\') p++;
        /* Print string padded to width */
        int printed = 0;
        for (int i = 0; i < width && i < slen; i++) { col_printf("%c", str[i]); printed++; }
        for (int i = printed; i < width; i++) col_puts(" ");
        return (int)(p - fmt);
    }
    return 0;
}

static error_t exec_print(env_t *env, stmt_t *s) {
    /* PRINT USING */
    if (s->print.using_fmt) {
        const char *fmt = s->print.using_fmt;
        int arg_idx = 0;
        while (*fmt) {
            if (*fmt == '#' || *fmt == '+' || *fmt == '$' || *fmt == '*') {
                /* Numeric format */
                if (arg_idx < s->print.nitems && s->print.items[arg_idx].expr) {
                    value_t v;
                    EVAL_CHECK(eval_expr(env, s->print.items[arg_idx].expr, &v));
                    double d = 0.0;
                    val_to_double(&v, &d);  /* best-effort; strings → 0 */
                    val_clear(&v);
                    int consumed = format_using_num(fmt, d);
                    if (consumed > 0) { fmt += consumed; arg_idx++; continue; }
                }
                col_printf("%c", *fmt++);
            } else if (*fmt == '!' || *fmt == '&' || *fmt == '\\') {
                /* String format */
                if (arg_idx < s->print.nitems && s->print.items[arg_idx].expr) {
                    value_t v;
                    EVAL_CHECK(eval_expr(env, s->print.items[arg_idx].expr, &v));
                    const char *str = (v.type == VAL_STRING) ? v.sval->data : "";
                    int slen = (v.type == VAL_STRING) ? v.sval->len : 0;
                    int consumed = format_using_str(fmt, str, slen);
                    val_clear(&v);
                    if (consumed > 0) { fmt += consumed; arg_idx++; continue; }
                }
                col_printf("%c", *fmt++);
            } else if (*fmt == '_') {
                /* Literal next character */
                fmt++;
                if (*fmt) col_printf("%c", *fmt++);
            } else {
                col_printf("%c", *fmt++);
            }
        }
        col_puts("\n");
        return ERR_NONE;
    }

    int needs_newline = 1;

    for (int i = 0; i < s->print.nitems; i++) {
        print_item_t *item = &s->print.items[i];

        if (item->expr) {
            value_t v;
            EVAL_CHECK(eval_expr(env, item->expr, &v));

            switch (v.type) {
                case VAL_INTEGER:
                    if (v.ival >= 0) col_printf(" %d", v.ival);
                    else col_printf("%d", v.ival);
                    break;
                case VAL_DOUBLE:
                    if (v.dval >= 0) col_printf(" %g", v.dval);
                    else col_printf("%g", v.dval);
                    break;
                case VAL_STRING:
                    col_puts(v.sval->data);
                    break;
                case VAL_RECORD:
                    break;
            }
            val_clear(&v);
        }

        if (item->sep == ';') {
            needs_newline = 0;
        } else if (item->sep == ',') {
            col_puts("\t");
            needs_newline = 0;
        } else {
            needs_newline = 1;
        }
    }

    if (needs_newline)
        col_puts("\n");

    return ERR_NONE;
}

static error_t exec_input(env_t *env, stmt_t *s) {
    if (s->input.prompt)
        col_puts(s->input.prompt);
    else
        col_puts("? ");

    char line[1024];
    int pos = 0;
    int ch;
    while ((ch = getchar()) != EOF && ch != '\n') {
        if (pos < 1023)
            line[pos++] = (char)ch;
    }
    line[pos] = '\0';

    print_col = 0; /* newline consumed by getchar */

    if (ch == EOF && pos == 0)
        return ERR_INPUT_PAST_END;

    const char *p = line;
    for (int i = 0; i < s->input.nvars; i++) {
        while (*p == ' ') p++;

        if (s->input.vartypes[i] == VAL_STRING) {
            const char *start = p;
            while (*p && *p != ',') p++;
            int len = (int)(p - start);
            while (len > 0 && start[len - 1] == ' ') len--;
            value_t v = val_string(start, len);
            env_set(env, s->input.varnames[i], &v);
            val_clear(&v);
        } else {
            char *endp;
            double d = strtod(p, &endp);
            if (endp == p) d = 0.0;
            p = endp;
            if (s->input.vartypes[i] == VAL_INTEGER) {
                value_t v = val_integer((int)d);
                env_set(env, s->input.varnames[i], &v);
            } else {
                value_t v = val_double(d);
                env_set(env, s->input.varnames[i], &v);
            }
        }

        if (*p == ',') p++;
    }

    return ERR_NONE;
}

/* Resolve an lvalue expression to a pointer to its storage.
   Supports: simple variable, array element, record field. */
static error_t resolve_lvalue(env_t *env, expr_t *e, value_t **out) {
    switch (e->type) {
    case EXPR_VARIABLE: {
        value_t *v = env_get(env, e->var.name);
        if (!v) {
            /* Auto-create with default type */
            value_t def = {.type = e->var.var_type};
            env_set(env, e->var.name, &def);
            v = env_get(env, e->var.name);
        }
        *out = v;
        return ERR_NONE;
    }
    case EXPR_CALL: {
        /* Array element access */
        int indices[MAX_ARRAY_DIMS];
        for (int i = 0; i < e->call.nargs; i++) {
            value_t iv;
            error_t err = eval_expr(env, e->call.args[i], &iv);
            if (err != ERR_NONE) return err;
            err = val_to_integer(&iv, &indices[i]);
            val_clear(&iv);
            if (err != ERR_NONE) return err;
        }
        return array_get(e->call.name, indices, e->call.nargs, out);
    }
    case EXPR_FIELD_ACCESS: {
        value_t *v = env_get(env, e->field.var_name);
        if (!v || v->type != VAL_RECORD) return ERR_UNDEFINED_VAR;
        type_def_t *td = &type_defs[v->rval->type_idx];
        int fi = find_field_index(td, e->field.field_name);
        if (fi < 0) return ERR_UNDEFINED_FIELD;
        *out = &v->rval->fields[fi];
        return ERR_NONE;
    }
    default:
        return ERR_TYPE_MISMATCH;
    }
}

static error_t exec_assign(env_t *env, stmt_t *s) {
    if (env_is_const(env, s->assign.name))
        return ERR_CONST_REASSIGN;

    value_t v;
    EVAL_CHECK(eval_expr(env, s->assign.value, &v));

    val_type_t vt = resolve_var_type(s->assign.name);
    if (vt == VAL_INTEGER && v.type != VAL_INTEGER) {
        int iv;
        error_t err = val_to_integer(&v, &iv);
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_integer(iv);
    } else if (vt == VAL_STRING && v.type != VAL_STRING) {
        char buf[256];
        error_t err = val_to_string(&v, buf, sizeof(buf));
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_string_cstr(buf);
    }

    env_set(env, s->assign.name, &v);
    val_clear(&v);
    return ERR_NONE;
}

static error_t exec_if(env_t *env, stmt_t *s) {
    value_t cond;
    EVAL_CHECK(eval_expr(env, s->if_stmt.condition, &cond));
    int is_true = val_is_true(&cond);
    val_clear(&cond);

    if (is_true) {
        if (s->if_stmt.then_body)
            return eval_stmts(env, s->if_stmt.then_body);
    } else {
        if (s->if_stmt.else_body)
            return eval_stmts(env, s->if_stmt.else_body);
    }
    return ERR_NONE;
}

static error_t exec_for(env_t *env, stmt_t *s) {
    value_t start_val, end_val, step_val;
    EVAL_CHECK(eval_expr(env, s->for_stmt.start, &start_val));

    error_t err = eval_expr(env, s->for_stmt.end, &end_val);
    if (err != ERR_NONE) { val_clear(&start_val); return err; }

    if (s->for_stmt.step) {
        err = eval_expr(env, s->for_stmt.step, &step_val);
        if (err != ERR_NONE) {
            val_clear(&start_val);
            val_clear(&end_val);
            return err;
        }
    } else {
        step_val = val_integer(1);
    }

    double counter, limit, step;
    error_t cerr = val_to_double(&start_val, &counter);
    if (cerr == ERR_NONE) cerr = val_to_double(&end_val, &limit);
    if (cerr == ERR_NONE) cerr = val_to_double(&step_val, &step);
    val_clear(&start_val);
    val_clear(&end_val);
    val_clear(&step_val);
    if (cerr != ERR_NONE) return cerr;

    if (step == 0.0) return ERR_ILLEGAL_FUNCTION_CALL;

    while (1) {
        if (step > 0 && counter > limit) break;
        if (step < 0 && counter < limit) break;

        if (s->for_stmt.var_type == VAL_INTEGER) {
            value_t v = val_integer((int)counter);
            env_set(env, s->for_stmt.var_name, &v);
        } else {
            value_t v = val_double(counter);
            env_set(env, s->for_stmt.var_name, &v);
        }

        if (s->for_stmt.body) {
            err = eval_stmts(env, s->for_stmt.body);
            if (err == ERR_BREAK || err == ERR_EXIT_FOR) break;
            if (err != ERR_NONE) return err;
        }

        counter += step;
    }

    return ERR_NONE;
}

static error_t exec_while(env_t *env, stmt_t *s) {
    while (1) {
        value_t cond;
        EVAL_CHECK(eval_expr(env, s->while_stmt.condition, &cond));
        int is_true = val_is_true(&cond);
        val_clear(&cond);
        if (!is_true) break;

        if (s->while_stmt.body) {
            error_t err = eval_stmts(env, s->while_stmt.body);
            if (err == ERR_BREAK || err == ERR_EXIT_WHILE) break;
            if (err != ERR_NONE) return err;
        }
    }
    return ERR_NONE;
}

static error_t exec_do_loop(env_t *env, stmt_t *s) {
    while (1) {
        if (s->do_loop.pre_cond) {
            value_t cond;
            EVAL_CHECK(eval_expr(env, s->do_loop.pre_cond, &cond));
            int is_true = val_is_true(&cond);
            val_clear(&cond);
            if (s->do_loop.pre_until) is_true = !is_true;
            if (!is_true) break;
        }

        if (s->do_loop.body) {
            error_t err = eval_stmts(env, s->do_loop.body);
            if (err == ERR_EXIT_DO) break;
            if (err != ERR_NONE) return err;
        }

        if (s->do_loop.post_cond) {
            value_t cond;
            EVAL_CHECK(eval_expr(env, s->do_loop.post_cond, &cond));
            int is_true = val_is_true(&cond);
            val_clear(&cond);
            if (s->do_loop.post_until) is_true = !is_true;
            if (!is_true) break;
        }
    }
    return ERR_NONE;
}

static error_t exec_select(env_t *env, stmt_t *s) {
    value_t test;
    EVAL_CHECK(eval_expr(env, s->select_stmt.test_expr, &test));

    case_clause_t *matched = NULL;
    case_clause_t *else_clause = NULL;

    for (case_clause_t *c = s->select_stmt.clauses; c; c = c->next) {
        if (c->is_else) {
            else_clause = c;
            continue;
        }

        int found = 0;
        for (int i = 0; i < c->nmatches && !found; i++) {
            case_match_t *m = &c->matches[i];
            if (m->match_type == 0) {
                value_t v;
                error_t err = eval_expr(env, m->expr1, &v);
                if (err != ERR_NONE) { val_clear(&test); return err; }
                int cmp;
                err = val_compare(&test, &v, &cmp);
                val_clear(&v);
                if (err != ERR_NONE) { val_clear(&test); return err; }
                if (cmp == 0) found = 1;
            } else if (m->match_type == 1) {
                value_t lo, hi;
                error_t err = eval_expr(env, m->expr1, &lo);
                if (err != ERR_NONE) { val_clear(&test); return err; }
                err = eval_expr(env, m->expr2, &hi);
                if (err != ERR_NONE) { val_clear(&lo); val_clear(&test); return err; }
                int cmp_lo, cmp_hi;
                err = val_compare(&test, &lo, &cmp_lo);
                if (err == ERR_NONE)
                    err = val_compare(&test, &hi, &cmp_hi);
                val_clear(&lo);
                val_clear(&hi);
                if (err != ERR_NONE) { val_clear(&test); return err; }
                if (cmp_lo >= 0 && cmp_hi <= 0) found = 1;
            } else if (m->match_type == 2) {
                value_t v;
                error_t err = eval_expr(env, m->expr1, &v);
                if (err != ERR_NONE) { val_clear(&test); return err; }
                int cmp;
                err = val_compare(&test, &v, &cmp);
                val_clear(&v);
                if (err != ERR_NONE) { val_clear(&test); return err; }
                switch (m->is_op) {
                    case CMP_EQ: found = (cmp == 0); break;
                    case CMP_NE: found = (cmp != 0); break;
                    case CMP_LT: found = (cmp < 0); break;
                    case CMP_GT: found = (cmp > 0); break;
                    case CMP_LE: found = (cmp <= 0); break;
                    case CMP_GE: found = (cmp >= 0); break;
                }
            }
        }

        if (found) {
            matched = c;
            break;
        }
    }

    val_clear(&test);
    if (!matched) matched = else_clause;
    if (matched && matched->body)
        return eval_stmts(env, matched->body);
    return ERR_NONE;
}

static error_t exec_const(env_t *env, stmt_t *s) {
    value_t v;
    EVAL_CHECK(eval_expr(env, s->const_stmt.value, &v));

    if (s->const_stmt.var_type == VAL_INTEGER && v.type != VAL_INTEGER) {
        int iv;
        error_t err = val_to_integer(&v, &iv);
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_integer(iv);
    } else if (s->const_stmt.var_type == VAL_STRING && v.type != VAL_STRING) {
        char buf[256];
        error_t err = val_to_string(&v, buf, sizeof(buf));
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_string_cstr(buf);
    }

    env_set_const(env, s->const_stmt.name, &v);
    val_clear(&v);
    return ERR_NONE;
}

static error_t exec_call_stmt(env_t *env, stmt_t *s) {
    proc_entry_t *proc = find_proc(s->call_stmt.name);
    if (!proc)
        return ERR_UNDEFINED_PROC;

    if (proc->def->type == STMT_FUNC_DEF) {
        value_t result;
        error_t err = call_proc(env, proc, s->call_stmt.args,
                                s->call_stmt.nargs, &result);
        if (err == ERR_NONE) val_clear(&result);
        return err;
    }

    return call_proc(env, proc, s->call_stmt.args,
                     s->call_stmt.nargs, NULL);
}

/* --- Stage 3 statement handlers --- */

static error_t exec_dim(env_t *env, stmt_t *s) {
    int sizes[MAX_ARRAY_DIMS];
    for (int i = 0; i < s->dim_stmt.ndims; i++) {
        value_t dv;
        EVAL_CHECK(eval_expr(env, s->dim_stmt.dims[i], &dv));
        int upper;
        error_t err = val_to_integer(&dv, &upper);
        val_clear(&dv);
        if (err != ERR_NONE) return err;
        sizes[i] = upper - option_base + 1;
        if (sizes[i] <= 0) return ERR_ILLEGAL_FUNCTION_CALL;
    }

    if (s->dim_stmt.is_redim)
        return array_redim(s->dim_stmt.name, s->dim_stmt.elem_type,
                           sizes, s->dim_stmt.ndims, option_base,
                           s->dim_stmt.preserve);
    else
        return array_dim(s->dim_stmt.name, s->dim_stmt.elem_type,
                         sizes, s->dim_stmt.ndims, option_base);
}

static error_t exec_array_assign(env_t *env, stmt_t *s) {
    int indices[MAX_ARRAY_DIMS];
    for (int i = 0; i < s->array_assign.nindices; i++) {
        value_t iv;
        EVAL_CHECK(eval_expr(env, s->array_assign.indices[i], &iv));
        error_t err = val_to_integer(&iv, &indices[i]);
        val_clear(&iv);
        if (err != ERR_NONE) return err;
    }

    value_t v;
    EVAL_CHECK(eval_expr(env, s->array_assign.value, &v));

    /* Coerce to array element type */
    sb_array_t *arr = array_find(s->array_assign.name);
    if (!arr) {
        val_clear(&v);
        return ERR_UNDEFINED_VAR;
    }

    if (arr->elem_type == VAL_INTEGER && v.type != VAL_INTEGER) {
        int iv;
        error_t err = val_to_integer(&v, &iv);
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_integer(iv);
    } else if (arr->elem_type == VAL_STRING && v.type != VAL_STRING) {
        char buf[256];
        error_t err = val_to_string(&v, buf, sizeof(buf));
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_string_cstr(buf);
    }

    error_t err = array_set(s->array_assign.name, indices,
                            s->array_assign.nindices, &v);
    val_clear(&v);
    return err;
}

static error_t exec_erase(env_t *env, stmt_t *s) {
    for (int i = 0; i < s->erase_stmt.nnames; i++)
        array_erase(s->erase_stmt.names[i]);
    return ERR_NONE;
}

static error_t exec_read(env_t *env, stmt_t *s) {
    for (int i = 0; i < s->read_stmt.nvars; i++) {
        if (data_read_ptr >= data_pool_count)
            return ERR_OUT_OF_DATA;

        value_t v = val_copy(&data_pool[data_read_ptr++]);

        if (s->read_stmt.vartypes[i] == VAL_INTEGER && v.type != VAL_INTEGER) {
            int iv;
            error_t err = val_to_integer(&v, &iv);
            val_clear(&v);
            if (err != ERR_NONE) return err;
            v = val_integer(iv);
        } else if (s->read_stmt.vartypes[i] == VAL_STRING && v.type != VAL_STRING) {
            char buf[256];
            error_t err = val_to_string(&v, buf, sizeof(buf));
            val_clear(&v);
            if (err != ERR_NONE) return err;
            v = val_string_cstr(buf);
        }

        env_set(env, s->read_stmt.varnames[i], &v);
        val_clear(&v);
    }
    return ERR_NONE;
}

static error_t exec_restore(stmt_t *s) {
    if (s->restore_stmt.label[0] == '\0') {
        data_read_ptr = 0;
    } else {
        int offset = find_label_data_offset(s->restore_stmt.label);
        if (offset < 0) return ERR_UNDEFINED_LABEL;
        data_read_ptr = offset;
    }
    return ERR_NONE;
}

/* --- Stage 5: File I/O handlers --- */

static error_t exec_open(env_t *env, stmt_t *s) {
    value_t fname;
    EVAL_CHECK(eval_expr(env, s->open_stmt.filename, &fname));
    if (fname.type != VAL_STRING) { val_clear(&fname); return ERR_TYPE_MISMATCH; }
    value_t hv;
    EVAL_CHECK(eval_expr(env, s->open_stmt.handle_num, &hv));
    int handle;
    error_t err = val_to_integer(&hv, &handle);
    val_clear(&hv);
    if (err != ERR_NONE) { val_clear(&fname); return err; }
    err = fileio_open(fname.sval->data, (file_mode_t)s->open_stmt.mode, handle);
    val_clear(&fname);
    return err;
}

static error_t exec_close(env_t *env, stmt_t *s) {
    if (!s->close_stmt.handle_num)
        return fileio_close(0);
    value_t hv;
    EVAL_CHECK(eval_expr(env, s->close_stmt.handle_num, &hv));
    int handle;
    error_t err = val_to_integer(&hv, &handle);
    val_clear(&hv);
    if (err != ERR_NONE) return err;
    return fileio_close(handle);
}

static error_t exec_print_file(env_t *env, stmt_t *s) {
    value_t hv;
    EVAL_CHECK(eval_expr(env, s->print_file.handle_num, &hv));
    int handle;
    error_t err = val_to_integer(&hv, &handle);
    val_clear(&hv);
    if (err != ERR_NONE) return err;
    FILE *fp = fileio_get(handle);
    if (!fp) return ERR_FILE_NOT_OPEN;

    int needs_newline = 1;
    for (int i = 0; i < s->print_file.nitems; i++) {
        print_item_t *item = &s->print_file.items[i];
        if (item->expr) {
            value_t v;
            EVAL_CHECK(eval_expr(env, item->expr, &v));
            switch (v.type) {
                case VAL_INTEGER:
                    if (v.ival >= 0) fprintf(fp, " %d", v.ival);
                    else fprintf(fp, "%d", v.ival);
                    break;
                case VAL_DOUBLE:
                    if (v.dval >= 0) fprintf(fp, " %g", v.dval);
                    else fprintf(fp, "%g", v.dval);
                    break;
                case VAL_STRING:
                    fprintf(fp, "%s", v.sval->data);
                    break;
                case VAL_RECORD:
                    break;
            }
            val_clear(&v);
        }
        if (item->sep == ';') needs_newline = 0;
        else if (item->sep == ',') { fprintf(fp, "\t"); needs_newline = 0; }
        else needs_newline = 1;
    }
    if (needs_newline) fprintf(fp, "\n");
    return ERR_NONE;
}

static error_t exec_write_file(env_t *env, stmt_t *s) {
    value_t hv;
    EVAL_CHECK(eval_expr(env, s->print_file.handle_num, &hv));
    int handle;
    error_t err = val_to_integer(&hv, &handle);
    val_clear(&hv);
    if (err != ERR_NONE) return err;
    FILE *fp = fileio_get(handle);
    if (!fp) return ERR_FILE_NOT_OPEN;

    for (int i = 0; i < s->print_file.nitems; i++) {
        if (i > 0) fprintf(fp, ",");
        print_item_t *item = &s->print_file.items[i];
        if (item->expr) {
            value_t v;
            EVAL_CHECK(eval_expr(env, item->expr, &v));
            switch (v.type) {
                case VAL_INTEGER: fprintf(fp, "%d", v.ival); break;
                case VAL_DOUBLE: fprintf(fp, "%g", v.dval); break;
                case VAL_STRING: fprintf(fp, "\"%s\"", v.sval->data); break;
                case VAL_RECORD: break;
            }
            val_clear(&v);
        }
    }
    fprintf(fp, "\n");
    return ERR_NONE;
}

static error_t exec_input_file(env_t *env, stmt_t *s) {
    value_t hv;
    EVAL_CHECK(eval_expr(env, s->input_file.handle_num, &hv));
    int handle;
    error_t err = val_to_integer(&hv, &handle);
    val_clear(&hv);
    if (err != ERR_NONE) return err;
    FILE *fp = fileio_get(handle);
    if (!fp) return ERR_FILE_NOT_OPEN;

    for (int i = 0; i < s->input_file.nvars; i++) {
        /* Read value from file: skip whitespace, read until comma or newline */
        char buf[1024];
        int pos = 0;
        int ch;
        /* Skip leading whitespace */
        while ((ch = fgetc(fp)) != -1 && (ch == ' ' || ch == '\t'));
        if (ch == -1) return ERR_INPUT_PAST_END;

        int quoted = 0;
        if (ch == '"') { quoted = 1; }
        else if (ch != ',' && ch != '\n' && ch != '\r') {
            buf[pos++] = (char)ch;
        }

        if (quoted) {
            while ((ch = fgetc(fp)) != -1 && ch != '"' && pos < 1023)
                buf[pos++] = (char)ch;
            if (ch == '"') { /* skip closing quote */
                ch = fgetc(fp); /* eat comma or newline after quote */
            }
        } else {
            while ((ch = fgetc(fp)) != -1 && ch != ',' && ch != '\n' && ch != '\r' && pos < 1023)
                buf[pos++] = (char)ch;
        }
        buf[pos] = '\0';

        /* Assign to variable */
        val_type_t vtype = s->input_file.vartypes[i];
        value_t val;
        if (vtype == VAL_STRING) {
            val = val_string_cstr(buf);
        } else if (vtype == VAL_INTEGER) {
            val = val_integer(atoi(buf));
        } else {
            val = val_double(atof(buf));
        }
        env_set(env, s->input_file.varnames[i], &val);
        val_clear(&val);
    }
    return ERR_NONE;
}

static error_t exec_line_input(env_t *env, stmt_t *s) {
    value_t hv;
    EVAL_CHECK(eval_expr(env, s->input_file.handle_num, &hv));
    int handle;
    error_t err = val_to_integer(&hv, &handle);
    val_clear(&hv);
    if (err != ERR_NONE) return err;
    FILE *fp = fileio_get(handle);
    if (!fp) return ERR_FILE_NOT_OPEN;

    char buf[1024];
    int pos = 0;
    int ch;
    while ((ch = fgetc(fp)) != -1 && ch != '\n' && pos < 1023)
        buf[pos++] = (char)ch;
    buf[pos] = '\0';
    if (pos == 0 && ch == -1) return ERR_INPUT_PAST_END;

    if (s->input_file.nvars > 0) {
        value_t val = val_string_cstr(buf);
        env_set(env, s->input_file.varnames[0], &val);
        val_clear(&val);
    }
    return ERR_NONE;
}

static error_t exec_kill(env_t *env, stmt_t *s) {
    value_t fname;
    EVAL_CHECK(eval_expr(env, s->kill_stmt.filename, &fname));
    if (fname.type != VAL_STRING) { val_clear(&fname); return ERR_TYPE_MISMATCH; }
    error_t err = fileio_kill(fname.sval->data);
    val_clear(&fname);
    return err;
}

static error_t exec_name(env_t *env, stmt_t *s) {
    value_t old_v, new_v;
    EVAL_CHECK(eval_expr(env, s->name_stmt.oldname, &old_v));
    error_t err = eval_expr(env, s->name_stmt.newname, &new_v);
    if (err != ERR_NONE) { val_clear(&old_v); return err; }
    if (old_v.type != VAL_STRING || new_v.type != VAL_STRING) {
        val_clear(&old_v); val_clear(&new_v); return ERR_TYPE_MISMATCH;
    }
    err = fileio_rename(old_v.sval->data, new_v.sval->data);
    val_clear(&old_v); val_clear(&new_v);
    return err;
}

/* --- Stage 6 statement handlers --- */

static error_t exec_type_def(stmt_t *s) {
    if (type_def_count >= MAX_TYPE_DEFS)
        return ERR_OUT_OF_MEMORY;
    int idx = type_def_count++;
    strncpy(type_defs[idx].name, s->type_def.name, 63);
    type_defs[idx].nfields = s->type_def.nfields;
    for (int i = 0; i < s->type_def.nfields; i++) {
        strncpy(type_defs[idx].field_names[i],
                s->type_def.field_names[i], 63);
        type_defs[idx].field_types[i] = s->type_def.field_types[i];
    }
    return ERR_NONE;
}

static error_t exec_dim_as_type(env_t *env, stmt_t *s) {
    int ti = find_type_def(s->dim_as_type.type_name);
    if (ti < 0) return ERR_UNDEFINED_TYPE;
    type_def_t *td = &type_defs[ti];
    sb_record_t *rec = record_alloc(ti, td->nfields);
    if (!rec) return ERR_OUT_OF_MEMORY;
    for (int i = 0; i < td->nfields; i++) {
        val_clear(&rec->fields[i]);
        rec->fields[i] = val_default(td->field_types[i]);
    }
    value_t val;
    val.type = VAL_RECORD;
    val.rval = rec;
    env_set(env, s->dim_as_type.name, &val);
    val_clear(&val);
    return ERR_NONE;
}

static error_t exec_field_assign(env_t *env, stmt_t *s) {
    value_t *inst = env_get(env, s->field_assign.var_name);
    if (!inst || inst->type != VAL_RECORD) return ERR_UNDEFINED_VAR;
    type_def_t *td = &type_defs[inst->rval->type_idx];
    int fi = find_field_index(td, s->field_assign.field_name);
    if (fi < 0) return ERR_UNDEFINED_FIELD;
    value_t v;
    EVAL_CHECK(eval_expr(env, s->field_assign.value, &v));
    /* Coerce to field type */
    if (td->field_types[fi] == VAL_INTEGER && v.type != VAL_INTEGER) {
        int iv;
        error_t err = val_to_integer(&v, &iv);
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_integer(iv);
    } else if (td->field_types[fi] == VAL_STRING && v.type != VAL_STRING) {
        char buf[256];
        error_t err = val_to_string(&v, buf, sizeof(buf));
        val_clear(&v);
        if (err != ERR_NONE) return err;
        v = val_string_cstr(buf);
    }
    val_assign(&inst->rval->fields[fi], &v);
    val_clear(&v);
    return ERR_NONE;
}

/* --- Statement dispatch --- */

error_t eval_stmt(env_t *env, stmt_t *s) {
    switch (s->type) {
        case STMT_PRINT:        return exec_print(env, s);
        case STMT_INPUT:        return exec_input(env, s);
        case STMT_ASSIGN:       return exec_assign(env, s);
        case STMT_IF:           return exec_if(env, s);
        case STMT_FOR:          return exec_for(env, s);
        case STMT_WHILE:        return exec_while(env, s);
        case STMT_DO_LOOP:      return exec_do_loop(env, s);
        case STMT_SELECT:       return exec_select(env, s);
        case STMT_CALL:         return exec_call_stmt(env, s);
        case STMT_CONST:        return exec_const(env, s);
        case STMT_DIM:          return exec_dim(env, s);
        case STMT_ARRAY_ASSIGN: return exec_array_assign(env, s);
        case STMT_ERASE:        return exec_erase(env, s);
        case STMT_READ:         return exec_read(env, s);
        case STMT_RESTORE:      return exec_restore(s);
        case STMT_END:          return ERR_EXIT;
        case STMT_REM:          return ERR_NONE;
        case STMT_LABEL:        return ERR_NONE;
        case STMT_DECLARE:      return ERR_NONE;
        case STMT_SHARED:       return ERR_NONE;
        case STMT_DATA:         return ERR_NONE;

        case STMT_OPTION_BASE:
            option_base = s->option_base.base;
            return ERR_NONE;

        case STMT_SUB_DEF:
        case STMT_FUNC_DEF:
            return ERR_NONE;

        case STMT_EXIT:
            switch (s->exit_stmt.what) {
                case EXIT_FOR:      return ERR_EXIT_FOR;
                case EXIT_WHILE:    return ERR_EXIT_WHILE;
                case EXIT_DO:       return ERR_EXIT_DO;
                case EXIT_SUB:      return ERR_EXIT_SUB;
                case EXIT_FUNCTION: return ERR_EXIT_FUNCTION;
            }
            return ERR_INTERNAL;

        case STMT_GOTO: {
            int idx = find_label(s->goto_stmt.label);
            if (idx < 0) return ERR_UNDEFINED_LABEL;
            goto_target = idx;
            return ERR_GOTO;
        }

        case STMT_GOSUB: {
            int idx = find_label(s->goto_stmt.label);
            if (idx < 0) return ERR_UNDEFINED_LABEL;
            goto_target = idx;
            return ERR_GOSUB;
        }

        case STMT_RETURN:
            return ERR_RETURN;

        case STMT_SWAP: {
            value_t *v1, *v2;
            error_t e1 = resolve_lvalue(env, s->swap_stmt.lhs, &v1);
            if (e1 != ERR_NONE) return e1;
            error_t e2 = resolve_lvalue(env, s->swap_stmt.rhs, &v2);
            if (e2 != ERR_NONE) return e2;
            value_t tmp = *v1;
            *v1 = *v2;
            *v2 = tmp;
            return ERR_NONE;
        }

        case STMT_RANDOMIZE: {
            int seed = 42;
            if (s->randomize.seed) {
                value_t v;
                EVAL_CHECK(eval_expr(env, s->randomize.seed, &v));
                error_t cerr = val_to_integer(&v, &seed);
                val_clear(&v);
                if (cerr != ERR_NONE) return cerr;
            }
            builtin_randomize(seed);
            return ERR_NONE;
        }

        case STMT_OPEN:         return exec_open(env, s);
        case STMT_CLOSE:        return exec_close(env, s);
        case STMT_PRINT_FILE:   return exec_print_file(env, s);
        case STMT_WRITE_FILE:   return exec_write_file(env, s);
        case STMT_INPUT_FILE:   return exec_input_file(env, s);
        case STMT_LINE_INPUT:   return exec_line_input(env, s);
        case STMT_KILL:         return exec_kill(env, s);
        case STMT_NAME:         return exec_name(env, s);

        case STMT_TYPE_DEF:     return exec_type_def(s);
        case STMT_DIM_AS_TYPE:  return exec_dim_as_type(env, s);
        case STMT_FIELD_ASSIGN: return exec_field_assign(env, s);

        case STMT_ON_ERROR: {
            if (s->on_error.label[0] == '\0') {
                /* ON ERROR GOTO 0: disable error trapping */
                on_error_label_idx = -1;
            } else {
                int idx = find_label(s->on_error.label);
                if (idx < 0) return ERR_UNDEFINED_LABEL;
                on_error_label_idx = idx;
            }
            on_error_active = 0;
            return ERR_NONE;
        }

        case STMT_RESUME:
            resume_is_next = s->resume_stmt.resume_next;
            return ERR_RESUME_WITHOUT_ERROR; /* handled in eval_program loop */

        case STMT_ERROR_RAISE: {
            value_t v;
            EVAL_CHECK(eval_expr(env, s->error_raise.errnum, &v));
            int code;
            error_t cerr = val_to_integer(&v, &code);
            val_clear(&v);
            if (cerr != ERR_NONE) return cerr;
            on_error_err_code = code;
            on_error_err_line = s->line;
            return ERR_ON_ERROR_GOTO;
        }

        case STMT_DEFTYPE: {
            char from = s->deftype.from;
            char to = s->deftype.to;
            if (from >= 'A' && from <= 'Z' && to >= 'A' && to <= 'Z') {
                for (char c = from; c <= to; c++)
                    deftype_map[c - 'A'] = s->deftype.def_type;
            }
            return ERR_NONE;
        }

        case STMT_ON_GOTO: {
            value_t v;
            EVAL_CHECK(eval_expr(env, s->on_branch.index, &v));
            int idx;
            error_t cerr = val_to_integer(&v, &idx);
            val_clear(&v);
            if (cerr != ERR_NONE) return cerr;
            if (idx < 1 || idx > s->on_branch.nlabels)
                return ERR_NONE; /* out of range: just continue */
            int target = find_label(s->on_branch.labels[idx - 1]);
            if (target < 0) return ERR_UNDEFINED_LABEL;
            goto_target = target;
            return ERR_GOTO;
        }

        case STMT_ON_GOSUB: {
            value_t v;
            EVAL_CHECK(eval_expr(env, s->on_branch.index, &v));
            int idx;
            error_t cerr = val_to_integer(&v, &idx);
            val_clear(&v);
            if (cerr != ERR_NONE) return cerr;
            if (idx < 1 || idx > s->on_branch.nlabels)
                return ERR_NONE; /* out of range: just continue */
            int target = find_label(s->on_branch.labels[idx - 1]);
            if (target < 0) return ERR_UNDEFINED_LABEL;
            goto_target = target;
            return ERR_GOSUB;
        }
    }

    return ERR_INTERNAL;
}

error_t eval_stmts(env_t *env, stmt_t *stmts) {
    if (++eval_depth > MAX_EVAL_DEPTH) {
        eval_depth--;
        return ERR_STACK_OVERFLOW;
    }
    stmt_t *s = stmts;
    while (s) {
        error_t err = eval_stmt(env, s);
        if (err != ERR_NONE) { eval_depth--; return err; }
        s = s->next;
    }
    eval_depth--;
    return ERR_NONE;
}

/* --- Program execution with GOTO/GOSUB/SUB/FUNCTION support --- */

static void data_pool_clear(void) {
    for (int i = 0; i < data_pool_count; i++)
        val_clear(&data_pool[i]);
    data_pool_count = 0;
    data_read_ptr = 0;
}

error_t eval_program(env_t *env, stmt_t *program) {
    int count = 0;
    for (stmt_t *s = program; s; s = s->next)
        count++;

    if (count == 0) return ERR_NONE;

    stmt_t **stmts = malloc(count * sizeof(stmt_t *));
    if (!stmts) return ERR_OUT_OF_MEMORY;

    int i = 0;
    for (stmt_t *s = program; s; s = s->next)
        stmts[i++] = s;

    /* Initialize program state */
    program_global_env = env;
    option_base = 0;
    array_clear_all();
    data_pool_clear();
    fileio_init();
    deftype_init();
    type_def_count = 0;
    print_col = 0;
    eval_depth = 0;
    on_error_label_idx = -1;
    on_error_active = 0;
    on_error_err_code = 0;
    on_error_err_line = 0;
    resume_is_next = 0;

    /* First pass: collect labels, proc definitions, and DATA values */
    proc_count = 0;
    label_count = 0;

    for (i = 0; i < count; i++) {
        if (stmts[i]->type == STMT_LABEL) {
            if (label_count >= MAX_LABELS) return ERR_OUT_OF_MEMORY;
            strncpy(label_table[label_count].name,
                    stmts[i]->label.name, 63);
            label_table[label_count].name[63] = '\0';
            label_table[label_count].index = i;
            label_table[label_count].data_offset = data_pool_count;
            label_count++;
        }
        if (stmts[i]->type == STMT_DATA) {
            for (int j = 0; j < stmts[i]->data_stmt.nvalues; j++) {
                if (data_pool_count >= MAX_DATA_VALUES)
                    return ERR_OUT_OF_MEMORY;
                data_pool[data_pool_count++] =
                    val_copy(&stmts[i]->data_stmt.values[j]);
            }
        }
        if (stmts[i]->type == STMT_SUB_DEF ||
            stmts[i]->type == STMT_FUNC_DEF) {
            if (proc_count >= MAX_PROCS) return ERR_OUT_OF_MEMORY;
            strncpy(proc_table[proc_count].name,
                    stmts[i]->proc_def.name, 63);
            proc_table[proc_count].name[63] = '\0';
            proc_table[proc_count].def = stmts[i];
            proc_count++;
        }
    }

    /* Execute with program counter */
    int pc = 0;
    gosub_top = 0;
    error_t result = ERR_NONE;

    while (pc < count) {
        if (stmts[pc]->type == STMT_SUB_DEF ||
            stmts[pc]->type == STMT_FUNC_DEF) {
            pc++;
            continue;
        }

        error_t err = eval_stmt(env, stmts[pc]);

        if (err == ERR_NONE) {
            pc++;
            continue;
        }

        if (err == ERR_EXIT)
            break;

        if (err == ERR_GOTO) {
            pc = goto_target;
            continue;
        }

        if (err == ERR_GOSUB) {
            if (gosub_top >= MAX_GOSUB_DEPTH) {
                result = ERR_STACK_OVERFLOW;
                break;
            }
            gosub_stack[gosub_top++] = pc + 1;
            pc = goto_target;
            continue;
        }

        if (err == ERR_RETURN) {
            if (gosub_top <= 0) {
                result = ERR_RETURN_WITHOUT_GOSUB;
                break;
            }
            pc = gosub_stack[--gosub_top];
            continue;
        }

        /* RESUME / RESUME NEXT (from error handler) */
        if (err == ERR_RESUME_WITHOUT_ERROR && on_error_active) {
            on_error_active = 0;
            if (resume_is_next)
                pc = on_error_resume_next_pc;
            else
                pc = on_error_resume_pc;
            continue;
        }

        /* ON ERROR GOTO triggered by ERROR n statement */
        if (err == ERR_ON_ERROR_GOTO && on_error_label_idx >= 0) {
            on_error_active = 1;
            on_error_resume_pc = pc;
            on_error_resume_next_pc = pc + 1;
            pc = on_error_label_idx;
            continue;
        }

        /* Error trapping: if ON ERROR GOTO is active and this is a
           trappable error, jump to the handler */
        if (on_error_label_idx >= 0 && !on_error_active &&
            is_trappable_error(err)) {
            on_error_err_code = (int)err;
            on_error_err_line = stmts[pc]->line;
            on_error_active = 1;
            on_error_resume_pc = pc;
            on_error_resume_next_pc = pc + 1;
            pc = on_error_label_idx;
            continue;
        }

        result = err;
        break;
    }

    /* Clean up */
    fileio_close_all();
    data_pool_clear();
    array_clear_all();
    free(stmts);
    return result;
}
