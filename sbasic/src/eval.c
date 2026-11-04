#include "eval.h"
#include "builtin.h"
#include "array.h"
#include "fileio.h"
#include "terminal.h"
#include "graphics.h"
#include "machine.h"
#include "lower.h"
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>

/* --- Procedure and label tables --- */

#define MAX_PROCS 128
#define MAX_GOSUB_DEPTH 256

typedef struct compiled_proc compiled_proc_t;

typedef struct {
    char name[64];
    stmt_t *def;
    env_t *static_env;  /* persistent env for STATIC variables */
    code_t *code;       /* its body, lowered on the first call */
    /* what every call needs, found on the first: the SHARED and STATIC
     * names of its body, and its C if the program is compiled */
    int prepared;
    const char **shared_names, **static_names;
    int nshared, nstatic;
    const compiled_proc_t *compiled;
} proc_entry_t;

typedef struct {
    char name[64];
    int data_offset;    /* data pool offset for RESTORE */
} label_entry_t;

static proc_entry_t proc_table[MAX_PROCS];
static int proc_count = 0;

static label_entry_t *label_table = NULL;
static int label_count = 0, label_cap = 0;

static int gosub_stack[MAX_GOSUB_DEPTH];
static int gosub_top = 0;

static int goto_target = -1;

/* the unit running (GOTO's labels are its own) and the main program
 * (ON ERROR's handler is there) */
static code_t *cur_code = NULL;
static code_t *main_code = NULL;

/* Pointer to global env for SHARED variable access */
static env_t *program_global_env = NULL;

/* OPTION BASE setting */
static int option_base = 0;

/* DATA pool */
static value_t *data_pool = NULL;
static unsigned char *data_quoted = NULL;   /* each item: was it quoted */
static int data_pool_cap = 0;
static int data_pool_count = 0;
static int data_read_ptr = 0;

/* TYPE record definitions */
#define MAX_TYPE_DEFS 32

typedef struct {
    char name[64];
    char field_names[32][64];
    val_type_t field_types[32];
    int field_len[32];          /* a STRING * n field's n; else 0 */
    int nfields;
} type_def_t;

static type_def_t type_defs[MAX_TYPE_DEFS];
static int type_def_count = 0;

/* ON ERROR state */
static act_t *main_act;               /* the main program's activation, while it runs */
static int on_error_label_idx = -1;  /* -1 = no handler */
static int on_error_active = 0;      /* currently handling an error? */
static int on_error_err_code = 0;    /* ERR value */
static int on_error_err_line = 0;    /* ERL value */
int cur_basic_line = 0;              /* the running op's line number, for ERROR n */
static int on_error_resume_pc = 0;   /* PC to RESUME to */
static int on_error_resume_next_pc = 0; /* PC for RESUME NEXT */
static int resume_is_next = 0;       /* set by RESUME stmt for eval_program: 0=retry, 1=next, 2=label */
static char resume_label[64] = "";   /* target label for RESUME label */
static char return_label[64] = "";   /* target label for RETURN label */

/* Recursion depth limit (guards against C stack overflow) */
#define MAX_EVAL_DEPTH 1024   /* with the 1 MB stack build.sh gives */

/* The stack itself, beside the count: a frame's size depends on what is
 * running (a call with many arguments, compiled code), so nesting deeper
 * than STACK_LIMIT bytes below where the program started is Out of stack
 * space too, before the stack's real end (1 MB: build.sh, compile-bas.sh) */
#define STACK_LIMIT (900 * 1024)
static char *stack_base;

static int stack_exhausted(void) {
    char here;
    return stack_base && stack_base - &here > STACK_LIMIT;
}
static int eval_depth = 0;

/* TRACE mode: print line numbers as they execute */
int trace_enabled = 0;

/* PEEK/POKE simulated memory (64KB) */

/* OPTION EXPLICIT: require variables to be declared before use */
static int option_explicit = 0;

/* Is this a real error (trappable by ON ERROR) or a flow-control signal? */
static int is_trappable_error(error_t err) {
    switch (err) {
    case ERR_NONE:
    case ERR_EXIT:
    case ERR_RUN:
    case ERR_RESUME_LABEL:
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

/* an error ON ERROR GOTO's handler takes (compiled code's too) */
int eval_trappable(error_t err) { return is_trappable_error(err); }

/* DEFTYPE letter-to-type map: 26 letters A-Z, default VAL_SINGLE */
static val_type_t deftype_map[26];

static val_type_t deftype_lookup(char first_letter) {
    if (first_letter >= 'A' && first_letter <= 'Z')
        return deftype_map[first_letter - 'A'];
    if (first_letter >= 'a' && first_letter <= 'z')
        return deftype_map[first_letter - 'a'];
    return VAL_SINGLE;
}

static void deftype_init(void) {
    for (int i = 0; i < 26; i++)
        deftype_map[i] = VAL_SINGLE;
    env_deftype_hook = deftype_lookup;
}

static val_type_t resolve_var_type(const char *name);

/* letter's type for names without a suffix: compiled code (s32-bc) sets
 * the program's as it starts */
/* DEFtype is the text's (QuickBASIC's): the parser wrote each name with
 * the suffix of the DEFtype in force where it stands, so a name with none
 * is a SINGLE wherever it runs, and DEFtype does nothing as it runs */
void eval_deftype(char letter, val_type_t t) {
    (void)letter; (void)t;
}

static void exec_deftype(const stmt_t *s) {
    (void)s;
}

/* A DIM's elements: its suffix's or AS type, else DEFtype's as it runs
 * (QuickBASIC: DEFINT A-Z: DIM A(10) is an INTEGER array) */
static val_type_t dim_type(const stmt_t *s) {
    return s->dim_stmt.typed ? s->dim_stmt.elem_type : resolve_var_type(s->dim_stmt.name);
}

/* the type READ or INPUT reads for its target: an array element its
 * array's (made yet or not), a variable its own -- with DEFtype, as the
 * statement runs (DEFSTR S: READ S reads a string, as QuickBASIC does);
 * a field its TYPE's for it (found with no subscript evaluated: they are
 * when it is stored) */
static int find_field_index(type_def_t *td, const char *name);
static val_type_t target_type(env_t *env, const char *name, const expr_t *target) {
    if (target && target->type == EXPR_FIELD_ACCESS) {
        const value_t *r = NULL;
        if (target->field.elem) {
            sb_array_t *arr = array_find(target->field.var_name);
            if (arr && arr->total > 0) r = &arr->data[0];
        } else
            r = env_lookup(env, target->field.var_name);
        if (r && r->type == VAL_RECORD) {
            type_def_t *td = &type_defs[r->rval->type_idx];
            int fi = find_field_index(td, target->field.field_name);
            if (fi >= 0) return td->field_types[fi];
        }
        return VAL_STRING;              /* (the store says what is wrong) */
    }
    sb_array_t *arr = target ? array_find(name) : NULL;
    return arr ? arr->elem_type : resolve_var_type(name);
}

/* Resolve the effective type for a variable name.
   Explicit suffixes ($, %, #) take priority; bare names use deftype_map. */
static val_type_t resolve_var_type(const char *name) {
    int len = (int)strlen(name);
    if (len > 0) {
        char last = name[len - 1];
        if (last == '$') return VAL_STRING;
        if (last == '%') return VAL_INTEGER;
        if (last == '&') return VAL_LONG;
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

/* The procedures by name: an open hash of proc_table's indices (+1;
 * 0 is empty), since every call expression -- an array's element
 * among them -- asks first whether its name is a procedure's */
#define PROC_HASH (MAX_PROCS * 2)
static unsigned char proc_hash[PROC_HASH];

static unsigned proc_bucket(const char *s) {
    unsigned h = 5381;
    while (*s) h = h * 33 + (unsigned char)*s++;
    return h & (PROC_HASH - 1);
}

/* a SUB's or FUNCTION's name without its suffix (FUNCTION f% named f,
 * as QuickBASIC lets it be), the same way */
static unsigned char proc_alias_hash[PROC_HASH];
static int proc_naliases;

static proc_entry_t *find_proc(const char *name) {
    for (unsigned b = proc_bucket(name); proc_hash[b]; b = (b + 1) & (PROC_HASH - 1))
        if (strcmp(proc_table[proc_hash[b] - 1].name, name) == 0)
            return &proc_table[proc_hash[b] - 1];
    if (proc_naliases)
        for (unsigned b = proc_bucket(name); proc_alias_hash[b]; b = (b + 1) & (PROC_HASH - 1)) {
            char bare[64];
            proc_entry_t *p = &proc_table[proc_alias_hash[b] - 1];
            if (ast_name_bare(p->name, bare) && strcmp(bare, name) == 0) return p;
        }
    return NULL;
}

/* A SUB, FUNCTION or DEF FN into the table (the first of a name is the
 * one found) */
static error_t proc_register(const char *name, stmt_t *def) {
    if (proc_count >= MAX_PROCS) return ERR_OUT_OF_MEMORY;
    proc_entry_t *p = &proc_table[proc_count];
    strncpy(p->name, name, 63);
    p->name[63] = '\0';
    p->def = def;
    p->static_env = NULL;
    p->code = NULL;
    p->prepared = 0;
    p->shared_names = p->static_names = NULL;
    p->nshared = p->nstatic = 0;
    p->compiled = NULL;
    unsigned b = proc_bucket(p->name);
    while (proc_hash[b]) b = (b + 1) & (PROC_HASH - 1);
    proc_hash[b] = (unsigned char)(proc_count + 1);
    {
        char bare[64];
        if ((def->type == STMT_FUNC_DEF || def->type == STMT_SUB_DEF) &&
            ast_name_bare(p->name, bare) && !find_proc(bare)) {
            b = proc_bucket(bare);
            while (proc_alias_hash[b]) b = (b + 1) & (PROC_HASH - 1);
            proc_alias_hash[b] = (unsigned char)(proc_count + 1);
            proc_naliases++;
        }
    }
    proc_count++;
    return ERR_NONE;
}

static int find_label(const char *name) {
    return cur_code ? code_find_label(cur_code, name) : -1;
}

static error_t run_code(env_t *env, code_t *c, int is_main);
static error_t resolve_lvalue(env_t *env, expr_t *e, value_t **out);
static int data_number(const char *t, double *out);

/* A store: v made the type of the variable or element it goes into.
 * Every variable is created with its type (its suffix, DEFtype, DIM ...
 * AS), and so is every array element, so the slot's type is the rule. */
/* a fixed-length string (STRING * n): what it holds before anything is
 * stored, n CHR$(0)s, as QuickBASIC has it */
static value_t fixed_value(int n) {
    value_t v;
    v.type = VAL_STRING;
    v.sval = string_alloc(NULL, n > 0 ? n : 0);
    if (v.sval) memset(v.sval->data, 0, n > 0 ? n : 0);
    else v.sval = string_empty();
    return v;
}

/* v, a string, made what a STRING * n keeps of it: cut to n characters,
 * or padded to them with spaces */
static error_t fix_string(value_t *v, int n) {
    if (n <= 0 || v->type != VAL_STRING || v->sval->len == n) return ERR_NONE;
    sb_string_t *s = string_alloc(NULL, n);
    if (!s) return ERR_OUT_OF_MEMORY;
    int keep = v->sval->len < n ? v->sval->len : n;
    memcpy(s->data, v->sval->data, keep);
    memset(s->data + keep, ' ', n - keep);
    string_unref(v->sval);
    v->sval = s;
    return ERR_NONE;
}

/* store v into a slot of fixed length n (0: an ordinary one) */
static error_t store_into(value_t *slot, const value_t *v);
static error_t store_fixed(value_t *slot, const value_t *v, int n) {
    value_t c;
    error_t err;
    if (n <= 0 || v->type != VAL_STRING) return store_into(slot, v);
    c = val_copy(v);
    err = fix_string(&c, n);
    if (err == ERR_NONE) err = store_into(slot, &c);
    val_clear(&c);
    return err;
}

/* a record of type ti, its n fields of the types given, each 0 or empty
 * (lens, if not NULL: a STRING * n field's n) */
static sb_record_t *record_make(int ti, int n, const val_type_t *types, const int *lens) {
    sb_record_t *rec = record_alloc(ti, n);
    if (!rec) return NULL;
    for (int i = 0; i < n; i++) {
        val_clear(&rec->fields[i]);
        rec->fields[i] = lens && lens[i] ? fixed_value(lens[i]) : val_default(types[i]);
    }
    return rec;
}

/* a record stored is copied, field by field, into the record the slot
 * holds (QuickBASIC: b = a copies; b keeps its own storage, so a SUB
 * given b by reference sees the new fields) -- of the same TYPE only */
static error_t record_store(value_t *slot, const value_t *v) {
    sb_record_t *d, *r;
    if (slot->type != VAL_RECORD || v->type != VAL_RECORD)
        return ERR_TYPE_MISMATCH;
    d = slot->rval;
    r = v->rval;
    if (d == r) return ERR_NONE;
    if (d->type_idx != r->type_idx || d->nfields != r->nfields)
        return ERR_TYPE_MISMATCH;
    for (int i = 0; i < r->nfields; i++)
        val_assign(&d->fields[i], &r->fields[i]);
    return ERR_NONE;
}

/* the elements of array name not yet records made records of type ti
 * (a DIM AS a TYPE: each time it runs, so a static array, made before
 * the run, gets its records when its DIM is met, and REDIM PRESERVE's
 * new elements theirs) -- for compiled code too, whose TYPEs are its own */
error_t eval_array_records(const char *name, int ti, int n, const val_type_t *types,
                           const int *lens) {
    sb_array_t *arr = array_find(name);
    if (!arr) return ERR_INTERNAL;
    arr->elem_type = VAL_RECORD;
    for (int i = 0; i < arr->total; i++) {
        if (arr->data[i].type == VAL_RECORD) continue;
        sb_record_t *rec = record_make(ti, n, types, lens);
        if (!rec) return ERR_OUT_OF_MEMORY;
        val_clear(&arr->data[i]);
        arr->data[i].type = VAL_RECORD;
        arr->data[i].rval = rec;
    }
    return ERR_NONE;
}

static error_t field_slot(env_t *env, const char *var, const char *field,
                          expr_t *elem, value_t **out, val_type_t *ftype, int *flen);

/* array name a STRING * n's: each element not yet n characters (one new
 * to the DIM, or REDIM PRESERVE) made n CHR$(0)s -- for compiled code too */
error_t eval_array_fixed(const char *name, int n) {
    sb_array_t *arr = array_find(name);
    if (!arr) return ERR_INTERNAL;
    arr->fixlen = n;
    for (int i = 0; i < arr->total; i++) {
        if (arr->data[i].type == VAL_STRING && arr->data[i].sval->len == n) continue;
        val_clear(&arr->data[i]);
        arr->data[i] = fixed_value(n);
    }
    return ERR_NONE;
}

/* for compiled code (s32-bc): the same records and fixed-length strings */
sb_record_t *eval_record_make(int ti, int n, const val_type_t *types, const int *lens) {
    return record_make(ti, n, types, lens);
}
error_t eval_record_copy(value_t *dst, const value_t *src) { return record_store(dst, src); }
error_t eval_fix_string(value_t *v, int n) { return fix_string(v, n); }
value_t eval_fixed_value(int n) { return fixed_value(n); }
error_t eval_store_fixed(value_t *slot, const value_t *v, int n) { return store_fixed(slot, v, n); }

static error_t store_into(value_t *slot, const value_t *v) {
    value_t c;
    error_t err;
    if (!slot) return ERR_OUT_OF_MEMORY;
    if (slot->type == VAL_RECORD || v->type == VAL_RECORD)
        return record_store(slot, v);
    /* the common cases, a number into its own kind of slot */
    if (slot->type == v->type) {
        if (VAL_IS_INT(v->type) || v->type == VAL_DOUBLE) {
            if (v->type == VAL_DOUBLE && !(fabs(v->dval) <= 1.7976931348623157e308))
                return ERR_OVERFLOW;
            *slot = *v;
            return ERR_NONE;
        }
        if (v->type == VAL_SINGLE && fabs(v->dval) <= 3.4028234663852886e38) {
            slot->dval = single_round(v->dval);
            return ERR_NONE;
        }
    }
    c = val_copy(v);
    err = val_coerce(&c, slot->type);
    if (err != ERR_NONE) { val_clear(&c); return err; }
    val_clear(slot);
    *slot = c;
    return ERR_NONE;
}

/* READ's and INPUT's store: to the variable, or to the array element the
 * target expression names */
static int lvalue_fixlen(env_t *env, expr_t *e);
static error_t store_target(env_t *env, expr_t *target, const char *name, value_t *v) {
    value_t *slot;
    if (!target)
        return store_fixed(env_get(env, name), v, env_fixlen(env, name));
    EVAL_CHECK(resolve_lvalue(env, target, &slot));
    return store_fixed(slot, v, lvalue_fixlen(env, target));
}

static int find_label_data_offset(const char *name) {
    for (int i = 0; i < label_count; i++) {
        if (strcmp(label_table[i].name, name) == 0)
            return label_table[i].data_offset;
    }
    return -1;
}

/* An array used before any DIM: GW-BASIC, QuickBASIC and ECMA-55 give
 * it subscripts up to 10 in each of its dimensions.  A name that is a
 * function (built in, or FN...) is never one. */
static sb_array_t *array_implicit(const char *name, int ndims) {
    int sizes[MAX_ARRAY_DIMS];
    if (ndims < 1 || ndims > MAX_ARRAY_DIMS) return NULL;
    if (strncmp(name, "FN", 2) == 0 || builtin_exists(name) ||
        strcmp(name, "LBOUND") == 0 || strcmp(name, "UBOUND") == 0 ||
        strcmp(name, "JOIN$") == 0 ||
        strcmp(name, "ERR") == 0 || strcmp(name, "ERL") == 0)
        return NULL;
    int lb[MAX_ARRAY_DIMS];
    for (int i = 0; i < ndims; i++) {
        sizes[i] = 11 - option_base;
        lb[i] = option_base;
    }
    if (array_dim(name, resolve_var_type(name), sizes, ndims, lb) != ERR_NONE)
        return NULL;
    return array_find(name);
}

/* the value of arr(subscripts), e being the call expression */
static error_t array_elem_read(env_t *env, expr_t *e, sb_array_t *arr, value_t *out) {
    int indices[MAX_ARRAY_DIMS];
    if (e->call.nargs != arr->ndims)
        return ERR_WRONG_DIMS;
    for (int i = 0; i < e->call.nargs; i++) {
        value_t iv;
        error_t err = eval_expr(env, e->call.args[i], &iv);
        if (err != ERR_NONE) return err;
        err = val_to_integer(&iv, &indices[i]);
        val_clear(&iv);
        if (err != ERR_NONE) return err;
    }
    value_t *elem;
    EVAL_CHECK(array_get(e->call.name, indices, e->call.nargs, &elem));
    *out = val_copy(elem);
    return ERR_NONE;
}

/* DEF FN: its body evaluated with its parameters bound in a scope of
 * their own (typed by their names), the result made the type of the
 * function's name */
static error_t call_def_fn(env_t *env, stmt_t *def, expr_t **args, int nargs, value_t *out) {
    error_t err = ERR_NONE;
    if (nargs != def->def_fn.nparams)
        return ERR_ARG_COUNT;
    env_t *fn_env = env_create_sized(env, 8);
    if (!fn_env) return ERR_OUT_OF_MEMORY;
    for (int i = 0; i < nargs && err == ERR_NONE; i++) {
        value_t av, d = val_default(resolve_var_type(def->def_fn.params[i]));
        err = eval_expr(env, args[i], &av);
        if (err != ERR_NONE) break;
        env_set(fn_env, def->def_fn.params[i], &d);
        val_clear(&d);
        err = store_into(env_get(fn_env, def->def_fn.params[i]), &av);
        val_clear(&av);
    }
    if (err == ERR_NONE)
        err = eval_expr(fn_env, def->def_fn.body, out);
    env_destroy(fn_env);
    if (err == ERR_NONE) {
        /* (FNname's type is name's: DEFSTR S makes FNS a string, as in
         * QuickBASIC, and DEFINT F leaves it be) */
        err = val_coerce(out, resolve_var_type(def->def_fn.name + 2));
        if (err != ERR_NONE) val_clear(out);
    }
    return err;
}

/* name is a DEF FN its DEF has defined (s32-sbc inlines the body; the
 * answer holds until array_generation moves) */
int eval_def_fn_defined(const char *name) {
    proc_entry_t *p = find_proc(name);
    return p && p->def->type == STMT_DEF_FN;
}

/* SUBs and FUNCTIONs a compiled program has as C (s32-sbc), by name */
struct compiled_proc {
    const char *name;
    main_fn_t fn;
    int nops;
};

static compiled_proc_t *compiled_procs;
static int ncompiled_procs;

void eval_compiled_proc(const char *name, main_fn_t fn, int nops) {
    compiled_proc_t *t = realloc(compiled_procs, (ncompiled_procs + 1) * sizeof(*t));
    if (!t) return;                     /* it is then interpreted */
    compiled_procs = t;
    t[ncompiled_procs].name = name;
    t[ncompiled_procs].fn = fn;
    t[ncompiled_procs].nops = nops;
    ncompiled_procs++;
}

static const compiled_proc_t *find_compiled(const char *name) {
    for (int i = 0; i < ncompiled_procs; i++)
        if (strcmp(compiled_procs[i].name, name) == 0)
            return &compiled_procs[i];
    return NULL;
}

/* --- User-defined procedure call --- */

/* the main program's DIM SHARED names, an array's as NAME() (scan) --
 * and its CONSTs, which every procedure sees too */
static char (*dim_shared)[68];
static int ndim_shared, cap_dim_shared;

static void dim_shared_add(const char *name, int is_array) {
    if (ndim_shared == cap_dim_shared) {
        int cap = cap_dim_shared ? cap_dim_shared * 2 : 16;
        void *t = realloc(dim_shared, (size_t)cap * sizeof(*dim_shared));
        if (!t) return;
        dim_shared = t;
        cap_dim_shared = cap;
    }
    snprintf(dim_shared[ndim_shared++], sizeof dim_shared[0], "%s%s", name, is_array ? "()" : "");
}

/* a SHARED or STATIC list's entry for an array: NAME() (the parser's) */
static int is_array_entry(const char *e) {
    size_t n = strlen(e);
    return n > 2 && e[n - 2] == '(' && e[n - 1] == ')';
}

static int names_array(const char *e, const char *name) {
    size_t n = strlen(name);
    return is_array_entry(e) && strlen(e) == n + 2 && strncmp(e, name, n) == 0;
}

/* how proc's activation scopes array name (array.c): its STATIC, the main
 * program's (SHARED, DIM SHARED), else the activation's own */
static int proc_array_mode(const char *name, void *ctx) {
    proc_entry_t *p = ctx;
    for (int i = 0; i < p->nstatic; i++)
        if (names_array(p->static_names[i], name)) return ARRAY_STATIC;
    for (int i = 0; i < p->nshared; i++)
        if (names_array(p->shared_names[i], name)) return ARRAY_MODULE;
    for (int i = 0; i < ndim_shared; i++)
        if (names_array(dim_shared[i], name)) return ARRAY_MODULE;
    return ARRAY_LOCAL;
}

/* STATIC name AS type in procedure def's body: the type, else -1 */
static int static_astype(const stmt_t *def, const char *name) {
    for (const stmt_t *s = def->proc_def.body; s; s = s->next)
        if (s->type == STMT_STATIC)
            for (int i = 0; i < s->shared.nvars && i < 16; i++)
                if (s->shared.astype[i] && strcmp(s->shared.varnames[i], name) == 0)
                    return s->shared.astype[i] - 1;
    return -1;
}

/* the first call's work for every call: lower the body, collect its
 * SHARED and STATIC names, find its C */
static error_t proc_prepare(proc_entry_t *proc) {
    stmt_t *def = proc->def;
    error_t err = ERR_NONE;
    if (proc->prepared) return ERR_NONE;
    if (!proc->code)
        proc->code = lower_stmts(def->proc_def.body, &err);
    if (err != ERR_NONE) return err;
    for (int pass = 0; pass < 2; pass++) {
        stmt_type_t kind = pass ? STMT_STATIC : STMT_SHARED;
        int n = 0;
        for (stmt_t *s = def->proc_def.body; s; s = s->next)
            if (s->type == kind) n += s->shared.nvars;
        const char **names = n ? malloc(n * sizeof(char *)) : NULL;
        if (n && !names) return ERR_OUT_OF_MEMORY;
        n = 0;
        for (stmt_t *s = def->proc_def.body; s; s = s->next)
            if (s->type == kind)
                for (int i = 0; i < s->shared.nvars; i++)
                    names[n++] = s->shared.varnames[i];
        if (pass) { proc->static_names = names; proc->nstatic = n; }
        else { proc->shared_names = names; proc->nshared = n; }
    }
    proc->compiled = find_compiled(def->proc_def.name);
    if (proc->compiled && proc->compiled->nops != proc->code->nops)
        return ERR_INTERNAL;            /* not the ops its C was written from */
    proc->prepared = 1;
    return ERR_NONE;
}

/* An argument passed by reference, as QBasic passes a variable or an
 * array element: a variable's value is linked to the parameter; an
 * element is copied in and back out when the call returns (the array may
 * be REDIMmed meanwhile: copied back only where it still is) */
typedef struct {
    value_t *ref;               /* a variable's value */
    const char *arr;            /* an element's array, by name ... */
    int nidx, idx[MAX_ARRAY_DIMS];
    int fixlen;                 /* a STRING * n variable's n: cut to it after */
    int is_array;               /* a whole array, name(), for an array parameter: */
    char akey[ARRAY_KEY_MAX];   /* its key (array_ref_key) */
} arg_ref_t;

/* A parameter's type: its AS, or its name's -- under DEF*, as the
 * variable will be made (the parser knew suffixes, not DEF*) */
static val_type_t param_type(const stmt_t *def, int i) {
    const char *name = def->proc_def.params[i];
    size_t n = strlen(name);
    val_type_t t = def->proc_def.param_types[i];
    int suffixed = n && strchr("%&!#$", name[n - 1]);
    if (!suffixed && t == VAL_SINGLE)       /* no AS (or AS SINGLE) */
        return resolve_var_type(name);
    return t;
}

/* A call, its arguments evaluated (and taken: they are cleared); refs,
 * if not NULL, says which are passed by reference (a by-value one is
 * converted to its parameter's type) */
static error_t call_proc_values(proc_entry_t *proc, value_t *arg_vals, const arg_ref_t *refs,
                                int nargs, value_t *out) {
    stmt_t *def = proc->def;
    int is_function = (def->type == STMT_FUNC_DEF);
    error_t err = proc_prepare(proc);
    if (err != ERR_NONE) {
        for (int i = 0; i < nargs; i++) val_clear(&arg_vals[i]);
        return err;
    }

    env_t *local = env_create_sized(NULL, 16);
    if (!local) {
        for (int i = 0; i < nargs; i++) val_clear(&arg_vals[i]);
        return ERR_OUT_OF_MEMORY;
    }

    for (int i = 0; i < nargs; i++) {
        const char *pname = def->proc_def.params[i];
        if (refs && refs[i].is_array) continue;     /* (an array: aliased below) */
        if (refs && refs[i].ref) {
            env_link(local, pname, refs[i].ref, 0);   /* (a fixed string grows in here: cut when the call returns) */
            continue;
        }
        if (!(refs && refs[i].arr)) {
            /* by value: the parameter's type */
            err = val_coerce(&arg_vals[i], param_type(def, i));
            if (err != ERR_NONE) {
                for (int j = i; j < nargs; j++) val_clear(&arg_vals[j]);
                env_destroy(local);
                return err;
            }
        }
        /* a parameter named twice is the parser's to refuse; the first
         * is bound as env_set did */
        int dup = 0;
        for (int j = 0; j < i; j++)
            if (strcmp(def->proc_def.params[j], pname) == 0) dup = 1;
        if (dup) env_set(local, pname, &arg_vals[i]);
        else env_bind(local, pname, &arg_vals[i]);
        val_clear(&arg_vals[i]);
    }

    /* FUNCTION f%'s value is f's too (QuickBASIC: f = 1 in it sets it) */
    if (is_function) {
        char bare[64];
        if (ast_name_bare(def->proc_def.name, bare) && !env_has_local(local, bare)) {
            value_t *rv = env_get(local, def->proc_def.name);
            if (rv) env_link(local, bare, rv, 0);
        }
    }

    /* SHARED variables: the global ones, linked */
    for (int i = 0; i < proc->nshared; i++) {
        if (is_array_entry(proc->shared_names[i])) continue;    /* (A(): array.c's) */
        var_entry_t *ge = env_entry(program_global_env, proc->shared_names[i]);
        if (ge) env_link(local, proc->shared_names[i], ge->link ? ge->link : &ge->value, ge->fixlen);
    }
    /* DIM SHARED's variables: the main program's, everywhere -- but a
     * parameter of the name is the parameter */
    for (int i = 0; i < ndim_shared; i++) {
        if (is_array_entry(dim_shared[i]) || env_lookup(local, dim_shared[i])) continue;
        var_entry_t *ge = env_entry(program_global_env, dim_shared[i]);
        if (ge) env_link(local, dim_shared[i], ge->link ? ge->link : &ge->value, ge->fixlen);
    }

    /* STATIC variables: linked to the procedure's persistent env */
    if (proc->nstatic > 0) {
        if (!proc->static_env)
            proc->static_env = env_create(NULL);
        for (int i = 0; i < proc->nstatic; i++) {
            /* env_get auto-creates with default on first access */
            if (is_array_entry(proc->static_names[i])) continue;
            int fresh = !env_lookup(proc->static_env, proc->static_names[i]);
            var_entry_t *se = env_entry(proc->static_env, proc->static_names[i]);
            if (se && fresh) {          /* STATIC x AS type: made that type */
                int t = static_astype(def, proc->static_names[i]);
                if (t >= 0) { val_clear(&se->value); se->value = val_default((val_type_t)t); }
            }
            if (se) env_link(local, proc->static_names[i], se->link ? se->link : &se->value, se->fixlen);
        }
    }

    /* its arrays its own, but those it SHAREs or STATICs, or DIM SHARED */
    err = array_scope_push((int)(proc - proc_table), proc_array_mode, proc);
    if (err != ERR_NONE) { env_destroy(local); return err; }
    for (int i = 0; refs && i < nargs && err == ERR_NONE; i++)   /* array parameters: the callers' */
        if (refs[i].is_array) err = array_scope_alias(def->proc_def.params[i], refs[i].akey);
    if (err != ERR_NONE) { array_scope_pop(); env_destroy(local); return err; }
    if (proc->compiled)
        err = proc->compiled->fn(local, proc->code);
    else
        err = run_code(local, proc->code, 0);
    array_scope_pop();

    if (err == ERR_EXIT_SUB && !is_function) err = ERR_NONE;
    if (err == ERR_EXIT_FUNCTION && is_function) err = ERR_NONE;

    if (is_function && out) {
        if (err == ERR_NONE) {
            value_t *retval = env_get(local, def->proc_def.name);
            if (retval)
                *out = val_copy(retval);
            else
                *out = val_default(def->proc_def.return_type);
        }
    }

    /* elements passed by reference: their values back (and a fixed-length
     * string variable's cut or padded to its length, as QBasic does) */
    for (int i = 0; refs && i < nargs; i++) {
        if (refs[i].ref && refs[i].fixlen) {
            error_t e2 = fix_string(refs[i].ref, refs[i].fixlen);
            if (err == ERR_NONE) err = e2;
        }
        if (!refs[i].arr) continue;
        value_t *pv = env_lookup(local, def->proc_def.params[i]), *slot;
        sb_array_t *arr = array_find(refs[i].arr);
        if (pv && arr && arr->ndims == refs[i].nidx &&
            array_get(refs[i].arr, (int *)refs[i].idx, refs[i].nidx, &slot) == ERR_NONE) {
            error_t e2 = store_fixed(slot, pv, arr->fixlen);
            if (err == ERR_NONE) err = e2;
        }
    }

    env_destroy(local);
    return err;
}

/* How argument e is passed to a parameter of type pt: by reference (a
 * variable, an array element, not in parentheses -- *r filled, an
 * element's value in *v) or by value (*r empty, nothing evaluated) */
static error_t arg_by_ref(env_t *env, expr_t *e, val_type_t pt, arg_ref_t *r, value_t *v) {
    memset(r, 0, sizeof(*r));
    if (e->paren) return ERR_NONE;
    if (e->type == EXPR_VARIABLE) {
        const char *name = e->var.name;
        value_t *slot = eval_var_slot(env, name);
        if (!slot) {
            /* not a variable yet: a builtin's name, an FN or ERR/ERL is
             * a value; anything else becomes the variable */
            if (strcmp(name, "ERR") == 0 || strcmp(name, "ERL") == 0 ||
                strncmp(name, "FN", 2) == 0 || builtin_exists(name) || eval_is_function0(name))
                return ERR_NONE;
            slot = env_get(env, name);
            if (!slot) return ERR_OUT_OF_MEMORY;
        }
        if (slot->type != pt) return ERR_PARAM_MISMATCH;
        r->ref = slot;
        if (pt == VAL_STRING) r->fixlen = env_fixlen(env, name);
        return ERR_NONE;
    }
    if (e->type == EXPR_CALL) {
        sb_array_t *arr = eval_call_array(e->call.name);
        value_t *elem;
        if (!arr) return ERR_NONE;
        if (e->call.nargs != arr->ndims) return ERR_WRONG_DIMS;
        for (int i = 0; i < e->call.nargs; i++) {
            value_t iv;
            EVAL_CHECK(eval_expr(env, e->call.args[i], &iv));
            error_t err = val_to_integer(&iv, &r->idx[i]);
            val_clear(&iv);
            if (err != ERR_NONE) return err;
        }
        EVAL_CHECK(array_get(e->call.name, r->idx, e->call.nargs, &elem));
        if (elem->type != pt) return ERR_PARAM_MISMATCH;
        r->arr = e->call.name;
        r->nidx = e->call.nargs;
        *v = val_copy(elem);
        return ERR_NONE;
    }
    return ERR_NONE;
}

/* name is a FUNCTION of no parameters: named alone, it is called */
int eval_is_function0(const char *name) {
    proc_entry_t *fp = find_proc(name);
    return fp && fp->def->type == STMT_FUNC_DEF && fp->def->proc_def.nparams == 0;
}

/* an array parameter's argument: an array named with no subscripts,
 * name() (QuickBASIC: "Parameter type mismatch" else), of the
 * parameter's type; its key, for the callee's scope */
static error_t eval_array_arg(expr_t *e, val_type_t pt, arg_ref_t *r) {
    sb_array_t *arr;
    memset(r, 0, sizeof(*r));
    if (e->type != EXPR_CALL || e->call.nargs != 0 || e->paren) return ERR_PARAM_MISMATCH;
    arr = array_find(e->call.name);
    if (arr && arr->elem_type != pt) return ERR_PARAM_MISMATCH;
    r->is_array = 1;
    array_ref_key(e->call.name, r->akey, sizeof r->akey);
    return ERR_NONE;
}

static error_t call_proc(env_t *caller_env, proc_entry_t *proc,
                          expr_t **arg_exprs, int nargs, value_t *out) {
    stmt_t *def = proc->def;

    if (nargs != def->proc_def.nparams)
        return ERR_ARG_COUNT;

    /* sized to the call: a recursion's frames are these (the depth limit
     * must come before the stack's end) */
    if (nargs > 16) return ERR_ARG_COUNT;
    value_t arg_vals[nargs ? nargs : 1];
    arg_ref_t refs[nargs ? nargs : 1];
    for (int i = 0; i < nargs; i++) {
        error_t err;
        arg_vals[i] = val_integer(0);
        if (i < 16 && (def->proc_def.param_arrays >> i & 1)) {     /* name(): an array */
            err = eval_array_arg(arg_exprs[i], param_type(def, i), &refs[i]);
            if (err != ERR_NONE) {
                for (int j = 0; j <= i; j++) val_clear(&arg_vals[j]);
                return err;
            }
            continue;
        }
        err = arg_by_ref(caller_env, arg_exprs[i], param_type(def, i), &refs[i], &arg_vals[i]);
        if (err == ERR_NONE && !refs[i].ref && !refs[i].arr)
            err = eval_expr(caller_env, arg_exprs[i], &arg_vals[i]);
        if (err != ERR_NONE) {
            for (int j = 0; j <= i; j++) val_clear(&arg_vals[j]);
            return err;
        }
    }
    return call_proc_values(proc, arg_vals, refs, nargs, out);
}

/* A variable argument of compiled code: its value if it is passed by
 * reference (as arg_by_ref decides), NULL if by value */
/* after a call that was passed variable name by reference: a STRING * n
 * made n characters again (call_proc_values' own, for compiled calls) */
error_t eval_refix(env_t *env, const char *name) {
    var_entry_t *e;
    int n = env_fixlen(env, name);
    if (!n || !(e = env_entry(env, name))) return ERR_NONE;
    return fix_string(e->link ? e->link : &e->value, n);
}

value_t *eval_arg_ref(env_t *env, const char *name) {
    value_t *slot = eval_var_slot(env, name);
    if (slot) return slot;
    if (strcmp(name, "ERR") == 0 || strcmp(name, "ERL") == 0 ||
        strncmp(name, "FN", 2) == 0 || builtin_exists(name))
        return NULL;
    return env_get(env, name);
}

/* Compiled code's check of argument i passed by reference, made when
 * the interpreter makes it: as the argument is reached, before the next
 * one is evaluated */
error_t eval_arg_check(const char *name, void **cache, int i, const value_t *ref) {
    proc_entry_t *proc = *cache;
    if (!proc) {
        proc = find_proc(name);
        if (!proc || (proc->def->type != STMT_FUNC_DEF && proc->def->type != STMT_SUB_DEF))
            return proc ? ERR_INTERNAL : ERR_UNDEFINED_PROC;
        *cache = proc;
    }
    if (i < proc->def->proc_def.nparams && ref->type != param_type(proc->def, i))
        return ERR_PARAM_MISMATCH;
    return ERR_NONE;
}

/* A FUNCTION's call (or a SUB's, out NULL) from compiled code, its
 * arguments evaluated by it; *cache keeps the procedure found */
error_t eval_call_proc(const char *name, void **cache, value_t *args, value_t **vrefs,
                       int nargs, value_t *out) {
    proc_entry_t *proc = *cache;
    if (!proc) {
        proc = find_proc(name);
        if (!proc || (proc->def->type != STMT_FUNC_DEF && proc->def->type != STMT_SUB_DEF)) {
            for (int i = 0; i < nargs; i++) val_clear(&args[i]);
            return proc ? ERR_INTERNAL : ERR_UNDEFINED_PROC;
        }
        *cache = proc;
    }
    if (nargs != proc->def->proc_def.nparams) {
        for (int i = 0; i < nargs; i++) val_clear(&args[i]);
        return ERR_ARG_COUNT;
    }
    if (out && proc->def->type == STMT_SUB_DEF) {
        for (int i = 0; i < nargs; i++) val_clear(&args[i]);
        return ERR_INTERNAL;
    }
    if (nargs > 16) return ERR_ARG_COUNT;
    arg_ref_t refs[nargs ? nargs : 1];
    for (int i = 0; i < nargs; i++) {
        memset(&refs[i], 0, sizeof(refs[i]));
        if (vrefs && vrefs[i]) {
            if (vrefs[i]->type != param_type(proc->def, i)) {
                for (int j = 0; j < nargs; j++) val_clear(&args[j]);
                return ERR_PARAM_MISMATCH;
            }
            refs[i].ref = vrefs[i];
        }
    }
    if (!out && proc->def->type == STMT_FUNC_DEF) {
        /* CALL of a FUNCTION: its value dropped */
        value_t r;
        error_t err = call_proc_values(proc, args, refs, nargs, &r);
        if (err == ERR_NONE) val_clear(&r);
        return err;
    }
    return call_proc_values(proc, args, refs, nargs, out);
}

/* --- Expression evaluation --- */

static error_t eval_expr_impl(env_t *env, expr_t *e, value_t *out);

error_t eval_expr(env_t *env, expr_t *e, value_t *out) {
    /* (the stack is checked where calls begin, act_begin: expressions
     * alone nest within the count's frames) */
    if (++eval_depth > MAX_EVAL_DEPTH) {
        eval_depth--;
        return ERR_STACK_OVERFLOW;
    }
    error_t err = eval_expr_impl(env, e, out);
    eval_depth--;
    return err;
}

static inline __attribute__((always_inline)) error_t eval_variable_impl(env_t *env, const char *name, value_t *out) {
    /* ERR and ERL pseudo-variables */
    if (strcmp(name, "ERR") == 0) {
        *out = val_integer(on_error_err_code);
        return ERR_NONE;
    }
    if (strcmp(name, "ERL") == 0) {
        *out = val_integer(on_error_err_line);
        return ERR_NONE;
    }
    /* Bare niladic builtins, the GW way (k$ = INKEY$, t = TIMER):
       only when no variable of that name exists; any builtin
       error falls through to the plain-variable path */
    value_t *found = env_lookup(env, name);
    if (found) {
        *out = val_copy(found);
        return ERR_NONE;
    }
    /* a FUNCTION of no parameters, named alone, is called (QuickBASIC) */
    {
        proc_entry_t *fp = find_proc(name);
        if (fp && fp->def->type == STMT_FUNC_DEF && fp->def->proc_def.nparams == 0)
            return call_proc(env, fp, NULL, 0, out);
    }
    if (builtin_exists(name)) {
        if (builtin_call(name, NULL, 0, out) == ERR_NONE)
            return ERR_NONE;
    }
    /* DEF FNM = 123: FNM, no parentheses, calls it */
    if (strncmp(name, "FN", 2) == 0) {
        proc_entry_t *fp = find_proc(name);
        if (fp && fp->def->type == STMT_DEF_FN)
            return call_def_fn(env, fp->def, NULL, 0, out);
        if (!fp) return ERR_FUNC_NOT_DEFINED;
    }
    value_t *v = env_get(env, name);
    if (!v) return ERR_OUT_OF_MEMORY;
    *out = val_copy(v);
    return ERR_NONE;
}

error_t eval_variable(env_t *env, const char *name, value_t *out) {
    return eval_variable_impl(env, name, out);
}

/* The value eval_variable reads when it reads the variable itself (it
 * exists; not ERR or ERL), else NULL */
value_t *eval_var_slot(env_t *env, const char *name) {
    if (strcmp(name, "ERR") == 0 || strcmp(name, "ERL") == 0)
        return NULL;
    return env_lookup(env, name);
}

/* The value X = e stores into, after eval_assign_check passed: NULL
 * when it is not one to keep (a CONST), or not one to store into as it
 * is (a STRING * n: eval_assign_store cuts or pads) */
value_t *eval_assign_slot(env_t *env, const char *name) {
    if (env_is_const(env, name) || env_fixlen(env, name))
        return NULL;
    return env_get(env, name);
}

error_t eval_store(value_t *slot, const value_t *v) {
    return store_into(slot, v);
}

static inline __attribute__((always_inline)) error_t eval_unary_in(unaryop_t op, value_t *operand, value_t *out) {
    error_t err;
    if (op == OP_NEG) {
        err = val_neg(operand, out);
    } else {
        /* NOT, AND, ...: on the operands as integers, rounded half to
         * even (3.1 AND -1.5 is 3 AND -2) */
        int iv;
        err = val_to_integer(operand, &iv);
        if (err == ERR_NONE) {
            *out = val_integer(~iv);
            if (operand->type == VAL_LONG) out->type = VAL_LONG;
        }
    }
    return err;
}

/* a string variable QBasic concatenates by reference: one of the
 * variable-length kind that exists (not a fixed-length string, an array's
 * element, or anything not a variable) */
static int lazy_string_var(env_t *env, const char *name) {
    var_entry_t *v = env_entry(env, name);
    const value_t *x;
    if (!v) {                           /* (none yet: its name says what it will be) */
        size_t n = strlen(name);
        return n && name[n - 1] == '$';
    }
    if (v->fixlen) return 0;
    x = v->link ? v->link : &v->value;
    return x->type == VAL_STRING;
}

static inline __attribute__((always_inline)) error_t eval_binary_in(binop_t op, value_t *left, value_t *right, value_t *out) {
    error_t err = ERR_NONE;
    switch (op) {
        case OP_ADD:  err = val_add(left, right, out); break;
        case OP_STRCAT: {
            /* & operator: coerce both to strings and concatenate */
            char lbuf[64], rbuf[64];
            const char *ls, *rs;
            int llen, rlen;
            if (left->type == VAL_STRING) {
                ls = left->sval ? left->sval->data : "";
                llen = left->sval ? left->sval->len : 0;
            } else {
                val_format_number(left, lbuf, sizeof(lbuf));
                ls = lbuf; llen = (int)strlen(lbuf);
            }
            if (right->type == VAL_STRING) {
                rs = right->sval ? right->sval->data : "";
                rlen = right->sval ? right->sval->len : 0;
            } else {
                val_format_number(right, rbuf, sizeof(rbuf));
                rs = rbuf; rlen = (int)strlen(rbuf);
            }
            int total = llen + rlen;
            char *buf = malloc(total + 1);
            if (!buf) { err = ERR_OUT_OF_MEMORY; break; }
            memcpy(buf, ls, llen);
            memcpy(buf + llen, rs, rlen);
            buf[total] = '\0';
            *out = val_string(buf, total);
            free(buf);
            break;
        }
        case OP_SUB:  err = val_sub(left, right, out); break;
        case OP_MUL:  err = val_mul(left, right, out); break;
        case OP_DIV:  err = val_div(left, right, out); break;
        case OP_IDIV: err = val_idiv(left, right, out); break;
        case OP_MOD:  err = val_mod(left, right, out); break;
        case OP_POW:  err = val_pow(left, right, out); break;
        case OP_AND: case OP_OR: case OP_XOR: case OP_IMP: case OP_EQV: {
            int il, ir;
            err = val_to_integer(left, &il);
            if (err == ERR_NONE) err = val_to_integer(right, &ir);
            if (err == ERR_NONE) {
                if (op == OP_AND) *out = val_integer(il & ir);
                else if (op == OP_OR) *out = val_integer(il | ir);
                else if (op == OP_XOR) *out = val_integer(il ^ ir);
                else if (op == OP_IMP) *out = val_integer(~il | ir);
                else *out = val_integer(~(il ^ ir));
                /* a LONG operand (or one past 16 bits) makes a LONG */
                if (left->type == VAL_LONG || right->type == VAL_LONG ||
                    !INT16_OK(il) || !INT16_OK(ir))
                    out->type = VAL_LONG;
            }
            break;
        }
        default:      err = ERR_INTERNAL; break;
    }
    return err;
}

static inline __attribute__((always_inline)) error_t eval_compare_in(cmpop_t op, value_t *left, value_t *right, value_t *out) {
    int cmp, result;
    EVAL_CHECK(val_compare(left, right, &cmp));
    switch (op) {
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

static error_t eval_expr_impl(env_t *env, expr_t *e, value_t *out) {
    if (!e) return ERR_INTERNAL;

    switch (e->type) {
        case EXPR_LITERAL:
            *out = val_copy(&e->literal);
            return ERR_NONE;

        case EXPR_VARIABLE:
            return eval_variable_impl(env, e->var.name, out);

        case EXPR_UNARY: {
            value_t operand;
            EVAL_CHECK(eval_expr(env, e->unary.operand, &operand));
            error_t err = eval_unary_in(e->unary.op, &operand, out);
            val_clear(&operand);
            return err;
        }

        case EXPR_BINARY: {
            value_t left, right;
            error_t err;
            if (e->binary.op == OP_ADD && e->binary.left->type == EXPR_VARIABLE &&
                e->binary.right->type != EXPR_LITERAL &&
                (e->binary.right->type != EXPR_VARIABLE || find_proc(e->binary.right->var.name)) &&
                lazy_string_var(env, e->binary.left->var.name)) {
                /* s$ + e: QBasic reads s$ as it concatenates, after e --
                 * a FUNCTION in e that changes s$ is seen (s$ = s$ + f$
                 * appends to s$ as f$ leaves it); reading s$ does nothing
                 * else, so e first is the same */
                EVAL_CHECK(eval_expr(env, e->binary.right, &right));
                err = eval_expr(env, e->binary.left, &left);
                if (err != ERR_NONE) {
                    val_clear(&right);
                    return err;
                }
                err = eval_binary_in(e->binary.op, &left, &right, out);
                val_clear(&left);
                val_clear(&right);
                return err;
            }
            EVAL_CHECK(eval_expr(env, e->binary.left, &left));
            err = eval_expr(env, e->binary.right, &right);
            if (err != ERR_NONE) {
                val_clear(&left);
                return err;
            }
            err = eval_binary_in(e->binary.op, &left, &right, out);
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
            err = eval_compare_in(e->compare.op, &left, &right, out);
            val_clear(&left);
            val_clear(&right);
            return err;
        }

        case EXPR_CALL: {
            /* 1. Check for user-defined function */
            proc_entry_t *proc = find_proc(e->call.name);
            if (proc && proc->def->type == STMT_FUNC_DEF)
                return call_proc(env, proc, e->call.args, e->call.nargs, out);
            if (proc && proc->def->type == STMT_DEF_FN)
                return call_def_fn(env, proc->def, e->call.args, e->call.nargs, out);

            /* 2. Check for array element access */
            sb_array_t *arr = array_find(e->call.name);
            if (arr)
                return array_elem_read(env, e, arr, out);

            /* 3. LBOUND / UBOUND special handling */
            if (strcmp(e->call.name, "LBOUND") == 0 ||
                strcmp(e->call.name, "UBOUND") == 0) {
                if (e->call.nargs < 1) return ERR_ILLEGAL_FUNCTION_CALL;
                const char *arrname;
                if (e->call.args[0]->type == EXPR_VARIABLE)
                    arrname = e->call.args[0]->var.name;
                else if (e->call.args[0]->type == EXPR_CALL && e->call.args[0]->call.nargs == 0)
                    arrname = e->call.args[0]->call.name;  /* arr() syntax */
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
                return eval_bound(arrname, dim, e->call.name[0] == 'U', out);
            }

            /* 3b. JOIN$ special handling: JOIN$(array$(), delim$) */
            if (strcmp(e->call.name, "JOIN$") == 0) {
                if (e->call.nargs < 1 || e->call.nargs > 2)
                    return ERR_ILLEGAL_FUNCTION_CALL;
                const char *arrname;
                if (e->call.args[0]->type == EXPR_VARIABLE)
                    arrname = e->call.args[0]->var.name;
                else if (e->call.args[0]->type == EXPR_CALL && e->call.args[0]->call.nargs == 0)
                    arrname = e->call.args[0]->call.name;  /* arr$() syntax */
                else
                    return ERR_ILLEGAL_FUNCTION_CALL;
                /* Default delimiter is empty string */
                char delim[256] = "";
                if (e->call.nargs == 2) {
                    value_t dv;
                    EVAL_CHECK(eval_expr(env, e->call.args[1], &dv));
                    if (dv.type != VAL_STRING) { val_clear(&dv); return ERR_TYPE_MISMATCH; }
                    strncpy(delim, dv.sval->data, 255);
                    delim[255] = '\0';
                    val_clear(&dv);
                }
                sb_array_t *a = array_find(arrname);
                if (!a) return ERR_UNDEFINED_VAR;
                int dlen = (int)strlen(delim);
                /* Calculate total length */
                int total_len = 0;
                for (int i = 0; i < a->total; i++) {
                    if (i > 0) total_len += dlen;
                    if (a->data[i].type == VAL_STRING && a->data[i].sval)
                        total_len += a->data[i].sval->len;
                }
                if (total_len > MAX_STRING_LEN) total_len = MAX_STRING_LEN;
                char *buf = malloc(total_len + 1);
                if (!buf) return ERR_OUT_OF_MEMORY;
                int pos = 0;
                for (int i = 0; i < a->total; i++) {
                    if (i > 0 && dlen > 0 && pos + dlen <= total_len) {
                        memcpy(buf + pos, delim, dlen);
                        pos += dlen;
                    }
                    if (a->data[i].type == VAL_STRING && a->data[i].sval) {
                        int slen = a->data[i].sval->len;
                        if (pos + slen > total_len) slen = total_len - pos;
                        memcpy(buf + pos, a->data[i].sval->data, slen);
                        pos += slen;
                    }
                }
                buf[pos] = '\0';
                *out = val_string(buf, pos);
                free(buf);
                return ERR_NONE;
            }

            /* an FN name is a DEF FN's or nothing: no array, no builtin */
            if (!proc && strncmp(e->call.name, "FN", 2) == 0)
                return ERR_FUNC_NOT_DEFINED;

            /* 4. An array not DIMmed yet */
            if (!proc && (arr = array_implicit(e->call.name, e->call.nargs)) != NULL)
                return array_elem_read(env, e, arr, out);

            /* 5. Built-in functions */
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
            value_t *f;
            EVAL_CHECK(field_slot(env, e->field.var_name, e->field.field_name,
                                  e->field.elem, &f, NULL, NULL));
            *out = val_copy(f);
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
int print_row = 1;
int *active_print_col = &print_col;

/* The text screen, as QBasic keeps it, for SCREEN(row, col): each cell's
 * character and attribute, written as PRINT writes the screen.  Its rows
 * (WIDTH's), and the text viewport (VIEW PRINT's) that scrolls: rows 1
 * to 24 at the start, the last row outside it (QBasic, checked) */
#define SCR_ROWS 60
#define SCR_COLS 256
static unsigned char scr_ch[SCR_ROWS][SCR_COLS], scr_at[SCR_ROWS][SCR_COLS];
static int scr_rows = 25, view_top = 1, view_bottom = 24;

static void scr_clear_rows(int top, int bottom) {
    unsigned char a = (unsigned char)sb_term_attr();
    for (int r = top; r <= bottom && r <= SCR_ROWS; r++) {
        memset(scr_ch[r - 1], ' ', SCR_COLS);
        memset(scr_at[r - 1], a & 0x70 ? a : 7, SCR_COLS);
    }
}

static void scr_put(unsigned char c) {
    if (print_row >= 1 && print_row <= scr_rows && print_col >= 0 && print_col < SCR_COLS) {
        scr_ch[print_row - 1][print_col] = c;
        scr_at[print_row - 1][print_col] = (unsigned char)sb_term_attr();
    }
}

/* a new line: the next row, or the viewport scrolled at its bottom (and
 * from below it: the last row is outside) */
static void scr_newline(void) {
    if (print_row >= view_top && print_row < view_bottom) {
        print_row++;
        return;
    }
    for (int r = view_top; r < view_bottom && r < SCR_ROWS; r++) {
        memcpy(scr_ch[r - 1], scr_ch[r], SCR_COLS);
        memcpy(scr_at[r - 1], scr_at[r], SCR_COLS);
    }
    scr_clear_rows(view_bottom, view_bottom);
    print_row = view_bottom;
}

/* Update a column counter by scanning a string (same logic as col_puts) */
static void update_col(int *col, const char *s) {
    while (*s) {
        if (*s == '\n' || *s == '\r')
            *col = 0;
        else if (*s == '\t')
            *col = (*col + 8) & ~7;
        else
            (*col)++;
        s++;
    }
}

/* Where PRINT's output goes: the screen (stdout, print_col), the
 * printer (stderr, lprint_col) or a file (its own column).  col_puts
 * writes to col_output_fp and keeps *active_print_col, which TAB reads. */
static FILE *col_output_fp = NULL;
int lprint_col = 0;

/* The screen, as QBasic's behaves (checked): a character past the last
 * column goes to the next line first (the 80th leaves the wrap pending,
 * so a newline there makes no blank line); CR and LF are new lines; TAB
 * goes to the next stop of 8; BEL beeps, so it reaches a terminal and
 * nothing else; other control characters take no column. */
static int screen_tty = -1;

static void screen_putc(char c) {
    extern int console_width;
    if (c == '\n' || c == '\r') {
        putchar('\n');
        print_col = 0;
        scr_newline();
    } else if (c == '\a') {
        if (screen_tty < 0) screen_tty = isatty(1);
        if (screen_tty) putchar(c);
    } else if (c == '\t') {
        do screen_putc(' '); while (print_col % 8);
    } else if ((unsigned char)c < 32) {
        putchar(c);
    } else {
        if (console_width > 0 && print_col >= console_width) {
            putchar('\n');
            print_col = 0;
            scr_newline();
        }
        putchar(c);
        scr_put((unsigned char)c);
        print_col++;
    }
}

static int on_screen(void) {
    return active_print_col == &print_col && (!col_output_fp || col_output_fp == stdout);
}

/* s on the screen, as screen_putc writes it a character at a time: a
 * run of printing characters goes out in pieces that fit the line, each
 * one fwrite, not a putchar a character */
static void screen_puts(const char *s) {
    extern int console_width;
    while (*s) {
        if ((unsigned char)*s < 32) { screen_putc(*s++); continue; }
        const char *e = s;
        while ((unsigned char)*e >= 32) e++;
        while (s < e) {
            if (console_width > 0 && print_col >= console_width) {
                putchar('\n');
                print_col = 0;
                scr_newline();
            }
            int n = (int)(e - s);
            if (console_width > 0 && n > console_width - print_col) n = console_width - print_col;
            fwrite(s, 1, (size_t)n, stdout);
            for (int i = 0; i < n; i++, print_col++) scr_put((unsigned char)s[i]);
            s += n;
        }
    }
}

static void col_puts(const char *s) {
    FILE *fp = col_output_fp ? col_output_fp : stdout;
    if (on_screen()) {
        screen_puts(s);
        return;
    }
    fputs(s, fp);
    update_col(active_print_col, s);
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

/* Insert commas every 3 digits in the integer part of a formatted number.
   Works in-place on buf (must have room for expansion).
   Commas consume leading padding spaces so total width stays constant. */
static void insert_commas(char *buf, int bufsz) {
    /* Find the integer digit region: skip leading spaces/stars/signs,
       stop at '.' or end of string */
    int len = (int)strlen(buf);
    int first_digit = -1, last_digit = -1;
    int dot_pos = -1;
    for (int i = 0; i < len; i++) {
        if (buf[i] == '.') { dot_pos = i; break; }
        if (buf[i] >= '0' && buf[i] <= '9') {
            if (first_digit < 0) first_digit = i;
            last_digit = i;
        }
    }
    if (dot_pos < 0) {
        /* No dot — digits go to end (before any trailing sign) */
        for (int i = len - 1; i >= 0; i--) {
            if (buf[i] >= '0' && buf[i] <= '9') { last_digit = i; break; }
        }
        for (int i = 0; i < len; i++) {
            if (buf[i] >= '0' && buf[i] <= '9') {
                if (first_digit < 0) first_digit = i;
                break;
            }
        }
    }
    if (first_digit < 0 || last_digit < 0) return;
    int ndigits = last_digit - first_digit + 1;
    int ncommas = (ndigits - 1) / 3;
    if (ncommas <= 0) return;

    /* Count available leading padding (spaces or stars before first digit) */
    int avail_pad = 0;
    for (int i = 0; i < first_digit; i++) {
        if (buf[i] == ' ' || buf[i] == '*') avail_pad++;
    }
    /* We can only insert as many commas as we have padding to consume */
    if (ncommas > avail_pad) ncommas = avail_pad;
    if (ncommas <= 0) return;

    /* Build new string: remove ncommas leading pad chars, insert commas */
    char tmp[512];
    int out = 0;
    /* Copy leading padding minus ncommas */
    int skip = ncommas;
    for (int i = 0; i < first_digit && out < bufsz - 1; i++) {
        if (skip > 0 && (buf[i] == ' ' || buf[i] == '*')) { skip--; continue; }
        tmp[out++] = buf[i];
    }
    /* Copy digits with commas */
    int digits_from_right = last_digit - first_digit + 1;
    for (int i = first_digit; i <= last_digit && out < bufsz - 1; i++) {
        tmp[out++] = buf[i];
        int remain = last_digit - i;
        if (remain > 0 && remain % 3 == 0 && out < bufsz - 1)
            tmp[out++] = ',';
    }
    /* Copy rest (decimal part, trailing sign, etc.) */
    for (int i = last_digit + 1; i < len && out < bufsz - 1; i++)
        tmp[out++] = buf[i];
    tmp[out] = '\0';
    /* Copy back */
    for (int i = 0; i <= out; i++) buf[i] = tmp[i];
}

static int format_using_num(const char *fmt, double val) {
    const char *start = fmt;
    int has_lead_plus = 0, has_trail_plus = 0;
    int has_trail_minus = 0;
    int has_dollar = 0, has_stars = 0;
    int has_dot = 0, has_exp = 0, has_commas = 0;
    int width = 0, decimals = 0;

    /* --- Parse phase --- */
    const char *p = fmt;
    if (*p == '+') { has_lead_plus = 1; p++; width++; }
    /* **$ must be checked before ** or $$ */
    if (p[0] == '*' && p[1] == '*' && p[2] == '$') {
        has_stars = 1; has_dollar = 1; p += 3; width += 3;
    } else if (p[0] == '$' && p[1] == '$') {
        has_dollar = 1; p += 2; width += 2;
    } else if (p[0] == '*' && p[1] == '*') {
        has_stars = 1; p += 2; width += 2;
    }
    while (*p == '#' || *p == ',') {
        if (*p == ',') has_commas = 1;
        width++; p++;
    }
    if (*p == '.') {
        has_dot = 1; p++; width++;
        while (*p == '#') { decimals++; width++; p++; }
    }
    /* Trailing sign: - or + after digit section */
    if (*p == '-') { has_trail_minus = 1; p++; width++; }
    else if (*p == '+' && !has_lead_plus) { has_trail_plus = 1; p++; width++; }
    if (p[0] == '^' && p[1] == '^' && p[2] == '^' && p[3] == '^') {
        has_exp = 1; p += 4; width += 4;
    }

    if (width == 0) return 0;

    int is_neg = (val < 0.0);
    int any_sign = has_lead_plus || has_trail_plus || has_trail_minus;
    double abs_val = fmt_fabs(val);

    char buf[512];
    if (has_exp) {
        /* Scientific notation */
        double av = abs_val;
        int exp_val = 0;
        if (av != 0.0) {
            while (av >= 10.0) { av /= 10.0; exp_val++; }
            while (av < 1.0)  { av *= 10.0; exp_val--; }
        }
        int mw = width - 4; /* subtract ^^^^ */
        if (has_trail_minus || has_trail_plus) mw--;
        if (any_sign) {
            /* Format absolute value, prepend sign */
            char tmp[512];
            fmt_fixed(tmp, sizeof(tmp), av, mw - 1, decimals);
            /* Replace first leading space with sign char */
            char sc = is_neg ? '-' : '+';
            int tlen = (int)strlen(tmp);
            int i = 0;
            while (i < tlen && tmp[i] == ' ') i++;
            if (i > 0 && has_lead_plus) { tmp[i - 1] = sc; }
            else if (has_lead_plus) {
                /* No padding — prepend */
                buf[0] = sc;
                for (int j = 0; j <= tlen; j++) buf[j + 1] = tmp[j];
                col_puts(buf);
                goto exp_part;
            }
            col_puts(tmp);
        } else {
            if (is_neg) av = -av;
            fmt_fixed(buf, sizeof(buf), av, mw, decimals);
            col_puts(buf);
        }
exp_part:
        col_printf("E%c%02d", exp_val >= 0 ? '+' : '-',
               exp_val >= 0 ? exp_val : -exp_val);
        if (has_trail_minus) { col_puts(is_neg ? "-" : " "); }
        else if (has_trail_plus) { col_puts(is_neg ? "-" : "+"); }
    } else {
        /* Fixed-point format */
        int fw = width;
        if (has_trail_minus || has_trail_plus) fw--;
        /* When sign option active, format absolute value */
        double fval = any_sign ? abs_val : val;
        /* For leading +, reserve one position for the sign character */
        if (has_lead_plus) fw--;
        fmt_fixed(buf, sizeof(buf), fval, fw, decimals);
        int blen = (int)strlen(buf);

        /* Overflow check: if formatted number (trimmed) exceeds field width */
        {
            int trimlen = blen;
            int j = 0;
            while (j < blen && buf[j] == ' ') j++;
            trimlen = blen - j;
            if (trimlen > fw) {
                /* Overflow: emit % then the raw number */
                col_puts("%");
                col_puts(buf + j);
                if (has_trail_minus) col_puts(is_neg ? "-" : " ");
                else if (has_trail_plus) col_puts(is_neg ? "-" : "+");
                return (int)(p - start);
            }
        }

        /* Insert commas if format contained them */
        if (has_commas) {
            insert_commas(buf, sizeof(buf));
            blen = (int)strlen(buf);
        }

        /* Star fill */
        if (has_stars) {
            for (int i = 0; i < blen; i++) {
                if (buf[i] == ' ') buf[i] = '*';
                else break;
            }
        }
        /* Dollar sign: place $ just before first digit */
        if (has_dollar && blen > 0) {
            int i = 0;
            while (i < blen && (buf[i] == ' ' || buf[i] == '*')) i++;
            if (i > 0) buf[i - 1] = '$';
        }
        /* Leading + sign: replace leading padding with sign */
        if (has_lead_plus) {
            char sc = is_neg ? '-' : '+';
            /* Shift buf right by 1, prepend sign */
            char tmp[512];
            tmp[0] = sc;
            for (int i = 0; i <= blen && i < (int)sizeof(tmp) - 2; i++)
                tmp[i + 1] = buf[i];
            col_puts(tmp);
        } else {
            col_puts(buf);
        }
        /* Trailing sign */
        if (has_trail_minus) { col_puts(is_neg ? "-" : " "); }
        else if (has_trail_plus) { col_puts(is_neg ? "-" : "+"); }
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

/* Execute PRINT USING — separated to reduce register pressure in exec_print */
/* PRINT USING, item by item (for compiled code too): using_next says
 * which item the format wants evaluated next (-1: done, the line ended),
 * having written the format's text up to its field; using_put formats
 * the value there -- or, if the field cannot take it, writes the field's
 * character and leaves the item to the next field, where it is
 * evaluated again.  The format is used again while items remain. */
void using_begin(using_t *u, const char *fmt, int nitems, const unsigned char *has_expr) {
    snprintf(u->fmt_str, sizeof u->fmt_str, "%s", fmt);
    u->fmt = u->fmt_str;
    u->nitems = nitems;
    u->has_expr = has_expr;
    u->arg_idx = 0;
    u->args_this_pass = 0;
}

int using_next(using_t *u) {
    for (;;) {
        while (*u->fmt) {
            char c = *u->fmt;
            if (c == '#' || c == '+' || c == '$' || c == '*' || c == '!' || c == '&' || c == '\\') {
                if (u->arg_idx < u->nitems && u->has_expr[u->arg_idx]) return u->arg_idx;
                col_printf("%c", *u->fmt++);
            } else if (c == '_') {
                /* Literal next character */
                u->fmt++;
                if (*u->fmt) col_printf("%c", *u->fmt++);
            } else {
                col_printf("%c", *u->fmt++);
            }
        }
        /* If all args consumed, or no args consumed this pass, stop */
        if (u->arg_idx >= u->nitems || u->args_this_pass == 0) {
            col_puts("\n");
            return -1;
        }
        /* More args remain — print newline and restart format */
        col_puts("\n");
        u->fmt = u->fmt_str;
        u->args_this_pass = 0;
    }
}

void using_put(using_t *u, const value_t *v) {
    char c = *u->fmt;
    int consumed;
    if (c == '#' || c == '+' || c == '$' || c == '*') {
        double d = 0.0;
        val_to_double(v, &d);
        consumed = format_using_num(u->fmt, d);
    } else {
        const char *str = (v->type == VAL_STRING) ? v->sval->data : "";
        int slen = (v->type == VAL_STRING) ? v->sval->len : 0;
        consumed = format_using_str(u->fmt, str, slen);
    }
    if (consumed > 0) {
        u->fmt += consumed;
        u->arg_idx++;
        u->args_this_pass++;
    } else
        col_printf("%c", *u->fmt++);
}

static error_t exec_print_using_items(env_t *env, print_item_t *items, int nitems, const char *fmt_str) {
    using_t u;
    unsigned char has[256];
    int k;
    if (nitems > 256) return ERR_SYNTAX;
    for (int i = 0; i < nitems; i++) has[i] = items[i].expr != NULL;
    using_begin(&u, fmt_str, nitems, has);
    while ((k = using_next(&u)) >= 0) {
        value_t v;
        EVAL_CHECK(eval_expr(env, items[k].expr, &v));
        using_put(&u, &v);
        val_clear(&v);
    }
    return ERR_NONE;
}

static error_t exec_print_using(env_t *env, stmt_t *s, const char *fmt_str) {
    return exec_print_using_items(env, s->print.items, s->print.nitems, fmt_str);
}

/* Evaluate a USING expression and dispatch to exec_print_using */
static error_t exec_print_using_expr(env_t *env, stmt_t *s) {
    char fmt_buf[1024];
    value_t fv;
    EVAL_CHECK(eval_expr(env, s->print.using_expr, &fv));
    if (fv.type == VAL_STRING && fv.sval)
        snprintf(fmt_buf, sizeof(fmt_buf), "%s", fv.sval->data);
    else
        fmt_buf[0] = '\0';
    val_clear(&fv);
    return exec_print_using(env, s, fmt_buf);
}

/* PRINT's comma (GW-BASIC, QuickBASIC): on to the next 14-column zone.
 * On the screen or the printer the zone has to fit in the line, so past
 * the last one (column 56 of 80) it is a new line; a file has no width
 * (QBasic 1.1, checked). */
#define PRINT_ZONE 14
void print_comma(int width) {
    char buf[PRINT_ZONE + 1];
    int col = *active_print_col;
    int next = (col / PRINT_ZONE + 1) * PRINT_ZONE;
    if (width > 0 && next + PRINT_ZONE > width) {
        col_puts("\n");
        return;
    }
    memset(buf, ' ', next - col);
    buf[next - col] = 0;
    col_puts(buf);
}

/* PRINT's TAB(n) and SPC(n) */
static int is_tab_spc(const expr_t *e) {
    return e && e->type == EXPR_CALL &&
           (strcmp(e->call.name, "TAB") == 0 || strcmp(e->call.name, "SPC") == 0);
}

/* An item that does not fit in what is left of the line starts the next
 * one (unless the line is empty): QBasic, numbers and strings alike */
static void print_fit(int len, int width) {
    if (width > 0 && *active_print_col > 0 && *active_print_col + len > width)
        col_puts("\n");
}

/* The items of PRINT, LPRINT or PRINT #, to col_output_fp; width is the
 * line's, 0 for a file */
/* One PRINT item's value; tab_spc when it was TAB() or SPC(), which
 * place rather than fit */
error_t print_value(const value_t *v, int tab_spc, int width) {
    switch (v->type) {
        case VAL_INTEGER:
        case VAL_LONG:
        case VAL_DOUBLE:
        case VAL_SINGLE: {
            /* a space or the minus sign before, a space after */
            char item[72];
            int len;
            EVAL_CHECK(val_check_range(v));
            /* the number after the sign's place (a space, unless the
             * '-' is there), then a space */
            val_format_number(v, item + 1, (int)sizeof(item) - 2);
            if (item[1] == '-') {
                len = (int)strlen(item + 1);
                memmove(item, item + 1, (size_t)len);
            } else {
                item[0] = ' ';
                len = (int)strlen(item);
            }
            item[len] = ' ';
            item[len + 1] = '\0';
            print_fit(len + 1, width);
            col_puts(item);
            break;
        }
        case VAL_STRING:
            if (!tab_spc)
                print_fit(v->sval->len, width);
            col_puts(v->sval->data);
            break;
        case VAL_RECORD:
            break;
    }
    return ERR_NONE;
}

/* A PRINT item TAB(n) or SPC(n), its argument converted: the spaces
 * written as print_value writes the string fn_tab or fn_spc makes */
void print_tab_spc(int is_spc, int n) {
    char buf[MAX_STRING_LEN + 2];
    int nl = 0, spaces = is_spc ? sb_spc_spaces(n) : sb_tab_spaces(n, &nl);
    if (nl) buf[0] = '\n';
    memset(buf + nl, ' ', spaces);
    buf[spaces + nl] = '\0';
    col_puts(buf);
}

void print_text(const char *s) {
    col_puts(s);
}

static error_t print_items(env_t *env, print_item_t *items, int nitems, int width) {
    int needs_newline = 1;
    for (int i = 0; i < nitems; i++) {
        print_item_t *item = &items[i];
        if (item->expr) {
            value_t v;
            EVAL_CHECK(eval_expr(env, item->expr, &v));
            error_t perr = print_value(&v, is_tab_spc(item->expr), width);
            val_clear(&v);
            if (perr != ERR_NONE) return perr;
        }
        if (item->sep == ';') {
            needs_newline = 0;
        } else if (item->sep == ',') {
            print_comma(width);
            needs_newline = 0;
        } else {
            needs_newline = 1;
        }
    }
    if (needs_newline)
        col_puts("\n");
    return ERR_NONE;
}

static error_t exec_print(env_t *env, stmt_t *s) {
    /* PRINT USING */
    if (s->print.using_fmt)
        return exec_print_using(env, s, s->print.using_fmt);
    if (s->print.using_expr)
        return exec_print_using_expr(env, s);

    extern int console_width;
    return print_items(env, s->print.items, s->print.nitems, console_width);
}

/* LPRINT — like PRINT but output to stderr */
/* LPRINT's output: the printer -- stderr, its own column (80 wide) --
 * until eval_print_file_end puts back what was; for compiled code too */
void eval_lprint_begin(print_redirect_t *saved) {
    saved->col = active_print_col;
    saved->fp = col_output_fp;
    col_output_fp = stderr;
    active_print_col = &lprint_col;
}

static error_t exec_lprint(env_t *env, stmt_t *s) {
    /* the printer: stderr, its own column, 80 wide */
    print_redirect_t saved;
    FILE *saved_fp;
    int *saved_col;
    error_t err;
    eval_lprint_begin(&saved);
    saved_fp = saved.fp;
    saved_col = saved.col;
    if (s->print.using_fmt || s->print.using_expr) {
        const char *fmt;
        char fmt_buf[1024];
        err = ERR_NONE;
        if (s->print.using_fmt) {
            fmt = s->print.using_fmt;
        } else {
            value_t fv;
            err = eval_expr(env, s->print.using_expr, &fv);
            if (fv.type == VAL_STRING && fv.sval && err == ERR_NONE)
                snprintf(fmt_buf, sizeof(fmt_buf), "%s", fv.sval->data);
            else
                fmt_buf[0] = '\0';
            if (err == ERR_NONE) val_clear(&fv);
            fmt = fmt_buf;
        }
        if (err == ERR_NONE)
            err = exec_print_using_items(env, s->print.items, s->print.nitems, fmt);
    } else {
        err = print_items(env, s->print.items, s->print.nitems, 80);
    }
    col_output_fp = saved_fp;
    active_print_col = saved_col;
    return err;
}

/* A line from the keyboard.  When stdin is not a terminal the line is
 * echoed, so that a run's output reads as QBasic's screen does with the
 * same keys typed.  -1 at the end of input with nothing read. */
static int read_input_line(char *line, int size) {
    int pos = 0, ch;
    fflush(stdout);
    while ((ch = getchar()) != EOF && ch != '\n') {
        if (ch == '\r') continue;
        if (pos < size - 1)
            line[pos++] = (char)ch;
    }
    line[pos] = '\0';
    if (ch == EOF && pos == 0)
        return -1;
    if (!isatty(0))
        col_puts(line);
    return pos;
}

/* One field of an INPUT line for a variable of type t: a quoted string or
 * text to the next comma for a string, a number (or nothing, 0) for a
 * number.  0 when the field does not fit -- "Redo from start". */
static int input_field(const char **pp, val_type_t t, value_t *out) {
    const char *p = *pp, *start;
    int len;
    while (*p == ' ') p++;
    if (t == VAL_STRING && *p == '"') {
        start = ++p;
        while (*p && *p != '"') p++;
        len = (int)(p - start);
        if (*p == '"') p++;
        while (*p == ' ') p++;
        if (*p && *p != ',') return 0;
        *out = val_string(start, len);
    } else {
        start = p;
        while (*p && *p != ',') p++;
        len = (int)(p - start);
        while (len > 0 && start[len - 1] == ' ') len--;
        if (t == VAL_STRING) {
            *out = val_string(start, len);
        } else {
            char buf[256];
            double d;
            if (len >= (int)sizeof(buf)) return 0;
            memcpy(buf, start, len);
            buf[len] = '\0';
            if (!data_number(buf, &d)) return 0;
            *out = val_double(d);
        }
    }
    *pp = p;
    return 1;
}

/* INPUT [;] ["prompt" {;|,}] v, ...: QuickBASIC's.  The whole line is
 * taken before anything is stored; a field that is not of its variable's
 * type, or too few or too many of them, is "Redo from start" and the
 * prompt again. */
/* INPUT's line: the prompt (NULL: none), "? " unless no_question, a
 * line read, its n fields parsed as types[] says (VAL_STRING, or a
 * number), and "Redo from start" until they fit -- the values into
 * vals[], the caller's to store.  The interpreter's INPUT, and compiled
 * code's (s32-bc). */
error_t eval_input_line(const char *prompt, int no_question, int same_line,
                        int n, const val_type_t *types, value_t *vals) {
    char line[1024];
    while (1) {
        const char *p;
        int i, ok = 1;
        if (prompt) col_puts(prompt);
        if (!prompt || !no_question) col_puts("? ");
        if (read_input_line(line, sizeof(line)) < 0) {
            col_puts("\n");
            return ERR_INPUT_PAST_END;
        }
        /* the Enter: on a terminal it moved the cursor itself */
        if (isatty(0))
            print_col = 0;
        else if (!same_line)
            col_puts("\n");
        p = line;
        for (i = 0; i < n && ok; i++) {
            if (i > 0) {
                if (*p != ',') { ok = 0; break; }
                p++;
            }
            if (!input_field(&p, types[i], &vals[i])) { ok = 0; break; }
        }
        while (ok && *p == ' ') p++;
        if (ok && *p) ok = 0;            /* more fields than variables */
        if (!ok) {
            for (int j = 0; j < i; j++) val_clear(&vals[j]);
            col_puts("\nRedo from start\n");
            continue;
        }
        return ERR_NONE;
    }
}

static error_t exec_input(env_t *env, stmt_t *s) {
    value_t vals[64];
    val_type_t types[64];
    int n = s->input.nvars;
    if (n > 64) return ERR_SYNTAX;
    for (int i = 0; i < n; i++)
        if (env_is_const(env, s->input.varnames[i]))
            return ERR_CONST_REASSIGN;
    for (int i = 0; i < n; i++) {
        types[i] = target_type(env, s->input.varnames[i], s->input.targets[i]);
        if (s->input.targets[i] == NULL) {
            value_t *cur = env_get(env, s->input.varnames[i]);
            if (cur) types[i] = cur->type == VAL_STRING ? VAL_STRING : VAL_DOUBLE;
        }
    }
    EVAL_CHECK(eval_input_line(s->input.prompt, s->input.no_question, s->input.same_line,
                               n, types, vals));
    for (int i = 0; i < n; i++) {
        error_t err = store_target(env, s->input.targets[i], s->input.varnames[i], &vals[i]);
        val_clear(&vals[i]);
        if (err != ERR_NONE) {
            for (int j = i + 1; j < n; j++) val_clear(&vals[j]);
            return err;
        }
    }
    return ERR_NONE;
}

static error_t gfx_eval_int(env_t *env, expr_t *e, int *out);

static error_t exec_cls(env_t *env, stmt_t *s) {
    int m = -1;
    if (s->cls_stmt.mode) EVAL_CHECK(gfx_eval_int(env, s->cls_stmt.mode, &m));
    return eval_cls_mode(m);
}

/* CLS [mode] (-1: none): 0 and 1 (and none) a graphics screen's
 * viewport, 2 the text's -- QBasic, under VIEW -- for compiled code too */
error_t eval_cls_mode(int m) {
    if (m < -1 || m > 2) return ERR_ILLEGAL_FUNCTION_CALL;
    if (sb_gfx_active()) {
        if (m != 2) sb_gfx_cls();
        print_col = 0;
        print_row = view_top;
        return ERR_NONE;
    }
    if (m == 2) {                       /* the text viewport only */
        scr_clear_rows(view_top, view_bottom);
        print_col = 0;
        print_row = view_top;
        return ERR_NONE;
    }
    eval_cls();
    return ERR_NONE;
}

/* CLS -- for compiled code too */
void eval_cls(void) {
    if (sb_gfx_active()) {
        sb_gfx_cls();
        return;
    }
    sb_term_cls();
    scr_clear_rows(1, scr_rows);
    print_col = 0;
    print_row = 1;
}

/* --- Graphics statements --- */

static error_t gfx_eval_int(env_t *env, expr_t *e, int *out) {
    value_t v;
    EVAL_CHECK(eval_expr(env, e, &v));
    error_t err = val_to_integer(&v, out);
    val_clear(&v);
    return err;
}

/* Evaluate an optional color expression; -1 = "use foreground". */
static error_t gfx_eval_color(env_t *env, expr_t *e, int *out) {
    if (!e) { *out = -1; return ERR_NONE; }
    return gfx_eval_int(env, e, out);
}

/* a coordinate's value as it is (eval_gfx rounds it, or maps it) */
static error_t gfx_eval_num(env_t *env, expr_t *e, double *out) {
    value_t v;
    EVAL_CHECK(eval_expr(env, e, &v));
    error_t err = val_to_double(&v, out);
    val_clear(&v);
    return err;
}

static error_t exec_gfx(env_t *env, stmt_t *s) {
    double x1 = 0, y1 = 0, x2 = 0, y2 = 0;
    int color, aux;

    /* Evaluate operands even when degraded, so side effects and type
     * errors behave the same with or without a tube. */
    if (s->gfx.x1) EVAL_CHECK(gfx_eval_num(env, s->gfx.x1, &x1));
    if (s->gfx.y1) EVAL_CHECK(gfx_eval_num(env, s->gfx.y1, &y1));
    if (s->gfx.x2) EVAL_CHECK(gfx_eval_num(env, s->gfx.x2, &x2));
    if (s->gfx.y2) EVAL_CHECK(gfx_eval_num(env, s->gfx.y2, &y2));
    EVAL_CHECK(gfx_eval_color(env, s->gfx.color, &color));
    EVAL_CHECK(gfx_eval_color(env, s->gfx.aux, &aux));
    gfx_op_t g = { s->type, x1, y1, x2, y2, color, aux, s->gfx.x1 != NULL, s->gfx.color != NULL,
                   s->gfx.aux != NULL, s->gfx.step1, s->gfx.step2, s->gfx.has_from, s->gfx.box,
                   s->gfx.preset, 0, 0, 0, 0, 0, 0 };
    if (s->gfx.start) { EVAL_CHECK(gfx_eval_num(env, s->gfx.start, &g.start)); g.has_start = 1; }
    if (s->gfx.end) { EVAL_CHECK(gfx_eval_num(env, s->gfx.end, &g.end)); g.has_end = 1; }
    if (s->gfx.aspect) { EVAL_CHECK(gfx_eval_num(env, s->gfx.aspect, &g.aspect)); g.has_aspect = 1; }
    return eval_gfx(&g);
}

/* an element's bytes, as QuickBASIC keeps a number: an INTEGER 2, a
 * LONG and a SINGLE 4, a DOUBLE 8, little-endian -- a SINGLE's bits
 * moved by hand, so a NaN's (a sprite's bytes may make one) are not
 * quieted on the way */
static int elem_bytes(val_type_t t) {
    return t == VAL_INTEGER ? 2 : t == VAL_LONG || t == VAL_SINGLE ? 4 : t == VAL_DOUBLE ? 8 : 0;
}

static uint32_t single_bits(double d) {
    uint64_t b;
    uint32_t sign;
    memcpy(&b, &d, 8);
    sign = (uint32_t)(b >> 32) & 0x80000000u;
    if (((b >> 52) & 0x7FF) == 0x7FF && (b & 0xFFFFFFFFFFFFFull))   /* a NaN: its payload */
        return sign | 0x7F800000u | (uint32_t)((b >> 29) & 0x7FFFFF);
    float f = (float)d;
    uint32_t r;
    memcpy(&r, &f, 4);
    return r;
}

static double single_of_bits(uint32_t r) {
    if ((r & 0x7F800000u) == 0x7F800000u && (r & 0x7FFFFF)) {
        uint64_t b = ((uint64_t)(r & 0x80000000u) << 32) | 0x7FF0000000000000ull |
                     ((uint64_t)(r & 0x7FFFFF) << 29);
        double d;
        memcpy(&d, &b, 8);
        return d;
    }
    float f;
    memcpy(&f, &r, 4);
    return f;
}

static void elem_to_bytes(const value_t *v, val_type_t t, unsigned char *b) {
    uint32_t w = 0;
    if (t == VAL_DOUBLE) {
        double d = v->type == VAL_DOUBLE || v->type == VAL_SINGLE ? v->dval : v->ival;
        memcpy(b, &d, 8);               /* (SLOW-32 and its hosts: little-endian) */
        return;
    }
    if (t == VAL_SINGLE) w = single_bits(v->type == VAL_SINGLE || v->type == VAL_DOUBLE ? v->dval : v->ival);
    else w = (uint32_t)v->ival;
    for (int i = 0; i < elem_bytes(t); i++) b[i] = (unsigned char)(w >> (8 * i));
}

static void elem_from_bytes(value_t *v, val_type_t t, const unsigned char *b) {
    uint32_t w = 0;
    if (t == VAL_DOUBLE) {
        double d;
        memcpy(&d, b, 8);
        *v = val_double(d);
        return;
    }
    for (int i = 0; i < elem_bytes(t); i++) w |= (uint32_t)b[i] << (8 * i);
    if (t == VAL_INTEGER) *v = val_integer((short)w);
    else if (t == VAL_LONG) *v = val_long((int)w);
    else { v->type = VAL_SINGLE; v->dval = single_of_bits(w); }
}

static error_t gfx_coord(double v, double *out);
static error_t gfx_point(double x, double y, int step, double *lx, double *ly, int *ax, int *ay);

error_t eval_gfx_getput(const gfx_op_t *g, const char *name, int nidx, const int *idx) {
    int x1, y1, x2, y2, start = 0, avail, sz, n;
    double lx, ly, mx = 0, my = 0;
    sb_array_t *arr;
    unsigned char *buf;
    error_t err = ERR_NONE;
    if (!sb_gfx_active())
        return sb_gfx_degraded() ? ERR_NONE : ERR_ILLEGAL_FUNCTION_CALL;
    if (nidx) {
        value_t *e;
        EVAL_CHECK(eval_array_for(name, nidx, &arr));
        EVAL_CHECK(array_get(name, (int *)idx, nidx, &e));
        start = (int)(e - arr->data);
    } else if (!(arr = array_find(name)))
        return ERR_ILLEGAL_FUNCTION_CALL;
    if (!(sz = elem_bytes(arr->elem_type))) return ERR_TYPE_MISMATCH;
    avail = (arr->total - start) * sz;
    EVAL_CHECK(gfx_point(g->x1, g->y1, g->step1, &lx, &ly, &x1, &y1));
    if (g->type == STMT_GFX_GET) {
        EVAL_CHECK(gfx_coord(g->x2, &mx));
        EVAL_CHECK(gfx_coord(g->y2, &my));
        if (g->step2) { mx += lx; my += ly; }  /* (relative to the first, as LINE's) */
        if (sb_gfx_map(mx, my, &x2, &y2) < 0) return ERR_OVERFLOW;
        int left = x1 < x2 ? x1 : x2, right = x1 < x2 ? x2 : x1;
        int top = y1 < y2 ? y1 : y2, bottom = y1 < y2 ? y2 : y1;
        n = sb_gfx_image_size(right - left + 1, bottom - top + 1);
        if (n > avail) return ERR_ILLEGAL_FUNCTION_CALL;
        int ne = (n + sz - 1) / sz;
        if (!(buf = calloc((size_t)ne, (size_t)sz))) return ERR_OUT_OF_MEMORY;
        /* (the last element's bytes past the image kept as they are) */
        elem_to_bytes(&arr->data[start + ne - 1], arr->elem_type, buf + (ne - 1) * sz);
        sb_gfx_get(left, top, right, bottom, buf);
        for (int i = 0; i < ne; i++) {
            value_t nv;
            elem_from_bytes(&nv, arr->elem_type, buf + i * sz);
            val_clear(&arr->data[start + i]);
            arr->data[start + i] = nv;
        }
        free(buf);
        sb_gfx_set_last(mx, my);
        return ERR_NONE;
    }
    if (!(buf = malloc((size_t)avail + 1))) return ERR_OUT_OF_MEMORY;
    for (int i = 0; start + i < arr->total; i++)
        elem_to_bytes(&arr->data[start + i], arr->elem_type, buf + i * sz);
    if (sb_gfx_put(x1, y1, buf, avail, g->box) < 0) err = ERR_ILLEGAL_FUNCTION_CALL;
    else sb_gfx_set_last(lx, ly);
    free(buf);
    return err;
}

static error_t exec_gfx_getput(env_t *env, stmt_t *s) {
    double x1 = 0, y1 = 0, x2 = 0, y2 = 0;
    int idx[MAX_ARRAY_DIMS];
    expr_t *t = s->gfx.target;
    EVAL_CHECK(gfx_eval_num(env, s->gfx.x1, &x1));
    EVAL_CHECK(gfx_eval_num(env, s->gfx.y1, &y1));
    if (s->gfx.x2) EVAL_CHECK(gfx_eval_num(env, s->gfx.x2, &x2));
    if (s->gfx.y2) EVAL_CHECK(gfx_eval_num(env, s->gfx.y2, &y2));
    for (int i = 0; i < t->call.nargs; i++) EVAL_CHECK(gfx_eval_int(env, t->call.args[i], &idx[i]));
    gfx_op_t g = { s->type, x1, y1, x2, y2, -1, -1, 1, 0, 0, s->gfx.step1, s->gfx.step2, 1,
                   s->gfx.box, 0 };
    return eval_gfx_getput(&g, t->call.name, t->call.nargs, idx);
}

/* DRAW s$ -- for compiled code too: a string, else Type mismatch; a
 * command it has not, Illegal function call; no screen, as the other
 * graphics statements */
error_t eval_draw(const value_t *v) {
    if (v->type != VAL_STRING) return ERR_TYPE_MISMATCH;
    if (!sb_gfx_active())
        return sb_gfx_degraded() ? ERR_NONE : ERR_ILLEGAL_FUNCTION_CALL;
    return sb_gfx_draw(v->sval->data, v->sval->len) < 0 ? ERR_ILLEGAL_FUNCTION_CALL : ERR_NONE;
}

/* SCREEN mode -- for compiled code too */
/* a mode's text, as QBasic sets it on a VGA (probed): 40 columns in the
 * 320-pixel modes, 80 in the rest; 30 rows at 480 lines, 25 elsewhere.
 * SCREEN 0 from a graphics mode takes that mode's columns and 25 rows,
 * from text keeps the text's.  Every SCREEN clears it, cursor home. */
static int mode_cols(int mode) { return mode == 1 || mode == 7 || mode == 13 ? 40 : 80; }

static void screen_text(int mode, int prev) {
    extern int console_width;
    if (mode || prev) {
        console_width = mode_cols(mode ? mode : prev);
        scr_rows = mode == 11 || mode == 12 ? 30 : 25;
    }
    view_top = 1;
    view_bottom = scr_rows - 1;
    if (!mode) sb_term_cls();
    scr_clear_rows(1, scr_rows);
    print_col = 0;
    print_row = 1;
}

error_t eval_screen(int mode) {
    int prev = sb_gfx_mode(), r = sb_gfx_screen(mode);
    if (r == -2)
        return ERR_ILLEGAL_FUNCTION_CALL;
    if (r == 0) screen_text(mode, prev);
    return ERR_NONE;    /* denied tube already printed its message */
}

/* PSET, PRESET, LINE, CIRCLE, PAINT, PALETTE, their operands evaluated
 * (an absent color -1) -- for compiled code too */
/* a coordinate as a statement takes it: under a WINDOW the world's, as
 * it is; else a pixel's, rounded as CINT rounds (each one, before STEP
 * adds it) */
static error_t gfx_coord(double v, double *out) {
    int i;
    value_t t;
    if (sb_gfx_windowed()) { *out = v; return ERR_NONE; }
    t = val_double(v);
    EVAL_CHECK(val_to_integer(&t, &i));
    *out = i;
    return ERR_NONE;
}

/* [STEP](x, y): the world's point (STEP's from the last), and its pixel */
static error_t gfx_point(double x, double y, int step, double *lx, double *ly, int *ax, int *ay) {
    EVAL_CHECK(gfx_coord(x, lx));
    EVAL_CHECK(gfx_coord(y, ly));
    if (step) {
        double px, py;
        sb_gfx_last(&px, &py);
        *lx += px;
        *ly += py;
    }
    return sb_gfx_map(*lx, *ly, ax, ay) < 0 ? ERR_OVERFLOW : ERR_NONE;
}

error_t eval_gfx(const gfx_op_t *g) {
    int color = g->color, aux = g->aux, ax, ay, bx, by;
    double lx, ly, mx, my;
    if (!sb_gfx_active()) {
        if (g->type == STMT_PALETTE) {  /* (text mode: checked as QBasic does, kept no further) */
            int a = 0;
            if (g->has_x1) {
                value_t t = val_double(g->x1);
                EVAL_CHECK(val_to_integer(&t, &a));
            }
            return a < 0 || a > 15 || color < -1 || color > 63 ? ERR_ILLEGAL_FUNCTION_CALL : ERR_NONE;
        }
        if (sb_gfx_degraded())
            return ERR_NONE;            /* no tube: statements no-op */
        return ERR_ILLEGAL_FUNCTION_CALL;  /* no SCREEN yet */
    }

    switch (g->type) {
        case STMT_VIEW: {
            int v[4] = { 0, 0, 0, 0 };
            double c[4] = { g->x1, g->y1, g->x2, g->y2 };
            if (g->has_from)
                for (int i = 0; i < 4; i++) {
                    value_t t = val_double(c[i]);
                    EVAL_CHECK(val_to_integer(&t, &v[i]));
                }
            if (sb_gfx_view(g->has_from, g->box, v[0], v[1], v[2], v[3],
                            g->has_color ? color : -1, g->has_aux ? aux : -1) < 0)
                return ERR_ILLEGAL_FUNCTION_CALL;
            break;
        }
        case STMT_WINDOW:
            if (sb_gfx_window(g->has_from, g->box, g->x1, g->y1, g->x2, g->y2) < 0)
                return ERR_ILLEGAL_FUNCTION_CALL;
            break;
        case STMT_PSET:
            EVAL_CHECK(gfx_point(g->x1, g->y1, g->step1, &lx, &ly, &ax, &ay));
            /* PRESET without a color erases (background); with one it
             * draws that color, same as PSET */
            sb_gfx_pset(ax, ay, (g->preset && !g->has_color) ? 0 : color);
            sb_gfx_set_last(lx, ly);
            break;
        case STMT_LINE_G:
            if (g->has_from)
                EVAL_CHECK(gfx_point(g->x1, g->y1, g->step1, &lx, &ly, &ax, &ay));
            else {
                sb_gfx_last(&lx, &ly);
                if (sb_gfx_map(lx, ly, &ax, &ay) < 0) return ERR_OVERFLOW;
            }
            /* STEP on the second point is relative to the first */
            EVAL_CHECK(gfx_coord(g->x2, &mx));
            EVAL_CHECK(gfx_coord(g->y2, &my));
            if (g->step2) { mx += lx; my += ly; }
            if (sb_gfx_map(mx, my, &bx, &by) < 0) return ERR_OVERFLOW;
            sb_gfx_line(ax, ay, bx, by, color, g->box);
            sb_gfx_set_last(mx, my);
            break;
        case STMT_CIRCLE: {
            int r, arc = g->has_start || g->has_end;
            double s = g->has_start ? g->start : 0, e = g->has_end ? g->end : 0;
            value_t t;
            EVAL_CHECK(gfx_point(g->x1, g->y1, g->step1, &lx, &ly, &ax, &ay));
            t = val_double(g->x2 * sb_gfx_xscale());   /* (the world's radius: across) */
            EVAL_CHECK(val_to_integer(&t, &r));
            /* the arc's angles -2pi..2pi (a negative one's end joined to the
             * center); no end: all the way round */
            if (s < -2 * 3.1415926535897932 - 1e-6 || s > 2 * 3.1415926535897932 + 1e-6 ||
                e < -2 * 3.1415926535897932 - 1e-6 || e > 2 * 3.1415926535897932 + 1e-6)
                return ERR_ILLEGAL_FUNCTION_CALL;
            if (arc && !g->has_end) e = 2 * 3.1415926535897932;
            sb_gfx_circle(ax, ay, r, color, g->has_aspect ? g->aspect : sb_gfx_aspect(),
                          arc, fabs(s), fabs(e), s < 0, e < 0);
            sb_gfx_set_last(lx, ly);
            break;
        }
        case STMT_PAINT:
            EVAL_CHECK(gfx_point(g->x1, g->y1, g->step1, &lx, &ly, &ax, &ay));
            sb_gfx_paint(ax, ay, color, g->has_aux ? aux : -1);
            sb_gfx_set_last(lx, ly);
            break;
        case STMT_PALETTE:
            if (g->has_x1) {
                int attr;
                value_t t = val_double(g->x1);
                EVAL_CHECK(val_to_integer(&t, &attr));
                if (attr < 0 || sb_gfx_palette(attr, color) < 0) return ERR_ILLEGAL_FUNCTION_CALL;
            } else
                sb_gfx_palette(-1, 0);
            break;
        default:
            return ERR_INTERNAL;
    }
    return ERR_NONE;
}

static error_t eval_int_of(env_t *env, expr_t *e, int *out);

static error_t exec_locate(env_t *env, stmt_t *s) {
    value_t rv = val_integer(eval_cursor_row()), cv = val_integer(eval_cursor_col());
    if (s->locate_stmt.row) EVAL_CHECK(eval_expr(env, s->locate_stmt.row, &rv));
    if (s->locate_stmt.col) {
        error_t err = eval_expr(env, s->locate_stmt.col, &cv);
        if (err != ERR_NONE) {
            val_clear(&rv);
            return err;
        }
    }
    int row, col, rest;
    error_t err = val_to_integer(&rv, &row);
    if (err == ERR_NONE) err = val_to_integer(&cv, &col);
    val_clear(&rv);
    val_clear(&cv);
    if (err != ERR_NONE) return err;
    for (int i = 0; i < 3; i++)         /* (the cursor's shape: evaluated, no more) */
        if (s->locate_stmt.rest[i]) EVAL_CHECK(eval_int_of(env, s->locate_stmt.rest[i], &rest));
    return eval_locate(row, col);
}

/* the cursor: a LOCATE's row or column left out keeps it */
/* (a line filled to its last column: the next line's start, as QBasic
 * reports it -- the bottom of the viewport still its bottom, and from
 * the last row, below the viewport, the viewport's bottom) */
int eval_cursor_row(void) {
    extern int console_width;
    if (console_width > 0 && print_col >= console_width) {
        if (print_row < view_bottom) return print_row + 1;
        if (print_row > view_bottom) return view_bottom;
    }
    return print_row;
}
int eval_cursor_col(void) {
    extern int console_width;
    return console_width > 0 && print_col >= console_width ? 1 : print_col + 1;
}

/* SCREEN(row, col[, attribute]): the character there, or (attribute not
 * 0) its attribute -- for compiled code too */
error_t eval_screen_at(int row, int col, int attr, value_t *out) {
    extern int console_width;
    int cols = console_width > 0 ? console_width : 80;
    if (row < 1 || row > scr_rows || col < 1 || col > cols || col > SCR_COLS)
        return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_integer(attr ? scr_at[row - 1][col - 1] : scr_ch[row - 1][col - 1]);
    return ERR_NONE;
}

/* the text screen at B800 (machine.c): byte k of its cells, a character
 * then an attribute each, a row of the width's cells after another --
 * read (v < 0), or written and shown */
int eval_text_byte(int k, int v) {
    extern int console_width;
    int cols = console_width > 0 && console_width <= SCR_COLS ? console_width : 80;
    int cell = k / 2, row = cell / cols, col = cell % cols;
    if (row >= scr_rows) return 0;
    if (v < 0) return k & 1 ? scr_at[row][col] : scr_ch[row][col];
    if (k & 1) scr_at[row][col] = (unsigned char)v;
    else scr_ch[row][col] = (unsigned char)v;
    sb_term_putat(row + 1, col + 1, scr_ch[row][col], scr_at[row][col], print_row, print_col + 1);
    return v;
}

int eval_text_cols(void) {
    extern int console_width;
    return console_width > 0 ? console_width : 80;
}

int eval_text_rows(void) { return scr_rows; }

/* VIEW PRINT [top TO bottom] (none: all the rows) -- for compiled code too */
error_t eval_view_print(int has, int top, int bottom) {
    if (!has) { top = 1; bottom = scr_rows; }
    if (top < 1 || bottom > scr_rows || top > bottom) return ERR_ILLEGAL_FUNCTION_CALL;
    view_top = top;
    view_bottom = bottom;
    print_row = top;                    /* (the model's cursor: a terminal has no viewport to move in) */
    print_col = 0;
    return ERR_NONE;
}

/* WIDTH's rows (25, 43, 50 ...): the screen cleared, the viewport all of
 * it but the last row -- for compiled code too */
/* WIDTH's rows: what a VGA's text has -- 25, 43 or 50, and at 480
 * lines (SCREEN 11, 12) 30 or 60 too (QBasic, probed) */
error_t eval_width_rows(int rows) {
    int m = sb_gfx_mode();
    if (rows != 25 && rows != 43 && rows != 50 &&
        !((rows == 30 || rows == 60) && (m == 11 || m == 12)))
        return ERR_ILLEGAL_FUNCTION_CALL;
    if (rows == scr_rows) return ERR_NONE;
    scr_rows = rows;
    view_top = 1;
    view_bottom = rows > 1 ? rows - 1 : 1;
    eval_cls();
    return ERR_NONE;
}

/* LOCATE row, col, made integers -- for compiled code too: a row in the
 * text viewport, or the last (QBasic) */
error_t eval_locate(int row, int col) {
    if (row < 1 || col < 1 || col > eval_text_cols()) return ERR_ILLEGAL_FUNCTION_CALL;
    if ((row < view_top || row > view_bottom) && row != scr_rows) return ERR_ILLEGAL_FUNCTION_CALL;
    sb_term_locate(row, col);
    print_col = col - 1;
    print_row = row;
    return ERR_NONE;
}

static error_t exec_color(env_t *env, stmt_t *s) {
    int fg = 7;
    int bg = 0;
    if (s->color_stmt.has_fg) {
        value_t fv;
        EVAL_CHECK(eval_expr(env, s->color_stmt.fg, &fv));
        error_t err = val_to_integer(&fv, &fg);
        val_clear(&fv);
        if (err != ERR_NONE) return err;
        EVAL_CHECK(eval_color_arg(fg));
    }
    if (s->color_stmt.has_bg) {
        value_t bv;
        EVAL_CHECK(eval_expr(env, s->color_stmt.bg, &bv));
        error_t err = val_to_integer(&bv, &bg);
        val_clear(&bv);
        if (err != ERR_NONE) return err;
        EVAL_CHECK(eval_color_arg(bg));
    }
    eval_color(fg, bg, s->color_stmt.has_fg, s->color_stmt.has_bg);
    return ERR_NONE;
}

/* COLOR's colors, each 0 to 15, then COLOR -- for compiled code too */
error_t eval_color_arg(int c) {
    return c < 0 || c > 15 ? ERR_ILLEGAL_FUNCTION_CALL : ERR_NONE;
}

void eval_color(int fg, int bg, int has_fg, int has_bg) {
    sb_term_color(fg, bg, has_fg, has_bg);
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
        if (!array_find(e->call.name))
            array_implicit(e->call.name, e->call.nargs);
        return array_get(e->call.name, indices, e->call.nargs, out);
    }
    case EXPR_FIELD_ACCESS:
        return field_slot(env, e->field.var_name, e->field.field_name,
                          e->field.elem, out, NULL, NULL);
    default:
        return ERR_TYPE_MISMATCH;
    }
}

/* var.field, or var(subscripts).field (elem, the element): the field's
 * storage, and its type */
static error_t field_slot(env_t *env, const char *var, const char *field,
                          expr_t *elem, value_t **out, val_type_t *ftype, int *flen) {
    value_t *v;
    if (elem) EVAL_CHECK(resolve_lvalue(env, elem, &v));
    else v = env_get(env, var);
    if (!v || v->type != VAL_RECORD) return elem ? ERR_TYPE_MISMATCH : ERR_UNDEFINED_VAR;
    type_def_t *td = &type_defs[v->rval->type_idx];
    int fi = find_field_index(td, field);
    if (fi < 0) return ERR_UNDEFINED_FIELD;
    *out = &v->rval->fields[fi];
    if (ftype) *ftype = td->field_types[fi];
    if (flen) *flen = td->field_len[fi];
    return ERR_NONE;
}

/* the fixed length of the storage lvalue e names (a STRING * n variable,
 * element or field), else 0 */
static int lvalue_fixlen(env_t *env, expr_t *e) {
    if (e->type == EXPR_VARIABLE)
        return env_fixlen(env, e->var.name);
    if (e->type == EXPR_CALL) {
        sb_array_t *arr = array_find(e->call.name);
        return arr ? arr->fixlen : 0;
    }
    if (e->type == EXPR_FIELD_ACCESS) {
        value_t *f;
        int n = 0;
        if (field_slot(env, e->field.var_name, e->field.field_name, e->field.elem,
                       &f, NULL, &n) != ERR_NONE)
            return 0;
        return n;
    }
    return 0;
}

/* Check if an lvalue expression refers to a CONST variable */
static int lvalue_is_const(env_t *env, expr_t *e) {
    if (e->type == EXPR_VARIABLE)
        return env_is_const(env, e->var.name);
    if (e->type == EXPR_FIELD_ACCESS && !e->field.elem)
        return env_is_const(env, e->field.var_name);
    return 0;
}

/* X = e, before e is evaluated: X is no CONST, and under OPTION EXPLICIT
 * it exists */
error_t eval_assign_check(env_t *env, const char *name) {
    if (env_is_const(env, name))
        return ERR_CONST_REASSIGN;
    if (option_explicit && !env_exists(env, name))
        return ERR_UNDEFINED_VAR;
    return ERR_NONE;
}

error_t eval_assign_store(env_t *env, const char *name, const value_t *v) {
    var_entry_t *e = env_entry(env, name);
    if (!e) return ERR_OUT_OF_MEMORY;
    return store_fixed(e->link ? e->link : &e->value, v, e->fixlen);
}

static error_t exec_assign(env_t *env, stmt_t *s) {
    EVAL_CHECK(eval_assign_check(env, s->assign.name));
    value_t v;
    EVAL_CHECK(eval_expr(env, s->assign.value, &v));
    error_t err = eval_assign_store(env, s->assign.name, &v);
    val_clear(&v);
    return err;
}

/* FOR, as GW-BASIC and QuickBASIC run it: the limit and the step are
 * evaluated once, into a frame on the activation's FOR stack; the control
 * variable is the counter, so the body may change it, and NEXT adds the
 * step to whatever it holds.  NEXT steps the topmost frame of its
 * variable (the top frame if it names none), dropping those above it --
 * loops left by GOTO -- so two FORs may share a NEXT (checkers).  A FOR
 * replaces only a frame of its own, with those above it (a loop entered
 * again by GOTO): an inner FOR of the same variable leaves the outer
 * loop running, as in QBasic (NBS P054). */
/* for_slot_t: eval.h */

static int for_in_range(double v, const for_slot_t *f) {
    return f->step >= 0 ? v <= f->limit : v >= f->limit;
}

static error_t for_set(env_t *env, stmt_t *s, double v) {
    value_t nv = val_double(v);
    return store_into(env_get(env, s->for_stmt.var_name), &nv);
}

static error_t for_start(env_t *env, stmt_t *s, for_slot_t *f, int *runs) {
    if (env_is_const(env, s->for_stmt.var_name))
        return ERR_CONST_REASSIGN;
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
    double counter;
    value_t *cv;
    error_t cerr = val_to_double(&start_val, &counter);
    if (cerr == ERR_NONE) cerr = val_to_double(&end_val, &f->limit);
    if (cerr == ERR_NONE) cerr = val_to_double(&step_val, &f->step);
    val_clear(&start_val);
    val_clear(&end_val);
    val_clear(&step_val);
    if (cerr != ERR_NONE) return cerr;
    EVAL_CHECK(for_set(env, s, counter));
    cv = env_get(env, s->for_stmt.var_name);
    if (cv) val_to_double(cv, &counter);
    *runs = for_in_range(counter, f);
    return ERR_NONE;
}

static error_t for_next(env_t *env, stmt_t *s, for_slot_t *f, int *again) {
    value_t *cv;
    if (f->var && f->gen == env_generation) {
        cv = f->var;
    } else {
        /* (a NEXT with no loop running reaches a CONST the failed FOR
         * did not count: it must not step it either) */
        if (env_is_const(env, s->for_stmt.var_name))
            return ERR_CONST_REASSIGN;
        cv = f->var = env_get(env, s->for_stmt.var_name);
        f->gen = env_generation;
    }
    double v = 0.0;
    /* the variable as FOR made it: stepped where it lives */
    if (cv && VAL_IS_INT(cv->type)) {
        v = (double)cv->ival + f->step;
        if (!(v <= 2147483647.0 && v >= -2147483648.0) || v != (double)(int)v ||
            (cv->type == VAL_INTEGER && !INT16_OK(v))) {
            EVAL_CHECK(for_set(env, s, v));       /* rounds, or overflows */
            cv = env_get(env, s->for_stmt.var_name);
            if (cv) EVAL_CHECK(val_to_double(cv, &v));
        } else {
            cv->ival = (int)v;
        }
    } else if (cv && cv->type == VAL_DOUBLE) {
        v = cv->dval + f->step;
        cv->dval = v;
    } else if (cv && cv->type == VAL_SINGLE) {
        v = single_round(cv->dval + f->step);
        cv->dval = v;
    } else {
        if (cv) EVAL_CHECK(val_to_double(cv, &v));
        v += f->step;
        EVAL_CHECK(for_set(env, s, v));
        cv = env_get(env, s->for_stmt.var_name);
        if (cv) EVAL_CHECK(val_to_double(cv, &v));
    }
    *again = for_in_range(v, f);
    return ERR_NONE;
}

/* SELECT CASE: *which is the index of the clause that matches, in
 * order, CASE ELSE's when none does, the number of clauses when there is
 * no CASE ELSE either */
static error_t select_match(env_t *env, stmt_t *s, int *which) {
    value_t test;
    EVAL_CHECK(eval_expr(env, s->select_stmt.test_expr, &test));

    int k = 0, else_k = -1;
    *which = -1;

    for (case_clause_t *c = s->select_stmt.clauses; c; c = c->next, k++) {
        if (c->is_else) {
            else_k = k;
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
            *which = k;
            break;
        }
    }

    val_clear(&test);
    if (*which < 0) *which = else_k >= 0 ? else_k : k;
    return ERR_NONE;
}

static error_t exec_const(env_t *env, stmt_t *s) {
    value_t v;
    EVAL_CHECK(eval_expr(env, s->const_stmt.value, &v));

    /* CONST takes its value's type, unless its name has a suffix */
    {
        const char *nm = s->const_stmt.name;
        int len = (int)strlen(nm);
        if (len > 0 && strchr("%&!#$", nm[len - 1])) {
            error_t err = val_coerce(&v, s->const_stmt.var_type);
            if (err != ERR_NONE) { val_clear(&v); return err; }
        }
    }

    env_set_const(env, s->const_stmt.name, &v);
    val_clear(&v);
    return ERR_NONE;
}

static error_t exec_call_stmt(env_t *env, stmt_t *s) {
    proc_entry_t *proc = find_proc(s->call_stmt.name);
    if (!proc)
        return ast_machine_sub(s->call_stmt.name) ? ERR_ADVANCED : ERR_UNDEFINED_PROC;

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

/* DIM or REDIM name(lowers TO uppers...): the interpreter's DIM, and
 * compiled code's (s32-bc) */
error_t eval_dim(const char *name, val_type_t type, int ndims, const int *lowers,
                 const int *uppers, int is_redim, int preserve) {
    int sizes[MAX_ARRAY_DIMS];
    for (int i = 0; i < ndims; i++) {
        sizes[i] = uppers[i] - lowers[i] + 1;
        if (sizes[i] <= 0) return ERR_SUBSCRIPT_OUT_OF_RANGE;
    }
    if (is_redim)
        return array_redim(name, type, sizes, ndims, lowers, preserve);
    {   /* a procedure's STATIC array DIMmed again with the same bounds: a
         * declaration (QuickBASIC makes one of constant bounds at load) */
        sb_array_t *a = array_find(name);
        if (a && array_is_static(name) && a->ndims == ndims) {
            int same = 1;
            for (int i = 0; i < ndims; i++)
                if (a->dims[i] != sizes[i] || a->lbound[i] != lowers[i]) same = 0;
            if (same) return ERR_NONE;
        }
    }
    return array_dim(name, type, sizes, ndims, lowers);
}

/* a DIM's bounds of one dimension, made integers: a range with nothing
 * in it is Subscript out of range (QBasic, DIM A(-1) and A(5 TO 1) both)
 * -- checked before the next dimension's are evaluated */
error_t eval_dim_range(int lower, int upper) {
    return upper - lower + 1 <= 0 ? ERR_SUBSCRIPT_OUT_OF_RANGE : ERR_NONE;
}

int eval_option_base(void) { return option_base; }
void eval_set_option_base(int base) { option_base = base; }

static error_t eval_int_of(env_t *env, expr_t *e, int *out) {
    value_t dv;
    EVAL_CHECK(eval_expr(env, e, &dv));
    error_t err = val_to_integer(&dv, out);
    val_clear(&dv);
    return err;
}

static error_t exec_dim(env_t *env, stmt_t *s) {
    int lowers[MAX_ARRAY_DIMS], uppers[MAX_ARRAY_DIMS];
    /* a static array was made before the run; DIM is then a declaration
     * (unless ERASE took it away) */
    if (!(s->dim_stmt.is_static && array_find(s->dim_stmt.name))) {
        for (int i = 0; i < s->dim_stmt.ndims; i++) {
            lowers[i] = option_base;
            if (s->dim_stmt.lower[i]) EVAL_CHECK(eval_int_of(env, s->dim_stmt.lower[i], &lowers[i]));
            EVAL_CHECK(eval_int_of(env, s->dim_stmt.dims[i], &uppers[i]));
            EVAL_CHECK(eval_dim_range(lowers[i], uppers[i]));
        }
        EVAL_CHECK(eval_dim(s->dim_stmt.name, dim_type(s), s->dim_stmt.ndims, lowers, uppers,
                            s->dim_stmt.is_redim, s->dim_stmt.preserve));
    }
    if (s->dim_stmt.rec_type[0]) {
        int ti = find_type_def(s->dim_stmt.rec_type);
        if (ti < 0) return ERR_UNDEFINED_TYPE;
        return eval_array_records(s->dim_stmt.name, ti, type_defs[ti].nfields,
                                  type_defs[ti].field_types, type_defs[ti].field_len);
    }
    if (s->dim_stmt.fixlen)
        return eval_array_fixed(s->dim_stmt.name, s->dim_stmt.fixlen);
    return ERR_NONE;
}

/* name(indices) read as an expression reads an array: the array, or an
 * implicit one made (subscripts to 10), then the element -- for
 * compiled code (s32-bc) */
error_t eval_array_get(const char *name, int nidx, int *indices, value_t *out) {
    sb_array_t *arr;
    value_t *elem;
    EVAL_CHECK(eval_array_for(name, nidx, &arr));
    EVAL_CHECK(array_get(name, indices, nidx, &elem));
    *out = val_copy(elem);
    return ERR_NONE;
}

/* RANDOMIZE seed, or RANDOMIZE (no seed given: QuickBASIC asks for
 * one) -- for compiled code too */
error_t eval_randomize(int has_seed, double seed) {
    if (!has_seed) {
        char line[128];
        col_puts("Random-number seed (-32768 to 32767)? ");
        if (read_input_line(line, sizeof(line)) < 0) {
            col_puts("\n");
            return ERR_INPUT_PAST_END;
        }
        if (isatty(0)) print_col = 0;
        else col_puts("\n");
        seed = atof(line);
    }
    builtin_randomize(seed);
    return ERR_NONE;
}

/* LBOUND(name, dim) or UBOUND (upper) -- for compiled code too */
error_t eval_bound(const char *name, int dim, int upper, value_t *out) {
    sb_array_t *a = array_find(name);
    if (!a) return ERR_UNDEFINED_VAR;
    if (dim < 1 || dim > a->ndims)
        return ERR_ILLEGAL_FUNCTION_CALL;
    *out = val_integer(upper ? a->lbound[dim - 1] + a->dims[dim - 1] - 1 : a->lbound[dim - 1]);
    return ERR_NONE;
}

/* the array name(i, ...) reaches with nidx subscripts, read or
 * assigned: one DIMmed, or an implicit one made (subscripts to 10) */
error_t eval_array_for(const char *name, int nidx, sb_array_t **out) {
    sb_array_t *arr = array_find(name);
    if (!arr) arr = array_implicit(name, nidx);
    if (!arr) return ERR_UNDEFINED_VAR;
    if (nidx != arr->ndims) return ERR_WRONG_DIMS;
    *out = arr;
    return ERR_NONE;
}

/* A(i, ...) = v, the subscripts and the value evaluated (an array not
 * DIMmed yet is made) */
error_t eval_array_store(const char *name, int nidx, int *indices, const value_t *v) {
    sb_array_t *arr;
    value_t *slot;
    EVAL_CHECK(eval_array_for(name, nidx, &arr));
    EVAL_CHECK(array_get(name, indices, nidx, &slot));
    return store_fixed(slot, v, arr->fixlen);
}

/* The array an expression name(args) reads, when that is what it does
 * (no FUNCTION or DEF FN of the name, and the array exists); NULL
 * otherwise -- the interpreter's own route then decides */
sb_array_t *eval_call_array(const char *name) {
    proc_entry_t *proc = find_proc(name);
    if (proc && (proc->def->type == STMT_FUNC_DEF || proc->def->type == STMT_DEF_FN))
        return NULL;
    return array_find(name);
}

/* arr(indices), an array eval_call_array gave */
error_t eval_array_read(sb_array_t *arr, int nidx, int *indices, value_t *out) {
    value_t *elem;
    if (nidx != arr->ndims)
        return ERR_WRONG_DIMS;
    EVAL_CHECK(array_elem_of(arr, indices, nidx, &elem));
    *out = val_copy(elem);
    return ERR_NONE;
}

/* A(i, ...) = e: e first, then the subscripts (QBasic's order: a(f) = g
 * calls g, then f) */
static error_t exec_array_assign(env_t *env, stmt_t *s) {
    int indices[MAX_ARRAY_DIMS];
    value_t v;
    error_t err;
    EVAL_CHECK(eval_expr(env, s->array_assign.value, &v));
    for (int i = 0; i < s->array_assign.nindices; i++) {
        value_t iv;
        err = eval_expr(env, s->array_assign.indices[i], &iv);
        if (err == ERR_NONE) {
            err = val_to_integer(&iv, &indices[i]);
            val_clear(&iv);
        }
        if (err != ERR_NONE) { val_clear(&v); return err; }
    }
    err = eval_array_store(s->array_assign.name, s->array_assign.nindices, indices, &v);
    val_clear(&v);
    return err;
}

static error_t exec_erase(env_t *env, stmt_t *s) {
    for (int i = 0; i < s->erase_stmt.nnames; i++)
        array_erase(s->erase_stmt.names[i]);
    return ERR_NONE;
}

/* A DATA item as a number: [+|-] digits [. digits] [E|D [+|-] digits],
 * the whole item, or nothing at all (which is 0) */
static int data_number(const char *t, double *out) {
    const char *q = t;
    char buf[256];
    int n = 0, digits = 0;
    if (*q == '+' || *q == '-') buf[n++] = *q++;
    while (isdigit((unsigned char)*q) && n < 250) { buf[n++] = *q++; digits++; }
    if (*q == '.') {
        buf[n++] = *q++;
        while (isdigit((unsigned char)*q) && n < 250) { buf[n++] = *q++; digits++; }
    }
    if (!digits) {
        *out = 0.0;
        return *t == '\0';
    }
    if (*q == 'E' || *q == 'e' || *q == 'D' || *q == 'd') {
        buf[n++] = 'E';
        q++;
        if (*q == '+' || *q == '-') buf[n++] = *q++;
        if (!isdigit((unsigned char)*q)) return 0;
        while (isdigit((unsigned char)*q) && n < 254) buf[n++] = *q++;
    }
    buf[n] = '\0';
    if (*q) return 0;
    *out = strtod(buf, NULL);
    return 1;
}

/* the next DATA item, read into a variable of type t: a DATA item is
 * text, and read into a number it has to be one, and not a quoted string.
 * The interpreter's READ, and compiled code's (s32-bc). */
error_t eval_read_value(val_type_t t, value_t *out) {
    if (data_read_ptr >= data_pool_count)
        return ERR_OUT_OF_DATA;
    int quoted = data_quoted[data_read_ptr];
    value_t v = val_copy(&data_pool[data_read_ptr++]);
    if (t != VAL_STRING && v.type == VAL_STRING) {
        double d;
        int ok = !quoted && data_number(v.sval->data, &d);
        val_clear(&v);
        if (!ok) return ERR_SYNTAX;
        v = val_double(d);
    }
    *out = v;
    return ERR_NONE;
}

/* RESTORE: the next item read is the offset-th of the program's */
void eval_restore(int offset) { data_read_ptr = offset; }

static error_t exec_read(env_t *env, stmt_t *s) {
    for (int i = 0; i < s->read_stmt.nvars; i++) {
        if (env_is_const(env, s->read_stmt.varnames[i]))
            return ERR_CONST_REASSIGN;
        value_t v;
        EVAL_CHECK(eval_read_value(target_type(env, s->read_stmt.varnames[i],
                                               s->read_stmt.targets[i]), &v));

        error_t err = store_target(env, s->read_stmt.targets[i], s->read_stmt.varnames[i], &v);
        val_clear(&v);
        if (err != ERR_NONE) return err;
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
    error_t err = eval_expr(env, s->open_stmt.handle_num, &hv);
    if (err != ERR_NONE) { val_clear(&fname); return err; }
    int handle;
    err = val_to_integer(&hv, &handle);
    val_clear(&hv);
    if (err != ERR_NONE) { val_clear(&fname); return err; }
    err = fileio_open(fname.sval->data, (file_mode_t)s->open_stmt.mode, handle);
    val_clear(&fname);
    if (err != ERR_NONE) return err;

    /* Set record length for RANDOM mode */
    if (s->open_stmt.mode == FMODE_RANDOM) {
        int reclen = 128; /* QBasic default */
        if (s->open_stmt.reclen) {
            value_t rv;
            err = eval_expr(env, s->open_stmt.reclen, &rv);
            if (err != ERR_NONE) return err;
            err = val_to_integer(&rv, &reclen);
            val_clear(&rv);
            if (err != ERR_NONE) return err;
        }
        fileio_set_reclen(handle, reclen);
    }
    return ERR_NONE;
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

/* PRINT #handle's output: to the file, with its own column, until
 * eval_print_file_end puts back what was -- for compiled code too */
error_t eval_print_file_begin(int handle, print_redirect_t *saved) {
    FILE *fp = fileio_get(handle);
    if (!fp) return ERR_FILE_NOT_OPEN;
    int *file_col = fileio_get_col_ptr(handle);
    saved->col = active_print_col;
    saved->fp = col_output_fp;
    if (file_col) active_print_col = file_col;
    col_output_fp = fp;
    return ERR_NONE;
}

void eval_print_file_end(const print_redirect_t *saved) {
    col_output_fp = saved->fp;
    active_print_col = saved->col;
}

static error_t exec_print_file(env_t *env, stmt_t *s) {
    value_t hv;
    EVAL_CHECK(eval_expr(env, s->print_file.handle_num, &hv));
    int handle;
    error_t err = val_to_integer(&hv, &handle);
    val_clear(&hv);
    if (err != ERR_NONE) return err;
    print_redirect_t saved;
    EVAL_CHECK(eval_print_file_begin(handle, &saved));
    FILE *saved_fp = saved.fp;
    int *saved_col = saved.col;
    if (s->print_file.using_fmt || s->print_file.using_expr) {
        const char *fmt;
        char fmt_buf[1024];
        if (s->print_file.using_fmt) {
            fmt = s->print_file.using_fmt;
        } else {
            value_t fv;
            err = eval_expr(env, s->print_file.using_expr, &fv);
            if (err != ERR_NONE) { col_output_fp = saved_fp; active_print_col = saved_col; return err; }
            if (fv.type == VAL_STRING && fv.sval)
                snprintf(fmt_buf, sizeof(fmt_buf), "%s", fv.sval->data);
            else
                fmt_buf[0] = '\0';
            val_clear(&fv);
            fmt = fmt_buf;
        }
        err = exec_print_using_items(env, s->print_file.items, s->print_file.nitems, fmt);
    } else {
        err = print_items(env, s->print_file.items, s->print_file.nitems, 0);
    }
    col_output_fp = saved_fp;
    active_print_col = saved_col;
    return err;
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
            eval_write_file_value(fp, &v);
            val_clear(&v);
        }
    }
    fprintf(fp, "\n");
    return ERR_NONE;
}

/* a WRITE # item's value, as it writes it -- for compiled code too */
void eval_write_file_value(FILE *fp, const value_t *v) {
    switch (v->type) {
        case VAL_INTEGER:
        case VAL_LONG: fprintf(fp, "%d", v->ival); break;
        case VAL_DOUBLE:
        case VAL_SINGLE: {
            char buf[64];
            val_format_number(v, buf, sizeof(buf));
            fputs(buf, fp);
            break;
        }
        case VAL_STRING: fprintf(fp, "\"%s\"", v->sval->data); break;
        case VAL_RECORD: break;
    }
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
        if (env_is_const(env, s->input_file.varnames[i]))
            return ERR_CONST_REASSIGN;
        value_t val;
        EVAL_CHECK(eval_input_field(fp, target_type(env, s->input_file.varnames[i], s->input_file.targets[i]),
                                    &val));
        err = store_target(env, s->input_file.targets[i], s->input_file.varnames[i], &val);
        val_clear(&val);
        if (err != ERR_NONE) return err;
    }
    return ERR_NONE;
}

/* INPUT #'s next field, a string or (otherwise) a number, for a target
 * of type t -- for compiled code too */
error_t eval_input_field(FILE *fp, val_type_t t, value_t *out) {
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
    *out = t == VAL_STRING ? val_string_cstr(buf) : val_double(atof(buf));
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

    value_t val;
    EVAL_CHECK(eval_line_input_file(fp, &val));
    if (s->input_file.nvars > 0) {
        if (env_is_const(env, s->input_file.varnames[0])) {
            val_clear(&val);
            return ERR_CONST_REASSIGN;
        }
        if (s->input_file.targets[0]) {     /* an array element */
            err = store_target(env, s->input_file.targets[0], s->input_file.varnames[0], &val);
            val_clear(&val);
            return err;
        }
        env_set(env, s->input_file.varnames[0], &val);
    }
    val_clear(&val);
    return ERR_NONE;
}

/* LINE INPUT #'s line -- for compiled code too */
error_t eval_line_input_file(FILE *fp, value_t *out) {
    char buf[1024];
    int pos = 0;
    int ch;
    while ((ch = fgetc(fp)) != -1 && ch != '\n' && pos < 1023)
        buf[pos++] = (char)ch;
    buf[pos] = '\0';
    if (pos == 0 && ch == -1) return ERR_INPUT_PAST_END;
    *out = val_string_cstr(buf);
    return ERR_NONE;
}

/* LINE INPUT (console) — read full line without comma/quote parsing */
static error_t exec_line_input_console(env_t *env, stmt_t *s) {
    value_t val;
    EVAL_CHECK(eval_line_input(s->input.prompt, &val));
    if (s->input.nvars > 0) {
        if (env_is_const(env, s->input.varnames[0])) {
            val_clear(&val);
            return ERR_CONST_REASSIGN;
        }
        if (s->input.targets[0]) {          /* an array element */
            error_t err = store_target(env, s->input.targets[0], s->input.varnames[0], &val);
            val_clear(&val);
            return err;
        }
        env_set(env, s->input.varnames[0], &val);
    }
    val_clear(&val);
    return ERR_NONE;
}

/* LINE INPUT's line (its prompt first, if any) -- for compiled code too */
error_t eval_line_input(const char *prompt, value_t *out) {
    if (prompt)
        col_puts(prompt);

    char line[1024];
    int pos = 0;
    int ch;
    while ((ch = getchar()) != EOF && ch != '\n') {
        if (pos < 1023)
            line[pos++] = (char)ch;
    }
    line[pos] = '\0';
    print_col = 0;

    if (ch == EOF && pos == 0)
        return ERR_INPUT_PAST_END;
    *out = val_string_cstr(line);
    return ERR_NONE;
}

/* WRITE (console) — comma-delimited, quoted string output */
static error_t exec_write_console(env_t *env, stmt_t *s) {
    for (int i = 0; i < s->print.nitems; i++) {
        if (i > 0) col_puts(",");
        print_item_t *item = &s->print.items[i];
        if (item->expr) {
            value_t v;
            EVAL_CHECK(eval_expr(env, item->expr, &v));
            eval_write_value(&v);
            val_clear(&v);
        }
    }
    col_puts("\n");
    return ERR_NONE;
}

/* a WRITE item's value, as WRITE writes it -- for compiled code too */
void eval_write_value(const value_t *v) {
    switch (v->type) {
        case VAL_INTEGER:
        case VAL_LONG: {
            char buf[32];
            snprintf(buf, sizeof(buf), "%d", v->ival);
            col_puts(buf);
            break;
        }
        case VAL_DOUBLE:
        case VAL_SINGLE: {
            char buf[64];
            val_format_number(v, buf, sizeof(buf));
            col_puts(buf);
            break;
        }
        case VAL_STRING:
            col_puts("\"");
            col_puts(v->sval ? v->sval->data : "");
            col_puts("\"");
            break;
        case VAL_RECORD: break;
    }
}

/* CHDIR, MKDIR or RMDIR (kind: the statement's) path -- for compiled code too */
error_t eval_dir(int kind, const value_t *path) {
    if (path->type != VAL_STRING) return ERR_TYPE_MISMATCH;
    const char *p = path->sval ? path->sval->data : "";
    int r = kind == STMT_CHDIR ? chdir(p) : kind == STMT_MKDIR ? mkdir(p, 0777) : rmdir(p);
    return r != 0 ? ERR_FILE_NOT_FOUND : ERR_NONE;
}

/* POKE offset, v (in DEF SEG's segment: machine.c) -- for compiled code too */
error_t eval_poke(int offset, int val) {
    return pc_poke(offset, val);
}

/* SLEEP seconds (v NULL: SLEEP alone) -- for compiled code too */
error_t eval_sleep(const value_t *v) {
    double secs = 0.0;
    if (v) {
        if (v->type == VAL_STRING) return ERR_TYPE_MISMATCH;
        if (val_to_double(v, &secs) != ERR_NONE) secs = 0.0;
        if (secs < 0) return ERR_ILLEGAL_FUNCTION_CALL;
    }
    /* Fractional seconds sleep (SLEEP 0.03 paces animation) */
    if (secs > 0) {
        unsigned int whole = (unsigned int)secs;
        unsigned int usecs = (unsigned int)((secs - whole) * 1e6);
        if (whole > 0) sleep(whole);
        if (usecs > 0) usleep(usecs);
    }
    return ERR_NONE;
}

/* WIDTH w, made an integer -- for compiled code too */
/* WIDTH's columns (20 to 255 kept, for the dialects that set a line's
 * length so).  In a graphics mode QBasic changes to the mode of that
 * width (probed): 80 makes SCREEN 1 into 2, 7 into 8, and 13 into text
 * (SCREEN 0, 80 columns); 40 makes 2 into 1, and 8 or 9 into 7; 40 in
 * SCREEN 11 or 12 is refused; a mode already that wide is left alone */
error_t eval_width(int w) {
    extern int console_width;
    int m = sb_gfx_mode(), to = -1;
    if (w == 80) to = m == 1 ? 2 : m == 7 ? 8 : m == 13 ? 0 : -1;
    if (w == 40) {
        if (m == 11 || m == 12) return ERR_ILLEGAL_FUNCTION_CALL;
        to = m == 2 ? 1 : m == 8 || m == 9 ? 7 : -1;
    }
    if (to >= 0) {
        EVAL_CHECK(eval_screen(to));
        if (!to) console_width = 80;
        return ERR_NONE;
    }
    if (m && (w == 40 || w == 80)) return ERR_NONE;
    if (w >= 20 && w <= 255) console_width = w;
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

/* --- Binary I/O: GET, PUT, SEEK --- */

static error_t exec_get(env_t *env, stmt_t *s) {
    value_t hv;
    EVAL_CHECK(eval_expr(env, s->get_put_stmt.handle_num, &hv));
    int handle;
    error_t err = val_to_integer(&hv, &handle);
    val_clear(&hv);
    if (err != ERR_NONE) return err;

    FILE *fp;
    EVAL_CHECK(eval_getput_file(handle, &fp));

    /* Seek to position if given */
    if (s->get_put_stmt.position) {
        value_t pv;
        EVAL_CHECK(eval_expr(env, s->get_put_stmt.position, &pv));
        int pos;
        err = val_to_integer(&pv, &pos);
        val_clear(&pv);
        if (err != ERR_NONE) return err;
        EVAL_CHECK(eval_file_pos(handle, fp, pos));
    }

    /* Read as the variable, element or field is typed (a record field
     * by field, a STRING * n its n bytes) */
    value_t *sv;
    int fl;
    if (s->get_put_stmt.target) {
        EVAL_CHECK(resolve_lvalue(env, s->get_put_stmt.target, &sv));
        fl = lvalue_fixlen(env, s->get_put_stmt.target);
    } else {
        sv = env_get(env, s->get_put_stmt.varname);
        fl = env_fixlen(env, s->get_put_stmt.varname);
    }
    if (!sv) return ERR_OUT_OF_MEMORY;
    if (sv->type == VAL_RECORD)
        return eval_get_record(fp, sv->rval);
    value_t result;
    EVAL_CHECK(eval_get_value(fp, sv->type, sv->type == VAL_STRING ? sv->sval->len : 0, &result));
    err = store_fixed(sv, &result, fl);
    val_clear(&result);
    return err;
}

/* GET's value of type vt (a string's len bytes, at least 1) */
error_t eval_get_value(FILE *fp, val_type_t vt, int len, value_t *out) {
    value_t result;
    if (vt == VAL_INTEGER) {
        short sval = 0;                 /* QB: an INTEGER is 2 bytes */
        fread(&sval, 2, 1, fp);
        result = val_integer(sval);
    } else if (vt == VAL_LONG) {
        int ival = 0;
        fread(&ival, 4, 1, fp);
        result = val_long(ival);
    } else if (vt == VAL_DOUBLE) {
        double dval = 0.0;
        fread(&dval, 8, 1, fp);
        result = val_double(dval);
    } else if (vt == VAL_SINGLE) {
        float fval = 0.0f;
        fread(&fval, 4, 1, fp);
        result = val_single(fval);
    } else if (vt == VAL_STRING) {
        /* String GET: read LEN(var$) bytes */
        if (len <= 0) len = 1;
        char *buf = malloc(len + 1);
        if (!buf) return ERR_OUT_OF_MEMORY;
        int got = fread(buf, 1, len, fp);
        buf[got] = '\0';
        result = val_string(buf, got);
        free(buf);
    } else {
        return ERR_TYPE_MISMATCH;
    }
    *out = result;
    return ERR_NONE;
}

static error_t exec_put(env_t *env, stmt_t *s) {
    value_t hv;
    EVAL_CHECK(eval_expr(env, s->get_put_stmt.handle_num, &hv));
    int handle;
    error_t err = val_to_integer(&hv, &handle);
    val_clear(&hv);
    if (err != ERR_NONE) return err;

    FILE *fp;
    EVAL_CHECK(eval_getput_file(handle, &fp));

    /* Seek to position if given */
    if (s->get_put_stmt.position) {
        value_t pv;
        EVAL_CHECK(eval_expr(env, s->get_put_stmt.position, &pv));
        int pos;
        err = val_to_integer(&pv, &pos);
        val_clear(&pv);
        if (err != ERR_NONE) return err;
        EVAL_CHECK(eval_file_pos(handle, fp, pos));
    }

    /* Write as the variable, element or field is typed (a record field
     * by field) */
    value_t *v;
    if (s->get_put_stmt.target) EVAL_CHECK(resolve_lvalue(env, s->get_put_stmt.target, &v));
    else v = env_get(env, s->get_put_stmt.varname);
    if (!v) return ERR_UNDEFINED_VAR;
    if (v->type == VAL_RECORD)
        return eval_put_record(fp, v->rval);
    return eval_put_value(fp, v);
}

/* GET # and PUT # of a record: field by field, each as its type reads
 * and writes it, a string (a STRING * n: QuickBASIC's TYPEs have no
 * other) its length's bytes -- for compiled code too */
error_t eval_get_record(FILE *fp, sb_record_t *r) {
    for (int i = 0; i < r->nfields; i++) {
        value_t *fv = &r->fields[i], f;
        int n = fv->type == VAL_STRING ? fv->sval->len : 0;
        EVAL_CHECK(eval_get_value(fp, fv->type, n, &f));
        error_t err = store_fixed(fv, &f, n);
        val_clear(&f);
        if (err != ERR_NONE) return err;
    }
    return ERR_NONE;
}

error_t eval_put_record(FILE *fp, const sb_record_t *r) {
    for (int i = 0; i < r->nfields; i++)
        EVAL_CHECK(eval_put_value(fp, &r->fields[i]));
    return ERR_NONE;
}

/* PUT's value, as its type writes it */
error_t eval_put_value(FILE *fp, const value_t *v) {
    if (v->type == VAL_INTEGER) {
        short sval = (short)v->ival;
        fwrite(&sval, 2, 1, fp);
    } else if (v->type == VAL_LONG) {
        int ival = v->ival;
        fwrite(&ival, 4, 1, fp);
    } else if (v->type == VAL_DOUBLE) {
        double dval = v->dval;
        fwrite(&dval, 8, 1, fp);
    } else if (v->type == VAL_SINGLE) {
        float fval = (float)v->dval;
        fwrite(&fval, 4, 1, fp);
    } else if (v->type == VAL_STRING) {
        int len = v->sval->len;
        fwrite(v->sval->data, 1, len, fp);
    } else {
        return ERR_TYPE_MISMATCH;
    }
    fflush(fp);
    return ERR_NONE;
}

static error_t exec_seek(env_t *env, stmt_t *s) {
    value_t hv;
    EVAL_CHECK(eval_expr(env, s->seek_stmt.seek_handle, &hv));
    int handle;
    error_t err = val_to_integer(&hv, &handle);
    val_clear(&hv);
    if (err != ERR_NONE) return err;

    FILE *fp = fileio_get(handle);
    if (!fp) return ERR_FILE_NOT_OPEN;

    value_t pv;
    EVAL_CHECK(eval_expr(env, s->seek_stmt.seek_position, &pv));
    int pos;
    err = val_to_integer(&pv, &pos);
    val_clear(&pv);
    if (err != ERR_NONE) return err;
    return eval_file_pos(handle, fp, pos);
}

/* LSET (left) or RSET: nv, padded or cut to curlen -- for compiled code too */
error_t eval_lrset(int curlen, const value_t *nv, int left, value_t *out) {
    if (nv->type != VAL_STRING) return ERR_TYPE_MISMATCH;
    const char *src = nv->sval ? nv->sval->data : "";
    int srclen = nv->sval ? nv->sval->len : 0;
    char *buf = malloc(curlen + 1);
    if (!buf) return ERR_OUT_OF_MEMORY;
    memset(buf, ' ', curlen);
    buf[curlen] = '\0';
    int copylen = srclen < curlen ? srclen : curlen;
    if (left) memcpy(buf, src, copylen);
    else memcpy(buf + curlen - copylen, src, copylen);
    *out = val_string(buf, curlen);
    free(buf);
    return ERR_NONE;
}

/* GET's and PUT's file: open, and BINARY or RANDOM -- for compiled code too */
error_t eval_getput_file(int handle, FILE **fp) {
    *fp = fileio_get(handle);
    if (!*fp) return ERR_FILE_NOT_OPEN;
    file_mode_t mode = fileio_get_mode(handle);
    if (mode != FMODE_BINARY && mode != FMODE_RANDOM)
        return ERR_ILLEGAL_FUNCTION_CALL;
    return ERR_NONE;
}

/* the file at position pos (from 1): a byte, or a RANDOM file's record */
error_t eval_file_pos(int handle, FILE *fp, int pos) {
    if (pos < 1) return ERR_ILLEGAL_FUNCTION_CALL;
    if (fileio_get_mode(handle) == FMODE_RANDOM) {
        int reclen = fileio_get_reclen(handle);
        if (reclen <= 0) reclen = 128;
        fseek(fp, (long)(pos - 1) * reclen, SEEK_SET);
    } else {
        fseek(fp, (long)(pos - 1), SEEK_SET);
    }
    return ERR_NONE;
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
                s->type_def.fields[i].name, 63);
        type_defs[idx].field_types[i] = s->type_def.fields[i].type;
        type_defs[idx].field_len[i] = s->type_def.fields[i].fixlen;
    }
    return ERR_NONE;
}

static error_t exec_dim_as_type(env_t *env, stmt_t *s) {
    int ti = find_type_def(s->dim_as_type.type_name);
    if (ti < 0) return ERR_UNDEFINED_TYPE;
    /* a declaration: the record made once (QuickBASIC: a DIM in a loop
     * keeps its fields) */
    value_t *have = env_lookup(env, s->dim_as_type.name);
    if (have && have->type == VAL_RECORD) return ERR_NONE;
    type_def_t *td = &type_defs[ti];
    sb_record_t *rec = record_make(ti, td->nfields, td->field_types, td->field_len);
    if (!rec) return ERR_OUT_OF_MEMORY;
    value_t val;
    val.type = VAL_RECORD;
    val.rval = rec;
    env_set(env, s->dim_as_type.name, &val);
    val_clear(&val);
    return ERR_NONE;
}

static error_t exec_field_assign(env_t *env, stmt_t *s) {
    if (!s->field_assign.elem && env_is_const(env, s->field_assign.var_name))
        return ERR_CONST_REASSIGN;
    value_t *f;
    val_type_t ft;
    int fl;
    /* the value first, then the target's subscripts (QBasic's order) */
    value_t v;
    EVAL_CHECK(eval_expr(env, s->field_assign.value, &v));
    error_t err = field_slot(env, s->field_assign.var_name, s->field_assign.field_name,
                             s->field_assign.elem, &f, &ft, &fl);
    if (err == ERR_NONE) err = val_coerce(&v, ft);
    if (err == ERR_NONE) err = fix_string(&v, fl);
    if (err == ERR_NONE) val_assign(f, &v);
    val_clear(&v);
    return err;
}

/* --- MID$ l-value assignment --- */

static error_t exec_mid_assign(env_t *env, stmt_t *s) {
    value_t *sv;
    unsigned gen;
    if (s->mid_assign.target)           /* an element's string, or a field's */
        EVAL_CHECK(resolve_lvalue(env, s->mid_assign.target, &sv));
    else {
        if (env_is_const(env, s->mid_assign.var_name))
            return ERR_CONST_REASSIGN;
        sv = env_get(env, s->mid_assign.var_name);
    }
    gen = array_generation;
    if (!sv) return ERR_UNDEFINED_VAR;
    if (sv->type != VAL_STRING) return ERR_TYPE_MISMATCH;

    /* Evaluate start position (1-based) */
    value_t start_v;
    EVAL_CHECK(eval_expr(env, s->mid_assign.start, &start_v));
    int start;
    error_t err = val_to_integer(&start_v, &start);
    val_clear(&start_v);
    if (err != ERR_NONE) return err;

    int slen = sv->sval->len;
    start--; /* convert to 0-based */
    if (start < 0 || start >= slen) return ERR_NONE; /* out of range: no change */

    /* Evaluate optional length */
    int maxlen = slen - start;
    if (s->mid_assign.length) {
        value_t len_v;
        EVAL_CHECK(eval_expr(env, s->mid_assign.length, &len_v));
        int length;
        err = val_to_integer(&len_v, &length);
        val_clear(&len_v);
        if (err != ERR_NONE) return err;
        if (length < 0) return ERR_ILLEGAL_FUNCTION_CALL;
        if (length < maxlen) maxlen = length;
    }

    /* Evaluate replacement string */
    value_t rep_v, result;
    EVAL_CHECK(eval_expr(env, s->mid_assign.value, &rep_v));
    err = eval_mid_set(sv->sval, start, slen, maxlen, &rep_v, &result);
    val_clear(&rep_v);
    if (err != ERR_NONE) return err;
    if (s->mid_assign.target) {
        /* (the element found again only if an array moved meanwhile: a
         * REDIM in a FUNCTION the value called) */
        if (array_generation != gen) err = resolve_lvalue(env, s->mid_assign.target, &sv);
        if (err == ERR_NONE) val_assign(sv, &result);
    } else
        env_set(env, s->mid_assign.var_name, &result);
    val_clear(&result);
    return err;
}

/* MID$ = rep's new string: slen bytes of old, rep's over them from start
 * (0-based), maxlen at most -- for compiled code too */
error_t eval_mid_set(const sb_string_t *old, int start, int slen, int maxlen,
                     const value_t *rep, value_t *out) {
    if (rep->type != VAL_STRING) return ERR_TYPE_MISMATCH;
    int rep_len = rep->sval->len;
    int replace_count = (rep_len < maxlen) ? rep_len : maxlen;

    /* Build result: same length as original */
    char *buf = malloc(slen + 1);
    if (!buf) return ERR_OUT_OF_MEMORY;
    memcpy(buf, old->data, slen);
    memcpy(buf + start, rep->sval->data, replace_count);
    buf[slen] = '\0';
    *out = val_string(buf, slen);
    free(buf);
    return ERR_NONE;
}

/* --- Statement dispatch --- */

/* TRACE ON's [line] before a statement */
void eval_trace(const stmt_t *s) {
    if (trace_enabled && s->type != STMT_REM && s->type != STMT_LABEL
        && s->type != STMT_DATA && s->type != STMT_TRACE)
        col_printf("[%d] ", s->line);
}

error_t eval_stmt(env_t *env, stmt_t *s) {
    if (trace_enabled)
        eval_trace(s);
    switch (s->type) {
        case STMT_PRINT:        return exec_print(env, s);
        case STMT_LPRINT:       return exec_lprint(env, s);
        case STMT_INPUT:        return exec_input(env, s);
        case STMT_CLS:          return exec_cls(env, s);
        case STMT_LOCATE:       return exec_locate(env, s);
        case STMT_COLOR:        return exec_color(env, s);
        case STMT_ASSIGN:       return exec_assign(env, s);
        case STMT_NEXT:
            /* a NEXT outside a lowered unit has no FOR to find */
            return ERR_NEXT_WITHOUT_FOR;
        case STMT_IF:
        case STMT_FOR:
        case STMT_WHILE:
        case STMT_DO_LOOP:
        case STMT_SELECT: {
            /* a block statement on its own (immediate mode): lowered
             * and run as a unit */
            error_t err;
            code_t *c = lower_one(s, &err);
            if (!c) return err;
            err = run_code(env, c, 0);
            code_free(c);
            return err;
        }
        case STMT_CALL:         return exec_call_stmt(env, s);
        case STMT_CONST:        return exec_const(env, s);
        case STMT_DIM:          return exec_dim(env, s);
        case STMT_ARRAY_ASSIGN: return exec_array_assign(env, s);
        case STMT_ERASE:        return exec_erase(env, s);
        case STMT_READ:         return exec_read(env, s);
        case STMT_RESTORE:      return exec_restore(s);
        case STMT_END:          return ERR_EXIT;
        case STMT_RUN:          return ERR_RUN;
        case STMT_REM:          return ERR_NONE;
        case STMT_LABEL:        return ERR_NONE;
        case STMT_DECLARE:      return ERR_NONE;
        case STMT_SHARED:       return ERR_NONE;
        case STMT_STATIC:       return ERR_NONE;
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
            if (s->goto_stmt.label[0]) ast_name_copy(return_label, s->goto_stmt.label);
            else return_label[0] = '\0';
            return ERR_RETURN;

        case STMT_SWAP: {
            if (lvalue_is_const(env, s->swap_stmt.lhs))
                return ERR_CONST_REASSIGN;
            if (lvalue_is_const(env, s->swap_stmt.rhs))
                return ERR_CONST_REASSIGN;
            value_t *v1, *v2;
            error_t e1 = resolve_lvalue(env, s->swap_stmt.lhs, &v1);
            if (e1 != ERR_NONE) return e1;
            error_t e2 = resolve_lvalue(env, s->swap_stmt.rhs, &v2);
            if (e2 != ERR_NONE) return e2;
            value_t tmp = *v1;
            *v1 = *v2;
            *v2 = tmp;
            /* (a fixed-length string gets the other's value cut or padded) */
            EVAL_CHECK(fix_string(v1, lvalue_fixlen(env, s->swap_stmt.lhs)));
            return fix_string(v2, lvalue_fixlen(env, s->swap_stmt.rhs));
        }

        case STMT_RANDOMIZE: {
            double seed = 0.0;
            if (s->randomize.seed) {
                value_t v;
                EVAL_CHECK(eval_expr(env, s->randomize.seed, &v));
                error_t cerr = val_to_double(&v, &seed);
                val_clear(&v);
                if (cerr != ERR_NONE) return cerr;
            }
            return eval_randomize(s->randomize.seed != NULL, seed);
        }

        case STMT_SLEEP: {
            if (s->sleep_stmt.duration) {
                value_t v;
                EVAL_CHECK(eval_expr(env, s->sleep_stmt.duration, &v));
                error_t err = eval_sleep(&v);
                val_clear(&v);
                return err;
            }
            return eval_sleep(NULL);
        }

        case STMT_OPEN:         return exec_open(env, s);
        case STMT_CLOSE:        return exec_close(env, s);
        case STMT_PRINT_FILE:   return exec_print_file(env, s);
        case STMT_WRITE_FILE:   return exec_write_file(env, s);
        case STMT_INPUT_FILE:   return exec_input_file(env, s);
        case STMT_LINE_INPUT:   return exec_line_input(env, s);
        case STMT_LINE_INPUT_CONSOLE: return exec_line_input_console(env, s);
        case STMT_WRITE_CONSOLE: return exec_write_console(env, s);
        case STMT_DEF_FN: {
            /* The scan registered it (a DEF FN is defined before the
             * program runs: a use before the DEF is fine, as s32-bc has it
             * -- the user's ruling, QuickBASIC refusing it).  Met again,
             * nothing: it used to register once more each time it ran, a
             * DEF in a loop filling the table. */
            proc_entry_t *p = find_proc(s->def_fn.name);
            if (p && p->def == s) return ERR_NONE;
            /* Register DEF FN in proc table (whether name(args) is an
             * array depends on it: a cached array is stale) */
            array_generation++;
            return proc_register(s->def_fn.name, s);
        }

        case STMT_SHELL: {
            value_t cmd;
            EVAL_CHECK(eval_expr(env, s->shell_stmt.command, &cmd));
            if (cmd.type != VAL_STRING) { val_clear(&cmd); return ERR_TYPE_MISMATCH; }
            col_puts("SHELL not supported: ");
            col_puts(cmd.sval ? cmd.sval->data : "");
            col_puts("\n");
            val_clear(&cmd);
            return ERR_NONE;
        }

        case STMT_CHDIR:
        case STMT_MKDIR:
        case STMT_RMDIR: {
            value_t path;
            EVAL_CHECK(eval_expr(env, s->shell_stmt.command, &path));
            error_t err = eval_dir(s->type, &path);
            val_clear(&path);
            return err;
        }

        case STMT_POKE: {
            extern int def_seg;
            value_t av, vv;
            EVAL_CHECK(eval_expr(env, s->poke_stmt.addr, &av));
            error_t err = eval_expr(env, s->poke_stmt.value, &vv);
            if (err != ERR_NONE) { val_clear(&av); return err; }
            int offset, val;
            err = val_to_integer(&av, &offset);
            if (err == ERR_NONE) err = val_to_integer(&vv, &val);
            val_clear(&av); val_clear(&vv);
            if (err != ERR_NONE) return err;
            return eval_poke(offset, val);
        }

        case STMT_DEF_SEG: {
            extern int def_seg;
            if (s->shell_stmt.command) {
                value_t v;
                EVAL_CHECK(eval_expr(env, s->shell_stmt.command, &v));
                int seg;
                error_t err = val_to_integer(&v, &seg);
                val_clear(&v);
                if (err != ERR_NONE) return err;
                def_seg = seg;
            } else {
                def_seg = 0;
            }
            return ERR_NONE;
        }

        case STMT_OUT:
        case STMT_WAIT: {               /* the port, then the value(s), made integers */
            int port, a, b = 0;
            EVAL_CHECK(eval_int_of(env, s->port_stmt.port, &port));
            EVAL_CHECK(eval_int_of(env, s->port_stmt.a, &a));
            if (s->port_stmt.b) EVAL_CHECK(eval_int_of(env, s->port_stmt.b, &b));
            return s->type == STMT_OUT ? pc_out(port, a) : pc_wait(port, a, b);
        }

        case STMT_VIEW_PRINT: {         /* VIEW PRINT [top TO bottom] */
            int top = 0, bottom = 0;
            if (s->width_stmt.columns) {
                EVAL_CHECK(eval_int_of(env, s->width_stmt.columns, &top));
                EVAL_CHECK(eval_int_of(env, s->width_stmt.rows, &bottom));
            }
            return eval_view_print(s->width_stmt.columns != NULL, top, bottom);
        }

        case STMT_DRAW: {
            value_t v;
            EVAL_CHECK(eval_expr(env, s->shell_stmt.command, &v));
            error_t err = eval_draw(&v);
            val_clear(&v);
            return err;
        }

        case STMT_SCREEN: {
            int mode;
            if (!s->shell_stmt.command) return ERR_NONE;    /* (SCREEN , , 3, 2: pages only) */
            EVAL_CHECK(gfx_eval_int(env, s->shell_stmt.command, &mode));
            return eval_screen(mode);
        }

        case STMT_LSET:
        case STMT_RSET: {
            /* LSET/RSET var$ = expr$: pad/truncate to var's current length */
            value_t *cur = env_get(env, s->lrset.varname);
            if (!cur || cur->type != VAL_STRING) return ERR_TYPE_MISMATCH;
            int curlen = cur->sval ? cur->sval->len : 0;
            if (curlen == 0) return ERR_NONE; /* zero-length: nothing to do */
            value_t nv, result;
            EVAL_CHECK(eval_expr(env, s->lrset.value, &nv));
            error_t err = eval_lrset(curlen, &nv, s->type == STMT_LSET, &result);
            val_clear(&nv);
            if (err != ERR_NONE) return err;
            env_set(env, s->lrset.varname, &result);
            val_clear(&result);
            return ERR_NONE;
        }

        case STMT_WIDTH: {
            int w, rows;
            if (s->width_stmt.columns) EVAL_CHECK(eval_int_of(env, s->width_stmt.columns, &w));
            if (s->width_stmt.rows) EVAL_CHECK(eval_int_of(env, s->width_stmt.rows, &rows));
            if (s->width_stmt.columns) EVAL_CHECK(eval_width(w));
            return s->width_stmt.rows ? eval_width_rows(rows) : ERR_NONE;
        }

        case STMT_NOOP:
            return ERR_NONE;

        case STMT_PSET:
        case STMT_LINE_G:
        case STMT_CIRCLE:
        case STMT_PAINT:
        case STMT_PALETTE:
        case STMT_VIEW:
        case STMT_WINDOW:
            return exec_gfx(env, s);

        case STMT_GFX_GET:
        case STMT_GFX_PUT:
            return exec_gfx_getput(env, s);

        case STMT_CLEAR:
            env_clear(env);
            array_clear_all();
            return ERR_NONE;

        case STMT_KILL:         return exec_kill(env, s);
        case STMT_NAME:         return exec_name(env, s);
        case STMT_GET:          return exec_get(env, s);
        case STMT_PUT:          return exec_put(env, s);
        case STMT_SEEK:         return exec_seek(env, s);

        case STMT_TYPE_DEF:     return exec_type_def(s);
        case STMT_DIM_AS_TYPE:  return exec_dim_as_type(env, s);
        case STMT_FIELD_ASSIGN: return exec_field_assign(env, s);
        case STMT_MID_ASSIGN:   return exec_mid_assign(env, s);

        case STMT_ON_ERROR: {
            /* (in the handler, QBasic: GOTO 0 stops the program with the
             * error being handled; another label is the handler from now,
             * this one still RESUMEs) */
            if (s->on_error.label[0] == '\0') {
                /* ON ERROR GOTO 0: disable error trapping */
                on_error_label_idx = -1;
                if (on_error_active) { error_raised_code = on_error_err_code; return ERR_ON_ERROR_GOTO; }
            } else {
                int idx = main_code ? code_find_label(main_code, s->on_error.label) : -1;
                if (idx < 0) return ERR_UNDEFINED_LABEL;
                on_error_label_idx = idx;
            }
            return ERR_NONE;
        }

        case STMT_RESUME:
            resume_is_next = s->resume_stmt.resume_next;
            if (resume_is_next == 2) {
                strncpy(resume_label, s->resume_stmt.label, 63);
                resume_label[63] = '\0';
            }
            return ERR_RESUME_WITHOUT_ERROR; /* handled in eval_program loop */

        case STMT_ERROR_RAISE: {
            value_t v;
            EVAL_CHECK(eval_expr(env, s->error_raise.errnum, &v));
            int code;
            error_t cerr = val_to_integer(&v, &code);
            val_clear(&v);
            if (cerr != ERR_NONE) return cerr;
            on_error_err_code = code;
            error_raised_code = code;
            on_error_err_line = cur_basic_line;
            return ERR_ON_ERROR_GOTO;
        }

        case STMT_DEFTYPE:
            exec_deftype(s);
            return ERR_NONE;

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

        case STMT_BEEP:
            return ERR_NONE;

        case STMT_OPTION_EXPLICIT:
            option_explicit = 1;
            return ERR_NONE;

        case STMT_DIM_SCALAR: {
            /* Declare scalar variable with default value -- once: DIM is
             * a declaration (QuickBASIC: one in a loop does not reset it) */
            if (env_lookup(env, s->dim_scalar.name)) return ERR_NONE;
            value_t def = s->dim_scalar.fixlen ? fixed_value(s->dim_scalar.fixlen)
                        : val_default(s->dim_scalar.typed ? s->dim_scalar.scalar_type
                                                          : resolve_var_type(s->dim_scalar.name));
            env_set(env, s->dim_scalar.name, &def);
            val_clear(&def);
            if (s->dim_scalar.fixlen) {
                var_entry_t *e = env_entry(env, s->dim_scalar.name);
                if (e) e->fixlen = s->dim_scalar.fixlen;
            }
            return ERR_NONE;
        }

        case STMT_TRACE:
            trace_enabled = s->trace.enabled;
            return ERR_NONE;

        case STMT_SPLIT: {
            /* SPLIT str$, delim$, array$() */
            value_t sv, dv;
            EVAL_CHECK(eval_expr(env, s->split.str_expr, &sv));
            error_t err2 = eval_expr(env, s->split.delim_expr, &dv);
            if (err2 != ERR_NONE) { val_clear(&sv); return err2; }
            const char *str = (sv.type == VAL_STRING && sv.sval) ? sv.sval->data : "";
            const char *delim = (dv.type == VAL_STRING && dv.sval) ? dv.sval->data : "";
            int dlen = (int)strlen(delim);

            /* Count pieces */
            int count = 1;
            if (dlen > 0) {
                const char *p = str;
                while ((p = strstr(p, delim)) != NULL) { count++; p += dlen; }
            }

            /* REDIM the target array */
            int sizes[1] = { count }, lb[1] = { option_base };
            array_erase(s->split.array_name);
            error_t aerr = array_dim(s->split.array_name, VAL_STRING,
                                      sizes, 1, lb);
            if (aerr != ERR_NONE) { val_clear(&sv); val_clear(&dv); return aerr; }

            /* Split and store */
            const char *p = str;
            for (int i = 0; i < count; i++) {
                const char *next = (dlen > 0) ? strstr(p, delim) : NULL;
                int plen = next ? (int)(next - p) : (int)strlen(p);
                if (plen > MAX_STRING_LEN) plen = MAX_STRING_LEN;
                value_t piece = val_string(p, plen);
                int idx[1] = { i + option_base };
                array_set(s->split.array_name, idx, 1, &piece);
                val_clear(&piece);
                p = next ? next + dlen : p + plen;
            }
            val_clear(&sv);
            val_clear(&dv);
            return ERR_NONE;
        }
    }

    return ERR_INTERNAL;
}

error_t eval_stmts(env_t *env, stmt_t *stmts) {
    error_t err;
    code_t *c = lower_stmts(stmts, &err);
    if (!c) return err;
    err = run_code(env, c, 0);
    code_free(c);
    return err;
}

/* --- Program execution with GOTO/GOSUB/SUB/FUNCTION support --- */

/* a DATA item, the next of the program's: the interpreter's scan, and
 * compiled code's (s32-bc) as it starts */
error_t eval_data_add(const value_t *v, int quoted) {
    if (data_pool_count == data_pool_cap) {
        int cap = data_pool_cap ? data_pool_cap * 2 : 1024;
        value_t *t = realloc(data_pool, cap * sizeof(*t));
        if (!t) return ERR_OUT_OF_MEMORY;
        data_pool = t;
        unsigned char *q = realloc(data_quoted, cap);
        if (!q) return ERR_OUT_OF_MEMORY;
        data_quoted = q;
        data_pool_cap = cap;
    }
    data_quoted[data_pool_count] = (unsigned char)quoted;
    data_pool[data_pool_count++] = val_copy(v);
    return ERR_NONE;
}

static void data_pool_clear(void) {
    for (int i = 0; i < data_pool_count; i++)
        val_clear(&data_pool[i]);
    data_pool_count = 0;
    data_read_ptr = 0;
}

/* Recursively scan a statement list for labels (RESTORE's DATA offsets),
 * DATA, and SUB/FUNCTION defs. */
static error_t scan_stmt_list(stmt_t *s);

/* DIM with constant bounds in the main program: QuickBASIC makes such an
 * array when it compiles, so it exists from the start and a reference
 * may come before the DIM.  OPTION BASE counts in source order. */
/* a bound written as a number (a static DIM's): its value */
static int literal_int(const expr_t *d, int *out) {
    return d && d->type == EXPR_LITERAL && d->literal.type != VAL_STRING &&
           val_to_integer(&d->literal, out) == ERR_NONE;
}

static void scan_static_dim(stmt_t *s) {
    int sizes[MAX_ARRAY_DIMS], lowers[MAX_ARRAY_DIMS];
    s->dim_stmt.is_static = 0;
    if (s->dim_stmt.is_redim) return;
    for (int i = 0; i < s->dim_stmt.ndims; i++) {
        int upper;
        lowers[i] = option_base;
        if (s->dim_stmt.lower[i] && !literal_int(s->dim_stmt.lower[i], &lowers[i])) return;
        if (!literal_int(s->dim_stmt.dims[i], &upper)) return;
        sizes[i] = upper - lowers[i] + 1;
        if (sizes[i] <= 0) return;
    }
    if (array_find(s->dim_stmt.name)) return;
    if (array_dim(s->dim_stmt.name, dim_type(s), sizes,
                  s->dim_stmt.ndims, lowers) == ERR_NONE)
        s->dim_stmt.is_static = 1;
}

static error_t scan_one_stmt(stmt_t *s) {
    if (s->type == STMT_OPTION_BASE)
        option_base = s->option_base.base;
    if (s->type == STMT_DEFTYPE)        /* (in the text's order, as QuickBASIC reads them, for static DIMs) */
        exec_deftype(s);
    if (s->type == STMT_DIM) {
        if (s->dim_stmt.shared) dim_shared_add(s->dim_stmt.name, 1);
        scan_static_dim(s);
    }
    if (s->type == STMT_DIM_SCALAR && s->dim_scalar.shared)
        dim_shared_add(s->dim_scalar.name, 0);
    if (s->type == STMT_DIM_AS_TYPE && s->dim_as_type.shared)
        dim_shared_add(s->dim_as_type.name, 0);
    if (s->type == STMT_CONST)          /* (a module CONST: every procedure's; the scan is the main program's) */
        dim_shared_add(s->const_stmt.name, 0);
    if (s->type == STMT_LABEL) {
        if (label_count == label_cap) {
            int cap = label_cap ? label_cap * 2 : 256;
            label_entry_t *t = realloc(label_table, cap * sizeof(*t));
            if (!t) return ERR_OUT_OF_MEMORY;
            label_table = t;
            label_cap = cap;
        }
        ast_name_copy(label_table[label_count].name, s->label.name);
        label_table[label_count].data_offset = data_pool_count;
        label_count++;
    }
    if (s->type == STMT_DEF_FN && !find_proc(s->def_fn.name)) {
        error_t perr = proc_register(s->def_fn.name, s);
        if (perr != ERR_NONE) return perr;
    }
    if (s->type == STMT_DATA)
        for (int j = 0; j < s->data_stmt.nvalues; j++)
            EVAL_CHECK(eval_data_add(&s->data_stmt.values[j], s->data_stmt.quoted[j]));
    if (s->type == STMT_SUB_DEF || s->type == STMT_FUNC_DEF) {
        error_t perr = proc_register(s->proc_def.name, s);
        if (perr != ERR_NONE) return perr;
    }
    /* Recurse into block bodies */
    error_t err;
    switch (s->type) {
        case STMT_IF:
            err = scan_stmt_list(s->if_stmt.then_body);
            if (err != ERR_NONE) return err;
            if (s->if_stmt.else_body) {
                err = scan_stmt_list(s->if_stmt.else_body);
                if (err != ERR_NONE) return err;
            }
            break;
        case STMT_FOR:
            err = scan_stmt_list(s->for_stmt.body);
            if (err != ERR_NONE) return err;
            break;
        case STMT_WHILE:
            err = scan_stmt_list(s->while_stmt.body);
            if (err != ERR_NONE) return err;
            break;
        case STMT_DO_LOOP:
            err = scan_stmt_list(s->do_loop.body);
            if (err != ERR_NONE) return err;
            break;
        case STMT_SELECT:
            for (case_clause_t *c = s->select_stmt.clauses; c; c = c->next) {
                err = scan_stmt_list(c->body);
                if (err != ERR_NONE) return err;
            }
            break;
        default:
            break;
    }
    return ERR_NONE;
}

static error_t scan_stmt_list(stmt_t *s) {
    for (; s; s = s->next) {
        error_t err = scan_one_stmt(s);
        if (err != ERR_NONE) return err;
    }
    return ERR_NONE;
}

error_t act_begin(act_t *a, env_t *env, code_t *c, int is_main) {
    if (++eval_depth > MAX_EVAL_DEPTH || stack_exhausted()) {
        eval_depth--;
        return ERR_STACK_OVERFLOW;
    }
    a->env = env;
    a->c = c;
    a->is_main = is_main;
    a->saved_code = cur_code;
    a->fors = NULL;
    a->nfors = a->cap_fors = 0;
    a->gosub_base = gosub_top;
    cur_code = c;
    if (is_main) main_act = a;
    return ERR_NONE;
}

/* a native procedure's activation (s32-sbc): the depth and the stack,
 * as act_begin counts them */
error_t eval_enter(void) {
    if (++eval_depth > MAX_EVAL_DEPTH || stack_exhausted()) {
        eval_depth--;
        return ERR_STACK_OVERFLOW;
    }
    return ERR_NONE;
}

void eval_leave(void) {
    eval_depth--;
}

/* none of these names is read as anything but its variable: not a
 * builtin's, not ERR or ERL (a native procedure keeps its variables in
 * C, where a builtin's name would be a variable) */
int eval_names_plain(const char *const *names, int n) {
    for (int i = 0; i < n; i++)
        if (builtin_exists(names[i]) || strcmp(names[i], "ERR") == 0 ||
            strcmp(names[i], "ERL") == 0 || strncmp(names[i], "FN", 2) == 0)
            return 0;
    return 1;
}

void act_end(act_t *a) {
    if (a == main_act) main_act = NULL;
    if (gosub_top > a->gosub_base) gosub_top = a->gosub_base;
    free(a->fors);
    cur_code = a->saved_code;
    eval_depth--;
}

/* one op; inlined into the executor's loop, called by compiled code */
/* A plain RETURN (no label), when a GOSUB of this activation is open:
 * 1, *pc the op after it, as act_transfer returns; 0 when it is not
 * (RETURN without GOSUB is the executor's to report) */
int act_return(act_t *a, int *pc) {
    if (gosub_top <= a->gosub_base) return 0;
    *pc = gosub_stack[--gosub_top];
    return 1;
}

/* A FOR reached, its counter set: its own loop's frame, if it is
 * running (entered again by GOTO), and every loop above that, dropped;
 * a new frame on top when the loop runs */
error_t act_for_enter(act_t *a, stmt_t *s, int runs, double limit, double step,
                      int body_pc, value_t *var) {
    for (int k = a->nfors - 1; k >= 0; k--)
        if (a->fors[k].s == s) { a->nfors = k; break; }
    if (!runs) return ERR_NONE;
    if (a->nfors == a->cap_fors) {
        int cap = a->cap_fors ? a->cap_fors * 2 : 8;
        for_slot_t *t = realloc(a->fors, cap * sizeof(*t));
        if (!t) return ERR_OUT_OF_MEMORY;
        a->fors = t;
        a->cap_fors = cap;
    }
    for_slot_t *f = &a->fors[a->nfors++];
    f->s = s;
    f->limit = limit;
    f->step = step;
    f->body_pc = body_pc;
    f->var = var;               /* the counter's value, as of now */
    f->gen = env_generation;
    return ERR_NONE;
}

static inline __attribute__((always_inline)) error_t act_step_in(act_t *a, int *ppc) {
    env_t *env = a->env;
    int pc = *ppc;
    op_t *op = &a->c->ops[pc];
    error_t err;
    int yes;
    switch (op->kind) {
        case OP_JUMP:
            *ppc = op->target;
            return ERR_NONE;
        case OP_JFALSE: {
            value_t cond;
            EVAL_CHECK(eval_expr(env, op->cond, &cond));
            yes = val_is_true(&cond);
            val_clear(&cond);
            *ppc = (yes != op->invert) ? pc + 1 : op->target;
            return ERR_NONE;
        }
        case OP_FOR: {
            for_slot_t f;
            EVAL_CHECK(for_start(env, op->s, &f, &yes));
            EVAL_CHECK(act_for_enter(a, op->s, yes, f.limit, f.step, pc + 1, NULL));
            if (!yes) {
                if (op->target < 0) return ERR_FOR_WITHOUT_NEXT;
                *ppc = op->target;
                return ERR_NONE;
            }
            *ppc = pc + 1;
            return ERR_NONE;
        }
        case OP_NEXT: {
            int k = a->nfors - 1;
            if (!op->invert)
                while (k >= 0 && a->fors[k].s != op->s &&
                       strcmp(a->fors[k].s->for_stmt.var_name,
                              op->s->for_stmt.var_name) != 0)
                    k--;
            if (k < 0) {
                /* no loop running (a GOTO into its body, a FOR that
                 * failed and was resumed past): QBasic's NEXT then has
                 * its FOR's limit as never set, 0, and its step as the
                 * FOR states it -- 1 without STEP, a constant one as
                 * written (compiled into the NEXT), else 0 (NBS P055;
                 * a FOR whose start overflowed, checked) */
                double zstep = 0.0;
                const expr_t *se = op->s->for_stmt.step;
                if (!se) zstep = 1.0;
                else if (se->type == EXPR_LITERAL && se->literal.type != VAL_STRING) {
                    value_t sv = se->literal;
                    val_to_double(&sv, &zstep);
                }
                for_slot_t z = { op->s, 0.0, zstep, op->slot + 1, NULL, 0 };
                EVAL_CHECK(for_next(env, op->s, &z, &yes));
                *ppc = yes ? z.body_pc : pc + 1;
                return ERR_NONE;
            }
            a->nfors = k + 1;
            err = for_next(env, a->fors[k].s, &a->fors[k], &yes);
            if (err != ERR_NONE) {
                /* the loop is over (RESUME NEXT goes past its NEXT): its
                 * frame must not be found by a later NEXT of the name */
                a->nfors = k;
                return err;
            }
            if (yes) {
                *ppc = a->fors[k].body_pc;
            } else {
                a->nfors = k;
                *ppc = pc + 1;
            }
            return ERR_NONE;
        }
        case OP_EXITFOR:
            /* its own loop's frame, if that is the one on top (a FOR
             * that failed has none, and an enclosing loop's stays) */
            if (a->nfors > 0 && a->fors[a->nfors - 1].s == a->c->ops[op->slot].s)
                a->nfors--;
            *ppc = op->target;
            return ERR_NONE;
        case OP_SELECT:
            EVAL_CHECK(select_match(env, op->s, &yes));
            *ppc = op->targets[yes];
            return ERR_NONE;
        default:
            cur_basic_line = op->basic_line;
            err = eval_stmt(env, op->s);
            if (err == ERR_NONE) *ppc = pc + 1;
            return err;
    }
}

error_t act_step(act_t *a, int *ppc) {
    return act_step_in(a, ppc);
}

error_t act_gosub(act_t *a, int return_pc) {
    (void)a;
    if (gosub_top >= MAX_GOSUB_DEPTH) return ERR_STACK_OVERFLOW;
    gosub_stack[gosub_top++] = return_pc;
    return ERR_NONE;
}

/* The handler, for an error in a procedure: the main program's code
 * from the handler's label, run here -- the procedure's activation still
 * under it -- until a RESUME (*how: 0 the statement again, 1 NEXT, 2 a
 * label), as QuickBASIC does it: RESUME and RESUME NEXT return into the
 * procedure.  Whatever else stops it (END, an error in the handler) is
 * returned. */
static error_t run_handler(int *how) {
    act_t *m = main_act;
    int pc = on_error_label_idx;
    error_t result = ERR_NONE;
    while (pc < m->c->nops) {
        error_t err = act_step_in(m, &pc);
        if (err == ERR_NONE) continue;
        if (err == ERR_RESUME_WITHOUT_ERROR && on_error_active) {
            on_error_active = 0;
            on_error_err_code = 0;      /* (ERR is 0 after a RESUME, QBasic) */
            *how = resume_is_next;
            return ERR_NONE;
        }
        if (!act_transfer(m, err, &pc, &result)) return result;
    }
    return ERR_EXIT;                    /* (off the end of the program: it ends) */
}

int act_transfer(act_t *a, error_t err, int *ppc, error_t *result) {
    int pc = *ppc;
    op_t *op = &a->c->ops[pc];
    if (err == ERR_GOTO) {
        *ppc = goto_target;
        return 1;
    }
    if (err == ERR_GOSUB) {
        if (gosub_top >= MAX_GOSUB_DEPTH) { *result = ERR_STACK_OVERFLOW; return 0; }
        gosub_stack[gosub_top++] = pc + 1;
        *ppc = goto_target;
        return 1;
    }
    if (err == ERR_RETURN) {
        if (gosub_top <= a->gosub_base) { *result = ERR_RETURN_WITHOUT_GOSUB; return 0; }
        gosub_top--;
        if (return_label[0]) {
            /* RETURN label -- to the label instead of the caller */
            int idx = find_label(return_label);
            return_label[0] = '\0';
            if (idx < 0) { *result = ERR_UNDEFINED_LABEL; return 0; }
            *ppc = idx;
        } else {
            *ppc = gosub_stack[gosub_top];
        }
        return 1;
    }
    /* (out of stack in a procedure: QBasic does not trap it -- the
     * handler would need the stack -- and stops) */
    if (!a->is_main && err == ERR_STACK_OVERFLOW) on_error_label_idx = -1;
    /* an error in a procedure, a handler set: the handler runs now */
    if (!a->is_main && main_act && on_error_label_idx >= 0 && !on_error_active &&
        (err == ERR_ON_ERROR_GOTO || is_trappable_error(err))) {
        int how = 0;
        error_t r;
        if (err != ERR_ON_ERROR_GOTO) {
            on_error_err_code = error_qb_code(err);
            on_error_err_line = op->basic_line;
        }
        on_error_active = 1;
        r = run_handler(&how);
        if (r != ERR_NONE) { *result = r; return 0; }
        if (how == 2) { *result = ERR_RESUME_LABEL; return 0; }   /* (out to the main program) */
        *ppc = how ? pc + 1 : pc;
        return 1;
    }
    if (err == ERR_RESUME_LABEL && a->is_main) {    /* RESUME label, from a procedure */
        int idx = find_label(resume_label);
        if (idx < 0) { *result = ERR_UNDEFINED_LABEL; return 0; }
        *ppc = idx;
        return 1;
    }
    if (!a->is_main || err == ERR_EXIT) {
        *result = err;
        return 0;
    }

    /* RESUME / RESUME NEXT / RESUME label (from error handler) */
    if (err == ERR_RESUME_WITHOUT_ERROR && on_error_active) {
        on_error_active = 0;
        on_error_err_code = 0;          /* (ERR is 0 after a RESUME, QBasic) */
        if (resume_is_next == 1)
            *ppc = on_error_resume_next_pc;
        else if (resume_is_next == 2) {
            int idx = find_label(resume_label);
            if (idx < 0) { *result = ERR_UNDEFINED_LABEL; return 0; }
            *ppc = idx;
        } else
            *ppc = on_error_resume_pc;
        return 1;
    }

    /* ON ERROR GOTO triggered by ERROR n statement */
    if (err == ERR_ON_ERROR_GOTO && on_error_label_idx >= 0 &&
        !on_error_active) {
        on_error_active = 1;
        on_error_resume_pc = pc;
        on_error_resume_next_pc = pc + 1;
        *ppc = on_error_label_idx;
        return 1;
    }

    /* Error trapping: if ON ERROR GOTO is active and this is a
       trappable error, jump to the handler */
    if (on_error_label_idx >= 0 && !on_error_active &&
        is_trappable_error(err)) {
        on_error_err_code = error_qb_code(err);
        on_error_err_line = op->basic_line;
        on_error_active = 1;
        on_error_resume_pc = pc;
        on_error_resume_next_pc = pc + 1;
        *ppc = on_error_label_idx;
        return 1;
    }

    *result = err;
    return 0;
}

/* Run a unit's code: each op by act_step, the transfers of control --
 * GOTO, GOSUB, RETURN, ON ... GOTO/GOSUB -- and errors by act_transfer.
 * Only the main program traps errors (ON ERROR's handler is there); an
 * error in a procedure comes out of its CALL, and RESUME returns to
 * that.  A compiled program (s32-sbc) calls the same two for what it
 * does not do itself. */
static error_t run_code(env_t *env, code_t *c, int is_main) {
    act_t a;
    error_t result = act_begin(&a, env, c, is_main);
    int pc = 0;
    if (result != ERR_NONE) return result;
    while (pc < c->nops) {
        error_t err = act_step_in(&a, &pc);
        if (err != ERR_NONE && !act_transfer(&a, err, &pc, &result))
            break;
    }
    act_end(&a);
    return result;
}

error_t eval_program(env_t *env, stmt_t *program) {
    return eval_program_with(env, program, NULL);
}

/* A program's state as it starts: the interpreter's, and compiled
 * code's (s32-bc's runtime calls it) */
void eval_reset(void) {
    option_base = 0;
    builtin_rnd_reset();
    array_clear_all();
    data_pool_clear();
    fileio_init();
    sb_term_init();
    deftype_init();
    type_def_count = 0;
    print_col = 0;
    print_row = 1;
    scr_rows = 25;                      /* (the text screen: 25 rows, 1 to 24 scroll) */
    view_top = 1;
    view_bottom = 24;
    scr_clear_rows(1, SCR_ROWS);
    pc_reset();
    eval_depth = 0;
    on_error_label_idx = -1;
    on_error_active = 0;
    on_error_err_code = 0;
    on_error_err_line = 0;
    resume_is_next = 0;
    resume_label[0] = '\0';
    return_label[0] = '\0';
    trace_enabled = 0;
    option_explicit = 0;
}

error_t eval_program_with(env_t *env, stmt_t *program, main_fn_t main_fn) {
    int i;
    char base;
    stack_base = &base;
    if (!program) return ERR_NONE;
again:                                  /* (RUN: all of it again, the variables cleared) */

    /* Initialize program state */
    program_global_env = env;
    eval_reset();

    error_t result = ERR_NONE;

    /* First pass: collect labels, proc definitions, and DATA values */
    proc_count = 0;
    memset(proc_hash, 0, sizeof proc_hash);
    memset(proc_alias_hash, 0, sizeof proc_alias_hash);
    proc_naliases = 0;
    label_count = 0;

    ndim_shared = 0;
    result = scan_stmt_list(program);
    deftype_init();             /* (the scan's DEFtypes were for its static DIMs: the run makes its own) */
    if (result != ERR_NONE)
        goto cleanup;

    main_code = lower_stmts(program, &result);
    if (main_code) {
        gosub_top = 0;
        result = main_fn ? main_fn(env, main_code) : run_code(env, main_code, 1);
        if (result == ERR_EXIT) result = ERR_NONE;
    }

cleanup:
    /* Clean up */
    fileio_close_all();
    data_pool_clear();
    array_clear_all();
    code_free(main_code);
    main_code = NULL;
    for (i = 0; i < proc_count; i++) {
        code_free(proc_table[i].code);
        proc_table[i].code = NULL;
        free(proc_table[i].shared_names);
        free(proc_table[i].static_names);
        proc_table[i].shared_names = proc_table[i].static_names = NULL;
        proc_table[i].prepared = 0;
        if (proc_table[i].static_env) {
            env_destroy(proc_table[i].static_env);
            proc_table[i].static_env = NULL;
        }
    }
    if (result == ERR_RUN) {
        env_clear(env);
        goto again;
    }
    sb_term_shutdown();
    return result;
}

error_t eval_unary(unaryop_t op, value_t *operand, value_t *out) {
    return eval_unary_in(op, operand, out);
}

error_t eval_binary(binop_t op, value_t *left, value_t *right, value_t *out) {
    return eval_binary_in(op, left, right, out);
}

error_t eval_compare(cmpop_t op, value_t *left, value_t *right, value_t *out) {
    return eval_compare_in(op, left, right, out);
}
