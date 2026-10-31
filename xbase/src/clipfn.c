/* The rest of Clipper 5.2's functions (clipfn.h).  Where a dBASE command
 * already does the work, the function builds the command line and runs
 * it, so the two forms cannot drift apart; the queries read the work
 * area directly. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <unistd.h>
#include "clipfn.h"
#include "ast.h"
#include "func.h"
#include "area.h"
#include "command.h"
#include "program.h"
#include "screen.h"
#include "getsys.h"
#include "memoedit.h"
#include "keys.h"
#include "memvar.h"
#include "util.h"
#include "date.h"

static value_t vnum(double n) {
    return val_num_dec(n, 0, 0);
}

static int arg_num(value_t *args, int nargs, int i, int dflt) {
    return i < nargs && args[i].type == VAL_NUM ? (int)args[i].num : dflt;
}

static const char *arg_str(value_t *args, int nargs, int i, const char *dflt) {
    return i < nargs && args[i].type == VAL_CHAR ? args[i].str : dflt;
}

static int arg_logic(value_t *args, int nargs, int i, int dflt) {
    return i < nargs && args[i].type == VAL_LOGIC ? args[i].logic : dflt;
}

/* A command line run as the interpreter runs one */
static int run_cmd(const char *fmt, ...) {
    char line[1024];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(line, sizeof(line), fmt, ap);
    va_end(ap);
    return cmd_execute(NULL, line);
}

static int nil_result(value_t *result) {
    *result = val_nil();
    return 0;
}

/* The work area an alias or number names; the current one when none */
static work_area_t *area_arg(value_t *args, int nargs, int i) {
    if (i < nargs && args[i].type == VAL_CHAR) {
        int idx = area_resolve_alias(args[i].str);
        return idx >= 0 ? area_get(idx) : NULL;
    }
    if (i < nargs && args[i].type == VAL_NUM) {
        int idx = (int)args[i].num - 1;
        return idx >= 0 && idx < MAX_AREAS ? area_get(idx) : NULL;
    }
    return area_get_current();
}

static dbf_t *cur_db_open(expr_ctx_t *ctx) {
    work_area_t *wa = area_get_current();
    if (!wa || !dbf_is_open(&wa->db)) {
        ctx->error = "No database in use";
        return NULL;
    }
    return &wa->db;
}

/* ---- the DB*() forms ---- */

static int fn_dbappend(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    cmd_api_append_blank();
    return nil_result(result);
}

static int fn_dbclearfil(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    run_cmd("SET FILTER TO");
    return nil_result(result);
}

static int fn_dbclearind(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    run_cmd("SET INDEX TO");
    return nil_result(result);
}

static int fn_dbclearrel(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    run_cmd("SET RELATION TO");
    return nil_result(result);
}

static int fn_dbcloseall(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    run_cmd("CLOSE DATABASES");
    return nil_result(result);
}

static int fn_dbclosearea(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    run_cmd("USE");
    return nil_result(result);
}

static int fn_dbcommit(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    work_area_t *wa = area_get_current();
    (void)ctx; (void)args; (void)nargs;
    if (wa && dbf_is_open(&wa->db)) {
        dbf_flush_record(&wa->db);
        dbf_write_header_counts(&wa->db);
        fflush(wa->db.fp);
    }
    return nil_result(result);
}

static int fn_dbcommitall(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int i;
    (void)ctx; (void)args; (void)nargs;
    for (i = 0; i < MAX_AREAS; i++) {
        work_area_t *wa = area_get(i);
        if (wa && dbf_is_open(&wa->db)) {
            dbf_flush_record(&wa->db);
            dbf_write_header_counts(&wa->db);
            fflush(wa->db.fp);
        }
    }
    return nil_result(result);
}

/* DBCREATE(file, {{name, type, len, dec}, ...}) */
static int fn_dbcreate(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    dbf_field_t fields[DBF_MAX_FIELDS];
    array_t *a;
    char name[300];
    int i, n = 0;
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_ARRAY || !args[1].array) {
        ctx->error = "DBCREATE requires (filename, structure array)";
        return -1;
    }
    a = args[1].array;
    for (i = 0; i < a->len && n < DBF_MAX_FIELDS; i++) {
        array_t *f = a->el[i].type == VAL_ARRAY ? a->el[i].array : NULL;
        if (!f || f->len < 2 || f->el[0].type != VAL_CHAR || f->el[1].type != VAL_CHAR) {
            ctx->error = "DBCREATE: each field is {name, type, length, decimals}";
            return -1;
        }
        memset(&fields[n], 0, sizeof(fields[n]));
        snprintf(fields[n].name, sizeof(fields[n].name), "%s", f->el[0].str);
        str_upper(fields[n].name);
        trim_right(fields[n].name);
        fields[n].type = (char)toupper((unsigned char)f->el[1].str[0]);
        fields[n].length = (uint8_t)(f->len > 2 && f->el[2].type == VAL_NUM ? f->el[2].num : 10);
        fields[n].decimals = (uint8_t)(f->len > 3 && f->el[3].type == VAL_NUM ? f->el[3].num : 0);
        if (fields[n].type == 'D') fields[n].length = 8;
        if (fields[n].type == 'L') fields[n].length = 1;
        if (fields[n].type == 'M') fields[n].length = 10;
        n++;
    }
    snprintf(name, sizeof(name), "%s", args[0].str);
    if (!strchr(name, '.')) strcat(name, ".DBF");
    if (cmd_create_table(name, fields, n) != 0)
        return nil_result(result);   /* the create error raised, or defaulted */
    for (i = 0; i < n; i++) {
        if (fields[i].type == 'M') {   /* a memo field: its file beside */
            dbf_memo_create(name);
            break;
        }
    }
    return nil_result(result);
}

/* DBCREATEIND(file, keytext, [block], [unique]) */
static int fn_dbcreateind(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_CHAR) {
        ctx->error = "DBCREATEIND requires (filename, key expression)";
        return -1;
    }
    run_cmd("INDEX ON %s TO %s%s", args[1].str, args[0].str, arg_logic(args, nargs, 3, 0) ? " UNIQUE" : "");
    return nil_result(result);
}

static int fn_dbdelete(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    run_cmd("DELETE");
    return nil_result(result);
}

static int fn_dbrecall(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    run_cmd("RECALL");
    return nil_result(result);
}

static int at_end(expr_ctx_t *ctx, const char *fn) {
    value_t v;
    return func_call(ctx, fn, NULL, 0, &v) == 0 && v.type == VAL_LOGIC && v.logic;
}

static int block_truth(expr_ctx_t *ctx, value_t *b, int *truth) {
    value_t r;
    if (!b || b->type != VAL_BLOCK) { *truth = 1; return 0; }
    if (ast_eval_block(ctx, b, NULL, 0, &r) != 0) return -1;
    *truth = r.type == VAL_LOGIC && r.logic;
    return 0;
}

/* DBEVAL(block, [for], [while], [next], [record], [rest]): the block for
 * each record in the scope that meets the conditions */
static int fn_dbeval(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    value_t *b, *bfor, *bwhile, r;
    int next = -1, rest, truth, count = 0;
    char n[32];
    if (nargs < 1 || args[0].type != VAL_BLOCK) {
        ctx->error = "DBEVAL requires (block [, for, while, next, record, rest])";
        return -1;
    }
    if (!cur_db_open(ctx)) return -1;
    b = &args[0];
    bfor = nargs > 1 && args[1].type == VAL_BLOCK ? &args[1] : NULL;
    bwhile = nargs > 2 && args[2].type == VAL_BLOCK ? &args[2] : NULL;
    if (nargs > 3 && args[3].type == VAL_NUM) next = (int)args[3].num;
    rest = arg_logic(args, nargs, 5, 0);
    if (nargs > 4 && args[4].type == VAL_NUM) {   /* one record */
        snprintf(n, sizeof(n), "%d", (int)args[4].num);
        cmd_api_go(n);
        if (block_truth(ctx, bfor, &truth) != 0) return -1;
        if (truth && ast_eval_block(ctx, b, NULL, 0, &r) != 0) return -1;
        return nil_result(result);
    }
    if (next < 0 && !rest && !bwhile) cmd_api_go("TOP");
    while (!at_end(ctx, "EOF") && (next < 0 || count < next)) {
        if (block_truth(ctx, bwhile, &truth) != 0) return -1;
        if (!truth) break;
        if (block_truth(ctx, bfor, &truth) != 0) return -1;
        if (truth && ast_eval_block(ctx, b, NULL, 0, &r) != 0) return -1;
        count++;
        cmd_api_skip("1");
    }
    return nil_result(result);
}

static int fn_dbfilter(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    work_area_t *wa = area_get_current();
    (void)ctx; (void)args; (void)nargs;
    *result = val_str(wa ? wa->filter_cond : "");
    return 0;
}

static int fn_dbgoto(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char n[32];
    if (nargs < 1 || args[0].type != VAL_NUM) {
        ctx->error = "DBGOTO requires a record number";
        return -1;
    }
    snprintf(n, sizeof(n), "%d", (int)args[0].num);
    cmd_api_go(n);
    return nil_result(result);
}

static int fn_dbreindex(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    run_cmd("REINDEX");
    return nil_result(result);
}

static int fn_dbrelation(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    work_area_t *wa = area_get_current();
    (void)ctx;
    *result = val_str(wa && arg_num(args, nargs, 0, 1) == 1 ? wa->relation_expr : "");
    return 0;
}

static int fn_dbrselect(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    work_area_t *wa = area_get_current();
    (void)ctx;
    *result = vnum(wa && (wa->relation_expr[0] || wa->relation_block) && arg_num(args, nargs, 0, 1) == 1 ? wa->relation_target + 1 : 0);
    return 0;
}

/* __ALIASEVAL(alias, block): alias->(e1, e2 ...) (ast.c parse_alias_expr),
 * the block evaluated with the alias's work area current */
static int fn_aliaseval(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int area, saved = area_get_current_idx(), rc;
    if (nargs < 2 || args[1].type != VAL_BLOCK) {
        ctx->error = "__ALIASEVAL requires (alias, block)";
        return -1;
    }
    if ((area = area_resolve_alias(arg_str(args, nargs, 0, ""))) < 0) {
        snprintf(ctx->err_msg, sizeof(ctx->err_msg), "Alias does not exist: %s", arg_str(args, nargs, 0, ""));
        ctx->error = ctx->err_msg;
        return -1;
    }
    area_set_current_idx(area);
    rc = ast_eval_block(ctx, &args[1], NULL, 0, result);
    area_set_current_idx(saved);
    return rc;
}

/* DBSEEK(value, [softseek]): FOUND() */
static int fn_dbseek(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    value_t f;
    if (nargs < 1) {
        ctx->error = "DBSEEK requires a key value";
        return -1;
    }
    if (!cmd_api_seek_ready()) {
        ctx->error = "DBSEEK: no controlling index";
        return -1;
    }
    cmd_api_seek_value(&args[0]);
    func_call(ctx, "FOUND", NULL, 0, &f);
    *result = f;
    return 0;
}

static int fn_dbselectar(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char a[64];
    (void)ctx;
    if (nargs >= 1 && args[0].type == VAL_NUM) snprintf(a, sizeof(a), "%d", (int)args[0].num);
    else snprintf(a, sizeof(a), "%s", arg_str(args, nargs, 0, "1"));
    cmd_api_select(a);
    return nil_result(result);
}

static int fn_dbsetdriver(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str("DBFNDX");   /* the one driver: DBF with NDX indexes */
    return 0;
}

/* DBSETFILTER(block, [text]): the block is the condition; the text is
 * what DBFILTER() reports */
static int fn_dbsetfilter(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (nargs < 1 || args[0].type != VAL_BLOCK) {
        ctx->error = "DBSETFILTER requires (block [, text])";
        return -1;
    }
    cmd_api_set_filter(args[0].block, arg_str(args, nargs, 1, ""));
    return nil_result(result);
}

static int fn_dbsetindex(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    work_area_t *wa = area_get_current();
    char list[600] = "";
    int i;
    if (nargs < 1 || args[0].type != VAL_CHAR) {
        ctx->error = "DBSETINDEX requires an index file name";
        return -1;
    }
    /* added to the list: SET INDEX TO replaces it, so the old ones go first */
    for (i = 0; wa && i < wa->num_indexes; i++) {
        snprintf(list + strlen(list), sizeof(list) - strlen(list), "%s%s", i ? ", " : "", wa->indexes[i].filename);
    }
    run_cmd("SET INDEX TO %s%s%s", list, list[0] ? ", " : "", args[0].str);
    return nil_result(result);
}

static int fn_dbsetorder(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    run_cmd("SET ORDER TO %d", arg_num(args, nargs, 0, 0));
    return nil_result(result);
}

/* DBSETRELAT(area, block, [text]): the block is the key, as DBSETFILTER's
 * is the condition */
static int fn_dbsetrelat(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char target[64];
    int area;
    if (nargs < 2 || args[1].type != VAL_BLOCK) {
        ctx->error = "DBSETRELAT requires (area, block [, text])";
        return -1;
    }
    if (args[0].type == VAL_NUM) snprintf(target, sizeof(target), "%d", (int)args[0].num);
    else snprintf(target, sizeof(target), "%s", arg_str(args, nargs, 0, ""));
    if ((area = area_resolve_alias(target)) < 0) {
        prog_error(ERR_ALIAS_NOT_FOUND, "Invalid work area");
        return -1;
    }
    cmd_api_set_relation(area, args[1].block, arg_str(args, nargs, 2, ""));
    return nil_result(result);
}

static int fn_dbunlock(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    return nil_result(result);   /* single user: nothing is locked */
}

/* DBUSEAREA([new], [driver], file, [alias], [shared], [readonly]) */
static int fn_dbusearea(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const char *file = arg_str(args, nargs, 2, NULL);
    const char *alias = arg_str(args, nargs, 3, NULL);
    if (!file) {
        ctx->error = "DBUSEAREA requires the file name (new, driver, file, alias)";
        return -1;
    }
    if (arg_logic(args, nargs, 0, 0)) cmd_api_select("0");
    if (alias && alias[0]) run_cmd("USE %s ALIAS %s", file, alias);
    else run_cmd("USE %s", file);
    return nil_result(result);
}

/* DESCEND(x): the value whose order is the reverse of x's */
static int fn_descend(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (nargs < 1) { ctx->error = "DESCEND requires an argument"; return -1; }
    switch (args[0].type) {
    case VAL_CHAR: {
        value_t v = args[0];
        char *p;
        for (p = v.str; *p; p++) *p = (char)(256 - (unsigned char)*p);
        *result = v;
        return 0;
    }
    case VAL_NUM:
        *result = val_num_dec(-args[0].num, val_set_decimals, 0);   /* a computed value: SET DECIMALS */
        return 0;
    case VAL_DATE:
        *result = vnum(5231808 - args[0].date);   /* Clipper's: a number counting down */
        return 0;
    case VAL_LOGIC:
        *result = val_logic(!args[0].logic);
        return 0;
    default:
        *result = args[0];
        return 0;
    }
}

/* ---- fields ---- */

static int field_index_arg(expr_ctx_t *ctx, value_t *args, int nargs, dbf_t **dbp) {
    dbf_t *db = cur_db_open(ctx);
    int n;
    if (!db) return -1;
    if (nargs < 1 || args[0].type != VAL_NUM) {
        ctx->error = "A field number is required";
        return -1;
    }
    n = (int)args[0].num;
    if (n < 1 || n > db->field_count) {
        ctx->error = "Field number out of range";
        return -1;
    }
    *dbp = db;
    return n - 1;
}

static int fn_fieldname(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    return func_call(ctx, "FIELD", args, nargs, result);
}

static int fn_fieldget(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    dbf_t *db;
    int i = field_index_arg(ctx, args, nargs, &db);
    if (i < 0) return -1;
    return ast_eval_alias_field(ctx, "FIELD", db->fields[i].name, result);
}

static int fn_fieldput(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    dbf_t *db;
    int i = field_index_arg(ctx, args, nargs, &db);
    if (i < 0) return -1;
    if (nargs < 2) { ctx->error = "FIELDPUT requires (number, value)"; return -1; }
    if (ast_store_alias_field(ctx, "FIELD", db->fields[i].name, &args[1]) != 0) return -1;
    *result = args[1];
    return 0;
}

/* The set-get blocks: {|x| IF(x == NIL, ref, ref := x)} over a field, a
 * field of another work area, or a memory variable */
static int setget_block(expr_ctx_t *ctx, const char *ref, value_t *result) {
    char text[400];
    snprintf(text, sizeof(text), "{|x| IIF(x == NIL, %s, %s := x)}", ref, ref);
    return ast_eval_dynamic(text, ctx, result);
}

static int fn_fieldblock(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char ref[200];
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "FIELDBLOCK requires a field name"; return -1; }
    snprintf(ref, sizeof(ref), "FIELD->%s", args[0].str);
    return setget_block(ctx, ref, result);
}

static int fn_fieldwblock(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char ref[200];
    work_area_t *wa;
    if (nargs < 2 || args[0].type != VAL_CHAR) { ctx->error = "FIELDWBLOCK requires (field name, work area)"; return -1; }
    wa = area_arg(args, nargs, 1);
    if (!wa || !wa->alias[0]) { ctx->error = "FIELDWBLOCK: no such work area"; return -1; }
    snprintf(ref, sizeof(ref), "%s->%s", wa->alias, args[0].str);
    return setget_block(ctx, ref, result);
}

static int fn_memvarblock(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    char ref[200];
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "MEMVARBLOCK requires a variable name"; return -1; }
    snprintf(ref, sizeof(ref), "M->%s", args[0].str);
    return setget_block(ctx, ref, result);
}

/* ---- indexes and work areas ---- */

static int fn_indexext(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str(".NDX");
    return 0;
}

static int fn_indexkey(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    work_area_t *wa = area_get_current();
    int n = arg_num(args, nargs, 0, 0);
    (void)ctx;
    if (wa && n == 0) n = wa->order;
    if (!wa || n < 1 || n > wa->num_indexes) { *result = val_str(""); return 0; }
    *result = val_str(wa->indexes[n - 1].key_expr);
    return 0;
}

static int fn_indexord(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    work_area_t *wa = area_get_current();
    (void)ctx; (void)args; (void)nargs;
    *result = vnum(wa && wa->num_indexes > 0 ? wa->order : 0);
    return 0;
}

static int fn_lastrec(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    return func_call(ctx, "RECCOUNT", args, nargs, result);
}

static int fn_neterr(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_logic(0);
    return 0;
}

/* ---- binary conversions (little-endian, as Clipper's) ---- */

static int fn_bin2i(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const unsigned char *s;
    int n;
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "BIN2I requires a string"; return -1; }
    s = (const unsigned char *)val_text(&args[0]); n = val_len(&args[0]);
    *result = vnum((int16_t)((n > 0 ? s[0] : 0) | (n > 1 ? s[1] : 0) << 8));
    return 0;
}

static int fn_bin2w(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const unsigned char *s;
    int n;
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "BIN2W requires a string"; return -1; }
    s = (const unsigned char *)val_text(&args[0]); n = val_len(&args[0]);
    *result = vnum((uint16_t)((n > 0 ? s[0] : 0) | (n > 1 ? s[1] : 0) << 8));
    return 0;
}

static int fn_bin2l(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const unsigned char *s;
    uint32_t v = 0;
    int i, n;
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "BIN2L requires a string"; return -1; }
    s = (const unsigned char *)val_text(&args[0]); n = val_len(&args[0]);
    for (i = 0; i < 4 && i < n; i++) v |= (uint32_t)s[i] << (8 * i);
    *result = vnum((int32_t)v);
    return 0;
}

/* The bytes, little-endian, zero bytes among them (a string value holds
 * them now) */
static int bin_string(int32_t v, int bytes, value_t *result) {
    char b[4];
    int i;
    for (i = 0; i < bytes; i++) b[i] = (char)(unsigned char)((uint32_t)v >> (8 * i));
    *result = val_strn(b, bytes);
    return 0;
}
static int fn_i2bin(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (nargs < 1 || args[0].type != VAL_NUM) { ctx->error = "I2BIN requires a number"; return -1; }
    return bin_string((int32_t)args[0].num, 2, result);
}

static int fn_l2bin(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (nargs < 1 || args[0].type != VAL_NUM) { ctx->error = "L2BIN requires a number"; return -1; }
    return bin_string((int32_t)args[0].num, 4, result);
}

/* ---- memo text: lines of a string under a width ---- */

/* the lWrap argument at index i: word wrap unless .F. is given */
static int arg_wrap(const value_t *args, int nargs, int i) {
    return !(nargs > i && args[i].type == VAL_LOGIC && !args[i].logic);
}

/* The start and length of line n (1-based) of s wrapped at width, words
 * kept whole; a hard return (CR LF, or LF) ends a line.  0 when there is
 * no such line. */
static int memo_line(const char *s, int width, int n, const char **start, int *len, int wrap) {
    const char *p = s;
    int line = 0;
    if (width < 4) width = 4;
    while (*p || line == 0) {
        const char *e = p, *brk = NULL;
        int col = 0, broke = 0;
        while (*e && *e != '\r' && *e != '\n' && col < width) {
            if (*e == ' ') brk = e;
            e++;
            col++;
        }
        if (*e && *e != '\r' && *e != '\n') {
            /* wrapped: back to the last blank -- or, lWrap .F., right at
             * the width (Harbour's reading; the guide says only "off") */
            if (wrap && brk && brk > p) e = brk;
            broke = !wrap;
        }
        line++;
        if (line == n) {
            *start = p;
            *len = (int)(e - p) + (*e == '\r' ? 1 : 0);   /* a hard return stays in its line (MEMOTRAN takes it out) */
            return 1;
        }
        p = e;
        if (*p == ' ' && !broke) p++;
        if (*p == '\r') p++;
        if (*p == '\n') p++;
        if (!*p) break;
    }
    return 0;
}

static int memo_lines(const char *s, int width, int wrap) {
    const char *st;
    int len, n = 0;
    if (!*s) return 0;   /* MLCOUNT("") is 0 */
    while (memo_line(s, width, n + 1, &st, &len, wrap)) n++;
    return n;
}

static int fn_memoline(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const char *st;
    int len, width = arg_num(args, nargs, 1, 79), n = arg_num(args, nargs, 2, 1);
    value_t r = val_str("");
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "MEMOLINE requires a string"; return -1; }
    if (memo_line(val_text(&args[0]), width, n, &st, &len, arg_wrap(args, nargs, 4))) {
        snprintf(r.str, sizeof(r.str), "%.*s", len, st);
        len = (int)strlen(r.str);
    } else {
        len = 0;   /* past the last line: blanks, the width wide */
    }
    while (len < width && len < (int)sizeof(r.str) - 1) r.str[len++] = ' ';   /* padded to the width */
    r.str[len] = '\0';
    r = val_str(r.str);
    *result = r;
    return 0;
}

static int fn_mlcount(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "MLCOUNT requires a string"; return -1; }
    *result = vnum(memo_lines(val_text(&args[0]), arg_num(args, nargs, 1, 79), arg_wrap(args, nargs, 3)));
    return 0;
}

static int fn_mlpos(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const char *st;
    int len;
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "MLPOS requires (string, width, line)"; return -1; }
    if (memo_line(val_text(&args[0]), arg_num(args, nargs, 1, 79), arg_num(args, nargs, 2, 1), &st, &len, arg_wrap(args, nargs, 4)))
        *result = vnum((int)(st - val_text(&args[0])) + 1);
    else
        *result = vnum((int)strlen(val_text(&args[0])) + 1);
    return 0;
}

static int fn_mlctopos(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const char *st;
    int len;
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "MLCTOPOS requires (string, width, line, col)"; return -1; }
    if (memo_line(val_text(&args[0]), arg_num(args, nargs, 1, 79), arg_num(args, nargs, 2, 1), &st, &len, arg_wrap(args, nargs, 5)))
        *result = vnum((int)(st - val_text(&args[0])) + 1 + arg_num(args, nargs, 3, 0));
    else
        *result = vnum((int)strlen(val_text(&args[0])) + 1);
    return 0;
}

/* MPOSTOLC(string, width, pos): {line, col} */
static int fn_mpostolc(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const char *st;
    int len, n = 1, pos = arg_num(args, nargs, 2, 1), width = arg_num(args, nargs, 1, 79);
    value_t lc[2];
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "MPOSTOLC requires (string, width, pos)"; return -1; }
    lc[0] = vnum(1);
    lc[1] = vnum(0);
    while (memo_line(val_text(&args[0]), width, n, &st, &len, arg_wrap(args, nargs, 4))) {
        int off = (int)(st - val_text(&args[0])) + 1;
        if (pos >= off && pos <= off + len) {
            lc[0] = vnum(n);
            lc[1] = vnum(pos - off);
            break;
        }
        n++;
    }
    return ast_make_array(ctx, lc, 2, result);
}

/* HARDCR: soft returns (141 10) made hard (13 10) */
static int fn_hardcr(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    value_t r;
    char *p;
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "HARDCR requires a string"; return -1; }
    r = args[0];
    for (p = r.str; *p; p++) {
        if ((unsigned char)*p == 141) *p = '\r';
    }
    *result = r;
    return 0;
}

/* MEMOTRAN(string, [hard], [soft]): CR LF pairs replaced, hard ones by ;
 * and soft ones by a blank, as the guide has it */
static int fn_memotran(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    value_t r = val_str("");
    const char *s, *hard = arg_str(args, nargs, 1, ";"), *soft = arg_str(args, nargs, 2, " ");
    int o = 0;
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "MEMOTRAN requires a string"; return -1; }
    for (s = val_text(&args[0]); *s && o < (int)sizeof(r.str) - 1; s++) {
        if ((*s == '\r' || (unsigned char)*s == 141) && s[1] == '\n') {
            r.str[o++] = (*s == '\r' ? hard : soft)[0];
            s++;
        } else {
            r.str[o++] = *s;
        }
    }
    r.str[o] = '\0';
    *result = r;
    return 0;
}

/* MEMOREAD(file) --> the file's text, whole */
static int fn_memoread(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    FILE *f;
    long size;
    char *buf;
    (void)ctx;
    if (nargs < 1 || args[0].type != VAL_CHAR) { *result = val_str(""); return 0; }
    f = fopen(val_text(&args[0]), "rb");
    if (!f) { *result = val_str(""); return 0; }
    fseek(f, 0, SEEK_END);
    size = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (size < 0) size = 0;
    buf = malloc((size_t)size + 1);
    if (!buf) { fclose(f); *result = val_str(""); return 0; }
    size = (long)fread(buf, 1, (size_t)size, f);
    fclose(f);
    *result = val_strn(buf, (int)size);
    free(buf);
    return 0;
}

/* MEMOWRIT(file, text) --> written whole */
static int fn_memowrit(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    FILE *f;
    int ok = 0;
    (void)ctx;
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_CHAR) { *result = val_logic(0); return 0; }
    f = fopen(val_text(&args[0]), "wb");
    if (f) {
        ok = fwrite(val_text(&args[1]), 1, (size_t)val_len(&args[1]), f) == (size_t)val_len(&args[1]);
        fclose(f);
    }
    *result = val_logic(ok);
    return 0;
}

/* ---- files and the environment ---- */

static int fn_ferase(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (nargs < 1 || args[0].type != VAL_CHAR) { ctx->error = "FERASE requires a file name"; return -1; }
    *result = vnum(unlink(args[0].str) == 0 ? 0 : -1);
    return 0;
}

static int fn_frename(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (nargs < 2 || args[0].type != VAL_CHAR || args[1].type != VAL_CHAR) {
        ctx->error = "FRENAME requires (old name, new name)";
        return -1;
    }
    *result = vnum(rename(args[0].str, args[1].str) == 0 ? 0 : -1);
    return 0;
}

/* DIRECTORY([spec], [attributes]): ADIR's listing as {{name, size, date, time, attr}, ...} */
static int fn_directory(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    value_t names, sizes, dates, times, count, spec, a[5];
    array_t *out;
    int n, i;
    spec = val_str(arg_str(args, nargs, 0, "*.*"));
    if (func_call(ctx, "ARRAY", &(value_t){ .type = VAL_NUM, .num = 1024 }, 1, &names) != 0) return -1;
    if (func_call(ctx, "ARRAY", &(value_t){ .type = VAL_NUM, .num = 1024 }, 1, &sizes) != 0) return -1;
    if (func_call(ctx, "ARRAY", &(value_t){ .type = VAL_NUM, .num = 1024 }, 1, &dates) != 0) return -1;
    if (func_call(ctx, "ARRAY", &(value_t){ .type = VAL_NUM, .num = 1024 }, 1, &times) != 0) return -1;
    a[0] = spec; a[1] = names; a[2] = sizes; a[3] = dates; a[4] = times;
    if (func_call(ctx, "ADIR", a, 5, &count) != 0) return -1;
    n = count.type == VAL_NUM ? (int)count.num : 0;
    if (ast_make_array(ctx, NULL, 0, result) != 0) return -1;
    out = result->array;
    for (i = 0; i < n && i < 1024; i++) {
        value_t row[5], rv, add[2], r;
        row[0] = names.array->el[i];
        row[1] = sizes.array->el[i];
        row[2] = dates.array->el[i];
        row[3] = times.array->el[i];
        row[4] = val_str("A");
        if (ast_make_array(ctx, row, 5, &rv) != 0) return -1;
        add[0] = *result;
        add[1] = rv;
        if (func_call(ctx, "AADD", add, 2, &r) != 0) return -1;
    }
    (void)out;
    return 0;
}

static int errorlevel_value;
int clip_errorlevel(void) { return errorlevel_value; }   /* the exit status QUIT and the end of the program give */

static int fn_errorlevel(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    *result = vnum(errorlevel_value);
    if (nargs >= 1 && args[0].type == VAL_NUM) errorlevel_value = (int)args[0].num;
    return 0;
}

static int fn_doserror(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = vnum(0);
    return 0;
}

static int fn_netname(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_str("");
    return 0;
}

static int fn_isprinter(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = val_logic(0);
    return 0;
}

static int fn_isdigit(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    *result = val_logic(nargs >= 1 && args[0].type == VAL_CHAR && isdigit((unsigned char)args[0].str[0]));
    return 0;
}

static int fn_exp(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (nargs < 1 || args[0].type != VAL_NUM) { ctx->error = "EXP requires a number"; return -1; }
    *result = val_num_dec(exp(args[0].num), val_set_decimals, 0);
    return 0;
}

static int fn_log(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    if (nargs < 1 || args[0].type != VAL_NUM || args[0].num <= 0) { ctx->error = "LOG requires a positive number"; return -1; }
    *result = val_num_dec(log(args[0].num), val_set_decimals, 0);
    return 0;
}

static int fn_word(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    *result = nargs >= 1 ? args[0] : val_nil();
    return 0;
}

/* DEVOUTPICT(value, picture): TRANSFORM, then DISPOUT */
static int fn_devoutpict(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    value_t t;
    if (nargs < 2) { ctx->error = "DEVOUTPICT requires (value, picture)"; return -1; }
    if (func_call(ctx, "TRANSFORM", args, 2, &t) != 0) return -1;
    return func_call(ctx, "DEVOUT", &t, 1, result);
}

static int fn_setprc(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    screen_set_printer_pos(arg_num(args, nargs, 0, 0), arg_num(args, nargs, 1, 0));
    return nil_result(result);
}

static int fn_nextkey(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = vnum(screen_nextkey());
    return 0;
}

static int flag_setting(int *flag, value_t *args, int nargs, value_t *result) {
    *result = val_logic(*flag);
    if (nargs >= 1 && args[0].type == VAL_LOGIC) *flag = args[0].logic;
    return 0;
}

static int setcancel_flag = 1, setblink_flag = 1, nosnow_flag;

static int fn_setcancel(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    return flag_setting(&setcancel_flag, args, nargs, result);
}

static int fn_setblink(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    return flag_setting(&setblink_flag, args, nargs, result);
}

static int fn_nosnow(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    return flag_setting(&nosnow_flag, args, nargs, result);
}

/* SETMODE([rows], [cols]): a size not given (or not a number) stays */
static int fn_setmode(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx;
    *result = val_logic(screen_setmode(arg_num(args, nargs, 0, screen_maxrow() + 1),
                                       arg_num(args, nargs, 1, screen_maxcol() + 1)));
    return 0;
}

static int fn_fkmax(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = vnum(40);
    return 0;
}

static int fn_fklabel(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    int n = arg_num(args, nargs, 0, 0);
    char buf[16];
    (void)ctx;
    if (n >= 1 && n <= 40) snprintf(buf, sizeof(buf), "F%d", n);
    else buf[0] = '\0';
    *result = val_str(buf);
    return 0;
}

static int fn_nil(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    return nil_result(result);
}

static int fn_zero(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)ctx; (void)args; (void)nargs;
    *result = vnum(0);
    return 0;
}

/* ---- the interactive ones, as far as a stream allows ---- */

/* ALERT(message, [options]): the message with its options; the first
 * option is chosen (1) when no terminal can offer the choice */
/* ALERT(message [, options]) --> the option chosen (1-based), 0 for Esc.
 * The message's lines (; between them) centred in a box in the middle of
 * the screen, the options on the line below, Left / Right / Tab moving
 * among them, Return choosing, a first letter choosing too; the screen
 * under the box put back.  Without a terminal: the message and the
 * options on one line, the first option chosen. */
static int fn_alert(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const char *msg = arg_str(args, nargs, 0, "");
    array_t *opts = nargs >= 2 && args[1].type == VAL_ARRAY ? args[1].array : NULL;
    const char *optv[16];
    int nopt = 0, i, nlines = 0, width = 0, cur = 0, top, left, height, row, key, optw = 0;
    const char *lines[16];
    int linelen[16];
    char line[512];
    (void)ctx;
    /* the lines */
    {
        const char *p = msg;
        while (nlines < 16) {
            const char *q = strchr(p, ';');
            lines[nlines] = p;
            linelen[nlines] = q ? (int)(q - p) : (int)strlen(p);
            if (linelen[nlines] > width) width = linelen[nlines];
            nlines++;
            if (!q) break;
            p = q + 1;
        }
    }
    if (opts) {
        for (i = 0; i < opts->len && nopt < 16; i++)
            if (opts->el[i].type == VAL_CHAR && opts->el[i].str[0]) optv[nopt++] = opts->el[i].str;
    }
    if (nopt == 0) { optv[0] = "Ok"; nopt = 1; }
    for (i = 0; i < nopt; i++) optw += (int)strlen(optv[i]) + 2 + (i ? 2 : 0);
    if (optw > width) width = optw;
    if (!screen_term_available()) {
        /* No screen: the message, its ; as line breaks, then the choices
         * in parentheses, and the first is taken -- as Harbour renders
         * an Alert() to a stream, so the two can be compared */
        char *s;
        snprintf(line, sizeof(line), "%s", msg);
        for (s = line; *s; s++) if (*s == ';') *s = '\n';
        strncat(line, " (", sizeof(line) - strlen(line) - 1);
        for (i = 0; i < nopt; i++) {
            if (i) strncat(line, ", ", sizeof(line) - strlen(line) - 1);
            strncat(line, optv[i], sizeof(line) - strlen(line) - 1);
        }
        strncat(line, ") ", sizeof(line) - strlen(line) - 1);
        printf("%s", line);
        *result = vnum(1);
        return 0;
    }
    height = nlines + 4;
    if (width > 76) width = 76;
    top = (screen_maxrow() + 1 - height) / 2;
    left = (screen_maxcol() + 1 - (width + 4)) / 2;
    {
        value_t saved = screen_save_region(top, left, top + height - 1, left + width + 3);
        screen_color_pair(2);
        screen_box(top, left, top + height - 1, left + width + 3, 1);
        for (row = 0; row < nlines; row++) {
            int n = linelen[row] > width ? width : linelen[row], pad = (width - n) / 2;
            memset(line, ' ', (size_t)width + 2);
            memcpy(line + 1 + pad, lines[row], (size_t)n);
            line[width + 2] = '\0';
            screen_out_at(top + 1 + row, left + 1, line);
        }
        memset(line, ' ', (size_t)width + 2); line[width + 2] = '\0';
        screen_out_at(top + 1 + nlines, left + 1, line);
        screen_out_at(top + 2 + nlines, left + 1, line);
        for (;;) {
            int x = left + 2 + (width - optw) / 2;
            for (i = 0; i < nopt; i++) {
                char cell[80];
                snprintf(cell, sizeof(cell), " %s ", optv[i]);
                screen_color_pair(i == cur ? 1 : 2);
                screen_out_at(top + 2 + nlines, x, cell);
                x += (int)strlen(cell) + 2;
            }
            screen_color_pair(1);
            key = screen_getkey();
            if (key == K_ESC || (key == KEY_NONE && screen_input_ended())) { cur = -1; break; }   /* no key will come: Esc */
            if (key == K_ENTER || key == 0) break;
            if (key == K_LEFT || key == K_SH_TAB) cur = cur > 0 ? cur - 1 : nopt - 1;
            else if (key == K_RIGHT || key == K_TAB || key == K_SPACE) cur = (cur + 1) % nopt;
            else if (key >= 32 && key <= 255) {
                for (i = 0; i < nopt; i++)
                    if (toupper((unsigned char)optv[i][0]) == toupper(key)) { cur = i; key = K_ENTER; break; }
                if (key == K_ENTER) break;
            }
        }
        screen_restore_region(top, left, top + height - 1, left + width + 3, &saved);
    }
    *result = vnum(cur + 1);
    return 0;
}

/* ---- ACHOICE(): the pop-up menu ---- */

/* achoice.ch: the modes the user function is called with, its answers */
enum { AC_IDLE = 0, AC_HITTOP = 1, AC_HITBOTTOM = 2, AC_EXCEPT = 3, AC_NOITEM = 4 };
enum { AC_ABORT = 0, AC_SELECT = 1, AC_CONT = 2, AC_GOTO = 3 };

typedef struct {
    expr_ctx_t *ctx;
    int top, left, bottom, right;
    array_t *items;
    int nitems;             /* up to the first null string: "stops displaying" */
    const value_t *sel;     /* parallel array, one logical, or NULL */
    const char *userfunc;   /* a function name, or NULL */
    int cur;                /* the highlighted item, 0-based */
    int first;              /* the item on the window's first row */
    int rows, width;
} achoice_t;

static int achoice_selectable(achoice_t *a, int i) {
    const value_t *s = a->sel;
    value_t v;
    if (i < 0 || i >= a->nitems) return 0;
    if (!s) return 1;
    if (s->type == VAL_LOGIC) return s->logic;
    if (s->type != VAL_ARRAY) return 1;
    if (i >= s->array->len) return 1;
    v = s->array->el[i];
    if (v.type == VAL_LOGIC) return v.logic;
    if (v.type == VAL_CHAR) {
        value_t r;
        if (ast_eval_dynamic(v.str, a->ctx, &r) != 0) { a->ctx->error = NULL; return 1; }
        return r.type == VAL_LOGIC ? r.logic : 1;
    }
    return 1;
}

/* The nearest selectable item from i stepping by dir; -1 when none */
static int achoice_near(achoice_t *a, int i, int dir) {
    for (; i >= 0 && i < a->nitems; i += dir)
        if (achoice_selectable(a, i)) return i;
    return -1;
}

static void achoice_paint(achoice_t *a) {
    int r;
    char line[256];
    if (!screen_term_available()) return;
    for (r = 0; r < a->rows; r++) {
        int i = a->first + r;
        int len = 0;
        if (i < a->nitems && a->items->el[i].type == VAL_CHAR) {
            const char *t = a->items->el[i].str;
            len = (int)strlen(t);
            if (len > a->width) len = a->width;
            memcpy(line, t, len);
        }
        memset(line + len, ' ', a->width - len);
        line[a->width] = '\0';
        screen_color_pair(i == a->cur ? 2 : (i < a->nitems && !achoice_selectable(a, i)) ? 5 : 1);
        screen_out_at(a->top + r, a->left, line);
    }
    screen_color_pair(1);
}

/* Keep the highlight inside the window */
static void achoice_scroll(achoice_t *a) {
    if (a->cur < a->first) a->first = a->cur;
    if (a->cur >= a->first + a->rows) a->first = a->cur - a->rows + 1;
    if (a->first < 0) a->first = 0;
}

/* The user function, with the mode, the current element (1-based) and the
 * relative row; its answer, AC_CONT when it has none */
static int achoice_call(achoice_t *a, int mode) {
    char call[160];
    value_t r;
    if (!a->userfunc) return AC_CONT;
    snprintf(call, sizeof(call), "%s(%d, %d, %d)", a->userfunc, mode, a->cur + 1, a->cur - a->first);
    if (ast_eval_dynamic(call, a->ctx, &r) != 0) { a->ctx->error = NULL; return AC_ABORT; }
    return r.type == VAL_NUM ? (int)r.num : AC_CONT;
}

/* The next selectable item (wrapping) whose first letter is key */
static int achoice_letter(achoice_t *a, int key) {
    int n, i;
    if (key < 32 || key > 255) return -1;
    for (n = 1; n <= a->nitems; n++) {
        i = (a->cur + n) % a->nitems;
        if (a->items->el[i].type == VAL_CHAR && achoice_selectable(a, i) &&
            toupper((unsigned char)a->items->el[i].str[0]) == toupper(key))
            return i;
    }
    return -1;
}

/* ACHOICE(top, left, bottom, right, items, [selectable], [userfunc],
 * [initial], [windowrow]) --> the position chosen, 0 when aborted.
 * The guide's two key sets: without a user function Home/End, Return,
 * Esc, the side arrows and first letters are ACHOICE's; with one, only
 * the vertical movement is, and every other key is an exception the
 * function answers. */
static int fn_achoice(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    achoice_t a;
    int i, init, winrow, idle_zero = 0, mode, key = 0;

    memset(&a, 0, sizeof(a));
    a.ctx = ctx;
    a.top = arg_num(args, nargs, 0, 0);
    a.left = arg_num(args, nargs, 1, 0);
    a.bottom = arg_num(args, nargs, 2, a.top);
    a.right = arg_num(args, nargs, 3, a.left);
    a.items = nargs > 4 && args[4].type == VAL_ARRAY ? args[4].array : NULL;
    if (!a.items || a.bottom < a.top || a.right < a.left) { *result = vnum(0); return 0; }
    for (i = 0; i < a.items->len; i++)
        if (a.items->el[i].type != VAL_CHAR || a.items->el[i].str[0] == '\0') break;
    a.nitems = i;
    a.sel = nargs > 5 && (args[5].type == VAL_ARRAY || args[5].type == VAL_LOGIC) ? &args[5] : NULL;
    a.userfunc = nargs > 6 && args[6].type == VAL_CHAR && args[6].str[0] ? args[6].str : NULL;
    init = arg_num(args, nargs, 7, 0);
    winrow = arg_num(args, nargs, 8, -1);
    a.rows = a.bottom - a.top + 1;
    a.width = a.right - a.left + 1;
    if (a.width > 255) a.width = 255;

    a.cur = (init >= 1 && achoice_selectable(&a, init - 1)) ? init - 1 : achoice_near(&a, 0, 1);
    if (a.cur < 0) {
        a.cur = -1;             /* the user function sees element 0, row -1 */
        mode = AC_NOITEM;
    } else {
        mode = AC_IDLE;
    }
    {
        /* The window is placed by the initial item as given (the guide:
         * nWindowRow is the row that item appears on), even when the
         * highlight moved on to the first selectable one; DBU's Set menu
         * starts on an unselectable Relation that must stay in view.  The
         * same with nothing selectable: DBU lists its view's fields with
         * ACHOICE(..., .F., "", top_el) and reads nothing */
        int base = (init >= 1 && init <= a.nitems) ? init - 1 : (a.cur >= 0 ? a.cur : 0);
        if (winrow >= 0 && winrow < a.rows) a.first = base - winrow;
        else a.first = a.nitems - a.rows;   /* no empty rows at the bottom */
        if (a.first > base) a.first = base;
        if (a.first < 0) a.first = 0;
    }
    if (mode == AC_NOITEM) achoice_paint(&a);   /* shown even when there is nothing to choose */

    /* One pass per key: read it (an exception or the no-item state keeps
     * the key it has), act on it, then tell the user function the mode the
     * pass ended in.  Its answer AC_GOTO replays the key as a first-letter
     * search; AC_CONT reads on. */
    for (;;) {
        int rc, next;
        if (mode != AC_EXCEPT && mode != AC_NOITEM) {
            achoice_scroll(&a);
            achoice_paint(&a);
            key = screen_inkey(-1.0);
            if (key == 0 && ++idle_zero >= 2) { *result = vnum(0); return 0; }  /* input gone */
            if (key) idle_zero = 0;
            mode = AC_IDLE;
        }
        if (mode == AC_NOITEM && !a.userfunc) { *result = vnum(0); return 0; }
        if (mode != AC_NOITEM) switch (key) {
        case K_ESC:
            if (!a.userfunc) { *result = vnum(0); return 0; }
            mode = AC_EXCEPT;
            break;
        case K_UP:
            next = achoice_near(&a, a.cur - 1, -1);
            if (next >= 0) a.cur = next; else mode = AC_HITTOP;
            break;
        case K_DOWN:
            next = achoice_near(&a, a.cur + 1, 1);
            if (next >= 0) a.cur = next; else mode = AC_HITBOTTOM;
            break;
        case K_CTRL_HOME:
            next = achoice_near(&a, a.first, 1);
            if (next >= 0 && next < a.first + a.rows && next != a.cur) a.cur = next;
            else if (a.cur == achoice_near(&a, 0, 1)) mode = AC_HITTOP;
            break;
        case K_CTRL_END:
            i = a.first + a.rows - 1;
            if (i >= a.nitems) i = a.nitems - 1;
            next = achoice_near(&a, i, -1);
            if (next >= a.first && next != a.cur) a.cur = next;
            else if (a.cur == achoice_near(&a, a.nitems - 1, -1)) mode = AC_HITBOTTOM;
            break;
        case K_PGUP:
            if (a.cur == achoice_near(&a, 0, 1)) { mode = AC_HITTOP; break; }
            next = achoice_near(&a, a.first, 1);
            if (next >= 0 && next < a.cur) { a.cur = next; break; }
            a.first -= a.rows;
            if (a.first < 0) a.first = 0;
            next = achoice_near(&a, a.first, 1);
            if (next >= 0) a.cur = next;
            break;
        case K_PGDN:
            if (a.cur == achoice_near(&a, a.nitems - 1, -1)) { mode = AC_HITBOTTOM; break; }
            i = a.first + a.rows - 1;
            if (i >= a.nitems) i = a.nitems - 1;
            next = achoice_near(&a, i, -1);
            if (next > a.cur) { a.cur = next; break; }
            a.first += a.rows;
            if (a.first > a.nitems - a.rows) a.first = a.nitems - a.rows;
            if (a.first < 0) a.first = 0;
            i = a.first + a.rows - 1;
            if (i >= a.nitems) i = a.nitems - 1;
            next = achoice_near(&a, i, -1);
            if (next >= 0) a.cur = next;
            break;
        case K_CTRL_PGUP:
            next = achoice_near(&a, 0, 1);
            if (next >= 0) { a.cur = next; a.first = 0; }
            break;
        case K_CTRL_PGDN:
            next = achoice_near(&a, a.nitems - 1, -1);
            if (next >= 0) { a.cur = next; a.first = a.nitems - a.rows; }
            break;
        case K_ENTER:
            if (!a.userfunc) { *result = vnum(a.cur + 1); return 0; }
            mode = AC_EXCEPT;
            break;
        case K_LEFT: case K_RIGHT:
            if (!a.userfunc) { *result = vnum(0); return 0; }
            mode = AC_EXCEPT;
            break;
        case K_HOME:
            if (!a.userfunc) { next = achoice_near(&a, 0, 1); if (next >= 0) a.cur = next; break; }
            mode = AC_EXCEPT;
            break;
        case K_END:
            if (!a.userfunc) { next = achoice_near(&a, a.nitems - 1, -1); if (next >= 0) a.cur = next; break; }
            mode = AC_EXCEPT;
            break;
        default:
            if (key >= 32 && key <= 255 && (!a.userfunc || mode == AC_EXCEPT)) {
                next = achoice_letter(&a, key);
                if (next >= 0) a.cur = next;
                mode = AC_IDLE;
            } else if (mode == AC_EXCEPT) {
                mode = AC_IDLE;
            } else {
                mode = key == 0 ? AC_IDLE : AC_EXCEPT;
            }
            break;
        }
        if (!a.userfunc) continue;
        if (mode != AC_EXCEPT) { achoice_scroll(&a); achoice_paint(&a); }
        rc = achoice_call(&a, mode);
        if (rc == AC_ABORT) { *result = vnum(0); return 0; }
        if (rc == AC_SELECT) { *result = vnum(a.cur < 0 ? 0 : a.cur + 1); return 0; }
        if (rc == AC_GOTO) mode = AC_EXCEPT;
        else if (rc == AC_CONT) { if (mode != AC_NOITEM) mode = AC_IDLE; }
    }
}

static int fn_browse(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    (void)args; (void)nargs;
    if (!cur_db_open(ctx)) { *result = val_logic(0); return 0; }
    run_cmd("BROWSE");
    *result = val_logic(1);
    return 0;
}



static int fn_setcancel(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result);

/* ---- SET(): set.ch's numbering, Clipper's types ---- */

typedef struct { int code; const char *name; char kind; } set_entry_t;
static const set_entry_t set_entries[] = {
    { 1, "EXACT", 'L' }, { 2, "FIXED", 'L' }, { 3, "DECIMALS", 'N' }, { 4, "DATEFORMAT", 'C' },
    { 5, "EPOCH", 'N' }, { 6, "PATH", 'C' }, { 7, "DEFAULT", 'C' }, { 8, "EXCLUSIVE", 'L' },
    { 9, "SOFTSEEK", 'L' }, { 10, "UNIQUE", 'L' }, { 11, "DELETED", 'L' }, { 12, "CANCEL", 'L' },
    { 13, "DEBUG", 'L' }, { 14, "TYPEAHEAD", 'N' }, { 15, "COLOR", 'C' }, { 16, "CURSOR", 'N' },
    { 17, "CONSOLE", 'L' }, { 18, "ALTERNATE", 'L' }, { 19, "ALTFILE", 'C' }, { 20, "DEVICE", 'C' },
    { 21, "EXTRA", 'L' }, { 22, "EXTRAFILE", 'C' }, { 23, "PRINTER", 'L' }, { 24, "PRINTFILE", 'C' },
    { 25, "MARGIN", 'N' }, { 26, "BELL", 'L' }, { 27, "CONFIRM", 'L' }, { 28, "ESCAPE", 'L' },
    { 29, "INSERT", 'L' }, { 30, "EXIT", 'L' }, { 31, "INTENSITY", 'L' }, { 32, "SCOREBOARD", 'L' },
    { 33, "DELIMITERS", 'L' }, { 34, "DELIMCHARS", 'C' }, { 35, "WRAP", 'L' }, { 36, "MESSAGE", 'N' },
    { 37, "MCENTER", 'L' }, { 38, "SCROLLBREAK", 'L' }, { 0, NULL, 0 } };

/* Settings this interpreter does not act on are remembered here */
static value_t set_shadow[40];
static int set_shadow_ready;

static void set_shadow_init(void) {
    if (set_shadow_ready) return;
    set_shadow_ready = 1;
    set_shadow[7] = val_str("");
    set_shadow[8] = val_logic(1);
    set_shadow[13] = val_logic(0);
    set_shadow[19] = val_str("");
    set_shadow[21] = val_logic(0);
    set_shadow[22] = val_str("");
    set_shadow[23] = val_logic(0);
    set_shadow[24] = val_str("");
    set_shadow[31] = val_logic(1);
    set_shadow[32] = val_logic(1);
    set_shadow[33] = val_logic(0);
    set_shadow[34] = val_str("::");
    set_shadow[37] = val_logic(0);
    set_shadow[38] = val_logic(1);
}

/* The format as Clipper spells it: "mm/dd/yy", "dd.mm.yyyy" ... */
static const char *date_format_text(const set_options_t *o, char *buf, int size) {
    static const char *const two[] = { "mm/dd/yy", "yy.mm.dd", "dd/mm/yy", "dd/mm/yy", "dd.mm.yy", "dd-mm-yy", "yy/mm/dd" };
    static const char *const four[] = { "mm/dd/yyyy", "yyyy.mm.dd", "dd/mm/yyyy", "dd/mm/yyyy", "dd.mm.yyyy", "dd-mm-yyyy", "yyyy/mm/dd" };
    int i = (int)o->date_format;
    if (i < 0 || i > 6) i = 0;
    snprintf(buf, (size_t)size, "%s", o->century ? four[i] : two[i]);
    return buf;
}

static value_t set_current(expr_ctx_t *ctx, const set_entry_t *e) {
    const set_options_t *o = cmd_get_set_opts();
    char buf[64];
    value_t v;
    switch (e->code) {
    case 1: return val_logic(o->exact);
    case 2: return val_logic(o->fixed);
    case 3: return vnum(o->decimals);
    case 4: return val_str(date_format_text(o, buf, (int)sizeof(buf)));
    case 5: return vnum(o->epoch ? o->epoch : 1900);
    case 6: return val_str(o->path);
    case 9: return val_logic(o->softseek);
    case 10: return val_logic(o->unique);
    case 11: return val_logic(o->deleted);
    case 12: fn_setcancel(ctx, NULL, 0, &v); return v;
    case 15: return val_str(screen_color_spec());
    case 16: fn_setcursor(ctx, NULL, 0, &v); return v;
    case 17: return val_logic(o->console);
    case 18: return val_logic(o->alternate_on);
    case 20: return val_str(o->device ? "PRINTER" : "SCREEN");
    case 23: return val_logic(o->printer_on);
    case 14: return vnum(o->typeahead);
    case 25: return vnum(o->margin);
    case 26: return val_logic(o->bell);
    case 27: return val_logic(o->confirm);
    case 28: return val_logic(o->escape);
    case 32: return val_logic(o->scoreboard);
    case 29: fn_readinsert(ctx, NULL, 0, &v); return v;
    case 30: fn_readexit(ctx, NULL, 0, &v); return v;
    case 35: return val_logic(o->wrap);
    case 36: return vnum(o->message_row < 0 ? 0 : o->message_row);
    default: return set_shadow[e->code];
    }
}

static int truth_arg(const value_t *v) {
    if (v->type == VAL_LOGIC) return v->logic;
    if (v->type == VAL_CHAR) return strcasecmp(v->str, "ON") == 0;
    return 0;
}

static void set_apply(expr_ctx_t *ctx, const set_entry_t *e, value_t *v) {
    char buf[300];
    value_t r;
    switch (e->code) {
    case 1: case 2: case 9: case 10: case 11: case 17: case 18: case 23: case 26: case 27: case 28: case 32: case 35:
        run_cmd("SET %s %s", e->name, truth_arg(v) ? "ON" : "OFF");
        return;
    case 3: case 5: case 14: case 25: case 36:
        if (v->type == VAL_NUM) run_cmd("SET %s TO %d", e->name, (int)v->num);
        return;
    case 4: {   /* "dd/mm/yyyy" and the like, to the named format */
        static const struct { const char *text; const char *name; } fm[] = {
            { "mm/dd/yy", "AMERICAN" }, { "yy.mm.dd", "ANSI" }, { "dd/mm/yy", "BRITISH" }, { "dd.mm.yy", "GERMAN" },
            { "dd-mm-yy", "ITALIAN" }, { "yy/mm/dd", "JAPAN" }, { "mm/dd/yyyy", "AMERICAN" }, { "yyyy.mm.dd", "ANSI" },
            { "dd/mm/yyyy", "BRITISH" }, { "dd.mm.yyyy", "GERMAN" }, { "dd-mm-yyyy", "ITALIAN" }, { "yyyy/mm/dd", "JAPAN" },
            { NULL, NULL } };
        int i;
        if (v->type != VAL_CHAR) return;
        for (i = 0; fm[i].text; i++) {
            if (strcasecmp(fm[i].text, v->str) == 0) {
                run_cmd("SET DATE %s", fm[i].name);
                run_cmd("SET CENTURY %s", strstr(v->str, "yyyy") || strstr(v->str, "YYYY") ? "ON" : "OFF");
                return;
            }
        }
        return;
    }
    case 6: if (v->type == VAL_CHAR) run_cmd("SET PATH TO %s", v->str); return;
    case 12: fn_setcancel(ctx, v, 1, &r); return;
    case 15: if (v->type == VAL_CHAR) run_cmd("SET COLOR TO %s", v->str); return;
    case 16: fn_setcursor(ctx, v, 1, &r); return;
    case 20: if (v->type == VAL_CHAR) run_cmd("SET DEVICE TO %s", strcasecmp(v->str, "PRINTER") == 0 ? "PRINT" : v->str); return;
    case 29: fn_readinsert(ctx, v, 1, &r); return;
    case 30: fn_readexit(ctx, v, 1, &r); return;
    default:
        if (e->kind == 'L') set_shadow[e->code] = val_logic(truth_arg(v));
        else set_shadow[e->code] = *v;
        (void)buf;
        return;
    }
}

int clipfn_set(expr_ctx_t *ctx, value_t *args, int nargs, value_t *result) {
    const set_entry_t *e;
    int code;
    if (nargs < 1 || args[0].type != VAL_NUM) {
        ctx->error = "SET requires (setting [, new value])";
        return -1;
    }
    set_shadow_init();
    code = (int)args[0].num;
    for (e = set_entries; e->name && e->code != code; e++) {}
    if (!e->name) {
        *result = val_nil();
        return 0;
    }
    *result = set_current(ctx, e);
    if (nargs >= 2 && args[1].type != VAL_NIL) set_apply(ctx, e, &args[1]);
    return 0;
}

const clipfn_entry_t clipfn_table[] = {
    { "__ALIASEVAL", fn_aliaseval },   /* alias->(expr) */
    { "DBAPPEND", fn_dbappend }, { "DBCLEARFIL", fn_dbclearfil }, { "DBCLEARIND", fn_dbclearind },
    { "DBCLEARREL", fn_dbclearrel }, { "DBCLOSEALL", fn_dbcloseall }, { "DBCLOSEAREA", fn_dbclosearea },
    { "DBCOMMIT", fn_dbcommit }, { "DBCOMMITALL", fn_dbcommitall }, { "DBCREATE", fn_dbcreate },
    { "DBCREATEIND", fn_dbcreateind }, { "DBDELETE", fn_dbdelete }, { "DBEVAL", fn_dbeval },
    { "DBFILTER", fn_dbfilter }, { "DBGOTO", fn_dbgoto }, { "DBRECALL", fn_dbrecall },
    { "DBREINDEX", fn_dbreindex }, { "DBRELATION", fn_dbrelation }, { "DBRSELECT", fn_dbrselect },
    { "DBSEEK", fn_dbseek }, { "DBSELECTAR", fn_dbselectar }, { "DBSETDRIVER", fn_dbsetdriver },
    { "DBSETFILTER", fn_dbsetfilter }, { "DBSETINDEX", fn_dbsetindex }, { "DBSETORDER", fn_dbsetorder },
    { "DBSETRELAT", fn_dbsetrelat }, { "DBUNLOCK", fn_dbunlock }, { "DBUNLOCKALL", fn_dbunlock },
    { "DBUSEAREA", fn_dbusearea }, { "DESCEND", fn_descend },
    { "FIELDBLOCK", fn_fieldblock }, { "FIELDGET", fn_fieldget }, { "FIELDNAME", fn_fieldname },
    { "FIELDPUT", fn_fieldput }, { "FIELDWBLOCK", fn_fieldwblock }, { "MEMVARBLOCK", fn_memvarblock },
    { "INDEXEXT", fn_indexext }, { "INDEXKEY", fn_indexkey }, { "INDEXORD", fn_indexord },
    { "LASTREC", fn_lastrec }, { "NETERR", fn_neterr },
    { "BIN2I", fn_bin2i }, { "BIN2W", fn_bin2w }, { "BIN2L", fn_bin2l }, { "I2BIN", fn_i2bin }, { "L2BIN", fn_l2bin },
    { "MEMOLINE", fn_memoline }, { "MLCOUNT", fn_mlcount }, { "MLPOS", fn_mlpos }, { "MLCTOPOS", fn_mlctopos },
    { "MPOSTOLC", fn_mpostolc }, { "HARDCR", fn_hardcr }, { "MEMOTRAN", fn_memotran },
    { "MEMOREAD", fn_memoread }, { "MEMOWRIT", fn_memowrit },
    { "FERASE", fn_ferase }, { "FRENAME", fn_frename }, { "DIRECTORY", fn_directory },
    { "ERRORLEVEL", fn_errorlevel }, { "DOSERROR", fn_doserror }, { "NETNAME", fn_netname },
    { "ISPRINTER", fn_isprinter }, { "ISDIGIT", fn_isdigit }, { "EXP", fn_exp }, { "LOG", fn_log },
    { "WORD", fn_word }, { "DEVOUTPICT", fn_devoutpict }, { "SETPRC", fn_setprc }, { "NEXTKEY", fn_nextkey },
    { "SETCANCEL", fn_setcancel }, { "SETBLINK", fn_setblink }, { "NOSNOW", fn_nosnow }, { "SETMODE", fn_setmode },
    { "FKMAX", fn_fkmax }, { "FKLABEL", fn_fklabel }, { "ALTD", fn_nil }, { "COLORSELECT", fn_nil },
    { "DISPCOUNT", fn_zero },
    { "ALERT", fn_alert }, { "RANGECHECK", fn_rangecheck }, { "ACHOICE", fn_achoice }, { "BROWSE", fn_browse }, { "DBEDIT", fn_dbedit },
    { "MEMOEDIT", fn_memoedit },
    { NULL, NULL }
};
