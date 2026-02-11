#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "command.h"
#include "dbf.h"
#include "field.h"
#include "expr.h"
#include "func.h"
#include "date.h"
#include "util.h"
#include "memvar.h"
#include "set.h"
#include "program.h"
#include "screen.h"
#include "index.h"
#include "report.h"
#include "label.h"
#include "lex.h"
#include "area.h"

/* Persistent expression context */
static expr_ctx_t expr_ctx;

/* Stage 3 state */
static memvar_store_t memvar_store;
static set_options_t set_opts;
static int stage3_initialized;

/* Forward declarations */
/* area_lookup_dbf from area.h is used as callback */
static void common_list_display(dbf_t *db, lexer_t *l, int is_display);
static void cmd_list(dbf_t *db, lexer_t *l);
static void cmd_display(dbf_t *db, lexer_t *l);
static void cmd_display_structure(dbf_t *db);
static void cmd_replace(dbf_t *db, lexer_t *l);
static void cmd_report_form(dbf_t *db, lexer_t *l);
static void cmd_count(dbf_t *db, lexer_t *l);
static void cmd_sum(dbf_t *db, lexer_t *l);
static void cmd_average(dbf_t *db, lexer_t *l);
static void cmd_delete(dbf_t *db, lexer_t *l);
static void cmd_recall(dbf_t *db, lexer_t *l);
static void cmd_locate(dbf_t *db, lexer_t *l);
static void cmd_continue(dbf_t *db);
static void cmd_store(dbf_t *db, lexer_t *l);

static void report_expr_error(void);
static int skip_deleted(const char *rec_buf);
static int check_filter(dbf_t *db);

static work_area_t *cur_wa(void) { return area_get_current(); }
static dbf_t *cur_db(void) { return &area_get_current()->db; }

/* ---- Helper: initialize expr_ctx ---- */
static int screen_initialized;
static void ctx_setup(void) {
    if (!stage3_initialized) {
        area_init_all();
        memvar_init(&memvar_store);
        set_init(&set_opts);
        stage3_initialized = 1;
    }
    if (!screen_initialized) {
        screen_init();
        screen_initialized = 1;
    }
    expr_ctx.db = &area_get_current()->db;
    expr_ctx.vars = &memvar_store;
    expr_ctx.opts = (struct set_options *)&set_opts;
    expr_ctx.area_lookup = area_lookup_dbf;
}

/* ---- Command lex helpers ---- */
static int cmd_kw(const lexer_t *l, const char *kw) {
    return l->current.type == TOK_IDENT && is_keyword(l->current.text, kw);
}

static int cmd_peek_kw(const lexer_t *l, const char *kw) {
    lexer_t tmp = *l;
    lex_next(&tmp);
    return cmd_kw(&tmp, kw);
}

static const char *cmd_after(const lexer_t *l) {
    return skip_ws(l->p);
}

static int cmd_kw_at(const char *text, const char *kw, const char **after) {
    lexer_t t;
    lexer_init_ext(&t, text, expr_ctx.vars);
    if (cmd_kw(&t, kw)) {
        if (after) *after = cmd_after(&t);
        return 1;
    }
    return 0;
}

/* ---- Index helpers ---- */
static index_t *controlling_index(void) {
    work_area_t *wa = cur_wa();
    if (wa->order > 0 && wa->order <= wa->num_indexes)
        return &wa->indexes[wa->order - 1];
    return NULL;
}

static void close_all_indexes(work_area_t *wa) {
    int i;
    for (i = 0; i < wa->num_indexes; i++) {
        if (wa->indexes[i].active)
            index_close(&wa->indexes[i]);
    }
    wa->num_indexes = 0;
    wa->order = 0;
}

/* ---- Index maintenance helpers ---- */

/* Evaluate key expression for current record, store in keybuf (space-padded to key_len) */
static int eval_index_key(index_t *idx, char *keybuf) {
    value_t val;
    char tmp[MAX_INDEX_KEY + 1];

    if (expr_eval_str(&expr_ctx, idx->key_expr, &val) != 0)
        return -1;
    val_to_string(&val, tmp, sizeof(tmp));
    /* Pad to key_len with spaces */
    {
        int len = strlen(tmp);
        if (len > idx->key_len) len = idx->key_len;
        memcpy(keybuf, tmp, len);
        if (len < idx->key_len)
            memset(keybuf + len, ' ', idx->key_len - len);
        keybuf[idx->key_len] = '\0';
    }
    return 0;
}

/* Insert current record into all active indexes for current work area */
static void indexes_insert_current(dbf_t *db) {
    work_area_t *wa = cur_wa();
    int i;
    ctx_setup();
    for (i = 0; i < wa->num_indexes; i++) {
        index_t *idx = &wa->indexes[i];
        if (idx->active) {
            char keybuf[MAX_INDEX_KEY + 1];
            if (eval_index_key(idx, keybuf) == 0) {
                index_insert(idx, keybuf, db->current_record);
                index_flush(idx);
            }
        }
    }
}

/* Capture current keys for all active indexes (before REPLACE modifies fields) */
static void indexes_capture_keys(dbf_t *db, char keys[][MAX_INDEX_KEY + 1]) {
    work_area_t *wa = cur_wa();
    int i;
    (void)db;
    ctx_setup();
    for (i = 0; i < wa->num_indexes; i++) {
        index_t *idx = &wa->indexes[i];
        keys[i][0] = '\0';
        if (idx->active) {
            eval_index_key(idx, keys[i]);
        }
    }
}

/* Update all active indexes after REPLACE (remove old key, insert new key) */
static void indexes_update_current(dbf_t *db, char old_keys[][MAX_INDEX_KEY + 1]) {
    work_area_t *wa = cur_wa();
    int i;
    ctx_setup();
    for (i = 0; i < wa->num_indexes; i++) {
        index_t *idx = &wa->indexes[i];
        if (idx->active) {
            char new_key[MAX_INDEX_KEY + 1];
            if (eval_index_key(idx, new_key) == 0) {
                if (memcmp(old_keys[i], new_key, idx->key_len) != 0) {
                    index_remove(idx, old_keys[i], db->current_record);
                    index_insert(idx, new_key, db->current_record);
                    index_flush(idx);
                }
            }
        }
    }
}

/* Rebuild all active indexes (after PACK changes record numbers) */
static void indexes_rebuild_all(dbf_t *db) {
    work_area_t *wa = cur_wa();
    int i;
    ctx_setup();
    for (i = 0; i < wa->num_indexes; i++) {
        index_t *idx = &wa->indexes[i];
        if (idx->active) {
            char key_expr[256], filename[64];
            str_copy(key_expr, idx->key_expr, sizeof(key_expr));
            str_copy(filename, idx->filename, sizeof(filename));
            index_build(idx, db, &expr_ctx, key_expr, filename);
            index_write(idx);
        }
    }
}

/* Clear all active indexes (after ZAP) */
static void indexes_clear_all(void) {
    work_area_t *wa = cur_wa();
    int i;
    for (i = 0; i < wa->num_indexes; i++) {
        index_t *idx = &wa->indexes[i];
        if (idx->active) {
            index_clear(idx);
            index_write(idx);
        }
    }
}

/* ---- SET RELATION: follow child area ---- */
static void follow_relations(void) {
    work_area_t *wa = cur_wa();
    if (wa->relation_expr[0] && wa->relation_target >= 0) {
        int saved_area = area_get_current_idx();
        value_t key;
        /* Evaluate relation expression in parent context */
        ctx_setup();
        if (expr_eval_str(&expr_ctx, wa->relation_expr, &key) == 0) {
            char buf[256];
            val_to_string(&key, buf, sizeof(buf));
            /* Switch to child, seek */
            area_set_current_idx(wa->relation_target);
            {
                work_area_t *child = cur_wa();
                index_t *idx = NULL;
                if (child->order > 0 && child->order <= child->num_indexes)
                    idx = &child->indexes[child->order - 1];
                if (idx && idx->active) {
                    index_seek(idx, buf);
                    {
                        uint32_t rec = index_current_recno(idx);
                        if (rec > 0)
                            dbf_read_record(&child->db, rec);
                        else
                            child->db.current_record = child->db.record_count + 1;
                    }
                } else {
                    /* No index: record number search */
                    uint32_t rec = (uint32_t)atoi(buf); 
                    if (rec >= 1 && rec <= child->db.record_count)
                        dbf_read_record(&child->db, rec);
                    else
                        child->db.current_record = child->db.record_count + 1;
                }
            }
        }
        area_set_current_idx(saved_area);
    }
}

/* ---- Helper: check FOR clause ---- */
static const char *find_for_clause(const char *p) {
    const char *s = p;
    while (*s) {
        s = skip_ws(s);
        if (str_imatch(s, "FOR")) {
            return skip_ws(s + 3);
        }
        s++;
    }
    return NULL;
}

/* ---- Helper: parse field list before FOR clause ---- */
static int parse_field_list(dbf_t *db, const char *arg, int *indices, int max_fields, const char **rest) {
    const char *p = skip_ws(arg);
    int count = 0;
    char name[DBF_MAX_FIELD_NAME];

    *rest = p;

    while (*p && count < max_fields) {
        int i, idx;

        if (str_imatch(p, "FOR")) break;
        /* Also stop at scope keywords that might appear in COPY TO context */
        if (str_imatch(p, "ALL") || str_imatch(p, "REST") ||
            str_imatch(p, "NEXT") || str_imatch(p, "RECORD")) break;

        i = 0;
        while (*p && *p != ',' && *p != ' ' && *p != '\t' && i < DBF_MAX_FIELD_NAME - 1)
            name[i++] = *p++;
        name[i] = '\0';

        if (name[0] == '\0') break;

        idx = dbf_find_field(db, name);
        if (idx < 0) {
            printf("Field not found: %s\n", name);
            return -1;
        }
        indices[count++] = idx;

        p = skip_ws(p);
        if (*p == ',') { p++; p = skip_ws(p); }
    }

    *rest = p;
    return count;
}

/* ---- Helper: display one field value ---- */
#define PRINT_TO(f, ...) do { if (f) fprintf(f, __VA_ARGS__); else printf(__VA_ARGS__); } while(0)

static void print_field_value(dbf_t *db, int f, char *raw, char *display, FILE *to_file, int for_list, int width) {
    dbf_get_field_raw(db, f, raw, 256);
    switch (db->fields[f].type) {
    case 'C':
        field_display_char(display, raw, db->fields[f].length);
        if (for_list) {
            trim_right(display);
            PRINT_TO(to_file, " %-*s", width, display);
        } else {
            trim_right(display);
            PRINT_TO(to_file, "%10s: %s\n", db->fields[f].name, display);
        }
        break;
    case 'N':
        field_display_numeric(display, raw, db->fields[f].length);
        if (for_list)
            PRINT_TO(to_file, " %*s", width, display);
        else
            PRINT_TO(to_file, "%10s: %s\n", db->fields[f].name, display);
        break;
    case 'D':
        field_display_date(display, raw);
        if (for_list)
            PRINT_TO(to_file, " %-8s", display);
        else
            PRINT_TO(to_file, "%10s: %s\n", db->fields[f].name, display);
        break;
    case 'L':
        field_display_logical(display, raw);
        if (for_list)
            PRINT_TO(to_file, " %-3s", display);
        else
            PRINT_TO(to_file, "%10s: %s\n", db->fields[f].name, display);
        break;
    default:
        field_display_char(display, raw, db->fields[f].length);
        if (for_list) {
            trim_right(display);
            PRINT_TO(to_file, " %-*s", width, display);
        } else {
            trim_right(display);
            PRINT_TO(to_file, "%10s: %s\n", db->fields[f].name, display);
        }
        break;
    }
}

static int field_display_width(dbf_t *db, int f) {
    int w = db->fields[f].length;
    int nlen = strlen(db->fields[f].name);
    if (w < nlen) w = nlen;
    if (db->fields[f].type == 'D') w = 8;
    if (db->fields[f].type == 'L') w = 3;
    return w;
}

/* ---- Helper: extract filename stem (without .DBF extension) ---- */
static void filename_stem(const char *filename, char *stem, int size) {
    int len;
    str_copy(stem, filename, size);
    len = strlen(stem);
    if (len >= 4 && str_icmp(stem + len - 4, ".DBF") == 0)
        stem[len - 4] = '\0';
}

/* ---- Helper: ensure filename has .DBF extension ---- */
static void ensure_dbf_ext(char *filename, int size) {
    str_upper(filename);
    trim_right(filename);
    if (strlen(filename) < 5 || str_icmp(filename + strlen(filename) - 4, ".DBF") != 0) {
        if ((int)strlen(filename) + 4 < size)
            strcat(filename, ".DBF");
    }
}

/* ---- Helper: ensure filename has .NDX extension ---- */
static void ensure_ndx_ext(char *filename, int size) {
    str_upper(filename);
    trim_right(filename);
    if (strlen(filename) < 5 || str_icmp(filename + strlen(filename) - 4, ".NDX") != 0) {
        if ((int)strlen(filename) + 4 < size)
            strcat(filename, ".NDX");
    }
}

/* ---- Helper: parse filename from arg ---- */
static const char *parse_filename(const char *arg, char *filename, int size) {
    const char *p = skip_ws(arg);
    int i = 0;
    while (*p && *p != ' ' && *p != '\t' && i < size - 1)
        filename[i++] = *p++;
    filename[i] = '\0';
    ensure_dbf_ext(filename, size);
    return p;
}

/* ---- CREATE ---- */
static void cmd_create(dbf_t *db, const char *arg) {
    char filename[64];
    dbf_field_t fields[DBF_MAX_FIELDS];
    int nfields = 0;
    char line[256];
    char type_line[16];
    char width_line[16];
    char dec_line[16];

    (void)db;
    arg = skip_ws(arg);
    if (*arg == '\0') {
        printf("Syntax: CREATE <filename>\n");
        return;
    }

    str_copy(filename, arg, sizeof(filename));
    ensure_dbf_ext(filename, sizeof(filename));

    printf("Enter field definitions (blank name to finish):\n");

    while (nfields < DBF_MAX_FIELDS) {
        printf("Field name: ");
        if (read_line(line, sizeof(line)) < 0) break;
        trim_right(line);
        if (line[0] == '\0') break;

        str_upper(line);
        str_copy(fields[nfields].name, line, DBF_MAX_FIELD_NAME);

        printf("Type (C/N/D/L): ");
        if (read_line(type_line, sizeof(type_line)) < 0) break;
        trim_right(type_line);
        str_upper(type_line);

        fields[nfields].type = type_line[0];
        fields[nfields].decimals = 0;

        if (type_line[0] == 'D') {
            fields[nfields].length = 8;
        } else if (type_line[0] == 'L') {
            fields[nfields].length = 1;
        } else {
            int maxw = (type_line[0] == 'N') ? 19 : 254;
            printf("Width (1-%d): ", maxw);
            if (read_line(width_line, sizeof(width_line)) < 0) break;
            fields[nfields].length = atoi(width_line);
            if (fields[nfields].length < 1) fields[nfields].length = 1;
            if (fields[nfields].length > maxw) fields[nfields].length = maxw;

            if (type_line[0] == 'N') {
                printf("Decimals: ");
                if (read_line(dec_line, sizeof(dec_line)) < 0) break;
                fields[nfields].decimals = atoi(dec_line);
                if (fields[nfields].decimals >= fields[nfields].length)
                    fields[nfields].decimals = fields[nfields].length - 2;
                if (fields[nfields].decimals < 0) fields[nfields].decimals = 0;
            }
        }

        fields[nfields].offset = 0;
        nfields++;
    }

    if (nfields == 0) {
        printf("No fields defined.\n");
        return;
    }

    if (dbf_create(filename, fields, nfields) < 0) {
        printf("Error creating %s\n", filename);
        return;
    }

    printf("Database %s created with %d field(s).\n", filename, nfields);
}

/* ---- SELECT ---- */
static void cmd_select(const char *arg) {
    const char *p = skip_ws(arg);
    int idx;

    if (*p == '\0') {
        printf("Current work area: %d\n", area_get_current_idx() + 1);
        return;
    }

    idx = area_resolve_alias(p);
    if (idx >= 0) {
        area_set_current_idx(idx);
        return;
    }

    printf("Invalid work area.\n");
}

/* ---- Helper: report file-not-found error ---- */
static void file_not_found(const char *filename) {
    char buf[128];
    snprintf(buf, sizeof(buf), "File not found: %s", filename);
    prog_error(ERR_FILE_NOT_FOUND, buf);
}

/* ---- Helper: map expression error string to error code ---- */
static int expr_error_code(const char *msg) {
    if (!msg) return ERR_SYNTAX;
    if (strstr(msg, "ivision by zero")) return ERR_DIV_ZERO;
    if (strstr(msg, "ype mismatch")) return ERR_TYPE_MISMATCH;
    return ERR_SYNTAX;
}

/* ---- Helper: report expression error via prog_error ---- */
static void report_expr_error(void) {
    if (expr_ctx.error) {
        prog_error(expr_error_code(expr_ctx.error), expr_ctx.error);
    }
}

/* ---- Helper: check if record should be skipped (SET DELETED) ---- */
static int skip_deleted(const char *rec_buf) {
    return (rec_buf[0] == '*' && set_opts.deleted);
}

/* ---- Helper: check if current record passes SET FILTER ---- */
/* Returns 1 if record passes (or no filter set), 0 if filtered out */
static int check_filter(dbf_t *db) {
    value_t cond;
    if (cur_wa()->filter_cond[0] == '\0')
        return 1;
    if (expr_eval_str(&expr_ctx, cur_wa()->filter_cond, &cond) != 0)
        return 1;  /* on error, pass through */
    return (cond.type == VAL_LOGIC && cond.logic);
}

/* ---- USE ---- */
static void cmd_use(dbf_t *db, const char *arg) {
    char filename[64];
    const char *p;
    int i;
    work_area_t *wa = cur_wa();

    arg = skip_ws(arg);

    if (*arg == '\0') {
        if (dbf_is_open(db)) {
            dbf_close(db);
            close_all_indexes(wa);
            wa->alias[0] = '\0';
            wa->locate_cond[0] = '\0';
            wa->locate_last_rec = 0;
            wa->filter_cond[0] = '\0';
            printf("Database closed.\n");
        }
        return;
    }

    if (dbf_is_open(db)) {
        dbf_close(db);
        close_all_indexes(wa);
        wa->alias[0] = '\0';
    }

    /* Parse filename (up to whitespace) */
    p = arg;
    i = 0;
    while (*p && *p != ' ' && *p != '\t' && i < 63)
        filename[i++] = *p++;
    filename[i] = '\0';
    if (filename[0] == '\0') {
        printf("Syntax: USE [filename] [INDEX idx] [ALIAS alias]\n");
        return;
    }

    ensure_dbf_ext(filename, sizeof(filename));

    if (dbf_open(db, filename) < 0) {
        file_not_found(filename);
        return;
    }

    /* Set alias to filename stem */
    filename_stem(filename, wa->alias, sizeof(wa->alias));

    /* Parse optional INDEX clause */
    p = skip_ws(p);
    if (str_imatch(p, "INDEX")) {
        p = skip_ws(p + 5);
        while (*p && !str_imatch(p, "ALIAS") && wa->num_indexes < MAX_INDEXES) {
            char ndxfile[64];
            i = 0;
            while (*p && *p != ' ' && *p != '\t' && *p != ',' && i < 63)
                ndxfile[i++] = *p++;
            ndxfile[i] = '\0';

            if (ndxfile[0]) {
                ensure_ndx_ext(ndxfile, sizeof(ndxfile));
                {
                    int slot = wa->num_indexes;
                    if (index_read(&wa->indexes[slot], ndxfile) == 0) {
                        wa->num_indexes++;
                    } else {
                        printf("Cannot open index: %s\n", ndxfile);
                    }
                }
            }

            p = skip_ws(p);
            if (*p == ',') { p++; p = skip_ws(p); }
            else break;
        }

        if (wa->num_indexes > 0)
            wa->order = 1;
    }

    /* Parse optional ALIAS clause */
    p = skip_ws(p);
    if (str_imatch(p, "ALIAS")) {
        p = skip_ws(p + 5);
        i = 0;
        while (*p && *p != ' ' && *p != '\t' && i < DBF_MAX_FIELD_NAME - 1)
            wa->alias[i++] = *p++;
        wa->alias[i] = '\0';
        str_upper(wa->alias);
    }

    /* Reset navigation state */
    expr_ctx.eof_flag = 0;
    expr_ctx.bof_flag = 0;
    expr_ctx.found = 0;
    wa->locate_cond[0] = '\0';
    wa->locate_last_rec = 0;

    printf("Database %s opened with %d record(s).\n",
           db->filename, (int)db->record_count);
}

/* ---- APPEND BLANK ---- */
static void cmd_append_blank(dbf_t *db) {
    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    if (dbf_append_blank(db) < 0) {
        prog_error(ERR_FILE_IO, "Error appending record");
        return;
    }

    indexes_insert_current(db);

    expr_ctx.eof_flag = 0;
    expr_ctx.bof_flag = 0;
    printf("Record %d added.\n", (int)db->record_count);
}

/* ---- GO / GOTO ---- */
static void cmd_go(dbf_t *db, const char *arg) {
    const char *p;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    p = skip_ws(arg);

    if (str_imatch(p, "TOP")) {
        if (db->record_count == 0) {
            expr_ctx.eof_flag = 1;
            expr_ctx.bof_flag = 1;
            printf("No records.\n");
            return;
        }
        {
            index_t *idx = controlling_index();
            if (idx) {
                uint32_t rec;
                index_top(idx);
                rec = index_current_recno(idx);
                if (rec > 0) dbf_read_record(db, rec);
                /* Advance past filtered records */
                while (rec > 0 && (!check_filter(db) || skip_deleted(db->record_buf))) {
                    if (index_next(idx) < 0) {
                        expr_ctx.eof_flag = 1;
                        db->current_record = db->record_count + 1;
                        return;
                    }
                    rec = index_current_recno(idx);
                    if (rec > 0) dbf_read_record(db, rec);
                }
            } else {
                uint32_t r;
                dbf_read_record(db, 1);
                /* Advance past filtered records */
                for (r = 1; r <= db->record_count; r++) {
                    dbf_read_record(db, r);
                    if (!skip_deleted(db->record_buf) && check_filter(db))
                        break;
                }
                if (r > db->record_count) {
                    expr_ctx.eof_flag = 1;
                    db->current_record = db->record_count + 1;
                    return;
                }
            }
        }
        expr_ctx.eof_flag = 0;
        expr_ctx.bof_flag = 0;
        return;
    }

    if (str_imatch(p, "BOTTOM")) {
        if (db->record_count == 0) {
            expr_ctx.eof_flag = 1;
            expr_ctx.bof_flag = 1;
            printf("No records.\n");
            return;
        }
        {
            index_t *idx = controlling_index();
            if (idx) {
                uint32_t rec;
                index_bottom(idx);
                rec = index_current_recno(idx);
                if (rec > 0) dbf_read_record(db, rec);
                /* Back up past filtered records */
                while (rec > 0 && (!check_filter(db) || skip_deleted(db->record_buf))) {
                    if (index_prev(idx) < 0) {
                        expr_ctx.bof_flag = 1;
                        return;
                    }
                    rec = index_current_recno(idx);
                    if (rec > 0) dbf_read_record(db, rec);
                }
            } else {
                uint32_t r;
                /* Scan backward for last matching record */
                for (r = db->record_count; r >= 1; r--) {
                    dbf_read_record(db, r);
                    if (!skip_deleted(db->record_buf) && check_filter(db))
                        break;
                }
                if (r < 1) {
                    expr_ctx.bof_flag = 1;
                    return;
                }
            }
        }
        expr_ctx.eof_flag = 0;
        expr_ctx.bof_flag = 0;
        return;
    }

    /* GO n */
    {
        int n = atoi(p);
        if (n < 1 || (uint32_t)n > db->record_count) {
            prog_error(ERR_RECORD_RANGE, "Record out of range");
            return;
        }
        dbf_read_record(db, (uint32_t)n);
        expr_ctx.eof_flag = 0;
        expr_ctx.bof_flag = 0;
    }
}

/* ---- SKIP ---- */
static void cmd_skip(dbf_t *db, const char *arg) {
    const char *p;
    int n;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    p = skip_ws(arg);
    if (*p == '\0')
        n = 1;
    else
        n = atoi(p);

    /* Index-ordered skip */
    {
        index_t *idx = controlling_index();
    if (idx) {
        int i, found;
        uint32_t rec;

        if (n > 0) {
            for (i = 0; i < n; i++) {
                found = 0;
                while (!found) {
                    if (index_next(idx) < 0) {
                        expr_ctx.eof_flag = 1;
                        expr_ctx.bof_flag = 0;
                        db->current_record = db->record_count + 1;
                        return;
                    }
                    rec = index_current_recno(idx);
                    if (rec > 0) dbf_read_record(db, rec);
                    if (!skip_deleted(db->record_buf) && check_filter(db))
                        found = 1;
                }
            }
        } else if (n < 0) {
            for (i = 0; i < -n; i++) {
                found = 0;
                while (!found) {
                    if (index_prev(idx) < 0) {
                        index_top(idx);
                        rec = index_current_recno(idx);
                        if (rec > 0) dbf_read_record(db, rec);
                        expr_ctx.bof_flag = 1;
                        expr_ctx.eof_flag = 0;
                        return;
                    }
                    rec = index_current_recno(idx);
                    if (rec > 0) dbf_read_record(db, rec);
                    if (!skip_deleted(db->record_buf) && check_filter(db))
                        found = 1;
                }
            }
        }

        rec = index_current_recno(idx);
        if (rec > 0) {
            dbf_read_record(db, rec);
            expr_ctx.eof_flag = 0;
            expr_ctx.bof_flag = 0;
        }
        return;
    }
    }

    /* Physical order skip */
    {
        int32_t target;
        int dir = (n > 0) ? 1 : -1;
        int remaining = (n > 0) ? n : -n;

        if (db->current_record == 0) {
            if (db->record_count == 0) {
                expr_ctx.eof_flag = 1;
                return;
            }
            dbf_read_record(db, 1);
            expr_ctx.eof_flag = 0;
            expr_ctx.bof_flag = 0;
            if (n > 0 && !skip_deleted(db->record_buf) && check_filter(db)) {
                remaining--;
            }
            if (remaining == 0) return;
        }

        target = (int32_t)db->current_record;
        while (remaining > 0) {
            target += dir;
            if (target < 1) {
                dbf_read_record(db, 1);
                expr_ctx.bof_flag = 1;
                expr_ctx.eof_flag = 0;
                return;
            }
            if ((uint32_t)target > db->record_count) {
                expr_ctx.eof_flag = 1;
                expr_ctx.bof_flag = 0;
                db->current_record = db->record_count + 1;
                return;
            }
            dbf_read_record(db, (uint32_t)target);
            if (!skip_deleted(db->record_buf) && check_filter(db))
                remaining--;
        }
        expr_ctx.eof_flag = 0;
        expr_ctx.bof_flag = 0;
    }
}

/* ---- Scope infrastructure ---- */

void clause_init(clause_t *c) {
    memset(c, 0, sizeof(*c));
    c->scope.type = SCOPE_DEFAULT;
    c->has_scope = 0;
}

static int consume_filename(lexer_t *l, char *out, int size) {
    const char *p = l->token_start;
    const char *start;
    int len = 0;
    char quote;

    p = skip_ws(p);
    if (*p == '\0') return -1;

    if (*p == '"' || *p == '\'') {
        quote = *p++;
        start = p;
        while (*p && *p != quote) p++;
        len = (int)(p - start);
        if (*p == quote) p++;
    } else {
        start = p;
        while (*p && *p != ' ' && *p != '\t') p++;
        len = (int)(p - start);
    }

    if (len >= size) len = size - 1;
    memcpy(out, start, len);
    out[len] = '\0';

    lexer_init_ext(l, p, l->store);
    return 0;
}

/* Parse common dBase clauses (FOR, WHILE, TO PRINT, TO FILE, OFF, etc.)
   Returns 0 on success. Consumes tokens until end of line or unknown keyword. */
int parse_clauses(lexer_t *l, clause_t *c) {
    while (l->current.type != TOK_EOF) {
        if (cmd_kw(l, "FOR")) {
            lex_next(l);
            const char *start = l->token_start;
            value_t dummy;
            expr_eval(&expr_ctx, &start, &dummy);
            int len = (int)(start - l->token_start);
            if (len > (int)sizeof(c->for_cond) - 1) len = sizeof(c->for_cond) - 1;
            memcpy(c->for_cond, l->token_start, len);
            c->for_cond[len] = '\0';
            lexer_init_ext(l, start, l->store);
        } else if (cmd_kw(l, "WHILE")) {
            lex_next(l);
            const char *start = l->token_start;
            value_t dummy;
            expr_eval(&expr_ctx, &start, &dummy);
            int len = (int)(start - l->token_start);
            if (len > (int)sizeof(c->while_cond) - 1) len = sizeof(c->while_cond) - 1;
            memcpy(c->while_cond, l->token_start, len);
            c->while_cond[len] = '\0';
            lexer_init_ext(l, start, l->store);
        } else if (cmd_kw(l, "TO")) {
            lex_next(l);
            if (cmd_kw(l, "PRINT")) {
                c->to_print = 1;
                lex_next(l);
            } else if (cmd_kw(l, "FILE")) {
                lex_next(l);
                if (consume_filename(l, c->to_file, sizeof(c->to_file)) < 0)
                    return -1;
            } else {
                return -1; /* Unknown TO clause */
            }
        } else if (cmd_kw(l, "OFF")) {
            c->off = 1;
            lex_next(l);
        } else if (cmd_kw(l, "PLAIN")) {
            c->plain = 1;
            lex_next(l);
        } else if (cmd_kw(l, "SUMMARY")) {
            c->summary = 1;
            lex_next(l);
        } else if (cmd_kw(l, "NOEJECT")) {
            c->noeject = 1;
            lex_next(l);
        } else if (cmd_kw(l, "HEADING")) {
            lex_next(l);
            if (l->current.type == TOK_STRING) {
                str_copy(c->heading, l->current.text, sizeof(c->heading));
                lex_next(l);
            }
        } else if (cmd_kw(l, "ALL")) {
            c->scope.type = SCOPE_ALL;
            c->has_scope = 1;
            lex_next(l);
        } else if (cmd_kw(l, "NEXT")) {
            lex_next(l);
            if (l->current.type == TOK_NUMBER) {
                c->scope.type = SCOPE_NEXT;
                c->scope.count = (uint32_t)l->current.num_val;
                c->has_scope = 1;
                lex_next(l);
            }
        } else if (cmd_kw(l, "RECORD")) {
            lex_next(l);
            if (l->current.type == TOK_NUMBER) {
                c->scope.type = SCOPE_RECORD;
                c->scope.count = (uint32_t)l->current.num_val;
                c->has_scope = 1;
                lex_next(l);
            }
        } else if (cmd_kw(l, "REST")) {
            c->scope.type = SCOPE_REST;
            c->has_scope = 1;
            lex_next(l);
        } else {
            /* Unknown token - might be field list or syntax error */
            return 0;
        }
    }
    return 0;
}

typedef int (*record_callback_t)(dbf_t *db, uint32_t recno, void *userdata);

typedef enum {
    REC_CONTINUE = 0,
    REC_STOP = 1,
    REC_ERROR = -1
} record_result_t;

#define PROC_USE_FILTER        0x01
#define PROC_SKIP_DELETED_SET  0x02
#define PROC_SKIP_DELETED_ANY  0x04
#define PROC_REQUIRE_DELETED   0x08
#define PROC_LEAVE_ON_MATCH    0x10

/* Generic record processor for commands with scope/FOR/WHILE.
   Handles index traversal if active. Returns number of records processed. */
static int process_records(dbf_t *db, clause_t *c, int flags, record_callback_t cb, void *userdata) {
    index_t *idx = controlling_index();
    uint32_t remaining = 0xFFFFFFFF;
    int has_limit = 0;
    int count = 0;
    uint32_t last_matched = 0;

    /* Default scope logic:
       If no explicit scope was parsed, but a FOR was present, dBase defaults to ALL.
       Otherwise, use the command-provided default (usually provided in c->scope before parse_clauses).
    */
    if (!c->has_scope) {
        if (c->for_cond[0]) c->scope.type = SCOPE_ALL;
        else if (c->scope.type == SCOPE_ALL_DEFAULT) c->scope.type = SCOPE_ALL;
        else if (c->scope.type == SCOPE_DEFAULT) {
            /* If no scope, no FOR, and still DEFAULT, fall back to NEXT 1 */
            c->scope.type = SCOPE_NEXT;
            c->scope.count = 1;
        }
    }

    /* Initial positioning */
    if (c->scope.type == SCOPE_ALL) {
        if (idx) index_top(idx);
        else dbf_read_record(db, 1);
        expr_ctx.eof_flag = 0;
        expr_ctx.bof_flag = 0;
    } else if (c->scope.type == SCOPE_RECORD) {
        dbf_read_record(db, c->scope.count);
        remaining = 1;
        has_limit = 1;
        expr_ctx.eof_flag = 0;
    } else if (c->scope.type == SCOPE_NEXT) {
        remaining = c->scope.count;
        has_limit = 1;
    }

    while (!expr_ctx.eof_flag && (!has_limit || remaining > 0)) {
        uint32_t current_rec = idx ? index_current_recno(idx) : db->current_record;
        if (current_rec == 0 || current_rec > db->record_count) break;

        dbf_read_record(db, current_rec);

        int matched = 1;
        if (flags & PROC_SKIP_DELETED_ANY) {
            if (db->record_buf[0] == '*') matched = 0;
        } else if (flags & PROC_SKIP_DELETED_SET) {
            if (skip_deleted(db->record_buf)) matched = 0;
        }
        if (matched && (flags & PROC_REQUIRE_DELETED)) {
            if (db->record_buf[0] != '*') matched = 0;
        }
        if (matched && (flags & PROC_USE_FILTER)) {
            if (!check_filter(db)) matched = 0;
        }

        if (matched && c->for_cond[0]) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, c->for_cond, &cond) != 0) { report_expr_error(); return -1; }
            if (cond.type != VAL_LOGIC || !cond.logic) matched = 0;
        }

        if (matched && c->while_cond[0]) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, c->while_cond, &cond) != 0) { report_expr_error(); return -1; }
            if (cond.type != VAL_LOGIC || !cond.logic) break;
        }

        if (matched) {
            int cb_result;
            last_matched = current_rec;
            cb_result = cb(db, current_rec, userdata);
            if (cb_result == REC_ERROR) return -1;
            count++;
            if (has_limit) remaining--;
            if (cb_result == REC_STOP) break;
        }

        /* Advance */
        if (idx) {
            if (index_next(idx) < 0) expr_ctx.eof_flag = 1;
        } else {
            if (current_rec >= db->record_count) expr_ctx.eof_flag = 1;
            else dbf_read_record(db, current_rec + 1);
        }
        if (c->scope.type == SCOPE_RECORD) break;
    }

    /* If we matched anything, leave pointer on last record processed (dBase rule for DELETE/REPLACE) */
    if ((flags & PROC_LEAVE_ON_MATCH) && last_matched > 0) {
        dbf_read_record(db, last_matched);
        /* If it was SCOPE_NEXT 1 (default), we might want to preserve EOF?
           Actually dBase leaves it on the record. */
        expr_ctx.eof_flag = 0;
    }

    return count;
}

static scope_t parse_scope(const char **pp) {
    scope_t s;
    const char *p = skip_ws(*pp);
    s.type = SCOPE_ALL;
    s.count = 0;

    if (str_imatch(p, "ALL")) {
        s.type = SCOPE_ALL;
        *pp = skip_ws(p + 3);
    } else if (str_imatch(p, "REST")) {
        s.type = SCOPE_REST;
        *pp = skip_ws(p + 4);
    } else if (str_imatch(p, "RECORD")) {
        s.type = SCOPE_RECORD;
        p = skip_ws(p + 6);
        s.count = atoi(p);
        while (*p >= '0' && *p <= '9') p++;
        *pp = skip_ws(p);
    } else if (str_imatch(p, "NEXT")) {
        s.type = SCOPE_NEXT;
        p = skip_ws(p + 4);
        s.count = atoi(p);
        if (s.count == 0) s.count = 1;
        while (*p >= '0' && *p <= '9') p++;
        *pp = skip_ws(p);
    }

    return s;
}

int scope_bounds(dbf_t *db, const scope_t *s, uint32_t *start, uint32_t *end) {
    switch (s->type) {
    case SCOPE_DEFAULT:
        *start = (db->current_record > 0) ? db->current_record : 1;
        *end = *start;
        break;
    case SCOPE_ALL_DEFAULT:
        *start = 1;
        *end = db->record_count;
        break;
    case SCOPE_ALL:
        *start = 1;
        *end = db->record_count;
        break;
    case SCOPE_REST:
        *start = (db->current_record > 0) ? db->current_record : 1;
        *end = db->record_count;
        break;
    case SCOPE_RECORD:
        if (s->count < 1 || s->count > db->record_count) return -1;
        *start = s->count;
        *end = s->count;
        break;
    case SCOPE_NEXT:
        *start = (db->current_record > 0) ? db->current_record : 1;
        *end = *start + s->count - 1;
        if (*end > db->record_count) *end = db->record_count;
        break;
    }
    return 0;
}

static int locate_cb(dbf_t *db, uint32_t recno, void *userdata) {
    (void)db;
    cur_wa()->locate_last_rec = recno;
    expr_ctx.found = 1;
    if (set_opts.talk) printf("Record = %d\n", (int)recno);
    return REC_STOP; /* Stop after first match */
}

/* ---- LOCATE [scope] FOR ---- */
static void cmd_locate(dbf_t *db, lexer_t *l) {
    clause_t c;
    int count;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    clause_init(&c);
    c.scope.type = SCOPE_ALL_DEFAULT;
    if (parse_clauses(l, &c) < 0) return;

    if (c.for_cond[0] == '\0') {
        printf("Syntax: LOCATE [scope] FOR <condition>\n");
        return;
    }

    /* Save for CONTINUE */
    str_copy(cur_wa()->locate_cond, c.for_cond, sizeof(cur_wa()->locate_cond));
    expr_ctx.found = 0;

    count = process_records(db, &c, PROC_USE_FILTER | PROC_SKIP_DELETED_SET | PROC_LEAVE_ON_MATCH, locate_cb, NULL);
    if (count < 0) return;
    if (count == 0) {
        expr_ctx.found = 0;
        expr_ctx.eof_flag = 1;
        cur_wa()->locate_last_rec = 0;
        if (set_opts.talk) printf("End of LOCATE scope\n");
    }
}

/* ---- CONTINUE ---- */
static void cmd_continue(dbf_t *db) {
    clause_t c;
    int count;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    if (cur_wa()->locate_cond[0] == '\0') {
        printf("No LOCATE active.\n");
        return;
    }

    clause_init(&c);
    c.scope.type = SCOPE_REST;
    c.has_scope = 1;
    str_copy(c.for_cond, cur_wa()->locate_cond, sizeof(c.for_cond));

    /* CONTINUE starts from NEXT record */
    if (db->current_record < db->record_count) {
        dbf_read_record(db, db->current_record + 1);
    } else {
        expr_ctx.eof_flag = 1;
        expr_ctx.found = 0;
        if (set_opts.talk) printf("End of LOCATE scope\n");
        return;
    }

    expr_ctx.found = 0;
    count = process_records(db, &c, PROC_USE_FILTER | PROC_SKIP_DELETED_SET | PROC_LEAVE_ON_MATCH, locate_cb, NULL);
    if (count < 0) return;
    if (count == 0) {
        expr_ctx.found = 0;
        expr_ctx.eof_flag = 1;
        if (set_opts.talk) printf("End of LOCATE scope\n");
    }
}

/* ---- COUNT [scope] [FOR cond] ---- */
static int count_cb(dbf_t *db, uint32_t recno, void *userdata) {
    (void)db; (void)recno; (void)userdata;
    return REC_CONTINUE; /* Just counting matches */
}

static void cmd_count(dbf_t *db, lexer_t *l) {
    clause_t c;
    int count;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    clause_init(&c);
    c.scope.type = SCOPE_ALL_DEFAULT;
    if (parse_clauses(l, &c) < 0) return;

    count = process_records(db, &c, PROC_USE_FILTER | PROC_SKIP_DELETED_SET, count_cb, NULL);
    if (count < 0) return;
    printf("%d record(s)\n", count);
}

typedef struct {
    char expr[256];
    double total;
} agg_ctx_t;

static int sum_cb(dbf_t *db, uint32_t recno, void *userdata) {
    agg_ctx_t *ctx = (agg_ctx_t *)userdata;
    value_t val;
    (void)db; (void)recno;

    if (expr_eval_str(&expr_ctx, ctx->expr, &val) != 0) {
        report_expr_error();
        return REC_ERROR;
    }
    if (val.type != VAL_NUM) {
        printf("SUM requires numeric expression.\n");
        return REC_ERROR;
    }
    ctx->total += val.num;
    return REC_CONTINUE;
}

static void cmd_sum(dbf_t *db, lexer_t *l) {
    clause_t c;
    agg_ctx_t actx;
    int count;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    clause_init(&c);
    c.scope.type = SCOPE_ALL_DEFAULT;

    if (l->current.type == TOK_EOF) {
        printf("Syntax: SUM <expr> [scope] [FOR <cond>] [WHILE <cond>]\n");
        return;
    }

    /* Optional leading scope (ALL/NEXT/RECORD/REST) */
    if (cmd_kw(l, "ALL") || cmd_kw(l, "NEXT") || cmd_kw(l, "RECORD") || cmd_kw(l, "REST")) {
        if (parse_clauses(l, &c) < 0) return;
    }

    /* Parse expression, then trailing clauses (FOR/WHILE/scope) */
    {
        const char *expr_start = l->token_start;
        lexer_t tmp = *l;
        while (tmp.current.type != TOK_EOF) {
            if (cmd_kw(&tmp, "FOR") || cmd_kw(&tmp, "WHILE") ||
                cmd_kw(&tmp, "ALL") || cmd_kw(&tmp, "NEXT") ||
                cmd_kw(&tmp, "RECORD") || cmd_kw(&tmp, "REST")) {
                break;
            }
            lex_next(&tmp);
        }
        {
            int len = (int)(tmp.token_start - expr_start);
            if (len >= (int)sizeof(actx.expr)) len = (int)sizeof(actx.expr) - 1;
            memcpy(actx.expr, expr_start, len);
            actx.expr[len] = '\0';
            trim_right(actx.expr);
        }
        if (actx.expr[0] == '\0') {
            printf("Syntax: SUM <expr> [scope] [FOR <cond>] [WHILE <cond>]\n");
            return;
        }
        lexer_init_ext(l, tmp.token_start, l->store);
        if (parse_clauses(l, &c) < 0) return;
    }

    actx.total = 0.0;

    count = process_records(db, &c, PROC_USE_FILTER | PROC_SKIP_DELETED_SET, sum_cb, &actx);
    if (count < 0) return;
    if (count >= 0) {
        value_t v = val_num(actx.total);
        char buf[64];
        val_to_string(&v, buf, sizeof(buf));
        printf("%d record(s) summed\n", count);
        printf("      SUM: %s\n", buf);
    }
}

static void cmd_average(dbf_t *db, lexer_t *l) {
    clause_t c;
    agg_ctx_t actx;
    int count;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    clause_init(&c);
    c.scope.type = SCOPE_ALL_DEFAULT;

    if (l->current.type == TOK_EOF) {
        printf("Syntax: AVERAGE <expr> [scope] [FOR <cond>] [WHILE <cond>]\n");
        return;
    }

    /* Optional leading scope (ALL/NEXT/RECORD/REST) */
    if (cmd_kw(l, "ALL") || cmd_kw(l, "NEXT") || cmd_kw(l, "RECORD") || cmd_kw(l, "REST")) {
        if (parse_clauses(l, &c) < 0) return;
    }

    /* Parse expression, then trailing clauses */
    {
        const char *expr_start = l->token_start;
        lexer_t tmp = *l;
        while (tmp.current.type != TOK_EOF) {
            if (cmd_kw(&tmp, "FOR") || cmd_kw(&tmp, "WHILE") ||
                cmd_kw(&tmp, "ALL") || cmd_kw(&tmp, "NEXT") ||
                cmd_kw(&tmp, "RECORD") || cmd_kw(&tmp, "REST")) {
                break;
            }
            lex_next(&tmp);
        }
        {
            int len = (int)(tmp.token_start - expr_start);
            if (len >= (int)sizeof(actx.expr)) len = (int)sizeof(actx.expr) - 1;
            memcpy(actx.expr, expr_start, len);
            actx.expr[len] = '\0';
            trim_right(actx.expr);
        }
        if (actx.expr[0] == '\0') {
            printf("Syntax: AVERAGE <expr> [scope] [FOR <cond>] [WHILE <cond>]\n");
            return;
        }
        lexer_init_ext(l, tmp.token_start, l->store);
        if (parse_clauses(l, &c) < 0) return;
    }

    actx.total = 0.0;

    count = process_records(db, &c, PROC_USE_FILTER | PROC_SKIP_DELETED_SET, sum_cb, &actx);
    if (count < 0) return;
    if (count > 0) {
        value_t v = val_num(actx.total / count);
        char buf[64];
        val_to_string(&v, buf, sizeof(buf));
        printf("%d record(s) averaged\n", count);
        printf("  AVERAGE: %s\n", buf);
    } else if (count == 0) {
        printf("No records.\n");
    }
}

/* ---- REPLACE (enhanced with expr_eval) ---- */
static void cmd_replace(dbf_t *db, lexer_t *l) {
    char field_name[DBF_MAX_FIELD_NAME];
    char formatted[256];
    int idx;
    char old_keys[MAX_INDEXES][MAX_INDEX_KEY + 1];
    int has_indexes;
    clause_t c;
    uint32_t start, end, i;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    clause_init(&c);
    /* dBase default for REPLACE is current record only (NEXT 1) if no clauses */
    c.scope.type = SCOPE_NEXT;
    c.scope.count = 1;

    /* For REPLACE, the field list/expression must come first, before clauses */
    const char *body_start = l->token_start;

    /* To find the end of the field list, we need to scan ahead for FOR/WHILE/ALL/NEXT etc. */
    lexer_t temp_l = *l;
    while (temp_l.current.type != TOK_EOF) {
        if (cmd_kw(&temp_l, "FOR") || cmd_kw(&temp_l, "WHILE") ||
            cmd_kw(&temp_l, "ALL") || cmd_kw(&temp_l, "NEXT") ||
            cmd_kw(&temp_l, "RECORD") || cmd_kw(&temp_l, "REST")) {
            break;
        }
        lex_next(&temp_l);
    }
    
    if (parse_clauses(&temp_l, &c) < 0) return;

    if (scope_bounds(db, &c.scope, &start, &end) < 0) {
        printf("Invalid scope.\n");
        return;
    }

    for (i = start; i <= end; i++) {
        if (dbf_read_record(db, i) < 0) continue;
        if (skip_deleted(db->record_buf) || !check_filter(db)) continue;

        if (c.for_cond[0]) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, c.for_cond, &cond) != 0) { report_expr_error(); return; }
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }
        if (c.while_cond[0]) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, c.while_cond, &cond) != 0) { report_expr_error(); return; }
            if (cond.type != VAL_LOGIC || !cond.logic) break;
        }

        /* Re-parse the field list for each record (brute force for now) */
        lexer_t replace_l;
        lexer_init_ext(&replace_l, body_start, expr_ctx.vars);

        /* Capture keys before modification */
        has_indexes = (cur_wa()->num_indexes > 0);
        if (has_indexes)
            indexes_capture_keys(db, old_keys);

        while (replace_l.current.type == TOK_IDENT &&
               !is_keyword(replace_l.current.text, "FOR") &&
               !is_keyword(replace_l.current.text, "WHILE") &&
               !is_keyword(replace_l.current.text, "ALL") &&
               !is_keyword(replace_l.current.text, "NEXT") &&
               !is_keyword(replace_l.current.text, "RECORD") &&
               !is_keyword(replace_l.current.text, "REST")) {

            str_copy(field_name, replace_l.current.text, sizeof(field_name));
            str_upper(field_name);
            lex_next(&replace_l);

            idx = dbf_find_field(db, field_name);
            if (idx < 0) {
                printf("Field not found: %s\n", field_name);
                return;
            }

            if (cmd_kw(&replace_l, "WITH")) {
                lex_next(&replace_l);
            }

            value_t val;
            char valbuf[256];
            if (expr_eval(&expr_ctx, &replace_l.token_start, &val) != 0) {
                report_expr_error();
                return;
            }
            lexer_init_ext(&replace_l, replace_l.token_start, expr_ctx.vars);

            val_to_string(&val, valbuf, sizeof(valbuf));

            switch (db->fields[idx].type) {
            case 'C':
                if (val.type == VAL_CHAR)
                    field_format_char(formatted, db->fields[idx].length, val.str);
                else
                    field_format_char(formatted, db->fields[idx].length, valbuf);
                break;
            case 'N':
                field_format_numeric(formatted, db->fields[idx].length,
                                     db->fields[idx].decimals, valbuf);
                break;
            case 'D':
                if (val.type == VAL_DATE) date_to_dbf(val.date, formatted);
                else field_format_date(formatted, valbuf);
                break;
            case 'L':
                if (val.type == VAL_LOGIC) {
                    formatted[0] = val.logic ? 'T' : 'F';
                    formatted[1] = '\0';
                } else field_format_logical(formatted, valbuf);
                break;
            default:
                field_format_char(formatted, db->fields[idx].length, valbuf);
                break;
            }

            dbf_set_field_raw(db, idx, formatted);

            if (replace_l.current.type == TOK_COMMA) {
                lex_next(&replace_l);
            }
        }

        dbf_flush_record(db);

        /* Update indexes after field modification */
        if (has_indexes)
            indexes_update_current(db, old_keys);
    }
}

/* ---- DISPLAY (enhanced with field list) ---- */
static void common_list_display(dbf_t *db, lexer_t *l, int is_display) {
    int f;
    char raw[256], display[256];
    int field_indices[DBF_MAX_FIELDS];
    int nfields = 0;
    int use_all_fields;
    FILE *to_file = NULL;
    clause_t c;
    int row_count = 0;
    index_t *idx = controlling_index();
    uint32_t remaining = 0xFFFFFFFF;
    int has_limit = 0;
    uint32_t orig_rec = db->current_record;
    int orig_eof = expr_ctx.eof_flag;
    int orig_bof = expr_ctx.bof_flag;
    uint32_t last_rec = 0;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }
    if (is_display && (db->current_record == 0 || expr_ctx.eof_flag)) {
        printf("No current record.\n");
        return;
    }

    if (db->record_count == 0) {
        printf("No records.\n");
        return;
    }

    clause_init(&c);

    /* Check if there's a field list before clauses */
    if (l->current.type == TOK_IDENT &&
        !is_keyword(l->current.text, "FOR") &&
        !is_keyword(l->current.text, "WHILE") &&
        !is_keyword(l->current.text, "TO") &&
        !is_keyword(l->current.text, "OFF") &&
        !is_keyword(l->current.text, "ALL") &&
        !is_keyword(l->current.text, "NEXT") &&
        !is_keyword(l->current.text, "RECORD") &&
        !is_keyword(l->current.text, "REST")) {

        const char *p = l->token_start;
        nfields = parse_field_list(db, p, field_indices, DBF_MAX_FIELDS, &p);
        if (nfields < 0) return;
        lexer_init_ext(l, p, l->store);
    }

    if (parse_clauses(l, &c) < 0) return;

    if (!c.has_scope) {
        if (is_display) {
            c.scope.type = SCOPE_NEXT;
            c.scope.count = 1;
        } else {
            c.scope.type = SCOPE_ALL_DEFAULT;
        }
    }

    if (c.for_cond[0] && !c.has_scope) {
        c.scope.type = SCOPE_ALL_DEFAULT;
    }
    if (c.scope.type == SCOPE_ALL_DEFAULT) {
        c.scope.type = SCOPE_ALL;
    } else if (c.scope.type == SCOPE_DEFAULT) {
        c.scope.type = SCOPE_NEXT;
        c.scope.count = 1;
    }

    if (c.to_file[0]) {
        to_file = fopen(c.to_file, "w");
    }

    use_all_fields = (nfields == 0);

    /* Positioning based on scope */
    if (c.scope.type == SCOPE_ALL) {
        if (idx) index_top(idx);
        else dbf_read_record(db, 1);
    } else if (c.scope.type == SCOPE_RECORD) {
        if (idx) {
            /* This is inefficient but correctly follows index if RECORD N is used?
               Actually dBase RECORD N always uses physical record. */
            dbf_read_record(db, c.scope.count);
        } else {
            dbf_read_record(db, c.scope.count);
        }
        remaining = 1;
        has_limit = 1;
    } else if (c.scope.type == SCOPE_NEXT) {
        remaining = c.scope.count;
        has_limit = 1;
    }

    if (set_opts.heading && !(is_display && use_all_fields)) {
        if (!c.off) PRINT_TO(to_file, "Record#");
        if (use_all_fields) {
            for (f = 0; f < db->field_count; f++) {
                int w = field_display_width(db, f);
                PRINT_TO(to_file, " %-*s", w, db->fields[f].name);
            }
        } else {
            for (f = 0; f < nfields; f++) {
                int idx_f = field_indices[f];
                int w = field_display_width(db, idx_f);
                PRINT_TO(to_file, " %-*s", w, db->fields[idx_f].name);
            }
        }
        PRINT_TO(to_file, "\n");
    }

    expr_ctx.eof_flag = 0;
    while (!expr_ctx.eof_flag && (!has_limit || remaining > 0)) {
        uint32_t current_rec = idx ? index_current_recno(idx) : db->current_record;
        if (current_rec == 0 || current_rec > db->record_count) break;

        dbf_read_record(db, current_rec);

        int matched = 1;
        if (skip_deleted(db->record_buf) || !check_filter(db)) matched = 0;

        if (matched && c.for_cond[0]) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, c.for_cond, &cond) != 0) { report_expr_error(); break; }
            if (cond.type != VAL_LOGIC || !cond.logic) matched = 0;
        }

        if (matched && c.while_cond[0]) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, c.while_cond, &cond) != 0) { report_expr_error(); break; }
            if (cond.type != VAL_LOGIC || !cond.logic) break;
        }

        if (matched) {
            if (is_display && !to_file && row_count > 0 && (row_count % 20) == 0) {
                printf("Press any key to continue...");
                getchar();
                printf("\r                           \r");
            }

            if (is_display && use_all_fields) {
                PRINT_TO(to_file, "Record# %d\n", (int)current_rec);
                for (f = 0; f < db->field_count; f++)
                    print_field_value(db, f, raw, display, to_file, 0, 0);
            } else {
                if (!c.off) PRINT_TO(to_file, "%7d", (int)current_rec);
                if (use_all_fields) {
                    for (f = 0; f < db->field_count; f++) {
                        int w = field_display_width(db, f);
                        print_field_value(db, f, raw, display, to_file, 1, w);
                    }
                } else {
                    for (f = 0; f < nfields; f++) {
                        int idx_f = field_indices[f];
                        int w = field_display_width(db, idx_f);
                        print_field_value(db, idx_f, raw, display, to_file, 1, w);
                    }
                }
                PRINT_TO(to_file, "\n");
            }
            last_rec = current_rec;
            row_count++;
            if (has_limit) remaining--;
        }

        /* Advance */
        if (idx) {
            if (index_next(idx) < 0) expr_ctx.eof_flag = 1;
        } else {
            if (db->current_record >= db->record_count) expr_ctx.eof_flag = 1;
            else dbf_read_record(db, db->current_record + 1);
        }
        if (c.scope.type == SCOPE_RECORD) break;
    }

    if (to_file) fclose(to_file);

    /* Restore current record to last displayed, preserving pre-list flags. */
    if (last_rec > 0)
        dbf_read_record(db, last_rec);
    else if (orig_rec > 0)
        dbf_read_record(db, orig_rec);
    expr_ctx.eof_flag = orig_eof;
    expr_ctx.bof_flag = orig_bof;
}

static void cmd_list(dbf_t *db, lexer_t *l) {
    common_list_display(db, l, 0);
}

static void cmd_display(dbf_t *db, lexer_t *l) {
    common_list_display(db, l, 1);
}

/* ---- DISPLAY STRUCTURE ---- */
static void cmd_display_structure(dbf_t *db) {
    int i;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    printf("Structure for database: %s\n", db->filename);
    printf("Number of records: %d\n", (int)db->record_count);
    printf("Header size: %d\n", (int)db->header_size);
    printf("Record size: %d\n", (int)db->record_size);
    printf("Field  Name        Type  Width  Dec\n");
    for (i = 0; i < db->field_count; i++) {
        printf("%5d  %-10s  %c      %2d      %d\n",
               i + 1, db->fields[i].name, db->fields[i].type,
               db->fields[i].length, db->fields[i].decimals);
    }
    printf("** Total **                  %2d\n", (int)db->record_size);
}

/* ---- STORE expr TO var ---- */
static void cmd_store(dbf_t *db, lexer_t *l) {
    char expr_str[256];
    value_t val;
    char name[MEMVAR_NAMELEN];

    (void)db;

    /* Expression start */
    const char *expr_start = l->token_start;
    lexer_t tmp = *l;
    while (tmp.current.type != TOK_EOF && !cmd_kw(&tmp, "TO"))
        lex_next(&tmp);
    
    if (!cmd_kw(&tmp, "TO")) {
        printf("Syntax: STORE <expr> TO <variable>\n");
        return;
    }

    int len = (int)(tmp.token_start - expr_start);
    if (len >= (int)sizeof(expr_str)) len = (int)sizeof(expr_str) - 1;
    memcpy(expr_str, expr_start, len);
    expr_str[len] = '\0';
    trim_right(expr_str);

    if (expr_eval_str(&expr_ctx, expr_str, &val) != 0) {
        report_expr_error();
        return;
    }

    lex_next(&tmp); /* skip TO */
    if (tmp.current.type != TOK_IDENT) {
        printf("Syntax: STORE <expr> TO <variable>\n");
        return;
    }
    str_copy(name, tmp.current.text, sizeof(name));
    str_upper(name);

    if (lex_is_reserved(name)) {
        char err[128];
        snprintf(err, sizeof(err), "%s is a reserved keyword", name);
        prog_error(ERR_SYNTAX, err);
        return;
    }

    memvar_set(&memvar_store, name, &val);
    /* No need to advance original l, we're at EOL anyway */
}

/* ---- RELEASE ---- */
static void cmd_release(const char *arg) {
    const char *p = skip_ws(arg);
    char name[MEMVAR_NAMELEN];
    int i;

    if (str_imatch(p, "ALL")) {
        const char *q = skip_ws(p + 3);
        if (str_imatch(q, "LIKE")) {
            q = skip_ws(q + 4);
            char pat[MEMVAR_NAMELEN];
            int pi = 0;
            while (*q && *q != ' ' && *q != '\t' && pi < MEMVAR_NAMELEN - 1)
                pat[pi++] = *q++;
            pat[pi] = '\0';
            str_upper(pat);
            memvar_release_matching(&memvar_store, pat, 1);
        } else if (str_imatch(q, "EXCEPT")) {
            q = skip_ws(q + 6);
            char pat[MEMVAR_NAMELEN];
            int pi = 0;
            while (*q && *q != ' ' && *q != '\t' && pi < MEMVAR_NAMELEN - 1)
                pat[pi++] = *q++;
            pat[pi] = '\0';
            str_upper(pat);
            memvar_release_matching(&memvar_store, pat, 0);
        } else {
            memvar_release_all(&memvar_store);
        }
        return;
    }

    while (*p) {
        i = 0;
        while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
            name[i++] = *p++;
        name[i] = '\0';

        if (name[0] != '\0') {
            if (memvar_release(&memvar_store, name) < 0)
                printf("Variable not found: %s\n", name);
        }

        p = skip_ws(p);
        if (*p == ',') { p++; p = skip_ws(p); }
        else break;
    }
}

/* ---- DELETE [scope] [FOR cond] ---- */
static int delete_cb(dbf_t *db, uint32_t recno, void *userdata) {
    (void)recno; (void)userdata;
    db->record_buf[0] = '*';
    db->record_dirty = 1;
    dbf_flush_record(db);
    return REC_CONTINUE;
}

static int recall_cb(dbf_t *db, uint32_t recno, void *userdata) {
    (void)recno; (void)userdata;
    db->record_buf[0] = ' ';
    db->record_dirty = 1;
    dbf_flush_record(db);
    return REC_CONTINUE;
}

static void cmd_delete(dbf_t *db, lexer_t *l) {
    clause_t c;
    int count;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    if (l->current.type == TOK_EOF) {
        if (db->current_record == 0 || expr_ctx.eof_flag) {
            printf("No current record.\n");
            return;
        }
        db->record_buf[0] = '*';
        db->record_dirty = 1;
        dbf_flush_record(db);
        printf("1 record deleted.\n");
        return;
    }

    clause_init(&c);
    c.scope.type = SCOPE_DEFAULT;
    if (parse_clauses(l, &c) < 0) return;

    count = process_records(db, &c, PROC_SKIP_DELETED_ANY | PROC_LEAVE_ON_MATCH, delete_cb, NULL);
    if (count < 0) return;
    if (count == 1) printf("1 record deleted.\n");
    else printf("%d record(s) deleted.\n", count);
}

static void cmd_recall(dbf_t *db, lexer_t *l) {
    clause_t c;
    int count;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    if (l->current.type == TOK_EOF) {
        if (db->current_record == 0 || expr_ctx.eof_flag) {
            printf("No current record.\n");
            return;
        }
        if (db->record_buf[0] == '*') {
            db->record_buf[0] = ' ';
            db->record_dirty = 1;
            dbf_flush_record(db);
        }
        printf("1 record recalled.\n");
        return;
    }

    clause_init(&c);
    c.scope.type = SCOPE_DEFAULT;
    if (parse_clauses(l, &c) < 0) return;

    count = process_records(db, &c, PROC_REQUIRE_DELETED | PROC_LEAVE_ON_MATCH, recall_cb, NULL);
    if (count < 0) return;

    if (count == 1) printf("1 record recalled.\n");
    else printf("%d record(s) recalled.\n", count);
}

/* ---- PACK ---- */
static void cmd_pack(dbf_t *db) {
    uint32_t src, dst;
    int rec_size;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    rec_size = db->record_size;
    dst = 0;

    for (src = 1; src <= db->record_count; src++) {
        dbf_read_record(db, src);
        if (db->record_buf[0] == '*') continue;
        dst++;
        if (dst != src) {
            long pos = db->header_size + (long)(dst - 1) * rec_size;
            fseek(db->fp, pos, 0);
            fwrite(db->record_buf, 1, rec_size, db->fp);
        }
    }

    db->record_count = dst;
    dbf_write_header_counts(db);
    fflush(db->fp);
    area_invalidate_all(db->filename);

    if (dst > 0) {
        dbf_read_record(db, 1);
        expr_ctx.eof_flag = 0;
    } else {
        db->current_record = 0;
        expr_ctx.eof_flag = 1;
    }

    /* Rebuild all indexes with new record numbers */
    if (cur_wa()->num_indexes > 0)
        indexes_rebuild_all(db);

    printf("%d record(s) remaining.\n", (int)dst);
}

/* ---- ZAP ---- */
static void cmd_zap(dbf_t *db) {
    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    db->record_count = 0;
    db->current_record = 0;
    db->record_dirty = 0;
    dbf_write_header_counts(db);
    area_invalidate_all(db->filename);
    expr_ctx.eof_flag = 1;

    /* Clear all active indexes */
    if (cur_wa()->num_indexes > 0)
        indexes_clear_all();

    printf("Zap complete.\n");
}

/* ---- ACCEPT ---- */
static void cmd_accept(const char *arg) {
    const char *p = skip_ws(arg);
    char line[256];
    char name[MEMVAR_NAMELEN];
    int i;
    value_t val;

    if (*p == '"' || *p == '\'') {
        char quote = *p++;
        while (*p && *p != quote) {
            putchar(*p);
            p++;
        }
        if (*p == quote) p++;
    }

    p = skip_ws(p);
    if (!str_imatch(p, "TO")) {
        printf("Syntax: ACCEPT [\"prompt\"] TO <variable>\n");
        return;
    }
    p = skip_ws(p + 2);

    i = 0;
    while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
        name[i++] = *p++;
    name[i] = '\0';

    if (name[0] == '\0') {
        printf("Syntax: ACCEPT [\"prompt\"] TO <variable>\n");
        return;
    }
    if (lex_is_reserved(name)) {
        char err[128];
        snprintf(err, sizeof(err), "%s is a reserved keyword", name);
        prog_error(ERR_SYNTAX, err);
        return;
    }

    if (read_line(line, sizeof(line)) < 0)
        line[0] = '\0';

    val = val_str(line);
    memvar_set(&memvar_store, name, &val);
}

/* ---- INPUT ---- */
static void cmd_input(const char *arg) {
    const char *p = skip_ws(arg);
    char line[256];
    char name[MEMVAR_NAMELEN];
    int i;
    value_t val;

    if (*p == '"' || *p == '\'') {
        char quote = *p++;
        while (*p && *p != quote) {
            putchar(*p);
            p++;
        }
        if (*p == quote) p++;
    }

    p = skip_ws(p);
    if (!str_imatch(p, "TO")) {
        printf("Syntax: INPUT [\"prompt\"] TO <variable>\n");
        return;
    }
    p = skip_ws(p + 2);

    i = 0;
    while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
        name[i++] = *p++;
    name[i] = '\0';

    if (name[0] == '\0') {
        printf("Syntax: INPUT [\"prompt\"] TO <variable>\n");
        return;
    }
    if (lex_is_reserved(name)) {
        char err[128];
        snprintf(err, sizeof(err), "%s is a reserved keyword", name);
        prog_error(ERR_SYNTAX, err);
        return;
    }

    if (read_line(line, sizeof(line)) < 0)
        line[0] = '\0';

    ctx_setup();
    if (expr_eval_str(&expr_ctx, line, &val) != 0) {
        report_expr_error();
        val = val_num(0);
    }

    memvar_set(&memvar_store, name, &val);
}

/* ---- WAIT ---- */
static void cmd_wait(const char *arg) {
    const char *p = skip_ws(arg);
    char name[MEMVAR_NAMELEN];
    int has_to = 0;
    int c;

    if (*p == '"' || *p == '\'') {
        char quote = *p++;
        while (*p && *p != quote) {
            putchar(*p);
            p++;
        }
        if (*p == quote) p++;
        p = skip_ws(p);
    } else if (*p == '\0' || str_imatch(p, "TO")) {
        printf("Press any key to continue...");
    }

    if (str_imatch(p, "TO")) {
        int i;
        p = skip_ws(p + 2);
        i = 0;
        while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
            name[i++] = *p++;
        name[i] = '\0';
        if (name[0] != '\0') {
            if (lex_is_reserved(name)) {
                char err[128];
                snprintf(err, sizeof(err), "%s is a reserved keyword", name);
                prog_error(ERR_SYNTAX, err);
                return;
            }
            has_to = 1;
        }
    }

    c = getchar();
    if (c == -1) c = 0;
    if (c != '\n') {
        int next = getchar();
        (void)next;
    }

    if (has_to) {
        char ch[2];
        value_t val;
        ch[0] = (c == '\n') ? ' ' : (char)c;
        ch[1] = '\0';
        val = val_str(ch);
        memvar_set(&memvar_store, name, &val);
    }
    printf("\n");
}

/* ---- ? / ?? (print expressions) ---- */
static void cmd_print_expr(const char *arg, int newline) {
    const char *p;
    int first = 1;
    int is_print = (cmd_get_device() == 1);

    p = skip_ws(arg);

    if (*p == '\0') {
        if (newline) {
            if (is_print) screen_print_newline();
            else printf("\n");
        }
        return;
    }

    while (*p) {
        value_t val;
        char buf[256];

        p = skip_ws(p);
        if (*p == '\0') break;

        if (expr_eval(&expr_ctx, &p, &val) != 0) {
            report_expr_error();
            return;
        }

        val_to_string(&val, buf, sizeof(buf));
        if (!first) {
            if (is_print) screen_print_text(" ");
            else printf(" ");
        }
        if (is_print) screen_print_text(buf);
        else printf("%s", buf);
        first = 0;

        p = skip_ws(p);
        if (*p == ',') {
            p++;
        }
    }

    if (newline) {
        if (is_print) screen_print_newline();
        else printf("\n");
    }
}

/* ---- COPY TO [FIELDS ...] [scope] [FOR cond] ---- */
static void cmd_copy_to(dbf_t *db, const char *arg) {
    char filename[64];
    const char *p;
    int field_indices[DBF_MAX_FIELDS];
    int nfields = 0;
    int use_all_fields;
    scope_t scope;
    const char *cond_str = NULL;
    uint32_t start, end, i;
    dbf_t dest;
    dbf_field_t dest_fields[DBF_MAX_FIELDS];
    int dest_nfields;
    int count = 0;
    int f;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    p = parse_filename(arg, filename, sizeof(filename));

    if (filename[0] == '\0' || str_icmp(filename, ".DBF") == 0) {
        printf("Syntax: COPY TO <filename> [FIELDS f1,f2,...] [scope] [FOR cond]\n");
        return;
    }

    p = skip_ws(p);

    /* Parse optional FIELDS clause */
    if (str_imatch(p, "FIELDS")) {
        p = skip_ws(p + 6);
        nfields = parse_field_list(db, p, field_indices, DBF_MAX_FIELDS, &p);
        if (nfields < 0) return;
        p = skip_ws(p);
    }

    /* Parse scope */
    scope = parse_scope(&p);

    /* Parse FOR */
    if (str_imatch(p, "FOR")) {
        cond_str = skip_ws(p + 3);
    }

    use_all_fields = (nfields == 0);

    /* Build dest field list */
    if (use_all_fields) {
        dest_nfields = db->field_count;
        for (f = 0; f < dest_nfields; f++)
            dest_fields[f] = db->fields[f];
    } else {
        dest_nfields = nfields;
        for (f = 0; f < nfields; f++)
            dest_fields[f] = db->fields[field_indices[f]];
    }

    /* Recompute offsets for dest */
    {
        uint16_t off = 0;
        for (f = 0; f < dest_nfields; f++) {
            dest_fields[f].offset = off;
            off += dest_fields[f].length;
        }
    }

    if (dbf_create(filename, dest_fields, dest_nfields) < 0) {
        printf("Error creating %s\n", filename);
        return;
    }

    dbf_init(&dest);
    if (dbf_open(&dest, filename) < 0) {
        printf("Error opening %s\n", filename);
        return;
    }

    if (scope_bounds(db, &scope, &start, &end) < 0) {
        printf("Invalid scope.\n");
        dbf_close(&dest);
        return;
    }

    for (i = start; i <= end; i++) {
        char raw[256];
        dbf_read_record(db, i);
        if (skip_deleted(db->record_buf) || !check_filter(db)) continue;

        if (cond_str) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, cond_str, &cond) != 0) {
                report_expr_error();
                dbf_close(&dest);
                return;
            }
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }

        dbf_append_blank(&dest);

        if (use_all_fields) {
            for (f = 0; f < db->field_count; f++) {
                dbf_get_field_raw(db, f, raw, sizeof(raw));
                dbf_set_field_raw(&dest, f, raw);
            }
        } else {
            for (f = 0; f < nfields; f++) {
                dbf_get_field_raw(db, field_indices[f], raw, sizeof(raw));
                dbf_set_field_raw(&dest, f, raw);
            }
        }
        dbf_flush_record(&dest);
        count++;
    }

    dbf_close(&dest);
    printf("%d record(s) copied.\n", count);
}

/* ---- COPY STRUCTURE TO ---- */
static void cmd_copy_structure(dbf_t *db, const char *arg) {
    char filename[64];

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    parse_filename(arg, filename, sizeof(filename));

    if (filename[0] == '\0' || str_icmp(filename, ".DBF") == 0) {
        printf("Syntax: COPY STRUCTURE TO <filename>\n");
        return;
    }

    /* Recompute offsets for create */
    {
        dbf_field_t fields[DBF_MAX_FIELDS];
        int f;
        uint16_t off = 0;
        for (f = 0; f < db->field_count; f++) {
            fields[f] = db->fields[f];
            fields[f].offset = off;
            off += fields[f].length;
        }
        if (dbf_create(filename, fields, db->field_count) < 0) {
            printf("Error creating %s\n", filename);
            return;
        }
    }

    printf("Structure copied to %s.\n", filename);
}

/* ---- APPEND FROM [FOR cond] ---- */
static void cmd_append_from(dbf_t *db, const char *arg) {
    char filename[64];
    const char *p;
    const char *cond_str = NULL;
    dbf_t source;
    uint32_t i;
    int count = 0;
    dbf_t *saved_db;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    p = parse_filename(arg, filename, sizeof(filename));

    if (filename[0] == '\0' || str_icmp(filename, ".DBF") == 0) {
        printf("Syntax: APPEND FROM <filename> [FOR cond]\n");
        return;
    }

    p = skip_ws(p);
    if (str_imatch(p, "FOR")) {
        cond_str = skip_ws(p + 3);
    }

    dbf_init(&source);
    if (dbf_open(&source, filename) < 0) {
        file_not_found(filename);
        return;
    }

    /* FOR evaluates against source records */
    saved_db = expr_ctx.db;

    for (i = 1; i <= source.record_count; i++) {
        int f;
        char raw[256];

        dbf_read_record(&source, i);
        if (source.record_buf[0] == '*') continue;

        if (cond_str) {
            value_t cond;
            expr_ctx.db = &source;
            if (expr_eval_str(&expr_ctx, cond_str, &cond) != 0) {
                report_expr_error();
                expr_ctx.db = saved_db;
                dbf_close(&source);
                return;
            }
            expr_ctx.db = saved_db;
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }

        dbf_append_blank(db);

        /* Copy matching fields by name */
        for (f = 0; f < source.field_count; f++) {
            int didx = dbf_find_field(db, source.fields[f].name);
            if (didx < 0) continue;
            /* Re-read source record since append_blank may have flushed db */
            dbf_read_record(&source, i);
            dbf_get_field_raw(&source, f, raw, sizeof(raw));
            dbf_set_field_raw(db, didx, raw);
        }
        dbf_flush_record(db);
        indexes_insert_current(db);
        count++;
    }

    dbf_close(&source);
    printf("%d record(s) appended.\n", count);
}

/* ---- SORT TO file ON field [/A][/D][/C] [scope] [FOR cond] ---- */

/* ---- SORT TO file ON field [/A][/D][/C] [scope] [FOR cond] ---- */

typedef struct {
    uint32_t recno;
    char key[256];
} sort_entry_t;

typedef struct {
    int ascending;
    int case_insensitive;
} sort_ctx_t;

static int sort_compare_r(const void *a, const void *b, void *arg) {
    const sort_entry_t *sa = (const sort_entry_t *)a;
    const sort_entry_t *sb = (const sort_entry_t *)b;
    sort_ctx_t *ctx = (sort_ctx_t *)arg;
    int cmp;
    if (ctx->case_insensitive)
        cmp = str_icmp(sa->key, sb->key);
    else
        cmp = strcmp(sa->key, sb->key);
    return ctx->ascending ? cmp : -cmp;
}

#define MAX_SORT_ENTRIES 2000
#define MAX_SORT_CHUNKS 64

static int make_sort_chunk_name(char *out, size_t out_size) {
    static unsigned counter;
    unsigned attempt = 0;
    unsigned seed = (unsigned)time(NULL);
    while (attempt < 1000) {
        unsigned id = seed + counter++;
        snprintf(out, out_size, "sort_%08x.tmp", id);
        FILE *tf = fopen(out, "rb");
        if (!tf) return 1;
        fclose(tf);
        attempt++;
    }
    return 0;
}

static void cleanup_sort_chunks(char chunk_names[][64], int nchunks) {
    int j;
    for (j = 0; j < nchunks; j++) {
        remove(chunk_names[j]);
    }
}

static void cmd_sort(dbf_t *db, const char *arg) {
    char filename[64];
    char field_name[DBF_MAX_FIELD_NAME];
    const char *p;
    int field_idx;
    int nentries = 0;
    scope_t scope;
    const char *cond_str = NULL;
    uint32_t start, end, i;
    dbf_t dest;
    int count = 0;
    int f;
    sort_ctx_t ctx;
    sort_entry_t *sort_entries;
    int nchunks = 0;
    char chunk_names[MAX_SORT_CHUNKS][64];
    int chunk_overflow = 0;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    /* Parse: SORT TO <filename> ON <field> [/A][/D][/C] [scope] [FOR cond] */
    p = parse_filename(arg, filename, sizeof(filename));

    if (filename[0] == '\0' || str_icmp(filename, ".DBF") == 0) {
        printf("Syntax: SORT TO <filename> ON <field> [/A][/D][/C]\n");
        return;
    }

    p = skip_ws(p);
    if (!str_imatch(p, "ON")) {
        printf("Syntax: SORT TO <filename> ON <field> [/A][/D][/C]\n");
        return;
    }
    p = skip_ws(p + 2);

    /* Parse field name */
    {
        int fi = 0;
        while (is_ident_char(*p) && fi < DBF_MAX_FIELD_NAME - 1)
            field_name[fi++] = *p++;
        field_name[fi] = '\0';
    }
    str_upper(field_name);

    field_idx = dbf_find_field(db, field_name);
    if (field_idx < 0) {
        printf("Field not found: %s\n", field_name);
        return;
    }

    /* Parse flags */
    ctx.ascending = 1;
    ctx.case_insensitive = 0;
    p = skip_ws(p);
    while (*p == '/') {
        p++;
        char fc = *p;
        if (fc >= 'a' && fc <= 'z') fc -= 32;
        if (fc == 'A') ctx.ascending = 1;
        else if (fc == 'D') ctx.ascending = 0;
        else if (fc == 'C') ctx.case_insensitive = 1;
        p++;
        p = skip_ws(p);
    }

    /* Parse scope */
    scope = parse_scope(&p);

    /* Parse FOR */
    if (str_imatch(p, "FOR")) {
        cond_str = skip_ws(p + 3);
    }

    if (scope_bounds(db, &scope, &start, &end) < 0) {
        printf("Invalid scope.\n");
        return;
    }

    sort_entries = (sort_entry_t *)malloc(MAX_SORT_ENTRIES * sizeof(sort_entry_t));
    if (!sort_entries) {
        printf("Out of memory for sort buffer.\n");
        return;
    }

    /* Phase 1: Create sorted chunks */
    for (i = start; i <= end; i++) {
        char raw[256];
        dbf_read_record(db, i);
        if (skip_deleted(db->record_buf) || !check_filter(db)) continue;

        if (cond_str) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, cond_str, &cond) != 0) {
                report_expr_error();
                free(sort_entries);
                cleanup_sort_chunks(chunk_names, nchunks);
                return;
            }
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }

        sort_entries[nentries].recno = i;
        dbf_get_field_raw(db, field_idx, raw, sizeof(raw));
        str_copy(sort_entries[nentries].key, raw, sizeof(sort_entries[nentries].key));
        nentries++;

        if (nentries == MAX_SORT_ENTRIES) {
            /* Sort and write chunk */
            qsort_r(sort_entries, nentries, sizeof(sort_entry_t), sort_compare_r, &ctx);
            if (!make_sort_chunk_name(chunk_names[nchunks], sizeof(chunk_names[nchunks]))) {
                printf("Error creating sort chunk.\n");
                chunk_overflow = 1;
                break;
            }
            FILE *tf = fopen(chunk_names[nchunks], "wb");
            if (tf) {
                fwrite(sort_entries, sizeof(sort_entry_t), nentries, tf);
                fclose(tf);
                nchunks++;
                nentries = 0;
                if (nchunks >= MAX_SORT_CHUNKS) {
                    printf("Too many sort chunks.\n");
                    chunk_overflow = 1;
                    break;
                }
            } else {
                printf("Error creating sort chunk.\n");
                chunk_overflow = 1;
                break;
            }
        }
    }
    if (chunk_overflow) {
        free(sort_entries);
        cleanup_sort_chunks(chunk_names, nchunks);
        return;
    }

    /* Final chunk */
    if (nentries > 0) {
        qsort_r(sort_entries, nentries, sizeof(sort_entry_t), sort_compare_r, &ctx);
        if (nchunks == 0) {
            /* All fit in memory, proceed directly to write phase below */
        } else {
            if (!make_sort_chunk_name(chunk_names[nchunks], sizeof(chunk_names[nchunks]))) {
                printf("Error creating sort chunk.\n");
                free(sort_entries);
                cleanup_sort_chunks(chunk_names, nchunks);
                return;
            }
            FILE *tf = fopen(chunk_names[nchunks], "wb");
            if (!tf) {
                printf("Error creating sort chunk.\n");
                free(sort_entries);
                cleanup_sort_chunks(chunk_names, nchunks);
                return;
            }
            fwrite(sort_entries, sizeof(sort_entry_t), nentries, tf);
            fclose(tf);
            nchunks++;
            nentries = 0;
        }
    }

    if (nchunks == 0 && nentries == 0) {
        printf("No records to sort.\n");
        free(sort_entries);
        return;
    }

    /* Phase 2: Create destination file */
    {
        dbf_field_t fields[DBF_MAX_FIELDS];
        uint16_t off = 0;
        for (f = 0; f < db->field_count; f++) {
            fields[f] = db->fields[f];
            fields[f].offset = off;
            off += fields[f].length;
        }
        if (dbf_create(filename, fields, db->field_count) < 0) {
            printf("Error creating %s\n", filename);
            free(sort_entries);
            cleanup_sort_chunks(chunk_names, nchunks);
            return;
        }
    }

    dbf_init(&dest);
    if (dbf_open(&dest, filename) < 0) {
        printf("Error opening %s\n", filename);
        free(sort_entries);
        cleanup_sort_chunks(chunk_names, nchunks);
        return;
    }

    if (nchunks == 0) {
        /* In-memory sort result */
        for (i = 0; i < (uint32_t)nentries; i++) {
            char raw[256];
            dbf_read_record(db, sort_entries[i].recno);
            dbf_append_blank(&dest);
            for (f = 0; f < db->field_count; f++) {
                dbf_get_field_raw(db, f, raw, sizeof(raw));
                dbf_set_field_raw(&dest, f, raw);
            }
            dbf_flush_record(&dest);
            count++;
        }
    } else {
        /* Phase 3: K-way Merge */
        FILE *tfs[MAX_SORT_CHUNKS];
        sort_entry_t current[MAX_SORT_CHUNKS];
        int active[MAX_SORT_CHUNKS];
        int active_count = 0;

        for (int j = 0; j < nchunks; j++) {
            tfs[j] = fopen(chunk_names[j], "rb");
            if (tfs[j] && fread(&current[j], sizeof(sort_entry_t), 1, tfs[j]) == 1) {
                active[j] = 1;
                active_count++;
            } else {
                active[j] = 0;
                if (tfs[j]) fclose(tfs[j]);
            }
        }

        while (active_count > 0) {
            int best = -1;
            for (int j = 0; j < nchunks; j++) {
                if (!active[j]) continue;
                if (best == -1 || sort_compare_r(&current[j], &current[best], &ctx) < 0) {
                    best = j;
                }
            }

            if (best != -1) {
                /* Write best record to dest */
                char raw[256];
                dbf_read_record(db, current[best].recno);
                dbf_append_blank(&dest);
                for (f = 0; f < db->field_count; f++) {
                    dbf_get_field_raw(db, f, raw, sizeof(raw));
                    dbf_set_field_raw(&dest, f, raw);
                }
                dbf_flush_record(&dest);
                count++;

                /* Read next from that chunk */
                if (fread(&current[best], sizeof(sort_entry_t), 1, tfs[best]) != 1) {
                    active[best] = 0;
                    active_count--;
                    fclose(tfs[best]);
                    remove(chunk_names[best]);
                }
            }
        }
        /* Cleanup any unopened/empty chunks */
        cleanup_sort_chunks(chunk_names, nchunks);
    }

    dbf_close(&dest);
    free(sort_entries);
    printf("%d record(s) sorted.\n", count);
}

/* ---- ERASE <file> ---- */
static void cmd_erase(const char *arg) {
    char filename[64];
    const char *p = skip_ws(arg);
    int i = 0;
    while (*p && *p != ' ' && *p != '\t' && i < 63)
        filename[i++] = *p++;
    filename[i] = '\0';
    if (filename[0] == '\0') {
        printf("Syntax: ERASE <filename>\n");
        return;
    }
    if (remove(filename) != 0) {
        file_not_found(filename);
    } else {
        printf("File erased.\n");
    }
}

/* ---- RENAME <old> TO <new> ---- */
static void cmd_rename(const char *arg) {
    char oldname[64], newname[64];
    const char *p = skip_ws(arg);
    int i = 0;
    while (*p && *p != ' ' && *p != '\t' && i < 63)
        oldname[i++] = *p++;
    oldname[i] = '\0';
    p = skip_ws(p);
    if (!str_imatch(p, "TO")) {
        printf("Syntax: RENAME <oldfile> TO <newfile>\n");
        return;
    }
    p = skip_ws(p + 2);
    i = 0;
    while (*p && *p != ' ' && *p != '\t' && i < 63)
        newname[i++] = *p++;
    newname[i] = '\0';
    if (oldname[0] == '\0' || newname[0] == '\0') {
        printf("Syntax: RENAME <oldfile> TO <newfile>\n");
        return;
    }
    /* Try native rename first, fall back to copy+delete */
    if (rename(oldname, newname) != 0) {
        FILE *fin = fopen(oldname, "rb");
        if (!fin) {
            file_not_found(oldname);
            return;
        }
        {
            FILE *fout = fopen(newname, "wb");
            char buf[512];
            int n;
            if (!fout) {
                fclose(fin);
                printf("Cannot create: %s\n", newname);
                return;
            }
            while ((n = fread(buf, 1, sizeof(buf), fin)) > 0)
                fwrite(buf, 1, n, fout);
            fclose(fin);
            fclose(fout);
            remove(oldname);
        }
        printf("File renamed.\n");
    } else {
        printf("File renamed.\n");
    }
}

/* ---- COPY FILE <src> TO <dest> ---- */
static void cmd_copy_file(const char *arg) {
    char src[64], dst[64];
    const char *p = skip_ws(arg);
    int i = 0;
    FILE *fin, *fout;
    char buf[512];
    int n;

    while (*p && *p != ' ' && *p != '\t' && i < 63)
        src[i++] = *p++;
    src[i] = '\0';
    p = skip_ws(p);
    if (!str_imatch(p, "TO")) {
        printf("Syntax: COPY FILE <source> TO <destination>\n");
        return;
    }
    p = skip_ws(p + 2);
    i = 0;
    while (*p && *p != ' ' && *p != '\t' && i < 63)
        dst[i++] = *p++;
    dst[i] = '\0';
    if (src[0] == '\0' || dst[0] == '\0') {
        printf("Syntax: COPY FILE <source> TO <destination>\n");
        return;
    }
    fin = fopen(src, "rb");
    if (!fin) {
        file_not_found(src);
        return;
    }
    fout = fopen(dst, "wb");
    if (!fout) {
        fclose(fin);
        printf("Cannot create: %s\n", dst);
        return;
    }
    while ((n = fread(buf, 1, sizeof(buf), fin)) > 0)
        fwrite(buf, 1, n, fout);
    fclose(fin);
    fclose(fout);
    printf("File copied.\n");
}

/* ---- INDEX ON <expr> TO <file> ---- */
static void cmd_index_on(dbf_t *db, const char *arg) {
    const char *p = skip_ws(arg);
    char key_expr[256];
    char filename[64];
    const char *to_pos = NULL;
    const char *f;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    /* Allow optional leading ON (when called with remaining input) */
    if (str_imatch(p, "ON")) {
        p = skip_ws(p + 2);
    }

    /* Find TO keyword */
    f = p;
    while (*f) {
        if (str_imatch(f, "TO")) { to_pos = f; break; }
        f++;
    }

    if (!to_pos) {
        printf("Syntax: INDEX ON <expr> TO <filename>\n");
        return;
    }

    {
        int len = (int)(to_pos - p);
        if (len > (int)sizeof(key_expr) - 1) len = (int)sizeof(key_expr) - 1;
        memcpy(key_expr, p, len);
        key_expr[len] = '\0';
        trim_right(key_expr);
    }

    f = skip_ws(to_pos + 2);
    {
        int i = 0;
        while (*f && *f != ' ' && *f != '\t' && i < 63)
            filename[i++] = *f++;
        filename[i] = '\0';
    }

    ensure_ndx_ext(filename, sizeof(filename));

    if (filename[0] == '\0' || str_icmp(filename, ".NDX") == 0) {
        printf("Syntax: INDEX ON <expr> TO <filename>\n");
        return;
    }

    /* Close all existing indexes and build fresh */
    close_all_indexes(cur_wa());

    /* Build the index in slot 0 */
    if (index_build(&cur_wa()->indexes[0], db, &expr_ctx, key_expr, filename) < 0) {
        prog_error(ERR_FILE_IO, "Error building index");
        return;
    }

    /* Write to file */
    if (index_write(&cur_wa()->indexes[0]) < 0) {
        prog_error(ERR_FILE_IO, "Error writing index file");
        return;
    }

    cur_wa()->num_indexes = 1;
    cur_wa()->order = 1;

    /* Position to first indexed record */
    if (cur_wa()->indexes[0].nentries > 0) {
        uint32_t rec;
        index_top(&cur_wa()->indexes[0]);
        rec = index_current_recno(&cur_wa()->indexes[0]);
        if (rec > 0) dbf_read_record(db, rec);
        expr_ctx.eof_flag = 0;
        expr_ctx.bof_flag = 0;
    }

    printf("%d record(s) indexed.\n", cur_wa()->indexes[0].nentries);
}

/* ---- SET INDEX TO [file1 [, file2 ...]] ---- */
static void cmd_set_index(dbf_t *db, const char *arg) {
    const char *p = skip_ws(arg);

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    /* Close all existing indexes */
    close_all_indexes(cur_wa());

    if (*p == '\0') {
        /* SET INDEX TO with no args = close all */
        printf("Index closed.\n");
        return;
    }

    /* Parse comma-separated index files */
    while (*p && cur_wa()->num_indexes < MAX_INDEXES) {
        char filename[64];
        int i = 0;
        while (*p && *p != ' ' && *p != '\t' && *p != ',' && i < 63)
            filename[i++] = *p++;
        filename[i] = '\0';

        if (filename[0]) {
            int slot = cur_wa()->num_indexes;
            ensure_ndx_ext(filename, sizeof(filename));
            if (index_read(&cur_wa()->indexes[slot], filename) == 0) {
                cur_wa()->num_indexes++;
                printf("Index %s opened (%d entries).\n", filename,
                       cur_wa()->indexes[slot].nentries);
            } else {
                printf("Cannot open index: %s\n", filename);
            }
        }

        p = skip_ws(p);
        if (*p == ',') { p++; p = skip_ws(p); }
        else break;
    }

    /* Set controlling index to first one */
    if (cur_wa()->num_indexes > 0) {
        index_t *idx;
        uint32_t rec;
        cur_wa()->order = 1;

        /* Position to first indexed record */
        idx = controlling_index();
        if (idx && idx->nentries > 0) {
            index_top(idx);
            rec = index_current_recno(idx);
            if (rec > 0) dbf_read_record(db, rec);
            expr_ctx.eof_flag = 0;
            expr_ctx.bof_flag = 0;
        }
    }
}

/* ---- SEEK <expr> ---- */
static void cmd_seek(dbf_t *db, const char *arg) {
    const char *p = skip_ws(arg);
    value_t val;
    char key[MAX_INDEX_KEY + 1];
    index_t *idx;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    idx = controlling_index();
    if (!idx) {
        printf("No index active.\n");
        return;
    }

    if (expr_eval_str(&expr_ctx, p, &val) != 0) {
        report_expr_error();
        return;
    }

    val_to_string(&val, key, sizeof(key));
    trim_right(key);

    if (index_seek(idx, key)) {
        uint32_t rec = index_current_recno(idx);
        expr_ctx.found = 1;
        if (rec > 0) {
            dbf_read_record(db, rec);
            expr_ctx.eof_flag = 0;
            expr_ctx.bof_flag = 0;
        }
    } else {
        expr_ctx.found = 0;
        {
            uint32_t rec = index_current_recno(idx);
            if (rec > 0) {
                dbf_read_record(db, rec);
                expr_ctx.eof_flag = 0;
            } else {
                expr_ctx.eof_flag = 1;
            }
        }
        if (set_opts.talk) printf("Not found.\n");
    }
}

/* ---- FIND <literal> ---- */
static void cmd_find(dbf_t *db, const char *arg) {
    const char *p = skip_ws(arg);
    char key[MAX_INDEX_KEY + 1];
    index_t *idx;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    idx = controlling_index();
    if (!idx) {
        printf("No index active.\n");
        return;
    }

    str_copy(key, p, sizeof(key));
    trim_right(key);

    if (index_seek(idx, key)) {
        uint32_t rec = index_current_recno(idx);
        expr_ctx.found = 1;
        if (rec > 0) {
            dbf_read_record(db, rec);
            expr_ctx.eof_flag = 0;
            expr_ctx.bof_flag = 0;
        }
    } else {
        expr_ctx.found = 0;
        {
            uint32_t rec = index_current_recno(idx);
            if (rec > 0) {
                dbf_read_record(db, rec);
                expr_ctx.eof_flag = 0;
            } else {
                expr_ctx.eof_flag = 1;
            }
        }
        if (set_opts.talk) printf("Not found.\n");
    }
}

/* ---- REINDEX ---- */
static void cmd_reindex(dbf_t *db) {
    work_area_t *wa = cur_wa();
    int i, total = 0;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    if (wa->num_indexes == 0) {
        printf("No index active.\n");
        return;
    }

    for (i = 0; i < wa->num_indexes; i++) {
        index_t *idx = &wa->indexes[i];
        char key_expr[256];
        char filename[64];
        str_copy(key_expr, idx->key_expr, sizeof(key_expr));
        str_copy(filename, idx->filename, sizeof(filename));

        if (index_build(idx, db, &expr_ctx, key_expr, filename) < 0) {
            printf("Error rebuilding index %s.\n", filename);
            return;
        }

        if (index_write(idx) < 0) {
            printf("Error writing index file %s.\n", filename);
            return;
        }
        total += idx->nentries;
    }

    /* Position to first entry in controlling index */
    {
        index_t *ctrl = controlling_index();
        if (ctrl && ctrl->nentries > 0) {
            uint32_t rec;
            index_top(ctrl);
            rec = index_current_recno(ctrl);
            if (rec > 0) dbf_read_record(db, rec);
            expr_ctx.eof_flag = 0;
            expr_ctx.bof_flag = 0;
        }
    }

    printf("%d record(s) reindexed.\n", total);
}

/* ---- Close all work areas ---- */
void cmd_close_all(void) {
    int i;
    for (i = 0; i < MAX_AREAS; i++) {
        work_area_t *wa = area_get(i);
        if (dbf_is_open(&wa->db))
            dbf_close(&wa->db);
        close_all_indexes(wa);
        wa->alias[0] = '\0';
        wa->locate_cond[0] = '\0';
        wa->locate_last_rec = 0;
        wa->filter_cond[0] = '\0';
        wa->relation_expr[0] = '\0';
        wa->relation_target = -1;
    }
    area_set_current_idx(0);
}

/* ---- Helper: ensure filename has .FRM extension ---- */
static void ensure_frm_ext(char *filename, int size) {
    str_upper(filename);
    trim_right(filename);
    if (strlen(filename) < 5 || str_icmp(filename + strlen(filename) - 4, ".FRM") != 0) {
        if ((int)strlen(filename) + 4 < size)
            strcat(filename, ".FRM");
    }
}

/* ---- Helper: ensure filename has .LBL extension ---- */
static void ensure_lbl_ext(char *filename, int size) {
    str_upper(filename);
    trim_right(filename);
    if (strlen(filename) < 5 || str_icmp(filename + strlen(filename) - 4, ".LBL") != 0) {
        if ((int)strlen(filename) + 4 < size)
            strcat(filename, ".LBL");
    }
}

/* ---- CREATE REPORT ---- */
static void cmd_create_report(const char *arg) {
    char filename[64];
    char line[256];
    frm_def_t def;

    arg = skip_ws(arg);
    if (*arg == '\0') {
        printf("Syntax: CREATE REPORT <filename>\n");
        return;
    }

    str_copy(filename, arg, sizeof(filename));
    ensure_frm_ext(filename, sizeof(filename));

    frm_init(&def);

    /* Page title */
    printf("Page title? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    str_copy(def.title, line, sizeof(def.title));

    /* Page width */
    printf("Page width (80)? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    if (line[0]) def.page_width = atoi(line);

    /* Left margin */
    printf("Left margin (8)? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    if (line[0]) def.left_margin = atoi(line);

    /* Right margin */
    printf("Right margin (0)? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    if (line[0]) def.right_margin = atoi(line);

    /* Lines per page */
    printf("Lines per page (58)? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    if (line[0]) def.lines_per_page = atoi(line);

    /* Double space */
    printf("Double space report? (Y/N) ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    str_upper(line);
    def.double_space = (line[0] == 'Y') ? 1 : 0;

    /* Group on */
    printf("Group on expression? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    str_copy(def.group_expr, line, sizeof(def.group_expr));

    if (def.group_expr[0]) {
        printf("Group heading? ");
        if (read_line(line, sizeof(line)) < 0) return;
        trim_right(line);
        str_copy(def.group_header, line, sizeof(def.group_header));
    }

    /* Sub-group on */
    printf("Sub-group on expression? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    str_copy(def.subgroup_expr, line, sizeof(def.subgroup_expr));

    if (def.subgroup_expr[0]) {
        printf("Sub-group heading? ");
        if (read_line(line, sizeof(line)) < 0) return;
        trim_right(line);
        str_copy(def.subgroup_header, line, sizeof(def.subgroup_header));
    }

    /* Summary only */
    printf("Summary report only? (Y/N) ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    str_upper(line);
    def.summary_only = (line[0] == 'Y') ? 1 : 0;

    /* Eject after */
    printf("Page eject after report? (Y/N) ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    str_upper(line);
    def.eject_after = (line[0] == 'Y') ? 1 : 0;

    /* Columns */
    while (def.num_columns < FRM_MAX_COLUMNS) {
        int col = def.num_columns;

        printf("Col %2d:\n", col + 1);
        printf("  Contents? ");
        if (read_line(line, sizeof(line)) < 0) break;
        trim_right(line);
        if (line[0] == '\0') break;
        str_copy(def.columns[col].content, line, sizeof(def.columns[col].content));

        printf("  Heading? ");
        if (read_line(line, sizeof(line)) < 0) break;
        trim_right(line);
        str_copy(def.columns[col].header, line, sizeof(def.columns[col].header));

        printf("  Width? ");
        if (read_line(line, sizeof(line)) < 0) break;
        trim_right(line);
        def.columns[col].width = atoi(line);
        if (def.columns[col].width < 1) def.columns[col].width = 10;

        printf("  Decimal places? ");
        if (read_line(line, sizeof(line)) < 0) break;
        trim_right(line);
        def.columns[col].decimals = atoi(line);

        if (def.columns[col].decimals > 0) {
            printf("  Totals? (Y/N) ");
            if (read_line(line, sizeof(line)) < 0) break;
            trim_right(line);
            str_upper(line);
            def.columns[col].totals = (line[0] == 'Y') ? 1 : 0;
        }

        def.num_columns++;
    }

    if (def.num_columns == 0) {
        printf("No columns defined.\n");
        return;
    }

    if (frm_write(filename, &def) < 0) {
        printf("Error creating %s\n", filename);
        return;
    }
    printf("Report definition %s created.\n", filename);
}

static int report_kw_boundary(const char *p, const char *kw) {
    int n = (int)strlen(kw);
    if (!str_imatch(p, kw)) return 0;
    if (p[n] == '\0' || p[n] == ' ' || p[n] == '\t') return 1;
    return 0;
}

static char *report_find_next_kw(char *p) {
    char *s = p;
    while (*s) {
        if (s == p || s[-1] == ' ' || s[-1] == '\t') {
            if (report_kw_boundary(s, "FOR") ||
                report_kw_boundary(s, "HEADING") ||
                report_kw_boundary(s, "PLAIN") ||
                report_kw_boundary(s, "SUMMARY") ||
                report_kw_boundary(s, "NOEJECT") ||
                report_kw_boundary(s, "TO")) {
                return s;
            }
        }
        s++;
    }
    return NULL;
}

/* ---- REPORT FORM ---- */
static void cmd_report_form(dbf_t *db, lexer_t *l) {
    char filename[64];
    FILE *outfile = NULL;
    frm_def_t def;
    clause_t c;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    if (l->current.type != TOK_IDENT) {
        printf("Syntax: REPORT FORM <filename> [scope] [FOR <cond>] [WHILE <cond>] [HEADING <expr>] [PLAIN] [SUMMARY] [NOEJECT] [TO PRINT] [TO FILE <file>]\n");
        return;
    }

    str_copy(filename, l->current.text, sizeof(filename));
    ensure_frm_ext(filename, sizeof(filename));
    lex_next(l);

    clause_init(&c);
    if (parse_clauses(l, &c) < 0) return;

    if (c.to_file[0]) {
        outfile = fopen(c.to_file, "w");
        if (!outfile) {
            printf("Cannot create %s\n", c.to_file);
            return;
        }
    }

    /* Read the .FRM file */
    if (frm_read(filename, &def) < 0) {
        file_not_found(filename);
        if (outfile) fclose(outfile);
        return;
    }

    /* Determine output destination */
    if (!outfile)
        outfile = stdout;

    ctx_setup();
    /* Note: currently report_generate ignores WHILE/scope, passing them would require signature change */
    report_generate(&def, db, &expr_ctx, c.for_cond[0] ? c.for_cond : NULL,
                    c.heading[0] ? c.heading : NULL,
                    c.plain, c.summary, c.noeject, outfile);

    if (outfile != stdout)
        fclose(outfile);
}

/* ---- CREATE LABEL ---- */
static void cmd_create_label(const char *arg) {
    char filename[64];
    char line[256];
    lbl_def_t def;
    int i;

    arg = skip_ws(arg);
    if (*arg == '\0') {
        printf("Syntax: CREATE LABEL <filename>\n");
        return;
    }

    str_copy(filename, arg, sizeof(filename));
    ensure_lbl_ext(filename, sizeof(filename));

    lbl_init(&def);

    /* Remark */
    printf("Label remark? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    str_copy(def.remark, line, sizeof(def.remark));

    /* Height */
    printf("Label height (5)? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    if (line[0]) def.height = atoi(line);
    if (def.height < 1) def.height = 1;
    if (def.height > LBL_MAX_LINES) def.height = LBL_MAX_LINES;

    /* Width */
    printf("Label width (35)? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    if (line[0]) def.width = atoi(line);
    if (def.width < 1) def.width = 1;

    /* Left margin */
    printf("Left margin (0)? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    if (line[0]) def.left_margin = atoi(line);

    /* Lines between */
    printf("Lines between labels (1)? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    if (line[0]) def.lines_between = atoi(line);

    /* Spaces between */
    printf("Spaces between labels (0)? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    if (line[0]) def.spaces_between = atoi(line);

    /* Labels across */
    printf("Labels across (1)? ");
    if (read_line(line, sizeof(line)) < 0) return;
    trim_right(line);
    if (line[0]) def.across = atoi(line);
    if (def.across < 1) def.across = 1;

    /* Content lines */
    def.num_lines = 0;
    for (i = 0; i < def.height; i++) {
        printf("Line %d contents? ", i + 1);
        if (read_line(line, sizeof(line)) < 0) break;
        trim_right(line);
        if (line[0] == '\0') break;
        str_copy(def.lines[i], line, LBL_EXPR_SIZE + 1);
        def.num_lines++;
    }

    if (def.num_lines == 0) {
        printf("No label lines defined.\n");
        return;
    }

    if (lbl_write(filename, &def) < 0) {
        printf("Error creating %s\n", filename);
        return;
    }
    printf("Label definition %s created.\n", filename);
}

/* ---- LABEL FORM ---- */
static void cmd_label_form(dbf_t *db, const char *arg) {
    char filename[64];
    const char *p;
    const char *for_cond = NULL;
    int sample = 0;
    FILE *outfile = NULL;
    lbl_def_t def;
    int i;

    if (!dbf_is_open(db)) {
        prog_error(ERR_NO_DATABASE, "No database in use");
        return;
    }

    p = skip_ws(arg);
    /* Parse filename */
    i = 0;
    while (*p && *p != ' ' && *p != '\t' && i < 63)
        filename[i++] = *p++;
    filename[i] = '\0';
    ensure_lbl_ext(filename, sizeof(filename));

    /* Parse optional clauses */
    while (*p) {
        p = skip_ws(p);
        if (*p == '\0') break;

        if (str_imatch(p, "FOR")) {
            for_cond = skip_ws(p + 3);
            break;  /* FOR consumes rest of line */
        }
        if (str_imatch(p, "SAMPLE")) {
            sample = 1;
            p += 6;
            continue;
        }
        if (str_imatch(p, "TO")) {
            p = skip_ws(p + 2);
            if (str_imatch(p, "PRINT")) {
                p += 5;
                continue;
            }
            if (str_imatch(p, "FILE")) {
                char fname[128];
                int fi = 0;
                p = skip_ws(p + 4);
                while (*p && *p != ' ' && fi < 127) fname[fi++] = *p++;
                fname[fi] = '\0';
                outfile = fopen(fname, "w");
                if (!outfile) {
                    printf("Cannot create %s\n", fname);
                    return;
                }
                continue;
            }
        }
        /* Skip unrecognized word */
        while (*p && *p != ' ' && *p != '\t') p++;
    }

    /* Read the .LBL file */
    if (lbl_read(filename, &def) < 0) {
        file_not_found(filename);
        if (outfile) fclose(outfile);
        return;
    }

    if (!outfile)
        outfile = stdout;

    ctx_setup();
    label_generate(&def, db, &expr_ctx, for_cond, sample, outfile);

    if (outfile != stdout)
        fclose(outfile);
}

/* ---- Accessors for program.c / screen.c ---- */
memvar_store_t *cmd_get_memvar_store(void) {
    return &memvar_store;
}

expr_ctx_t *cmd_get_expr_ctx(void) {
    ctx_setup();
    return &expr_ctx;
}

int cmd_get_device(void) {
    return set_opts.device;
}

int cmd_get_console(void) {
    return set_opts.console;
}

/* ---- Dispatch ---- */
/* ---- Command Dispatch Table ---- */

typedef void (*cmd_func_t)(dbf_t *db, lexer_t *l);

typedef struct {
    const char *name;
    cmd_func_t func;
} cmd_entry_t;

/* Wrappers for legacy or complex-parsing commands */
static void h_do(dbf_t *db, lexer_t *l) { (void)db; char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); prog_do(arg); }
static void h_if(dbf_t *db, lexer_t *l) { (void)db; if (prog_is_running()) { char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); prog_if(arg); } else printf("IF not allowed in interactive mode.\n"); }
static void h_else(dbf_t *db, lexer_t *l) { (void)db; (void)l; if (prog_is_running()) prog_else(); else printf("ELSE without IF.\n"); }
static void h_endif(dbf_t *db, lexer_t *l) { (void)db; (void)l; if (prog_is_running()) prog_endif(); else printf("ENDIF without IF.\n"); }
static void h_enddo(dbf_t *db, lexer_t *l) { (void)db; (void)l; if (prog_is_running()) prog_enddo(); else printf("ENDDO without DO WHILE.\n"); }
static void h_loop(dbf_t *db, lexer_t *l) { (void)db; (void)l; if (prog_is_running()) prog_loop(); else printf("LOOP without DO WHILE.\n"); }
static void h_scan(dbf_t *db, lexer_t *l) { (void)db; if (prog_is_running()) { char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); prog_scan(arg); } else printf("SCAN not allowed in interactive mode.\n"); }
static void h_endscan(dbf_t *db, lexer_t *l) { (void)db; (void)l; if (prog_is_running()) prog_endscan(); else printf("ENDSCAN without SCAN.\n"); }
static void h_case(dbf_t *db, lexer_t *l) { (void)db; if (prog_is_running()) { char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); prog_case(arg); } else printf("CASE without DO CASE.\n"); }
static void h_otherwise(dbf_t *db, lexer_t *l) { (void)db; (void)l; if (prog_is_running()) prog_otherwise(); else printf("OTHERWISE without DO CASE.\n"); }
static void h_endcase(dbf_t *db, lexer_t *l) { (void)db; (void)l; if (prog_is_running()) prog_endcase(); else printf("ENDCASE without DO CASE.\n"); }
static void h_return(dbf_t *db, lexer_t *l) { (void)db; if (prog_is_running()) { char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); prog_return(arg); } else printf("RETURN not in program.\n"); }
static void h_procedure(dbf_t *db, lexer_t *l) { (void)db; if (prog_is_running()) { char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); prog_procedure(arg); } else printf("PROCEDURE not allowed in interactive mode.\n"); }
static void h_parameters(dbf_t *db, lexer_t *l) { (void)db; if (prog_is_running()) { char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); prog_parameters(arg); } else printf("PARAMETERS not allowed in interactive mode.\n"); }
static void h_private(dbf_t *db, lexer_t *l) { (void)db; char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); prog_private(arg); }
static void h_public(dbf_t *db, lexer_t *l) { (void)db; char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); prog_public(arg); }
static void h_cancel(dbf_t *db, lexer_t *l) { (void)db; (void)l; if (prog_is_running()) prog_cancel(); }
static void h_retry(dbf_t *db, lexer_t *l) { (void)db; (void)l; prog_retry(); }
static void h_suspend(dbf_t *db, lexer_t *l) { (void)db; (void)l; if (prog_is_running()) prog_suspend(); else printf("Not in program.\n"); }
static void h_resume(dbf_t *db, lexer_t *l) { (void)db; (void)l; prog_resume(); }
static void h_eject(dbf_t *db, lexer_t *l) { (void)db; (void)l; screen_eject(); }
static void h_run(dbf_t *db, lexer_t *l) { (void)db; (void)l; printf("RUN not supported.\n"); }
static void h_select(dbf_t *db, lexer_t *l) {
    (void)db;
    char arg[256];
    lex_next(l); /* skip SELECT */
    lex_get_remaining(l, arg, sizeof(arg));
    cmd_select(arg);
}
static void h_use(dbf_t *db, lexer_t *l) { char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); cmd_use(db, arg); }
static void h_replace(dbf_t *db, lexer_t *l) { lex_next(l); cmd_replace(db, l); }
static void h_list(dbf_t *db, lexer_t *l) { lex_next(l); cmd_list(db, l); }
static void h_display(dbf_t *db, lexer_t *l) {
    if (cmd_peek_kw(l, "STRUCTURE")) {
        lex_next(l); /* skip DISPLAY */
        lex_next(l); /* skip STRUCTURE */
        cmd_display_structure(db);
    } else if (cmd_peek_kw(l, "MEMORY")) {
        lex_next(l); /* skip DISPLAY */
        lex_next(l); /* skip MEMORY */
        memvar_display(&memvar_store);
    } else {
        lex_next(l);
        cmd_display(db, l);
    }
}
static void h_store(dbf_t *db, lexer_t *l) { lex_next(l); cmd_store(db, l); }
static void h_release(dbf_t *db, lexer_t *l) { (void)db; char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); cmd_release(arg); }
static void h_skip(dbf_t *db, lexer_t *l) { char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); cmd_skip(db, arg); follow_relations(); }
static void h_locate(dbf_t *db, lexer_t *l) { lex_next(l); cmd_locate(db, l); follow_relations(); }
static void h_continue(dbf_t *db, lexer_t *l) { (void)l; cmd_continue(db); follow_relations(); }
static void h_count(dbf_t *db, lexer_t *l) { lex_next(l); cmd_count(db, l); }
static void h_sum(dbf_t *db, lexer_t *l) { lex_next(l); cmd_sum(db, l); }
static void h_average(dbf_t *db, lexer_t *l) { lex_next(l); cmd_average(db, l); }
static void h_reindex(dbf_t *db, lexer_t *l) { (void)l; cmd_reindex(db); }
static void h_delete(dbf_t *db, lexer_t *l) { lex_next(l); cmd_delete(db, l); }
static void h_recall(dbf_t *db, lexer_t *l) { lex_next(l); cmd_recall(db, l); }
static void h_pack(dbf_t *db, lexer_t *l) { (void)l; cmd_pack(db); }
static void h_zap(dbf_t *db, lexer_t *l) { (void)l; cmd_zap(db); }
static void h_erase(dbf_t *db, lexer_t *l) { (void)db; char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); cmd_erase(arg); }
static void h_rename(dbf_t *db, lexer_t *l) { (void)db; char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); cmd_rename(arg); }
static void h_accept(dbf_t *db, lexer_t *l) { (void)db; char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); cmd_accept(arg); }
static void h_input(dbf_t *db, lexer_t *l) { (void)db; char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); cmd_input(arg); }
static void h_wait(dbf_t *db, lexer_t *l) { (void)db; char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); cmd_wait(arg); }
static void h_read(dbf_t *db, lexer_t *l) { (void)db; (void)l; screen_read(); }

static void h_go(dbf_t *db, lexer_t *l) {
    char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg));
    cmd_go(db, arg); follow_relations();
}

static void h_on(dbf_t *db, lexer_t *l) {
    (void)db;
    lex_next(l); /* skip ON */
    if (cmd_kw(l, "ERROR")) {
        lex_next(l);
        if (cmd_kw(l, "DO")) {
            lex_next(l);
            if (l->current.type == TOK_IDENT) {
                prog_on_error(l->current.text);
                lex_next(l);
            } else prog_on_error(NULL);
        } else prog_on_error(NULL);
    }
}

static void h_report(dbf_t *db, lexer_t *l) {
    lex_next(l); /* skip REPORT */
    if (cmd_kw(l, "FORM")) {
        lex_next(l);
        cmd_report_form(db, l);
    }
}

static void h_label(dbf_t *db, lexer_t *l) {
    lex_next(l); /* skip LABEL */
    if (cmd_kw(l, "FORM")) {
        lex_next(l);
        char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
        cmd_label_form(db, arg);
    }
}

static void h_create(dbf_t *db, lexer_t *l) {
    lex_next(l); /* skip CREATE */
    if (cmd_kw(l, "REPORT")) {
        lex_next(l);
        char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
        cmd_create_report(arg);
    } else if (cmd_kw(l, "LABEL")) {
        lex_next(l);
        char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
        cmd_create_label(arg);
    } else {
        char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
        cmd_create(db, arg);
    }
}

static void h_append(dbf_t *db, lexer_t *l) {
    lex_next(l); /* skip APPEND */
    if (cmd_kw(l, "BLANK")) {
        cmd_append_blank(db);
        lex_next(l);
    } else if (cmd_kw(l, "FROM")) {
        lex_next(l);
        char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
        cmd_append_from(db, arg);
    } else printf("Syntax: APPEND BLANK | APPEND FROM <filename>\n");
}

static void h_copy(dbf_t *db, lexer_t *l) {
    lex_next(l); /* skip COPY */
    if (cmd_kw(l, "STRUCTURE")) {
        lex_next(l);
        if (cmd_kw(l, "TO")) {
            lex_next(l);
            char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
            cmd_copy_structure(db, arg);
        } else printf("Syntax: COPY STRUCTURE TO <filename>\n");
    } else if (cmd_kw(l, "FILE")) {
        lex_next(l);
        char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
        cmd_copy_file(arg);
    } else if (cmd_kw(l, "TO")) {
        lex_next(l);
        char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
        cmd_copy_to(db, arg);
    }
}

static void h_sort(dbf_t *db, lexer_t *l) {
    lex_next(l); /* skip SORT */
    if (cmd_kw(l, "TO")) {
        lex_next(l);
        char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
        cmd_sort(db, arg);
    } else printf("Syntax: SORT TO <filename> ON <field> [/A][/D][/C]\n");
}

static void h_index(dbf_t *db, lexer_t *l) { char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); cmd_index_on(db, arg); }

static void h_seek(dbf_t *db, lexer_t *l) { char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); cmd_seek(db, arg); }
static void h_find(dbf_t *db, lexer_t *l) { char arg[256]; lex_next(l); lex_get_remaining(l, arg, sizeof(arg)); cmd_find(db, arg); }

static void h_close(dbf_t *db, lexer_t *l) {
    lex_next(l); /* skip CLOSE */
    if (cmd_kw(l, "DATABASES") || cmd_kw(l, "DATA")) {
        cmd_close_all();
        lex_next(l);
    } else if (cmd_kw(l, "ALL")) {
        cmd_close_all();
        memvar_release_all(&memvar_store);
        prog_set_procedure(NULL);
        lex_next(l);
    } else if (cmd_kw(l, "INDEX")) {
        close_all_indexes(cur_wa());
        lex_next(l);
    } else if (cmd_kw(l, "PROCEDURE")) {
        prog_set_procedure(NULL);
        lex_next(l);
    } else if (dbf_is_open(db)) {
        dbf_close(db);
        close_all_indexes(cur_wa());
        cur_wa()->alias[0] = '\0';
        cur_wa()->filter_cond[0] = '\0';
        cur_wa()->order = 0;
        cur_wa()->num_indexes = 0;
    }
}

static void h_clear(dbf_t *db, lexer_t *l) {
    lex_next(l); /* skip CLEAR */
    if (cmd_kw(l, "ALL")) {
        cmd_close_all();
        memvar_release_all(&memvar_store);
        prog_set_procedure(NULL);
        lex_next(l);
    } else if (cmd_kw(l, "MEMORY")) {
        memvar_release_all(&memvar_store);
        lex_next(l);
    } else if (cmd_kw(l, "GETS")) {
        screen_clear_gets();
        lex_next(l);
    } else {
        screen_clear();
    }
}

static void h_set(dbf_t *db, lexer_t *l) {
    lex_next(l); /* skip SET */
    if (cmd_kw(l, "COLOR")) {
        lex_next(l);
        if (cmd_kw(l, "TO")) {
            lex_next(l);
            char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
            screen_set_color(arg);
        }
    } else if (cmd_kw(l, "INDEX")) {
        lex_next(l);
        if (cmd_kw(l, "TO")) {
            lex_next(l);
            char arg[256];
            lex_get_remaining(l, arg, sizeof(arg));
            cmd_set_index(db, arg);
        }
    } else if (cmd_kw(l, "PROCEDURE")) {
        lex_next(l);
        if (cmd_kw(l, "TO")) {
            lex_next(l);
            char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
            prog_set_procedure(arg);
        }
    } else if (cmd_kw(l, "FILTER")) {
        lex_next(l);
        if (cmd_kw(l, "TO")) {
            lex_next(l);
            char arg[256]; lex_get_remaining(l, arg, sizeof(arg));
            if (arg[0] == '\0') {
                cur_wa()->filter_cond[0] = '\0';
                printf("Filter removed.\n");
            } else {
                str_copy(cur_wa()->filter_cond, arg, sizeof(cur_wa()->filter_cond));
                trim_right(arg);
                printf("Filter: %s\n", arg);
            }
        }
    } else if (cmd_kw(l, "ORDER")) {
        lex_next(l);
        if (cmd_kw(l, "TO")) {
            lex_next(l);
            if (l->current.type == TOK_NUMBER) {
                int n = (int)l->current.num_val;
                if (n >= 0 && n <= cur_wa()->num_indexes) {
                    cur_wa()->order = n;
                    if (n == 0) printf("Natural record order.\n");
                    else printf("Order set to %d (%s).\n", n, cur_wa()->indexes[n - 1].filename);
                }
                lex_next(l);
            }
        }
    } else if (cmd_kw(l, "RELATION")) {
        lex_next(l);
        if (cmd_kw(l, "TO")) {
            lex_next(l);
            if (l->current.type == TOK_EOF) {
                cur_wa()->relation_expr[0] = '\0';
                cur_wa()->relation_target = -1;
            } else {
                const char *expr_start = l->token_start;
                while (l->current.type != TOK_EOF && !cmd_kw(l, "INTO"))
                    lex_next(l);
                
                if (l->current.type == TOK_EOF) {
                    printf("Syntax: SET RELATION TO <expr> INTO <alias>\n");
                    return;
                }
                
                int len = (int)(l->token_start - expr_start);
                if (len >= (int)sizeof(cur_wa()->relation_expr))
                    len = sizeof(cur_wa()->relation_expr) - 1;
                memcpy(cur_wa()->relation_expr, expr_start, len);
                cur_wa()->relation_expr[len] = '\0';
                trim_right(cur_wa()->relation_expr);
                
                lex_next(l); /* skip INTO */
                if (l->current.type != TOK_IDENT) {
                    printf("Invalid work area.\n");
                    return;
                }
                cur_wa()->relation_target = area_resolve_alias(l->current.text);
                if (cur_wa()->relation_target < 0) {
                    printf("Invalid work area.\n");
                }
                lex_next(l);
            }
        }
    } else {
        char arg[256];
        lex_get_remaining(l, arg, sizeof(arg));
        set_execute(&set_opts, arg);
    }
}

static void h_modify(dbf_t *db, lexer_t *l) {
    lexer_t t = *l; lex_next(&t);
    if (cmd_kw(&t, "REPORT") || cmd_kw(&t, "LABEL")) printf("MODIFY not implemented.\n");
}

static void h_save(dbf_t *db, lexer_t *l) {
    (void)db;
    lex_next(l); /* skip SAVE */
    if (cmd_kw(l, "TO")) {
        lex_next(l);
        char fname[128];
        FILE *fp;
        fname[0] = '\0';
        while (l->current.type != TOK_EOF && !cmd_kw(l, "ALL")) {
            strncat(fname, l->current.text, sizeof(fname) - strlen(fname) - 1);
            lex_next(l);
        }
        str_upper(fname);
        if (strlen(fname) < 4 || str_icmp(fname + strlen(fname) - 4, ".MEM") != 0) strcat(fname, ".MEM");
        fp = fopen(fname, "wb");
        if (!fp) { printf("Cannot create %s\n", fname); return; }
        {
            int nv = 0, j;
            for (j = 0; j < MEMVAR_MAX; j++) { if (memvar_store.vars[j].used) nv++; }
            fwrite(&nv, 4, 1, fp);
            for (j = 0; j < MEMVAR_MAX; j++) {
                if (!memvar_store.vars[j].used) continue;
                fwrite(memvar_store.vars[j].name, MEMVAR_NAMELEN, 1, fp);
                fwrite(&memvar_store.vars[j].val.type, 4, 1, fp);
                if (memvar_store.vars[j].val.type == VAL_NUM) fwrite(&memvar_store.vars[j].val.num, 8, 1, fp);
                else if (memvar_store.vars[j].val.type == VAL_CHAR) {
                    int len = strlen(memvar_store.vars[j].val.str);
                    fwrite(&len, 4, 1, fp); fwrite(memvar_store.vars[j].val.str, len, 1, fp);
                } else if (memvar_store.vars[j].val.type == VAL_DATE) fwrite(&memvar_store.vars[j].val.date, 4, 1, fp);
                else if (memvar_store.vars[j].val.type == VAL_LOGIC) fwrite(&memvar_store.vars[j].val.logic, 4, 1, fp);
            }
        }
        fclose(fp);
    }
}

static void h_restore(dbf_t *db, lexer_t *l) {
    (void)db;
    lex_next(l); /* skip RESTORE */
    if (cmd_kw(l, "FROM")) {
        lex_next(l);
        char fname[128];
        int additive = 0;
        FILE *fp;
        fname[0] = '\0';
        while (l->current.type != TOK_EOF && !cmd_kw(l, "ADDITIVE")) {
            strncat(fname, l->current.text, sizeof(fname) - strlen(fname) - 1);
            lex_next(l);
        }
        if (cmd_kw(l, "ADDITIVE")) {
            additive = 1;
            lex_next(l);
        }
        str_upper(fname);
        if (strlen(fname) < 4 || str_icmp(fname + strlen(fname) - 4, ".MEM") != 0) strcat(fname, ".MEM");
        fp = fopen(fname, "rb");
        if (!fp) { file_not_found(fname); return; }
        if (!additive) memvar_release_all(&memvar_store);
        {
            int nv = 0, j; fread(&nv, 4, 1, fp);
            for (j = 0; j < nv; j++) {
                char name[MEMVAR_NAMELEN]; int type; value_t v;
                fread(name, MEMVAR_NAMELEN, 1, fp); fread(&type, 4, 1, fp);
                v.type = type;
                if (type == VAL_NUM) fread(&v.num, 8, 1, fp);
                else if (type == VAL_CHAR) {
                    int len = 0; fread(&len, 4, 1, fp);
                    if (len >= (int)sizeof(v.str)) len = sizeof(v.str) - 1;
                    fread(v.str, len, 1, fp); v.str[len] = '\0';
                } else if (type == VAL_DATE) fread(&v.date, 4, 1, fp);
                else if (type == VAL_LOGIC) fread(&v.logic, 4, 1, fp);
                memvar_set(&memvar_store, name, &v);
            }
        }
        fclose(fp);
    }
}

static void h_total(dbf_t *db, lexer_t *l) { (void)db; (void)l; printf("TOTAL ON not implemented.\n"); }
static void h_join(dbf_t *db, lexer_t *l) { (void)db; (void)l; printf("JOIN WITH not implemented.\n"); }
static void h_update(dbf_t *db, lexer_t *l) { (void)db; (void)l; printf("UPDATE ON not implemented.\n"); }
/* QUIT and EXIT are handled directly in cmd_execute() because they
   must return 1 to signal the interpreter to exit.  They are NOT in
   the dispatch table. */
static void h_stub(dbf_t *db, lexer_t *l) { (void)db; (void)l; printf("Command not implemented.\n"); }

static cmd_entry_t cmd_table[] = {
    { "ACCEPT", h_accept }, { "APPEND", h_append },
    { "AVERAGE", h_average }, { "CANCEL", h_cancel },
    { "CASE", h_case }, { "CLEAR", h_clear },
    { "CLOSE", h_close }, { "CONTINUE", h_continue },
    { "COPY", h_copy }, { "COUNT", h_count },
    { "CREATE", h_create }, { "DELETE", h_delete },
    { "DISPLAY", h_display }, { "DO", h_do },
    { "EJECT", h_eject }, { "ELSE", h_else },
    { "ENDCASE", h_endcase }, { "ENDDO", h_enddo },
    { "ENDIF", h_endif }, { "ENDSCAN", h_endscan },
    { "ERASE", h_erase }, { "FIND", h_find },
    { "GO", h_go }, { "GOTO", h_go },
    { "IF", h_if }, { "INDEX", h_index },
    { "INPUT", h_input }, { "JOIN", h_join },
    { "LABEL", h_label }, { "LIST", h_list },
    { "LOCATE", h_locate }, { "LOOP", h_loop },
    { "MODIFY", h_modify }, { "ON", h_on },
    { "OTHERWISE", h_otherwise }, { "PACK", h_pack },
    { "PARAMETERS", h_parameters }, { "PRIVATE", h_private },
    { "PROCEDURE", h_procedure }, { "PUBLIC", h_public },
    { "READ", h_read },
    { "RECALL", h_recall }, { "REINDEX", h_reindex },
    { "RELEASE", h_release }, { "RENAME", h_rename },
    { "REPLACE", h_replace }, { "REPORT", h_report },
    { "RESTORE", h_restore }, { "RESUME", h_resume },
    { "RETRY", h_retry }, { "RETURN", h_return },
    { "RUN", h_run }, { "SAVE", h_save },
    { "SCAN", h_scan }, { "SEEK", h_seek },
    { "SELECT", h_select }, { "SET", h_set },
    { "SKIP", h_skip }, { "SORT", h_sort },
    { "STORE", h_store }, { "SUM", h_sum },
    { "SUSPEND", h_suspend }, { "TOTAL", h_total },
    { "UPDATE", h_update }, { "USE", h_use },
    { "WAIT", h_wait }, { "ZAP", h_zap },
    { NULL, NULL }
};

/* ---- Command dispatch helpers ---- */
static int cmd_table_count(void) {
    int i = 0;
    while (cmd_table[i].name) i++;
    return i;
}

static int cmd_table_exact(const char *ident) {
    int lo = 0;
    int hi = cmd_table_count();
    while (lo < hi) {
        int mid = lo + (hi - lo) / 2;
        int cmp = str_icmp(cmd_table[mid].name, ident);
        if (cmp == 0) return mid;
        if (cmp < 0) lo = mid + 1;
        else hi = mid;
    }
    return -1;
}

static int cmd_table_abbrev(const char *ident, int *ambiguous) {
    int ident_len = (int)strlen(ident);
    int lo = 0;
    int hi = cmd_table_count();
    int i;
    int match = -1;

    *ambiguous = 0;
    while (lo < hi) {
        int mid = lo + (hi - lo) / 2;
        int cmp = str_icmp(cmd_table[mid].name, ident);
        if (cmp < 0) lo = mid + 1;
        else hi = mid;
    }

    for (i = lo; cmd_table[i].name; i++) {
        int name_len = (int)strlen(cmd_table[i].name);
        int min_len = (name_len < 4) ? name_len : 4;
        if (str_nicmp(cmd_table[i].name, ident, ident_len) != 0) break;
        if (ident_len < min_len || ident_len > name_len) continue;
        if (match >= 0) {
            *ambiguous = 1;
            return -1;
        }
        match = i;
    }
    return match;
}

int cmd_execute(dbf_t *db, char *line) {
    char *p;
    dbf_t *cdb;
    lexer_t l;
    int idx;
    int ambiguous;

    (void)db;
    ctx_setup();
    cdb = cur_db();

    p = skip_ws(line);
    if (*p == '\0') return 0;
    if (*p == '*') return 0;

    lexer_init_ext(&l, p, expr_ctx.vars);
    if (cmd_kw(&l, "NOTE")) return 0;

    /* Handle specific non-keyword prefix commands */
    if (p[0] == '@') { screen_at_cmd(p); return 0; }
    if (p[0] == '?') {
        if (p[1] == '?') cmd_print_expr(p + 2, 0);
        else cmd_print_expr(p + 1, 1);
        return 0;
    }

    /* Variable assignment: identifier = expr */
    if (is_ident_start(p[0])) {
        const char *q = p;
        char name[MEMVAR_NAMELEN];
        int j = 0;
        while (is_ident_char(*q) && j < MEMVAR_NAMELEN - 1)
            name[j++] = *q++;
        name[j] = '\0';
        q = skip_ws(q);
        if (*q == '=') {
            if (lex_is_reserved(name)) {
                char err[128];
                snprintf(err, sizeof(err), "%s is a reserved keyword", name);
                prog_error(ERR_SYNTAX, err);
                return 0;
            }
            value_t val;
            q++;
            if (expr_eval_str(&expr_ctx, q, &val) != 0) {
                report_expr_error();
                return 0;
            }
            memvar_set(&memvar_store, name, &val);
            return 0;
        }
    }

    /* Standard command dispatch (exact match, then unique abbreviation) */
    if (l.current.type == TOK_IDENT) {
        idx = cmd_table_exact(l.current.text);
        if (idx >= 0) {
            cmd_table[idx].func(cdb, &l);
            return 0;
        }
        idx = cmd_table_abbrev(l.current.text, &ambiguous);
        if (idx >= 0) {
            cmd_table[idx].func(cdb, &l);
            return 0;
        }
        if (ambiguous) {
            prog_error(ERR_SYNTAX, "Ambiguous command");
            return 0;
        }
    }

    /* QUIT/EXIT return 1 to exit the interpreter.
       EXIT inside a program exits the current DO WHILE loop instead. */
    if (cmd_kw(&l, "QUIT")) return 1;
    if (cmd_kw(&l, "EXIT")) {
        if (prog_is_running()) { prog_exit_loop(); return 0; }
        return 1;
    }

    prog_error(ERR_UNRECOGNIZED, "Unrecognized command");
    return 0;
}
