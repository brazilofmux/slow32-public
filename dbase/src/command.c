#include <stdio.h>
#include <string.h>
#include <stdlib.h>
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
#include "lex.h"

/* ---- Work area infrastructure ---- */
#define MAX_AREAS 10

typedef struct {
    dbf_t db;
    char alias[DBF_MAX_FIELD_NAME];
    char locate_cond[256];
    uint32_t locate_last_rec;
    char filter_cond[256];      /* SET FILTER TO condition */
    index_t indexes[MAX_INDEXES];
    int num_indexes;
    int order;          /* controlling index: 0=natural, 1-N=index number */
} work_area_t;

static work_area_t areas[MAX_AREAS];
static int current_area;

static dbf_t *cur_db(void) { return &areas[current_area].db; }

/* Persistent expression context */
static expr_ctx_t expr_ctx;

/* Stage 3 state */
static memvar_store_t memvar_store;
static set_options_t set_opts;
static int stage3_initialized;

/* Forward declarations */
static dbf_t *area_lookup(const char *alias);

/* ---- Helper: initialize expr_ctx ---- */
static int screen_initialized;
static void ctx_setup(void) {
    if (!stage3_initialized) {
        memvar_init(&memvar_store);
        set_init(&set_opts);
        stage3_initialized = 1;
    }
    if (!screen_initialized) {
        screen_init();
        screen_initialized = 1;
    }
    expr_ctx.db = cur_db();
    expr_ctx.vars = &memvar_store;
    expr_ctx.opts = (struct set_options *)&set_opts;
    expr_ctx.area_lookup = area_lookup;
}

/* ---- Area lookup callback for expr.c ---- */
static dbf_t *area_lookup(const char *alias) {
    int i;
    char c;
    /* Try single letter A-J */
    c = alias[0];
    if (c >= 'a' && c <= 'z') c -= 32;
    if (c >= 'A' && c <= 'J' && alias[1] == '\0')
        return &areas[c - 'A'].db;
    /* Try name alias */
    for (i = 0; i < MAX_AREAS; i++) {
        if (areas[i].alias[0] && str_icmp(areas[i].alias, alias) == 0)
            return &areas[i].db;
    }
    return NULL;
}

/* ---- Index helpers ---- */
static index_t *controlling_index(void) {
    work_area_t *wa = &areas[current_area];
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
static void print_field_value(dbf_t *db, int f, char *raw, char *display, int for_list, int width) {
    dbf_get_field_raw(db, f, raw, 256);
    switch (db->fields[f].type) {
    case 'C':
        field_display_char(display, raw, db->fields[f].length);
        if (for_list) {
            trim_right(display);
            printf(" %-*s", width, display);
        } else {
            trim_right(display);
            printf("%10s: %s\n", db->fields[f].name, display);
        }
        break;
    case 'N':
        field_display_numeric(display, raw, db->fields[f].length);
        if (for_list)
            printf(" %*s", width, display);
        else
            printf("%10s: %s\n", db->fields[f].name, display);
        break;
    case 'D':
        field_display_date(display, raw);
        if (for_list)
            printf(" %-8s", display);
        else
            printf("%10s: %s\n", db->fields[f].name, display);
        break;
    case 'L':
        field_display_logical(display, raw);
        if (for_list)
            printf(" %-3s", display);
        else
            printf("%10s: %s\n", db->fields[f].name, display);
        break;
    default:
        field_display_char(display, raw, db->fields[f].length);
        if (for_list) {
            trim_right(display);
            printf(" %-*s", width, display);
        } else {
            trim_right(display);
            printf("%10s: %s\n", db->fields[f].name, display);
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
    int area;
    char c;
    int i;

    if (*p == '\0') {
        printf("Current work area: %d\n", current_area + 1);
        return;
    }

    /* Try numeric 1-10 */
    if (*p >= '1' && *p <= '9') {
        area = atoi(p) - 1;
        if (area >= 0 && area < MAX_AREAS) {
            current_area = area;
            return;
        }
    }

    /* Try letter A-J */
    c = *p;
    if (c >= 'a' && c <= 'z') c -= 32;
    if (c >= 'A' && c <= 'J' && (p[1] == '\0' || p[1] == ' ' || p[1] == '\t')) {
        current_area = c - 'A';
        return;
    }

    /* Try alias name */
    for (i = 0; i < MAX_AREAS; i++) {
        if (areas[i].alias[0] && str_icmp(areas[i].alias, p) == 0) {
            current_area = i;
            return;
        }
    }
    printf("Invalid work area.\n");
}

/* ---- Helper: check if record should be skipped (SET DELETED) ---- */
static int skip_deleted(const char *rec_buf) {
    return (rec_buf[0] == '*' && set_opts.deleted);
}

/* ---- Helper: check if current record passes SET FILTER ---- */
/* Returns 1 if record passes (or no filter set), 0 if filtered out */
static int check_filter(dbf_t *db) {
    value_t cond;
    if (areas[current_area].filter_cond[0] == '\0')
        return 1;
    if (expr_eval_str(&expr_ctx, areas[current_area].filter_cond, &cond) != 0)
        return 1;  /* on error, pass through */
    return (cond.type == VAL_LOGIC && cond.logic);
}

/* ---- USE ---- */
static void cmd_use(dbf_t *db, const char *arg) {
    char filename[64];
    const char *p;
    int i;

    arg = skip_ws(arg);

    if (*arg == '\0') {
        if (dbf_is_open(db)) {
            dbf_close(db);
            close_all_indexes(&areas[current_area]);
            areas[current_area].alias[0] = '\0';
            areas[current_area].locate_cond[0] = '\0';
            areas[current_area].locate_last_rec = 0;
            areas[current_area].filter_cond[0] = '\0';
            printf("Database closed.\n");
        }
        return;
    }

    if (dbf_is_open(db)) {
        dbf_close(db);
        close_all_indexes(&areas[current_area]);
        areas[current_area].alias[0] = '\0';
    }

    /* Parse filename (up to whitespace) */
    p = arg;
    i = 0;
    while (*p && *p != ' ' && *p != '\t' && i < 63)
        filename[i++] = *p++;
    filename[i] = '\0';
    ensure_dbf_ext(filename, sizeof(filename));

    if (dbf_open(db, filename) < 0) {
        printf("File not found: %s\n", filename);
        return;
    }

    /* Set alias to filename stem */
    filename_stem(filename, areas[current_area].alias, sizeof(areas[current_area].alias));

    /* Parse optional INDEX clause */
    p = skip_ws(p);
    if (str_imatch(p, "INDEX")) {
        p = skip_ws(p + 5);
        while (*p && !str_imatch(p, "ALIAS") && areas[current_area].num_indexes < MAX_INDEXES) {
            char ndxfile[64];
            i = 0;
            while (*p && *p != ' ' && *p != '\t' && *p != ',' && i < 63)
                ndxfile[i++] = *p++;
            ndxfile[i] = '\0';

            if (ndxfile[0]) {
                ensure_ndx_ext(ndxfile, sizeof(ndxfile));
                {
                    int slot = areas[current_area].num_indexes;
                    if (index_read(&areas[current_area].indexes[slot], ndxfile) == 0) {
                        areas[current_area].num_indexes++;
                    } else {
                        printf("Cannot open index: %s\n", ndxfile);
                    }
                }
            }

            p = skip_ws(p);
            if (*p == ',') { p++; p = skip_ws(p); }
            else break;
        }

        if (areas[current_area].num_indexes > 0)
            areas[current_area].order = 1;
    }

    /* Parse optional ALIAS clause */
    p = skip_ws(p);
    if (str_imatch(p, "ALIAS")) {
        p = skip_ws(p + 5);
        i = 0;
        while (*p && *p != ' ' && *p != '\t' && i < DBF_MAX_FIELD_NAME - 1)
            areas[current_area].alias[i++] = *p++;
        areas[current_area].alias[i] = '\0';
        str_upper(areas[current_area].alias);
    }

    /* Reset navigation state */
    expr_ctx.eof_flag = 0;
    expr_ctx.bof_flag = 0;
    expr_ctx.found = 0;
    areas[current_area].locate_cond[0] = '\0';
    areas[current_area].locate_last_rec = 0;

    printf("Database %s opened with %d record(s).\n",
           db->filename, (int)db->record_count);
}

/* ---- APPEND BLANK ---- */
static void cmd_append_blank(dbf_t *db) {
    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    if (dbf_append_blank(db) < 0) {
        printf("Error appending record.\n");
        return;
    }

    expr_ctx.eof_flag = 0;
    expr_ctx.bof_flag = 0;
    printf("Record %d added.\n", (int)db->record_count);
}

/* ---- GO / GOTO ---- */
static void cmd_go(dbf_t *db, const char *arg) {
    const char *p;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
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
            printf("Record out of range.\n");
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
        printf("No database in use.\n");
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
typedef enum { SCOPE_ALL, SCOPE_NEXT, SCOPE_RECORD, SCOPE_REST } scope_type_t;
typedef struct { scope_type_t type; uint32_t count; } scope_t;

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

static int scope_bounds(dbf_t *db, const scope_t *s, uint32_t *start, uint32_t *end) {
    switch (s->type) {
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

/* ---- LOCATE [scope] FOR ---- */
static void cmd_locate(dbf_t *db, const char *arg) {
    const char *p;
    scope_t scope;
    uint32_t start, end, i;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    p = skip_ws(arg);
    scope = parse_scope(&p);

    if (!str_imatch(p, "FOR")) {
        printf("Syntax: LOCATE [scope] FOR <condition>\n");
        return;
    }
    p = skip_ws(p + 3);

    /* Save condition for CONTINUE (per-area) */
    str_copy(areas[current_area].locate_cond, p, sizeof(areas[current_area].locate_cond));

    if (scope_bounds(db, &scope, &start, &end) < 0) {
        printf("Invalid scope.\n");
        return;
    }

    expr_ctx.found = 0;

    for (i = start; i <= end; i++) {
        value_t cond;
        dbf_read_record(db, i);
        if (skip_deleted(db->record_buf) || !check_filter(db)) continue;

        if (expr_eval_str(&expr_ctx, areas[current_area].locate_cond, &cond) != 0) {
            if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
            return;
        }

        if (cond.type == VAL_LOGIC && cond.logic) {
            expr_ctx.found = 1;
            expr_ctx.eof_flag = 0;
            areas[current_area].locate_last_rec = i;
            if (set_opts.talk) printf("Record = %d\n", (int)i);
            return;
        }
    }

    expr_ctx.found = 0;
    expr_ctx.eof_flag = 1;
    areas[current_area].locate_last_rec = 0;
    if (set_opts.talk) printf("End of LOCATE scope\n");
}

/* ---- CONTINUE ---- */
static void cmd_continue(dbf_t *db) {
    uint32_t i;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    if (areas[current_area].locate_cond[0] == '\0') {
        printf("No LOCATE active.\n");
        return;
    }

    for (i = areas[current_area].locate_last_rec + 1; i <= db->record_count; i++) {
        value_t cond;
        dbf_read_record(db, i);
        if (skip_deleted(db->record_buf) || !check_filter(db)) continue;

        if (expr_eval_str(&expr_ctx, areas[current_area].locate_cond, &cond) != 0) {
            if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
            return;
        }

        if (cond.type == VAL_LOGIC && cond.logic) {
            expr_ctx.found = 1;
            expr_ctx.eof_flag = 0;
            areas[current_area].locate_last_rec = i;
            if (set_opts.talk) printf("Record = %d\n", (int)i);
            return;
        }
    }

    expr_ctx.found = 0;
    expr_ctx.eof_flag = 1;
    if (set_opts.talk) printf("End of LOCATE scope\n");
}

/* ---- COUNT [scope] [FOR cond] ---- */
static void cmd_count(dbf_t *db, const char *arg) {
    const char *p;
    const char *cond_str = NULL;
    scope_t scope;
    uint32_t start, end, i;
    int count = 0;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    p = skip_ws(arg);
    scope = parse_scope(&p);

    if (str_imatch(p, "FOR")) {
        cond_str = skip_ws(p + 3);
    }

    if (scope_bounds(db, &scope, &start, &end) < 0) {
        printf("Invalid scope.\n");
        return;
    }

    for (i = start; i <= end; i++) {
        dbf_read_record(db, i);
        if (skip_deleted(db->record_buf) || !check_filter(db)) continue;

        if (cond_str) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, cond_str, &cond) != 0) {
                if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
                return;
            }
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }
        count++;
    }

    printf("%d record(s)\n", count);
}

/* ---- SUM [scope] expr [FOR cond] ---- */
static void cmd_sum(dbf_t *db, const char *arg) {
    const char *p;
    char expr_str[256];
    const char *cond_str = NULL;
    scope_t scope;
    uint32_t start, end, i;
    double total = 0.0;
    int count = 0;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    p = skip_ws(arg);
    if (*p == '\0') {
        printf("Syntax: SUM <expr> [FOR <condition>]\n");
        return;
    }

    {
        const char *f = p;
        const char *for_pos = NULL;
        while (*f) {
            if (str_imatch(f, "FOR")) { for_pos = f; break; }
            f++;
        }
        if (for_pos) {
            int len = (int)(for_pos - p);
            if (len > (int)sizeof(expr_str) - 1) len = (int)sizeof(expr_str) - 1;
            memcpy(expr_str, p, len);
            expr_str[len] = '\0';
            trim_right(expr_str);
            cond_str = skip_ws(for_pos + 3);
        } else {
            str_copy(expr_str, p, sizeof(expr_str));
            trim_right(expr_str);
        }
    }

    {
        const char *sp = expr_str;
        scope = parse_scope(&sp);
        if (sp != expr_str) {
            const char *rest = skip_ws(sp);
            int len = strlen(rest);
            memmove(expr_str, rest, len + 1);
        }
    }

    if (scope_bounds(db, &scope, &start, &end) < 0) {
        printf("Invalid scope.\n");
        return;
    }

    for (i = start; i <= end; i++) {
        value_t val;
        dbf_read_record(db, i);
        if (skip_deleted(db->record_buf) || !check_filter(db)) continue;

        if (cond_str) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, cond_str, &cond) != 0) {
                if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
                return;
            }
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }

        if (expr_eval_str(&expr_ctx, expr_str, &val) != 0) {
            if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
            return;
        }
        if (val.type != VAL_NUM) {
            printf("SUM requires numeric expression.\n");
            return;
        }
        total += val.num;
        count++;
    }

    {
        value_t v = val_num(total);
        char buf[64];
        val_to_string(&v, buf, sizeof(buf));
        printf("%d record(s) summed\n", count);
        printf("      SUM: %s\n", buf);
    }
}

/* ---- AVERAGE [scope] expr [FOR cond] ---- */
static void cmd_average(dbf_t *db, const char *arg) {
    const char *p;
    char expr_str[256];
    const char *cond_str = NULL;
    scope_t scope;
    uint32_t start, end, i;
    double total = 0.0;
    int count = 0;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    p = skip_ws(arg);
    if (*p == '\0') {
        printf("Syntax: AVERAGE <expr> [FOR <condition>]\n");
        return;
    }

    {
        const char *f = p;
        const char *for_pos = NULL;
        while (*f) {
            if (str_imatch(f, "FOR")) { for_pos = f; break; }
            f++;
        }
        if (for_pos) {
            int len = (int)(for_pos - p);
            if (len > (int)sizeof(expr_str) - 1) len = (int)sizeof(expr_str) - 1;
            memcpy(expr_str, p, len);
            expr_str[len] = '\0';
            trim_right(expr_str);
            cond_str = skip_ws(for_pos + 3);
        } else {
            str_copy(expr_str, p, sizeof(expr_str));
            trim_right(expr_str);
        }
    }

    {
        const char *sp = expr_str;
        scope = parse_scope(&sp);
        if (sp != expr_str) {
            const char *rest = skip_ws(sp);
            int len = strlen(rest);
            memmove(expr_str, rest, len + 1);
        }
    }

    if (scope_bounds(db, &scope, &start, &end) < 0) {
        printf("Invalid scope.\n");
        return;
    }

    for (i = start; i <= end; i++) {
        value_t val;
        dbf_read_record(db, i);
        if (skip_deleted(db->record_buf) || !check_filter(db)) continue;

        if (cond_str) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, cond_str, &cond) != 0) {
                if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
                return;
            }
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }

        if (expr_eval_str(&expr_ctx, expr_str, &val) != 0) {
            if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
            return;
        }
        if (val.type != VAL_NUM) {
            printf("AVERAGE requires numeric expression.\n");
            return;
        }
        total += val.num;
        count++;
    }

    if (count == 0) {
        printf("No records.\n");
        return;
    }

    {
        value_t v = val_num(total / count);
        char buf[64];
        val_to_string(&v, buf, sizeof(buf));
        printf("%d record(s) averaged\n", count);
        printf("  AVERAGE: %s\n", buf);
    }
}

/* ---- REPLACE (enhanced with expr_eval) ---- */
static void cmd_replace(dbf_t *db, const char *arg) {
    char field_name[DBF_MAX_FIELD_NAME];
    char formatted[256];
    int idx;
    const char *p;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }
    if (db->current_record == 0 || expr_ctx.eof_flag) {
        printf("No current record. Use APPEND BLANK or GO first.\n");
        return;
    }

    p = skip_ws(arg);

    while (*p) {
        value_t val;
        char valbuf[256];

        {
            int i = 0;
            while (*p && *p != ' ' && *p != '\t' && i < DBF_MAX_FIELD_NAME - 1)
                field_name[i++] = *p++;
            field_name[i] = '\0';
        }
        str_upper(field_name);

        idx = dbf_find_field(db, field_name);
        if (idx < 0) {
            printf("Field not found: %s\n", field_name);
            return;
        }

        p = skip_ws(p);
        if (str_imatch(p, "WITH")) {
            p += 4;
            p = skip_ws(p);
        }

        if (expr_eval(&expr_ctx, &p, &val) != 0) {
            if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
            return;
        }

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
            if (val.type == VAL_DATE) {
                date_to_dbf(val.date, formatted);
            } else {
                field_format_date(formatted, valbuf);
            }
            break;
        case 'L':
            if (val.type == VAL_LOGIC) {
                formatted[0] = val.logic ? 'T' : 'F';
                formatted[1] = '\0';
            } else {
                field_format_logical(formatted, valbuf);
            }
            break;
        default:
            field_format_char(formatted, db->fields[idx].length, valbuf);
            break;
        }

        dbf_set_field_raw(db, idx, formatted);

        p = skip_ws(p);
        if (*p == ',') {
            p++;
            p = skip_ws(p);
        }
    }

    dbf_flush_record(db);
}

/* ---- LIST [scope] [field_list] [FOR clause] ---- */
static void cmd_list(dbf_t *db, const char *arg) {
    uint32_t i;
    int f;
    char raw[256], display[256];
    int field_indices[DBF_MAX_FIELDS];
    int nfields = 0;
    const char *rest;
    const char *cond_str = NULL;
    int use_all_fields;
    scope_t scope;
    uint32_t start, end;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    if (db->record_count == 0) {
        printf("No records.\n");
        return;
    }

    rest = skip_ws(arg);

    scope = parse_scope(&rest);

    if (str_imatch(rest, "FOR")) {
        cond_str = skip_ws(rest + 3);
        nfields = 0;
    } else if (*rest != '\0') {
        nfields = parse_field_list(db, rest, field_indices, DBF_MAX_FIELDS, &rest);
        if (nfields < 0) return;
        rest = skip_ws(rest);
        if (str_imatch(rest, "FOR")) {
            cond_str = skip_ws(rest + 3);
        }
    }

    use_all_fields = (nfields == 0);

    if (scope_bounds(db, &scope, &start, &end) < 0) {
        printf("Invalid scope.\n");
        return;
    }

    if (set_opts.heading) {
        printf("Record#");
        if (use_all_fields) {
            for (f = 0; f < db->field_count; f++) {
                int w = field_display_width(db, f);
                printf(" %-*s", w, db->fields[f].name);
            }
        } else {
            for (f = 0; f < nfields; f++) {
                int idx = field_indices[f];
                int w = field_display_width(db, idx);
                printf(" %-*s", w, db->fields[idx].name);
            }
        }
        printf("\n");
    }

    for (i = start; i <= end; i++) {
        if (dbf_read_record(db, i) < 0) continue;
        if (skip_deleted(db->record_buf) || !check_filter(db)) continue;

        if (cond_str) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, cond_str, &cond) != 0) {
                if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
                return;
            }
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }

        printf("%7d", (int)i);

        if (use_all_fields) {
            for (f = 0; f < db->field_count; f++) {
                int w = field_display_width(db, f);
                print_field_value(db, f, raw, display, 1, w);
            }
        } else {
            for (f = 0; f < nfields; f++) {
                int idx = field_indices[f];
                int w = field_display_width(db, idx);
                print_field_value(db, idx, raw, display, 1, w);
            }
        }
        printf("\n");
    }
}

/* ---- DISPLAY (enhanced with field list) ---- */
static void cmd_display(dbf_t *db, const char *arg) {
    int f;
    char raw[256], display[256];
    int field_indices[DBF_MAX_FIELDS];
    int nfields = 0;
    const char *rest;
    int use_all_fields;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }
    if (db->current_record == 0 || expr_ctx.eof_flag) {
        printf("No current record.\n");
        return;
    }

    rest = skip_ws(arg);
    if (*rest != '\0') {
        nfields = parse_field_list(db, rest, field_indices, DBF_MAX_FIELDS, &rest);
        if (nfields < 0) return;
    }

    use_all_fields = (nfields == 0);

    printf("Record# %d\n", (int)db->current_record);
    if (use_all_fields) {
        for (f = 0; f < db->field_count; f++)
            print_field_value(db, f, raw, display, 0, 0);
    } else {
        for (f = 0; f < nfields; f++)
            print_field_value(db, field_indices[f], raw, display, 0, 0);
    }
}

/* ---- DISPLAY STRUCTURE ---- */
static void cmd_display_structure(dbf_t *db) {
    int i;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    printf("Structure for database: %s\n", db->filename);
    printf("Number of records: %d\n", (int)db->record_count);
    printf("Header size: %d\n", (int)db->header_size);
    printf("Record size: %d\n", (int)db->record_size);
    printf("Field  Name        Type  Width  Dec\n");
    for (i = 0; i < db->field_count; i++) {
        printf("%5d  %-10s  %c     %3d    %3d\n",
               i + 1,
               db->fields[i].name,
               db->fields[i].type,
               db->fields[i].length,
               db->fields[i].decimals);
    }
    printf("** Total **               %5d\n", (int)db->record_size);
}

/* ---- STORE expr TO var ---- */
static void cmd_store(dbf_t *db, const char *arg) {
    const char *p = skip_ws(arg);
    char expr_str[256];
    value_t val;
    const char *to_pos;
    char name[MEMVAR_NAMELEN];
    int i;

    (void)db;

    {
        const char *f = p;
        to_pos = NULL;
        while (*f) {
            if (str_imatch(f, "TO")) { to_pos = f; break; }
            f++;
        }
    }

    if (!to_pos) {
        printf("Syntax: STORE <expr> TO <variable>\n");
        return;
    }

    {
        int len = (int)(to_pos - p);
        if (len > (int)sizeof(expr_str) - 1) len = (int)sizeof(expr_str) - 1;
        memcpy(expr_str, p, len);
        expr_str[len] = '\0';
        trim_right(expr_str);
    }

    if (expr_eval_str(&expr_ctx, expr_str, &val) != 0) {
        if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
        return;
    }

    p = skip_ws(to_pos + 2);
    i = 0;
    while (is_ident_char(*p) && i < MEMVAR_NAMELEN - 1)
        name[i++] = *p++;
    name[i] = '\0';

    if (name[0] == '\0') {
        printf("Syntax: STORE <expr> TO <variable>\n");
        return;
    }

    memvar_set(&memvar_store, name, &val);
}

/* ---- RELEASE ---- */
static void cmd_release(const char *arg) {
    const char *p = skip_ws(arg);
    char name[MEMVAR_NAMELEN];
    int i;

    if (str_imatch(p, "ALL")) {
        memvar_release_all(&memvar_store);
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
static void cmd_delete(dbf_t *db, const char *arg) {
    const char *p = skip_ws(arg);
    scope_t scope;
    uint32_t start, end, i;
    const char *cond_str = NULL;
    int count = 0;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    if (*p == '\0') {
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

    scope = parse_scope(&p);
    if (str_imatch(p, "FOR"))
        cond_str = skip_ws(p + 3);

    if (scope_bounds(db, &scope, &start, &end) < 0) {
        printf("Invalid scope.\n");
        return;
    }

    for (i = start; i <= end; i++) {
        dbf_read_record(db, i);
        if (db->record_buf[0] == '*') continue;

        if (cond_str) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, cond_str, &cond) != 0) {
                if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
                return;
            }
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }

        db->record_buf[0] = '*';
        db->record_dirty = 1;
        dbf_flush_record(db);
        count++;
    }

    printf("%d record(s) deleted.\n", count);
}

/* ---- RECALL [scope] [FOR cond] ---- */
static void cmd_recall(dbf_t *db, const char *arg) {
    const char *p = skip_ws(arg);
    scope_t scope;
    uint32_t start, end, i;
    const char *cond_str = NULL;
    int count = 0;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    if (*p == '\0') {
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

    scope = parse_scope(&p);
    if (str_imatch(p, "FOR"))
        cond_str = skip_ws(p + 3);

    if (scope_bounds(db, &scope, &start, &end) < 0) {
        printf("Invalid scope.\n");
        return;
    }

    for (i = start; i <= end; i++) {
        dbf_read_record(db, i);
        if (db->record_buf[0] != '*') continue;

        if (cond_str) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, cond_str, &cond) != 0) {
                if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
                return;
            }
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }

        db->record_buf[0] = ' ';
        db->record_dirty = 1;
        dbf_flush_record(db);
        count++;
    }

    printf("%d record(s) recalled.\n", count);
}

/* ---- PACK ---- */
static void cmd_pack(dbf_t *db) {
    uint32_t src, dst;
    int rec_size;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
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

    if (dst > 0) {
        dbf_read_record(db, 1);
        expr_ctx.eof_flag = 0;
    } else {
        db->current_record = 0;
        expr_ctx.eof_flag = 1;
    }

    printf("%d record(s) remaining.\n", (int)dst);
}

/* ---- ZAP ---- */
static void cmd_zap(dbf_t *db) {
    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    db->record_count = 0;
    db->current_record = 0;
    db->record_dirty = 0;
    dbf_write_header_counts(db);
    expr_ctx.eof_flag = 1;
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

    if (read_line(line, sizeof(line)) < 0)
        line[0] = '\0';

    ctx_setup();
    if (expr_eval_str(&expr_ctx, line, &val) != 0) {
        if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
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
        has_to = (name[0] != '\0');
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

    p = skip_ws(arg);

    if (*p == '\0') {
        if (newline) printf("\n");
        return;
    }

    while (*p) {
        value_t val;
        char buf[256];

        p = skip_ws(p);
        if (*p == '\0') break;

        if (expr_eval(&expr_ctx, &p, &val) != 0) {
            if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
            return;
        }

        val_to_string(&val, buf, sizeof(buf));
        if (!first) printf(" ");
        printf("%s", buf);
        first = 0;

        p = skip_ws(p);
        if (*p == ',') {
            p++;
        }
    }

    if (newline) printf("\n");
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
        printf("No database in use.\n");
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
                if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
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
        printf("No database in use.\n");
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
        printf("No database in use.\n");
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
        printf("File not found: %s\n", filename);
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
                if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
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
        count++;
    }

    dbf_close(&source);
    printf("%d record(s) appended.\n", count);
}

/* ---- SORT TO file ON field [/A][/D][/C] [scope] [FOR cond] ---- */

typedef struct {
    uint32_t recno;
    char key[256];
} sort_entry_t;

#define MAX_SORT_ENTRIES 2000
static sort_entry_t sort_entries[MAX_SORT_ENTRIES];

static int sort_ascending;
static int sort_case_insensitive;

static int sort_compare(const void *a, const void *b) {
    const sort_entry_t *sa = (const sort_entry_t *)a;
    const sort_entry_t *sb = (const sort_entry_t *)b;
    int cmp;
    if (sort_case_insensitive)
        cmp = str_icmp(sa->key, sb->key);
    else
        cmp = strcmp(sa->key, sb->key);
    return sort_ascending ? cmp : -cmp;
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

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
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
    sort_ascending = 1;
    sort_case_insensitive = 0;
    p = skip_ws(p);
    while (*p == '/') {
        p++;
        char fc = *p;
        if (fc >= 'a' && fc <= 'z') fc -= 32;
        if (fc == 'A') sort_ascending = 1;
        else if (fc == 'D') sort_ascending = 0;
        else if (fc == 'C') sort_case_insensitive = 1;
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

    /* Collect qualifying records */
    for (i = start; i <= end && nentries < MAX_SORT_ENTRIES; i++) {
        char raw[256];
        dbf_read_record(db, i);
        if (skip_deleted(db->record_buf) || !check_filter(db)) continue;

        if (cond_str) {
            value_t cond;
            if (expr_eval_str(&expr_ctx, cond_str, &cond) != 0) {
                if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
                return;
            }
            if (cond.type != VAL_LOGIC || !cond.logic) continue;
        }

        sort_entries[nentries].recno = i;
        dbf_get_field_raw(db, field_idx, raw, sizeof(raw));
        str_copy(sort_entries[nentries].key, raw, sizeof(sort_entries[nentries].key));
        nentries++;
    }

    if (nentries == 0) {
        printf("No records to sort.\n");
        return;
    }

    /* Sort */
    qsort(sort_entries, nentries, sizeof(sort_entry_t), sort_compare);

    /* Create dest with same structure */
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
            return;
        }
    }

    dbf_init(&dest);
    if (dbf_open(&dest, filename) < 0) {
        printf("Error opening %s\n", filename);
        return;
    }

    /* Write records in sorted order */
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

    dbf_close(&dest);
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
        printf("File not found: %s\n", filename);
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
            printf("File not found: %s\n", oldname);
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
        printf("File not found: %s\n", src);
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
        printf("No database in use.\n");
        return;
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
    close_all_indexes(&areas[current_area]);

    /* Build the index in slot 0 */
    if (index_build(&areas[current_area].indexes[0], db, &expr_ctx, key_expr, filename) < 0) {
        printf("Error building index.\n");
        return;
    }

    /* Write to file */
    if (index_write(&areas[current_area].indexes[0]) < 0) {
        printf("Error writing index file.\n");
        return;
    }

    areas[current_area].num_indexes = 1;
    areas[current_area].order = 1;

    /* Position to first indexed record */
    if (areas[current_area].indexes[0].nentries > 0) {
        uint32_t rec;
        index_top(&areas[current_area].indexes[0]);
        rec = index_current_recno(&areas[current_area].indexes[0]);
        if (rec > 0) dbf_read_record(db, rec);
        expr_ctx.eof_flag = 0;
        expr_ctx.bof_flag = 0;
    }

    printf("%d record(s) indexed.\n", areas[current_area].indexes[0].nentries);
}

/* ---- SET INDEX TO [file1 [, file2 ...]] ---- */
static void cmd_set_index(dbf_t *db, const char *arg) {
    const char *p = skip_ws(arg);

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    /* Close all existing indexes */
    close_all_indexes(&areas[current_area]);

    if (*p == '\0') {
        /* SET INDEX TO with no args = close all */
        printf("Index closed.\n");
        return;
    }

    /* Parse comma-separated index files */
    while (*p && areas[current_area].num_indexes < MAX_INDEXES) {
        char filename[64];
        int i = 0;
        while (*p && *p != ' ' && *p != '\t' && *p != ',' && i < 63)
            filename[i++] = *p++;
        filename[i] = '\0';

        if (filename[0]) {
            int slot = areas[current_area].num_indexes;
            ensure_ndx_ext(filename, sizeof(filename));
            if (index_read(&areas[current_area].indexes[slot], filename) == 0) {
                areas[current_area].num_indexes++;
                printf("Index %s opened (%d entries).\n", filename,
                       areas[current_area].indexes[slot].nentries);
            } else {
                printf("Cannot open index: %s\n", filename);
            }
        }

        p = skip_ws(p);
        if (*p == ',') { p++; p = skip_ws(p); }
        else break;
    }

    /* Set controlling index to first one */
    if (areas[current_area].num_indexes > 0) {
        index_t *idx;
        uint32_t rec;
        areas[current_area].order = 1;

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
    char key[MAX_INDEX_KEY];
    index_t *idx;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    idx = controlling_index();
    if (!idx) {
        printf("No index active.\n");
        return;
    }

    if (expr_eval_str(&expr_ctx, p, &val) != 0) {
        if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
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
    char key[MAX_INDEX_KEY];
    index_t *idx;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
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
    work_area_t *wa = &areas[current_area];
    int i, total = 0;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
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
        if (dbf_is_open(&areas[i].db))
            dbf_close(&areas[i].db);
        close_all_indexes(&areas[i]);
        areas[i].alias[0] = '\0';
        areas[i].locate_cond[0] = '\0';
        areas[i].locate_last_rec = 0;
        areas[i].filter_cond[0] = '\0';
    }
    current_area = 0;
}

/* ---- Accessors for program.c ---- */
memvar_store_t *cmd_get_memvar_store(void) {
    return &memvar_store;
}

expr_ctx_t *cmd_get_expr_ctx(void) {
    ctx_setup();
    return &expr_ctx;
}

/* ---- Dispatch ---- */
int cmd_execute(dbf_t *db, char *line) {
    lexer_t l;
    dbf_t *cdb;
    const char *p;
    char cmd[256];

    (void)db;
    ctx_setup();
    cdb = cur_db();

    lexer_init(&l, line);
    if (l.current.type == TOK_EOF) return 0;
    if (l.current.type == TOK_MUL) return 0; /* * comment */

    if (l.current.type != TOK_IDENT) {
        /* Check for special non-ident commands like ?, ??, @ */
        p = l.token_start;
        if (p[0] == '@') {
            screen_at_cmd(p);
            return 0;
        }
        if (p[0] == '?' && p[1] == '?') {
            cmd_print_expr(p + 2, 0);
            return 0;
        }
        if (p[0] == '?') {
            cmd_print_expr(p + 1, 1);
            return 0;
        }
        printf("Syntax error.\n");
        return 0;
    }

    str_copy(cmd, l.current.text, sizeof(cmd));
    p = skip_ws(l.p); /* Pointer to rest of line after command token */

    if (IS_KW(cmd, "NOTE")) return 0;

    if (IS_KW(cmd, "DO")) {
        prog_do(p);
        return 0;
    }

    /* Check for assignment early if it's an ident followed by = */
    {
        lexer_t la = l;
        lex_next(&la);
        if (la.current.type == TOK_EQ) {
            value_t val;
            if (expr_eval_str(&expr_ctx, la.p, &val) != 0) {
                if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
                return 0;
            }
            memvar_set(&memvar_store, cmd, &val);
            return 0;
        }
    }

    /* Control flow commands - only valid during program execution */
    if (IS_KW(cmd, "IF")) {
        if (prog_is_running()) { prog_if(p); }
        else printf("IF not allowed in interactive mode.\n");
        return 0;
    }
    if (IS_KW(cmd, "ELSE")) {
        if (prog_is_running()) prog_else();
        else printf("ELSE without IF.\n");
        return 0;
    }
    if (IS_KW(cmd, "ENDIF")) {
        if (prog_is_running()) prog_endif();
        else printf("ENDIF without IF.\n");
        return 0;
    }
    if (IS_KW(cmd, "ENDDO")) {
        if (prog_is_running()) prog_enddo();
        else printf("ENDDO without DO WHILE.\n");
        return 0;
    }
    if (IS_KW(cmd, "LOOP")) {
        if (prog_is_running()) prog_loop();
        else printf("LOOP without DO WHILE.\n");
        return 0;
    }
    if (IS_KW(cmd, "CASE")) {
        if (prog_is_running()) prog_case(p);
        else printf("CASE without DO CASE.\n");
        return 0;
    }
    if (IS_KW(cmd, "OTHERWISE")) {
        if (prog_is_running()) prog_otherwise();
        else printf("OTHERWISE without DO CASE.\n");
        return 0;
    }
    if (IS_KW(cmd, "ENDCASE")) {
        if (prog_is_running()) prog_endcase();
        else printf("ENDCASE without DO CASE.\n");
        return 0;
    }
    if (IS_KW(cmd, "RETURN")) {
        if (prog_is_running()) prog_return(p);
        else printf("RETURN not in program.\n");
        return 0;
    }
    if (IS_KW(cmd, "PROCEDURE")) {
        if (prog_is_running()) prog_procedure(p);
        else printf("PROCEDURE not allowed in interactive mode.\n");
        return 0;
    }
    if (IS_KW(cmd, "PARAMETERS")) {
        if (prog_is_running()) prog_parameters(p);
        else printf("PARAMETERS not allowed in interactive mode.\n");
        return 0;
    }
    if (IS_KW(cmd, "PRIVATE")) {
        prog_private(p);
        return 0;
    }
    if (IS_KW(cmd, "PUBLIC")) {
        prog_public(p);
        return 0;
    }
    if (IS_KW(cmd, "CANCEL")) {
        if (prog_is_running()) prog_cancel();
        return 0;
    }

    if (IS_KW(cmd, "QUIT")) {
        return 1;
    }
    if (IS_KW(cmd, "EXIT")) {
        /* EXIT in loop context = break, otherwise QUIT */
        if (prog_is_running()) {
            prog_exit_loop();
            return 0;
        }
        return 1;
    }

    if (IS_KW(cmd, "SELECT")) {
        cmd_select(p);
        return 0;
    }

    if (IS_KW(cmd, "CREATE")) {
        cmd_create(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "USE")) {
        cmd_use(cdb, p);
        return 0;
    }

    /* COPY TO / COPY STRUCTURE TO */
    if (IS_KW(cmd, "COPY")) {
        lexer_t l2;
        lexer_init(&l2, p);
        if (l2.current.type == TOK_IDENT) {
            if (IS_KW(l2.current.text, "STRUCTURE")) {
                lex_next(&l2);
                if (l2.current.type == TOK_IDENT && IS_KW(l2.current.text, "TO")) {
                    cmd_copy_structure(cdb, skip_ws(l2.p));
                } else {
                    printf("Syntax: COPY STRUCTURE TO <filename>\n");
                }
            } else if (IS_KW(l2.current.text, "FILE")) {
                cmd_copy_file(skip_ws(l2.p));
            } else if (IS_KW(l2.current.text, "TO")) {
                cmd_copy_to(cdb, skip_ws(l2.p));
            } else {
                printf("Syntax: COPY TO <filename> | COPY FILE <src> TO <dst> | COPY STRUCTURE TO <filename>\n");
            }
        } else {
            printf("Syntax: COPY TO <filename> | COPY FILE <src> TO <dst> | COPY STRUCTURE TO <filename>\n");
        }
        return 0;
    }

    /* APPEND BLANK / APPEND FROM */
    if (IS_KW(cmd, "APPEND")) {
        lexer_t l2;
        lexer_init(&l2, p);
        if (l2.current.type == TOK_IDENT) {
            if (IS_KW(l2.current.text, "BLANK")) {
                cmd_append_blank(cdb);
            } else if (IS_KW(l2.current.text, "FROM")) {
                cmd_append_from(cdb, skip_ws(l2.p));
            } else {
                printf("Syntax: APPEND BLANK | APPEND FROM <filename>\n");
            }
        } else {
            printf("Syntax: APPEND BLANK | APPEND FROM <filename>\n");
        }
        return 0;
    }

    if (IS_KW(cmd, "SORT")) {
        lexer_t l2;
        lexer_init(&l2, p);
        if (l2.current.type == TOK_IDENT && IS_KW(l2.current.text, "TO")) {
            cmd_sort(cdb, skip_ws(l2.p));
        } else {
            printf("Syntax: SORT TO <filename> ON <field> [/A][/D][/C]\n");
        }
        return 0;
    }

    if (IS_KW(cmd, "REPLACE")) {
        cmd_replace(cdb, p);
        return 0;
    }

    /* GO / GOTO */
    if (IS_KW(cmd, "GOTO") || IS_KW(cmd, "GO")) {
        cmd_go(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "SKIP")) {
        cmd_skip(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "LOCATE")) {
        cmd_locate(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "CONTINUE")) {
        cmd_continue(cdb);
        return 0;
    }

    if (IS_KW(cmd, "COUNT")) {
        cmd_count(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "SUM")) {
        cmd_sum(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "AVERAGE")) {
        cmd_average(cdb, p);
        return 0;
    }

    /* DISPLAY STRUCTURE / DISPLAY MEMORY must be checked before plain DISPLAY */
    if (IS_KW(cmd, "DISPLAY")) {
        lexer_t l2;
        lexer_init(&l2, p);
        if (l2.current.type == TOK_IDENT) {
            if (IS_KW(l2.current.text, "STRUCTURE")) {
                cmd_display_structure(cdb);
            } else if (IS_KW(l2.current.text, "MEMORY")) {
                memvar_display(&memvar_store);
            } else {
                cmd_display(cdb, p);
            }
        } else {
            cmd_display(cdb, p);
        }
        return 0;
    }

    if (IS_KW(cmd, "LIST")) {
        cmd_list(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "STORE")) {
        cmd_store(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "RELEASE")) {
        cmd_release(p);
        return 0;
    }

    if (IS_KW(cmd, "SET")) {
        lexer_t l2;
        lexer_init(&l2, p);
        if (l2.current.type == TOK_IDENT) {
            char set_cmd[64];
            str_copy(set_cmd, l2.current.text, sizeof(set_cmd));
            if (IS_KW(set_cmd, "COLOR")) {
                lex_next(&l2);
                if (l2.current.type == TOK_IDENT && IS_KW(l2.current.text, "TO"))
                    screen_set_color(skip_ws(l2.p));
                else
                    printf("Syntax: SET COLOR TO <color-spec>\n");
            } else if (IS_KW(set_cmd, "INDEX")) {
                lex_next(&l2);
                if (l2.current.type == TOK_IDENT && IS_KW(l2.current.text, "TO"))
                    cmd_set_index(cdb, skip_ws(l2.p));
                else
                    printf("Syntax: SET INDEX TO [filename]\n");
            } else if (IS_KW(set_cmd, "PROCEDURE")) {
                lex_next(&l2);
                if (l2.current.type == TOK_IDENT && IS_KW(l2.current.text, "TO")) {
                    prog_set_procedure(skip_ws(l2.p));
                } else {
                    printf("Syntax: SET PROCEDURE TO [filename]\n");
                }
            } else if (IS_KW(set_cmd, "FILTER")) {
                lex_next(&l2);
                if (l2.current.type == TOK_IDENT && IS_KW(l2.current.text, "TO")) {
                    const char *rest = skip_ws(l2.p);
                    if (*rest == '\0') {
                        areas[current_area].filter_cond[0] = '\0';
                        printf("Filter removed.\n");
                    } else {
                        str_copy(areas[current_area].filter_cond, rest,
                                 sizeof(areas[current_area].filter_cond));
                        printf("Filter: %s\n", areas[current_area].filter_cond);
                    }
                } else {
                    printf("Syntax: SET FILTER TO [condition]\n");
                }
            } else if (IS_KW(set_cmd, "ORDER")) {
                lex_next(&l2);
                if (l2.current.type == TOK_IDENT && IS_KW(l2.current.text, "TO")) {
                    lex_next(&l2);
                    if (l2.current.type == TOK_EOF) {
                        areas[current_area].order = 0;
                        printf("Natural record order.\n");
                    } else if (l2.current.type == TOK_NUMBER) {
                        int n = (int)l2.current.num_val;
                        if (n < 0 || n > areas[current_area].num_indexes) {
                            printf("Index number out of range (0-%d).\n", areas[current_area].num_indexes);
                        } else {
                            areas[current_area].order = n;
                            if (n > 0) {
                                index_t *idx = &areas[current_area].indexes[n - 1];
                                printf("Order set to %d (%s).\n", n, idx->filename);
                            } else {
                                printf("Natural record order.\n");
                            }
                        }
                    } else {
                        printf("Syntax: SET ORDER TO <n>\n");
                    }
                } else {
                    printf("Syntax: SET ORDER TO <n>\n");
                }
            } else {
                set_execute(&set_opts, p);
            }
        } else {
            set_execute(&set_opts, p);
        }
        return 0;
    }

    /* INDEX ON <expr> TO <file> */
    if (IS_KW(cmd, "INDEX")) {
        lexer_t l2;
        lexer_init(&l2, p);
        if (l2.current.type == TOK_IDENT && IS_KW(l2.current.text, "ON")) {
            cmd_index_on(cdb, skip_ws(l2.p));
        } else {
            printf("Syntax: INDEX ON <expr> TO <filename>\n");
        }
        return 0;
    }

    if (IS_KW(cmd, "SEEK")) {
        cmd_seek(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "FIND")) {
        cmd_find(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "REINDEX")) {
        cmd_reindex(cdb);
        return 0;
    }

    if (IS_KW(cmd, "DELETE")) {
        cmd_delete(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "RECALL")) {
        cmd_recall(cdb, p);
        return 0;
    }

    if (IS_KW(cmd, "PACK")) {
        cmd_pack(cdb);
        return 0;
    }

    if (IS_KW(cmd, "ZAP")) {
        cmd_zap(cdb);
        return 0;
    }

    if (IS_KW(cmd, "ERASE")) {
        cmd_erase(p);
        return 0;
    }

    if (IS_KW(cmd, "RENAME")) {
        cmd_rename(p);
        return 0;
    }

    if (IS_KW(cmd, "ACCEPT")) {
        cmd_accept(p);
        return 0;
    }

    if (IS_KW(cmd, "INPUT")) {
        cmd_input(p);
        return 0;
    }

    if (IS_KW(cmd, "WAIT")) {
        cmd_wait(p);
        return 0;
    }

    if (IS_KW(cmd, "CLEAR")) {
        screen_clear();
        return 0;
    }

    if (IS_KW(cmd, "READ")) {
        screen_read();
        return 0;
    }

    /* Variable assignment: moved to top of dispatch */

    printf("Unknown command: %s\n", cmd);
    return 0;
}