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

/* Persistent expression context */
static expr_ctx_t expr_ctx;

/* LOCATE state */
static char locate_condition[256];
static uint32_t locate_last_rec;

/* ---- Helper: initialize expr_ctx for a db ---- */
static void ctx_setup(dbf_t *db) {
    expr_ctx.db = db;
    /* found, bof_flag, eof_flag persist across commands */
}

/* ---- Helper: check FOR clause ---- */
/* Returns pointer past "FOR" keyword, or NULL if no FOR clause present */
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
/* Parse comma-separated field names. Returns count, fills indices[].
   Sets *rest to point past the field list. */
static int parse_field_list(dbf_t *db, const char *arg, int *indices, int max_fields, const char **rest) {
    const char *p = skip_ws(arg);
    int count = 0;
    char name[DBF_MAX_FIELD_NAME];

    *rest = p;

    while (*p && count < max_fields) {
        int i, idx;

        /* Check if we hit FOR keyword */
        if (str_imatch(p, "FOR")) break;

        /* Parse field name */
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
    trim_right(filename);
    str_upper(filename);
    if (strlen(filename) < 5 || str_icmp(filename + strlen(filename) - 4, ".DBF") != 0) {
        if (strlen(filename) + 4 < sizeof(filename))
            strcat(filename, ".DBF");
    }

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

/* ---- USE ---- */
static void cmd_use(dbf_t *db, const char *arg) {
    char filename[64];

    arg = skip_ws(arg);

    if (*arg == '\0') {
        if (dbf_is_open(db)) {
            dbf_close(db);
            printf("Database closed.\n");
        }
        return;
    }

    if (dbf_is_open(db))
        dbf_close(db);

    str_copy(filename, arg, sizeof(filename));
    trim_right(filename);
    str_upper(filename);
    if (strlen(filename) < 5 || str_icmp(filename + strlen(filename) - 4, ".DBF") != 0) {
        if (strlen(filename) + 4 < sizeof(filename))
            strcat(filename, ".DBF");
    }

    if (dbf_open(db, filename) < 0) {
        printf("File not found: %s\n", filename);
        return;
    }

    /* Reset navigation state */
    expr_ctx.eof_flag = 0;
    expr_ctx.bof_flag = 0;
    expr_ctx.found = 0;
    locate_condition[0] = '\0';
    locate_last_rec = 0;

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
        dbf_read_record(db, 1);
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
        dbf_read_record(db, db->record_count);
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
    int32_t target;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    p = skip_ws(arg);
    if (*p == '\0')
        n = 1;
    else
        n = atoi(p);

    if (db->current_record == 0) {
        /* No current record — go to first */
        if (db->record_count == 0) {
            expr_ctx.eof_flag = 1;
            return;
        }
        dbf_read_record(db, 1);
        expr_ctx.eof_flag = 0;
        expr_ctx.bof_flag = 0;
        if (n > 0) n--; /* already moved forward 1 */
        if (n == 0) return;
    }

    target = (int32_t)db->current_record + n;

    if (target < 1) {
        dbf_read_record(db, 1);
        expr_ctx.bof_flag = 1;
        expr_ctx.eof_flag = 0;
        return;
    }

    if ((uint32_t)target > db->record_count) {
        /* Move past EOF — stay at record_count but set eof_flag.
           dBase III behavior: current_record = record_count + 1 conceptually,
           but we keep it at record_count and set the flag. */
        expr_ctx.eof_flag = 1;
        expr_ctx.bof_flag = 0;
        /* Keep current_record at last record; some dBase impls set it to count+1 */
        db->current_record = db->record_count + 1;
        return;
    }

    dbf_read_record(db, (uint32_t)target);
    expr_ctx.eof_flag = 0;
    expr_ctx.bof_flag = 0;
}

/* ---- LOCATE FOR ---- */
static void cmd_locate(dbf_t *db, const char *arg) {
    const char *p;
    uint32_t i;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    p = skip_ws(arg);
    if (!str_imatch(p, "FOR")) {
        printf("Syntax: LOCATE FOR <condition>\n");
        return;
    }
    p = skip_ws(p + 3);

    /* Save condition for CONTINUE */
    str_copy(locate_condition, p, sizeof(locate_condition));

    expr_ctx.found = 0;

    for (i = 1; i <= db->record_count; i++) {
        value_t cond;
        dbf_read_record(db, i);
        if (db->record_buf[0] == '*') continue; /* skip deleted */

        if (expr_eval_str(&expr_ctx, locate_condition, &cond) != 0) {
            if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
            return;
        }

        if (cond.type == VAL_LOGIC && cond.logic) {
            expr_ctx.found = 1;
            expr_ctx.eof_flag = 0;
            locate_last_rec = i;
            printf("Record = %d\n", (int)i);
            return;
        }
    }

    /* Not found */
    expr_ctx.found = 0;
    expr_ctx.eof_flag = 1;
    locate_last_rec = 0;
    printf("End of LOCATE scope\n");
}

/* ---- CONTINUE ---- */
static void cmd_continue(dbf_t *db) {
    uint32_t i;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    if (locate_condition[0] == '\0') {
        printf("No LOCATE active.\n");
        return;
    }

    for (i = locate_last_rec + 1; i <= db->record_count; i++) {
        value_t cond;
        dbf_read_record(db, i);
        if (db->record_buf[0] == '*') continue;

        if (expr_eval_str(&expr_ctx, locate_condition, &cond) != 0) {
            if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
            return;
        }

        if (cond.type == VAL_LOGIC && cond.logic) {
            expr_ctx.found = 1;
            expr_ctx.eof_flag = 0;
            locate_last_rec = i;
            printf("Record = %d\n", (int)i);
            return;
        }
    }

    expr_ctx.found = 0;
    expr_ctx.eof_flag = 1;
    printf("End of LOCATE scope\n");
}

/* ---- COUNT ---- */
static void cmd_count(dbf_t *db, const char *arg) {
    const char *p;
    const char *cond_str = NULL;
    uint32_t i;
    int count = 0;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    p = skip_ws(arg);
    if (str_imatch(p, "FOR")) {
        cond_str = skip_ws(p + 3);
    }

    for (i = 1; i <= db->record_count; i++) {
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
        count++;
    }

    printf("%d record(s)\n", count);
}

/* ---- SUM ---- */
static void cmd_sum(dbf_t *db, const char *arg) {
    const char *p;
    char expr_str[256];
    const char *cond_str = NULL;
    uint32_t i;
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

    /* Extract expression (everything before FOR) */
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

    for (i = 1; i <= db->record_count; i++) {
        value_t val;
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

/* ---- AVERAGE ---- */
static void cmd_average(dbf_t *db, const char *arg) {
    const char *p;
    char expr_str[256];
    const char *cond_str = NULL;
    uint32_t i;
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

    for (i = 1; i <= db->record_count; i++) {
        value_t val;
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

        /* Parse field name */
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

        /* Skip "WITH" */
        p = skip_ws(p);
        if (str_imatch(p, "WITH")) {
            p += 4;
            p = skip_ws(p);
        }

        /* Evaluate expression for value */
        if (expr_eval(&expr_ctx, &p, &val) != 0) {
            if (expr_ctx.error) printf("Error: %s\n", expr_ctx.error);
            return;
        }

        /* Convert value to field format */
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

        /* Skip comma separator */
        p = skip_ws(p);
        if (*p == ',') {
            p++;
            p = skip_ws(p);
        }
    }

    dbf_flush_record(db);
}

/* ---- LIST (enhanced with field list + FOR clause) ---- */
static void cmd_list(dbf_t *db, const char *arg) {
    uint32_t i;
    int f;
    char raw[256], display[256];
    int field_indices[DBF_MAX_FIELDS];
    int nfields = 0;
    const char *rest;
    const char *cond_str = NULL;
    int use_all_fields;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    if (db->record_count == 0) {
        printf("No records.\n");
        return;
    }

    rest = skip_ws(arg);

    /* Check if there's a FOR clause directly */
    if (str_imatch(rest, "FOR")) {
        cond_str = skip_ws(rest + 3);
        nfields = 0;
    } else if (*rest != '\0') {
        /* Try to parse a field list */
        nfields = parse_field_list(db, rest, field_indices, DBF_MAX_FIELDS, &rest);
        if (nfields < 0) return; /* error already printed */
        rest = skip_ws(rest);
        if (str_imatch(rest, "FOR")) {
            cond_str = skip_ws(rest + 3);
        }
    }

    use_all_fields = (nfields == 0);

    /* Print header */
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

    /* Print records */
    for (i = 1; i <= db->record_count; i++) {
        if (dbf_read_record(db, i) < 0) continue;
        if (db->record_buf[0] == '*') continue;

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

/* ---- ? / ?? (print expressions) ---- */
static void cmd_print_expr(dbf_t *db, const char *arg, int newline) {
    const char *p;
    int first = 1;

    (void)db;
    ctx_setup(db);

    p = skip_ws(arg);

    if (*p == '\0') {
        /* Bare ? — just print newline */
        if (newline) printf("\n");
        return;
    }

    /* Evaluate comma-separated expressions */
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

/* ---- Dispatch ---- */
int cmd_execute(dbf_t *db, char *line) {
    char *p;

    ctx_setup(db);

    p = skip_ws(line);
    if (*p == '\0') return 0;

    if (str_imatch(p, "QUIT") || str_imatch(p, "EXIT")) {
        return 1;
    }

    if (str_imatch(p, "CREATE")) {
        cmd_create(db, p + 6);
        return 0;
    }

    if (str_imatch(p, "USE")) {
        cmd_use(db, p + 3);
        return 0;
    }

    /* APPEND BLANK */
    if (str_imatch(p, "APPEND")) {
        char *rest = skip_ws(p + 6);
        if (str_imatch(rest, "BLANK")) {
            cmd_append_blank(db);
        } else {
            printf("Syntax: APPEND BLANK\n");
        }
        return 0;
    }

    if (str_imatch(p, "REPLACE")) {
        cmd_replace(db, p + 7);
        return 0;
    }

    /* GO / GOTO */
    if (str_imatch(p, "GOTO")) {
        cmd_go(db, p + 4);
        return 0;
    }
    if (str_imatch(p, "GO")) {
        cmd_go(db, p + 2);
        return 0;
    }

    if (str_imatch(p, "SKIP")) {
        cmd_skip(db, p + 4);
        return 0;
    }

    if (str_imatch(p, "LOCATE")) {
        cmd_locate(db, p + 6);
        return 0;
    }

    if (str_imatch(p, "CONTINUE")) {
        cmd_continue(db);
        return 0;
    }

    if (str_imatch(p, "COUNT")) {
        cmd_count(db, p + 5);
        return 0;
    }

    if (str_imatch(p, "SUM")) {
        cmd_sum(db, p + 3);
        return 0;
    }

    if (str_imatch(p, "AVERAGE")) {
        cmd_average(db, p + 7);
        return 0;
    }

    /* DISPLAY STRUCTURE must be checked before plain DISPLAY */
    if (str_imatch(p, "DISPLAY")) {
        char *rest = skip_ws(p + 7);
        if (str_imatch(rest, "STRUCTURE")) {
            cmd_display_structure(db);
        } else {
            cmd_display(db, rest);
        }
        return 0;
    }

    if (str_imatch(p, "LIST")) {
        cmd_list(db, p + 4);
        return 0;
    }

    /* ?? (no newline) must be checked before ? */
    if (p[0] == '?' && p[1] == '?') {
        cmd_print_expr(db, p + 2, 0);
        return 0;
    }

    if (p[0] == '?') {
        cmd_print_expr(db, p + 1, 1);
        return 0;
    }

    printf("Unrecognized command.\n");
    return 0;
}
