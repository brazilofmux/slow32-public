#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "command.h"
#include "dbf.h"
#include "field.h"
#include "util.h"

/* ---- CREATE ---- */
static void cmd_create(dbf_t *db, const char *arg) {
    char filename[64];
    dbf_field_t fields[DBF_MAX_FIELDS];
    int nfields = 0;
    char line[256];
    char type_line[16];
    char width_line[16];
    char dec_line[16];

    arg = skip_ws(arg);
    if (*arg == '\0') {
        printf("Syntax: CREATE <filename>\n");
        return;
    }

    /* Copy filename, uppercase, append .DBF if needed */
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

        fields[nfields].offset = 0; /* will be computed by dbf_create/open */
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

    /* USE with no argument closes current db */
    if (*arg == '\0') {
        if (dbf_is_open(db)) {
            dbf_close(db);
            printf("Database closed.\n");
        }
        return;
    }

    /* Close current if open */
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

    printf("Record %d added.\n", (int)db->record_count);
}

/* ---- REPLACE ---- */
static void cmd_replace(dbf_t *db, const char *arg) {
    char field_name[DBF_MAX_FIELD_NAME];
    char value[256];
    char formatted[256];
    int idx;
    const char *p;

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }
    if (db->current_record == 0) {
        printf("No current record. Use APPEND BLANK or GO first.\n");
        return;
    }

    p = skip_ws(arg);

    while (*p) {
        /* Parse field name */
        {
            int i = 0;
            while (*p && *p != ' ' && *p != '\t' && i < DBF_MAX_FIELD_NAME - 1) {
                field_name[i++] = *p++;
            }
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

        /* Parse value */
        if (*p == '"' || *p == '\'') {
            /* String literal */
            char quote = *p++;
            int i = 0;
            while (*p && *p != quote && i < (int)sizeof(value) - 1)
                value[i++] = *p++;
            value[i] = '\0';
            if (*p == quote) p++;
        } else if (*p == '{') {
            /* Date literal */
            int i = 0;
            p++; /* skip { */
            while (*p && *p != '}' && i < (int)sizeof(value) - 1)
                value[i++] = *p++;
            value[i] = '\0';
            if (*p == '}') p++;
        } else if (*p == '.') {
            /* Logical: .T. or .F. */
            int i = 0;
            while (*p && *p != ' ' && *p != ',' && i < (int)sizeof(value) - 1)
                value[i++] = *p++;
            value[i] = '\0';
        } else {
            /* Numeric or other */
            int i = 0;
            while (*p && *p != ' ' && *p != ',' && *p != '\t' && i < (int)sizeof(value) - 1)
                value[i++] = *p++;
            value[i] = '\0';
        }

        /* Format and store */
        switch (db->fields[idx].type) {
        case 'C':
            field_format_char(formatted, db->fields[idx].length, value);
            break;
        case 'N':
            field_format_numeric(formatted, db->fields[idx].length,
                                 db->fields[idx].decimals, value);
            break;
        case 'D':
            field_format_date(formatted, value);
            break;
        case 'L':
            field_format_logical(formatted, value);
            break;
        default:
            field_format_char(formatted, db->fields[idx].length, value);
            break;
        }

        dbf_set_field_raw(db, idx, formatted);

        /* Skip comma separator if present */
        p = skip_ws(p);
        if (*p == ',') {
            p++;
            p = skip_ws(p);
        }
    }

    dbf_flush_record(db);
}

/* ---- LIST ---- */
static void cmd_list(dbf_t *db) {
    uint32_t i;
    int f;
    char raw[256], display[256];

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }

    if (db->record_count == 0) {
        printf("No records.\n");
        return;
    }

    /* Print header */
    printf("Record#");
    for (f = 0; f < db->field_count; f++) {
        int w = db->fields[f].length;
        int nlen = strlen(db->fields[f].name);
        if (w < nlen) w = nlen;
        if (db->fields[f].type == 'D') w = 8; /* MM/DD/YY */
        if (db->fields[f].type == 'L') w = 3; /* .T. */
        printf(" %-*s", w, db->fields[f].name);
    }
    printf("\n");

    /* Print records */
    for (i = 1; i <= db->record_count; i++) {
        if (dbf_read_record(db, i) < 0) continue;

        /* Skip deleted records */
        if (db->record_buf[0] == '*') continue;

        printf("%7d", (int)i);
        for (f = 0; f < db->field_count; f++) {
            int w = db->fields[f].length;
            int nlen = strlen(db->fields[f].name);
            if (w < nlen) w = nlen;

            dbf_get_field_raw(db, f, raw, sizeof(raw));
            switch (db->fields[f].type) {
            case 'C':
                field_display_char(display, raw, db->fields[f].length);
                trim_right(display);
                printf(" %-*s", w, display);
                break;
            case 'N':
                field_display_numeric(display, raw, db->fields[f].length);
                printf(" %*s", w, display);
                break;
            case 'D':
                field_display_date(display, raw);
                printf(" %-8s", display);
                break;
            case 'L':
                field_display_logical(display, raw);
                printf(" %-3s", display);
                break;
            default:
                field_display_char(display, raw, db->fields[f].length);
                printf(" %-*s", w, display);
                break;
            }
        }
        printf("\n");
    }
}

/* ---- DISPLAY ---- */
static void cmd_display(dbf_t *db) {
    int f;
    char raw[256], display[256];

    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }
    if (db->current_record == 0) {
        printf("No current record.\n");
        return;
    }

    printf("Record# %d\n", (int)db->current_record);
    for (f = 0; f < db->field_count; f++) {
        dbf_get_field_raw(db, f, raw, sizeof(raw));
        switch (db->fields[f].type) {
        case 'C':
            field_display_char(display, raw, db->fields[f].length);
            trim_right(display);
            break;
        case 'N':
            field_display_numeric(display, raw, db->fields[f].length);
            break;
        case 'D':
            field_display_date(display, raw);
            break;
        case 'L':
            field_display_logical(display, raw);
            break;
        default:
            field_display_char(display, raw, db->fields[f].length);
            break;
        }
        printf("%10s: %s\n", db->fields[f].name, display);
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

/* ---- ? (print expression) ---- */
static void cmd_print_expr(dbf_t *db, const char *arg) {
    char name_buf[DBF_MAX_FIELD_NAME];
    int idx;
    char raw[256], display[256];
    const char *p;

    p = skip_ws(arg);

    /* Check if it's a string literal */
    if (*p == '"' || *p == '\'') {
        char quote = *p++;
        while (*p && *p != quote)
            putchar(*p++);
        printf("\n");
        return;
    }

    /* Check if it's a number */
    if ((*p >= '0' && *p <= '9') || *p == '-' || *p == '+') {
        while (*p && *p != ' ' && *p != '\t')
            putchar(*p++);
        printf("\n");
        return;
    }

    /* Try field name */
    if (!dbf_is_open(db)) {
        printf("No database in use.\n");
        return;
    }
    if (db->current_record == 0) {
        printf("No current record.\n");
        return;
    }

    {
        int i = 0;
        while (*p && *p != ' ' && *p != '\t' && i < DBF_MAX_FIELD_NAME - 1)
            name_buf[i++] = *p++;
        name_buf[i] = '\0';
    }

    idx = dbf_find_field(db, name_buf);
    if (idx < 0) {
        printf("Variable not found: %s\n", name_buf);
        return;
    }

    dbf_get_field_raw(db, idx, raw, sizeof(raw));
    switch (db->fields[idx].type) {
    case 'C':
        field_display_char(display, raw, db->fields[idx].length);
        trim_right(display);
        break;
    case 'N':
        field_display_numeric(display, raw, db->fields[idx].length);
        break;
    case 'D':
        field_display_date(display, raw);
        break;
    case 'L':
        field_display_logical(display, raw);
        break;
    default:
        field_display_char(display, raw, db->fields[idx].length);
        break;
    }
    printf("%s\n", display);
}

/* ---- Dispatch ---- */
int cmd_execute(dbf_t *db, char *line) {
    char *p;

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

    /* APPEND BLANK must be checked before a standalone APPEND */
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

    /* DISPLAY STRUCTURE must be checked before plain DISPLAY */
    if (str_imatch(p, "DISPLAY")) {
        char *rest = skip_ws(p + 7);
        if (str_imatch(rest, "STRUCTURE")) {
            cmd_display_structure(db);
        } else {
            cmd_display(db);
        }
        return 0;
    }

    if (str_imatch(p, "LIST")) {
        cmd_list(db);
        return 0;
    }

    if (*p == '?') {
        cmd_print_expr(db, p + 1);
        return 0;
    }

    printf("Unrecognized command.\n");
    return 0;
}
