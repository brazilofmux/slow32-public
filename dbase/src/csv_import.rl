/*
 * csv_import.rl — Streaming CSV parser using Ragel -G2 (goto-driven).
 *
 * Supports:
 *   - Standard CSV (comma-separated, double-quote delimited)
 *   - Custom quote character (WITH <char>)
 *   - BLANK mode (space-delimited, no quotes) — separate function, no Ragel
 *
 * Generate with:  ragel -G2 csv_import.rl -o csv_import.c
 */

#include <stdio.h>
#include <string.h>
#include "csv_import.h"

static void add_char(csv_parser_t *ps, unsigned char ch) {
    if (ps->field_len < CSV_MAX_FIELD_LEN - 1)
        ps->field_buf[ps->field_len++] = ch;
}

static void save_field(csv_parser_t *ps) {
    ps->field_buf[ps->field_len] = '\0';
    if (ps->row.num_fields < CSV_MAX_FIELDS) {
        memcpy(ps->row.fields[ps->row.num_fields],
               ps->field_buf, ps->field_len + 1);
        ps->row.num_fields++;
    }
    ps->field_len = 0;
}

/* Returns non-zero if callback requested stop */
static int emit_row(csv_parser_t *ps) {
    int rc = 0;
    ps->line_number++;
    if (ps->row.num_fields > 0) {
        if (ps->callback)
            rc = ps->callback(ps->line_number, &ps->row, ps->user_data);
    }
    ps->row.num_fields = 0;
    ps->field_len = 0;
    return rc;
}

/* ---- Ragel machine definition ---- */

%%{
    machine csv_delimited;
    alphtype unsigned char;

    variable cs p_state->cs;

    # Start/main state: beginning of a field
    main_state := ( any @{
        unsigned char ch = fc;
        if (ch == '\n') {
            save_field(p_state);
            if (emit_row(p_state)) { p_state->error = 2; fbreak; }
        } else if (ch == '\r') {
            save_field(p_state);
            fgoto cr_state;
        } else if (ch == ',') {
            save_field(p_state);
        } else if (ch == (unsigned char)p_state->quote_char) {
            fgoto quoted_state;
        } else {
            add_char(p_state, ch);
            fgoto unquoted_state;
        }
    } )*;

    # Inside a quoted field
    quoted_state := ( any @{
        unsigned char ch = fc;
        if (ch == (unsigned char)p_state->quote_char) {
            fgoto after_quote_state;
        } else {
            add_char(p_state, ch);
        }
    } )*;

    # Just saw a closing quote — check for escaped quote, delimiter, or EOL
    after_quote_state := ( any @{
        unsigned char ch = fc;
        if (ch == (unsigned char)p_state->quote_char) {
            /* Escaped quote: "" -> " */
            add_char(p_state, ch);
            fgoto quoted_state;
        } else if (ch == ',') {
            save_field(p_state);
            fgoto main_state;
        } else if (ch == '\n') {
            save_field(p_state);
            if (emit_row(p_state)) { p_state->error = 2; fbreak; }
            fgoto main_state;
        } else if (ch == '\r') {
            save_field(p_state);
            fgoto cr_after_state;
        } else {
            /* Char after closing quote — tolerate, treat as content */
            add_char(p_state, ch);
            fgoto unquoted_state;
        }
    } )*;

    # Unquoted field content
    unquoted_state := ( any @{
        unsigned char ch = fc;
        if (ch == ',') {
            save_field(p_state);
            fgoto main_state;
        } else if (ch == '\n') {
            save_field(p_state);
            if (emit_row(p_state)) { p_state->error = 2; fbreak; }
            fgoto main_state;
        } else if (ch == '\r') {
            save_field(p_state);
            fgoto cr_after_state;
        } else {
            add_char(p_state, ch);
        }
    } )*;

    # After \r in main state (field already saved)
    cr_state := ( any @{
        unsigned char ch = fc;
        if (ch == '\n') {
            if (emit_row(p_state)) { p_state->error = 2; fbreak; }
            fgoto main_state;
        } else {
            /* Bare \r — treat as row end, re-process this char */
            if (emit_row(p_state)) { p_state->error = 2; fbreak; }
            fhold; fgoto main_state;
        }
    } )*;

    # After \r when field was already saved by a previous state
    cr_after_state := ( any @{
        unsigned char ch = fc;
        if (ch == '\n') {
            if (emit_row(p_state)) { p_state->error = 2; fbreak; }
            fgoto main_state;
        } else {
            if (emit_row(p_state)) { p_state->error = 2; fbreak; }
            fhold; fgoto main_state;
        }
    } )*;

    # Entry point
    main := ( any @{
        unsigned char ch = fc;
        if (ch == '\n') {
            save_field(p_state);
            if (emit_row(p_state)) { p_state->error = 2; fbreak; }
        } else if (ch == '\r') {
            save_field(p_state);
            fgoto cr_state;
        } else if (ch == ',') {
            save_field(p_state);
        } else if (ch == (unsigned char)p_state->quote_char) {
            fgoto quoted_state;
        } else {
            add_char(p_state, ch);
            fgoto unquoted_state;
        }
    } )*;

}%%

%% write data nofinal;

void csv_parser_init(csv_parser_t *p_state, char quote_char, csv_row_cb cb, void *ud) {
    memset(p_state, 0, sizeof(*p_state));
    p_state->quote_char = quote_char;
    p_state->callback = cb;
    p_state->user_data = ud;
    p_state->line_number = 0;
    %% write init;
}

int csv_parser_feed(csv_parser_t *p_state, const char *data, int len, int is_eof) {
    const unsigned char *p = (const unsigned char *)data;
    const unsigned char *pe = p + len;
    const unsigned char *eof = is_eof ? pe : 0;

    /* Suppress unused-variable warnings for Ragel entry points */
    (void)csv_delimited_en_main;
    (void)csv_delimited_en_main_state;
    (void)csv_delimited_en_quoted_state;
    (void)csv_delimited_en_after_quote_state;
    (void)csv_delimited_en_unquoted_state;
    (void)csv_delimited_en_cr_state;
    (void)csv_delimited_en_cr_after_state;

    %% write exec;

    if (p_state->error)
        return -1;

    /* On EOF, flush any remaining field/row */
    if (is_eof) {
        if (p_state->field_len > 0 || p_state->row.num_fields > 0) {
            save_field(p_state);
            emit_row(p_state);
        }
    }

    return 0;
}

/* ---- BLANK mode: space-delimited line reader ---- */

int csv_import_blank(const char *filename, csv_row_cb cb, void *ud) {
    FILE *fp;
    char line[4096];
    int line_number = 0;
    static csv_row_t row;

    fp = fopen(filename, "rb");
    if (!fp) return -1;

    while (fgets(line, sizeof(line), fp)) {
        char *s = line;
        char *end;
        int len;

        /* Strip trailing \r\n */
        len = strlen(line);
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r'))
            line[--len] = '\0';

        if (len == 0) continue; /* skip blank lines */

        line_number++;
        row.num_fields = 0;

        /* Split on spaces */
        while (*s && row.num_fields < CSV_MAX_FIELDS) {
            while (*s == ' ') s++;
            if (*s == '\0') break;

            end = s;
            while (*end && *end != ' ') end++;

            len = end - s;
            if (len >= CSV_MAX_FIELD_LEN) len = CSV_MAX_FIELD_LEN - 1;
            memcpy(row.fields[row.num_fields], s, len);
            row.fields[row.num_fields][len] = '\0';
            row.num_fields++;

            s = end;
        }

        if (row.num_fields > 0) {
            if (cb(line_number, &row, ud))
                break;
        }
    }

    fclose(fp);
    return 0;
}
