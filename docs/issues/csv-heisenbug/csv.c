/*
 * csv.c - CSV parser for Regal
 *
 * Strict RFC 4180 parser based on validatecsv state machine.
 */

#include "csv.h"
#include <stdlib.h>
#include <string.h>

enum CsvState {
    CSV_STATE_MAIN = 0,
    CSV_STATE_QUOTED,
    CSV_STATE_AFTER_QUOTE,
    CSV_STATE_UNQUOTED,
    CSV_STATE_AFTER_FIELD,
    CSV_STATE_CR0,
    CSV_STATE_CR1,
    CSV_STATE_AFTER_FIELD_CR,
    CSV_STATE_ERROR
};

enum CsvAction {
    CSV_ACT_NONE = 0,
    CSV_ACT_SAVE_BYTE,
    CSV_ACT_SAVE_BYTE_QUOTE,
    CSV_ACT_SAVE_FIELD,
    CSV_ACT_SAVE_FIELD_CR,
    CSV_ACT_SAVE_RECORD,
    CSV_ACT_SAVE_FIELD_REC,
    CSV_ACT_SAVE_QUOTE,
    CSV_ACT_CR_PENDING
};

void csv_init(CsvParser *p)
{
    memset(p, 0, sizeof(CsvParser));
    p->line_number = 1;
    p->state = CSV_STATE_MAIN;
    p->last_action = CSV_ACT_NONE;
    p->expected_fields = 0;
    p->have_expected = 0;
}

static int csv_load_file(CsvParser *p, const char *filename)
{
    FILE *fp = fopen(filename, "rb");
    if (fp == NULL) {
        return -1;
    }

    if (fseek(fp, 0, SEEK_END) != 0) {
        fclose(fp);
        return -1;
    }

    long size = ftell(fp);
    if (size < 0) {
        fclose(fp);
        return -1;
    }

    if (fseek(fp, 0, SEEK_SET) != 0) {
        fclose(fp);
        return -1;
    }

    if (size == 0) {
        p->data = NULL;
        p->len = 0;
        fclose(fp);
        return 0;
    }

    p->data = (char *)malloc((size_t)size);
    if (p->data == NULL) {
        fclose(fp);
        return -1;
    }

    size_t read = fread(p->data, 1, (size_t)size, fp);
    fclose(fp);

    if (read != (size_t)size) {
        free(p->data);
        p->data = NULL;
        p->len = 0;
        return -1;
    }

    p->len = (size_t)size;
    return 0;
}

int csv_open(CsvParser *p, const char *filename)
{
    csv_init(p);
    return csv_load_file(p, filename);
}

void csv_close(CsvParser *p)
{
    if (p->data != NULL) {
        free(p->data);
        p->data = NULL;
    }
    p->len = 0;
}

static void save_field(CsvParser *p)
{
    if (p->num_fields < CSV_MAX_FIELDS) {
        p->field_buf[p->field_len] = '\0';
        size_t len = strlen(p->field_buf);
        if (len >= CSV_MAX_FIELD_LEN) {
            len = CSV_MAX_FIELD_LEN - 1;
        }
        memcpy(p->fields[p->num_fields], p->field_buf, len);
        p->fields[p->num_fields][len] = '\0';
        p->num_fields++;
    }
    p->field_len = 0;
}

static void add_char(CsvParser *p, char ch)
{
    if (p->field_len < CSV_MAX_FIELD_LEN - 1) {
        p->field_buf[p->field_len++] = ch;
    }
}

static int emit_row(CsvParser *p, CsvRowCallback callback, void *user_data)
{
    if (!p->have_expected) {
        p->expected_fields = p->num_fields;
        p->have_expected = 1;
    } else if (p->num_fields != p->expected_fields) {
        return -1;
    }

    if (callback) {
        int result = callback(p->line_number, p->num_fields, p->fields, user_data);
        if (result != 0) {
            return result;
        }
    }
    p->num_fields = 0;
    p->field_len = 0;
    p->line_number++;
    return 0;
}

int csv_parse(CsvParser *p, CsvRowCallback callback, void *user_data)
{
    if (p->len == 0 || p->data == NULL) {
        return 0;
    }

    p->state = CSV_STATE_MAIN;
    p->last_action = CSV_ACT_NONE;
    p->num_fields = 0;
    p->field_len = 0;

    for (size_t i = 0; i < p->len; i++) {
        unsigned char ch = (unsigned char)p->data[i];

        switch (p->state) {
        case CSV_STATE_MAIN:
            if (ch == '\n') {
                p->last_action = CSV_ACT_SAVE_RECORD;
                int result = emit_row(p, callback, user_data);
                if (result != 0) return result;
            } else if (ch == '\r') {
                p->state = CSV_STATE_CR0;
                p->last_action = CSV_ACT_CR_PENDING;
            } else if (ch == ',') {
                save_field(p);
                p->last_action = CSV_ACT_SAVE_FIELD;
                p->state = CSV_STATE_AFTER_FIELD;
            } else if (ch == '"') {
                p->state = CSV_STATE_QUOTED;
            } else {
                if (ch != '\0') {
                    add_char(p, (char)ch);
                }
                p->last_action = CSV_ACT_SAVE_BYTE;
                p->state = CSV_STATE_UNQUOTED;
            }
            break;

        case CSV_STATE_QUOTED:
            if (ch == '"') {
                p->state = CSV_STATE_AFTER_QUOTE;
            } else {
                if (ch != '\0') {
                    add_char(p, (char)ch);
                }
                p->last_action = CSV_ACT_SAVE_BYTE_QUOTE;
            }
            break;

        case CSV_STATE_AFTER_QUOTE:
            if (ch == '\n') {
                save_field(p);
                p->last_action = CSV_ACT_SAVE_FIELD_REC;
                int result = emit_row(p, callback, user_data);
                if (result != 0) return result;
                p->state = CSV_STATE_MAIN;
            } else if (ch == '\r') {
                p->state = CSV_STATE_CR1;
                p->last_action = CSV_ACT_CR_PENDING;
            } else if (ch == ',') {
                save_field(p);
                p->last_action = CSV_ACT_SAVE_FIELD;
                p->state = CSV_STATE_AFTER_FIELD;
            } else if (ch == '"') {
                add_char(p, '"');
                p->last_action = CSV_ACT_SAVE_QUOTE;
                p->state = CSV_STATE_QUOTED;
            } else {
                return -1;
            }
            break;

        case CSV_STATE_UNQUOTED:
            if (ch == '\n') {
                save_field(p);
                p->last_action = CSV_ACT_SAVE_FIELD_REC;
                int result = emit_row(p, callback, user_data);
                if (result != 0) return result;
                p->state = CSV_STATE_MAIN;
            } else if (ch == '\r') {
                p->state = CSV_STATE_CR1;
                p->last_action = CSV_ACT_CR_PENDING;
            } else if (ch == ',') {
                save_field(p);
                p->last_action = CSV_ACT_SAVE_FIELD;
                p->state = CSV_STATE_AFTER_FIELD;
            } else if (ch == '"') {
                return -1;
            } else {
                if (ch != '\0') {
                    add_char(p, (char)ch);
                }
                p->last_action = CSV_ACT_SAVE_BYTE;
            }
            break;

        case CSV_STATE_AFTER_FIELD:
            if (ch == '\n') {
                save_field(p);
                p->last_action = CSV_ACT_SAVE_FIELD_REC;
                int result = emit_row(p, callback, user_data);
                if (result != 0) return result;
                p->state = CSV_STATE_MAIN;
            } else if (ch == '\r') {
                save_field(p);
                p->last_action = CSV_ACT_SAVE_FIELD_CR;
                p->state = CSV_STATE_AFTER_FIELD_CR;
            } else if (ch == ',') {
                save_field(p);
                p->last_action = CSV_ACT_SAVE_FIELD;
                p->state = CSV_STATE_AFTER_FIELD;
            } else if (ch == '"') {
                p->state = CSV_STATE_QUOTED;
            } else {
                if (ch != '\0') {
                    add_char(p, (char)ch);
                }
                p->last_action = CSV_ACT_SAVE_BYTE;
                p->state = CSV_STATE_UNQUOTED;
            }
            break;

        case CSV_STATE_CR0:
            if (ch == '\n') {
                p->last_action = CSV_ACT_SAVE_RECORD;
                int result = emit_row(p, callback, user_data);
                if (result != 0) return result;
                p->state = CSV_STATE_MAIN;
            } else {
                return -1;
            }
            break;

        case CSV_STATE_CR1:
            if (ch == '\n') {
                save_field(p);
                p->last_action = CSV_ACT_SAVE_FIELD_REC;
                int result = emit_row(p, callback, user_data);
                if (result != 0) return result;
                p->state = CSV_STATE_MAIN;
            } else {
                return -1;
            }
            break;

        case CSV_STATE_AFTER_FIELD_CR:
            if (ch == '\n') {
                p->last_action = CSV_ACT_SAVE_RECORD;
                int result = emit_row(p, callback, user_data);
                if (result != 0) return result;
                p->state = CSV_STATE_MAIN;
            } else {
                return -1;
            }
            break;

        default:
            return -1;
        }
    }

    switch (p->state) {
    case CSV_STATE_UNQUOTED:
    case CSV_STATE_AFTER_FIELD:
    case CSV_STATE_AFTER_QUOTE:
        save_field(p);
        return emit_row(p, callback, user_data);
    case CSV_STATE_MAIN:
        return 0;
    case CSV_STATE_QUOTED:
    case CSV_STATE_CR0:
    case CSV_STATE_CR1:
    case CSV_STATE_AFTER_FIELD_CR:
    default:
        return -1;
    }
}

int csv_parse_file(const char *filename, CsvRowCallback callback, void *user_data)
{
    CsvParser parser;
    if (csv_open(&parser, filename) != 0) {
        return -1;
    }

    int result = csv_parse(&parser, callback, user_data);
    csv_close(&parser);

    return result;
}
