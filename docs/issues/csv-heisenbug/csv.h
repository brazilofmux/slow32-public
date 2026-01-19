/*
 * csv.h - CSV parser for Regal
 *
 * Based on validatecsv strict parser state machine.
 */

#ifndef REGAL_CSV_H
#define REGAL_CSV_H

#include <stdio.h>
#include <stddef.h>

#define CSV_MAX_FIELDS    32
#define CSV_MAX_FIELD_LEN 512

/* CSV parser state */
typedef struct {
    char  *data;
    size_t len;
    size_t pos;

    int   line_number;

    /* Current field being built */
    char  field_buf[CSV_MAX_FIELD_LEN];
    int   field_len;

    /* All fields for current row */
    char  fields[CSV_MAX_FIELDS][CSV_MAX_FIELD_LEN];
    int   num_fields;

    int   state;
    int   last_action;

    int   expected_fields;
    int   have_expected;
} CsvParser;

/* Callback for each row. Return 0 to continue, non-zero to stop. */
typedef int (*CsvRowCallback)(int line_number, int num_fields,
                               char fields[CSV_MAX_FIELDS][CSV_MAX_FIELD_LEN],
                               void *user_data);

/* Initialize parser */
void csv_init(CsvParser *p);

/* Open a CSV file for parsing */
int csv_open(CsvParser *p, const char *filename);

/* Close the CSV file */
void csv_close(CsvParser *p);

/* Parse entire file, calling callback for each row */
int csv_parse(CsvParser *p, CsvRowCallback callback, void *user_data);

/* Parse a single CSV file with callback (convenience function) */
int csv_parse_file(const char *filename, CsvRowCallback callback, void *user_data);

#endif /* REGAL_CSV_H */
