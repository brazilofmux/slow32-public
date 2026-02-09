#ifndef DBF_H
#define DBF_H

#include <stdio.h>
#include <stdint.h>

#define DBF_MAX_FIELDS      128
#define DBF_MAX_FIELD_NAME   11
#define DBF_MAX_RECORD_SIZE 4000

typedef struct {
    char name[DBF_MAX_FIELD_NAME];  /* NUL-terminated, uppercased */
    char type;                       /* 'C', 'N', 'D', 'L' */
    uint8_t length;
    uint8_t decimals;
    uint16_t offset;                 /* byte offset within record (after deletion flag) */
} dbf_field_t;

typedef struct {
    FILE *fp;
    char filename[64];
    uint32_t record_count;
    uint16_t header_size;            /* 32 + nfields*32 + 1 */
    uint16_t record_size;            /* 1 + sum of field lengths */
    int field_count;
    dbf_field_t fields[DBF_MAX_FIELDS];
    uint32_t current_record;         /* 1-based; 0 = no current record */
    char record_buf[DBF_MAX_RECORD_SIZE];
    int record_dirty;
} dbf_t;

void dbf_init(dbf_t *db);
int  dbf_create(const char *filename, const dbf_field_t *fields, int nfields);
int  dbf_open(dbf_t *db, const char *filename);
void dbf_close(dbf_t *db);
int  dbf_is_open(const dbf_t *db);
int  dbf_append_blank(dbf_t *db);
int  dbf_read_record(dbf_t *db, uint32_t recno);
int  dbf_flush_record(dbf_t *db);
int  dbf_find_field(const dbf_t *db, const char *name);
int  dbf_get_field_raw(const dbf_t *db, int idx, char *buf, int bufsize);
int  dbf_set_field_raw(dbf_t *db, int idx, const char *value);
int  dbf_write_header_counts(dbf_t *db);

#endif
