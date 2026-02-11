#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "dbf.h"
#include "area.h"
#include "util.h"
#include "util.h"

#define DBF_CACHE_RECORDS 32

void dbf_init(dbf_t *db) {
    memset(db, 0, sizeof(*db));
}

void dbf_cache_invalidate(dbf_t *db) {
    db->cache_start = 0;
    db->cache_count = 0;
}

static void dbf_cache_free(dbf_t *db) {
    if (db->cache_buf) {
        free(db->cache_buf);
        db->cache_buf = NULL;
    }
    db->cache_capacity = 0;
    dbf_cache_invalidate(db);
}

static void dbf_cache_init(dbf_t *db) {
    size_t bytes;

    dbf_cache_free(db);

    if (db->record_size == 0)
        return;

    db->cache_capacity = DBF_CACHE_RECORDS;
    bytes = (size_t)db->record_size * (size_t)db->cache_capacity;
    db->cache_buf = (char *)malloc(bytes);
    if (!db->cache_buf) {
        db->cache_capacity = 0;
        return;
    }
    dbf_cache_invalidate(db);
}

static void write_le16(unsigned char *p, uint16_t v) {
    p[0] = v & 0xFF;
    p[1] = (v >> 8) & 0xFF;
}

static void write_le32(unsigned char *p, uint32_t v) {
    p[0] = v & 0xFF;
    p[1] = (v >> 8) & 0xFF;
    p[2] = (v >> 16) & 0xFF;
    p[3] = (v >> 24) & 0xFF;
}

static uint16_t read_le16(const unsigned char *p) {
    return p[0] | ((uint16_t)p[1] << 8);
}

static uint32_t read_le32(const unsigned char *p) {
    return p[0] | ((uint32_t)p[1] << 8) |
           ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}

int dbf_create(const char *filename, const dbf_field_t *fields, int nfields) {
    FILE *fp;
    unsigned char hdr[32];
    unsigned char fdesc[32];
    unsigned char term;
    int i;
    uint16_t hdr_size, rec_size;
    char norm_path[64];

    str_copy(norm_path, filename, sizeof(norm_path));
    path_normalize(norm_path);

    if (nfields < 1 || nfields > DBF_MAX_FIELDS) return -1;

    fp = fopen(norm_path, "w+b");
    if (!fp) return -1;

    hdr_size = 32 + nfields * 32 + 1;
    rec_size = 1; /* deletion flag */
    for (i = 0; i < nfields; i++)
        rec_size += fields[i].length;

    /* Write header */
    memset(hdr, 0, 32);
    hdr[0] = 0x03; /* version: dBase III */
    hdr[1] = 26;   /* year (2026 - 1900) */
    hdr[2] = 2;    /* month */
    hdr[3] = 9;    /* day */
    write_le32(hdr + 4, 0);          /* record count = 0 */
    write_le16(hdr + 8, hdr_size);
    write_le16(hdr + 10, rec_size);
    fwrite(hdr, 1, 32, fp);

    /* Write field descriptors */
    for (i = 0; i < nfields; i++) {
        memset(fdesc, 0, 32);
        str_copy((char *)fdesc, fields[i].name, 11);
        fdesc[11] = fields[i].type;
        fdesc[16] = fields[i].length;
        fdesc[17] = fields[i].decimals;
        fwrite(fdesc, 1, 32, fp);
    }

    /* Header terminator */
    term = 0x0D;
    fwrite(&term, 1, 1, fp);

    fclose(fp);
    return 0;
}

int dbf_open(dbf_t *db, const char *filename) {
    unsigned char hdr[32];
    unsigned char fdesc[32];
    int i, nfields;
    uint16_t offset;
    char norm_path[64];

    str_copy(norm_path, filename, sizeof(norm_path));
    path_normalize(norm_path);

    if (db->fp) dbf_close(db);

    db->fp = fopen(norm_path, "r+b");
    if (!db->fp) return -1;

    str_copy(db->filename, norm_path, sizeof(db->filename));

    /* Read header */
    if (fread(hdr, 1, 32, db->fp) != 32) {
        fclose(db->fp);
        db->fp = NULL;
        return -1;
    }

    db->record_count = read_le32(hdr + 4);
    db->header_size = read_le16(hdr + 8);
    db->record_size = read_le16(hdr + 10);

    /* Number of fields = (header_size - 32 - 1) / 32 */
    nfields = (db->header_size - 33) / 32;
    if (nfields < 0 || nfields > DBF_MAX_FIELDS) {
        fclose(db->fp);
        db->fp = NULL;
        return -1;
    }
    db->field_count = nfields;

    /* Read field descriptors */
    offset = 0;
    for (i = 0; i < nfields; i++) {
        if (fread(fdesc, 1, 32, db->fp) != 32) {
            fclose(db->fp);
            db->fp = NULL;
            return -1;
        }
        str_copy(db->fields[i].name, (char *)fdesc, DBF_MAX_FIELD_NAME);
        db->fields[i].type = fdesc[11];
        db->fields[i].length = fdesc[16];
        db->fields[i].decimals = fdesc[17];
        db->fields[i].offset = offset;
        offset += fdesc[16];
    }

    db->current_record = 0;
    db->record_dirty = 0;
    dbf_cache_init(db);
    return 0;
}

void dbf_close(dbf_t *db) {
    if (!db->fp) return;
    if (db->record_dirty)
        dbf_flush_record(db);
    dbf_write_header_counts(db);
    fclose(db->fp);
    db->fp = NULL;
    db->filename[0] = '\0';
    db->current_record = 0;
    db->record_dirty = 0;
    dbf_cache_free(db);
}

int dbf_is_open(const dbf_t *db) {
    return db->fp != NULL;
}

int dbf_append_blank(dbf_t *db) {
    long pos;

    if (!db->fp) return -1;

    /* Flush current record if dirty */
    if (db->record_dirty)
        dbf_flush_record(db);

    db->record_count++;
    db->current_record = db->record_count;

    /* Fill record buffer with spaces, deletion flag = ' ' */
    memset(db->record_buf, ' ', db->record_size);
    db->record_buf[db->record_size] = '\0';

    /* Seek to record position and write */
    pos = db->header_size + (long)(db->current_record - 1) * db->record_size;
    fseek(db->fp, pos, 0); /* SEEK_SET = 0 */
    fwrite(db->record_buf, 1, db->record_size, db->fp);
    fflush(db->fp);

    /* Update header */
    dbf_write_header_counts(db);

    db->record_dirty = 0;
    area_invalidate_all(db->filename);
    return 0;
}

int dbf_read_record(dbf_t *db, uint32_t recno) {
    long pos;
    uint32_t start;
    uint32_t available;
    int to_read;
    size_t bytes;

    if (!db->fp) return -1;
    if (recno < 1 || recno > db->record_count) return -1;

    /* Flush current if dirty */
    if (db->record_dirty)
        dbf_flush_record(db);

    if (db->cache_buf && db->cache_count > 0 &&
        recno >= db->cache_start &&
        recno < db->cache_start + (uint32_t)db->cache_count) {
        int idx = (int)(recno - db->cache_start);
        memcpy(db->record_buf, db->cache_buf + idx * db->record_size, db->record_size);
        db->record_buf[db->record_size] = '\0';
    } else if (db->cache_buf && db->cache_capacity > 0) {
        start = recno;
        available = db->record_count - start + 1;
        to_read = db->cache_capacity;
        if (to_read > (int)available) to_read = (int)available;
        bytes = (size_t)db->record_size * (size_t)to_read;

        pos = db->header_size + (long)(start - 1) * db->record_size;
        fseek(db->fp, pos, 0);
        if (fread(db->cache_buf, 1, bytes, db->fp) != bytes)
            return -1;
        db->cache_start = start;
        db->cache_count = to_read;

        memcpy(db->record_buf, db->cache_buf, db->record_size);
        db->record_buf[db->record_size] = '\0';
    } else {
        pos = db->header_size + (long)(recno - 1) * db->record_size;
        fseek(db->fp, pos, 0);
        if (fread(db->record_buf, 1, db->record_size, db->fp) != db->record_size)
            return -1;
        db->record_buf[db->record_size] = '\0';
    }

    db->current_record = recno;
    db->record_dirty = 0;
    return 0;
}

int dbf_flush_record(dbf_t *db) {
    long pos;

    if (!db->fp || !db->record_dirty || db->current_record == 0)
        return 0;

    pos = db->header_size + (long)(db->current_record - 1) * db->record_size;
    fseek(db->fp, pos, 0);
    fwrite(db->record_buf, 1, db->record_size, db->fp);
    fflush(db->fp);
    db->record_dirty = 0;
    area_invalidate_all(db->filename);
    return 0;
}

int dbf_find_field(const dbf_t *db, const char *name) {
    int i;
    for (i = 0; i < db->field_count; i++) {
        if (str_icmp(db->fields[i].name, name) == 0)
            return i;
    }
    return -1;
}

int dbf_get_field_raw(const dbf_t *db, int idx, char *buf, int bufsize) {
    int len;

    if (idx < 0 || idx >= db->field_count) return -1;
    if (db->current_record == 0) return -1;

    len = db->fields[idx].length;
    if (len >= bufsize) len = bufsize - 1;

    /* +1 to skip deletion flag byte */
    memcpy(buf, db->record_buf + 1 + db->fields[idx].offset, len);
    buf[len] = '\0';
    return 0;
}

int dbf_set_field_raw(dbf_t *db, int idx, const char *value) {
    int len, vlen;

    if (idx < 0 || idx >= db->field_count) return -1;
    if (db->current_record == 0) return -1;

    len = db->fields[idx].length;
    vlen = strlen(value);
    if (vlen > len) vlen = len;

    /* Copy value, pad with spaces */
    memcpy(db->record_buf + 1 + db->fields[idx].offset, value, vlen);
    if (vlen < len)
        memset(db->record_buf + 1 + db->fields[idx].offset + vlen, ' ', len - vlen);

    db->record_dirty = 1;
    dbf_cache_invalidate(db);
    return 0;
}

int dbf_write_header_counts(dbf_t *db) {
    unsigned char buf[8];

    if (!db->fp) return -1;

    fseek(db->fp, 4, 0);
    write_le32(buf, db->record_count);
    fwrite(buf, 1, 4, db->fp);
    fflush(db->fp);
    return 0;
}
