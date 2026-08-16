#include "dbfuser.h"

#include <ctype.h>
#include <string.h>
#include <stdlib.h>

static uint16_t rd16(const unsigned char *p) {
    return (uint16_t)(p[0] | (p[1] << 8));
}

static uint32_t rd32(const unsigned char *p) {
    return (uint32_t)p[0] | ((uint32_t)p[1] << 8) |
           ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}

static int icmp(const char *a, const char *b) {
    while (*a && *b) {
        int ca = toupper((unsigned char)*a);
        int cb = toupper((unsigned char)*b);
        if (ca != cb) {
            return ca - cb;
        }
        a++;
        b++;
    }
    return (unsigned char)toupper((unsigned char)*a) -
           (unsigned char)toupper((unsigned char)*b);
}

static int is_name_field(const char *n) {
    return icmp(n, "NAME") == 0 || icmp(n, "USER") == 0 ||
           icmp(n, "USERNAME") == 0;
}

static int is_pass_field(const char *n) {
    return icmp(n, "PASS") == 0 || icmp(n, "PASSWORD") == 0 ||
           icmp(n, "PWD") == 0;
}

static void trim_copy(char *dst, int dstsz, const char *src, int n) {
    int i = 0;
    int j;
    while (i < n && src[i] == ' ') {
        i++;
    }
    j = n;
    while (j > i && src[j - 1] == ' ') {
        j--;
    }
    if (j - i >= dstsz) {
        j = i + dstsz - 1;
    }
    memcpy(dst, src + i, (size_t)(j - i));
    dst[j - i] = '\0';
}

int userdb_open(userdb_t *db, const char *path) {
    unsigned char hdr[32];
    unsigned char fdesc[32];
    int nf;
    int i;
    int off;
    int name_i = -1;
    int pass_i = -1;
    int field_off[32];
    int field_len[32];

    memset(db, 0, sizeof(*db));
    db->fp = fopen(path, "r");
    if (!db->fp) {
        return -1;
    }
    if (fread(hdr, 1, 32, db->fp) != 32) {
        userdb_close(db);
        return -1;
    }
    if (hdr[0] != 0x03 && hdr[0] != 0x83) {
        userdb_close(db);
        return -1;
    }
    db->nrec = rd32(hdr + 4);
    db->hdr_size = rd16(hdr + 8);
    db->rec_size = rd16(hdr + 10);
    if (db->rec_size < 2) {
        userdb_close(db);
        return -1;
    }
    nf = ((int)db->hdr_size - 33) / 32;
    if (nf < 1 || nf > 32) {
        userdb_close(db);
        return -1;
    }

    off = 1;
    for (i = 0; i < nf; i++) {
        char fname[12];
        if (fread(fdesc, 1, 32, db->fp) != 32) {
            userdb_close(db);
            return -1;
        }
        memcpy(fname, fdesc, 11);
        fname[11] = '\0';
        {
            int k;
            for (k = 10; k >= 0 && fname[k] == ' '; k--) {
                fname[k] = '\0';
            }
        }
        field_off[i] = off;
        field_len[i] = fdesc[16];
        off += field_len[i];
        if (is_name_field(fname) && name_i < 0) {
            name_i = i;
        }
        if (is_pass_field(fname) && pass_i < 0) {
            pass_i = i;
        }
    }
    /* Reject a header whose fields extend past the record: otherwise
     * userdb_check's trim_copy reads past the malloc(rec_size) block on the
     * pre-auth login path. off is the running end offset. */
    if (off > (int)db->rec_size) {
        userdb_close(db);
        return -1;
    }
    if (name_i < 0) {
        name_i = 0;
    }
    if (pass_i < 0) {
        pass_i = (nf > 1) ? 1 : 0;
    }
    db->name_off = field_off[name_i];
    db->name_len = field_len[name_i];
    db->pass_off = field_off[pass_i];
    db->pass_len = field_len[pass_i];
    return 0;
}

void userdb_close(userdb_t *db) {
    if (db->fp) {
        fclose(db->fp);
        db->fp = NULL;
    }
}

int userdb_check(userdb_t *db, const char *name, const char *pass) {
    uint32_t recno;
    char *rec;
    char have_name[64];
    char have_pass[64];

    if (!db->fp || !name || !pass) {
        return 0;
    }
    rec = (char *)malloc(db->rec_size);
    if (!rec) {
        return 0;
    }
    if (fseek(db->fp, (long)db->hdr_size, SEEK_SET) != 0) {
        free(rec);
        return 0;
    }
    for (recno = 0; recno < db->nrec; recno++) {
        if (fread(rec, 1, db->rec_size, db->fp) != db->rec_size) {
            break;
        }
        if (rec[0] == '*') {
            continue;
        }
        trim_copy(have_name, (int)sizeof(have_name),
                  rec + db->name_off, db->name_len);
        trim_copy(have_pass, (int)sizeof(have_pass),
                  rec + db->pass_off, db->pass_len);
        if (icmp(have_name, name) == 0 && strcmp(have_pass, pass) == 0) {
            free(rec);
            return 1;
        }
    }
    free(rec);
    return 0;
}
