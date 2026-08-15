#include "dbfmsg.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static uint16_t rd16(const unsigned char *p) {
    return (uint16_t)(p[0] | (p[1] << 8));
}

static uint32_t rd32(const unsigned char *p) {
    return (uint32_t)p[0] | ((uint32_t)p[1] << 8) |
           ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}

static void wr32(unsigned char *p, uint32_t v) {
    p[0] = (unsigned char)(v & 0xff);
    p[1] = (unsigned char)((v >> 8) & 0xff);
    p[2] = (unsigned char)((v >> 16) & 0xff);
    p[3] = (unsigned char)((v >> 24) & 0xff);
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

static void put_field(char *rec, int off, int len, const char *s) {
    int n = s ? (int)strlen(s) : 0;
    if (n > len) {
        n = len;
    }
    memset(rec + off, ' ', (size_t)len);
    memcpy(rec + off, s, (size_t)n);
}

static int create_empty(const char *path) {
    FILE *f;
    unsigned char hdr[32];
    unsigned char fd[32];
    unsigned char term;
    /* FROM 16, TO 16, SUBJ 40, TEXT 254 + delete flag */
    uint16_t rec_size = 1 + 16 + 16 + 40 + 254;
    uint16_t hdr_size = 32 + 4 * 32 + 1;
    int i;
    const char *names[4] = { "FROM", "TO", "SUBJ", "TEXT" };
    const int lens[4] = { 16, 16, 40, 254 };

    f = fopen(path, "w");
    if (!f) {
        return -1;
    }
    memset(hdr, 0, sizeof(hdr));
    hdr[0] = 0x03;
    hdr[1] = 126;
    hdr[2] = 8;
    hdr[3] = 15;
    wr32(hdr + 4, 0);
    hdr[8] = (unsigned char)(hdr_size & 0xff);
    hdr[9] = (unsigned char)(hdr_size >> 8);
    hdr[10] = (unsigned char)(rec_size & 0xff);
    hdr[11] = (unsigned char)(rec_size >> 8);
    if (fwrite(hdr, 1, 32, f) != 32) {
        fclose(f);
        return -1;
    }
    for (i = 0; i < 4; i++) {
        memset(fd, 0, sizeof(fd));
        memcpy(fd, names[i], strlen(names[i]));
        fd[11] = 'C';
        fd[16] = (unsigned char)lens[i];
        if (fwrite(fd, 1, 32, f) != 32) {
            fclose(f);
            return -1;
        }
    }
    term = 0x0D;
    fwrite(&term, 1, 1, f);
    term = 0x1A;
    fwrite(&term, 1, 1, f);
    fclose(f);
    return 0;
}

int msgdb_open(msgdb_t *db, const char *path) {
    unsigned char hdr[32];
    unsigned char fdesc[32];
    int nf, i, off;
    int from_i = -1, to_i = -1, subj_i = -1, text_i = -1;
    int field_off[16];
    int field_len[16];

    memset(db, 0, sizeof(*db));
    db->fp = fopen(path, "r+");
    if (!db->fp) {
        if (create_empty(path) != 0) {
            return -1;
        }
        db->fp = fopen(path, "r+");
        if (!db->fp) {
            return -1;
        }
    }
    if (fread(hdr, 1, 32, db->fp) != 32) {
        msgdb_close(db);
        return -1;
    }
    if (hdr[0] != 0x03 && hdr[0] != 0x83) {
        msgdb_close(db);
        return -1;
    }
    db->nrec = rd32(hdr + 4);
    db->hdr_size = rd16(hdr + 8);
    db->rec_size = rd16(hdr + 10);
    nf = ((int)db->hdr_size - 33) / 32;
    if (nf < 1 || nf > 16 || db->rec_size < 2) {
        msgdb_close(db);
        return -1;
    }
    off = 1;
    for (i = 0; i < nf; i++) {
        char fname[12];
        int k;
        if (fread(fdesc, 1, 32, db->fp) != 32) {
            msgdb_close(db);
            return -1;
        }
        memcpy(fname, fdesc, 11);
        fname[11] = '\0';
        for (k = 10; k >= 0 && fname[k] == ' '; k--) {
            fname[k] = '\0';
        }
        field_off[i] = off;
        field_len[i] = fdesc[16];
        off += field_len[i];
        if (icmp(fname, "FROM") == 0) from_i = i;
        if (icmp(fname, "TO") == 0) to_i = i;
        if (icmp(fname, "SUBJ") == 0 || icmp(fname, "SUBJECT") == 0) subj_i = i;
        if (icmp(fname, "TEXT") == 0 || icmp(fname, "BODY") == 0 ||
            icmp(fname, "MSG") == 0) text_i = i;
    }
    if (from_i < 0) from_i = 0;
    if (to_i < 0) to_i = (nf > 1) ? 1 : 0;
    if (subj_i < 0) subj_i = (nf > 2) ? 2 : 0;
    if (text_i < 0) text_i = (nf > 3) ? 3 : 0;
    db->from_off = field_off[from_i];
    db->from_len = field_len[from_i];
    db->to_off = field_off[to_i];
    db->to_len = field_len[to_i];
    db->subj_off = field_off[subj_i];
    db->subj_len = field_len[subj_i];
    db->text_off = field_off[text_i];
    db->text_len = field_len[text_i];
    return 0;
}

void msgdb_close(msgdb_t *db) {
    if (db->fp) {
        fclose(db->fp);
        db->fp = NULL;
    }
}

uint32_t msgdb_count(const msgdb_t *db) {
    return db->nrec;
}

int msgdb_get(msgdb_t *db, uint32_t recno, msg_t *out) {
    char *rec;
    if (!db->fp || recno < 1 || recno > db->nrec) {
        return -1;
    }
    rec = (char *)malloc(db->rec_size);
    if (!rec) {
        return -1;
    }
    if (fseek(db->fp, (long)db->hdr_size + (long)(recno - 1) * db->rec_size,
              SEEK_SET) != 0 ||
        fread(rec, 1, db->rec_size, db->fp) != db->rec_size) {
        free(rec);
        return -1;
    }
    if (rec[0] == '*') {
        free(rec);
        return -1;
    }
    memset(out, 0, sizeof(*out));
    trim_copy(out->from, (int)sizeof(out->from), rec + db->from_off, db->from_len);
    trim_copy(out->to, (int)sizeof(out->to), rec + db->to_off, db->to_len);
    trim_copy(out->subj, (int)sizeof(out->subj), rec + db->subj_off, db->subj_len);
    trim_copy(out->text, (int)sizeof(out->text), rec + db->text_off, db->text_len);
    free(rec);
    return 0;
}

int msgdb_post(msgdb_t *db, const char *from, const char *to,
               const char *subj, const char *text) {
    char *rec;
    unsigned char nbuf[4];
    unsigned char eofb;
    if (!db->fp) {
        return -1;
    }
    rec = (char *)malloc(db->rec_size);
    if (!rec) {
        return -1;
    }
    memset(rec, ' ', db->rec_size);
    rec[0] = ' ';
    put_field(rec, db->from_off, db->from_len, from);
    put_field(rec, db->to_off, db->to_len, to);
    put_field(rec, db->subj_off, db->subj_len, subj);
    put_field(rec, db->text_off, db->text_len, text);

    if (fseek(db->fp, (long)db->hdr_size + (long)db->nrec * db->rec_size,
              SEEK_SET) != 0 ||
        fwrite(rec, 1, db->rec_size, db->fp) != db->rec_size) {
        free(rec);
        return -1;
    }
    eofb = 0x1A;
    fwrite(&eofb, 1, 1, db->fp);
    db->nrec++;
    wr32(nbuf, db->nrec);
    if (fseek(db->fp, 4, SEEK_SET) != 0 || fwrite(nbuf, 1, 4, db->fp) != 4) {
        free(rec);
        return -1;
    }
    fflush(db->fp);
    free(rec);
    return 0;
}
