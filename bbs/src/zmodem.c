#include "zmodem.h"

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/stat.h>

#define ZPAD   '*'
#define ZDLE   0x18
#define ZBIN   'A'
#define ZHEX   'B'

#define ZRQINIT 0
#define ZRINIT  1
#define ZACK    3
#define ZFILE   4
#define ZNAK    6
#define ZFIN    8
#define ZRPOS   9
#define ZDATA   10
#define ZEOF    11

#define ZCRCE   'h'
#define ZCRCG   'i'
#define ZCRCW   'k'

#define HDR_MAX 32
#define DATA_MAX 1024

static uint16_t crc16_add(uint16_t crc, unsigned char b) {
    int i;
    crc ^= (uint16_t)b << 8;
    for (i = 0; i < 8; i++) {
        if (crc & 0x8000u) {
            crc = (uint16_t)((crc << 1) ^ 0x1021u);
        } else {
            crc = (uint16_t)(crc << 1);
        }
    }
    return crc;
}

static uint16_t crc16_buf(const unsigned char *p, int n) {
    uint16_t crc = 0;
    int i;
    for (i = 0; i < n; i++) {
        crc = crc16_add(crc, p[i]);
    }
    return crc;
}

static int raw_put(int fd, unsigned char c) {
    return send(fd, &c, 1, 0) == 1 ? 0 : -1;
}

static int raw_get(int fd) {
    unsigned char c;
    int n = recv(fd, &c, 1, 0);
    if (n <= 0) {
        return -1;
    }
    return (int)c;
}

static int needs_escape(unsigned char c) {
    return c == ZDLE || c == 0x10 || c == 0x11 || c == 0x13 ||
           c == 0x90 || c == 0x91 || c == 0x93;
}

static int zdle_put(int fd, unsigned char c) {
    if (needs_escape(c)) {
        if (raw_put(fd, ZDLE) < 0) {
            return -1;
        }
        return raw_put(fd, (unsigned char)(c ^ 0x40));
    }
    return raw_put(fd, c);
}

static int send_hex_hdr(int fd, int type, uint32_t pos) {
    unsigned char raw[5];
    uint16_t crc;
    char hex[32];
    int i, n;
    raw[0] = (unsigned char)type;
    raw[1] = (unsigned char)(pos & 0xff);
    raw[2] = (unsigned char)((pos >> 8) & 0xff);
    raw[3] = (unsigned char)((pos >> 16) & 0xff);
    raw[4] = (unsigned char)((pos >> 24) & 0xff);
    crc = crc16_buf(raw, 5);
    if (raw_put(fd, ZPAD) < 0 || raw_put(fd, ZPAD) < 0 ||
        raw_put(fd, ZDLE) < 0 || raw_put(fd, ZHEX) < 0) {
        return -1;
    }
    n = 0;
    for (i = 0; i < 5; i++) {
        hex[n++] = "0123456789abcdef"[(raw[i] >> 4) & 0xf];
        hex[n++] = "0123456789abcdef"[raw[i] & 0xf];
    }
    hex[n++] = "0123456789abcdef"[(crc >> 12) & 0xf];
    hex[n++] = "0123456789abcdef"[(crc >> 8) & 0xf];
    hex[n++] = "0123456789abcdef"[(crc >> 4) & 0xf];
    hex[n++] = "0123456789abcdef"[crc & 0xf];
    hex[n++] = '\r';
    hex[n++] = '\n';
    return send(fd, hex, n, 0) == n ? 0 : -1;
}

static int send_bin_hdr(int fd, int type, uint32_t pos) {
    unsigned char raw[5];
    uint16_t crc;
    int i;
    raw[0] = (unsigned char)type;
    raw[1] = (unsigned char)(pos & 0xff);
    raw[2] = (unsigned char)((pos >> 8) & 0xff);
    raw[3] = (unsigned char)((pos >> 16) & 0xff);
    raw[4] = (unsigned char)((pos >> 24) & 0xff);
    crc = crc16_buf(raw, 5);
    if (raw_put(fd, ZPAD) < 0 || raw_put(fd, ZDLE) < 0 ||
        raw_put(fd, ZBIN) < 0) {
        return -1;
    }
    for (i = 0; i < 5; i++) {
        if (zdle_put(fd, raw[i]) < 0) {
            return -1;
        }
    }
    if (zdle_put(fd, (unsigned char)(crc >> 8)) < 0) {
        return -1;
    }
    return zdle_put(fd, (unsigned char)(crc & 0xff));
}

static int send_data(int fd, const unsigned char *p, int n, int frameend) {
    uint16_t crc = 0;
    int i;
    for (i = 0; i < n; i++) {
        crc = crc16_add(crc, p[i]);
        if (zdle_put(fd, p[i]) < 0) {
            return -1;
        }
    }
    crc = crc16_add(crc, (unsigned char)frameend);
    if (raw_put(fd, ZDLE) < 0 || raw_put(fd, (unsigned char)frameend) < 0) {
        return -1;
    }
    if (zdle_put(fd, (unsigned char)(crc >> 8)) < 0) {
        return -1;
    }
    return zdle_put(fd, (unsigned char)(crc & 0xff));
}

static int hexval(int c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'A' + 5) return c - 'A' + 10;
    return -1;
}

static int read_hex_byte(int fd) {
    int hi = raw_get(fd);
    int lo = raw_get(fd);
    int h, l;
    if (hi < 0 || lo < 0) {
        return -1;
    }
    h = hexval(hi);
    l = hexval(lo);
    if (h < 0 || l < 0) {
        return -1;
    }
    return (h << 4) | l;
}

static int read_zdle_byte(int fd) {
    int c = raw_get(fd);
    if (c < 0) {
        return -1;
    }
    if (c != ZDLE) {
        return c;
    }
    c = raw_get(fd);
    if (c < 0) {
        return -1;
    }
    return c ^ 0x40;
}

static int recv_header(int fd, int *type, uint32_t *pos) {
    int c;
    int kind;
    unsigned char raw[5];
    uint16_t got, want;
    int i;
    int crchi, crclo;

    /* Scan for ZPAD ... ZDLE ZBIN/ZHEX */
    for (;;) {
        c = raw_get(fd);
        if (c < 0) {
            return -1;
        }
        if (c != ZPAD) {
            continue;
        }
        do {
            c = raw_get(fd);
            if (c < 0) {
                return -1;
            }
        } while (c == ZPAD);
        if (c != ZDLE) {
            continue;
        }
        kind = raw_get(fd);
        if (kind < 0) {
            return -1;
        }
        if (kind == ZHEX || kind == ZBIN) {
            break;
        }
    }

    if (kind == ZHEX) {
        for (i = 0; i < 5; i++) {
            c = read_hex_byte(fd);
            if (c < 0) {
                return -1;
            }
            raw[i] = (unsigned char)c;
        }
        crchi = read_hex_byte(fd);
        crclo = read_hex_byte(fd);
        if (crchi < 0 || crclo < 0) {
            return -1;
        }
        /* swallow CR LF and optional XON */
        c = raw_get(fd);
        if (c == '\r') {
            raw_get(fd);
        }
    } else {
        for (i = 0; i < 5; i++) {
            c = read_zdle_byte(fd);
            if (c < 0) {
                return -1;
            }
            raw[i] = (unsigned char)c;
        }
        crchi = read_zdle_byte(fd);
        crclo = read_zdle_byte(fd);
        if (crchi < 0 || crclo < 0) {
            return -1;
        }
    }
    want = crc16_buf(raw, 5);
    got = (uint16_t)(((unsigned)crchi << 8) | (unsigned)crclo);
    if (got != want) {
        return -1;
    }
    *type = raw[0];
    *pos = (uint32_t)raw[1] | ((uint32_t)raw[2] << 8) |
           ((uint32_t)raw[3] << 16) | ((uint32_t)raw[4] << 24);
    return 0;
}

int zmodem_send(int fd, const char *path) {
    FILE *f;
    struct stat st;
    const char *base;
    unsigned char chunk[DATA_MAX];
    char namepkt[256];
    int typ;
    uint32_t pos;
    size_t n;
    uint32_t sent = 0;
    int namelen;

    if (stat(path, &st) != 0 || !S_ISREG(st.st_mode)) {
        return -1;
    }
    f = fopen(path, "r");
    if (!f) {
        return -1;
    }
    base = path;
    {
        const char *s;
        for (s = path; *s; s++) {
            if (*s == '/' || *s == '\\') {
                base = s + 1;
            }
        }
    }
    namelen = (int)strlen(base);
    if (namelen <= 0 || namelen > 80) {
        fclose(f);
        return -1;
    }
    memcpy(namepkt, base, (size_t)namelen);
    namepkt[namelen] = '\0';
    namelen += 1;

    if (send_hex_hdr(fd, ZRQINIT, 0) < 0) {
        fclose(f);
        return -1;
    }
    if (recv_header(fd, &typ, &pos) < 0 || typ != ZRINIT) {
        fclose(f);
        return -1;
    }
    if (send_bin_hdr(fd, ZFILE, 0) < 0 ||
        send_data(fd, (unsigned char *)namepkt, namelen, ZCRCW) < 0) {
        fclose(f);
        return -1;
    }
    if (recv_header(fd, &typ, &pos) < 0 || typ != ZRPOS) {
        fclose(f);
        return -1;
    }
    if (pos != 0) {
        if (fseek(f, (long)pos, SEEK_SET) != 0) {
            fclose(f);
            return -1;
        }
        sent = pos;
    }
    if (send_bin_hdr(fd, ZDATA, sent) < 0) {
        fclose(f);
        return -1;
    }
    for (;;) {
        n = fread(chunk, 1, sizeof(chunk), f);
        if (n == 0) {
            break;
        }
        if (send_data(fd, chunk, (int)n, ZCRCG) < 0) {
            fclose(f);
            return -1;
        }
        sent += (uint32_t)n;
    }
    /* empty ZCRCE finishes the data stream */
    if (send_data(fd, chunk, 0, ZCRCE) < 0) {
        fclose(f);
        return -1;
    }
    if (send_bin_hdr(fd, ZEOF, sent) < 0) {
        fclose(f);
        return -1;
    }
    if (recv_header(fd, &typ, &pos) < 0) {
        fclose(f);
        return -1;
    }
    /* Receiver may send ZRINIT (more files?) or ZFIN */
    if (typ == ZRINIT || typ == ZFIN) {
        if (send_hex_hdr(fd, ZFIN, 0) < 0) {
            fclose(f);
            return -1;
        }
        if (typ != ZFIN) {
            if (recv_header(fd, &typ, &pos) < 0 || typ != ZFIN) {
                fclose(f);
                return -1;
            }
        }
        raw_put(fd, 'O');
        raw_put(fd, 'O');
    }
    fclose(f);
    return 0;
}
