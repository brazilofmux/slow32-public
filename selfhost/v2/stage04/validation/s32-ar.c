#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define MAX_MEMBERS 64
#define MAX_SYMBOLS 4096
#define MAX_STRTAB 262144
#define MAX_OBJ_STRTAB 65536
#define MAX_OBJ_FILE 262144

typedef struct {
    const char *path;
    const char *name;
    uint32_t name_off;
    uint32_t size;
    uint32_t data_off;
} member_t;

typedef struct {
    uint32_t name_off;
    uint32_t member_idx;
} symbol_t;

static member_t g_members[MAX_MEMBERS];
static symbol_t g_symbols[MAX_SYMBOLS];
static uint8_t g_strtab[MAX_STRTAB];
static uint8_t g_obj_strtab[MAX_OBJ_STRTAB];
static uint8_t g_obj_file[MAX_OBJ_FILE];

static uint16_t rd16(const uint8_t *p) {
    return (uint16_t)((uint16_t)p[0] | ((uint16_t)p[1] << 8));
}

static uint32_t rd32(const uint8_t *p) {
    return (uint32_t)p[0]
        | ((uint32_t)p[1] << 8)
        | ((uint32_t)p[2] << 16)
        | ((uint32_t)p[3] << 24);
}

static void w16(FILE *f, uint16_t v) {
    fputc((int)(v & 0xFF), f);
    fputc((int)((v >> 8) & 0xFF), f);
}

static void w32(FILE *f, uint32_t v) {
    fputc((int)(v & 0xFF), f);
    fputc((int)((v >> 8) & 0xFF), f);
    fputc((int)((v >> 16) & 0xFF), f);
    fputc((int)((v >> 24) & 0xFF), f);
}

static uint32_t cstr_len_limit(const uint8_t *s, uint32_t max_len) {
    uint32_t i = 0;
    while (i < max_len) {
        if (s[i] == 0) return i;
        i++;
    }
    return max_len;
}

static const char *basename_ptr(const char *path) {
    const char *p = path;
    const char *base = path;
    while (*p) {
        if (*p == '/' || *p == '\\') {
            base = p + 1;
        }
        p++;
    }
    return base;
}

static uint32_t append_strtab(const char *s, uint32_t *str_used) {
    uint32_t len = (uint32_t)strlen(s);
    uint32_t off = *str_used;
    uint32_t i;
    if (off + len + 1 > MAX_STRTAB) return 0xFFFFFFFFu;
    for (i = 0; i < len; i++) g_strtab[off + i] = (uint8_t)s[i];
    g_strtab[off + len] = 0;
    *str_used = off + len + 1;
    return off;
}

static int symbol_exists(const char *name, uint32_t nsymbols) {
    uint32_t i;
    for (i = 0; i < nsymbols; i++) {
        const char *have = (const char *)(g_strtab + g_symbols[i].name_off);
        if (strcmp(have, name) == 0) return 1;
    }
    return 0;
}

static int has_flag(const char *cmd, char ch) {
    while (*cmd) {
        if (*cmd == ch) return 1;
        cmd++;
    }
    return 0;
}

static int count_file_bytes(const char *path, uint32_t *out_size) {
    FILE *f = fopen(path, "rb");
    uint32_t total = 0;
    int ch;
    if (!f) return 0;
    for (;;) {
        ch = fgetc(f);
        if (ch == EOF) break;
        total++;
    }
    fclose(f);
    *out_size = total;
    return 1;
}

static int scan_obj_symbols(const member_t *m, uint32_t member_idx,
                            uint32_t *nsymbols, uint32_t *str_used) {
    FILE *f;
    uint8_t *buf = g_obj_file;
    uint32_t file_size;
    uint32_t nsym;
    uint32_t sym_off;
    uint32_t str_off;
    uint32_t str_sz;
    uint32_t i;

    if (!count_file_bytes(m->path, &file_size)) return 1;
    if (file_size < 36) return 2;

    if (file_size > MAX_OBJ_FILE) return 3;

    f = fopen(m->path, "rb");
    if (!f) {
        return 4;
    }
    if (fread(buf, 1, file_size, f) != file_size) {
        fclose(f);
        return 5;
    }
    fclose(f);

    if (buf[0] != 'O' || buf[1] != '2' || buf[2] != '3' || buf[3] != 'S') {
        return 6;
    }

    nsym = rd32(buf + 0x14);
    sym_off = rd32(buf + 0x18);
    str_off = rd32(buf + 0x1C);
    str_sz = rd32(buf + 0x20);

    if (str_sz > MAX_OBJ_STRTAB) {
        return 7;
    }
    if (sym_off > file_size || str_off > file_size) {
        return 8;
    }
    if (nsym > 0 && sym_off + nsym * 16u > file_size) {
        return 9;
    }
    if (str_sz > 0 && str_off + str_sz > file_size) {
        return 10;
    }

    if (str_sz > 0) memcpy(g_obj_strtab, buf + str_off, str_sz);

    for (i = 0; i < nsym; i++) {
        uint8_t *sym = buf + sym_off + i * 16u;
        uint32_t name_off;
        uint16_t sec_idx;
        uint8_t bind;
        const char *name;
        uint32_t maxn;
        uint32_t nlen;
        uint32_t off;

        name_off = rd32(sym + 0);
        sec_idx = rd16(sym + 8);
        bind = sym[11];
        if (sec_idx == 0 || bind == 0) continue;
        if (name_off >= str_sz) continue;

        maxn = str_sz - name_off;
        nlen = cstr_len_limit(g_obj_strtab + name_off, maxn);
        if (nlen == 0 || nlen >= maxn) continue;

        name = (const char *)(g_obj_strtab + name_off);
        if (symbol_exists(name, *nsymbols)) continue;
        if (*nsymbols >= MAX_SYMBOLS) {
            return 11;
        }
        off = append_strtab(name, str_used);
        if (off == 0xFFFFFFFFu) {
            return 12;
        }
        g_symbols[*nsymbols].name_off = off;
        g_symbols[*nsymbols].member_idx = member_idx;
        (*nsymbols)++;
    }

    return 0;
}

static int copy_member_data(FILE *out, const char *path) {
    FILE *in = fopen(path, "rb");
    int ch;
    if (!in) return 0;
    for (;;) {
        ch = fgetc(in);
        if (ch == EOF) break;
        if (fputc(ch, out) == EOF) {
            fclose(in);
            return 0;
        }
    }
    fclose(in);
    return 1;
}

static int write_archive(const char *out_path, uint32_t nmembers, uint32_t nsymbols, uint32_t str_used) {
    FILE *out;
    uint32_t sym_off = 32u;
    uint32_t mem_off = sym_off + nsymbols * 8u;
    uint32_t str_off = mem_off + nmembers * 24u;
    uint32_t data_off = str_off + str_used;
    uint32_t i;

    for (i = 0; i < nmembers; i++) {
        g_members[i].data_off = data_off;
        data_off += g_members[i].size;
    }

    out = fopen(out_path, "wb");
    if (!out) return 0;

    /* Header */
    fputc('A', out); fputc('2', out); fputc('3', out); fputc('S', out);
    w16(out, 1); fputc(1, out); fputc(0, out);
    w32(out, nmembers);
    w32(out, mem_off);
    w32(out, nsymbols);
    w32(out, sym_off);
    w32(out, str_off);
    w32(out, str_used);

    /* Symbol table */
    for (i = 0; i < nsymbols; i++) {
        w32(out, g_symbols[i].name_off);
        w32(out, g_symbols[i].member_idx);
    }

    /* Member table */
    for (i = 0; i < nmembers; i++) {
        w32(out, g_members[i].name_off);
        w32(out, g_members[i].data_off);
        w32(out, g_members[i].size);
        w32(out, 0);
        w32(out, 0);
        w32(out, 0);
    }

    /* String table */
    if (str_used > 0) {
        if (fwrite(g_strtab, 1, str_used, out) != str_used) {
            fclose(out);
            return 0;
        }
    }

    /* Data */
    for (i = 0; i < nmembers; i++) {
        if (!copy_member_data(out, g_members[i].path)) {
            fclose(out);
            return 0;
        }
    }

    if (fclose(out) != 0) return 0;
    return 1;
}

int main(int argc, char **argv) {
    const char *cmd;
    const char *archive;
    uint32_t nmembers = 0;
    uint32_t nsymbols = 0;
    uint32_t str_used = 1; /* leading NUL */
    int i;

    if (argc < 4) return 101;
    cmd = argv[1];
    archive = argv[2];

    if (!has_flag(cmd, 'c') && !has_flag(cmd, 'r')) return 102;

    g_strtab[0] = 0;
    for (i = 3; i < argc; i++) {
        const char *path = argv[i];
        const char *name = basename_ptr(path);
        uint32_t off;

        if (nmembers >= MAX_MEMBERS) return 103;
        if (!count_file_bytes(path, &g_members[nmembers].size)) return 110 + i;
        off = append_strtab(name, &str_used);
        if (off == 0xFFFFFFFFu) return 120 + i;

        g_members[nmembers].path = path;
        g_members[nmembers].name = name;
        g_members[nmembers].name_off = off;
        g_members[nmembers].data_off = 0;
        nmembers++;
    }

    nsymbols = 0;
    if (0) {
        for (i = 0; i < (int)nmembers; i++) {
            int sc = scan_obj_symbols(&g_members[i], (uint32_t)i, &nsymbols, &str_used);
            if (sc != 0) return 130 + sc;
        }
    }

    if (!write_archive(archive, nmembers, nsymbols, str_used)) return 140;
    return 0;
}
