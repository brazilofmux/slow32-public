#include <stdio.h>
#include <stdint.h>

#define MAX_MEMBERS 64
#define MAX_STRTAB 32768

typedef struct {
    const char *path;
    uint32_t name_off;
    uint32_t size;
    uint32_t data_off;
} member_t;

typedef struct {
    uint32_t name_off;
    uint32_t member_idx;
} symbol_t;

static member_t g_members[MAX_MEMBERS];
static symbol_t g_symbols[1];
static uint8_t g_strtab[MAX_STRTAB];

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

static int has_flag(const char *cmd, char ch) {
    while (*cmd) {
        if (*cmd == ch) return 1;
        cmd++;
    }
    return 0;
}

static const char *basename_ptr(const char *path) {
    const char *p = path;
    const char *base = path;
    while (*p) {
        if (*p == '/' || *p == '\\') base = p + 1;
        p++;
    }
    return base;
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

static uint32_t append_strtab(const char *s, uint32_t *str_used) {
    uint32_t off = *str_used;
    uint32_t i = 0;
    while (s[i]) {
        if (off + i + 2 > MAX_STRTAB) return 0xFFFFFFFFu;
        g_strtab[off + i] = (uint8_t)s[i];
        i++;
    }
    g_strtab[off + i] = 0;
    *str_used = off + i + 1;
    return off;
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

    fputc('A', out); fputc('2', out); fputc('3', out); fputc('S', out);
    w16(out, 1); fputc(1, out); fputc(0, out);
    w32(out, nmembers);
    w32(out, mem_off);
    w32(out, nsymbols);
    w32(out, sym_off);
    w32(out, str_off);
    w32(out, str_used);

    for (i = 0; i < nsymbols; i++) {
        w32(out, g_symbols[i].name_off);
        w32(out, g_symbols[i].member_idx);
    }

    for (i = 0; i < nmembers; i++) {
        w32(out, g_members[i].name_off);
        w32(out, g_members[i].data_off);
        w32(out, g_members[i].size);
        w32(out, 0);
        w32(out, 0);
        w32(out, 0);
    }

    if (str_used > 0) {
        if (fwrite(g_strtab, 1, str_used, out) != str_used) {
            fclose(out);
            return 0;
        }
    }

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
    uint32_t str_used = 1;
    int i;

    if (argc < 4) return 1;
    cmd = argv[1];
    archive = argv[2];
    if (!has_flag(cmd, 'c') && !has_flag(cmd, 'r')) return 1;

    g_strtab[0] = 0;
    for (i = 3; i < argc; i++) {
        const char *path = argv[i];
        const char *name = basename_ptr(path);
        uint32_t off;

        if (nmembers >= MAX_MEMBERS) return 1;
        if (!count_file_bytes(path, &g_members[nmembers].size)) return 1;
        off = append_strtab(name, &str_used);
        if (off == 0xFFFFFFFFu) return 1;

        g_members[nmembers].path = path;
        g_members[nmembers].name_off = off;
        g_members[nmembers].data_off = 0;
        nmembers++;
    }

    /* Bounded opt-in path: only emit a single symbol index entry for one-member archives. */
    if (has_flag(cmd, 's') && nmembers == 1) {
        g_symbols[0].name_off = g_members[0].name_off;
        g_symbols[0].member_idx = 0;
        nsymbols = 1;
    }

    if (!write_archive(archive, nmembers, nsymbols, str_used)) return 1;
    return 0;
}
