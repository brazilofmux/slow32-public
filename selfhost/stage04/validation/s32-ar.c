#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define MAX_MEMBERS 64
#define MAX_STRTAB 32768
#define MAX_DATA 4194304
#define SRC_FILE 1
#define SRC_BUFFER 2

typedef struct {
    const char *path;
    uint32_t name_off;
    uint32_t size;
    uint32_t data_off;
    uint32_t src_kind;
    uint32_t src_off;
} member_t;

typedef struct {
    uint32_t name_off;
    uint32_t data_off;
    uint32_t size;
} arc_member_t;

static member_t g_members[MAX_MEMBERS];
static member_t g_members_tmp[MAX_MEMBERS];
static arc_member_t g_arc_members[MAX_MEMBERS];
static uint8_t g_strtab[MAX_STRTAB];
static uint8_t g_old_strtab[MAX_STRTAB];
static uint8_t g_arc_strtab[MAX_STRTAB];
static uint8_t g_data[MAX_DATA];
static uint32_t g_data_used;

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

static uint32_t r32(const uint8_t *p) {
    return ((uint32_t)p[0]) | (((uint32_t)p[1]) << 8) | (((uint32_t)p[2]) << 16) | (((uint32_t)p[3]) << 24);
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

static int find_member_by_name(const char *name, uint32_t nmembers) {
    uint32_t i;
    for (i = 0; i < nmembers; i++) {
        const char *cur = (const char *)(g_strtab + g_members[i].name_off);
        if (strcmp(cur, name) == 0) return (int)i;
    }
    return -1;
}

static int copy_member_file(FILE *out, const char *path) {
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

static int copy_member_data(FILE *out, const member_t *m) {
    if (m->src_kind == SRC_FILE) {
        return copy_member_file(out, m->path);
    }
    if (m->src_kind == SRC_BUFFER) {
        if (m->src_off + m->size > g_data_used) return 0;
        return fwrite(g_data + m->src_off, 1, m->size, out) == m->size;
    }
    return 0;
}

static int load_archive_view(const char *archive_path, uint32_t *out_nmembers, uint32_t *out_file_size, uint32_t *out_str_size) {
    FILE *in = fopen(archive_path, "rb");
    long end_pos;
    uint32_t file_size;
    uint8_t hdr[32];
    uint32_t in_nmembers;
    uint32_t in_mem_off;
    uint32_t in_str_off;
    uint32_t in_str_sz;
    uint32_t i;

    if (!in) return 0;
    if (fseek(in, 0, SEEK_END) != 0) {
        fclose(in);
        return 0;
    }
    end_pos = ftell(in);
    if (end_pos < 32) {
        fclose(in);
        return 0;
    }
    file_size = (uint32_t)end_pos;
    if (fseek(in, 0, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    if (fread(hdr, 1, 32, in) != 32) {
        fclose(in);
        return 0;
    }

    if (hdr[0] != 'A' || hdr[1] != '2' || hdr[2] != '3' || hdr[3] != 'S') {
        fclose(in);
        return 0;
    }

    in_nmembers = r32(hdr + 8);
    in_mem_off = r32(hdr + 12);
    in_str_off = r32(hdr + 24);
    in_str_sz = r32(hdr + 28);

    if (in_nmembers > MAX_MEMBERS) {
        fclose(in);
        return 0;
    }
    if (in_str_sz > MAX_STRTAB) {
        fclose(in);
        return 0;
    }
    if (in_mem_off + in_nmembers * 24u > file_size) {
        fclose(in);
        return 0;
    }
    if (in_str_off + in_str_sz > file_size) {
        fclose(in);
        return 0;
    }

    if (fseek(in, (long)in_str_off, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    if (in_str_sz > 0 && fread(g_arc_strtab, 1, in_str_sz, in) != in_str_sz) {
        fclose(in);
        return 0;
    }

    for (i = 0; i < in_nmembers; i++) {
        uint8_t ent[24];
        uint32_t ent_off = in_mem_off + i * 24u;
        uint32_t name_off;
        uint32_t data_off;
        uint32_t size;

        if (fseek(in, (long)ent_off, SEEK_SET) != 0) {
            fclose(in);
            return 0;
        }
        if (fread(ent, 1, 24, in) != 24) {
            fclose(in);
            return 0;
        }

        name_off = r32(ent + 0);
        data_off = r32(ent + 4);
        size = r32(ent + 8);

        if (name_off >= in_str_sz) {
            fclose(in);
            return 0;
        }
        if (data_off + size > file_size) {
            fclose(in);
            return 0;
        }

        g_arc_members[i].name_off = name_off;
        g_arc_members[i].data_off = data_off;
        g_arc_members[i].size = size;
    }

    fclose(in);
    *out_nmembers = in_nmembers;
    *out_file_size = file_size;
    *out_str_size = in_str_sz;
    return 1;
}

static int list_archive(const char *archive_path) {
    uint32_t nmembers;
    uint32_t file_size;
    uint32_t str_size;
    uint32_t i;

    if (!load_archive_view(archive_path, &nmembers, &file_size, &str_size)) return 0;
    (void)file_size;
    (void)str_size;
    for (i = 0; i < nmembers; i++) {
        const char *name = (const char *)(g_arc_strtab + g_arc_members[i].name_off);
        printf("%u %s\n", g_arc_members[i].size, name);
    }
    return 1;
}

static int extract_one(const char *archive_path, uint32_t midx) {
    const char *name = (const char *)(g_arc_strtab + g_arc_members[midx].name_off);
    const char *out_name = basename_ptr(name);
    FILE *in = fopen(archive_path, "rb");
    FILE *out;
    uint32_t i;
    int ch;

    if (!in) return 0;
    if (fseek(in, (long)g_arc_members[midx].data_off, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    out = fopen(out_name, "wb");
    if (!out) {
        fclose(in);
        return 0;
    }
    for (i = 0; i < g_arc_members[midx].size; i++) {
        ch = fgetc(in);
        if (ch == EOF) {
            fclose(out);
            fclose(in);
            return 0;
        }
        if (fputc(ch, out) == EOF) {
            fclose(out);
            fclose(in);
            return 0;
        }
    }
    fclose(out);
    fclose(in);
    return 1;
}

static int name_matches(const char *member_name, const char *target_name) {
    if (strcmp(member_name, target_name) == 0) return 1;
    return strcmp(basename_ptr(member_name), target_name) == 0;
}

static int extract_archive(const char *archive_path, int reqc, char **reqv) {
    uint32_t nmembers;
    uint32_t file_size;
    uint32_t str_size;
    uint8_t req_found[MAX_MEMBERS];
    uint32_t i;

    if ((uint32_t)reqc > MAX_MEMBERS) return 0;
    if (!load_archive_view(archive_path, &nmembers, &file_size, &str_size)) return 0;
    (void)file_size;
    (void)str_size;
    for (i = 0; i < (uint32_t)reqc; i++) req_found[i] = 0;

    for (i = 0; i < nmembers; i++) {
        const char *name = (const char *)(g_arc_strtab + g_arc_members[i].name_off);
        if (reqc == 0) {
            if (!extract_one(archive_path, i)) return 0;
            continue;
        }
        {
            uint32_t j;
            for (j = 0; j < (uint32_t)reqc; j++) {
                if (name_matches(name, reqv[j])) {
                    if (!extract_one(archive_path, i)) return 0;
                    req_found[j] = 1;
                    break;
                }
            }
        }
    }

    for (i = 0; i < (uint32_t)reqc; i++) {
        if (!req_found[i]) return 0;
    }
    return 1;
}

static int print_one(const char *archive_path, uint32_t midx) {
    FILE *in = fopen(archive_path, "rb");
    uint32_t i;
    int ch;

    if (!in) return 0;
    if (fseek(in, (long)g_arc_members[midx].data_off, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    for (i = 0; i < g_arc_members[midx].size; i++) {
        ch = fgetc(in);
        if (ch == EOF) {
            fclose(in);
            return 0;
        }
        putchar(ch);
    }
    fclose(in);
    return 1;
}

static int print_archive(const char *archive_path, int reqc, char **reqv) {
    uint32_t nmembers;
    uint32_t file_size;
    uint32_t str_size;
    uint32_t i;

    if (!load_archive_view(archive_path, &nmembers, &file_size, &str_size)) return 0;
    (void)file_size;
    (void)str_size;

    if (reqc == 0) {
        for (i = 0; i < nmembers; i++) {
            if (!print_one(archive_path, i)) return 0;
        }
        return 1;
    }

    for (i = 0; i < (uint32_t)reqc; i++) {
        uint32_t j;
        int found = 0;
        for (j = 0; j < nmembers; j++) {
            const char *name = (const char *)(g_arc_strtab + g_arc_members[j].name_off);
            if (name_matches(name, reqv[i])) {
                if (!print_one(archive_path, j)) return 0;
                found = 1;
                break;
            }
        }
        if (!found) return 0;
    }
    return 1;
}

static int write_archive(const char *out_path, uint32_t nmembers, uint32_t str_used) {
    FILE *out;
    uint32_t nsymbols = 0;
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
        if (!copy_member_data(out, &g_members[i])) {
            fclose(out);
            return 0;
        }
    }

    if (fclose(out) != 0) return 0;
    return 1;
}

static int load_existing_archive(const char *archive_path, uint32_t *nmembers, uint32_t *str_used) {
    FILE *in = fopen(archive_path, "rb");
    long end_pos;
    uint32_t file_size;
    uint8_t hdr[32];
    uint32_t in_nmembers;
    uint32_t in_mem_off;
    uint32_t in_str_off;
    uint32_t in_str_sz;
    uint32_t i;

    if (!in) return 0;
    if (fseek(in, 0, SEEK_END) != 0) {
        fclose(in);
        return 0;
    }
    end_pos = ftell(in);
    if (end_pos < 32) {
        fclose(in);
        return 0;
    }
    file_size = (uint32_t)end_pos;
    if (fseek(in, 0, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    if (fread(hdr, 1, 32, in) != 32) {
        fclose(in);
        return 0;
    }

    if (hdr[0] != 'A' || hdr[1] != '2' || hdr[2] != '3' || hdr[3] != 'S') {
        fclose(in);
        return 0;
    }

    in_nmembers = r32(hdr + 8);
    in_mem_off = r32(hdr + 12);
    in_str_off = r32(hdr + 24);
    in_str_sz = r32(hdr + 28);

    if (in_nmembers > MAX_MEMBERS) {
        fclose(in);
        return 0;
    }
    if (in_str_sz > MAX_STRTAB) {
        fclose(in);
        return 0;
    }
    if (in_mem_off + in_nmembers * 24u > file_size) {
        fclose(in);
        return 0;
    }
    if (in_str_off + in_str_sz > file_size) {
        fclose(in);
        return 0;
    }

    if (fseek(in, (long)in_str_off, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    if (in_str_sz > 0 && fread(g_old_strtab, 1, in_str_sz, in) != in_str_sz) {
        fclose(in);
        return 0;
    }

    for (i = 0; i < in_nmembers; i++) {
        uint8_t ent[24];
        uint32_t ent_off = in_mem_off + i * 24u;
        uint32_t name_off;
        uint32_t data_off;
        uint32_t size;
        uint32_t off;

        if (fseek(in, (long)ent_off, SEEK_SET) != 0) {
            fclose(in);
            return 0;
        }
        if (fread(ent, 1, 24, in) != 24) {
            fclose(in);
            return 0;
        }

        name_off = r32(ent + 0);
        data_off = r32(ent + 4);
        size = r32(ent + 8);

        if (name_off >= in_str_sz) {
            fclose(in);
            return 0;
        }
        if (data_off + size > file_size) {
            fclose(in);
            return 0;
        }
        if (g_data_used + size > MAX_DATA) {
            fclose(in);
            return 0;
        }
        if (fseek(in, (long)data_off, SEEK_SET) != 0) {
            fclose(in);
            return 0;
        }
        if (size > 0 && fread(g_data + g_data_used, 1, size, in) != size) {
            fclose(in);
            return 0;
        }

        off = append_strtab((const char *)(g_old_strtab + name_off), str_used);
        if (off == 0xFFFFFFFFu) {
            fclose(in);
            return 0;
        }

        g_members[*nmembers].path = archive_path;
        g_members[*nmembers].name_off = off;
        g_members[*nmembers].size = size;
        g_members[*nmembers].data_off = 0;
        g_members[*nmembers].src_kind = SRC_BUFFER;
        g_members[*nmembers].src_off = g_data_used;
        g_data_used += size;
        *nmembers = *nmembers + 1;
    }

    fclose(in);
    return 1;
}

static int delete_members(const char *archive_path, int reqc, char **reqv) {
    uint32_t nmembers = 0;
    uint32_t str_used = 1;
    uint32_t out_n = 0;
    uint8_t req_found[MAX_MEMBERS];
    uint32_t i;

    if (reqc <= 0) return 0;
    if ((uint32_t)reqc > MAX_MEMBERS) return 0;

    g_strtab[0] = 0;
    g_data_used = 0;
    if (!load_existing_archive(archive_path, &nmembers, &str_used)) return 0;

    for (i = 0; i < (uint32_t)reqc; i++) req_found[i] = 0;

    for (i = 0; i < nmembers; i++) {
        const char *name = (const char *)(g_strtab + g_members[i].name_off);
        uint32_t j;
        int remove = 0;
        for (j = 0; j < (uint32_t)reqc; j++) {
            if (name_matches(name, reqv[j])) {
                req_found[j] = 1;
                remove = 1;
            }
        }
        if (!remove) {
            if (out_n != i) {
                g_members[out_n].path = g_members[i].path;
                g_members[out_n].name_off = g_members[i].name_off;
                g_members[out_n].size = g_members[i].size;
                g_members[out_n].data_off = g_members[i].data_off;
                g_members[out_n].src_kind = g_members[i].src_kind;
                g_members[out_n].src_off = g_members[i].src_off;
            }
            out_n++;
        }
    }

    for (i = 0; i < (uint32_t)reqc; i++) {
        if (!req_found[i]) return 0;
    }

    return write_archive(archive_path, out_n, str_used);
}

static int move_members(const char *archive_path, int reqc, char **reqv) {
    uint32_t nmembers = 0;
    uint32_t str_used = 1;
    uint8_t req_found[MAX_MEMBERS];
    uint32_t out_n = 0;
    uint32_t i;

    if (reqc <= 0) return 0;
    if ((uint32_t)reqc > MAX_MEMBERS) return 0;

    g_strtab[0] = 0;
    g_data_used = 0;
    if (!load_existing_archive(archive_path, &nmembers, &str_used)) return 0;
    for (i = 0; i < (uint32_t)reqc; i++) req_found[i] = 0;

    for (i = 0; i < nmembers; i++) {
        const char *name = (const char *)(g_strtab + g_members[i].name_off);
        uint32_t j;
        int move_it = 0;
        for (j = 0; j < (uint32_t)reqc; j++) {
            if (name_matches(name, reqv[j])) {
                req_found[j] = 1;
                move_it = 1;
            }
        }
        if (!move_it) {
            g_members_tmp[out_n].path = g_members[i].path;
            g_members_tmp[out_n].name_off = g_members[i].name_off;
            g_members_tmp[out_n].size = g_members[i].size;
            g_members_tmp[out_n].data_off = g_members[i].data_off;
            g_members_tmp[out_n].src_kind = g_members[i].src_kind;
            g_members_tmp[out_n].src_off = g_members[i].src_off;
            out_n++;
        }
    }
    for (i = 0; i < nmembers; i++) {
        const char *name = (const char *)(g_strtab + g_members[i].name_off);
        uint32_t j;
        int move_it = 0;
        for (j = 0; j < (uint32_t)reqc; j++) {
            if (name_matches(name, reqv[j])) move_it = 1;
        }
        if (move_it) {
            g_members_tmp[out_n].path = g_members[i].path;
            g_members_tmp[out_n].name_off = g_members[i].name_off;
            g_members_tmp[out_n].size = g_members[i].size;
            g_members_tmp[out_n].data_off = g_members[i].data_off;
            g_members_tmp[out_n].src_kind = g_members[i].src_kind;
            g_members_tmp[out_n].src_off = g_members[i].src_off;
            out_n++;
        }
    }
    for (i = 0; i < (uint32_t)reqc; i++) {
        if (!req_found[i]) return 0;
    }
    for (i = 0; i < out_n; i++) {
        g_members[i].path = g_members_tmp[i].path;
        g_members[i].name_off = g_members_tmp[i].name_off;
        g_members[i].size = g_members_tmp[i].size;
        g_members[i].data_off = g_members_tmp[i].data_off;
        g_members[i].src_kind = g_members_tmp[i].src_kind;
        g_members[i].src_off = g_members_tmp[i].src_off;
    }
    return write_archive(archive_path, out_n, str_used);
}

int main(int argc, char **argv) {
    const char *cmd;
    const char *archive;
    uint32_t nmembers = 0;
    uint32_t str_used = 1;
    int i;

    if (argc < 3) return 1;
    cmd = argv[1];
    archive = argv[2];

    if (has_flag(cmd, 't')) {
        if (argc != 3) return 1;
        if (!list_archive(archive)) return 1;
        return 0;
    }
    if (has_flag(cmd, 'x')) {
        if (!extract_archive(archive, argc - 3, argv + 3)) return 1;
        return 0;
    }
    if (has_flag(cmd, 'p')) {
        if (!print_archive(archive, argc - 3, argv + 3)) return 1;
        return 0;
    }
    if (has_flag(cmd, 'd')) {
        if (argc < 4) return 1;
        if (!delete_members(archive, argc - 3, argv + 3)) return 1;
        return 0;
    }
    if (has_flag(cmd, 'm')) {
        if (argc < 4) return 1;
        if (!move_members(archive, argc - 3, argv + 3)) return 1;
        return 0;
    }

    if (argc < 4) return 1;
    if (!has_flag(cmd, 'c') && !has_flag(cmd, 'r')) return 1;

    g_strtab[0] = 0;
    g_data_used = 0;

    if (has_flag(cmd, 'r')) {
        int loaded = load_existing_archive(archive, &nmembers, &str_used);
        if (!loaded && !has_flag(cmd, 'c')) return 1;
    }

    for (i = 3; i < argc; i++) {
        const char *path = argv[i];
        const char *name = basename_ptr(path);
        uint32_t size;
        int idx;

        if (!count_file_bytes(path, &size)) return 1;

        idx = find_member_by_name(name, nmembers);
        if (idx >= 0) {
            g_members[idx].path = path;
            g_members[idx].size = size;
            g_members[idx].src_kind = SRC_FILE;
            g_members[idx].src_off = 0;
        } else {
            uint32_t off;
            if (nmembers >= MAX_MEMBERS) return 1;
            off = append_strtab(name, &str_used);
            if (off == 0xFFFFFFFFu) return 1;

            g_members[nmembers].path = path;
            g_members[nmembers].name_off = off;
            g_members[nmembers].size = size;
            g_members[nmembers].data_off = 0;
            g_members[nmembers].src_kind = SRC_FILE;
            g_members[nmembers].src_off = 0;
            nmembers++;
        }
    }

    if (!write_archive(archive, nmembers, str_used)) return 1;
    return 0;
}
