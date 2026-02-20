/* s32-ar-port.c -- cc-min compatible port of stage02/s32-ar.c
 *
 * Mechanical port of the full-featured archiver to cc-min subset C,
 * plus symbol index building so archives work with the Forth linker.
 *
 * Porting changes from s32-ar.c:
 *   - uint8_t/uint32_t/int32_t/size_t/long -> int or char
 *   - FILE * -> int (selfhost libc convention)
 *   - const removed from variables
 *   - static removed from functions
 *   - Block-scoped declarations hoisted to function scope
 *   - Postfix ++/-- replaced with x = x + 1
 *   - for(;;) replaced with while(1)
 *   - (void)x casts removed
 *   - 0xFFFFFFFFu replaced with -1
 *   - u suffixes removed from integer literals
 *   - putchar(ch) replaced with fputc(ch, stdout)
 *   - stdout/stderr are globals resolved by linker against libc
 */

#include "s32ar_min.h"

#define MAX_MEMBERS 128
#define MAX_STRTAB 65536
#define MAX_DATA 4194304
#define SRC_FILE 1
#define SRC_BUFFER 2

/* --- Struct definitions --- */

struct member {
    char *path;
    int name_off;
    int size;
    int data_off;
    int src_kind;
    int src_off;
};
typedef struct member member_t;

struct arc_member {
    int name_off;
    int data_off;
    int size;
};
typedef struct arc_member arc_member_t;

/* --- Global arrays --- */

member_t g_members[MAX_MEMBERS];
member_t g_members_tmp[MAX_MEMBERS];
arc_member_t g_arc_members[MAX_MEMBERS];
char g_strtab[MAX_STRTAB];
char g_old_strtab[MAX_STRTAB];
char g_arc_strtab[MAX_STRTAB];
char g_data[MAX_DATA];
int g_data_used;

/* Symbol index arrays */
#define MAX_SIDX 8192
int g_sidx_name[MAX_SIDX];
int g_sidx_member[MAX_SIDX];
int g_nsidx;

/* Temp buffers (hoisted from local arrays) */
char g_hdr[32];
char g_ent[24];
char g_req_found[MAX_MEMBERS];

/* --- Helper functions --- */

void w16(int f, int v) {
    fputc(v & 255, f);
    fputc((v >> 8) & 255, f);
}

void w32(int f, int v) {
    fputc(v & 255, f);
    fputc((v >> 8) & 255, f);
    fputc((v >> 16) & 255, f);
    fputc((v >> 24) & 255, f);
}

int r32(char *p) {
    int b0;
    int b1;
    int b2;
    int b3;
    b0 = p[0] & 255;
    b1 = p[1] & 255;
    b2 = p[2] & 255;
    b3 = p[3] & 255;
    return b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
}

int r16(char *p) {
    int b0;
    int b1;
    b0 = p[0] & 255;
    b1 = p[1] & 255;
    return b0 | (b1 << 8);
}

int r8(char *p) {
    return p[0] & 255;
}

int has_flag(char *cmd, int ch) {
    while (*cmd) {
        if (*cmd == ch) return 1;
        cmd = cmd + 1;
    }
    return 0;
}

int in_bounds(int off, int size, int total) {
    if (off < 0 || size < 0 || total < 0) return 0;
    if (off > total) return 0;
    if (size > total - off) return 0;
    return 1;
}

char *basename_ptr(char *path) {
    char *p;
    char *base;
    p = path;
    base = path;
    while (*p) {
        if (*p == '/' || *p == 92) base = p + 1;
        p = p + 1;
    }
    return base;
}

int count_file_bytes(char *path, int *out_size) {
    int f;
    int total;
    int ch;
    f = fopen(path, "rb");
    if (!f) return 0;
    total = 0;
    while (1) {
        ch = fgetc(f);
        if (ch == EOF) break;
        total = total + 1;
    }
    fclose(f);
    *out_size = total;
    return 1;
}

int append_strtab(char *s, int *str_used) {
    int off;
    int i;
    off = *str_used;
    i = 0;
    while (s[i]) {
        if (off + i + 2 > MAX_STRTAB) return -1;
        g_strtab[off + i] = s[i];
        i = i + 1;
    }
    g_strtab[off + i] = 0;
    *str_used = off + i + 1;
    return off;
}

int find_member_by_name(char *name, int nmembers) {
    int i;
    char *cur;
    i = 0;
    while (i < nmembers) {
        cur = g_strtab + g_members[i].name_off;
        if (strcmp(cur, name) == 0) return i;
        i = i + 1;
    }
    return -1;
}

int copy_member_file(int out, char *path) {
    int in;
    int ch;
    in = fopen(path, "rb");
    if (!in) return 0;
    while (1) {
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

int copy_member_data(int out, int midx) {
    if (g_members[midx].src_kind == SRC_FILE) {
        return copy_member_file(out, g_members[midx].path);
    }
    if (g_members[midx].src_kind == SRC_BUFFER) {
        if (g_members[midx].src_off + g_members[midx].size > g_data_used) return 0;
        return fwrite(g_data + g_members[midx].src_off, 1, g_members[midx].size, out) == g_members[midx].size;
    }
    return 0;
}

/* --- Archive reading --- */

int load_archive_view(char *archive_path, int *out_nmembers, int *out_file_size, int *out_str_size) {
    int in;
    int end_pos;
    int file_size;
    int in_nmembers;
    int in_mem_off;
    int in_str_off;
    int in_str_sz;
    int i;
    int ent_off;
    int name_off;
    int data_off;
    int size;

    in = fopen(archive_path, "rb");
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
    file_size = end_pos;
    if (fseek(in, 0, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    if (fread(g_hdr, 1, 32, in) != 32) {
        fclose(in);
        return 0;
    }

    if (g_hdr[0] != 'A' || g_hdr[1] != '2' || g_hdr[2] != '3' || g_hdr[3] != 'S') {
        fclose(in);
        return 0;
    }

    in_nmembers = r32(g_hdr + 8);
    in_mem_off = r32(g_hdr + 12);
    in_str_off = r32(g_hdr + 24);
    in_str_sz = r32(g_hdr + 28);

    if (in_nmembers > MAX_MEMBERS) {
        fclose(in);
        return 0;
    }
    if (in_str_sz > MAX_STRTAB) {
        fclose(in);
        return 0;
    }
    if (in_nmembers < 0 || in_nmembers > (file_size / 24)) {
        fclose(in);
        return 0;
    }
    if (!in_bounds(in_mem_off, in_nmembers * 24, file_size)) {
        fclose(in);
        return 0;
    }
    if (!in_bounds(in_str_off, in_str_sz, file_size)) {
        fclose(in);
        return 0;
    }

    if (fseek(in, in_str_off, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    if (in_str_sz > 0) {
        if (fread(g_arc_strtab, 1, in_str_sz, in) != in_str_sz) {
            fclose(in);
            return 0;
        }
    }

    i = 0;
    while (i < in_nmembers) {
        ent_off = in_mem_off + i * 24;
        if (fseek(in, ent_off, SEEK_SET) != 0) {
            fclose(in);
            return 0;
        }
        if (fread(g_ent, 1, 24, in) != 24) {
            fclose(in);
            return 0;
        }

        name_off = r32(g_ent);
        data_off = r32(g_ent + 4);
        size = r32(g_ent + 8);

        if (name_off >= in_str_sz) {
            fclose(in);
            return 0;
        }
        if (!in_bounds(data_off, size, file_size)) {
            fclose(in);
            return 0;
        }

        g_arc_members[i].name_off = name_off;
        g_arc_members[i].data_off = data_off;
        g_arc_members[i].size = size;

        i = i + 1;
    }

    fclose(in);
    *out_nmembers = in_nmembers;
    *out_file_size = file_size;
    *out_str_size = in_str_sz;
    return 1;
}

int list_archive(char *archive_path) {
    int nmembers;
    int file_size;
    int str_size;
    int i;
    char *name;

    if (!load_archive_view(archive_path, &nmembers, &file_size, &str_size)) return 0;
    i = 0;
    while (i < nmembers) {
        name = g_arc_strtab + g_arc_members[i].name_off;
        fput_uint(stdout, g_arc_members[i].size);
        fputc(' ', stdout);
        fputs(name, stdout);
        fputc('\n', stdout);
        i = i + 1;
    }
    return 1;
}

/* --- Extract --- */

int extract_one(char *archive_path, int midx) {
    char *name;
    char *out_name;
    int in;
    int out;
    int i;
    int ch;

    name = g_arc_strtab + g_arc_members[midx].name_off;
    out_name = basename_ptr(name);
    in = fopen(archive_path, "rb");
    if (!in) return 0;
    if (fseek(in, g_arc_members[midx].data_off, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    out = fopen(out_name, "wb");
    if (!out) {
        fclose(in);
        return 0;
    }
    i = 0;
    while (i < g_arc_members[midx].size) {
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
        i = i + 1;
    }
    fclose(out);
    fclose(in);
    return 1;
}

int name_matches(char *member_name, char *target_name) {
    if (strcmp(member_name, target_name) == 0) return 1;
    return strcmp(basename_ptr(member_name), target_name) == 0;
}

int extract_archive(char *archive_path, int reqc, char **reqv) {
    int nmembers;
    int file_size;
    int str_size;
    int i;
    int j;
    char *name;

    if (reqc > MAX_MEMBERS) return 0;
    if (!load_archive_view(archive_path, &nmembers, &file_size, &str_size)) return 0;
    i = 0;
    while (i < reqc) {
        g_req_found[i] = 0;
        i = i + 1;
    }

    i = 0;
    while (i < nmembers) {
        name = g_arc_strtab + g_arc_members[i].name_off;
        if (reqc == 0) {
            if (!extract_one(archive_path, i)) return 0;
            i = i + 1;
            continue;
        }
        j = 0;
        while (j < reqc) {
            if (name_matches(name, reqv[j])) {
                if (!extract_one(archive_path, i)) return 0;
                g_req_found[j] = 1;
                break;
            }
            j = j + 1;
        }
        i = i + 1;
    }

    i = 0;
    while (i < reqc) {
        if (!g_req_found[i]) return 0;
        i = i + 1;
    }
    return 1;
}

/* --- Print --- */

int print_one(char *archive_path, int midx) {
    int in;
    int i;
    int ch;

    in = fopen(archive_path, "rb");
    if (!in) return 0;
    if (fseek(in, g_arc_members[midx].data_off, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    i = 0;
    while (i < g_arc_members[midx].size) {
        ch = fgetc(in);
        if (ch == EOF) {
            fclose(in);
            return 0;
        }
        fputc(ch, stdout);
        i = i + 1;
    }
    fclose(in);
    return 1;
}

int print_archive(char *archive_path, int reqc, char **reqv) {
    int nmembers;
    int file_size;
    int str_size;
    int i;
    int j;
    int found;
    char *name;

    if (!load_archive_view(archive_path, &nmembers, &file_size, &str_size)) return 0;

    if (reqc == 0) {
        i = 0;
        while (i < nmembers) {
            if (!print_one(archive_path, i)) return 0;
            i = i + 1;
        }
        return 1;
    }

    i = 0;
    while (i < reqc) {
        j = 0;
        found = 0;
        while (j < nmembers) {
            name = g_arc_strtab + g_arc_members[j].name_off;
            if (name_matches(name, reqv[i])) {
                if (!print_one(archive_path, j)) return 0;
                found = 1;
                break;
            }
            j = j + 1;
        }
        if (!found) return 0;
        i = i + 1;
    }
    return 1;
}

/* --- Symbol index building (NEW) --- */

int load_file_to_data(int midx) {
    int f;
    int ch;
    int off;
    int sz;

    if (g_members[midx].src_kind != SRC_FILE) return 1;

    f = fopen(g_members[midx].path, "rb");
    if (!f) return 0;

    off = g_data_used;
    sz = 0;
    while (1) {
        ch = fgetc(f);
        if (ch == EOF) break;
        if (off + sz >= MAX_DATA) {
            fclose(f);
            return 0;
        }
        g_data[off + sz] = ch;
        sz = sz + 1;
    }
    fclose(f);

    g_members[midx].src_kind = SRC_BUFFER;
    g_members[midx].src_off = off;
    g_data_used = off + sz;
    return 1;
}

int build_symbol_index(int nmembers, int *str_used) {
    int i;
    int base;
    int magic;
    int nsyms;
    int sym_off;
    int str_off;
    int str_sz;
    int j;
    int entry_off;
    int name_off;
    int sec_idx;
    int bind;
    char *sym_name;
    int soff;

    g_nsidx = 0;

    i = 0;
    while (i < nmembers) {
        if (g_members[i].src_kind != SRC_BUFFER) {
            i = i + 1;
            continue;
        }

        base = g_members[i].src_off;
        if (g_members[i].size < 40) {
            i = i + 1;
            continue;
        }

        magic = r32(g_data + base);
        if (magic != S32O_MAGIC) {
            i = i + 1;
            continue;
        }

        nsyms = r32(g_data + base + 20);
        sym_off = r32(g_data + base + 24);
        str_off = r32(g_data + base + 28);
        str_sz = r32(g_data + base + 32);

        if (nsyms < 0 || nsyms > (g_members[i].size / 16)) {
            i = i + 1;
            continue;
        }
        if (!in_bounds(sym_off, nsyms * 16, g_members[i].size)) {
            i = i + 1;
            continue;
        }
        if (!in_bounds(str_off, str_sz, g_members[i].size)) {
            i = i + 1;
            continue;
        }

        j = 0;
        while (j < nsyms) {
            entry_off = base + sym_off + j * 16;
            name_off = r32(g_data + entry_off);
            sec_idx = r16(g_data + entry_off + 8);
            bind = r8(g_data + entry_off + 11);

            if (bind == S32O_BIND_GLOBAL && sec_idx != 0) {
                if (name_off < str_sz) {
                    sym_name = g_data + base + str_off + name_off;
                    if (g_nsidx < MAX_SIDX) {
                        soff = append_strtab(sym_name, str_used);
                        if (soff != -1) {
                            g_sidx_name[g_nsidx] = soff;
                            g_sidx_member[g_nsidx] = i;
                            g_nsidx = g_nsidx + 1;
                        }
                    }
                }
            }

            j = j + 1;
        }

        i = i + 1;
    }

    return 1;
}

/* --- Archive writing --- */

int write_archive(char *out_path, int nmembers, int str_used) {
    int out;
    int nsymbols;
    int sym_off;
    int mem_off;
    int str_off;
    int data_off;
    int i;

    nsymbols = g_nsidx;
    sym_off = 32;
    mem_off = sym_off + nsymbols * 8;
    str_off = mem_off + nmembers * 24;
    data_off = str_off + str_used;

    i = 0;
    while (i < nmembers) {
        g_members[i].data_off = data_off;
        data_off = data_off + g_members[i].size;
        i = i + 1;
    }

    out = fopen(out_path, "wb");
    if (!out) return 0;

    /* Header: "A23S" magic */
    fputc('A', out);
    fputc('2', out);
    fputc('3', out);
    fputc('S', out);
    w16(out, 1);       /* version */
    fputc(1, out);     /* endian */
    fputc(0, out);     /* reserved */
    w32(out, nmembers);
    w32(out, mem_off);
    w32(out, nsymbols);
    w32(out, sym_off);
    w32(out, str_off);
    w32(out, str_used);

    /* Symbol index entries: 8 bytes each (name_off, member_idx) */
    i = 0;
    while (i < nsymbols) {
        w32(out, g_sidx_name[i]);
        w32(out, g_sidx_member[i]);
        i = i + 1;
    }

    /* Member table entries: 24 bytes each */
    i = 0;
    while (i < nmembers) {
        w32(out, g_members[i].name_off);
        w32(out, g_members[i].data_off);
        w32(out, g_members[i].size);
        w32(out, 0);
        w32(out, 0);
        w32(out, 0);
        i = i + 1;
    }

    /* String table */
    if (str_used > 0) {
        if (fwrite(g_strtab, 1, str_used, out) != str_used) {
            fclose(out);
            return 0;
        }
    }

    /* Member data */
    i = 0;
    while (i < nmembers) {
        if (!copy_member_data(out, i)) {
            fclose(out);
            return 0;
        }
        i = i + 1;
    }

    if (fclose(out) != 0) return 0;
    return 1;
}

/* --- Load existing archive (for replace/delete/move) --- */

int load_existing_archive(char *archive_path, int *nmembers, int *str_used) {
    int in;
    int end_pos;
    int file_size;
    int in_nmembers;
    int in_mem_off;
    int in_str_off;
    int in_str_sz;
    int i;
    int ent_off;
    int name_off;
    int data_off;
    int size;
    int off;
    int n;

    in = fopen(archive_path, "rb");
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
    file_size = end_pos;
    if (fseek(in, 0, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    if (fread(g_hdr, 1, 32, in) != 32) {
        fclose(in);
        return 0;
    }

    if (g_hdr[0] != 'A' || g_hdr[1] != '2' || g_hdr[2] != '3' || g_hdr[3] != 'S') {
        fclose(in);
        return 0;
    }

    in_nmembers = r32(g_hdr + 8);
    in_mem_off = r32(g_hdr + 12);
    in_str_off = r32(g_hdr + 24);
    in_str_sz = r32(g_hdr + 28);

    if (in_nmembers > MAX_MEMBERS) {
        fclose(in);
        return 0;
    }
    if (in_str_sz > MAX_STRTAB) {
        fclose(in);
        return 0;
    }
    if (in_nmembers < 0 || in_nmembers > (file_size / 24)) {
        fclose(in);
        return 0;
    }
    if (!in_bounds(in_mem_off, in_nmembers * 24, file_size)) {
        fclose(in);
        return 0;
    }
    if (!in_bounds(in_str_off, in_str_sz, file_size)) {
        fclose(in);
        return 0;
    }

    if (fseek(in, in_str_off, SEEK_SET) != 0) {
        fclose(in);
        return 0;
    }
    if (in_str_sz > 0) {
        if (fread(g_old_strtab, 1, in_str_sz, in) != in_str_sz) {
            fclose(in);
            return 0;
        }
    }

    n = *nmembers;
    i = 0;
    while (i < in_nmembers) {
        ent_off = in_mem_off + i * 24;
        if (fseek(in, ent_off, SEEK_SET) != 0) {
            fclose(in);
            return 0;
        }
        if (fread(g_ent, 1, 24, in) != 24) {
            fclose(in);
            return 0;
        }

        name_off = r32(g_ent);
        data_off = r32(g_ent + 4);
        size = r32(g_ent + 8);

        if (name_off >= in_str_sz) {
            fclose(in);
            return 0;
        }
        if (!in_bounds(data_off, size, file_size)) {
            fclose(in);
            return 0;
        }
        if (g_data_used + size > MAX_DATA) {
            fclose(in);
            return 0;
        }
        if (fseek(in, data_off, SEEK_SET) != 0) {
            fclose(in);
            return 0;
        }
        if (size > 0) {
            if (fread(g_data + g_data_used, 1, size, in) != size) {
                fclose(in);
                return 0;
            }
        }

        off = append_strtab(g_old_strtab + name_off, str_used);
        if (off == -1) {
            fclose(in);
            return 0;
        }

        g_members[n].path = archive_path;
        g_members[n].name_off = off;
        g_members[n].size = size;
        g_members[n].data_off = 0;
        g_members[n].src_kind = SRC_BUFFER;
        g_members[n].src_off = g_data_used;
        g_data_used = g_data_used + size;
        n = n + 1;

        i = i + 1;
    }

    fclose(in);
    *nmembers = n;
    return 1;
}

/* --- Delete members --- */

int delete_members(char *archive_path, int reqc, char **reqv) {
    int nmembers;
    int str_used;
    int out_n;
    int i;
    int j;
    int remove;
    char *name;

    nmembers = 0;
    str_used = 1;
    out_n = 0;

    if (reqc <= 0) return 0;
    if (reqc > MAX_MEMBERS) return 0;

    g_strtab[0] = 0;
    g_data_used = 0;
    if (!load_existing_archive(archive_path, &nmembers, &str_used)) return 0;

    i = 0;
    while (i < reqc) {
        g_req_found[i] = 0;
        i = i + 1;
    }

    i = 0;
    while (i < nmembers) {
        name = g_strtab + g_members[i].name_off;
        j = 0;
        remove = 0;
        while (j < reqc) {
            if (name_matches(name, reqv[j])) {
                g_req_found[j] = 1;
                remove = 1;
            }
            j = j + 1;
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
            out_n = out_n + 1;
        }
        i = i + 1;
    }

    i = 0;
    while (i < reqc) {
        if (!g_req_found[i]) return 0;
        i = i + 1;
    }

    build_symbol_index(out_n, &str_used);
    return write_archive(archive_path, out_n, str_used);
}

/* --- Move members --- */

int move_members(char *archive_path, int reqc, char **reqv) {
    int nmembers;
    int str_used;
    int out_n;
    int i;
    int j;
    int move_it;
    char *name;

    nmembers = 0;
    str_used = 1;
    out_n = 0;

    if (reqc <= 0) return 0;
    if (reqc > MAX_MEMBERS) return 0;

    g_strtab[0] = 0;
    g_data_used = 0;
    if (!load_existing_archive(archive_path, &nmembers, &str_used)) return 0;

    i = 0;
    while (i < reqc) {
        g_req_found[i] = 0;
        i = i + 1;
    }

    /* First pass: non-moved members */
    i = 0;
    while (i < nmembers) {
        name = g_strtab + g_members[i].name_off;
        j = 0;
        move_it = 0;
        while (j < reqc) {
            if (name_matches(name, reqv[j])) {
                g_req_found[j] = 1;
                move_it = 1;
            }
            j = j + 1;
        }
        if (!move_it) {
            g_members_tmp[out_n].path = g_members[i].path;
            g_members_tmp[out_n].name_off = g_members[i].name_off;
            g_members_tmp[out_n].size = g_members[i].size;
            g_members_tmp[out_n].data_off = g_members[i].data_off;
            g_members_tmp[out_n].src_kind = g_members[i].src_kind;
            g_members_tmp[out_n].src_off = g_members[i].src_off;
            out_n = out_n + 1;
        }
        i = i + 1;
    }

    /* Second pass: moved members appended at end */
    i = 0;
    while (i < nmembers) {
        name = g_strtab + g_members[i].name_off;
        j = 0;
        move_it = 0;
        while (j < reqc) {
            if (name_matches(name, reqv[j])) move_it = 1;
            j = j + 1;
        }
        if (move_it) {
            g_members_tmp[out_n].path = g_members[i].path;
            g_members_tmp[out_n].name_off = g_members[i].name_off;
            g_members_tmp[out_n].size = g_members[i].size;
            g_members_tmp[out_n].data_off = g_members[i].data_off;
            g_members_tmp[out_n].src_kind = g_members[i].src_kind;
            g_members_tmp[out_n].src_off = g_members[i].src_off;
            out_n = out_n + 1;
        }
        i = i + 1;
    }

    i = 0;
    while (i < reqc) {
        if (!g_req_found[i]) return 0;
        i = i + 1;
    }

    /* Copy back from tmp */
    i = 0;
    while (i < out_n) {
        g_members[i].path = g_members_tmp[i].path;
        g_members[i].name_off = g_members_tmp[i].name_off;
        g_members[i].size = g_members_tmp[i].size;
        g_members[i].data_off = g_members_tmp[i].data_off;
        g_members[i].src_kind = g_members_tmp[i].src_kind;
        g_members[i].src_off = g_members_tmp[i].src_off;
        i = i + 1;
    }

    build_symbol_index(out_n, &str_used);
    return write_archive(archive_path, out_n, str_used);
}

/* --- Main --- */

int main(int argc, char **argv) {
    char *cmd;
    char *archive;
    int nmembers;
    int str_used;
    int i;
    char *path;
    char *name;
    int size;
    int idx;
    int off;
    int loaded;

    nmembers = 0;
    str_used = 1;

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
        loaded = load_existing_archive(archive, &nmembers, &str_used);
        if (!loaded && !has_flag(cmd, 'c')) return 1;
    }

    i = 3;
    while (i < argc) {
        path = argv[i];
        name = basename_ptr(path);

        if (!count_file_bytes(path, &size)) return 1;

        idx = find_member_by_name(name, nmembers);
        if (idx >= 0) {
            g_members[idx].path = path;
            g_members[idx].size = size;
            g_members[idx].src_kind = SRC_FILE;
            g_members[idx].src_off = 0;
        } else {
            if (nmembers >= MAX_MEMBERS) return 1;
            off = append_strtab(name, &str_used);
            if (off == -1) return 1;

            g_members[nmembers].path = path;
            g_members[nmembers].name_off = off;
            g_members[nmembers].size = size;
            g_members[nmembers].data_off = 0;
            g_members[nmembers].src_kind = SRC_FILE;
            g_members[nmembers].src_off = 0;
            nmembers = nmembers + 1;
        }

        i = i + 1;
    }

    /* Load all SRC_FILE members into g_data for symbol index scanning */
    i = 0;
    while (i < nmembers) {
        if (!load_file_to_data(i)) return 1;
        i = i + 1;
    }

    /* Build symbol index from .s32o member data */
    build_symbol_index(nmembers, &str_used);

    if (!write_archive(archive, nmembers, str_used)) return 1;
    return 0;
}
