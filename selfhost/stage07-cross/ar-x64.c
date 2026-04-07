/* ar-x64.c -- Simple archiver for ELF64 object files
 *
 * Produces standard Unix ar format archives with a symbol index.
 * Only supports 'rcs' mode (create archive with symbol index).
 *
 * Usage: ar-x64 rcs output.a obj1.o obj2.o ...
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>

/* ELF64 structures (minimal, for symbol extraction) */
typedef struct {
    unsigned char e_ident[16];
    uint16_t e_type;
    uint16_t e_machine;
    uint32_t e_version;
    uint64_t e_entry;
    uint64_t e_phoff;
    uint64_t e_shoff;
    uint32_t e_flags;
    uint16_t e_ehsize;
    uint16_t e_phentsize;
    uint16_t e_phnum;
    uint16_t e_shentsize;
    uint16_t e_shnum;
    uint16_t e_shstrndx;
} Elf64_Ehdr;

typedef struct {
    uint32_t sh_name;
    uint32_t sh_type;
    uint64_t sh_flags;
    uint64_t sh_addr;
    uint64_t sh_offset;
    uint64_t sh_size;
    uint32_t sh_link;
    uint32_t sh_info;
    uint64_t sh_addralign;
    uint64_t sh_entsize;
} Elf64_Shdr;

typedef struct {
    uint32_t st_name;
    uint8_t  st_info;
    uint8_t  st_other;
    uint16_t st_shndx;
    uint64_t st_value;
    uint64_t st_size;
} Elf64_Sym;

#define SHT_SYMTAB 2
#define SHN_UNDEF  0

#define MAX_MEMBERS 256
#define MAX_SYMS    16384

/* Member info */
typedef struct {
    const char *path;
    const char *basename;
    uint8_t    *data;
    size_t      size;
} Member;

/* Symbol index entry */
typedef struct {
    const char *name;
    int         member_idx;
} SymEntry;

static Member members[MAX_MEMBERS];
static int    nmembers;

static SymEntry syms[MAX_SYMS];
static int      nsyms;

static uint8_t *read_file(const char *path, size_t *out_size) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    uint8_t *buf = malloc(sz);
    if (!buf) { fclose(f); return NULL; }
    fread(buf, 1, sz, f);
    fclose(f);
    *out_size = sz;
    return buf;
}

static const char *get_basename(const char *path) {
    const char *p = path;
    const char *last = path;
    while (*p) {
        if (*p == '/') last = p + 1;
        p++;
    }
    return last;
}

/* Extract global defined symbols from an ELF object */
static void extract_symbols(int member_idx) {
    Member *m = &members[member_idx];
    Elf64_Ehdr *ehdr = (Elf64_Ehdr *)m->data;

    if (m->size < sizeof(Elf64_Ehdr)) return;
    if (ehdr->e_ident[0] != 0x7F || ehdr->e_ident[1] != 'E') return;

    Elf64_Shdr *shdrs = (Elf64_Shdr *)(m->data + ehdr->e_shoff);

    for (int i = 0; i < ehdr->e_shnum; i++) {
        if (shdrs[i].sh_type != SHT_SYMTAB) continue;

        Elf64_Sym *symtab = (Elf64_Sym *)(m->data + shdrs[i].sh_offset);
        int nsym = shdrs[i].sh_size / sizeof(Elf64_Sym);
        int stridx = shdrs[i].sh_link;
        char *strtab = (char *)(m->data + shdrs[stridx].sh_offset);

        for (int j = 0; j < nsym; j++) {
            int bind = symtab[j].st_info >> 4;
            if (bind != 1 && bind != 2) continue; /* GLOBAL or WEAK only */
            if (symtab[j].st_shndx == SHN_UNDEF) continue; /* skip undefined */
            if (symtab[j].st_name == 0) continue;

            if (nsyms >= MAX_SYMS) {
                fprintf(stderr, "ar-x64: too many symbols\n");
                exit(1);
            }
            syms[nsyms].name = strtab + symtab[j].st_name;
            syms[nsyms].member_idx = member_idx;
            nsyms++;
        }
    }
}

/* Write a big-endian 32-bit value */
static void write_be32(FILE *f, uint32_t v) {
    uint8_t buf[4];
    buf[0] = (v >> 24) & 0xFF;
    buf[1] = (v >> 16) & 0xFF;
    buf[2] = (v >> 8) & 0xFF;
    buf[3] = v & 0xFF;
    fwrite(buf, 1, 4, f);
}

int main(int argc, char **argv) {
    if (argc < 4 || strcmp(argv[1], "rcs") != 0) {
        fprintf(stderr, "Usage: ar-x64 rcs output.a obj1.o obj2.o ...\n");
        return 1;
    }

    const char *outfile = argv[2];

    /* Load all member files */
    for (int i = 3; i < argc; i++) {
        if (nmembers >= MAX_MEMBERS) {
            fprintf(stderr, "ar-x64: too many members\n");
            return 1;
        }
        Member *m = &members[nmembers];
        m->path = argv[i];
        m->basename = get_basename(argv[i]);
        m->data = read_file(argv[i], &m->size);
        if (!m->data) {
            fprintf(stderr, "ar-x64: cannot read: %s\n", argv[i]);
            return 1;
        }
        extract_symbols(nmembers);
        nmembers++;
    }

    /* Build symbol index content (System V format):
     *   4 bytes big-endian: number of symbols
     *   N * 4 bytes big-endian: file offset of member defining each symbol
     *   N * NUL-terminated strings: symbol names
     *
     * We don't know member offsets yet, so we'll do two passes. */

    /* Compute symbol index size */
    size_t sym_strtab_size = 0;
    for (int i = 0; i < nsyms; i++)
        sym_strtab_size += strlen(syms[i].name) + 1;

    size_t sym_index_size = 4 + nsyms * 4 + sym_strtab_size;

    /* Ar header for symbol index: "/               " */
    /* Each ar member header is 60 bytes */
    /* Compute member offsets:
     *   8 (ar magic)
     *   + 60 (symbol index header) + sym_index_size (padded to even)
     *   + for each member: 60 (header) + size (padded to even) */

    size_t sym_padded = sym_index_size + (sym_index_size & 1);
    size_t offset = 8 + 60 + sym_padded;

    /* Compute each member's file offset */
    size_t member_offsets[MAX_MEMBERS];
    for (int i = 0; i < nmembers; i++) {
        member_offsets[i] = offset;
        offset += 60 + members[i].size + (members[i].size & 1);
    }

    /* Write the archive */
    FILE *f = fopen(outfile, "wb");
    if (!f) {
        fprintf(stderr, "ar-x64: cannot create: %s\n", outfile);
        return 1;
    }

    /* Archive magic */
    fwrite("!<arch>\n", 1, 8, f);

    /* Symbol index member header */
    {
        char hdr[60];
        memset(hdr, ' ', 60);
        hdr[0] = '/'; /* name = "/" */
        /* timestamp, uid, gid = 0 */
        hdr[16] = '0'; /* timestamp */
        hdr[48] = '0'; /* uid */
        hdr[54] = '0'; /* gid */
        /* mode */
        hdr[40] = '0';
        /* size */
        char sizebuf[16];
        sprintf(sizebuf, "%lu", (unsigned long)sym_index_size);
        memcpy(hdr + 48, sizebuf, strlen(sizebuf));

        /* Actually, ar header format is:
         * name[16] mtime[12] uid[6] gid[6] mode[8] size[10] fmag[2] */
        memset(hdr, ' ', 60);
        memcpy(hdr, "/", 1);               /* name */
        memcpy(hdr + 16, "0", 1);          /* mtime */
        memcpy(hdr + 28, "0", 1);          /* uid */
        memcpy(hdr + 34, "0", 1);          /* gid */
        memcpy(hdr + 40, "100644", 6);     /* mode */
        sprintf(sizebuf, "%lu", (unsigned long)sym_index_size);
        memcpy(hdr + 48, sizebuf, strlen(sizebuf));
        hdr[58] = '`';
        hdr[59] = '\n';
        fwrite(hdr, 1, 60, f);
    }

    /* Symbol index content */
    write_be32(f, nsyms);
    for (int i = 0; i < nsyms; i++)
        write_be32(f, member_offsets[syms[i].member_idx]);
    for (int i = 0; i < nsyms; i++) {
        fwrite(syms[i].name, 1, strlen(syms[i].name) + 1, f);
    }
    /* Pad to even */
    if (sym_index_size & 1) fputc('\n', f);

    /* Write each member */
    for (int i = 0; i < nmembers; i++) {
        char hdr[60];
        char namebuf[17];
        char sizebuf[16];

        memset(hdr, ' ', 60);

        /* Name: "filename.o/" padded to 16 chars */
        snprintf(namebuf, sizeof(namebuf), "%s/", members[i].basename);
        memcpy(hdr, namebuf, strlen(namebuf));

        /* mtime */
        sprintf(sizebuf, "%lu", (unsigned long)time(NULL));
        memcpy(hdr + 16, sizebuf, strlen(sizebuf));

        /* uid, gid */
        hdr[28] = '0';
        hdr[34] = '0';

        /* mode */
        memcpy(hdr + 40, "100644", 6);

        /* size */
        sprintf(sizebuf, "%lu", (unsigned long)members[i].size);
        memcpy(hdr + 48, sizebuf, strlen(sizebuf));

        /* fmag */
        hdr[58] = '`';
        hdr[59] = '\n';

        fwrite(hdr, 1, 60, f);
        fwrite(members[i].data, 1, members[i].size, f);

        /* Pad to even */
        if (members[i].size & 1) fputc('\n', f);
    }

    fclose(f);
    fprintf(stderr, "ar-x64: created %s (%d members, %d symbols)\n",
            outfile, nmembers, nsyms);
    return 0;
}
