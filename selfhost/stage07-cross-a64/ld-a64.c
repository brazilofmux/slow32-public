/* ld-a64.c -- Static linker for AArch64 ELF64 relocatable objects
 *
 * Reads .o files (ELF64 ET_REL, EM_AARCH64), merges sections, resolves
 * symbols, applies relocations, writes a static ELF64 executable.
 * Also supports .a archives.
 *
 * Usage: ld-a64 [-o output] [-e entry] obj1.o obj2.o [lib.a ...]
 *
 * Mirror of stage07-cross/ld-x64.c with the AArch64 relocation set:
 *   R_AARCH64_NONE
 *   R_AARCH64_ABS64        (8-byte absolute, S+A)
 *   R_AARCH64_ABS32        (4-byte absolute, S+A)
 *   R_AARCH64_PREL32       (4-byte PC-rel)
 *   R_AARCH64_CALL26       (BL imm26 = (S+A-P)>>2)
 *   R_AARCH64_JUMP26       (B  imm26 = (S+A-P)>>2)
 *   R_AARCH64_ADR_PREL_PG_HI21
 *                          (ADRP imm21 = page(S+A) - page(P))
 *   R_AARCH64_ADD_ABS_LO12_NC
 *                          (ADD  imm12 = (S+A) & 0xFFF)
 *   R_AARCH64_LDST{8,16,32,64}_ABS_LO12_NC
 *                          (LDR/STR imm12, scaled by access size)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

/* ============================================================================
 * ELF64 definitions
 * ============================================================================ */

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

typedef struct {
    uint64_t r_offset;
    uint64_t r_info;
    int64_t  r_addend;
} Elf64_Rela;

typedef struct {
    uint32_t p_type;
    uint32_t p_flags;
    uint64_t p_offset;
    uint64_t p_vaddr;
    uint64_t p_paddr;
    uint64_t p_filesz;
    uint64_t p_memsz;
    uint64_t p_align;
} Elf64_Phdr;

#define ET_REL     1
#define ET_EXEC    2
#define EM_AARCH64 183

#define SHT_NULL     0
#define SHT_PROGBITS 1
#define SHT_SYMTAB   2
#define SHT_STRTAB   3
#define SHT_RELA     4
#define SHT_NOBITS   8

#define SHF_WRITE     1
#define SHF_ALLOC     2
#define SHF_EXECINSTR 4

#define STB_LOCAL  0
#define STB_GLOBAL 1
#define STB_WEAK   2

#define STT_NOTYPE  0
#define STT_OBJECT  1
#define STT_FUNC    2
#define STT_SECTION 3

#define SHN_UNDEF 0
#define SHN_ABS   0xFFF1

#define PT_LOAD 1
#define PF_X 1
#define PF_W 2
#define PF_R 4

/* AArch64 relocation types (from elf_aarch64.h / ABI). */
#define R_AARCH64_NONE                 0
#define R_AARCH64_ABS64              257
#define R_AARCH64_ABS32              258
#define R_AARCH64_PREL64             260
#define R_AARCH64_PREL32             261
#define R_AARCH64_ADR_PREL_LO21      274
#define R_AARCH64_ADR_PREL_PG_HI21   275
#define R_AARCH64_ADD_ABS_LO12_NC    277
#define R_AARCH64_LDST8_ABS_LO12_NC  278
#define R_AARCH64_JUMP26             282
#define R_AARCH64_CALL26             283
#define R_AARCH64_LDST16_ABS_LO12_NC 284
#define R_AARCH64_LDST32_ABS_LO12_NC 285
#define R_AARCH64_LDST64_ABS_LO12_NC 286

#define ELF64_R_SYM(i)  ((i) >> 32)
#define ELF64_R_TYPE(i)  ((i) & 0xFFFFFFFF)
#define ELF64_ST_BIND(i) ((i) >> 4)
#define ELF64_ST_TYPE(i) ((i) & 0xF)

#define ELF_BASE  0x400000
#define PAGE_SIZE 0x1000
#define PAGE_ALIGN(x) (((x) + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1))

/* ============================================================================
 * Limits
 * ============================================================================ */

#define MAX_OBJECTS   64
#define MAX_SYMBOLS   16384
#define MAX_SECTIONS  256
#define MAX_RELOCS    65536

/* ============================================================================
 * Input objects + global symbol table
 * ============================================================================ */

typedef struct {
    uint8_t *data;
    size_t   size;

    Elf64_Ehdr *ehdr;
    Elf64_Shdr *shdrs;
    int         nshdr;
    char       *shstrtab;

    Elf64_Sym  *symtab;
    int         nsyms;
    char       *strtab;

    uint64_t    sec_base[MAX_SECTIONS];
    int         sec_map[MAX_SECTIONS];
} Object;

typedef struct {
    char    *name;
    uint64_t value;
    int      defined;
    int      obj_idx;
    int      sec_type;
    int      binding;
} GlobalSym;

static Object objects[MAX_OBJECTS];
static int    nobjects;

static GlobalSym gsyms[MAX_SYMBOLS];
static int       ngsyms;

static uint8_t *merged_text;
static uint64_t merged_text_len;
static uint64_t merged_text_cap;

static uint8_t *merged_rodata;
static uint64_t merged_rodata_len;
static uint64_t merged_rodata_cap;

static uint8_t *merged_data;
static uint64_t merged_data_len;
static uint64_t merged_data_cap;

static uint64_t merged_bss_len;

static uint64_t text_vaddr;
static uint64_t rodata_vaddr;
static uint64_t data_vaddr;
static uint64_t bss_vaddr;

/* ============================================================================
 * Helpers
 * ============================================================================ */

static void die(const char *msg) {
    fprintf(stderr, "ld-a64: %s\n", msg);
    exit(1);
}

static void dief(const char *fmt, const char *arg) {
    fprintf(stderr, "ld-a64: ");
    fprintf(stderr, fmt, arg);
    fprintf(stderr, "\n");
    exit(1);
}

static uint8_t *read_file(const char *path, size_t *out_size) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    uint8_t *buf = malloc(sz);
    if (!buf) { fclose(f); return NULL; }
    if (fread(buf, 1, sz, f) != (size_t)sz) { free(buf); fclose(f); return NULL; }
    fclose(f);
    *out_size = sz;
    return buf;
}

static int gsym_find(const char *name) {
    for (int i = 0; i < ngsyms; i++)
        if (strcmp(gsyms[i].name, name) == 0) return i;
    return -1;
}

static int gsym_add(const char *name) {
    if (ngsyms >= MAX_SYMBOLS) die("too many symbols");
    int idx = ngsyms++;
    gsyms[idx].name = strdup(name);
    gsyms[idx].value = 0;
    gsyms[idx].defined = 0;
    gsyms[idx].obj_idx = -1;
    gsyms[idx].sec_type = 0;
    gsyms[idx].binding = STB_GLOBAL;
    return idx;
}

static int classify_section(const char *name) {
    if (strcmp(name, ".text") == 0) return 1;
    if (strncmp(name, ".text.", 6) == 0) return 1;
    if (strcmp(name, ".rodata") == 0) return 2;
    if (strncmp(name, ".rodata.", 8) == 0) return 2;
    if (strcmp(name, ".data") == 0) return 3;
    if (strncmp(name, ".data.", 6) == 0) return 3;
    if (strcmp(name, ".bss") == 0) return 4;
    if (strncmp(name, ".bss.", 5) == 0) return 4;
    return 0;
}

static void ensure_cap(uint8_t **buf, uint64_t *cap, uint64_t needed) {
    if (needed <= *cap) return;
    uint64_t newcap = *cap ? *cap * 2 : 65536;
    while (newcap < needed) newcap *= 2;
    *buf = realloc(*buf, newcap);
    if (!*buf) die("out of memory");
    *cap = newcap;
}

static uint64_t class_vaddr(int cls) {
    switch (cls) {
    case 1: return text_vaddr;
    case 2: return rodata_vaddr;
    case 3: return data_vaddr;
    case 4: return bss_vaddr;
    default: return 0;
    }
}

/* ============================================================================
 * Load and parse an ELF64 relocatable object
 * ============================================================================ */

static void load_object(const char *path) {
    Object *obj;
    size_t sz;
    uint8_t *data;

    if (nobjects >= MAX_OBJECTS) die("too many input objects");

    data = read_file(path, &sz);
    if (!data) dief("cannot open: %s", path);

    obj = &objects[nobjects];
    memset(obj, 0, sizeof(*obj));
    obj->data = data;
    obj->size = sz;

    if (sz < sizeof(Elf64_Ehdr)) dief("too small: %s", path);
    obj->ehdr = (Elf64_Ehdr *)data;

    if (obj->ehdr->e_ident[0] != 0x7F || obj->ehdr->e_ident[1] != 'E' ||
        obj->ehdr->e_ident[2] != 'L' || obj->ehdr->e_ident[3] != 'F')
        dief("not an ELF file: %s", path);

    if (obj->ehdr->e_ident[4] != 2) dief("not ELF64: %s", path);
    if (obj->ehdr->e_type != ET_REL) dief("not relocatable: %s", path);
    if (obj->ehdr->e_machine != EM_AARCH64) dief("not AArch64: %s", path);

    obj->shdrs = (Elf64_Shdr *)(data + obj->ehdr->e_shoff);
    obj->nshdr = obj->ehdr->e_shnum;

    if (obj->ehdr->e_shstrndx < obj->nshdr) {
        Elf64_Shdr *shstr = &obj->shdrs[obj->ehdr->e_shstrndx];
        obj->shstrtab = (char *)(data + shstr->sh_offset);
    }

    obj->symtab = NULL;
    obj->nsyms = 0;
    obj->strtab = NULL;

    for (int i = 0; i < obj->nshdr; i++) {
        if (obj->shdrs[i].sh_type == SHT_SYMTAB) {
            obj->symtab = (Elf64_Sym *)(data + obj->shdrs[i].sh_offset);
            obj->nsyms = obj->shdrs[i].sh_size / sizeof(Elf64_Sym);
            int stridx = obj->shdrs[i].sh_link;
            if (stridx < obj->nshdr)
                obj->strtab = (char *)(data + obj->shdrs[stridx].sh_offset);
        }
    }

    for (int i = 0; i < obj->nshdr && i < MAX_SECTIONS; i++) {
        const char *name = obj->shstrtab + obj->shdrs[i].sh_name;
        obj->sec_map[i] = classify_section(name);
    }

    nobjects++;
}

/* ============================================================================
 * Merge sections from all objects
 * ============================================================================ */

static void merge_sections(void) {
    for (int oi = 0; oi < nobjects; oi++) {
        Object *obj = &objects[oi];

        for (int si = 0; si < obj->nshdr; si++) {
            Elf64_Shdr *sh = &obj->shdrs[si];
            int cls = obj->sec_map[si];
            if (cls == 0) continue;

            uint64_t align = sh->sh_addralign;
            if (align < 1) align = 1;
            /* AArch64 instructions need 4-byte alignment; force it for .text. */
            if (cls == 1 && align < 4) align = 4;

            switch (cls) {
            case 1: { /* .text — fill alignment with NOPs (0xD503201F LE) */
                while (merged_text_len & (align - 1)) {
                    ensure_cap(&merged_text, &merged_text_cap, merged_text_len + 4);
                    if ((merged_text_len & 3) == 0) {
                        merged_text[merged_text_len + 0] = 0x1F;
                        merged_text[merged_text_len + 1] = 0x20;
                        merged_text[merged_text_len + 2] = 0x03;
                        merged_text[merged_text_len + 3] = 0xD5;
                        merged_text_len += 4;
                    } else {
                        /* Should not happen: we always start aligned-to-4
                         * after a clean section boundary. Pad with zero. */
                        merged_text[merged_text_len++] = 0;
                    }
                }
                obj->sec_base[si] = merged_text_len;
                if (sh->sh_size > 0) {
                    ensure_cap(&merged_text, &merged_text_cap, merged_text_len + sh->sh_size);
                    memcpy(merged_text + merged_text_len,
                           obj->data + sh->sh_offset, sh->sh_size);
                    merged_text_len += sh->sh_size;
                }
                break;
            }
            case 2: { /* .rodata */
                while (merged_rodata_len & (align - 1)) {
                    ensure_cap(&merged_rodata, &merged_rodata_cap, merged_rodata_len + 1);
                    merged_rodata[merged_rodata_len++] = 0;
                }
                obj->sec_base[si] = merged_rodata_len;
                if (sh->sh_size > 0) {
                    ensure_cap(&merged_rodata, &merged_rodata_cap, merged_rodata_len + sh->sh_size);
                    memcpy(merged_rodata + merged_rodata_len,
                           obj->data + sh->sh_offset, sh->sh_size);
                    merged_rodata_len += sh->sh_size;
                }
                break;
            }
            case 3: { /* .data */
                while (merged_data_len & (align - 1)) {
                    ensure_cap(&merged_data, &merged_data_cap, merged_data_len + 1);
                    merged_data[merged_data_len++] = 0;
                }
                obj->sec_base[si] = merged_data_len;
                if (sh->sh_size > 0) {
                    ensure_cap(&merged_data, &merged_data_cap, merged_data_len + sh->sh_size);
                    memcpy(merged_data + merged_data_len,
                           obj->data + sh->sh_offset, sh->sh_size);
                    merged_data_len += sh->sh_size;
                }
                break;
            }
            case 4: { /* .bss */
                while (merged_bss_len & (align - 1)) merged_bss_len++;
                obj->sec_base[si] = merged_bss_len;
                merged_bss_len += sh->sh_size;
                break;
            }
            }
        }
    }
}

/* ============================================================================
 * Compute virtual address layout
 * ============================================================================ */

static void compute_layout(void) {
    text_vaddr = ELF_BASE + PAGE_SIZE;
    rodata_vaddr = text_vaddr + PAGE_ALIGN(merged_text_len);
    if (merged_rodata_len > 0)
        data_vaddr = rodata_vaddr + PAGE_ALIGN(merged_rodata_len);
    else
        data_vaddr = rodata_vaddr;
    bss_vaddr = data_vaddr + merged_data_len;
}

/* ============================================================================
 * Resolve a local symbol → final virtual address
 * ============================================================================ */

static uint64_t resolve_local_sym(Object *obj, int sym_idx) {
    Elf64_Sym *sym = &obj->symtab[sym_idx];
    int bind = ELF64_ST_BIND(sym->st_info);
    int type = ELF64_ST_TYPE(sym->st_info);
    const char *name = obj->strtab + sym->st_name;

    if (type == STT_SECTION) {
        int shndx = sym->st_shndx;
        int cls = obj->sec_map[shndx];
        if (shndx >= MAX_SECTIONS) return 0;
        return class_vaddr(cls) + obj->sec_base[shndx];
    }

    if (sym->st_shndx == SHN_UNDEF) {
        int gi = gsym_find(name);
        if (gi < 0 || !gsyms[gi].defined) {
            fprintf(stderr, "ld-a64: undefined symbol: %s\n", name);
            exit(1);
        }
        return gsyms[gi].value;
    }

    if (sym->st_shndx < MAX_SECTIONS) {
        int cls = obj->sec_map[sym->st_shndx];
        if (cls != 0)
            return class_vaddr(cls) + obj->sec_base[sym->st_shndx] + sym->st_value;
    }

    if (bind == STB_GLOBAL && sym->st_name != 0) {
        int gi = gsym_find(name);
        if (gi >= 0 && gsyms[gi].defined)
            return gsyms[gi].value;
    }
    return sym->st_value;
}

/* ============================================================================
 * Collect global symbols from all objects
 * ============================================================================ */

static void collect_symbols(void) {
    for (int oi = 0; oi < nobjects; oi++) {
        Object *obj = &objects[oi];
        if (!obj->symtab) continue;

        for (int si = 0; si < obj->nsyms; si++) {
            Elf64_Sym *sym = &obj->symtab[si];
            int bind = ELF64_ST_BIND(sym->st_info);
            int type = ELF64_ST_TYPE(sym->st_info);

            if (bind != STB_GLOBAL && bind != STB_WEAK) continue;
            if (type == STT_SECTION) continue;
            if (sym->st_name == 0) continue;

            const char *name = obj->strtab + sym->st_name;
            int gi = gsym_find(name);

            if (sym->st_shndx == SHN_UNDEF) {
                if (gi < 0) gsym_add(name);
                continue;
            }

            int cls = sym->st_shndx < MAX_SECTIONS ? obj->sec_map[sym->st_shndx] : 0;
            uint64_t val = sym->st_value;
            if (cls != 0 && sym->st_shndx < MAX_SECTIONS)
                val += class_vaddr(cls) + obj->sec_base[sym->st_shndx];

            if (gi < 0) {
                gi = gsym_add(name);
            } else if (gsyms[gi].defined && gsyms[gi].binding != STB_WEAK) {
                if (bind != STB_WEAK) {
                    fprintf(stderr, "ld-a64: duplicate symbol: %s\n", name);
                    exit(1);
                }
                continue;
            }

            gsyms[gi].value = val;
            gsyms[gi].defined = 1;
            gsyms[gi].obj_idx = oi;
            gsyms[gi].sec_type = cls;
            gsyms[gi].binding = bind;
        }
    }
}

/* ============================================================================
 * AArch64 instruction patching helpers
 * ============================================================================ */

static uint32_t a64_read_word(uint8_t *p) {
    return (uint32_t)p[0]
         | ((uint32_t)p[1] << 8)
         | ((uint32_t)p[2] << 16)
         | ((uint32_t)p[3] << 24);
}

static void a64_write_word(uint8_t *p, uint32_t w) {
    p[0] = (w >>  0) & 0xFF;
    p[1] = (w >>  8) & 0xFF;
    p[2] = (w >> 16) & 0xFF;
    p[3] = (w >> 24) & 0xFF;
}

/* Patch B / BL imm26 = (target - PC) >> 2.  Range +/- 128MB. */
static void patch_imm26(uint8_t *p, int64_t diff) {
    uint32_t inst = a64_read_word(p);
    int64_t shifted = diff >> 2;
    if (shifted < -(1LL << 25) || shifted >= (1LL << 25)) {
        fprintf(stderr, "ld-a64: branch out of imm26 range (%ld)\n", (long)shifted);
        exit(1);
    }
    inst = (inst & 0xFC000000u) | ((uint32_t)shifted & 0x03FFFFFFu);
    a64_write_word(p, inst);
}

/* Patch ADRP imm21 = (page(target) - page(P)) >> 12.  Range +/- 4GB. */
static void patch_adrp_hi21(uint8_t *p, uint64_t target, uint64_t pc) {
    uint64_t tgt_page = target & ~0xFFFULL;
    uint64_t pc_page  = pc & ~0xFFFULL;
    int64_t diff = (int64_t)(tgt_page - pc_page);
    int64_t scaled = diff >> 12;
    if (scaled < -(1LL << 20) || scaled >= (1LL << 20)) {
        fprintf(stderr, "ld-a64: ADRP out of range (%ld pages)\n", (long)scaled);
        exit(1);
    }
    uint32_t inst = a64_read_word(p);
    uint32_t immlo = (uint32_t)(scaled & 3);
    uint32_t immhi = (uint32_t)((scaled >> 2) & 0x7FFFF);
    inst = (inst & 0x9F00001Fu) | (immlo << 29) | (immhi << 5);
    a64_write_word(p, inst);
}

/* Patch ADD/LDR/STR imm12 at bits 21..10 with (target & 0xFFF) >> shift. */
static void patch_imm12_lo12(uint8_t *p, uint64_t target, int shift) {
    uint32_t lo12 = target & 0xFFF;
    uint32_t scaled = lo12 >> shift;
    uint32_t inst = a64_read_word(p);
    inst = (inst & 0xFFC003FFu) | ((scaled & 0xFFF) << 10);
    a64_write_word(p, inst);
}

/* ============================================================================
 * Apply relocations
 * ============================================================================ */

static void apply_relocations(void) {
    for (int oi = 0; oi < nobjects; oi++) {
        Object *obj = &objects[oi];

        for (int si = 0; si < obj->nshdr; si++) {
            Elf64_Shdr *sh = &obj->shdrs[si];
            if (sh->sh_type != SHT_RELA) continue;

            int target_sec = sh->sh_info;
            int target_cls = obj->sec_map[target_sec];
            if (target_cls == 0) continue;

            uint8_t *buf;
            uint64_t buf_base_off;
            uint64_t buf_vaddr;

            switch (target_cls) {
            case 1: buf = merged_text;   buf_vaddr = text_vaddr;   break;
            case 2: buf = merged_rodata; buf_vaddr = rodata_vaddr; break;
            case 3: buf = merged_data;   buf_vaddr = data_vaddr;   break;
            default: continue;
            }
            buf_base_off = obj->sec_base[target_sec];

            int nrela = sh->sh_size / sizeof(Elf64_Rela);
            Elf64_Rela *relas = (Elf64_Rela *)(obj->data + sh->sh_offset);

            for (int ri = 0; ri < nrela; ri++) {
                Elf64_Rela *r = &relas[ri];
                uint32_t sym_idx = ELF64_R_SYM(r->r_info);
                uint32_t rtype = ELF64_R_TYPE(r->r_info);

                uint64_t S = resolve_local_sym(obj, sym_idx);
                int64_t  A = r->r_addend;
                uint64_t P = buf_vaddr + buf_base_off + r->r_offset;
                uint8_t *patch = buf + buf_base_off + r->r_offset;
                uint64_t SA = (uint64_t)((int64_t)S + A);

                switch (rtype) {
                case R_AARCH64_NONE:
                    break;

                case R_AARCH64_ABS64:
                    /* Write 8-byte little-endian. */
                    {
                        uint64_t v = SA;
                        for (int b = 0; b < 8; b++)
                            patch[b] = (uint8_t)((v >> (b * 8)) & 0xFF);
                    }
                    break;

                case R_AARCH64_ABS32:
                    {
                        uint32_t v = (uint32_t)SA;
                        a64_write_word(patch, v);
                    }
                    break;

                case R_AARCH64_PREL32:
                    {
                        int64_t v = (int64_t)SA - (int64_t)P;
                        if (v < -(1LL << 31) || v >= (1LL << 31)) {
                            fprintf(stderr, "ld-a64: PREL32 overflow\n");
                            exit(1);
                        }
                        a64_write_word(patch, (uint32_t)(int32_t)v);
                    }
                    break;

                case R_AARCH64_CALL26:
                case R_AARCH64_JUMP26:
                    patch_imm26(patch, (int64_t)SA - (int64_t)P);
                    break;

                case R_AARCH64_ADR_PREL_PG_HI21:
                    patch_adrp_hi21(patch, SA, P);
                    break;

                case R_AARCH64_ADD_ABS_LO12_NC:
                    patch_imm12_lo12(patch, SA, 0);
                    break;

                case R_AARCH64_LDST8_ABS_LO12_NC:
                    patch_imm12_lo12(patch, SA, 0);
                    break;

                case R_AARCH64_LDST16_ABS_LO12_NC:
                    patch_imm12_lo12(patch, SA, 1);
                    break;

                case R_AARCH64_LDST32_ABS_LO12_NC:
                    patch_imm12_lo12(patch, SA, 2);
                    break;

                case R_AARCH64_LDST64_ABS_LO12_NC:
                    patch_imm12_lo12(patch, SA, 3);
                    break;

                default:
                    fprintf(stderr, "ld-a64: unsupported relocation type %u\n", rtype);
                    exit(1);
                }
            }
        }
    }
}

/* ============================================================================
 * Write output ELF executable
 * ============================================================================ */

static void write_executable(const char *filename, const char *entry_name) {
    int gi = gsym_find(entry_name);
    if (gi < 0 || !gsyms[gi].defined) {
        fprintf(stderr, "ld-a64: entry point '%s' not found\n", entry_name);
        exit(1);
    }
    uint64_t entry = gsyms[gi].value;

    int nphdr = 1;
    if (merged_rodata_len > 0) nphdr++;
    if (merged_data_len > 0 || merged_bss_len > 0) nphdr++;

    uint64_t ehdr_size = sizeof(Elf64_Ehdr);
    uint64_t phdr_size = nphdr * sizeof(Elf64_Phdr);
    (void)phdr_size;

    uint64_t text_foff = PAGE_SIZE;
    uint64_t rodata_foff = text_foff + PAGE_ALIGN(merged_text_len);
    uint64_t data_foff;

    if (merged_rodata_len > 0)
        data_foff = rodata_foff + PAGE_ALIGN(merged_rodata_len);
    else
        data_foff = rodata_foff;

    uint64_t file_size = data_foff + merged_data_len;
    if (file_size < PAGE_SIZE) file_size = PAGE_SIZE;

    uint8_t *out = calloc(1, file_size);
    if (!out) die("out of memory for output");

    Elf64_Ehdr *ehdr = (Elf64_Ehdr *)out;
    ehdr->e_ident[0] = 0x7F;
    ehdr->e_ident[1] = 'E';
    ehdr->e_ident[2] = 'L';
    ehdr->e_ident[3] = 'F';
    ehdr->e_ident[4] = 2;
    ehdr->e_ident[5] = 1;
    ehdr->e_ident[6] = 1;
    ehdr->e_type = ET_EXEC;
    ehdr->e_machine = EM_AARCH64;
    ehdr->e_version = 1;
    ehdr->e_entry = entry;
    ehdr->e_phoff = ehdr_size;
    ehdr->e_shoff = 0;
    ehdr->e_ehsize = sizeof(Elf64_Ehdr);
    ehdr->e_phentsize = sizeof(Elf64_Phdr);
    ehdr->e_phnum = nphdr;

    Elf64_Phdr *phdrs = (Elf64_Phdr *)(out + ehdr_size);
    int phi = 0;

    phdrs[phi].p_type = PT_LOAD;
    phdrs[phi].p_flags = PF_R | PF_X;
    phdrs[phi].p_offset = text_foff;
    phdrs[phi].p_vaddr = text_vaddr;
    phdrs[phi].p_paddr = text_vaddr;
    phdrs[phi].p_filesz = merged_text_len;
    phdrs[phi].p_memsz = merged_text_len;
    phdrs[phi].p_align = PAGE_SIZE;
    phi++;

    if (merged_rodata_len > 0) {
        phdrs[phi].p_type = PT_LOAD;
        phdrs[phi].p_flags = PF_R;
        phdrs[phi].p_offset = rodata_foff;
        phdrs[phi].p_vaddr = rodata_vaddr;
        phdrs[phi].p_paddr = rodata_vaddr;
        phdrs[phi].p_filesz = merged_rodata_len;
        phdrs[phi].p_memsz = merged_rodata_len;
        phdrs[phi].p_align = PAGE_SIZE;
        phi++;
    }

    if (merged_data_len > 0 || merged_bss_len > 0) {
        phdrs[phi].p_type = PT_LOAD;
        phdrs[phi].p_flags = PF_R | PF_W;
        phdrs[phi].p_offset = data_foff;
        phdrs[phi].p_vaddr = data_vaddr;
        phdrs[phi].p_paddr = data_vaddr;
        phdrs[phi].p_filesz = merged_data_len;
        phdrs[phi].p_memsz = merged_data_len + merged_bss_len;
        phdrs[phi].p_align = PAGE_SIZE;
        phi++;
    }

    if (merged_text_len > 0)
        memcpy(out + text_foff, merged_text, merged_text_len);
    if (merged_rodata_len > 0)
        memcpy(out + rodata_foff, merged_rodata, merged_rodata_len);
    if (merged_data_len > 0)
        memcpy(out + data_foff, merged_data, merged_data_len);

    int fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0755);
    if (fd < 0) dief("cannot create: %s", filename);

    size_t total = 0;
    while (total < file_size) {
        ssize_t n = write(fd, out + total, file_size - total);
        if (n <= 0) { close(fd); dief("write error: %s", filename); }
        total += n;
    }
    close(fd);
    free(out);
}

/* ============================================================================
 * Archive support — same format as Unix `ar`, fully arch-agnostic except
 * we sanity-check that members are EM_AARCH64 ELF objects.
 * ============================================================================ */

#define MAX_ARCHIVES 32

typedef struct {
    uint8_t *data;
    size_t   size;
    int      nsyms;
    char   **sym_names;
    uint32_t *sym_offsets;
    int      loaded[MAX_OBJECTS];
    int      nloaded;
} Archive;

static Archive archives[MAX_ARCHIVES];
static int     narchives;

static uint32_t read_be32(const uint8_t *p) {
    return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) |
           ((uint32_t)p[2] << 8) | p[3];
}

static void load_archive(const char *path) {
    if (narchives >= MAX_ARCHIVES) die("too many archives");

    Archive *ar = &archives[narchives];
    memset(ar, 0, sizeof(*ar));

    ar->data = read_file(path, &ar->size);
    if (!ar->data) dief("cannot open archive: %s", path);

    if (ar->size < 8 || memcmp(ar->data, "!<arch>\n", 8) != 0)
        dief("not an archive: %s", path);

    size_t pos = 8;
    if (pos + 60 > ar->size) dief("truncated archive: %s", path);

    char *hdr = (char *)(ar->data + pos);
    if (hdr[0] != '/') dief("archive has no symbol index: %s", path);

    long idx_size = strtol(hdr + 48, NULL, 10);
    pos += 60;

    if (pos + idx_size > ar->size) dief("truncated archive index: %s", path);

    uint8_t *idx = ar->data + pos;
    ar->nsyms = read_be32(idx);
    ar->sym_offsets = malloc(ar->nsyms * sizeof(uint32_t));
    ar->sym_names = malloc(ar->nsyms * sizeof(char *));

    for (int i = 0; i < ar->nsyms; i++)
        ar->sym_offsets[i] = read_be32(idx + 4 + i * 4);

    char *names = (char *)(idx + 4 + ar->nsyms * 4);
    for (int i = 0; i < ar->nsyms; i++) {
        ar->sym_names[i] = names;
        names += strlen(names) + 1;
    }

    ar->nloaded = 0;
    narchives++;
}

static int archive_member_loaded(Archive *ar, uint32_t offset) {
    for (int i = 0; i < ar->nloaded; i++)
        if (ar->loaded[i] == (int)offset) return 1;
    return 0;
}

static void load_archive_member(Archive *ar, uint32_t offset) {
    if (archive_member_loaded(ar, offset)) return;
    if (ar->nloaded >= MAX_OBJECTS) die("too many archive members");

    ar->loaded[ar->nloaded++] = offset;

    if (offset + 60 > ar->size) die("truncated archive member");
    char *hdr = (char *)(ar->data + offset);
    long mem_size = strtol(hdr + 48, NULL, 10);
    uint8_t *mem_data = ar->data + offset + 60;

    if (offset + 60 + mem_size > ar->size) die("truncated archive member data");

    if (nobjects >= MAX_OBJECTS) die("too many objects");
    Object *obj = &objects[nobjects];
    memset(obj, 0, sizeof(*obj));
    obj->data = malloc(mem_size);
    memcpy(obj->data, mem_data, mem_size);
    obj->size = mem_size;

    obj->ehdr = (Elf64_Ehdr *)obj->data;
    if (mem_size < (long)sizeof(Elf64_Ehdr) ||
        obj->ehdr->e_ident[0] != 0x7F ||
        obj->ehdr->e_ident[1] != 'E' ||
        obj->ehdr->e_ident[2] != 'L' ||
        obj->ehdr->e_ident[3] != 'F') {
        free(obj->data);
        return;
    }
    if (obj->ehdr->e_ident[4] != 2) die("archive member is not ELF64");
    if (obj->ehdr->e_type != ET_REL) die("archive member is not relocatable");
    if (obj->ehdr->e_machine != EM_AARCH64) die("archive member is not AArch64");

    obj->shdrs = (Elf64_Shdr *)(obj->data + obj->ehdr->e_shoff);
    obj->nshdr = obj->ehdr->e_shnum;

    if (obj->ehdr->e_shstrndx < obj->nshdr) {
        Elf64_Shdr *shstr = &obj->shdrs[obj->ehdr->e_shstrndx];
        obj->shstrtab = (char *)(obj->data + shstr->sh_offset);
    }

    obj->symtab = NULL;
    obj->nsyms = 0;
    obj->strtab = NULL;

    for (int i = 0; i < obj->nshdr; i++) {
        if (obj->shdrs[i].sh_type == SHT_SYMTAB) {
            obj->symtab = (Elf64_Sym *)(obj->data + obj->shdrs[i].sh_offset);
            obj->nsyms = obj->shdrs[i].sh_size / sizeof(Elf64_Sym);
            int stridx = obj->shdrs[i].sh_link;
            if (stridx < obj->nshdr)
                obj->strtab = (char *)(obj->data + obj->shdrs[stridx].sh_offset);
        }
    }

    for (int i = 0; i < obj->nshdr && i < MAX_SECTIONS; i++) {
        const char *name = obj->shstrtab + obj->shdrs[i].sh_name;
        obj->sec_map[i] = classify_section(name);
    }

    nobjects++;
}

static int count_undefined(void) {
    ngsyms = 0;

    for (int oi = 0; oi < nobjects; oi++) {
        Object *obj = &objects[oi];
        if (!obj->symtab) continue;

        for (int si = 0; si < obj->nsyms; si++) {
            Elf64_Sym *sym = &obj->symtab[si];
            int bind = ELF64_ST_BIND(sym->st_info);
            int type = ELF64_ST_TYPE(sym->st_info);

            if (bind != STB_GLOBAL && bind != STB_WEAK) continue;
            if (type == STT_SECTION) continue;
            if (sym->st_name == 0) continue;

            const char *name = obj->strtab + sym->st_name;
            int gi = gsym_find(name);

            if (sym->st_shndx == SHN_UNDEF) {
                if (gi < 0) gi = gsym_add(name);
                continue;
            }

            if (gi < 0) gi = gsym_add(name);
            gsyms[gi].defined = 1;
        }
    }

    int undef = 0;
    for (int i = 0; i < ngsyms; i++)
        if (!gsyms[i].defined) undef++;
    return undef;
}

static void resolve_archives(void) {
    int changed = 1;
    while (changed) {
        changed = 0;

        ngsyms = 0;
        count_undefined();

        for (int gi = 0; gi < ngsyms; gi++) {
            int matched;
            if (gsyms[gi].defined) continue;
            matched = 0;

            for (int ai = 0; ai < narchives && !matched; ai++) {
                Archive *ar = &archives[ai];
                for (int si = 0; si < ar->nsyms; si++) {
                    if (strcmp(gsyms[gi].name, ar->sym_names[si]) == 0) {
                        if (!archive_member_loaded(ar, ar->sym_offsets[si])) {
                            load_archive_member(ar, ar->sym_offsets[si]);
                            changed = 1;
                        }
                        matched = 1;
                        break;
                    }
                }
            }
        }
    }
}

/* ============================================================================
 * Main
 * ============================================================================ */

int main(int argc, char **argv) {
    const char *outfile = "a.out";
    const char *entry = "_start";
    int i;

    i = 1;
    while (i < argc) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            outfile = argv[++i];
        } else if (strcmp(argv[i], "-e") == 0 && i + 1 < argc) {
            entry = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "ld-a64: unknown option: %s\n", argv[i]);
            return 1;
        } else {
            size_t len = strlen(argv[i]);
            if (len > 2 && argv[i][len-2] == '.' && argv[i][len-1] == 'a')
                load_archive(argv[i]);
            else
                load_object(argv[i]);
        }
        i++;
    }

    if (nobjects == 0 && narchives == 0) {
        fprintf(stderr, "Usage: ld-a64 [-o output] [-e entry] obj1.o obj2.o [lib.a ...]\n");
        return 1;
    }

    if (narchives > 0) resolve_archives();

    merge_sections();
    compute_layout();

    ngsyms = 0;
    collect_symbols();

    {
        int undef = 0;
        for (int j = 0; j < ngsyms; j++) {
            if (!gsyms[j].defined) {
                fprintf(stderr, "ld-a64: undefined symbol: %s\n", gsyms[j].name);
                undef++;
            }
        }
        if (undef) return 1;
    }

    apply_relocations();
    write_executable(outfile, entry);

    fprintf(stderr, "ld-a64: wrote %s (text=%lu rodata=%lu data=%lu bss=%lu)\n",
            outfile, merged_text_len, merged_rodata_len,
            merged_data_len, merged_bss_len);

    return 0;
}
