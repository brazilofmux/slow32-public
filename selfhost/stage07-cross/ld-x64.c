/* ld-x64.c -- Static linker for x86-64 ELF64 relocatable objects
 *
 * Reads .o files (ELF64 ET_REL), merges sections, resolves symbols,
 * applies relocations, writes a static ELF64 executable.
 *
 * Usage: ld-x64 [-o output] [-e entry] obj1.o obj2.o ...
 *
 * For now, compiled with host GCC. Eventually compilable by stage07.
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

/* ELF header */
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

/* Section header */
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

/* Symbol */
typedef struct {
    uint32_t st_name;
    uint8_t  st_info;
    uint8_t  st_other;
    uint16_t st_shndx;
    uint64_t st_value;
    uint64_t st_size;
} Elf64_Sym;

/* Relocation with addend */
typedef struct {
    uint64_t r_offset;
    uint64_t r_info;
    int64_t  r_addend;
} Elf64_Rela;

/* Program header */
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

/* Constants */
#define ET_REL   1
#define ET_EXEC  2
#define EM_X86_64 62

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

#define R_X86_64_NONE  0
#define R_X86_64_64    1
#define R_X86_64_PC32  2
#define R_X86_64_32S   11

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
 * Input object tracking
 * ============================================================================ */

typedef struct {
    /* Raw file data */
    uint8_t *data;
    size_t   size;

    /* Parsed ELF */
    Elf64_Ehdr *ehdr;
    Elf64_Shdr *shdrs;
    int         nshdr;
    char       *shstrtab;

    /* Symbol table */
    Elf64_Sym  *symtab;
    int         nsyms;
    char       *strtab;

    /* Section merge offsets: merged_base[local_shndx] = offset in merged section */
    uint64_t    text_base;
    uint64_t    rodata_base;
    uint64_t    data_base;
    uint64_t    bss_base;

    /* Map local section index → which merged section (0=none, 1=text, 2=rodata, 3=data, 4=bss) */
    int         sec_map[MAX_SECTIONS];
} Object;

/* ============================================================================
 * Global symbol table
 * ============================================================================ */

typedef struct {
    char    *name;
    uint64_t value;      /* final virtual address */
    int      defined;    /* 1 = defined, 0 = undefined */
    int      obj_idx;    /* which object defined it */
    int      sec_type;   /* 1=text, 2=rodata, 3=data, 4=bss */
    int      binding;    /* STB_LOCAL, STB_GLOBAL, STB_WEAK */
} GlobalSym;

/* ============================================================================
 * State
 * ============================================================================ */

static Object objects[MAX_OBJECTS];
static int    nobjects;

static GlobalSym gsyms[MAX_SYMBOLS];
static int       ngsyms;

/* Merged section buffers */
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

/* Virtual addresses (computed after merging) */
static uint64_t text_vaddr;
static uint64_t rodata_vaddr;
static uint64_t data_vaddr;
static uint64_t bss_vaddr;

/* ============================================================================
 * Helpers
 * ============================================================================ */

static void die(const char *msg) {
    fprintf(stderr, "ld-x64: %s\n", msg);
    exit(1);
}

static void dief(const char *fmt, const char *arg) {
    fprintf(stderr, "ld-x64: ");
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
    fread(buf, 1, sz, f);
    fclose(f);
    *out_size = sz;
    return buf;
}

static int gsym_find(const char *name) {
    for (int i = 0; i < ngsyms; i++) {
        if (strcmp(gsyms[i].name, name) == 0)
            return i;
    }
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

/* Classify a section by name → merged section type */
static int classify_section(const char *name) {
    if (strcmp(name, ".text") == 0) return 1;
    if (strcmp(name, ".rodata") == 0) return 2;
    if (strcmp(name, ".data") == 0) return 3;
    if (strcmp(name, ".bss") == 0) return 4;
    return 0; /* not a mergeable section */
}

static void ensure_cap(uint8_t **buf, uint64_t *cap, uint64_t needed) {
    if (needed <= *cap) return;
    uint64_t newcap = *cap ? *cap * 2 : 65536;
    while (newcap < needed) newcap *= 2;
    *buf = realloc(*buf, newcap);
    if (!*buf) die("out of memory");
    *cap = newcap;
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

    /* Validate ELF header */
    if (sz < sizeof(Elf64_Ehdr)) dief("too small: %s", path);
    obj->ehdr = (Elf64_Ehdr *)data;

    if (obj->ehdr->e_ident[0] != 0x7F || obj->ehdr->e_ident[1] != 'E' ||
        obj->ehdr->e_ident[2] != 'L' || obj->ehdr->e_ident[3] != 'F')
        dief("not an ELF file: %s", path);

    if (obj->ehdr->e_ident[4] != 2) dief("not ELF64: %s", path);
    if (obj->ehdr->e_type != ET_REL) dief("not relocatable: %s", path);
    if (obj->ehdr->e_machine != EM_X86_64) dief("not x86-64: %s", path);

    /* Section headers */
    obj->shdrs = (Elf64_Shdr *)(data + obj->ehdr->e_shoff);
    obj->nshdr = obj->ehdr->e_shnum;

    /* Section header string table */
    if (obj->ehdr->e_shstrndx < obj->nshdr) {
        Elf64_Shdr *shstr = &obj->shdrs[obj->ehdr->e_shstrndx];
        obj->shstrtab = (char *)(data + shstr->sh_offset);
    }

    /* Find .symtab and .strtab */
    obj->symtab = NULL;
    obj->nsyms = 0;
    obj->strtab = NULL;

    for (int i = 0; i < obj->nshdr; i++) {
        if (obj->shdrs[i].sh_type == SHT_SYMTAB) {
            obj->symtab = (Elf64_Sym *)(data + obj->shdrs[i].sh_offset);
            obj->nsyms = obj->shdrs[i].sh_size / sizeof(Elf64_Sym);
            /* strtab is linked via sh_link */
            int stridx = obj->shdrs[i].sh_link;
            if (stridx < obj->nshdr)
                obj->strtab = (char *)(data + obj->shdrs[stridx].sh_offset);
        }
    }

    /* Classify sections */
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

            switch (cls) {
            case 1: { /* .text */
                /* Align */
                while (merged_text_len & (align - 1)) {
                    ensure_cap(&merged_text, &merged_text_cap, merged_text_len + 1);
                    merged_text[merged_text_len++] = 0xCC; /* INT3 padding */
                }
                obj->text_base = merged_text_len;
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
                obj->rodata_base = merged_rodata_len;
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
                obj->data_base = merged_data_len;
                if (sh->sh_size > 0) {
                    ensure_cap(&merged_data, &merged_data_cap, merged_data_len + sh->sh_size);
                    memcpy(merged_data + merged_data_len,
                           obj->data + sh->sh_offset, sh->sh_size);
                    merged_data_len += sh->sh_size;
                }
                break;
            }
            case 4: { /* .bss */
                while (merged_bss_len & (align - 1))
                    merged_bss_len++;
                obj->bss_base = merged_bss_len;
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
    /* .text at first page after headers */
    text_vaddr = ELF_BASE + PAGE_SIZE;

    /* .rodata at next page after .text */
    rodata_vaddr = text_vaddr + PAGE_ALIGN(merged_text_len);

    /* .data at next page after .rodata */
    if (merged_rodata_len > 0)
        data_vaddr = rodata_vaddr + PAGE_ALIGN(merged_rodata_len);
    else
        data_vaddr = rodata_vaddr;

    /* .bss immediately after .data */
    bss_vaddr = data_vaddr + merged_data_len;
}

/* ============================================================================
 * Resolve symbol for a given object's local symbol index
 * Returns the virtual address of the symbol.
 * ============================================================================ */

static uint64_t resolve_local_sym(Object *obj, int sym_idx) {
    Elf64_Sym *sym = &obj->symtab[sym_idx];
    int bind = ELF64_ST_BIND(sym->st_info);
    int type = ELF64_ST_TYPE(sym->st_info);
    const char *name = obj->strtab + sym->st_name;

    /* Section symbols: value is offset within section */
    if (type == STT_SECTION) {
        int shndx = sym->st_shndx;
        int cls = obj->sec_map[shndx];
        switch (cls) {
        case 1: return text_vaddr + obj->text_base;
        case 2: return rodata_vaddr + obj->rodata_base;
        case 3: return data_vaddr + obj->data_base;
        case 4: return bss_vaddr + obj->bss_base;
        default: return 0;
        }
    }

    /* Undefined symbol → look up in global table */
    if (sym->st_shndx == SHN_UNDEF) {
        int gi = gsym_find(name);
        if (gi < 0 || !gsyms[gi].defined) {
            fprintf(stderr, "ld-x64: undefined symbol: %s\n", name);
            exit(1);
        }
        return gsyms[gi].value;
    }

    /* Defined symbol: section-relative value + merged base + vaddr */
    int cls = obj->sec_map[sym->st_shndx];
    switch (cls) {
    case 1: return text_vaddr + obj->text_base + sym->st_value;
    case 2: return rodata_vaddr + obj->rodata_base + sym->st_value;
    case 3: return data_vaddr + obj->data_base + sym->st_value;
    case 4: return bss_vaddr + obj->bss_base + sym->st_value;
    default:
        /* Global symbols in non-mergeable sections — look up by name */
        if (bind == STB_GLOBAL && sym->st_name != 0) {
            int gi = gsym_find(name);
            if (gi >= 0 && gsyms[gi].defined)
                return gsyms[gi].value;
        }
        return sym->st_value;
    }
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
                /* Undefined reference — add if not known */
                if (gi < 0) gsym_add(name);
                continue;
            }

            /* Defined symbol */
            int cls = obj->sec_map[sym->st_shndx];
            uint64_t val = sym->st_value;

            switch (cls) {
            case 1: val += text_vaddr + obj->text_base; break;
            case 2: val += rodata_vaddr + obj->rodata_base; break;
            case 3: val += data_vaddr + obj->data_base; break;
            case 4: val += bss_vaddr + obj->bss_base; break;
            }

            if (gi < 0) {
                gi = gsym_add(name);
            } else if (gsyms[gi].defined && gsyms[gi].binding != STB_WEAK) {
                if (bind != STB_WEAK) {
                    fprintf(stderr, "ld-x64: duplicate symbol: %s\n", name);
                    exit(1);
                }
                continue; /* weak doesn't override strong */
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
 * Apply relocations
 * ============================================================================ */

static void apply_relocations(void) {
    for (int oi = 0; oi < nobjects; oi++) {
        Object *obj = &objects[oi];

        for (int si = 0; si < obj->nshdr; si++) {
            Elf64_Shdr *sh = &obj->shdrs[si];
            if (sh->sh_type != SHT_RELA) continue;

            /* Which section does this rela apply to? */
            int target_sec = sh->sh_info;
            int target_cls = obj->sec_map[target_sec];
            if (target_cls == 0) continue;

            /* Get the merged buffer and base for the target section */
            uint8_t *buf;
            uint64_t buf_base_off;
            uint64_t buf_vaddr;

            switch (target_cls) {
            case 1:
                buf = merged_text;
                buf_base_off = obj->text_base;
                buf_vaddr = text_vaddr;
                break;
            case 2:
                buf = merged_rodata;
                buf_base_off = obj->rodata_base;
                buf_vaddr = rodata_vaddr;
                break;
            case 3:
                buf = merged_data;
                buf_base_off = obj->data_base;
                buf_vaddr = data_vaddr;
                break;
            default:
                continue; /* can't relocate into BSS */
            }

            int nrela = sh->sh_size / sizeof(Elf64_Rela);
            Elf64_Rela *relas = (Elf64_Rela *)(obj->data + sh->sh_offset);

            for (int ri = 0; ri < nrela; ri++) {
                Elf64_Rela *r = &relas[ri];
                uint32_t sym_idx = ELF64_R_SYM(r->r_info);
                uint32_t rtype = ELF64_R_TYPE(r->r_info);

                /* Resolve symbol */
                uint64_t S = resolve_local_sym(obj, sym_idx);
                int64_t A = r->r_addend;
                uint64_t P = buf_vaddr + buf_base_off + r->r_offset;

                /* Patch location in merged buffer */
                uint8_t *patch = buf + buf_base_off + r->r_offset;

                switch (rtype) {
                case R_X86_64_64: {
                    /* S + A → 64-bit absolute */
                    uint64_t val = S + A;
                    memcpy(patch, &val, 8);
                    break;
                }
                case R_X86_64_PC32: {
                    /* S + A - P → 32-bit PC-relative */
                    int64_t val = (int64_t)(S + A) - (int64_t)P;
                    if (val < -2147483648LL || val > 2147483647LL) {
                        fprintf(stderr, "ld-x64: R_X86_64_PC32 overflow for symbol at 0x%lx\n",
                                (unsigned long)S);
                        exit(1);
                    }
                    int32_t v32 = (int32_t)val;
                    memcpy(patch, &v32, 4);
                    break;
                }
                case R_X86_64_32S: {
                    /* S + A → 32-bit signed absolute */
                    int64_t val = (int64_t)(S + A);
                    if (val < -2147483648LL || val > 2147483647LL) {
                        fprintf(stderr, "ld-x64: R_X86_64_32S overflow\n");
                        exit(1);
                    }
                    int32_t v32 = (int32_t)val;
                    memcpy(patch, &v32, 4);
                    break;
                }
                case R_X86_64_NONE:
                    break;
                default:
                    fprintf(stderr, "ld-x64: unsupported relocation type %u\n", rtype);
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
    /* Find entry point */
    int gi = gsym_find(entry_name);
    if (gi < 0 || !gsyms[gi].defined) {
        fprintf(stderr, "ld-x64: entry point '%s' not found\n", entry_name);
        exit(1);
    }
    uint64_t entry = gsyms[gi].value;

    /* How many PT_LOAD segments? */
    int nphdr = 1; /* .text always */
    if (merged_rodata_len > 0) nphdr++;
    if (merged_data_len > 0 || merged_bss_len > 0) nphdr++;

    /* File layout */
    uint64_t ehdr_size = sizeof(Elf64_Ehdr);
    uint64_t phdr_size = nphdr * sizeof(Elf64_Phdr);
    uint64_t headers_size = ehdr_size + phdr_size;

    uint64_t text_foff = PAGE_SIZE; /* file offset for .text */
    uint64_t rodata_foff = text_foff + PAGE_ALIGN(merged_text_len);
    uint64_t data_foff;

    if (merged_rodata_len > 0)
        data_foff = rodata_foff + PAGE_ALIGN(merged_rodata_len);
    else
        data_foff = rodata_foff;

    uint64_t file_size = data_foff + merged_data_len;

    /* Build file */
    uint8_t *out = calloc(1, file_size > PAGE_SIZE ? file_size : PAGE_SIZE);
    if (!out) die("out of memory for output");

    /* ELF header */
    Elf64_Ehdr *ehdr = (Elf64_Ehdr *)out;
    ehdr->e_ident[0] = 0x7F;
    ehdr->e_ident[1] = 'E';
    ehdr->e_ident[2] = 'L';
    ehdr->e_ident[3] = 'F';
    ehdr->e_ident[4] = 2;  /* ELFCLASS64 */
    ehdr->e_ident[5] = 1;  /* ELFDATA2LSB */
    ehdr->e_ident[6] = 1;  /* EV_CURRENT */
    ehdr->e_type = ET_EXEC;
    ehdr->e_machine = EM_X86_64;
    ehdr->e_version = 1;
    ehdr->e_entry = entry;
    ehdr->e_phoff = ehdr_size;
    ehdr->e_shoff = 0; /* no section headers in executable */
    ehdr->e_ehsize = sizeof(Elf64_Ehdr);
    ehdr->e_phentsize = sizeof(Elf64_Phdr);
    ehdr->e_phnum = nphdr;

    /* Program headers */
    Elf64_Phdr *phdrs = (Elf64_Phdr *)(out + ehdr_size);
    int phi = 0;

    /* .text (R+X) */
    phdrs[phi].p_type = PT_LOAD;
    phdrs[phi].p_flags = PF_R | PF_X;
    phdrs[phi].p_offset = text_foff;
    phdrs[phi].p_vaddr = text_vaddr;
    phdrs[phi].p_paddr = text_vaddr;
    phdrs[phi].p_filesz = merged_text_len;
    phdrs[phi].p_memsz = merged_text_len;
    phdrs[phi].p_align = PAGE_SIZE;
    phi++;

    /* .rodata (R) */
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

    /* .data + .bss (R+W) */
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

    /* Copy section data */
    if (merged_text_len > 0)
        memcpy(out + text_foff, merged_text, merged_text_len);
    if (merged_rodata_len > 0)
        memcpy(out + rodata_foff, merged_rodata, merged_rodata_len);
    if (merged_data_len > 0)
        memcpy(out + data_foff, merged_data, merged_data_len);

    /* Write file */
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
 * Main
 * ============================================================================ */

int main(int argc, char **argv) {
    const char *outfile = "a.out";
    const char *entry = "_start";
    int i;

    /* Parse arguments */
    i = 1;
    while (i < argc) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            outfile = argv[++i];
        } else if (strcmp(argv[i], "-e") == 0 && i + 1 < argc) {
            entry = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "ld-x64: unknown option: %s\n", argv[i]);
            return 1;
        } else {
            load_object(argv[i]);
        }
        i++;
    }

    if (nobjects == 0) {
        fprintf(stderr, "Usage: ld-x64 [-o output] [-e entry] obj1.o obj2.o ...\n");
        return 1;
    }

    /* Phase 1: Merge sections */
    merge_sections();

    /* Phase 2: Compute layout */
    compute_layout();

    /* Phase 3: Collect and resolve symbols */
    collect_symbols();

    /* Check for undefined symbols */
    {
        int undef = 0;
        for (int j = 0; j < ngsyms; j++) {
            if (!gsyms[j].defined) {
                fprintf(stderr, "ld-x64: undefined symbol: %s\n", gsyms[j].name);
                undef++;
            }
        }
        if (undef) return 1;
    }

    /* Phase 4: Apply relocations */
    apply_relocations();

    /* Phase 5: Write executable */
    write_executable(outfile, entry);

    fprintf(stderr, "ld-x64: wrote %s (text=%lu rodata=%lu data=%lu bss=%lu)\n",
            outfile, merged_text_len, merged_rodata_len,
            merged_data_len, merged_bss_len);

    return 0;
}
