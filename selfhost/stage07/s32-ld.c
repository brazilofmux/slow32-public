/*
 * Stage07 bootstrap linker — full-featured version
 *
 * Supports:
 *   - Multiple .s32o input files with section merging
 *   - .s32a archive with iterative pull loop
 *   - All relocation types (REL_32, HI20, LO12, BRANCH, JAL, CALL, PCREL, ABS32)
 *   - Linker-defined symbols (__bss_start, __mmio_base, __heap_start, etc.)
 *   - Expression resolution for __mmio_base+N patterns
 *   - --mmio SIZE for MMIO region
 *   - Page-aligned section layout
 *   - Backward compat: old-style positional args (input [aux] output)
 *
 * No #include — inlined prototypes to avoid INC-BUF-SZ overflow.
 */

/* === Inlined types === */
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef int int32_t;
typedef unsigned int size_t;
typedef struct _FILE FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

#define NULL ((void *)0)
#define EOF (-1)
#define SEEK_SET 0
#define SEEK_END 2

FILE *fopen(const char *path, const char *mode);
int fclose(FILE *fp);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *fp);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *fp);
int fseek(FILE *fp, long offset, int whence);
long ftell(FILE *fp);
int fputc(int c, FILE *fp);
int fputs(const char *s, FILE *fp);
void fput_uint(FILE *fp, unsigned int val);

size_t strlen(const char *s);
int strcmp(const char *s1, const char *s2);
void *memcpy(void *dst, const void *src, size_t n);
void *memset(void *s, int c, size_t n);

/* === Format constants === */
#define S32O_MAGIC  0x5333324FU
#define S32X_MAGIC  0x53333258U
#define S32A_MAGIC  0x53333241U

#define SEC_CODE   1
#define SEC_DATA   2
#define SEC_BSS    3
#define SEC_RODATA 4

#define SF_XRA   0x0D
#define SF_WRA   0x0E
#define SF_RA    0x0C

#define REL_32         1
#define REL_HI20       2
#define REL_LO12       3
#define REL_BRANCH     4
#define REL_JAL        5
#define REL_CALL       6
#define REL_PCREL_HI20 7
#define REL_PCREL_LO12 8
#define REL_ABS32      32

#define BIND_LOCAL  0
#define BIND_GLOBAL 1
#define BIND_WEAK   2

/* Instruction encoding */
#define MASK5   0x1FU
#define MASK6   0x3FU
#define MASK7   0x7FU
#define MASK8   0xFFU
#define MASK10  0x3FFU
#define MASK12  0xFFFU
#define MASK20  0xFFFFFU
#define HALF12  0x800U
#define OP_STB  0x38U
#define OP_STH  0x39U
#define OP_STW  0x3AU
#define S_MASK  0xFE000F80U
#define B_MASK  0x01FFF07FU

/* Binary layout sizes */
#define S32X_HDR_SZ  64
#define S32X_SEC_SZ  28
#define S32O_SEC_SZ  32
#define S32O_SYM_SZ  16
#define S32O_REL_SZ  16

#define S32_MACHINE      0x32U
#define S32_ENDIAN       0x01U
#define S32X_FLAG_WXORX  0x01U
#define S32X_FLAG_MMIO   0x80U

/* Memory layout */
#define STACK_BASE     0x0FFFFFF0U
#define STACK_SIZE     262144U
#define MEM_SIZE       0x10000000U
#define HEAP_GAP       0x800000U
#define CODE_LIMIT_MIN 0x100000U
#define PAGE_MASK      0xFFFFF000U

/* Buffer sizes */
#define MAX_GSYM      1024
#define MAX_GREL      8192
#define GSYM_NBUF_SZ  32768
#define TEXT_BUFSZ    262144
#define DATA_BUFSZ    131072
#define RODATA_BUFSZ  65536
#define FILE_BUFSZ    262144
#define MAX_FILE_SYM  512
#define MAX_MEMBERS   512
#define OUT_STRTAB_SZ 8192
#define AR_SYMTAB_SZ  16384
#define AR_MEMTAB_SZ  12288
#define AR_STRTAB_SZ  16384

/* === Merged section buffers === */
static uint8_t text_buf[TEXT_BUFSZ];
static uint8_t data_buf[DATA_BUFSZ];
static uint8_t rodata_buf[RODATA_BUFSZ];
static uint32_t text_sz;
static uint32_t data_sz;
static uint32_t rodata_sz;
static uint32_t bss_sz;

/* === Global symbol table (parallel arrays) === */
static uint32_t gsym_noff[MAX_GSYM];
static uint32_t gsym_nlen[MAX_GSYM];
static uint32_t gsym_val[MAX_GSYM];
static uint32_t gsym_sec[MAX_GSYM];
static uint32_t gsym_bind[MAX_GSYM];
static uint32_t gsym_def[MAX_GSYM];
static char gsym_nbuf[GSYM_NBUF_SZ];
static uint32_t gsym_cnt;
static uint32_t gsym_nptr;

/* === Global relocation table (parallel arrays) === */
static uint32_t grel_off[MAX_GREL];
static uint32_t grel_sec[MAX_GREL];
static uint32_t grel_typ[MAX_GREL];
static int32_t grel_add[MAX_GREL];
static uint32_t grel_sym[MAX_GREL];
static uint32_t grel_cnt;

/* === Layout === */
static uint32_t text_va;
static uint32_t rodata_va;
static uint32_t data_va;
static uint32_t bss_va;
static uint32_t bss_end_va;
static uint32_t heap_va;
static uint32_t mmio_va;
static uint32_t mmio_size_arg;
static uint32_t entry_pt;

/* === File I/O buffer === */
static uint8_t file_buf[FILE_BUFSZ];

/* === Per-file state === */
static uint32_t sec_base[8];
static uint32_t sym_map[MAX_FILE_SYM];
static uint32_t obj_nsec;
static uint32_t obj_nsym;
static uint32_t obj_sec_off;
static uint32_t obj_sym_off;
static uint32_t obj_str_off;

/* === Archive state === */
static uint8_t ar_symtab[AR_SYMTAB_SZ];
static uint8_t ar_memtab[AR_MEMTAB_SZ];
static uint8_t ar_strtab[AR_STRTAB_SZ];
static uint8_t mem_loaded[MAX_MEMBERS];
static uint32_t ar_nmembers;
static uint32_t ar_nsymbols;
static uint32_t ar_str_sz;

/* === Output state === */
static char out_strtab[OUT_STRTAB_SZ];
static uint32_t out_strtab_sz;
static uint8_t wb_buf[64];

/* Error flag */
static int link_error;

/* Temp buffer for gsym name extraction */
static char gsym_name_tmp[256];
/* Temp buffer for expression base name */
static char expr_base_buf[256];

/* === Helper functions === */

static uint32_t rd32(const uint8_t *buf, uint32_t off) {
    return (uint32_t)buf[off]
         | ((uint32_t)buf[off + 1] << 8)
         | ((uint32_t)buf[off + 2] << 16)
         | ((uint32_t)buf[off + 3] << 24);
}

static uint32_t rd16(const uint8_t *buf, uint32_t off) {
    return (uint32_t)buf[off] | ((uint32_t)buf[off + 1] << 8);
}

static uint32_t rd8(const uint8_t *buf, uint32_t off) {
    return (uint32_t)buf[off];
}

static void wr32(uint8_t *buf, uint32_t off, uint32_t val) {
    buf[off]     = (uint8_t)(val & 0xFFU);
    buf[off + 1] = (uint8_t)((val >> 8) & 0xFFU);
    buf[off + 2] = (uint8_t)((val >> 16) & 0xFFU);
    buf[off + 3] = (uint8_t)((val >> 24) & 0xFFU);
}

static void wr16(uint8_t *buf, uint32_t off, uint32_t val) {
    buf[off]     = (uint8_t)(val & 0xFFU);
    buf[off + 1] = (uint8_t)((val >> 8) & 0xFFU);
}

static uint32_t align4(uint32_t n) { return (n + 3U) & ~3U; }
static uint32_t align16(uint32_t n) { return (n + 15U) & ~15U; }
static uint32_t page_align(uint32_t n) { return (n + 4095U) & PAGE_MASK; }

static int str_eq(const char *a, const char *b) {
    return strcmp(a, b) == 0;
}

/* String length in file_buf starting at offset */
static uint32_t fbuf_strlen(uint32_t off) {
    uint32_t len;
    len = 0;
    while (file_buf[off + len] != 0) {
        len = len + 1;
    }
    return len;
}

/* Compare NUL-terminated name with gsym entry */
static int name_eq_gsym(const char *name, uint32_t idx) {
    uint32_t noff;
    uint32_t nlen;
    uint32_t i;
    noff = gsym_noff[idx];
    nlen = gsym_nlen[idx];
    for (i = 0; i < nlen; i = i + 1) {
        if (name[i] == 0) return 0;
        if (name[i] != gsym_nbuf[noff + i]) return 0;
    }
    if (name[nlen] != 0) return 0;
    return 1;
}

/* === Output string table === */

static void ostrtab_init(void) {
    out_strtab[0] = 0;
    out_strtab_sz = 1;
}

static uint32_t ostrtab_add(const char *s, uint32_t slen) {
    uint32_t off;
    uint32_t i;
    off = out_strtab_sz;
    if (off + slen + 1 > OUT_STRTAB_SZ) return off;
    for (i = 0; i < slen; i = i + 1) {
        out_strtab[off + i] = s[i];
    }
    out_strtab[off + slen] = 0;
    out_strtab_sz = off + slen + 1;
    return off;
}

/* === Symbol table === */

static int gsym_find(const char *name) {
    uint32_t i;
    for (i = 0; i < gsym_cnt; i = i + 1) {
        if (name_eq_gsym(name, i)) return (int)i;
    }
    return -1;
}

static uint32_t gsym_add_name(const char *name, uint32_t len) {
    uint32_t off;
    uint32_t i;
    off = gsym_nptr;
    if (off + len > GSYM_NBUF_SZ) {
        fputs("error: gsym name buffer overflow\n", stderr);
        link_error = 1;
        return 0;
    }
    for (i = 0; i < len; i = i + 1) {
        gsym_nbuf[off + i] = name[i];
    }
    gsym_nptr = off + len;
    return off;
}

static int gsym_add_new(const char *name, uint32_t sec, uint32_t val,
                        uint32_t bind, uint32_t def) {
    uint32_t idx;
    uint32_t nlen;
    uint32_t noff;
    if (gsym_cnt >= MAX_GSYM) {
        fputs("error: gsym table full\n", stderr);
        link_error = 1;
        return -1;
    }
    idx = gsym_cnt;
    nlen = strlen(name);
    noff = gsym_add_name(name, nlen);
    gsym_noff[idx] = noff;
    gsym_nlen[idx] = nlen;
    gsym_sec[idx] = sec;
    gsym_val[idx] = val;
    gsym_bind[idx] = bind;
    gsym_def[idx] = def;
    gsym_cnt = gsym_cnt + 1;
    return (int)idx;
}

static int gsym_upsert(const char *name, uint32_t sec, uint32_t val,
                       uint32_t bind) {
    int idx;
    uint32_t def;
    idx = gsym_find(name);
    if (idx < 0) {
        def = (sec != 0) ? 1 : 0;
        return gsym_add_new(name, sec, val, bind, def);
    }
    /* Found — update if currently undefined and new is defined */
    if (sec != 0 && gsym_def[idx] == 0) {
        gsym_sec[idx] = sec;
        gsym_val[idx] = val;
        gsym_bind[idx] = bind;
        gsym_def[idx] = 1;
    }
    return idx;
}

static int gsym_add_undef(const char *name) {
    return gsym_upsert(name, 0, 0, BIND_GLOBAL);
}

/* === Section helpers === */

static uint8_t *sec_buf(uint32_t sec_type) {
    if (sec_type == SEC_CODE) return text_buf;
    if (sec_type == SEC_DATA) return data_buf;
    if (sec_type == SEC_RODATA) return rodata_buf;
    return NULL;
}

static uint32_t sec_cap(uint32_t sec_type) {
    if (sec_type == SEC_CODE) return TEXT_BUFSZ;
    if (sec_type == SEC_DATA) return DATA_BUFSZ;
    if (sec_type == SEC_RODATA) return RODATA_BUFSZ;
    return 0;
}

static uint32_t sec_sz_get(uint32_t sec_type) {
    if (sec_type == SEC_CODE) return text_sz;
    if (sec_type == SEC_DATA) return data_sz;
    if (sec_type == SEC_BSS) return bss_sz;
    if (sec_type == SEC_RODATA) return rodata_sz;
    return 0;
}

static void sec_sz_set(uint32_t sec_type, uint32_t val) {
    if (sec_type == SEC_CODE) text_sz = val;
    if (sec_type == SEC_DATA) data_sz = val;
    if (sec_type == SEC_BSS) bss_sz = val;
    if (sec_type == SEC_RODATA) rodata_sz = val;
}

static uint32_t sec_va_get(uint32_t sec_type) {
    if (sec_type == SEC_CODE) return text_va;
    if (sec_type == SEC_DATA) return data_va;
    if (sec_type == SEC_BSS) return bss_va;
    if (sec_type == SEC_RODATA) return rodata_va;
    return 0;
}

/* === Parse .s32o header from file_buf === */

static int parse_obj_header(void) {
    uint32_t magic;
    magic = rd32(file_buf, 0);
    if (magic != S32O_MAGIC) {
        fputs("error: bad .s32o magic\n", stderr);
        return 0;
    }
    obj_nsec    = rd32(file_buf, 12);
    obj_sec_off = rd32(file_buf, 16);
    obj_nsym    = rd32(file_buf, 20);
    obj_sym_off = rd32(file_buf, 24);
    obj_str_off = rd32(file_buf, 28);
    return 1;
}

/* === Merge sections from file_buf === */

static void merge_sections(void) {
    uint32_t i;
    uint32_t sec_off;
    uint32_t stype;
    uint32_t ssize;
    uint32_t sfileoff;
    uint32_t base;
    uint32_t cur_sz;
    uint8_t *dst;
    uint8_t *src;
    uint32_t j;

    for (i = 0; i < 8; i = i + 1) sec_base[i] = 0;

    for (i = 0; i < obj_nsec; i = i + 1) {
        sec_off  = i * S32O_SEC_SZ + obj_sec_off;
        stype    = rd32(file_buf, sec_off + 4);
        ssize    = rd32(file_buf, sec_off + 12);
        sfileoff = rd32(file_buf, sec_off + 16);

        if (stype != SEC_CODE && stype != SEC_DATA &&
            stype != SEC_BSS && stype != SEC_RODATA) continue;

        cur_sz = sec_sz_get(stype);
        base = (cur_sz > 0) ? align4(cur_sz) : 0;
        sec_base[stype] = base;

        if (stype != SEC_BSS && ssize > 0 && sfileoff > 0) {
            if (base + ssize > sec_cap(stype)) {
                fputs("error: section buffer overflow\n", stderr);
                link_error = 1;
                return;
            }
            dst = sec_buf(stype) + base;
            src = file_buf + sfileoff;
            for (j = 0; j < ssize; j = j + 1) dst[j] = src[j];
        }

        sec_sz_set(stype, base + ssize);
    }
}

/* === Merge symbols from file_buf === */

static void merge_symbols(void) {
    uint32_t i;
    uint32_t ent_off;
    uint32_t sy_noff;
    uint32_t sy_val;
    uint32_t sy_sec;
    uint32_t sy_bind;
    uint32_t sy_sectype;
    const char *name;
    int gidx;

    if (obj_nsym > MAX_FILE_SYM) {
        fputs("error: object symbol table too large\n", stderr);
        link_error = 1;
        return;
    }

    for (i = 0; i < obj_nsym; i = i + 1) {
        ent_off = i * S32O_SYM_SZ + obj_sym_off;
        sy_noff = rd32(file_buf, ent_off);
        sy_val  = rd32(file_buf, ent_off + 4);
        sy_sec  = rd16(file_buf, ent_off + 8);
        sy_bind = rd8(file_buf, ent_off + 11);
        name = (const char *)(file_buf + obj_str_off + sy_noff);

        if (sy_sec == 0) {
            if (sy_bind == BIND_LOCAL) {
                gidx = -1;
            } else {
                gidx = gsym_add_undef(name);
            }
        } else {
            sy_sectype = rd32(file_buf, (sy_sec - 1) * S32O_SEC_SZ + obj_sec_off + 4);
            sy_val = sec_base[sy_sectype] + sy_val;
            if (sy_bind == BIND_LOCAL) {
                gidx = gsym_add_new(name, sy_sectype, sy_val, sy_bind, 1);
            } else {
                gidx = gsym_upsert(name, sy_sectype, sy_val, sy_bind);
            }
        }

        sym_map[i] = (uint32_t)gidx;
    }
}

/* === Merge relocations from file_buf === */

static void merge_relocs(void) {
    uint32_t i;
    uint32_t sec_off;
    uint32_t stype;
    uint32_t nrelocs;
    uint32_t reloc_off;
    uint32_t r;
    uint32_t roff;
    uint32_t rl_off;
    uint32_t rl_sym;
    uint32_t rl_typ;
    int32_t rl_add;

    for (i = 0; i < obj_nsec; i = i + 1) {
        sec_off   = i * S32O_SEC_SZ + obj_sec_off;
        stype     = rd32(file_buf, sec_off + 4);
        nrelocs   = rd32(file_buf, sec_off + 24);
        reloc_off = rd32(file_buf, sec_off + 28);

        for (r = 0; r < nrelocs; r = r + 1) {
            roff   = reloc_off + r * S32O_REL_SZ;
            rl_off = rd32(file_buf, roff);
            rl_sym = rd32(file_buf, roff + 4);
            rl_typ = rd32(file_buf, roff + 8);
            rl_add = (int32_t)rd32(file_buf, roff + 12);

            if (grel_cnt < MAX_GREL) {
                grel_off[grel_cnt] = rl_off + sec_base[stype];
                grel_sec[grel_cnt] = stype;
                grel_typ[grel_cnt] = rl_typ;
                grel_add[grel_cnt] = rl_add;
                grel_sym[grel_cnt] = (rl_sym < MAX_FILE_SYM) ? sym_map[rl_sym] : 0xFFFFFFFFU;
                grel_cnt = grel_cnt + 1;
            }
        }
    }
}

/* === Read file into file_buf === */

static int read_file_buf(const char *path) {
    FILE *f;
    long sz;
    size_t nr;

    f = fopen(path, "rb");
    if (!f) {
        fputs("error: cannot open ", stderr);
        fputs(path, stderr);
        fputc('\n', stderr);
        return -1;
    }
    fseek(f, 0, SEEK_END);
    sz = ftell(f);
    if (sz > FILE_BUFSZ) {
        fputs("error: file too large: ", stderr);
        fputs(path, stderr);
        fputc('\n', stderr);
        fclose(f);
        return -1;
    }
    fseek(f, 0, SEEK_SET);
    if (sz > 0) {
        nr = fread(file_buf, 1, (size_t)sz, f);
        if (nr != (size_t)sz) {
            fputs("error: short read: ", stderr);
            fputs(path, stderr);
            fputc('\n', stderr);
            fclose(f);
            return -1;
        }
    }
    fclose(f);
    return (int)sz;
}

/* === Link an object file === */

static void link_obj(const char *path) {
    int sz;
    uint32_t i;

    sz = read_file_buf(path);
    if (sz < 0) { link_error = 1; return; }

    if (!parse_obj_header()) { link_error = 1; return; }

    for (i = 0; i < MAX_FILE_SYM; i = i + 1) sym_map[i] = 0;

    merge_sections();
    if (link_error) return;
    merge_symbols();
    if (link_error) return;
    merge_relocs();
}

/* === Archive loading === */

static uint32_t ar_sym_name_off(uint32_t idx) {
    return rd32(ar_symtab, idx * 8);
}
static uint32_t ar_sym_member(uint32_t idx) {
    return rd32(ar_symtab, idx * 8 + 4);
}
static uint32_t ar_mem_offset(uint32_t idx) {
    return rd32(ar_memtab, idx * 24 + 4);
}
static uint32_t ar_mem_size(uint32_t idx) {
    return rd32(ar_memtab, idx * 24 + 8);
}

/* Check if symbol name is undefined in gsym table */
static int sym_undefined(const char *name) {
    int idx;
    idx = gsym_find(name);
    if (idx < 0) return 0;
    return gsym_def[idx] == 0;
}

/* Load archive member from open file handle */
static void load_ar_member(FILE *ar_fh, uint32_t midx) {
    uint32_t mem_off;
    uint32_t mem_sz;
    uint32_t i;

    if (midx >= ar_nmembers) return;
    if (mem_loaded[midx]) return;
    mem_loaded[midx] = 1;

    mem_sz = ar_mem_size(midx);
    mem_off = ar_mem_offset(midx);

    if (mem_sz > FILE_BUFSZ) {
        fputs("error: archive member too large\n", stderr);
        link_error = 1;
        return;
    }

    fseek(ar_fh, (long)mem_off, SEEK_SET);
    if (fread(file_buf, 1, mem_sz, ar_fh) != mem_sz) {
        fputs("error: short read on archive member\n", stderr);
        link_error = 1;
        return;
    }

    if (!parse_obj_header()) { link_error = 1; return; }

    for (i = 0; i < MAX_FILE_SYM; i = i + 1) sym_map[i] = 0;

    merge_sections();
    if (link_error) return;
    merge_symbols();
    if (link_error) return;
    merge_relocs();
}

static void link_archive(const char *path) {
    FILE *ar_fh;
    uint32_t hdr_buf_sz;
    uint32_t ar_mem_off;
    uint32_t ar_sym_off;
    uint32_t ar_str_off;
    int added_any;
    uint32_t i;
    uint32_t midx;
    uint32_t name_off;
    const char *name;

    ar_fh = fopen(path, "rb");
    if (!ar_fh) {
        fputs("error: cannot open archive: ", stderr);
        fputs(path, stderr);
        fputc('\n', stderr);
        link_error = 1;
        return;
    }

    /* Read 32-byte archive header */
    if (fread(file_buf, 1, 32, ar_fh) != 32) {
        fputs("error: short archive header\n", stderr);
        fclose(ar_fh);
        link_error = 1;
        return;
    }

    if (rd32(file_buf, 0) != S32A_MAGIC) {
        fputs("error: bad .s32a magic\n", stderr);
        fclose(ar_fh);
        link_error = 1;
        return;
    }

    ar_nmembers  = rd32(file_buf, 8);
    ar_mem_off   = rd32(file_buf, 12);
    ar_nsymbols  = rd32(file_buf, 16);
    ar_sym_off   = rd32(file_buf, 20);
    ar_str_off   = rd32(file_buf, 24);
    ar_str_sz    = rd32(file_buf, 28);

    if (ar_nmembers > MAX_MEMBERS ||
        ar_nsymbols * 8 > AR_SYMTAB_SZ ||
        ar_nmembers * 24 > AR_MEMTAB_SZ ||
        ar_str_sz > AR_STRTAB_SZ) {
        fputs("error: archive tables too large\n", stderr);
        fclose(ar_fh);
        link_error = 1;
        return;
    }

    /* Read symbol index, member table, string table */
    fseek(ar_fh, (long)ar_sym_off, SEEK_SET);
    fread(ar_symtab, 1, ar_nsymbols * 8, ar_fh);
    fseek(ar_fh, (long)ar_mem_off, SEEK_SET);
    fread(ar_memtab, 1, ar_nmembers * 24, ar_fh);
    fseek(ar_fh, (long)ar_str_off, SEEK_SET);
    fread(ar_strtab, 1, ar_str_sz, ar_fh);

    for (i = 0; i < MAX_MEMBERS; i = i + 1) mem_loaded[i] = 0;

    /* Iterative pull loop */
    while (1) {
        added_any = 0;
        for (i = 0; i < ar_nsymbols; i = i + 1) {
            midx = ar_sym_member(i);
            if (midx < ar_nmembers && !mem_loaded[midx]) {
                name_off = ar_sym_name_off(i);
                if (name_off < ar_str_sz) {
                    name = (const char *)(ar_strtab + name_off);
                    if (sym_undefined(name)) {
                        load_ar_member(ar_fh, midx);
                        added_any = 1;
                    }
                }
            }
            if (link_error) break;
        }
        if (link_error) break;
        if (!added_any) break;
    }

    fclose(ar_fh);
}

/* === Section layout === */

static void layout_sections(void) {
    uint32_t t;
    text_va = 0;
    t = page_align(text_sz);
    if (t < CODE_LIMIT_MIN) t = CODE_LIMIT_MIN;
    rodata_va = t;
    t = page_align(rodata_va + rodata_sz);
    data_va = t;
    bss_va = data_va + data_sz;
    bss_end_va = bss_va + bss_sz;
    heap_va = page_align(bss_end_va);

    if (mmio_size_arg > 0) {
        mmio_va = page_align(heap_va + HEAP_GAP);
    } else {
        mmio_va = 0;
    }
}

/* === Symbol VA computation === */

static uint32_t gsym_va(uint32_t idx) {
    if (gsym_def[idx] == 2) return gsym_val[idx]; /* linker-defined: absolute */
    return sec_va_get(gsym_sec[idx]) + gsym_val[idx];
}

/* Get gsym name as NUL-terminated string */
static const char *gsym_name_str(uint32_t idx) {
    uint32_t noff;
    uint32_t nlen;
    uint32_t i;
    noff = gsym_noff[idx];
    nlen = gsym_nlen[idx];
    if (nlen > 255) nlen = 255;
    for (i = 0; i < nlen; i = i + 1) {
        gsym_name_tmp[i] = gsym_nbuf[noff + i];
    }
    gsym_name_tmp[nlen] = 0;
    return gsym_name_tmp;
}

/* === Expression resolution === */

static int parse_udec(const char *s) {
    int acc;
    int ch;
    if (*s == 0) return -1;
    acc = 0;
    while (*s) {
        ch = *s & 0xFF;
        if (ch < '0' || ch > '9') return -1;
        acc = acc * 10 + (ch - '0');
        s = s + 1;
    }
    return acc;
}

/* Resolve symbol VA for relocation (with expression resolution).
 * Returns 1 on success (VA written to *out_va), 0 on failure. */
static int gsym_va_rel(uint32_t idx, uint32_t *out_va) {
    const char *name;
    uint32_t i;
    int pos;
    int sign;
    int base_idx;
    int addend;
    const char *after;

    if (idx >= gsym_cnt) return 0;

    /* If defined, just return VA */
    if (gsym_def[idx] != 0) {
        *out_va = gsym_va(idx);
        return 1;
    }

    /* Undefined — try expression resolution */
    name = gsym_name_str(idx);

    /* Look for '+' or '-' */
    pos = -1;
    sign = 1;
    for (i = 1; name[i] != 0; i = i + 1) {
        if (name[i] == '+') { pos = (int)i; sign = 1; break; }
        if (name[i] == '-') { pos = (int)i; sign = -1; break; }
    }
    if (pos < 0) return 0;

    /* Split into base name and addend */
    for (i = 0; (int)i < pos && i < 255; i = i + 1) {
        expr_base_buf[i] = name[i];
    }
    expr_base_buf[i] = 0;

    after = name + pos + 1;
    addend = parse_udec(after);
    if (addend < 0) return 0;
    addend = addend * sign;

    base_idx = gsym_find(expr_base_buf);
    if (base_idx < 0) return 0;
    if (gsym_def[base_idx] == 0) return 0;

    *out_va = gsym_va((uint32_t)base_idx) + (uint32_t)addend;
    return 1;
}

/* === Inject linker-defined symbols === */

static void inject_sym(const char *name, uint32_t value) {
    int idx;
    idx = gsym_find(name);
    if (idx < 0) {
        gsym_add_new(name, SEC_CODE, value, BIND_GLOBAL, 2);
    } else if (gsym_def[idx] == 0) {
        gsym_val[idx] = value;
        gsym_def[idx] = 2;
    }
}

static void inject_linker_symbols(void) {
    uint32_t rodata_end;
    uint32_t heap_end;

    rodata_end = page_align(rodata_va + rodata_sz);

    inject_sym("__bss_start", bss_va);
    inject_sym("__bss_end", bss_end_va);
    inject_sym("__bss_start__", bss_va);
    inject_sym("__bss_end__", bss_end_va);
    inject_sym("_bss_start", bss_va);
    inject_sym("_bss_end", bss_end_va);

    inject_sym("__mmio_base", mmio_va);
    inject_sym("__mmio_end", mmio_va + mmio_size_arg);

    inject_sym("__heap_start", heap_va);
    if (mmio_size_arg > 0) {
        heap_end = mmio_va;
    } else {
        heap_end = STACK_BASE - STACK_SIZE;
    }
    inject_sym("__heap_end", heap_end);

    inject_sym("__stack_base", STACK_BASE);
    inject_sym("__stack_top", STACK_BASE);
    inject_sym("__stack_end", STACK_BASE - STACK_SIZE);
    inject_sym("__stack_bottom", STACK_BASE - STACK_SIZE);
    inject_sym("_stack", STACK_BASE);

    inject_sym("__code_start", 0);
    inject_sym("__code_end", page_align(text_sz));

    inject_sym("__rodata_start", rodata_va);
    inject_sym("__rodata_end", rodata_end);

    inject_sym("__data_start", data_va);
    inject_sym("__data_end", bss_va);
    inject_sym("__data_load", data_va);
    inject_sym("__data_load_start", data_va);

    inject_sym("__etext", rodata_end);
    inject_sym("_etext", rodata_end);
    inject_sym("etext", rodata_end);

    inject_sym("__edata", bss_va);
    inject_sym("_edata", bss_va);
    inject_sym("edata", bss_va);

    inject_sym("__end", bss_end_va);
    inject_sym("_end", bss_end_va);
    inject_sym("end", bss_end_va);

    inject_sym("__init_array_start", 0);
    inject_sym("__init_array_end", 0);
}

/* === Relocation application === */

static int is_store(uint32_t opcode) {
    return (opcode == OP_STB || opcode == OP_STH || opcode == OP_STW);
}

static void apply_rel(uint32_t idx) {
    uint32_t r_off;
    uint32_t r_sec;
    uint32_t r_typ;
    int32_t r_add;
    uint32_t r_gsym_idx;
    uint32_t val;
    int ok;
    uint8_t *tgt;
    uint32_t pc;
    uint32_t insn;
    uint32_t opcode;
    uint32_t lo12;
    uint32_t hi20;
    int32_t offset;
    uint32_t off_u;
    uint32_t sec_size;
    uint32_t hi_pc;
    int found_hi;
    uint32_t j;

    r_off = grel_off[idx];
    r_sec = grel_sec[idx];
    r_typ = grel_typ[idx];
    r_add = grel_add[idx];
    r_gsym_idx = grel_sym[idx];

    /* Compute value = sym_va + addend */
    ok = gsym_va_rel(r_gsym_idx, &val);
    if (ok) {
        val = val + (uint32_t)r_add;
    } else {
        fputs("error: unresolved symbol: ", stderr);
        if (r_gsym_idx < gsym_cnt) {
            fputs(gsym_name_str(r_gsym_idx), stderr);
        } else {
            fputs("<invalid>", stderr);
        }
        fputc('\n', stderr);
        link_error = 1;
        val = 0;
    }

    /* Bounds check */
    if (r_sec == SEC_BSS) {
        fputs("error: reloc in BSS\n", stderr);
        link_error = 1;
        return;
    }
    sec_size = sec_sz_get(r_sec);
    if (r_off + 4 > sec_size) {
        fputs("error: reloc target out of range\n", stderr);
        link_error = 1;
        return;
    }

    tgt = sec_buf(r_sec) + r_off;
    pc = sec_va_get(r_sec) + r_off;

    if (r_typ == REL_32 || r_typ == REL_ABS32) {
        wr32(tgt, 0, val);

    } else if (r_typ == REL_HI20) {
        lo12 = val & MASK12;
        hi20 = (val >> 12) & MASK20;
        if (lo12 & HALF12) hi20 = (hi20 + 1) & MASK20;
        insn = rd32(tgt, 0);
        insn = (insn & MASK12) | (hi20 << 12);
        wr32(tgt, 0, insn);

    } else if (r_typ == REL_LO12) {
        lo12 = val & MASK12;
        insn = rd32(tgt, 0);
        opcode = insn & MASK7;
        if (is_store(opcode)) {
            insn = (insn & S_MASK)
                 | ((lo12 & MASK5) << 7)
                 | (((lo12 >> 5) & MASK7) << 25);
        } else {
            insn = (insn & MASK20) | (lo12 << 20);
        }
        wr32(tgt, 0, insn);

    } else if (r_typ == REL_BRANCH) {
        offset = (int32_t)(val - pc - 4);
        insn = rd32(tgt, 0);
        insn = (insn & B_MASK)
             | ((((uint32_t)offset >> 1) & 0xFU) << 8)
             | ((((uint32_t)offset >> 5) & MASK6) << 25)
             | ((((uint32_t)offset >> 11) & 1U) << 7)
             | ((((uint32_t)offset >> 12) & 1U) << 31);
        wr32(tgt, 0, insn);

    } else if (r_typ == REL_JAL || r_typ == REL_CALL) {
        offset = (int32_t)(val - pc);
        insn = rd32(tgt, 0);
        insn = (insn & MASK12)
             | ((((uint32_t)offset >> 1) & MASK10) << 21)
             | ((((uint32_t)offset >> 11) & 1U) << 20)
             | ((((uint32_t)offset >> 12) & MASK8) << 12)
             | ((((uint32_t)offset >> 20) & 1U) << 31);
        wr32(tgt, 0, insn);

    } else if (r_typ == REL_PCREL_HI20) {
        offset = (int32_t)(val - pc);
        lo12 = (uint32_t)offset & MASK12;
        hi20 = ((uint32_t)offset >> 12) & MASK20;
        if (lo12 & HALF12) hi20 = (hi20 + 1) & MASK20;
        insn = rd32(tgt, 0);
        insn = (insn & MASK12) | (hi20 << 12);
        wr32(tgt, 0, insn);

    } else if (r_typ == REL_PCREL_LO12) {
        /* Find matching PCREL_HI20 for same symbol */
        found_hi = 0;
        hi_pc = 0;
        for (j = 0; j < grel_cnt; j = j + 1) {
            if (grel_typ[j] == REL_PCREL_HI20 && grel_sym[j] == r_gsym_idx) {
                hi_pc = sec_va_get(grel_sec[j]) + grel_off[j];
                found_hi = 1;
                break;
            }
        }
        if (!found_hi) {
            fputs("error: PCREL_LO12 no matching HI20\n", stderr);
            link_error = 1;
        } else {
            lo12 = (val - hi_pc) & MASK12;
            insn = rd32(tgt, 0);
            opcode = insn & MASK7;
            if (is_store(opcode)) {
                insn = (insn & S_MASK)
                     | ((lo12 & MASK5) << 7)
                     | (((lo12 >> 5) & MASK7) << 25);
            } else {
                insn = (insn & MASK20) | (lo12 << 20);
            }
            wr32(tgt, 0, insn);
        }
    }
    /* Unknown types silently skipped */
}

static void apply_relocations(void) {
    uint32_t i;
    for (i = 0; i < grel_cnt; i = i + 1) {
        apply_rel(i);
    }
}

/* === Find entry point === */

static void find_entry(void) {
    int idx;
    idx = gsym_find("_start");
    if (idx < 0) {
        /* Fallback: try main */
        idx = gsym_find("main");
    }
    if (idx < 0) {
        fputs("error: _start not found\n", stderr);
        entry_pt = 0;
    } else {
        entry_pt = gsym_va((uint32_t)idx);
    }
}

/* === Output helpers === */

static uint32_t str_text_off;
static uint32_t str_data_off;
static uint32_t str_bss_off;
static uint32_t str_rodata_off;

static uint32_t count_sections(void) {
    uint32_t n;
    n = 0;
    if (text_sz > 0) n = n + 1;
    if (data_sz > 0) n = n + 1;
    if (bss_sz > 0) n = n + 1;
    if (rodata_sz > 0) n = n + 1;
    return n;
}

static void build_strtab(void) {
    ostrtab_init();
    if (text_sz > 0) str_text_off = ostrtab_add(".text", 5);
    if (data_sz > 0) str_data_off = ostrtab_add(".data", 5);
    if (bss_sz > 0) str_bss_off = ostrtab_add(".bss", 4);
    if (rodata_sz > 0) str_rodata_off = ostrtab_add(".rodata", 7);
}

static void wb_init(void) {
    memset(wb_buf, 0, 64);
}

static void wb_wr32(uint32_t off, uint32_t val) {
    wr32(wb_buf, off, val);
}

static void wb_wr16(uint32_t off, uint32_t val) {
    wr16(wb_buf, off, val);
}

static void wb_wr8(uint32_t off, uint32_t val) {
    wb_buf[off] = (uint8_t)(val & 0xFFU);
}

static void fwrite_zeros(FILE *f, uint32_t n) {
    uint32_t i;
    for (i = 0; i < n; i = i + 1) fputc(0, f);
}

/* === Emit linked executable === */

static void link_emit(const char *out_path) {
    uint32_t out_nsec;
    uint32_t strtab_off;
    uint32_t data_off;
    uint32_t file_pos;
    uint32_t sec_text_foff;
    uint32_t sec_data_foff;
    uint32_t sec_rodata_foff;
    uint32_t pad;
    uint32_t code_limit;
    uint32_t flags;
    FILE *f;

    /* Final passes */
    layout_sections();
    inject_linker_symbols();
    apply_relocations();
    find_entry();

    if (link_error) {
        fputs("error: link failed\n", stderr);
        return;
    }

    out_nsec = count_sections();
    build_strtab();

    strtab_off = S32X_HDR_SZ + out_nsec * S32X_SEC_SZ;
    data_off = align16(strtab_off + out_strtab_sz);

    /* Compute file offsets for section data */
    file_pos = data_off;
    sec_text_foff = 0;
    sec_data_foff = 0;
    sec_rodata_foff = 0;
    if (text_sz > 0) {
        sec_text_foff = file_pos;
        file_pos = align4(file_pos + text_sz);
    }
    if (data_sz > 0) {
        sec_data_foff = file_pos;
        file_pos = align4(file_pos + data_sz);
    }
    if (rodata_sz > 0) {
        sec_rodata_foff = file_pos;
    }

    f = fopen(out_path, "wb");
    if (!f) {
        fputs("error: cannot create output: ", stderr);
        fputs(out_path, stderr);
        fputc('\n', stderr);
        link_error = 1;
        return;
    }

    /* 64-byte header */
    wb_init();
    wb_wr32(0, S32X_MAGIC);
    wb_wr16(4, 1);
    wb_wr8(6, S32_ENDIAN);
    wb_wr8(7, S32_MACHINE);
    wb_wr32(8, entry_pt);
    wb_wr32(12, out_nsec);
    wb_wr32(16, S32X_HDR_SZ);
    wb_wr32(20, strtab_off);
    wb_wr32(24, out_strtab_sz);
    flags = S32X_FLAG_WXORX;
    if (mmio_size_arg > 0) flags = flags | S32X_FLAG_MMIO;
    wb_wr32(28, flags);
    code_limit = page_align(text_sz);
    if (code_limit < CODE_LIMIT_MIN) code_limit = CODE_LIMIT_MIN;
    wb_wr32(32, code_limit);
    wb_wr32(36, page_align(rodata_va + rodata_sz));
    wb_wr32(40, bss_end_va);
    wb_wr32(44, STACK_BASE);
    wb_wr32(48, MEM_SIZE);
    wb_wr32(52, heap_va);
    wb_wr32(56, STACK_BASE - STACK_SIZE);
    wb_wr32(60, mmio_va);
    fwrite(wb_buf, 1, 64, f);

    /* Section table entries */
    if (text_sz > 0) {
        wb_init();
        wb_wr32(0, str_text_off);
        wb_wr32(4, SEC_CODE);
        wb_wr32(8, text_va);
        wb_wr32(12, sec_text_foff);
        wb_wr32(16, text_sz);
        wb_wr32(20, text_sz);
        wb_wr32(24, SF_XRA);
        fwrite(wb_buf, 1, S32X_SEC_SZ, f);
    }
    if (data_sz > 0) {
        wb_init();
        wb_wr32(0, str_data_off);
        wb_wr32(4, SEC_DATA);
        wb_wr32(8, data_va);
        wb_wr32(12, sec_data_foff);
        wb_wr32(16, data_sz);
        wb_wr32(20, data_sz);
        wb_wr32(24, SF_WRA);
        fwrite(wb_buf, 1, S32X_SEC_SZ, f);
    }
    if (bss_sz > 0) {
        wb_init();
        wb_wr32(0, str_bss_off);
        wb_wr32(4, SEC_BSS);
        wb_wr32(8, bss_va);
        wb_wr32(12, 0);
        wb_wr32(16, 0);
        wb_wr32(20, bss_sz);
        wb_wr32(24, SF_WRA);
        fwrite(wb_buf, 1, S32X_SEC_SZ, f);
    }
    if (rodata_sz > 0) {
        wb_init();
        wb_wr32(0, str_rodata_off);
        wb_wr32(4, SEC_RODATA);
        wb_wr32(8, rodata_va);
        wb_wr32(12, sec_rodata_foff);
        wb_wr32(16, rodata_sz);
        wb_wr32(20, rodata_sz);
        wb_wr32(24, SF_RA);
        fwrite(wb_buf, 1, S32X_SEC_SZ, f);
    }

    /* String table */
    fwrite(out_strtab, 1, out_strtab_sz, f);

    /* Pad to 16 bytes */
    file_pos = strtab_off + out_strtab_sz;
    pad = align16(file_pos) - file_pos;
    if (pad > 0) fwrite_zeros(f, pad);

    /* Section data */
    if (text_sz > 0) {
        fwrite(text_buf, 1, text_sz, f);
        pad = align4(text_sz) - text_sz;
        if (pad > 0) fwrite_zeros(f, pad);
    }
    if (data_sz > 0) {
        fwrite(data_buf, 1, data_sz, f);
        pad = align4(data_sz) - data_sz;
        if (pad > 0) fwrite_zeros(f, pad);
    }
    if (rodata_sz > 0) {
        fwrite(rodata_buf, 1, rodata_sz, f);
    }

    fclose(f);
}

/* === Parse --mmio argument === */

static uint32_t parse_mmio_size(const char *arg) {
    uint32_t val;
    uint32_t mult;
    uint32_t len;
    char last;

    len = strlen(arg);
    if (len == 0) return 0;

    last = arg[len - 1];
    mult = 1;
    if (last == 'K' || last == 'k') {
        mult = 1024;
        len = len - 1;
    } else if (last == 'M' || last == 'm') {
        mult = 1048576;
        len = len - 1;
    }

    val = 0;
    while (len > 0) {
        if (*arg < '0' || *arg > '9') return 0;
        val = val * 10 + (uint32_t)(*arg - '0');
        arg = arg + 1;
        len = len - 1;
    }
    return val * mult;
}

/* === Detect file type by reading magic === */

static int detect_file_type(const char *path) {
    FILE *f;
    uint8_t hdr[4];
    uint32_t magic;
    f = fopen(path, "rb");
    if (!f) return 0;
    if (fread(hdr, 1, 4, f) != 4) {
        fclose(f);
        return 0;
    }
    fclose(f);
    magic = (uint32_t)hdr[0] | ((uint32_t)hdr[1] << 8)
          | ((uint32_t)hdr[2] << 16) | ((uint32_t)hdr[3] << 24);
    if (magic == S32A_MAGIC) return 2; /* archive */
    if (magic == S32O_MAGIC) return 1; /* object */
    return 0;
}

/* === Main === */

int main(int argc, char **argv) {
    const char *out_path;
    int i;
    int ftype;
    int has_o_flag;

    out_path = NULL;
    mmio_size_arg = 0;
    link_error = 0;

    /* Init state */
    text_sz = 0;
    data_sz = 0;
    rodata_sz = 0;
    bss_sz = 0;
    gsym_cnt = 0;
    gsym_nptr = 0;
    grel_cnt = 0;
    text_va = 0;
    rodata_va = 0;
    data_va = 0;
    bss_va = 0;
    bss_end_va = 0;
    heap_va = 0;
    mmio_va = 0;
    entry_pt = 0;
    ostrtab_init();

    if (argc < 3) {
        fputs("Usage: s32-ld -o output.s32x [--mmio SIZE] file1 ...\n", stderr);
        fputs("   or: s32-ld input.s32o [aux] output.s32x\n", stderr);
        return 1;
    }

    /* Detect CLI style: new-style has -o flag */
    has_o_flag = 0;
    for (i = 1; i < argc; i = i + 1) {
        if (str_eq(argv[i], "-o")) { has_o_flag = 1; break; }
    }

    if (has_o_flag) {
        /* New-style: -o output [--mmio SIZE] file1 file2 ... */
        for (i = 1; i < argc; i = i + 1) {
            if (str_eq(argv[i], "-o")) {
                i = i + 1;
                if (i >= argc) {
                    fputs("error: -o requires an argument\n", stderr);
                    return 1;
                }
                out_path = argv[i];
            } else if (str_eq(argv[i], "--mmio")) {
                i = i + 1;
                if (i >= argc) {
                    fputs("error: --mmio requires an argument\n", stderr);
                    return 1;
                }
                mmio_size_arg = parse_mmio_size(argv[i]);
            } else {
                /* Input file */
                ftype = detect_file_type(argv[i]);
                if (ftype == 1) {
                    link_obj(argv[i]);
                } else if (ftype == 2) {
                    link_archive(argv[i]);
                } else {
                    fputs("error: unknown file type: ", stderr);
                    fputs(argv[i], stderr);
                    fputc('\n', stderr);
                    return 1;
                }
                if (link_error) return 1;
            }
        }
    } else {
        /* Old-style: input.s32o [aux.s32o|aux.s32a] output.s32x */
        out_path = argv[argc - 1];
        link_obj(argv[1]);
        if (link_error) return 1;
        if (argc == 4) {
            ftype = detect_file_type(argv[2]);
            if (ftype == 1) {
                link_obj(argv[2]);
            } else if (ftype == 2) {
                link_archive(argv[2]);
            }
            if (link_error) return 1;
        }
    }

    if (out_path == NULL) {
        fputs("error: no output file specified\n", stderr);
        return 1;
    }

    link_emit(out_path);
    if (link_error) return 1;

    fputs("stage07: linked -> ", stdout);
    fputs(out_path, stdout);
    fputc('\n', stdout);
    return 0;
}
