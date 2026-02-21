/* s32-ld-port.c -- cc-min compatible linker for .s32o/.s32a -> .s32x
 *
 * Full-featured linker matching the stage03 reference linker.
 * Supports:
 *   - Multiple .s32o object files with section merging
 *   - .s32a archive iterative pull loop
 *   - All 9 relocation types
 *   - Linker-defined symbols (__bss_start, __mmio_base, etc.)
 *   - Dynamic expression resolution for __mmio_base+N patterns
 *
 * Porting constraints (cc-min subset C):
 *   - No postfix ++/-- (use x = x + 1)
 *   - No ~expr (use constant equivalents)
 *   - No void* return types (use char*)
 *   - No for(;;) (use while(1))
 *   - No block-scoped declarations
 *   - All arrays are globals (no large local arrays)
 */

/* === Inlined header (s32ld_min.h) === */

/* Standard file handles */
int stdout;
int stderr;

#define NULL 0
#define EOF -1
#define SEEK_SET 0
#define SEEK_END 2

#define S32O_MAGIC 0x5333324F
#define S32X_MAGIC 0x53333258
#define S32A_MAGIC 0x53333241

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

#define MASK5   0x1F
#define MASK6   0x3F
#define MASK7   0x7F
#define MASK8   0xFF
#define MASK10  0x3FF
#define MASK12  0xFFF
#define MASK20  0xFFFFF
#define HALF12  0x800

#define OP_STB 0x38
#define OP_STH 0x39
#define OP_STW 0x3A

#define S_MASK 0x01FFF07F
#define B_MASK 0x01FFF07F

#define STACK_BASE    0x0FFFFFF0
#define STACK_SIZE    1048576
#define MEM_SIZE      0x10000000
#define HEAP_GAP      0x800000
#define CODE_LIMIT_MIN 0x100000
#define PAGE_MASK     0xFFFFF000

#define S32X_HDR_SZ 64
#define S32X_SEC_SZ 28
#define S32O_SEC_SZ 32
#define S32O_SYM_SZ 16
#define S32O_REL_SZ 16

#define S32_MACHINE  0x32
#define S32_ENDIAN   0x01
#define S32X_FLAG_WXORX 0x01
#define S32X_FLAG_MMIO  0x80

int strcmp(char *a, char *b);
int strlen(char *s);
char *strchr(char *s, int c);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);

int fopen(char *path, char *mode);
int fclose(int f);
int fputc(int c, int f);
int fputs(char *s, int f);
int fgetc(int f);
int fseek(int f, int off, int whence);
int ftell(int f);
int fwrite(char *buf, int sz, int count, int f);
int fread(char *buf, int sz, int count, int f);
int fput_uint(int f, int v);

/* === End inlined header === */

/* === Buffer size constants === */
#define MAX_GSYM     4096
#define MAX_GREL     32768
#define GSYM_NBUF_SZ 131072
#define TEXT_BUFSZ   1048576
#define DATA_BUFSZ   1048576
#define RODATA_BUFSZ 262144
#define FILE_BUFSZ   1048576
#define MAX_FILE_SYM 2048
#define MAX_MEMBERS  512
#define OUT_STRTAB_SZ 32768

/* Archive table sizes */
#define AR_SYMTAB_SZ 32768
#define AR_MEMTAB_SZ 24576
#define AR_STRTAB_SZ 32768

/* === Merged section buffers === */
char text_buf[TEXT_BUFSZ];
char data_buf[DATA_BUFSZ];
char rodata_buf[RODATA_BUFSZ];
int text_sz;
int data_sz;
int rodata_sz;
int bss_sz;

/* === Global symbol table (parallel arrays) === */
int gsym_noff[MAX_GSYM];
int gsym_nlen[MAX_GSYM];
int gsym_val[MAX_GSYM];
int gsym_sec[MAX_GSYM];
int gsym_bind[MAX_GSYM];
int gsym_def[MAX_GSYM];
char gsym_nbuf[GSYM_NBUF_SZ];
int gsym_cnt;
int gsym_nptr;

/* === Global relocation table (parallel arrays) === */
int grel_off[MAX_GREL];
int grel_sec[MAX_GREL];
int grel_typ[MAX_GREL];
int grel_add[MAX_GREL];
int grel_sym[MAX_GREL];
int grel_cnt;

/* === Layout variables === */
int text_va;
int rodata_va;
int data_va;
int bss_va;
int bss_end_va;
int heap_va;
int mmio_sz;
int mmio_va;
int entry_pt;

/* === File I/O buffer === */
char file_buf[FILE_BUFSZ];

/* === Per-file state === */
#define MAX_OBJ_SEC 32
int sec_base_by_idx[MAX_OBJ_SEC];
int sym_map[MAX_FILE_SYM];

/* Object header parse state */
int obj_nsec;
int obj_nsym;
int obj_sec_off;
int obj_sym_off;
int obj_str_off;

/* === Archive state === */
char ar_symtab[AR_SYMTAB_SZ];
char ar_memtab[AR_MEMTAB_SZ];
char ar_strtab[AR_STRTAB_SZ];
char mem_loaded[MAX_MEMBERS];
int ar_nmembers;
int ar_mem_off;
int ar_nsymbols;
int ar_sym_off;
int ar_str_off;
int ar_str_sz;

/* === Output state === */
char out_strtab[OUT_STRTAB_SZ];
int out_strtab_sz;

/* Write buffer for header/section entries */
char wb_buf[64];

/* Link error flag */
int link_error;

/* MMIO size from command line */
int mmio_size_arg;

/* === Helper functions === */

int rd32(char *buf, int off) {
    int b0;
    int b1;
    int b2;
    int b3;
    b0 = buf[off] & 255;
    b1 = buf[off + 1] & 255;
    b2 = buf[off + 2] & 255;
    b3 = buf[off + 3] & 255;
    return b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
}

int rd16(char *buf, int off) {
    int b0;
    int b1;
    b0 = buf[off] & 255;
    b1 = buf[off + 1] & 255;
    return b0 | (b1 << 8);
}

int rd8(char *buf, int off) {
    return buf[off] & 255;
}

void wr32(char *buf, int off, int val) {
    buf[off] = val & 255;
    buf[off + 1] = (val >> 8) & 255;
    buf[off + 2] = (val >> 16) & 255;
    buf[off + 3] = (val >> 24) & 255;
}

void wr16(char *buf, int off, int val) {
    buf[off] = val & 255;
    buf[off + 1] = (val >> 8) & 255;
}

int align4(int n) {
    return (n + 3) & (-4);
}

int align16(int n) {
    return (n + 15) & (-16);
}

int page_align(int n) {
    return (n + 4095) & PAGE_MASK;
}

int max_val(int a, int b) {
    if (a > b) return a;
    return b;
}

int streq(char *a, char *b) {
    return strcmp(a, b) == 0;
}

/* NUL-terminated string length in file_buf starting at offset */
int fbuf_strlen(int off) {
    int len;
    len = 0;
    while (file_buf[off + len] != 0) {
        len = len + 1;
    }
    return len;
}

/* String comparison: name (NUL-terminated) vs gsym entry */
int name_eq_gsym(char *name, int idx) {
    int noff;
    int nlen;
    int i;
    noff = gsym_noff[idx];
    nlen = gsym_nlen[idx];
    i = 0;
    while (i < nlen) {
        if (name[i] == 0) return 0;
        if (name[i] != gsym_nbuf[noff + i]) return 0;
        i = i + 1;
    }
    if (name[nlen] != 0) return 0;
    return 1;
}

/* === File I/O === */

int read_file(char *path) {
    int f;
    int sz;
    int nr;
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
        nr = fread(file_buf, 1, sz, f);
        if (nr != sz) {
            fputs("error: short read: ", stderr);
            fputs(path, stderr);
            fputc('\n', stderr);
            fclose(f);
            return -1;
        }
    }
    fclose(f);
    return sz;
}

/* === Output string table === */

void ostrtab_init() {
    out_strtab[0] = 0;
    out_strtab_sz = 1;
}

int ostrtab_add(char *s, int slen) {
    int off;
    int i;
    off = out_strtab_sz;
    if (off + slen + 1 > OUT_STRTAB_SZ) return off;
    i = 0;
    while (i < slen) {
        out_strtab[off + i] = s[i];
        i = i + 1;
    }
    out_strtab[off + slen] = 0;
    out_strtab_sz = off + slen + 1;
    return off;
}

/* === Symbol table === */

int gsym_find(char *name) {
    int i;
    i = 0;
    while (i < gsym_cnt) {
        if (name_eq_gsym(name, i)) return i;
        i = i + 1;
    }
    return -1;
}

int gsym_add_name(char *name, int len) {
    int off;
    int i;
    off = gsym_nptr;
    if (off + len > GSYM_NBUF_SZ) {
        fputs("error: gsym name buffer overflow\n", stderr);
        link_error = 1;
        return 0;
    }
    i = 0;
    while (i < len) {
        gsym_nbuf[off + i] = name[i];
        i = i + 1;
    }
    gsym_nptr = off + len;
    return off;
}

int gsym_add_new(char *name, int sec, int val, int bind, int def) {
    int idx;
    int nlen;
    int noff;
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
    return idx;
}

int gsym_upsert(char *name, int sec, int val, int bind) {
    int idx;
    int def;
    idx = gsym_find(name);
    if (idx < 0) {
        /* Not found -- add new */
        if (sec != 0) {
            def = 1;
        } else {
            def = 0;
        }
        return gsym_add_new(name, sec, val, bind, def);
    }
    /* Found -- handle based on binding priority */
    if (sec != 0) {
        if (gsym_def[idx] == 0) {
            /* Was undefined, now defined -- always update */
            gsym_sec[idx] = sec;
            gsym_val[idx] = val;
            gsym_bind[idx] = bind;
            gsym_def[idx] = 1;
        } else if (gsym_def[idx] == 2) {
            /* Linker-defined -- keep linker definition */
        } else if (gsym_bind[idx] == BIND_WEAK) {
            /* Existing is weak -- replace with new (strong or weak) */
            gsym_sec[idx] = sec;
            gsym_val[idx] = val;
            gsym_bind[idx] = bind;
        } else if (bind != BIND_WEAK) {
            /* Both strong -- warn on multiple definition (allow BSS merging) */
            if (gsym_sec[idx] != SEC_BSS || sec != SEC_BSS) {
                fputs("warning: multiple definition of '", stderr);
                fputs(name, stderr);
                fputs("'\n", stderr);
            }
        }
        /* else: existing is strong, new is weak -- keep existing */
    }
    return idx;
}

int gsym_add_undef(char *name) {
    return gsym_upsert(name, 0, 0, BIND_GLOBAL);
}

/* === Section helpers === */

char *sec_buf(int sec_type) {
    if (sec_type == SEC_CODE) return text_buf;
    if (sec_type == SEC_DATA) return data_buf;
    if (sec_type == SEC_RODATA) return rodata_buf;
    return NULL;
}

int sec_cap(int sec_type) {
    if (sec_type == SEC_CODE) return TEXT_BUFSZ;
    if (sec_type == SEC_DATA) return DATA_BUFSZ;
    if (sec_type == SEC_RODATA) return RODATA_BUFSZ;
    return 0;
}

int sec_sz_get(int sec_type) {
    if (sec_type == SEC_CODE) return text_sz;
    if (sec_type == SEC_DATA) return data_sz;
    if (sec_type == SEC_BSS) return bss_sz;
    if (sec_type == SEC_RODATA) return rodata_sz;
    return 0;
}

void sec_sz_set(int sec_type, int val) {
    if (sec_type == SEC_CODE) text_sz = val;
    if (sec_type == SEC_DATA) data_sz = val;
    if (sec_type == SEC_BSS) bss_sz = val;
    if (sec_type == SEC_RODATA) rodata_sz = val;
}

int sec_va_get(int sec_type) {
    if (sec_type == SEC_CODE) return text_va;
    if (sec_type == SEC_DATA) return data_va;
    if (sec_type == SEC_BSS) return bss_va;
    if (sec_type == SEC_RODATA) return rodata_va;
    return 0;
}

/* === Parse .s32o header === */

int parse_obj_header() {
    int magic;
    magic = rd32(file_buf, 0);
    if (magic != S32O_MAGIC) {
        fputs("error: bad .s32o magic\n", stderr);
        return 0;
    }
    obj_nsec = rd32(file_buf, 12);
    obj_sec_off = rd32(file_buf, 16);
    obj_nsym = rd32(file_buf, 20);
    obj_sym_off = rd32(file_buf, 24);
    obj_str_off = rd32(file_buf, 28);
    return 1;
}

/* === Merge sections from current .s32o === */

void merge_sections() {
    int i;
    int sec_off;
    int stype;
    int ssize;
    int sfileoff;
    int base;
    int cur_sz;
    char *dst;
    char *src;
    int j;

    i = 0;
    while (i < MAX_OBJ_SEC) {
        sec_base_by_idx[i] = 0;
        i = i + 1;
    }

    i = 0;
    while (i < obj_nsec) {
        sec_off = i * S32O_SEC_SZ + obj_sec_off;
        stype = rd32(file_buf, sec_off + 4);
        ssize = rd32(file_buf, sec_off + 12);
        sfileoff = rd32(file_buf, sec_off + 16);

        if (stype == SEC_CODE || stype == SEC_DATA ||
            stype == SEC_BSS || stype == SEC_RODATA) {

            /* Compute aligned base in merged section */
            cur_sz = sec_sz_get(stype);
            if (cur_sz > 0) {
                base = align4(cur_sz);
            } else {
                base = 0;
            }

            /* Record base for symbol/reloc adjustment (per section index) */
            if (i < MAX_OBJ_SEC) {
                sec_base_by_idx[i] = base;
            }

            /* Copy data (skip BSS) */
            if (stype != SEC_BSS) {
                if (ssize > 0 && sfileoff > 0) {
                    if (base + ssize > sec_cap(stype)) {
                        fputs("error: section buffer overflow\n", stderr);
                        link_error = 1;
                        return;
                    }
                    dst = sec_buf(stype) + base;
                    src = file_buf + sfileoff;
                    j = 0;
                    while (j < ssize) {
                        dst[j] = src[j];
                        j = j + 1;
                    }
                }
            }

            /* Update merged section size */
            sec_sz_set(stype, base + ssize);
        }

        i = i + 1;
    }
}

/* === Merge symbols from current .s32o === */

void merge_symbols() {
    int i;
    int ent_off;
    int sy_noff;
    int sy_val;
    int sy_sec;
    int sy_bind;
    int sy_sectype;
    char *name;
    int name_off;
    int name_len;
    int gidx;

    if (obj_nsym > MAX_FILE_SYM) {
        fputs("error: object symbol table too large\n", stderr);
        link_error = 1;
        return;
    }

    i = 0;
    while (i < obj_nsym) {
        ent_off = i * S32O_SYM_SZ + obj_sym_off;
        sy_noff = rd32(file_buf, ent_off);
        sy_val = rd32(file_buf, ent_off + 4);
        sy_sec = rd16(file_buf, ent_off + 8);
        sy_bind = rd8(file_buf, ent_off + 11);

        /* Get name from string table */
        name_off = obj_str_off + sy_noff;
        name = file_buf + name_off;

        if (sy_sec == 0) {
            /* Undefined symbol */
            if (sy_bind == BIND_LOCAL) {
                gidx = -1;
            } else {
                gidx = gsym_add_undef(name);
            }
        } else {
            /* Defined: get section type from section table */
            sy_sectype = rd32(file_buf, (sy_sec - 1) * S32O_SEC_SZ + obj_sec_off + 4);

            /* Adjust value by section base offset (per section index) */
            sy_val = sec_base_by_idx[sy_sec - 1] + sy_val;

            if (sy_bind == BIND_LOCAL) {
                /* Locals are file-scoped */
                gidx = gsym_add_new(name, sy_sectype, sy_val, sy_bind, 1);
            } else {
                gidx = gsym_upsert(name, sy_sectype, sy_val, sy_bind);
            }
        }

        sym_map[i] = gidx;
        i = i + 1;
    }
}

/* === Merge relocations from current .s32o === */

void merge_relocs() {
    int i;
    int sec_off;
    int stype;
    int nrelocs;
    int reloc_off;
    int r;
    int roff;
    int rl_off;
    int rl_sym;
    int rl_typ;
    int rl_add;
    int mapped_sym;

    i = 0;
    while (i < obj_nsec) {
        sec_off = i * S32O_SEC_SZ + obj_sec_off;
        stype = rd32(file_buf, sec_off + 4);
        nrelocs = rd32(file_buf, sec_off + 24);
        reloc_off = rd32(file_buf, sec_off + 28);

        r = 0;
        while (r < nrelocs) {
            roff = reloc_off + r * S32O_REL_SZ;
            rl_off = rd32(file_buf, roff);
            rl_sym = rd32(file_buf, roff + 4);
            rl_typ = rd32(file_buf, roff + 8);
            rl_add = rd32(file_buf, roff + 12);

            if (grel_cnt < MAX_GREL) {
                grel_add[grel_cnt] = rl_add;
                grel_typ[grel_cnt] = rl_typ;

                /* Map file-local symbol to gsym */
                if (rl_sym < MAX_FILE_SYM) {
                    mapped_sym = sym_map[rl_sym];
                } else {
                    mapped_sym = -1;
                }
                grel_sym[grel_cnt] = mapped_sym;

                /* Adjust offset by section base (per section index) */
                grel_off[grel_cnt] = rl_off + sec_base_by_idx[i];

                /* Store section type */
                grel_sec[grel_cnt] = stype;

                grel_cnt = grel_cnt + 1;
            }

            r = r + 1;
        }

        i = i + 1;
    }
}

/* === Link object file === */

void link_obj(char *path) {
    int sz;
    int i;

    fputs("Loading: ", stderr);
    fputs(path, stderr);
    fputc('\n', stderr);

    sz = read_file(path);
    if (sz < 0) {
        link_error = 1;
        return;
    }

    if (!parse_obj_header()) {
        link_error = 1;
        return;
    }

    /* Clear sym_map */
    i = 0;
    while (i < MAX_FILE_SYM) {
        sym_map[i] = 0;
        i = i + 1;
    }

    merge_sections();
    if (link_error) return;
    merge_symbols();
    if (link_error) return;
    merge_relocs();
}

/* === Archive loading === */

/* Get name of archive symbol at index */
char *ar_sym_name(int idx) {
    int name_off;
    name_off = rd32(ar_symtab, idx * 8);
    if (name_off >= ar_str_sz) return ar_strtab;
    return ar_strtab + name_off;
}

/* Get member index for archive symbol */
int ar_sym_member(int idx) {
    return rd32(ar_symtab, idx * 8 + 4);
}

/* Get file offset of archive member */
int ar_mem_offset(int idx) {
    return rd32(ar_memtab, idx * 24 + 4);
}

/* Get size of archive member */
int ar_mem_size(int idx) {
    return rd32(ar_memtab, idx * 24 + 8);
}

/* Check if symbol name is undefined in gsym table */
int sym_undefined(char *name) {
    int idx;
    idx = gsym_find(name);
    if (idx < 0) return 0;
    return gsym_def[idx] == 0;
}

/* Load archive member into file_buf and link it */
void load_ar_member(int ar_fh, int midx) {
    int mem_off;
    int mem_sz;
    int i;

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

    fseek(ar_fh, mem_off, SEEK_SET);
    if (fread(file_buf, 1, mem_sz, ar_fh) != mem_sz) {
        fputs("error: short read on archive member\n", stderr);
        link_error = 1;
        return;
    }

    if (!parse_obj_header()) {
        link_error = 1;
        return;
    }

    /* Clear sym_map */
    i = 0;
    while (i < MAX_FILE_SYM) {
        sym_map[i] = 0;
        i = i + 1;
    }

    merge_sections();
    if (link_error) return;
    merge_symbols();
    if (link_error) return;
    merge_relocs();
}

void link_archive(char *path) {
    int ar_fh;
    int hdr_nmembers;
    int added_any;
    int i;
    int midx;
    char *name;

    fputs("Loading archive: ", stderr);
    fputs(path, stderr);
    fputc('\n', stderr);

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

    ar_nmembers = rd32(file_buf, 8);
    ar_mem_off = rd32(file_buf, 12);
    ar_nsymbols = rd32(file_buf, 16);
    ar_sym_off = rd32(file_buf, 20);
    ar_str_off = rd32(file_buf, 24);
    ar_str_sz = rd32(file_buf, 28);

    if (ar_nmembers > MAX_MEMBERS) {
        fputs("error: too many archive members\n", stderr);
        fclose(ar_fh);
        link_error = 1;
        return;
    }
    if (ar_nsymbols * 8 > AR_SYMTAB_SZ) {
        fputs("error: archive symbol table too large\n", stderr);
        fclose(ar_fh);
        link_error = 1;
        return;
    }
    if (ar_nmembers * 24 > AR_MEMTAB_SZ) {
        fputs("error: archive member table too large\n", stderr);
        fclose(ar_fh);
        link_error = 1;
        return;
    }
    if (ar_str_sz > AR_STRTAB_SZ) {
        fputs("error: archive string table too large\n", stderr);
        fclose(ar_fh);
        link_error = 1;
        return;
    }

    /* Read symbol index */
    fseek(ar_fh, ar_sym_off, SEEK_SET);
    fread(ar_symtab, 1, ar_nsymbols * 8, ar_fh);

    /* Read member table */
    fseek(ar_fh, ar_mem_off, SEEK_SET);
    fread(ar_memtab, 1, ar_nmembers * 24, ar_fh);

    /* Read string table */
    fseek(ar_fh, ar_str_off, SEEK_SET);
    fread(ar_strtab, 1, ar_str_sz, ar_fh);

    /* Clear loaded flags */
    i = 0;
    while (i < MAX_MEMBERS) {
        mem_loaded[i] = 0;
        i = i + 1;
    }

    /* Iterative pull loop */
    while (1) {
        added_any = 0;
        i = 0;
        while (i < ar_nsymbols) {
            midx = ar_sym_member(i);
            if (midx < ar_nmembers) {
                if (!mem_loaded[midx]) {
                    name = ar_sym_name(i);
                    if (sym_undefined(name)) {
                        load_ar_member(ar_fh, midx);
                        added_any = 1;
                    }
                }
            }
            if (link_error) break;
            i = i + 1;
        }
        if (link_error) break;
        if (!added_any) break;
    }

    fclose(ar_fh);
}

/* === Section layout === */

void layout_sections() {
    int t;
    text_va = 0;
    t = page_align(text_sz);
    if (t < CODE_LIMIT_MIN) t = CODE_LIMIT_MIN;
    rodata_va = t;
    t = page_align(rodata_va + rodata_sz);
    data_va = t;
    t = data_va + data_sz;
    bss_va = t;
    t = bss_va + bss_sz;
    bss_end_va = t;
    heap_va = page_align(t);

    if (mmio_size_arg > 0) {
        mmio_va = page_align(heap_va + HEAP_GAP);
    } else {
        mmio_va = 0;
    }
}

/* === Symbol VA computation === */

int gsym_va(int idx) {
    if (gsym_def[idx] == 2) {
        /* Linker-defined: value is absolute */
        return gsym_val[idx];
    }
    return sec_va_get(gsym_sec[idx]) + gsym_val[idx];
}

/* Get gsym name as NUL-terminated string pointer */
/* Note: we need to temporarily NUL-terminate since gsym_nbuf is packed */
char gsym_name_tmp[256];

char *gsym_name_str(int idx) {
    int noff;
    int nlen;
    int i;
    noff = gsym_noff[idx];
    nlen = gsym_nlen[idx];
    if (nlen > 255) nlen = 255;
    i = 0;
    while (i < nlen) {
        gsym_name_tmp[i] = gsym_nbuf[noff + i];
        i = i + 1;
    }
    gsym_name_tmp[nlen] = 0;
    return gsym_name_tmp;
}

/* === Expression resolution === */

/* Parse unsigned decimal from string, return -1 if not a number */
int parse_udec(char *s) {
    int acc;
    int ch;
    if (*s == 0) return -1;
    acc = 0;
    while (*s) {
        ch = *s & 255;
        if (ch < '0' || ch > '9') return -1;
        acc = acc * 10 + (ch - '0');
        s = s + 1;
    }
    return acc;
}

/* Resolve symbol VA for relocation, with expression resolution.
 * Returns value in *out_va, returns 1 on success, 0 on failure. */
char expr_base_buf[256];

int gsym_va_rel(int idx, int *out_va) {
    char *name;
    int i;
    int pos;
    int base_idx;
    int addend;
    int sign;
    char *after;

    if (idx < 0) return 0;

    /* If symbol is defined, just return its VA */
    if (gsym_def[idx] != 0) {
        *out_va = gsym_va(idx);
        return 1;
    }

    /* Symbol is undefined -- try expression resolution */
    name = gsym_name_str(idx);

    /* Look for '+' or '-' in name */
    pos = -1;
    sign = 1;
    i = 1;  /* Start at 1: first char can't be the split point */
    while (name[i] != 0) {
        if (name[i] == '+') {
            pos = i;
            sign = 1;
            break;
        }
        if (name[i] == '-') {
            pos = i;
            sign = -1;
            break;
        }
        i = i + 1;
    }

    if (pos < 0) return 0;

    /* Split into base name and addend */
    i = 0;
    while (i < pos && i < 255) {
        expr_base_buf[i] = name[i];
        i = i + 1;
    }
    expr_base_buf[i] = 0;

    after = name + pos + 1;
    addend = parse_udec(after);
    if (addend < 0) return 0;
    addend = addend * sign;

    /* Look up base symbol */
    base_idx = gsym_find(expr_base_buf);
    if (base_idx < 0) return 0;
    if (gsym_def[base_idx] == 0) return 0;

    *out_va = gsym_va(base_idx) + addend;
    return 1;
}

/* === Inject linker-defined symbols === */

void inject_sym(char *name, int value) {
    int idx;
    idx = gsym_find(name);
    if (idx < 0) {
        /* Not in table -- add as linker-defined */
        gsym_add_new(name, SEC_CODE, value, BIND_GLOBAL, 2);
    } else {
        /* In table -- update only if undefined */
        if (gsym_def[idx] == 0) {
            gsym_val[idx] = value;
            gsym_def[idx] = 2;
        }
    }
}

void inject_linker_symbols() {
    int heap_end;
    int rodata_end;

    rodata_end = page_align(rodata_va + rodata_sz);

    inject_sym("__bss_start", bss_va);
    inject_sym("__bss_end", bss_end_va);
    inject_sym("__bss_start__", bss_va);
    inject_sym("__bss_end__", bss_end_va);
    inject_sym("_bss_start", bss_va);
    inject_sym("_bss_end", bss_end_va);

    inject_sym("__mmio_base", mmio_va);

    /* Pre-inject common __mmio_base+N variants */
    inject_sym("__mmio_base+4", mmio_va + 4);
    inject_sym("__mmio_base+4096", mmio_va + 4096);
    inject_sym("__mmio_base+4100", mmio_va + 4100);
    inject_sym("__mmio_base+4104", mmio_va + 4104);
    inject_sym("__mmio_base+4108", mmio_va + 4108);
    inject_sym("__mmio_base+8192", mmio_va + 8192);
    inject_sym("__mmio_base+8196", mmio_va + 8196);
    inject_sym("__mmio_base+12300", mmio_va + 12300);
    inject_sym("__mmio_base+16384", mmio_va + 16384);
    inject_sym("__mmio_base+16388", mmio_va + 16388);
    inject_sym("__mmio_base+16392", mmio_va + 16392);
    inject_sym("__mmio_base+16396", mmio_va + 16396);
    inject_sym("__mmio_base+16400", mmio_va + 16400);
    inject_sym("__mmio_base+16404", mmio_va + 16404);
    inject_sym("__mmio_base+16408", mmio_va + 16408);
    inject_sym("__mmio_base+16412", mmio_va + 16412);
    inject_sym("__mmio_base+16416", mmio_va + 16416);
    inject_sym("__mmio_base+16420", mmio_va + 16420);
    inject_sym("__mmio_base+16424", mmio_va + 16424);
    inject_sym("__mmio_base+16428", mmio_va + 16428);
    inject_sym("__mmio_base+16432", mmio_va + 16432);

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

int is_store(int opcode) {
    if (opcode == OP_STB) return 1;
    if (opcode == OP_STH) return 1;
    if (opcode == OP_STW) return 1;
    return 0;
}

/* Check if relocation target is in range */
int rel_tgt_ok(int sec, int off) {
    int sz;
    if (off < 0) return 0;
    if (sec == SEC_BSS) return 0;
    sz = sec_sz_get(sec);
    if (sz < 4) return 0;
    if (off > sz - 4) return 0;
    return 1;
}

void apply_rel(int idx) {
    int r_off;
    int r_sec;
    int r_typ;
    int r_add;
    int r_gsym;
    int val;
    int ok;
    char *tgt;
    int pc;
    int insn;
    int opcode;
    int lo12;
    int hi20;
    int offset;
    int imm12;
    int imm11;
    int imm10_5;
    int imm4_1;
    int imm20;
    int imm19_12;
    int imm10_1;
    int hi_pc;
    int j;

    r_off = grel_off[idx];
    r_sec = grel_sec[idx];
    r_typ = grel_typ[idx];
    r_add = grel_add[idx];
    r_gsym = grel_sym[idx];

    /* Compute value = sym_va + addend */
    ok = gsym_va_rel(r_gsym, &val);
    if (ok) {
        val = val + r_add;
    } else {
        fputs("error: unresolved symbol: ", stderr);
        if (r_gsym >= 0 && r_gsym < gsym_cnt) {
            fputs(gsym_name_str(r_gsym), stderr);
        } else {
            fputs("<invalid>", stderr);
        }
        fputc('\n', stderr);
        link_error = 1;
        val = 0;
    }

    /* Check target is in range */
    if (!rel_tgt_ok(r_sec, r_off)) {
        fputs("error: reloc target out of range\n", stderr);
        link_error = 1;
        return;
    }

    /* Target buffer address */
    tgt = sec_buf(r_sec) + r_off;

    /* PC = section_va + offset */
    pc = sec_va_get(r_sec) + r_off;

    if (r_typ == REL_32 || r_typ == REL_ABS32) {
        wr32(tgt, 0, val);

    } else if (r_typ == REL_HI20) {
        lo12 = val & MASK12;
        hi20 = (val >> 12) & MASK20;
        if (lo12 & HALF12) {
            hi20 = (hi20 + 1) & MASK20;
        }
        insn = rd32(tgt, 0);
        insn = (insn & MASK12) | (hi20 << 12);
        wr32(tgt, 0, insn);

    } else if (r_typ == REL_LO12) {
        lo12 = val & MASK12;
        insn = rd32(tgt, 0);
        opcode = insn & MASK7;
        if (is_store(opcode)) {
            insn = insn & S_MASK;
            insn = insn | ((lo12 & MASK5) << 7);
            insn = insn | (((lo12 >> 5) & MASK7) << 25);
        } else {
            insn = insn & MASK20;
            insn = insn | (lo12 << 20);
        }
        wr32(tgt, 0, insn);

    } else if (r_typ == REL_BRANCH) {
        offset = val - pc - 4;
        imm12 = (offset >> 12) & 1;
        imm11 = (offset >> 11) & 1;
        imm10_5 = (offset >> 5) & MASK6;
        imm4_1 = (offset >> 1) & 15;
        insn = rd32(tgt, 0);
        insn = insn & B_MASK;
        insn = insn | (imm4_1 << 8);
        insn = insn | (imm10_5 << 25);
        insn = insn | (imm11 << 7);
        insn = insn | (imm12 << 31);
        wr32(tgt, 0, insn);

    } else if (r_typ == REL_JAL || r_typ == REL_CALL) {
        offset = val - pc;
        imm20 = (offset >> 20) & 1;
        imm19_12 = (offset >> 12) & MASK8;
        imm11 = (offset >> 11) & 1;
        imm10_1 = (offset >> 1) & MASK10;
        insn = rd32(tgt, 0);
        insn = insn & MASK12;
        insn = insn | (imm10_1 << 21);
        insn = insn | (imm11 << 20);
        insn = insn | (imm19_12 << 12);
        insn = insn | (imm20 << 31);
        wr32(tgt, 0, insn);

    } else if (r_typ == REL_PCREL_HI20) {
        offset = val - pc;
        lo12 = offset & MASK12;
        hi20 = (offset >> 12) & MASK20;
        if (lo12 & HALF12) {
            hi20 = (hi20 + 1) & MASK20;
        }
        insn = rd32(tgt, 0);
        insn = (insn & MASK12) | (hi20 << 12);
        wr32(tgt, 0, insn);

    } else if (r_typ == REL_PCREL_LO12) {
        /* Find matching PCREL_HI20 */
        hi_pc = -1;
        j = 0;
        while (j < grel_cnt) {
            if (grel_typ[j] == REL_PCREL_HI20) {
                if (grel_sym[j] == r_gsym) {
                    hi_pc = sec_va_get(grel_sec[j]) + grel_off[j];
                    break;
                }
            }
            j = j + 1;
        }
        if (hi_pc < 0) {
            fputs("error: PCREL_LO12 no matching HI20\n", stderr);
            link_error = 1;
        } else {
            lo12 = (val - hi_pc) & MASK12;
            insn = rd32(tgt, 0);
            opcode = insn & MASK7;
            if (is_store(opcode)) {
                insn = insn & S_MASK;
                insn = insn | ((lo12 & MASK5) << 7);
                insn = insn | (((lo12 >> 5) & MASK7) << 25);
            } else {
                insn = insn & MASK20;
                insn = insn | (lo12 << 20);
            }
            wr32(tgt, 0, insn);
        }
    }
    /* Unknown types silently skipped */
}

void apply_relocations() {
    int i;
    i = 0;
    while (i < grel_cnt) {
        apply_rel(i);
        i = i + 1;
    }
}

/* === Find entry point === */

void find_entry() {
    int idx;
    idx = gsym_find("_start");
    if (idx < 0) {
        fputs("error: _start not found\n", stderr);
        entry_pt = 0;
    } else {
        entry_pt = gsym_va(idx);
    }
}

/* === Count output sections === */

int count_sections() {
    int n;
    n = 0;
    if (text_sz > 0) n = n + 1;
    if (data_sz > 0) n = n + 1;
    if (bss_sz > 0) n = n + 1;
    if (rodata_sz > 0) n = n + 1;
    return n;
}

/* === Build output string table === */

int str_text_off;
int str_data_off;
int str_bss_off;
int str_rodata_off;

void build_strtab() {
    ostrtab_init();
    if (text_sz > 0)   str_text_off = ostrtab_add(".text", 5);
    if (data_sz > 0)   str_data_off = ostrtab_add(".data", 5);
    if (bss_sz > 0)    str_bss_off = ostrtab_add(".bss", 4);
    if (rodata_sz > 0) str_rodata_off = ostrtab_add(".rodata", 7);
}

/* === Write helpers === */

void wb_init() {
    int i;
    i = 0;
    while (i < 64) {
        wb_buf[i] = 0;
        i = i + 1;
    }
}

void wb_wr32(int off, int val) {
    wr32(wb_buf, off, val);
}

void wb_wr16(int off, int val) {
    wr16(wb_buf, off, val);
}

void wb_wr8(int off, int val) {
    wb_buf[off] = val & 255;
}

void fwrite_zeros(int f, int n) {
    int i;
    char zero;
    zero = 0;
    i = 0;
    while (i < n) {
        fputc(0, f);
        i = i + 1;
    }
}

/* === Output === */

void link_emit(char *out_path) {
    int out_nsec;
    int strtab_off;
    int data_off;
    int file_pos;
    int sec_text_foff;
    int sec_data_foff;
    int sec_rodata_foff;
    int pad;
    int f;
    int code_limit;
    int flags;

    /* Layout and resolve */
    layout_sections();
    inject_linker_symbols();
    apply_relocations();
    find_entry();

    if (link_error) {
        fputs("error: link failed due to errors\n", stderr);
        return;
    }

    out_nsec = count_sections();
    build_strtab();

    /* Compute file layout offsets */
    strtab_off = S32X_HDR_SZ + out_nsec * S32X_SEC_SZ;
    data_off = align16(strtab_off + out_strtab_sz);

    /* Compute file offsets for section data */
    file_pos = data_off;
    if (text_sz > 0) {
        sec_text_foff = file_pos;
        file_pos = align4(file_pos + text_sz);
    } else {
        sec_text_foff = 0;
    }
    if (data_sz > 0) {
        sec_data_foff = file_pos;
        file_pos = align4(file_pos + data_sz);
    } else {
        sec_data_foff = 0;
    }
    if (rodata_sz > 0) {
        sec_rodata_foff = file_pos;
    } else {
        sec_rodata_foff = 0;
    }

    /* Open output file */
    f = fopen(out_path, "wb");
    if (!f) {
        fputs("error: cannot create output: ", stderr);
        fputs(out_path, stderr);
        fputc('\n', stderr);
        link_error = 1;
        return;
    }

    /* Write 64-byte header */
    wb_init();
    wb_wr32(0, S32X_MAGIC);
    wb_wr16(4, 1);               /* version */
    wb_wr8(6, S32_ENDIAN);       /* endian */
    wb_wr8(7, S32_MACHINE);      /* machine */
    wb_wr32(8, entry_pt);        /* entry */
    wb_wr32(12, out_nsec);       /* nsections */
    wb_wr32(16, S32X_HDR_SZ);    /* sec_offset */
    wb_wr32(20, strtab_off);     /* str_offset */
    wb_wr32(24, out_strtab_sz);  /* str_size */

    /* flags */
    flags = S32X_FLAG_WXORX;
    if (mmio_size_arg > 0) flags = flags | S32X_FLAG_MMIO;
    wb_wr32(28, flags);

    code_limit = page_align(text_sz);
    if (code_limit < CODE_LIMIT_MIN) code_limit = CODE_LIMIT_MIN;
    wb_wr32(32, code_limit);                                    /* code_limit */
    wb_wr32(36, page_align(rodata_va + rodata_sz));            /* rodata_limit */
    wb_wr32(40, bss_end_va);                                    /* data_limit */
    wb_wr32(44, STACK_BASE);                                    /* stack_base */
    wb_wr32(48, MEM_SIZE);                                      /* mem_size */
    wb_wr32(52, heap_va);                                       /* heap_base */
    wb_wr32(56, STACK_BASE - STACK_SIZE);                       /* stack_end */
    wb_wr32(60, mmio_va);                                       /* mmio_base */
    fwrite(wb_buf, 1, 64, f);

    /* Write section table entries */
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

    /* Write string table */
    fwrite(out_strtab, 1, out_strtab_sz, f);

    /* Pad to 16 bytes */
    file_pos = strtab_off + out_strtab_sz;
    pad = align16(file_pos) - file_pos;
    if (pad > 0) fwrite_zeros(f, pad);

    /* Write section data */
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

    fputs("Link complete.\n", stderr);
}

/* === Parse --mmio argument === */

int parse_mmio_size(char *arg) {
    int val;
    int len;
    int mult;
    char last;

    len = strlen(arg);
    if (len == 0) return 0;

    last = arg[len - 1];
    mult = 1;
    if (last == 'K' || last == 'k') {
        mult = 1024;
        /* Parse without the suffix */
        arg[len - 1] = 0;
    } else if (last == 'M' || last == 'm') {
        mult = 1048576;
        arg[len - 1] = 0;
    }

    val = 0;
    while (*arg) {
        if (*arg < '0' || *arg > '9') return 0;
        val = val * 10 + (*arg - '0');
        arg = arg + 1;
    }
    return val * mult;
}

/* === Main === */

/* Detect file type by reading first 4 bytes */
int detect_file_type(char *path) {
    int f;
    char hdr[4];
    int magic;
    f = fopen(path, "rb");
    if (!f) return 0;
    if (fread(hdr, 1, 4, f) != 4) {
        fclose(f);
        return 0;
    }
    fclose(f);
    magic = (hdr[0] & 255) | ((hdr[1] & 255) << 8) |
            ((hdr[2] & 255) << 16) | ((hdr[3] & 255) << 24);
    if (magic == S32A_MAGIC) return 2;  /* archive */
    if (magic == S32O_MAGIC) return 1;  /* object */
    return 0;
}

int main(int argc, char **argv) {
    char *out_path;
    int i;
    int ftype;

    out_path = NULL;
    mmio_size_arg = 0;
    link_error = 0;

    /* Initialize state */
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
    mmio_sz = 0;
    mmio_va = 0;
    entry_pt = 0;

    ostrtab_init();

    if (argc < 3) {
        fputs("Usage: s32-ld-port -o output.s32x [--mmio SIZE] file1 file2 ...\n", stderr);
        return 1;
    }

    /* Parse arguments */
    i = 1;
    while (i < argc) {
        if (streq(argv[i], "-o")) {
            i = i + 1;
            if (i >= argc) {
                fputs("error: -o requires an argument\n", stderr);
                return 1;
            }
            out_path = argv[i];
        } else if (streq(argv[i], "--mmio")) {
            i = i + 1;
            if (i >= argc) {
                fputs("error: --mmio requires an argument\n", stderr);
                return 1;
            }
            mmio_size_arg = parse_mmio_size(argv[i]);
        } else {
            /* Input file -- process it */
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
        i = i + 1;
    }

    if (out_path == NULL) {
        fputs("error: no output file specified (-o)\n", stderr);
        return 1;
    }

    link_emit(out_path);

    if (link_error) return 1;
    return 0;
}
