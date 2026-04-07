/* obj_writer.h -- ELF64 relocatable object file writer
 *
 * Emits standard ELF64 relocatable objects (.o) from the codegen's
 * internal data structures (x64_buf, cg_cpatch, cg_dreloc, cg_func, etc.)
 *
 * Must be included AFTER codegen_x64.h (uses its globals).
 *
 * Usage:
 *   gen_program(prog);   // with cg_object_mode = 1
 *   obj_write_file("output.o");
 */

#ifndef OBJ_WRITER_H
#define OBJ_WRITER_H

/* ============================================================================
 * ELF64 constants for relocatable objects
 * ============================================================================ */

/* e_type */
#define OBJ_ET_REL       1

/* Section header types */
#define OBJ_SHT_NULL     0
#define OBJ_SHT_PROGBITS 1
#define OBJ_SHT_SYMTAB   2
#define OBJ_SHT_STRTAB   3
#define OBJ_SHT_RELA     4
#define OBJ_SHT_NOBITS   8

/* Section flags */
#define OBJ_SHF_WRITE     1
#define OBJ_SHF_ALLOC     2
#define OBJ_SHF_EXECINSTR 4

/* Symbol binding */
#define OBJ_STB_LOCAL  0
#define OBJ_STB_GLOBAL 1

/* Symbol type */
#define OBJ_STT_NOTYPE  0
#define OBJ_STT_OBJECT  1
#define OBJ_STT_FUNC    2
#define OBJ_STT_SECTION 3

/* x86-64 relocation types */
#define OBJ_R_X86_64_64   1
#define OBJ_R_X86_64_PC32 2

/* Fixed section indices */
#define OBJ_SEC_NULL      0
#define OBJ_SEC_TEXT      1
#define OBJ_SEC_RODATA    2
#define OBJ_SEC_DATA      3
#define OBJ_SEC_BSS       4
#define OBJ_SEC_RELA_TEXT 5
#define OBJ_SEC_RELA_DATA 6
#define OBJ_SEC_SYMTAB    7
#define OBJ_SEC_STRTAB    8
#define OBJ_SEC_SHSTRTAB  9
#define OBJ_NUM_SECTIONS  10

/* ============================================================================
 * Symbol table
 * ============================================================================ */

#define OBJ_MAX_SYMS 4096

static char *obj_sym_name[OBJ_MAX_SYMS];
static int   obj_sym_stridx[OBJ_MAX_SYMS];
static int   obj_sym_value[OBJ_MAX_SYMS];
static int   obj_sym_shndx[OBJ_MAX_SYMS];
static int   obj_sym_info[OBJ_MAX_SYMS];
static int   obj_nsyms;
static int   obj_first_global;

/* ============================================================================
 * String tables
 * ============================================================================ */

#define OBJ_STRTAB_SZ 65536

static char obj_strtab[OBJ_STRTAB_SZ];
static int  obj_strtab_len;

static char obj_shstrtab[512];
static int  obj_shstrtab_len;

/* ============================================================================
 * Write buffer — streams to fd through a small buffer
 * ============================================================================ */

static int obj_fd;
static int obj_pos;
static unsigned char obj_wbuf[4096];
static int obj_wlen;

static void obj_flush(void) {
    int total;
    int n;
    if (obj_wlen > 0) {
        total = 0;
        while (total < obj_wlen) {
            n = write(obj_fd, obj_wbuf + total, obj_wlen - total);
            if (n <= 0) break;
            total = total + n;
        }
        obj_wlen = 0;
    }
}

static void obj_byte(int b) {
    if (obj_wlen >= 4096) obj_flush();
    obj_wbuf[obj_wlen] = b & 0xFF;
    obj_wlen = obj_wlen + 1;
    obj_pos = obj_pos + 1;
}

static void obj_half(int v) {
    obj_byte(v & 0xFF);
    obj_byte((v >> 8) & 0xFF);
}

static void obj_word(int v) {
    obj_byte(v & 0xFF);
    obj_byte((v >> 8) & 0xFF);
    obj_byte((v >> 16) & 0xFF);
    obj_byte((v >> 24) & 0xFF);
}

static void obj_xword(int lo, int hi) {
    obj_word(lo);
    obj_word(hi);
}

static void obj_write_bytes(unsigned char *buf, int len) {
    int i;
    i = 0;
    while (i < len) {
        obj_byte(buf[i]);
        i = i + 1;
    }
}

static void obj_write_strtab(char *buf, int len) {
    int i;
    i = 0;
    while (i < len) {
        obj_byte(buf[i]);
        i = i + 1;
    }
}

static void obj_pad_to(int target) {
    while (obj_pos < target)
        obj_byte(0);
}

/* ============================================================================
 * String table helpers
 * ============================================================================ */

static int obj_strtab_add(char *s) {
    int off;
    int i;
    off = obj_strtab_len;
    i = 0;
    while (s[i]) {
        if (obj_strtab_len < OBJ_STRTAB_SZ)
            obj_strtab[obj_strtab_len] = s[i];
        obj_strtab_len = obj_strtab_len + 1;
        i = i + 1;
    }
    if (obj_strtab_len < OBJ_STRTAB_SZ)
        obj_strtab[obj_strtab_len] = 0;
    obj_strtab_len = obj_strtab_len + 1;
    return off;
}

static int obj_shstrtab_add(char *s) {
    int off;
    int i;
    off = obj_shstrtab_len;
    i = 0;
    while (s[i]) {
        if (obj_shstrtab_len < 512)
            obj_shstrtab[obj_shstrtab_len] = s[i];
        obj_shstrtab_len = obj_shstrtab_len + 1;
        i = i + 1;
    }
    if (obj_shstrtab_len < 512)
        obj_shstrtab[obj_shstrtab_len] = 0;
    obj_shstrtab_len = obj_shstrtab_len + 1;
    return off;
}

/* ============================================================================
 * Symbol table helpers
 * ============================================================================ */

static int obj_add_sym(char *name, int value, int shndx, int bind, int type) {
    int idx;
    idx = obj_nsyms;
    obj_sym_name[idx] = name;
    if (name != 0) {
        obj_sym_stridx[idx] = obj_strtab_add(name);
    } else {
        obj_sym_stridx[idx] = 0;
    }
    obj_sym_value[idx] = value;
    obj_sym_shndx[idx] = shndx;
    obj_sym_info[idx] = (bind << 4) | type;
    obj_nsyms = obj_nsyms + 1;
    return idx;
}

static int obj_find_sym(char *name) {
    int i;
    i = 0;
    while (i < obj_nsyms) {
        if (obj_sym_name[i] != 0) {
            if (cg_strcmp(name, obj_sym_name[i]) == 0)
                return i;
        }
        i = i + 1;
    }
    return -1;
}

static int obj_is_defined_func(char *name) {
    int i;
    i = 0;
    while (i < cg_nfuncs) {
        if (cg_strcmp(name, cg_func_name[i]) == 0)
            return 1;
        i = i + 1;
    }
    return 0;
}

/* ============================================================================
 * ELF structure emitters
 * ============================================================================ */

/* Round up to 8-byte alignment */
static int obj_align8(int v) {
    return (v + 7) & ~7;
}

/* Emit ELF64 header (64 bytes) */
static void obj_emit_ehdr(int shoff, int shnum, int shstrndx) {
    /* e_ident[16] */
    obj_byte(0x7F);
    obj_byte(69);   /* 'E' */
    obj_byte(76);   /* 'L' */
    obj_byte(70);   /* 'F' */
    obj_byte(2);    /* ELFCLASS64 */
    obj_byte(1);    /* ELFDATA2LSB */
    obj_byte(1);    /* EV_CURRENT */
    obj_byte(0);    /* ELFOSABI_NONE */
    obj_byte(0); obj_byte(0); obj_byte(0); obj_byte(0);
    obj_byte(0); obj_byte(0); obj_byte(0); obj_byte(0);

    obj_half(OBJ_ET_REL);     /* e_type */
    obj_half(62);              /* e_machine = EM_X86_64 */
    obj_word(1);               /* e_version */
    obj_xword(0, 0);          /* e_entry = 0 */
    obj_xword(0, 0);          /* e_phoff = 0 */
    obj_xword(shoff, 0);      /* e_shoff */
    obj_word(0);               /* e_flags */
    obj_half(64);              /* e_ehsize */
    obj_half(0);               /* e_phentsize */
    obj_half(0);               /* e_phnum */
    obj_half(64);              /* e_shentsize */
    obj_half(shnum);           /* e_shnum */
    obj_half(shstrndx);       /* e_shstrndx */
}

/* Emit one Elf64_Shdr (64 bytes) */
static void obj_emit_shdr(int name, int type, int flags,
                          int offset, int size, int link,
                          int info, int addralign, int entsize) {
    obj_word(name);            /* sh_name */
    obj_word(type);            /* sh_type */
    obj_xword(flags, 0);      /* sh_flags */
    obj_xword(0, 0);          /* sh_addr = 0 */
    obj_xword(offset, 0);     /* sh_offset */
    obj_xword(size, 0);       /* sh_size */
    obj_word(link);            /* sh_link */
    obj_word(info);            /* sh_info */
    obj_xword(addralign, 0);  /* sh_addralign */
    obj_xword(entsize, 0);    /* sh_entsize */
}

/* Emit one Elf64_Sym (24 bytes) */
static void obj_emit_sym(int name, int info, int shndx, int value) {
    obj_word(name);            /* st_name */
    obj_byte(info);            /* st_info */
    obj_byte(0);               /* st_other */
    obj_half(shndx);           /* st_shndx */
    obj_xword(value, 0);      /* st_value */
    obj_xword(0, 0);          /* st_size */
}

/* Emit one Elf64_Rela (24 bytes) */
static void obj_emit_rela(int offset, int sym_idx, int type, int addend) {
    int addend_hi;
    obj_xword(offset, 0);     /* r_offset */
    obj_word(type);            /* r_info lo32 = type */
    obj_word(sym_idx);         /* r_info hi32 = symbol index */
    addend_hi = 0;
    if (addend < 0) addend_hi = -1;
    obj_xword(addend, addend_hi);  /* r_addend */
}

/* ============================================================================
 * Main: write ELF64 relocatable object
 * ============================================================================ */

static int obj_write_file(char *filename) {
    int i;
    int off;
    int sym_idx;
    int rela_text_count;
    int rela_data_count;

    /* Section name offsets in .shstrtab */
    int shn_text;
    int shn_rodata;
    int shn_data;
    int shn_bss;
    int shn_rela_text;
    int shn_rela_data;
    int shn_symtab;
    int shn_strtab;
    int shn_shstrtab;

    /* File offsets for section data */
    int off_text;
    int off_rodata;
    int off_data;
    int off_rela_text;
    int off_rela_data;
    int off_symtab;
    int off_strtab;
    int off_shstrtab;
    int off_shtable;

    /* Section sizes */
    int text_size;
    int rodata_size;
    int data_size;
    int bss_size;
    int rela_text_size;
    int rela_data_size;
    int symtab_size;

    /* ================================================================
     * Step 1: Build .shstrtab (section name string table)
     * ================================================================ */
    obj_shstrtab_len = 0;
    obj_shstrtab[0] = 0;
    obj_shstrtab_len = 1;

    shn_text      = obj_shstrtab_add(".text");
    shn_rodata    = obj_shstrtab_add(".rodata");
    shn_data      = obj_shstrtab_add(".data");
    shn_bss       = obj_shstrtab_add(".bss");
    shn_rela_text = obj_shstrtab_add(".rela.text");
    shn_rela_data = obj_shstrtab_add(".rela.data");
    shn_symtab    = obj_shstrtab_add(".symtab");
    shn_strtab    = obj_shstrtab_add(".strtab");
    shn_shstrtab  = obj_shstrtab_add(".shstrtab");

    /* ================================================================
     * Step 2: Build symbol table
     * ================================================================ */
    obj_nsyms = 0;
    obj_strtab_len = 0;
    obj_strtab[0] = 0;
    obj_strtab_len = 1;

    /* [0] Null symbol (required) */
    obj_add_sym(0, 0, 0, OBJ_STB_LOCAL, OBJ_STT_NOTYPE);

    /* [1-4] Section symbols */
    obj_add_sym(0, 0, OBJ_SEC_TEXT,   OBJ_STB_LOCAL, OBJ_STT_SECTION);
    obj_add_sym(0, 0, OBJ_SEC_RODATA, OBJ_STB_LOCAL, OBJ_STT_SECTION);
    obj_add_sym(0, 0, OBJ_SEC_DATA,   OBJ_STB_LOCAL, OBJ_STT_SECTION);
    obj_add_sym(0, 0, OBJ_SEC_BSS,    OBJ_STB_LOCAL, OBJ_STT_SECTION);

    obj_first_global = obj_nsyms; /* = 5 */

    /* Defined functions → STT_FUNC in .text */
    i = 0;
    while (i < cg_nfuncs) {
        obj_add_sym(cg_func_name[i], cg_func_off[i],
                    OBJ_SEC_TEXT, OBJ_STB_GLOBAL, OBJ_STT_FUNC);
        i = i + 1;
    }

    /* Defined globals → STT_OBJECT in .data */
    i = 0;
    while (i < cg_nglobals) {
        obj_add_sym(cg_glob_name[i], cg_glob_data_off[i],
                    OBJ_SEC_DATA, OBJ_STB_GLOBAL, OBJ_STT_OBJECT);
        i = i + 1;
    }

    /* Undefined symbols from call patches (deduped) */
    i = 0;
    while (i < cg_ncpatches) {
        if (!obj_is_defined_func(cg_cpatch_name[i])) {
            if (obj_find_sym(cg_cpatch_name[i]) < 0) {
                obj_add_sym(cg_cpatch_name[i], 0, 0,
                            OBJ_STB_GLOBAL, OBJ_STT_NOTYPE);
            }
        }
        i = i + 1;
    }

    /* ================================================================
     * Step 3: Count relocations
     * ================================================================ */
    rela_text_count = 0;
    rela_data_count = 0;

    /* All call patches → .rela.text */
    rela_text_count = rela_text_count + cg_ncpatches;

    /* Data relocations: positive offset → .rela.text, negative → .rela.data */
    i = 0;
    while (i < cg_ndrelocs) {
        if (cg_dreloc_off[i] >= 0) {
            rela_text_count = rela_text_count + 1;
        } else {
            rela_data_count = rela_data_count + 1;
        }
        i = i + 1;
    }

    /* ================================================================
     * Step 4: Compute section sizes and file layout
     * ================================================================ */
    text_size      = x64_off;
    rodata_size    = cg_rodata_len;
    data_size      = cg_data_len;
    bss_size       = cg_bss_size;
    rela_text_size = rela_text_count * 24;
    rela_data_size = rela_data_count * 24;
    symtab_size    = obj_nsyms * 24;

    off_text      = 64;  /* right after ELF header */
    off_rodata    = obj_align8(off_text + text_size);
    off_data      = obj_align8(off_rodata + rodata_size);
    off_rela_text = obj_align8(off_data + data_size);
    off_rela_data = obj_align8(off_rela_text + rela_text_size);
    off_symtab    = obj_align8(off_rela_data + rela_data_size);
    off_strtab    = off_symtab + symtab_size;
    off_shstrtab  = off_strtab + obj_strtab_len;
    off_shtable   = obj_align8(off_shstrtab + obj_shstrtab_len);

    /* ================================================================
     * Step 5: Open file and write
     * ================================================================ */
    obj_fd = open(filename, 577, 420);  /* O_WRONLY|O_CREAT|O_TRUNC, 0644 */
    if (obj_fd < 0) return -1;

    obj_pos = 0;
    obj_wlen = 0;

    /* ---- ELF header ---- */
    obj_emit_ehdr(off_shtable, OBJ_NUM_SECTIONS, OBJ_SEC_SHSTRTAB);

    /* ---- .text ---- */
    obj_pad_to(off_text);
    if (text_size > 0)
        obj_write_bytes(x64_buf, text_size);

    /* ---- .rodata ---- */
    obj_pad_to(off_rodata);
    if (rodata_size > 0)
        obj_write_bytes(cg_rodata, rodata_size);

    /* ---- .data ---- */
    obj_pad_to(off_data);
    if (data_size > 0)
        obj_write_bytes(cg_data, data_size);

    /* ---- .rela.text ---- */
    obj_pad_to(off_rela_text);

    /* Call patches → R_X86_64_PC32 or R_X86_64_64 */
    i = 0;
    while (i < cg_ncpatches) {
        off = cg_cpatch_off[i];
        sym_idx = obj_find_sym(cg_cpatch_name[i]);
        if (sym_idx < 0) sym_idx = 0;

        if (off >= 0) {
            /* CALL rel32: offset is position of the 4-byte displacement */
            obj_emit_rela(off, sym_idx, OBJ_R_X86_64_PC32, -4);
        } else {
            /* MOVABS imm64: offset = -(off+1) is position of 8-byte immediate */
            obj_emit_rela(-(off + 1), sym_idx, OBJ_R_X86_64_64, 0);
        }
        i = i + 1;
    }

    /* Code-section data relocations → R_X86_64_64 */
    i = 0;
    while (i < cg_ndrelocs) {
        off = cg_dreloc_off[i];
        if (off >= 0) {
            if (cg_dreloc_kind[i] == DRELOC_STRING) {
                /* .rodata section symbol (index 2) + offset within rodata */
                obj_emit_rela(off, OBJ_SEC_RODATA, OBJ_R_X86_64_64,
                              cg_str_rodata_off[cg_dreloc_idx[i]]);
            } else {
                /* .data section symbol (index 3) + offset within data */
                obj_emit_rela(off, OBJ_SEC_DATA, OBJ_R_X86_64_64,
                              cg_glob_data_off[cg_dreloc_idx[i]]);
            }
        }
        i = i + 1;
    }

    /* ---- .rela.data ---- */
    obj_pad_to(off_rela_data);

    i = 0;
    while (i < cg_ndrelocs) {
        off = cg_dreloc_off[i];
        if (off < 0) {
            int doff;
            doff = -(off + 1);
            if (cg_dreloc_kind[i] == DRELOC_STRING) {
                obj_emit_rela(doff, OBJ_SEC_RODATA, OBJ_R_X86_64_64,
                              cg_str_rodata_off[cg_dreloc_idx[i]]);
            } else {
                obj_emit_rela(doff, OBJ_SEC_DATA, OBJ_R_X86_64_64,
                              cg_glob_data_off[cg_dreloc_idx[i]]);
            }
        }
        i = i + 1;
    }

    /* ---- .symtab ---- */
    obj_pad_to(off_symtab);
    i = 0;
    while (i < obj_nsyms) {
        obj_emit_sym(obj_sym_stridx[i], obj_sym_info[i],
                     obj_sym_shndx[i], obj_sym_value[i]);
        i = i + 1;
    }

    /* ---- .strtab ---- */
    obj_write_strtab(obj_strtab, obj_strtab_len);

    /* ---- .shstrtab ---- */
    obj_write_strtab(obj_shstrtab, obj_shstrtab_len);

    /* ---- Section header table ---- */
    obj_pad_to(off_shtable);

    /* [0] SHT_NULL */
    obj_emit_shdr(0, OBJ_SHT_NULL, 0, 0, 0, 0, 0, 0, 0);

    /* [1] .text */
    obj_emit_shdr(shn_text, OBJ_SHT_PROGBITS,
                  OBJ_SHF_ALLOC | OBJ_SHF_EXECINSTR,
                  off_text, text_size, 0, 0, 16, 0);

    /* [2] .rodata */
    obj_emit_shdr(shn_rodata, OBJ_SHT_PROGBITS,
                  OBJ_SHF_ALLOC,
                  off_rodata, rodata_size, 0, 0, 8, 0);

    /* [3] .data */
    obj_emit_shdr(shn_data, OBJ_SHT_PROGBITS,
                  OBJ_SHF_ALLOC | OBJ_SHF_WRITE,
                  off_data, data_size, 0, 0, 8, 0);

    /* [4] .bss */
    obj_emit_shdr(shn_bss, OBJ_SHT_NOBITS,
                  OBJ_SHF_ALLOC | OBJ_SHF_WRITE,
                  off_data + data_size, bss_size, 0, 0, 8, 0);

    /* [5] .rela.text: sh_link=symtab(7), sh_info=.text(1) */
    obj_emit_shdr(shn_rela_text, OBJ_SHT_RELA, 0,
                  off_rela_text, rela_text_size,
                  OBJ_SEC_SYMTAB, OBJ_SEC_TEXT, 8, 24);

    /* [6] .rela.data: sh_link=symtab(7), sh_info=.data(3) */
    obj_emit_shdr(shn_rela_data, OBJ_SHT_RELA, 0,
                  off_rela_data, rela_data_size,
                  OBJ_SEC_SYMTAB, OBJ_SEC_DATA, 8, 24);

    /* [7] .symtab: sh_link=strtab(8), sh_info=first_global */
    obj_emit_shdr(shn_symtab, OBJ_SHT_SYMTAB, 0,
                  off_symtab, symtab_size,
                  OBJ_SEC_STRTAB, obj_first_global, 8, 24);

    /* [8] .strtab */
    obj_emit_shdr(shn_strtab, OBJ_SHT_STRTAB, 0,
                  off_strtab, obj_strtab_len, 0, 0, 1, 0);

    /* [9] .shstrtab */
    obj_emit_shdr(shn_shstrtab, OBJ_SHT_STRTAB, 0,
                  off_shstrtab, obj_shstrtab_len, 0, 0, 1, 0);

    /* ---- Done ---- */
    obj_flush();
    close(obj_fd);
    return 0;
}

#endif /* OBJ_WRITER_H */
