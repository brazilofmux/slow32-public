/* obj_writer.h — ELF64 relocatable object file writer (AArch64 target).
 *
 * Mirror of stage07-cross/obj_writer.h.
 *
 * Differences from the x86-64 sibling:
 *   - e_machine = EM_AARCH64 (183)
 *   - Each call/address patch has an explicit cg_cpatch_kind[] entry;
 *     the AArch64 codegen has many distinct relocation forms
 *     (CALL26, ADR_PG_HI21, ADD_LO12, LDST{8,16,32,64}_LO12) and we
 *     can't infer them from the offset's sign the way the x64 writer
 *     does (negative=ABS64 movabs vs positive=PC32 call).
 *   - 8-byte data-section pointer relocations use R_AARCH64_ABS64.
 *
 * Must be included AFTER the codegen header — uses its globals
 * (a64_buf, a64_off, cg_*).
 */

#ifndef OBJ_WRITER_H
#define OBJ_WRITER_H

/* ============================================================================
 * ELF64 constants
 * ============================================================================ */

#define OBJ_ET_REL       1

#define OBJ_SHT_NULL     0
#define OBJ_SHT_PROGBITS 1
#define OBJ_SHT_SYMTAB   2
#define OBJ_SHT_STRTAB   3
#define OBJ_SHT_RELA     4
#define OBJ_SHT_NOBITS   8

#define OBJ_SHF_WRITE     1
#define OBJ_SHF_ALLOC     2
#define OBJ_SHF_EXECINSTR 4

#define OBJ_STB_LOCAL  0
#define OBJ_STB_GLOBAL 1

#define OBJ_STT_NOTYPE  0
#define OBJ_STT_OBJECT  1
#define OBJ_STT_FUNC    2
#define OBJ_STT_SECTION 3

/* AArch64 ELF relocation type numbers (from elf_aarch64.h / ABI doc) */
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

/* cg_cpatch_kind values: see a64_reloc_kinds.h. Included here for
 * standalone use of obj_writer in the smoke tests. */
#include "a64_reloc_kinds.h"

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

#define OBJ_STRTAB_SZ 65536
static char obj_strtab[OBJ_STRTAB_SZ];
static int  obj_strtab_len;

static char obj_shstrtab[512];
static int  obj_shstrtab_len;

/* Streamed write buffer */
static int  obj_fd;
static int  obj_pos;
static unsigned char obj_wbuf[4096];
static int  obj_wlen;

static void obj_flush(void) {
    int total; int n;
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
    obj_wbuf[obj_wlen] = (unsigned char)(b & 0xFF);
    obj_wlen = obj_wlen + 1;
    obj_pos = obj_pos + 1;
}

static void obj_half(int v)  { obj_byte(v & 0xFF); obj_byte((v >> 8) & 0xFF); }
static void obj_word(int v)  {
    obj_byte(v & 0xFF);
    obj_byte((v >> 8) & 0xFF);
    obj_byte((v >> 16) & 0xFF);
    obj_byte((v >> 24) & 0xFF);
}
static void obj_xword(int lo, int hi) { obj_word(lo); obj_word(hi); }

static void obj_write_bytes(unsigned char *buf, int len) {
    int i;
    i = 0;
    while (i < len) { obj_byte(buf[i]); i = i + 1; }
}

static void obj_write_strtab(char *buf, int len) {
    int i;
    i = 0;
    while (i < len) { obj_byte(buf[i]); i = i + 1; }
}

static void obj_pad_to(int target) {
    while (obj_pos < target) obj_byte(0);
}

/* ============================================================================
 * String tables
 * ============================================================================ */

static int obj_strtab_add(char *s) {
    int off; int i;
    off = obj_strtab_len;
    i = 0;
    while (s[i]) {
        if (obj_strtab_len < OBJ_STRTAB_SZ) obj_strtab[obj_strtab_len] = s[i];
        obj_strtab_len = obj_strtab_len + 1;
        i = i + 1;
    }
    if (obj_strtab_len < OBJ_STRTAB_SZ) obj_strtab[obj_strtab_len] = 0;
    obj_strtab_len = obj_strtab_len + 1;
    return off;
}

static int obj_shstrtab_add(char *s) {
    int off; int i;
    off = obj_shstrtab_len;
    i = 0;
    while (s[i]) {
        if (obj_shstrtab_len < 512) obj_shstrtab[obj_shstrtab_len] = s[i];
        obj_shstrtab_len = obj_shstrtab_len + 1;
        i = i + 1;
    }
    if (obj_shstrtab_len < 512) obj_shstrtab[obj_shstrtab_len] = 0;
    obj_shstrtab_len = obj_shstrtab_len + 1;
    return off;
}

/* ============================================================================
 * Symbol helpers
 * ============================================================================ */

static int obj_add_sym(char *name, int value, int shndx, int bind, int type) {
    int idx;
    idx = obj_nsyms;
    obj_sym_name[idx] = name;
    if (name != 0) obj_sym_stridx[idx] = obj_strtab_add(name);
    else           obj_sym_stridx[idx] = 0;
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
            if (cg_strcmp(name, obj_sym_name[i]) == 0) return i;
        }
        i = i + 1;
    }
    return -1;
}

static int obj_is_defined_func(char *name) {
    int i;
    i = 0;
    while (i < cg_nfuncs) {
        if (cg_strcmp(name, cg_func_name[i]) == 0) return 1;
        i = i + 1;
    }
    return 0;
}

/* ============================================================================
 * ELF emitters
 * ============================================================================ */

static int obj_align8(int v) { return (v + 7) & ~7; }

/* Map cg_cpatch_kind -> AArch64 ELF relocation type. */
static int obj_kind_to_rtype(int kind) {
    if (kind == A64K_CALL26)      return R_AARCH64_CALL26;
    if (kind == A64K_JUMP26)      return R_AARCH64_JUMP26;
    if (kind == A64K_ADR_HI21)    return R_AARCH64_ADR_PREL_PG_HI21;
    if (kind == A64K_ADD_LO12)    return R_AARCH64_ADD_ABS_LO12_NC;
    if (kind == A64K_LDST8_LO12)  return R_AARCH64_LDST8_ABS_LO12_NC;
    if (kind == A64K_LDST16_LO12) return R_AARCH64_LDST16_ABS_LO12_NC;
    if (kind == A64K_LDST32_LO12) return R_AARCH64_LDST32_ABS_LO12_NC;
    if (kind == A64K_LDST64_LO12) return R_AARCH64_LDST64_ABS_LO12_NC;
    return R_AARCH64_NONE;
}

static void obj_emit_ehdr(int shoff, int shnum, int shstrndx) {
    obj_byte(0x7F); obj_byte('E'); obj_byte('L'); obj_byte('F');
    obj_byte(2); obj_byte(1); obj_byte(1); obj_byte(0);
    obj_byte(0); obj_byte(0); obj_byte(0); obj_byte(0);
    obj_byte(0); obj_byte(0); obj_byte(0); obj_byte(0);

    obj_half(OBJ_ET_REL);
    obj_half(183);                  /* EM_AARCH64 */
    obj_word(1);                    /* e_version */
    obj_xword(0, 0);                /* e_entry */
    obj_xword(0, 0);                /* e_phoff */
    obj_xword(shoff, 0);
    obj_word(0);                    /* e_flags */
    obj_half(64);                   /* e_ehsize */
    obj_half(0);                    /* e_phentsize */
    obj_half(0);                    /* e_phnum */
    obj_half(64);                   /* e_shentsize */
    obj_half(shnum);
    obj_half(shstrndx);
}

static void obj_emit_shdr(int name, int type, int flags,
                          int offset, int size, int link, int info,
                          int addralign, int entsize) {
    obj_word(name);
    obj_word(type);
    obj_xword(flags, 0);
    obj_xword(0, 0);
    obj_xword(offset, 0);
    obj_xword(size, 0);
    obj_word(link);
    obj_word(info);
    obj_xword(addralign, 0);
    obj_xword(entsize, 0);
}

static void obj_emit_sym(int name, int info, int shndx, int value) {
    obj_word(name);
    obj_byte(info);
    obj_byte(0);
    obj_half(shndx);
    obj_xword(value, 0);
    obj_xword(0, 0);                /* st_size = 0 */
}

/* Emit one Elf64_Rela (24 bytes). r_info = (sym << 32) | type. */
static void obj_emit_rela(int offset, int sym_idx, int type, int addend) {
    int addend_hi;
    obj_xword(offset, 0);
    obj_word(type);                 /* r_info lo32 = type */
    obj_word(sym_idx);              /* r_info hi32 = sym idx */
    addend_hi = 0;
    if (addend < 0) addend_hi = -1;
    obj_xword(addend, addend_hi);
}

/* ============================================================================
 * Main: write ELF64 relocatable object
 * ============================================================================ */

static int obj_write_file(char *filename) {
    int i; int off; int sym_idx;
    int rela_text_count; int rela_data_count;

    int shn_text; int shn_rodata; int shn_data; int shn_bss;
    int shn_rela_text; int shn_rela_data;
    int shn_symtab; int shn_strtab; int shn_shstrtab;

    int off_text; int off_rodata; int off_data;
    int off_rela_text; int off_rela_data;
    int off_symtab; int off_strtab; int off_shstrtab; int off_shtable;

    int text_size; int rodata_size; int data_size; int bss_size;
    int rela_text_size; int rela_data_size; int symtab_size;

    /* ---- Step 1: .shstrtab ---- */
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

    /* ---- Step 2: symbol table ---- */
    obj_nsyms = 0;
    obj_strtab_len = 0;
    obj_strtab[0] = 0;
    obj_strtab_len = 1;

    obj_add_sym(0, 0, 0, OBJ_STB_LOCAL, OBJ_STT_NOTYPE);
    obj_add_sym(0, 0, OBJ_SEC_TEXT,   OBJ_STB_LOCAL, OBJ_STT_SECTION);
    obj_add_sym(0, 0, OBJ_SEC_RODATA, OBJ_STB_LOCAL, OBJ_STT_SECTION);
    obj_add_sym(0, 0, OBJ_SEC_DATA,   OBJ_STB_LOCAL, OBJ_STT_SECTION);
    obj_add_sym(0, 0, OBJ_SEC_BSS,    OBJ_STB_LOCAL, OBJ_STT_SECTION);

    /* ELF requires STB_LOCAL symbols before STB_GLOBAL.  Emit static
     * functions first, then mark the boundary. */
    i = 0;
    while (i < cg_nfuncs) {
        if (cg_func_local[i]) {
            obj_add_sym(cg_func_name[i], cg_func_off[i],
                        OBJ_SEC_TEXT, OBJ_STB_LOCAL, OBJ_STT_FUNC);
        }
        i = i + 1;
    }

    obj_first_global = obj_nsyms;

    i = 0;
    while (i < cg_nfuncs) {
        if (!cg_func_local[i]) {
            obj_add_sym(cg_func_name[i], cg_func_off[i],
                        OBJ_SEC_TEXT, OBJ_STB_GLOBAL, OBJ_STT_FUNC);
        }
        i = i + 1;
    }

    i = 0;
    while (i < cg_nglobals) {
        if (cg_glob_extern[i]) {
            obj_add_sym(cg_glob_name[i], 0, 0,
                        OBJ_STB_GLOBAL, OBJ_STT_OBJECT);
        } else {
            obj_add_sym(cg_glob_name[i], cg_glob_data_off[i],
                        cg_glob_in_bss[i] ? OBJ_SEC_BSS : OBJ_SEC_DATA,
                        OBJ_STB_GLOBAL, OBJ_STT_OBJECT);
        }
        i = i + 1;
    }

    i = 0;
    while (i < cg_ncpatches) {
        char *cn;
        cn = cg_cpatch_name[i];
        /* @@str is the magic string-pseudo-name — no symbol entry needed
         * (we'll relocate against the .rodata section symbol instead). */
        if (cg_strcmp(cn, "@@str") != 0) {
            if (!obj_is_defined_func(cn)) {
                if (obj_find_sym(cn) < 0)
                    obj_add_sym(cn, 0, 0, OBJ_STB_GLOBAL, OBJ_STT_NOTYPE);
            }
        }
        i = i + 1;
    }

    i = 0;
    while (i < cg_ndrelocs) {
        if (cg_dreloc_kind[i] == DRELOC_SYMBOL && cg_dreloc_name[i] != 0) {
            if (obj_find_sym(cg_dreloc_name[i]) < 0)
                obj_add_sym(cg_dreloc_name[i], 0, 0, OBJ_STB_GLOBAL, OBJ_STT_NOTYPE);
        }
        i = i + 1;
    }

    /* ---- Step 3: count relocations ----
     * Code-section relocations all live in cg_cpatch (CALL26, ADRP, ADD,
     * LDST*).  Data-section relocations come from cg_dreloc with off<0
     * (ABS64 fixups for string-init pointers in .data). */
    rela_text_count = cg_ncpatches;
    rela_data_count = 0;
    i = 0;
    while (i < cg_ndrelocs) {
        if (cg_dreloc_off[i] < 0) rela_data_count = rela_data_count + 1;
        i = i + 1;
    }

    /* ---- Step 4: section sizes & file layout ---- */
    text_size      = a64_off;
    rodata_size    = cg_rodata_len;
    data_size      = cg_data_len;
    bss_size       = cg_bss_size;
    rela_text_size = rela_text_count * 24;
    rela_data_size = rela_data_count * 24;
    symtab_size    = obj_nsyms * 24;

    off_text      = 64;
    off_rodata    = obj_align8(off_text + text_size);
    off_data      = obj_align8(off_rodata + rodata_size);
    off_rela_text = obj_align8(off_data + data_size);
    off_rela_data = obj_align8(off_rela_text + rela_text_size);
    off_symtab    = obj_align8(off_rela_data + rela_data_size);
    off_strtab    = off_symtab + symtab_size;
    off_shstrtab  = off_strtab + obj_strtab_len;
    off_shtable   = obj_align8(off_shstrtab + obj_shstrtab_len);

    /* ---- Step 5: open + write ---- */
    obj_fd = open(filename, 577, 420);  /* O_WRONLY|O_CREAT|O_TRUNC, 0644 */
    if (obj_fd < 0) return -1;

    obj_pos = 0;
    obj_wlen = 0;

    obj_emit_ehdr(off_shtable, OBJ_NUM_SECTIONS, OBJ_SEC_SHSTRTAB);

    obj_pad_to(off_text);
    if (text_size > 0) obj_write_bytes(a64_buf, text_size);

    obj_pad_to(off_rodata);
    if (rodata_size > 0) obj_write_bytes(cg_rodata, rodata_size);

    obj_pad_to(off_data);
    if (data_size > 0) obj_write_bytes(cg_data, data_size);

    /* ---- .rela.text ---- */
    obj_pad_to(off_rela_text);

    /* Code patches — one Rela per cg_cpatch entry. */
    i = 0;
    while (i < cg_ncpatches) {
        int kind; int rtype; int addend;
        char *cn;
        off    = cg_cpatch_off[i];
        cn     = cg_cpatch_name[i];
        kind   = cg_cpatch_kind[i];
        rtype  = obj_kind_to_rtype(kind);
        addend = cg_cpatch_addend[i];

        if (cg_strcmp(cn, "@@str") == 0) {
            /* String reference: relocate against .rodata section symbol with
             * addend = the string's offset into .rodata. */
            int str_idx;
            str_idx = addend;
            obj_emit_rela(off, OBJ_SEC_RODATA, rtype,
                          cg_str_rodata_off[str_idx]);
        } else {
            sym_idx = obj_find_sym(cn);
            if (sym_idx < 0) sym_idx = 0;
            obj_emit_rela(off, sym_idx, rtype, addend);
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
                obj_emit_rela(doff, OBJ_SEC_RODATA, R_AARCH64_ABS64,
                              cg_str_rodata_off[cg_dreloc_idx[i]]);
            } else if (cg_dreloc_kind[i] == DRELOC_SYMBOL) {
                int sym;
                sym = obj_find_sym(cg_dreloc_name[i]);
                if (sym < 0) sym = 0;
                obj_emit_rela(doff, sym, R_AARCH64_ABS64, 0);
            } else {
                int gidx; int gsec;
                gidx = cg_dreloc_idx[i];
                if (gidx < cg_nglobals && cg_glob_extern[gidx]) {
                    int sym;
                    sym = obj_find_sym(cg_glob_name[gidx]);
                    if (sym < 0) sym = 0;
                    obj_emit_rela(doff, sym, R_AARCH64_ABS64, 0);
                } else {
                    gsec = (gidx < cg_nglobals && cg_glob_in_bss[gidx]) ? OBJ_SEC_BSS : OBJ_SEC_DATA;
                    obj_emit_rela(doff, gsec, R_AARCH64_ABS64,
                                  cg_glob_data_off[gidx]);
                }
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

    /* ---- .strtab / .shstrtab ---- */
    obj_write_strtab(obj_strtab, obj_strtab_len);
    obj_write_strtab(obj_shstrtab, obj_shstrtab_len);

    /* ---- Section headers ---- */
    obj_pad_to(off_shtable);

    obj_emit_shdr(0, OBJ_SHT_NULL, 0, 0, 0, 0, 0, 0, 0);

    obj_emit_shdr(shn_text, OBJ_SHT_PROGBITS,
                  OBJ_SHF_ALLOC | OBJ_SHF_EXECINSTR,
                  off_text, text_size, 0, 0, 4, 0);

    obj_emit_shdr(shn_rodata, OBJ_SHT_PROGBITS,
                  OBJ_SHF_ALLOC,
                  off_rodata, rodata_size, 0, 0, 8, 0);

    obj_emit_shdr(shn_data, OBJ_SHT_PROGBITS,
                  OBJ_SHF_ALLOC | OBJ_SHF_WRITE,
                  off_data, data_size, 0, 0, 8, 0);

    obj_emit_shdr(shn_bss, OBJ_SHT_NOBITS,
                  OBJ_SHF_ALLOC | OBJ_SHF_WRITE,
                  off_data + data_size, bss_size, 0, 0, 8, 0);

    obj_emit_shdr(shn_rela_text, OBJ_SHT_RELA, 0,
                  off_rela_text, rela_text_size,
                  OBJ_SEC_SYMTAB, OBJ_SEC_TEXT, 8, 24);

    obj_emit_shdr(shn_rela_data, OBJ_SHT_RELA, 0,
                  off_rela_data, rela_data_size,
                  OBJ_SEC_SYMTAB, OBJ_SEC_DATA, 8, 24);

    obj_emit_shdr(shn_symtab, OBJ_SHT_SYMTAB, 0,
                  off_symtab, symtab_size,
                  OBJ_SEC_STRTAB, obj_first_global, 8, 24);

    obj_emit_shdr(shn_strtab, OBJ_SHT_STRTAB, 0,
                  off_strtab, obj_strtab_len, 0, 0, 1, 0);

    obj_emit_shdr(shn_shstrtab, OBJ_SHT_STRTAB, 0,
                  off_shstrtab, obj_shstrtab_len, 0, 0, 1, 0);

    obj_flush();
    close(obj_fd);
    return 0;
}

#endif /* OBJ_WRITER_H */
