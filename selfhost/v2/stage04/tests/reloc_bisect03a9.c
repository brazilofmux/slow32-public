#define REL_BISECT_LEVEL 3
#define REL_BISECT_LO12_SCAN 0
#define REL_BISECT_LO12_PACK 0
#define REL_BISECT_LO12_STYLE 9
/*
 * Stage07 bootstrap linker spike
 *
 * Bounded linker for one .s32o input and one .s32x output.
 * Current scope:
 *   - Supports .text/.data/.rodata/.bss merge from a single object
 *   - Supports bounded REL_32 relocations within a single object
 *   - Ignores archives for now (next widening step)
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#ifndef REL_BISECT_LEVEL
#define REL_BISECT_LEVEL 1
#endif

#ifndef REL_BISECT_LO12_SCAN
#define REL_BISECT_LO12_SCAN 1
#endif

#ifndef REL_BISECT_LO12_PACK
#define REL_BISECT_LO12_PACK 1
#endif

#ifndef REL_BISECT_LO12_PACK_MODE
#define REL_BISECT_LO12_PACK_MODE 2
#endif

#ifndef REL_BISECT_LO12_STYLE
#define REL_BISECT_LO12_STYLE 1
#endif

#ifndef REL_BISECT_FAKE_LO12_CASE
#define REL_BISECT_FAKE_LO12_CASE 0
#endif

#define S32O_MAGIC 0x5333324FU
#define S32X_MAGIC 0x53333258U

#define S32_ENDIAN_LITTLE 0x01
#define S32_MACHINE_SLOW32 0x32

#define S32_SEC_CODE   0x0001
#define S32_SEC_DATA   0x0002
#define S32_SEC_BSS    0x0003
#define S32_SEC_RODATA 0x0004

#define S32_SEC_FLAG_EXEC  0x0001
#define S32_SEC_FLAG_WRITE 0x0002
#define S32_SEC_FLAG_READ  0x0004
#define S32_SEC_FLAG_ALLOC 0x0008

#define S32X_FLAG_W_XOR_X 0x0001
#define S32O_REL_32 0x0001
#define S32O_REL_HI20   0x0002
#define S32O_REL_LO12   0x0003
#define S32O_REL_BRANCH 0x0004
#define S32O_REL_JAL    0x0005

#define MAX_OBJ_SIZE 262144
#define MAX_SECTIONS 16
#define MAX_SYMBOLS  1024
#define MAX_OUT_SIZE 524288

typedef struct {
    uint32_t magic;
    uint16_t version;
    uint8_t endian;
    uint8_t machine;
    uint32_t flags;
    uint32_t nsections;
    uint32_t sec_offset;
    uint32_t nsymbols;
    uint32_t sym_offset;
    uint32_t str_offset;
    uint32_t str_size;
    uint32_t checksum;
} s32o_header_t;

typedef struct {
    uint32_t name_offset;
    uint32_t type;
    uint32_t flags;
    uint32_t size;
    uint32_t offset;
    uint32_t align;
    uint32_t nrelocs;
    uint32_t reloc_offset;
} s32o_section_t;

typedef struct {
    uint32_t name_offset;
    uint32_t value;
    uint16_t section;
    uint8_t type;
    uint8_t binding;
    uint32_t size;
} s32o_symbol_t;

typedef struct {
    uint32_t offset;
    uint32_t symbol;
    uint32_t type;
    int32_t addend;
} s32o_reloc_t;

typedef struct {
    uint32_t magic;
    uint16_t version;
    uint8_t endian;
    uint8_t machine;
    uint32_t entry;
    uint32_t nsections;
    uint32_t sec_offset;
    uint32_t str_offset;
    uint32_t str_size;
    uint32_t flags;
    uint32_t code_limit;
    uint32_t rodata_limit;
    uint32_t data_limit;
    uint32_t stack_base;
    uint32_t mem_size;
    uint32_t heap_base;
    uint32_t stack_end;
    uint32_t mmio_base;
} s32x_header_t;

typedef struct {
    uint32_t name_offset;
    uint32_t type;
    uint32_t vaddr;
    uint32_t offset;
    uint32_t size;
    uint32_t mem_size;
    uint32_t flags;
} s32x_section_t;

typedef struct {
    int present;
    uint32_t idx;
    uint32_t size;
    uint32_t offset;
} in_sec_t;

static uint8_t g_obj[MAX_OBJ_SIZE];
static uint8_t g_out[MAX_OUT_SIZE];

static uint32_t align4(uint32_t n) {
    return (n + 3U) & ~3U;
}

static uint32_t align16(uint32_t n) {
    return (n + 15U) & ~15U;
}

static uint32_t page_align(uint32_t n) {
    return (n + 4095U) & ~4095U;
}

static uint32_t rd32(const uint8_t *p) {
    uint32_t v;
    v = (uint32_t)p[0];
    v |= ((uint32_t)p[1] << 8);
    v |= ((uint32_t)p[2] << 16);
    v |= ((uint32_t)p[3] << 24);
    return v;
}

static void wr32(uint8_t *p, uint32_t v) {
    p[0] = (uint8_t)(v & 255u);
    p[1] = (uint8_t)((v >> 8) & 255u);
    p[2] = (uint8_t)((v >> 16) & 255u);
    p[3] = (uint8_t)((v >> 24) & 255u);
}

static int read_file(const char *path, uint8_t *buf, uint32_t max_size, uint32_t *out_size) {
    FILE *f;
    long sz;

    f = fopen(path, "rb");
    if (!f) return 0;
    if (fseek(f, 0, SEEK_END) != 0) {
        fclose(f);
        return 0;
    }
    sz = ftell(f);
    if (sz < 0 || (uint32_t)sz > max_size) {
        fclose(f);
        return 0;
    }
    if (fseek(f, 0, SEEK_SET) != 0) {
        fclose(f);
        return 0;
    }
    if ((uint32_t)sz > 0) {
        if (fread(buf, 1, (uint32_t)sz, f) != (uint32_t)sz) {
            fclose(f);
            return 0;
        }
    }
    fclose(f);
    *out_size = (uint32_t)sz;
    return 1;
}

static int write_file(const char *path, const uint8_t *buf, uint32_t size) {
    FILE *f;
    f = fopen(path, "wb");
    if (!f) return 0;
    if (size > 0 && fwrite(buf, 1, size, f) != size) {
        fclose(f);
        return 0;
    }
    fclose(f);
    return 1;
}

static int in_bounds(uint32_t off, uint32_t size, uint32_t total) {
    if (off > total) return 0;
    if (size > total - off) return 0;
    return 1;
}

static int str_eq(const char *a, const char *b) {
    return strcmp(a, b) == 0;
}

int main(int argc, char **argv) {
    s32o_header_t *oh;
    s32o_section_t *osec;
    s32o_symbol_t *osym;
    const char *ostr;
    uint32_t obj_size;
    uint32_t i;

    in_sec_t text;
    in_sec_t data;
    in_sec_t rodata;
    in_sec_t bss;

    uint32_t text_va;
    uint32_t rodata_va;
    uint32_t data_va;
    uint32_t bss_va;
    uint32_t bss_end;

    uint32_t out_str_size;
    uint32_t out_nsec;
    uint32_t out_data_off;
    uint32_t text_out_off;
    uint32_t data_out_off;
    uint32_t rodata_out_off;
    uint32_t cur;
    uint32_t out_size;

    s32x_header_t *xh;
    s32x_section_t *xsec;
    int main_found;

    if (argc != 3) {
        fprintf(stderr, "Usage: %s <input.s32o> <output.s32x>\n", argv[0]);
        return 1;
    }

    if (!read_file(argv[1], g_obj, MAX_OBJ_SIZE, &obj_size)) {
        fprintf(stderr, "error: cannot read %s\n", argv[1]);
        return 1;
    }
    if (obj_size < sizeof(s32o_header_t)) {
        fprintf(stderr, "error: object too small\n");
        return 1;
    }

    oh = (s32o_header_t *)g_obj;
    if (oh->magic != S32O_MAGIC) {
        fprintf(stderr, "error: bad .s32o magic\n");
        return 1;
    }
    if (oh->endian != S32_ENDIAN_LITTLE || oh->machine != S32_MACHINE_SLOW32) {
        fprintf(stderr, "error: unsupported object target\n");
        return 1;
    }
    if (oh->nsections > MAX_SECTIONS || oh->nsymbols > MAX_SYMBOLS) {
        fprintf(stderr, "error: object exceeds stage07 bounds\n");
        return 1;
    }
    if (!in_bounds(oh->sec_offset, oh->nsections * (uint32_t)sizeof(s32o_section_t), obj_size) ||
        !in_bounds(oh->sym_offset, oh->nsymbols * (uint32_t)sizeof(s32o_symbol_t), obj_size) ||
        !in_bounds(oh->str_offset, oh->str_size, obj_size)) {
        fprintf(stderr, "error: object tables out of bounds\n");
        return 1;
    }

    osec = (s32o_section_t *)(g_obj + oh->sec_offset);
    osym = (s32o_symbol_t *)(g_obj + oh->sym_offset);
    ostr = (const char *)(g_obj + oh->str_offset);

    text.present = 0;
    text.idx = 0;
    text.size = 0;
    text.offset = 0;
    data.present = 0;
    data.idx = 0;
    data.size = 0;
    data.offset = 0;
    rodata.present = 0;
    rodata.idx = 0;
    rodata.size = 0;
    rodata.offset = 0;
    bss.present = 0;
    bss.idx = 0;
    bss.size = 0;
    bss.offset = 0;

    for (i = 0; i < oh->nsections; i = i + 1) {
        s32o_section_t *s = &osec[i];
        if (s->nrelocs > 0) {
            if (!in_bounds(s->reloc_offset,
                           s->nrelocs * (uint32_t)sizeof(s32o_reloc_t),
                           obj_size)) {
                fprintf(stderr, "error: relocation table out of bounds (section %u)\n", i);
                return 1;
            }
        }

        if (s->type == S32_SEC_CODE) {
            if (text.present) {
                fprintf(stderr, "error: multiple .text sections not supported\n");
                return 1;
            }
            if (!in_bounds(s->offset, s->size, obj_size)) {
                fprintf(stderr, "error: .text out of bounds\n");
                return 1;
            }
            text.present = 1;
            text.idx = i;
            text.size = s->size;
            text.offset = s->offset;
        } else if (s->type == S32_SEC_DATA) {
            if (data.present) {
                fprintf(stderr, "error: multiple .data sections not supported\n");
                return 1;
            }
            if (!in_bounds(s->offset, s->size, obj_size)) {
                fprintf(stderr, "error: .data out of bounds\n");
                return 1;
            }
            data.present = 1;
            data.idx = i;
            data.size = s->size;
            data.offset = s->offset;
        } else if (s->type == S32_SEC_RODATA) {
            if (rodata.present) {
                fprintf(stderr, "error: multiple .rodata sections not supported\n");
                return 1;
            }
            if (!in_bounds(s->offset, s->size, obj_size)) {
                fprintf(stderr, "error: .rodata out of bounds\n");
                return 1;
            }
            rodata.present = 1;
            rodata.idx = i;
            rodata.size = s->size;
            rodata.offset = s->offset;
        } else if (s->type == S32_SEC_BSS) {
            if (bss.present) {
                fprintf(stderr, "error: multiple .bss sections not supported\n");
                return 1;
            }
            bss.present = 1;
            bss.idx = i;
            bss.size = s->size;
            bss.offset = 0;
        }
    }

    if (!text.present || text.size == 0) {
        fprintf(stderr, "error: missing .text section\n");
        return 1;
    }

    main_found = 0;
    for (i = 0; i < oh->nsymbols; i = i + 1) {
        const char *nm;
        uint16_t sec;
        if (osym[i].name_offset >= oh->str_size) continue;
        nm = ostr + osym[i].name_offset;
        sec = osym[i].section;
        if (sec == 0 || sec > oh->nsections) continue;
        if (str_eq(nm, "main")) {
            if (osec[sec - 1].type == S32_SEC_CODE) {
                main_found = 1;
                break;
            }
        }
    }
    if (!main_found) {
        fprintf(stderr, "error: main symbol not found in .text\n");
        return 1;
    }

    text_va = 0;
    rodata_va = align4(text_va + text.size);
    data_va = align4(rodata_va + rodata.size);
    bss_va = align4(data_va + data.size);
    bss_end = bss_va + bss.size;

    out_str_size = 24; /* ".text\0.data\0.bss\0.rodata\0" */
    out_nsec = 1;
    if (data.size > 0) out_nsec = out_nsec + 1;
    if (bss.size > 0) out_nsec = out_nsec + 1;
    if (rodata.size > 0) out_nsec = out_nsec + 1;

    out_data_off = (uint32_t)sizeof(s32x_header_t) + out_nsec * (uint32_t)sizeof(s32x_section_t) + out_str_size;
    out_data_off = align4(out_data_off);

    out_size = out_data_off + text.size + data.size + rodata.size;
    if (out_size > MAX_OUT_SIZE) {
        fprintf(stderr, "error: output exceeds stage07 bounds\n");
        return 1;
    }

    memset(g_out, 0, out_size);

    xh = (s32x_header_t *)g_out;
    xsec = (s32x_section_t *)(g_out + sizeof(s32x_header_t));

    xh->magic = S32X_MAGIC;
    xh->version = 1;
    xh->endian = S32_ENDIAN_LITTLE;
    xh->machine = S32_MACHINE_SLOW32;
    xh->entry = 0;
    xh->nsections = out_nsec;
    xh->sec_offset = (uint32_t)sizeof(s32x_header_t);
    xh->str_offset = (uint32_t)sizeof(s32x_header_t) + out_nsec * (uint32_t)sizeof(s32x_section_t);
    xh->str_size = out_str_size;
    xh->flags = S32X_FLAG_W_XOR_X;

    xh->code_limit = page_align(text.size);
    if (xh->code_limit < 4096U) xh->code_limit = 4096U;
    xh->rodata_limit = page_align(rodata_va + rodata.size);
    if (xh->rodata_limit < xh->code_limit) xh->rodata_limit = xh->code_limit;
    xh->data_limit = align16(bss_end);

    xh->stack_base = 0x0FFFFFF0U;
    xh->mem_size = 0x10000000U;
    xh->heap_base = page_align(xh->data_limit + 4096U);
    xh->stack_end = xh->stack_base - 65536U;
    xh->mmio_base = 0;

    memcpy(g_out + xh->str_offset, ".text\0.data\0.bss\0.rodata\0", out_str_size);

    cur = out_data_off;
    text_out_off = cur;

    xsec[0].name_offset = 0;
    xsec[0].type = S32_SEC_CODE;
    xsec[0].vaddr = text_va;
    xsec[0].offset = cur;
    xsec[0].size = text.size;
    xsec[0].mem_size = text.size;
    xsec[0].flags = S32_SEC_FLAG_READ | S32_SEC_FLAG_EXEC | S32_SEC_FLAG_ALLOC;
    memcpy(g_out + cur, g_obj + text.offset, text.size);
    cur = cur + text.size;

    i = 1;
    data_out_off = 0;
    if (data.size > 0) {
        data_out_off = cur;
        xsec[i].name_offset = 6;
        xsec[i].type = S32_SEC_DATA;
        xsec[i].vaddr = data_va;
        xsec[i].offset = cur;
        xsec[i].size = data.size;
        xsec[i].mem_size = data.size;
        xsec[i].flags = S32_SEC_FLAG_READ | S32_SEC_FLAG_WRITE | S32_SEC_FLAG_ALLOC;
        memcpy(g_out + cur, g_obj + data.offset, data.size);
        cur = cur + data.size;
        i = i + 1;
    }
    if (bss.size > 0) {
        xsec[i].name_offset = 12;
        xsec[i].type = S32_SEC_BSS;
        xsec[i].vaddr = bss_va;
        xsec[i].offset = cur;
        xsec[i].size = 0;
        xsec[i].mem_size = bss.size;
        xsec[i].flags = S32_SEC_FLAG_READ | S32_SEC_FLAG_WRITE | S32_SEC_FLAG_ALLOC;
        i = i + 1;
    }
    rodata_out_off = 0;
    if (rodata.size > 0) {
        rodata_out_off = cur;
        xsec[i].name_offset = 17;
        xsec[i].type = S32_SEC_RODATA;
        xsec[i].vaddr = rodata_va;
        xsec[i].offset = cur;
        xsec[i].size = rodata.size;
        xsec[i].mem_size = rodata.size;
        xsec[i].flags = S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC;
        memcpy(g_out + cur, g_obj + rodata.offset, rodata.size);
        cur = cur + rodata.size;
    }

    for (i = 0; i < oh->nsections; i = i + 1) {
        s32o_section_t *s = &osec[i];
        uint32_t sec_va;
        uint32_t sec_out;
        uint32_t sec_size;
        uint32_t r;

        if (s->nrelocs == 0) continue;

        if (i == text.idx) {
            sec_va = text_va;
            sec_out = text_out_off;
            sec_size = text.size;
        } else if (data.present && i == data.idx) {
            sec_va = data_va;
            sec_out = data_out_off;
            sec_size = data.size;
        } else if (rodata.present && i == rodata.idx) {
            sec_va = rodata_va;
            sec_out = rodata_out_off;
            sec_size = rodata.size;
        } else {
            fprintf(stderr, "error: relocations in unsupported section %u\n", i);
            return 1;
        }

        for (r = 0; r < s->nrelocs; r = r + 1) {
            s32o_reloc_t *rel;
            uint32_t sym_sec;
            uint32_t sym_abs;
            uint32_t patched;

            rel = (s32o_reloc_t *)(g_obj + s->reloc_offset + r * (uint32_t)sizeof(s32o_reloc_t));
            if (rel->symbol >= oh->nsymbols) {
                fprintf(stderr, "error: relocation symbol index out of range\n");
                return 1;
            }
            if (rel->offset + 4u > sec_size) {
                fprintf(stderr, "error: relocation offset out of section bounds\n");
                return 1;
            }
            sym_sec = osym[rel->symbol].section;
            if (sym_sec == 0 || sym_sec > oh->nsections) {
                fprintf(stderr, "error: unresolved symbol in relocation\n");
                return 1;
            }
            sym_sec = sym_sec - 1u;
            if (sym_sec == text.idx) {
                if (osym[rel->symbol].value > text.size) return 1;
                sym_abs = text_va + osym[rel->symbol].value;
            } else if (data.present && sym_sec == data.idx) {
                if (osym[rel->symbol].value > data.size) return 1;
                sym_abs = data_va + osym[rel->symbol].value;
            } else if (rodata.present && sym_sec == rodata.idx) {
                if (osym[rel->symbol].value > rodata.size) return 1;
                sym_abs = rodata_va + osym[rel->symbol].value;
            } else if (bss.present && sym_sec == bss.idx) {
                if (osym[rel->symbol].value > bss.size) return 1;
                sym_abs = bss_va + osym[rel->symbol].value;
            } else {
                fprintf(stderr, "error: symbol section unsupported in relocation\n");
                return 1;
            }

            patched = sym_abs + (uint32_t)rel->addend;
            if (rel->type == S32O_REL_32) {
                wr32(g_out + sec_out + rel->offset, patched);
#if REL_BISECT_LEVEL >= 2
            } else if (rel->type == S32O_REL_HI20) {
                uint32_t inst;
                uint32_t lo12;
                uint32_t hi20;
                inst = rd32(g_out + sec_out + rel->offset);
                lo12 = patched & 0xFFFu;
                hi20 = (patched >> 12) & 0xFFFFFu;
                if ((lo12 & 0x800u) != 0u) hi20 = (hi20 + 1u) & 0xFFFFFu;
                inst = (inst & 0x00000FFFu) | (hi20 << 12);
                wr32(g_out + sec_out + rel->offset, inst);
#endif
#if REL_BISECT_LEVEL >= 3 || REL_BISECT_FAKE_LO12_CASE
            } else if (rel->type == S32O_REL_LO12) {
#if REL_BISECT_LEVEL >= 3
 #if REL_BISECT_LO12_STYLE == 9
                uint32_t out_inst;
                out_inst = patched;
                out_inst = out_inst + 0u;
                patched = out_inst;
 #elif REL_BISECT_LO12_STYLE == 8
                uint32_t out_inst;
                out_inst = (uint32_t)g_out[0];
                patched = out_inst;
 #elif REL_BISECT_LO12_STYLE == 7
                uint32_t out_inst;
                out_inst = rd32(g_out);
                patched = out_inst;
 #elif REL_BISECT_LO12_STYLE == 6
                uint32_t out_inst;
                uint32_t off2;
                off2 = sec_out + rel->offset;
                out_inst = rd32(g_out + off2);
                patched = out_inst;
 #elif REL_BISECT_LO12_STYLE == 5
                uint32_t out_inst;
                out_inst = rd32(g_out + sec_out + rel->offset);
                patched = out_inst;
 #elif REL_BISECT_LO12_STYLE == 4
                wr32(g_out + sec_out + rel->offset, patched);
 #elif REL_BISECT_LO12_STYLE == 2
                patched = patched;
 #elif REL_BISECT_LO12_STYLE == 0
                uint32_t out_inst;
                out_inst = rd32(g_out + sec_out + rel->offset);
                wr32(g_out + sec_out + rel->offset, out_inst);
 #else
                uint32_t out_inst;
                uint32_t inst;
                uint32_t opcode;
                uint32_t imm;
                int paired;
                uint32_t k;
                inst = rd32(g_out + sec_out + rel->offset);
                opcode = inst & 0x7Fu;
                imm = patched & 0xFFFu;
                paired = 0;
#if REL_BISECT_LO12_SCAN
                for (k = 0; k < s->nrelocs; k = k + 1) {
                    s32o_reloc_t *other;
                    other = (s32o_reloc_t *)(g_obj + s->reloc_offset + k * (uint32_t)sizeof(s32o_reloc_t));
                    if (other->type == S32O_REL_HI20 && other->symbol == rel->symbol) {
                        paired = 1;
                        break;
                    }
                }
                if (!paired) {
                    int32_t sv;
                    sv = (int32_t)patched;
                    if (sv < -2048 || sv > 2047) {
                        fprintf(stderr, "error: unpaired LO12 relocation out of range\n");
                        return 1;
                    }
                }
#else
                paired = 1;
#endif
#if REL_BISECT_LO12_PACK
 #if REL_BISECT_LO12_PACK_MODE == 1
                inst = (inst & 0x000FFFFFu) | (imm << 20);
 #elif REL_BISECT_LO12_PACK_MODE == 3
                inst = (inst & 0xFE000F80u)
                    | ((imm & 0x1Fu) << 7)
                    | (((imm >> 5) & 0x7Fu) << 25);
 #else
                if (opcode == 0x38u || opcode == 0x39u || opcode == 0x3Au) {
                    inst = (inst & 0xFE000F80u)
                        | ((imm & 0x1Fu) << 7)
                        | (((imm >> 5) & 0x7Fu) << 25);
                } else {
                    inst = (inst & 0x000FFFFFu) | (imm << 20);
                }
 #endif
#else
                patched = patched;
#endif
                out_inst = inst;
                wr32(g_out + sec_out + rel->offset, out_inst);
 #endif
 #endif
#endif
#if REL_BISECT_LEVEL >= 4
            } else if (rel->type == S32O_REL_BRANCH) {
                int32_t off;
                uint32_t pc;
                uint32_t inst;
                uint32_t imm12;
                uint32_t imm11;
                uint32_t imm10_5;
                uint32_t imm4_1;
                pc = sec_va + rel->offset;
                off = (int32_t)(patched - (pc + 4u));
                if ((off & 1) != 0 || off < -4096 || off > 4094) {
                    fprintf(stderr, "error: branch relocation out of range\n");
                    return 1;
                }
                imm12 = ((uint32_t)off >> 12) & 1u;
                imm11 = ((uint32_t)off >> 11) & 1u;
                imm10_5 = ((uint32_t)off >> 5) & 0x3Fu;
                imm4_1 = ((uint32_t)off >> 1) & 0xFu;
                inst = rd32(g_out + sec_out + rel->offset);
                inst = (inst & 0x01FFF07Fu)
                    | (imm11 << 7)
                    | (imm4_1 << 8)
                    | (imm10_5 << 25)
                    | (imm12 << 31);
                wr32(g_out + sec_out + rel->offset, inst);
            } else if (rel->type == S32O_REL_JAL) {
                int32_t off;
                uint32_t pc;
                uint32_t inst;
                uint32_t imm20;
                uint32_t imm19_12;
                uint32_t imm11;
                uint32_t imm10_1;
                pc = sec_va + rel->offset;
                off = (int32_t)(patched - pc);
                if ((off & 1) != 0 || off < -1048576 || off > 1048574) {
                    fprintf(stderr, "error: jal relocation out of range\n");
                    return 1;
                }
                imm20 = ((uint32_t)off >> 20) & 1u;
                imm19_12 = ((uint32_t)off >> 12) & 0xFFu;
                imm11 = ((uint32_t)off >> 11) & 1u;
                imm10_1 = ((uint32_t)off >> 1) & 0x3FFu;
                inst = rd32(g_out + sec_out + rel->offset);
                inst = (inst & 0x00000FFFu)
                    | (imm19_12 << 12)
                    | (imm11 << 20)
                    | (imm10_1 << 21)
                    | (imm20 << 31);
                wr32(g_out + sec_out + rel->offset, inst);
#endif
            } else {
                fprintf(stderr, "error: relocation type %u not yet supported\n", rel->type);
                return 1;
            }
        }
    }

    if (!write_file(argv[2], g_out, cur)) {
        fprintf(stderr, "error: cannot write %s\n", argv[2]);
        return 1;
    }

    printf("stage07: linked %s -> %s\n", argv[1], argv[2]);
    return 0;
}
