/*
 * Stage07 bootstrap linker spike
 *
 * Bounded linker for one or two .s32o inputs and one .s32x output.
 * Current scope:
 *   - Supports .text/.data/.rodata/.bss merge from a primary object
 *   - Optional bounded aux object path: append aux .text (no relocs)
 *   - Supports bounded REL_32 relocations within a single object
 *   - Ignores archives for now (next widening step)
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>

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
#define S32O_REL_HI20 0x0002
#define S32O_REL_LO12 0x0003
#define S32O_REL_BRANCH 0x0004
#define S32O_REL_JAL 0x0005

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
static uint8_t g_aux_obj[MAX_OBJ_SIZE];
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

static void wr32(uint8_t *p, uint32_t v) {
    p[0] = (uint8_t)(v & 255u);
    p[1] = (uint8_t)((v >> 8) & 255u);
    p[2] = (uint8_t)((v >> 16) & 255u);
    p[3] = (uint8_t)((v >> 24) & 255u);
}

static uint32_t rd32(const uint8_t *p) {
    return (uint32_t)p[0]
         | ((uint32_t)p[1] << 8)
         | ((uint32_t)p[2] << 16)
         | ((uint32_t)p[3] << 24);
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
    const char *obj_path;
    const char *aux_path;
    const char *out_path;
    s32o_header_t *oh;
    s32o_section_t *osec;
    s32o_symbol_t *osym;
    const char *ostr;
    s32o_header_t *aoh;
    s32o_section_t *asec;
    uint32_t aux_size;
    uint32_t obj_size;
    uint32_t i;
    uint32_t total_text;
    uint32_t aux_text_va;
    int aux_loaded;

    in_sec_t text;
    in_sec_t data;
    in_sec_t rodata;
    in_sec_t bss;
    in_sec_t atext;

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

    if (argc != 3 && argc != 4) {
        fprintf(stderr, "Usage: %s <input.s32o> [aux.s32o] <output.s32x>\n", argv[0]);
        return 1;
    }
    obj_path = argv[1];
    aux_path = NULL;
    out_path = argv[argc - 1];
    if (argc == 4) aux_path = argv[2];

    if (!read_file(obj_path, g_obj, MAX_OBJ_SIZE, &obj_size)) {
        fprintf(stderr, "error: cannot read %s\n", obj_path);
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
    atext.present = 0;
    atext.idx = 0;
    atext.size = 0;
    atext.offset = 0;
    aux_loaded = 0;

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

    if (aux_path != NULL) {
        if (!read_file(aux_path, g_aux_obj, MAX_OBJ_SIZE, &aux_size)) {
            fprintf(stderr, "error: cannot read %s\n", aux_path);
            return 1;
        }
        if (aux_size < sizeof(s32o_header_t)) return 1;
        aoh = (s32o_header_t *)g_aux_obj;
        if (aoh->magic != S32O_MAGIC ||
            aoh->endian != S32_ENDIAN_LITTLE ||
            aoh->machine != S32_MACHINE_SLOW32 ||
            aoh->nsections < 1 ||
            !in_bounds(aoh->sec_offset, (uint32_t)sizeof(s32o_section_t), aux_size)) return 1;
        asec = (s32o_section_t *)(g_aux_obj + aoh->sec_offset);
        if (asec[0].type != S32_SEC_CODE || asec[0].nrelocs != 0 ||
            !in_bounds(asec[0].offset, asec[0].size, aux_size) || asec[0].size == 0) return 1;
        atext.present = 1;
        atext.idx = 0;
        atext.size = asec[0].size;
        atext.offset = asec[0].offset;
        aux_loaded = 1;
    }

    total_text = text.size + (aux_loaded ? atext.size : 0u);
    text_va = 0;
    aux_text_va = text_va + text.size;
    rodata_va = align4(text_va + total_text);
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

    out_size = out_data_off + total_text + data.size + rodata.size;
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

    xh->code_limit = page_align(total_text);
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
    xsec[0].size = total_text;
    xsec[0].mem_size = total_text;
    xsec[0].flags = S32_SEC_FLAG_READ | S32_SEC_FLAG_EXEC | S32_SEC_FLAG_ALLOC;
    memcpy(g_out + cur, g_obj + text.offset, text.size);
    cur = cur + text.size;
    if (aux_loaded) {
        memcpy(g_out + cur, g_aux_obj + atext.offset, atext.size);
        cur = cur + atext.size;
    }

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
            if (sym_sec == 0) {
                if (!aux_loaded) {
                    fprintf(stderr, "error: unresolved symbol in relocation\n");
                    return 1;
                }
                sym_abs = aux_text_va;
            } else {
                if (sym_sec > oh->nsections) {
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
            }

            patched = sym_abs + (uint32_t)rel->addend;
            if (rel->type == S32O_REL_32) {
                wr32(g_out + sec_out + rel->offset, patched);
            } else if (rel->type == S32O_REL_HI20) {
                uint32_t inst = rd32(g_out + sec_out + rel->offset);
                uint32_t hi20 = (patched + 0x800U) >> 12;
                inst = (inst & 0x00000FFFU) | ((hi20 & 0x000FFFFFU) << 12);
                wr32(g_out + sec_out + rel->offset, inst);
            } else if (rel->type == S32O_REL_LO12) {
                uint32_t inst = rd32(g_out + sec_out + rel->offset);
                uint32_t opcode = inst & 0x7FU;
                uint32_t lo12 = patched & 0x00000FFFU;
                if (opcode == 0x38U || opcode == 0x39U || opcode == 0x3AU) {
                    inst = (inst & 0xFE000F80U)
                        | ((lo12 & 0x1FU) << 7)
                        | (((lo12 >> 5) & 0x7FU) << 25);
                } else {
                    inst = (inst & 0x000FFFFFU) | (lo12 << 20);
                }
                wr32(g_out + sec_out + rel->offset, inst);
            } else if (rel->type == S32O_REL_BRANCH) {
                int32_t off;
                uint32_t pc = sec_va + rel->offset;
                uint32_t inst;
                uint32_t imm12;
                uint32_t imm11;
                uint32_t imm10_5;
                uint32_t imm4_1;
                off = (int32_t)(patched - (pc + 4U));
                if ((off & 1) != 0 || off < -4096 || off > 4094) {
                    fprintf(stderr, "error: branch relocation out of range\n");
                    return 1;
                }
                imm12 = ((uint32_t)off >> 12) & 1U;
                imm11 = ((uint32_t)off >> 11) & 1U;
                imm10_5 = ((uint32_t)off >> 5) & 0x3FU;
                imm4_1 = ((uint32_t)off >> 1) & 0xFU;
                inst = rd32(g_out + sec_out + rel->offset);
                inst = (inst & 0x01FFF07FU)
                    | (imm11 << 7)
                    | (imm4_1 << 8)
                    | (imm10_5 << 25)
                    | (imm12 << 31);
                wr32(g_out + sec_out + rel->offset, inst);
            } else if (rel->type == S32O_REL_JAL) {
                int32_t off;
                uint32_t pc = sec_va + rel->offset;
                uint32_t inst;
                uint32_t imm20;
                uint32_t imm19_12;
                uint32_t imm11;
                uint32_t imm10_1;
                off = (int32_t)(patched - pc);
                if ((off & 1) != 0 || off < -1048576 || off > 1048574) {
                    fprintf(stderr, "error: jal relocation out of range\n");
                    return 1;
                }
                imm20 = ((uint32_t)off >> 20) & 1U;
                imm19_12 = ((uint32_t)off >> 12) & 0xFFU;
                imm11 = ((uint32_t)off >> 11) & 1U;
                imm10_1 = ((uint32_t)off >> 1) & 0x3FFU;
                inst = rd32(g_out + sec_out + rel->offset);
                inst = (inst & 0x00000FFFU)
                    | (imm19_12 << 12)
                    | (imm11 << 20)
                    | (imm10_1 << 21)
                    | (imm20 << 31);
                wr32(g_out + sec_out + rel->offset, inst);
            } else {
                fprintf(stderr, "error: relocation type %u not yet supported\n", rel->type);
                return 1;
            }
        }
    }

    if (!write_file(out_path, g_out, cur)) {
        fprintf(stderr, "error: cannot write %s\n", out_path);
        return 1;
    }

    printf("stage07: linked %s -> %s\n", obj_path, out_path);
    return 0;
}
