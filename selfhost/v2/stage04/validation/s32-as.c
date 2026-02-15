#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <s32_formats.h>

#define MAX_LINE 1024
#define MAX_TOK 8
#define MAX_LBL 512
#define MAX_REL 4096
#define MAX_SYM 512
#define MAX_TEXT 262144
#define MAX_DATA 262144
#define MAX_BSS 262144
#define MAX_STR 16384

enum { SEC_TEXT = 0, SEC_DATA = 1, SEC_BSS = 2 };

static char g_lbl_name_pool[32768];
static uint32_t g_lbl_name_off[MAX_LBL];
static uint32_t g_lbl_name_ptr = 0;
static uint32_t g_lbl_sec[MAX_LBL];
static uint32_t g_lbl_val[MAX_LBL];
static uint8_t g_lbl_defd[MAX_LBL];
static uint8_t g_lbl_glob[MAX_LBL];
static uint8_t g_lbl_refd[MAX_LBL];
static uint32_t g_nlbl = 0;

static uint32_t g_rel_sec[MAX_REL];
static uint32_t g_rel_off[MAX_REL];
static uint32_t g_rel_typ[MAX_REL];
static uint32_t g_rel_sym[MAX_REL];
static int32_t g_rel_add[MAX_REL];
static uint32_t g_nrel = 0;

static uint8_t g_text[MAX_TEXT];
static uint8_t g_data[MAX_DATA];
static uint32_t g_tsz = 0;
static uint32_t g_dsz = 0;
static uint32_t g_bsz = 0;
static uint32_t g_sec = SEC_TEXT;

static uint32_t g_lbl_to_sym[MAX_LBL];
static uint32_t g_nsym = 0;
static uint32_t g_sym_lbl[MAX_SYM];
static uint32_t g_sym_name[MAX_SYM];
static uint32_t g_sym_val[MAX_SYM];
static uint16_t g_sym_sec[MAX_SYM];
static uint8_t g_sym_bind[MAX_SYM];

static char g_str[MAX_STR];
static char g_line[MAX_LINE];
static uint32_t g_ssz = 0;
static FILE *g_in = 0;
static FILE *g_out = 0;

static char *trim(char *s) {
    int n;
    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n') s++;
    n = (int)strlen(s);
    while (n > 0 && (s[n - 1] == ' ' || s[n - 1] == '\t' || s[n - 1] == '\r' || s[n - 1] == '\n')) {
        s[n - 1] = 0;
        n--;
    }
    return s;
}

static void strip_comment(char *s) {
    while (*s) {
        if (*s == '#') { *s = 0; return; }
        s++;
    }
}

static int split(char *s, char *tok[]) {
    int n = 0;
    while (*s && n < MAX_TOK) {
        while (*s == ' ' || *s == '\t' || *s == ',') s++;
        if (*s == 0) break;
        tok[n++] = s;
        while (*s && *s != ' ' && *s != '\t' && *s != ',') s++;
        if (*s == 0) break;
        *s = 0;
        s++;
    }
    return n;
}

static int parse_reg(const char *s) {
    char *e;
    long v;
    if (s[0] != 'r' && s[0] != 'R') return -1;
    v = strtol(s + 1, &e, 10);
    if (*e != 0 || v < 0 || v > 31) return -1;
    return (int)v;
}

static int parse_num(const char *s, int *ok) {
    char *e;
    long v = strtol(s, &e, 0);
    if (*s == 0 || *e != 0) { *ok = 0; return 0; }
    *ok = 1;
    return (int)v;
}

static int parse_reloc_expr(const char *s, const char *prefix, char *out, int out_sz) {
    int i = 0;
    int pfx_len = (int)strlen(prefix);
    if (strncmp(s, prefix, (size_t)pfx_len) != 0) return 0;
    s += pfx_len;
    while (*s && *s != ')') {
        if (i + 1 >= out_sz) return 0;
        out[i++] = *s++;
    }
    if (*s != ')' || s[1] != 0) return 0;
    out[i] = 0;
    return i > 0;
}

static int find_lbl(const char *name) {
    uint32_t i;
    for (i = 0; i < g_nlbl; i++) {
        if (strcmp(g_lbl_name_pool + g_lbl_name_off[i], name) == 0) return (int)i;
    }
    return -1;
}

static int get_lbl(const char *name) {
    int i = find_lbl(name);
    uint32_t n;
    if (i >= 0) return i;
    if (g_nlbl >= MAX_LBL) return -1;
    n = (uint32_t)strlen(name) + 1u;
    if (g_lbl_name_ptr + n > 32768u) return -1;
    g_lbl_name_off[g_nlbl] = g_lbl_name_ptr;
    memcpy(g_lbl_name_pool + g_lbl_name_ptr, name, n);
    g_lbl_name_ptr += n;
    g_lbl_sec[g_nlbl] = 0;
    g_lbl_val[g_nlbl] = 0;
    g_lbl_defd[g_nlbl] = 0;
    g_lbl_glob[g_nlbl] = 0;
    g_lbl_refd[g_nlbl] = 0;
    i = (int)g_nlbl;
    g_nlbl++;
    return i;
}

static uint32_t cur_off(void) {
    if (g_sec == SEC_TEXT) return g_tsz;
    if (g_sec == SEC_DATA) return g_dsz;
    return g_bsz;
}

static int emit8(uint8_t b) {
    if (g_sec == SEC_TEXT) {
        if (g_tsz >= MAX_TEXT) return -1;
        g_text[g_tsz++] = b;
    } else if (g_sec == SEC_DATA) {
        if (g_dsz >= MAX_DATA) return -1;
        g_data[g_dsz++] = b;
    } else {
        if (g_bsz >= MAX_BSS) return -1;
        g_bsz++;
    }
    return 0;
}

static int emit32(uint32_t w) {
    if (emit8((uint8_t)(w & 255)) != 0) return -1;
    if (emit8((uint8_t)((w >> 8) & 255)) != 0) return -1;
    if (emit8((uint8_t)((w >> 16) & 255)) != 0) return -1;
    if (emit8((uint8_t)((w >> 24) & 255)) != 0) return -1;
    return 0;
}

static int emit_byte_list(char *p) {
    char *e;
    long v;
    for (;;) {
        while (*p == ' ' || *p == '\t' || *p == ',') p++;
        if (*p == 0) return 0;
        v = strtol(p, &e, 0);
        if (e == p || (*e != 0 && *e != ',' && *e != ' ' && *e != '\t')) return -1;
        if (emit8((uint8_t)(v & 255)) != 0) return -1;
        p = e;
    }
}

static int emit_word_list(char *p) {
    char *e;
    long v;
    for (;;) {
        while (*p == ' ' || *p == '\t' || *p == ',') p++;
        if (*p == 0) return 0;
        v = strtol(p, &e, 0);
        if (e == p || (*e != 0 && *e != ',' && *e != ' ' && *e != '\t')) return -1;
        if (emit32((uint32_t)v) != 0) return -1;
        p = e;
    }
}

static int add_reloc(uint32_t typ, uint32_t off, const char *name) {
    int li;
    if (g_nrel >= MAX_REL) return -1;
    li = get_lbl(name);
    if (li < 0) return -1;
    g_lbl_refd[li] = 1;
    g_rel_sec[g_nrel] = g_sec;
    g_rel_off[g_nrel] = off;
    g_rel_typ[g_nrel] = typ;
    g_rel_sym[g_nrel] = (uint32_t)li;
    g_rel_add[g_nrel] = 0;
    g_nrel++;
    return 0;
}

static uint32_t enc_r(uint32_t op, int rd, int rs1, int rs2) { return op | ((uint32_t)rd << 7) | ((uint32_t)rs1 << 15) | ((uint32_t)rs2 << 20); }
static uint32_t enc_i(uint32_t op, int rd, int rs1, int imm) { return op | ((uint32_t)rd << 7) | ((uint32_t)rs1 << 15) | (((uint32_t)imm & 4095u) << 20); }
static uint32_t enc_s(uint32_t op, int rs1, int rs2, int imm) { uint32_t u=(uint32_t)imm; return op | ((u & 31u) << 7) | ((uint32_t)rs1 << 15) | ((uint32_t)rs2 << 20) | (((u >> 5) & 127u) << 25); }
static uint32_t enc_b(uint32_t op, int rs1, int rs2, int imm) {
    uint32_t u = (uint32_t)imm;
    uint32_t w = op;
    uint32_t b11 = (u >> 11) & 1u;
    uint32_t b4_1 = (u >> 1) & 15u;
    uint32_t b10_5 = (u >> 5) & 63u;
    uint32_t b12 = (u >> 12) & 1u;
    w |= b11 << 7;
    w |= b4_1 << 8;
    w |= ((uint32_t)rs1) << 15;
    w |= ((uint32_t)rs2) << 20;
    w |= b10_5 << 25;
    w |= b12 << 31;
    return w;
}
static uint32_t enc_u(uint32_t op, int rd, int imm20) { return op | ((uint32_t)rd << 7) | (((uint32_t)imm20 & 0xFFFFFu) << 12); }
static uint32_t enc_j(uint32_t op, int rd, int imm) {
    uint32_t u = (uint32_t)imm;
    uint32_t w = op;
    uint32_t b19_12 = (u >> 12) & 255u;
    uint32_t b11 = (u >> 11) & 1u;
    uint32_t b10_1 = (u >> 1) & 1023u;
    uint32_t b20 = (u >> 20) & 1u;
    w |= ((uint32_t)rd) << 7;
    w |= b19_12 << 12;
    w |= b11 << 20;
    w |= b10_1 << 21;
    w |= b20 << 31;
    return w;
}

static uint32_t rd32(uint8_t *p) {
    uint32_t v = (uint32_t)p[0];
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

static int do_align(int p) {
    uint32_t a = 1u << p;
    uint32_t n = cur_off();
    while ((n & (a - 1u)) != 0) {
        if (emit8(0) != 0) return -1;
        n++;
    }
    return 0;
}

static int handle(char *line) {
    char *tok[MAX_TOK];
    int n;
    char *c;
    int ok;

    strip_comment(line);
    line = trim(line);
    if (*line == 0) return 0;

    c = strchr(line, ':');
    if (c) {
        int li;
        *c = 0;
        li = get_lbl(trim(line));
        if (li < 0) return -1;
        g_lbl_defd[li] = 1;
        g_lbl_sec[li] = g_sec;
        g_lbl_val[li] = cur_off();
        line = trim(c + 1);
        if (*line == 0) return 0;
    }

    if (strncmp(line, ".byte", 5) == 0 &&
        (line[5] == 0 || line[5] == ' ' || line[5] == '\t' || line[5] == ',')) {
        return emit_byte_list(line + 5);
    }
    if (strncmp(line, ".word", 5) == 0 &&
        (line[5] == 0 || line[5] == ' ' || line[5] == '\t' || line[5] == ',')) {
        return emit_word_list(line + 5);
    }

    n = split(line, tok);
    if (n == 0) return 0;

    if (tok[0][0] == '.') {
        if (strcmp(tok[0], ".text") == 0) { g_sec = SEC_TEXT; return 0; }
        if (strcmp(tok[0], ".data") == 0) { g_sec = SEC_DATA; return 0; }
        if (strcmp(tok[0], ".bss") == 0) { g_sec = SEC_BSS; return 0; }
        if (strcmp(tok[0], ".global") == 0) { if (n < 2) return -1; ok = get_lbl(tok[1]); if (ok < 0) return -1; g_lbl_glob[ok] = 1; return 0; }
        if (strcmp(tok[0], ".align") == 0) { if (n < 2) return -1; ok = parse_num(tok[1], &n); if (!n) return -1; return do_align(ok); }
        if (strcmp(tok[0], ".byte") == 0) {
            int i;
            for (i = 1; i < n; i++) {
                int v = parse_num(tok[i], &ok);
                if (!ok) return -1;
                if (emit8((uint8_t)(v & 255)) != 0) return -1;
            }
            return 0;
        }
        if (strcmp(tok[0], ".space") == 0) {
            int i;
            int cnt;
            if (n != 2) return -1;
            cnt = parse_num(tok[1], &ok);
            if (!ok || cnt < 0) return -1;
            for (i = 0; i < cnt; i++) {
                if (emit8(0) != 0) return -1;
            }
            return 0;
        }
        return 0;
    }

    if (strcmp(tok[0], "add") == 0 || strcmp(tok[0], "sub") == 0 || strcmp(tok[0], "and") == 0 || strcmp(tok[0], "or") == 0 || strcmp(tok[0], "xor") == 0 || strcmp(tok[0], "mul") == 0 || strcmp(tok[0], "div") == 0 || strcmp(tok[0], "rem") == 0 || strcmp(tok[0], "slt") == 0 || strcmp(tok[0], "sltu") == 0 || strcmp(tok[0], "sge") == 0 || strcmp(tok[0], "sgeu") == 0 || strcmp(tok[0], "sle") == 0 || strcmp(tok[0], "sleu") == 0 || strcmp(tok[0], "seq") == 0 || strcmp(tok[0], "sne") == 0 || strcmp(tok[0], "sgt") == 0 || strcmp(tok[0], "sgtu") == 0 || strcmp(tok[0], "sll") == 0 || strcmp(tok[0], "srl") == 0 || strcmp(tok[0], "sra") == 0) {
        int rd, rs1, rs2;
        uint32_t op = 0;
        if (n != 4) return -1;
        rd = parse_reg(tok[1]); rs1 = parse_reg(tok[2]); rs2 = parse_reg(tok[3]);
        if (rd < 0 || rs1 < 0 || rs2 < 0) return -1;
        if (strcmp(tok[0], "add") == 0) op = 0x00;
        else if (strcmp(tok[0], "sub") == 0) op = 0x01;
        else if (strcmp(tok[0], "xor") == 0) op = 0x02;
        else if (strcmp(tok[0], "or") == 0) op = 0x03;
        else if (strcmp(tok[0], "and") == 0) op = 0x04;
        else if (strcmp(tok[0], "slt") == 0) op = 0x08;
        else if (strcmp(tok[0], "sltu") == 0) op = 0x09;
        else if (strcmp(tok[0], "mul") == 0) op = 0x0A;
        else if (strcmp(tok[0], "div") == 0) op = 0x0C;
        else if (strcmp(tok[0], "rem") == 0) op = 0x0D;
        else if (strcmp(tok[0], "seq") == 0) op = 0x0E;
        else if (strcmp(tok[0], "sne") == 0) op = 0x0F;
        else if (strcmp(tok[0], "sgt") == 0) op = 0x18;
        else if (strcmp(tok[0], "sgtu") == 0) op = 0x19;
        else if (strcmp(tok[0], "sle") == 0) op = 0x1A;
        else if (strcmp(tok[0], "sleu") == 0) op = 0x1B;
        else if (strcmp(tok[0], "sgeu") == 0) op = 0x1D;
        else if (strcmp(tok[0], "sll") == 0) op = 0x05;
        else if (strcmp(tok[0], "srl") == 0) op = 0x06;
        else if (strcmp(tok[0], "sra") == 0) op = 0x07;
        else op = 0x1C;
        return emit32(enc_r(op, rd, rs1, rs2));
    }

    if (strcmp(tok[0], "addi") == 0 || strcmp(tok[0], "slli") == 0 || strcmp(tok[0], "ldw") == 0 || strcmp(tok[0], "ldb") == 0 || strcmp(tok[0], "lbu") == 0 || strcmp(tok[0], "lhu") == 0 || strcmp(tok[0], "jalr") == 0) {
        int rd, rs1, imm;
        uint32_t op = 0;
        uint32_t off;
        char sym[128];
        int has_lo = 0;
        int has_hi = 0;
        if (n != 4) return -1;
        rd = parse_reg(tok[1]); rs1 = parse_reg(tok[2]); imm = parse_num(tok[3], &ok);
        if (!ok && strcmp(tok[0], "addi") == 0) {
            if (parse_reloc_expr(tok[3], "%lo(", sym, 128)) has_lo = 1;
            else if (parse_reloc_expr(tok[3], "%hi(", sym, 128)) has_hi = 1;
            else return -1;
            imm = 0;
            ok = 1;
        }
        if (rd < 0 || rs1 < 0 || !ok) return -1;
        if (strcmp(tok[0], "addi") == 0) op = 0x10;
        else if (strcmp(tok[0], "slli") == 0) op = 0x13;
        else if (strcmp(tok[0], "ldb") == 0) op = 0x30;
        else if (strcmp(tok[0], "ldw") == 0) op = 0x32;
        else if (strcmp(tok[0], "lbu") == 0) op = 0x33;
        else if (strcmp(tok[0], "lhu") == 0) op = 0x34;
        else op = 0x41;
        if (emit32(enc_i(op, rd, rs1, imm)) != 0) return -1;
        off = cur_off() - 4u;
        if (has_lo) return add_reloc(S32O_REL_LO12, off, sym);
        if (has_hi) return add_reloc(S32O_REL_HI20, off, sym);
        return 0;
    }

    if (strcmp(tok[0], "stw") == 0 || strcmp(tok[0], "stb") == 0) {
        int rs1, rs2, imm;
        uint32_t op;
        if (n != 4) return -1;
        rs1 = parse_reg(tok[1]); rs2 = parse_reg(tok[2]); imm = parse_num(tok[3], &ok);
        if (rs1 < 0 || rs2 < 0 || !ok) return -1;
        op = (strcmp(tok[0], "stw") == 0) ? 0x3A : 0x38;
        return emit32(enc_s(op, rs1, rs2, imm));
    }

    if (strcmp(tok[0], "beq") == 0 || strcmp(tok[0], "bne") == 0) {
        int rs1, rs2;
        uint32_t off;
        uint32_t op;
        if (n != 4) return -1;
        rs1 = parse_reg(tok[1]); rs2 = parse_reg(tok[2]);
        if (rs1 < 0 || rs2 < 0) return -1;
        if (strcmp(tok[0], "beq") == 0) op = 0x48;
        else op = 0x49;
        off = cur_off();
        if (emit32(enc_b(0, rs1, rs2, 0)) != 0) return -1;
        if (off >= g_tsz) return -1;
        g_text[off] = (uint8_t)((g_text[off] & ~0x7Fu) | (op & 0x7Fu));
        return add_reloc(S32O_REL_BRANCH, off, tok[3]);
    }

    if (strcmp(tok[0], "lui") == 0) {
        int rd, imm;
        char sym[128];
        uint32_t off;
        int has_hi = 0;
        if (n != 3) return -1;
        rd = parse_reg(tok[1]); imm = parse_num(tok[2], &ok);
        if (!ok) {
            if (parse_reloc_expr(tok[2], "%hi(", sym, 128)) {
                has_hi = 1;
                imm = 0;
                ok = 1;
            } else return -1;
        }
        if (rd < 0 || !ok) return -1;
        if (emit32(enc_u(0x20, rd, imm)) != 0) return -1;
        if (!has_hi) return 0;
        off = cur_off() - 4u;
        return add_reloc(S32O_REL_HI20, off, sym);
    }

    if (strcmp(tok[0], "jal") == 0) {
        int rd;
        uint32_t off;
        if (n != 3) return -1;
        rd = parse_reg(tok[1]);
        if (rd < 0) return -1;
        off = cur_off();
        if (emit32(enc_j(0, rd, 0)) != 0) return -1;
        if (off >= g_tsz) return -1;
        g_text[off] = (uint8_t)((g_text[off] & ~0x7Fu) | 0x40u);
        return add_reloc(S32O_REL_JAL, off, tok[2]);
    }

    if (strcmp(tok[0], "la") == 0) {
        int rd;
        uint32_t off;
        uint32_t off2;
        if (n != 3) return -1;
        rd = parse_reg(tok[1]);
        if (rd < 0) return -1;
        if (emit32(enc_u(0x20, rd, 0)) != 0) return -1;
        if (emit32(enc_i(0x10, rd, rd, 0)) != 0) return -1;
        off2 = cur_off();
        off = off2 - 8u;
        off2 = off2 - 4u;
        if (add_reloc(S32O_REL_HI20, off, tok[2]) != 0) return -1;
        return add_reloc(S32O_REL_LO12, off2, tok[2]);
    }

    if (strcmp(tok[0], "call") == 0) {
        uint32_t off;
        uint32_t off2;
        if (n != 2) return -1;
        if (emit32(enc_u(0x20, 2, 0)) != 0) return -1;
        if (emit32(enc_i(0x41, 31, 2, 0)) != 0) return -1;
        off2 = cur_off();
        off = off2 - 8u;
        off2 = off2 - 4u;
        if (add_reloc(S32O_REL_HI20, off, tok[1]) != 0) return -1;
        return add_reloc(S32O_REL_LO12, off2, tok[1]);
    }

    return -1;
}

static uint32_t s_add(const char *s) {
    uint32_t off = g_ssz;
    uint32_t n = (uint32_t)strlen(s) + 1u;
    if (g_ssz + n > MAX_STR) return 0;
    memcpy(g_str + g_ssz, s, n);
    g_ssz += n;
    return off;
}

static void w16(uint16_t v) { fputc((int)(v & 255), g_out); fputc((int)((v >> 8) & 255), g_out); }
static void w32(uint32_t v) { fputc((int)(v & 255), g_out); fputc((int)((v >> 8) & 255), g_out); fputc((int)((v >> 16) & 255), g_out); fputc((int)((v >> 24) & 255), g_out); }

static int build_syms(int text_idx, int data_idx, int bss_idx) {
    uint32_t i;
    g_nsym = 0;
    for (i = 0; i < g_nlbl; i++) g_lbl_to_sym[i] = 0xFFFFFFFFu;
    for (i = 0; i < g_nlbl; i++) {
        const char *name = g_lbl_name_pool + g_lbl_name_off[i];
        uint16_t sec = 0;
        uint8_t bind;
        if (!(g_lbl_glob[i] || g_lbl_refd[i])) continue;
        /* Keep local numeric labels in symtab when branch/jal relocations are
           preserved for the stage03 linker. */
        if (g_nsym >= MAX_SYM) return -1;
        if (g_lbl_defd[i]) {
            if (g_lbl_sec[i] == SEC_TEXT) sec = (uint16_t)text_idx;
            else if (g_lbl_sec[i] == SEC_DATA) sec = (uint16_t)data_idx;
            else sec = (uint16_t)bss_idx;
        }
        bind = g_lbl_glob[i] ? S32O_BIND_GLOBAL : S32O_BIND_LOCAL;
        if (!g_lbl_defd[i]) bind = S32O_BIND_GLOBAL;
        g_sym_lbl[g_nsym] = i;
        g_sym_name[g_nsym] = s_add(g_lbl_name_pool + g_lbl_name_off[i]);
        g_sym_val[g_nsym] = g_lbl_defd[i] ? g_lbl_val[i] : 0;
        g_sym_sec[g_nsym] = sec;
        g_sym_bind[g_nsym] = bind;
        g_lbl_to_sym[i] = g_nsym;
        g_nsym++;
    }
    return 0;
}

static int resolve_local_text_relocs(void) {
    uint32_t i, out = 0;
    for (i = 0; i < g_nrel; i++) {
        uint32_t li = g_rel_sym[i];
        uint32_t typ = g_rel_typ[i];
        uint32_t off = g_rel_off[i];
        int keep = 1;

        if (g_rel_sec[i] == SEC_TEXT &&
            (typ == S32O_REL_BRANCH || typ == S32O_REL_JAL) &&
            li < g_nlbl &&
            g_lbl_defd[li] &&
            g_lbl_sec[li] == SEC_TEXT) {
            int32_t target = (int32_t)g_lbl_val[li] + g_rel_add[i];
            int32_t disp = target - (int32_t)off;

            if (off + 4u > g_tsz) return -1;
            if ((off & 3u) != 0u) return -1;

            if (typ == S32O_REL_BRANCH) {
                uint32_t inst = rd32(g_text + off);
                int rs1 = (int)((inst >> 15) & 31u);
                int rs2 = (int)((inst >> 20) & 31u);
                disp -= 4;
                uint32_t patched = enc_b(0, rs1, rs2, disp);
                /* Preserve original branch opcode bits. */
                patched |= (inst & 0x7Fu);
                wr32(g_text + off, patched);
            } else {
                uint32_t inst = rd32(g_text + off);
                int rd = (int)((inst >> 7) & 31u);
                uint32_t patched = enc_j(0, rd, disp);
                /* Preserve original jal opcode/rd low bits. */
                patched |= (inst & 0xFFFu);
                wr32(g_text + off, patched);
            }
            keep = 0;
        }

        if (keep) {
            if (out != i) {
                g_rel_sec[out] = g_rel_sec[i];
                g_rel_off[out] = g_rel_off[i];
                g_rel_typ[out] = g_rel_typ[i];
                g_rel_sym[out] = g_rel_sym[i];
                g_rel_add[out] = g_rel_add[i];
            }
            out++;
        }
    }
    g_nrel = out;
    return 0;
}

static int write_obj(const char *out) {
    uint32_t nsec = 0;
    int text_idx = 0;
    int data_idx = 0;
    int bss_idx = 0;
    uint32_t text_rel = 0;
    uint32_t data_rel = 0;
    uint32_t bss_rel = 0;
    uint32_t i;
    uint32_t off;
    uint32_t text_name = 0;
    uint32_t data_name = 0;
    uint32_t bss_name = 0;
    uint32_t sec_off, sym_off, rel_off, str_off;
    uint32_t text_rel_off = 0, data_rel_off = 0, bss_rel_off = 0;
    uint32_t text_data_off = 0, data_data_off = 0;
    uint32_t v;

    if (g_tsz) { nsec++; text_idx = (int)nsec; }
    if (g_dsz) { nsec++; data_idx = (int)nsec; }
    if (g_bsz) { nsec++; bss_idx = (int)nsec; }

    /* Keep local text branch/jal relocations for linker resolution.
       This avoids stage5 self-host fold-path corruption. */

    for (i = 0; i < g_nrel; i++) {
        if (g_rel_sec[i] == SEC_TEXT) text_rel++;
        else if (g_rel_sec[i] == SEC_DATA) data_rel++;
        else bss_rel++;
    }

    g_ssz = 0;
    g_str[g_ssz++] = 0;
    if (text_idx) text_name = s_add(".text");
    if (data_idx) data_name = s_add(".data");
    if (bss_idx) bss_name = s_add(".bss");

    if (build_syms(text_idx, data_idx, bss_idx) != 0) return -1;

    sec_off = (uint32_t)sizeof(s32o_header_t);
    sym_off = sec_off + nsec * (uint32_t)sizeof(s32o_section_t);
    rel_off = sym_off + g_nsym * (uint32_t)sizeof(s32o_symbol_t);
    off = rel_off;
    if (text_rel) { text_rel_off = off; off += text_rel * (uint32_t)sizeof(s32o_reloc_t); }
    if (data_rel) { data_rel_off = off; off += data_rel * (uint32_t)sizeof(s32o_reloc_t); }
    if (bss_rel) { bss_rel_off = off; off += bss_rel * (uint32_t)sizeof(s32o_reloc_t); }
    str_off = off;
    off += g_ssz;
    off = (off + 3u) & ~3u;
    if (text_idx) { text_data_off = off; off += g_tsz; }
    if (data_idx) { data_data_off = off; off += g_dsz; }

    g_out = fopen(out, "wb");
    if (!g_out) return -1;

    v = S32O_MAGIC; w32(v);
    w16(1);
    fputc(S32_ENDIAN_LITTLE, g_out);
    fputc(S32_MACHINE_SLOW32, g_out);
    v = 0; w32(v);
    v = nsec; w32(v);
    v = sec_off; w32(v);
    v = g_nsym; w32(v);
    v = sym_off; w32(v);
    v = str_off; w32(v);
    v = g_ssz; w32(v);
    v = 0; w32(v);

    if (text_idx) {
        v = text_name; w32(v);
        v = S32_SEC_CODE; w32(v);
        v = S32_SEC_FLAG_EXEC | S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC; w32(v);
        v = g_tsz; w32(v);
        v = text_data_off; w32(v);
        v = 4; w32(v);
        v = text_rel; w32(v);
        v = text_rel_off; w32(v);
    }
    if (data_idx) {
        v = data_name; w32(v);
        v = S32_SEC_DATA; w32(v);
        v = S32_SEC_FLAG_WRITE | S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC; w32(v);
        v = g_dsz; w32(v);
        v = data_data_off; w32(v);
        v = 4; w32(v);
        v = data_rel; w32(v);
        v = data_rel_off; w32(v);
    }
    if (bss_idx) {
        v = bss_name; w32(v);
        v = S32_SEC_BSS; w32(v);
        v = S32_SEC_FLAG_WRITE | S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC; w32(v);
        v = g_bsz; w32(v);
        v = 0; w32(v);
        v = 4; w32(v);
        v = bss_rel; w32(v);
        v = bss_rel_off; w32(v);
    }

    for (i = 0; i < g_nsym; i++) {
        v = g_sym_name[i]; w32(v);
        v = g_sym_val[i]; w32(v);
        w16(g_sym_sec[i]);
        fputc(S32O_SYM_NOTYPE, g_out);
        fputc(g_sym_bind[i], g_out);
        v = 0; w32(v);
    }

    for (i = 0; i < g_nrel; i++) {
        if (g_rel_sec[i] == SEC_TEXT) {
            v = g_rel_off[i]; w32(v);
            v = g_lbl_to_sym[g_rel_sym[i]]; w32(v);
            v = g_rel_typ[i]; w32(v);
            v = (uint32_t)g_rel_add[i]; w32(v);
        }
    }
    for (i = 0; i < g_nrel; i++) {
        if (g_rel_sec[i] == SEC_DATA) {
            v = g_rel_off[i]; w32(v);
            v = g_lbl_to_sym[g_rel_sym[i]]; w32(v);
            v = g_rel_typ[i]; w32(v);
            v = (uint32_t)g_rel_add[i]; w32(v);
        }
    }
    for (i = 0; i < g_nrel; i++) {
        if (g_rel_sec[i] == SEC_BSS) {
            v = g_rel_off[i]; w32(v);
            v = g_lbl_to_sym[g_rel_sym[i]]; w32(v);
            v = g_rel_typ[i]; w32(v);
            v = (uint32_t)g_rel_add[i]; w32(v);
        }
    }

    fwrite(g_str, 1, g_ssz, g_out);
    while (((uint32_t)ftell(g_out) & 3u) != 0) fputc(0, g_out);
    if (g_tsz) fwrite(g_text, 1, g_tsz, g_out);
    if (g_dsz) fwrite(g_data, 1, g_dsz, g_out);

    fclose(g_out);
    g_out = 0;
    return 0;
}

int main(int argc, char **argv) {
    uint32_t lno = 0;

    if (argc != 3) {
        fprintf(stderr, "Usage: %s <input.s> <output.s32o>\n", argv[0]);
        return 1;
    }

    g_in = fopen(argv[1], "rb");
    if (!g_in) {
        fprintf(stderr, "cannot open input\n");
        return 1;
    }

    while (fgets(g_line, sizeof(g_line), g_in)) {
        lno++;
        if (handle(g_line) != 0) {
            fprintf(stderr, "assemble error at line %u\n", lno);
            fclose(g_in);
            g_in = 0;
            return 1;
        }
    }
    fclose(g_in);
    g_in = 0;

    if (resolve_local_text_relocs() != 0) {
        fprintf(stderr, "resolve local text relocations failed\n");
        return 1;
    }

    if (write_obj(argv[2]) != 0) {
        fprintf(stderr, "write object failed\n");
        return 1;
    }
    return 0;
}
