#include "s32_formats_min.h"

#define MAX_LINE 1024
#define MAX_TOK 8
#define MAX_LBL 8192
#define MAX_REL 32768
#define MAX_SYM 8192
#define MAX_TEXT 1048576
#define MAX_DATA 1048576
#define MAX_BSS 16777216
#define MAX_STR 65536
#define LBL_POOL_SZ 262144

#define SEC_TEXT 0
#define SEC_DATA 1
#define SEC_BSS  2

static char g_lbl_name_pool[262144];
static int g_lbl_name_off[8192];
static int g_lbl_name_ptr;
static int g_lbl_sec[8192];
static int g_lbl_val[8192];
static char g_lbl_defd[8192];
static char g_lbl_glob[8192];
static char g_lbl_refd[8192];
static char g_lbl_abs[8192];
static int g_nlbl;

static int g_rel_sec[32768];
static int g_rel_off[32768];
static int g_rel_typ[32768];
static int g_rel_sym[32768];
static int g_rel_add[32768];
static int g_nrel;

static char g_text[1048576];
static char g_data[1048576];
static int g_tsz;
static int g_dsz;
static int g_bsz;
static int g_sec;

static int g_lbl_to_sym[8192];
static int g_nsym;
static int g_sym_lbl[8192];
static int g_sym_name[8192];
static int g_sym_val[8192];
static int g_sym_sec[8192];
static char g_sym_bind[8192];

static char g_str[65536];
static char g_line[1024];
static int g_ssz;
static int g_in;
static int g_out;

int find_lbl(char *name);

char *trim(char *s) {
    int n;
    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n') s = s + 1;
    n = strlen(s);
    while (n > 0 && (s[n - 1] == ' ' || s[n - 1] == '\t' || s[n - 1] == '\r' || s[n - 1] == '\n')) {
        s[n - 1] = 0;
        n = n - 1;
    }
    return s;
}

void strip_comment(char *s) {
    while (*s) {
        if (*s == '#') { *s = 0; return; }
        s = s + 1;
    }
}

int split(char *s, char **tok) {
    int n;
    n = 0;
    while (*s && n < MAX_TOK) {
        while (*s == ' ' || *s == '\t' || *s == ',') s = s + 1;
        if (*s == 0) break;
        tok[n] = s;
        n = n + 1;
        while (*s && *s != ' ' && *s != '\t' && *s != ',') s = s + 1;
        if (*s == 0) break;
        *s = 0;
        s = s + 1;
    }
    return n;
}

int parse_reg(char *s) {
    char *e;
    int v;
    if (strcmp(s, "zero") == 0) return 0;
    if (strcmp(s, "sp") == 0) return 29;
    if (strcmp(s, "fp") == 0) return 30;
    if (strcmp(s, "ra") == 0 || strcmp(s, "lr") == 0) return 31;
    if (s[0] != 'r' && s[0] != 'R') return -1;
    v = strtol(s + 1, &e, 10);
    if (*e != 0 || v < 0 || v > 31) return -1;
    return v;
}

int parse_num(char *s, int *ok) {
    char *e;
    int v;
    v = strtol(s, &e, 0);
    if (*s == 0 || *e != 0) { *ok = 0; return 0; }
    *ok = 1;
    return v;
}

static void ex_skip(char *s, int *p) {
    while (s[*p] == ' ' || s[*p] == '\t') *p = *p + 1;
}

static int ex_is_id0(int c) {
    if (c >= 'A' && c <= 'Z') return 1;
    if (c >= 'a' && c <= 'z') return 1;
    if (c == '_' || c == '.' || c == '$') return 1;
    return 0;
}

static int ex_is_idn(int c) {
    if (ex_is_id0(c)) return 1;
    if (c >= '0' && c <= '9') return 1;
    return 0;
}

static int ex_expr_abs(char *s, int *p, int *ok);

static int ex_factor_abs(char *s, int *p, int *ok) {
    int sign;
    int v;
    int q;
    int li;
    int i;
    char name[128];
    char *e;

    ex_skip(s, p);
    sign = 1;
    while (s[*p] == '+' || s[*p] == '-') {
        if (s[*p] == '-') sign = 0 - sign;
        *p = *p + 1;
        ex_skip(s, p);
    }

    if (s[*p] == '(') {
        *p = *p + 1;
        v = ex_expr_abs(s, p, ok);
        if (!*ok) return 0;
        ex_skip(s, p);
        if (s[*p] != ')') { *ok = 0; return 0; }
        *p = *p + 1;
        return sign * v;
    }

    if ((s[*p] >= '0' && s[*p] <= '9')) {
        v = strtol(s + *p, &e, 0);
        if (e == s + *p) { *ok = 0; return 0; }
        *p = *p + (e - (s + *p));
        return sign * v;
    }

    if (ex_is_id0(s[*p])) {
        i = 0;
        while (ex_is_idn(s[*p])) {
            if (i + 1 >= 128) { *ok = 0; return 0; }
            name[i] = s[*p];
            i = i + 1;
            *p = *p + 1;
        }
        name[i] = 0;
        li = find_lbl(name);
        if (li < 0 || !g_lbl_defd[li] || !g_lbl_abs[li]) { *ok = 0; return 0; }
        return sign * g_lbl_val[li];
    }

    *ok = 0;
    return 0;
}

static int ex_term_abs(char *s, int *p, int *ok) {
    int v;
    int r;
    char op;

    v = ex_factor_abs(s, p, ok);
    if (!*ok) return 0;

    while (1) {
        ex_skip(s, p);
        op = s[*p];
        if (op != '*' && op != '/') break;
        *p = *p + 1;
        r = ex_factor_abs(s, p, ok);
        if (!*ok) return 0;
        if (op == '*') v = v * r;
        else {
            if (r == 0) { *ok = 0; return 0; }
            v = v / r;
        }
    }
    return v;
}

static int ex_expr_abs(char *s, int *p, int *ok) {
    int v;
    int r;
    char op;

    v = ex_term_abs(s, p, ok);
    if (!*ok) return 0;

    while (1) {
        ex_skip(s, p);
        op = s[*p];
        if (op != '+' && op != '-') break;
        *p = *p + 1;
        r = ex_term_abs(s, p, ok);
        if (!*ok) return 0;
        if (op == '+') v = v + r;
        else v = v - r;
    }
    return v;
}

int parse_num_or_abs(char *s, int *ok) {
    int p;
    int v;
    p = 0;
    *ok = 1;
    v = ex_expr_abs(s, &p, ok);
    if (!*ok) return 0;
    ex_skip(s, &p);
    if (s[p] != 0) { *ok = 0; return 0; }
    return v;
}

int parse_sym_add(char *s, char *sym, int sym_sz, int *add, int *ok) {
    int i;
    int ni;
    char c;
    int li;
    int a;
    int sgn;
    int v;
    int depth;
    int beg;
    int t;
    int k;
    char part[128];

    i = 0;
    while (s[i] == ' ' || s[i] == '\t') i = i + 1;

    ni = 0;
    c = s[i];
    if (!((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
          c == '_' || c == '.' || c == '$')) {
        *ok = 0;
        return 0;
    }
    while (1) {
        c = s[i];
        if (!((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
              (c >= '0' && c <= '9') || c == '_' || c == '.' || c == '$')) {
            break;
        }
        if (ni + 1 >= sym_sz) { *ok = 0; return 0; }
        sym[ni] = c;
        ni = ni + 1;
        i = i + 1;
    }
    sym[ni] = 0;

    a = 0;
    while (1) {
        while (s[i] == ' ' || s[i] == '\t') i = i + 1;
        if (s[i] != '+' && s[i] != '-') break;
        sgn = (s[i] == '-') ? -1 : 1;
        i = i + 1;
        while (s[i] == ' ' || s[i] == '\t') i = i + 1;
        depth = 0;
        beg = i;
        while (s[i]) {
            if (s[i] == '(') depth = depth + 1;
            else if (s[i] == ')') {
                if (depth > 0) depth = depth - 1;
            } else if (depth == 0 && (s[i] == '+' || s[i] == '-')) {
                break;
            }
            i = i + 1;
        }
        if (i <= beg) { *ok = 0; return 0; }
        if (i - beg >= 127) { *ok = 0; return 0; }
        k = 0;
        t = beg;
        while (t < i) {
            part[k] = s[t];
            k = k + 1;
            t = t + 1;
        }
        part[k] = 0;
        v = parse_num_or_abs(part, &li);
        if (!li) { *ok = 0; return 0; }
        a = a + sgn * v;
    }

    while (s[i] == ' ' || s[i] == '\t') i = i + 1;
    if (s[i] != 0) { *ok = 0; return 0; }
    *add = a;
    *ok = 1;
    return 0;
}

int parse_reloc_expr(char *s, char *prefix, char *out, int out_sz) {
    int i;
    int pfx_len;
    i = 0;
    pfx_len = strlen(prefix);
    if (strncmp(s, prefix, pfx_len) != 0) return 0;
    s = s + pfx_len;
    while (*s && *s != ')') {
        if (i + 1 >= out_sz) return 0;
        out[i] = *s;
        i = i + 1;
        s = s + 1;
    }
    if (*s != ')' || s[1] != 0) return 0;
    out[i] = 0;
    return i > 0;
}

int find_lbl(char *name) {
    int i;
    for (i = 0; i < g_nlbl; i = i + 1) {
        if (strcmp(g_lbl_name_pool + g_lbl_name_off[i], name) == 0) return i;
    }
    return -1;
}

int get_lbl(char *name) {
    int i;
    int n;
    i = find_lbl(name);
    if (i >= 0) return i;
    if (g_nlbl >= MAX_LBL) return -1;
    n = strlen(name) + 1;
    if (g_lbl_name_ptr + n > LBL_POOL_SZ) return -1;
    g_lbl_name_off[g_nlbl] = g_lbl_name_ptr;
    memcpy(g_lbl_name_pool + g_lbl_name_ptr, name, n);
    g_lbl_name_ptr = g_lbl_name_ptr + n;
    g_lbl_sec[g_nlbl] = 0;
    g_lbl_val[g_nlbl] = 0;
    g_lbl_defd[g_nlbl] = 0;
    g_lbl_glob[g_nlbl] = 0;
    g_lbl_refd[g_nlbl] = 0;
    g_lbl_abs[g_nlbl] = 0;
    i = g_nlbl;
    g_nlbl = g_nlbl + 1;
    return i;
}

int cur_off() {
    if (g_sec == SEC_TEXT) return g_tsz;
    if (g_sec == SEC_DATA) return g_dsz;
    return g_bsz;
}

int emit8(int b) {
    if (g_sec == SEC_TEXT) {
        if (g_tsz >= MAX_TEXT) return -1;
        g_text[g_tsz] = b;
        g_tsz = g_tsz + 1;
    } else if (g_sec == SEC_DATA) {
        if (g_dsz >= MAX_DATA) return -1;
        g_data[g_dsz] = b;
        g_dsz = g_dsz + 1;
    } else {
        if (g_bsz >= MAX_BSS) return -1;
        g_bsz = g_bsz + 1;
    }
    return 0;
}

int emit32(int w) {
    if (emit8(w & 255) != 0) return -1;
    if (emit8((w >> 8) & 255) != 0) return -1;
    if (emit8((w >> 16) & 255) != 0) return -1;
    if (emit8((w >> 24) & 255) != 0) return -1;
    return 0;
}

int emit_byte_list(char *p) {
    char *e;
    int v;
    while (1) {
        while (*p == ' ' || *p == '\t' || *p == ',') p = p + 1;
        if (*p == 0) return 0;
        v = strtol(p, &e, 0);
        if (e == p || (*e != 0 && *e != ',' && *e != ' ' && *e != '\t')) return -1;
        if (emit8(v & 255) != 0) return -1;
        p = e;
    }
}

int emit_word_list(char *p) {
    char *e;
    int v;
    while (1) {
        while (*p == ' ' || *p == '\t' || *p == ',') p = p + 1;
        if (*p == 0) return 0;
        v = strtol(p, &e, 0);
        if (e == p || (*e != 0 && *e != ',' && *e != ' ' && *e != '\t')) return -1;
        if (emit32(v) != 0) return -1;
        p = e;
    }
}

int add_reloc_ex(int typ, int off, char *name, int add) {
    int li;
    if (g_nrel >= MAX_REL) return -1;
    li = get_lbl(name);
    if (li < 0) return -1;
    g_lbl_refd[li] = 1;
    g_rel_sec[g_nrel] = g_sec;
    g_rel_off[g_nrel] = off;
    g_rel_typ[g_nrel] = typ;
    g_rel_sym[g_nrel] = li;
    g_rel_add[g_nrel] = add;
    g_nrel = g_nrel + 1;
    return 0;
}

int add_reloc(int typ, int off, char *name) {
    return add_reloc_ex(typ, off, name, 0);
}

int enc_r(int op, int rd, int rs1, int rs2) {
    return op | (rd << 7) | (rs1 << 15) | (rs2 << 20);
}

int enc_i(int op, int rd, int rs1, int imm) {
    return op | (rd << 7) | (rs1 << 15) | ((imm & 4095) << 20);
}

int enc_s(int op, int rs1, int rs2, int imm) {
    int u;
    u = imm;
    return op | ((u & 31) << 7) | (rs1 << 15) | (rs2 << 20) | (((u >> 5) & 127) << 25);
}

int enc_b(int op, int rs1, int rs2, int imm) {
    int u;
    int w;
    int b11;
    int b4_1;
    int b10_5;
    int b12;
    u = imm;
    w = op;
    b11 = (u >> 11) & 1;
    b4_1 = (u >> 1) & 15;
    b10_5 = (u >> 5) & 63;
    b12 = (u >> 12) & 1;
    w = w | (b11 << 7);
    w = w | (b4_1 << 8);
    w = w | (rs1 << 15);
    w = w | (rs2 << 20);
    w = w | (b10_5 << 25);
    w = w | (b12 << 31);
    return w;
}

int enc_u(int op, int rd, int imm20) {
    return op | (rd << 7) | ((imm20 & 0xFFFFF) << 12);
}

int enc_j(int op, int rd, int imm) {
    int u;
    int w;
    int b19_12;
    int b11;
    int b10_1;
    int b20;
    u = imm;
    w = op;
    b19_12 = (u >> 12) & 255;
    b11 = (u >> 11) & 1;
    b10_1 = (u >> 1) & 1023;
    b20 = (u >> 20) & 1;
    w = w | (rd << 7);
    w = w | (b19_12 << 12);
    w = w | (b11 << 20);
    w = w | (b10_1 << 21);
    w = w | (b20 << 31);
    return w;
}

int rd32(char *p) {
    int v;
    v = p[0] & 255;
    v = v | ((p[1] & 255) << 8);
    v = v | ((p[2] & 255) << 16);
    v = v | ((p[3] & 255) << 24);
    return v;
}

void wr32(char *p, int v) {
    p[0] = v & 255;
    p[1] = (v >> 8) & 255;
    p[2] = (v >> 16) & 255;
    p[3] = (v >> 24) & 255;
}

int do_align(int p) {
    int a;
    int n;
    a = 1 << p;
    n = cur_off();
    while ((n & (a - 1)) != 0) {
        if (emit8(0) != 0) return -1;
        n = n + 1;
    }
    return 0;
}

int do_balign(int bytes, int fill) {
    int n;
    if (bytes <= 0) return -1;
    n = cur_off();
    while ((n % bytes) != 0) {
        if (emit8(fill & 255) != 0) return -1;
        n = n + 1;
    }
    return 0;
}

int handle(char *line) {
    char *tok[8];
    int n;
    char *c;
    int ok;
    int li;
    int i;
    int v;
    int cnt;
    int rd;
    int rs1;
    int rs2;
    int imm;
    int op;
    int off;
    int off2;
    char sym[128];
    char rexpr[128];
    int has_lo;
    int has_hi;
    int rel_add;
    int fill;
    int lo;
    int hi;
    int align;

    strip_comment(line);
    line = trim(line);
    if (*line == 0) return 0;

    c = strchr(line, ':');
    if (c) {
        *c = 0;
        li = get_lbl(trim(line));
        if (li < 0) return -1;
        g_lbl_defd[li] = 1;
        g_lbl_sec[li] = g_sec;
        g_lbl_val[li] = cur_off();
        g_lbl_abs[li] = 0;
        line = trim(c + 1);
        if (*line == 0) return 0;
    }

    if (strncmp(line, ".byte", 5) == 0 &&
        (line[5] == 0 || line[5] == ' ' || line[5] == '\t' || line[5] == ',')) {
        return emit_byte_list(line + 5);
    }
    /* .word/.long handled in tokenized path for symbol relocation support */
    if (strncmp(line, ".asciz", 6) == 0 || strncmp(line, ".string", 7) == 0 || strncmp(line, ".ascii", 6) == 0) {
        int nul;
        nul = (strncmp(line, ".ascii", 6) == 0 && line[6] != 'z') ? 0 : 1;
        c = strchr(line, '"');
        if (!c) return -1;
        c = c + 1;
        while (*c && *c != '"') {
            if (*c == '\\') {
                c = c + 1;
                if (*c == 'n') { if (emit8(10) != 0) return -1; }
                else if (*c == 't') { if (emit8(9) != 0) return -1; }
                else if (*c == '\\') { if (emit8(92) != 0) return -1; }
                else if (*c == '"') { if (emit8(34) != 0) return -1; }
                else if (*c == '0') { if (emit8(0) != 0) return -1; }
                else { if (emit8(*c & 255) != 0) return -1; }
            } else {
                if (emit8(*c & 255) != 0) return -1;
            }
            c = c + 1;
        }
        if (nul) { if (emit8(0) != 0) return -1; }
        return 0;
    }

    n = split(line, tok);
    if (n == 0) return 0;

    if (tok[0][0] == '.') {
        if (strcmp(tok[0], ".text") == 0) { g_sec = SEC_TEXT; return 0; }
        if (strcmp(tok[0], ".data") == 0) { g_sec = SEC_DATA; return 0; }
        if (strcmp(tok[0], ".bss") == 0) { g_sec = SEC_BSS; return 0; }
        if (strcmp(tok[0], ".rodata") == 0) { g_sec = SEC_DATA; return 0; }
        if (strcmp(tok[0], ".global") == 0 || strcmp(tok[0], ".globl") == 0) { if (n < 2) return -1; ok = get_lbl(tok[1]); if (ok < 0) return -1; g_lbl_glob[ok] = 1; return 0; }
        if (strcmp(tok[0], ".align") == 0 || strcmp(tok[0], ".p2align") == 0) { if (n < 2) return -1; ok = parse_num_or_abs(tok[1], &n); if (!n) return -1; return do_align(ok); }
        if (strcmp(tok[0], ".balign") == 0) {
            if (n < 2) return -1;
            v = parse_num_or_abs(tok[1], &ok);
            if (!ok || v <= 0) return -1;
            fill = 0;
            if (n >= 3) {
                fill = parse_num_or_abs(tok[2], &ok);
                if (!ok) return -1;
            }
            return do_balign(v, fill);
        }
        if (strcmp(tok[0], ".section") == 0) {
            if (n < 2) return -1;
            if (strncmp(tok[1], ".rodata", 7) == 0) { g_sec = SEC_DATA; return 0; }
            if (strncmp(tok[1], ".init_array", 11) == 0) { g_sec = SEC_DATA; return 0; }
            if (strncmp(tok[1], ".fpc", 4) == 0) { g_sec = SEC_DATA; return 0; }
            if (strncmp(tok[1], ".debug", 6) == 0) { g_sec = SEC_DATA; return 0; }
            if (strcmp(tok[1], ".text") == 0) { g_sec = SEC_TEXT; return 0; }
            if (strcmp(tok[1], ".data") == 0) { g_sec = SEC_DATA; return 0; }
            if (strcmp(tok[1], ".bss") == 0) { g_sec = SEC_BSS; return 0; }
            return 0;
        }
        if (strcmp(tok[0], ".type") == 0 || strcmp(tok[0], ".size") == 0 || strcmp(tok[0], ".file") == 0 || strcmp(tok[0], ".ident") == 0) return 0;
        if (strcmp(tok[0], ".byte") == 0) {
            for (i = 1; i < n; i = i + 1) {
                v = parse_num_or_abs(tok[i], &ok);
                if (!ok) return -1;
                if (emit8(v & 255) != 0) return -1;
            }
            return 0;
        }
        if (strcmp(tok[0], ".word") == 0 || strcmp(tok[0], ".long") == 0) {
            for (i = 1; i < n; i = i + 1) {
                v = parse_num_or_abs(tok[i], &ok);
                if (ok) { if (emit32(v) != 0) return -1; }
                else {
                    parse_sym_add(tok[i], sym, 128, &rel_add, &ok);
                    if (!ok) return -1;
                    if (add_reloc_ex(S32O_REL_32, cur_off(), sym, rel_add) != 0) return -1;
                    if (emit32(0) != 0) return -1;
                }
            }
            return 0;
        }
        if (strcmp(tok[0], ".quad") == 0) {
            for (i = 1; i < n; i = i + 1) {
                v = parse_num_or_abs(tok[i], &ok);
                if (ok) {
                    lo = v;
                    if (v < 0) hi = -1;
                    else hi = 0;
                    if (emit32(lo) != 0) return -1;
                    if (emit32(hi) != 0) return -1;
                } else {
                    parse_sym_add(tok[i], sym, 128, &rel_add, &ok);
                    if (!ok) return -1;
                    if (add_reloc_ex(S32O_REL_32, cur_off(), sym, rel_add) != 0) return -1;
                    if (emit32(0) != 0) return -1;
                    if (emit32(0) != 0) return -1;
                }
            }
            return 0;
        }
        if (strcmp(tok[0], ".equ") == 0 || strcmp(tok[0], ".set") == 0) {
            if (n != 3) return -1;
            v = parse_num_or_abs(tok[2], &ok);
            if (!ok) return -1;
            li = get_lbl(tok[1]);
            if (li < 0) return -1;
            g_lbl_defd[li] = 1;
            g_lbl_sec[li] = g_sec;
            g_lbl_val[li] = v;
            g_lbl_abs[li] = 1;
            return 0;
        }
        if (strcmp(tok[0], ".comm") == 0) {
            if (n < 3) return -1;
            cnt = parse_num_or_abs(tok[2], &ok);
            if (!ok || cnt < 0) return -1;
            align = 1;
            if (n >= 4) {
                align = parse_num_or_abs(tok[3], &ok);
                if (!ok || align <= 0) return -1;
            }
            li = get_lbl(tok[1]);
            if (li < 0) return -1;
            while ((g_bsz % align) != 0) {
                g_bsz = g_bsz + 1;
                if (g_bsz >= MAX_BSS) return -1;
            }
            g_lbl_defd[li] = 1;
            g_lbl_sec[li] = SEC_BSS;
            g_lbl_val[li] = g_bsz;
            g_lbl_abs[li] = 0;
            g_bsz = g_bsz + cnt;
            if (g_bsz > MAX_BSS) return -1;
            return 0;
        }
        if (strcmp(tok[0], ".space") == 0 || strcmp(tok[0], ".zero") == 0) {
            if (n != 2) return -1;
            cnt = parse_num_or_abs(tok[1], &ok);
            if (!ok || cnt < 0) return -1;
            for (i = 0; i < cnt; i = i + 1) {
                if (emit8(0) != 0) return -1;
            }
            return 0;
        }
        if (strcmp(tok[0], ".half") == 0 || strcmp(tok[0], ".short") == 0) {
            for (i = 1; i < n; i = i + 1) {
                v = parse_num_or_abs(tok[i], &ok);
                if (!ok) return -1;
                if (emit8(v & 255) != 0) return -1;
                if (emit8((v >> 8) & 255) != 0) return -1;
            }
            return 0;
        }
        return 0;
    }

    if (strcmp(tok[0], "add") == 0 || strcmp(tok[0], "sub") == 0 || strcmp(tok[0], "and") == 0 || strcmp(tok[0], "or") == 0 || strcmp(tok[0], "xor") == 0 || strcmp(tok[0], "mul") == 0 || strcmp(tok[0], "mulh") == 0 || strcmp(tok[0], "mulhu") == 0 || strcmp(tok[0], "div") == 0 || strcmp(tok[0], "rem") == 0 || strcmp(tok[0], "slt") == 0 || strcmp(tok[0], "sltu") == 0 || strcmp(tok[0], "sge") == 0 || strcmp(tok[0], "sgeu") == 0 || strcmp(tok[0], "sle") == 0 || strcmp(tok[0], "sleu") == 0 || strcmp(tok[0], "seq") == 0 || strcmp(tok[0], "sne") == 0 || strcmp(tok[0], "sgt") == 0 || strcmp(tok[0], "sgtu") == 0 || strcmp(tok[0], "sll") == 0 || strcmp(tok[0], "srl") == 0 || strcmp(tok[0], "sra") == 0) {
        op = 0;
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
        else if (strcmp(tok[0], "mulh") == 0) op = 0x0B;
        else if (strcmp(tok[0], "mulhu") == 0) op = 0x1F;
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

    if (strcmp(tok[0], "addi") == 0 || strcmp(tok[0], "ori") == 0 || strcmp(tok[0], "andi") == 0 || strcmp(tok[0], "xori") == 0 || strcmp(tok[0], "slli") == 0 || strcmp(tok[0], "srli") == 0 || strcmp(tok[0], "srai") == 0 || strcmp(tok[0], "slti") == 0 || strcmp(tok[0], "sltiu") == 0 || strcmp(tok[0], "ldw") == 0 || strcmp(tok[0], "ldb") == 0 || strcmp(tok[0], "ldbu") == 0 || strcmp(tok[0], "lbu") == 0 || strcmp(tok[0], "ldh") == 0 || strcmp(tok[0], "ldhu") == 0 || strcmp(tok[0], "lhu") == 0 || strcmp(tok[0], "jalr") == 0) {
        op = 0;
        has_lo = 0;
        has_hi = 0;
        rel_add = 0;
        if (n != 4) return -1;
        rd = parse_reg(tok[1]); rs1 = parse_reg(tok[2]); imm = parse_num_or_abs(tok[3], &ok);
        if (!ok) {
            if (parse_reloc_expr(tok[3], "%lo(", rexpr, 128)) {
                parse_sym_add(rexpr, sym, 128, &rel_add, &ok);
                if (!ok) return -1;
                has_lo = 1;
            } else if (parse_reloc_expr(tok[3], "%hi(", rexpr, 128)) {
                parse_sym_add(rexpr, sym, 128, &rel_add, &ok);
                if (!ok) return -1;
                has_hi = 1;
            }
            else return -1;
            imm = 0;
            ok = 1;
        }
        if (rd < 0 || rs1 < 0 || !ok) return -1;
        if (strcmp(tok[0], "addi") == 0) op = 0x10;
        else if (strcmp(tok[0], "ori") == 0) op = 0x11;
        else if (strcmp(tok[0], "andi") == 0) op = 0x12;
        else if (strcmp(tok[0], "slli") == 0) op = 0x13;
        else if (strcmp(tok[0], "srli") == 0) op = 0x14;
        else if (strcmp(tok[0], "srai") == 0) op = 0x15;
        else if (strcmp(tok[0], "slti") == 0) op = 0x16;
        else if (strcmp(tok[0], "sltiu") == 0) op = 0x17;
        else if (strcmp(tok[0], "xori") == 0) op = 0x1E;
        else if (strcmp(tok[0], "ldb") == 0) op = 0x30;
        else if (strcmp(tok[0], "ldh") == 0) op = 0x31;
        else if (strcmp(tok[0], "ldw") == 0) op = 0x32;
        else if (strcmp(tok[0], "ldbu") == 0 || strcmp(tok[0], "lbu") == 0) op = 0x33;
        else if (strcmp(tok[0], "ldhu") == 0 || strcmp(tok[0], "lhu") == 0) op = 0x34;
        else op = 0x41;
        if (emit32(enc_i(op, rd, rs1, imm)) != 0) return -1;
        off = cur_off() - 4;
        if (has_lo) return add_reloc_ex(S32O_REL_LO12, off, sym, rel_add);
        if (has_hi) return add_reloc_ex(S32O_REL_HI20, off, sym, rel_add);
        return 0;
    }

    if (strcmp(tok[0], "stw") == 0 || strcmp(tok[0], "sth") == 0 || strcmp(tok[0], "stb") == 0) {
        has_lo = 0;
        has_hi = 0;
        rel_add = 0;
        if (n != 4) return -1;
        rs1 = parse_reg(tok[1]); rs2 = parse_reg(tok[2]); imm = parse_num_or_abs(tok[3], &ok);
        if (!ok) {
            if (parse_reloc_expr(tok[3], "%lo(", rexpr, 128)) {
                parse_sym_add(rexpr, sym, 128, &rel_add, &ok);
                if (!ok) return -1;
                has_lo = 1;
            } else if (parse_reloc_expr(tok[3], "%hi(", rexpr, 128)) {
                parse_sym_add(rexpr, sym, 128, &rel_add, &ok);
                if (!ok) return -1;
                has_hi = 1;
            }
            else return -1;
            imm = 0;
            ok = 1;
        }
        if (rs1 < 0 || rs2 < 0 || !ok) return -1;
        if (strcmp(tok[0], "stw") == 0) op = 0x3A;
        else if (strcmp(tok[0], "sth") == 0) op = 0x39;
        else op = 0x38;
        if (emit32(enc_s(op, rs1, rs2, imm)) != 0) return -1;
        off = cur_off() - 4;
        if (has_lo) return add_reloc_ex(S32O_REL_LO12, off, sym, rel_add);
        if (has_hi) return add_reloc_ex(S32O_REL_HI20, off, sym, rel_add);
        return 0;
    }

    if (strcmp(tok[0], "beq") == 0 || strcmp(tok[0], "bne") == 0 ||
        strcmp(tok[0], "blt") == 0 || strcmp(tok[0], "bge") == 0 ||
        strcmp(tok[0], "bltu") == 0 || strcmp(tok[0], "bgeu") == 0 ||
        strcmp(tok[0], "bgt") == 0 || strcmp(tok[0], "ble") == 0 ||
        strcmp(tok[0], "bgtu") == 0 || strcmp(tok[0], "bleu") == 0) {
        if (n != 4) return -1;
        rs1 = parse_reg(tok[1]); rs2 = parse_reg(tok[2]);
        if (rs1 < 0 || rs2 < 0) return -1;
        /* Pseudos: bgt/ble/bgtu/bleu swap operands */
        if (strcmp(tok[0], "bgt") == 0 || strcmp(tok[0], "ble") == 0 ||
            strcmp(tok[0], "bgtu") == 0 || strcmp(tok[0], "bleu") == 0) {
            imm = rs1; rs1 = rs2; rs2 = imm;
        }
        if (strcmp(tok[0], "beq") == 0) op = 0x48;
        else if (strcmp(tok[0], "bne") == 0) op = 0x49;
        else if (strcmp(tok[0], "blt") == 0 || strcmp(tok[0], "bgt") == 0) op = 0x4A;
        else if (strcmp(tok[0], "bge") == 0 || strcmp(tok[0], "ble") == 0) op = 0x4B;
        else if (strcmp(tok[0], "bltu") == 0 || strcmp(tok[0], "bgtu") == 0) op = 0x4C;
        else op = 0x4D;
        off = cur_off();
        if (emit32(enc_b(0, rs1, rs2, 0)) != 0) return -1;
        if (off >= g_tsz) return -1;
        g_text[off] = ((g_text[off] & 255) & -128) | (op & 0x7F);
        return add_reloc(S32O_REL_BRANCH, off, tok[3]);
    }

    if (strcmp(tok[0], "lui") == 0) {
        has_hi = 0;
        rel_add = 0;
        if (n != 3) return -1;
        rd = parse_reg(tok[1]); imm = parse_num_or_abs(tok[2], &ok);
        if (!ok) {
            if (parse_reloc_expr(tok[2], "%hi(", rexpr, 128)) {
                parse_sym_add(rexpr, sym, 128, &rel_add, &ok);
                if (!ok) return -1;
                has_hi = 1;
                imm = 0;
                ok = 1;
            } else return -1;
        }
        if (rd < 0 || !ok) return -1;
        if (emit32(enc_u(0x20, rd, imm)) != 0) return -1;
        if (!has_hi) return 0;
        off = cur_off() - 4;
        return add_reloc_ex(S32O_REL_HI20, off, sym, rel_add);
    }

    if (strcmp(tok[0], "jal") == 0) {
        if (n == 2) { rd = 31; tok[2] = tok[1]; }
        else if (n != 3) return -1;
        else { rd = parse_reg(tok[1]); }
        if (rd < 0) return -1;
        off = cur_off();
        if (emit32(enc_j(0, rd, 0)) != 0) return -1;
        if (off >= g_tsz) return -1;
        g_text[off] = ((g_text[off] & 255) & -128) | 0x40;
        rel_add = 0;
        parse_sym_add(tok[2], sym, 128, &rel_add, &ok);
        if (!ok) return -1;
        return add_reloc_ex(S32O_REL_JAL, off, sym, rel_add);
    }

    if (strcmp(tok[0], "la") == 0) {
        if (n != 3) return -1;
        rd = parse_reg(tok[1]);
        if (rd < 0) return -1;
        if (emit32(enc_u(0x20, rd, 0)) != 0) return -1;
        if (emit32(enc_i(0x10, rd, rd, 0)) != 0) return -1;
        off2 = cur_off();
        off = off2 - 8;
        off2 = off2 - 4;
        rel_add = 0;
        parse_sym_add(tok[2], sym, 128, &rel_add, &ok);
        if (!ok) return -1;
        if (add_reloc_ex(S32O_REL_HI20, off, sym, rel_add) != 0) return -1;
        return add_reloc_ex(S32O_REL_LO12, off2, sym, rel_add);
    }

    if (strcmp(tok[0], "call") == 0) {
        if (n != 2) return -1;
        if (emit32(enc_u(0x20, 2, 0)) != 0) return -1;
        if (emit32(enc_i(0x41, 31, 2, 0)) != 0) return -1;
        off2 = cur_off();
        off = off2 - 8;
        off2 = off2 - 4;
        rel_add = 0;
        parse_sym_add(tok[1], sym, 128, &rel_add, &ok);
        if (!ok) return -1;
        if (add_reloc_ex(S32O_REL_HI20, off, sym, rel_add) != 0) return -1;
        return add_reloc_ex(S32O_REL_LO12, off2, sym, rel_add);
    }

    if (strcmp(tok[0], "tail") == 0) {
        if (n != 2) return -1;
        if (emit32(enc_j(0x40, 0, 0)) != 0) return -1;
        rel_add = 0;
        parse_sym_add(tok[1], sym, 128, &rel_add, &ok);
        if (!ok) return -1;
        return add_reloc_ex(S32O_REL_JAL, cur_off() - 4, sym, rel_add);
    }

    if (strcmp(tok[0], "nop") == 0) { return emit32(enc_r(0x00, 0, 0, 0)); }
    if (strcmp(tok[0], "halt") == 0) { return emit32(enc_r(0x7F, 0, 0, 0)); }
    if (strcmp(tok[0], "yield") == 0) { return emit32(enc_r(0x51, 0, 0, 0)); }

    if (strcmp(tok[0], "debug") == 0) {
        if (n != 2) return -1;
        rs1 = parse_reg(tok[1]);
        if (rs1 < 0) return -1;
        return emit32(enc_r(0x52, 0, rs1, 0));
    }

    if (strcmp(tok[0], "not") == 0) {
        if (n != 3) return -1;
        rd = parse_reg(tok[1]); rs1 = parse_reg(tok[2]);
        if (rd < 0 || rs1 < 0) return -1;
        return emit32(enc_i(0x1E, rd, rs1, -1));
    }

    if (strcmp(tok[0], "neg") == 0) {
        if (n != 3) return -1;
        rd = parse_reg(tok[1]); rs1 = parse_reg(tok[2]);
        if (rd < 0 || rs1 < 0) return -1;
        return emit32(enc_r(0x01, rd, 0, rs1));
    }

    if (strcmp(tok[0], "seqz") == 0) {
        if (n != 3) return -1;
        rd = parse_reg(tok[1]); rs1 = parse_reg(tok[2]);
        if (rd < 0 || rs1 < 0) return -1;
        return emit32(enc_r(0x0E, rd, rs1, 0));
    }

    if (strcmp(tok[0], "snez") == 0) {
        if (n != 3) return -1;
        rd = parse_reg(tok[1]); rs1 = parse_reg(tok[2]);
        if (rd < 0 || rs1 < 0) return -1;
        return emit32(enc_r(0x0F, rd, rs1, 0));
    }

    if (strcmp(tok[0], "mv") == 0) {
        if (n != 3) return -1;
        rd = parse_reg(tok[1]); rs1 = parse_reg(tok[2]);
        if (rd < 0 || rs1 < 0) return -1;
        return emit32(enc_r(0x00, rd, rs1, 0));
    }

    if (strcmp(tok[0], "li") == 0) {
        if (n != 3) return -1;
        rd = parse_reg(tok[1]); imm = parse_num_or_abs(tok[2], &ok);
        if (rd < 0 || !ok) return -1;
        if (imm >= -2048 && imm < 2048) return emit32(enc_i(0x10, rd, 0, imm));
        if (emit32(enc_u(0x20, rd, ((imm + 0x800) >> 12) & 0xFFFFF)) != 0) return -1;
        return emit32(enc_i(0x10, rd, rd, imm & 0xFFF));
    }

    if (strcmp(tok[0], "j") == 0) {
        if (n != 2) return -1;
        off = cur_off();
        if (emit32(enc_j(0, 0, 0)) != 0) return -1;
        if (off >= g_tsz) return -1;
        g_text[off] = ((g_text[off] & 255) & -128) | 0x40;
        rel_add = 0;
        parse_sym_add(tok[1], sym, 128, &rel_add, &ok);
        if (!ok) return -1;
        return add_reloc_ex(S32O_REL_JAL, off, sym, rel_add);
    }

    if (strcmp(tok[0], "jr") == 0) {
        if (n != 2) return -1;
        rs1 = parse_reg(tok[1]);
        if (rs1 < 0) return -1;
        return emit32(enc_i(0x41, 0, rs1, 0));
    }

    if (strcmp(tok[0], "ret") == 0) { return emit32(enc_i(0x41, 0, 31, 0)); }
    if (strcmp(tok[0], "assert_eq") == 0) {
        if (n != 3) return -1;
        rs1 = parse_reg(tok[1]); rs2 = parse_reg(tok[2]);
        if (rs1 < 0 || rs2 < 0) return -1;
        return emit32(enc_r(0x3F, 0, rs1, rs2));
    }

    if (strcmp(tok[0], "fadd.s") == 0 || strcmp(tok[0], "fsub.s") == 0 ||
        strcmp(tok[0], "fmul.s") == 0 || strcmp(tok[0], "fdiv.s") == 0 ||
        strcmp(tok[0], "fsqrt.s") == 0 || strcmp(tok[0], "feq.s") == 0 ||
        strcmp(tok[0], "flt.s") == 0 || strcmp(tok[0], "fle.s") == 0 ||
        strcmp(tok[0], "fcvt.w.s") == 0 || strcmp(tok[0], "fcvt.wu.s") == 0 ||
        strcmp(tok[0], "fcvt.s.w") == 0 || strcmp(tok[0], "fcvt.s.wu") == 0 ||
        strcmp(tok[0], "fneg.s") == 0 || strcmp(tok[0], "fabs.s") == 0 ||
        strcmp(tok[0], "fadd.d") == 0 || strcmp(tok[0], "fsub.d") == 0 ||
        strcmp(tok[0], "fmul.d") == 0 || strcmp(tok[0], "fdiv.d") == 0 ||
        strcmp(tok[0], "fsqrt.d") == 0 || strcmp(tok[0], "feq.d") == 0 ||
        strcmp(tok[0], "flt.d") == 0 || strcmp(tok[0], "fle.d") == 0 ||
        strcmp(tok[0], "fcvt.w.d") == 0 || strcmp(tok[0], "fcvt.wu.d") == 0 ||
        strcmp(tok[0], "fcvt.d.w") == 0 || strcmp(tok[0], "fcvt.d.wu") == 0 ||
        strcmp(tok[0], "fcvt.d.s") == 0 || strcmp(tok[0], "fcvt.s.d") == 0 ||
        strcmp(tok[0], "fneg.d") == 0 || strcmp(tok[0], "fabs.d") == 0 ||
        strcmp(tok[0], "fcvt.l.s") == 0 || strcmp(tok[0], "fcvt.lu.s") == 0 ||
        strcmp(tok[0], "fcvt.s.l") == 0 || strcmp(tok[0], "fcvt.s.lu") == 0 ||
        strcmp(tok[0], "fcvt.l.d") == 0 || strcmp(tok[0], "fcvt.lu.d") == 0 ||
        strcmp(tok[0], "fcvt.d.l") == 0 || strcmp(tok[0], "fcvt.d.lu") == 0) {
        rs2 = 0;
        if (n != 3 && n != 4) return -1;
        rd = parse_reg(tok[1]); rs1 = parse_reg(tok[2]);
        if (rd < 0 || rs1 < 0) return -1;
        if (n == 4) {
            rs2 = parse_reg(tok[3]);
            if (rs2 < 0) return -1;
        }
        if (strcmp(tok[0], "fadd.s") == 0) op = 0x53;
        else if (strcmp(tok[0], "fsub.s") == 0) op = 0x54;
        else if (strcmp(tok[0], "fmul.s") == 0) op = 0x55;
        else if (strcmp(tok[0], "fdiv.s") == 0) op = 0x56;
        else if (strcmp(tok[0], "fsqrt.s") == 0) op = 0x57;
        else if (strcmp(tok[0], "feq.s") == 0) op = 0x58;
        else if (strcmp(tok[0], "flt.s") == 0) op = 0x59;
        else if (strcmp(tok[0], "fle.s") == 0) op = 0x5A;
        else if (strcmp(tok[0], "fcvt.w.s") == 0) op = 0x5B;
        else if (strcmp(tok[0], "fcvt.wu.s") == 0) op = 0x5C;
        else if (strcmp(tok[0], "fcvt.s.w") == 0) op = 0x5D;
        else if (strcmp(tok[0], "fcvt.s.wu") == 0) op = 0x5E;
        else if (strcmp(tok[0], "fneg.s") == 0) op = 0x5F;
        else if (strcmp(tok[0], "fabs.s") == 0) op = 0x60;
        else if (strcmp(tok[0], "fadd.d") == 0) op = 0x61;
        else if (strcmp(tok[0], "fsub.d") == 0) op = 0x62;
        else if (strcmp(tok[0], "fmul.d") == 0) op = 0x63;
        else if (strcmp(tok[0], "fdiv.d") == 0) op = 0x64;
        else if (strcmp(tok[0], "fsqrt.d") == 0) op = 0x65;
        else if (strcmp(tok[0], "feq.d") == 0) op = 0x66;
        else if (strcmp(tok[0], "flt.d") == 0) op = 0x67;
        else if (strcmp(tok[0], "fle.d") == 0) op = 0x68;
        else if (strcmp(tok[0], "fcvt.w.d") == 0) op = 0x69;
        else if (strcmp(tok[0], "fcvt.wu.d") == 0) op = 0x6A;
        else if (strcmp(tok[0], "fcvt.d.w") == 0) op = 0x6B;
        else if (strcmp(tok[0], "fcvt.d.wu") == 0) op = 0x6C;
        else if (strcmp(tok[0], "fcvt.d.s") == 0) op = 0x6D;
        else if (strcmp(tok[0], "fcvt.s.d") == 0) op = 0x6E;
        else if (strcmp(tok[0], "fneg.d") == 0) op = 0x6F;
        else if (strcmp(tok[0], "fabs.d") == 0) op = 0x70;
        else if (strcmp(tok[0], "fcvt.l.s") == 0) op = 0x71;
        else if (strcmp(tok[0], "fcvt.lu.s") == 0) op = 0x72;
        else if (strcmp(tok[0], "fcvt.s.l") == 0) op = 0x73;
        else if (strcmp(tok[0], "fcvt.s.lu") == 0) op = 0x74;
        else if (strcmp(tok[0], "fcvt.l.d") == 0) op = 0x75;
        else if (strcmp(tok[0], "fcvt.lu.d") == 0) op = 0x76;
        else if (strcmp(tok[0], "fcvt.d.l") == 0) op = 0x77;
        else op = 0x78;
        return emit32(enc_r(op, rd, rs1, rs2));
    }

    return -1;
}

int s_add(char *s) {
    int off;
    int n;
    off = g_ssz;
    n = strlen(s) + 1;
    if (g_ssz + n > MAX_STR) return -1;
    memcpy(g_str + g_ssz, s, n);
    g_ssz = g_ssz + n;
    return off;
}

void w16(int v) {
    fputc(v & 255, g_out);
    fputc((v >> 8) & 255, g_out);
}

void w32(int v) {
    fputc(v & 255, g_out);
    fputc((v >> 8) & 255, g_out);
    fputc((v >> 16) & 255, g_out);
    fputc((v >> 24) & 255, g_out);
}

int build_syms(int text_idx, int data_idx, int bss_idx) {
    int i;
    char *name;
    int sec;
    int bind;
    g_nsym = 0;
    for (i = 0; i < g_nlbl; i = i + 1) g_lbl_to_sym[i] = -1;
    for (i = 0; i < g_nlbl; i = i + 1) {
        name = g_lbl_name_pool + g_lbl_name_off[i];
        sec = 0;
        if (!(g_lbl_glob[i] || g_lbl_refd[i])) continue;
        if (g_nsym >= MAX_SYM) return -1;
        if (g_lbl_defd[i]) {
            if (g_lbl_sec[i] == SEC_TEXT) sec = text_idx;
            else if (g_lbl_sec[i] == SEC_DATA) sec = data_idx;
            else sec = bss_idx;
        }
        if (g_lbl_glob[i]) bind = S32O_BIND_GLOBAL;
        else bind = S32O_BIND_LOCAL;
        if (!g_lbl_defd[i]) bind = S32O_BIND_GLOBAL;
        g_sym_lbl[g_nsym] = i;
        g_sym_name[g_nsym] = s_add(g_lbl_name_pool + g_lbl_name_off[i]);
        if (g_sym_name[g_nsym] < 0) return -1;
        if (g_lbl_defd[i]) g_sym_val[g_nsym] = g_lbl_val[i];
        else g_sym_val[g_nsym] = 0;
        g_sym_sec[g_nsym] = sec;
        g_sym_bind[g_nsym] = bind;
        g_lbl_to_sym[i] = g_nsym;
        g_nsym = g_nsym + 1;
    }
    return 0;
}

int resolve_local_text_relocs() {
    int i;
    int out;
    int li;
    int typ;
    int off;
    int keep;
    int target;
    int disp;
    int inst;
    int patched;
    int rs1;
    int rs2;
    int rd;

    out = 0;
    for (i = 0; i < g_nrel; i = i + 1) {
        li = g_rel_sym[i];
        typ = g_rel_typ[i];
        off = g_rel_off[i];
        keep = 1;

        if (g_rel_sec[i] == SEC_TEXT &&
            (typ == S32O_REL_BRANCH || typ == S32O_REL_JAL) &&
            li < g_nlbl &&
            g_lbl_defd[li] &&
            g_lbl_sec[li] == SEC_TEXT) {
            target = g_lbl_val[li] + g_rel_add[i];
            disp = target - off;

            if (off + 4 > g_tsz) return -1;
            if ((off & 3) != 0) return -1;

            if (typ == S32O_REL_BRANCH) {
                inst = rd32(g_text + off);
                rs1 = (inst >> 15) & 31;
                rs2 = (inst >> 20) & 31;
                disp = disp - 4;
                patched = enc_b(0, rs1, rs2, disp);
                patched = patched | (inst & 0x7F);
                wr32(g_text + off, patched);
            } else {
                inst = rd32(g_text + off);
                rd = (inst >> 7) & 31;
                patched = enc_j(0, rd, disp);
                patched = patched | (inst & 0xFFF);
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
            out = out + 1;
        }
    }
    g_nrel = out;

    for (i = 0; i < g_nlbl; i = i + 1) g_lbl_refd[i] = 0;
    for (i = 0; i < g_nrel; i = i + 1) {
        li = g_rel_sym[i];
        if (li < g_nlbl) g_lbl_refd[li] = 1;
    }
    return 0;
}

int write_obj(char *out) {
    int nsec;
    int text_idx;
    int data_idx;
    int bss_idx;
    int text_rel;
    int data_rel;
    int bss_rel;
    int i;
    int off;
    int text_name;
    int data_name;
    int bss_name;
    int sec_off;
    int sym_off;
    int rel_off;
    int str_off;
    int text_rel_off;
    int data_rel_off;
    int bss_rel_off;
    int text_data_off;
    int data_data_off;
    int v;

    nsec = 0;
    text_idx = 0;
    data_idx = 0;
    bss_idx = 0;
    text_rel = 0;
    data_rel = 0;
    bss_rel = 0;
    text_name = 0;
    data_name = 0;
    bss_name = 0;
    text_rel_off = 0;
    data_rel_off = 0;
    bss_rel_off = 0;
    text_data_off = 0;
    data_data_off = 0;

    if (g_tsz) { nsec = nsec + 1; text_idx = nsec; }
    if (g_dsz) { nsec = nsec + 1; data_idx = nsec; }
    if (g_bsz) { nsec = nsec + 1; bss_idx = nsec; }

    for (i = 0; i < g_nrel; i = i + 1) {
        if (g_rel_sec[i] == SEC_TEXT) text_rel = text_rel + 1;
        else if (g_rel_sec[i] == SEC_DATA) data_rel = data_rel + 1;
        else bss_rel = bss_rel + 1;
    }

    g_ssz = 0;
    g_str[g_ssz] = 0;
    g_ssz = g_ssz + 1;
    if (text_idx) text_name = s_add(".text");
    if (data_idx) data_name = s_add(".data");
    if (bss_idx) bss_name = s_add(".bss");
    if ((text_idx && text_name < 0) ||
        (data_idx && data_name < 0) ||
        (bss_idx && bss_name < 0)) {
        return -1;
    }

    if (build_syms(text_idx, data_idx, bss_idx) != 0) return -1;

    sec_off = SIZEOF_S32O_HEADER;
    sym_off = sec_off + nsec * SIZEOF_S32O_SECTION;
    rel_off = sym_off + g_nsym * SIZEOF_S32O_SYMBOL;
    off = rel_off;
    if (text_rel) { text_rel_off = off; off = off + text_rel * SIZEOF_S32O_RELOC; }
    if (data_rel) { data_rel_off = off; off = off + data_rel * SIZEOF_S32O_RELOC; }
    if (bss_rel) { bss_rel_off = off; off = off + bss_rel * SIZEOF_S32O_RELOC; }
    str_off = off;
    off = off + g_ssz;
    off = (off + 3) & -4;
    if (text_idx) { text_data_off = off; off = off + g_tsz; }
    if (data_idx) { data_data_off = off; off = off + g_dsz; }

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

    for (i = 0; i < g_nsym; i = i + 1) {
        v = g_sym_name[i]; w32(v);
        v = g_sym_val[i]; w32(v);
        w16(g_sym_sec[i]);
        fputc(S32O_SYM_NOTYPE, g_out);
        fputc(g_sym_bind[i], g_out);
        v = 0; w32(v);
    }

    for (i = 0; i < g_nrel; i = i + 1) {
        if (g_rel_sec[i] == SEC_TEXT) {
            v = g_rel_off[i]; w32(v);
            v = g_lbl_to_sym[g_rel_sym[i]]; w32(v);
            v = g_rel_typ[i]; w32(v);
            v = g_rel_add[i]; w32(v);
        }
    }
    for (i = 0; i < g_nrel; i = i + 1) {
        if (g_rel_sec[i] == SEC_DATA) {
            v = g_rel_off[i]; w32(v);
            v = g_lbl_to_sym[g_rel_sym[i]]; w32(v);
            v = g_rel_typ[i]; w32(v);
            v = g_rel_add[i]; w32(v);
        }
    }
    for (i = 0; i < g_nrel; i = i + 1) {
        if (g_rel_sec[i] == SEC_BSS) {
            v = g_rel_off[i]; w32(v);
            v = g_lbl_to_sym[g_rel_sym[i]]; w32(v);
            v = g_rel_typ[i]; w32(v);
            v = g_rel_add[i]; w32(v);
        }
    }

    fwrite(g_str, 1, g_ssz, g_out);
    while ((ftell(g_out) & 3) != 0) fputc(0, g_out);
    if (g_tsz) fwrite(g_text, 1, g_tsz, g_out);
    if (g_dsz) fwrite(g_data, 1, g_dsz, g_out);

    fclose(g_out);
    g_out = 0;
    return 0;
}

int main(int argc, char **argv) {
    int lno;
    lno = 0;

    if (argc != 3) {
        fputs("Usage: s32-as <input.s> <output.s32o>\n", stderr);
        return 1;
    }

    g_in = fopen(argv[1], "rb");
    if (!g_in) {
        fputs("cannot open input\n", stderr);
        return 1;
    }

    while (fgets(g_line, MAX_LINE, g_in)) {
        lno = lno + 1;
        if (handle(g_line) != 0) {
            fputs("assemble error at line ", stderr);
            fput_uint(stderr, lno);
            fputc('\n', stderr);
            fclose(g_in);
            g_in = 0;
            return 1;
        }
    }
    fclose(g_in);
    g_in = 0;

    if (resolve_local_text_relocs() != 0) {
        fputs("resolve local text relocations failed\n", stderr);
        return 1;
    }

    if (write_obj(argv[2]) != 0) {
        fputs("write object failed\n", stderr);
        return 1;
    }
    return 0;
}
