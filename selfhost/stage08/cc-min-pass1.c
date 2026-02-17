#include <stdio.h>

#define MAX_SRC 65536

static char g_src[MAX_SRC];
static int g_src_len;
static int g_pos;
static int g_line;

static char g_output[65536];
static int g_output_len;

static char g_for_buf[4096];
static int g_for_len;
static int g_emit_to_for;

static int g_label_count;

#define MAX_LOOP 16
static int g_break_labels[MAX_LOOP];
static int g_cont_labels[MAX_LOOP];
static int g_loop_depth;

static int read_file(const char *path, char *buf, int max_len, int *out_len) {
    FILE *f;
    int n;
    int ch;
    f = fopen(path, "rb");
    if (!f) return 0;
    n = 0;
    for (;;) {
        ch = fgetc(f);
        if (ch < 0) break;
        if (n + 1 >= max_len) { fclose(f); return 0; }
        buf[n] = ch;
        n = n + 1;
    }
    fclose(f);
    buf[n] = 0;
    *out_len = n;
    return 1;
}

int ccmin_load_source(const char *path) {
    if (!read_file(path, g_src, MAX_SRC, &g_src_len)) return 0;
    return 1;
}

#define TK_EOF      0
#define TK_NUM      1
#define TK_IDENT    2
#define TK_INT      3
#define TK_VOID     4
#define TK_IF       5
#define TK_ELSE     6
#define TK_WHILE    7
#define TK_FOR      8
#define TK_RETURN   9
#define TK_BREAK    10
#define TK_CONTINUE 11
#define TK_PLUS     20
#define TK_MINUS    21
#define TK_STAR     22
#define TK_SLASH    23
#define TK_PERCENT  24
#define TK_AMP      25
#define TK_PIPE     26
#define TK_CARET    27
#define TK_TILDE    28
#define TK_BANG     29
#define TK_EQ       30
#define TK_NE       31
#define TK_LT       32
#define TK_GT       33
#define TK_LE       34
#define TK_GE       35
#define TK_LAND     36
#define TK_LOR      37
#define TK_LSHIFT   38
#define TK_RSHIFT   39
#define TK_ASSIGN   40
#define TK_PLUSEQ   41
#define TK_MINUSEQ  42
#define TK_INC      43
#define TK_DEC      44
#define TK_LPAREN   50
#define TK_RPAREN   51
#define TK_LBRACE   52
#define TK_RBRACE   53
#define TK_SEMI     54
#define TK_COMMA    55
#define TK_QMARK    56
#define TK_COLON    57
#define TK_CHAR_KW  58

static int g_tok;
static int g_tok_val;
static char g_tok_str[128];
static int g_tok_slen;

static int is_space(char c) {
    if (c == ' ') return 1;
    if (c == '\t') return 1;
    if (c == '\n') return 1;
    if (c == '\r') return 1;
    return 0;
}

static int is_alpha(char c) {
    if (c >= 'a' && c <= 'z') return 1;
    if (c >= 'A' && c <= 'Z') return 1;
    if (c == '_') return 1;
    return 0;
}

static int is_digit(char c) {
    return c >= '0' && c <= '9';
}

static int str_eq(const char *a, const char *b) {
    int i;
    i = 0;
    for (;;) {
        if (a[i] != b[i]) return 0;
        if (a[i] == 0) return 1;
        i = i + 1;
    }
}

static void cc_error(const char *msg) {
    char nb[12];
    int v;
    int i;
    fputs("cc-min error: ", stderr);
    fputs(msg, stderr);
    fputs(" (line ", stderr);
    v = g_line;
    if (v <= 0) v = 1;
    i = 0;
    while (v > 0) { nb[i] = '0' + (v % 10); i = i + 1; v = v / 10; }
    if (i == 0) { nb[0] = '0'; i = 1; }
    while (i > 0) { i = i - 1; fputc(nb[i], stderr); }
    fputs(")\n", stderr);
}

static void skip_ws(void) {
    char c;
    for (;;) {
        if (g_pos >= g_src_len) return;
        c = g_src[g_pos];
        if (c == '\n') { g_line = g_line + 1; g_pos = g_pos + 1; }
        else if (is_space(c)) { g_pos = g_pos + 1; }
        else if (c == '/' && g_pos + 1 < g_src_len && g_src[g_pos+1] == '/') {
            g_pos = g_pos + 2;
            while (g_pos < g_src_len && g_src[g_pos] != '\n') g_pos = g_pos + 1;
        } else if (c == '/' && g_pos + 1 < g_src_len && g_src[g_pos+1] == '*') {
            g_pos = g_pos + 2;
            for (;;) {
                if (g_pos + 1 >= g_src_len) break;
                if (g_src[g_pos] == '\n') g_line = g_line + 1;
                if (g_src[g_pos] == '*' && g_src[g_pos+1] == '/') { g_pos = g_pos + 2; break; }
                g_pos = g_pos + 1;
            }
        } else { return; }
    }
}

static void next_token(void) {
    char c;
    int v;
    int i;
    skip_ws();
    if (g_pos >= g_src_len) { g_tok = TK_EOF; return; }
    c = g_src[g_pos];
    if (is_digit(c)) {
        v = 0;
        while (g_pos < g_src_len && is_digit(g_src[g_pos])) {
            v = v * 10 + (g_src[g_pos] - '0');
            g_pos = g_pos + 1;
        }
        g_tok = TK_NUM; g_tok_val = v; return;
    }
    if (is_alpha(c)) {
        i = 0;
        while (g_pos < g_src_len && (is_alpha(g_src[g_pos]) || is_digit(g_src[g_pos]))) {
            if (i < 126) { g_tok_str[i] = g_src[g_pos]; i = i + 1; }
            g_pos = g_pos + 1;
        }
        g_tok_str[i] = 0; g_tok_slen = i;
        if (str_eq(g_tok_str, "int")) { g_tok = TK_INT; return; }
        if (str_eq(g_tok_str, "void")) { g_tok = TK_VOID; return; }
        if (str_eq(g_tok_str, "char")) { g_tok = TK_CHAR_KW; return; }
        if (str_eq(g_tok_str, "if")) { g_tok = TK_IF; return; }
        if (str_eq(g_tok_str, "else")) { g_tok = TK_ELSE; return; }
        if (str_eq(g_tok_str, "while")) { g_tok = TK_WHILE; return; }
        if (str_eq(g_tok_str, "for")) { g_tok = TK_FOR; return; }
        if (str_eq(g_tok_str, "return")) { g_tok = TK_RETURN; return; }
        if (str_eq(g_tok_str, "break")) { g_tok = TK_BREAK; return; }
        if (str_eq(g_tok_str, "continue")) { g_tok = TK_CONTINUE; return; }
        g_tok = TK_IDENT; return;
    }
    g_pos = g_pos + 1;
    if (c == '=' && g_pos < g_src_len && g_src[g_pos] == '=') { g_pos = g_pos+1; g_tok = TK_EQ; return; }
    if (c == '!' && g_pos < g_src_len && g_src[g_pos] == '=') { g_pos = g_pos+1; g_tok = TK_NE; return; }
    if (c == '<' && g_pos < g_src_len && g_src[g_pos] == '=') { g_pos = g_pos+1; g_tok = TK_LE; return; }
    if (c == '>' && g_pos < g_src_len && g_src[g_pos] == '=') { g_pos = g_pos+1; g_tok = TK_GE; return; }
    if (c == '&' && g_pos < g_src_len && g_src[g_pos] == '&') { g_pos = g_pos+1; g_tok = TK_LAND; return; }
    if (c == '|' && g_pos < g_src_len && g_src[g_pos] == '|') { g_pos = g_pos+1; g_tok = TK_LOR; return; }
    if (c == '<' && g_pos < g_src_len && g_src[g_pos] == '<') { g_pos = g_pos+1; g_tok = TK_LSHIFT; return; }
    if (c == '>' && g_pos < g_src_len && g_src[g_pos] == '>') { g_pos = g_pos+1; g_tok = TK_RSHIFT; return; }
    if (c == '+' && g_pos < g_src_len && g_src[g_pos] == '+') { g_pos = g_pos+1; g_tok = TK_INC; return; }
    if (c == '-' && g_pos < g_src_len && g_src[g_pos] == '-') { g_pos = g_pos+1; g_tok = TK_DEC; return; }
    if (c == '+' && g_pos < g_src_len && g_src[g_pos] == '=') { g_pos = g_pos+1; g_tok = TK_PLUSEQ; return; }
    if (c == '-' && g_pos < g_src_len && g_src[g_pos] == '=') { g_pos = g_pos+1; g_tok = TK_MINUSEQ; return; }
    if (c == '+') { g_tok = TK_PLUS; return; }
    if (c == '-') { g_tok = TK_MINUS; return; }
    if (c == '*') { g_tok = TK_STAR; return; }
    if (c == '/') { g_tok = TK_SLASH; return; }
    if (c == '%') { g_tok = TK_PERCENT; return; }
    if (c == '&') { g_tok = TK_AMP; return; }
    if (c == '|') { g_tok = TK_PIPE; return; }
    if (c == '^') { g_tok = TK_CARET; return; }
    if (c == '~') { g_tok = TK_TILDE; return; }
    if (c == '!') { g_tok = TK_BANG; return; }
    if (c == '=') { g_tok = TK_ASSIGN; return; }
    if (c == '<') { g_tok = TK_LT; return; }
    if (c == '>') { g_tok = TK_GT; return; }
    if (c == '(') { g_tok = TK_LPAREN; return; }
    if (c == ')') { g_tok = TK_RPAREN; return; }
    if (c == '{') { g_tok = TK_LBRACE; return; }
    if (c == '}') { g_tok = TK_RBRACE; return; }
    if (c == ';') { g_tok = TK_SEMI; return; }
    if (c == ',') { g_tok = TK_COMMA; return; }
    if (c == '?') { g_tok = TK_QMARK; return; }
    if (c == ':') { g_tok = TK_COLON; return; }
    cc_error("unexpected character");
    g_tok = TK_EOF;
}

static int expect(int tok) {
    if (g_tok != tok) { cc_error("unexpected token"); return 0; }
    next_token();
    return 1;
}

#define MAX_LOCALS 64
#define MAX_FUNCS 128
#define NAMESZ 32
#define LNBUF_SZ 2048
#define FNBUF_SZ 4096
static char g_lnames[LNBUF_SZ];
static int g_loffs[MAX_LOCALS];
static int g_nlocals;
static int g_local_off;

static char g_fnames[FNBUF_SZ];
static int g_fparams[MAX_FUNCS];
static int g_nfuncs;

static int g_ret_label;

static int str_copy(char *dst, const char *src, int max) {
    int i;
    i = 0;
    while (src[i] != 0 && i < max - 1) { dst[i] = src[i]; i = i + 1; }
    dst[i] = 0;
    return i;
}

static void sym_set_name(char *buf, int idx, const char *name) {
    int base;
    int i;
    base = idx * NAMESZ;
    i = 0;
    while (name[i] != 0 && i < NAMESZ - 1) {
        buf[base + i] = name[i];
        i = i + 1;
    }
    buf[base + i] = 0;
}

static int sym_name_eq(char *buf, int idx, const char *name) {
    int base;
    int i;
    base = idx * NAMESZ;
    i = 0;
    for (;;) {
        if (buf[base + i] != name[i]) return 0;
        if (name[i] == 0) return 1;
        i = i + 1;
    }
}

static int find_local(const char *name) {
    int i;
    i = 0;
    while (i < g_nlocals) {
        if (sym_name_eq(g_lnames, i, name)) return g_loffs[i];
        i = i + 1;
    }
    return 99999;
}

static void add_local(const char *name) {
    if (g_nlocals >= MAX_LOCALS) { cc_error("too many locals"); return; }
    g_local_off = g_local_off - 4;
    sym_set_name(g_lnames, g_nlocals, name);
    g_loffs[g_nlocals] = g_local_off;
    g_nlocals = g_nlocals + 1;
}

static void add_param(const char *name, int idx) {
    int off;
    if (g_nlocals >= MAX_LOCALS) { cc_error("too many locals"); return; }
    off = -12 - idx * 4;
    sym_set_name(g_lnames, g_nlocals, name);
    g_loffs[g_nlocals] = off;
    g_nlocals = g_nlocals + 1;
}

static int find_func(const char *name) {
    int i;
    i = 0;
    while (i < g_nfuncs) {
        if (sym_name_eq(g_fnames, i, name)) return g_fparams[i];
        i = i + 1;
    }
    return -1;
}

static void add_func(const char *name, int np) {
    if (g_nfuncs >= MAX_FUNCS) { cc_error("too many funcs"); return; }
    sym_set_name(g_fnames, g_nfuncs, name);
    g_fparams[g_nfuncs] = np;
    g_nfuncs = g_nfuncs + 1;
}

static int new_label(void) {
    int l;
    l = g_label_count;
    g_label_count = g_label_count + 1;
    return l;
}

static void emit(const char *s) {
    int i;
    i = 0;
    while (s[i] != 0) {
        if (g_emit_to_for) {
            if (g_for_len < 4095) { g_for_buf[g_for_len] = s[i]; g_for_len = g_for_len + 1; }
        } else {
            if (g_output_len < 65535) { g_output[g_output_len] = s[i]; g_output_len = g_output_len + 1; }
        }
        i = i + 1;
    }
}

static void emit_ch(int c) {
    if (g_emit_to_for) {
        if (g_for_len < 4095) { g_for_buf[g_for_len] = c; g_for_len = g_for_len + 1; }
    } else {
        if (g_output_len < 65535) { g_output[g_output_len] = c; g_output_len = g_output_len + 1; }
    }
}

static void emit_num(int v) {
    char buf[12];
    int i;
    int neg;
    neg = 0;
    if (v < 0) { neg = 1; v = 0 - v; }
    i = 0;
    if (v == 0) { buf[0] = '0'; i = 1; }
    while (v > 0) { buf[i] = '0' + (v % 10); i = i + 1; v = v / 10; }
    if (neg) emit_ch('-');
    while (i > 0) { i = i - 1; emit_ch(buf[i]); }
}

static void emit_ldef(int id) { emit(".L"); emit_num(id); emit(":\n"); }
static void emit_lref(int id) { emit(".L"); emit_num(id); }

static void emit_push(void) {
    emit("    addi r29, r29, -4\n    stw r29, r1, 0\n");
}

static void emit_pop(void) {
    emit("    ldw r2, r29, 0\n    addi r29, r29, 4\n");
}

static void emit_li(int val) {
    int hi;
    int lo;
    if (val >= -2048 && val <= 2047) {
        emit("    addi r1, r0, "); emit_num(val); emit_ch('\n');
    } else {
        hi = (val + 2048) >> 12;
        hi = hi & 1048575;
        lo = val & 4095;
        if (lo >= 2048) lo = lo - 4096;
        emit("    lui r1, "); emit_num(hi); emit_ch('\n');
        emit("    addi r1, r1, "); emit_num(lo); emit_ch('\n');
    }
}

static void emit_ldloc(int off) {
    emit("    ldw r1, r30, "); emit_num(off); emit_ch('\n');
}

static void emit_stloc(int off) {
    emit("    stw r30, r1, "); emit_num(off); emit_ch('\n');
}

static void emit_bz(int lbl) {
    int skip;
    skip = new_label();
    emit("    bne r1, r0, "); emit_lref(skip); emit_ch('\n');
    emit("    jal r0, "); emit_lref(lbl); emit_ch('\n');
    emit_ldef(skip);
}

static void emit_jmp(int lbl) {
    emit("    jal r0, "); emit_lref(lbl); emit_ch('\n');
}

static void emit_prologue(void) {
    emit("    addi r29, r29, -256\n");
    emit("    stw r29, r31, 252\n");
    emit("    stw r29, r30, 248\n");
    emit("    addi r30, r29, 256\n");
}

static void emit_epilogue(int rl) {
    emit_ldef(rl);
    emit("    ldw r31, r29, 252\n");
    emit("    ldw r30, r29, 248\n");
    emit("    addi r29, r29, 256\n");
    emit("    jalr r0, r31, 0\n");
}

static void emit_call(const char *name) {
    emit("    jal r31, "); emit(name); emit_ch('\n');
}

static void parse_expr(void);
static void parse_assign(void);

static void parse_primary(void) {
    int off;
    char sn[128];
    int nargs;
    int k;
    if (g_tok == TK_NUM) { emit_li(g_tok_val); next_token(); return; }
    if (g_tok == TK_LPAREN) { next_token(); parse_expr(); expect(TK_RPAREN); return; }
    if (g_tok == TK_IDENT) {
        str_copy(sn, g_tok_str, 128);
        next_token();
        if (g_tok == TK_LPAREN) {
            next_token();
            nargs = 0;
            if (g_tok != TK_RPAREN) {
                parse_expr(); emit_push(); nargs = 1;
                while (g_tok == TK_COMMA) {
                    next_token(); parse_expr(); emit_push(); nargs = nargs + 1;
                }
            }
            expect(TK_RPAREN);
            k = nargs;
            while (k > 0) {
                k = k - 1;
                emit("    ldw r"); emit_num(3 + k); emit(", r29, 0\n");
                emit("    addi r29, r29, 4\n");
            }
            emit_call(sn);
            return;
        }
        off = find_local(sn);
        if (off != 99999) { emit_ldloc(off); return; }
        cc_error("undefined variable");
        return;
    }
    cc_error("expected expression");
}

static void parse_postfix(void) { parse_primary(); }

static void parse_unary(void) {
    int off;
    if (g_tok == TK_BANG) { next_token(); parse_unary(); emit("    seq r1, r1, r0\n"); return; }
    if (g_tok == TK_MINUS) { next_token(); parse_unary(); emit("    sub r1, r0, r1\n"); return; }
    if (g_tok == TK_TILDE) { next_token(); parse_unary(); emit("    xori r1, r1, -1\n"); return; }
    if (g_tok == TK_INC) {
        next_token();
        if (g_tok == TK_IDENT) {
            off = find_local(g_tok_str);
            if (off != 99999) {
                emit_ldloc(off); emit("    addi r1, r1, 1\n"); emit_stloc(off);
                next_token(); return;
            }
        }
        parse_unary(); emit("    addi r1, r1, 1\n"); return;
    }
    if (g_tok == TK_DEC) {
        next_token();
        if (g_tok == TK_IDENT) {
            off = find_local(g_tok_str);
            if (off != 99999) {
                emit_ldloc(off); emit("    addi r1, r1, -1\n"); emit_stloc(off);
                next_token(); return;
            }
        }
        parse_unary(); emit("    addi r1, r1, -1\n"); return;
    }
    parse_postfix();
}

static void parse_mul(void) {
    int op;
    parse_unary();
    for (;;) {
        if (g_tok == TK_STAR) op = 0;
        else if (g_tok == TK_SLASH) op = 1;
        else if (g_tok == TK_PERCENT) op = 2;
        else break;
        next_token(); emit_push(); parse_unary(); emit_pop();
        if (op == 0) emit("    mul r1, r2, r1\n");
        else if (op == 1) emit("    div r1, r2, r1\n");
        else emit("    rem r1, r2, r1\n");
    }
}

static void parse_add(void) {
    int op;
    parse_mul();
    for (;;) {
        if (g_tok == TK_PLUS) op = 0;
        else if (g_tok == TK_MINUS) op = 1;
        else break;
        next_token(); emit_push(); parse_mul(); emit_pop();
        if (op == 0) emit("    add r1, r2, r1\n");
        else emit("    sub r1, r2, r1\n");
    }
}

static void parse_shift(void) {
    int op;
    parse_add();
    for (;;) {
        if (g_tok == TK_LSHIFT) op = 0;
        else if (g_tok == TK_RSHIFT) op = 1;
        else break;
        next_token(); emit_push(); parse_add(); emit_pop();
        if (op == 0) emit("    sll r1, r2, r1\n");
        else emit("    sra r1, r2, r1\n");
    }
}

static void parse_rel(void) {
    int op;
    parse_shift();
    for (;;) {
        if (g_tok == TK_LT) op = 0;
        else if (g_tok == TK_GT) op = 1;
        else if (g_tok == TK_LE) op = 2;
        else if (g_tok == TK_GE) op = 3;
        else break;
        next_token(); emit_push(); parse_shift(); emit_pop();
        if (op == 0) emit("    slt r1, r2, r1\n");
        else if (op == 1) emit("    sgt r1, r2, r1\n");
        else if (op == 2) emit("    sle r1, r2, r1\n");
        else emit("    sge r1, r2, r1\n");
    }
}

static void parse_eq(void) {
    int op;
    parse_rel();
    for (;;) {
        if (g_tok == TK_EQ) op = 0;
        else if (g_tok == TK_NE) op = 1;
        else break;
        next_token(); emit_push(); parse_rel(); emit_pop();
        if (op == 0) emit("    seq r1, r2, r1\n");
        else emit("    sne r1, r2, r1\n");
    }
}

static void parse_bitand(void) {
    parse_eq();
    while (g_tok == TK_AMP) {
        next_token(); emit_push(); parse_eq(); emit_pop();
        emit("    and r1, r2, r1\n");
    }
}

static void parse_bitxor(void) {
    parse_bitand();
    while (g_tok == TK_CARET) {
        next_token(); emit_push(); parse_bitand(); emit_pop();
        emit("    xor r1, r2, r1\n");
    }
}

static void parse_bitor(void) {
    parse_bitxor();
    while (g_tok == TK_PIPE) {
        next_token(); emit_push(); parse_bitxor(); emit_pop();
        emit("    or r1, r2, r1\n");
    }
}

static void parse_and(void) {
    parse_bitor();
    while (g_tok == TK_LAND) {
        next_token(); emit_push(); parse_bitor(); emit_pop();
        emit("    sne r2, r2, r0\n    sne r1, r1, r0\n    and r1, r2, r1\n");
    }
}

static void parse_or(void) {
    parse_and();
    while (g_tok == TK_LOR) {
        next_token(); emit_push(); parse_and(); emit_pop();
        emit("    sne r2, r2, r0\n    sne r1, r1, r0\n    or r1, r2, r1\n");
    }
}

static void parse_ternary(void) {
    int le;
    int lend;
    parse_or();
    if (g_tok == TK_QMARK) {
        next_token(); le = new_label(); lend = new_label();
        emit_bz(le); parse_expr(); expect(TK_COLON);
        emit_jmp(lend); emit_ldef(le); parse_ternary(); emit_ldef(lend);
    }
}

static void parse_assign(void) {
    char sn[128];
    int off;
    int spos;
    int stok;
    int sline;
    if (g_tok == TK_IDENT) {
        str_copy(sn, g_tok_str, 128);
        off = find_local(sn);
        if (off != 99999) {
            spos = g_pos; stok = g_tok; sline = g_line;
            next_token();
            if (g_tok == TK_ASSIGN) {
                next_token(); parse_assign(); emit_stloc(off); return;
            }
            if (g_tok == TK_PLUSEQ) {
                next_token(); emit_ldloc(off); emit_push();
                parse_assign(); emit_pop(); emit("    add r1, r2, r1\n");
                emit_stloc(off); return;
            }
            if (g_tok == TK_MINUSEQ) {
                next_token(); emit_ldloc(off); emit_push();
                parse_assign(); emit_pop(); emit("    sub r1, r2, r1\n");
                emit_stloc(off); return;
            }
            g_pos = spos; g_tok = stok; g_line = sline;
            str_copy(g_tok_str, sn, 128);
        }
    }
    parse_ternary();
}

static void parse_expr(void) { parse_assign(); }

static void parse_stmt(void);

static void parse_compound(void) {
    while (g_tok != TK_RBRACE && g_tok != TK_EOF) parse_stmt();
    expect(TK_RBRACE);
}

static void parse_stmt(void) {
    int le;
    int lend;
    int lloop;
    int lbrk;
    int lcont;
    int sfl;
    int sef;
    char nb[128];
    int off;
    int fi;

    if (g_tok == TK_LBRACE) { next_token(); parse_compound(); return; }

    if (g_tok == TK_INT || g_tok == TK_CHAR_KW) {
        next_token();
        if (g_tok != TK_IDENT) { cc_error("expected var name"); return; }
        str_copy(nb, g_tok_str, 128);
        add_local(nb);
        off = find_local(nb);
        next_token();
        if (g_tok == TK_ASSIGN) { next_token(); parse_expr(); emit_stloc(off); }
        expect(TK_SEMI);
        return;
    }

    if (g_tok == TK_IF) {
        next_token(); expect(TK_LPAREN); parse_expr(); expect(TK_RPAREN);
        le = new_label(); lend = new_label();
        emit_bz(le); parse_stmt();
        if (g_tok == TK_ELSE) {
            next_token(); emit_jmp(lend); emit_ldef(le); parse_stmt(); emit_ldef(lend);
        } else { emit_ldef(le); }
        return;
    }

    if (g_tok == TK_WHILE) {
        next_token();
        lloop = new_label(); lbrk = new_label();
        emit_ldef(lloop);
        expect(TK_LPAREN); parse_expr(); expect(TK_RPAREN);
        emit_bz(lbrk);
        if (g_loop_depth < MAX_LOOP) {
            g_break_labels[g_loop_depth] = lbrk;
            g_cont_labels[g_loop_depth] = lloop;
            g_loop_depth = g_loop_depth + 1;
        }
        parse_stmt();
        g_loop_depth = g_loop_depth - 1;
        emit_jmp(lloop); emit_ldef(lbrk);
        return;
    }

    if (g_tok == TK_FOR) {
        next_token(); expect(TK_LPAREN);
        if (g_tok == TK_INT || g_tok == TK_CHAR_KW) {
            next_token();
            if (g_tok == TK_IDENT) {
                str_copy(nb, g_tok_str, 128);
                add_local(nb); off = find_local(nb);
                next_token();
                if (g_tok == TK_ASSIGN) { next_token(); parse_expr(); emit_stloc(off); }
            }
            expect(TK_SEMI);
        } else if (g_tok != TK_SEMI) { parse_expr(); expect(TK_SEMI); }
        else { next_token(); }
        lloop = new_label(); lbrk = new_label(); lcont = new_label();
        emit_ldef(lloop);
        if (g_tok != TK_SEMI) { parse_expr(); emit_bz(lbrk); }
        expect(TK_SEMI);
        sfl = g_for_len; sef = g_emit_to_for;
        g_for_len = 0; g_emit_to_for = 1;
        if (g_tok != TK_RPAREN) parse_expr();
        g_emit_to_for = sef;
        expect(TK_RPAREN);
        if (g_loop_depth < MAX_LOOP) {
            g_break_labels[g_loop_depth] = lbrk;
            g_cont_labels[g_loop_depth] = lcont;
            g_loop_depth = g_loop_depth + 1;
        }
        parse_stmt();
        g_loop_depth = g_loop_depth - 1;
        emit_ldef(lcont);
        fi = 0;
        while (fi < g_for_len) { emit_ch(g_for_buf[fi]); fi = fi + 1; }
        g_for_len = sfl;
        emit_jmp(lloop); emit_ldef(lbrk);
        return;
    }

    if (g_tok == TK_RETURN) {
        next_token();
        if (g_tok != TK_SEMI) parse_expr();
        emit_jmp(g_ret_label);
        expect(TK_SEMI);
        return;
    }

    if (g_tok == TK_BREAK) {
        next_token();
        if (g_loop_depth > 0) emit_jmp(g_break_labels[g_loop_depth - 1]);
        else cc_error("break outside loop");
        expect(TK_SEMI); return;
    }

    if (g_tok == TK_CONTINUE) {
        next_token();
        if (g_loop_depth > 0) emit_jmp(g_cont_labels[g_loop_depth - 1]);
        else cc_error("continue outside loop");
        expect(TK_SEMI); return;
    }

    if (g_tok == TK_SEMI) { next_token(); return; }

    parse_expr(); expect(TK_SEMI);
}

static void parse_function(void) {
    char fn[128];
    int np;
    int i;

    if (g_tok != TK_INT && g_tok != TK_VOID && g_tok != TK_CHAR_KW) {
        cc_error("expected type"); return;
    }
    next_token();
    if (g_tok != TK_IDENT) { cc_error("expected function name"); return; }
    str_copy(fn, g_tok_str, 128);
    next_token();

    emit(fn); emit(":\n    .global "); emit(fn); emit_ch('\n');

    expect(TK_LPAREN);
    g_nlocals = 0;
    g_local_off = -12;
    g_loop_depth = 0;
    g_ret_label = new_label();
    np = 0;

    if (g_tok != TK_RPAREN) {
        if (g_tok == TK_VOID) {
            next_token();
        } else {
            for (;;) {
                if (g_tok != TK_INT && g_tok != TK_CHAR_KW) { cc_error("expected param type"); break; }
                next_token();
                if (g_tok != TK_IDENT) { cc_error("expected param name"); break; }
                add_param(g_tok_str, np);
                np = np + 1;
                next_token();
                if (g_tok != TK_COMMA) break;
                next_token();
            }
        }
    }
    expect(TK_RPAREN);
    add_func(fn, np);
    g_local_off = -12 - np * 4;

    emit_prologue();
    i = 0;
    while (i < np && i < 8) {
        emit("    stw r30, r"); emit_num(3 + i); emit(", ");
        emit_num(-12 - i * 4); emit_ch('\n');
        i = i + 1;
    }

    expect(TK_LBRACE);
    parse_compound();
    emit_epilogue(g_ret_label);
    emit_ch('\n');
}

static void parse_program(void) {
    emit("# Generated by cc-min\n.text\n");
    while (g_tok != TK_EOF) parse_function();
}

int pass1_parse_to_ir(void) {
    g_pos = 0; g_line = 1;
    g_output_len = 0; g_label_count = 0;
    g_nfuncs = 0; g_nlocals = 0;
    g_loop_depth = 0; g_emit_to_for = 0; g_for_len = 0;
    next_token();
    parse_program();
    g_output[g_output_len] = 0;
    return 1;
}

int ccmin_get_output_len(void) { return g_output_len; }
char *ccmin_get_output_buf(void) { return g_output; }
int ccmin_get_ret_imm(void) { return 0; }
