/* cc-min pass1: parser + code generator
 * No #include <stdio.h> — cc.fth auto-declares fopen/fclose/fgetc.
 * This avoids the 32KB include-save-buffer limit in cc.fth. */

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

/* Type system */
#define TY_INT  0
#define TY_CHAR 1
#define TY_VOID 2
static int g_lval;
static int g_expr_type;

/* String pool */
#define MAX_STRINGS 128
#define STR_POOL_SZ 4096
static char g_str_pool[STR_POOL_SZ];
static int g_str_offs[MAX_STRINGS];
static int g_str_lens[MAX_STRINGS];
static int g_str_pool_len;
static int g_nstrings;

int ccmin_load_source(const char *path) {
    int f;
    int ch;
    f = fopen(path, "rb");
    if (!f) return 0;
    g_src_len = 0;
    for (;;) {
        ch = fgetc(f);
        if (ch < 0) break;
        if (g_src_len >= MAX_SRC - 1) { fclose(f); return 0; }
        g_src[g_src_len] = ch;
        g_src_len = g_src_len + 1;
    }
    fclose(f);
    g_src[g_src_len] = 0;
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
#define TK_LBRACK   59
#define TK_RBRACK   60
#define TK_STRING   61
#define TK_CHARLIT  62
#define TK_CONST    63
#define TK_STATIC   64

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

static int is_digit(char c) { return c >= '0' && c <= '9'; }

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
    /* Minimal error handler — no stdio dependency. */
    /* Errors are visible as bad/missing codegen in test output. */
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

static int scan_escape(void) {
    char c;
    g_pos = g_pos + 1;
    if (g_pos >= g_src_len) return 0;
    c = g_src[g_pos];
    g_pos = g_pos + 1;
    if (c == 'n') return 10;
    if (c == 't') return 9;
    if (c == 'r') return 13;
    if (c == '0') return 0;
    if (c == '\\') return 92;
    if (c == '\'') return 39;
    if (c == '"') return 34;
    return c;
}

static void next_token(void) {
    char c;
    int v;
    int i;
    int si;
    skip_ws();
    if (g_pos >= g_src_len) { g_tok = TK_EOF; return; }
    c = g_src[g_pos];
    if (is_digit(c)) {
        v = 0;
        if (c == '0' && g_pos + 1 < g_src_len && g_src[g_pos+1] == 'x') {
            g_pos = g_pos + 2;
            while (g_pos < g_src_len) {
                c = g_src[g_pos];
                if (c >= '0' && c <= '9') v = v * 16 + (c - '0');
                else if (c >= 'a' && c <= 'f') v = v * 16 + (c - 'a' + 10);
                else if (c >= 'A' && c <= 'F') v = v * 16 + (c - 'A' + 10);
                else break;
                g_pos = g_pos + 1;
            }
        } else {
            while (g_pos < g_src_len && is_digit(g_src[g_pos])) {
                v = v * 10 + (g_src[g_pos] - '0');
                g_pos = g_pos + 1;
            }
        }
        g_tok = TK_NUM; g_tok_val = v; return;
    }
    if (c == '\'') {
        g_pos = g_pos + 1;
        if (g_pos < g_src_len && g_src[g_pos] == '\\') {
            v = scan_escape();
        } else {
            v = g_src[g_pos]; g_pos = g_pos + 1;
        }
        if (g_pos < g_src_len && g_src[g_pos] == '\'') g_pos = g_pos + 1;
        g_tok = TK_CHARLIT; g_tok_val = v; return;
    }
    if (c == '"') {
        g_pos = g_pos + 1;
        si = g_str_pool_len;
        while (g_pos < g_src_len && g_src[g_pos] != '"') {
            if (g_src[g_pos] == '\\') {
                v = scan_escape();
            } else {
                v = g_src[g_pos]; g_pos = g_pos + 1;
            }
            if (g_str_pool_len < STR_POOL_SZ) {
                g_str_pool[g_str_pool_len] = v;
                g_str_pool_len = g_str_pool_len + 1;
            }
        }
        if (g_pos < g_src_len) g_pos = g_pos + 1;
        if (g_str_pool_len < STR_POOL_SZ) {
            g_str_pool[g_str_pool_len] = 0;
            g_str_pool_len = g_str_pool_len + 1;
        }
        if (g_nstrings < MAX_STRINGS) {
            g_str_offs[g_nstrings] = si;
            g_str_lens[g_nstrings] = g_str_pool_len - si;
            g_tok = TK_STRING; g_tok_val = g_nstrings;
            g_nstrings = g_nstrings + 1;
        }
        return;
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
        if (str_eq(g_tok_str, "const")) { g_tok = TK_CONST; return; }
        if (str_eq(g_tok_str, "static")) { g_tok = TK_STATIC; return; }
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
    if (c == '[') { g_tok = TK_LBRACK; return; }
    if (c == ']') { g_tok = TK_RBRACK; return; }
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

/* ---- Symbol tables ---- */
#define MAX_LOCALS 64
#define MAX_FUNCS 128
#define NAMESZ 32
#define LNBUF_SZ 2048
#define FNBUF_SZ 4096
static char g_lnames[LNBUF_SZ];
static int g_loffs[MAX_LOCALS];
static int g_ltypes[MAX_LOCALS];
static int g_lsizes[MAX_LOCALS];
static int g_nlocals;
static int g_local_off;

static char g_fnames[FNBUF_SZ];
static int g_fparams[MAX_FUNCS];
static int g_fret[MAX_FUNCS];
static int g_nfuncs;

#define MAX_GLOBALS 64
#define GNBUF_SZ 2048
static char g_gnames[GNBUF_SZ];
static int g_gtypes[MAX_GLOBALS];
static int g_gsizes[MAX_GLOBALS];
static int g_gbytes[MAX_GLOBALS];
static int g_nglobals;

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
        if (sym_name_eq(g_lnames, i, name)) return i;
        i = i + 1;
    }
    return -1;
}

static void add_local(const char *name, int type, int arr_cnt) {
    int sz;
    int esz;
    if (g_nlocals >= MAX_LOCALS) { cc_error("too many locals"); return; }
    if (arr_cnt > 0) {
        esz = 4;
        if ((type & 255) == TY_CHAR && ((type >> 8) & 255) == 0) esz = 1;
        sz = arr_cnt * esz;
        sz = ((sz + 3) / 4) * 4;
    } else {
        sz = 4;
    }
    g_local_off = g_local_off - sz;
    sym_set_name(g_lnames, g_nlocals, name);
    g_loffs[g_nlocals] = g_local_off;
    g_ltypes[g_nlocals] = type;
    g_lsizes[g_nlocals] = arr_cnt;
    g_nlocals = g_nlocals + 1;
}

static void add_param(const char *name, int idx, int type) {
    int off;
    if (g_nlocals >= MAX_LOCALS) { cc_error("too many locals"); return; }
    off = -12 - idx * 4;
    sym_set_name(g_lnames, g_nlocals, name);
    g_loffs[g_nlocals] = off;
    g_ltypes[g_nlocals] = type;
    g_lsizes[g_nlocals] = 0;
    g_nlocals = g_nlocals + 1;
}

static int find_func(const char *name) {
    int i;
    i = 0;
    while (i < g_nfuncs) {
        if (sym_name_eq(g_fnames, i, name)) return i;
        i = i + 1;
    }
    return -1;
}

static void add_func(const char *name, int np, int rtype) {
    if (g_nfuncs >= MAX_FUNCS) { cc_error("too many funcs"); return; }
    sym_set_name(g_fnames, g_nfuncs, name);
    g_fparams[g_nfuncs] = np;
    g_fret[g_nfuncs] = rtype;
    g_nfuncs = g_nfuncs + 1;
}

static int find_global(const char *name) {
    int i;
    i = 0;
    while (i < g_nglobals) {
        if (sym_name_eq(g_gnames, i, name)) return i;
        i = i + 1;
    }
    return -1;
}

static void add_global(const char *name, int type, int arr_cnt, int bsz) {
    if (g_nglobals >= MAX_GLOBALS) { cc_error("too many globals"); return; }
    sym_set_name(g_gnames, g_nglobals, name);
    g_gtypes[g_nglobals] = type;
    g_gsizes[g_nglobals] = arr_cnt;
    g_gbytes[g_nglobals] = bsz;
    g_nglobals = g_nglobals + 1;
}

/* ---- Emit helpers ---- */
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
    emit("    addi r29, r29, -256\n    stw r29, r31, 252\n    stw r29, r30, 248\n    addi r30, r29, 256\n");
}

static void emit_epilogue(int rl) {
    emit_ldef(rl);
    emit("    ldw r31, r29, 252\n    ldw r30, r29, 248\n    addi r29, r29, 256\n    jalr r0, r31, 0\n");
}

static void emit_call(const char *name) {
    emit("    jal r31, "); emit(name); emit_ch('\n');
}

static void emit_sym_name(char *buf, int idx) {
    int base;
    int i;
    base = idx * NAMESZ;
    i = 0;
    while (buf[base + i] != 0) { emit_ch(buf[base + i]); i = i + 1; }
}

static void lval_to_rval(void) {
    if (!g_lval) return;
    if ((g_expr_type & 255) == TY_CHAR && ((g_expr_type >> 8) & 255) == 0)
        emit("    ldb r1, r1, 0\n");
    else
        emit("    ldw r1, r1, 0\n");
    g_lval = 0;
}

static void emit_store_ind(int type) {
    if ((type & 255) == TY_CHAR && ((type >> 8) & 255) == 0)
        emit("    stb r2, r1, 0\n");
    else
        emit("    stw r2, r1, 0\n");
}

static void emit_global_addr(int gidx) {
    emit("    lui r1, %hi(");
    emit_sym_name(g_gnames, gidx);
    emit(")\n    addi r1, r1, %lo(");
    emit_sym_name(g_gnames, gidx);
    emit(")\n");
}

/* ---- Expression parser ---- */
static void parse_expr(void);
static void parse_assign(void);

static void parse_primary(void) {
    char sn[128];
    int nargs;
    int k;
    int li;
    int gi;
    int fi;
    int bt;
    int pt;
    int sid;
    if (g_tok == TK_NUM) {
        emit_li(g_tok_val); g_lval = 0; g_expr_type = TY_INT;
        next_token(); return;
    }
    if (g_tok == TK_CHARLIT) {
        emit_li(g_tok_val); g_lval = 0; g_expr_type = TY_INT;
        next_token(); return;
    }
    if (g_tok == TK_STRING) {
        sid = g_tok_val;
        emit("    lui r1, %hi(.Lstr_"); emit_num(sid);
        emit(")\n    addi r1, r1, %lo(.Lstr_"); emit_num(sid);
        emit(")\n");
        g_lval = 0; g_expr_type = TY_CHAR | (1 << 8);
        next_token(); return;
    }
    if (g_tok == TK_LPAREN) {
        next_token(); parse_expr(); expect(TK_RPAREN); return;
    }
    if (g_tok == TK_IDENT) {
        str_copy(sn, g_tok_str, 128);
        next_token();
        if (g_tok == TK_LPAREN) {
            next_token();
            nargs = 0;
            if (g_tok != TK_RPAREN) {
                parse_expr(); lval_to_rval(); emit_push(); nargs = 1;
                while (g_tok == TK_COMMA) {
                    next_token(); parse_expr(); lval_to_rval(); emit_push(); nargs = nargs + 1;
                }
            }
            expect(TK_RPAREN);
            k = nargs;
            while (k > 0) {
                k = k - 1;
                emit("    ldw r"); emit_num(3 + k); emit(", r29, 0\n    addi r29, r29, 4\n");
            }
            emit_call(sn);
            g_lval = 0;
            fi = find_func(sn);
            if (fi >= 0) g_expr_type = g_fret[fi];
            else g_expr_type = TY_INT;
            return;
        }
        li = find_local(sn);
        if (li >= 0) {
            emit("    addi r1, r30, "); emit_num(g_loffs[li]); emit_ch('\n');
            if (g_lsizes[li] > 0) {
                bt = g_ltypes[li] & 255;
                pt = (g_ltypes[li] >> 8) & 255;
                g_lval = 0;
                g_expr_type = bt | ((pt + 1) << 8);
            } else {
                g_lval = 1;
                g_expr_type = g_ltypes[li];
            }
            return;
        }
        gi = find_global(sn);
        if (gi >= 0) {
            emit_global_addr(gi);
            if (g_gsizes[gi] > 0) {
                bt = g_gtypes[gi] & 255;
                pt = (g_gtypes[gi] >> 8) & 255;
                g_lval = 0;
                g_expr_type = bt | ((pt + 1) << 8);
            } else {
                g_lval = 1;
                g_expr_type = g_gtypes[gi];
            }
            return;
        }
        cc_error("undefined variable");
        return;
    }
    cc_error("expected expression");
}

static void parse_postfix(void) {
    int btype;
    int ebt;
    int ept;
    int esz;
    parse_primary();
    while (g_tok == TK_LBRACK) {
        btype = g_expr_type;
        lval_to_rval();
        emit_push();
        next_token();
        parse_expr();
        lval_to_rval();
        ept = (btype >> 8) & 255;
        ebt = btype & 255;
        if (ept > 0) ept = ept - 1;
        esz = 4;
        if (ebt == TY_CHAR && ept == 0) esz = 1;
        if (esz == 4) emit("    slli r1, r1, 2\n");
        emit_pop();
        emit("    add r1, r2, r1\n");
        g_lval = 1;
        g_expr_type = ebt | (ept << 8);
        expect(TK_RBRACK);
    }
}

static void parse_unary(void) {
    int ty;
    int bt;
    int pt;
    if (g_tok == TK_BANG) {
        next_token(); parse_unary(); lval_to_rval();
        emit("    seq r1, r1, r0\n");
        g_lval = 0; g_expr_type = TY_INT; return;
    }
    if (g_tok == TK_MINUS) {
        next_token(); parse_unary(); lval_to_rval();
        emit("    sub r1, r0, r1\n");
        g_lval = 0; g_expr_type = TY_INT; return;
    }
    if (g_tok == TK_TILDE) {
        next_token(); parse_unary(); lval_to_rval();
        emit("    xori r1, r1, -1\n");
        g_lval = 0; g_expr_type = TY_INT; return;
    }
    if (g_tok == TK_STAR) {
        next_token(); parse_unary(); lval_to_rval();
        pt = (g_expr_type >> 8) & 255;
        bt = g_expr_type & 255;
        if (pt > 0) g_expr_type = bt | ((pt - 1) << 8);
        g_lval = 1;
        return;
    }
    if (g_tok == TK_AMP) {
        next_token(); parse_unary();
        if (!g_lval) { cc_error("& requires lvalue"); return; }
        g_lval = 0;
        bt = g_expr_type & 255;
        pt = (g_expr_type >> 8) & 255;
        g_expr_type = bt | ((pt + 1) << 8);
        return;
    }
    if (g_tok == TK_INC) {
        next_token(); parse_unary();
        if (g_lval) {
            ty = g_expr_type;
            emit("    addi r2, r1, 0\n");
            lval_to_rval();
            emit("    addi r1, r1, 1\n");
            emit_store_ind(ty);
            g_lval = 0;
        } else {
            emit("    addi r1, r1, 1\n");
        }
        return;
    }
    if (g_tok == TK_DEC) {
        next_token(); parse_unary();
        if (g_lval) {
            ty = g_expr_type;
            emit("    addi r2, r1, 0\n");
            lval_to_rval();
            emit("    addi r1, r1, -1\n");
            emit_store_ind(ty);
            g_lval = 0;
        } else {
            emit("    addi r1, r1, -1\n");
        }
        return;
    }
    parse_postfix();
}

/* Precedence climbing for binary operators */
static int binop_prec(int tok) {
    if (tok == TK_STAR || tok == TK_SLASH || tok == TK_PERCENT) return 10;
    if (tok == TK_PLUS || tok == TK_MINUS) return 9;
    if (tok == TK_LSHIFT || tok == TK_RSHIFT) return 8;
    if (tok == TK_LT || tok == TK_GT || tok == TK_LE || tok == TK_GE) return 7;
    if (tok == TK_EQ || tok == TK_NE) return 6;
    if (tok == TK_AMP) return 5;
    if (tok == TK_CARET) return 4;
    if (tok == TK_PIPE) return 3;
    if (tok == TK_LAND) return 2;
    if (tok == TK_LOR) return 1;
    return 0;
}

static void emit_binop(int tok) {
    if (tok == TK_STAR) emit("    mul r1, r2, r1\n");
    else if (tok == TK_SLASH) emit("    div r1, r2, r1\n");
    else if (tok == TK_PERCENT) emit("    rem r1, r2, r1\n");
    else if (tok == TK_PLUS) emit("    add r1, r2, r1\n");
    else if (tok == TK_MINUS) emit("    sub r1, r2, r1\n");
    else if (tok == TK_LSHIFT) emit("    sll r1, r2, r1\n");
    else if (tok == TK_RSHIFT) emit("    sra r1, r2, r1\n");
    else if (tok == TK_LT) emit("    slt r1, r2, r1\n");
    else if (tok == TK_GT) emit("    sgt r1, r2, r1\n");
    else if (tok == TK_LE) emit("    sle r1, r2, r1\n");
    else if (tok == TK_GE) emit("    sge r1, r2, r1\n");
    else if (tok == TK_EQ) emit("    seq r1, r2, r1\n");
    else if (tok == TK_NE) emit("    sne r1, r2, r1\n");
    else if (tok == TK_AMP) emit("    and r1, r2, r1\n");
    else if (tok == TK_CARET) emit("    xor r1, r2, r1\n");
    else if (tok == TK_PIPE) emit("    or r1, r2, r1\n");
    else if (tok == TK_LAND) emit("    sne r2, r2, r0\n    sne r1, r1, r0\n    and r1, r2, r1\n");
    else if (tok == TK_LOR) emit("    sne r2, r2, r0\n    sne r1, r1, r0\n    or r1, r2, r1\n");
}

static void parse_binop(int min_prec) {
    int op;
    int p;
    parse_unary();
    for (;;) {
        p = binop_prec(g_tok);
        if (p < min_prec) break;
        op = g_tok;
        lval_to_rval();
        next_token();
        emit_push();
        parse_binop(p + 1);
        lval_to_rval();
        emit_pop();
        emit_binop(op);
        g_lval = 0; g_expr_type = TY_INT;
    }
}

static void parse_ternary(void) {
    int le;
    int lend;
    parse_binop(1);
    if (g_tok == TK_QMARK) {
        lval_to_rval();
        next_token(); le = new_label(); lend = new_label();
        emit_bz(le); parse_expr(); lval_to_rval(); expect(TK_COLON);
        emit_jmp(lend); emit_ldef(le); parse_ternary(); lval_to_rval(); emit_ldef(lend);
    }
}

static void parse_assign(void) {
    int sty;
    parse_ternary();
    if (g_tok == TK_ASSIGN) {
        if (!g_lval) { cc_error("assign to non-lvalue"); }
        sty = g_expr_type;
        emit_push();
        next_token(); parse_assign(); lval_to_rval();
        emit_pop();
        emit_store_ind(sty);
        g_lval = 0;
        return;
    }
    if (g_tok == TK_PLUSEQ) {
        if (!g_lval) { cc_error("+= to non-lvalue"); }
        sty = g_expr_type;
        emit("    addi r2, r1, 0\n");
        emit_push();
        lval_to_rval();
        emit_push();
        next_token(); parse_assign(); lval_to_rval();
        emit_pop();
        emit("    add r1, r2, r1\n");
        emit_pop();
        emit_store_ind(sty);
        g_lval = 0;
        return;
    }
    if (g_tok == TK_MINUSEQ) {
        if (!g_lval) { cc_error("-= to non-lvalue"); }
        sty = g_expr_type;
        emit("    addi r2, r1, 0\n");
        emit_push();
        lval_to_rval();
        emit_push();
        next_token(); parse_assign(); lval_to_rval();
        emit_pop();
        emit("    sub r1, r2, r1\n");
        emit_pop();
        emit_store_ind(sty);
        g_lval = 0;
        return;
    }
}

static void parse_expr(void) { parse_assign(); }

/* ---- Statement parser ---- */
static void parse_stmt(void);

static void parse_compound(void) {
    while (g_tok != TK_RBRACE && g_tok != TK_EOF) parse_stmt();
    expect(TK_RBRACE);
}

static int parse_base_type(void) {
    int bt;
    if (g_tok == TK_CONST) next_token();
    if (g_tok == TK_INT) { bt = TY_INT; next_token(); }
    else if (g_tok == TK_CHAR_KW) { bt = TY_CHAR; next_token(); }
    else if (g_tok == TK_VOID) { bt = TY_VOID; next_token(); }
    else { bt = TY_INT; }
    return bt;
}

static int parse_ptr_stars(void) {
    int p;
    p = 0;
    while (g_tok == TK_STAR) { p = p + 1; next_token(); }
    return p;
}

static void parse_local_decl(void) {
    int bt;
    int pt;
    int type;
    int arr;
    int off;
    char nb[128];
    bt = parse_base_type();
    pt = parse_ptr_stars();
    type = bt | (pt << 8);
    if (g_tok != TK_IDENT) { cc_error("expected var name"); return; }
    str_copy(nb, g_tok_str, 128);
    next_token();
    if (g_tok == TK_LBRACK) {
        next_token();
        arr = g_tok_val;
        expect(TK_NUM);
        expect(TK_RBRACK);
        add_local(nb, type, arr);
    } else {
        add_local(nb, type, 0);
        off = g_loffs[g_nlocals - 1];
        if (g_tok == TK_ASSIGN) {
            next_token(); parse_expr(); lval_to_rval();
            emit("    stw r30, r1, "); emit_num(off); emit_ch('\n');
        }
    }
    expect(TK_SEMI);
}

static void parse_stmt(void) {
    int le;
    int lend;
    int lloop;
    int lbrk;
    int lcont;
    int sfl;
    int sef;
    int fi;
    int off;
    int bt;
    int pt;
    int type;
    char nb[128];

    if (g_tok == TK_LBRACE) { next_token(); parse_compound(); return; }

    if (g_tok == TK_INT || g_tok == TK_CHAR_KW || g_tok == TK_CONST) {
        parse_local_decl();
        return;
    }

    if (g_tok == TK_IF) {
        next_token(); expect(TK_LPAREN); parse_expr(); lval_to_rval(); expect(TK_RPAREN);
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
        expect(TK_LPAREN); parse_expr(); lval_to_rval(); expect(TK_RPAREN);
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
        if (g_tok == TK_INT || g_tok == TK_CHAR_KW || g_tok == TK_CONST) {
            bt = parse_base_type();
            pt = parse_ptr_stars();
            type = bt | (pt << 8);
            if (g_tok == TK_IDENT) {
                str_copy(nb, g_tok_str, 128);
                add_local(nb, type, 0);
                off = g_loffs[g_nlocals - 1];
                next_token();
                if (g_tok == TK_ASSIGN) {
                    next_token(); parse_expr(); lval_to_rval();
                    emit("    stw r30, r1, "); emit_num(off); emit_ch('\n');
                }
            }
            expect(TK_SEMI);
        } else if (g_tok != TK_SEMI) { parse_expr(); lval_to_rval(); expect(TK_SEMI); }
        else { next_token(); }
        lloop = new_label(); lbrk = new_label(); lcont = new_label();
        emit_ldef(lloop);
        if (g_tok != TK_SEMI) { parse_expr(); lval_to_rval(); emit_bz(lbrk); }
        expect(TK_SEMI);
        sfl = g_for_len; sef = g_emit_to_for;
        g_for_len = 0; g_emit_to_for = 1;
        if (g_tok != TK_RPAREN) { parse_expr(); lval_to_rval(); }
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
        if (g_tok != TK_SEMI) { parse_expr(); lval_to_rval(); }
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

    parse_expr(); lval_to_rval(); expect(TK_SEMI);
}

/* ---- Function and program parsers ---- */
static void parse_function(const char *fname, int rtype) {
    int np;
    int i;
    int bt;
    int pt;
    int ptype;

    emit(fname); emit(":\n    .global "); emit(fname); emit_ch('\n');

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
                if (g_tok == TK_CONST) next_token();
                if (g_tok != TK_INT && g_tok != TK_CHAR_KW && g_tok != TK_VOID) {
                    cc_error("expected param type"); break;
                }
                bt = TY_INT;
                if (g_tok == TK_CHAR_KW) bt = TY_CHAR;
                next_token();
                pt = parse_ptr_stars();
                ptype = bt | (pt << 8);
                if (g_tok != TK_IDENT) { cc_error("expected param name"); break; }
                add_param(g_tok_str, np, ptype);
                np = np + 1;
                next_token();
                if (g_tok != TK_COMMA) break;
                next_token();
            }
        }
    }
    expect(TK_RPAREN);
    add_func(fname, np, rtype);
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

static void parse_global_decl(const char *name, int type) {
    int arr;
    int esz;
    int bsz;
    if (g_tok == TK_LBRACK) {
        next_token();
        arr = g_tok_val;
        expect(TK_NUM);
        expect(TK_RBRACK);
        esz = 4;
        if ((type & 255) == TY_CHAR && ((type >> 8) & 255) == 0) esz = 1;
        bsz = arr * esz;
        bsz = ((bsz + 3) / 4) * 4;
        add_global(name, type, arr, bsz);
    } else {
        add_global(name, type, 0, 4);
    }
    expect(TK_SEMI);
}

static void emit_strings(void) {
    int i;
    int j;
    int off;
    int len;
    int bv;
    if (g_nstrings == 0) return;
    emit(".data\n");
    i = 0;
    while (i < g_nstrings) {
        emit(".align 4\n.Lstr_"); emit_num(i); emit(":\n");
        off = g_str_offs[i];
        len = g_str_lens[i];
        j = 0;
        while (j < len) {
            emit(".byte ");
            bv = g_str_pool[off + j];
            if (bv < 0) bv = bv + 256;
            emit_num(bv);
            emit_ch('\n');
            j = j + 1;
        }
        i = i + 1;
    }
}

static void emit_globals_bss(void) {
    int i;
    if (g_nglobals == 0) return;
    emit(".bss\n");
    i = 0;
    while (i < g_nglobals) {
        emit(".align 4\n");
        emit_sym_name(g_gnames, i);
        emit(":\n    .global ");
        emit_sym_name(g_gnames, i);
        emit("\n    .space ");
        emit_num(g_gbytes[i]);
        emit_ch('\n');
        i = i + 1;
    }
}

static void parse_program(void) {
    int bt;
    int pt;
    int rtype;
    char fn[128];
    emit("# Generated by cc-min\n.text\n");
    while (g_tok != TK_EOF) {
        if (g_tok == TK_STATIC) next_token();
        if (g_tok == TK_CONST) next_token();
        bt = TY_INT;
        if (g_tok == TK_INT) { bt = TY_INT; next_token(); }
        else if (g_tok == TK_CHAR_KW) { bt = TY_CHAR; next_token(); }
        else if (g_tok == TK_VOID) { bt = TY_VOID; next_token(); }
        else { next_token(); }
        pt = parse_ptr_stars();
        rtype = bt | (pt << 8);
        if (g_tok != TK_IDENT) { cc_error("expected name"); break; }
        str_copy(fn, g_tok_str, 128);
        next_token();
        if (g_tok == TK_LPAREN) {
            parse_function(fn, rtype);
        } else {
            parse_global_decl(fn, rtype);
        }
    }
    emit_strings();
    emit_globals_bss();
}

int pass1_parse_to_ir(void) {
    g_pos = 0; g_line = 1;
    g_output_len = 0; g_label_count = 0;
    g_nfuncs = 0; g_nlocals = 0; g_nglobals = 0;
    g_loop_depth = 0; g_emit_to_for = 0; g_for_len = 0;
    g_lval = 0; g_expr_type = 0;
    g_nstrings = 0; g_str_pool_len = 0;
    next_token();
    parse_program();
    g_output[g_output_len] = 0;
    return 1;
}

int ccmin_get_output_len(void) { return g_output_len; }
char *ccmin_get_output_buf(void) { return g_output; }
int ccmin_get_ret_imm(void) { return 0; }
