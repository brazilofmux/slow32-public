#include <stdio.h>
#include <stdint.h>

#define MAX_SRC 8192

static char g_src[MAX_SRC];
static uint32_t g_pos;
static int g_have_x;
static int g_have_y;
static int g_x_val;
static int g_y_val;
static int g_have_helper;
static int g_helper_ret;
static int g_helper_has_param;
static int g_helper_has_two_params;
static int g_helper_addend;
static int g_helper_twoarg_if_lt;

static int read_file(const char *path, char *buf, uint32_t max_len, uint32_t *out_len) {
    FILE *f;
    uint32_t n;
    int ch;

    f = fopen(path, "rb");
    if (!f) return 0;
    n = 0;
    for (;;) {
        ch = fgetc(f);
        if (ch == EOF) break;
        if (n + 1 >= max_len) {
            fclose(f);
            return 0;
        }
        buf[n] = (char)ch;
        n = n + 1;
    }
    fclose(f);
    buf[n] = 0;
    *out_len = n;
    return 1;
}

static int is_space_char(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

static int is_alpha_char(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static int is_digit_char(char c) {
    return c >= '0' && c <= '9';
}

static void skip_space(void) {
    while (is_space_char(g_src[g_pos])) g_pos = g_pos + 1;
}

static int consume_char(char ch) {
    skip_space();
    if (g_src[g_pos] != ch) return 0;
    g_pos = g_pos + 1;
    return 1;
}

static int consume_kw(const char *kw) {
    uint32_t i = 0;
    skip_space();
    while (kw[i] != 0) {
        if (g_src[g_pos + i] != kw[i]) return 0;
        i = i + 1;
    }
    if (is_alpha_char(g_src[g_pos + i]) || is_digit_char(g_src[g_pos + i])) return 0;
    g_pos = g_pos + i;
    return 1;
}

static int parse_int_lit(int *out_v) {
    int sign = 1;
    int v = 0;
    int any = 0;

    skip_space();
    if (g_src[g_pos] == '-') {
        sign = -1;
        g_pos = g_pos + 1;
    }
    while (is_digit_char(g_src[g_pos])) {
        any = 1;
        v = v * 10 + (int)(g_src[g_pos] - '0');
        g_pos = g_pos + 1;
    }
    if (!any) return 0;
    *out_v = v * sign;
    return 1;
}

static int parse_expr(int *out_v);

static int parse_helper_call_value(int *out_v) {
    int arg_v;
    int arg_v2;

    if (!consume_kw("helper")) return 0;
    if (!g_have_helper) return 0;
    if (!consume_char('(')) return 0;
    if (g_helper_has_param) {
        if (!parse_expr(&arg_v)) return 0;
        if (!consume_char(')')) return 0;
        *out_v = arg_v + g_helper_addend;
        return 1;
    }
    if (g_helper_has_two_params) {
        if (!parse_expr(&arg_v)) return 0;
        if (!consume_char(',')) return 0;
        if (!parse_expr(&arg_v2)) return 0;
        if (!consume_char(')')) return 0;
        if (g_helper_twoarg_if_lt) {
            *out_v = (arg_v < arg_v2) ? arg_v : arg_v2;
        } else {
            *out_v = arg_v + arg_v2;
        }
        return 1;
    }
    if (!consume_char(')')) return 0;
    *out_v = g_helper_ret;
    return 1;
}

static int parse_primary(int *out_v) {
    int v;
    skip_space();
    if (consume_char('(')) {
        if (!parse_expr(&v)) return 0;
        if (!consume_char(')')) return 0;
        *out_v = v;
        return 1;
    }
    if (consume_kw("x")) {
        if (!g_have_x) return 0;
        *out_v = g_x_val;
        return 1;
    }
    if (consume_kw("y")) {
        if (!g_have_y) return 0;
        *out_v = g_y_val;
        return 1;
    }
    if (parse_helper_call_value(out_v)) return 1;
    return parse_int_lit(out_v);
}

static int parse_unary(int *out_v) {
    int v;
    skip_space();
    if (consume_char('!')) {
        if (!parse_unary(&v)) return 0;
        *out_v = (v == 0) ? 1 : 0;
        return 1;
    }
    return parse_primary(out_v);
}

static int parse_mul(int *out_v) {
    int lhs;
    int rhs;
    char op;

    if (!parse_unary(&lhs)) return 0;
    for (;;) {
        skip_space();
        op = g_src[g_pos];
        if (op != '*' && op != '/') break;
        g_pos = g_pos + 1;
        if (!parse_unary(&rhs)) return 0;
        if (op == '*') {
            lhs = lhs * rhs;
        } else {
            if (rhs == 0) return 0;
            lhs = lhs / rhs;
        }
    }
    *out_v = lhs;
    return 1;
}

static int parse_add(int *out_v) {
    int lhs;
    int rhs;
    char op;

    if (!parse_mul(&lhs)) return 0;
    for (;;) {
        skip_space();
        op = g_src[g_pos];
        if (op != '+' && op != '-') break;
        g_pos = g_pos + 1;
        if (!parse_mul(&rhs)) return 0;
        if (op == '+') {
            lhs = lhs + rhs;
        } else {
            lhs = lhs - rhs;
        }
    }
    *out_v = lhs;
    return 1;
}

static int parse_rel(int *out_v) {
    int lhs;
    int rhs;

    if (!parse_add(&lhs)) return 0;
    for (;;) {
        skip_space();
        if (g_src[g_pos] == '<' && g_src[g_pos + 1] == '=') {
            g_pos = g_pos + 2;
            if (!parse_add(&rhs)) return 0;
            lhs = (lhs <= rhs) ? 1 : 0;
        } else if (g_src[g_pos] == '>' && g_src[g_pos + 1] == '=') {
            g_pos = g_pos + 2;
            if (!parse_add(&rhs)) return 0;
            lhs = (lhs >= rhs) ? 1 : 0;
        } else if (g_src[g_pos] == '<') {
            g_pos = g_pos + 1;
            if (!parse_add(&rhs)) return 0;
            lhs = (lhs < rhs) ? 1 : 0;
        } else if (g_src[g_pos] == '>') {
            g_pos = g_pos + 1;
            if (!parse_add(&rhs)) return 0;
            lhs = (lhs > rhs) ? 1 : 0;
        } else {
            break;
        }
    }
    *out_v = lhs;
    return 1;
}

static int parse_expr(int *out_v) {
    int lhs;
    int rhs;

    if (!parse_rel(&lhs)) return 0;
    for (;;) {
        skip_space();
        if (g_src[g_pos] == '=' && g_src[g_pos + 1] == '=') {
            g_pos = g_pos + 2;
            if (!parse_rel(&rhs)) return 0;
            lhs = (lhs == rhs) ? 1 : 0;
        } else if (g_src[g_pos] == '!' && g_src[g_pos + 1] == '=') {
            g_pos = g_pos + 2;
            if (!parse_rel(&rhs)) return 0;
            lhs = (lhs != rhs) ? 1 : 0;
        } else {
            break;
        }
    }
    *out_v = lhs;
    return 1;
}

static int parse_program_return_value(int *out_ret) {
    int v;
    int yv;
    int helper_v;
    int helper_addend;
    int helper_mode_param;
    int helper_mode_two_param;
    int helper_step;
    int cond_v;
    int then_v;
    int else_v;
    int step_v;
    int iter;
    g_have_x = 0;
    g_have_y = 0;
    g_x_val = 0;
    g_y_val = 0;
    g_have_helper = 0;
    g_helper_ret = 0;
    g_helper_has_param = 0;
    g_helper_has_two_params = 0;
    g_helper_addend = 0;
    g_helper_twoarg_if_lt = 0;
    if (!consume_kw("int")) return 0;
    if (consume_kw("helper")) {
        if (!consume_char('(')) return 0;
        helper_mode_param = 0;
        helper_mode_two_param = 0;
        if (!consume_char(')')) {
            if (consume_kw("void")) {
                if (!consume_char(')')) return 0;
                helper_mode_param = 0;
            } else {
                if (!consume_kw("int")) return 0;
                if (!consume_kw("a")) return 0;
                if (consume_char(',')) {
                    if (!consume_kw("int")) return 0;
                    if (!consume_kw("b")) return 0;
                    helper_mode_param = 0;
                    helper_mode_two_param = 1;
                } else {
                    helper_mode_param = 1;
                }
                if (!consume_char(')')) return 0;
            }
        }
        if (!consume_char('{')) return 0;
        if (helper_mode_two_param) {
            if (consume_kw("if")) {
                /* Tiny helper-two-arg conditional form:
                   if (a < b) return a; return b;
                */
                if (!consume_char('(')) return 0;
                if (!consume_kw("a")) return 0;
                if (!consume_char('<')) return 0;
                if (!consume_kw("b")) return 0;
                if (!consume_char(')')) return 0;
                if (!consume_kw("return")) return 0;
                if (!consume_kw("a")) return 0;
                if (!consume_char(';')) return 0;
                if (!consume_kw("return")) return 0;
                if (!consume_kw("b")) return 0;
                g_helper_twoarg_if_lt = 1;
            } else {
                if (!consume_kw("return")) return 0;
                if (!consume_kw("a")) return 0;
                if (!consume_char('+')) return 0;
                if (!consume_kw("b")) return 0;
                g_helper_twoarg_if_lt = 0;
            }
            g_helper_has_two_params = 1;
            g_helper_has_param = 0;
            g_helper_addend = 0;
            g_helper_ret = 0;
        } else if (helper_mode_param) {
            helper_addend = 0;
            if (consume_kw("int")) {
                /* Tiny helper-local form: int t; t = a [+/- n]; return t; */
                if (!consume_kw("t")) return 0;
                if (!consume_char(';')) return 0;
                if (!consume_kw("t")) return 0;
                if (!consume_char('=')) return 0;
                if (!consume_kw("a")) return 0;
                if (consume_char('+')) {
                    if (!parse_int_lit(&helper_step)) return 0;
                    helper_addend = helper_step;
                } else if (consume_char('-')) {
                    if (!parse_int_lit(&helper_step)) return 0;
                    helper_addend = -helper_step;
                }
                if (!consume_char(';')) return 0;
                if (!consume_kw("return")) return 0;
                if (!consume_kw("t")) return 0;
            } else {
                if (!consume_kw("return")) return 0;
                if (!consume_kw("a")) return 0;
                if (consume_char('+')) {
                    if (!parse_int_lit(&helper_step)) return 0;
                    helper_addend = helper_step;
                } else if (consume_char('-')) {
                    if (!parse_int_lit(&helper_step)) return 0;
                    helper_addend = -helper_step;
                }
            }
            g_helper_has_param = 1;
            g_helper_has_two_params = 0;
            g_helper_addend = helper_addend;
            g_helper_ret = 0;
        } else {
            if (!consume_kw("return")) return 0;
            if (!parse_expr(&helper_v)) return 0;
            g_helper_has_param = 0;
            g_helper_has_two_params = 0;
            g_helper_ret = helper_v;
            g_helper_addend = 0;
        }
        if (!consume_char(';')) return 0;
        if (!consume_char('}')) return 0;
        g_have_helper = 1;
        if (!consume_kw("int")) return 0;
    }
    if (!consume_kw("main")) return 0;
    if (!consume_char('(')) return 0;
    if (!consume_char(')')) {
        if (!consume_kw("void")) return 0;
        if (!consume_char(')')) return 0;
    }
    if (!consume_char('{')) return 0;

    if (consume_kw("if")) {
        /* Tiny if form: if (<expr>) return <expr>; return <expr>; */
        if (!consume_char('(')) return 0;
        if (!parse_expr(&cond_v)) return 0;
        if (!consume_char(')')) return 0;
        if (!consume_kw("return")) return 0;
        if (!parse_expr(&then_v)) return 0;
        if (!consume_char(';')) return 0;
        if (!consume_kw("return")) return 0;
        if (!parse_expr(&else_v)) return 0;
        if (!consume_char(';')) return 0;
        *out_ret = (cond_v != 0) ? then_v : else_v;
    } else if (consume_kw("int")) {
        /* Tiny local-int forms:
           int x; x = <expr>; [while...] return x;
           int x; int y; x = <expr>; y = <expr>; return y;
        */
        if (!consume_kw("x")) return 0;
        if (!consume_char(';')) return 0;
        g_have_x = 1;
        if (consume_kw("int")) {
            if (!consume_kw("y")) return 0;
            if (!consume_char(';')) return 0;
            g_have_y = 1;
        }
        if (!consume_kw("x")) return 0;
        if (!consume_char('=')) return 0;
        if (parse_helper_call_value(&v)) {
        } else {
            if (!parse_expr(&v)) return 0;
        }
        if (!consume_char(';')) return 0;
        g_x_val = v;
        if (g_have_y) {
            if (!consume_kw("y")) return 0;
            if (!consume_char('=')) return 0;
            if (!parse_expr(&yv)) return 0;
            if (!consume_char(';')) return 0;
            g_y_val = yv;
            if (!consume_kw("return")) return 0;
            if (!consume_kw("y")) return 0;
            if (!consume_char(';')) return 0;
            *out_ret = g_y_val;
            if (!consume_char('}')) return 0;
            skip_space();
            return g_src[g_pos] == 0;
        }
        if (consume_kw("while")) {
            /* Tiny while form: while (x) x = x - 1; return x; */
            if (!consume_char('(')) return 0;
            if (!consume_kw("x")) return 0;
            if (!consume_char(')')) return 0;
            if (!consume_kw("x")) return 0;
            if (!consume_char('=')) return 0;
            if (!consume_kw("x")) return 0;
            if (!consume_char('-')) return 0;
            if (!parse_int_lit(&step_v)) return 0;
            if (!consume_char(';')) return 0;
            if (!consume_kw("return")) return 0;
            if (!consume_kw("x")) return 0;
            if (!consume_char(';')) return 0;
            if (step_v != 1) return 0;
            if (v < 0) return 0;
            iter = 0;
            while (v != 0) {
                v = v - step_v;
                iter = iter + 1;
                if (iter > 100000) return 0;
            }
            g_x_val = v;
            *out_ret = v;
        } else {
            if (!consume_kw("return")) return 0;
            if (!consume_kw("x")) return 0;
            if (!consume_char(';')) return 0;
            *out_ret = v;
        }
    } else {
        if (!consume_kw("return")) return 0;
        if (parse_helper_call_value(out_ret)) {
        } else {
            if (!parse_expr(out_ret)) return 0;
        }
        if (!consume_char(';')) return 0;
    }

    if (!consume_char('}')) return 0;
    skip_space();
    return g_src[g_pos] == 0;
}

static int emit_min_asm(const char *out_path, int ret_imm) {
    FILE *f = fopen(out_path, "wb");
    if (!f) return 0;
    if (fputs("# Generated by Stage 8 cc-min spike\n", f) < 0) { fclose(f); return 0; }
    if (fputs(".text\n", f) < 0) { fclose(f); return 0; }
    if (fputs("main:\n", f) < 0) { fclose(f); return 0; }
    if (fputs("    .global main\n", f) < 0) { fclose(f); return 0; }
    if (fputs("    addi r29, r29, -256\n", f) < 0) { fclose(f); return 0; }
    if (fputs("    stw r29, r31, 252\n", f) < 0) { fclose(f); return 0; }
    if (fputs("    stw r29, r30, 248\n", f) < 0) { fclose(f); return 0; }
    if (fputs("    addi r30, r29, 256\n", f) < 0) { fclose(f); return 0; }
    if (fprintf(f, "    addi r1, r0, %d\n", ret_imm) < 0) { fclose(f); return 0; }
    if (fputs("    jal r0, .L0\n", f) < 0) { fclose(f); return 0; }
    if (fputs(".L0:\n", f) < 0) { fclose(f); return 0; }
    if (fputs("    ldw r31, r29, 252\n", f) < 0) { fclose(f); return 0; }
    if (fputs("    ldw r30, r29, 248\n", f) < 0) { fclose(f); return 0; }
    if (fputs("    addi r29, r29, 256\n", f) < 0) { fclose(f); return 0; }
    if (fputs("    jalr r0, r31, 0\n", f) < 0) { fclose(f); return 0; }
    if (fclose(f) != 0) return 0;
    return 1;
}

int main(int argc, char **argv) {
    uint32_t src_len;
    int ret_imm;
    if (argc != 3) {
        fprintf(stderr, "usage: %s <input.c> <output.s>\n", argv[0]);
        return 1;
    }
    if (!read_file(argv[1], g_src, MAX_SRC, &src_len)) {
        fprintf(stderr, "error: unable to read %s\n", argv[1]);
        return 1;
    }
    (void)src_len;
    g_pos = 0;
    if (!parse_program_return_value(&ret_imm)) {
        fprintf(stderr, "error: unsupported source shape\n");
        return 1;
    }
    if (ret_imm < -2048 || ret_imm > 2047) {
        fprintf(stderr, "error: return immediate out of range\n");
        return 1;
    }
    if (!emit_min_asm(argv[2], ret_imm)) {
        fprintf(stderr, "error: unable to write %s\n", argv[2]);
        return 1;
    }
    return 0;
}
