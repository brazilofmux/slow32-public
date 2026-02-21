/* s12cc.c -- Stage 12 C compiler
 *
 * Phase 1 narrow spike: int arithmetic, if/while, function calls.
 * Ragel lexer → recursive-descent parser → AST → tree-walk codegen.
 *
 * Usage: s12cc input.c output.s
 *
 * Compiled by stage03 s32-cc.
 */

#include "c_lexer_gen.c"

/* --- fd-based I/O wrappers (stderr=2 from lexer) --- */
int write(int fd, char *buf, int len);
int read(int fd, char *buf, int len);
int open(char *path, int flags);
int close(int fd);

int fputs(char *s, int fd) {
    int len;
    len = strlen(s);
    write(fd, s, len);
    return 0;
}

int fputc(int c, int fd) {
    char buf[1];
    buf[0] = c;
    write(fd, buf, 1);
    return c;
}

void fput_uint(int fd, int val) {
    char buf[11];
    int i;
    int d;
    int started;
    if (val == 0) { fputc(48, fd); return; }
    i = 0;
    d = 1000000000;
    started = 0;
    while (d > 0) {
        if (val >= d || started) {
            buf[i] = 48 + val / d;
            val = val - (val / d) * d;
            i = i + 1;
            started = 1;
        }
        d = d / 10;
    }
    write(fd, buf, i);
}

#include "pp.h"
#include "ast.h"
#include "parser.h"
#include "sema.h"
#include "optimize.h"
#include "hir.h"
#include "hir_lower.h"
#include "hir_ssa.h"
#include "hir_opt.h"
#include "hir_licm.h"
#include "hir_burg.h"
#include "hir_regalloc.h"
#include "hir_codegen.h"

/* --- O(n) post-emit asm cleanup ---
 * 1) remove "addi rX, rX, 0"
 * 2) remove "jal r0, .Lk" when next non-blank line is ".Lk:"
 * 3) remove duplicate consecutive "jal r0, .Lk"
 */

#define CG_MAX_LINES 200000
static int cg_lstart[CG_MAX_LINES];
static int cg_lend[CG_MAX_LINES];
static char cg_tmp[CG_MAX_OUT];

static int cg_is_ws(int ch) {
    return ch == 32 || ch == 9 || ch == 13;
}

static int cg_parse_uint_line(int s, int e, int *out) {
    int v;
    int any;
    while (s < e && cg_is_ws(cg_out[s])) s = s + 1;
    v = 0;
    any = 0;
    while (s < e && cg_out[s] >= 48 && cg_out[s] <= 57) {
        v = v * 10 + (cg_out[s] - 48);
        s = s + 1;
        any = 1;
    }
    if (!any) return 0;
    while (s < e && cg_is_ws(cg_out[s])) s = s + 1;
    if (s != e) return 0;
    *out = v;
    return 1;
}

static int cg_parse_reg_line(int s, int e, int *out) {
    if (s >= e || cg_out[s] != 114) return 0;  /* 'r' */
    return cg_parse_uint_line(s + 1, e, out);
}

static int cg_line_blank(int li) {
    int i;
    i = cg_lstart[li];
    while (i < cg_lend[li]) {
        if (cg_out[i] == 10) break;
        if (!cg_is_ws(cg_out[i])) return 0;
        i = i + 1;
    }
    return 1;
}

static int cg_line_label(int li, int *lbl) {
    int s;
    int e;
    s = cg_lstart[li];
    e = cg_lend[li];
    while (s < e && cg_is_ws(cg_out[s])) s = s + 1;
    if (s + 2 >= e) return 0;
    if (cg_out[s] != 46 || cg_out[s + 1] != 76) return 0;  /* ".L" */
    s = s + 2;
    if (s >= e) return 0;
    while (e > s && (cg_out[e - 1] == 10 || cg_is_ws(cg_out[e - 1]))) e = e - 1;
    if (e <= s || cg_out[e - 1] != 58) return 0;  /* ':' */
    return cg_parse_uint_line(s, e - 1, lbl);
}

static int cg_line_self_addi_zero(int li) {
    int s;
    int e;
    int p;
    int rd;
    int rs;
    int imm;
    s = cg_lstart[li];
    e = cg_lend[li];
    while (s < e && cg_is_ws(cg_out[s])) s = s + 1;
    /* addi */
    if (s + 4 > e) return 0;
    if (cg_out[s] != 97 || cg_out[s + 1] != 100 || cg_out[s + 2] != 100 || cg_out[s + 3] != 105) return 0;
    p = s + 4;
    while (p < e && cg_is_ws(cg_out[p])) p = p + 1;
    s = p;
    while (p < e && cg_out[p] != 44) p = p + 1;
    if (p >= e) return 0;
    if (!cg_parse_reg_line(s, p, &rd)) return 0;
    p = p + 1;
    while (p < e && cg_is_ws(cg_out[p])) p = p + 1;
    s = p;
    while (p < e && cg_out[p] != 44) p = p + 1;
    if (p >= e) return 0;
    if (!cg_parse_reg_line(s, p, &rs)) return 0;
    p = p + 1;
    if (!cg_parse_uint_line(p, e, &imm)) return 0;
    if (imm != 0) return 0;
    return rd == rs;
}

static int cg_line_jal_r0_label(int li, int *lbl) {
    int s;
    int e;
    int p;
    int rd;
    s = cg_lstart[li];
    e = cg_lend[li];
    while (s < e && cg_is_ws(cg_out[s])) s = s + 1;
    /* jal */
    if (s + 3 > e) return 0;
    if (cg_out[s] != 106 || cg_out[s + 1] != 97 || cg_out[s + 2] != 108) return 0;
    p = s + 3;
    while (p < e && cg_is_ws(cg_out[p])) p = p + 1;
    s = p;
    while (p < e && cg_out[p] != 44) p = p + 1;
    if (p >= e) return 0;
    if (!cg_parse_reg_line(s, p, &rd)) return 0;
    if (rd != 0) return 0;
    p = p + 1;
    while (p < e && cg_is_ws(cg_out[p])) p = p + 1;
    if (p + 2 >= e) return 0;
    if (cg_out[p] != 46 || cg_out[p + 1] != 76) return 0;  /* ".L" */
    return cg_parse_uint_line(p + 2, e, lbl);
}

static void cg_post_opt(void) {
    int i;
    int s;
    int nline;
    int li;
    int lj;
    int len;
    int out_len;
    int prev_jal_lbl;
    int has_prev_jal;
    int lbl;
    int tgt;
    int keep;

    /* Build line index */
    nline = 0;
    s = 0;
    i = 0;
    while (i < cg_olen) {
        if (nline < CG_MAX_LINES) {
            cg_lstart[nline] = s;
        }
        while (i < cg_olen && cg_out[i] != 10) i = i + 1;
        if (i < cg_olen && cg_out[i] == 10) i = i + 1;
        if (nline < CG_MAX_LINES) {
            cg_lend[nline] = i;
        }
        nline = nline + 1;
        s = i;
    }
    if (nline > CG_MAX_LINES) nline = CG_MAX_LINES;

    out_len = 0;
    has_prev_jal = 0;
    prev_jal_lbl = -1;

    li = 0;
    while (li < nline) {
        keep = 1;

        if (cg_line_self_addi_zero(li)) {
            keep = 0;
        } else if (cg_line_jal_r0_label(li, &tgt)) {
            /* remove duplicate consecutive jal r0, .Lk */
            if (has_prev_jal && prev_jal_lbl == tgt) {
                keep = 0;
            } else {
                /* remove jal to immediately-next non-blank label */
                lj = li + 1;
                while (lj < nline && cg_line_blank(lj)) lj = lj + 1;
                if (lj < nline && cg_line_label(lj, &lbl) && lbl == tgt) {
                    keep = 0;
                }
            }
            if (keep) {
                has_prev_jal = 1;
                prev_jal_lbl = tgt;
            }
        } else if (!cg_line_blank(li)) {
            has_prev_jal = 0;
        }

        if (keep) {
            s = cg_lstart[li];
            len = cg_lend[li] - s;
            i = 0;
            while (i < len && out_len < CG_MAX_OUT - 1) {
                cg_tmp[out_len] = cg_out[s + i];
                out_len = out_len + 1;
                i = i + 1;
            }
        }

        li = li + 1;
    }

    i = 0;
    while (i < out_len) {
        cg_out[i] = cg_tmp[i];
        i = i + 1;
    }
    cg_olen = out_len;
}

int main(int argc, char **argv) {
    int fd;
    int n;
    int i;
    int last_slash;
    char *src;
    Node *prog;

    if (argc < 3) {
        fputs("Usage: s12cc input.c output.s\n", stderr);
        return 1;
    }

    /* Extract source directory for #include resolution */
    last_slash = -1;
    i = 0;
    while (argv[1][i] != 0) {
        if (argv[1][i] == 47) last_slash = i;  /* '/' */
        i = i + 1;
    }
    if (last_slash >= 0) {
        i = 0;
        while (i <= last_slash) {
            pp_sdir[i] = argv[1][i];
            i = i + 1;
        }
        pp_sdir[i] = 0;
    } else {
        pp_sdir[0] = 0;
    }

    /* Init preprocessor state */
    pp_ndefs = 0;
    pp_skip = 0;
    pp_dep = 0;
    ps_ntypedefs = 0;

    /* Read source file */
    fd = open(argv[1], 0);
    if (fd < 0) {
        fputs("s12cc: cannot open input: ", stderr);
        fputs(argv[1], stderr);
        fputc(10, stderr);
        return 1;
    }
    src = malloc(LEX_SRC_SZ);
    if (!src) {
        fputs("s12cc: out of memory\n", stderr);
        return 1;
    }
    n = read(fd, src, LEX_SRC_SZ - 1);
    close(fd);
    if (n < 0) n = 0;
    src[n] = 0;

    /* Lex + Parse */
    lex_init(src, n);
    prog = parse_program();

    /* Semantic analysis (type propagation) */
    sema(prog);

    /* Optimize AST */
    optimize(prog);

    /* Codegen (cg_lbl not reset — parser may have allocated goto labels) */
    cg_olen = 0;
    gen_program(prog);
    cg_post_opt();

    /* Write output */
    fd = open(argv[2], 26);  /* O_WRONLY | O_CREAT | O_TRUNC */
    if (fd < 0) {
        fputs("s12cc: cannot open output: ", stderr);
        fputs(argv[2], stderr);
        fputc(10, stderr);
        return 1;
    }
    write(fd, cg_out, cg_olen);
    close(fd);

    /* Print optimization stats to stderr */
    fputs("hir_opt: ", stderr);
    fput_uint(stderr, ho_stat_elim);
    fputs(" HIR instructions eliminated\n", stderr);
    fputs("hir_licm: ", stderr);
    fput_uint(stderr, licm_stat_hoisted);
    fputs(" instructions hoisted\n", stderr);
    fputs("hir_burg: ", stderr);
    fput_uint(stderr, bg_stat_folded);
    fputs(" instructions folded\n", stderr);

    return 0;
}
