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

/* --- I/O from libc (stdio.c) --- */
int read(int fd, char *buf, int len);
int open(char *path, int flags);
int close(int fd);

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

static void print_burg_top_ops(int topn) {
    int used[BG_OP_SZ];
    int i;
    int r;
    int best;
    int bestc;

    i = 0;
    while (i < BG_OP_SZ) {
        used[i] = 0;
        i = i + 1;
    }

    fputs("hir_burg_top_ops:\n", stderr);
    r = 0;
    while (r < topn) {
        best = -1;
        bestc = 0;
        i = 0;
        while (i <= BG_MAX_OP) {
            if (!used[i] && bg_stat_sel_op[i] > bestc) {
                best = i;
                bestc = bg_stat_sel_op[i];
            }
            i = i + 1;
        }
        if (best < 0 || bestc <= 0) break;
        used[best] = 1;
        fputs("  ", stderr);
        fputs(bg_op_name(best), stderr);
        fputs("=", stderr);
        fput_uint(stderr, bestc);
        fputc(10, stderr);
        r = r + 1;
    }
}

static void print_burg_top_patterns(int topn) {
    int used[BG_MAX_PAT];
    int i;
    int r;
    int best;
    int bestc;
    int lnt;
    int rnt;

    i = 0;
    while (i < BG_MAX_PAT) {
        used[i] = 0;
        i = i + 1;
    }

    fputs("hir_burg_top_pats:\n", stderr);
    r = 0;
    while (r < topn) {
        best = -1;
        bestc = 0;
        i = 0;
        while (i < bg_npat) {
            if (!used[i] && bg_stat_sel_pat[i] > bestc) {
                best = i;
                bestc = bg_stat_sel_pat[i];
            }
            i = i + 1;
        }
        if (best < 0 || bestc <= 0) break;
        used[best] = 1;
        lnt = bg_plnt[best];
        rnt = bg_prnt[best];
        fputs("  #", stderr);
        fput_uint(stderr, best);
        fputs(" ", stderr);
        fputs(bg_op_name(bg_pop[best]), stderr);
        fputs("(", stderr);
        if (lnt < 0) fputs("_", stderr);
        else fputs(bg_nt_name(lnt), stderr);
        fputs(",", stderr);
        if (rnt < 0) fputs("_", stderr);
        else fputs(bg_nt_name(rnt), stderr);
        fputs(")->", stderr);
        fputs(bg_nt_name(bg_pnt[best]), stderr);
        fputs(" x", stderr);
        fput_uint(stderr, bestc);
        fputc(10, stderr);
        r = r + 1;
    }
}

static void print_iconst_nonimm_top_ops(int topn) {
    int used[BG_OP_SZ];
    int i;
    int r;
    int best;
    int bestc;

    i = 0;
    while (i < BG_OP_SZ) {
        used[i] = 0;
        i = i + 1;
    }

    fputs("hir_iconst_nonimm_top_ops:\n", stderr);
    r = 0;
    while (r < topn) {
        best = -1;
        bestc = 0;
        i = 0;
        while (i <= BG_MAX_OP) {
            if (!used[i] && bg_stat_iconst_nonimm_use_op[i] > bestc) {
                best = i;
                bestc = bg_stat_iconst_nonimm_use_op[i];
            }
            i = i + 1;
        }
        if (best < 0 || bestc <= 0) break;
        used[best] = 1;
        fputs("  ", stderr);
        fputs(bg_op_name(best), stderr);
        fputs("=", stderr);
        fput_uint(stderr, bestc);
        fputc(10, stderr);
        r = r + 1;
    }
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
    fputs(" HIR instructions eliminated, cse=", stderr);
    fput_uint(stderr, ho_stat_cse);
    fputs(" dse=", stderr);
    fput_uint(stderr, ho_stat_dse);
    fputs(" slf=", stderr);
    fput_uint(stderr, ho_stat_slf);
    fputs("\n", stderr);
    fputs("hir_licm: ", stderr);
    fput_uint(stderr, licm_stat_hoisted);
    fputs(" instructions hoisted\n", stderr);
    fputs("hir_burg: ", stderr);
    fput_uint(stderr, bg_stat_folded);
    fputs(" folded, ", stderr);
    fput_uint(stderr, bg_stat_cand);
    fputs(" cand, rej: uses=", stderr);
    fput_uint(stderr, bg_stat_rej_uses);
    fputs(" remat=", stderr);
    fput_uint(stderr, bg_stat_rej_remat);
    fputs(" blk=", stderr);
    fput_uint(stderr, bg_stat_rej_blk);
    fputs(" cost=", stderr);
    fput_uint(stderr, bg_stat_rej_cost);
    fputs("\n", stderr);
    fputs("hir_burg_select: total=", stderr);
    fput_uint(stderr, bg_stat_sel_total);
    fputs(" chain=", stderr);
    fput_uint(stderr, bg_stat_sel_chain);
    fputs(" imm=", stderr);
    fput_uint(stderr, bg_stat_sel_imm);
    fputs(" faddr=", stderr);
    fput_uint(stderr, bg_stat_sel_faddr);
    fputs(" saddr=", stderr);
    fput_uint(stderr, bg_stat_sel_saddr);
    fputs(" burg_skipped=", stderr);
    fput_uint(stderr, bg_stat_burg_skipped);
    fputs(" spills=", stderr);
    fput_uint(stderr, ra_stat_spills);
    fputs("\n", stderr);
    fputs("hir_imm_sel: add ", stderr);
    fput_uint(stderr, hcg_stat_imm_hit_add);
    fputs("/", stderr);
    fput_uint(stderr, hcg_stat_imm_opp_add);
    fputs(" miss=", stderr);
    fput_uint(stderr, hcg_stat_imm_miss_add);
    fputs(" | sub ", stderr);
    fput_uint(stderr, hcg_stat_imm_hit_sub);
    fputs("/", stderr);
    fput_uint(stderr, hcg_stat_imm_opp_sub);
    fputs(" miss=", stderr);
    fput_uint(stderr, hcg_stat_imm_miss_sub);
    fputs(" | logic ", stderr);
    fput_uint(stderr, hcg_stat_imm_hit_logic);
    fputs("/", stderr);
    fput_uint(stderr, hcg_stat_imm_opp_logic);
    fputs(" miss=", stderr);
    fput_uint(stderr, hcg_stat_imm_miss_logic);
    fputs(" | shift ", stderr);
    fput_uint(stderr, hcg_stat_imm_hit_shift);
    fputs("/", stderr);
    fput_uint(stderr, hcg_stat_imm_opp_shift);
    fputs(" miss=", stderr);
    fput_uint(stderr, hcg_stat_imm_miss_shift);
    fputs(" | cmp ", stderr);
    fput_uint(stderr, hcg_stat_imm_hit_cmp);
    fputs("/", stderr);
    fput_uint(stderr, hcg_stat_imm_opp_cmp);
    fputs(" miss=", stderr);
    fput_uint(stderr, hcg_stat_imm_miss_cmp);
    fputs("\n", stderr);
    fputs("hir_codegen_li: total=", stderr);
    fput_uint(stderr, hcg_stat_li_total);
    fputs(" small=", stderr);
    fput_uint(stderr, hcg_stat_li_small);
    fputs(" lui_only=", stderr);
    fput_uint(stderr, hcg_stat_li_lui_only);
    fputs(" lui_addi=", stderr);
    fput_uint(stderr, hcg_stat_li_lui_addi);
    fputs(" copy_mov=", stderr);
    fput_uint(stderr, hcg_stat_copy_emit);
    fputs(" addi0_elide=", stderr);
    fput_uint(stderr, hcg_stat_addi0_elide);
    fputs(" divrem_pow2=", stderr);
    fput_uint(stderr, hcg_stat_divrem_pow2);
    fputs(" tailcall=", stderr);
    fput_uint(stderr, hcg_stat_tailcall);
    fputs("\n", stderr);
    fputs("hir_iconst_use: total=", stderr);
    fput_uint(stderr, bg_stat_iconst_total);
    fputs(" imm_only=", stderr);
    fput_uint(stderr, bg_stat_iconst_imm_only);
    fputs(" nonimm=", stderr);
    fput_uint(stderr, bg_stat_iconst_nonimm);
    fputs(" unused=", stderr);
    fput_uint(stderr, bg_stat_iconst_unused);
    fputs("\n", stderr);
    print_iconst_nonimm_top_ops(8);
    print_burg_top_ops(8);
    print_burg_top_patterns(8);

    return 0;
}
