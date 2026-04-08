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

    fdputs("hir_burg_top_ops:\n", 2);
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
        fdputs("  ", 2);
        fdputs(bg_op_name(best), 2);
        fdputs("=", 2);
        fdputuint(2, bestc);
        fdputc(10, 2);
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

    fdputs("hir_burg_top_pats:\n", 2);
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
        fdputs("  #", 2);
        fdputuint(2, best);
        fdputs(" ", 2);
        fdputs(bg_op_name(bg_pop[best]), 2);
        fdputs("(", 2);
        if (lnt < 0) fdputs("_", 2);
        else fdputs(bg_nt_name(lnt), 2);
        fdputs(",", 2);
        if (rnt < 0) fdputs("_", 2);
        else fdputs(bg_nt_name(rnt), 2);
        fdputs(")->", 2);
        fdputs(bg_nt_name(bg_pnt[best]), 2);
        fdputs(" x", 2);
        fdputuint(2, bestc);
        fdputc(10, 2);
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

    fdputs("hir_iconst_nonimm_top_ops:\n", 2);
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
        fdputs("  ", 2);
        fdputs(bg_op_name(best), 2);
        fdputs("=", 2);
        fdputuint(2, bestc);
        fdputc(10, 2);
        r = r + 1;
    }
}

int main(int argc, char **argv) {
    int fd;
    int n;
    int i;
    int j;
    int last_slash;
    int argi;
    char *infile;
    char *outfile;
    char *src;
    Node *prog;

    /* Parse arguments: [-I dir] input.c output.s */
    pp_idir[0] = 0;
    infile = 0;
    outfile = 0;
    argi = 1;
    while (argi < argc) {
        if (argv[argi][0] == 45 && argv[argi][1] == 73) {  /* "-I" */
            if (argv[argi][2] != 0) {
                /* -Idir (no space) */
                i = 0;
                j = 2;
                while (argv[argi][j] != 0) {
                    pp_idir[i] = argv[argi][j];
                    i = i + 1;
                    j = j + 1;
                }
                /* Ensure trailing slash */
                if (i > 0 && pp_idir[i - 1] != 47) {
                    pp_idir[i] = 47;
                    i = i + 1;
                }
                pp_idir[i] = 0;
            } else if (argi + 1 < argc) {
                /* -I dir (with space) */
                argi = argi + 1;
                i = 0;
                j = 0;
                while (argv[argi][j] != 0) {
                    pp_idir[i] = argv[argi][j];
                    i = i + 1;
                    j = j + 1;
                }
                if (i > 0 && pp_idir[i - 1] != 47) {
                    pp_idir[i] = 47;
                    i = i + 1;
                }
                pp_idir[i] = 0;
            }
        } else if (infile == 0) {
            infile = argv[argi];
        } else if (outfile == 0) {
            outfile = argv[argi];
        }
        argi = argi + 1;
    }

    if (infile == 0 || outfile == 0) {
        fdputs("Usage: s12cc [-I dir] input.c output.s\n", 2);
        return 1;
    }

    /* Extract source directory for #include resolution */
    last_slash = -1;
    i = 0;
    while (infile[i] != 0) {
        if (infile[i] == 47) last_slash = i;  /* '/' */
        i = i + 1;
    }
    if (last_slash >= 0) {
        i = 0;
        while (i <= last_slash) {
            pp_sdir[i] = infile[i];
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
    fd = open(infile, 0);
    if (fd < 0) {
        fdputs("s12cc: cannot open input: ", 2);
        fdputs(infile, 2);
        fdputc(10, 2);
        return 1;
    }
    src = malloc(LEX_SRC_SZ);
    if (!src) {
        fdputs("s12cc: out of memory\n", 2);
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
    fd = open(outfile, 26);  /* O_WRONLY | O_CREAT | O_TRUNC */
    if (fd < 0) {
        fdputs("s12cc: cannot open output: ", 2);
        fdputs(outfile, 2);
        fdputc(10, 2);
        return 1;
    }
    write(fd, cg_out, cg_olen);
    close(fd);

    /* Print optimization stats to stderr */
    fdputs("hir_opt: ", 2);
    fdputuint(2, ho_stat_elim);
    fdputs(" HIR instructions eliminated, cse=", 2);
    fdputuint(2, ho_stat_cse);
    fdputs(" dse=", 2);
    fdputuint(2, ho_stat_dse);
    fdputs(" slf=", 2);
    fdputuint(2, ho_stat_slf);
    fdputs(" lse=", 2);
    fdputuint(2, ho_stat_lse);
    fdputs("\n", 2);
    fdputs("hir_licm: ", 2);
    fdputuint(2, licm_stat_hoisted);
    fdputs(" instructions hoisted\n", 2);
    fdputs("hir_burg: ", 2);
    fdputuint(2, bg_stat_folded);
    fdputs(" folded, ", 2);
    fdputuint(2, bg_stat_cand);
    fdputs(" cand, rej: uses=", 2);
    fdputuint(2, bg_stat_rej_uses);
    fdputs(" remat=", 2);
    fdputuint(2, bg_stat_rej_remat);
    fdputs(" blk=", 2);
    fdputuint(2, bg_stat_rej_blk);
    fdputs(" cost=", 2);
    fdputuint(2, bg_stat_rej_cost);
    fdputs("\n", 2);
    fdputs("hir_burg_select: total=", 2);
    fdputuint(2, bg_stat_sel_total);
    fdputs(" chain=", 2);
    fdputuint(2, bg_stat_sel_chain);
    fdputs(" imm=", 2);
    fdputuint(2, bg_stat_sel_imm);
    fdputs(" faddr=", 2);
    fdputuint(2, bg_stat_sel_faddr);
    fdputs(" saddr=", 2);
    fdputuint(2, bg_stat_sel_saddr);
    fdputs(" burg_skipped=", 2);
    fdputuint(2, bg_stat_burg_skipped);
    fdputs(" spills=", 2);
    fdputuint(2, ra_stat_spills);
    fdputs("\n", 2);
    fdputs("hir_imm_sel: add ", 2);
    fdputuint(2, hcg_stat_imm_hit_add);
    fdputs("/", 2);
    fdputuint(2, hcg_stat_imm_opp_add);
    fdputs(" miss=", 2);
    fdputuint(2, hcg_stat_imm_miss_add);
    fdputs(" | sub ", 2);
    fdputuint(2, hcg_stat_imm_hit_sub);
    fdputs("/", 2);
    fdputuint(2, hcg_stat_imm_opp_sub);
    fdputs(" miss=", 2);
    fdputuint(2, hcg_stat_imm_miss_sub);
    fdputs(" | logic ", 2);
    fdputuint(2, hcg_stat_imm_hit_logic);
    fdputs("/", 2);
    fdputuint(2, hcg_stat_imm_opp_logic);
    fdputs(" miss=", 2);
    fdputuint(2, hcg_stat_imm_miss_logic);
    fdputs(" | shift ", 2);
    fdputuint(2, hcg_stat_imm_hit_shift);
    fdputs("/", 2);
    fdputuint(2, hcg_stat_imm_opp_shift);
    fdputs(" miss=", 2);
    fdputuint(2, hcg_stat_imm_miss_shift);
    fdputs(" | cmp ", 2);
    fdputuint(2, hcg_stat_imm_hit_cmp);
    fdputs("/", 2);
    fdputuint(2, hcg_stat_imm_opp_cmp);
    fdputs(" miss=", 2);
    fdputuint(2, hcg_stat_imm_miss_cmp);
    fdputs("\n", 2);
    fdputs("hir_codegen_li: total=", 2);
    fdputuint(2, hcg_stat_li_total);
    fdputs(" small=", 2);
    fdputuint(2, hcg_stat_li_small);
    fdputs(" lui_only=", 2);
    fdputuint(2, hcg_stat_li_lui_only);
    fdputs(" lui_addi=", 2);
    fdputuint(2, hcg_stat_li_lui_addi);
    fdputs(" copy_mov=", 2);
    fdputuint(2, hcg_stat_copy_emit);
    fdputs(" addi0_elide=", 2);
    fdputuint(2, hcg_stat_addi0_elide);
    fdputs(" divrem_pow2=", 2);
    fdputuint(2, hcg_stat_divrem_pow2);
    fdputs(" tailcall=", 2);
    fdputuint(2, hcg_stat_tailcall);
    fdputs(" brc_fuse=", 2);
    fdputuint(2, hcg_stat_brc_fuse);
    fdputs(" br_fallthru=", 2);
    fdputuint(2, hcg_stat_br_fallthru);
    fdputs("\n", 2);
    fdputs("hir_iconst_use: total=", 2);
    fdputuint(2, bg_stat_iconst_total);
    fdputs(" imm_only=", 2);
    fdputuint(2, bg_stat_iconst_imm_only);
    fdputs(" nonimm=", 2);
    fdputuint(2, bg_stat_iconst_nonimm);
    fdputs(" unused=", 2);
    fdputuint(2, bg_stat_iconst_unused);
    fdputs("\n", 2);
    print_iconst_nonimm_top_ops(8);
    print_burg_top_ops(8);
    print_burg_top_patterns(8);

    return 0;
}
