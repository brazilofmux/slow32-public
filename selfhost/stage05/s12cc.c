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
    fputs(" HIR instructions eliminated\n", stderr);
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

    return 0;
}
