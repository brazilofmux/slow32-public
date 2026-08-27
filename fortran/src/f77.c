/* f77.c -- Fortran 77 compiler for SLOW-32.
 *
 * Single translation unit, like the rest of this project's compilers:
 * this file includes everything.  Source in, SLOW-32 assembly out.
 *
 *     f77 prog.f prog.s
 *
 * The PROGRAM unit is emitted as `main`, so the existing crt0 and
 * runtime start it and turn its return value into the process exit
 * status -- which is exactly what `STOP n` needs, with no Fortran I/O
 * runtime in the picture yet.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#include "f77_shim.h"
#include "f77_contract.h"

static void f77_error(char *msg);

#include "f77_card.h"
#include "f77_lexer_gen.c"
#include "hir.h"
#include "hir_ssa.h"
#include "hir_opt.h"
#include "hir_licm.h"
#include "hir_burg.h"
#include "hir_regalloc.h"
#include "f77_parse.h"
#include "hir_codegen.h"

static int f77_nerr;

static void f77_error(char *msg) {
    fdputs("f77: line ", 2);
    fdputuint(2, (unsigned int)lx_stmt_line);
    fdputs(": ", 2);
    fdputs(msg, 2);
    fdputc('\n', 2);
    fdputs("  in: ", 2);
    fdputs(lx_stmt, 2);
    fdputc('\n', 2);
    f77_nerr = f77_nerr + 1;
}

static Node f77_main_unit;
static Node f77_program;

/* The backend asks the frontend to build HIR for one unit. */
static void hl_func(Node *fn) {
    int b_entry;
    (void)fn;

    hir_reset();
    hl_nalloca = 0;
    hl_temp_stack = 0;
    f77_nsym = 0;
    f77_nlabel = 0;
    f77_ctl_reset();
    f77_frame = 0;

    b_entry = hir_new_block();
    f77_begin_blk(b_entry);

    while (f77_next_stmt()) {
        f77_statement();
    }

    /* An implicit RET keeps every path terminated even when the source
     * ends without STOP or END. */
    if (f77_cur_blk_live) hi_emit(HI_RET, TY_INT, f77_iconst(0), -1, 0, NULL);

    fn->locals_size = f77_frame;
}

int main(int argc, char **argv) {
    static char src[1 << 20];
    int fd;
    int n;

    if (argc < 3) {
        fdputs("usage: f77 source.f output.s\n", 2);
        return 1;
    }

    fd = open(argv[1], 0);
    if (fd < 0) {
        fdputs("f77: cannot open ", 2);
        fdputs(argv[1], 2);
        fdputc('\n', 2);
        return 1;
    }
    n = (int)read(fd, src, sizeof(src) - 1);
    if (n < 0) n = 0;
    src[n] = 0;
    close(fd);

    lx_src = src;
    lx_len = n;
    lx_pos = 0;
    lx_line = 1;

    f77_main_unit.name = "main";
    f77_main_unit.nparams = 0;
    f77_main_unit.is_varargs = 0;
    f77_main_unit.next = NULL;
    f77_main_unit.body = NULL;
    f77_program.body = &f77_main_unit;

    gen_program(&f77_program);

    if (f77_nerr > 0) {
        fdputs("f77: compilation failed\n", 2);
        return 1;
    }

    fd = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd < 0) {
        fdputs("f77: cannot open output\n", 2);
        return 1;
    }
    write(fd, cg_out, cg_olen);
    close(fd);
    return 0;
}
