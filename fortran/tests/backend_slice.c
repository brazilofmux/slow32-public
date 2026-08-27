/* Architecture proof: drive the copied backend with hand-built HIR,
 * no Fortran frontend involved.  Emits a function equivalent to
 *     int f77_slice(int n) { int s=0,i; for(i=1;i<=n;i++) s+=i*i; return s; }
 * which exercises phis, a loop, LICM, BURG selection and regalloc. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include "f77_shim.h"
#include "f77_contract.h"
#include "hir.h"
#include "hir_ssa.h"
#include "hir_opt.h"
#include "hir_licm.h"
#include "hir_burg.h"
#include "hir_regalloc.h"
#include "hir_codegen.h"

static Node g_fn, g_prog;

/* The one function the backend needs from a frontend: build HIR for fn. */
static void hl_func(Node *fn) {
    int a_s, a_i, p_n, c0, c1, ld_i, ld_s, mul, add, inc, ld_i2, cmp, ld_ret;
    int b_entry, b_head, b_body, b_exit;
    (void)fn;

    hir_reset();
    hl_nalloca = 0; hl_temp_stack = 0;

    b_entry = hir_new_block(); b_head = hir_new_block();
    b_body  = hir_new_block(); b_exit = hir_new_block();

    hl_switch_block(b_entry);
    p_n  = hi_emit(HI_PARAM, TY_INT, 0, -1, 0, NULL);
    a_s  = hi_emit(HI_ALLOCA, TY_INT, -1, -1, -4, NULL);
    hl_ainst[hl_nalloca] = a_s; hl_aoff[hl_nalloca] = -4; hl_nalloca++;
    a_i  = hi_emit(HI_ALLOCA, TY_INT, -1, -1, -8, NULL);
    hl_ainst[hl_nalloca] = a_i; hl_aoff[hl_nalloca] = -8; hl_nalloca++;
    c0   = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
    c1   = hi_emit(HI_ICONST, TY_INT, -1, -1, 1, NULL);
    hi_emit(HI_STORE, TY_INT, a_s, c0, 0, NULL);
    hi_emit(HI_STORE, TY_INT, a_i, c1, 0, NULL);
    hi_emit(HI_BR, TY_VOID, -1, -1, b_head, NULL);

    hl_switch_block(b_head);
    ld_i = hi_emit(HI_LOAD, TY_INT, a_i, -1, 0, NULL);
    cmp  = hi_emit(HI_SLE, TY_INT, ld_i, p_n, 0, NULL);
    hi_emit(HI_BRC, TY_VOID, cmp, b_body, b_exit, NULL);  /* src2=true, val=false */

    hl_switch_block(b_body);
    ld_i2 = hi_emit(HI_LOAD, TY_INT, a_i, -1, 0, NULL);
    mul   = hi_emit(HI_MUL, TY_INT, ld_i2, ld_i2, 0, NULL);
    ld_s  = hi_emit(HI_LOAD, TY_INT, a_s, -1, 0, NULL);
    add   = hi_emit(HI_ADD, TY_INT, ld_s, mul, 0, NULL);
    hi_emit(HI_STORE, TY_INT, a_s, add, 0, NULL);
    inc   = hi_emit(HI_ADDI, TY_INT, ld_i2, -1, 1, NULL);
    hi_emit(HI_STORE, TY_INT, a_i, inc, 0, NULL);
    hi_emit(HI_BR, TY_VOID, -1, -1, b_head, NULL);

    hl_switch_block(b_exit);
    ld_ret = hi_emit(HI_LOAD, TY_INT, a_s, -1, 0, NULL);
    hi_emit(HI_RET, TY_INT, ld_ret, -1, 0, NULL);
}

int main(int argc, char **argv) {
    int fd;
    g_fn.name = "f77_slice"; g_fn.locals_size = 8; g_fn.nparams = 1;
    g_fn.is_varargs = 0; g_fn.next = NULL; g_fn.body = NULL;
    g_prog.body = &g_fn;
    gen_program(&g_prog);
    fd = open(argv[1], O_WRONLY | O_CREAT | O_TRUNC, 0644);
    write(fd, cg_out, cg_olen);
    close(fd);
    return 0;
}
