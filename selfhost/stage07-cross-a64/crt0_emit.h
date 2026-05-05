/* crt0_emit.h — emit AArch64 _start stub as a relocatable object.
 *
 * The kernel hands control to _start with the stack shaped as:
 *
 *      [sp + 0]                argc          (8 bytes; only low 32 bits used)
 *      [sp + 8]                argv[0]
 *      [sp + 8 + 8*argc]       NULL          (argv terminator)
 *      [sp + 8 + 8*(argc+1)]   envp[0]
 *      ...                      NULL
 *      ...                      auxv...
 *
 * Our _start:
 *   - clears FP/LR (defensive — backtrace tools stop here)
 *   - reads argc/argv/envp off the stack
 *   - calls __save_envp(envp)              (libc records envp for getenv)
 *   - calls main(argc, argv)
 *   - syscalls SYS_exit (93) with main's return value in w0.
 *
 * Caller must have included a64_encode.h and obj_writer.h; the writer's
 * cg_* arrays must be defined (the cc-a64 driver provides them via the
 * codegen header — for tests we provide stubs).
 *
 * Usage:
 *   reset cg state, call emit_crt0_to_buf(), then obj_write_file(...).
 */

#ifndef CRT0_EMIT_H
#define CRT0_EMIT_H

/* AArch64 Linux syscall number for exit. */
#define A64_SYS_EXIT 93

static void emit_crt0_to_buf(void) {
    int call_envp_off;
    int call_main_off;

    /* mov x29, xzr        ; clear frame pointer */
    a64_mov_x(A64_X29, A64_XZR);
    /* mov x30, xzr        ; clear link register (defensive) */
    a64_mov_x(A64_X30, A64_XZR);

    /* ldr x0, [sp]        ; argc (kernel stores it as a 64-bit value) */
    a64_ldr_x_imm(A64_X0, A64_SP, 0);

    /* add x1, sp, #8      ; argv = sp + 8 */
    a64_add_x_imm(A64_X1, A64_SP, 8);

    /* add x2, x0, #1      ; argc+1 */
    a64_add_x_imm(A64_X2, A64_X0, 1);
    /* lsl x2, x2, #3      ; (argc+1)*8 */
    a64_lsl_x_imm(A64_X2, A64_X2, 3);
    /* add x2, x1, x2      ; envp = argv + (argc+1)*8 */
    a64_add_x(A64_X2, A64_X1, A64_X2);

    /* Allocate a 32-byte frame: save fp/lr at [sp,#0..15], argc/argv at [sp,#16..31].
     *   stp x29, x30, [sp, #-32]!
     */
    a64_stp_x_pre(A64_X29, A64_X30, A64_SP, -32);
    /*   stp x0, x1, [sp, #16]   ; save argc, argv */
    a64_stp_x_off(A64_X0, A64_X1, A64_SP, 16);
    /*   mov x29, sp             ; set new frame pointer */
    a64_mov_sp_x(A64_X29, A64_SP);

    /* x0 = envp, then BL __save_envp */
    a64_mov_x(A64_X0, A64_X2);
    call_envp_off = a64_off;
    a64_bl(0);
    cg_cpatch_name[cg_ncpatches] = "__save_envp";
    cg_cpatch_off[cg_ncpatches]  = call_envp_off;
    cg_cpatch_kind[cg_ncpatches] = A64K_CALL26;
    cg_ncpatches = cg_ncpatches + 1;

    /* Restore argc/argv into x0/x1 and BL main */
    a64_ldp_x_off(A64_X0, A64_X1, A64_SP, 16);
    call_main_off = a64_off;
    a64_bl(0);
    cg_cpatch_name[cg_ncpatches] = "main";
    cg_cpatch_off[cg_ncpatches]  = call_main_off;
    cg_cpatch_kind[cg_ncpatches] = A64K_CALL26;
    cg_ncpatches = cg_ncpatches + 1;

    /* sys_exit(w0): mov w8, #93; svc #0 */
    a64_mov_w_imm(A64_X8, A64_SYS_EXIT);
    a64_svc(0);
}

/* Convenience wrapper: reset cg state, emit crt0 with _start at offset 0,
 * write it to outfile. */
static int emit_crt0_object(char *outfile) {
    a64_off = 0;
    cg_nfuncs    = 0;
    cg_nglobals  = 0;
    cg_ncpatches = 0;
    cg_ndrelocs  = 0;
    cg_rodata_len = 0;
    cg_data_len   = 0;
    cg_bss_size   = 0;

    /* Define _start as a global function symbol at .text offset 0. */
    cg_func_name[cg_nfuncs] = "_start";
    cg_func_off[cg_nfuncs]  = 0;
    cg_nfuncs = cg_nfuncs + 1;

    emit_crt0_to_buf();

    return obj_write_file(outfile);
}

#endif /* CRT0_EMIT_H */
