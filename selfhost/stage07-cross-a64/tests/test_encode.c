/* test_encode.c — verify a64_encode.h against a known-good byte stream.
 *
 * The reference bytes were produced by GNU as on the same instruction set,
 * captured into ref.bin (built by the test runner). We emit each instruction
 * here in the SAME ORDER and diff the byte streams. Any mismatch points to
 * the exact instruction whose encoding is wrong.
 *
 * To regenerate the reference: tests/gen_ref.sh
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../a64_encode.h"

/* Each test entry: a label and a function that emits one instruction. */
typedef struct {
    const char *label;
    void (*emit)(void);
} entry_t;

/* The instructions we test, in lockstep with tests/ref.s */

static void t_mov_w_imm_lo(void)   { a64_mov_w_imm(A64_X0, 0x1234); }
static void t_mov_w_imm_hi(void)   { a64_mov_w_imm(A64_X1, 0xABCD0000); }

static void t_movz_x_lsl0(void)    { a64_movz_x(A64_X3, 0x1234, 0); }
static void t_movz_x_lsl16(void)   { a64_movz_x(A64_X3, 0x5678, 16); }
static void t_movz_x_lsl32(void)   { a64_movz_x(A64_X3, 0x9ABC, 32); }
static void t_movz_x_lsl48(void)   { a64_movz_x(A64_X3, 0xDEF0, 48); }
static void t_movk_x_lsl16(void)   { a64_movk_x(A64_X4, 0xCAFE, 16); }

static void t_mov_w(void)          { a64_mov_w(A64_X5, A64_X6); }
static void t_mov_x(void)          { a64_mov_x(A64_X7, A64_X8); }

static void t_add_w(void)          { a64_add_w(A64_X9, A64_X10, A64_X11); }
static void t_sub_w(void)          { a64_sub_w(A64_X9, A64_X10, A64_X11); }
static void t_add_w_imm(void)      { a64_add_w_imm(A64_X12, A64_X13, 0x123); }
static void t_sub_w_imm(void)      { a64_sub_w_imm(A64_X12, A64_X13, 0x456); }
static void t_neg_w(void)          { a64_neg_w(A64_X14, A64_X15); }

static void t_add_x(void)          { a64_add_x(A64_X9, A64_X10, A64_X11); }
static void t_sub_x(void)          { a64_sub_x(A64_X9, A64_X10, A64_X11); }
static void t_add_x_imm(void)      { a64_add_x_imm(A64_X16, A64_X17, 0x100); }
static void t_sub_x_imm(void)      { a64_sub_x_imm(A64_X16, A64_X17, 0x200); }

static void t_mul_w(void)          { a64_mul_w(A64_X0, A64_X1, A64_X2); }
static void t_sdiv_w(void)         { a64_sdiv_w(A64_X3, A64_X4, A64_X5); }
static void t_udiv_w(void)         { a64_udiv_w(A64_X6, A64_X7, A64_X8); }
static void t_msub_w(void)         { a64_msub_w(A64_X9, A64_X10, A64_X11, A64_X12); }
static void t_madd_w(void)         { a64_madd_w(A64_X9, A64_X10, A64_X11, A64_X12); }
static void t_smull(void)          { a64_smull(A64_X13, A64_X14, A64_X15); }
static void t_umull(void)          { a64_umull(A64_X16, A64_X17, A64_X18); }
static void t_mul_x(void)          { a64_mul_x(A64_X0, A64_X1, A64_X2); }
static void t_sdiv_x(void)         { a64_sdiv_x(A64_X3, A64_X4, A64_X5); }
static void t_udiv_x(void)         { a64_udiv_x(A64_X6, A64_X7, A64_X8); }

static void t_and_w(void)          { a64_and_w(A64_X0, A64_X1, A64_X2); }
static void t_orr_w(void)          { a64_orr_w(A64_X0, A64_X1, A64_X2); }
static void t_eor_w(void)          { a64_eor_w(A64_X0, A64_X1, A64_X2); }
static void t_ands_w(void)         { a64_ands_w(A64_X3, A64_X4, A64_X5); }
static void t_mvn_w(void)          { a64_mvn_w(A64_X6, A64_X7); }
static void t_tst_w(void)          { a64_tst_w(A64_X8, A64_X9); }
static void t_and_x(void)          { a64_and_x(A64_X10, A64_X11, A64_X12); }
static void t_orr_x(void)          { a64_orr_x(A64_X10, A64_X11, A64_X12); }
static void t_eor_x(void)          { a64_eor_x(A64_X10, A64_X11, A64_X12); }
static void t_orr_x_lsl(void)      { a64_orr_x_lsl(A64_X13, A64_X14, A64_X15, 8); }

static void t_and_w_imm(void) {
    int ok;
    ok = a64_and_w_imm(A64_X0, A64_X1, 0xFF);
    if (!ok) { fprintf(stderr, "BUG: and_w_imm 0xFF should encode\n"); exit(1); }
}
static void t_orr_w_imm(void) {
    int ok;
    ok = a64_orr_w_imm(A64_X0, A64_X1, 0xF0F0F0F0);
    if (!ok) { fprintf(stderr, "BUG: orr_w_imm 0xF0F0F0F0 should encode\n"); exit(1); }
}
static void t_eor_w_imm(void) {
    int ok;
    ok = a64_eor_w_imm(A64_X0, A64_X1, 0x33333333);
    if (!ok) { fprintf(stderr, "BUG: eor_w_imm 0x33333333 should encode\n"); exit(1); }
}

static void t_lslv_w(void)         { a64_lslv_w(A64_X0, A64_X1, A64_X2); }
static void t_lsrv_w(void)         { a64_lsrv_w(A64_X0, A64_X1, A64_X2); }
static void t_asrv_w(void)         { a64_asrv_w(A64_X0, A64_X1, A64_X2); }
static void t_lsl_w_imm(void)      { a64_lsl_w_imm(A64_X3, A64_X4, 5); }
static void t_lsr_w_imm(void)      { a64_lsr_w_imm(A64_X3, A64_X4, 7); }
static void t_asr_w_imm(void)      { a64_asr_w_imm(A64_X3, A64_X4, 11); }
static void t_lslv_x(void)         { a64_lslv_x(A64_X0, A64_X1, A64_X2); }
static void t_lsrv_x(void)         { a64_lsrv_x(A64_X0, A64_X1, A64_X2); }
static void t_asrv_x(void)         { a64_asrv_x(A64_X0, A64_X1, A64_X2); }
static void t_lsl_x_imm(void)      { a64_lsl_x_imm(A64_X3, A64_X4, 5); }
static void t_lsr_x_imm(void)      { a64_lsr_x_imm(A64_X3, A64_X4, 7); }
static void t_asr_x_imm(void)      { a64_asr_x_imm(A64_X3, A64_X4, 11); }

static void t_sxtb_w(void)         { a64_sxtb_w(A64_X0, A64_X1); }
static void t_sxth_w(void)         { a64_sxth_w(A64_X0, A64_X1); }
static void t_sxtw(void)           { a64_sxtw(A64_X0, A64_X1); }
static void t_uxtb_w(void)         { a64_uxtb_w(A64_X0, A64_X1); }
static void t_uxth_w(void)         { a64_uxth_w(A64_X0, A64_X1); }

static void t_cmp_w(void)          { a64_cmp_w(A64_X0, A64_X1); }
static void t_cmp_w_imm(void)      { a64_cmp_w_imm(A64_X2, 0x123); }
static void t_cmn_w_imm(void)      { a64_cmn_w_imm(A64_X2, 0x456); }
static void t_cmp_x(void)          { a64_cmp_x(A64_X3, A64_X4); }
static void t_cmp_x_imm(void)      { a64_cmp_x_imm(A64_X5, 0x789); }

static void t_cset_eq(void)        { a64_cset_w(A64_X0, A64_COND_EQ); }
static void t_cset_lt(void)        { a64_cset_w(A64_X1, A64_COND_LT); }
static void t_csel_w(void)         { a64_csel_w(A64_X2, A64_X3, A64_X4, A64_COND_GE); }

static void t_b_fwd(void)          { a64_b(0x1000); }       /* +1024 instructions */
static void t_b_back(void)         { a64_b(-0x40); }        /* -16 instructions */
static void t_bl_fwd(void)         { a64_bl(0x800); }
static void t_b_eq(void)           { a64_b_cond(A64_COND_EQ, 0x40); }
static void t_b_lt(void)           { a64_b_cond(A64_COND_LT, -0x80); }
static void t_cbz_w(void)          { a64_cbz_w(A64_X3, 0x20); }
static void t_cbnz_w(void)         { a64_cbnz_w(A64_X3, 0x20); }
static void t_cbz_x(void)          { a64_cbz_x(A64_X4, 0x40); }
static void t_cbnz_x(void)         { a64_cbnz_x(A64_X4, 0x40); }
static void t_br(void)             { a64_br(A64_X16); }
static void t_blr(void)            { a64_blr(A64_X17); }
static void t_ret(void)            { a64_ret(); }

static void t_ldr_w_imm(void)      { a64_ldr_w_imm(A64_X0, A64_X1, 16); }
static void t_str_w_imm(void)      { a64_str_w_imm(A64_X0, A64_X1, 16); }
static void t_ldr_x_imm(void)      { a64_ldr_x_imm(A64_X2, A64_X3, 32); }
static void t_str_x_imm(void)      { a64_str_x_imm(A64_X2, A64_X3, 32); }
static void t_ldrb(void)           { a64_ldrb_imm(A64_X4, A64_X5, 7); }
static void t_strb(void)           { a64_strb_imm(A64_X4, A64_X5, 7); }
static void t_ldrh(void)           { a64_ldrh_imm(A64_X6, A64_X7, 14); }
static void t_strh(void)           { a64_strh_imm(A64_X6, A64_X7, 14); }
static void t_ldrsb_w(void)        { a64_ldrsb_w_imm(A64_X8, A64_X9, 3); }
static void t_ldrsh_w(void)        { a64_ldrsh_w_imm(A64_X8, A64_X9, 8); }

static void t_str_x_pre(void)      { a64_str_x_pre(A64_X19, A64_SP, -16); }
static void t_ldr_x_post(void)     { a64_ldr_x_post(A64_X19, A64_SP, 16); }

static void t_stp_pre(void)        { a64_stp_x_pre(A64_X29, A64_X30, A64_SP, -16); }
static void t_ldp_post(void)       { a64_ldp_x_post(A64_X29, A64_X30, A64_SP, 16); }
static void t_stp_off(void)        { a64_stp_x_off(A64_X19, A64_X20, A64_SP, 16); }
static void t_ldp_off(void)        { a64_ldp_x_off(A64_X19, A64_X20, A64_SP, 16); }

static void t_adr(void)            { a64_adr(A64_X0, 0x123); }
/* adrp is verified in a separate test (gas emits a reloc, leaving the imm21 zero,
 * so a byte-stream diff against gas output is not meaningful). See test_adrp_unit. */

static void t_fmov_s_w(void)       { a64_fmov_s_w(0, A64_X0); }
static void t_fmov_w_s(void)       { a64_fmov_w_s(A64_X0, 1); }
static void t_fmov_d_x(void)       { a64_fmov_d_x(2, A64_X3); }
static void t_fmov_x_d(void)       { a64_fmov_x_d(A64_X4, 5); }
static void t_fmov_s_s(void)       { a64_fmov_s_s(6, 7); }
static void t_fmov_d_d(void)       { a64_fmov_d_d(8, 9); }

static void t_fadd_s(void)         { a64_fadd_s(0, 1, 2); }
static void t_fadd_d(void)         { a64_fadd_d(0, 1, 2); }
static void t_fsub_s(void)         { a64_fsub_s(0, 1, 2); }
static void t_fmul_s(void)         { a64_fmul_s(0, 1, 2); }
static void t_fdiv_s(void)         { a64_fdiv_s(0, 1, 2); }
static void t_fneg_s(void)         { a64_fneg_s(3, 4); }
static void t_fneg_d(void)         { a64_fneg_d(3, 4); }
static void t_fabs_s(void)         { a64_fabs_s(5, 6); }
static void t_fsqrt_d(void)        { a64_fsqrt_d(7, 8); }

static void t_fcmp_s(void)         { a64_fcmp_s(0, 1); }
static void t_fcmp_d(void)         { a64_fcmp_d(0, 1); }
static void t_fcvt_s_d(void)       { a64_fcvt_s_d(0, 1); }
static void t_fcvt_d_s(void)       { a64_fcvt_d_s(0, 1); }

static void t_fcvtzs_w_s(void)     { a64_fcvtzs(0, 0, A64_X0, 1); }
static void t_fcvtzu_x_d(void)     { a64_fcvtzu(1, 1, A64_X2, 3); }
static void t_scvtf_s_w(void)      { a64_scvtf(0, 0, 4, A64_X5); }
static void t_ucvtf_d_x(void)      { a64_ucvtf(1, 1, 6, A64_X7); }

static void t_ldr_s(void)          { a64_ldr_s_imm(0, A64_X1, 16); }
static void t_str_s(void)          { a64_str_s_imm(0, A64_X1, 16); }
static void t_ldr_d(void)          { a64_ldr_d_imm(2, A64_X3, 32); }
static void t_str_d(void)          { a64_str_d_imm(2, A64_X3, 32); }

static void t_nop(void)            { a64_nop(); }
static void t_brk(void)            { a64_brk(0xCAFE); }
static void t_svc(void)            { a64_svc(0); }
static void t_dmb(void)            { a64_dmb_ishst(); }

static void t_mov_sp_x(void)       { a64_mov_sp_x(A64_X29, A64_SP); }   /* mov x29, sp */

static const entry_t TESTS[] = {
    {"mov_w_imm_lo",   t_mov_w_imm_lo},
    {"mov_w_imm_hi",   t_mov_w_imm_hi},
    {"movz_x_lsl0",    t_movz_x_lsl0},
    {"movz_x_lsl16",   t_movz_x_lsl16},
    {"movz_x_lsl32",   t_movz_x_lsl32},
    {"movz_x_lsl48",   t_movz_x_lsl48},
    {"movk_x_lsl16",   t_movk_x_lsl16},
    {"mov_w",          t_mov_w},
    {"mov_x",          t_mov_x},
    {"add_w",          t_add_w},
    {"sub_w",          t_sub_w},
    {"add_w_imm",      t_add_w_imm},
    {"sub_w_imm",      t_sub_w_imm},
    {"neg_w",          t_neg_w},
    {"add_x",          t_add_x},
    {"sub_x",          t_sub_x},
    {"add_x_imm",      t_add_x_imm},
    {"sub_x_imm",      t_sub_x_imm},
    {"mul_w",          t_mul_w},
    {"sdiv_w",         t_sdiv_w},
    {"udiv_w",         t_udiv_w},
    {"msub_w",         t_msub_w},
    {"madd_w",         t_madd_w},
    {"smull",          t_smull},
    {"umull",          t_umull},
    {"mul_x",          t_mul_x},
    {"sdiv_x",         t_sdiv_x},
    {"udiv_x",         t_udiv_x},
    {"and_w",          t_and_w},
    {"orr_w",          t_orr_w},
    {"eor_w",          t_eor_w},
    {"ands_w",         t_ands_w},
    {"mvn_w",          t_mvn_w},
    {"tst_w",          t_tst_w},
    {"and_x",          t_and_x},
    {"orr_x",          t_orr_x},
    {"eor_x",          t_eor_x},
    {"orr_x_lsl",      t_orr_x_lsl},
    {"and_w_imm",      t_and_w_imm},
    {"orr_w_imm",      t_orr_w_imm},
    {"eor_w_imm",      t_eor_w_imm},
    {"lslv_w",         t_lslv_w},
    {"lsrv_w",         t_lsrv_w},
    {"asrv_w",         t_asrv_w},
    {"lsl_w_imm",      t_lsl_w_imm},
    {"lsr_w_imm",      t_lsr_w_imm},
    {"asr_w_imm",      t_asr_w_imm},
    {"lslv_x",         t_lslv_x},
    {"lsrv_x",         t_lsrv_x},
    {"asrv_x",         t_asrv_x},
    {"lsl_x_imm",      t_lsl_x_imm},
    {"lsr_x_imm",      t_lsr_x_imm},
    {"asr_x_imm",      t_asr_x_imm},
    {"sxtb_w",         t_sxtb_w},
    {"sxth_w",         t_sxth_w},
    {"sxtw",           t_sxtw},
    {"uxtb_w",         t_uxtb_w},
    {"uxth_w",         t_uxth_w},
    {"cmp_w",          t_cmp_w},
    {"cmp_w_imm",      t_cmp_w_imm},
    {"cmn_w_imm",      t_cmn_w_imm},
    {"cmp_x",          t_cmp_x},
    {"cmp_x_imm",      t_cmp_x_imm},
    {"cset_eq",        t_cset_eq},
    {"cset_lt",        t_cset_lt},
    {"csel_w",         t_csel_w},
    {"b_fwd",          t_b_fwd},
    {"b_back",         t_b_back},
    {"bl_fwd",         t_bl_fwd},
    {"b_eq",           t_b_eq},
    {"b_lt",           t_b_lt},
    {"cbz_w",          t_cbz_w},
    {"cbnz_w",         t_cbnz_w},
    {"cbz_x",          t_cbz_x},
    {"cbnz_x",         t_cbnz_x},
    {"br",             t_br},
    {"blr",            t_blr},
    {"ret",            t_ret},
    {"ldr_w_imm",      t_ldr_w_imm},
    {"str_w_imm",      t_str_w_imm},
    {"ldr_x_imm",      t_ldr_x_imm},
    {"str_x_imm",      t_str_x_imm},
    {"ldrb",           t_ldrb},
    {"strb",           t_strb},
    {"ldrh",           t_ldrh},
    {"strh",           t_strh},
    {"ldrsb_w",        t_ldrsb_w},
    {"ldrsh_w",        t_ldrsh_w},
    {"str_x_pre",      t_str_x_pre},
    {"ldr_x_post",     t_ldr_x_post},
    {"stp_pre",        t_stp_pre},
    {"ldp_post",       t_ldp_post},
    {"stp_off",        t_stp_off},
    {"ldp_off",        t_ldp_off},
    {"adr",            t_adr},
    {"fmov_s_w",       t_fmov_s_w},
    {"fmov_w_s",       t_fmov_w_s},
    {"fmov_d_x",       t_fmov_d_x},
    {"fmov_x_d",       t_fmov_x_d},
    {"fmov_s_s",       t_fmov_s_s},
    {"fmov_d_d",       t_fmov_d_d},
    {"fadd_s",         t_fadd_s},
    {"fadd_d",         t_fadd_d},
    {"fsub_s",         t_fsub_s},
    {"fmul_s",         t_fmul_s},
    {"fdiv_s",         t_fdiv_s},
    {"fneg_s",         t_fneg_s},
    {"fneg_d",         t_fneg_d},
    {"fabs_s",         t_fabs_s},
    {"fsqrt_d",        t_fsqrt_d},
    {"fcmp_s",         t_fcmp_s},
    {"fcmp_d",         t_fcmp_d},
    {"fcvt_s_d",       t_fcvt_s_d},
    {"fcvt_d_s",       t_fcvt_d_s},
    {"fcvtzs_w_s",     t_fcvtzs_w_s},
    {"fcvtzu_x_d",     t_fcvtzu_x_d},
    {"scvtf_s_w",      t_scvtf_s_w},
    {"ucvtf_d_x",      t_ucvtf_d_x},
    {"ldr_s",          t_ldr_s},
    {"str_s",          t_str_s},
    {"ldr_d",          t_ldr_d},
    {"str_d",          t_str_d},
    {"nop",            t_nop},
    {"brk",            t_brk},
    {"svc",            t_svc},
    {"dmb",            t_dmb},
    {"mov_sp_x",       t_mov_sp_x},
};

#define N_TESTS (int)(sizeof(TESTS) / sizeof(TESTS[0]))

/* Direct bit-pattern checks for instructions that gas would emit with an
 * unresolved relocation (we can't byte-diff those). */
static int unit_checks(void) {
    int fails;
    int word;
    int saved_off;

    fails = 0;
    saved_off = a64_off;

    /* adrp x0, . + 0x12000 should encode as 0x10000000 | (sf=1<<31) | imm21=0x12.
     *   imm21 = 0x12, immlo = bits 0..1 = 2, immhi = bits 2..20 = 4
     *   word = 0x90000000 | (2 << 29) | (4 << 5) | 0 = 0xD0000080
     */
    a64_off = 0;
    a64_adrp(A64_X0, 0x12000);
    word = (a64_buf[0]) | (a64_buf[1] << 8) | (a64_buf[2] << 16) | (a64_buf[3] << 24);
    if (word != (int)0xD0000080) {
        fprintf(stderr, "FAIL adrp_unit  ours=0x%08x  expected=0xD0000080\n", word);
        fails++;
    }

    /* adrp x5, +0x1000 -> imm21=1, immlo=1, immhi=0, rd=5  -> 0xB0000005 */
    a64_off = 0;
    a64_adrp(A64_X5, 0x1000);
    word = (a64_buf[0]) | (a64_buf[1] << 8) | (a64_buf[2] << 16) | (a64_buf[3] << 24);
    if (word != (int)0xB0000005) {
        fprintf(stderr, "FAIL adrp_unit2 ours=0x%08x  expected=0xB0000005\n", word);
        fails++;
    }

    a64_off = saved_off;
    return fails;
}

int main(int argc, char **argv) {
    int i;
    int dump_only;
    FILE *fp;

    dump_only = 0;
    if (argc > 1 && strcmp(argv[1], "--dump") == 0) dump_only = 1;
    if (argc > 1 && strcmp(argv[1], "--list") == 0) {
        for (i = 0; i < N_TESTS; i++) printf("%s\n", TESTS[i].label);
        return 0;
    }

    /* Reset buffer and emit each test in order. */
    a64_off = 0;
    for (i = 0; i < N_TESTS; i++) {
        int before;
        before = a64_off;
        TESTS[i].emit();
        if (a64_off - before != 4) {
            fprintf(stderr, "BUG: %s emitted %d bytes (expected 4)\n",
                    TESTS[i].label, a64_off - before);
            return 1;
        }
    }

    if (dump_only) {
        /* Write our buffer as bytes to stdout */
        fwrite(a64_buf, 1, a64_off, stdout);
        return 0;
    }

    /* Diff against ref.bin (built by gen_ref.sh) */
    fp = fopen("tests/ref.bin", "rb");
    if (!fp) { perror("ref.bin"); return 1; }
    {
        unsigned char ref[N_TESTS * 4];
        int n;
        n = (int)fread(ref, 1, sizeof ref, fp);
        fclose(fp);
        if (n != a64_off) {
            fprintf(stderr, "size mismatch: ours=%d ref=%d\n", a64_off, n);
            return 1;
        }
        {
            int fails;
            fails = 0;
            for (i = 0; i < N_TESTS; i++) {
                int off; int j; int our_w; int ref_w;
                off = i * 4;
                our_w = (a64_buf[off]) | (a64_buf[off+1] << 8)
                      | (a64_buf[off+2] << 16) | (a64_buf[off+3] << 24);
                ref_w = (ref[off]) | (ref[off+1] << 8)
                      | (ref[off+2] << 16) | (ref[off+3] << 24);
                if (our_w != ref_w) {
                    fprintf(stderr, "FAIL %-18s  ours=0x%08x  ref=0x%08x\n",
                            TESTS[i].label, our_w, ref_w);
                    fails++;
                } else if (argc > 1 && strcmp(argv[1], "-v") == 0) {
                    printf("ok   %-18s  0x%08x\n", TESTS[i].label, our_w);
                }
                (void)j;
            }
            if (fails) {
                fprintf(stderr, "\n%d/%d encoder mismatches\n", fails, N_TESTS);
                return 1;
            }
            printf("encoder: %d/%d ok\n", N_TESTS, N_TESTS);
        }
    }

    /* Direct bit-pattern checks (for instructions gas can't resolve at assembly time) */
    {
        int unit_fails;
        unit_fails = unit_checks();
        if (unit_fails) {
            fprintf(stderr, "%d unit-check failures\n", unit_fails);
            return 1;
        }
        printf("unit checks: ok\n");
    }
    return 0;
}
