// ref.s — reference instruction stream for test_encode.c
//
// Each .word is the encoding GNU as produces for the matching test case.
// We use raw asm directives (in lockstep) so the test diff is straightforward.
//
// The instructions here MUST match the order in TESTS[] in test_encode.c.

    .arch armv8-a
    .text

    // mov_w_imm_lo, mov_w_imm_hi
    movz w0, #0x1234
    movz w1, #0xABCD, lsl #16

    // movz_x_lsl0..lsl48, movk_x_lsl16
    movz x3, #0x1234
    movz x3, #0x5678, lsl #16
    movz x3, #0x9ABC, lsl #32
    movz x3, #0xDEF0, lsl #48
    movk x4, #0xCAFE, lsl #16

    // mov_w, mov_x  (use canonical mov)
    mov w5, w6
    mov x7, x8

    // add_w, sub_w, add_w_imm, sub_w_imm, neg_w
    add w9, w10, w11
    sub w9, w10, w11
    add w12, w13, #0x123
    sub w12, w13, #0x456
    neg w14, w15

    // add_x, sub_x, add_x_imm, sub_x_imm
    add x9, x10, x11
    sub x9, x10, x11
    add x16, x17, #0x100
    sub x16, x17, #0x200

    // mul_w, sdiv_w, udiv_w, msub_w, madd_w, smull, umull, mul_x, sdiv_x, udiv_x
    mul w0, w1, w2
    sdiv w3, w4, w5
    udiv w6, w7, w8
    msub w9, w10, w11, w12
    madd w9, w10, w11, w12
    smull x13, w14, w15
    umull x16, w17, w18
    mul x0, x1, x2
    sdiv x3, x4, x5
    udiv x6, x7, x8

    // and_w, orr_w, eor_w, ands_w, mvn_w, tst_w, and_x, orr_x, eor_x, orr_x_lsl
    and w0, w1, w2
    orr w0, w1, w2
    eor w0, w1, w2
    ands w3, w4, w5
    mvn w6, w7
    tst w8, w9
    and x10, x11, x12
    orr x10, x11, x12
    eor x10, x11, x12
    orr x13, x14, x15, lsl #8

    // and_w_imm 0xFF, orr_w_imm 0xF0F0F0F0, eor_w_imm 0x33333333
    and w0, w1, #0xFF
    orr w0, w1, #0xF0F0F0F0
    eor w0, w1, #0x33333333

    // lslv_w, lsrv_w, asrv_w, lsl/lsr/asr w_imm
    lsl w0, w1, w2
    lsr w0, w1, w2
    asr w0, w1, w2
    lsl w3, w4, #5
    lsr w3, w4, #7
    asr w3, w4, #11

    // lslv_x, lsrv_x, asrv_x, lsl/lsr/asr x_imm
    lsl x0, x1, x2
    lsr x0, x1, x2
    asr x0, x1, x2
    lsl x3, x4, #5
    lsr x3, x4, #7
    asr x3, x4, #11

    // sxtb_w, sxth_w, sxtw, uxtb_w, uxth_w
    sxtb w0, w1
    sxth w0, w1
    sxtw x0, w1
    uxtb w0, w1
    uxth w0, w1

    // cmp_w, cmp_w_imm, cmn_w_imm, cmp_x, cmp_x_imm
    cmp w0, w1
    cmp w2, #0x123
    cmn w2, #0x456
    cmp x3, x4
    cmp x5, #0x789

    // cset_eq, cset_lt, csel_w
    cset w0, eq
    cset w1, lt
    csel w2, w3, w4, ge

    // b_fwd (+0x1000), b_back (-0x40), bl_fwd (+0x800),
    // b_eq (+0x40), b_lt (-0x80),
    // cbz_w +0x20, cbnz_w +0x20, cbz_x +0x40, cbnz_x +0x40,
    // br x16, blr x17, ret
    b   . + 0x1000
    b   . - 0x40
    bl  . + 0x800
    b.eq . + 0x40
    b.lt . - 0x80
    cbz  w3, . + 0x20
    cbnz w3, . + 0x20
    cbz  x4, . + 0x40
    cbnz x4, . + 0x40
    br x16
    blr x17
    ret

    // ldr/str_w_imm 16, ldr/str_x_imm 32, ldrb/strb 7, ldrh/strh 14,
    // ldrsb_w 3, ldrsh_w 8
    ldr w0, [x1, #16]
    str w0, [x1, #16]
    ldr x2, [x3, #32]
    str x2, [x3, #32]
    ldrb w4, [x5, #7]
    strb w4, [x5, #7]
    ldrh w6, [x7, #14]
    strh w6, [x7, #14]
    ldrsb w8, [x9, #3]
    ldrsh w8, [x9, #8]

    // str_x_pre [-16], ldr_x_post [16]
    str x19, [sp, #-16]!
    ldr x19, [sp], #16

    // stp pre/post-index, stp/ldp signed offset
    stp x29, x30, [sp, #-16]!
    ldp x29, x30, [sp], #16
    stp x19, x20, [sp, #16]
    ldp x19, x20, [sp, #16]

    // adr +0x123  (adrp is byte-tested separately — gas emits a reloc here)
    adr x0, . + 0x123

    // FMOV variants: s<-w, w<-s, d<-x, x<-d, s<-s, d<-d
    fmov s0, w0
    fmov w0, s1
    fmov d2, x3
    fmov x4, d5
    fmov s6, s7
    fmov d8, d9

    // fadd/fsub/fmul/fdiv s, fadd_d, fneg s/d, fabs_s, fsqrt_d
    fadd s0, s1, s2
    fadd d0, d1, d2
    fsub s0, s1, s2
    fmul s0, s1, s2
    fdiv s0, s1, s2
    fneg s3, s4
    fneg d3, d4
    fabs s5, s6
    fsqrt d7, d8

    // fcmp_s/d, fcvt s<->d
    fcmp s0, s1
    fcmp d0, d1
    fcvt s0, d1
    fcvt d0, s1

    // fcvtzs w0, s1   ;   fcvtzu x2, d3
    // scvtf s4, w5    ;   ucvtf d6, x7
    fcvtzs w0, s1
    fcvtzu x2, d3
    scvtf s4, w5
    ucvtf d6, x7

    // ldr/str s 16, ldr/str d 32
    ldr s0, [x1, #16]
    str s0, [x1, #16]
    ldr d2, [x3, #32]
    str d2, [x3, #32]

    // nop, brk #0xCAFE, svc #0, dmb ishst
    nop
    brk #0xCAFE
    svc #0
    dmb ishst

    // mov x29, sp  (special-case: encoded as add x29, sp, #0)
    mov x29, sp
