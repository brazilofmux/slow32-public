// SLOW-32 DBT: AArch64 Code Emitter
// Stage 1 - Minimal emitter for correctness-first translation

#ifndef DBT_EMIT_A64_H
#define DBT_EMIT_A64_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

// Emitter context (shared with x86-64 emitter — same layout)
typedef struct {
    uint8_t *buf;       // Output buffer
    size_t capacity;    // Buffer size
    size_t offset;      // Current write position
    bool overflow;      // Set if we ran out of space
    bool rax_pending;   // DEBUG: Set if W0 holds a pending write value
    bool trace_enabled; // Emit trace logging for this block
    const char *trace_tag; // Optional tag for emit trace
} emit_ctx_t;

// AArch64 register encoding (0-30 = X0-X30, 31 = XZR/SP depending on context)
typedef enum {
    A64_NOREG = -1,   // Sentinel: register not available
    W0  =  0, W1  =  1, W2  =  2, W3  =  3,
    W4  =  4, W5  =  5, W6  =  6, W7  =  7,
    W8  =  8, W9  =  9, W10 = 10, W11 = 11,
    W12 = 12, W13 = 13, W14 = 14, W15 = 15,
    W16 = 16, W17 = 17, W18 = 18, W19 = 19,
    W20 = 20, W21 = 21, W22 = 22, W23 = 23,
    W24 = 24, W25 = 25, W26 = 26, W27 = 27,
    W28 = 28, W29 = 29, W30 = 30,
    WZR = 31   // Zero register (or SP in some contexts)
} a64_reg_t;

// AArch64 condition codes (for B.cond, CSET, CSEL, etc.)
typedef enum {
    COND_EQ = 0x0,   // Equal (Z=1)
    COND_NE = 0x1,   // Not equal (Z=0)
    COND_CS = 0x2,   // Carry set / unsigned >= (C=1)
    COND_HS = 0x2,   // Alias for CS
    COND_CC = 0x3,   // Carry clear / unsigned < (C=0)
    COND_LO = 0x3,   // Alias for CC
    COND_MI = 0x4,   // Minus / negative (N=1)
    COND_PL = 0x5,   // Plus / positive or zero (N=0)
    COND_VS = 0x6,   // Overflow (V=1)
    COND_VC = 0x7,   // No overflow (V=0)
    COND_HI = 0x8,   // Unsigned > (C=1 && Z=0)
    COND_LS = 0x9,   // Unsigned <= (C=0 || Z=1)
    COND_GE = 0xA,   // Signed >= (N=V)
    COND_LT = 0xB,   // Signed < (N!=V)
    COND_GT = 0xC,   // Signed > (Z=0 && N=V)
    COND_LE = 0xD,   // Signed <= (Z=1 || N!=V)
    COND_AL = 0xE,   // Always
} a64_cond_t;

// ============================================================================
// Core emitter functions
// ============================================================================

// Initialize emitter with buffer
void emit_init(emit_ctx_t *ctx, uint8_t *buf, size_t capacity);

// Get current position
static inline uint8_t *emit_ptr(emit_ctx_t *ctx) { return ctx->buf + ctx->offset; }
static inline size_t emit_offset(emit_ctx_t *ctx) { return ctx->offset; }

// Raw emission
void emit_inst(emit_ctx_t *ctx, uint32_t inst);

// ============================================================================
// Immediate helpers
// ============================================================================

// MOVZ Wd, #imm16, LSL #shift  (shift = 0 or 16)
void emit_movz_w32(emit_ctx_t *ctx, a64_reg_t rd, uint16_t imm16, int shift);

// MOVK Wd, #imm16, LSL #shift
void emit_movk_w32(emit_ctx_t *ctx, a64_reg_t rd, uint16_t imm16, int shift);

// MOV Wd, #imm32 (MOVZ + optional MOVK sequence)
void emit_mov_w32_imm32(emit_ctx_t *ctx, a64_reg_t rd, uint32_t imm);

// MOVZ Xd, #imm16, LSL #shift  (shift = 0, 16, 32, 48)
void emit_movz_x64(emit_ctx_t *ctx, a64_reg_t rd, uint16_t imm16, int shift);

// MOVK Xd, #imm16, LSL #shift
void emit_movk_x64(emit_ctx_t *ctx, a64_reg_t rd, uint16_t imm16, int shift);

// MOV Xd, #imm64 (MOVZ + MOVK sequence, up to 4 instructions)
void emit_mov_x64_imm64(emit_ctx_t *ctx, a64_reg_t rd, uint64_t imm);

// ============================================================================
// Data movement
// ============================================================================

// MOV Wd, Ws  (ORR Wd, WZR, Ws)
void emit_mov_w32_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rs);

// MOV Xd, Xs  (ORR Xd, XZR, Xs)
void emit_mov_x64_x64(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rs);

// ============================================================================
// Load/Store - unsigned immediate offset
// ============================================================================

// LDR Wt, [Xn, #imm]  (unsigned offset, scaled by 4)
void emit_ldr_w32_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset);

// STR Wt, [Xn, #imm]
void emit_str_w32_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset);

// LDRB Wt, [Xn, #imm]
void emit_ldrb_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset);

// LDRSB Wt, [Xn, #imm]
void emit_ldrsb_w32_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset);

// LDRH Wt, [Xn, #imm]
void emit_ldrh_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset);

// LDRSH Wt, [Xn, #imm]
void emit_ldrsh_w32_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset);

// STRB Wt, [Xn, #imm]
void emit_strb_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset);

// STRH Wt, [Xn, #imm]
void emit_strh_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset);

// ============================================================================
// Load/Store - 64-bit (for pointer loads: block table, host_code, etc.)
// ============================================================================

// LDR Xt, [Xn, #imm]  (unsigned offset, scaled by 8)
void emit_ldr_x64_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset);

// LDR Xt, [Xn, Xm, LSL #3]  (register offset, scaled by 8)
void emit_ldr_x64_reg_lsl3(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm);

// STR Xt, [Xn, #imm]  (unsigned offset, scaled by 8)
void emit_str_x64_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset);

// CBZ Xt, offset (64-bit zero test)
void emit_cbz_x64(emit_ctx_t *ctx, a64_reg_t rt, int32_t byte_offset);

// ============================================================================
// Load/Store - register offset (for guest memory: [Xbase, Windex, UXTW])
// ============================================================================

// LDR Wt, [Xn, Wm, UXTW]
void emit_ldr_w32_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm);

// STR Wt, [Xn, Wm, UXTW]
void emit_str_w32_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm);

// LDRB Wt, [Xn, Wm, UXTW]
void emit_ldrb_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm);

// LDRSB Wt, [Xn, Wm, UXTW]
void emit_ldrsb_w32_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm);

// LDRH Wt, [Xn, Wm, UXTW]
void emit_ldrh_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm);

// LDRSH Wt, [Xn, Wm, UXTW]
void emit_ldrsh_w32_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm);

// STRB Wt, [Xn, Wm, UXTW]
void emit_strb_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm);

// STRH Wt, [Xn, Wm, UXTW]
void emit_strh_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm);

// ============================================================================
// Arithmetic (32-bit)
// ============================================================================

// ADD Wd, Wn, Wm
void emit_add_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// ADD Wd, Wn, #imm12
void emit_add_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t imm12);

// SUB Wd, Wn, Wm
void emit_sub_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// SUB Wd, Wn, #imm12
void emit_sub_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t imm12);

// MUL Wd, Wn, Wm
void emit_mul_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// SDIV Wd, Wn, Wm
void emit_sdiv_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// UDIV Wd, Wn, Wm
void emit_udiv_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// MSUB Wd, Wn, Wm, Wa  (Wa - Wn*Wm)  — used for remainder
void emit_msub_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm, a64_reg_t ra);

// SMULL Xd, Wn, Wm  (signed 32x32→64)
void emit_smull(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// UMULL Xd, Wn, Wm  (unsigned 32x32→64)
void emit_umull(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// NEG Wd, Wm  (SUB Wd, WZR, Wm)
void emit_neg_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rm);

// ============================================================================
// Logical (32-bit)
// ============================================================================

// AND Wd, Wn, Wm
void emit_and_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// AND Wd, Wn, #imm  (logical immediate — fallback to register form if not encodable)
bool emit_and_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t imm);

// ORR Wd, Wn, Wm
void emit_orr_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// ORR Wd, Wn, #imm  (logical immediate)
bool emit_orr_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t imm);

// EOR Wd, Wn, Wm
void emit_eor_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// EOR Wd, Wn, Wm, LSR #shift
void emit_eor_w32_lsr(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm, uint32_t shift);

// EOR Wd, Wn, #imm  (logical immediate)
bool emit_eor_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t imm);

// MVN Wd, Wm  (ORN Wd, WZR, Wm)
void emit_mvn_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rm);

// TST Wn, Wm  (ANDS WZR, Wn, Wm)
void emit_tst_w32(emit_ctx_t *ctx, a64_reg_t rn, a64_reg_t rm);

// ============================================================================
// Shifts (32-bit)
// ============================================================================

// LSLV Wd, Wn, Wm
void emit_lslv_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// LSRV Wd, Wn, Wm
void emit_lsrv_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// ASRV Wd, Wn, Wm
void emit_asrv_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm);

// LSL Wd, Wn, #imm  (UBFM alias)
void emit_lsl_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t shift);

// LSR Wd, Wn, #imm  (UBFM alias)
void emit_lsr_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t shift);

// ASR Wd, Wn, #imm  (SBFM alias)
void emit_asr_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t shift);

// ASR Xd, Xn, #imm  (SBFM alias, 64-bit)
void emit_asr_x64_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t shift);

// ============================================================================
// Compare
// ============================================================================

// CMP Wn, Wm  (SUBS WZR, Wn, Wm)
void emit_cmp_w32_w32(emit_ctx_t *ctx, a64_reg_t rn, a64_reg_t rm);

// CMP Wn, #imm12
void emit_cmp_w32_imm(emit_ctx_t *ctx, a64_reg_t rn, uint32_t imm12);

// CMN Wn, #imm12  (ADDS WZR, Wn, #imm12) — for comparing against negative immediates
void emit_cmn_w32_imm(emit_ctx_t *ctx, a64_reg_t rn, uint32_t imm12);

// ============================================================================
// Conditional set
// ============================================================================

// CSET Wd, cond  (CSINC Wd, WZR, WZR, invert(cond))
void emit_cset_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_cond_t cond);

// ============================================================================
// Branches
// ============================================================================

// B (unconditional, PC-relative, +-128MB)
void emit_b(emit_ctx_t *ctx, int32_t byte_offset);

// B.cond (conditional, PC-relative, +-1MB)
void emit_b_cond(emit_ctx_t *ctx, a64_cond_t cond, int32_t byte_offset);

// CBZ Wt, offset
void emit_cbz_w32(emit_ctx_t *ctx, a64_reg_t rt, int32_t byte_offset);

// CBNZ Wt, offset
void emit_cbnz_w32(emit_ctx_t *ctx, a64_reg_t rt, int32_t byte_offset);

// BR Xn  (branch to register)
void emit_br(emit_ctx_t *ctx, a64_reg_t rn);

// BLR Xn  (branch-link to register)
void emit_blr(emit_ctx_t *ctx, a64_reg_t rn);

// RET {Xn}  (default X30)
void emit_ret_lr(emit_ctx_t *ctx);

// ============================================================================
// Stack (save/restore pairs)
// ============================================================================

// STP Xt1, Xt2, [SP, #imm]!  (pre-index)
void emit_stp_pre_x64(emit_ctx_t *ctx, a64_reg_t rt1, a64_reg_t rt2, int32_t imm);

// LDP Xt1, Xt2, [SP], #imm  (post-index)
void emit_ldp_post_x64(emit_ctx_t *ctx, a64_reg_t rt1, a64_reg_t rt2, int32_t imm);

// ============================================================================
// Misc
// ============================================================================

// NOP
void emit_nop(emit_ctx_t *ctx);

// BRK #imm16 (breakpoint)
void emit_brk(emit_ctx_t *ctx, uint16_t imm);

// ============================================================================
// Patching
// ============================================================================

// Patch a B instruction at patch_site to jump to target
void emit_patch_b26(uint32_t *patch_site, uint32_t *target);

// Patch a rel32 (used by block cache infrastructure)
// On AArch64 this patches a B instruction
void emit_patch_rel32(emit_ctx_t *ctx, size_t patch_offset, size_t target_offset);

// ============================================================================
// Logical immediate encoder
// ============================================================================

// Try to encode a 32-bit value as an AArch64 logical immediate.
// Returns true and sets *encoded if encodable, false otherwise.
bool a64_encode_logical_imm(uint32_t val, uint32_t *encoded);

#endif // DBT_EMIT_A64_H
