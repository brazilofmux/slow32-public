// SLOW-32 DBT: x86-64 Code Emitter
// Stage 1 - Minimal emitter for correctness-first translation

#ifndef DBT_EMIT_X64_H
#define DBT_EMIT_X64_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

// Emitter context
typedef struct {
    uint8_t *buf;       // Output buffer
    size_t capacity;    // Buffer size
    size_t offset;      // Current write position
    bool overflow;      // Set if we ran out of space
    bool rax_pending;   // DEBUG: Set if RAX holds a pending write value (must not be clobbered)
    bool trace_enabled; // Emit trace logging for this block
    const char *trace_tag; // Optional tag for emit trace
} emit_ctx_t;

// x86-64 register encoding
typedef enum {
    X64_NOREG = -1,  // Sentinel: register not available
    RAX = 0, RCX = 1, RDX = 2, RBX = 3,
    RSP = 4, RBP = 5, RSI = 6, RDI = 7,
    R8  = 8, R9  = 9, R10 = 10, R11 = 11,
    R12 = 12, R13 = 13, R14 = 14, R15 = 15
} x64_reg_t;

// REX prefix bits
#define REX_BASE    0x40
#define REX_W       0x08    // 64-bit operand
#define REX_R       0x04    // ModR/M reg extension
#define REX_X       0x02    // SIB index extension
#define REX_B       0x01    // ModR/M r/m or SIB base extension

// ModR/M byte construction
#define MODRM(mod, reg, rm) (((mod) << 6) | (((reg) & 7) << 3) | ((rm) & 7))
#define MOD_INDIRECT    0   // [rm] or [SIB]
#define MOD_DISP8       1   // [rm + disp8]
#define MOD_DISP32      2   // [rm + disp32]
#define MOD_DIRECT      3   // rm (register direct)

// SIB byte construction
#define SIB(scale, index, base) (((scale) << 6) | (((index) & 7) << 3) | ((base) & 7))

// ============================================================================
// Core emitter functions
// ============================================================================

// Initialize emitter with buffer
void emit_init(emit_ctx_t *ctx, uint8_t *buf, size_t capacity);

// Get current position
static inline uint8_t *emit_ptr(emit_ctx_t *ctx) { return ctx->buf + ctx->offset; }
static inline size_t emit_offset(emit_ctx_t *ctx) { return ctx->offset; }

// Raw byte emission
void emit_byte(emit_ctx_t *ctx, uint8_t b);
void emit_word(emit_ctx_t *ctx, uint16_t w);
void emit_dword(emit_ctx_t *ctx, uint32_t d);
void emit_qword(emit_ctx_t *ctx, uint64_t q);

// ============================================================================
// Data movement
// ============================================================================

// mov r32, r32
void emit_mov_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);

// mov r64, r64
void emit_mov_r64_r64(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);

// mov r32, imm32
void emit_mov_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, uint32_t imm);

// mov r64, imm64
void emit_mov_r64_imm64(emit_ctx_t *ctx, x64_reg_t dst, uint64_t imm);

// mov r32, [base + disp32]
void emit_mov_r32_m32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp);

// movzx r32, byte [base + disp32]
void emit_movzx_r32_m8(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp);

// mov [base + disp32], r32
void emit_mov_m32_r32(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, x64_reg_t src);

// mov [base + disp32], imm32
void emit_mov_m32_imm32(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, uint32_t imm);

// mov r32, [base + index*1]  (for guest memory access)
void emit_mov_r32_m32_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index);

// mov [base + index*1], r32
void emit_mov_m32_r32_idx(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index, x64_reg_t src);

// movsx r32, byte [base + index]  (sign-extend byte)
void emit_movsx_r32_m8_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index);

// movzx r32, byte [base + index]  (zero-extend byte)
void emit_movzx_r32_m8_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index);

// movsx r32, word [base + index]  (sign-extend word)
void emit_movsx_r32_m16_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index);

// movzx r32, word [base + index]  (zero-extend word)
void emit_movzx_r32_m16_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index);

// mov byte [base + index], r8
void emit_mov_m8_r8_idx(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index, x64_reg_t src);

// mov word [base + index], r16
void emit_mov_m16_r16_idx(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index, x64_reg_t src);

// lea r32, [base + disp]  (compute address without memory access)
void emit_lea_r32_r32_disp(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp);

// movzx r32, r8 (zero-extend byte register to dword)
void emit_movzx_r32_r8(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);

// ============================================================================
// 64-bit memory operations (Stage 3)
// ============================================================================

// mov r64, [base + disp32]
void emit_mov_r64_m64(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp);

// mov r64, [base + index*scale + disp32]  (scale: 1,2,4,8)
void emit_mov_r64_m64_sib(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base,
                          x64_reg_t index, uint8_t scale, int32_t disp);

// cmp [base + disp32], r32
void emit_cmp_m32_r32(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, x64_reg_t src);

// test r64, r64
void emit_test_r64_r64(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b);

// jz rel32 (alias for je)
#define emit_jz_rel32 emit_je_rel32

// ============================================================================
// Stage 3 Phase 2: RAS support
// ============================================================================

// mov r32, [base + index*scale + disp32]
void emit_mov_r32_m32_sib(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base,
                          x64_reg_t index, uint8_t scale, int32_t disp);

// mov [base + index*scale + disp32], imm32
void emit_mov_m32_imm32_sib(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index,
                            uint8_t scale, int32_t disp, uint32_t imm);

// ============================================================================
// Arithmetic
// ============================================================================

// add r32, r32
void emit_add_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);

// add r32, imm32
void emit_add_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm);

// sub r32, r32
void emit_sub_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);

// sub r32, imm32
void emit_sub_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm);

// imul r32, r32  (signed multiply, low 32 bits)
void emit_imul_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);

// mul r32  (unsigned: edx:eax = eax * r32)
void emit_mul_r32(emit_ctx_t *ctx, x64_reg_t src);

// idiv r32  (signed: eax = edx:eax / r32, edx = remainder)
void emit_idiv_r32(emit_ctx_t *ctx, x64_reg_t src);

// div r32  (unsigned version)
void emit_div_r32(emit_ctx_t *ctx, x64_reg_t src);

// cdq  (sign-extend eax into edx:eax)
void emit_cdq(emit_ctx_t *ctx);

// xor r32, r32 (also used to zero a register)
void emit_xor_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);

// and r32, r32
void emit_and_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);

// and r32, imm32
void emit_and_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm);

// or r32, r32
void emit_or_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src);

// or r32, imm32
void emit_or_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm);

// xor r32, imm32
void emit_xor_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm);

// neg r32  (two's complement negate)
void emit_neg_r32(emit_ctx_t *ctx, x64_reg_t dst);

// not r32  (bitwise NOT)
void emit_not_r32(emit_ctx_t *ctx, x64_reg_t dst);

// ============================================================================
// Shifts
// ============================================================================

// shl r32, cl
void emit_shl_r32_cl(emit_ctx_t *ctx, x64_reg_t dst);

// shl r32, imm8
void emit_shl_r32_imm8(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm);

// shr r32, cl
void emit_shr_r32_cl(emit_ctx_t *ctx, x64_reg_t dst);

// shr r32, imm8
void emit_shr_r32_imm8(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm);

// sar r32, cl
void emit_sar_r32_cl(emit_ctx_t *ctx, x64_reg_t dst);

// sar r32, imm8
void emit_sar_r32_imm8(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm);

// ============================================================================
// Comparisons
// ============================================================================

// cmp r32, r32
void emit_cmp_r32_r32(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b);

// cmp r32, imm32
void emit_cmp_r32_imm32(emit_ctx_t *ctx, x64_reg_t a, int32_t imm);

// test r32, r32
void emit_test_r32_r32(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b);

// Conditional set instructions (result in low byte of register)
void emit_sete(emit_ctx_t *ctx, x64_reg_t dst);     // ZF=1
void emit_setne(emit_ctx_t *ctx, x64_reg_t dst);    // ZF=0
void emit_setl(emit_ctx_t *ctx, x64_reg_t dst);     // SF!=OF (signed <)
void emit_setge(emit_ctx_t *ctx, x64_reg_t dst);    // SF==OF (signed >=)
void emit_setg(emit_ctx_t *ctx, x64_reg_t dst);     // ZF=0 && SF==OF (signed >)
void emit_setle(emit_ctx_t *ctx, x64_reg_t dst);    // ZF=1 || SF!=OF (signed <=)
void emit_setb(emit_ctx_t *ctx, x64_reg_t dst);     // CF=1 (unsigned <)
void emit_setae(emit_ctx_t *ctx, x64_reg_t dst);    // CF=0 (unsigned >=)
void emit_seta(emit_ctx_t *ctx, x64_reg_t dst);     // CF=0 && ZF=0 (unsigned >)
void emit_setbe(emit_ctx_t *ctx, x64_reg_t dst);    // CF=1 || ZF=1 (unsigned <=)

// Helper to get jcc function from condition code (0x80-0x8F)
void *emit_jcc_rel32_from_cc(uint8_t cc);

// ============================================================================
// Control flow
// ============================================================================

// jmp rel32
void emit_jmp_rel32(emit_ctx_t *ctx, int32_t offset);

// jmp r64
void emit_jmp_r64(emit_ctx_t *ctx, x64_reg_t target);

// Conditional jumps (rel32)
void emit_je_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jne_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jl_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jge_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jg_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jle_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jb_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jae_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_ja_rel32(emit_ctx_t *ctx, int32_t offset);
void emit_jbe_rel32(emit_ctx_t *ctx, int32_t offset);

// Short conditional jump: 7x rel8 (2 bytes)
// cc is the condition code (low nibble of near 0F 8x opcode)
void emit_jcc_short(emit_ctx_t *ctx, uint8_t cc, int8_t rel8);

// call rel32
void emit_call_rel32(emit_ctx_t *ctx, int32_t offset);

// call r64
void emit_call_r64(emit_ctx_t *ctx, x64_reg_t target);

// ret
void emit_ret(emit_ctx_t *ctx);

// ============================================================================
// Stack operations
// ============================================================================

// push r64
void emit_push_r64(emit_ctx_t *ctx, x64_reg_t reg);

// pop r64
void emit_pop_r64(emit_ctx_t *ctx, x64_reg_t reg);

// ============================================================================
// Misc
// ============================================================================

// nop
void emit_nop(emit_ctx_t *ctx);

// int3 (breakpoint)
void emit_int3(emit_ctx_t *ctx);

// ============================================================================
// SSE / FP helpers (f32 inline translation)
// ============================================================================

// movd xmm, r32  (66 [REX] 0F 6E modrm)
void emit_movd_xmm_r32(emit_ctx_t *ctx, uint8_t xmm, x64_reg_t r32);

// movd r32, xmm  (66 [REX] 0F 7E modrm)
void emit_movd_r32_xmm(emit_ctx_t *ctx, x64_reg_t r32, uint8_t xmm);

// addss xmm_dst, xmm_src  (F3 0F 58 modrm)
void emit_addss(emit_ctx_t *ctx, uint8_t dst_xmm, uint8_t src_xmm);

// subss xmm_dst, xmm_src  (F3 0F 5C modrm)
void emit_subss(emit_ctx_t *ctx, uint8_t dst_xmm, uint8_t src_xmm);

// mulss xmm_dst, xmm_src  (F3 0F 59 modrm)
void emit_mulss(emit_ctx_t *ctx, uint8_t dst_xmm, uint8_t src_xmm);

// divss xmm_dst, xmm_src  (F3 0F 5E modrm)
void emit_divss(emit_ctx_t *ctx, uint8_t dst_xmm, uint8_t src_xmm);

// sqrtss xmm_dst, xmm_src  (F3 0F 51 modrm)
void emit_sqrtss(emit_ctx_t *ctx, uint8_t dst_xmm, uint8_t src_xmm);

// ucomiss xmm1, xmm2  (0F 2E modrm)
void emit_ucomiss(emit_ctx_t *ctx, uint8_t xmm1, uint8_t xmm2);

// cvttss2si r32, xmm  (F3 [REX] 0F 2C modrm)
void emit_cvttss2si_r32_xmm(emit_ctx_t *ctx, x64_reg_t r32, uint8_t xmm);

// cvtsi2ss xmm, r32  (F3 [REX] 0F 2A modrm)
void emit_cvtsi2ss_xmm_r32(emit_ctx_t *ctx, uint8_t xmm, x64_reg_t r32);

// cvtsi2ss xmm, r64  (F3 REX.W 0F 2A modrm) — for unsigned int32 via zero-extended int64
void emit_cvtsi2ss_xmm_r64(emit_ctx_t *ctx, uint8_t xmm, x64_reg_t r64);

// setnp r8  (0F 9B modrm)  — PF=0
void emit_setnp(emit_ctx_t *ctx, x64_reg_t dst);

// setp r8   (0F 9A modrm)  — PF=1
void emit_setp(emit_ctx_t *ctx, x64_reg_t dst);

// ============================================================================
// Patching helpers
// ============================================================================

// Patch a rel32 displacement at the given offset
// target_offset is where we want to jump TO
// patch_offset is where the rel32 value lives (after opcode)
void emit_patch_rel32(emit_ctx_t *ctx, size_t patch_offset, size_t target_offset);

#endif // DBT_EMIT_X64_H
