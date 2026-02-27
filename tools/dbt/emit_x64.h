// SLOW-32 DBT: x86-64 Code Emitter (all-inline)
//
// Every emitter function is static inline to eliminate function-call overhead
// during translation. Debug/trace features are preserved but cost only a
// single predicted branch when disabled.

#ifndef DBT_EMIT_X64_H
#define DBT_EMIT_X64_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

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
// Internal helpers
// ============================================================================

#define CHECK_RAX_WRITE(ctx, reg) \
    do { \
        if ((reg) == RAX) { \
            assert(!(ctx)->rax_pending && "Clobbering RAX with pending write!"); \
        } \
    } while (0)

static inline uint8_t rex_for_regs(x64_reg_t r, x64_reg_t rm, bool w64) {
    uint8_t rex = 0;
    if (w64) rex |= REX_W;
    if (r >= R8) rex |= REX_R;
    if (rm >= R8) rex |= REX_B;
    return rex;
}

static inline uint8_t rex_for_sib(x64_reg_t r, x64_reg_t base, x64_reg_t index, bool w64) {
    uint8_t rex = 0;
    if (w64) rex |= REX_W;
    if (r >= R8) rex |= REX_R;
    if (base >= R8) rex |= REX_B;
    if (index >= R8) rex |= REX_X;
    return rex;
}

static inline void emit_rex_if_needed(emit_ctx_t *ctx, uint8_t rex) {
    if (rex) {
        if (ctx->offset < ctx->capacity)
            ctx->buf[ctx->offset] = REX_BASE | rex;
        ctx->offset++;
    }
}

static inline void emit_trace_inst(emit_ctx_t *ctx, const char *op,
                                    size_t start_off, size_t end_off) {
    if (!ctx->trace_enabled) return;
    const char *tag = ctx->trace_tag ? ctx->trace_tag : "none";
    fprintf(stderr, "[emit] tag=%s op=%s off=0x%zx bytes=", tag, op, start_off);
    for (size_t i = start_off; i < end_off; i++)
        fprintf(stderr, "%02X", ctx->buf[i]);
    fprintf(stderr, "\n");
}

// Some instructions need REX for low byte access to SPL, BPL, SIL, DIL
static inline bool needs_rex_for_byte_reg(x64_reg_t r) {
    return r >= RSP && r <= RDI;
}

// ============================================================================
// Core emitter functions
// ============================================================================

static inline void emit_init(emit_ctx_t *ctx, uint8_t *buf, size_t capacity) {
    ctx->buf = buf;
    ctx->capacity = capacity;
    ctx->offset = 0;
    ctx->overflow = false;
    ctx->trace_enabled = false;
    ctx->trace_tag = NULL;
}

// Get current position
static inline uint8_t *emit_ptr(emit_ctx_t *ctx) { return ctx->buf + ctx->offset; }
static inline size_t emit_offset(emit_ctx_t *ctx) { return ctx->offset; }

// Raw byte emission
static inline void emit_byte(emit_ctx_t *ctx, uint8_t b) {
    if (ctx->offset < ctx->capacity)
        ctx->buf[ctx->offset] = b;
    else
        ctx->overflow = true;
    ctx->offset++;
}

static inline void emit_word(emit_ctx_t *ctx, uint16_t w) {
    emit_byte(ctx, w & 0xFF);
    emit_byte(ctx, (w >> 8) & 0xFF);
}

static inline void emit_dword(emit_ctx_t *ctx, uint32_t d) {
    if (ctx->offset + 4 <= ctx->capacity) {
        memcpy(ctx->buf + ctx->offset, &d, 4);
        ctx->offset += 4;
    } else {
        emit_byte(ctx, d & 0xFF);
        emit_byte(ctx, (d >> 8) & 0xFF);
        emit_byte(ctx, (d >> 16) & 0xFF);
        emit_byte(ctx, (d >> 24) & 0xFF);
    }
}

static inline void emit_qword(emit_ctx_t *ctx, uint64_t q) {
    if (ctx->offset + 8 <= ctx->capacity) {
        memcpy(ctx->buf + ctx->offset, &q, 8);
        ctx->offset += 8;
    } else {
        emit_dword(ctx, q & 0xFFFFFFFF);
        emit_dword(ctx, (q >> 32) & 0xFFFFFFFF);
    }
}

// ============================================================================
// Data movement
// ============================================================================

// mov r32, r32  (89 /r)
static inline void emit_mov_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x89);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
}

// mov r64, r64  (REX.W 89 /r)
static inline void emit_mov_r64_r64(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    uint8_t rex = rex_for_regs(src, dst, true);
    emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x89);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
}

// mov r32, imm32  (B8+rd id)
static inline void emit_mov_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, uint32_t imm) {
    CHECK_RAX_WRITE(ctx, dst);
    if (dst >= R8) emit_byte(ctx, REX_BASE | REX_B);
    emit_byte(ctx, 0xB8 + (dst & 7));
    emit_dword(ctx, imm);
}

// mov r64, imm64  (REX.W B8+rd io)
static inline void emit_mov_r64_imm64(emit_ctx_t *ctx, x64_reg_t dst, uint64_t imm) {
    uint8_t rex = REX_W;
    if (dst >= R8) rex |= REX_B;
    emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0xB8 + (dst & 7));
    emit_qword(ctx, imm);
}

// mov r32, [base + disp]  (8B /r) - uses disp8 when possible
static inline void emit_mov_r32_m32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_regs(dst, base, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x8B);

    bool use_disp8 = (disp >= -128 && disp <= 127);
    uint8_t mod = use_disp8 ? MOD_DISP8 : MOD_DISP32;

    if ((base & 7) == RSP) {
        emit_byte(ctx, MODRM(mod, dst, RSP));
        emit_byte(ctx, SIB(0, RSP, RSP));
    } else {
        emit_byte(ctx, MODRM(mod, dst, base));
    }
    if (use_disp8)
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    else
        emit_dword(ctx, (uint32_t)disp);
}

// movzx r32, byte [base + disp]  (0F B6 /r) - uses disp8 when possible
static inline void emit_movzx_r32_m8(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_regs(dst, base, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xB6);

    bool use_disp8 = (disp >= -128 && disp <= 127);
    uint8_t mod = use_disp8 ? MOD_DISP8 : MOD_DISP32;

    if ((base & 7) == RSP) {
        emit_byte(ctx, MODRM(mod, dst, RSP));
        emit_byte(ctx, SIB(0, RSP, RSP));
    } else {
        emit_byte(ctx, MODRM(mod, dst, base));
    }
    if (use_disp8)
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    else
        emit_dword(ctx, (uint32_t)disp);
}

// mov [base + disp], r32  (89 /r) - uses disp8 when possible
static inline void emit_mov_m32_r32(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, x64_reg_t src) {
    uint8_t rex = rex_for_regs(src, base, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x89);

    bool use_disp8 = (disp >= -128 && disp <= 127);
    uint8_t mod = use_disp8 ? MOD_DISP8 : MOD_DISP32;

    if ((base & 7) == RSP) {
        emit_byte(ctx, MODRM(mod, src, RSP));
        emit_byte(ctx, SIB(0, RSP, RSP));
    } else {
        emit_byte(ctx, MODRM(mod, src, base));
    }
    if (use_disp8)
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    else
        emit_dword(ctx, (uint32_t)disp);
}

// mov [base + disp], imm32  (C7 /0 id) - uses disp8 when possible
static inline void emit_mov_m32_imm32(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, uint32_t imm) {
    uint8_t rex = 0;
    if (base >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xC7);

    bool use_disp8 = (disp >= -128 && disp <= 127);
    uint8_t mod = use_disp8 ? MOD_DISP8 : MOD_DISP32;

    if ((base & 7) == RSP) {
        emit_byte(ctx, MODRM(mod, 0, RSP));
        emit_byte(ctx, SIB(0, RSP, RSP));
    } else {
        emit_byte(ctx, MODRM(mod, 0, base));
    }
    if (use_disp8)
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    else
        emit_dword(ctx, (uint32_t)disp);
    emit_dword(ctx, imm);
}

// mov r32, [base + index*1]  (8B /r with SIB)
static inline void emit_mov_r32_m32_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_sib(dst, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x8B);
    emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// mov [base + index*1], r32  (89 /r with SIB)
static inline void emit_mov_m32_r32_idx(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index, x64_reg_t src) {
    uint8_t rex = rex_for_sib(src, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x89);
    emit_byte(ctx, MODRM(MOD_INDIRECT, src, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// movsx r32, byte [base + index]  (0F BE /r with SIB)
static inline void emit_movsx_r32_m8_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_sib(dst, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xBE);
    emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// movzx r32, byte [base + index]  (0F B6 /r with SIB)
static inline void emit_movzx_r32_m8_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_sib(dst, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xB6);
    emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// movsx r32, word [base + index]  (0F BF /r with SIB)
static inline void emit_movsx_r32_m16_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_sib(dst, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xBF);
    emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// movzx r32, word [base + index]  (0F B7 /r with SIB)
static inline void emit_movzx_r32_m16_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_sib(dst, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xB7);
    emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// mov byte [base + index], r8  (88 /r with SIB)
static inline void emit_mov_m8_r8_idx(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index, x64_reg_t src) {
    uint8_t rex = rex_for_sib(src, base, index, false);
    if (needs_rex_for_byte_reg(src)) rex |= 0;  // Just ensure REX exists
    if (rex || needs_rex_for_byte_reg(src)) emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x88);
    emit_byte(ctx, MODRM(MOD_INDIRECT, src, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// mov word [base + index], r16  (66 89 /r with SIB)
static inline void emit_mov_m16_r16_idx(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index, x64_reg_t src) {
    emit_byte(ctx, 0x66);
    uint8_t rex = rex_for_sib(src, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x89);
    emit_byte(ctx, MODRM(MOD_INDIRECT, src, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// lea r32, [base + disp]  (8D /r)
static inline void emit_lea_r32_r32_disp(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_regs(dst, base, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x8D);

    // R13/RBP (low 3 bits = 101) with mod=00 encodes [RIP+disp32], so force disp8
    bool force_disp = ((base & 7) == RBP);
    bool use_disp8 = (disp >= -128 && disp <= 127);

    uint8_t mod;
    if (disp == 0 && !force_disp)
        mod = MOD_INDIRECT;
    else if (use_disp8)
        mod = MOD_DISP8;
    else
        mod = MOD_DISP32;

    if ((base & 7) == RSP) {
        emit_byte(ctx, MODRM(mod, dst, RSP));
        emit_byte(ctx, SIB(0, RSP, base));
    } else {
        emit_byte(ctx, MODRM(mod, dst, base));
    }

    if (mod == MOD_DISP8)
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    else if (mod == MOD_DISP32)
        emit_dword(ctx, (uint32_t)disp);
}

// movzx r32, r8  (0F B6 /r)
static inline void emit_movzx_r32_r8(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    uint8_t rex = rex_for_regs(dst, src, false);
    if (needs_rex_for_byte_reg(src)) rex |= 0;
    if (rex || needs_rex_for_byte_reg(src)) emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xB6);
    emit_byte(ctx, MODRM(MOD_DIRECT, dst, src));
}

// ============================================================================
// 64-bit memory operations
// ============================================================================

// mov r64, [base + disp]  (REX.W 8B /r) - uses disp8 when possible
static inline void emit_mov_r64_m64(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_regs(dst, base, true);
    emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x8B);

    bool use_disp8 = (disp >= -128 && disp <= 127);
    uint8_t mod = use_disp8 ? MOD_DISP8 : MOD_DISP32;

    if ((base & 7) == RSP) {
        emit_byte(ctx, MODRM(mod, dst, RSP));
        emit_byte(ctx, SIB(0, RSP, RSP));
    } else {
        emit_byte(ctx, MODRM(mod, dst, base));
    }
    if (use_disp8)
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    else
        emit_dword(ctx, (uint32_t)disp);
}

// Helper: convert scale value (1,2,4,8) to SIB scale bits (0,1,2,3)
static inline uint8_t scale_to_bits(uint8_t scale) {
    switch (scale) {
        case 1: return 0;
        case 2: return 1;
        case 4: return 2;
        case 8: return 3;
        default: return 0;
    }
}

// mov r64, [base + index*scale + disp32]  (REX.W 8B /r with SIB)
static inline void emit_mov_r64_m64_sib(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base,
                                         x64_reg_t index, uint8_t scale, int32_t disp) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t sb = scale_to_bits(scale);
    uint8_t rex = rex_for_sib(dst, base, index, true);
    emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x8B);

    if (disp == 0 && (base & 7) != RBP) {
        emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
        emit_byte(ctx, SIB(sb, index, base));
    } else if (disp >= -128 && disp <= 127) {
        emit_byte(ctx, MODRM(MOD_DISP8, dst, RSP));
        emit_byte(ctx, SIB(sb, index, base));
        emit_byte(ctx, (uint8_t)disp);
    } else {
        emit_byte(ctx, MODRM(MOD_DISP32, dst, RSP));
        emit_byte(ctx, SIB(sb, index, base));
        emit_dword(ctx, (uint32_t)disp);
    }
}

// cmp [base + disp], r32  (39 /r) - uses disp8 when possible
static inline void emit_cmp_m32_r32(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, x64_reg_t src) {
    uint8_t rex = rex_for_regs(src, base, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x39);

    bool use_disp8 = (disp >= -128 && disp <= 127);
    uint8_t mod = use_disp8 ? MOD_DISP8 : MOD_DISP32;

    if ((base & 7) == RSP) {
        emit_byte(ctx, MODRM(mod, src, RSP));
        emit_byte(ctx, SIB(0, RSP, RSP));
    } else {
        emit_byte(ctx, MODRM(mod, src, base));
    }
    if (use_disp8)
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    else
        emit_dword(ctx, (uint32_t)disp);
}

// test r64, r64  (REX.W 85 /r)
static inline void emit_test_r64_r64(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b) {
    uint8_t rex = rex_for_regs(b, a, true);
    emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x85);
    emit_byte(ctx, MODRM(MOD_DIRECT, b, a));
}

// jz rel32 (alias for je)
#define emit_jz_rel32 emit_je_rel32

// ============================================================================
// SIB-based memory operations (RAS support)
// ============================================================================

// mov r32, [base + index*scale + disp32]  (8B /r with SIB)
static inline void emit_mov_r32_m32_sib(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base,
                                         x64_reg_t index, uint8_t scale, int32_t disp) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t sb = scale_to_bits(scale);
    uint8_t rex = rex_for_sib(dst, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x8B);

    if (disp == 0 && (base & 7) != RBP) {
        emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
        emit_byte(ctx, SIB(sb, index, base));
    } else if (disp >= -128 && disp <= 127) {
        emit_byte(ctx, MODRM(MOD_DISP8, dst, RSP));
        emit_byte(ctx, SIB(sb, index, base));
        emit_byte(ctx, (uint8_t)disp);
    } else {
        emit_byte(ctx, MODRM(MOD_DISP32, dst, RSP));
        emit_byte(ctx, SIB(sb, index, base));
        emit_dword(ctx, (uint32_t)disp);
    }
}

// mov [base + index*scale + disp32], r32  (89 /r with SIB)
static inline void emit_mov_m32_r32_sib(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index,
                                         uint8_t scale, int32_t disp, x64_reg_t src) {
    uint8_t sb = scale_to_bits(scale);
    uint8_t rex = rex_for_sib(src, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x89);

    if (disp == 0 && (base & 7) != RBP) {
        emit_byte(ctx, MODRM(MOD_INDIRECT, src, RSP));
        emit_byte(ctx, SIB(sb, index, base));
    } else if (disp >= -128 && disp <= 127) {
        emit_byte(ctx, MODRM(MOD_DISP8, src, RSP));
        emit_byte(ctx, SIB(sb, index, base));
        emit_byte(ctx, (uint8_t)disp);
    } else {
        emit_byte(ctx, MODRM(MOD_DISP32, src, RSP));
        emit_byte(ctx, SIB(sb, index, base));
        emit_dword(ctx, (uint32_t)disp);
    }
}

// mov [base + index*scale + disp32], imm32  (C7 /0 with SIB)
static inline void emit_mov_m32_imm32_sib(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index,
                                            uint8_t scale, int32_t disp, uint32_t imm) {
    uint8_t sb = scale_to_bits(scale);
    uint8_t rex = 0;
    if (base >= R8) rex |= REX_B;
    if (index >= R8) rex |= REX_X;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xC7);

    if (disp == 0 && (base & 7) != RBP) {
        emit_byte(ctx, MODRM(MOD_INDIRECT, 0, RSP));
        emit_byte(ctx, SIB(sb, index, base));
    } else if (disp >= -128 && disp <= 127) {
        emit_byte(ctx, MODRM(MOD_DISP8, 0, RSP));
        emit_byte(ctx, SIB(sb, index, base));
        emit_byte(ctx, (uint8_t)disp);
    } else {
        emit_byte(ctx, MODRM(MOD_DISP32, 0, RSP));
        emit_byte(ctx, SIB(sb, index, base));
        emit_dword(ctx, (uint32_t)disp);
    }
    emit_dword(ctx, imm);
}

// ============================================================================
// Arithmetic
// ============================================================================

// add r32, r32  (01 /r)
static inline void emit_add_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x01);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
}

// add r32, imm32  (81 /0 id or 83 /0 ib for small imm)
static inline void emit_add_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);

    if (imm >= -128 && imm <= 127) {
        emit_byte(ctx, 0x83);
        emit_byte(ctx, MODRM(MOD_DIRECT, 0, dst));
        emit_byte(ctx, (uint8_t)imm);
    } else {
        emit_byte(ctx, 0x81);
        emit_byte(ctx, MODRM(MOD_DIRECT, 0, dst));
        emit_dword(ctx, (uint32_t)imm);
    }
}

// sub r32, r32  (29 /r)
static inline void emit_sub_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x29);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
}

// sub r32, imm32  (81 /5 id or 83 /5 ib)
static inline void emit_sub_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);

    if (imm >= -128 && imm <= 127) {
        emit_byte(ctx, 0x83);
        emit_byte(ctx, MODRM(MOD_DIRECT, 5, dst));
        emit_byte(ctx, (uint8_t)imm);
    } else {
        emit_byte(ctx, 0x81);
        emit_byte(ctx, MODRM(MOD_DIRECT, 5, dst));
        emit_dword(ctx, (uint32_t)imm);
    }
}

// imul r32, r32  (0F AF /r)
static inline void emit_imul_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_regs(dst, src, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xAF);
    emit_byte(ctx, MODRM(MOD_DIRECT, dst, src));
}

// mul r32  (F7 /4) - edx:eax = eax * r32
static inline void emit_mul_r32(emit_ctx_t *ctx, x64_reg_t src) {
    CHECK_RAX_WRITE(ctx, RAX);
    uint8_t rex = 0;
    if (src >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xF7);
    emit_byte(ctx, MODRM(MOD_DIRECT, 4, src));
}

// imul r32  (F7 /5) - signed edx:eax = eax * r32
static inline void emit_imul_one_r32(emit_ctx_t *ctx, x64_reg_t src) {
    CHECK_RAX_WRITE(ctx, RAX);
    uint8_t rex = 0;
    if (src >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xF7);
    emit_byte(ctx, MODRM(MOD_DIRECT, 5, src));
}

// idiv r32  (F7 /7)
static inline void emit_idiv_r32(emit_ctx_t *ctx, x64_reg_t src) {
    CHECK_RAX_WRITE(ctx, RAX);
    uint8_t rex = 0;
    if (src >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xF7);
    emit_byte(ctx, MODRM(MOD_DIRECT, 7, src));
}

// div r32  (F7 /6)
static inline void emit_div_r32(emit_ctx_t *ctx, x64_reg_t src) {
    CHECK_RAX_WRITE(ctx, RAX);
    uint8_t rex = 0;
    if (src >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xF7);
    emit_byte(ctx, MODRM(MOD_DIRECT, 6, src));
}

// cdq  (99)
static inline void emit_cdq(emit_ctx_t *ctx) {
    emit_byte(ctx, 0x99);
}

// xor r32, r32  (31 /r)
static inline void emit_xor_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x31);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
}

// and r32, r32  (21 /r)
static inline void emit_and_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x21);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
}

// and r32, imm32  (81 /4 id or 83 /4 ib)
static inline void emit_and_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);

    if (imm >= -128 && imm <= 127) {
        emit_byte(ctx, 0x83);
        emit_byte(ctx, MODRM(MOD_DIRECT, 4, dst));
        emit_byte(ctx, (uint8_t)imm);
    } else {
        emit_byte(ctx, 0x81);
        emit_byte(ctx, MODRM(MOD_DIRECT, 4, dst));
        emit_dword(ctx, (uint32_t)imm);
    }
}

// or r32, r32  (09 /r)
static inline void emit_or_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x09);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
}

// or r32, imm32  (81 /1 id or 83 /1 ib)
static inline void emit_or_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);

    if (imm >= -128 && imm <= 127) {
        emit_byte(ctx, 0x83);
        emit_byte(ctx, MODRM(MOD_DIRECT, 1, dst));
        emit_byte(ctx, (uint8_t)imm);
    } else {
        emit_byte(ctx, 0x81);
        emit_byte(ctx, MODRM(MOD_DIRECT, 1, dst));
        emit_dword(ctx, (uint32_t)imm);
    }
}

// xor r32, imm32  (81 /6 id or 83 /6 ib)
static inline void emit_xor_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);

    if (imm >= -128 && imm <= 127) {
        emit_byte(ctx, 0x83);
        emit_byte(ctx, MODRM(MOD_DIRECT, 6, dst));
        emit_byte(ctx, (uint8_t)imm);
    } else {
        emit_byte(ctx, 0x81);
        emit_byte(ctx, MODRM(MOD_DIRECT, 6, dst));
        emit_dword(ctx, (uint32_t)imm);
    }
}

// neg r32  (F7 /3)
static inline void emit_neg_r32(emit_ctx_t *ctx, x64_reg_t dst) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xF7);
    emit_byte(ctx, MODRM(MOD_DIRECT, 3, dst));
}

// not r32  (F7 /2)
static inline void emit_not_r32(emit_ctx_t *ctx, x64_reg_t dst) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xF7);
    emit_byte(ctx, MODRM(MOD_DIRECT, 2, dst));
}

// ============================================================================
// Shifts
// ============================================================================

// shl r32, cl  (D3 /4)
static inline void emit_shl_r32_cl(emit_ctx_t *ctx, x64_reg_t dst) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xD3);
    emit_byte(ctx, MODRM(MOD_DIRECT, 4, dst));
}

// shl r32, imm8  (C1 /4 ib)
static inline void emit_shl_r32_imm8(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm) {
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xC1);
    emit_byte(ctx, MODRM(MOD_DIRECT, 4, dst));
    emit_byte(ctx, imm & 0x1F);
}

// shr r32, cl  (D3 /5)
static inline void emit_shr_r32_cl(emit_ctx_t *ctx, x64_reg_t dst) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xD3);
    emit_byte(ctx, MODRM(MOD_DIRECT, 5, dst));
}

// shr r32, imm8  (C1 /5 ib)
static inline void emit_shr_r32_imm8(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm) {
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xC1);
    emit_byte(ctx, MODRM(MOD_DIRECT, 5, dst));
    emit_byte(ctx, imm & 0x1F);
}

// sar r32, cl  (D3 /7)
static inline void emit_sar_r32_cl(emit_ctx_t *ctx, x64_reg_t dst) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xD3);
    emit_byte(ctx, MODRM(MOD_DIRECT, 7, dst));
}

// sar r32, imm8  (C1 /7 ib)
static inline void emit_sar_r32_imm8(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm) {
    CHECK_RAX_WRITE(ctx, dst);
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xC1);
    emit_byte(ctx, MODRM(MOD_DIRECT, 7, dst));
    emit_byte(ctx, imm & 0x1F);
}

// ============================================================================
// Comparisons
// ============================================================================

// cmp r32, r32  (39 /r)
static inline void emit_cmp_r32_r32(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b) {
    uint8_t rex = rex_for_regs(b, a, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x39);
    emit_byte(ctx, MODRM(MOD_DIRECT, b, a));
}

// cmp r32, imm32  (81 /7 id or 83 /7 ib)
static inline void emit_cmp_r32_imm32(emit_ctx_t *ctx, x64_reg_t a, int32_t imm) {
    uint8_t rex = 0;
    if (a >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);

    if (imm >= -128 && imm <= 127) {
        emit_byte(ctx, 0x83);
        emit_byte(ctx, MODRM(MOD_DIRECT, 7, a));
        emit_byte(ctx, (uint8_t)imm);
    } else {
        emit_byte(ctx, 0x81);
        emit_byte(ctx, MODRM(MOD_DIRECT, 7, a));
        emit_dword(ctx, (uint32_t)imm);
    }
}

// test r32, r32  (85 /r)
static inline void emit_test_r32_r32(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b) {
    uint8_t rex = rex_for_regs(b, a, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x85);
    emit_byte(ctx, MODRM(MOD_DIRECT, b, a));
}

// Generic setcc helper
static inline void emit_setcc(emit_ctx_t *ctx, uint8_t cc, x64_reg_t dst) {
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    if (needs_rex_for_byte_reg(dst)) rex |= 0;
    if (rex || needs_rex_for_byte_reg(dst)) emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, cc);
    emit_byte(ctx, MODRM(MOD_DIRECT, 0, dst));
}

static inline void emit_sete(emit_ctx_t *ctx, x64_reg_t dst)  { emit_setcc(ctx, 0x94, dst); }
static inline void emit_setne(emit_ctx_t *ctx, x64_reg_t dst) { emit_setcc(ctx, 0x95, dst); }
static inline void emit_setl(emit_ctx_t *ctx, x64_reg_t dst)  { emit_setcc(ctx, 0x9C, dst); }
static inline void emit_setge(emit_ctx_t *ctx, x64_reg_t dst) { emit_setcc(ctx, 0x9D, dst); }
static inline void emit_setg(emit_ctx_t *ctx, x64_reg_t dst)  { emit_setcc(ctx, 0x9F, dst); }
static inline void emit_setle(emit_ctx_t *ctx, x64_reg_t dst) { emit_setcc(ctx, 0x9E, dst); }
static inline void emit_setb(emit_ctx_t *ctx, x64_reg_t dst)  { emit_setcc(ctx, 0x92, dst); }
static inline void emit_setae(emit_ctx_t *ctx, x64_reg_t dst) { emit_setcc(ctx, 0x93, dst); }
static inline void emit_seta(emit_ctx_t *ctx, x64_reg_t dst)  { emit_setcc(ctx, 0x97, dst); }
static inline void emit_setbe(emit_ctx_t *ctx, x64_reg_t dst) { emit_setcc(ctx, 0x96, dst); }

// ============================================================================
// Control flow
// ============================================================================

// jmp rel32  (E9 cd)
static inline void emit_jmp_rel32(emit_ctx_t *ctx, int32_t offset) {
    emit_byte(ctx, 0xE9);
    emit_dword(ctx, (uint32_t)offset);
}

// jmp r64  (FF /4)
static inline void emit_jmp_r64(emit_ctx_t *ctx, x64_reg_t target) {
    uint8_t rex = 0;
    if (target >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xFF);
    emit_byte(ctx, MODRM(MOD_DIRECT, 4, target));
}

// Generic jcc rel32 helper
static inline void emit_jcc_rel32_generic(emit_ctx_t *ctx, uint8_t cc, int32_t offset) {
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, cc);
    emit_dword(ctx, (uint32_t)offset);
}

static inline void emit_je_rel32(emit_ctx_t *ctx, int32_t offset)  { emit_jcc_rel32_generic(ctx, 0x84, offset); }
static inline void emit_jne_rel32(emit_ctx_t *ctx, int32_t offset) { emit_jcc_rel32_generic(ctx, 0x85, offset); }
static inline void emit_jl_rel32(emit_ctx_t *ctx, int32_t offset)  { emit_jcc_rel32_generic(ctx, 0x8C, offset); }
static inline void emit_jge_rel32(emit_ctx_t *ctx, int32_t offset) { emit_jcc_rel32_generic(ctx, 0x8D, offset); }
static inline void emit_jg_rel32(emit_ctx_t *ctx, int32_t offset)  { emit_jcc_rel32_generic(ctx, 0x8F, offset); }
static inline void emit_jle_rel32(emit_ctx_t *ctx, int32_t offset) { emit_jcc_rel32_generic(ctx, 0x8E, offset); }
static inline void emit_jb_rel32(emit_ctx_t *ctx, int32_t offset)  { emit_jcc_rel32_generic(ctx, 0x82, offset); }
static inline void emit_jae_rel32(emit_ctx_t *ctx, int32_t offset) { emit_jcc_rel32_generic(ctx, 0x83, offset); }
static inline void emit_ja_rel32(emit_ctx_t *ctx, int32_t offset)  { emit_jcc_rel32_generic(ctx, 0x87, offset); }
static inline void emit_jbe_rel32(emit_ctx_t *ctx, int32_t offset) { emit_jcc_rel32_generic(ctx, 0x86, offset); }

// Helper to get jcc function pointer from condition code (0x80-0x8F)
static inline void *emit_jcc_rel32_from_cc(uint8_t cc) {
    switch (cc) {
        case 0x84: return (void *)emit_je_rel32;
        case 0x85: return (void *)emit_jne_rel32;
        case 0x8C: return (void *)emit_jl_rel32;
        case 0x8D: return (void *)emit_jge_rel32;
        case 0x8F: return (void *)emit_jg_rel32;
        case 0x8E: return (void *)emit_jle_rel32;
        case 0x82: return (void *)emit_jb_rel32;
        case 0x83: return (void *)emit_jae_rel32;
        case 0x87: return (void *)emit_ja_rel32;
        case 0x86: return (void *)emit_jbe_rel32;
        default: return NULL;
    }
}

// Short conditional jump: 7x rel8 (2 bytes total)
static inline void emit_jcc_short(emit_ctx_t *ctx, uint8_t cc, int8_t rel8) {
    emit_byte(ctx, 0x70 | (cc & 0x0F));
    emit_byte(ctx, (uint8_t)rel8);
}

// call rel32  (E8 cd)
static inline void emit_call_rel32(emit_ctx_t *ctx, int32_t offset) {
    emit_byte(ctx, 0xE8);
    emit_dword(ctx, (uint32_t)offset);
}

// call r64  (FF /2)
static inline void emit_call_r64(emit_ctx_t *ctx, x64_reg_t target) {
    uint8_t rex = 0;
    if (target >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xFF);
    emit_byte(ctx, MODRM(MOD_DIRECT, 2, target));
}

// ret  (C3)
static inline void emit_ret(emit_ctx_t *ctx) {
    emit_byte(ctx, 0xC3);
}

// ============================================================================
// Stack operations
// ============================================================================

// push r64  (50+rd)
static inline void emit_push_r64(emit_ctx_t *ctx, x64_reg_t reg) {
    if (reg >= R8) emit_byte(ctx, REX_BASE | REX_B);
    emit_byte(ctx, 0x50 + (reg & 7));
}

// pop r64  (58+rd)
static inline void emit_pop_r64(emit_ctx_t *ctx, x64_reg_t reg) {
    CHECK_RAX_WRITE(ctx, reg);
    if (reg >= R8) emit_byte(ctx, REX_BASE | REX_B);
    emit_byte(ctx, 0x58 + (reg & 7));
}

// ============================================================================
// Misc
// ============================================================================

// nop  (90)
static inline void emit_nop(emit_ctx_t *ctx) {
    emit_byte(ctx, 0x90);
}

// int3  (CC)
static inline void emit_int3(emit_ctx_t *ctx) {
    emit_byte(ctx, 0xCC);
}

// ============================================================================
// SSE / FP helpers (f32 inline translation)
// ============================================================================

// Helper: emit F3 0F xx modrm for SSE scalar single instructions (xmm, xmm)
static inline void emit_sse_ss_rr(emit_ctx_t *ctx, uint8_t op2, uint8_t dst_xmm, uint8_t src_xmm) {
    emit_byte(ctx, 0xF3);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, op2);
    emit_byte(ctx, MODRM(MOD_DIRECT, dst_xmm & 7, src_xmm & 7));
}

// movd xmm, r32  (66 [REX] 0F 6E /r)
static inline void emit_movd_xmm_r32(emit_ctx_t *ctx, uint8_t xmm, x64_reg_t r32) {
    emit_byte(ctx, 0x66);
    if (r32 >= R8) emit_byte(ctx, REX_BASE | REX_B);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0x6E);
    emit_byte(ctx, MODRM(MOD_DIRECT, xmm & 7, r32 & 7));
}

// movd r32, xmm  (66 [REX] 0F 7E /r)
static inline void emit_movd_r32_xmm(emit_ctx_t *ctx, x64_reg_t r32, uint8_t xmm) {
    emit_byte(ctx, 0x66);
    if (r32 >= R8) emit_byte(ctx, REX_BASE | REX_B);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0x7E);
    emit_byte(ctx, MODRM(MOD_DIRECT, xmm & 7, r32 & 7));
}

static inline void emit_addss(emit_ctx_t *ctx, uint8_t dst_xmm, uint8_t src_xmm)  { emit_sse_ss_rr(ctx, 0x58, dst_xmm, src_xmm); }
static inline void emit_subss(emit_ctx_t *ctx, uint8_t dst_xmm, uint8_t src_xmm)  { emit_sse_ss_rr(ctx, 0x5C, dst_xmm, src_xmm); }
static inline void emit_mulss(emit_ctx_t *ctx, uint8_t dst_xmm, uint8_t src_xmm)  { emit_sse_ss_rr(ctx, 0x59, dst_xmm, src_xmm); }
static inline void emit_divss(emit_ctx_t *ctx, uint8_t dst_xmm, uint8_t src_xmm)  { emit_sse_ss_rr(ctx, 0x5E, dst_xmm, src_xmm); }
static inline void emit_sqrtss(emit_ctx_t *ctx, uint8_t dst_xmm, uint8_t src_xmm) { emit_sse_ss_rr(ctx, 0x51, dst_xmm, src_xmm); }

// ucomiss xmm, xmm  (0F 2E /r)
static inline void emit_ucomiss(emit_ctx_t *ctx, uint8_t xmm1, uint8_t xmm2) {
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0x2E);
    emit_byte(ctx, MODRM(MOD_DIRECT, xmm1 & 7, xmm2 & 7));
}

// cvttss2si r32, xmm  (F3 [REX] 0F 2C /r)
static inline void emit_cvttss2si_r32_xmm(emit_ctx_t *ctx, x64_reg_t r32, uint8_t xmm) {
    emit_byte(ctx, 0xF3);
    if (r32 >= R8) emit_byte(ctx, REX_BASE | REX_R);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0x2C);
    emit_byte(ctx, MODRM(MOD_DIRECT, r32 & 7, xmm & 7));
}

// cvtsi2ss xmm, r32  (F3 [REX] 0F 2A /r)
static inline void emit_cvtsi2ss_xmm_r32(emit_ctx_t *ctx, uint8_t xmm, x64_reg_t r32) {
    emit_byte(ctx, 0xF3);
    if (r32 >= R8) emit_byte(ctx, REX_BASE | REX_B);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0x2A);
    emit_byte(ctx, MODRM(MOD_DIRECT, xmm & 7, r32 & 7));
}

// cvtsi2ss xmm, r64  (F3 REX.W 0F 2A /r) — 64-bit source
static inline void emit_cvtsi2ss_xmm_r64(emit_ctx_t *ctx, uint8_t xmm, x64_reg_t r64) {
    emit_byte(ctx, 0xF3);
    uint8_t rex = REX_W;
    if (r64 >= R8) rex |= REX_B;
    emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0x2A);
    emit_byte(ctx, MODRM(MOD_DIRECT, xmm & 7, r64 & 7));
}

// setnp (0F 9B /0)
static inline void emit_setnp(emit_ctx_t *ctx, x64_reg_t dst) { emit_setcc(ctx, 0x9B, dst); }

// setp (0F 9A /0)
static inline void emit_setp(emit_ctx_t *ctx, x64_reg_t dst)  { emit_setcc(ctx, 0x9A, dst); }

// ============================================================================
// Patching helpers
// ============================================================================

static inline void emit_patch_rel32(emit_ctx_t *ctx, size_t patch_offset, size_t target_offset) {
    int32_t rel = (int32_t)(target_offset - (patch_offset + 4));
    memcpy(ctx->buf + patch_offset, &rel, 4);
}

#endif // DBT_EMIT_X64_H
