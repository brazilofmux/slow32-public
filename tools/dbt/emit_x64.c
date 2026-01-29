// SLOW-32 DBT: x86-64 Code Emitter Implementation
// Stage 1 - Correctness-first, readable code over micro-optimization

#include "emit_x64.h"
#include <string.h>
#include <stdio.h>

// ============================================================================
// Core emitter functions
// ============================================================================

void emit_init(emit_ctx_t *ctx, uint8_t *buf, size_t capacity) {
    ctx->buf = buf;
    ctx->capacity = capacity;
    ctx->offset = 0;
    ctx->overflow = false;
    ctx->trace_enabled = false;
    ctx->trace_tag = NULL;
}

void emit_byte(emit_ctx_t *ctx, uint8_t b) {
    if (ctx->offset >= ctx->capacity) {
        ctx->overflow = true;
        return;
    }
    ctx->buf[ctx->offset++] = b;
}

void emit_word(emit_ctx_t *ctx, uint16_t w) {
    emit_byte(ctx, w & 0xFF);
    emit_byte(ctx, (w >> 8) & 0xFF);
}

void emit_dword(emit_ctx_t *ctx, uint32_t d) {
    emit_byte(ctx, d & 0xFF);
    emit_byte(ctx, (d >> 8) & 0xFF);
    emit_byte(ctx, (d >> 16) & 0xFF);
    emit_byte(ctx, (d >> 24) & 0xFF);
}

void emit_qword(emit_ctx_t *ctx, uint64_t q) {
    emit_dword(ctx, q & 0xFFFFFFFF);
    emit_dword(ctx, (q >> 32) & 0xFFFFFFFF);
}

// ============================================================================
// REX prefix helpers
// ============================================================================

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
    if (rex) emit_byte(ctx, REX_BASE | rex);
}

static void emit_trace_inst(emit_ctx_t *ctx, const char *op, size_t start_off, size_t end_off) {
    if (!ctx->trace_enabled) {
        return;
    }
    const char *tag = ctx->trace_tag ? ctx->trace_tag : "none";
    fprintf(stderr, "[emit] tag=%s op=%s off=0x%zx bytes=", tag, op, start_off);
    for (size_t i = start_off; i < end_off; i++) {
        fprintf(stderr, "%02X", ctx->buf[i]);
    }
    fprintf(stderr, "\n");
}

// Some instructions need REX for low byte access to SPL, BPL, SIL, DIL
static inline bool needs_rex_for_byte_reg(x64_reg_t r) {
    return r >= RSP && r <= RDI;
}

// ============================================================================
// Data movement
// ============================================================================

// mov r32, r32  (89 /r)
void emit_mov_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    size_t start_off = ctx->offset;
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x89);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
    emit_trace_inst(ctx, "mov_r32_r32", start_off, ctx->offset);
}

// mov r64, r64  (REX.W 89 /r)
void emit_mov_r64_r64(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    uint8_t rex = rex_for_regs(src, dst, true);
    emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x89);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
}

// mov r32, imm32  (B8+rd id)
void emit_mov_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, uint32_t imm) {
    size_t start_off = ctx->offset;
    if (dst >= R8) emit_byte(ctx, REX_BASE | REX_B);
    emit_byte(ctx, 0xB8 + (dst & 7));
    emit_dword(ctx, imm);
    emit_trace_inst(ctx, "mov_r32_imm32", start_off, ctx->offset);
}

// mov r64, imm64  (REX.W B8+rd io)
void emit_mov_r64_imm64(emit_ctx_t *ctx, x64_reg_t dst, uint64_t imm) {
    uint8_t rex = REX_W;
    if (dst >= R8) rex |= REX_B;
    emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0xB8 + (dst & 7));
    emit_qword(ctx, imm);
}

// mov r32, [base + disp]  (8B /r) - uses disp8 when possible
void emit_mov_r32_m32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp) {
    size_t start_off = ctx->offset;
    uint8_t rex = rex_for_regs(dst, base, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x8B);

    bool use_disp8 = (disp >= -128 && disp <= 127);
    uint8_t mod = use_disp8 ? MOD_DISP8 : MOD_DISP32;

    // RSP/R12 needs SIB byte
    if ((base & 7) == RSP) {
        emit_byte(ctx, MODRM(mod, dst, RSP));
        emit_byte(ctx, SIB(0, RSP, RSP));  // scale=0, index=none, base=rsp
    } else {
        emit_byte(ctx, MODRM(mod, dst, base));
    }
    if (use_disp8) {
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    } else {
        emit_dword(ctx, (uint32_t)disp);
    }
    emit_trace_inst(ctx, "mov_r32_m32", start_off, ctx->offset);
}

// movzx r32, byte [base + disp]  (0F B6 /r) - uses disp8 when possible
void emit_movzx_r32_m8(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp) {
    size_t start_off = ctx->offset;
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
    if (use_disp8) {
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    } else {
        emit_dword(ctx, (uint32_t)disp);
    }
    emit_trace_inst(ctx, "movzx_r32_m8", start_off, ctx->offset);
}

// mov [base + disp], r32  (89 /r) - uses disp8 when possible
void emit_mov_m32_r32(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, x64_reg_t src) {
    size_t start_off = ctx->offset;
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
    if (use_disp8) {
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    } else {
        emit_dword(ctx, (uint32_t)disp);
    }
    emit_trace_inst(ctx, "mov_m32_r32", start_off, ctx->offset);
}

// mov [base + disp], imm32  (C7 /0 id) - uses disp8 when possible
void emit_mov_m32_imm32(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, uint32_t imm) {
    size_t start_off = ctx->offset;
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
    if (use_disp8) {
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    } else {
        emit_dword(ctx, (uint32_t)disp);
    }
    emit_dword(ctx, imm);
    emit_trace_inst(ctx, "mov_m32_imm32", start_off, ctx->offset);
}

// mov r32, [base + index*1]  (8B /r with SIB)
void emit_mov_r32_m32_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index) {
    uint8_t rex = rex_for_sib(dst, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x8B);
    emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));  // rm=100 means SIB follows
    emit_byte(ctx, SIB(0, index, base));  // scale=1 (2^0)
}

// mov [base + index*1], r32  (89 /r with SIB)
void emit_mov_m32_r32_idx(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index, x64_reg_t src) {
    uint8_t rex = rex_for_sib(src, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x89);
    emit_byte(ctx, MODRM(MOD_INDIRECT, src, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// movsx r32, byte [base + index]  (0F BE /r with SIB)
void emit_movsx_r32_m8_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index) {
    uint8_t rex = rex_for_sib(dst, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xBE);
    emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// movzx r32, byte [base + index]  (0F B6 /r with SIB)
void emit_movzx_r32_m8_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index) {
    uint8_t rex = rex_for_sib(dst, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xB6);
    emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// movsx r32, word [base + index]  (0F BF /r with SIB)
void emit_movsx_r32_m16_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index) {
    uint8_t rex = rex_for_sib(dst, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xBF);
    emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// movzx r32, word [base + index]  (0F B7 /r with SIB)
void emit_movzx_r32_m16_idx(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, x64_reg_t index) {
    uint8_t rex = rex_for_sib(dst, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xB7);
    emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// mov byte [base + index], r8  (88 /r with SIB)
void emit_mov_m8_r8_idx(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index, x64_reg_t src) {
    uint8_t rex = rex_for_sib(src, base, index, false);
    // Need REX to access SPL/BPL/SIL/DIL
    if (needs_rex_for_byte_reg(src)) rex |= 0;  // Just ensure REX exists
    if (rex || needs_rex_for_byte_reg(src)) emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x88);
    emit_byte(ctx, MODRM(MOD_INDIRECT, src, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// mov word [base + index], r16  (66 89 /r with SIB)
void emit_mov_m16_r16_idx(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index, x64_reg_t src) {
    emit_byte(ctx, 0x66);  // Operand size prefix
    uint8_t rex = rex_for_sib(src, base, index, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x89);
    emit_byte(ctx, MODRM(MOD_INDIRECT, src, RSP));
    emit_byte(ctx, SIB(0, index, base));
}

// movzx r32, r8  (0F B6 /r)
void emit_movzx_r32_r8(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    uint8_t rex = rex_for_regs(dst, src, false);
    if (needs_rex_for_byte_reg(src)) rex |= 0;  // Force REX if needed
    if (rex || needs_rex_for_byte_reg(src)) emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xB6);
    emit_byte(ctx, MODRM(MOD_DIRECT, dst, src));
}

// ============================================================================
// 64-bit memory operations (Stage 3)
// ============================================================================

// mov r64, [base + disp]  (REX.W 8B /r) - uses disp8 when possible
void emit_mov_r64_m64(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp) {
    uint8_t rex = rex_for_regs(dst, base, true);  // REX.W for 64-bit
    emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x8B);

    bool use_disp8 = (disp >= -128 && disp <= 127);
    uint8_t mod = use_disp8 ? MOD_DISP8 : MOD_DISP32;

    // RSP/R12 needs SIB byte
    if ((base & 7) == RSP) {
        emit_byte(ctx, MODRM(mod, dst, RSP));
        emit_byte(ctx, SIB(0, RSP, RSP));
    } else {
        emit_byte(ctx, MODRM(mod, dst, base));
    }
    if (use_disp8) {
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    } else {
        emit_dword(ctx, (uint32_t)disp);
    }
}

// mov r64, [base + index*scale + disp32]  (REX.W 8B /r with SIB)
// scale_log2: 0=*1, 1=*2, 2=*4, 3=*8
void emit_mov_r64_m64_sib(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base,
                          x64_reg_t index, uint8_t scale, int32_t disp) {
    // Convert scale to log2: 1->0, 2->1, 4->2, 8->3
    uint8_t scale_bits;
    switch (scale) {
        case 1: scale_bits = 0; break;
        case 2: scale_bits = 1; break;
        case 4: scale_bits = 2; break;
        case 8: scale_bits = 3; break;
        default: scale_bits = 0; break;
    }

    uint8_t rex = rex_for_sib(dst, base, index, true);  // REX.W for 64-bit
    emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x8B);

    if (disp == 0 && (base & 7) != RBP) {
        // No displacement needed (except for RBP which requires disp8)
        emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));  // rm=100 means SIB
        emit_byte(ctx, SIB(scale_bits, index, base));
    } else if (disp >= -128 && disp <= 127) {
        emit_byte(ctx, MODRM(MOD_DISP8, dst, RSP));
        emit_byte(ctx, SIB(scale_bits, index, base));
        emit_byte(ctx, (uint8_t)disp);
    } else {
        emit_byte(ctx, MODRM(MOD_DISP32, dst, RSP));
        emit_byte(ctx, SIB(scale_bits, index, base));
        emit_dword(ctx, (uint32_t)disp);
    }
}

// cmp [base + disp], r32  (39 /r) - uses disp8 when possible
void emit_cmp_m32_r32(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, x64_reg_t src) {
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
    if (use_disp8) {
        emit_byte(ctx, (uint8_t)(int8_t)disp);
    } else {
        emit_dword(ctx, (uint32_t)disp);
    }
}

// test r64, r64  (REX.W 85 /r)
void emit_test_r64_r64(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b) {
    uint8_t rex = rex_for_regs(b, a, true);  // REX.W for 64-bit
    emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x85);
    emit_byte(ctx, MODRM(MOD_DIRECT, b, a));
}

// ============================================================================
// Stage 3 Phase 2: RAS support
// ============================================================================

// mov r32, [base + index*scale + disp32]  (8B /r with SIB)
void emit_mov_r32_m32_sib(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base,
                          x64_reg_t index, uint8_t scale, int32_t disp) {
    // Convert scale to log2: 1->0, 2->1, 4->2, 8->3
    uint8_t scale_bits;
    switch (scale) {
        case 1: scale_bits = 0; break;
        case 2: scale_bits = 1; break;
        case 4: scale_bits = 2; break;
        case 8: scale_bits = 3; break;
        default: scale_bits = 0; break;
    }

    uint8_t rex = rex_for_sib(dst, base, index, false);  // 32-bit
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x8B);

    if (disp == 0 && (base & 7) != RBP) {
        emit_byte(ctx, MODRM(MOD_INDIRECT, dst, RSP));
        emit_byte(ctx, SIB(scale_bits, index, base));
    } else if (disp >= -128 && disp <= 127) {
        emit_byte(ctx, MODRM(MOD_DISP8, dst, RSP));
        emit_byte(ctx, SIB(scale_bits, index, base));
        emit_byte(ctx, (uint8_t)disp);
    } else {
        emit_byte(ctx, MODRM(MOD_DISP32, dst, RSP));
        emit_byte(ctx, SIB(scale_bits, index, base));
        emit_dword(ctx, (uint32_t)disp);
    }
}

// mov [base + index*scale + disp32], r32  (89 /r with SIB)
void emit_mov_m32_r32_sib(emit_ctx_t *ctx, x64_reg_t base, x64_reg_t index,
                          uint8_t scale, int32_t disp, x64_reg_t src) {
    uint8_t scale_bits;
    switch (scale) {
        case 1: scale_bits = 0; break;
        case 2: scale_bits = 1; break;
        case 4: scale_bits = 2; break;
        case 8: scale_bits = 3; break;
        default: scale_bits = 0; break;
    }

    uint8_t rex = rex_for_sib(src, base, index, false);  // 32-bit
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x89);

    if (disp == 0 && (base & 7) != RBP) {
        emit_byte(ctx, MODRM(MOD_INDIRECT, src, RSP));
        emit_byte(ctx, SIB(scale_bits, index, base));
    } else if (disp >= -128 && disp <= 127) {
        emit_byte(ctx, MODRM(MOD_DISP8, src, RSP));
        emit_byte(ctx, SIB(scale_bits, index, base));
        emit_byte(ctx, (uint8_t)disp);
    } else {
        emit_byte(ctx, MODRM(MOD_DISP32, src, RSP));
        emit_byte(ctx, SIB(scale_bits, index, base));
        emit_dword(ctx, (uint32_t)disp);
    }
}

// ============================================================================
// Arithmetic
// ============================================================================

// add r32, r32  (01 /r)
void emit_add_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    size_t start_off = ctx->offset;
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x01);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
    emit_trace_inst(ctx, "add_r32_r32", start_off, ctx->offset);
}

// add r32, imm32  (81 /0 id or 83 /0 ib for small imm)
void emit_add_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm) {
    size_t start_off = ctx->offset;
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
    emit_trace_inst(ctx, "add_r32_imm32", start_off, ctx->offset);
}

// sub r32, r32  (29 /r)
void emit_sub_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x29);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
}

// sub r32, imm32  (81 /5 id or 83 /5 ib)
void emit_sub_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm) {
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
void emit_imul_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    uint8_t rex = rex_for_regs(dst, src, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, 0xAF);
    emit_byte(ctx, MODRM(MOD_DIRECT, dst, src));
}

// mul r32  (F7 /4) - edx:eax = eax * r32
void emit_mul_r32(emit_ctx_t *ctx, x64_reg_t src) {
    uint8_t rex = 0;
    if (src >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xF7);
    emit_byte(ctx, MODRM(MOD_DIRECT, 4, src));
}

// idiv r32  (F7 /7) - eax = edx:eax / r32, edx = remainder
void emit_idiv_r32(emit_ctx_t *ctx, x64_reg_t src) {
    uint8_t rex = 0;
    if (src >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xF7);
    emit_byte(ctx, MODRM(MOD_DIRECT, 7, src));
}

// div r32  (F7 /6)
void emit_div_r32(emit_ctx_t *ctx, x64_reg_t src) {
    uint8_t rex = 0;
    if (src >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xF7);
    emit_byte(ctx, MODRM(MOD_DIRECT, 6, src));
}

// cdq  (99) - sign-extend eax into edx:eax
void emit_cdq(emit_ctx_t *ctx) {
    emit_byte(ctx, 0x99);
}

// xor r32, r32  (31 /r)
void emit_xor_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    size_t start_off = ctx->offset;
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x31);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
    emit_trace_inst(ctx, "xor_r32_r32", start_off, ctx->offset);
}

// and r32, r32  (21 /r)
void emit_and_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x21);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
}

// and r32, imm32  (81 /4 id or 83 /4 ib)
void emit_and_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm) {
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
void emit_or_r32_r32(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t src) {
    size_t start_off = ctx->offset;
    uint8_t rex = rex_for_regs(src, dst, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x09);
    emit_byte(ctx, MODRM(MOD_DIRECT, src, dst));
    emit_trace_inst(ctx, "or_r32_r32", start_off, ctx->offset);
}

// or r32, imm32  (81 /1 id or 83 /1 ib)
void emit_or_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm) {
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
void emit_xor_r32_imm32(emit_ctx_t *ctx, x64_reg_t dst, int32_t imm) {
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
void emit_neg_r32(emit_ctx_t *ctx, x64_reg_t dst) {
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xF7);
    emit_byte(ctx, MODRM(MOD_DIRECT, 3, dst));
}

// not r32  (F7 /2)
void emit_not_r32(emit_ctx_t *ctx, x64_reg_t dst) {
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
void emit_shl_r32_cl(emit_ctx_t *ctx, x64_reg_t dst) {
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xD3);
    emit_byte(ctx, MODRM(MOD_DIRECT, 4, dst));
}

// shl r32, imm8  (C1 /4 ib)
void emit_shl_r32_imm8(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm) {
    size_t start_off = ctx->offset;
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xC1);
    emit_byte(ctx, MODRM(MOD_DIRECT, 4, dst));
    emit_byte(ctx, imm & 0x1F);
    emit_trace_inst(ctx, "shl_r32_imm8", start_off, ctx->offset);
}

// shr r32, cl  (D3 /5)
void emit_shr_r32_cl(emit_ctx_t *ctx, x64_reg_t dst) {
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xD3);
    emit_byte(ctx, MODRM(MOD_DIRECT, 5, dst));
}

// shr r32, imm8  (C1 /5 ib)
void emit_shr_r32_imm8(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm) {
    size_t start_off = ctx->offset;
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xC1);
    emit_byte(ctx, MODRM(MOD_DIRECT, 5, dst));
    emit_byte(ctx, imm & 0x1F);
    emit_trace_inst(ctx, "shr_r32_imm8", start_off, ctx->offset);
}

// sar r32, cl  (D3 /7)
void emit_sar_r32_cl(emit_ctx_t *ctx, x64_reg_t dst) {
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xD3);
    emit_byte(ctx, MODRM(MOD_DIRECT, 7, dst));
}

// sar r32, imm8  (C1 /7 ib)
void emit_sar_r32_imm8(emit_ctx_t *ctx, x64_reg_t dst, uint8_t imm) {
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
void emit_cmp_r32_r32(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b) {
    size_t start_off = ctx->offset;
    uint8_t rex = rex_for_regs(b, a, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x39);
    emit_byte(ctx, MODRM(MOD_DIRECT, b, a));
    emit_trace_inst(ctx, "cmp_r32_r32", start_off, ctx->offset);
}

// cmp r32, imm32  (81 /7 id or 83 /7 ib)
void emit_cmp_r32_imm32(emit_ctx_t *ctx, x64_reg_t a, int32_t imm) {
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
void emit_test_r32_r32(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b) {
    size_t start_off = ctx->offset;
    uint8_t rex = rex_for_regs(b, a, false);
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0x85);
    emit_byte(ctx, MODRM(MOD_DIRECT, b, a));
    emit_trace_inst(ctx, "test_r32_r32", start_off, ctx->offset);
}

// Generic setcc helper
static void emit_setcc(emit_ctx_t *ctx, uint8_t cc, x64_reg_t dst) {
    // setcc uses r/m8, so need REX for accessing SPL/BPL/SIL/DIL
    uint8_t rex = 0;
    if (dst >= R8) rex |= REX_B;
    if (needs_rex_for_byte_reg(dst)) rex |= 0;  // Force REX presence
    if (rex || needs_rex_for_byte_reg(dst)) emit_byte(ctx, REX_BASE | rex);
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, cc);
    emit_byte(ctx, MODRM(MOD_DIRECT, 0, dst));
}

void emit_sete(emit_ctx_t *ctx, x64_reg_t dst)  { emit_setcc(ctx, 0x94, dst); }
void emit_setne(emit_ctx_t *ctx, x64_reg_t dst) { emit_setcc(ctx, 0x95, dst); }
void emit_setl(emit_ctx_t *ctx, x64_reg_t dst)  { emit_setcc(ctx, 0x9C, dst); }
void emit_setge(emit_ctx_t *ctx, x64_reg_t dst) { emit_setcc(ctx, 0x9D, dst); }
void emit_setg(emit_ctx_t *ctx, x64_reg_t dst)  { emit_setcc(ctx, 0x9F, dst); }
void emit_setle(emit_ctx_t *ctx, x64_reg_t dst) { emit_setcc(ctx, 0x9E, dst); }
void emit_setb(emit_ctx_t *ctx, x64_reg_t dst)  { emit_setcc(ctx, 0x92, dst); }
void emit_setae(emit_ctx_t *ctx, x64_reg_t dst) { emit_setcc(ctx, 0x93, dst); }
void emit_seta(emit_ctx_t *ctx, x64_reg_t dst)  { emit_setcc(ctx, 0x97, dst); }
void emit_setbe(emit_ctx_t *ctx, x64_reg_t dst) { emit_setcc(ctx, 0x96, dst); }

// ============================================================================
// Control flow
// ============================================================================

// jmp rel32  (E9 cd)
void emit_jmp_rel32(emit_ctx_t *ctx, int32_t offset) {
    emit_byte(ctx, 0xE9);
    emit_dword(ctx, (uint32_t)offset);
}

// jmp r64  (FF /4)
void emit_jmp_r64(emit_ctx_t *ctx, x64_reg_t target) {
    uint8_t rex = 0;
    if (target >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xFF);
    emit_byte(ctx, MODRM(MOD_DIRECT, 4, target));
}

// Generic jcc rel32 helper
static void emit_jcc_rel32(emit_ctx_t *ctx, uint8_t cc, int32_t offset) {
    emit_byte(ctx, 0x0F);
    emit_byte(ctx, cc);
    emit_dword(ctx, (uint32_t)offset);
}

void emit_je_rel32(emit_ctx_t *ctx, int32_t offset)  { emit_jcc_rel32(ctx, 0x84, offset); }
void emit_jne_rel32(emit_ctx_t *ctx, int32_t offset) { emit_jcc_rel32(ctx, 0x85, offset); }
void emit_jl_rel32(emit_ctx_t *ctx, int32_t offset)  { emit_jcc_rel32(ctx, 0x8C, offset); }
void emit_jge_rel32(emit_ctx_t *ctx, int32_t offset) { emit_jcc_rel32(ctx, 0x8D, offset); }
void emit_jg_rel32(emit_ctx_t *ctx, int32_t offset)  { emit_jcc_rel32(ctx, 0x8F, offset); }
void emit_jle_rel32(emit_ctx_t *ctx, int32_t offset) { emit_jcc_rel32(ctx, 0x8E, offset); }
void emit_jb_rel32(emit_ctx_t *ctx, int32_t offset)  { emit_jcc_rel32(ctx, 0x82, offset); }
void emit_jae_rel32(emit_ctx_t *ctx, int32_t offset) { emit_jcc_rel32(ctx, 0x83, offset); }
void emit_ja_rel32(emit_ctx_t *ctx, int32_t offset)  { emit_jcc_rel32(ctx, 0x87, offset); }
void emit_jbe_rel32(emit_ctx_t *ctx, int32_t offset) { emit_jcc_rel32(ctx, 0x86, offset); }

// Short conditional jump: 7x rel8 (2 bytes total)
// cc is the condition code (low nibble of the 0F 8x near opcode)
void emit_jcc_short(emit_ctx_t *ctx, uint8_t cc, int8_t rel8) {
    emit_byte(ctx, 0x70 | (cc & 0x0F));
    emit_byte(ctx, (uint8_t)rel8);
}

// call rel32  (E8 cd)
void emit_call_rel32(emit_ctx_t *ctx, int32_t offset) {
    emit_byte(ctx, 0xE8);
    emit_dword(ctx, (uint32_t)offset);
}

// call r64  (FF /2)
void emit_call_r64(emit_ctx_t *ctx, x64_reg_t target) {
    uint8_t rex = 0;
    if (target >= R8) rex |= REX_B;
    emit_rex_if_needed(ctx, rex);
    emit_byte(ctx, 0xFF);
    emit_byte(ctx, MODRM(MOD_DIRECT, 2, target));
}

// ret  (C3)
void emit_ret(emit_ctx_t *ctx) {
    emit_byte(ctx, 0xC3);
}

// ============================================================================
// Stack operations
// ============================================================================

// push r64  (50+rd)
void emit_push_r64(emit_ctx_t *ctx, x64_reg_t reg) {
    if (reg >= R8) emit_byte(ctx, REX_BASE | REX_B);
    emit_byte(ctx, 0x50 + (reg & 7));
}

// pop r64  (58+rd)
void emit_pop_r64(emit_ctx_t *ctx, x64_reg_t reg) {
    if (reg >= R8) emit_byte(ctx, REX_BASE | REX_B);
    emit_byte(ctx, 0x58 + (reg & 7));
}

// ============================================================================
// Misc
// ============================================================================

// nop  (90)
void emit_nop(emit_ctx_t *ctx) {
    emit_byte(ctx, 0x90);
}

// int3  (CC)
void emit_int3(emit_ctx_t *ctx) {
    emit_byte(ctx, 0xCC);
}

// ============================================================================
// Patching
// ============================================================================

void emit_patch_rel32(emit_ctx_t *ctx, size_t patch_offset, size_t target_offset) {
    // rel32 is relative to the END of the instruction (patch_offset + 4)
    int32_t rel = (int32_t)(target_offset - (patch_offset + 4));
    ctx->buf[patch_offset + 0] = rel & 0xFF;
    ctx->buf[patch_offset + 1] = (rel >> 8) & 0xFF;
    ctx->buf[patch_offset + 2] = (rel >> 16) & 0xFF;
    ctx->buf[patch_offset + 3] = (rel >> 24) & 0xFF;
}
