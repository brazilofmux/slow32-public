// SLOW-32 DBT: AArch64 Code Emitter Implementation
// Stage 1 - All AArch64 instructions are fixed 32-bit words

#include "emit_a64.h"
#include <stdio.h>
#include <string.h>

// ============================================================================
// Core emitter
// ============================================================================

void emit_init(emit_ctx_t *ctx, uint8_t *buf, size_t capacity) {
    ctx->buf = buf;
    ctx->capacity = capacity;
    ctx->offset = 0;
    ctx->overflow = false;
    ctx->rax_pending = false;
    ctx->trace_enabled = false;
    ctx->trace_tag = NULL;
}

void emit_inst(emit_ctx_t *ctx, uint32_t inst) {
    if (ctx->offset + 4 > ctx->capacity) {
        ctx->overflow = true;
        return;
    }
    // AArch64 instructions are always little-endian
    uint8_t *p = ctx->buf + ctx->offset;
    p[0] = (inst >>  0) & 0xFF;
    p[1] = (inst >>  8) & 0xFF;
    p[2] = (inst >> 16) & 0xFF;
    p[3] = (inst >> 24) & 0xFF;
    ctx->offset += 4;
}

// ============================================================================
// Logical immediate encoder
// ============================================================================

// AArch64 logical immediates use a complex N/immr/imms encoding.
// For 32-bit: N=0, element size e in {2,4,8,16,32}.
// The pattern is a run of ones rotated within the element.

bool a64_encode_logical_imm(uint32_t val, uint32_t *encoded) {
    if (val == 0 || val == 0xFFFFFFFF) {
        return false;  // Cannot encode all-zeros or all-ones
    }

    // Try each element size: 2, 4, 8, 16, 32
    for (int esz = 2; esz <= 32; esz <<= 1) {
        uint32_t mask = (esz == 32) ? 0xFFFFFFFF : ((1u << esz) - 1);
        uint32_t elem = val & mask;

        // Check that all elements are the same
        bool uniform = true;
        for (int i = esz; i < 32; i += esz) {
            if (((val >> i) & mask) != elem) {
                uniform = false;
                break;
            }
        }
        if (!uniform) continue;

        // elem is the repeating pattern within esz bits.
        // Find a rotation such that elem = rotate_right(ones_run, r)
        // where ones_run is s+1 consecutive ones.

        // Try each rotation
        for (int r = 0; r < esz; r++) {
            // Rotate elem left by r to get the canonical form
            uint32_t rotated = ((elem << r) | (elem >> (esz - r))) & mask;

            // Check if rotated is a contiguous run of ones from bit 0
            if (rotated == 0) continue;
            int ones = __builtin_ctz(~rotated);  // Count trailing ones
            uint32_t expected = (1u << ones) - 1;
            if (rotated != expected) continue;
            if (ones == esz) continue;  // All ones in element = invalid

            // Encode: N=0 for 32-bit, immr=r, imms encodes ones-1 + element size
            int s = ones - 1;
            int immr = r;
            int imms;

            switch (esz) {
                case 2:  imms = 0x3C | s; break;  // 1111xx
                case 4:  imms = 0x38 | s; break;  // 111xxx
                case 8:  imms = 0x30 | s; break;  // 11xxxx
                case 16: imms = 0x20 | s; break;  // 1xxxxx
                case 32: imms = s;         break;  // 0xxxxx
                default: continue;
            }

            // N=0 for 32-bit logical immediates
            *encoded = (0u << 12) | ((immr & 0x3F) << 6) | (imms & 0x3F);
            return true;
        }
    }
    return false;
}

// ============================================================================
// Immediate helpers
// ============================================================================

// MOVZ Wd, #imm16, LSL #shift
void emit_movz_w32(emit_ctx_t *ctx, a64_reg_t rd, uint16_t imm16, int shift) {
    // 0 10 100101 hw imm16 Rd
    uint32_t hw = (shift == 16) ? 1 : 0;
    uint32_t inst = (0x52800000) | (hw << 21) | ((uint32_t)imm16 << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// MOVK Wd, #imm16, LSL #shift
void emit_movk_w32(emit_ctx_t *ctx, a64_reg_t rd, uint16_t imm16, int shift) {
    // 0 11 100101 hw imm16 Rd
    uint32_t hw = (shift == 16) ? 1 : 0;
    uint32_t inst = (0x72800000) | (hw << 21) | ((uint32_t)imm16 << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// MOV Wd, #imm32 — uses MOVZ + optional MOVK
void emit_mov_w32_imm32(emit_ctx_t *ctx, a64_reg_t rd, uint32_t imm) {
    uint16_t lo = (uint16_t)(imm & 0xFFFF);
    uint16_t hi = (uint16_t)(imm >> 16);

    if (hi == 0) {
        // Single MOVZ suffices
        emit_movz_w32(ctx, rd, lo, 0);
    } else if (lo == 0) {
        // Single MOVZ with shift
        emit_movz_w32(ctx, rd, hi, 16);
    } else {
        // MOVZ low half + MOVK high half
        emit_movz_w32(ctx, rd, lo, 0);
        emit_movk_w32(ctx, rd, hi, 16);
    }
}

// MOVZ Xd, #imm16, LSL #shift
void emit_movz_x64(emit_ctx_t *ctx, a64_reg_t rd, uint16_t imm16, int shift) {
    // 1 10 100101 hw imm16 Rd
    uint32_t hw = shift / 16;
    uint32_t inst = (0xD2800000) | (hw << 21) | ((uint32_t)imm16 << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// MOVK Xd, #imm16, LSL #shift
void emit_movk_x64(emit_ctx_t *ctx, a64_reg_t rd, uint16_t imm16, int shift) {
    // 1 11 100101 hw imm16 Rd
    uint32_t hw = shift / 16;
    uint32_t inst = (0xF2800000) | (hw << 21) | ((uint32_t)imm16 << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// MOV Xd, #imm64
void emit_mov_x64_imm64(emit_ctx_t *ctx, a64_reg_t rd, uint64_t imm) {
    uint16_t w0 = (uint16_t)(imm & 0xFFFF);
    uint16_t w1 = (uint16_t)((imm >> 16) & 0xFFFF);
    uint16_t w2 = (uint16_t)((imm >> 32) & 0xFFFF);
    uint16_t w3 = (uint16_t)((imm >> 48) & 0xFFFF);

    // Find first non-zero halfword for MOVZ
    bool first = true;
    if (w0 != 0 || (w1 == 0 && w2 == 0 && w3 == 0)) {
        emit_movz_x64(ctx, rd, w0, 0);
        first = false;
    }
    if (w1 != 0) {
        if (first) { emit_movz_x64(ctx, rd, w1, 16); first = false; }
        else emit_movk_x64(ctx, rd, w1, 16);
    }
    if (w2 != 0) {
        if (first) { emit_movz_x64(ctx, rd, w2, 32); first = false; }
        else emit_movk_x64(ctx, rd, w2, 32);
    }
    if (w3 != 0) {
        if (first) { emit_movz_x64(ctx, rd, w3, 48); first = false; }
        else emit_movk_x64(ctx, rd, w3, 48);
    }
}

// ============================================================================
// Data movement
// ============================================================================

// MOV Wd, Ws → ORR Wd, WZR, Ws
void emit_mov_w32_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rs) {
    // ORR Wd, WZR, Ws: 0 01 01010 00 0 Rm 000000 11111 Rd
    uint32_t inst = 0x2A0003E0 | ((rs & 0x1F) << 16) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// MOV Xd, Xs → ORR Xd, XZR, Xs
void emit_mov_x64_x64(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rs) {
    // ORR Xd, XZR, Xs: 1 01 01010 00 0 Rm 000000 11111 Rd
    uint32_t inst = 0xAA0003E0 | ((rs & 0x1F) << 16) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// ============================================================================
// Load/Store — unsigned immediate offset
// ============================================================================

// LDR Wt, [Xn, #imm]  — unsigned offset scaled by 4
void emit_ldr_w32_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset) {
    // 10 111 0 01 01 imm12 Rn Rt
    uint32_t scaled = byte_offset / 4;
    uint32_t inst = 0xB9400000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    emit_inst(ctx, inst);
}

// STR Wt, [Xn, #imm]
void emit_str_w32_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset) {
    // 10 111 0 01 00 imm12 Rn Rt
    uint32_t scaled = byte_offset / 4;
    uint32_t inst = 0xB9000000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    emit_inst(ctx, inst);
}

// LDRB Wt, [Xn, #imm]
void emit_ldrb_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset) {
    // 00 111 0 01 01 imm12 Rn Rt
    uint32_t inst = 0x39400000 | ((byte_offset & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    emit_inst(ctx, inst);
}

// LDRSB Wt, [Xn, #imm]
void emit_ldrsb_w32_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset) {
    // 00 111 0 01 11 imm12 Rn Rt  (opc=11 = sign-extend to 32-bit)
    uint32_t inst = 0x39C00000 | ((byte_offset & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    emit_inst(ctx, inst);
}

// LDRH Wt, [Xn, #imm]
void emit_ldrh_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset) {
    // 01 111 0 01 01 imm12 Rn Rt  — offset scaled by 2
    uint32_t scaled = byte_offset / 2;
    uint32_t inst = 0x79400000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    emit_inst(ctx, inst);
}

// LDRSH Wt, [Xn, #imm]
void emit_ldrsh_w32_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset) {
    // 01 111 0 01 11 imm12 Rn Rt  (opc=11 = sign-extend to 32-bit)
    uint32_t scaled = byte_offset / 2;
    uint32_t inst = 0x79C00000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    emit_inst(ctx, inst);
}

// STRB Wt, [Xn, #imm]
void emit_strb_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset) {
    // 00 111 0 01 00 imm12 Rn Rt
    uint32_t inst = 0x39000000 | ((byte_offset & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    emit_inst(ctx, inst);
}

// STRH Wt, [Xn, #imm]
void emit_strh_imm(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, uint32_t byte_offset) {
    // 01 111 0 01 00 imm12 Rn Rt  — offset scaled by 2
    uint32_t scaled = byte_offset / 2;
    uint32_t inst = 0x79000000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    emit_inst(ctx, inst);
}

// ============================================================================
// Load/Store — register offset with UXTW (for 32-bit guest addresses)
// ============================================================================

// Generic register-offset load/store with UXTW extend
// size: 00=byte, 01=half, 10=word, 11=dword
// opc: load=01(unsigned), 10(sign-extend64), 11(sign-extend32); store=00
// option: 010=UXTW (zero-extend 32-bit index)
// S: 0=no shift, 1=shift by size

static void emit_ldst_reg_uxtw(emit_ctx_t *ctx, uint32_t size, uint32_t opc,
                                 a64_reg_t rt, a64_reg_t rn, a64_reg_t rm) {
    // size opc 111 0 00 1 Rm option S 10 Rn Rt
    // option=010 (UXTW), S=0 (no scaling for byte/half; for word: doesn't matter, S=0 is fine)
    uint32_t inst = ((size & 3) << 30) | ((opc & 3) << 22) | 0x38204800
                  | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    emit_inst(ctx, inst);
}

void emit_ldr_w32_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm) {
    emit_ldst_reg_uxtw(ctx, 2, 1, rt, rn, rm);  // size=10, opc=01 (LDR 32-bit)
}

void emit_str_w32_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm) {
    emit_ldst_reg_uxtw(ctx, 2, 0, rt, rn, rm);  // size=10, opc=00 (STR 32-bit)
}

void emit_ldrb_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm) {
    emit_ldst_reg_uxtw(ctx, 0, 1, rt, rn, rm);  // size=00, opc=01 (LDRB)
}

void emit_ldrsb_w32_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm) {
    emit_ldst_reg_uxtw(ctx, 0, 3, rt, rn, rm);  // size=00, opc=11 (LDRSB to 32-bit)
}

void emit_ldrh_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm) {
    emit_ldst_reg_uxtw(ctx, 1, 1, rt, rn, rm);  // size=01, opc=01 (LDRH)
}

void emit_ldrsh_w32_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm) {
    emit_ldst_reg_uxtw(ctx, 1, 3, rt, rn, rm);  // size=01, opc=11 (LDRSH to 32-bit)
}

void emit_strb_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm) {
    emit_ldst_reg_uxtw(ctx, 0, 0, rt, rn, rm);  // size=00, opc=00 (STRB)
}

void emit_strh_reg_uxtw(emit_ctx_t *ctx, a64_reg_t rt, a64_reg_t rn, a64_reg_t rm) {
    emit_ldst_reg_uxtw(ctx, 1, 0, rt, rn, rm);  // size=01, opc=00 (STRH)
}

// ============================================================================
// Arithmetic (32-bit)
// ============================================================================

// ADD Wd, Wn, Wm
void emit_add_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 0 00 01011 00 0 Rm 000000 Rn Rd
    uint32_t inst = 0x0B000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// ADD Wd, Wn, #imm12
void emit_add_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t imm12) {
    // 0 00 100010 0 imm12 Rn Rd
    uint32_t inst = 0x11000000 | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// SUB Wd, Wn, Wm
void emit_sub_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 0 10 01011 00 0 Rm 000000 Rn Rd
    uint32_t inst = 0x4B000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// SUB Wd, Wn, #imm12
void emit_sub_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t imm12) {
    // 0 10 100010 0 imm12 Rn Rd
    uint32_t inst = 0x51000000 | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// MUL Wd, Wn, Wm  (MADD Wd, Wn, Wm, WZR)
void emit_mul_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 0 00 11011 000 Rm 0 11111 Rn Rd
    uint32_t inst = 0x1B007C00 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// SDIV Wd, Wn, Wm
void emit_sdiv_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 0 00 11010110 Rm 00001 1 Rn Rd
    uint32_t inst = 0x1AC00C00 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// UDIV Wd, Wn, Wm
void emit_udiv_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 0 00 11010110 Rm 00001 0 Rn Rd
    uint32_t inst = 0x1AC00800 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// MSUB Wd, Wn, Wm, Wa  (Wa - Wn*Wm)
void emit_msub_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm, a64_reg_t ra) {
    // 0 00 11011 000 Rm 1 Ra Rn Rd
    uint32_t inst = 0x1B008000 | ((rm & 0x1F) << 16) | ((ra & 0x1F) << 10)
                  | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// SMULL Xd, Wn, Wm  (signed 32x32→64)
void emit_smull(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 1 00 11011 001 Rm 0 11111 Rn Rd  (SMADDL with Ra=XZR)
    uint32_t inst = 0x9B207C00 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// UMULL Xd, Wn, Wm  (unsigned 32x32→64)
void emit_umull(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 1 00 11011 101 Rm 0 11111 Rn Rd  (UMADDL with Ra=XZR)
    uint32_t inst = 0x9BA07C00 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// NEG Wd, Wm → SUB Wd, WZR, Wm
void emit_neg_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rm) {
    emit_sub_w32(ctx, rd, WZR, rm);
}

// ============================================================================
// Logical (32-bit)
// ============================================================================

// AND Wd, Wn, Wm
void emit_and_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 0 00 01010 00 0 Rm 000000 Rn Rd
    uint32_t inst = 0x0A000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// AND Wd, Wn, #imm  (logical immediate)
bool emit_and_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t imm) {
    uint32_t enc;
    if (!a64_encode_logical_imm(imm, &enc)) return false;
    // 0 00 100100 0 N:immr:imms Rn Rd
    uint32_t inst = 0x12000000 | (enc << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
    return true;
}

// ORR Wd, Wn, Wm
void emit_orr_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 0 01 01010 00 0 Rm 000000 Rn Rd
    uint32_t inst = 0x2A000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// ORR Wd, Wn, #imm
bool emit_orr_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t imm) {
    uint32_t enc;
    if (!a64_encode_logical_imm(imm, &enc)) return false;
    // 0 01 100100 0 N:immr:imms Rn Rd
    uint32_t inst = 0x32000000 | (enc << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
    return true;
}

// EOR Wd, Wn, Wm
void emit_eor_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 0 10 01010 00 0 Rm 000000 Rn Rd
    uint32_t inst = 0x4A000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// EOR Wd, Wn, #imm
bool emit_eor_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t imm) {
    uint32_t enc;
    if (!a64_encode_logical_imm(imm, &enc)) return false;
    // 0 10 100100 0 N:immr:imms Rn Rd
    uint32_t inst = 0x52000000 | (enc << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
    return true;
}

// MVN Wd, Wm → ORN Wd, WZR, Wm
void emit_mvn_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rm) {
    // 0 01 01010 00 1 Rm 000000 11111 Rd  (ORN with Rn=WZR)
    uint32_t inst = 0x2A2003E0 | ((rm & 0x1F) << 16) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// TST Wn, Wm → ANDS WZR, Wn, Wm
void emit_tst_w32(emit_ctx_t *ctx, a64_reg_t rn, a64_reg_t rm) {
    // 0 11 01010 00 0 Rm 000000 Rn 11111 (Rd=WZR)
    uint32_t inst = 0x6A00001F | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5);
    emit_inst(ctx, inst);
}

// ============================================================================
// Shifts (32-bit)
// ============================================================================

// LSLV Wd, Wn, Wm
void emit_lslv_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 0 00 11010110 Rm 0010 00 Rn Rd
    uint32_t inst = 0x1AC02000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// LSRV Wd, Wn, Wm
void emit_lsrv_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 0 00 11010110 Rm 0010 01 Rn Rd
    uint32_t inst = 0x1AC02400 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// ASRV Wd, Wn, Wm
void emit_asrv_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, a64_reg_t rm) {
    // 0 00 11010110 Rm 0010 10 Rn Rd
    uint32_t inst = 0x1AC02800 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// LSL Wd, Wn, #shift → UBFM Wd, Wn, #(-shift MOD 32), #(31-shift)
void emit_lsl_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t shift) {
    shift &= 31;
    if (shift == 0) {
        if (rd != rn) emit_mov_w32_w32(ctx, rd, rn);
        return;
    }
    uint32_t immr = (32 - shift) & 31;
    uint32_t imms = 31 - shift;
    // UBFM Wd, Wn, #immr, #imms: 0 10 100110 0 immr imms Rn Rd
    uint32_t inst = 0x53000000 | ((immr & 0x3F) << 16) | ((imms & 0x3F) << 10)
                  | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// LSR Wd, Wn, #shift → UBFM Wd, Wn, #shift, #31
void emit_lsr_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t shift) {
    shift &= 31;
    if (shift == 0) {
        if (rd != rn) emit_mov_w32_w32(ctx, rd, rn);
        return;
    }
    // UBFM Wd, Wn, #shift, #31
    uint32_t inst = 0x53000000 | ((shift & 0x3F) << 16) | (31 << 10)
                  | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// ASR Wd, Wn, #shift → SBFM Wd, Wn, #shift, #31
void emit_asr_w32_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t shift) {
    shift &= 31;
    if (shift == 0) {
        if (rd != rn) emit_mov_w32_w32(ctx, rd, rn);
        return;
    }
    // SBFM Wd, Wn, #shift, #31: 0 00 100110 0 immr imms Rn Rd
    uint32_t inst = 0x13000000 | ((shift & 0x3F) << 16) | (31 << 10)
                  | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// ASR Xd, Xn, #shift → SBFM Xd, Xn, #shift, #63
void emit_asr_x64_imm(emit_ctx_t *ctx, a64_reg_t rd, a64_reg_t rn, uint32_t shift) {
    shift &= 63;
    if (shift == 0) {
        if (rd != rn) emit_mov_x64_x64(ctx, rd, rn);
        return;
    }
    // SBFM Xd, Xn, #shift, #63: 1 00 100110 1 immr imms Rn Rd
    uint32_t inst = 0x9340FC00 | ((shift & 0x3F) << 16)
                  | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// ============================================================================
// Compare
// ============================================================================

// CMP Wn, Wm → SUBS WZR, Wn, Wm
void emit_cmp_w32_w32(emit_ctx_t *ctx, a64_reg_t rn, a64_reg_t rm) {
    // 0 11 01011 00 0 Rm 000000 Rn 11111
    uint32_t inst = 0x6B00001F | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5);
    emit_inst(ctx, inst);
}

// CMP Wn, #imm12 → SUBS WZR, Wn, #imm12
void emit_cmp_w32_imm(emit_ctx_t *ctx, a64_reg_t rn, uint32_t imm12) {
    // 0 11 100010 0 imm12 Rn 11111
    uint32_t inst = 0x7100001F | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5);
    emit_inst(ctx, inst);
}

// CMN Wn, #imm12 → ADDS WZR, Wn, #imm12
void emit_cmn_w32_imm(emit_ctx_t *ctx, a64_reg_t rn, uint32_t imm12) {
    // 0 01 100010 0 imm12 Rn 11111  (wait, ADDS is 0 11 => no)
    // ADDS: 0 01 10001 sh imm12 Rn Rd  ... Actually:
    // ADDS Wd, Wn, #imm: 0 01 100010 sh imm12 Rn Rd
    // Wait, let me get this right:
    // ADD  = 0 00 100010 ...
    // ADDS = 0 01 100010 ...  (sets flags, S bit)
    // Hmm, actually the encoding is:
    // sf=0 op=0 S=1 10001 sh imm12 Rn Rd
    // = 0 0 1 10001 0 imm12 Rn Rd  = 0x31000000
    // CMN = ADDS with Rd=WZR
    uint32_t inst = 0x3100001F | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5);
    emit_inst(ctx, inst);
}

// ============================================================================
// Conditional set
// ============================================================================

// CSET Wd, cond → CSINC Wd, WZR, WZR, invert(cond)
void emit_cset_w32(emit_ctx_t *ctx, a64_reg_t rd, a64_cond_t cond) {
    // Invert condition: flip bit 0
    uint32_t inv_cond = cond ^ 1;
    // CSINC Wd, WZR, WZR, inv_cond: 0 00 11010100 11111 inv_cond 0 1 11111 Rd
    uint32_t inst = 0x1A9F07E0 | ((inv_cond & 0xF) << 12) | (rd & 0x1F);
    emit_inst(ctx, inst);
}

// ============================================================================
// Branches
// ============================================================================

// B offset (PC-relative, +-128MB, offset in bytes)
void emit_b(emit_ctx_t *ctx, int32_t byte_offset) {
    int32_t imm26 = byte_offset >> 2;
    // 000101 imm26
    uint32_t inst = 0x14000000 | (imm26 & 0x03FFFFFF);
    emit_inst(ctx, inst);
}

// B.cond offset (PC-relative, +-1MB, offset in bytes)
void emit_b_cond(emit_ctx_t *ctx, a64_cond_t cond, int32_t byte_offset) {
    int32_t imm19 = byte_offset >> 2;
    // 01010100 imm19 0 cond
    uint32_t inst = 0x54000000 | ((imm19 & 0x7FFFF) << 5) | (cond & 0xF);
    emit_inst(ctx, inst);
}

// CBZ Wt, offset
void emit_cbz_w32(emit_ctx_t *ctx, a64_reg_t rt, int32_t byte_offset) {
    int32_t imm19 = byte_offset >> 2;
    // 0 011010 0 imm19 Rt
    uint32_t inst = 0x34000000 | ((imm19 & 0x7FFFF) << 5) | (rt & 0x1F);
    emit_inst(ctx, inst);
}

// CBNZ Wt, offset
void emit_cbnz_w32(emit_ctx_t *ctx, a64_reg_t rt, int32_t byte_offset) {
    int32_t imm19 = byte_offset >> 2;
    // 0 011010 1 imm19 Rt
    uint32_t inst = 0x35000000 | ((imm19 & 0x7FFFF) << 5) | (rt & 0x1F);
    emit_inst(ctx, inst);
}

// BR Xn
void emit_br(emit_ctx_t *ctx, a64_reg_t rn) {
    // 1101011 0000 11111 000000 Rn 00000
    uint32_t inst = 0xD61F0000 | ((rn & 0x1F) << 5);
    emit_inst(ctx, inst);
}

// BLR Xn
void emit_blr(emit_ctx_t *ctx, a64_reg_t rn) {
    // 1101011 0001 11111 000000 Rn 00000
    uint32_t inst = 0xD63F0000 | ((rn & 0x1F) << 5);
    emit_inst(ctx, inst);
}

// RET (X30)
void emit_ret_lr(emit_ctx_t *ctx) {
    // 1101011 0010 11111 000000 11110 00000
    emit_inst(ctx, 0xD65F03C0);
}

// ============================================================================
// Stack
// ============================================================================

// STP Xt1, Xt2, [SP, #imm]!  (pre-index, 64-bit)
void emit_stp_pre_x64(emit_ctx_t *ctx, a64_reg_t rt1, a64_reg_t rt2, int32_t imm) {
    // 10 101 0 011 1 imm7 Rt2 Rn Rt1  (Rn=SP=31)
    int32_t scaled = imm / 8;  // Scaled by 8 for 64-bit
    uint32_t inst = 0xA9800000 | ((scaled & 0x7F) << 15) | ((rt2 & 0x1F) << 10)
                  | (31 << 5) | (rt1 & 0x1F);
    emit_inst(ctx, inst);
}

// LDP Xt1, Xt2, [SP], #imm  (post-index, 64-bit)
void emit_ldp_post_x64(emit_ctx_t *ctx, a64_reg_t rt1, a64_reg_t rt2, int32_t imm) {
    // 10 101 0 001 1 imm7 Rt2 Rn Rt1  (Rn=SP=31)
    int32_t scaled = imm / 8;
    uint32_t inst = 0xA8C00000 | ((scaled & 0x7F) << 15) | ((rt2 & 0x1F) << 10)
                  | (31 << 5) | (rt1 & 0x1F);
    emit_inst(ctx, inst);
}

// ============================================================================
// Misc
// ============================================================================

void emit_nop(emit_ctx_t *ctx) {
    emit_inst(ctx, 0xD503201F);
}

void emit_brk(emit_ctx_t *ctx, uint16_t imm) {
    // 11010100 001 imm16 000 00
    uint32_t inst = 0xD4200000 | ((uint32_t)imm << 5);
    emit_inst(ctx, inst);
}

// ============================================================================
// Patching
// ============================================================================

// Patch a B instruction at patch_site to jump to target
void emit_patch_b26(uint32_t *patch_site, uint32_t *target) {
    int64_t diff = (int64_t)((uint8_t*)target - (uint8_t*)patch_site);
    int32_t imm26 = (int32_t)(diff >> 2);
    *patch_site = 0x14000000 | (imm26 & 0x03FFFFFF);
    // Caller must flush I-cache
}

// Patch using emit_ctx offsets (for inline patching during translation)
void emit_patch_rel32(emit_ctx_t *ctx, size_t patch_offset, size_t target_offset) {
    int32_t byte_diff = (int32_t)(target_offset - patch_offset);
    int32_t imm26 = byte_diff >> 2;
    // Rewrite the B instruction at patch_offset
    uint8_t *p = ctx->buf + patch_offset;
    uint32_t inst = 0x14000000 | (imm26 & 0x03FFFFFF);
    p[0] = (inst >>  0) & 0xFF;
    p[1] = (inst >>  8) & 0xFF;
    p[2] = (inst >> 16) & 0xFF;
    p[3] = (inst >> 24) & 0xFF;
}
