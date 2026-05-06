// a64_encode.h — AArch64 instruction encoder for the SLOW-32 cross-compiler
//
// Adapted from tools/dbt/emit_a64.{h,c} (proven in production DBT).
// Style matches x64_encode.h in this repo: global code buffer, int-only types,
// no bool, no ctx struct. Static functions get inlined by host gcc and become
// regular calls when compiled by the self-hosted SLOW-32 / a64 compiler.
//
// AArch64 instructions are fixed 32 bits, little-endian. Most encoders here
// just OR fields into a base opcode and call a64_inst(w).

#ifndef A64_ENCODE_H
#define A64_ENCODE_H

// ============================================================================
// Code buffer — global state (mirrors x64_buf / x64_off pattern)
// ============================================================================

#define A64_BUF_SIZE 4194304

static unsigned char a64_buf[A64_BUF_SIZE];
static int a64_off;     // current write position in bytes

// ============================================================================
// Register encoding
//   X0..X30 = 0..30, 31 = XZR (or SP, depending on instruction context)
//   W = 32-bit view of the same register file
// ============================================================================

#define A64_X0   0
#define A64_X1   1
#define A64_X2   2
#define A64_X3   3
#define A64_X4   4
#define A64_X5   5
#define A64_X6   6
#define A64_X7   7
#define A64_X8   8
#define A64_X9   9
#define A64_X10  10
#define A64_X11  11
#define A64_X12  12
#define A64_X13  13
#define A64_X14  14
#define A64_X15  15
#define A64_X16  16
#define A64_X17  17
#define A64_X18  18
#define A64_X19  19
#define A64_X20  20
#define A64_X21  21
#define A64_X22  22
#define A64_X23  23
#define A64_X24  24
#define A64_X25  25
#define A64_X26  26
#define A64_X27  27
#define A64_X28  28
#define A64_X29  29  /* FP */
#define A64_X30  30  /* LR */
#define A64_XZR  31
#define A64_SP   31  /* same encoding as XZR; instruction selects meaning */

// ============================================================================
// Condition codes (B.cond, CSET, CSEL, CSINC, ...)
// ============================================================================

#define A64_COND_EQ  0x0   /* Z=1 */
#define A64_COND_NE  0x1   /* Z=0 */
#define A64_COND_CS  0x2   /* C=1 (alias HS, unsigned >=) */
#define A64_COND_HS  0x2
#define A64_COND_CC  0x3   /* C=0 (alias LO, unsigned <)  */
#define A64_COND_LO  0x3
#define A64_COND_MI  0x4   /* N=1 */
#define A64_COND_PL  0x5   /* N=0 */
#define A64_COND_VS  0x6
#define A64_COND_VC  0x7
#define A64_COND_HI  0x8   /* unsigned >  */
#define A64_COND_LS  0x9   /* unsigned <= */
#define A64_COND_GE  0xA   /* signed   >= */
#define A64_COND_LT  0xB   /* signed   <  */
#define A64_COND_GT  0xC   /* signed   >  */
#define A64_COND_LE  0xD   /* signed   <= */
#define A64_COND_AL  0xE

// ============================================================================
// Raw byte / instruction emission
// ============================================================================

static void a64_byte(int b) {
    if (a64_off < A64_BUF_SIZE)
        a64_buf[a64_off] = (unsigned char)b;
    a64_off = a64_off + 1;
}

// All AArch64 instructions are 32-bit little-endian.
static void a64_inst(int w) {
    a64_byte(w & 0xFF);
    a64_byte((w >> 8) & 0xFF);
    a64_byte((w >> 16) & 0xFF);
    a64_byte((w >> 24) & 0xFF);
}

static int a64_pos(void) { return a64_off; }

// Patch a 32-bit word at an earlier offset.
static void a64_patch_inst(int off, int w) {
    a64_buf[off + 0] = (unsigned char)(w & 0xFF);
    a64_buf[off + 1] = (unsigned char)((w >> 8) & 0xFF);
    a64_buf[off + 2] = (unsigned char)((w >> 16) & 0xFF);
    a64_buf[off + 3] = (unsigned char)((w >> 24) & 0xFF);
}

// Read back a 32-bit word for read-modify-write patching.
static int a64_read_inst(int off) {
    int b0; int b1; int b2; int b3;
    b0 = a64_buf[off + 0] & 0xFF;
    b1 = a64_buf[off + 1] & 0xFF;
    b2 = a64_buf[off + 2] & 0xFF;
    b3 = a64_buf[off + 3] & 0xFF;
    return b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
}

// ============================================================================
// Logical-immediate encoder
//
// AArch64 logical-immediate encoding for AND/ORR/EOR uses N:immr:imms (13 bits
// for 64-bit, 12 bits + N=0 for 32-bit). The pattern is a run of `s+1` ones
// rotated right by `r`, replicated to fill the register width. Element sizes
// for 32-bit: 2, 4, 8, 16, 32.
//
// Returns 1 on success and writes the packed (N<<12) | (immr<<6) | imms into
// *encoded. Returns 0 if val is not encodable as a 32-bit logical immediate.
// ============================================================================

// Count trailing ones in low 32 bits of v. Returns 0..32.
static int a64_ctz_ones(int v) {
    int n;
    n = 0;
    while (n < 32 && (v & 1)) {
        v = (v >> 1) & 0x7FFFFFFF;  /* logical shift via masking */
        n = n + 1;
    }
    return n;
}

static int a64_encode_logical_imm32(int val, int *encoded) {
    int esz; int mask; int elem; int uniform; int i; int r;
    int rotated; int ones; int expected; int s; int immr; int imms;

    if (val == 0) return 0;
    if (val == -1) return 0;            /* 0xFFFFFFFF */

    esz = 2;
    while (esz <= 32) {
        if (esz == 32) mask = -1;
        else mask = (1 << esz) - 1;
        elem = val & mask;

        uniform = 1;
        i = esz;
        while (i < 32) {
            int chunk;
            if (esz == 32) chunk = (val >> i) & mask;  /* won't happen: i<32 only */
            else chunk = ((val >> i) & mask);
            if (chunk != elem) { uniform = 0; i = 32; }
            else i = i + esz;
        }
        if (!uniform) { esz = esz << 1; continue; }

        r = 0;
        while (r < esz) {
            if (r == 0) rotated = elem & mask;
            else {
                int lo; int hi;
                lo = (elem << r) & mask;
                /* unsigned right shift via mask */
                hi = (elem >> (esz - r)) & ((1 << r) - 1);
                rotated = (lo | hi) & mask;
            }
            if (rotated != 0) {
                ones = a64_ctz_ones(rotated);
                if (ones < esz) {
                    expected = (1 << ones) - 1;
                    if (rotated == expected) {
                        s = ones - 1;
                        immr = r;
                        if      (esz == 2)  imms = 0x3C | s;
                        else if (esz == 4)  imms = 0x38 | s;
                        else if (esz == 8)  imms = 0x30 | s;
                        else if (esz == 16) imms = 0x20 | s;
                        else                imms = s;        /* esz == 32 */
                        *encoded = ((immr & 0x3F) << 6) | (imms & 0x3F);
                        return 1;
                    }
                }
            }
            r = r + 1;
        }
        esz = esz << 1;
    }
    return 0;
}

// ============================================================================
// MOV-immediate (MOVZ / MOVK) — 32-bit
// ============================================================================

// MOVZ Wd, #imm16, LSL #(0|16)
static void a64_movz_w(int rd, int imm16, int shift) {
    int hw; int inst;
    if (shift == 16) hw = 1; else hw = 0;
    inst = 0x52800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// MOVK Wd, #imm16, LSL #(0|16)
static void a64_movk_w(int rd, int imm16, int shift) {
    int hw; int inst;
    if (shift == 16) hw = 1; else hw = 0;
    inst = 0x72800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// MOV Wd, #imm32 — MOVZ + optional MOVK
static void a64_mov_w_imm(int rd, int imm) {
    int lo; int hi;
    lo = imm & 0xFFFF;
    hi = (imm >> 16) & 0xFFFF;
    if (hi == 0)        a64_movz_w(rd, lo, 0);
    else if (lo == 0)   a64_movz_w(rd, hi, 16);
    else { a64_movz_w(rd, lo, 0); a64_movk_w(rd, hi, 16); }
}

// MOVN Wd, #imm16, LSL #(0|16). Sets Wd = ~(imm16 << shift).
static void a64_movn_w(int rd, int imm16, int shift) {
    int hw; int inst;
    if (shift == 16) hw = 1; else hw = 0;
    inst = 0x12800000 | (hw << 21) | ((imm16 & 0xFFFF) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// MOV-immediate (MOVZ / MOVK) — 64-bit
// ============================================================================

// MOVZ Xd, #imm16, LSL #(0|16|32|48). shift is in bits.
static void a64_movz_x(int rd, int imm16, int shift) {
    int hw; int inst;
    hw = shift / 16;
    inst = 0xD2800000 | ((hw & 3) << 21) | ((imm16 & 0xFFFF) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// MOVK Xd, #imm16, LSL #(0|16|32|48)
static void a64_movk_x(int rd, int imm16, int shift) {
    int hw; int inst;
    hw = shift / 16;
    inst = 0xF2800000 | ((hw & 3) << 21) | ((imm16 & 0xFFFF) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// MOV Xd, #imm64 (lo, hi halves). Up to 4 instructions.
static void a64_mov_x_imm64(int rd, int lo, int hi) {
    int w0; int w1; int w2; int w3; int first;
    w0 = lo & 0xFFFF;
    w1 = (lo >> 16) & 0xFFFF;
    w2 = hi & 0xFFFF;
    w3 = (hi >> 16) & 0xFFFF;

    first = 1;
    if (w0 != 0 || (w1 == 0 && w2 == 0 && w3 == 0)) {
        a64_movz_x(rd, w0, 0);
        first = 0;
    }
    if (w1 != 0) {
        if (first) { a64_movz_x(rd, w1, 16); first = 0; }
        else a64_movk_x(rd, w1, 16);
    }
    if (w2 != 0) {
        if (first) { a64_movz_x(rd, w2, 32); first = 0; }
        else a64_movk_x(rd, w2, 32);
    }
    if (w3 != 0) {
        if (first) { a64_movz_x(rd, w3, 48); first = 0; }
        else a64_movk_x(rd, w3, 48);
    }
}

// ============================================================================
// Register-to-register MOV
// ============================================================================

// MOV Wd, Wn  (ORR Wd, WZR, Wn)
static void a64_mov_w(int rd, int rn) {
    int inst;
    inst = 0x2A0003E0 | ((rn & 0x1F) << 16) | (rd & 0x1F);
    a64_inst(inst);
}

// MOV Xd, Xn  (ORR Xd, XZR, Xn)
static void a64_mov_x(int rd, int rn) {
    int inst;
    inst = 0xAA0003E0 | ((rn & 0x1F) << 16) | (rd & 0x1F);
    a64_inst(inst);
}

// MOV Xd, SP  /  MOV SP, Xn  (ADD Xd, Xn, #0 — required for SP).
static void a64_mov_sp_x(int rd, int rn) {
    int inst;
    inst = 0x91000000 | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// Add / Sub — 32-bit
// ============================================================================

// ADD Wd, Wn, Wm
static void a64_add_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x0B000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SUB Wd, Wn, Wm
static void a64_sub_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x4B000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ADD Wd, Wn, #imm12 (shift 0). Caller responsible for fitting 0..4095.
static void a64_add_w_imm(int rd, int rn, int imm12) {
    int inst;
    inst = 0x11000000 | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ADD Wd, Wn, #imm12, LSL #12  (12-bit imm, shifted by 12 — gives 4MB range)
static void a64_add_w_imm_lsl12(int rd, int rn, int imm12) {
    int inst;
    inst = 0x11400000 | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SUB Wd, Wn, #imm12 (shift 0)
static void a64_sub_w_imm(int rd, int rn, int imm12) {
    int inst;
    inst = 0x51000000 | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SUB Wd, Wn, #imm12, LSL #12
static void a64_sub_w_imm_lsl12(int rd, int rn, int imm12) {
    int inst;
    inst = 0x51400000 | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// NEG Wd, Wn  (SUB Wd, WZR, Wn)
static void a64_neg_w(int rd, int rn) { a64_sub_w(rd, A64_XZR, rn); }

// ============================================================================
// Add / Sub — 64-bit
// ============================================================================

// ADD Xd, Xn, Xm
static void a64_add_x(int rd, int rn, int rm) {
    int inst;
    inst = 0x8B000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SUB Xd, Xn, Xm
static void a64_sub_x(int rd, int rn, int rm) {
    int inst;
    inst = 0xCB000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ADD Xd, Xn, #imm12 (shift 0)
static void a64_add_x_imm(int rd, int rn, int imm12) {
    int inst;
    inst = 0x91000000 | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ADD Xd, Xn, #imm12, LSL #12
static void a64_add_x_imm_lsl12(int rd, int rn, int imm12) {
    int inst;
    inst = 0x91400000 | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SUB Xd, Xn, #imm12
static void a64_sub_x_imm(int rd, int rn, int imm12) {
    int inst;
    inst = 0xD1000000 | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SUB Xd, Xn, #imm12, LSL #12
static void a64_sub_x_imm_lsl12(int rd, int rn, int imm12) {
    int inst;
    inst = 0xD1400000 | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ADD Xd, Xn, Wm, UXTW  (zero-extend 32-bit Wm to 64 and add)
static void a64_add_x_w_uxtw(int rd, int rn, int wm) {
    int inst;
    inst = 0x8B204000 | ((wm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// Mul / Div — 32-bit
// ============================================================================

// MUL Wd, Wn, Wm   (MADD with Ra=WZR)
static void a64_mul_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x1B007C00 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SDIV Wd, Wn, Wm
static void a64_sdiv_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x1AC00C00 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// UDIV Wd, Wn, Wm
static void a64_udiv_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x1AC00800 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// MSUB Wd, Wn, Wm, Wa  (Wa - Wn*Wm)  — used to compute remainder
static void a64_msub_w(int rd, int rn, int rm, int ra) {
    int inst;
    inst = 0x1B008000 | ((rm & 0x1F) << 16) | ((ra & 0x1F) << 10)
         | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// MADD Wd, Wn, Wm, Wa  (Wa + Wn*Wm)
static void a64_madd_w(int rd, int rn, int rm, int ra) {
    int inst;
    inst = 0x1B000000 | ((rm & 0x1F) << 16) | ((ra & 0x1F) << 10)
         | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SMULL Xd, Wn, Wm  (signed 32x32 -> 64)  /  UMULL: same opcode tweak
static void a64_smull(int rd, int rn, int rm) {
    int inst;
    inst = 0x9B207C00 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_umull(int rd, int rn, int rm) {
    int inst;
    inst = 0x9BA07C00 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// Mul / Div — 64-bit
// ============================================================================

// MUL Xd, Xn, Xm
static void a64_mul_x(int rd, int rn, int rm) {
    int inst;
    inst = 0x9B007C00 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SDIV Xd, Xn, Xm
static void a64_sdiv_x(int rd, int rn, int rm) {
    int inst;
    inst = 0x9AC00C00 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// UDIV Xd, Xn, Xm
static void a64_udiv_x(int rd, int rn, int rm) {
    int inst;
    inst = 0x9AC00800 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// Logical — 32-bit (AND, ORR, EOR; bitwise NOT via MVN)
// ============================================================================

static void a64_and_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x0A000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_orr_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x2A000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_eor_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x4A000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ANDS Wd, Wn, Wm  (sets flags)
static void a64_ands_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x6A000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// BIC/ORN/EON Wd, Wn, Wm — register-form `Wn op ~Wm` (N=1 flips Rm).
static void a64_bic_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x0A200000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_orn_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x2A200000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_eon_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x4A200000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// MVN Wd, Wn  (ORN Wd, WZR, Wn)
static void a64_mvn_w(int rd, int rn) {
    int inst;
    inst = 0x2A2003E0 | ((rn & 0x1F) << 16) | (rd & 0x1F);
    a64_inst(inst);
}

// TST Wn, Wm  (ANDS WZR, Wn, Wm)
static void a64_tst_w(int rn, int rm) {
    int inst;
    inst = 0x6A00001F | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5);
    a64_inst(inst);
}

// AND/ORR/EOR Wd, Wn, #imm — return 1 if encodable, 0 otherwise.
static int a64_and_w_imm(int rd, int rn, int imm) {
    int enc; int inst;
    if (!a64_encode_logical_imm32(imm, &enc)) return 0;
    /* base 0x12000000 with N=0 (already absent). enc has immr<<6|imms. */
    inst = 0x12000000 | (enc << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
    return 1;
}

static int a64_orr_w_imm(int rd, int rn, int imm) {
    int enc; int inst;
    if (!a64_encode_logical_imm32(imm, &enc)) return 0;
    inst = 0x32000000 | (enc << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
    return 1;
}

static int a64_eor_w_imm(int rd, int rn, int imm) {
    int enc; int inst;
    if (!a64_encode_logical_imm32(imm, &enc)) return 0;
    inst = 0x52000000 | (enc << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
    return 1;
}

// Count trailing ones in low 64 bits of v. Returns 0..64.
static int a64_ctz_ones64(long long v) {
    int n;
    n = 0;
    while (n < 64 && (v & 1)) {
        v = (long long)((unsigned long long)v >> 1);
        n = n + 1;
    }
    return n;
}

// AArch64 64-bit logical-immediate encoding.  Same scheme as 32-bit but
// the additional N=1, esz=64 case enables full-width patterns.  Returns
// the packed 13-bit field (N<<12 | immr<<6 | imms) so the call site can
// `inst |= enc << 10` exactly like the W-form (the extra N bit lands at
// inst bit 22).
static int a64_encode_logical_imm64(long long val, int *encoded) {
    int esz; long long mask; long long elem; int uniform; int i; int r;
    long long rotated; int ones; long long expected; int s; int immr;
    int imms; int n_bit;

    if (val == 0)    return 0;
    if (val == -1LL) return 0;

    esz = 2;
    while (esz <= 64) {
        if (esz == 64) mask = -1LL;
        else           mask = ((long long)1 << esz) - 1;
        elem = val & mask;

        uniform = 1;
        i = esz;
        while (i < 64) {
            long long chunk;
            chunk = (long long)((unsigned long long)val >> i) & mask;
            if (chunk != elem) { uniform = 0; i = 64; }
            else i = i + esz;
        }
        if (!uniform) { esz = esz << 1; continue; }

        r = 0;
        while (r < esz) {
            if (r == 0) rotated = elem & mask;
            else {
                long long lo; long long hi;
                lo = ((unsigned long long)elem << r) & (unsigned long long)mask;
                hi = (long long)((unsigned long long)elem >> (esz - r))
                   & (((long long)1 << r) - 1);
                rotated = (lo | hi) & mask;
            }
            if (rotated != 0 && rotated != mask) {
                ones = a64_ctz_ones64(rotated);
                if (ones > 0 && ones < esz) {
                    expected = ((long long)1 << ones) - 1;
                    if (rotated == expected) {
                        s = ones - 1;
                        immr = r;
                        if      (esz == 2)  { imms = 0x3C | s; n_bit = 0; }
                        else if (esz == 4)  { imms = 0x38 | s; n_bit = 0; }
                        else if (esz == 8)  { imms = 0x30 | s; n_bit = 0; }
                        else if (esz == 16) { imms = 0x20 | s; n_bit = 0; }
                        else if (esz == 32) { imms = s;        n_bit = 0; }
                        else                { imms = s;        n_bit = 1; }
                        *encoded = (n_bit << 12) | ((immr & 0x3F) << 6)
                                 | (imms & 0x3F);
                        return 1;
                    }
                }
            }
            r = r + 1;
        }
        esz = esz << 1;
    }
    return 0;
}

// AND/ORR/EOR Xd, Xn, #imm — 64-bit, return 1 if encodable.
// `imm` is the full 64-bit value the caller wants AND'd / OR'd / EOR'd.
// (For SEXT32(ICONST n) widening, pass (long long)(int)n; for ZEXT32,
//  pass (long long)(unsigned long long)(unsigned int)n.)
static int a64_and_x_imm(int rd, int rn, long long imm) {
    int enc; int inst;
    if (!a64_encode_logical_imm64(imm, &enc)) return 0;
    inst = 0x92000000 | (enc << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
    return 1;
}

static int a64_orr_x_imm(int rd, int rn, long long imm) {
    int enc; int inst;
    if (!a64_encode_logical_imm64(imm, &enc)) return 0;
    inst = 0xB2000000 | (enc << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
    return 1;
}

static int a64_eor_x_imm(int rd, int rn, long long imm) {
    int enc; int inst;
    if (!a64_encode_logical_imm64(imm, &enc)) return 0;
    inst = 0xD2000000 | (enc << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
    return 1;
}

// ============================================================================
// Logical — 64-bit
// ============================================================================

static void a64_and_x(int rd, int rn, int rm) {
    int inst;
    inst = 0x8A000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_orr_x(int rd, int rn, int rm) {
    int inst;
    inst = 0xAA000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_eor_x(int rd, int rn, int rm) {
    int inst;
    inst = 0xCA000000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ORR Xd, Xn, Xm, LSL #shift
static void a64_orr_x_lsl(int rd, int rn, int rm, int shift) {
    int inst;
    inst = 0xAA000000 | ((rm & 0x1F) << 16) | ((shift & 0x3F) << 10)
         | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// BIC/ORN/EON Xd, Xn, Xm — register-form `Xn op ~Xm` (N=1 flips Rm).
static void a64_bic_x(int rd, int rn, int rm) {
    int inst;
    inst = 0x8A200000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_orn_x(int rd, int rn, int rm) {
    int inst;
    inst = 0xAA200000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_eon_x(int rd, int rn, int rm) {
    int inst;
    inst = 0xCA200000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// Shifts — 32-bit
// ============================================================================

// LSLV Wd, Wn, Wm
static void a64_lslv_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x1AC02000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// LSRV Wd, Wn, Wm
static void a64_lsrv_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x1AC02400 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ASRV Wd, Wn, Wm
static void a64_asrv_w(int rd, int rn, int rm) {
    int inst;
    inst = 0x1AC02800 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// LSL Wd, Wn, #shift (UBFM alias)
static void a64_lsl_w_imm(int rd, int rn, int shift) {
    int immr; int imms; int inst;
    shift = shift & 31;
    if (shift == 0) { if (rd != rn) a64_mov_w(rd, rn); return; }
    immr = (32 - shift) & 31;
    imms = 31 - shift;
    inst = 0x53000000 | ((immr & 0x3F) << 16) | ((imms & 0x3F) << 10)
         | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// LSR Wd, Wn, #shift (UBFM alias)
static void a64_lsr_w_imm(int rd, int rn, int shift) {
    int inst;
    shift = shift & 31;
    if (shift == 0) { if (rd != rn) a64_mov_w(rd, rn); return; }
    inst = 0x53000000 | ((shift & 0x3F) << 16) | (31 << 10)
         | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ASR Wd, Wn, #shift (SBFM alias)
static void a64_asr_w_imm(int rd, int rn, int shift) {
    int inst;
    shift = shift & 31;
    if (shift == 0) { if (rd != rn) a64_mov_w(rd, rn); return; }
    inst = 0x13000000 | ((shift & 0x3F) << 16) | (31 << 10)
         | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// Shifts — 64-bit
// ============================================================================

static void a64_lslv_x(int rd, int rn, int rm) {
    int inst;
    inst = 0x9AC02000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_lsrv_x(int rd, int rn, int rm) {
    int inst;
    inst = 0x9AC02400 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_asrv_x(int rd, int rn, int rm) {
    int inst;
    inst = 0x9AC02800 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// LSR Xd, Xn, #shift (UBFM alias)
static void a64_lsr_x_imm(int rd, int rn, int shift) {
    int inst;
    shift = shift & 63;
    if (shift == 0) { if (rd != rn) a64_mov_x(rd, rn); return; }
    inst = 0xD340FC00 | ((shift & 0x3F) << 16)
         | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ASR Xd, Xn, #shift (SBFM alias)
static void a64_asr_x_imm(int rd, int rn, int shift) {
    int inst;
    shift = shift & 63;
    if (shift == 0) { if (rd != rn) a64_mov_x(rd, rn); return; }
    inst = 0x9340FC00 | ((shift & 0x3F) << 16)
         | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// LSL Xd, Xn, #shift (UBFM alias)
static void a64_lsl_x_imm(int rd, int rn, int shift) {
    int immr; int imms; int inst;
    shift = shift & 63;
    if (shift == 0) { if (rd != rn) a64_mov_x(rd, rn); return; }
    immr = (64 - shift) & 63;
    imms = 63 - shift;
    inst = 0xD3400000 | ((immr & 0x3F) << 16) | ((imms & 0x3F) << 10)
         | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// Sign / zero extension (BFM aliases)
// ============================================================================

// SXTB Wd, Wn — sign-extend byte to 32 bits  (SBFM Wd, Wn, #0, #7)
static void a64_sxtb_w(int rd, int rn) {
    int inst;
    inst = 0x13001C00 | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SXTH Wd, Wn — sign-extend half to 32 bits  (SBFM Wd, Wn, #0, #15)
static void a64_sxth_w(int rd, int rn) {
    int inst;
    inst = 0x13003C00 | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SXTW Xd, Wn — sign-extend 32 to 64  (SBFM Xd, Xn, #0, #31)
static void a64_sxtw(int rd, int rn) {
    int inst;
    inst = 0x93407C00 | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// UXTB Wd, Wn — zero-extend byte  (AND Wd, Wn, #0xFF)
static void a64_uxtb_w(int rd, int rn) {
    int inst;
    /* UBFM Wd, Wn, #0, #7 */
    inst = 0x53001C00 | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// UXTH Wd, Wn — zero-extend half  (UBFM Wd, Wn, #0, #15)
static void a64_uxth_w(int rd, int rn) {
    int inst;
    inst = 0x53003C00 | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// Compare
// ============================================================================

// CMP Wn, Wm  (SUBS WZR, Wn, Wm)
static void a64_cmp_w(int rn, int rm) {
    int inst;
    inst = 0x6B00001F | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5);
    a64_inst(inst);
}

// CMP Wn, #imm12
static void a64_cmp_w_imm(int rn, int imm12) {
    int inst;
    inst = 0x7100001F | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5);
    a64_inst(inst);
}

// CMN Wn, #imm12  (ADDS WZR, Wn, #imm12) — for compare against -imm
static void a64_cmn_w_imm(int rn, int imm12) {
    int inst;
    inst = 0x3100001F | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5);
    a64_inst(inst);
}

// CMP Xn, Xm
static void a64_cmp_x(int rn, int rm) {
    int inst;
    inst = 0xEB00001F | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5);
    a64_inst(inst);
}

// CMP Xn, #imm12
static void a64_cmp_x_imm(int rn, int imm12) {
    int inst;
    inst = 0xF100001F | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5);
    a64_inst(inst);
}

// CMN Xn, #imm12  (ADDS XZR, Xn, #imm12) — for compare against -imm
static void a64_cmn_x_imm(int rn, int imm12) {
    int inst;
    inst = 0xB100001F | ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5);
    a64_inst(inst);
}

// ============================================================================
// Conditional set / select
// ============================================================================

// CSET Wd, cond  (CSINC Wd, WZR, WZR, invert(cond))
static void a64_cset_w(int rd, int cond) {
    int inv; int inst;
    inv = cond ^ 1;
    inst = 0x1A9F07E0 | ((inv & 0xF) << 12) | (rd & 0x1F);
    a64_inst(inst);
}

// CSEL Wd, Wn, Wm, cond  — Wd = cond? Wn : Wm
static void a64_csel_w(int rd, int rn, int rm, int cond) {
    int inst;
    inst = 0x1A800000 | ((rm & 0x1F) << 16) | ((cond & 0xF) << 12)
         | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// Branches
// ============================================================================

// B target_off (PC-relative byte offset, +/- 128MB).
static void a64_b(int byte_off) {
    int imm26; int inst;
    imm26 = byte_off >> 2;
    inst = 0x14000000 | (imm26 & 0x03FFFFFF);
    a64_inst(inst);
}

// BL target_off (branch-with-link, PC-relative byte offset, +/- 128MB).
static void a64_bl(int byte_off) {
    int imm26; int inst;
    imm26 = byte_off >> 2;
    inst = 0x94000000 | (imm26 & 0x03FFFFFF);
    a64_inst(inst);
}

// B.cond target_off (PC-relative byte offset, +/- 1MB, imm19)
static void a64_b_cond(int cond, int byte_off) {
    int imm19; int inst;
    imm19 = byte_off >> 2;
    inst = 0x54000000 | ((imm19 & 0x7FFFF) << 5) | (cond & 0xF);
    a64_inst(inst);
}

// CBZ/CBNZ Wt, target_off
static void a64_cbz_w(int rt, int byte_off) {
    int imm19; int inst;
    imm19 = byte_off >> 2;
    inst = 0x34000000 | ((imm19 & 0x7FFFF) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

static void a64_cbnz_w(int rt, int byte_off) {
    int imm19; int inst;
    imm19 = byte_off >> 2;
    inst = 0x35000000 | ((imm19 & 0x7FFFF) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// CBZ/CBNZ Xt, target_off
static void a64_cbz_x(int rt, int byte_off) {
    int imm19; int inst;
    imm19 = byte_off >> 2;
    inst = 0xB4000000 | ((imm19 & 0x7FFFF) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

static void a64_cbnz_x(int rt, int byte_off) {
    int imm19; int inst;
    imm19 = byte_off >> 2;
    inst = 0xB5000000 | ((imm19 & 0x7FFFF) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// BR Xn  /  BLR Xn  /  RET (X30 implicit)
static void a64_br(int rn) {
    int inst;
    inst = 0xD61F0000 | ((rn & 0x1F) << 5);
    a64_inst(inst);
}

static void a64_blr(int rn) {
    int inst;
    inst = 0xD63F0000 | ((rn & 0x1F) << 5);
    a64_inst(inst);
}

static void a64_ret(void) {
    a64_inst(0xD65F03C0);
}

// ============================================================================
// Loads / stores — unsigned scaled offset
//   LDR/STR Wt, [Xn, #imm12*4]   (32-bit)
//   LDR/STR Xt, [Xn, #imm12*8]   (64-bit)
//   LDRB/STRB           (no scale)
//   LDRH/STRH           (scale 2)
//   LDRSB/LDRSH (sign-extending) Wt and Xt forms
// ============================================================================

static void a64_ldr_w_imm(int rt, int rn, int byte_off) {
    int scaled; int inst;
    scaled = byte_off / 4;
    inst = 0xB9400000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

static void a64_str_w_imm(int rt, int rn, int byte_off) {
    int scaled; int inst;
    scaled = byte_off / 4;
    inst = 0xB9000000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

static void a64_ldr_x_imm(int rt, int rn, int byte_off) {
    int scaled; int inst;
    scaled = byte_off / 8;
    inst = 0xF9400000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

static void a64_str_x_imm(int rt, int rn, int byte_off) {
    int scaled; int inst;
    scaled = byte_off / 8;
    inst = 0xF9000000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

static void a64_ldrb_imm(int rt, int rn, int byte_off) {
    int inst;
    inst = 0x39400000 | ((byte_off & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

static void a64_strb_imm(int rt, int rn, int byte_off) {
    int inst;
    inst = 0x39000000 | ((byte_off & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

static void a64_ldrh_imm(int rt, int rn, int byte_off) {
    int scaled; int inst;
    scaled = byte_off / 2;
    inst = 0x79400000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

static void a64_strh_imm(int rt, int rn, int byte_off) {
    int scaled; int inst;
    scaled = byte_off / 2;
    inst = 0x79000000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// LDRSB Wt — sign-extend byte to 32 (opc=11)
static void a64_ldrsb_w_imm(int rt, int rn, int byte_off) {
    int inst;
    inst = 0x39C00000 | ((byte_off & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// LDRSH Wt — sign-extend half to 32 (opc=11, scale 2)
static void a64_ldrsh_w_imm(int rt, int rn, int byte_off) {
    int scaled; int inst;
    scaled = byte_off / 2;
    inst = 0x79C00000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// Loads / stores — pre/post-indexed (LDR/STR Wt or Xt, [Xn, #imm9]!  / , #imm9)
// imm9 is signed -256..+255. Used for stack-frame setup with single regs.
// ============================================================================

// STR Xt, [Xn, #imm9]!   (pre-index, writeback)
static void a64_str_x_pre(int rt, int rn, int imm9) {
    int inst;
    inst = 0xF8000C00 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// LDR Xt, [Xn], #imm9    (post-index, writeback)
static void a64_ldr_x_post(int rt, int rn, int imm9) {
    int inst;
    inst = 0xF8400400 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// LDUR / STUR — unscaled signed-offset loads/stores (imm9 in -256..+255).
// Useful for negative frame offsets (FP-N) which the unsigned-offset
// form (LDR/STR with byte_off/scale) cannot encode.
// ============================================================================

// STUR Wt, [Xn, #imm9]
static void a64_stur_w_imm(int rt, int rn, int imm9) {
    int inst;
    inst = 0xB8000000 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// LDUR Wt, [Xn, #imm9]
static void a64_ldur_w_imm(int rt, int rn, int imm9) {
    int inst;
    inst = 0xB8400000 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// STUR Xt, [Xn, #imm9]
static void a64_stur_x_imm(int rt, int rn, int imm9) {
    int inst;
    inst = 0xF8000000 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// LDUR Xt, [Xn, #imm9]
static void a64_ldur_x_imm(int rt, int rn, int imm9) {
    int inst;
    inst = 0xF8400000 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// STURB Wt, [Xn, #imm9]
static void a64_sturb_imm(int rt, int rn, int imm9) {
    int inst;
    inst = 0x38000000 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// LDURB Wt, [Xn, #imm9]
static void a64_ldurb_imm(int rt, int rn, int imm9) {
    int inst;
    inst = 0x38400000 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// LDURSB Wt, [Xn, #imm9]   (sign-extending byte → W)
static void a64_ldursb_w_imm(int rt, int rn, int imm9) {
    int inst;
    inst = 0x38C00000 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// STURH Wt, [Xn, #imm9]
static void a64_sturh_imm(int rt, int rn, int imm9) {
    int inst;
    inst = 0x78000000 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// LDURH Wt, [Xn, #imm9]
static void a64_ldurh_imm(int rt, int rn, int imm9) {
    int inst;
    inst = 0x78400000 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// LDURSH Wt, [Xn, #imm9]
static void a64_ldursh_w_imm(int rt, int rn, int imm9) {
    int inst;
    inst = 0x78C00000 | ((imm9 & 0x1FF) << 12) | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// Loads / stores — register-indexed (LDR/STR Wt or Xt, [Xn, Xm{, LSL #k}])
//
// Encoding (LDR W, register form):
//   bits[31:21] = 10111000011    (32-bit LDR-reg)
//   bits[20:16] = Rm
//   bits[15:13] = option         (011 = LSL, 010 = UXTW)
//   bit[12]     = S              (0 = no scaling, 1 = scale by access size)
//   bits[11:10] = 10
//   bits[9:5]   = Rn
//   bits[4:0]   = Rt
//
// `lsl` here is the shift amount: 0 (S=0, no scale) or the access-size
// shift (S=1: 2 for W, 3 for X).  Other shift amounts can't be encoded
// with this form on AArch64.
// ============================================================================

// LDR Wt, [Xn, Xm, LSL #lsl]   (lsl ∈ {0, 2})
static void a64_ldr_w_reg_lsl(int rt, int rn, int rm, int lsl) {
    int s_bit; int inst;
    s_bit = (lsl == 2) ? 1 : 0;
    inst = 0xB8606800 | ((rm & 0x1F) << 16) | (s_bit << 12)
         | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// STR Wt, [Xn, Xm, LSL #lsl]   (lsl ∈ {0, 2})
static void a64_str_w_reg_lsl(int rt, int rn, int rm, int lsl) {
    int s_bit; int inst;
    s_bit = (lsl == 2) ? 1 : 0;
    inst = 0xB8206800 | ((rm & 0x1F) << 16) | (s_bit << 12)
         | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// LDR Xt, [Xn, Xm, LSL #lsl]   (lsl ∈ {0, 3})
static void a64_ldr_x_reg_lsl(int rt, int rn, int rm, int lsl) {
    int s_bit; int inst;
    s_bit = (lsl == 3) ? 1 : 0;
    inst = 0xF8606800 | ((rm & 0x1F) << 16) | (s_bit << 12)
         | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// STR Xt, [Xn, Xm, LSL #lsl]   (lsl ∈ {0, 3})
static void a64_str_x_reg_lsl(int rt, int rn, int rm, int lsl) {
    int s_bit; int inst;
    s_bit = (lsl == 3) ? 1 : 0;
    inst = 0xF8206800 | ((rm & 0x1F) << 16) | (s_bit << 12)
         | ((rn & 0x1F) << 5) | (rt & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// STP / LDP — pair load/store
// ============================================================================

// STP Xt1, Xt2, [Xn, #imm]!  (pre-index, scaled by 8)
static void a64_stp_x_pre(int rt1, int rt2, int rn, int imm) {
    int scaled; int inst;
    scaled = imm / 8;
    inst = 0xA9800000 | ((scaled & 0x7F) << 15) | ((rt2 & 0x1F) << 10)
         | ((rn & 0x1F) << 5) | (rt1 & 0x1F);
    a64_inst(inst);
}

// LDP Xt1, Xt2, [Xn], #imm  (post-index, scaled by 8)
static void a64_ldp_x_post(int rt1, int rt2, int rn, int imm) {
    int scaled; int inst;
    scaled = imm / 8;
    inst = 0xA8C00000 | ((scaled & 0x7F) << 15) | ((rt2 & 0x1F) << 10)
         | ((rn & 0x1F) << 5) | (rt1 & 0x1F);
    a64_inst(inst);
}

// STP Xt1, Xt2, [Xn, #imm]   (signed offset, no writeback, scaled by 8)
static void a64_stp_x_off(int rt1, int rt2, int rn, int imm) {
    int scaled; int inst;
    scaled = imm / 8;
    inst = 0xA9000000 | ((scaled & 0x7F) << 15) | ((rt2 & 0x1F) << 10)
         | ((rn & 0x1F) << 5) | (rt1 & 0x1F);
    a64_inst(inst);
}

// LDP Xt1, Xt2, [Xn, #imm]   (signed offset, no writeback, scaled by 8)
static void a64_ldp_x_off(int rt1, int rt2, int rn, int imm) {
    int scaled; int inst;
    scaled = imm / 8;
    inst = 0xA9400000 | ((scaled & 0x7F) << 15) | ((rt2 & 0x1F) << 10)
         | ((rn & 0x1F) << 5) | (rt1 & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// PC-relative addressing (ADR / ADRP) — used for global / function addresses
// ============================================================================

// ADR Xd, label   — 21-bit signed byte offset from current PC.
// imm21 is the byte distance: low 2 bits go in immlo (bits 30:29), top 19 in immhi.
static void a64_adr(int rd, int byte_off) {
    int immlo; int immhi; int inst;
    immlo = byte_off & 3;
    immhi = (byte_off >> 2) & 0x7FFFF;
    inst = 0x10000000 | (immlo << 29) | (immhi << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ADRP Xd, label  — page-relative; 21-bit signed offset scaled by 4096 (page).
// byte_page_off is the byte offset to the *target page* from the *PC page*.
static void a64_adrp(int rd, int byte_page_off) {
    int scaled; int immlo; int immhi; int inst;
    scaled = byte_page_off >> 12;       /* arithmetic shift */
    immlo = scaled & 3;
    immhi = (scaled >> 2) & 0x7FFFF;
    inst = 0x90000000 | (immlo << 29) | (immhi << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// FP — FMOV between GPR and SIMD/FP scalar
// ============================================================================

static void a64_fmov_s_w(int sd, int wn) {
    int inst;
    inst = 0x1E270000 | ((wn & 0x1F) << 5) | (sd & 0x1F);
    a64_inst(inst);
}

static void a64_fmov_w_s(int wd, int sn) {
    int inst;
    inst = 0x1E260000 | ((sn & 0x1F) << 5) | (wd & 0x1F);
    a64_inst(inst);
}

static void a64_fmov_d_x(int dd, int xn) {
    int inst;
    inst = 0x9E670000 | ((xn & 0x1F) << 5) | (dd & 0x1F);
    a64_inst(inst);
}

static void a64_fmov_x_d(int xd, int dn) {
    int inst;
    inst = 0x9E660000 | ((dn & 0x1F) << 5) | (xd & 0x1F);
    a64_inst(inst);
}

// FMOV Sd, Sn  /  FMOV Dd, Dn
static void a64_fmov_s_s(int sd, int sn) {
    int inst;
    inst = 0x1E204000 | ((sn & 0x1F) << 5) | (sd & 0x1F);
    a64_inst(inst);
}

static void a64_fmov_d_d(int dd, int dn) {
    int inst;
    inst = 0x1E604000 | ((dn & 0x1F) << 5) | (dd & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// FP arithmetic
// ============================================================================

// 2-source (FADD, FSUB, FMUL, FDIV).  type: 0=S, 1=D.
//   opcode field (10:15): FADD=0x0A FSUB=0x0E FMUL=0x02 FDIV=0x06 FNMUL=0x22
static void a64_fp_2src(int type, int opcode, int rd, int rn, int rm) {
    int inst;
    inst = 0x1E200800 | ((type & 1) << 22) | ((rm & 0x1F) << 16)
         | ((opcode & 0x3F) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_fadd_s(int rd, int rn, int rm) { a64_fp_2src(0, 0x0A, rd, rn, rm); }
static void a64_fadd_d(int rd, int rn, int rm) { a64_fp_2src(1, 0x0A, rd, rn, rm); }
static void a64_fsub_s(int rd, int rn, int rm) { a64_fp_2src(0, 0x0E, rd, rn, rm); }
static void a64_fsub_d(int rd, int rn, int rm) { a64_fp_2src(1, 0x0E, rd, rn, rm); }
static void a64_fmul_s(int rd, int rn, int rm) { a64_fp_2src(0, 0x02, rd, rn, rm); }
static void a64_fmul_d(int rd, int rn, int rm) { a64_fp_2src(1, 0x02, rd, rn, rm); }
static void a64_fdiv_s(int rd, int rn, int rm) { a64_fp_2src(0, 0x06, rd, rn, rm); }
static void a64_fdiv_d(int rd, int rn, int rm) { a64_fp_2src(1, 0x06, rd, rn, rm); }

// 1-source FNEG / FABS / FSQRT — base depends on op, type bit toggles S vs D.
static void a64_fp_1src(int base_s, int type, int rd, int rn) {
    int inst;
    inst = base_s | ((type & 1) << 22) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_fneg_s(int rd, int rn)  { a64_fp_1src(0x1E214000, 0, rd, rn); }
static void a64_fneg_d(int rd, int rn)  { a64_fp_1src(0x1E214000, 1, rd, rn); }
static void a64_fabs_s(int rd, int rn)  { a64_fp_1src(0x1E20C000, 0, rd, rn); }
static void a64_fabs_d(int rd, int rn)  { a64_fp_1src(0x1E20C000, 1, rd, rn); }
static void a64_fsqrt_s(int rd, int rn) { a64_fp_1src(0x1E21C000, 0, rd, rn); }
static void a64_fsqrt_d(int rd, int rn) { a64_fp_1src(0x1E21C000, 1, rd, rn); }

// FCMP Sn, Sm / FCMP Dn, Dm
static void a64_fcmp_s(int rn, int rm) {
    int inst;
    inst = 0x1E202000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5);
    a64_inst(inst);
}

static void a64_fcmp_d(int rn, int rm) {
    int inst;
    inst = 0x1E602000 | ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5);
    a64_inst(inst);
}

// FCVT — between scalar widths
static void a64_fcvt_s_d(int sd, int dn) {
    int inst;
    inst = 0x1E624000 | ((dn & 0x1F) << 5) | (sd & 0x1F);
    a64_inst(inst);
}

static void a64_fcvt_d_s(int dd, int sn) {
    int inst;
    inst = 0x1E22C000 | ((sn & 0x1F) << 5) | (dd & 0x1F);
    a64_inst(inst);
}

// FCVTZS/FCVTZU — float to int (round toward zero). sf: 0=W,1=X. type:0=S,1=D.
static void a64_fcvtzs(int sf, int type, int rd, int sn) {
    int inst;
    inst = 0x1E380000 | ((sf & 1) << 31) | ((type & 1) << 22)
         | ((sn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

static void a64_fcvtzu(int sf, int type, int rd, int sn) {
    int inst;
    inst = 0x1E390000 | ((sf & 1) << 31) | ((type & 1) << 22)
         | ((sn & 0x1F) << 5) | (rd & 0x1F);
    a64_inst(inst);
}

// SCVTF/UCVTF — int to float. sf: 0=W,1=X. type: 0=S,1=D.
static void a64_scvtf(int sf, int type, int sd, int rn) {
    int inst;
    inst = 0x1E220000 | ((sf & 1) << 31) | ((type & 1) << 22)
         | ((rn & 0x1F) << 5) | (sd & 0x1F);
    a64_inst(inst);
}

static void a64_ucvtf(int sf, int type, int sd, int rn) {
    int inst;
    inst = 0x1E230000 | ((sf & 1) << 31) | ((type & 1) << 22)
         | ((rn & 0x1F) << 5) | (sd & 0x1F);
    a64_inst(inst);
}

// FP loads/stores (32 / 64 bit, unsigned scaled offset)
static void a64_ldr_s_imm(int st, int rn, int byte_off) {
    int scaled; int inst;
    scaled = byte_off / 4;
    /* size=10 V=1 opc=01: 1011 1101 010 imm12 Rn Rt */
    inst = 0xBD400000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (st & 0x1F);
    a64_inst(inst);
}

static void a64_str_s_imm(int st, int rn, int byte_off) {
    int scaled; int inst;
    scaled = byte_off / 4;
    inst = 0xBD000000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (st & 0x1F);
    a64_inst(inst);
}

static void a64_ldr_d_imm(int dt, int rn, int byte_off) {
    int scaled; int inst;
    scaled = byte_off / 8;
    inst = 0xFD400000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (dt & 0x1F);
    a64_inst(inst);
}

static void a64_str_d_imm(int dt, int rn, int byte_off) {
    int scaled; int inst;
    scaled = byte_off / 8;
    inst = 0xFD000000 | ((scaled & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (dt & 0x1F);
    a64_inst(inst);
}

// ============================================================================
// System / misc
// ============================================================================

static void a64_nop(void)        { a64_inst(0xD503201F); }
static void a64_brk(int imm16)   { a64_inst(0xD4200000 | ((imm16 & 0xFFFF) << 5)); }
static void a64_svc(int imm16)   { a64_inst(0xD4000001 | ((imm16 & 0xFFFF) << 5)); }
static void a64_dmb_ishst(void)  { a64_inst(0xD5033ABF); }

/* MRS Xt, CNTVCT_EL0. */
static void a64_mrs_cntvct_el0(int rt) {
    a64_inst(0xD53BE040 | (rt & 0x1F));
}

// ============================================================================
// Patching helpers — for forward branches whose target is unknown at emit time
// ============================================================================

// Patch a B (unconditional) at offset `site` to jump to byte offset `target`.
static void a64_patch_b(int site, int target) {
    int diff; int imm26; int inst;
    diff = target - site;
    imm26 = diff >> 2;
    inst = 0x14000000 | (imm26 & 0x03FFFFFF);
    a64_patch_inst(site, inst);
}

// Patch a B.cond / CBZ / CBNZ (imm19) at offset `site` to jump to `target`.
// Preserves the existing condition / opcode / register fields.
static void a64_patch_cond(int site, int target) {
    int diff; int imm19; int word; int kept;
    diff = target - site;
    imm19 = diff >> 2;
    word = a64_read_inst(site);
    /* Clear bits 23..5 (imm19 field) and OR in new value. */
    kept = word & 0xFF00001F;
    word = kept | ((imm19 & 0x7FFFF) << 5);
    a64_patch_inst(site, word);
}

#endif // A64_ENCODE_H
