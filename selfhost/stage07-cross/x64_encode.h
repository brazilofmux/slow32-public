// x64_encode.h — x86-64 instruction encoder for the SLOW-32 cross-compiler
//
// Adapted from tools/dbt/emit_x64.h (proven in production DBT).
// Simplified for compiler use: no trace infrastructure, no RAX-pending checks.
// Uses a global code buffer (x64_buf/x64_off) matching s12cc's output style.
//
// All functions are static — inlined when compiled natively, regular calls
// when compiled by s12cc (which treats 'inline' as a no-op).

#ifndef X64_ENCODE_H
#define X64_ENCODE_H

// ============================================================================
// Code buffer — global state
// ============================================================================

#define X64_BUF_SIZE 4194304

static unsigned char x64_buf[X64_BUF_SIZE];
static int x64_off;     // current write position

// ============================================================================
// Register encoding
// ============================================================================

#define X64_RAX  0
#define X64_RCX  1
#define X64_RDX  2
#define X64_RBX  3
#define X64_RSP  4
#define X64_RBP  5
#define X64_RSI  6
#define X64_RDI  7
#define X64_R8   8
#define X64_R9   9
#define X64_R10  10
#define X64_R11  11
#define X64_R12  12
#define X64_R13  13
#define X64_R14  14
#define X64_R15  15

// ============================================================================
// REX prefix bits
// ============================================================================

#define REX_BASE 0x40
#define REX_W    0x08
#define REX_R    0x04
#define REX_X    0x02
#define REX_B    0x01

// ============================================================================
// ModR/M and SIB construction
// ============================================================================

#define MODRM(mod, reg, rm) (((mod) << 6) | (((reg) & 7) << 3) | ((rm) & 7))
#define SIB(scale, index, base) (((scale) << 6) | (((index) & 7) << 3) | ((base) & 7))

#define MOD_INDIRECT 0
#define MOD_DISP8    1
#define MOD_DISP32   2
#define MOD_DIRECT   3

// ============================================================================
// Raw byte emission
// ============================================================================

static void x64_byte(int b) {
    if (x64_off < X64_BUF_SIZE)
        x64_buf[x64_off] = (unsigned char)b;
    x64_off++;
}

static void x64_word(int w) {
    x64_byte(w & 0xFF);
    x64_byte((w >> 8) & 0xFF);
}

static void x64_dword(int d) {
    x64_byte(d & 0xFF);
    x64_byte((d >> 8) & 0xFF);
    x64_byte((d >> 16) & 0xFF);
    x64_byte((d >> 24) & 0xFF);
}

// Emit 8 bytes (for imm64) — takes two 32-bit halves (lo, hi)
static void x64_qword(int lo, int hi) {
    x64_dword(lo);
    x64_dword(hi);
}

// Get/set current offset
static int x64_pos(void) { return x64_off; }

// ============================================================================
// REX helpers
// ============================================================================

static int x64_rex_rr(int r, int rm, int w64) {
    int rex = 0;
    if (w64) rex = rex | REX_W;
    if (r >= 8) rex = rex | REX_R;
    if (rm >= 8) rex = rex | REX_B;
    return rex;
}

static int x64_rex_sib(int r, int base, int index, int w64) {
    int rex = 0;
    if (w64) rex = rex | REX_W;
    if (r >= 8) rex = rex | REX_R;
    if (base >= 8) rex = rex | REX_B;
    if (index >= 8) rex = rex | REX_X;
    return rex;
}

static void x64_rex_emit(int rex) {
    if (rex) x64_byte(REX_BASE | rex);
}

// Some byte-register encodings (SPL, BPL, SIL, DIL) need REX even if no
// extension bits are set, to disambiguate from AH/CH/DH/BH.
static int x64_needs_rex_byte(int r) {
    return r >= X64_RSP && r <= X64_RDI;
}

// ============================================================================
// Memory operand helper — emits ModR/M [base + disp], handling:
//   - RSP needs SIB byte
//   - RBP with disp=0 needs explicit disp8=0
//   - Auto-selects disp8 vs disp32
// ============================================================================

static void x64_modrm_mem(int reg, int base, int disp) {
    int use_disp8;
    int mod;
    int force_disp;

    force_disp = ((base & 7) == X64_RBP);
    use_disp8 = (disp >= -128 && disp <= 127);

    if (disp == 0 && !force_disp)
        mod = MOD_INDIRECT;
    else if (use_disp8)
        mod = MOD_DISP8;
    else
        mod = MOD_DISP32;

    if ((base & 7) == X64_RSP) {
        x64_byte(MODRM(mod, reg, X64_RSP));
        x64_byte(SIB(0, X64_RSP, base));
    } else {
        x64_byte(MODRM(mod, reg, base));
    }

    if (mod == MOD_DISP8)
        x64_byte(disp & 0xFF);
    else if (mod == MOD_DISP32)
        x64_dword(disp);
}

// ============================================================================
// SIB memory operand — emits ModR/M + SIB for [base + index*scale + disp]
// scale: 0=1, 1=2, 2=4, 3=8
// ============================================================================

static void x64_modrm_sib(int reg, int base, int index, int scale, int disp) {
    int use_disp8;
    int mod;
    int force_disp;

    force_disp = ((base & 7) == X64_RBP);
    use_disp8 = (disp >= -128 && disp <= 127);

    if (disp == 0 && !force_disp)
        mod = MOD_INDIRECT;
    else if (use_disp8)
        mod = MOD_DISP8;
    else
        mod = MOD_DISP32;

    x64_byte(MODRM(mod, reg, 4));  /* rm=4 signals SIB follows */
    x64_byte(SIB(scale, index, base));

    if (mod == MOD_DISP8)
        x64_byte(disp & 0xFF);
    else if (mod == MOD_DISP32)
        x64_dword(disp);
}

// mov r32, [base + index*scale + disp]
static void x64_mov_rm_sib(int dst, int base, int index, int scale, int disp) {
    x64_rex_emit(x64_rex_sib(dst, base, index, 0));
    x64_byte(0x8B);
    x64_modrm_sib(dst, base, index, scale, disp);
}

// mov [base + index*scale + disp], r32
static void x64_mov_mr_sib(int base, int index, int scale, int disp, int src) {
    x64_rex_emit(x64_rex_sib(src, base, index, 0));
    x64_byte(0x89);
    x64_modrm_sib(src, base, index, scale, disp);
}

// add dword [base + disp], imm32
static void x64_add_mi(int base, int disp, int imm) {
    int use_imm8;
    int rex;
    rex = 0;
    if (base >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    use_imm8 = (imm >= -128 && imm <= 127);
    if (use_imm8) {
        x64_byte(0x83);
        x64_modrm_mem(0, base, disp);  /* /0 = ADD */
        x64_byte(imm & 0xFF);
    } else {
        x64_byte(0x81);
        x64_modrm_mem(0, base, disp);  /* /0 = ADD */
        x64_dword(imm);
    }
}

// ============================================================================
// Data movement
// ============================================================================

// mov r32, r32
static void x64_mov_rr(int dst, int src) {
    x64_rex_emit(x64_rex_rr(src, dst, 0));
    x64_byte(0x89);
    x64_byte(MODRM(MOD_DIRECT, src, dst));
}

// mov r64, r64
static void x64_mov_rr64(int dst, int src) {
    x64_byte(REX_BASE | x64_rex_rr(src, dst, 1));
    x64_byte(0x89);
    x64_byte(MODRM(MOD_DIRECT, src, dst));
}

// mov r32, imm32
static void x64_mov_ri(int dst, int imm) {
    if (dst >= 8) x64_byte(REX_BASE | REX_B);
    x64_byte(0xB8 + (dst & 7));
    x64_dword(imm);
}

// mov r64, imm64 (lo, hi halves)
static void x64_mov_ri64(int dst, int lo, int hi) {
    int rex = REX_W;
    if (dst >= 8) rex = rex | REX_B;
    x64_byte(REX_BASE | rex);
    x64_byte(0xB8 + (dst & 7));
    x64_qword(lo, hi);
}

// mov r32, [base + disp]
static void x64_mov_rm(int dst, int base, int disp) {
    x64_rex_emit(x64_rex_rr(dst, base, 0));
    x64_byte(0x8B);
    x64_modrm_mem(dst, base, disp);
}

// mov r64, [base + disp]
static void x64_mov_rm64(int dst, int base, int disp) {
    x64_byte(REX_BASE | x64_rex_rr(dst, base, 1));
    x64_byte(0x8B);
    x64_modrm_mem(dst, base, disp);
}

// mov [base + disp], r32
static void x64_mov_mr(int base, int disp, int src) {
    x64_rex_emit(x64_rex_rr(src, base, 0));
    x64_byte(0x89);
    x64_modrm_mem(src, base, disp);
}

// mov [base + disp], r64
static void x64_mov_mr64(int base, int disp, int src) {
    x64_byte(REX_BASE | x64_rex_rr(src, base, 1));
    x64_byte(0x89);
    x64_modrm_mem(src, base, disp);
}

// mov [base + disp], imm32
static void x64_mov_mi(int base, int disp, int imm) {
    int rex = 0;
    if (base >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xC7);
    x64_modrm_mem(0, base, disp);
    x64_dword(imm);
}

// movzx r32, byte [base + disp]
static void x64_movzx_rm8(int dst, int base, int disp) {
    x64_rex_emit(x64_rex_rr(dst, base, 0));
    x64_byte(0x0F);
    x64_byte(0xB6);
    x64_modrm_mem(dst, base, disp);
}

// movsx r32, byte [base + disp]
static void x64_movsx_rm8(int dst, int base, int disp) {
    x64_rex_emit(x64_rex_rr(dst, base, 0));
    x64_byte(0x0F);
    x64_byte(0xBE);
    x64_modrm_mem(dst, base, disp);
}

// movzx r32, word [base + disp]
static void x64_movzx_rm16(int dst, int base, int disp) {
    x64_rex_emit(x64_rex_rr(dst, base, 0));
    x64_byte(0x0F);
    x64_byte(0xB7);
    x64_modrm_mem(dst, base, disp);
}

// movsx r32, word [base + disp]
static void x64_movsx_rm16(int dst, int base, int disp) {
    x64_rex_emit(x64_rex_rr(dst, base, 0));
    x64_byte(0x0F);
    x64_byte(0xBF);
    x64_modrm_mem(dst, base, disp);
}

// mov byte [base + disp], r8
static void x64_mov_mr8(int base, int disp, int src) {
    int rex = x64_rex_rr(src, base, 0);
    if (x64_needs_rex_byte(src)) rex = rex | 0;  // force REX if needed
    if (rex || x64_needs_rex_byte(src)) x64_byte(REX_BASE | rex);
    x64_byte(0x88);
    x64_modrm_mem(src, base, disp);
}

// mov word [base + disp], r16
static void x64_mov_mr16(int base, int disp, int src) {
    x64_byte(0x66);  // operand size prefix
    x64_rex_emit(x64_rex_rr(src, base, 0));
    x64_byte(0x89);
    x64_modrm_mem(src, base, disp);
}

// movzx r32, r8 (zero-extend byte to dword)
static void x64_movzx_rr8(int dst, int src) {
    int rex = x64_rex_rr(dst, src, 0);
    if (x64_needs_rex_byte(src)) rex = rex | 0;
    if (rex || x64_needs_rex_byte(src)) x64_byte(REX_BASE | rex);
    x64_byte(0x0F);
    x64_byte(0xB6);
    x64_byte(MODRM(MOD_DIRECT, dst, src));
}

// movsx r32, r8 (sign-extend byte to dword)
static void x64_movsx_rr8(int dst, int src) {
    int rex = x64_rex_rr(dst, src, 0);
    if (x64_needs_rex_byte(src)) rex = rex | 0;
    if (rex || x64_needs_rex_byte(src)) x64_byte(REX_BASE | rex);
    x64_byte(0x0F);
    x64_byte(0xBE);
    x64_byte(MODRM(MOD_DIRECT, dst, src));
}

// movzx r32, r16 (zero-extend word to dword)
static void x64_movzx_rr16(int dst, int src) {
    x64_rex_emit(x64_rex_rr(dst, src, 0));
    x64_byte(0x0F);
    x64_byte(0xB7);
    x64_byte(MODRM(MOD_DIRECT, dst, src));
}

// movsx r32, r16 (sign-extend word to dword)
static void x64_movsx_rr16(int dst, int src) {
    x64_rex_emit(x64_rex_rr(dst, src, 0));
    x64_byte(0x0F);
    x64_byte(0xBF);
    x64_byte(MODRM(MOD_DIRECT, dst, src));
}

// movsxd r64, r32 (sign-extend dword to qword)
static void x64_movsxd(int dst, int src) {
    x64_byte(REX_BASE | x64_rex_rr(dst, src, 1));
    x64_byte(0x63);
    x64_byte(MODRM(MOD_DIRECT, dst, src));
}

// lea r64, [base + disp]
static void x64_lea(int dst, int base, int disp) {
    x64_byte(REX_BASE | x64_rex_rr(dst, base, 1));
    x64_byte(0x8D);
    x64_modrm_mem(dst, base, disp);
}

// lea r32, [base + disp] (32-bit)
static void x64_lea32(int dst, int base, int disp) {
    x64_rex_emit(x64_rex_rr(dst, base, 0));
    x64_byte(0x8D);
    x64_modrm_mem(dst, base, disp);
}

// ============================================================================
// ALU — reg, reg forms
// ============================================================================

// Generic: op r32, r32  (opcode /r)
static void x64_alu_rr(int opcode, int dst, int src) {
    x64_rex_emit(x64_rex_rr(src, dst, 0));
    x64_byte(opcode);
    x64_byte(MODRM(MOD_DIRECT, src, dst));
}

static void x64_add_rr(int dst, int src) { x64_alu_rr(0x01, dst, src); }
static void x64_sub_rr(int dst, int src) { x64_alu_rr(0x29, dst, src); }
static void x64_and_rr(int dst, int src) { x64_alu_rr(0x21, dst, src); }
static void x64_or_rr(int dst, int src)  { x64_alu_rr(0x09, dst, src); }
static void x64_xor_rr(int dst, int src) { x64_alu_rr(0x31, dst, src); }
static void x64_cmp_rr(int a, int b)     { x64_alu_rr(0x39, a, b); }
static void x64_test_rr(int a, int b)    { x64_alu_rr(0x85, a, b); }

// 64-bit ALU: op r64, r64
static void x64_alu_rr64(int opcode, int dst, int src) {
    x64_byte(REX_BASE | x64_rex_rr(src, dst, 1));
    x64_byte(opcode);
    x64_byte(MODRM(MOD_DIRECT, src, dst));
}

static void x64_add_rr64(int dst, int src) { x64_alu_rr64(0x01, dst, src); }
static void x64_sub_rr64(int dst, int src) { x64_alu_rr64(0x29, dst, src); }
static void x64_and_rr64(int dst, int src) { x64_alu_rr64(0x21, dst, src); }
static void x64_or_rr64(int dst, int src)  { x64_alu_rr64(0x09, dst, src); }
static void x64_xor_rr64(int dst, int src) { x64_alu_rr64(0x31, dst, src); }
static void x64_cmp_rr64(int a, int b)     { x64_alu_rr64(0x39, a, b); }
static void x64_test_rr64(int a, int b)    { x64_alu_rr64(0x85, a, b); }

// ============================================================================
// ALU — reg, [base + disp] forms (memory source operand)
// ============================================================================

// Generic: op r32, [base + disp]  (opcode+2 /r for reg-src direction)
static void x64_alu_rm(int opcode, int dst, int base, int disp) {
    x64_rex_emit(x64_rex_rr(dst, base, 0));
    x64_byte(opcode + 2);  /* direction bit: reg is destination */
    x64_modrm_mem(dst, base, disp);
}

static void x64_add_rm(int dst, int base, int disp) { x64_alu_rm(0x01, dst, base, disp); }
static void x64_sub_rm(int dst, int base, int disp) { x64_alu_rm(0x29, dst, base, disp); }
static void x64_and_rm(int dst, int base, int disp) { x64_alu_rm(0x21, dst, base, disp); }
static void x64_or_rm(int dst, int base, int disp)  { x64_alu_rm(0x09, dst, base, disp); }
static void x64_xor_rm(int dst, int base, int disp) { x64_alu_rm(0x31, dst, base, disp); }
static void x64_cmp_rm(int dst, int base, int disp) { x64_alu_rm(0x39, dst, base, disp); }

// ============================================================================
// ALU — reg, imm forms
// ============================================================================

// Generic: op r32, imm  (83 /N ib or 81 /N id)
static void x64_alu_ri(int slash, int dst, int imm) {
    int rex = 0;
    if (dst >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);

    if (imm >= -128 && imm <= 127) {
        x64_byte(0x83);
        x64_byte(MODRM(MOD_DIRECT, slash, dst));
        x64_byte(imm & 0xFF);
    } else {
        x64_byte(0x81);
        x64_byte(MODRM(MOD_DIRECT, slash, dst));
        x64_dword(imm);
    }
}

static void x64_add_ri(int dst, int imm) { x64_alu_ri(0, dst, imm); }
static void x64_or_ri(int dst, int imm)  { x64_alu_ri(1, dst, imm); }
static void x64_and_ri(int dst, int imm) { x64_alu_ri(4, dst, imm); }
static void x64_sub_ri(int dst, int imm) { x64_alu_ri(5, dst, imm); }
static void x64_xor_ri(int dst, int imm) { x64_alu_ri(6, dst, imm); }
static void x64_cmp_ri(int dst, int imm) { x64_alu_ri(7, dst, imm); }

// 64-bit: op r64, imm  (REX.W 83/81 /N)
static void x64_alu_ri64(int slash, int dst, int imm) {
    int rex = REX_W;
    if (dst >= 8) rex = rex | REX_B;
    x64_byte(REX_BASE | rex);

    if (imm >= -128 && imm <= 127) {
        x64_byte(0x83);
        x64_byte(MODRM(MOD_DIRECT, slash, dst));
        x64_byte(imm & 0xFF);
    } else {
        x64_byte(0x81);
        x64_byte(MODRM(MOD_DIRECT, slash, dst));
        x64_dword(imm);
    }
}

static void x64_add_ri64(int dst, int imm) { x64_alu_ri64(0, dst, imm); }
static void x64_or_ri64(int dst, int imm)  { x64_alu_ri64(1, dst, imm); }
static void x64_and_ri64(int dst, int imm) { x64_alu_ri64(4, dst, imm); }
static void x64_sub_ri64(int dst, int imm) { x64_alu_ri64(5, dst, imm); }
static void x64_xor_ri64(int dst, int imm) { x64_alu_ri64(6, dst, imm); }
static void x64_cmp_ri64(int dst, int imm) { x64_alu_ri64(7, dst, imm); }

// ============================================================================
// Unary ALU
// ============================================================================

// neg r32 (F7 /3)
static void x64_neg(int dst) {
    int rex = 0;
    if (dst >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xF7);
    x64_byte(MODRM(MOD_DIRECT, 3, dst));
}

// neg r64 (REX.W F7 /3)
static void x64_neg64(int dst) {
    int rex = REX_W;
    if (dst >= 8) rex = rex | REX_B;
    x64_byte(REX_BASE | rex);
    x64_byte(0xF7);
    x64_byte(MODRM(MOD_DIRECT, 3, dst));
}

// not r32 (F7 /2)
static void x64_not(int dst) {
    int rex = 0;
    if (dst >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xF7);
    x64_byte(MODRM(MOD_DIRECT, 2, dst));
}

// not r64 (REX.W F7 /2)
static void x64_not64(int dst) {
    int rex = REX_W;
    if (dst >= 8) rex = rex | REX_B;
    x64_byte(REX_BASE | rex);
    x64_byte(0xF7);
    x64_byte(MODRM(MOD_DIRECT, 2, dst));
}

// ============================================================================
// Multiply / Divide
// ============================================================================

// imul r32, r32  (0F AF /r)  — two-operand: dst *= src
static void x64_imul_rr(int dst, int src) {
    x64_rex_emit(x64_rex_rr(dst, src, 0));
    x64_byte(0x0F);
    x64_byte(0xAF);
    x64_byte(MODRM(MOD_DIRECT, dst, src));
}

// imul r64, r64  (REX.W 0F AF /r)
static void x64_imul_rr64(int dst, int src) {
    x64_byte(REX_BASE | x64_rex_rr(dst, src, 1));
    x64_byte(0x0F);
    x64_byte(0xAF);
    x64_byte(MODRM(MOD_DIRECT, dst, src));
}

// imul r32, r32, imm32  (69 /r id or 6B /r ib)  — three-operand
static void x64_imul_rri(int dst, int src, int imm) {
    x64_rex_emit(x64_rex_rr(dst, src, 0));
    if (imm >= -128 && imm <= 127) {
        x64_byte(0x6B);
        x64_byte(MODRM(MOD_DIRECT, dst, src));
        x64_byte(imm & 0xFF);
    } else {
        x64_byte(0x69);
        x64_byte(MODRM(MOD_DIRECT, dst, src));
        x64_dword(imm);
    }
}

// mul r32  (F7 /4)  — unsigned: edx:eax = eax * src
static void x64_mul(int src) {
    int rex = 0;
    if (src >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xF7);
    x64_byte(MODRM(MOD_DIRECT, 4, src));
}

// imul r32  (F7 /5)  — signed one-operand: edx:eax = eax * src
static void x64_imul1(int src) {
    int rex = 0;
    if (src >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xF7);
    x64_byte(MODRM(MOD_DIRECT, 5, src));
}

// idiv r32  (F7 /7)  — signed: eax=quotient, edx=remainder
static void x64_idiv(int src) {
    int rex = 0;
    if (src >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xF7);
    x64_byte(MODRM(MOD_DIRECT, 7, src));
}

// idiv r64  (REX.W F7 /7)  — signed 64-bit
static void x64_idiv64(int src) {
    int rex = REX_W;
    if (src >= 8) rex = rex | REX_B;
    x64_byte(REX_BASE | rex);
    x64_byte(0xF7);
    x64_byte(MODRM(MOD_DIRECT, 7, src));
}

// div r32  (F7 /6)  — unsigned
static void x64_div(int src) {
    int rex = 0;
    if (src >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xF7);
    x64_byte(MODRM(MOD_DIRECT, 6, src));
}

// div r64  (REX.W F7 /6)  — unsigned 64-bit
static void x64_div64(int src) {
    int rex = REX_W;
    if (src >= 8) rex = rex | REX_B;
    x64_byte(REX_BASE | rex);
    x64_byte(0xF7);
    x64_byte(MODRM(MOD_DIRECT, 6, src));
}

// cdq  (99)  — sign-extend eax into edx:eax
static void x64_cdq(void) {
    x64_byte(0x99);
}

// cqo  (REX.W 99)  — sign-extend rax into rdx:rax
static void x64_cqo(void) {
    x64_byte(REX_BASE | REX_W);
    x64_byte(0x99);
}

// xor eax, eax — common idiom to zero edx before unsigned div
static void x64_zero(int r) {
    x64_xor_rr(r, r);
}

// ============================================================================
// Shifts
// ============================================================================

// shl r32, imm8  (C1 /4 ib)
static void x64_shl_ri(int dst, int imm) {
    int rex = 0;
    if (dst >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xC1);
    x64_byte(MODRM(MOD_DIRECT, 4, dst));
    x64_byte(imm & 0x1F);
}

// shl r32, cl  (D3 /4)
static void x64_shl_cl(int dst) {
    int rex = 0;
    if (dst >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xD3);
    x64_byte(MODRM(MOD_DIRECT, 4, dst));
}

// shl r64, cl  (REX.W D3 /4)
static void x64_shl_cl64(int dst) {
    int rex = REX_W;
    if (dst >= 8) rex = rex | REX_B;
    x64_byte(REX_BASE | rex);
    x64_byte(0xD3);
    x64_byte(MODRM(MOD_DIRECT, 4, dst));
}

// shr r32, imm8  (C1 /5 ib)
static void x64_shr_ri(int dst, int imm) {
    int rex = 0;
    if (dst >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xC1);
    x64_byte(MODRM(MOD_DIRECT, 5, dst));
    x64_byte(imm & 0x1F);
}

// shr r32, cl  (D3 /5)
static void x64_shr_cl(int dst) {
    int rex = 0;
    if (dst >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xD3);
    x64_byte(MODRM(MOD_DIRECT, 5, dst));
}

// shr r64, cl  (REX.W D3 /5)
static void x64_shr_cl64(int dst) {
    int rex = REX_W;
    if (dst >= 8) rex = rex | REX_B;
    x64_byte(REX_BASE | rex);
    x64_byte(0xD3);
    x64_byte(MODRM(MOD_DIRECT, 5, dst));
}

// sar r32, imm8  (C1 /7 ib)
static void x64_sar_ri(int dst, int imm) {
    int rex = 0;
    if (dst >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xC1);
    x64_byte(MODRM(MOD_DIRECT, 7, dst));
    x64_byte(imm & 0x1F);
}

// sar r32, cl  (D3 /7)
static void x64_sar_cl(int dst) {
    int rex = 0;
    if (dst >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xD3);
    x64_byte(MODRM(MOD_DIRECT, 7, dst));
}

// sar r64, cl  (REX.W D3 /7)
static void x64_sar_cl64(int dst) {
    int rex = REX_W;
    if (dst >= 8) rex = rex | REX_B;
    x64_byte(REX_BASE | rex);
    x64_byte(0xD3);
    x64_byte(MODRM(MOD_DIRECT, 7, dst));
}

// ============================================================================
// Setcc — set byte to 0/1 based on condition
// ============================================================================

// Generic setcc (0F xx /0)
static void x64_setcc(int cc, int dst) {
    int rex = 0;
    if (dst >= 8) rex = rex | REX_B;
    if (rex || x64_needs_rex_byte(dst)) x64_byte(REX_BASE | rex);
    x64_byte(0x0F);
    x64_byte(cc);
    x64_byte(MODRM(MOD_DIRECT, 0, dst));
}

static void x64_sete(int dst)  { x64_setcc(0x94, dst); }
static void x64_setne(int dst) { x64_setcc(0x95, dst); }
static void x64_setl(int dst)  { x64_setcc(0x9C, dst); }
static void x64_setge(int dst) { x64_setcc(0x9D, dst); }
static void x64_setg(int dst)  { x64_setcc(0x9F, dst); }
static void x64_setle(int dst) { x64_setcc(0x9E, dst); }
static void x64_setb(int dst)  { x64_setcc(0x92, dst); }
static void x64_setae(int dst) { x64_setcc(0x93, dst); }
static void x64_seta(int dst)  { x64_setcc(0x97, dst); }
static void x64_setbe(int dst) { x64_setcc(0x96, dst); }

// ============================================================================
// Control flow
// ============================================================================

// jmp rel32  (E9 cd)
static void x64_jmp_rel32(int offset) {
    x64_byte(0xE9);
    x64_dword(offset);
}

// jmp r64  (FF /4)
static void x64_jmp_r(int target) {
    int rex = 0;
    if (target >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xFF);
    x64_byte(MODRM(MOD_DIRECT, 4, target));
}

// Generic jcc rel32  (0F cc cd)
static void x64_jcc_rel32(int cc, int offset) {
    x64_byte(0x0F);
    x64_byte(cc);
    x64_dword(offset);
}

static void x64_je(int off)  { x64_jcc_rel32(0x84, off); }
static void x64_jne(int off) { x64_jcc_rel32(0x85, off); }
static void x64_jl(int off)  { x64_jcc_rel32(0x8C, off); }
static void x64_jge(int off) { x64_jcc_rel32(0x8D, off); }
static void x64_jg(int off)  { x64_jcc_rel32(0x8F, off); }
static void x64_jle(int off) { x64_jcc_rel32(0x8E, off); }
static void x64_jb(int off)  { x64_jcc_rel32(0x82, off); }
static void x64_jae(int off) { x64_jcc_rel32(0x83, off); }
static void x64_ja(int off)  { x64_jcc_rel32(0x87, off); }
static void x64_jbe(int off) { x64_jcc_rel32(0x86, off); }

// call rel32  (E8 cd)
static void x64_call_rel32(int offset) {
    x64_byte(0xE8);
    x64_dword(offset);
}

// call r64  (FF /2)
static void x64_call_r(int target) {
    int rex = 0;
    if (target >= 8) rex = rex | REX_B;
    x64_rex_emit(rex);
    x64_byte(0xFF);
    x64_byte(MODRM(MOD_DIRECT, 2, target));
}

// ret  (C3)
static void x64_ret(void) {
    x64_byte(0xC3);
}

// ============================================================================
// Stack
// ============================================================================

// push r64  (50+rd)
static void x64_push(int reg) {
    if (reg >= 8) x64_byte(REX_BASE | REX_B);
    x64_byte(0x50 + (reg & 7));
}

// pop r64  (58+rd)
static void x64_pop(int reg) {
    if (reg >= 8) x64_byte(REX_BASE | REX_B);
    x64_byte(0x58 + (reg & 7));
}

// ============================================================================
// Misc
// ============================================================================

// nop  (90)
static void x64_nop(void) { x64_byte(0x90); }

// int3  (CC)
static void x64_int3(void) { x64_byte(0xCC); }

// syscall  (0F 05)
static void x64_syscall(void) {
    x64_byte(0x0F);
    x64_byte(0x05);
}

// ============================================================================
// Patching
// ============================================================================

// Patch a rel32 displacement at the given buffer offset.
// target_off is the offset of the instruction AFTER the one being patched.
static void x64_patch_rel32(int patch_off, int target_off) {
    int rel;
    rel = target_off - (patch_off + 4);
    x64_buf[patch_off]     = rel & 0xFF;
    x64_buf[patch_off + 1] = (rel >> 8) & 0xFF;
    x64_buf[patch_off + 2] = (rel >> 16) & 0xFF;
    x64_buf[patch_off + 3] = (rel >> 24) & 0xFF;
}

// Emit a jmp/jcc with placeholder rel32, return offset of the rel32 field
// for later patching. Example:
//   int patch = x64_je_placeholder();
//   ... emit code ...
//   x64_patch_rel32(patch, x64_pos());
static int x64_jmp_placeholder(void) {
    int patch;
    x64_byte(0xE9);
    patch = x64_off;
    x64_dword(0);
    return patch;
}

static int x64_jcc_placeholder(int cc) {
    int patch;
    x64_byte(0x0F);
    x64_byte(cc);
    patch = x64_off;
    x64_dword(0);
    return patch;
}

static int x64_call_placeholder(void) {
    int patch;
    x64_byte(0xE8);
    patch = x64_off;
    x64_dword(0);
    return patch;
}

// Condition code constants (for jcc/setcc)
#define X64_CC_E   0x84
#define X64_CC_NE  0x85
#define X64_CC_L   0x8C
#define X64_CC_GE  0x8D
#define X64_CC_G   0x8F
#define X64_CC_LE  0x8E
#define X64_CC_B   0x82
#define X64_CC_AE  0x83
#define X64_CC_A   0x87
#define X64_CC_BE  0x86

// ============================================================================
// Alignment
// ============================================================================

// Align code position to N-byte boundary with NOPs
static void x64_align(int n) {
    while (x64_off & (n - 1))
        x64_nop();
}

#endif // X64_ENCODE_H
