// SLOW-32 DBT: Paranoid Mode — Lockstep Shadow Interpreter
//
// Pure-C interpreter adapted from selfhost/stage00/s32-emu.c.
// Stores go to a shadow buffer (not real memory); loads check the buffer
// first (store forwarding) then fall back to real guest memory.
// After each block, registers/PC/stores are compared with the DBT result.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <math.h>

#include "shadow_interp.h"
#include "translate.h"      // decode_instruction, OP_* constants

bool paranoid_mode = false;
bool dbt_no_chain = false;

// ============================================================================
// Initialization
// ============================================================================

void shadow_init(shadow_state_t *s, dbt_cpu_state_t *cpu) {
    memset(s, 0, sizeof(*s));
    s->mem_base = cpu->mem_base;
    s->mem_size = cpu->mem_size;
    s->code_limit = cpu->code_limit;
    s->rodata_limit = cpu->rodata_limit;
    s->mmio_base = cpu->mmio_base;
    s->mmio_enabled = cpu->mmio_enabled;
    s->wxorx_enabled = cpu->wxorx_enabled;
    s->align_traps_enabled = cpu->align_traps_enabled;
    s->bounds_checks_disabled = cpu->bounds_checks_disabled;
    s->enabled = true;
    s->check_memory = true;

    // Copy intrinsic addresses for skip detection
    s->intrinsic_memcpy  = cpu->intrinsic_memcpy;
    s->intrinsic_memset  = cpu->intrinsic_memset;
    s->intrinsic_memmove = cpu->intrinsic_memmove;
    s->intrinsic_strlen  = cpu->intrinsic_strlen;
    s->intrinsic_memswap = cpu->intrinsic_memswap;

    // Copy math intercept addresses
    s->num_intercepts = cpu->num_intercepts;
    for (int i = 0; i < cpu->num_intercepts && i < MAX_INTERCEPTS; i++) {
        s->intercept_addrs[i] = cpu->intercepts[i].guest_addr;
    }
}

// ============================================================================
// Store buffer helpers
// ============================================================================

static void shadow_store_byte(shadow_state_t *s, uint32_t addr, uint8_t val) {
    // Check for existing entry at same address
    for (int i = 0; i < s->store_buf_count; i++) {
        if (s->store_buf[i].addr == addr) {
            s->store_buf[i].value = val;
            return;
        }
    }
    if (s->store_buf_count < SHADOW_STORE_BUF_SIZE) {
        s->store_buf[s->store_buf_count].addr = addr;
        s->store_buf[s->store_buf_count].value = val;
        s->store_buf_count++;
    } else {
        fprintf(stderr, "PARANOID: store buffer overflow at addr 0x%08X\n", addr);
    }
}

static void shadow_store16(shadow_state_t *s, uint32_t addr, uint16_t val) {
    shadow_store_byte(s, addr,     (uint8_t)(val));
    shadow_store_byte(s, addr + 1, (uint8_t)(val >> 8));
}

static void shadow_store32(shadow_state_t *s, uint32_t addr, uint32_t val) {
    shadow_store_byte(s, addr,     (uint8_t)(val));
    shadow_store_byte(s, addr + 1, (uint8_t)(val >> 8));
    shadow_store_byte(s, addr + 2, (uint8_t)(val >> 16));
    shadow_store_byte(s, addr + 3, (uint8_t)(val >> 24));
}

// Load with store forwarding: check shadow buffer first, then real memory
static uint8_t shadow_load_byte(shadow_state_t *s, uint32_t addr) {
    // Search buffer in reverse for most recent store
    for (int i = s->store_buf_count - 1; i >= 0; i--) {
        if (s->store_buf[i].addr == addr) {
            return s->store_buf[i].value;
        }
    }
    if (addr < s->mem_size) {
        return s->mem_base[addr];
    }
    return 0;
}

static uint16_t shadow_load16(shadow_state_t *s, uint32_t addr) {
    return (uint16_t)shadow_load_byte(s, addr) |
           ((uint16_t)shadow_load_byte(s, addr + 1) << 8);
}

static uint32_t shadow_load32(shadow_state_t *s, uint32_t addr) {
    return (uint32_t)shadow_load_byte(s, addr) |
           ((uint32_t)shadow_load_byte(s, addr + 1) << 8) |
           ((uint32_t)shadow_load_byte(s, addr + 2) << 16) |
           ((uint32_t)shadow_load_byte(s, addr + 3) << 24);
}

// Mirror fixed-size DBT memory checks (bounds/alignment/MMIO/W^X).
// Returns true if access would fault.
static bool shadow_mem_access_fault(shadow_state_t *s, uint32_t addr,
                                    uint32_t size, bool is_store) {
    if (s->bounds_checks_disabled) return false;
    if (s->mem_size < size) return true;

    uint32_t limit = s->mem_size - size;
    if (addr > limit) return true;

    if (s->align_traps_enabled && size > 1 && (addr & (size - 1)) != 0) {
        return true;
    }

    if (!s->mmio_enabled && s->mmio_base != 0) {
        if (addr >= s->mmio_base && addr < s->mmio_base + 0x10000) {
            return true;
        }
    }

    if (is_store && s->wxorx_enabled) {
        uint32_t wx_limit = s->rodata_limit ? s->rodata_limit : s->code_limit;
        if (wx_limit > 0 && addr < wx_limit) {
            return true;
        }
    }

    return false;
}

// ============================================================================
// Shadow single-step (adapted from s32-emu.c:step)
// ============================================================================

// Returns true if the instruction was a block-ender (branch/jump/halt/debug/yield)
static bool shadow_step(shadow_state_t *s) {
    uint32_t pc = s->pc;
    if (pc + 4 > s->mem_size) {
        fprintf(stderr, "PARANOID: shadow PC out of bounds: 0x%08X\n", pc);
        return true;
    }

    // Fetch from real memory (code is read-only, no store forwarding needed)
    uint32_t raw = *(uint32_t *)(s->mem_base + pc);

    uint32_t op  = raw & 0x7F;
    uint32_t rd  = (raw >> 7) & 0x1F;
    uint32_t rs1 = (raw >> 15) & 0x1F;
    uint32_t rs2 = (raw >> 20) & 0x1F;

    // Pre-compute immediates (same as s32-emu.c)
    int32_t  imm_i  = ((int32_t)raw) >> 20;                    // sign-extended
    uint32_t imm_iz = (raw >> 20) & 0xFFFu;                    // zero-extended

    int32_t  imm_s  = (int32_t)(((raw >> 7) & 0x1Fu) |
                                (((raw >> 25) & 0x7Fu) << 5));
    if (imm_s & 0x800) imm_s |= (int32_t)0xFFFFF000;

    int32_t  imm_b  = (int32_t)((((raw >> 8) & 0xFu) << 1) |
                                (((raw >> 25) & 0x3Fu) << 5) |
                                (((raw >> 7) & 0x1u) << 11) |
                                (((raw >> 31) & 0x1u) << 12));
    if (imm_b & 0x1000) imm_b |= (int32_t)0xFFFFE000;

    uint32_t imm_u  = raw & 0xFFFFF000u;

    uint32_t jbits  = (((raw >> 31) & 1) << 20)
                    | (((raw >> 12) & 0xFF) << 12)
                    | (((raw >> 20) & 1) << 11)
                    | (((raw >> 21) & 0x3FF) << 1);
    int32_t  imm_j  = (int32_t)jbits;
    if (imm_j & 0x100000) imm_j |= (int32_t)0xFFE00000;

    uint32_t next_pc = pc + 4;
    uint32_t *r = s->regs;
    bool block_end = false;

    switch (op) {
    // R-type arithmetic (0x00-0x0F)
    case 0x00: r[rd] = r[rs1] + r[rs2]; break;                         // ADD
    case 0x01: r[rd] = r[rs1] - r[rs2]; break;                         // SUB
    case 0x02: r[rd] = r[rs1] ^ r[rs2]; break;                         // XOR
    case 0x03: r[rd] = r[rs1] | r[rs2]; break;                         // OR
    case 0x04: r[rd] = r[rs1] & r[rs2]; break;                         // AND
    case 0x05: r[rd] = r[rs1] << (r[rs2] & 0x1F); break;               // SLL
    case 0x06: r[rd] = r[rs1] >> (r[rs2] & 0x1F); break;               // SRL
    case 0x07: r[rd] = (uint32_t)((int32_t)r[rs1] >> (r[rs2] & 0x1F)); break; // SRA
    case 0x08: r[rd] = ((int32_t)r[rs1] < (int32_t)r[rs2]) ? 1 : 0; break;   // SLT
    case 0x09: r[rd] = (r[rs1] < r[rs2]) ? 1 : 0; break;                      // SLTU
    case 0x0A: r[rd] = r[rs1] * r[rs2]; break;                         // MUL
    case 0x0B: { // MULH (signed)
        int64_t p = (int64_t)(int32_t)r[rs1] * (int64_t)(int32_t)r[rs2];
        r[rd] = (uint32_t)(p >> 32);
        break;
    }
    case 0x0C: // DIV
        if (r[rs2] == 0) r[rd] = 0xFFFFFFFF;
        else if (r[rs1] == 0x80000000 && r[rs2] == 0xFFFFFFFF) r[rd] = 0x80000000;
        else r[rd] = (uint32_t)((int32_t)r[rs1] / (int32_t)r[rs2]);
        break;
    case 0x0D: // REM
        if (r[rs2] == 0) r[rd] = r[rs1];
        else if (r[rs1] == 0x80000000 && r[rs2] == 0xFFFFFFFF) r[rd] = 0;
        else r[rd] = (uint32_t)((int32_t)r[rs1] % (int32_t)r[rs2]);
        break;
    case 0x0E: r[rd] = (r[rs1] == r[rs2]) ? 1 : 0; break;             // SEQ
    case 0x0F: r[rd] = (r[rs1] != r[rs2]) ? 1 : 0; break;             // SNE

    // I-type arithmetic (0x10-0x1F)
    case 0x10: r[rd] = r[rs1] + imm_i; break;                          // ADDI
    case 0x11: r[rd] = r[rs1] | imm_iz; break;                         // ORI
    case 0x12: r[rd] = r[rs1] & imm_iz; break;                         // ANDI
    case 0x13: r[rd] = r[rs1] << (imm_i & 0x1F); break;                // SLLI
    case 0x14: r[rd] = r[rs1] >> (imm_i & 0x1F); break;                // SRLI
    case 0x15: r[rd] = (uint32_t)((int32_t)r[rs1] >> (imm_i & 0x1F)); break; // SRAI
    case 0x16: r[rd] = ((int32_t)r[rs1] < imm_i) ? 1 : 0; break;      // SLTI
    case 0x17: r[rd] = (r[rs1] < imm_iz) ? 1 : 0; break;              // SLTIU
    case 0x18: r[rd] = ((int32_t)r[rs1] > (int32_t)r[rs2]) ? 1 : 0; break;   // SGT
    case 0x19: r[rd] = (r[rs1] > r[rs2]) ? 1 : 0; break;              // SGTU
    case 0x1A: r[rd] = ((int32_t)r[rs1] <= (int32_t)r[rs2]) ? 1 : 0; break;  // SLE
    case 0x1B: r[rd] = (r[rs1] <= r[rs2]) ? 1 : 0; break;             // SLEU
    case 0x1C: r[rd] = ((int32_t)r[rs1] >= (int32_t)r[rs2]) ? 1 : 0; break;  // SGE
    case 0x1D: r[rd] = (r[rs1] >= r[rs2]) ? 1 : 0; break;             // SGEU
    case 0x1E: r[rd] = r[rs1] ^ imm_iz; break;                        // XORI
    case 0x1F: { // MULHU (unsigned)
        uint64_t p = (uint64_t)r[rs1] * (uint64_t)r[rs2];
        r[rd] = (uint32_t)(p >> 32);
        break;
    }

    // U-type (0x20)
    case 0x20: r[rd] = imm_u; break;                                   // LUI

    // Load instructions (0x30-0x34)
    case 0x30: { // LDB (sign-extend)
        uint32_t a = r[rs1] + imm_i;
        if (shadow_mem_access_fault(s, a, 1, false)) {
            block_end = true;
            next_pc = pc;
            break;
        }
        r[rd] = (uint32_t)(int32_t)(int8_t)shadow_load_byte(s, a);
        break;
    }
    case 0x31: { // LDH (sign-extend)
        uint32_t a = r[rs1] + imm_i;
        if (shadow_mem_access_fault(s, a, 2, false)) {
            block_end = true;
            next_pc = pc;
            break;
        }
        r[rd] = (uint32_t)(int32_t)(int16_t)shadow_load16(s, a);
        break;
    }
    case 0x32: { // LDW
        uint32_t a = r[rs1] + imm_i;
        if (shadow_mem_access_fault(s, a, 4, false)) {
            block_end = true;
            next_pc = pc;
            break;
        }
        r[rd] = shadow_load32(s, a);
        break;
    }
    case 0x33: { // LDBU (zero-extend)
        uint32_t a = r[rs1] + imm_i;
        if (shadow_mem_access_fault(s, a, 1, false)) {
            block_end = true;
            next_pc = pc;
            break;
        }
        r[rd] = shadow_load_byte(s, a);
        break;
    }
    case 0x34: { // LDHU (zero-extend)
        uint32_t a = r[rs1] + imm_i;
        if (shadow_mem_access_fault(s, a, 2, false)) {
            block_end = true;
            next_pc = pc;
            break;
        }
        r[rd] = shadow_load16(s, a);
        break;
    }

    // Store instructions (0x38-0x3A)
    case 0x38: { // STB
        uint32_t a = r[rs1] + imm_s;
        if (shadow_mem_access_fault(s, a, 1, true)) {
            block_end = true;
            next_pc = pc;
            break;
        }
        shadow_store_byte(s, a, (uint8_t)r[rs2]);
        break;
    }
    case 0x39: { // STH
        uint32_t a = r[rs1] + imm_s;
        if (shadow_mem_access_fault(s, a, 2, true)) {
            block_end = true;
            next_pc = pc;
            break;
        }
        shadow_store16(s, a, (uint16_t)r[rs2]);
        break;
    }
    case 0x3A: { // STW
        uint32_t a = r[rs1] + imm_s;
        if (shadow_mem_access_fault(s, a, 4, true)) {
            block_end = true;
            next_pc = pc;
            break;
        }
        shadow_store32(s, a, r[rs2]);
        break;
    }

    // ASSERT_EQ (0x3F)
    case 0x3F:
        if (r[rs1] != r[rs2]) {
            s->hit_assert_fail = true;
            block_end = true;
        }
        break;

    // JAL (0x40) — not a block-ender in superblocks; outer loop checks PC range
    case 0x40:
        r[rd] = pc + 4;
        next_pc = pc + imm_j;
        break;

    // JALR (0x41) — target may leave block; outer loop checks
    case 0x41:
        r[rd] = pc + 4;
        next_pc = (r[rs1] + imm_i) & ~1u;
        break;

    // Branch instructions (0x48-0x4D) — PC+4 relative; outer loop checks PC range
    case 0x48: if (r[rs1] == r[rs2]) next_pc += imm_b; break;               // BEQ
    case 0x49: if (r[rs1] != r[rs2]) next_pc += imm_b; break;               // BNE
    case 0x4A: if ((int32_t)r[rs1] < (int32_t)r[rs2]) next_pc += imm_b; break;  // BLT
    case 0x4B: if ((int32_t)r[rs1] >= (int32_t)r[rs2]) next_pc += imm_b; break; // BGE
    case 0x4C: if (r[rs1] < r[rs2]) next_pc += imm_b; break;                // BLTU
    case 0x4D: if (r[rs1] >= r[rs2]) next_pc += imm_b; break;               // BGEU

    // System instructions
    case 0x50: break;  // NOP
    case 0x51: // YIELD
        s->hit_yield = true;
        block_end = true;
        break;
    case 0x52: // DEBUG
        s->hit_debug = true;
        s->debug_char = r[rs1] & 0xFF;
        block_end = true;
        break;

    case 0x7F: // HALT
        s->hit_halt = true;
        block_end = true;
        break;

    // ==================================================================
    // Floating-point single (f32) — values in integer registers
    // ==================================================================
    case 0x53: { // FADD.S
        float a, b, res; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4);
        res = a + b; memcpy(&r[rd], &res, 4); break;
    }
    case 0x54: { // FSUB.S
        float a, b, res; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4);
        res = a - b; memcpy(&r[rd], &res, 4); break;
    }
    case 0x55: { // FMUL.S
        float a, b, res; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4);
        res = a * b; memcpy(&r[rd], &res, 4); break;
    }
    case 0x56: { // FDIV.S
        float a, b, res; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4);
        res = a / b; memcpy(&r[rd], &res, 4); break;
    }
    case 0x57: { // FSQRT.S
        float a, res; memcpy(&a, &r[rs1], 4);
        res = sqrtf(a); memcpy(&r[rd], &res, 4); break;
    }
    case 0x58: { // FEQ.S
        float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4);
        r[rd] = (a == b) ? 1 : 0; break;
    }
    case 0x59: { // FLT.S
        float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4);
        r[rd] = (a < b) ? 1 : 0; break;
    }
    case 0x5A: { // FLE.S
        float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4);
        r[rd] = (a <= b) ? 1 : 0; break;
    }
    case 0x5B: { // FCVT.W.S
        float a; memcpy(&a, &r[rs1], 4);
        r[rd] = (uint32_t)(int32_t)a; break;
    }
    case 0x5C: { // FCVT.WU.S
        float a; memcpy(&a, &r[rs1], 4);
        r[rd] = (uint32_t)a; break;
    }
    case 0x5D: { // FCVT.S.W
        float res = (float)(int32_t)r[rs1];
        memcpy(&r[rd], &res, 4); break;
    }
    case 0x5E: { // FCVT.S.WU
        float res = (float)r[rs1];
        memcpy(&r[rd], &res, 4); break;
    }
    case 0x5F: // FNEG.S
        r[rd] = r[rs1] ^ 0x80000000u; break;
    case 0x60: // FABS.S
        r[rd] = r[rs1] & 0x7FFFFFFFu; break;

    // ==================================================================
    // Floating-point double (f64) — register pairs (even-numbered)
    // ==================================================================
    #define SHADOW_LOAD_F64(reg, var) do { \
        uint64_t bits_ = ((uint64_t)r[(reg)+1] << 32) | r[(reg)]; \
        memcpy(&(var), &bits_, 8); \
    } while(0)
    #define SHADOW_STORE_F64(reg, var) do { \
        uint64_t bits_; memcpy(&bits_, &(var), 8); \
        r[(reg)] = (uint32_t)bits_; \
        r[(reg)+1] = (uint32_t)(bits_ >> 32); \
    } while(0)

    case 0x61: { // FADD.D
        double a, b, res; SHADOW_LOAD_F64(rs1, a); SHADOW_LOAD_F64(rs2, b);
        res = a + b; SHADOW_STORE_F64(rd, res); break;
    }
    case 0x62: { // FSUB.D
        double a, b, res; SHADOW_LOAD_F64(rs1, a); SHADOW_LOAD_F64(rs2, b);
        res = a - b; SHADOW_STORE_F64(rd, res); break;
    }
    case 0x63: { // FMUL.D
        double a, b, res; SHADOW_LOAD_F64(rs1, a); SHADOW_LOAD_F64(rs2, b);
        res = a * b; SHADOW_STORE_F64(rd, res); break;
    }
    case 0x64: { // FDIV.D
        double a, b, res; SHADOW_LOAD_F64(rs1, a); SHADOW_LOAD_F64(rs2, b);
        res = a / b; SHADOW_STORE_F64(rd, res); break;
    }
    case 0x65: { // FSQRT.D
        double a, res; SHADOW_LOAD_F64(rs1, a);
        res = sqrt(a); SHADOW_STORE_F64(rd, res); break;
    }
    case 0x66: { // FEQ.D
        double a, b; SHADOW_LOAD_F64(rs1, a); SHADOW_LOAD_F64(rs2, b);
        r[rd] = (a == b) ? 1 : 0; break;
    }
    case 0x67: { // FLT.D
        double a, b; SHADOW_LOAD_F64(rs1, a); SHADOW_LOAD_F64(rs2, b);
        r[rd] = (a < b) ? 1 : 0; break;
    }
    case 0x68: { // FLE.D
        double a, b; SHADOW_LOAD_F64(rs1, a); SHADOW_LOAD_F64(rs2, b);
        r[rd] = (a <= b) ? 1 : 0; break;
    }
    case 0x69: { // FCVT.W.D
        double a; SHADOW_LOAD_F64(rs1, a);
        r[rd] = (uint32_t)(int32_t)a; break;
    }
    case 0x6A: { // FCVT.WU.D
        double a; SHADOW_LOAD_F64(rs1, a);
        r[rd] = (uint32_t)a; break;
    }
    case 0x6B: { // FCVT.D.W
        double res = (double)(int32_t)r[rs1];
        SHADOW_STORE_F64(rd, res); break;
    }
    case 0x6C: { // FCVT.D.WU
        double res = (double)r[rs1];
        SHADOW_STORE_F64(rd, res); break;
    }
    case 0x6D: { // FCVT.D.S
        float a; memcpy(&a, &r[rs1], 4);
        double res = (double)a; SHADOW_STORE_F64(rd, res); break;
    }
    case 0x6E: { // FCVT.S.D
        double a; SHADOW_LOAD_F64(rs1, a);
        float res = (float)a; memcpy(&r[rd], &res, 4); break;
    }
    case 0x6F: // FNEG.D
        r[rd] = r[rs1]; r[rd+1] = r[rs1+1] ^ 0x80000000u; break;
    case 0x70: // FABS.D
        r[rd] = r[rs1]; r[rd+1] = r[rs1+1] & 0x7FFFFFFFu; break;
    // float <-> int64 conversions
    case 0x71: { // FCVT.L.S
        float a; memcpy(&a, &r[rs1], 4);
        int64_t res = (int64_t)a;
        r[rd] = (uint32_t)res; r[rd+1] = (uint32_t)((uint64_t)res >> 32); break;
    }
    case 0x72: { // FCVT.LU.S
        float a; memcpy(&a, &r[rs1], 4);
        uint64_t res = (uint64_t)a;
        r[rd] = (uint32_t)res; r[rd+1] = (uint32_t)(res >> 32); break;
    }
    case 0x73: { // FCVT.S.L
        int64_t val = (int64_t)(((uint64_t)r[rs1+1] << 32) | r[rs1]);
        float res = (float)val; memcpy(&r[rd], &res, 4); break;
    }
    case 0x74: { // FCVT.S.LU
        uint64_t val = ((uint64_t)r[rs1+1] << 32) | r[rs1];
        float res = (float)val; memcpy(&r[rd], &res, 4); break;
    }
    case 0x75: { // FCVT.L.D
        double a; SHADOW_LOAD_F64(rs1, a);
        int64_t res = (int64_t)a;
        r[rd] = (uint32_t)res; r[rd+1] = (uint32_t)((uint64_t)res >> 32); break;
    }
    case 0x76: { // FCVT.LU.D
        double a; SHADOW_LOAD_F64(rs1, a);
        uint64_t res = (uint64_t)a;
        r[rd] = (uint32_t)res; r[rd+1] = (uint32_t)(res >> 32); break;
    }
    case 0x77: { // FCVT.D.L
        int64_t val = (int64_t)(((uint64_t)r[rs1+1] << 32) | r[rs1]);
        double res = (double)val; SHADOW_STORE_F64(rd, res); break;
    }
    case 0x78: { // FCVT.D.LU
        uint64_t val = ((uint64_t)r[rs1+1] << 32) | r[rs1];
        double res = (double)val; SHADOW_STORE_F64(rd, res); break;
    }

    #undef SHADOW_LOAD_F64
    #undef SHADOW_STORE_F64

    default:
        fprintf(stderr, "PARANOID: unknown opcode 0x%02X at shadow PC=0x%08X\n", op, pc);
        block_end = true;
        break;
    }

    r[0] = 0;  // r0 always zero
    s->pc = next_pc;
    return block_end;
}

// ============================================================================
// Block-level execution
// ============================================================================

// Execute the block via shadow interpreter, matching the DBT's control flow.
//
// The DBT translates instructions linearly.  Any non-sequential PC change
// (branch taken, jump) is a block exit only when the target leaves the
// translated block range.
//
// So the shadow:
//   1. Steps forward, checking PC advances by +4 (sequential).
//   2. If PC changes to something other than old_pc+4:
//      a. If new PC remains inside [block_start, block_end) → continue
//      b. Otherwise → block exit, stop
//   3. HALT/DEBUG/YIELD → stop immediately
static void shadow_execute_block(shadow_state_t *s, uint32_t block_start,
                                 uint32_t block_size) {
    uint32_t block_end = block_start + block_size;
    // In paranoid mode, some transforms still keep control flow within the same
    // translated block (for example in-block forward JAL r0 inlining). Match
    // DBT behavior by continuing while the PC stays inside this block range.
    int max_steps = block_size / 4 + 1;

    for (int i = 0; i < max_steps; i++) {
        if (s->pc < block_start || s->pc >= block_end) {
            break;
        }
        uint32_t pc_before = s->pc;
        uint32_t raw_before = *(uint32_t *)(s->mem_base + pc_before);
        bool ended = shadow_step(s);
        s->instructions_verified++;
        if (ended) break;  // HALT/DEBUG/YIELD/ASSERT_FAIL

        if (s->pc != pc_before + 4) {
            uint8_t op = raw_before & 0x7F;
            uint8_t rd = (raw_before >> 7) & 0x1F;
            bool inblock_forward_jal =
                (op == 0x40) && (rd == 0) &&
                (s->pc > pc_before) &&
                (s->pc >= block_start && s->pc < block_end);
            if (!inblock_forward_jal) {
                break;
            }
        }
    }
}

// ============================================================================
// Disassemble helper for error reporting
// ============================================================================

static const char *opcode_name(uint8_t op) {
    switch (op) {
    case 0x00: return "add";   case 0x01: return "sub";
    case 0x02: return "xor";   case 0x03: return "or";
    case 0x04: return "and";   case 0x05: return "sll";
    case 0x06: return "srl";   case 0x07: return "sra";
    case 0x08: return "slt";   case 0x09: return "sltu";
    case 0x0A: return "mul";   case 0x0B: return "mulh";
    case 0x0C: return "div";   case 0x0D: return "rem";
    case 0x0E: return "seq";   case 0x0F: return "sne";
    case 0x10: return "addi";  case 0x11: return "ori";
    case 0x12: return "andi";  case 0x13: return "slli";
    case 0x14: return "srli";  case 0x15: return "srai";
    case 0x16: return "slti";  case 0x17: return "sltiu";
    case 0x18: return "sgt";   case 0x19: return "sgtu";
    case 0x1A: return "sle";   case 0x1B: return "sleu";
    case 0x1C: return "sge";   case 0x1D: return "sgeu";
    case 0x1E: return "xori";  case 0x1F: return "mulhu";
    case 0x20: return "lui";
    case 0x30: return "ldb";   case 0x31: return "ldh";
    case 0x32: return "ldw";   case 0x33: return "ldbu";
    case 0x34: return "ldhu";
    case 0x38: return "stb";   case 0x39: return "sth";
    case 0x3A: return "stw";
    case 0x3F: return "assert_eq";
    case 0x40: return "jal";   case 0x41: return "jalr";
    case 0x48: return "beq";   case 0x49: return "bne";
    case 0x4A: return "blt";   case 0x4B: return "bge";
    case 0x4C: return "bltu";  case 0x4D: return "bgeu";
    case 0x50: return "nop";   case 0x51: return "yield";
    case 0x52: return "debug"; case 0x7F: return "halt";
    default:   return "???";
    }
}

static void print_instruction(uint32_t pc, uint32_t raw) {
    decoded_inst_t inst = decode_instruction(raw);
    switch (inst.format) {
    case FMT_R:
        fprintf(stderr, "  0x%08X: %-6s r%d, r%d, r%d\n",
                pc, opcode_name(inst.opcode), inst.rd, inst.rs1, inst.rs2);
        break;
    case FMT_I:
        fprintf(stderr, "  0x%08X: %-6s r%d, r%d, %d\n",
                pc, opcode_name(inst.opcode), inst.rd, inst.rs1, inst.imm);
        break;
    case FMT_S:
        fprintf(stderr, "  0x%08X: %-6s r%d, %d(r%d)\n",
                pc, opcode_name(inst.opcode), inst.rs2, inst.imm, inst.rs1);
        break;
    case FMT_B:
        fprintf(stderr, "  0x%08X: %-6s r%d, r%d, 0x%X\n",
                pc, opcode_name(inst.opcode), inst.rs1, inst.rs2,
                (uint32_t)(pc + 4 + inst.imm));
        break;
    case FMT_U:
        fprintf(stderr, "  0x%08X: %-6s r%d, 0x%X\n",
                pc, opcode_name(inst.opcode), inst.rd, (uint32_t)inst.imm);
        break;
    case FMT_J:
        fprintf(stderr, "  0x%08X: %-6s r%d, 0x%X\n",
                pc, opcode_name(inst.opcode), inst.rd,
                (uint32_t)(pc + inst.imm));
        break;
    }
}

// ============================================================================
// Instruction-level trace replay for diagnostics
// ============================================================================

static void replay_with_trace(shadow_state_t *s, uint32_t block_start,
                              uint32_t block_size, uint32_t *dbt_regs,
                              uint32_t dbt_pc) {
    // Reset shadow to snapshot state
    memcpy(s->regs, s->snap_regs, sizeof(s->regs));
    s->pc = s->snap_pc;
    s->store_buf_count = 0;
    s->hit_debug = s->hit_yield = s->hit_halt = s->hit_assert_fail = false;

    uint32_t block_end = block_start + block_size;
    int max_steps = 200;  // Limit trace output to first 200 steps
    int step_num = 0;

    fprintf(stderr, "\nInstruction trace (shadow re-execution, first %d steps):\n",
            max_steps);

    for (int i = 0; i < max_steps; i++) {
        if (s->pc < block_start || s->pc >= block_end) break;

        uint32_t pc_before = s->pc;
        uint32_t raw = *(uint32_t *)(s->mem_base + s->pc);

        // Save pre-step state
        uint32_t pre_regs[32];
        memcpy(pre_regs, s->regs, sizeof(pre_regs));

        bool ended = shadow_step(s);

        // Find what changed
        fprintf(stderr, "  [%d] ", step_num);
        print_instruction(pc_before, raw);

        for (int r = 1; r < 32; r++) {
            if (s->regs[r] != pre_regs[r]) {
                // Mark if this register diverges from DBT
                bool diverges = (s->regs[r] != dbt_regs[r]);
                fprintf(stderr, "        -> r%d=0x%08X%s\n",
                        r, s->regs[r],
                        diverges ? "  <<< DBT DIFFERS" : "");
            }
        }
        if (s->pc != pc_before + 4) {
            fprintf(stderr, "        -> pc=0x%08X\n", s->pc);
        }

        step_num++;
        if (ended) break;
        if (s->pc != pc_before + 4) {
            uint8_t op = raw & 0x7F;
            uint8_t rd = (raw >> 7) & 0x1F;
            bool inblock_forward_jal =
                (op == 0x40) && (rd == 0) &&
                (s->pc > pc_before) &&
                (s->pc >= block_start && s->pc < block_end);
            if (!inblock_forward_jal) break;
        }
    }
}

// ============================================================================
// Snapshot and Verify
// ============================================================================

void shadow_snapshot(shadow_state_t *s, dbt_cpu_state_t *cpu) {
    memcpy(s->snap_regs, cpu->regs, sizeof(s->snap_regs));
    s->snap_pc = cpu->pc;
}

static bool is_intrinsic_block(shadow_state_t *s, uint32_t guest_pc) {
    if (guest_pc == s->intrinsic_memcpy  && guest_pc != 0) return true;
    if (guest_pc == s->intrinsic_memset  && guest_pc != 0) return true;
    if (guest_pc == s->intrinsic_memmove && guest_pc != 0) return true;
    if (guest_pc == s->intrinsic_strlen  && guest_pc != 0) return true;
    if (guest_pc == s->intrinsic_memswap && guest_pc != 0) return true;
    for (int i = 0; i < s->num_intercepts; i++) {
        if (s->intercept_addrs[i] == guest_pc) return true;
    }
    return false;
}

void shadow_pre_execute(shadow_state_t *s, translated_block_t *block) {
    if (!s->enabled || !block) return;

    // Skip intrinsic/intercept blocks
    if (is_intrinsic_block(s, block->guest_pc)) return;

    // PC filter
    if (s->pc_filter != 0 && block->guest_pc != s->pc_filter) return;

    // Skip count (don't decrement yet — that happens in verify)
    if (s->skip_remaining > 0) return;

    // Initialize shadow state from snapshot
    memcpy(s->regs, s->snap_regs, sizeof(s->regs));
    s->pc = s->snap_pc;
    s->store_buf_count = 0;
    s->hit_debug = s->hit_yield = s->hit_halt = s->hit_assert_fail = false;
    s->debug_char = 0;

    // Execute the block via shadow interpreter (reads pre-block memory)
    shadow_execute_block(s, block->guest_pc, block->guest_size);
}

bool shadow_verify(shadow_state_t *s, dbt_cpu_state_t *cpu,
                   translated_block_t *block, uint64_t exec_num) {
    if (!s->enabled || !block) return true;

    // Skip intrinsic/intercept blocks
    if (is_intrinsic_block(s, block->guest_pc)) {
        s->blocks_skipped++;
        return true;
    }

    // PC filter
    if (s->pc_filter != 0 && block->guest_pc != s->pc_filter) {
        s->blocks_skipped++;
        return true;
    }

    // Skip count
    if (s->skip_remaining > 0) {
        s->skip_remaining--;
        s->blocks_skipped++;
        return true;
    }

    // Shadow was already executed in shadow_pre_execute() — just compare

    // Compare PC (hard error)
    bool pc_mismatch = (s->pc != cpu->pc);

    // Compare store buffer vs real memory (hard error)
    int mem_mismatches = 0;
    if (s->check_memory && s->store_buf_count > 0) {
        for (int i = 0; i < s->store_buf_count; i++) {
            uint32_t addr = s->store_buf[i].addr;
            if (addr < s->mem_size) {
                uint8_t real = s->mem_base[addr];
                uint8_t shadow_val = s->store_buf[i].value;
                if (real != shadow_val) {
                    mem_mismatches++;
                }
            }
        }
    }

    // Compare registers (soft: only report if PC/memory also diverge)
    bool reg_mismatch[32] = {false};
    int reg_mismatch_count = 0;
    for (int i = 0; i < 32; i++) {
        if (s->regs[i] != cpu->regs[i]) {
            reg_mismatch[i] = true;
            reg_mismatch_count++;
        }
    }

    // Hard divergence: PC or memory mismatch → abort
    // Soft divergence: register-only → DBT dead-temp optimization; skip
    bool hard_mismatch = pc_mismatch || mem_mismatches > 0;

    if (hard_mismatch) {
        fprintf(stderr,
                "\n=== PARANOID DIVERGENCE at block 0x%08X (exec #%" PRIu64 ") ===\n",
                block->guest_pc, exec_num);
        fprintf(stderr, "Block: 0x%08X..0x%08X (%u instructions, %u bytes guest, %u bytes host)\n",
                block->guest_pc, block->guest_pc + block->guest_size,
                block->guest_size / 4, block->guest_size, block->host_size);
        fprintf(stderr, "Flags: 0x%02X", block->flags);
        if (block->flags & BLOCK_FLAG_DIRECT)   fprintf(stderr, " DIRECT");
        if (block->flags & BLOCK_FLAG_INDIRECT)  fprintf(stderr, " INDIRECT");
        if (block->flags & BLOCK_FLAG_CALL)      fprintf(stderr, " CALL");
        if (block->flags & BLOCK_FLAG_RETURN)    fprintf(stderr, " RETURN");
        if (block->flags & BLOCK_FLAG_STAGE5)    fprintf(stderr, " STAGE5");
        fprintf(stderr, "\n\n");

        // Register comparison table
        fprintf(stderr, "%-12s %-12s %-12s %s\n", "Register", "Shadow", "DBT", "");
        for (int i = 0; i < 32; i++) {
            if (reg_mismatch[i] || s->regs[i] != 0 || cpu->regs[i] != 0) {
                const char *tag = reg_mismatch[i] ? "MISMATCH" : "ok";
                fprintf(stderr, "r%-11d 0x%08X   0x%08X   %s\n",
                        i, s->regs[i], cpu->regs[i], tag);
            }
        }
        fprintf(stderr, "%-12s 0x%08X   0x%08X   %s\n",
                "PC", s->pc, cpu->pc, pc_mismatch ? "MISMATCH" : "ok");

        // Pre-block register dump
        fprintf(stderr, "\nPre-block state:\n");
        for (int i = 0; i < 32; i += 4) {
            fprintf(stderr, "  r%-2d=0x%08X r%-2d=0x%08X r%-2d=0x%08X r%-2d=0x%08X\n",
                    i, s->snap_regs[i], i+1, s->snap_regs[i+1],
                    i+2, s->snap_regs[i+2], i+3, s->snap_regs[i+3]);
        }
        fprintf(stderr, "  pc=0x%08X\n", s->snap_pc);

        // Store buffer summary
        if (s->store_buf_count > 0) {
            fprintf(stderr, "\nStore buffer: %d entries\n", s->store_buf_count);
            int show = s->store_buf_count < 32 ? s->store_buf_count : 32;
            for (int i = 0; i < show; i++) {
                uint32_t addr = s->store_buf[i].addr;
                uint8_t shadow_val = s->store_buf[i].value;
                if (addr < s->mem_size) {
                    uint8_t real_val = s->mem_base[addr];
                    fprintf(stderr, "  [%d] 0x%08X: shadow=0x%02X real=0x%02X %s\n",
                            i, addr, shadow_val, real_val,
                            (shadow_val == real_val) ? "(match)" : "(MISMATCH)");
                } else {
                    fprintf(stderr, "  [%d] 0x%08X: shadow=0x%02X (out of bounds)\n",
                            i, addr, shadow_val);
                }
            }
            if (s->store_buf_count > 32) {
                fprintf(stderr, "  ... and %d more\n", s->store_buf_count - 32);
            }
        }

        // Replay with instruction-level trace
        replay_with_trace(s, block->guest_pc, block->guest_size,
                          cpu->regs, cpu->pc);

        // Block disassembly
        fprintf(stderr, "\nFull block disassembly:\n");
        for (uint32_t pc = block->guest_pc;
             pc < block->guest_pc + block->guest_size && pc + 4 <= s->mem_size;
             pc += 4) {
            uint32_t raw = *(uint32_t *)(s->mem_base + pc);
            print_instruction(pc, raw);
        }

        fprintf(stderr, "\n=== END PARANOID DIVERGENCE ===\n");
        fprintf(stderr, "Stats: %" PRIu64 " blocks verified, %" PRIu64 " skipped, "
                "%" PRIu64 " instructions\n",
                s->blocks_verified, s->blocks_skipped, s->instructions_verified);

        abort();
    }

    s->blocks_verified++;

    if (s->verbose && (s->blocks_verified % 100000) == 0) {
        fprintf(stderr, "PARANOID: %" PRIu64 " blocks verified OK (%" PRIu64 " skipped)\n",
                s->blocks_verified, s->blocks_skipped);
    }

    return true;
}

// ============================================================================
// Statistics
// ============================================================================

void shadow_print_stats(shadow_state_t *s) {
    fprintf(stderr, "Paranoid mode: %" PRIu64 " blocks verified, %" PRIu64 " skipped, "
            "%" PRIu64 " instructions — 0 divergences\n",
            s->blocks_verified, s->blocks_skipped, s->instructions_verified);
}
