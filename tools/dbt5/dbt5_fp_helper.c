// SLOW-32 DBT5 — Floating-point helper for x86-64 native blocks
//
// The Stage 5 x86-64 codegen lowers FP opcodes as call-outs to this C
// helper (registers are flushed/reloaded around the call).  This is the
// dbt5-local implementation; it does not depend on the Stage 1-4 runtime
// in tools/dbt/.
//
// Called from JIT code with the SysV calling convention:
//   RDI = cpu state pointer
//   ESI = opcode
//   EDX = rd
//   ECX = rs1
//   R8D = rs2

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "cpu_state.h"
#include "slow32_inst.h"

static inline void load_f64_pair(uint32_t *regs, uint8_t reg, double *out) {
    if (reg >= 31 || (reg & 1)) {
        fprintf(stderr, "f64 register fault: r%d is invalid (must be even, < 31)\n", reg);
        *out = 0.0;
        return;
    }
    uint64_t bits = ((uint64_t)regs[reg + 1] << 32) | regs[reg];
    memcpy(out, &bits, 8);
}

static inline void store_f64_pair(uint32_t *regs, uint8_t reg, double val) {
    if (reg >= 31 || (reg & 1)) {
        fprintf(stderr, "f64 register fault: r%d is invalid (must be even, < 31)\n", reg);
        return;
    }
    uint64_t bits;
    memcpy(&bits, &val, 8);
    regs[reg] = (uint32_t)bits;
    regs[reg + 1] = (uint32_t)(bits >> 32);
}

static inline int load_u64_pair(uint32_t *regs, uint8_t reg, uint64_t *out) {
    if (reg >= 31 || (reg & 1)) {
        fprintf(stderr, "f64 register fault: r%d is invalid (must be even, < 31)\n", reg);
        *out = 0;
        return 0;
    }
    *out = ((uint64_t)regs[reg + 1] << 32) | regs[reg];
    return 1;
}

static inline int store_u64_pair(uint32_t *regs, uint8_t reg, uint64_t val) {
    if (reg >= 31 || (reg & 1)) {
        fprintf(stderr, "f64 register fault: r%d is invalid (must be even, < 31)\n", reg);
        return 0;
    }
    regs[reg] = (uint32_t)val;
    regs[reg + 1] = (uint32_t)(val >> 32);
    return 1;
}

void dbt_fp_helper(dbt_cpu_state_t *cpu, uint32_t opcode,
                   uint32_t rd, uint32_t rs1, uint32_t rs2) {
    uint32_t *r = cpu->regs;

    switch (opcode) {
    // f32 arithmetic
    case OP_FADD_S: case OP_FSUB_S: case OP_FMUL_S: case OP_FDIV_S: {
        float a, b, res;
        memcpy(&a, &r[rs1], 4);
        memcpy(&b, &r[rs2], 4);
        switch (opcode) {
            case OP_FADD_S: res = a + b; break;
            case OP_FSUB_S: res = a - b; break;
            case OP_FMUL_S: res = a * b; break;
            default:        res = a / b; break;
        }
        memcpy(&r[rd], &res, 4);
        break;
    }
    case OP_FSQRT_S: {
        float a, res; memcpy(&a, &r[rs1], 4);
        res = sqrtf(a);
        memcpy(&r[rd], &res, 4);
        break;
    }
    case OP_FEQ_S: { float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); r[rd] = (a == b) ? 1 : 0; break; }
    case OP_FLT_S: { float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); r[rd] = (a <  b) ? 1 : 0; break; }
    case OP_FLE_S: { float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); r[rd] = (a <= b) ? 1 : 0; break; }
    case OP_FCVT_W_S:  { float a; memcpy(&a, &r[rs1], 4); r[rd] = (uint32_t)(int32_t)a; break; }
    case OP_FCVT_WU_S: { float a; memcpy(&a, &r[rs1], 4); r[rd] = (uint32_t)a; break; }
    case OP_FCVT_S_W:  { float res = (float)(int32_t)r[rs1]; memcpy(&r[rd], &res, 4); break; }
    case OP_FCVT_S_WU: { float res = (float)r[rs1];          memcpy(&r[rd], &res, 4); break; }
    case OP_FNEG_S: r[rd] = r[rs1] ^ 0x80000000u; break;
    case OP_FABS_S: r[rd] = r[rs1] & 0x7FFFFFFFu; break;

    // f64 arithmetic
    case OP_FADD_D: case OP_FSUB_D: case OP_FMUL_D: case OP_FDIV_D: {
        double a, b, res;
        load_f64_pair(r, rs1, &a); load_f64_pair(r, rs2, &b);
        switch (opcode) {
            case OP_FADD_D: res = a + b; break;
            case OP_FSUB_D: res = a - b; break;
            case OP_FMUL_D: res = a * b; break;
            default:        res = a / b; break;
        }
        store_f64_pair(r, rd, res);
        break;
    }
    case OP_FSQRT_D: { double a, res; load_f64_pair(r, rs1, &a); res = sqrt(a); store_f64_pair(r, rd, res); break; }
    case OP_FEQ_D: { double a, b; load_f64_pair(r, rs1, &a); load_f64_pair(r, rs2, &b); r[rd] = (a == b) ? 1 : 0; break; }
    case OP_FLT_D: { double a, b; load_f64_pair(r, rs1, &a); load_f64_pair(r, rs2, &b); r[rd] = (a <  b) ? 1 : 0; break; }
    case OP_FLE_D: { double a, b; load_f64_pair(r, rs1, &a); load_f64_pair(r, rs2, &b); r[rd] = (a <= b) ? 1 : 0; break; }
    case OP_FCVT_W_D:  { double a; load_f64_pair(r, rs1, &a); r[rd] = (uint32_t)(int32_t)a; break; }
    case OP_FCVT_WU_D: { double a; load_f64_pair(r, rs1, &a); r[rd] = (uint32_t)a; break; }
    case OP_FCVT_D_W:  { store_f64_pair(r, rd, (double)(int32_t)r[rs1]); break; }
    case OP_FCVT_D_WU: { store_f64_pair(r, rd, (double)r[rs1]); break; }
    case OP_FCVT_D_S:  { float a; memcpy(&a, &r[rs1], 4); store_f64_pair(r, rd, (double)a); break; }
    case OP_FCVT_S_D:  { double a; load_f64_pair(r, rs1, &a); float res = (float)a; memcpy(&r[rd], &res, 4); break; }
    case OP_FNEG_D: {
        double a;
        load_f64_pair(r, rs1, &a);
        uint64_t bits;
        memcpy(&bits, &a, 8);
        bits ^= 0x8000000000000000ull;
        memcpy(&a, &bits, 8);
        store_f64_pair(r, rd, a);
        break;
    }
    case OP_FABS_D: {
        double a;
        load_f64_pair(r, rs1, &a);
        uint64_t bits;
        memcpy(&bits, &a, 8);
        bits &= 0x7FFFFFFFFFFFFFFFull;
        memcpy(&a, &bits, 8);
        store_f64_pair(r, rd, a);
        break;
    }

    // float <-> int64
    case OP_FCVT_L_S: {
        float a; memcpy(&a, &r[rs1], 4);
        int64_t v = (int64_t)a;
        store_u64_pair(r, rd, (uint64_t)v);
        break;
    }
    case OP_FCVT_LU_S: {
        float a; memcpy(&a, &r[rs1], 4);
        uint64_t v = (uint64_t)a;
        store_u64_pair(r, rd, v);
        break;
    }
    case OP_FCVT_L_D: {
        double a; load_f64_pair(r, rs1, &a);
        int64_t v = (int64_t)a;
        store_u64_pair(r, rd, (uint64_t)v);
        break;
    }
    case OP_FCVT_LU_D: {
        double a; load_f64_pair(r, rs1, &a);
        uint64_t v = (uint64_t)a;
        store_u64_pair(r, rd, v);
        break;
    }
    case OP_FCVT_D_L: {
        uint64_t v;
        if (!load_u64_pair(r, rs1, &v)) break;
        store_f64_pair(r, rd, (double)(int64_t)v);
        break;
    }
    case OP_FCVT_D_LU: {
        uint64_t v;
        if (!load_u64_pair(r, rs1, &v)) break;
        store_f64_pair(r, rd, (double)v);
        break;
    }
    }

    r[0] = 0;  // Ensure r0 stays 0
}
