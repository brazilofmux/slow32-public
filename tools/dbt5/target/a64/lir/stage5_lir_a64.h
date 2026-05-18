// SLOW-32 DBT Stage 5: Low-Level IR (LIR) for AArch64 (clean-room experimental)
//
// This is the target-specific LIR for the AArch64 pipeline (post-MIR split).
// D1 complete: three-operand ALU forms (dst = src0 OP src1/imm) are the
// default, conditions are target-neutral (lir_cond_t), and the A64 codegen
// emits native AArch64 instructions without x86 two-operand glue or CC-byte
// translation. The x64 pipeline retains the older LIR for legacy reasons.

#ifndef DBT_STAGE5_LIR_A64_H
#define DBT_STAGE5_LIR_A64_H

#include <stdint.h>
#include <stdbool.h>
#include "pre/lift/stage5_lift.h"
#include "pre/ssa/stage5_ssa.h"   // for stage5_ssa_overlay_t in lir_optimize declaration

typedef enum {
    LIR_OP_NOP = 0,
    LIR_OP_MOV_RR,      // dst_v = src_v0
    LIR_OP_MOV_RI,      // dst_v = imm
    LIR_OP_MOV_RM,      // dst_v = [src_v0 + disp]
    LIR_OP_MOV_MR,      // [src_v0 + disp] = src_v1
    LIR_OP_LEA,         // dst_v = src_v0 + disp
    // Three-operand ALU (dst = src0 OP src1 / imm)
    // Three-operand forms (dst = src0 OP src1 / imm) — A64 preferred shape
    LIR_OP_ADD_RR,      // dst = src0 + src1
    LIR_OP_ADD_RI,      // dst = src0 + imm
    LIR_OP_SUB_RR,      // dst = src0 - src1
    LIR_OP_SUB_RI,
    LIR_OP_AND_RR,
    LIR_OP_AND_RI,
    LIR_OP_OR_RR,
    LIR_OP_OR_RI,
    LIR_OP_XOR_RR,
    LIR_OP_XOR_RI,
    LIR_OP_SHL_RI,
    LIR_OP_SHR_RI,
    LIR_OP_SAR_RI,
    LIR_OP_SHL_RR,      // dst = src0 << src1
    LIR_OP_SHR_RR,      // dst = src0 >> src1 (logical)
    LIR_OP_SAR_RR,      // dst = src0 >> src1 (arithmetic)
    LIR_OP_IMUL_RR,     // dst = src0 * src1
    LIR_OP_MULH_RR,     // signed high 32 bits of 32×32 multiply
    LIR_OP_MULHU_RR,    // unsigned high 32 bits of 32×32 multiply
    LIR_OP_IDIV,        // implicit RAX/RDX, cond=0 quotient, cond=1 remainder
    LIR_OP_UDIV,        // unsigned divide
    LIR_OP_CMP_RR,
    LIR_OP_CMP_RI,
    LIR_OP_TEST_RR,
    LIR_OP_SETCC,       // dst_v = cond
    LIR_OP_JCC,         // if (cond) jump imm
    LIR_OP_CMP_JCC,     // fused: cmp src0, src1; jcc (cond in l->cond)
    LIR_OP_CMP_RI_JCC,  // fused: cmp src0, disp; jcc (cond in l->cond)
    LIR_OP_TEST_JCC,    // fused: test src0, src0; jcc (cond in l->cond)
    // A64-native compare-against-zero shapes. Single-instruction terminals:
    //   CBZ  Wt, target   — branch if src0 == 0
    //   CBNZ Wt, target   — branch if src0 != 0
    // No CMP needed, no flags consumed. Replaces the cmp+b.cond pair on the
    // dominant `bne/beq rd, r0, target` SLOW-32 pattern.
    LIR_OP_CBZ,         // if (src0 == 0) jump imm
    LIR_OP_CBNZ,        // if (src0 != 0) jump imm
    LIR_OP_JMP,         // jump imm
    LIR_OP_CALL,
    LIR_OP_RET,
    LIR_OP_SYSCALL,
    LIR_OP_FP_HELPER    // call fp helper
} lir_op_t;

/* Architecture-neutral condition codes for LIR (D1).
   These replace the previous raw x86 CC bytes.
*/
typedef enum {
    LIR_COND_NONE = 0,

    LIR_COND_EQ,
    LIR_COND_NE,

    /* Signed comparisons */
    LIR_COND_LT,
    LIR_COND_GE,
    LIR_COND_LE,
    LIR_COND_GT,

    /* Unsigned comparisons */
    LIR_COND_LTU,
    LIR_COND_GEU,
    LIR_COND_LEU,
    LIR_COND_GTU,
} lir_cond_t;

typedef struct {
    lir_op_t op;
    uint8_t guest_opcode;
    uint16_t dst_v;
    uint16_t src_v[2];
    int32_t imm;
    int32_t disp;
    lir_cond_t cond;    // target-neutral (D1): LIR_COND_EQ/NE/LT/... ; for IDIV/UDIV this re-uses the field as a 0=quotient/1=remainder flag (A64 codegen handles via sdiv+msub)
    uint8_t size;       // memory access size: 1, 2, or 4
    bool is_signed;     // signed memory access (for sub-word loads)
    uint32_t guest_pc;
    uint8_t rd, rs1, rs2; // original GPRs
    bool is_side_exit;
} lir_node_t;

#define STAGE5_MAX_LIR_NODES (STAGE5_MAX_IR_NODES * 4)

typedef struct {
    uint32_t node_count;
    lir_node_t nodes[STAGE5_MAX_LIR_NODES];
} stage5_lir_t;

void stage5_lir_optimize(stage5_lir_t *lir, const stage5_ssa_overlay_t *ssa);

// Diagnostic helper (used by dbt5 driver and future tools)
const char *lir_op_name(lir_op_t op);

#endif // DBT_STAGE5_LIR_A64_H
