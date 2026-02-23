// SLOW-32 DBT Stage 5: Low-Level IR (LIR) for x86-64
// Machine-mapped IR using SSA Value IDs as virtual registers.

#ifndef DBT_STAGE5_LIR_H
#define DBT_STAGE5_LIR_H

#include <stdint.h>
#include <stdbool.h>
#include "emit_x64.h"
#include "stage5_lift.h"

typedef enum {
    LIR_OP_NOP = 0,
    LIR_OP_MOV_RR,      // dst_v = src_v0
    LIR_OP_MOV_RI,      // dst_v = imm
    LIR_OP_MOV_RM,      // dst_v = [src_v0 + disp]
    LIR_OP_MOV_MR,      // [src_v0 + disp] = src_v1
    LIR_OP_LEA,         // dst_v = src_v0 + disp
    LIR_OP_ADD_RR,      // dst_v += src_v1
    LIR_OP_ADD_RI,      // dst_v += imm
    LIR_OP_SUB_RR,
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
    LIR_OP_SHL_RR,      // dst = src0 << src1 (needs CL)
    LIR_OP_SHR_RR,      // dst = src0 >> src1 (needs CL)
    LIR_OP_SAR_RR,      // dst = src0 >>> src1 (needs CL)
    LIR_OP_IMUL_RR,
    LIR_OP_MULH_RR,     // signed high multiply
    LIR_OP_MULHU_RR,    // unsigned high multiply
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
    LIR_OP_JMP,         // jump imm
    LIR_OP_CALL,
    LIR_OP_RET,
    LIR_OP_SYSCALL,
    LIR_OP_FP_HELPER    // call fp helper
} lir_op_t;

typedef struct {
    lir_op_t op;
    uint8_t guest_opcode;
    uint16_t dst_v;
    uint16_t src_v[2];
    int32_t imm;
    int32_t disp;
    uint8_t cond;       // x86 CC (or remainder flag for DIV)
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

#endif // DBT_STAGE5_LIR_H
