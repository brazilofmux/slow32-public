// SLOW-32 DBT Stage 5: Middle-Level IR (MIR)
// Value-based SSA representation for optimizations and tree matching.

#ifndef DBT_STAGE5_MIR_H
#define DBT_STAGE5_MIR_H

#include <stdint.h>
#include <stdbool.h>
#include "stage5_lift.h"
#include "stage5_ssa.h"

typedef enum {
    MIR_OP_NOP = 0,
    MIR_OP_CONST,       // dst = imm
    MIR_OP_ADD,         // dst = src0 + src1
    MIR_OP_SUB,         // dst = src0 - src1
    MIR_OP_AND,         // dst = src0 & src1
    MIR_OP_OR,          // dst = src0 | src1
    MIR_OP_XOR,         // dst = src0 ^ src1
    MIR_OP_SLL,         // dst = src0 << src1
    MIR_OP_SRL,         // dst = src0 >> src1
    MIR_OP_SRA,         // dst = src0 >>> src1
    MIR_OP_MUL,         // dst = src0 * src1
    MIR_OP_DIV,         // dst = src0 / src1
    MIR_OP_REM,         // dst = src0 % src1
    MIR_OP_CMP_EQ,      // dst = (src0 == src1)
    MIR_OP_CMP_NE,      // dst = (src0 != src1)
    MIR_OP_CMP_LT,      // dst = (src0 < src1)
    MIR_OP_CMP_LTU,     // dst = (src0 <u src1)
    MIR_OP_CMP_LE,      // dst = (src0 <= src1)
    MIR_OP_CMP_LEU,     // dst = (src0 <=u src1)
    MIR_OP_LOAD,        // dst = [src0 + imm]
    MIR_OP_STORE,       // [src0 + imm] = src1
    MIR_OP_BRANCH,      // if (src0 op src1) jump imm
    MIR_OP_CALL,        // call imm, rd = ret
    MIR_OP_RET,         // return
    MIR_OP_HALT,
    MIR_OP_YIELD,
    MIR_OP_DEBUG,
    MIR_OP_FP           // generic FP op (guest opcode in field)
} mir_op_t;

typedef struct {
    mir_op_t op;
    uint8_t guest_opcode;
    uint16_t dst_v;
    uint16_t src_v[2];
    int32_t imm;
    uint8_t size;       // 1, 2, 4 bytes
    bool is_signed;
    uint32_t guest_pc;
    uint8_t rd, rs1, rs2; // original GPRs for boundaries/helpers
    bool is_side_exit;
} mir_node_t;

#define STAGE5_MAX_MIR_NODES (STAGE5_MAX_IR_NODES * 2)

typedef struct {
    uint32_t node_count;
    mir_node_t nodes[STAGE5_MAX_MIR_NODES];
} stage5_mir_t;

void stage5_mir_build(const stage5_lift_region_t *region,
                      const stage5_ssa_overlay_t *ssa,
                      stage5_mir_t *mir);

#endif // DBT_STAGE5_MIR_H
