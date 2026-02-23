// SLOW-32 DBT Stage 5: MIR Builder Implementation

#include "stage5_mir.h"
#include "stage5_ssa.h"
#include <string.h>

static mir_op_t mir_op_from_guest(uint8_t opcode) {
    switch (opcode) {
        case 0x00: case 0x10: return MIR_OP_ADD;
        case 0x01: return MIR_OP_SUB;
        case 0x02: case 0x1E: return MIR_OP_XOR;
        case 0x03: case 0x11: return MIR_OP_OR;
        case 0x04: case 0x12: return MIR_OP_AND;
        case 0x05: case 0x13: return MIR_OP_SLL;
        case 0x06: case 0x14: return MIR_OP_SRL;
        case 0x07: case 0x15: return MIR_OP_SRA;
        case 0x0A: case 0x1F: return MIR_OP_MUL;
        case 0x0C: return MIR_OP_DIV;
        case 0x0D: return MIR_OP_REM;
        case 0x0E: return MIR_OP_CMP_EQ;
        case 0x0F: return MIR_OP_CMP_NE;
        case 0x08: case 0x16: return MIR_OP_CMP_LT;
        case 0x09: case 0x17: return MIR_OP_CMP_LTU;
        case 0x1A: return MIR_OP_CMP_LE;
        case 0x1B: return MIR_OP_CMP_LEU;
        case 0x20: return MIR_OP_CONST;
        case 0x30: case 0x31: case 0x32: case 0x33: case 0x34: return MIR_OP_LOAD;
        case 0x38: case 0x39: case 0x3A: return MIR_OP_STORE;
        case 0x48: case 0x49: case 0x4A: case 0x4B: case 0x4C: case 0x4D: return MIR_OP_BRANCH;
        case 0x40: return MIR_OP_CALL;
        case 0x41: return MIR_OP_RET;
        case 0x7F: return MIR_OP_HALT;
        case 0x51: return MIR_OP_YIELD;
        case 0x52: return MIR_OP_DEBUG;
        case 0x53: case 0x61: return MIR_OP_FP;
        default: return MIR_OP_NOP;
    }
}

void stage5_mir_build(const stage5_lift_region_t *region, const stage5_ssa_overlay_t *ssa, stage5_mir_t *mir) {
    if (!region || !ssa || !mir) return;
    memset(mir, 0, sizeof(*mir));

    for (uint32_t i = 0; i < region->ir_count; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic && n->kind != STAGE5_IR_GPR_WRITE) continue;

        mir_node_t *m = &mir->nodes[mir->node_count++];
        m->op = mir_op_from_guest(n->opcode);
        m->guest_opcode = n->opcode;
        m->dst_v = ssa->node_def_value[i];
        m->src_v[0] = ssa->node_use0_value[i];
        m->src_v[1] = ssa->node_use1_value[i];
        m->imm = n->imm;
        m->guest_pc = n->pc;
        m->rd = n->rd;
        m->rs1 = n->rs1;
        m->rs2 = n->rs2;
        m->is_side_exit = n->is_side_exit;

        if (m->op == MIR_OP_LOAD || m->op == MIR_OP_STORE) {
            switch (n->opcode) {
                case 0x30: case 0x33: case 0x38: m->size = 1; break;
                case 0x31: case 0x34: case 0x39: m->size = 2; break;
                case 0x32: case 0x3A: m->size = 4; break;
            }
            m->is_signed = (n->opcode == 0x30 || n->opcode == 0x31 || n->opcode == 0x32);
        }
    }
}
