// SLOW-32 DBT Stage 5: SSA Management Implementation

#include "stage5_ssa.h"
#include <string.h>

static bool stage5_ssa_opcode_writes_rd(uint8_t op) {
    switch (op) {
        case 0x38: case 0x39: case 0x3A: // STB, STH, STW
        case 0x48: case 0x49: case 0x4A: case 0x4B: case 0x4C: case 0x4D: // Branches
        case 0x50: case 0x51: case 0x52: case 0x7F: // NOP, YIELD, DEBUG, HALT
            return false;
        default: return true;
    }
}

static bool stage5_ssa_opcode_uses_rs1(uint8_t op) {
    if (op == 0x20 || op == 0x40 || op == 0x50 || op == 0x7F) return false; // LUI, JAL, NOP, HALT
    return true;
}

static bool stage5_ssa_opcode_uses_rs2(uint8_t op) {
    switch (op) {
        case 0x00: case 0x01: case 0x02: case 0x03: case 0x04: // RR ALU
        case 0x05: case 0x06: case 0x07: case 0x08: case 0x09: // RR Shift/Cmp
        case 0x0A: case 0x0C: case 0x0D: case 0x1F:           // RR Mul/Div
        case 0x0E: case 0x0F: case 0x18: case 0x19:           // RR Cmp
        case 0x1A: case 0x1B: case 0x1C: case 0x1D:           // RR Cmp
        case 0x38: case 0x39: case 0x3A:                      // STB, STH, STW
        case 0x48: case 0x49: case 0x4A: case 0x4B: case 0x4C: case 0x4D: // Branches
            return true;
        default: return false;
    }
}

bool stage5_ssa_build_overlay(const stage5_lift_region_t *region, stage5_ssa_overlay_t *ssa) {
    if (!region || !ssa) return false;
    memset(ssa, 0, sizeof(*ssa));
    memset(ssa->value_def_node, 0xFF, sizeof(ssa->value_def_node));

    uint16_t current_val[32] = {0};
    uint16_t next_v = 1;

    for (uint32_t i = 0; i < region->ir_count; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        uint8_t op = n->opcode;

        if (stage5_ssa_opcode_uses_rs1(op) && n->rs1 != 0) {
            if (current_val[n->rs1] == 0) {
                if (next_v >= STAGE5_SSA_MAX_VALUES) return false;
                uint16_t v = next_v++;
                current_val[n->rs1] = v;
                ssa->value_to_reg[v] = n->rs1;
            }
            uint16_t v = current_val[n->rs1];
            ssa->node_use0_value[i] = v;
            ssa->value_use_count[v]++;
        }

        if (stage5_ssa_opcode_uses_rs2(op) && n->rs2 != 0) {
            if (current_val[n->rs2] == 0) {
                if (next_v >= STAGE5_SSA_MAX_VALUES) return false;
                uint16_t v = next_v++;
                current_val[n->rs2] = v;
                ssa->value_to_reg[v] = n->rs2;
            }
            uint16_t v = current_val[n->rs2];
            ssa->node_use1_value[i] = v;
            ssa->value_use_count[v]++;
        }

        if (stage5_ssa_opcode_writes_rd(op) && n->rd != 0) {
            if (next_v >= STAGE5_SSA_MAX_VALUES) return false;
            uint16_t v = next_v++;
            ssa->node_def_value[i] = v;
            ssa->value_to_reg[v] = n->rd;
            ssa->value_def_node[v] = (uint16_t)i;
            current_val[n->rd] = v;

            // Simple constant folding
            if (op == 0x20) { // LUI
                ssa->value_is_const[v] = true;
                ssa->value_const_val[v] = (uint32_t)n->imm;
            } else if (op == 0x10 && n->rs1 == 0) { // ADDI r0, imm
                ssa->value_is_const[v] = true;
                ssa->value_const_val[v] = (uint32_t)n->imm;
            } else if (op == 0x10 && ssa->value_is_const[ssa->node_use0_value[i]]) { // ADDI const, imm
                ssa->value_is_const[v] = true;
                ssa->value_const_val[v] = ssa->value_const_val[ssa->node_use0_value[i]] + (uint32_t)n->imm;
            }
        }
    }

    ssa->value_count = next_v - 1;
    return true;
}

bool stage5_ssa_build_phi_elim_plan(const stage5_lift_region_t *region, stage5_ssa_overlay_t *ssa, stage5_phi_elim_plan_t *plan) {
    // For single-superblock regions, we only need to sync live-out values at the exit.
    // PHIs are only needed if we have internal joins.
    if (!region || !ssa || !plan) return false;
    memset(plan, 0, sizeof(*plan));
    // For now, regions are trace-like, so we handle live-outs at the final boundary.
    return true;
}
