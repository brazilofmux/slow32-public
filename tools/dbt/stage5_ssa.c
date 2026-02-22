// SLOW-32 DBT: Stage 5 SSA overlay (region-local)

#include "stage5_ssa.h"
#include "translate.h"

#include <string.h>
#include <stdlib.h>

uint32_t stage5_ssa_regions_attempted;
uint32_t stage5_ssa_regions_built;
uint32_t stage5_ssa_regions_needs_phi;
uint64_t stage5_ssa_values_total;

bool stage5_ssa_enabled(void) {
    static bool inited = false;
    static bool enabled = true;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_SSA");
        if (v && v[0] != '\0') {
            enabled = (strcmp(v, "0") != 0);
        }
        inited = true;
    }
    return enabled;
}

static bool stage5_ssa_opcode_uses_rs1(uint8_t op) {
    switch (op) {
        case OP_ADD: case OP_SUB: case OP_XOR: case OP_OR: case OP_AND:
        case OP_SLL: case OP_SRL: case OP_SRA:
        case OP_SLT: case OP_SLTU: case OP_MUL: case OP_MULH:
        case OP_DIV: case OP_REM: case OP_SEQ: case OP_SNE:
        case OP_ADDI: case OP_ORI: case OP_ANDI:
        case OP_SLLI: case OP_SRLI: case OP_SRAI:
        case OP_SLTI: case OP_SLTIU:
        case OP_SGT: case OP_SGTU: case OP_SLE: case OP_SLEU:
        case OP_SGE: case OP_SGEU: case OP_XORI: case OP_MULHU:
        case OP_LDB: case OP_LDH: case OP_LDW: case OP_LDBU: case OP_LDHU:
        case OP_STB: case OP_STH: case OP_STW:
        case OP_JALR:
        case OP_BEQ: case OP_BNE: case OP_BLT: case OP_BGE: case OP_BLTU: case OP_BGEU:
            return true;
        default:
            return false;
    }
}

static bool stage5_ssa_opcode_uses_rs2(uint8_t op) {
    switch (op) {
        case OP_ADD: case OP_SUB: case OP_XOR: case OP_OR: case OP_AND:
        case OP_SLL: case OP_SRL: case OP_SRA:
        case OP_SLT: case OP_SLTU: case OP_MUL: case OP_MULH:
        case OP_DIV: case OP_REM: case OP_SEQ: case OP_SNE:
        case OP_SGT: case OP_SGTU: case OP_SLE: case OP_SLEU:
        case OP_SGE: case OP_SGEU: case OP_MULHU:
        case OP_STB: case OP_STH: case OP_STW:
        case OP_BEQ: case OP_BNE: case OP_BLT: case OP_BGE: case OP_BLTU: case OP_BGEU:
            return true;
        default:
            return false;
    }
}

static bool stage5_ssa_opcode_writes_rd(uint8_t op) {
    switch (op) {
        case OP_STB: case OP_STH: case OP_STW:
        case OP_BEQ: case OP_BNE: case OP_BLT: case OP_BGE: case OP_BLTU: case OP_BGEU:
        case OP_NOP:
        case OP_YIELD:
        case OP_DEBUG:
        case OP_HALT:
            return false;
        default:
            return true;
    }
}

static bool stage5_ssa_seed_input_value(uint16_t *current_value,
                                        uint16_t *next_value,
                                        uint8_t reg) {
    if (reg == 0) return true;
    if (current_value[reg] != 0) return true;
    if (*next_value >= STAGE5_SSA_MAX_VALUES) return false;
    current_value[reg] = (*next_value)++;
    return true;
}

bool stage5_ssa_build_overlay(const stage5_lift_region_t *region,
                              stage5_ssa_overlay_t *overlay) {
    stage5_ssa_regions_attempted++;
    if (!region || !overlay) return false;

    memset(overlay, 0, sizeof(*overlay));

    if (region->cfg_valid && region->cfg_block_count > 0) {
        uint8_t pred_count[STAGE5_MAX_CFG_BLOCKS];
        memset(pred_count, 0, sizeof(pred_count));
        for (uint32_t b = 0; b < region->cfg_block_count; b++) {
            const stage5_cfg_block_t *blk = &region->cfg_blocks[b];
            for (uint8_t s = 0; s < blk->succ_count; s++) {
                int16_t succ = blk->succ_block[s];
                if (succ >= 0 && (uint32_t)succ < region->cfg_block_count &&
                    pred_count[succ] < 255) {
                    pred_count[succ]++;
                }
            }
        }
        for (uint32_t b = 0; b < region->cfg_block_count; b++) {
            if (pred_count[b] > 1) {
                overlay->join_block_count++;
                if (region->cfg_blocks[b].live_in_mask != 0) {
                    overlay->needs_phi = true;
                }
            }
        }
    }

    uint16_t current_value[32];
    memset(current_value, 0, sizeof(current_value));
    uint16_t next_value = 1;

    for (uint32_t i = 0; i < region->ir_count; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;

        uint8_t op = (uint8_t)(n->opcode & 0x7F);
        if (stage5_ssa_opcode_uses_rs1(op) && n->rs1 != 0) {
            if (!stage5_ssa_seed_input_value(current_value, &next_value, n->rs1)) {
                return false;
            }
            overlay->node_use0_valid[i] = true;
            overlay->node_use0_reg[i] = n->rs1;
            overlay->node_use0_value[i] = current_value[n->rs1];
        }
        if (stage5_ssa_opcode_uses_rs2(op) && n->rs2 != 0) {
            if (!stage5_ssa_seed_input_value(current_value, &next_value, n->rs2)) {
                return false;
            }
            overlay->node_use1_valid[i] = true;
            overlay->node_use1_reg[i] = n->rs2;
            overlay->node_use1_value[i] = current_value[n->rs2];
        }

        if (stage5_ssa_opcode_writes_rd(op) && n->rd != 0) {
            if (next_value >= STAGE5_SSA_MAX_VALUES) {
                return false;
            }
            overlay->node_def_reg[i] = n->rd;
            overlay->node_def_value[i] = next_value;
            current_value[n->rd] = next_value;
            next_value++;
        }
    }

    overlay->value_count = (next_value > 0) ? (uint16_t)(next_value - 1) : 0;
    stage5_ssa_regions_built++;
    if (overlay->needs_phi) {
        stage5_ssa_regions_needs_phi++;
    }
    stage5_ssa_values_total += overlay->value_count;
    return true;
}
