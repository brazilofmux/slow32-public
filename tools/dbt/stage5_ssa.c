// SLOW-32 DBT: Stage 5 SSA overlay (region-local)

#include "stage5_ssa.h"
#include "translate.h"

#include <string.h>
#include <stdlib.h>

uint32_t stage5_ssa_regions_attempted;
uint32_t stage5_ssa_regions_built;
uint32_t stage5_ssa_regions_needs_phi;
uint64_t stage5_ssa_values_total;
uint32_t stage5_ssa_phi_plans_built;
uint64_t stage5_ssa_phi_edge_plans_total;
uint64_t stage5_ssa_phi_copies_total;

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

bool stage5_ssa_build_phi_elim_plan(const stage5_lift_region_t *region,
                                    stage5_ssa_overlay_t *overlay,
                                    stage5_phi_elim_plan_t *plan) {
    if (!region || !overlay || !plan) return false;
    memset(plan, 0, sizeof(*plan));
    if (!region->cfg_valid || region->cfg_block_count == 0) return true;

    uint16_t inst_to_block[STAGE5_MAX_IR_NODES];
    uint8_t pred_count[STAGE5_MAX_CFG_BLOCKS];
    uint8_t preds[STAGE5_MAX_CFG_BLOCKS][STAGE5_MAX_CFG_BLOCKS];
    uint16_t phi_value[STAGE5_MAX_CFG_BLOCKS][32];
    uint16_t entry_value[STAGE5_MAX_CFG_BLOCKS][32];
    uint16_t exit_value[STAGE5_MAX_CFG_BLOCKS][32];
    uint8_t block_has_def[STAGE5_MAX_CFG_BLOCKS][32];
    uint16_t block_last_def_value[STAGE5_MAX_CFG_BLOCKS][32];
    memset(inst_to_block, 0xFF, sizeof(inst_to_block));
    memset(pred_count, 0, sizeof(pred_count));
    memset(preds, 0xFF, sizeof(preds));
    memset(phi_value, 0, sizeof(phi_value));
    memset(entry_value, 0, sizeof(entry_value));
    memset(exit_value, 0, sizeof(exit_value));
    memset(block_has_def, 0, sizeof(block_has_def));
    memset(block_last_def_value, 0, sizeof(block_last_def_value));

    for (uint32_t b = 0; b < region->cfg_block_count; b++) {
        const stage5_cfg_block_t *blk = &region->cfg_blocks[b];
        uint16_t i0 = blk->first_inst;
        uint16_t i1 = (uint16_t)(blk->first_inst + blk->inst_count);
        if (i1 > region->ir_count) i1 = (uint16_t)region->ir_count;
        for (uint16_t i = i0; i < i1; i++) {
            inst_to_block[i] = (uint16_t)b;
            uint8_t rd = overlay->node_def_reg[i];
            if (rd != 0 && overlay->node_def_value[i] != 0) {
                block_has_def[b][rd] = 1;
                block_last_def_value[b][rd] = overlay->node_def_value[i];
            }
        }
        for (uint8_t s = 0; s < blk->succ_count; s++) {
            int16_t succ = blk->succ_block[s];
            if (succ >= 0 && (uint32_t)succ < region->cfg_block_count) {
                uint8_t n = pred_count[succ];
                if (n < STAGE5_MAX_CFG_BLOCKS) {
                    preds[succ][n] = (uint8_t)b;
                    pred_count[succ] = (uint8_t)(n + 1);
                }
            }
        }
    }

    for (uint32_t i = 0; i < region->ir_count; i++) {
        uint16_t b = inst_to_block[i];
        if (b == 0xFFFFu) continue;
        if (overlay->node_use0_valid[i]) {
            uint8_t r = overlay->node_use0_reg[i];
            if (r != 0 && entry_value[b][r] == 0) {
                entry_value[b][r] = overlay->node_use0_value[i];
            }
        }
        if (overlay->node_use1_valid[i]) {
            uint8_t r = overlay->node_use1_reg[i];
            if (r != 0 && entry_value[b][r] == 0) {
                entry_value[b][r] = overlay->node_use1_value[i];
            }
        }
    }

    uint16_t next_value = (overlay->value_count == 0) ? 1 : (uint16_t)(overlay->value_count + 1);
    bool changed = true;
    int iter = 0;
    while (changed && iter < (int)(region->cfg_block_count * 4 + 8)) {
        changed = false;
        iter++;
        for (uint32_t b = 0; b < region->cfg_block_count; b++) {
            uint16_t new_entry[32];
            memcpy(new_entry, entry_value[b], sizeof(new_entry));

            if (pred_count[b] == 1) {
                uint8_t p = preds[b][0];
                for (uint8_t r = 1; r < 32; r++) {
                    uint16_t v = exit_value[p][r];
                    if (v != 0 && new_entry[r] != v) new_entry[r] = v;
                }
            } else if (pred_count[b] > 1) {
                for (uint8_t r = 1; r < 32; r++) {
                    uint16_t first = 0;
                    bool conflict = false;
                    for (uint8_t k = 0; k < pred_count[b]; k++) {
                        uint8_t p = preds[b][k];
                        uint16_t v = exit_value[p][r];
                        if (v == 0) continue;
                        if (first == 0) first = v;
                        else if (first != v) conflict = true;
                    }
                    if (!conflict && first != 0) {
                        if (new_entry[r] != first) new_entry[r] = first;
                    } else if (conflict) {
                        if (phi_value[b][r] == 0) {
                            if (next_value >= STAGE5_SSA_MAX_VALUES) return false;
                            phi_value[b][r] = next_value++;
                        }
                        if (new_entry[r] != phi_value[b][r]) new_entry[r] = phi_value[b][r];
                    }
                }
            }

            if (memcmp(new_entry, entry_value[b], sizeof(new_entry)) != 0) {
                memcpy(entry_value[b], new_entry, sizeof(new_entry));
                changed = true;
            }

            uint16_t new_exit[32];
            memcpy(new_exit, new_entry, sizeof(new_exit));
            for (uint8_t r = 1; r < 32; r++) {
                if (block_has_def[b][r]) {
                    new_exit[r] = block_last_def_value[b][r];
                }
            }
            if (memcmp(new_exit, exit_value[b], sizeof(new_exit)) != 0) {
                memcpy(exit_value[b], new_exit, sizeof(new_exit));
                changed = true;
            }
        }
    }

    for (uint32_t b = 0; b < region->cfg_block_count; b++) {
        if (pred_count[b] <= 1) continue;
        for (uint8_t k = 0; k < pred_count[b]; k++) {
            uint8_t p = preds[b][k];
            stage5_phi_edge_plan_t edge = {0};
            edge.pred_block = p;
            edge.succ_block = (uint16_t)b;
            for (uint8_t r = 1; r < 32; r++) {
                uint16_t dst = phi_value[b][r];
                if (dst == 0) continue;
                uint16_t src = exit_value[p][r];
                if (src == 0 || src == dst) continue;
                if (edge.copy_count >= STAGE5_MAX_PHI_COPIES_PER_EDGE) return false;
                stage5_phi_copy_t *c = &edge.copies[edge.copy_count++];
                c->guest_reg = r;
                c->src_value = src;
                c->dst_value = dst;
            }
            if (edge.copy_count == 0) continue;
            if (plan->edge_plan_count >= STAGE5_MAX_PHI_EDGE_PLANS) return false;
            plan->edges[plan->edge_plan_count++] = edge;
            plan->total_copies = (uint16_t)(plan->total_copies + edge.copy_count);
        }
    }

    overlay->needs_phi = (plan->edge_plan_count > 0);
    overlay->value_count = (uint16_t)(next_value > 0 ? (next_value - 1) : 0);
    stage5_ssa_phi_plans_built++;
    stage5_ssa_phi_edge_plans_total += plan->edge_plan_count;
    stage5_ssa_phi_copies_total += plan->total_copies;
    return true;
}
