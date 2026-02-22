// SLOW-32 DBT Stage 5: BURG selection over lifted regions (scaffold)

#include "stage5_burg.h"

static bool is_branch_opcode(uint8_t opcode) {
    return opcode == 0x48 || opcode == 0x49 || opcode == 0x4A ||
           opcode == 0x4B || opcode == 0x4C || opcode == 0x4D;
}

static bool is_terminal_cf_opcode(uint8_t opcode) {
    return opcode == 0x40 || opcode == 0x41 || opcode == 0x51 ||
           opcode == 0x52 || opcode == 0x7F;
}

static stage5_burg_pattern_t terminal_pattern_for_opcode(uint8_t opcode) {
    switch (opcode) {
        case 0x40: return STAGE5_BURG_PATTERN_JAL_JUMP;
        case 0x41: return STAGE5_BURG_PATTERN_JALR_INDIRECT;
        case 0x7F: return STAGE5_BURG_PATTERN_HALT;
        case 0x51: return STAGE5_BURG_PATTERN_YIELD;
        case 0x52: return STAGE5_BURG_PATTERN_DEBUG;
        default:   return STAGE5_BURG_PATTERN_NONE;
    }
}

static stage5_burg_pattern_t direct_branch_pattern_for_opcode(uint8_t opcode) {
    switch (opcode) {
        case 0x48:  // BEQ
            return STAGE5_BURG_PATTERN_DIRECT_BRANCH_EQ;
        case 0x49:  // BNE
            return STAGE5_BURG_PATTERN_DIRECT_BRANCH_NE;
        case 0x4A:  // BLT
        case 0x4B:  // BGE
            return STAGE5_BURG_PATTERN_DIRECT_BRANCH_REL;
        case 0x4C:  // BLTU
        case 0x4D:  // BGEU
            return STAGE5_BURG_PATTERN_DIRECT_BRANCH_RELU;
        default:
            return STAGE5_BURG_PATTERN_NONE;
    }
}

static bool is_cmp_opcode(uint8_t opcode) {
    return opcode == 0x08 || opcode == 0x09 || opcode == 0x0E || opcode == 0x0F ||
           opcode == 0x18 || opcode == 0x19 || opcode == 0x1A || opcode == 0x1B ||
           opcode == 0x1C || opcode == 0x1D || opcode == 0x16 || opcode == 0x17;
}

// For BEQ/BNE x, r0 (or r0, x), find the nearest dominating non-synthetic
// compare that defines x, as long as x is not clobbered in-between.
static int find_cmp_branch_zero_cmp_idx(const stage5_lift_region_t *region,
                                        int branch_idx) {
    if (!region || branch_idx <= 0 || branch_idx >= (int)region->ir_count) {
        return -1;
    }
    const stage5_ir_node_t *b = &region->ir[branch_idx];
    if (b->kind != STAGE5_IR_BRANCH) {
        return -1;
    }
    if (b->opcode != 0x48 && b->opcode != 0x49) {  // BEQ/BNE only
        return -1;
    }

    uint8_t cmp_rd = 0;
    if (b->rs1 == 0 && b->rs2 != 0) {
        cmp_rd = b->rs2;
    } else if (b->rs2 == 0 && b->rs1 != 0) {
        cmp_rd = b->rs1;
    } else {
        return -1;
    }

    for (int i = branch_idx - 1; i >= 0; i--) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) {
            continue;
        }
        // Do not fuse across an intervening control-flow boundary.
        if (n->kind == STAGE5_IR_BRANCH) {
            return -1;
        }
        if (n->rd == cmp_rd && n->kind == STAGE5_IR_CMP && is_cmp_opcode(n->opcode)) {
            return i;
        }
        // Any non-compare write to cmp_rd blocks fusion.
        if (n->rd == cmp_rd) {
            return -1;
        }
    }
    return -1;
}

void stage5_burg_result_init(stage5_burg_result_t *result) {
    if (!result) {
        return;
    }
    result->selected = false;
    result->pattern_count = 0;
    result->estimated_cost = 0;
    result->reason = STAGE5_BURG_NOT_IMPLEMENTED;
    result->pattern = STAGE5_BURG_PATTERN_NONE;
}

bool stage5_burg_select(const stage5_lift_region_t *region, stage5_burg_result_t *result) {
    if (!region || !result) {
        return false;
    }
    stage5_burg_result_init(result);

    // Require a valid lifted region first.
    if (region->reason != STAGE5_LIFT_OK) {
        result->reason = STAGE5_BURG_ILLEGAL_COVER;
        return false;
    }
    if (!region->has_terminal_branch && !region->ended_by_budget) {
        result->reason = STAGE5_BURG_NO_COVER;
        return false;
    }
    if (region->guest_inst_count == 0 || region->node_count == 0) {
        result->reason = STAGE5_BURG_ILLEGAL_COVER;
        return false;
    }

    // Initial legal-cover family:
    //   branch-terminated regions in the current small lift slice.
    // Keep this conservative for now to avoid selecting oversized regions.
    if (region->guest_inst_count > 128 || region->node_count > 1024) {
        result->reason = STAGE5_BURG_NO_COVER;
        return false;
    }

    if (region->ir_count == 0 || region->ir_count > STAGE5_MAX_IR_NODES) {
        result->reason = STAGE5_BURG_ILLEGAL_COVER;
        return false;
    }

    if (region->ended_by_budget && !region->has_terminal_branch) {
        result->selected = true;
        result->pattern = STAGE5_BURG_PATTERN_BLOCK_END;
        result->pattern_count = region->node_count ? region->node_count : 1;
        result->estimated_cost =
            (region->node_count * 2) + region->value_count + region->guest_inst_count;
        result->reason = STAGE5_BURG_OK;
        return true;
    }

    // Concrete pattern checks on lifted IR:
    // 1) cmp -> branch-on-result (BEQ/BNE rd, r0)
    // 2) direct branch compare (synthetic cmp + branch)
    // 3) terminal control op
    bool has_concrete_cover = false;
    for (uint32_t i = 0; i < region->ir_count; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->kind == STAGE5_IR_BRANCH && is_terminal_cf_opcode(n->opcode)) {
            has_concrete_cover = true;
            break;
        }
        if (n->kind == STAGE5_IR_BRANCH && is_branch_opcode(n->opcode)) {
            if (i > 0) {
                const stage5_ir_node_t *p = &region->ir[i - 1];
                if (p->kind == STAGE5_IR_CMP && p->synthetic) {
                    has_concrete_cover = true;  // direct compare-branch
                    break;
                }
            }
            if (find_cmp_branch_zero_cmp_idx(region, (int)i) >= 0) {
                has_concrete_cover = true;
                break;
            }
        }
    }
    if (!has_concrete_cover) {
        result->reason = STAGE5_BURG_NO_COVER;
        return false;
    }

    // Classify primary pattern from lifted tail.
    for (int i = (int)region->ir_count - 1; i >= 0; i--) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;
        if (n->kind != STAGE5_IR_BRANCH) continue;

        if (is_terminal_cf_opcode(n->opcode)) {
            result->pattern = terminal_pattern_for_opcode(n->opcode);
            if (n->opcode == 0x40 && n->rd == 31) {
                result->pattern = (region->guest_inst_count <= 4)
                    ? STAGE5_BURG_PATTERN_JAL_CALL_SHORT
                    : STAGE5_BURG_PATTERN_JAL_CALL_LONG;
            }
            if (n->opcode == 0x41 && n->rd == 0 && n->rs1 == 31 && n->imm == 0) {
                result->pattern = (region->guest_inst_count <= 4)
                    ? STAGE5_BURG_PATTERN_JALR_RET_SHORT
                    : STAGE5_BURG_PATTERN_JALR_RET_LONG;
            }
            break;
        }

        if (is_branch_opcode(n->opcode)) {
            if (find_cmp_branch_zero_cmp_idx(region, i) >= 0) {
                result->pattern = STAGE5_BURG_PATTERN_CMP_BRANCH_ZERO;
                break;
            }
            result->pattern = direct_branch_pattern_for_opcode(n->opcode);
            break;
        }
    }
    if (result->pattern == STAGE5_BURG_PATTERN_NONE) {
        result->pattern = STAGE5_BURG_PATTERN_GENERIC;
    }

    result->selected = true;
    // Pattern accounting from lifted nodes.
    result->pattern_count = 0;
    for (uint32_t i = 0; i < region->ir_count; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->kind == STAGE5_IR_CMP || n->kind == STAGE5_IR_BRANCH ||
            n->kind == STAGE5_IR_LOAD || n->kind == STAGE5_IR_STORE) {
            result->pattern_count++;
        }
    }
    if (result->pattern_count == 0) {
        result->pattern_count = 1;
    }
    if (result->pattern_count > region->node_count) {
        result->pattern_count = region->node_count;
    }
    // Simple cost model placeholder: weighted node/value/instruction count.
    result->estimated_cost = (region->node_count * 2) + region->value_count + region->guest_inst_count;
    result->reason = STAGE5_BURG_OK;
    return true;
}

const char *stage5_burg_reason_str(stage5_burg_reason_t reason) {
    switch (reason) {
        case STAGE5_BURG_OK:
            return "ok";
        case STAGE5_BURG_NOT_IMPLEMENTED:
            return "not_implemented";
        case STAGE5_BURG_NO_COVER:
            return "no_cover";
        case STAGE5_BURG_ILLEGAL_COVER:
            return "illegal_cover";
        case STAGE5_BURG_INTERNAL_ERROR:
            return "internal_error";
        default:
            return "unknown";
    }
}

const char *stage5_burg_pattern_str(stage5_burg_pattern_t pattern) {
    switch (pattern) {
        case STAGE5_BURG_PATTERN_NONE:
            return "none";
        case STAGE5_BURG_PATTERN_JAL_CALL_SHORT:
            return "jal_call_short";
        case STAGE5_BURG_PATTERN_JAL_CALL_LONG:
            return "jal_call_long";
        case STAGE5_BURG_PATTERN_JAL_JUMP:
            return "jal_jump";
        case STAGE5_BURG_PATTERN_JALR_RET_SHORT:
            return "jalr_ret_short";
        case STAGE5_BURG_PATTERN_JALR_RET_LONG:
            return "jalr_ret_long";
        case STAGE5_BURG_PATTERN_JALR_INDIRECT:
            return "jalr_indirect";
        case STAGE5_BURG_PATTERN_HALT:
            return "halt";
        case STAGE5_BURG_PATTERN_YIELD:
            return "yield";
        case STAGE5_BURG_PATTERN_DEBUG:
            return "debug";
        case STAGE5_BURG_PATTERN_DIRECT_BRANCH_EQ:
            return "direct_branch_eq";
        case STAGE5_BURG_PATTERN_DIRECT_BRANCH_NE:
            return "direct_branch_ne";
        case STAGE5_BURG_PATTERN_DIRECT_BRANCH_REL:
            return "direct_branch_rel";
        case STAGE5_BURG_PATTERN_DIRECT_BRANCH_RELU:
            return "direct_branch_relu";
        case STAGE5_BURG_PATTERN_CMP_BRANCH_ZERO:
            return "cmp_branch_zero";
        case STAGE5_BURG_PATTERN_BLOCK_END:
            return "block_end";
        case STAGE5_BURG_PATTERN_GENERIC:
            return "generic";
        default:
            return "unknown";
    }
}
