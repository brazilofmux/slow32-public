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

static bool is_cmp_opcode(uint8_t opcode) {
    return opcode == 0x08 || opcode == 0x09 || opcode == 0x0E || opcode == 0x0F ||
           opcode == 0x18 || opcode == 0x19 || opcode == 0x1A || opcode == 0x1B ||
           opcode == 0x1C || opcode == 0x1D || opcode == 0x16 || opcode == 0x17;
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
                if (p->kind == STAGE5_IR_CMP) {
                    if (p->synthetic) {
                        has_concrete_cover = true;  // direct compare-branch
                        break;
                    }
                    // Branch uses cmp result rd against r0.
                    if ((n->rs1 == p->rd && n->rs2 == 0) ||
                        (n->rs2 == p->rd && n->rs1 == 0)) {
                        has_concrete_cover = true;
                        break;
                    }
                }
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
            result->pattern = STAGE5_BURG_PATTERN_TERMINAL;
            break;
        }

        if (is_branch_opcode(n->opcode)) {
            if (i > 0) {
                const stage5_ir_node_t *p = &region->ir[i - 1];
                if (p->kind == STAGE5_IR_CMP && !p->synthetic && is_cmp_opcode(p->opcode) &&
                    ((n->rs1 == p->rd && n->rs2 == 0) || (n->rs2 == p->rd && n->rs1 == 0))) {
                    result->pattern = STAGE5_BURG_PATTERN_CMP_BRANCH_ZERO;
                    break;
                }
            }
            result->pattern = STAGE5_BURG_PATTERN_DIRECT_BRANCH;
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
        case STAGE5_BURG_PATTERN_TERMINAL:
            return "terminal";
        case STAGE5_BURG_PATTERN_DIRECT_BRANCH:
            return "direct_branch";
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
