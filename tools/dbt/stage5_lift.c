// SLOW-32 DBT Stage 5: Superblock lifting IR (scaffold)

#include "stage5_lift.h"

#define STAGE5_MAX_NODES 256

enum {
    OP_ADD = 0x00,
    OP_SUB = 0x01,
    OP_XOR = 0x02,
    OP_OR = 0x03,
    OP_AND = 0x04,
    OP_SLL = 0x05,
    OP_SRL = 0x06,
    OP_SRA = 0x07,
    OP_SLT = 0x08,
    OP_SLTU = 0x09,
    OP_MUL = 0x0A,
    OP_DIV = 0x0C,
    OP_REM = 0x0D,
    OP_SEQ = 0x0E,
    OP_SNE = 0x0F,
    OP_ADDI = 0x10,
    OP_ORI = 0x11,
    OP_ANDI = 0x12,
    OP_SLLI = 0x13,
    OP_SRLI = 0x14,
    OP_SRAI = 0x15,
    OP_SLTI = 0x16,
    OP_SLTIU = 0x17,
    OP_SGT = 0x18,
    OP_SGTU = 0x19,
    OP_SLE = 0x1A,
    OP_SLEU = 0x1B,
    OP_SGE = 0x1C,
    OP_SGEU = 0x1D,
    OP_XORI = 0x1E,
    OP_MULHU = 0x1F,
    OP_LUI = 0x20,
    OP_LDB = 0x30,
    OP_LDH = 0x31,
    OP_LDW = 0x32,
    OP_LDBU = 0x33,
    OP_LDHU = 0x34,
    OP_STB = 0x38,
    OP_STH = 0x39,
    OP_STW = 0x3A,
    OP_JAL = 0x40,
    OP_JALR = 0x41,
    OP_BEQ = 0x48,
    OP_BNE = 0x49,
    OP_BLT = 0x4A,
    OP_BGE = 0x4B,
    OP_BLTU = 0x4C,
    OP_BGEU = 0x4D,
    OP_NOP = 0x50
    ,
    OP_YIELD = 0x51,
    OP_DEBUG = 0x52,
    OP_HALT = 0x7F
};

static bool is_compare_opcode(uint8_t opcode) {
    switch (opcode) {
        case OP_SLT:
        case OP_SLTU:
        case OP_SEQ:
        case OP_SNE:
        case OP_SGT:
        case OP_SGTU:
        case OP_SLE:
        case OP_SLEU:
        case OP_SGE:
        case OP_SGEU:
        case OP_SLTI:
        case OP_SLTIU:
            return true;
        default:
            return false;
    }
}

static bool is_simple_alu_opcode(uint8_t opcode) {
    switch (opcode) {
        case OP_ADD:
        case OP_SUB:
        case OP_XOR:
        case OP_OR:
        case OP_AND:
        case OP_SLL:
        case OP_SRL:
        case OP_SRA:
        case OP_ADDI:
        case OP_ORI:
        case OP_ANDI:
        case OP_SLLI:
        case OP_SRLI:
        case OP_SRAI:
        case OP_LUI:
        case OP_MUL:
        case OP_MULHU:
        case OP_DIV:
        case OP_REM:
        case OP_XORI:
            return true;
        default:
            return false;
    }
}

static bool is_unsigned_compare_opcode(uint8_t opcode) {
    switch (opcode) {
        case OP_SLTU:
        case OP_SGTU:
        case OP_SLEU:
        case OP_SGEU:
        case OP_SLTIU:
            return true;
        default:
            return false;
    }
}

static bool is_address_opcode(uint8_t opcode) {
    switch (opcode) {
        case OP_ADDI:
        case OP_LDB:
        case OP_LDH:
        case OP_LDW:
        case OP_LDBU:
        case OP_LDHU:
        case OP_STB:
        case OP_STH:
        case OP_STW:
            return true;
        default:
            return false;
    }
}

static bool is_branch_opcode(uint8_t opcode) {
    switch (opcode) {
        case OP_BEQ:
        case OP_BNE:
        case OP_BLT:
        case OP_BGE:
        case OP_BLTU:
        case OP_BGEU:
            return true;
        default:
            return false;
    }
}

static bool is_unsigned_branch_opcode(uint8_t opcode) {
    return opcode == OP_BLTU || opcode == OP_BGEU;
}

static bool is_terminal_cf_opcode(uint8_t opcode) {
    return opcode == OP_JAL || opcode == OP_JALR ||
           opcode == OP_YIELD || opcode == OP_DEBUG || opcode == OP_HALT;
}

static bool stage5_cfg_is_branch_opcode(uint8_t opcode) {
    return is_branch_opcode(opcode);
}

static bool stage5_cfg_is_region_pc(const uint32_t *pcs, uint32_t count, uint32_t pc) {
    for (uint32_t i = 0; i < count; i++) {
        if (pcs[i] == pc) return true;
    }
    return false;
}

static bool stage5_cfg_insert_pc_sorted(uint32_t *pcs, uint32_t *count, uint32_t pc) {
    uint32_t n = *count;
    if (n >= STAGE5_MAX_CFG_BLOCKS) return false;
    for (uint32_t i = 0; i < n; i++) {
        if (pcs[i] == pc) return true;
        if (pc < pcs[i]) {
            for (uint32_t j = n; j > i; j--) pcs[j] = pcs[j - 1];
            pcs[i] = pc;
            *count = n + 1;
            return true;
        }
    }
    pcs[n] = pc;
    *count = n + 1;
    return true;
}

static int stage5_cfg_find_block_by_start(const stage5_lift_region_t *region, uint32_t start_pc) {
    for (uint32_t i = 0; i < region->cfg_block_count; i++) {
        if (region->cfg_blocks[i].start_pc == start_pc) return (int)i;
    }
    return -1;
}

static stage5_cfg_term_kind_t stage5_cfg_classify_term(uint8_t opcode, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (stage5_cfg_is_branch_opcode(opcode)) return STAGE5_CFG_TERM_BRANCH_COND;
    if (opcode == OP_JAL) {
        return rd == 31 ? STAGE5_CFG_TERM_JAL_CALL : STAGE5_CFG_TERM_JAL_JUMP;
    }
    if (opcode == OP_JALR) {
        return (rd == 0 && rs1 == 31 && imm == 0)
            ? STAGE5_CFG_TERM_JALR_RET
            : STAGE5_CFG_TERM_JALR_INDIRECT;
    }
    if (opcode == OP_HALT) return STAGE5_CFG_TERM_HALT;
    if (opcode == OP_DEBUG) return STAGE5_CFG_TERM_DEBUG;
    if (opcode == OP_YIELD) return STAGE5_CFG_TERM_YIELD;
    return STAGE5_CFG_TERM_FALLTHROUGH;
}

static void stage5_cfg_add_succ(stage5_cfg_block_t *blk, uint32_t pc) {
    if (blk->succ_count >= 2) return;
    for (uint8_t i = 0; i < blk->succ_count; i++) {
        if (blk->succ_pc[i] == pc) return;
    }
    blk->succ_pc[blk->succ_count] = pc;
    blk->succ_block[blk->succ_count] = -1;
    blk->succ_count++;
}

static void stage5_build_cfg(stage5_lift_region_t *region) {
    region->cfg_block_count = 0;
    region->cfg_valid = false;
    if (!region || region->ir_count == 0) return;

    // Collect linear instruction PCs and decode fields from lifted IR.
    uint32_t inst_pcs[STAGE5_MAX_IR_NODES];
    uint8_t inst_op[STAGE5_MAX_IR_NODES];
    uint8_t inst_rd[STAGE5_MAX_IR_NODES];
    uint8_t inst_rs1[STAGE5_MAX_IR_NODES];
    int32_t inst_imm[STAGE5_MAX_IR_NODES];
    uint32_t inst_count = 0;

    uint32_t seen_pc[STAGE5_MAX_IR_NODES];
    uint32_t seen_count = 0;
    for (uint32_t i = 0; i < region->ir_count; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;
        bool seen = false;
        for (uint32_t j = 0; j < seen_count; j++) {
            if (seen_pc[j] == n->pc) {
                seen = true;
                break;
            }
        }
        if (seen) continue;
        if (inst_count >= STAGE5_MAX_IR_NODES) return;
        seen_pc[seen_count++] = n->pc;
        inst_pcs[inst_count] = n->pc;
        inst_op[inst_count] = n->opcode;
        inst_rd[inst_count] = n->rd;
        inst_rs1[inst_count] = n->rs1;
        inst_imm[inst_count] = n->imm;
        inst_count++;
    }
    if (inst_count == 0) return;

    // Identify CFG block starts.
    uint32_t block_starts[STAGE5_MAX_CFG_BLOCKS];
    uint32_t block_count = 0;
    if (!stage5_cfg_insert_pc_sorted(block_starts, &block_count, inst_pcs[0])) return;
    for (uint32_t i = 0; i < inst_count; i++) {
        uint8_t op = inst_op[i];
        uint32_t pc = inst_pcs[i];
        int32_t imm = inst_imm[i];
        if (stage5_cfg_is_branch_opcode(op)) {
            uint32_t fall_pc = pc + 4;
            uint32_t taken_pc = fall_pc + (uint32_t)imm;
            if (stage5_cfg_is_region_pc(inst_pcs, inst_count, fall_pc) &&
                !stage5_cfg_insert_pc_sorted(block_starts, &block_count, fall_pc)) {
                return;
            }
            if (stage5_cfg_is_region_pc(inst_pcs, inst_count, taken_pc) &&
                !stage5_cfg_insert_pc_sorted(block_starts, &block_count, taken_pc)) {
                return;
            }
        }
    }

    if (block_count > STAGE5_MAX_CFG_BLOCKS) return;
    region->cfg_block_count = block_count;

    // Build blocks with terminal classification and successor PCs.
    for (uint32_t b = 0; b < block_count; b++) {
        stage5_cfg_block_t *blk = &region->cfg_blocks[b];
        blk->start_pc = block_starts[b];
        blk->end_pc = (b + 1 < block_count) ? block_starts[b + 1] : region->end_pc;
        blk->first_inst = 0;
        blk->inst_count = 0;
        blk->term_kind = STAGE5_CFG_TERM_FALLTHROUGH;
        blk->succ_count = 0;
        blk->succ_pc[0] = blk->succ_pc[1] = 0;
        blk->succ_block[0] = blk->succ_block[1] = -1;

        bool found_first = false;
        uint32_t first = 0;
        uint32_t count = 0;
        for (uint32_t i = 0; i < inst_count; i++) {
            if (inst_pcs[i] < blk->start_pc) continue;
            if (inst_pcs[i] >= blk->end_pc) break;
            if (!found_first) {
                first = i;
                found_first = true;
            }
            count++;
        }
        if (!found_first || count == 0) continue;

        blk->first_inst = (uint16_t)first;
        blk->inst_count = (uint16_t)count;
        uint32_t last_i = first + count - 1;
        uint32_t last_pc = inst_pcs[last_i];
        uint8_t last_op = inst_op[last_i];
        uint8_t last_rd = inst_rd[last_i];
        uint8_t last_rs1 = inst_rs1[last_i];
        int32_t last_imm = inst_imm[last_i];
        blk->term_kind = stage5_cfg_classify_term(last_op, last_rd, last_rs1, last_imm);

        if (blk->term_kind == STAGE5_CFG_TERM_BRANCH_COND) {
            uint32_t fall_pc = last_pc + 4;
            uint32_t taken_pc = fall_pc + (uint32_t)last_imm;
            stage5_cfg_add_succ(blk, fall_pc);
            stage5_cfg_add_succ(blk, taken_pc);
        } else if (blk->term_kind == STAGE5_CFG_TERM_JAL_CALL) {
            stage5_cfg_add_succ(blk, last_pc + (uint32_t)last_imm); // call target
            stage5_cfg_add_succ(blk, last_pc + 4);                  // return site
        } else if (blk->term_kind == STAGE5_CFG_TERM_JAL_JUMP) {
            stage5_cfg_add_succ(blk, last_pc + (uint32_t)last_imm);
        } else if (blk->term_kind == STAGE5_CFG_TERM_FALLTHROUGH) {
            stage5_cfg_add_succ(blk, last_pc + 4);
        }
    }

    // Map successor PCs to local block IDs where possible.
    for (uint32_t b = 0; b < region->cfg_block_count; b++) {
        stage5_cfg_block_t *blk = &region->cfg_blocks[b];
        for (uint8_t s = 0; s < blk->succ_count; s++) {
            blk->succ_block[s] = (int16_t)stage5_cfg_find_block_by_start(region, blk->succ_pc[s]);
        }
    }

    region->cfg_valid = true;
}

static void decode_inst_fields(uint32_t raw, uint8_t opcode,
                               uint8_t *rd, uint8_t *rs1, uint8_t *rs2, int32_t *imm) {
    *rd = (raw >> 7) & 0x1F;
    *rs1 = (raw >> 15) & 0x1F;
    *rs2 = (raw >> 20) & 0x1F;
    *imm = 0;

    if (is_simple_alu_opcode(opcode) || is_compare_opcode(opcode) || is_address_opcode(opcode) ||
        opcode == OP_JALR) {
        // I-type immediates (including logical immediates for now; this is sufficient
        // for Stage5 structural matching telemetry).
        *imm = ((int32_t)raw) >> 20;
        if (opcode == OP_ORI || opcode == OP_ANDI || opcode == OP_XORI || opcode == OP_SLTIU) {
            *imm = (int32_t)((raw >> 20) & 0xFFF);
        }
    }
    if (opcode == OP_STB || opcode == OP_STH || opcode == OP_STW) {
        int32_t simm = (int32_t)(((raw >> 7) & 0x1F) | ((raw >> 20) & 0xFE0));
        if (simm & 0x800) simm |= 0xFFFFF000;
        *imm = simm;
    }
    if (is_branch_opcode(opcode)) {
        *imm = (((raw >> 8) & 0xF) << 1) |
               (((raw >> 25) & 0x3F) << 5) |
               (((raw >> 7) & 0x1) << 11) |
               (((int32_t)raw >> 31) << 12);
    }
    if (opcode == OP_LUI) {
        *imm = (int32_t)(raw & 0xFFFFF000u);
    }
    if (opcode == OP_JAL) {
        uint32_t imm20 = (raw >> 31) & 0x1;
        uint32_t imm10_1 = (raw >> 21) & 0x3FF;
        uint32_t imm11 = (raw >> 20) & 0x1;
        uint32_t imm19_12 = (raw >> 12) & 0xFF;
        int32_t jimm = (int32_t)((imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1));
        if (jimm & 0x100000) jimm |= 0xFFE00000;
        *imm = jimm;
    }
}

static stage5_ir_node_kind_t node_kind_for_opcode(uint8_t opcode) {
    switch (opcode) {
        case OP_ADD: case OP_ADDI: return STAGE5_IR_ADD;
        case OP_SUB: return STAGE5_IR_SUB;
        case OP_AND: case OP_ANDI: return STAGE5_IR_AND;
        case OP_OR:  case OP_ORI:  return STAGE5_IR_OR;
        case OP_XOR: case OP_XORI: return STAGE5_IR_XOR;
        case OP_SLL: case OP_SLLI: return STAGE5_IR_SLL;
        case OP_SRL: case OP_SRLI: return STAGE5_IR_SRL;
        case OP_SRA: case OP_SRAI: return STAGE5_IR_SRA;
        case OP_MUL: case OP_MULHU: case OP_DIV: case OP_REM:
            return STAGE5_IR_ADD;   // arithmetic class for initial Stage5 matching
        case OP_LUI:
            return STAGE5_IR_CONST;
        case OP_SLT: case OP_SLTU: case OP_SEQ: case OP_SNE:
        case OP_SGT: case OP_SGTU: case OP_SLE: case OP_SLEU:
        case OP_SGE: case OP_SGEU: case OP_SLTI: case OP_SLTIU:
            return STAGE5_IR_CMP;
        case OP_JAL: case OP_JALR: case OP_YIELD: case OP_DEBUG: case OP_HALT:
        case OP_BEQ: case OP_BNE: case OP_BLT: case OP_BGE:
        case OP_BLTU: case OP_BGEU:
            return STAGE5_IR_BRANCH;
        case OP_LDB: case OP_LDH: case OP_LDW: case OP_LDBU: case OP_LDHU:
            return STAGE5_IR_LOAD;
        case OP_STB: case OP_STH: case OP_STW:
            return STAGE5_IR_STORE;
        case OP_NOP:
            return STAGE5_IR_INVALID;
        default:
            return STAGE5_IR_INVALID;
    }
}

static bool append_ir_node(stage5_lift_region_t *region, stage5_ir_node_kind_t kind,
                           uint8_t opcode, uint8_t rd, uint8_t rs1, uint8_t rs2,
                           int32_t imm, uint32_t pc, bool synthetic) {
    if (kind == STAGE5_IR_INVALID) return true;
    if (region->ir_count >= STAGE5_MAX_IR_NODES) {
        region->reason = STAGE5_LIFT_REGION_TOO_LARGE;
        return false;
    }
    stage5_ir_node_t *n = &region->ir[region->ir_count++];
    n->kind = kind;
    n->opcode = opcode;
    n->rd = rd;
    n->rs1 = rs1;
    n->rs2 = rs2;
    n->imm = imm;
    n->pc = pc;
    n->synthetic = synthetic;
    n->is_side_exit = false;
    return true;
}

void stage5_lift_region_init(stage5_lift_region_t *region, uint32_t start_pc) {
    if (!region) {
        return;
    }
    region->start_pc = start_pc;
    region->end_pc = start_pc;
    region->node_count = 0;
    region->value_count = 0;
    region->guest_inst_count = 0;
    region->has_terminal_branch = false;
    region->ended_by_budget = false;
    region->has_unsigned_cmp = false;
    region->has_unsupported_opcode = false;
    region->unsupported_opcode = 0;
    region->side_exit_count = 0;
    region->ir_count = 0;
    region->cfg_block_count = 0;
    region->cfg_valid = false;
    region->reason = STAGE5_LIFT_NOT_IMPLEMENTED;
}

bool stage5_lift_superblock(stage5_lift_region_t *region,
                            const uint8_t *mem_base,
                            uint32_t code_limit,
                            uint32_t max_guest_insts) {
    if (!region || !mem_base || max_guest_insts == 0) {
        if (region) {
            region->reason = STAGE5_LIFT_INTERNAL_ERROR;
        }
        return false;
    }

    uint32_t pc = region->start_pc;
    bool saw_supported = false;
    bool prev_was_compare = false;

    while (region->guest_inst_count < max_guest_insts) {
        if (pc >= code_limit || (code_limit - pc) < 4) {
            region->reason = saw_supported ? STAGE5_LIFT_OK : STAGE5_LIFT_INVALID_CONTROL_FLOW;
            region->end_pc = pc;
            break;
        }

        uint32_t raw = *(const uint32_t *)(mem_base + pc);
        uint8_t opcode = (uint8_t)(raw & 0x7F);
        uint8_t rd = 0, rs1 = 0, rs2 = 0;
        int32_t imm = 0;
        decode_inst_fields(raw, opcode, &rd, &rs1, &rs2, &imm);

        if (opcode == OP_NOP) {
            region->guest_inst_count++;
            pc += 4;
            prev_was_compare = false;
            continue;
        }

        if (is_address_opcode(opcode)) {
            // address-form ops map to a tiny subset of IR nodes
            region->node_count += 2;
            region->value_count++;
            if (!append_ir_node(region, node_kind_for_opcode(opcode), opcode, rd, rs1, rs2, imm, pc, false)) {
                region->end_pc = pc;
                return false;
            }
            saw_supported = true;
            prev_was_compare = false;
            region->guest_inst_count++;
            pc += 4;
            if (region->node_count > STAGE5_MAX_NODES) {
                region->reason = STAGE5_LIFT_REGION_TOO_LARGE;
                region->end_pc = pc;
                return false;
            }
            continue;
        }

        if (is_simple_alu_opcode(opcode)) {
            // simple ALU/logical/shift op
            region->node_count++;
            region->value_count++;
            if (!append_ir_node(region, node_kind_for_opcode(opcode), opcode, rd, rs1, rs2, imm, pc, false)) {
                region->end_pc = pc;
                return false;
            }
            saw_supported = true;
            prev_was_compare = false;
            region->guest_inst_count++;
            pc += 4;
            if (region->node_count > STAGE5_MAX_NODES) {
                region->reason = STAGE5_LIFT_REGION_TOO_LARGE;
                region->end_pc = pc;
                return false;
            }
            continue;
        }

        if (is_compare_opcode(opcode)) {
            region->node_count++;
            region->value_count++;
            if (!append_ir_node(region, STAGE5_IR_CMP, opcode, rd, rs1, rs2, imm, pc, false)) {
                region->end_pc = pc;
                return false;
            }
            if (is_unsigned_compare_opcode(opcode)) {
                region->has_unsigned_cmp = true;
            }
            saw_supported = true;
            prev_was_compare = true;
            region->guest_inst_count++;
            pc += 4;
            if (region->node_count > STAGE5_MAX_NODES) {
                region->reason = STAGE5_LIFT_REGION_TOO_LARGE;
                region->end_pc = pc;
                return false;
            }
            continue;
        }

        if (is_branch_opcode(opcode)) {
            // Forward branches (imm > 0) can become superblock side exits:
            // the fall-through is the hot path and we continue lifting.
            // Backward branches always terminate the region.
#if STAGE5_MAX_SIDE_EXITS > 0
            bool can_side_exit = (imm > 0) &&
                                 (region->side_exit_count < STAGE5_MAX_SIDE_EXITS);
#else
            bool can_side_exit = false;
#endif

            if (!prev_was_compare) {
                region->node_count++;  // synthetic compare node
                region->value_count++;
                if (!append_ir_node(region, STAGE5_IR_CMP, opcode, 0, rs1, rs2, 0, pc, true)) {
                    region->end_pc = pc;
                    return false;
                }
            }
            region->node_count++;
            if (!append_ir_node(region, STAGE5_IR_BRANCH, opcode, 0, rs1, rs2, imm, pc, false)) {
                region->end_pc = pc;
                return false;
            }
            saw_supported = true;
            if (is_unsigned_branch_opcode(opcode)) {
                region->has_unsigned_cmp = true;
            }
            region->guest_inst_count++;
            pc += 4;

            if (can_side_exit) {
                // Mark this branch as a side exit and continue lifting.
                region->ir[region->ir_count - 1].is_side_exit = true;
                region->side_exit_count++;
                prev_was_compare = false;
                if (region->node_count > STAGE5_MAX_NODES) {
                    region->reason = STAGE5_LIFT_REGION_TOO_LARGE;
                    region->end_pc = pc;
                    return false;
                }
                continue;
            }

            // Backward branch or side-exit limit: terminate region.
            region->has_terminal_branch = true;
            region->end_pc = pc;
            region->reason = STAGE5_LIFT_OK;
            stage5_build_cfg(region);
            return true;
        }

        if (is_terminal_cf_opcode(opcode)) {
            // Direct/indirect jump terminals are valid region exits for this slice.
            region->node_count++;
            region->value_count++;
            if (!append_ir_node(region, STAGE5_IR_BRANCH, opcode, rd, rs1, rs2, imm, pc, false)) {
                region->end_pc = pc;
                return false;
            }
            saw_supported = true;
            region->has_terminal_branch = true;
            region->guest_inst_count++;
            pc += 4;
            region->end_pc = pc;
            region->reason = STAGE5_LIFT_OK;
            stage5_build_cfg(region);
            return true;
        }

        // Unsupported instruction in current slice.
        region->has_unsupported_opcode = true;
        region->unsupported_opcode = opcode;
        region->reason = STAGE5_LIFT_UNSUPPORTED_OPCODE;
        region->end_pc = pc;
        return false;
    }

    // No explicit terminal branch found within budget.
    region->end_pc = pc;
    if (!saw_supported) {
        region->reason = STAGE5_LIFT_UNSUPPORTED_OPCODE;
        return false;
    }
    if (region->guest_inst_count >= max_guest_insts) {
        // Legal Stage5 cover for max-length straight-line blocks: emitter can
        // synthesize an explicit block-end chained exit.
        region->ended_by_budget = true;
        region->reason = STAGE5_LIFT_OK;
        stage5_build_cfg(region);
        return true;
    }
    region->reason = STAGE5_LIFT_INVALID_CONTROL_FLOW;
    return false;
}

const char *stage5_lift_reason_str(stage5_lift_reason_t reason) {
    switch (reason) {
        case STAGE5_LIFT_OK:
            return "ok";
        case STAGE5_LIFT_NOT_IMPLEMENTED:
            return "not_implemented";
        case STAGE5_LIFT_UNSUPPORTED_OPCODE:
            return "unsupported_opcode";
        case STAGE5_LIFT_REGION_TOO_LARGE:
            return "region_too_large";
        case STAGE5_LIFT_INVALID_CONTROL_FLOW:
            return "invalid_control_flow";
        case STAGE5_LIFT_INTERNAL_ERROR:
            return "internal_error";
        default:
            return "unknown";
    }
}
