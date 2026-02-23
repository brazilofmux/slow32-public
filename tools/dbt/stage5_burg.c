// SLOW-32 DBT Stage 5: BURG Selection (MIR -> LIR)
// Tiles abstract MIR operations into machine-mapped LIR.

#include "stage5_burg.h"
#include "stage5_ssa.h"
#include <string.h>

bool stage5_burg_lower(const stage5_mir_t *mir, const stage5_ssa_overlay_t *ssa, stage5_lir_t *lir) {
    if (!mir || !ssa || !lir) return false;
    memset(lir, 0, sizeof(*lir));

    bool used[STAGE5_MAX_MIR_NODES] = {0};

    for (uint32_t i = 0; i < mir->node_count; i++) {
        if (used[i]) continue;
        const mir_node_t *m = &mir->nodes[i];
        if (lir->node_count >= STAGE5_MAX_LIR_NODES) return false;
        lir_node_t *l = &lir->nodes[lir->node_count++];
        l->guest_pc = m->guest_pc;
        l->guest_opcode = m->guest_opcode;
        l->rd = m->rd; l->rs1 = m->rs1; l->rs2 = m->rs2;
        l->is_side_exit = m->is_side_exit;

        switch (m->op) {
            case MIR_OP_CONST:
                l->op = LIR_OP_MOV_RI;
                l->dst_v = m->dst_v;
                l->imm = m->imm;
                break;

            case MIR_OP_ADD:
                if (m->src_v[1] != 0 && ssa->value_is_const[m->src_v[1]]) {
                    l->op = LIR_OP_ADD_RI;
                    l->dst_v = m->dst_v;
                    l->src_v[0] = m->src_v[0];
                    l->imm = (int32_t)ssa->value_const_val[m->src_v[1]];
                } else {
                    l->op = LIR_OP_ADD_RR;
                    l->dst_v = m->dst_v;
                    l->src_v[0] = m->src_v[0];
                    l->src_v[1] = m->src_v[1];
                }
                break;

            case MIR_OP_LOAD: {
                // Addressing mode fusion: check if src_v[0] is an ADD(v, const)
                uint16_t addr_v = m->src_v[0];
                int addr_m_idx = -1;
                for (uint32_t j = 0; j < i; j++) {
                    if (mir->nodes[j].dst_v == addr_v) { addr_m_idx = (int)j; break; }
                }

                if (addr_m_idx >= 0) {
                    const mir_node_t *am = &mir->nodes[addr_m_idx];
                    if (am->op == MIR_OP_ADD && am->src_v[1] != 0 && ssa->value_is_const[am->src_v[1]] && ssa->value_use_count[addr_v] == 1) {
                        l->op = LIR_OP_MOV_RM;
                        l->dst_v = m->dst_v;
                        l->src_v[0] = am->src_v[0];
                        l->disp = (int32_t)ssa->value_const_val[am->src_v[1]] + m->imm;
                        used[addr_m_idx] = true; // Absorbed!
                        break;
                    }
                }
                l->op = LIR_OP_MOV_RM;
                l->dst_v = m->dst_v;
                l->src_v[0] = m->src_v[0];
                l->disp = m->imm;
                break;
            }

            case MIR_OP_STORE: {
                uint16_t addr_v = m->src_v[0];
                int addr_m_idx = -1;
                for (uint32_t j = 0; j < i; j++) {
                    if (mir->nodes[j].dst_v == addr_v) { addr_m_idx = (int)j; break; }
                }

                if (addr_m_idx >= 0) {
                    const mir_node_t *am = &mir->nodes[addr_m_idx];
                    if (am->op == MIR_OP_ADD && am->src_v[1] != 0 && ssa->value_is_const[am->src_v[1]] && ssa->value_use_count[addr_v] == 1) {
                        l->op = LIR_OP_MOV_MR;
                        l->src_v[0] = am->src_v[0]; // base
                        l->src_v[1] = m->src_v[1]; // value
                        l->disp = (int32_t)ssa->value_const_val[am->src_v[1]] + m->imm;
                        used[addr_m_idx] = true;
                        break;
                    }
                }
                l->op = LIR_OP_MOV_MR;
                l->src_v[0] = m->src_v[0];
                l->src_v[1] = m->src_v[1];
                l->disp = m->imm;
                break;
            }

            case MIR_OP_BRANCH:
                l->op = LIR_OP_JCC;
                l->src_v[0] = m->src_v[0];
                l->src_v[1] = m->src_v[1];
                l->imm = m->imm;
                break;

            case MIR_OP_CALL:
                l->op = LIR_OP_CALL;
                l->rd = m->rd;
                l->imm = m->imm;
                break;

            case MIR_OP_RET:
                l->op = LIR_OP_RET;
                l->rd = m->rd; l->rs1 = m->rs1; l->imm = m->imm;
                break;

            case MIR_OP_FP:
                l->op = LIR_OP_FP_HELPER;
                break;

            case MIR_OP_HALT: l->op = LIR_OP_SYSCALL; l->imm = 0x7F; break;
            case MIR_OP_YIELD: l->op = LIR_OP_SYSCALL; l->imm = 0x51; break;
            case MIR_OP_DEBUG: l->op = LIR_OP_SYSCALL; l->imm = 0x52; break;

            default:
                l->op = LIR_OP_MOV_RR; // catch-all
                l->dst_v = m->dst_v;
                l->src_v[0] = m->src_v[0];
                break;
        }
    }
    return true;
}

// Scaffold for existing BURG API compatibility
void stage5_burg_result_init(stage5_burg_result_t *result) {
    if (result) { memset(result, 0, sizeof(*result)); result->reason = STAGE5_BURG_OK; }
}

bool stage5_burg_select(const stage5_lift_region_t *region, stage5_burg_result_t *result) {
    if (result) { result->selected = true; result->pattern = STAGE5_BURG_PATTERN_GENERIC; }
    return true;
}

const char *stage5_burg_reason_str(stage5_burg_reason_t reason) { return "ok"; }
const char *stage5_burg_pattern_str(stage5_burg_pattern_t pattern) { return "generic"; }
