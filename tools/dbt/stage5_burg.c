// SLOW-32 DBT Stage 5: BURG Selection (MIR -> LIR)
// Tiles abstract MIR operations into machine-mapped LIR.
// ZERO Stage 4 dependencies. All lowering is self-contained.

#include "stage5_burg.h"
#include "stage5_ssa.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// Distinguish I-format (immediate) from R-format guest opcodes.
// I-format ops have their operand in m->imm, not m->src_v[1].
static bool is_i_format_guest(uint8_t opcode) {
    switch (opcode) {
        case 0x10: // ADDI
        case 0x11: // ORI
        case 0x12: // ANDI
        case 0x13: // SLLI
        case 0x14: // SRLI
        case 0x15: // SRAI
        case 0x16: // SLTI
        case 0x17: // SLTIU
        case 0x1E: // XORI
            return true;
        default:
            return false;
    }
}

// Lower a two-operand ALU op (ADD, SUB, AND, OR, XOR) to LIR.
// Handles I-format immediates, R-format constant folding, and register form.
static void lower_alu_rr_ri(const mir_node_t *m, const stage5_ssa_overlay_t *ssa,
                            lir_node_t *l, lir_op_t op_rr, lir_op_t op_ri) {
    l->dst_v = m->dst_v;
    l->src_v[0] = m->src_v[0];

    // I-format: immediate is in m->imm
    if (is_i_format_guest(m->guest_opcode)) {
        l->op = op_ri;
        l->imm = m->imm;
        return;
    }
    // R-format with constant src2: fold to immediate
    if (m->src_v[1] != 0 && ssa->value_is_const[m->src_v[1]]) {
        l->op = op_ri;
        l->imm = (int32_t)ssa->value_const_val[m->src_v[1]];
        return;
    }
    // R-format normal
    l->op = op_rr;
    l->src_v[1] = m->src_v[1];
}

// Lower a shift op to LIR.
static void lower_shift(const mir_node_t *m, const stage5_ssa_overlay_t *ssa,
                        lir_node_t *l, lir_op_t op_ri, lir_op_t op_rr) {
    l->dst_v = m->dst_v;
    l->src_v[0] = m->src_v[0];

    if (is_i_format_guest(m->guest_opcode)) {
        l->op = op_ri;
        l->imm = m->imm & 0x1F; // shift amount, 5 bits
        return;
    }
    if (m->src_v[1] != 0 && ssa->value_is_const[m->src_v[1]]) {
        l->op = op_ri;
        l->imm = (int32_t)(ssa->value_const_val[m->src_v[1]] & 0x1F);
        return;
    }
    l->op = op_rr;
    l->src_v[1] = m->src_v[1];
}

// Map SLOW-32 comparison opcode to x86 SETcc condition code.
// Returns the opcode byte for the SETcc/Jcc family (e.g. 0x94 = SETE).
static uint8_t cmp_opcode_to_x86_cc(uint8_t guest_opcode) {
    switch (guest_opcode) {
        case 0x0E:              return 0x94; // SEQ  -> SETE
        case 0x0F:              return 0x95; // SNE  -> SETNE
        case 0x08: case 0x16:   return 0x9C; // SLT/SLTI  -> SETL
        case 0x09: case 0x17:   return 0x92; // SLTU/SLTIU -> SETB
        case 0x18:              return 0x9F; // SGT  -> SETG
        case 0x19:              return 0x97; // SGTU -> SETA
        case 0x1A:              return 0x9E; // SLE  -> SETLE
        case 0x1B:              return 0x96; // SLEU -> SETBE
        case 0x1C:              return 0x9D; // SGE  -> SETGE
        case 0x1D:              return 0x93; // SGEU -> SETAE
        default:
            fprintf(stderr,
                    "FATAL: stage5_burg: unsupported compare opcode 0x%02X\n",
                    guest_opcode);
            abort();
    }
}

bool stage5_burg_lower(const stage5_mir_t *mir, const stage5_ssa_overlay_t *ssa, stage5_lir_t *lir) {
    if (!mir || !ssa || !lir) return false;
    memset(lir, 0, sizeof(*lir));

    bool used[STAGE5_MAX_MIR_NODES] = {0};

    for (uint32_t i = 0; i < mir->node_count; i++) {
        if (used[i]) continue;
        const mir_node_t *m = &mir->nodes[i];
        if (lir->node_count + 3 >= STAGE5_MAX_LIR_NODES) return false;
        lir_node_t *l = &lir->nodes[lir->node_count++];
        memset(l, 0, sizeof(*l));
        l->guest_pc = m->guest_pc;
        l->guest_opcode = m->guest_opcode;
        l->rd = m->rd; l->rs1 = m->rs1; l->rs2 = m->rs2;
        l->is_side_exit = m->is_side_exit;
        l->size = 4; // default

        switch (m->op) {
            case MIR_OP_NOP:
                l->op = LIR_OP_NOP;
                break;

            case MIR_OP_CONST:
                l->op = LIR_OP_MOV_RI;
                l->dst_v = m->dst_v;
                l->imm = m->imm;
                break;

            case MIR_OP_ADD:
                lower_alu_rr_ri(m, ssa, l, LIR_OP_ADD_RR, LIR_OP_ADD_RI);
                break;

            case MIR_OP_SUB:
                lower_alu_rr_ri(m, ssa, l, LIR_OP_SUB_RR, LIR_OP_SUB_RI);
                break;

            case MIR_OP_AND:
                lower_alu_rr_ri(m, ssa, l, LIR_OP_AND_RR, LIR_OP_AND_RI);
                break;

            case MIR_OP_OR:
                lower_alu_rr_ri(m, ssa, l, LIR_OP_OR_RR, LIR_OP_OR_RI);
                break;

            case MIR_OP_XOR:
                lower_alu_rr_ri(m, ssa, l, LIR_OP_XOR_RR, LIR_OP_XOR_RI);
                break;

            case MIR_OP_SLL:
                lower_shift(m, ssa, l, LIR_OP_SHL_RI, LIR_OP_SHL_RR);
                break;

            case MIR_OP_SRL:
                lower_shift(m, ssa, l, LIR_OP_SHR_RI, LIR_OP_SHR_RR);
                break;

            case MIR_OP_SRA:
                lower_shift(m, ssa, l, LIR_OP_SAR_RI, LIR_OP_SAR_RR);
                break;

            case MIR_OP_MUL:
                l->op = LIR_OP_IMUL_RR;
                l->dst_v = m->dst_v;
                l->src_v[0] = m->src_v[0];
                l->src_v[1] = m->src_v[1];
                break;

            case MIR_OP_MULH:
                l->op = LIR_OP_MULH_RR;
                l->dst_v = m->dst_v;
                l->src_v[0] = m->src_v[0];
                l->src_v[1] = m->src_v[1];
                break;

            case MIR_OP_MULHU:
                l->op = LIR_OP_MULHU_RR;
                l->dst_v = m->dst_v;
                l->src_v[0] = m->src_v[0];
                l->src_v[1] = m->src_v[1];
                break;

            case MIR_OP_DIV:
                l->op = LIR_OP_IDIV;
                l->dst_v = m->dst_v;
                l->src_v[0] = m->src_v[0];
                l->src_v[1] = m->src_v[1];
                l->cond = 0; // quotient
                break;

            case MIR_OP_REM:
                l->op = LIR_OP_IDIV;
                l->dst_v = m->dst_v;
                l->src_v[0] = m->src_v[0];
                l->src_v[1] = m->src_v[1];
                l->cond = 1; // remainder
                break;

            // Comparisons: CMP + SETCC fused pair
            case MIR_OP_CMP_EQ:
            case MIR_OP_CMP_NE:
            case MIR_OP_CMP_LT:
            case MIR_OP_CMP_LTU:
            case MIR_OP_CMP_LE:
            case MIR_OP_CMP_LEU: {
                // Emit CMP_RR or CMP_RI, then SETCC
                bool is_imm = is_i_format_guest(m->guest_opcode);

                if (is_imm) {
                    l->op = LIR_OP_CMP_RI;
                    l->src_v[0] = m->src_v[0];
                    l->imm = m->imm;
                } else {
                    l->op = LIR_OP_CMP_RR;
                    l->src_v[0] = m->src_v[0];
                    l->src_v[1] = m->src_v[1];
                }
                l->dst_v = 0; // CMP has no destination

                // Emit SETCC as second LIR node
                lir_node_t *s = &lir->nodes[lir->node_count++];
                memset(s, 0, sizeof(*s));
                s->op = LIR_OP_SETCC;
                s->dst_v = m->dst_v;
                s->cond = cmp_opcode_to_x86_cc(m->guest_opcode);
                s->guest_pc = m->guest_pc;
                s->guest_opcode = m->guest_opcode;
                s->rd = m->rd;
                s->size = 4;
                break;
            }

            case MIR_OP_LOAD: {
                // Addressing mode fusion: check if src_v[0] is an ADD(v, const)
                uint16_t addr_v = m->src_v[0];
                int addr_m_idx = -1;
                for (uint32_t j = 0; j < i; j++) {
                    if (mir->nodes[j].dst_v == addr_v) { addr_m_idx = (int)j; break; }
                }

                if (addr_m_idx >= 0) {
                    const mir_node_t *am = &mir->nodes[addr_m_idx];
                    if (am->op == MIR_OP_ADD && am->src_v[1] != 0 &&
                        ssa->value_is_const[am->src_v[1]] &&
                        ssa->value_use_count[addr_v] == 1) {
                        l->op = LIR_OP_MOV_RM;
                        l->dst_v = m->dst_v;
                        l->src_v[0] = am->src_v[0];
                        l->disp = (int32_t)ssa->value_const_val[am->src_v[1]] + m->imm;
                        l->size = m->size;
                        l->is_signed = m->is_signed;
                        used[addr_m_idx] = true;
                        break;
                    }
                }
                l->op = LIR_OP_MOV_RM;
                l->dst_v = m->dst_v;
                l->src_v[0] = m->src_v[0];
                l->disp = m->imm;
                l->size = m->size;
                l->is_signed = m->is_signed;
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
                    if (am->op == MIR_OP_ADD && am->src_v[1] != 0 &&
                        ssa->value_is_const[am->src_v[1]] &&
                        ssa->value_use_count[addr_v] == 1) {
                        l->op = LIR_OP_MOV_MR;
                        l->src_v[0] = am->src_v[0]; // base
                        l->src_v[1] = m->src_v[1];  // value
                        l->disp = (int32_t)ssa->value_const_val[am->src_v[1]] + m->imm;
                        l->size = m->size;
                        used[addr_m_idx] = true;
                        break;
                    }
                }
                l->op = LIR_OP_MOV_MR;
                l->src_v[0] = m->src_v[0];
                l->src_v[1] = m->src_v[1];
                l->disp = m->imm;
                l->size = m->size;
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
                l->src_v[0] = m->src_v[0]; // rs1's value for target computation
                break;

            case MIR_OP_FP:
                l->op = LIR_OP_FP_HELPER;
                l->rd = m->rd; l->rs1 = m->rs1; l->rs2 = m->rs2;
                break;

            case MIR_OP_HALT:
                l->op = LIR_OP_SYSCALL;
                l->imm = 0x7F;
                break;

            case MIR_OP_YIELD:
                l->op = LIR_OP_SYSCALL;
                l->imm = 0x51;
                break;

            case MIR_OP_DEBUG:
                l->op = LIR_OP_SYSCALL;
                l->imm = 0x52;
                l->src_v[0] = m->src_v[0]; // rs1 value (the character)
                l->rs1 = m->rs1;
                break;

            default:
                // Fail-fast: unknown MIR op. Do NOT silently produce wrong code.
                fprintf(stderr, "stage5-burg: unhandled MIR op %d (guest 0x%02X) at pc=0x%08X\n",
                        m->op, m->guest_opcode, m->guest_pc);
                return false;
        }
    }
    return true;
}

// Scaffold for existing BURG API compatibility
void stage5_burg_result_init(stage5_burg_result_t *result) {
    if (result) { memset(result, 0, sizeof(*result)); result->reason = STAGE5_BURG_OK; }
}

bool stage5_burg_select(const stage5_lift_region_t *region, stage5_burg_result_t *result) {
    if (result) {
        result->selected = true;
        // Budget-limited blocks (no terminal branch) get BLOCK_END so the
        // codegen synthesizes a fall-through exit.
        if (region && !region->has_terminal_branch)
            result->pattern = STAGE5_BURG_PATTERN_BLOCK_END;
        else
            result->pattern = STAGE5_BURG_PATTERN_GENERIC;
    }
    return true;
}

const char *stage5_burg_reason_str(stage5_burg_reason_t reason) { return "ok"; }
const char *stage5_burg_pattern_str(stage5_burg_pattern_t pattern) { return "generic"; }
