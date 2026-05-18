// SLOW-32 DBT5 — AArch64 Codegen Implementation (Initial Skeleton)
//
// This file is the start of the real independent AArch64 Stage 5 emitter.
//
// See the header (stage5_codegen_a64.h) for the strict rules and the
// design of stage5_cg_a64_ctx_t.
//
// The first concrete work will be to implement a narrow owning path
// (probably starting with pure ALU + simple loads/stores that fit in the
// 8 host slots provided by the RA plan) using only emit_* calls from
// emit_a64.c.
//
// Nothing in this file may ever reach for the old Stage 4 machinery.

#include "stage5_codegen_a64.h"
#include <string.h>

uint32_t stage5_codegen_a64_attempted = 0;
uint32_t stage5_codegen_a64_success   = 0;

// Map LIR condition (usually x86 CC from the BURG lowering) + guest opcode
// into a usable AArch64 condition code.
static a64_cond_t lir_cond_to_a64(uint8_t lir_cond, uint8_t guest_opcode)
{
    // First try the LIR cond field (most common path)
    switch (lir_cond) {
        case 0x84: return COND_EQ;   // JE
        case 0x85: return COND_NE;   // JNE
        case 0x8C: return COND_LT;   // JL
        case 0x8D: return COND_GE;   // JGE
        case 0x8E: return COND_LE;   // JLE
        case 0x8F: return COND_GT;   // JG
        case 0x82: return COND_LO;   // JB
        case 0x83: return COND_HS;   // JAE
        case 0x86: return COND_LS;   // JBE
        case 0x87: return COND_HI;   // JA
        case 0x88: return COND_MI;   // JS
        case 0x89: return COND_PL;   // JNS
        case 0x92: return COND_LO;   // JB (setcc)
        case 0x93: return COND_HS;   // JAE
        case 0x94: return COND_EQ;   // JE (setcc)
        case 0x95: return COND_NE;   // JNE
        case 0x96: return COND_LS;   // JBE
        case 0x97: return COND_HI;   // JA
        case 0x9C: return COND_LT;   // JL
        case 0x9D: return COND_GE;   // JGE
        case 0x9E: return COND_LE;   // JLE
        case 0x9F: return COND_GT;   // JG
    }

    // Fallback: derive from the original SLOW-32 guest opcode.
    switch (guest_opcode) {
        case 0x0E: return COND_EQ;   // SEQ
        case 0x0F: return COND_NE;   // SNE
        case 0x08: return COND_LT;   // SLT
        case 0x09: return COND_LO;   // SLTU
        case 0x18: return COND_GT;   // SGT
        case 0x19: return COND_HI;   // SGTU
        case 0x1A: return COND_LE;   // SLE
        case 0x1B: return COND_LS;   // SLEU
        case 0x1C: return COND_GE;   // SGE
        case 0x1D: return COND_HS;   // SGEU
        default:   return COND_EQ;
    }
}

void stage5_cg_a64_init(stage5_cg_a64_ctx_t *cg, uint8_t *code_buf, size_t capacity)
{
    if (!cg) return;
    memset(cg, 0, sizeof(*cg));
    emit_init(&cg->emit, code_buf, capacity);
}

bool stage5_codegen_a64(stage5_cg_a64_ctx_t *cg,
                        const stage5_lift_region_t *region,
                        const stage5_ssa_overlay_t *ssa,
                        const stage5_lir_t *lir,
                        const stage5_ra_plan_t *ra_plan)
{
    if (!cg || !region || !ssa || !lir || !ra_plan)
        return false;

    stage5_codegen_a64_attempted++;

    // ------------------------------------------------------------------
    // Step 1: Initialize the 8 host slots from the RA plan
    // ------------------------------------------------------------------
    memset(cg->host_regs, 0, sizeof(cg->host_regs));
    memset(cg->guest_in_slot, 0, sizeof(cg->guest_in_slot));
    memset(cg->slot_dirty, 0, sizeof(cg->slot_dirty));

    // Build a quick lookup: SSA value_id → host register (for values the RA assigned)
    a64_reg_t value_to_host[STAGE5_SSA_MAX_VALUES] = {0};
    uint8_t   value_to_gpr [STAGE5_SSA_MAX_VALUES] = {0};

    for (uint16_t i = 0; i < ra_plan->interval_count; i++) {
        const stage5_ra_interval_t *iv = &ra_plan->intervals[i];
        if (iv->value_id == 0 || iv->spilled || iv->assigned_slot < 0)
            continue;

        int slot = iv->assigned_slot;
        if (slot >= 0 && slot < STAGE5_A64_MAX_HOST_SLOTS) {
            static const a64_reg_t slot_to_host[8] = {
                W8, W9, W10, W11, W12, W13, W14, W15
            };
            a64_reg_t hreg = slot_to_host[slot];
            uint8_t gpr = ssa->value_to_reg[iv->value_id];

            cg->host_regs[slot]     = hreg;
            cg->guest_in_slot[slot] = gpr;
            cg->slot_dirty[slot]    = false;

            if (iv->value_id < STAGE5_SSA_MAX_VALUES) {
                value_to_host[iv->value_id] = hreg;
                value_to_gpr [iv->value_id] = gpr;
            }
        }
    }

    // ------------------------------------------------------------------
    // Step 2: Emit a minimal prologue (load live-ins that are in registers)
    // ------------------------------------------------------------------
    for (int s = 0; s < STAGE5_A64_MAX_HOST_SLOTS; s++) {
        a64_reg_t hreg = cg->host_regs[s];
        uint8_t   gpr  = cg->guest_in_slot[s];
        if (hreg == A64_NOREG || gpr == 0)
            continue;

        emit_ldr_w32_imm(&cg->emit, hreg, W20, (uint32_t)(gpr * 4));
    }

    // ------------------------------------------------------------------
    // Step 3: Walk the LIR and emit real instructions (narrow ALU first)
    // ------------------------------------------------------------------
    size_t lir_emit_off[STAGE5_MAX_LIR_NODES];
    memset(lir_emit_off, 0, sizeof(lir_emit_off));

    for (uint32_t i = 0; i < lir->node_count; i++) {
        const lir_node_t *n = &lir->nodes[i];
        if (n->op == LIR_OP_NOP)
            continue;

        lir_emit_off[i] = emit_offset(&cg->emit);

        // For v1 we only handle ops where the destination (if any) is in one of our 8 slots.
        a64_reg_t dst_h = (n->dst_v < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->dst_v] : A64_NOREG;

        switch (n->op) {
            case LIR_OP_MOV_RI:
                if (dst_h != A64_NOREG) {
                    emit_mov_w32_imm32(&cg->emit, dst_h, (uint32_t)n->imm);
                    // mark dirty (simplified)
                    for (int s = 0; s < STAGE5_A64_MAX_HOST_SLOTS; s++) {
                        if (cg->host_regs[s] == dst_h) { cg->slot_dirty[s] = true; break; }
                    }
                }
                break;

            case LIR_OP_MOV_RR: {
                a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : A64_NOREG;
                if (dst_h != A64_NOREG && src_h != A64_NOREG) {
                    emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                } else if (dst_h != A64_NOREG) {
                    // Fallback: load from memory (very conservative)
                    uint8_t gpr = value_to_gpr[n->src_v[0]];
                    if (gpr != 0)
                        emit_ldr_w32_imm(&cg->emit, dst_h, W20, gpr * 4);
                }
                break;
            }

            case LIR_OP_ADD_RI:
                if (dst_h != A64_NOREG) {
                    a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : dst_h;
                    if (src_h != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                    if (n->imm != 0)
                        emit_add_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)n->imm);
                }
                break;

            case LIR_OP_ADD_RR: {
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    if (src0 != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src0);
                    emit_add_w32(&cg->emit, dst_h, dst_h, src1);
                }
                break;
            }

            // ------------------------------------------------------------------
            // SUB / AND / OR / XOR (RI + RR) — narrow ALU support
            // ------------------------------------------------------------------
            case LIR_OP_SUB_RI:
                if (dst_h != A64_NOREG) {
                    a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : dst_h;
                    if (src_h != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                    if (n->imm != 0)
                        emit_sub_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)n->imm);
                }
                break;

            case LIR_OP_SUB_RR: {
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    if (src0 != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src0);
                    emit_sub_w32(&cg->emit, dst_h, dst_h, src1);
                }
                break;
            }

            case LIR_OP_AND_RI:
                if (dst_h != A64_NOREG) {
                    a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : dst_h;
                    if (src_h != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                    emit_and_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)n->imm);
                }
                break;

            case LIR_OP_AND_RR: {
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    if (src0 != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src0);
                    emit_and_w32(&cg->emit, dst_h, dst_h, src1);
                }
                break;
            }

            case LIR_OP_OR_RI:
                if (dst_h != A64_NOREG) {
                    a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : dst_h;
                    if (src_h != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                    emit_orr_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)n->imm);
                }
                break;

            case LIR_OP_OR_RR: {
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    if (src0 != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src0);
                    emit_orr_w32(&cg->emit, dst_h, dst_h, src1);
                }
                break;
            }

            case LIR_OP_XOR_RI:
                if (dst_h != A64_NOREG) {
                    a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : dst_h;
                    if (src_h != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                    emit_eor_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)n->imm);
                }
                break;

            case LIR_OP_XOR_RR: {
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    if (src0 != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src0);
                    emit_eor_w32(&cg->emit, dst_h, dst_h, src1);
                }
                break;
            }

            // ------------------------------------------------------------------
            // Comparisons that produce a result (seq, sne, slt, etc.)
            // Emit cmp + cset into the destination.
            // ------------------------------------------------------------------
            case LIR_OP_CMP_RR:
            case LIR_OP_CMP_RI:
            case LIR_OP_TEST_RR: {
                if (dst_h == A64_NOREG) break;

                // Emit the compare
                if (n->op == LIR_OP_CMP_RI) {
                    a64_reg_t src0 = value_to_host[n->src_v[0]];
                    emit_cmp_w32_imm(&cg->emit, src0, (uint32_t)n->imm);
                } else {
                    a64_reg_t src0 = value_to_host[n->src_v[0]];
                    a64_reg_t src1 = value_to_host[n->src_v[1]];
                    if (src0 != A64_NOREG && src1 != A64_NOREG)
                        emit_cmp_w32_w32(&cg->emit, src0, src1);
                }

                // Use the shared mapping (LIR cond first, then guest opcode fallback)
                a64_cond_t a64cond = lir_cond_to_a64(n->cond, n->guest_opcode);
                emit_cset_w32(&cg->emit, dst_h, a64cond);

                // Mark destination dirty
                for (int s = 0; s < STAGE5_A64_MAX_HOST_SLOTS; s++) {
                    if (cg->host_regs[s] == dst_h) { cg->slot_dirty[s] = true; break; }
                }
                break;
            }

            // ------------------------------------------------------------------
            // Loads and Stores (narrow path)
            // Assumes the address base is already in a host register.
            // ------------------------------------------------------------------
            case LIR_OP_MOV_RM: {  // dst = [base + disp]
                if (dst_h == A64_NOREG) break;

                a64_reg_t base_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                   ? value_to_host[n->src_v[0]] : A64_NOREG;
                if (base_h == A64_NOREG) break;

                switch (n->size) {
                    case 1:
                        if (n->is_signed)
                            emit_ldrsb_w32_imm(&cg->emit, dst_h, base_h, n->disp);
                        else
                            emit_ldrb_imm(&cg->emit, dst_h, base_h, n->disp);
                        break;
                    case 2:
                        if (n->is_signed)
                            emit_ldrsh_w32_imm(&cg->emit, dst_h, base_h, n->disp);
                        else
                            emit_ldrh_imm(&cg->emit, dst_h, base_h, n->disp);
                        break;
                    case 4:
                    default:
                        emit_ldr_w32_imm(&cg->emit, dst_h, base_h, n->disp);
                        break;
                }

                // Destination is now live and dirty in its slot
                for (int s = 0; s < STAGE5_A64_MAX_HOST_SLOTS; s++) {
                    if (cg->host_regs[s] == dst_h) {
                        cg->slot_dirty[s] = true;
                        break;
                    }
                }
                break;
            }

            case LIR_OP_MOV_MR: {  // [base + disp] = src1
                a64_reg_t base_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                   ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t data_h = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                   ? value_to_host[n->src_v[1]] : A64_NOREG;

                if (base_h == A64_NOREG || data_h == A64_NOREG) break;

                switch (n->size) {
                    case 1:  emit_strb_imm(&cg->emit, data_h, base_h, n->disp); break;
                    case 2:  emit_strh_imm(&cg->emit, data_h, base_h, n->disp); break;
                    case 4:
                    default: emit_str_w32_imm(&cg->emit, data_h, base_h, n->disp); break;
                }
                break;
            }

            case LIR_OP_LEA: {
                // dst = base + disp (very common for address calculation)
                if (dst_h == A64_NOREG) break;

                a64_reg_t base_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                   ? value_to_host[n->src_v[0]] : A64_NOREG;
                if (base_h == A64_NOREG) break;

                if (n->disp != 0) {
                    emit_add_w32_imm(&cg->emit, dst_h, base_h, (uint32_t)n->disp);
                } else if (base_h != dst_h) {
                    emit_mov_w32_w32(&cg->emit, dst_h, base_h);
                }

                // mark dst dirty
                for (int s = 0; s < STAGE5_A64_MAX_HOST_SLOTS; s++) {
                    if (cg->host_regs[s] == dst_h) {
                        cg->slot_dirty[s] = true;
                        break;
                    }
                }
                break;
            }

            // Shifts (very common)
            case LIR_OP_SHL_RI:
                if (dst_h != A64_NOREG) {
                    a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                      ? value_to_host[n->src_v[0]] : dst_h;
                    if (src_h != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                    emit_lsl_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)n->imm);
                }
                break;

            case LIR_OP_SHR_RI:
                if (dst_h != A64_NOREG) {
                    a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                      ? value_to_host[n->src_v[0]] : dst_h;
                    if (src_h != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                    emit_lsr_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)n->imm);
                }
                break;

            case LIR_OP_SAR_RI:
                if (dst_h != A64_NOREG) {
                    a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                      ? value_to_host[n->src_v[0]] : dst_h;
                    if (src_h != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                    emit_asr_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)n->imm);
                }
                break;

            case LIR_OP_SHL_RR: {
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    if (src0 != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src0);
                    emit_lslv_w32(&cg->emit, dst_h, dst_h, src1);
                }
                break;
            }

            case LIR_OP_SHR_RR: {
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    if (src0 != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src0);
                    emit_lsrv_w32(&cg->emit, dst_h, dst_h, src1);
                }
                break;
            }

            case LIR_OP_SAR_RR: {
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    if (src0 != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src0);
                    emit_asrv_w32(&cg->emit, dst_h, dst_h, src1);
                }
                break;
            }

            // ------------------------------------------------------------------
            // Internal forward branches (first control-flow support inside regions)
            // ------------------------------------------------------------------
            case LIR_OP_JCC:
            case LIR_OP_CMP_JCC:
            case LIR_OP_CMP_RI_JCC:
            case LIR_OP_TEST_JCC: {
                uint32_t tgt = n->guest_pc + 4 + n->imm;
                bool is_internal = !n->is_side_exit &&
                                   tgt >= region->start_pc &&
                                   tgt < region->end_pc;

                if (is_internal) {
                    // Record fixup for post-pass patching
                    size_t patch_off = emit_offset(&cg->emit);

                    // Map LIR condition to AArch64 condition.
                    // We first try the LIR cond field (x86-style CC from BURG).
                    // If that is unknown/zero, we fall back to the guest opcode
                    // (which we already handle well in the comparison emission path).
                    a64_cond_t a64c = lir_cond_to_a64(n->cond, n->guest_opcode);

                    emit_b_cond(&cg->emit, a64c, 0);

                    // Record in the dedicated internal branch fixup array
                    if (cg->internal_branch_count < 16) {
                        int idx = cg->internal_branch_count++;
                        cg->internal_branches[idx].patch_offset = patch_off;
                        cg->internal_branches[idx].target_pc    = tgt;
                    }
                } else {
                    // Side-exit or out-of-region conditional branch.
                    // For the narrow path we currently just emit a placeholder.
                    // Proper side-exit support comes in a later slice.
                    emit_b_cond(&cg->emit, COND_EQ, 0);
                }
                break;
            }

            case LIR_OP_IMUL_RR: {
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    if (src0 != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src0);
                    emit_mul_w32(&cg->emit, dst_h, dst_h, src1);
                }
                break;
            }

            case LIR_OP_IDIV: {
                // Signed div/rem: cond==0 → quotient, cond==1 → remainder
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;

                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    if (n->cond == 0) {
                        // quotient
                        emit_sdiv_w32(&cg->emit, dst_h, src0, src1);
                    } else {
                        // remainder = dividend - (quotient * divisor)
                        emit_sdiv_w32(&cg->emit, W16, src0, src1);
                        emit_msub_w32(&cg->emit, dst_h, W16, src1, src0);
                    }
                }
                break;
            }

            case LIR_OP_UDIV: {
                // unsigned division (quotient only for now)
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    emit_udiv_w32(&cg->emit, dst_h, src0, src1);
                }
                break;
            }

            case LIR_OP_MULH_RR: {
                // signed high 32 bits of 32x32 multiply
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    // Use W16 as 64-bit temp (X16)
                    emit_smull(&cg->emit, W16, src0, src1);
                    emit_lsr_w32_imm(&cg->emit, dst_h, W16, 32);
                }
                break;
            }

            case LIR_OP_MULHU_RR: {
                // unsigned high 32 bits of 32x32 multiply
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    emit_umull(&cg->emit, W16, src0, src1);
                    emit_lsr_w32_imm(&cg->emit, dst_h, W16, 32);
                }
                break;
            }

            // More ops coming (TEST is already partially handled above, MULH/MULHU, DIV/REM, FP, calls, etc.)

            default:
                // For now we silently skip unsupported ops in this narrow path.
                // A real version will return false or fall back when it hits something it can't handle.
                break;
        }

        // Very rough dirty tracking for the dest
        if (n->dst_v != 0 && dst_h != A64_NOREG) {
            // find which slot this dst_h lives in (slow but fine for early version)
            for (int s = 0; s < STAGE5_A64_MAX_HOST_SLOTS; s++) {
                if (cg->host_regs[s] == dst_h) {
                    cg->slot_dirty[s] = true;
                    break;
                }
            }
        }
    }

    // ------------------------------------------------------------------
    // Post-pass: patch internal forward branches
    // ------------------------------------------------------------------
    for (uint32_t f = 0; f < cg->internal_branch_count; f++) {
        uint32_t tgt_pc   = cg->internal_branches[f].target_pc;
        size_t   patch_off = cg->internal_branches[f].patch_offset;

        // Find the first LIR node at or after the target PC
        size_t target_emit_off = (size_t)-1;
        for (uint32_t j = 0; j < lir->node_count; j++) {
            if (lir->nodes[j].guest_pc >= tgt_pc) {
                target_emit_off = lir_emit_off[j];
                break;
            }
        }
        if (target_emit_off == (size_t)-1) continue;

        int32_t byte_disp = (int32_t)(target_emit_off - patch_off);
        if (byte_disp % 4 != 0) continue;

        int32_t word_disp = byte_disp / 4;

        // Patch the B.cond at patch_off
        uint32_t *p = (uint32_t *)(cg->emit.buf + patch_off);
        uint32_t inst = *p;
        inst &= ~0x00FFFFE0U;
        inst |= ((uint32_t)word_disp & 0x7FFFFU) << 5;
        *p = inst;
    }

    // ------------------------------------------------------------------
    // Step 4: Terminal handling (produce correct pc + exit_reason)
    // ------------------------------------------------------------------
    // We try to determine what the "next PC" should be after this region.
    // - If the last LIR instruction is a direct jump/call, use its target.
    // - Otherwise fall through to the end of the region.
    //
    // We also choose a reasonable exit reason for the test harness.
    uint32_t next_pc = region->end_pc;          // default: fall through
    uint32_t exit_reason = 9;                   // EXIT_BLOCK_END by default

    for (int i = (int)lir->node_count - 1; i >= 0; i--) {
        const lir_node_t *last = &lir->nodes[i];
        if (last->op == LIR_OP_NOP) continue;

        if (last->op == LIR_OP_JMP || last->op == LIR_OP_CALL) {
            next_pc = last->guest_pc + 4 + last->imm;
            exit_reason = 2;                    // treat as branch for now
            break;
        }
        if (last->op == LIR_OP_RET || last->op == LIR_OP_SYSCALL) {
            // For HALT/DEBUG/YIELD we can leave next_pc as-is or 0.
            // The exec test will still see the exit_reason.
            next_pc = last->guest_pc + 4;       // or keep region->end_pc
            exit_reason = (last->op == LIR_OP_SYSCALL) ? 0x51 : 0x7F; // rough
            break;
        }
    }

    // Record the exit information (useful for future real runtime)
    if (cg->exit_count < STAGE5_A64_MAX_EXITS) {
        int ex = cg->exit_count++;
        cg->exits[ex].target_pc    = next_pc;
        cg->exits[ex].patch_offset = emit_offset(&cg->emit);
        cg->exits[ex].is_side_exit = false;
    }

    // For the experimental execution path we emit a simple "write state + return"
    // sequence so the test harness gets back a sensible pc and exit_reason.
    emit_mov_w32_imm32(&cg->emit, W0, next_pc);
    emit_str_w32_imm(&cg->emit, W0, W20, 0x80);     // cpu->pc

    emit_mov_w32_imm32(&cg->emit, W0, exit_reason);
    emit_str_w32_imm(&cg->emit, W0, W20, 0x84);     // cpu->exit_reason

    emit_ret_lr(&cg->emit);

    stage5_codegen_a64_success++;
    return true;
}
