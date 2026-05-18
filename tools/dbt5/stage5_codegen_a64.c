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
    for (uint32_t i = 0; i < lir->node_count; i++) {
        const lir_node_t *n = &lir->nodes[i];
        if (n->op == LIR_OP_NOP)
            continue;

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

            // More ops coming (loads/stores, comparisons, etc.)

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
    // Step 4: Terminal (same as before, with exit recording)
    // ------------------------------------------------------------------
    uint32_t terminal_target = region->end_pc;
    bool     has_terminal    = false;

    for (int i = (int)lir->node_count - 1; i >= 0; i--) {
        const lir_node_t *last = &lir->nodes[i];
        if (last->op == LIR_OP_NOP) continue;

        if (last->op == LIR_OP_JMP || last->op == LIR_OP_CALL) {
            terminal_target = last->guest_pc + 4 + last->imm;
            has_terminal = true;
            break;
        }
        if (last->op == LIR_OP_RET || last->op == LIR_OP_SYSCALL) {
            terminal_target = 0;
            has_terminal = true;
            break;
        }
    }

    if (has_terminal && cg->exit_count < STAGE5_A64_MAX_EXITS) {
        int ex = cg->exit_count++;
        cg->exits[ex].target_pc    = terminal_target;
        cg->exits[ex].patch_offset = emit_offset(&cg->emit);
        cg->exits[ex].is_side_exit = false;
    }

    emit_b(&cg->emit, 0);   // placeholder branch

    stage5_codegen_a64_success++;
    return true;
}
