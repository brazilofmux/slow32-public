// SLOW-32 DBT5 — AArch64 Codegen Implementation (Initial Skeleton)
//
// This file is the start of the real independent AArch64 Stage 5 emitter.
//
// See the header (stage5_codegen_a64.h) for the strict rules and the
// design of stage5_cg_a64_ctx_t.
//
// The first concrete work will be to implement a narrow owning path
// (starting with ALU + simple loads/stores that fit in the 14 host slots
// provided by the RA plan: 8 caller-saved W8-W15 + 6 callee-saved) using
// only emit_* calls from emit_a64.c.
//
// Nothing in this file may ever reach for the old Stage 4 machinery.

#include "stage5_codegen_a64.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// G2/G3: use the canonical exit reason enum (defined in cpu_state.h for the
// dispatcher and launch contract). The numeric values are stable.
#include "../../../cpu_state.h"

// D4 helper: ensure we never leave a partially-emitted buffer on failure.
// The driver only installs/executes when the function returns true, but
// resetting the offset makes the contract bullet-proof.
static bool cg_bail(stage5_cg_a64_ctx_t *cg) {
    if (cg) cg->emit.offset = 0;
    return false;
}

uint32_t stage5_codegen_a64_attempted = 0;
uint32_t stage5_codegen_a64_success   = 0;

// Map a target-neutral LIR condition (D1) to an AArch64 condition code.
// After D1, the burg always supplies a proper LIR_COND_*; the guest_opcode
// parameter is retained only for diagnostics if the invariant is violated.
static a64_cond_t lir_cond_to_a64(lir_cond_t lir_cond, uint8_t guest_opcode)
{
    // Primary path: neutral LIR condition (D1)
    switch (lir_cond) {
        case LIR_COND_EQ:   return COND_EQ;
        case LIR_COND_NE:   return COND_NE;
        case LIR_COND_LT:   return COND_LT;   // signed
        case LIR_COND_GE:   return COND_GE;
        case LIR_COND_LE:   return COND_LE;
        case LIR_COND_GT:   return COND_GT;
        case LIR_COND_LTU:  return COND_LO;   // unsigned
        case LIR_COND_GEU:  return COND_HS;
        case LIR_COND_LEU:  return COND_LS;
        case LIR_COND_GTU:  return COND_HI;
        case LIR_COND_NONE:
            break;
    }

    // D1 finished: burg_a64 (and x64) now always produce neutral LIR_COND_* for
    // compare/branch nodes. Reaching here means a burg site forgot to set l->cond.
    fprintf(stderr,
            "FATAL (D1): lir_cond_to_a64 called with LIR_COND_NONE and guest_opcode=0x%02X\n"
            "            burg lowering did not set a neutral condition kind.\n",
            guest_opcode);
    abort();
}

// Invert an AArch64 condition for "skip the cold path" style side-exit emission.
// Works for the common paired conditions (EQ/NE, LT/GE, LO/HS, etc.).
static a64_cond_t invert_a64_cond(a64_cond_t c)
{
    return (a64_cond_t)(c ^ 1U);
}

// Save the 6 callee-saved registers we may use for slots 8..13. Called once
// at the very top of the JIT prologue, only when at least one such slot is
// actually live in the RA plan. Pairs match the slot ordering in
// slot_to_host[] below: (W19,W22), (W23,W24), (W25,W26).
static void cg_emit_callee_saved_save(stage5_cg_a64_ctx_t *cg)
{
    if (!cg->callee_saved_used) return;
    emit_stp_pre_x64(&cg->emit, W19, W22, -16);
    emit_stp_pre_x64(&cg->emit, W23, W24, -16);
    emit_stp_pre_x64(&cg->emit, W25, W26, -16);
}

// Restore the callee-saved set in LIFO order. Called before every emitted
// `ret` instruction (the final region epilogue AND every side-exit stub).
static void cg_emit_callee_saved_restore(stage5_cg_a64_ctx_t *cg)
{
    if (!cg->callee_saved_used) return;
    emit_ldp_post_x64(&cg->emit, W25, W26, 16);
    emit_ldp_post_x64(&cg->emit, W23, W24, 16);
    emit_ldp_post_x64(&cg->emit, W19, W22, 16);
}

// A8: write dirty guest registers from their host slots back to cpu->regs[].
// Emitted before every exit (main terminal + every side-exit stub) so that
// the guest's register file is consistent when the JIT hands control back
// to the dispatcher / shadow validator. r0 is never written (always zero
// by SLOW-32 convention). After emission, marks the registers clean so a
// subsequent exit in the same emission pass does not re-emit redundant
// stores (this matters because side-exit stubs are emitted inline during
// the LIR walk, but in practice the cold paths don't redefine registers).
static void cg_emit_writeback(stage5_cg_a64_ctx_t *cg)
{
    for (uint32_t g = 1; g < 32; g++) {
        if (!cg->gpr_dirty[g]) continue;
        a64_reg_t h = cg->gpr_to_host[g];
        if (h == A64_NOREG) continue;
        emit_str_w32_imm(&cg->emit, h, W20, g * 4);
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
    // Step 1: Initialize the host slots from the RA plan.
    // Slots 0..7  → caller-saved W8..W15 (free in the JIT body).
    // Slots 8..13 → callee-saved W19, W22..W26 (require save/restore).
    // ------------------------------------------------------------------
    memset(cg->host_regs, 0, sizeof(cg->host_regs));
    memset(cg->guest_in_slot, 0, sizeof(cg->guest_in_slot));
    cg->callee_saved_used = false;

    // A8: per-guest-register writeback tracking starts empty/clean.
    for (int g = 0; g < 32; g++) cg->gpr_to_host[g] = A64_NOREG;
    memset(cg->gpr_dirty, 0, sizeof(cg->gpr_dirty));

    // Build a quick lookup: SSA value_id → host register (for values the RA assigned)
    a64_reg_t value_to_host[STAGE5_SSA_MAX_VALUES];
    uint8_t   value_to_gpr [STAGE5_SSA_MAX_VALUES];
    memset(value_to_host, 0xFF, sizeof(value_to_host));  // A64_NOREG sentinel (all bytes 0xFF => -1)
    memset(value_to_gpr,  0,    sizeof(value_to_gpr));   // 0 = no original guest GPR known (r0 is valid but rare here)

    static const a64_reg_t slot_to_host[STAGE5_A64_MAX_HOST_SLOTS] = {
        // caller-saved (free during the JIT body)
        W8,  W9,  W10, W11, W12, W13, W14, W15,
        // callee-saved (must be paired with stp/ldp around the body)
        W19, W22, W23, W24, W25, W26,
    };

    // E1 (deeper): Build precise prologue loads.
    //
    // For each guest register that the lifter says is live-in on entry
    // (cfg_blocks[0].live_in_mask), we want to load it exactly once — into
    // the host slot that holds the "incoming" SSA value for that register
    // (the interval with the earliest start among those derived from it).
    //
    // This is the "unique interval whose start_idx corresponds to first
    // reference of the live-in" that the original E1 description asked for.
    bool gpr_has_prologue_load[32] = {0};

    for (uint16_t i = 0; i < ra_plan->interval_count; i++) {
        const stage5_ra_interval_t *iv = &ra_plan->intervals[i];
        if (iv->value_id == 0 || iv->spilled || iv->assigned_slot < 0)
            continue;

        int slot = iv->assigned_slot;
        if (slot >= 0 && slot < STAGE5_A64_MAX_HOST_SLOTS) {
            a64_reg_t hreg = slot_to_host[slot];
            uint8_t gpr = ssa->value_to_reg[iv->value_id];

            cg->host_regs[slot] = hreg;
            if (slot >= STAGE5_A64_CALLER_SAVED_SLOTS)
                cg->callee_saved_used = true;

            if (iv->value_id < STAGE5_SSA_MAX_VALUES) {
                value_to_host[iv->value_id] = hreg;
                value_to_gpr [iv->value_id] = gpr;
            }

            // Decide if this interval's value needs to be loaded in the prologue.
            bool is_live_in_gpr = false;
            if (region->cfg_valid && region->cfg_block_count > 0) {
                uint32_t entry_live = region->cfg_blocks[0].live_in_mask;
                if (gpr != 0 && (entry_live & (1u << gpr))) {
                    is_live_in_gpr = true;
                }
            } else if (iv->start_idx == 0) {
                is_live_in_gpr = true;
            }

            if (is_live_in_gpr && !gpr_has_prologue_load[gpr]) {
                cg->guest_in_slot[slot] = gpr;
                gpr_has_prologue_load[gpr] = true;
            }
        }
    }

    // ------------------------------------------------------------------
    // Step 2: Prologue (E1 deeper).
    //   (a) Save callee-saved regs we plan to clobber (only if used).
    //   (b) For every guest reg in the lifter's entry live_in_mask, load it
    //       exactly once into the earliest RA interval that carries its
    //       incoming value. This is the "unique first-reference interval"
    //       mapping described in the original E1 note.
    // ------------------------------------------------------------------
    cg_emit_callee_saved_save(cg);

    for (int s = 0; s < STAGE5_A64_MAX_HOST_SLOTS; s++) {
        a64_reg_t hreg = cg->host_regs[s];
        uint8_t   gpr  = cg->guest_in_slot[s];
        if (hreg == A64_NOREG || gpr == 0)
            continue;

        emit_ldr_w32_imm(&cg->emit, hreg, W20, (uint32_t)(gpr * 4));

        // A8: this guest reg's current host carrier is now `hreg`, and the
        // in-register value matches cpu->regs[gpr] (just loaded), so clean.
        cg->gpr_to_host[gpr] = hreg;
        cg->gpr_dirty[gpr]   = false;
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

        // A8: if dst_h currently holds the live, dirty value of some earlier
        // guest register (the RA reused this host slot), flush that value to
        // cpu->regs[] before we overwrite it. Without this the final
        // cg_emit_writeback() at exit would read the host reg's *new* contents
        // and store them under the *old* gpr's offset — corrupting state.
        if (dst_h != A64_NOREG) {
            for (uint32_t g = 1; g < 32; g++) {
                if (cg->gpr_dirty[g] && cg->gpr_to_host[g] == dst_h) {
                    emit_str_w32_imm(&cg->emit, dst_h, W20, g * 4);
                    cg->gpr_dirty[g]   = false;
                    cg->gpr_to_host[g] = A64_NOREG;
                }
            }
        }

        switch (n->op) {
            case LIR_OP_MOV_RI:
                if (dst_h != A64_NOREG) {
                    emit_mov_w32_imm32(&cg->emit, dst_h, (uint32_t)n->imm);
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
                    int32_t imm = n->imm;
                    if (imm != 0) {
                        if (imm > 0 && imm <= 4095) {
                            emit_add_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)imm);
                        } else if (imm < 0 && -imm <= 4095) {
                            emit_sub_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)(-imm));
                        } else {
                            // Out of 12-bit imm range: materialize and use RR form (W17 scratch)
                            emit_mov_w32_imm32(&cg->emit, W17, (uint32_t)imm);
                            emit_add_w32(&cg->emit, dst_h, dst_h, W17);
                        }
                    }
                }
                break;

            case LIR_OP_ADD_RR: {
                // Three-operand form (D1 reshape for A64)
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    emit_add_w32(&cg->emit, dst_h, src0, src1);   // native three-operand
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
                    int32_t imm = n->imm;
                    if (imm != 0) {
                        if (imm > 0 && imm <= 4095) {
                            emit_sub_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)imm);
                        } else if (imm < 0 && -imm <= 4095) {
                            emit_add_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)(-imm));
                        } else {
                            emit_mov_w32_imm32(&cg->emit, W17, (uint32_t)imm);
                            emit_sub_w32(&cg->emit, dst_h, dst_h, W17);
                        }
                    }
                }
                break;

            case LIR_OP_SUB_RR: {
                // Three-operand form (D1 reshape)
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES) ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    emit_sub_w32(&cg->emit, dst_h, src0, src1);
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
                    emit_and_w32(&cg->emit, dst_h, src0, src1);
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
                    emit_orr_w32(&cg->emit, dst_h, src0, src1);
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
                    emit_eor_w32(&cg->emit, dst_h, src0, src1);
                }
                break;
            }

            // ------------------------------------------------------------------
            // CMP / TEST nodes: emit the compare (for flag side-effect).
            // The boolean result (if any) is materialized by a following LIR_OP_SETCC.
            // After A1, dummy CMP nodes (dst_v=0 from BURG) correctly yield dst_h = NOREG
            // and we simply emit the cmp for the subsequent SETCC or fused consumer.
            // ------------------------------------------------------------------
            case LIR_OP_CMP_RR:
            case LIR_OP_CMP_RI:
            case LIR_OP_TEST_RR: {
                // We no longer try to cset here (that belongs to SETCC).
                // Emit the compare whenever the sources have host registers.
                if (n->op == LIR_OP_CMP_RI) {
                    a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                     ? value_to_host[n->src_v[0]] : A64_NOREG;
                    if (src0 != A64_NOREG) {
                        int32_t imm = n->imm;
                        if (imm >= 0 && imm <= 4095) {
                            emit_cmp_w32_imm(&cg->emit, src0, (uint32_t)imm);
                        } else if (imm < 0 && -imm <= 4095) {
                            // cmp with negative imm: emit cmp with positive and swap the cond?
                            // For simplicity in narrow path, materialize.
                            emit_mov_w32_imm32(&cg->emit, W17, (uint32_t)imm);
                            emit_cmp_w32_w32(&cg->emit, src0, W17);
                        } else {
                            emit_mov_w32_imm32(&cg->emit, W17, (uint32_t)imm);
                            emit_cmp_w32_w32(&cg->emit, src0, W17);
                        }
                    }
                } else {
                    a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                     ? value_to_host[n->src_v[0]] : A64_NOREG;
                    a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                     ? value_to_host[n->src_v[1]] : A64_NOREG;
                    if (src0 != A64_NOREG && src1 != A64_NOREG) {
                        emit_cmp_w32_w32(&cg->emit, src0, src1);
                    }
                }
                break;
            }

            case LIR_OP_SETCC: {
                // Materialize the boolean result of a preceding CMP/TEST into dst_h.
                // Uses the same cond mapping as the fused and CMP paths.
                if (dst_h == A64_NOREG) break;

                a64_cond_t a64cond = lir_cond_to_a64(n->cond, n->guest_opcode);
                emit_cset_w32(&cg->emit, dst_h, a64cond);
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
                    int32_t d = n->disp;
                    if (d > 0 && d <= 4095) {
                        emit_add_w32_imm(&cg->emit, dst_h, base_h, (uint32_t)d);
                    } else if (d < 0 && -d <= 4095) {
                        emit_sub_w32_imm(&cg->emit, dst_h, base_h, (uint32_t)(-d));
                    } else {
                        emit_mov_w32_imm32(&cg->emit, W17, (uint32_t)d);
                        emit_add_w32(&cg->emit, dst_h, base_h, W17);
                    }
                } else if (base_h != dst_h) {
                    emit_mov_w32_w32(&cg->emit, dst_h, base_h);
                }
                break;
            }

            // Shifts (very common)
            case LIR_OP_SHL_RI:
                // Three-operand form (A64).
                // For immediate shifts we still often need the mov because
                // the immediate may not fit the 12-bit encoding.
                if (dst_h != A64_NOREG) {
                    a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                      ? value_to_host[n->src_v[0]] : dst_h;
                    if (src_h != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                    emit_lsl_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)n->imm);
                }
                break;

            case LIR_OP_SHR_RI:
                // Three-operand form (A64)
                if (dst_h != A64_NOREG) {
                    a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                      ? value_to_host[n->src_v[0]] : dst_h;
                    if (src_h != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                    emit_lsr_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)n->imm);
                }
                break;

            case LIR_OP_SAR_RI:
                // Three-operand form (A64)
                if (dst_h != A64_NOREG) {
                    a64_reg_t src_h = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                      ? value_to_host[n->src_v[0]] : dst_h;
                    if (src_h != dst_h)
                        emit_mov_w32_w32(&cg->emit, dst_h, src_h);
                    emit_asr_w32_imm(&cg->emit, dst_h, dst_h, (uint32_t)n->imm);
                }
                break;

            case LIR_OP_SHL_RR: {
                // Three-operand form
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    emit_lslv_w32(&cg->emit, dst_h, src0, src1);
                }
                break;
            }

            case LIR_OP_SHR_RR: {
                // Three-operand form
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    emit_lsrv_w32(&cg->emit, dst_h, src0, src1);
                }
                break;
            }

            case LIR_OP_SAR_RR: {
                // Three-operand form
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    emit_asrv_w32(&cg->emit, dst_h, src0, src1);
                }
                break;
            }

            // ------------------------------------------------------------------
            // A64-native compare-against-zero terminals (CBZ / CBNZ).
            // CBZ/CBNZ share the same imm19 encoding as B.cond, so the patch
            // logic (mask 0x00FFFFE0 << 5) is bit-identical and the post-pass
            // patcher needs no changes.
            // ------------------------------------------------------------------
            case LIR_OP_CBZ:
            case LIR_OP_CBNZ: {
                a64_reg_t src = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                ? value_to_host[n->src_v[0]] : A64_NOREG;
                if (src == A64_NOREG)
                    return cg_bail(cg); // can't zero-test a value we don't hold in a host reg

                uint32_t tgt = n->guest_pc + 4 + n->imm;
                bool is_internal = !n->is_side_exit &&
                                   tgt >= region->start_pc &&
                                   tgt < region->end_pc;

                if (is_internal) {
                    size_t patch_off = emit_offset(&cg->emit);
                    if (n->op == LIR_OP_CBZ)
                        emit_cbz_w32(&cg->emit, src, 0);
                    else
                        emit_cbnz_w32(&cg->emit, src, 0);

                    if (cg->internal_branch_count < 32) {
                        int idx = cg->internal_branch_count++;
                        cg->internal_branches[idx].patch_offset = patch_off;
                        cg->internal_branches[idx].target_pc    = tgt;
                    } else {
                        return cg_bail(cg);
                    }
                } else {
                    // Side-exit: emit the *inverse* zero-test so the JIT skips
                    // the cold-path stub when the original condition is false.
                    size_t skip_off = emit_offset(&cg->emit);
                    if (n->op == LIR_OP_CBZ) {
                        // Original: branch if zero. Skip stub if non-zero.
                        emit_cbnz_w32(&cg->emit, src, 0);
                    } else {
                        // Original: branch if non-zero. Skip stub if zero.
                        emit_cbz_w32(&cg->emit, src, 0);
                    }

                    if (cg->exit_count < STAGE5_A64_MAX_EXITS) {
                        int ex = cg->exit_count++;
                        cg->exits[ex].target_pc    = tgt;
                        cg->exits[ex].patch_offset = skip_off;
                        cg->exits[ex].is_side_exit = true;
                    }

                    // Cold side-exit stub: flush dirty guest regs, then write
                    // pc + exit_reason and ret. cg_emit_writeback uses W20 as
                    // the cpu base — same as the prologue loads — so the
                    // round-trip is consistent.
                    cg_emit_writeback(cg);
                    emit_mov_w32_imm32(&cg->emit, W0, tgt);
                    emit_str_w32_imm(&cg->emit, W0, W20, 0x80);
                    emit_mov_w32_imm32(&cg->emit, W0, EXIT_BRANCH);  // 0
                    emit_str_w32_imm(&cg->emit, W0, W20, 0x84);
                    cg_emit_callee_saved_restore(cg);
                    emit_ret_lr(&cg->emit);

                    // Patch the skip branch to land past the stub.
                    size_t after_stub = emit_offset(&cg->emit);
                    int32_t byte_disp = (int32_t)(after_stub - skip_off);
                    if ((byte_disp % 4) == 0) {
                        int32_t word_disp = byte_disp / 4;
                        uint32_t *p = (uint32_t *)(cg->emit.buf + skip_off);
                        uint32_t inst = *p;
                        inst &= ~0x00FFFFE0U;
                        inst |= ((uint32_t)word_disp & 0x7FFFFU) << 5;
                        *p = inst;
                    }
                }
                break;
            }

            // ------------------------------------------------------------------
            // Fused and plain conditional branches (JCC family) — A64 experimental path
            // Uses neutral LIR conditions and the skip-stub side-exit strategy.
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

                    // Use the neutral LIR condition we received from the A64 BURG.
                    a64_cond_t a64c = lir_cond_to_a64(n->cond, n->guest_opcode);

                    emit_b_cond(&cg->emit, a64c, 0);

                    // Record in the dedicated internal branch fixup array
                    if (cg->internal_branch_count < 32) {
                        int idx = cg->internal_branch_count++;
                        cg->internal_branches[idx].patch_offset = patch_off;
                        cg->internal_branches[idx].target_pc    = tgt;
                    } else {
                        // A10: too many internal branches for the narrow path — fail cleanly
                        return cg_bail(cg);
                    }
                } else {
                    // Real side-exit (or out-of-region) conditional branch.
                    // Emit:   b.<inverse_cond>  skip_stub
                    //         <side-exit stub: write target_pc + exit_reason + ret>
                    //         <fallthrough continues here>
                    // This puts the (rare) exit cost on the cold path and uses the
                    // correct condition instead of the previous hard-coded COND_EQ + #0 no-op.
                    a64_cond_t take_cond = lir_cond_to_a64(n->cond, n->guest_opcode);
                    a64_cond_t skip_cond = invert_a64_cond(take_cond);

                    size_t skip_patch_off = emit_offset(&cg->emit);
                    emit_b_cond(&cg->emit, skip_cond, 0);   // placeholder displacement

                    // Record for the driver / translated_block (target_pc is what matters most)
                    if (cg->exit_count < STAGE5_A64_MAX_EXITS) {
                        int ex = cg->exit_count++;
                        cg->exits[ex].target_pc    = tgt;
                        cg->exits[ex].patch_offset = skip_patch_off;
                        cg->exits[ex].is_side_exit = true;
                    }

                    // Emit the side-exit stub (cold path): flush dirty guest
                    // regs, then write pc + exit_reason and ret.
                    cg_emit_writeback(cg);
                    emit_mov_w32_imm32(&cg->emit, W0, tgt);
                    emit_str_w32_imm(&cg->emit, W0, W20, 0x80);     // cpu->pc = target
                    emit_mov_w32_imm32(&cg->emit, W0, EXIT_BRANCH); // 0
                    emit_str_w32_imm(&cg->emit, W0, W20, 0x84);     // cpu->exit_reason
                    cg_emit_callee_saved_restore(cg);
                    emit_ret_lr(&cg->emit);

                    // Patch the skip branch displacement (word offset)
                    size_t after_stub = emit_offset(&cg->emit);
                    int32_t byte_disp = (int32_t)(after_stub - (skip_patch_off + 4));
                    if ((byte_disp % 4) == 0) {
                        int32_t word_disp = byte_disp / 4;
                        uint32_t *p = (uint32_t *)(cg->emit.buf + skip_patch_off);
                        uint32_t inst = *p;
                        inst &= ~0x00FFFFE0U;
                        inst |= ((uint32_t)word_disp & 0x7FFFFU) << 5;
                        *p = inst;
                    }
                }
                break;
            }

            case LIR_OP_IMUL_RR: {
                // Three-operand form
                a64_reg_t src0 = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[0]] : A64_NOREG;
                a64_reg_t src1 = (n->src_v[1] < STAGE5_SSA_MAX_VALUES)
                                 ? value_to_host[n->src_v[1]] : A64_NOREG;
                if (dst_h != A64_NOREG && src0 != A64_NOREG && src1 != A64_NOREG) {
                    emit_mul_w32(&cg->emit, dst_h, src0, src1);
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
                        // remainder = dividend - (quotient * divisor)  — three-operand style
                        emit_sdiv_w32(&cg->emit, W16, src0, src1);
                        emit_msub_w32(&cg->emit, dst_h, W16, src1, src0);
                    }
                }
                break;
            }

            case LIR_OP_UDIV: {
                // Unsigned division (quotient).
                // Note (A9): In the current SLOW-32 ISA there are no separate
                // unsigned division opcodes (only OP_DIV / OP_REM). This path
                // is currently dead / future-proofing. When/ if unsigned div
                // is added to the guest ISA or through software lowering, this
                // will be used.
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

            case LIR_OP_CALL: {
                // Intra-region call (rare in current lifter — JALs terminate).
                // Only indirect (blr) supported here; direct JALs are handled as
                // region terminals in G2 (link + pc=target, dispatcher enters callee).
                if (n->src_v[0] != 0) {
                    a64_reg_t target_reg = (n->src_v[0] < STAGE5_SSA_MAX_VALUES)
                                           ? value_to_host[n->src_v[0]] : A64_NOREG;
                    if (target_reg != A64_NOREG) {
                        emit_blr(&cg->emit, target_reg);
                    }
                }
                break;
            }

            // Region terminators (JMP / RET / SYSCALL): nothing to emit inline.
            // The terminal-handling step below derives next_pc and exit_reason
            // from the trailing LIR node and emits the writeback + pc/exit_reason
            // store + ret. Suppressing them here lets cg_bail() stay reserved for
            // ops we genuinely cannot translate.
            //   JMP     — direct unconditional jump out of the region (EXIT_BRANCH).
            //   RET     — any JALR (incl. returns). G2 emits runtime target =
            //             regs[rs1]+imm and optional link-reg write; exits with
            //             EXIT_INDIRECT so dispatcher can follow.
            //   SYSCALL — HALT / YIELD / DEBUG (see G3 for precise codes).
            case LIR_OP_JMP:
            case LIR_OP_RET:
            case LIR_OP_SYSCALL:
                break;

            // More ops coming (FP, more call/return variants, complex addressing, etc.)

            default:
                // Unsupported op for the current A64 emitter.
                // Return false so the driver knows we did not produce a complete translation
                // (and success counter is not incremented). This makes A6 failures loud.
                return cg_bail(cg);
        }

        // A8: if this LIR node defined a value that maps to a guest register,
        // the host reg holding that value is now the live carrier for that
        // guest reg, and the value differs from cpu->regs[g] until the next
        // writeback. Track it so the exit stubs can flush correctly.
        if (n->dst_v != 0 && n->dst_v < STAGE5_SSA_MAX_VALUES) {
            a64_reg_t def_h = value_to_host[n->dst_v];
            if (def_h != A64_NOREG) {
                uint8_t def_gpr = ssa->value_to_reg[n->dst_v];
                if (def_gpr != 0 && def_gpr < 32) {
                    cg->gpr_to_host[def_gpr] = def_h;
                    cg->gpr_dirty[def_gpr]   = true;
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

        // Patch the B.cond / CBZ / CBNZ at patch_off.
        // Must mask *while signed* so that negative (backward) displacements
        // keep their sign bits; casting after the & on a uint32_t of a negative
        // value was zeroing the sign extension and producing huge positive jumps.
        uint32_t uimm = (uint32_t)(word_disp & 0x7FFFF);

        uint32_t *p = (uint32_t *)(cg->emit.buf + patch_off);
        uint32_t inst = *p;
        inst &= ~0x00FFFFE0U;
        inst |= (uimm << 5) & 0x00FFFFE0U;
        *p = inst;
    }

    // ------------------------------------------------------------------
    // Step 4: Terminal handling (produce correct pc + exit_reason)
    // ------------------------------------------------------------------
    // We try to determine what the "next PC" should be after this region.
    // - JMP / direct JAL (CALL): target of the transfer (for JAL this is the
    //   *callee* entry, not the return site — G2 proper call semantics).
    // - JALR (RET): runtime computed, so we emit a load-and-add sequence.
    // - SYSCALL / others: stop the dispatcher.
    //
    // The final emission after writeback now performs the guest-visible side
    // effects for terminating JAL/JALR (link reg write + correct target pc).
    uint32_t next_pc = region->end_pc;          // default: fall through
    uint32_t exit_reason = EXIT_BLOCK_END;      // 9

    // G2 state for link-register + target emission on JAL/JALR terminals
    bool     emit_link     = false;
    uint8_t  link_rd       = 0;
    uint32_t link_value    = 0;
    uint32_t direct_target = 0;   // for JAL (const) or JMP
    bool     is_indirect   = false;
    uint8_t  jalr_rs1      = 0;
    int32_t  jalr_imm      = 0;
    a64_reg_t jalr_base_h  = A64_NOREG;

    for (int i = (int)lir->node_count - 1; i >= 0; i--) {
        const lir_node_t *last = &lir->nodes[i];
        if (last->op == LIR_OP_NOP) continue;

        if (last->op == LIR_OP_JMP) {
            next_pc = last->guest_pc + 4 + last->imm;
            exit_reason = EXIT_BRANCH;  // 0
            direct_target = next_pc;
            break;
        }
        if (last->op == LIR_OP_CALL) {
            // G2: JAL terminates the region. We emit the link write (if rd!=0)
            // and set pc to the *call target* so the dispatcher walks into the
            // callee on the next iteration instead of faking a return.
            uint32_t call_pc = last->guest_pc;
            uint32_t ret_addr = call_pc + 4;
            uint32_t tgt      = call_pc + 4 + (uint32_t)last->imm;
            uint8_t  rd       = last->rd;

            next_pc = tgt;                 // dispatcher will resume at callee
            exit_reason = EXIT_BRANCH;     // 0
            direct_target = tgt;
            if (rd != 0) {
                emit_link  = true;
                link_rd    = rd;
                link_value = ret_addr;
            }
            break;
        }
        if (last->op == LIR_OP_RET) {
            // G2: JALR (including returns). Target is dynamic: regs[rs1] + imm.
            // Emit runtime computation of target pc and optional link store.
            uint32_t ret_site = last->guest_pc + 4;
            uint8_t  rd       = last->rd;
            uint8_t  rs       = last->rs1;
            int32_t  im       = last->imm;

            next_pc = ret_site;            // actual target written at runtime
            exit_reason = EXIT_INDIRECT;   // 1
            is_indirect = true;
            jalr_rs1 = rs;
            jalr_imm = im;
            if (rd != 0) {
                emit_link  = true;
                link_rd    = rd;
                link_value = ret_site;
            }
            // Prefer the SSA value carrier for rs1 if present (most cases);
            // fall back to any gpr_to_host[rs] recorded by A8.
            if (last->src_v[0] != 0 && last->src_v[0] < STAGE5_SSA_MAX_VALUES) {
                jalr_base_h = value_to_host[last->src_v[0]];
            } else if (rs < 32) {
                jalr_base_h = cg->gpr_to_host[rs];
            }
            break;
        }
        if (last->op == LIR_OP_SYSCALL) {
            // G3: map the MIR/LIR imm (original guest opcode payload) to the
            // canonical exit codes so the dispatcher and validators see
            // EXIT_HALT / EXIT_DEBUG / EXIT_YIELD distinctly.
            uint32_t imm = (uint32_t)last->imm;
            next_pc = last->guest_pc + 4;
            if (imm == 0x7F)      exit_reason = EXIT_HALT;   // 2
            else if (imm == 0x52) exit_reason = EXIT_DEBUG;  // 3
            else if (imm == 0x51) exit_reason = EXIT_YIELD;  // 4
            else                  exit_reason = EXIT_HALT;
            // For DEBUG we also want the char in cpu->exit_info (0x88) for the
            // host to consume (future: MMIO ring or printf). The LIR node
            // carries src_v[0] for the char value if it was in an SSA reg.
            if (imm == 0x52 && last->src_v[0] != 0 &&
                last->src_v[0] < STAGE5_SSA_MAX_VALUES) {
                a64_reg_t ch_h = value_to_host[last->src_v[0]];
                if (ch_h != A64_NOREG) {
                    // Stash the low byte into exit_info right here (before wb
                    // and final stores).  The writeback itself won't touch it.
                    emit_strb_imm(&cg->emit, ch_h, W20, 0x88);  // exit_info
                }
            }
            break;
        }
    }

    // Record the exit information (useful for future real runtime / chaining)
    if (cg->exit_count < STAGE5_A64_MAX_EXITS) {
        int ex = cg->exit_count++;
        cg->exits[ex].target_pc    = next_pc;
        cg->exits[ex].patch_offset = emit_offset(&cg->emit);
        cg->exits[ex].is_side_exit = false;
    }

    // A8: flush dirty guest registers from their host carriers back into
    // cpu->regs[] before writing pc/exit_reason. Without this, validators
    // (and the future dispatcher) see stale guest state — most visibly the
    // SP mismatch on every fib/prime smoke run.
    cg_emit_writeback(cg);

    // G2: emit link-register update (for JAL/JALR that write rd) and the
    // correct target PC. For indirect JALR we emit a small load/add sequence
    // using any live carrier or falling back to the just-flushed cpu->regs[].
    if (emit_link && link_rd != 0) {
        emit_mov_w32_imm32(&cg->emit, W0, link_value);
        emit_str_w32_imm(&cg->emit, W0, W20, (uint32_t)link_rd * 4U);
    }

    if (is_indirect) {
        // runtime: target = (base from carrier or load) + imm; store to cpu->pc
        a64_reg_t base = jalr_base_h;
        if (base == A64_NOREG) {
            emit_ldr_w32_imm(&cg->emit, W1, W20, (uint32_t)jalr_rs1 * 4U);
            base = W1;
        }
        if (jalr_imm != 0) {
            emit_mov_w32_imm32(&cg->emit, W0, (uint32_t)jalr_imm);
            emit_add_w32(&cg->emit, W1, base, W0);
            base = W1;
        }
        emit_str_w32_imm(&cg->emit, base, W20, 0x80);  // cpu->pc = target
    } else {
        // const target (JMP, direct JAL/CALL, or fallthrough)
        uint32_t tgt = (direct_target != 0 ? direct_target : next_pc);
        emit_mov_w32_imm32(&cg->emit, W0, tgt);
        emit_str_w32_imm(&cg->emit, W0, W20, 0x80);    // cpu->pc
    }

    emit_mov_w32_imm32(&cg->emit, W0, exit_reason);
    emit_str_w32_imm(&cg->emit, W0, W20, 0x84);     // cpu->exit_reason

    cg_emit_callee_saved_restore(cg);
    emit_ret_lr(&cg->emit);

    stage5_codegen_a64_success++;
    return true;
}
