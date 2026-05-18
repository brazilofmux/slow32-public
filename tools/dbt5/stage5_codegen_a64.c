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

    // The RA plan tells us which values got which slots.
    // For the prologue we only care about values that are live at entry
    // (their live range starts at or near the beginning of the LIR).
    for (uint16_t i = 0; i < ra_plan->interval_count; i++) {
        const stage5_ra_interval_t *iv = &ra_plan->intervals[i];
        if (iv->value_id == 0 || iv->spilled || iv->assigned_slot < 0)
            continue;

        // Only consider values live at the very start of the region
        if (iv->start_idx > 2)   // allow a tiny fudge for constants
            continue;

        uint8_t gpr = ssa->value_to_reg[iv->value_id];
        if (gpr == 0)
            continue;  // r0 is always zero, nothing to load

        int slot = iv->assigned_slot;
        if (slot >= 0 && slot < STAGE5_A64_MAX_HOST_SLOTS) {
            // Map the canonical AArch64 register for this slot.
            // We use a simple fixed set: W19, W20, W21, W22, W23, W24, W25, W26
            // (avoiding the ones already used by the runtime: W20=cpu, W21=mem, etc.)
            static const a64_reg_t slot_to_host[8] = {
                W8, W9, W10, W11, W12, W13, W14, W15
            };
            cg->host_regs[slot]     = slot_to_host[slot];
            cg->guest_in_slot[slot] = gpr;
            cg->slot_dirty[slot]    = false;
        }
    }

    // ------------------------------------------------------------------
    // Step 2: Emit a minimal prologue (load live-ins)
    // ------------------------------------------------------------------
    // For now we just emit the loads.  A real version will also handle
    // constants that the RA decided to keep in registers.
    for (int s = 0; s < STAGE5_A64_MAX_HOST_SLOTS; s++) {
        a64_reg_t hreg = cg->host_regs[s];
        uint8_t   gpr  = cg->guest_in_slot[s];
        if (hreg == A64_NOREG || gpr == 0)
            continue;

        // ldr Wd, [X20, #offset_of_guest_reg(gpr)]
        emit_ldr_w32_imm(&cg->emit, hreg, W20, (uint32_t)(gpr * 4));
    }

    // ------------------------------------------------------------------
    // Step 3: Handle the final terminal with proper exit recording
    // ------------------------------------------------------------------
    // Find the last meaningful LIR node and treat it as the block terminal.
    // For the first real version we only support direct terminals.
    uint32_t terminal_target = region->end_pc;  // conservative default
    bool     has_terminal    = false;

    for (int i = (int)lir->node_count - 1; i >= 0; i--) {
        const lir_node_t *last = &lir->nodes[i];
        if (last->op == LIR_OP_NOP)
            continue;

        if (last->op == LIR_OP_JMP || last->op == LIR_OP_CALL) {
            terminal_target = last->guest_pc + 4 + last->imm;
            has_terminal = true;
            break;
        }
        if (last->op == LIR_OP_RET || last->op == LIR_OP_SYSCALL) {
            // These are special (return to dispatcher or syscall)
            terminal_target = 0;   // special marker for now
            has_terminal = true;
            break;
        }
    }

    // Record the exit so the runtime can patch it later.
    if (has_terminal && cg->exit_count < STAGE5_A64_MAX_EXITS) {
        int ex = cg->exit_count++;
        cg->exits[ex].target_pc   = terminal_target;
        cg->exits[ex].patch_offset = emit_offset(&cg->emit);
        cg->exits[ex].is_side_exit = false;
    }

    // Emit a placeholder branch (displacement 0 for now).
    // A real implementation will either:
    //   - emit a direct relative branch if the target is known and close, or
    //   - emit a branch to a shared dispatcher stub.
    emit_b(&cg->emit, 0);   // placeholder — will be patched by the runtime

    stage5_codegen_a64_success++;
    return true;   // We emitted something real with exit recording!
}
