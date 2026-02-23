// SLOW-32 DBT Stage 5: Native Codegen Driver
// Implements the HIR -> SSA -> MIR -> BURG -> LIR -> RA -> Native Pipeline.

#include "stage5_codegen.h"
#include "stage5_mir.h"
#include "stage5_burg.h"
#include "stage5_lir.h"
#include "stage5_ssa.h"
#include "stage5_ra.h"
#include "emit_x64.h"
#include "cpu_state.h"
#include "block_cache.h"
#include "translate.h"
#include <stdio.h>
#include <string.h>

// Telemetry
uint32_t stage5_codegen_attempted;
uint32_t stage5_codegen_success;
uint32_t stage5_codegen_fallback;
uint32_t stage5_codegen_fallback_emit_node;

typedef struct stage5_cg_s {
    translate_ctx_t *ctx;
    emit_ctx_t *e;
    const stage5_lift_region_t *region;
    const stage5_ssa_overlay_t *ssa;
    const stage5_ra_plan_t *ra_plan;
    const stage5_lir_t *lir;
    
    int8_t ssa_value_slot[STAGE5_SSA_MAX_VALUES];
    uint16_t ssa_slot_value[STAGE5_RA_HOST_SLOTS];
    bool ssa_slot_dirty[STAGE5_RA_HOST_SLOTS];
    uint8_t ssa_slot_guest_reg[STAGE5_RA_HOST_SLOTS];
} stage5_cg_t;

// Standard x86-64 calling convention for FP helper
extern void dbt_fp_helper(dbt_cpu_state_t *cpu, uint32_t opcode, uint32_t rd, uint32_t rs1, uint32_t rs2);

static const x64_reg_t ra_hosts[STAGE5_RA_HOST_SLOTS] = {
    RBX, R12, R13, RSI, RDI, R11, R8, R9
};

// ============================================================================
// Native Runtime Helpers
// ============================================================================

static void cg_sync_all(stage5_cg_t *cg) {
    for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
        if (cg->ssa_slot_dirty[s] && cg->ssa_slot_guest_reg[s] > 0) {
            emit_mov_m32_r32(cg->e, RBP, GUEST_REG_OFFSET(cg->ssa_slot_guest_reg[s]), ra_hosts[s]);
            cg->ssa_slot_dirty[s] = false;
        }
    }
}

static void cg_exit_native(stage5_cg_t *cg, uint32_t next_pc, uint32_t reason) {
    cg_sync_all(cg);
    emit_mov_m32_imm32(cg->e, RBP, CPU_PC_OFFSET, next_pc);
    emit_mov_m32_imm32(cg->e, RBP, CPU_EXIT_REASON_OFFSET, reason);
    emit_ret(cg->e);
}

static x64_reg_t cg_resolve_val(stage5_cg_t *cg, uint16_t v, x64_reg_t scratch) {
    if (v == 0) { emit_xor_r32_r32(cg->e, scratch, scratch); return scratch; }
    int8_t slot = cg->ssa_value_slot[v];
    if (slot >= 0) return ra_hosts[slot];
    
    uint8_t gpr = cg->ssa->value_to_reg[v];
    if (gpr != 0) emit_mov_r32_m32(cg->e, scratch, RBP, GUEST_REG_OFFSET(gpr));
    else emit_xor_r32_r32(cg->e, scratch, scratch);
    return scratch;
}

// ============================================================================
// LIR Emission (Native)
// ============================================================================

static bool cg_emit_lir_node(stage5_cg_t *cg, const lir_node_t *l) {
    x64_reg_t dst_h = X64_NOREG;
    int8_t dst_slot = (l->dst_v != 0) ? cg->ssa_value_slot[l->dst_v] : -1;
    if (dst_slot >= 0) dst_h = ra_hosts[dst_slot];
    else if (l->dst_v != 0) dst_h = RAX;

    switch (l->op) {
        case LIR_OP_NOP: return true;
        case LIR_OP_MOV_RI:
            if (dst_h != X64_NOREG) emit_mov_r32_imm32(cg->e, dst_h, (uint32_t)l->imm);
            break;
        case LIR_OP_MOV_RR: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], RCX);
            if (dst_h != src && dst_h != X64_NOREG) emit_mov_r32_r32(cg->e, dst_h, src);
            break;
        }
        case LIR_OP_ADD_RI: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], dst_h);
            if (dst_h != src && dst_h != X64_NOREG) emit_mov_r32_r32(cg->e, dst_h, src);
            if (dst_h != X64_NOREG) emit_add_r32_imm32(cg->e, dst_h, l->imm);
            break;
        }
        case LIR_OP_ADD_RR: {
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], dst_h);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (dst_h != src0 && dst_h != X64_NOREG) emit_mov_r32_r32(cg->e, dst_h, src0);
            if (dst_h != X64_NOREG) emit_add_r32_r32(cg->e, dst_h, src1);
            break;
        }
        case LIR_OP_MOV_RM: {
            x64_reg_t base = cg_resolve_val(cg, l->src_v[0], RAX);
            if (base != RAX) emit_mov_r32_r32(cg->e, RAX, base);
            if (l->disp != 0) emit_add_r32_imm32(cg->e, RAX, l->disp);
            if (dst_h != X64_NOREG) emit_mov_r32_m32_idx(cg->e, dst_h, R14, RAX);
            break;
        }
        case LIR_OP_MOV_MR: {
            x64_reg_t base = cg_resolve_val(cg, l->src_v[0], RAX);
            x64_reg_t val = cg_resolve_val(cg, l->src_v[1], RCX);
            if (base != RAX) emit_mov_r32_r32(cg->e, RAX, base);
            if (l->disp != 0) emit_add_r32_imm32(cg->e, RAX, l->disp);
            emit_mov_m32_r32_idx(cg->e, R14, RAX, val);
            break;
        }
        case LIR_OP_JCC: {
            x64_reg_t h1 = cg_resolve_val(cg, l->src_v[0], RAX);
            x64_reg_t h2 = cg_resolve_val(cg, l->src_v[1], RCX);
            emit_cmp_r32_r32(cg->e, h1, h2);
            
            uint8_t cc = 0;
            switch (l->guest_opcode) {
                case 0x48: cc = 0x84; break; // BEQ -> JE (rel32 version)
                case 0x49: cc = 0x85; break; // BNE -> JNE
                case 0x4A: cc = 0x8C; break; // BLT -> JL
                case 0x4B: cc = 0x8D; break; // BGE -> JGE
                case 0x4C: cc = 0x82; break; // BLTU -> JB
                case 0x4D: cc = 0x83; break; // BGEU -> JAE
                default: return false;
            }

            uint32_t taken_pc = l->guest_pc + 4 + l->imm;
            uint32_t fall_pc = l->guest_pc + 4;

            size_t jcc_patch = emit_offset(cg->e);
            emit_byte(cg->e, 0x0F);
            emit_byte(cg->e, cc);
            emit_dword(cg->e, 0); // Placeholder

            if (l->is_side_exit) {
                // Not taken: continue
                size_t taken_path = emit_offset(cg->e);
                cg_exit_native(cg, taken_pc, EXIT_BRANCH);
                emit_patch_rel32(cg->e, jcc_patch + 2, taken_path);
            } else {
                // Terminal
                cg_exit_native(cg, fall_pc, EXIT_BRANCH);
                size_t taken_path = emit_offset(cg->e);
                cg_exit_native(cg, taken_pc, EXIT_BRANCH);
                emit_patch_rel32(cg->e, jcc_patch + 2, taken_path);
            }
            return true;
        }
        case LIR_OP_SYSCALL:
            cg_exit_native(cg, l->guest_pc + 4, (l->imm == 0x52) ? EXIT_DEBUG : EXIT_HALT);
            return true;
        case LIR_OP_CALL:
            cg_exit_native(cg, l->guest_pc + l->imm, EXIT_BRANCH);
            return true;
        case LIR_OP_RET:
            cg_exit_native(cg, 0, EXIT_INDIRECT); 
            return true;
        default: return false;
    }

    if (l->dst_v != 0) {
        if (dst_slot >= 0) {
            cg->ssa_slot_value[dst_slot] = l->dst_v;
            cg->ssa_slot_dirty[dst_slot] = true;
            cg->ssa_slot_guest_reg[dst_slot] = cg->ssa->value_to_reg[l->dst_v];
        } else {
            uint8_t gpr = cg->ssa->value_to_reg[l->dst_v];
            if (gpr != 0) emit_mov_m32_r32(cg->e, RBP, GUEST_REG_OFFSET(gpr), dst_h);
        }
    }
    return true;
}

// ============================================================================
// Pipeline Driver
// ============================================================================

extern void stage5_lir_optimize(stage5_lir_t *lir, const stage5_ssa_overlay_t *ssa);

bool stage5_codegen(translate_ctx_t *ctx, const stage5_lift_region_t *region, uint32_t guest_pc,
                    int terminal_idx, int fuse_cmp_idx, int emitted_pattern,
                    bool synth_block_end, bool side_exit_enabled) {
    stage5_codegen_attempted++;
    stage5_cg_t cg = {0};
    cg.ctx = ctx; cg.e = &ctx->emit; cg.region = region;
    memset(cg.ssa_value_slot, -1, sizeof(cg.ssa_value_slot));

    stage5_ssa_overlay_t ssa;
    if (!stage5_ssa_build_overlay(region, &ssa)) {
        stage5_codegen_fallback++;
        return false;
    }
    cg.ssa = &ssa;

    stage5_mir_t mir;
    stage5_mir_build(region, &ssa, &mir);

    stage5_lir_t lir;
    if (!stage5_burg_lower(&mir, &ssa, &lir)) {
        stage5_codegen_fallback++;
        return false;
    }
    cg.lir = &lir;

    stage5_lir_optimize(&lir, &ssa);

    stage5_ra_plan_t ra_plan;
    if (!stage5_ra_build_plan_lir(&lir, &ssa, &ra_plan)) {
        stage5_codegen_fallback++;
        return false;
    }
    cg.ra_plan = &ra_plan;

    for (uint16_t i = 0; i < ra_plan.interval_count; i++) {
        if (!ra_plan.intervals[i].spilled) {
            cg.ssa_value_slot[ra_plan.intervals[i].value_id] = ra_plan.intervals[i].assigned_slot;
        }
    }

    // Load live-ins
    for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
        for (uint16_t i = 0; i < ra_plan.interval_count; i++) {
            if (ra_plan.intervals[i].assigned_slot == s && ra_plan.intervals[i].start_idx == 0) {
                uint16_t v = ra_plan.intervals[i].value_id;
                uint8_t gpr = ssa.value_to_reg[v];
                if (gpr != 0) {
                    emit_mov_r32_m32(cg.e, ra_hosts[s], RBP, GUEST_REG_OFFSET(gpr));
                    cg.ssa_slot_value[s] = v;
                    cg.ssa_slot_guest_reg[s] = gpr;
                }
                break;
            }
        }
    }

    bool ended = false;
    for (uint32_t i = 0; i < lir.node_count; i++) {
        if (!cg_emit_lir_node(&cg, &lir.nodes[i])) {
            stage5_codegen_fallback++;
            return false;
        }
        if (!lir.nodes[i].is_side_exit && (lir.nodes[i].op == LIR_OP_JCC || lir.nodes[i].op == LIR_OP_SYSCALL || lir.nodes[i].op == LIR_OP_RET || lir.nodes[i].op == LIR_OP_CALL)) {
            ended = true;
            break;
        }
    }

    if (!ended) {
        cg_exit_native(&cg, guest_pc + region->guest_inst_count * 4, EXIT_BRANCH);
    }

    stage5_codegen_success++;
    return true;
}
