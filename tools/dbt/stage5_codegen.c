// SLOW-32 DBT Stage 5: Native Codegen Driver
// Implements the HIR -> SSA -> MIR -> BURG -> LIR -> RA -> Native Pipeline.
//
// ZERO Stage 4 fallback. All x86-64 emission is self-contained.
// Uses emit_x64.h primitives and block_cache.h APIs directly.

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
#include "dbt_limits.h"
#include <stdio.h>
#include <stdlib.h>
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

// RET-based exit: returns to C dispatcher. Used for HALT/DEBUG/YIELD.
static void cg_exit_native(stage5_cg_t *cg, uint32_t next_pc, uint32_t reason) {
    cg_sync_all(cg);
    emit_mov_m32_imm32(cg->e, RBP, CPU_PC_OFFSET, next_pc);
    emit_mov_m32_imm32(cg->e, RBP, CPU_EXIT_REASON_OFFSET, reason);
    emit_ret(cg->e);
}

// JMP-based exit: chains directly to target block or shared dispatcher.
static void cg_exit_chained(stage5_cg_t *cg, uint32_t target_pc) {
    translate_ctx_t *ctx = cg->ctx;
    emit_ctx_t *e = cg->e;

    cg_sync_all(cg);
    emit_mov_m32_imm32(e, RBP, CPU_PC_OFFSET, target_pc);

    // Record patch site BEFORE emitting the JMP
    uint8_t *patch_site = emit_ptr(e) + 1; // points to rel32 within JMP

    // Try to chain directly to already-translated target
    translated_block_t *target = ctx->cache ? cache_lookup(ctx->cache, target_pc) : NULL;
    if (target && target->host_code) {
        int64_t rel = (int64_t)(target->host_code - (patch_site + 4));
        if (rel >= INT32_MIN && rel <= INT32_MAX) {
            emit_jmp_rel32(e, (int32_t)rel);
        } else {
            // Too far for rel32 — use shared exit
            if (ctx->cache && ctx->cache->shared_branch_exit) {
                int64_t srel = (int64_t)(ctx->cache->shared_branch_exit - (patch_site + 4));
                emit_jmp_rel32(e, (int32_t)srel);
            } else {
                emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_BRANCH);
                emit_ret(e);
                return;
            }
        }
    } else if (ctx->cache && ctx->cache->shared_branch_exit) {
        // Not yet translated — jump to shared dispatcher stub
        int64_t rel = (int64_t)(ctx->cache->shared_branch_exit - (patch_site + 4));
        emit_jmp_rel32(e, (int32_t)rel);
    } else {
        // No cache — RET-based exit
        emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_BRANCH);
        emit_ret(e);
        return;
    }

    // Record exit for future chaining
    int exit_idx = ctx->exit_idx++;
    if (exit_idx < MAX_BLOCK_EXITS && ctx->block) {
        cache_record_exit(ctx->cache, ctx->block, exit_idx, target_pc, patch_site);
        if (!target || !target->host_code) {
            cache_record_pending_chain(ctx->cache, ctx->block, exit_idx, target_pc);
        }
    }
}

// Resolve an SSA value to an x86-64 host register.
// If the value is currently in its assigned slot, returns that host register.
// If the slot was repurposed (RA reuse), falls through to load from memory.
static x64_reg_t cg_resolve_val(stage5_cg_t *cg, uint16_t v, x64_reg_t scratch) {
    if (v == 0) { emit_xor_r32_r32(cg->e, scratch, scratch); return scratch; }
    int8_t slot = cg->ssa_value_slot[v];
    // Only use the slot if it currently holds THIS value (handles RA slot reuse)
    if (slot >= 0 && cg->ssa_slot_value[slot] == v) return ra_hosts[slot];

    uint8_t gpr = cg->ssa->value_to_reg[v];
    if (gpr != 0) emit_mov_r32_m32(cg->e, scratch, RBP, GUEST_REG_OFFSET(gpr));
    else emit_xor_r32_r32(cg->e, scratch, scratch);
    return scratch;
}

// Mark destination slot as dirty after writing.
// If the slot held a different dirty value, flush it first.
static void cg_mark_dst(stage5_cg_t *cg, const lir_node_t *l, x64_reg_t dst_h, int8_t dst_slot) {
    if (l->dst_v == 0) return;
    if (dst_slot >= 0) {
        uint8_t gpr = cg->ssa->value_to_reg[l->dst_v];
        // Invalidate any OTHER dirty slot mapping to the same guest register.
        // Without this, cg_sync_all could write a stale earlier definition
        // AFTER the latest one (higher slot number wins), corrupting the result.
        if (gpr != 0) {
            for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
                if (s != dst_slot && cg->ssa_slot_dirty[s] && cg->ssa_slot_guest_reg[s] == gpr) {
                    cg->ssa_slot_guest_reg[s] = 0;
                }
            }
        }
        // Old occupant already pre-flushed before instruction emission.
        cg->ssa_slot_value[dst_slot] = l->dst_v;
        cg->ssa_slot_dirty[dst_slot] = true;
        cg->ssa_slot_guest_reg[dst_slot] = gpr;
    } else {
        uint8_t gpr = cg->ssa->value_to_reg[l->dst_v];
        if (gpr != 0) emit_mov_m32_r32(cg->e, RBP, GUEST_REG_OFFSET(gpr), dst_h);
    }
}

// ============================================================================
// LIR Emission (Native x86-64, ZERO Stage 4 dependencies)
// ============================================================================

// Invert an x86 JCC condition code (0x84 JE → 0x85 JNE, etc.)
static uint8_t invert_x86_cc(uint8_t cc) {
    return cc ^ 1; // x86 condition codes: even/odd pairs are inverses
}

static bool cg_emit_lir_node(stage5_cg_t *cg, const lir_node_t *l, uint32_t lir_idx) {
    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = X64_NOREG;
    int8_t dst_slot = (l->dst_v != 0) ? cg->ssa_value_slot[l->dst_v] : -1;
    if (dst_slot >= 0) dst_h = ra_hosts[dst_slot];
    else if (l->dst_v != 0) dst_h = RAX;

    // Pre-flush: if destination slot holds a different dirty value,
    // flush it BEFORE the instruction overwrites the host register.
    // (cg_mark_dst runs AFTER emission, so the register would already hold
    // the new value — too late to recover the old one.)
    if (l->dst_v != 0 && dst_slot >= 0 &&
        cg->ssa_slot_dirty[dst_slot] &&
        cg->ssa_slot_value[dst_slot] != 0 &&
        cg->ssa_slot_value[dst_slot] != l->dst_v) {
        uint8_t old_gpr = cg->ssa_slot_guest_reg[dst_slot];
        if (old_gpr != 0) {
            emit_mov_m32_r32(cg->e, RBP, GUEST_REG_OFFSET(old_gpr), ra_hosts[dst_slot]);
        }
        cg->ssa_slot_dirty[dst_slot] = false;
    }

    switch (l->op) {
        case LIR_OP_NOP:
            return true;

        case LIR_OP_MOV_RI:
            if (dst_h != X64_NOREG) {
                if (l->imm == 0) emit_xor_r32_r32(e, dst_h, dst_h);
                else emit_mov_r32_imm32(e, dst_h, (uint32_t)l->imm);
            }
            break;

        case LIR_OP_MOV_RR: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], RCX);
            if (dst_h != src && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src);
            break;
        }

        // ---- Arithmetic ----

        case LIR_OP_ADD_RI: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], dst_h);
            if (dst_h != src && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src);
            if (dst_h != X64_NOREG && l->imm != 0) emit_add_r32_imm32(e, dst_h, l->imm);
            break;
        }
        case LIR_OP_ADD_RR: {
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], dst_h);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (dst_h != src0 && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src0);
            if (dst_h != X64_NOREG) emit_add_r32_r32(e, dst_h, src1);
            break;
        }

        case LIR_OP_SUB_RI: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], dst_h);
            if (dst_h != src && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src);
            if (dst_h != X64_NOREG) emit_sub_r32_imm32(e, dst_h, l->imm);
            break;
        }
        case LIR_OP_SUB_RR: {
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], dst_h);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (dst_h != src0 && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src0);
            if (dst_h != X64_NOREG) emit_sub_r32_r32(e, dst_h, src1);
            break;
        }

        case LIR_OP_AND_RI: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], dst_h);
            if (dst_h != src && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src);
            if (dst_h != X64_NOREG) emit_and_r32_imm32(e, dst_h, l->imm);
            break;
        }
        case LIR_OP_AND_RR: {
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], dst_h);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (dst_h != src0 && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src0);
            if (dst_h != X64_NOREG) emit_and_r32_r32(e, dst_h, src1);
            break;
        }

        case LIR_OP_OR_RI: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], dst_h);
            if (dst_h != src && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src);
            if (dst_h != X64_NOREG) emit_or_r32_imm32(e, dst_h, l->imm);
            break;
        }
        case LIR_OP_OR_RR: {
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], dst_h);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (dst_h != src0 && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src0);
            if (dst_h != X64_NOREG) emit_or_r32_r32(e, dst_h, src1);
            break;
        }

        case LIR_OP_XOR_RI: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], dst_h);
            if (dst_h != src && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src);
            if (dst_h != X64_NOREG) emit_xor_r32_imm32(e, dst_h, l->imm);
            break;
        }
        case LIR_OP_XOR_RR: {
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], dst_h);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (dst_h != src0 && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src0);
            if (dst_h != X64_NOREG) emit_xor_r32_r32(e, dst_h, src1);
            break;
        }

        // ---- Shifts ----

        case LIR_OP_SHL_RI: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], dst_h);
            if (dst_h != src && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src);
            if (dst_h != X64_NOREG) emit_shl_r32_imm8(e, dst_h, (uint8_t)(l->imm & 0x1F));
            break;
        }
        case LIR_OP_SHR_RI: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], dst_h);
            if (dst_h != src && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src);
            if (dst_h != X64_NOREG) emit_shr_r32_imm8(e, dst_h, (uint8_t)(l->imm & 0x1F));
            break;
        }
        case LIR_OP_SAR_RI: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], dst_h);
            if (dst_h != src && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src);
            if (dst_h != X64_NOREG) emit_sar_r32_imm8(e, dst_h, (uint8_t)(l->imm & 0x1F));
            break;
        }

        case LIR_OP_SHL_RR: {
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], dst_h);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (src1 != RCX) emit_mov_r32_r32(e, RCX, src1);
            if (dst_h != src0 && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src0);
            if (dst_h != X64_NOREG) emit_shl_r32_cl(e, dst_h);
            break;
        }
        case LIR_OP_SHR_RR: {
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], dst_h);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (src1 != RCX) emit_mov_r32_r32(e, RCX, src1);
            if (dst_h != src0 && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src0);
            if (dst_h != X64_NOREG) emit_shr_r32_cl(e, dst_h);
            break;
        }
        case LIR_OP_SAR_RR: {
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], dst_h);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (src1 != RCX) emit_mov_r32_r32(e, RCX, src1);
            if (dst_h != src0 && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src0);
            if (dst_h != X64_NOREG) emit_sar_r32_cl(e, dst_h);
            break;
        }

        // ---- Multiply ----

        case LIR_OP_IMUL_RR: {
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], dst_h);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (dst_h != src0 && dst_h != X64_NOREG) emit_mov_r32_r32(e, dst_h, src0);
            if (dst_h != X64_NOREG) emit_imul_r32_r32(e, dst_h, src1);
            break;
        }

        case LIR_OP_MULH_RR: {
            // Signed high multiply: IMUL_one src → result in EDX:EAX, want EDX
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], RAX);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (src0 != RAX) emit_mov_r32_r32(e, RAX, src0);
            emit_imul_one_r32(e, src1);  // EDX:EAX = EAX * src1 (signed)
            if (dst_h != X64_NOREG && dst_h != RDX) emit_mov_r32_r32(e, dst_h, RDX);
            // If dst_h is RDX, result is already there
            if (dst_h == X64_NOREG) break; // spilled — handled by cg_mark_dst
            if (dst_h != RDX) break; // already moved
            break;
        }

        case LIR_OP_MULHU_RR: {
            // Unsigned high multiply: MUL src → result in EDX:EAX, want EDX
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], RAX);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (src0 != RAX) emit_mov_r32_r32(e, RAX, src0);
            emit_mul_r32(e, src1);  // EDX:EAX = EAX * src1 (unsigned)
            if (dst_h != X64_NOREG && dst_h != RDX) emit_mov_r32_r32(e, dst_h, RDX);
            break;
        }

        // ---- Division ----

        case LIR_OP_IDIV: {
            // Signed divide: CDQ + IDIV. cond=0→quotient(EAX), cond=1→remainder(EDX)
            // Guard: INT32_MIN / -1 overflow → return INT32_MIN (SLOW-32 convention)
            x64_reg_t src0 = cg_resolve_val(cg, l->src_v[0], RAX);
            x64_reg_t src1 = cg_resolve_val(cg, l->src_v[1], RCX);
            if (src0 != RAX) emit_mov_r32_r32(e, RAX, src0);
            if (src1 != RCX) emit_mov_r32_r32(e, RCX, src1);

            // Guard: divisor == 0 → result = 0 (avoid SIGFPE)
            emit_test_r32_r32(e, RCX, RCX);
            size_t jnz_patch = emit_offset(e);
            emit_byte(e, 0x0F); emit_byte(e, 0x85); emit_dword(e, 0); // JNE over
            // divisor is zero: result = 0
            emit_xor_r32_r32(e, RAX, RAX);
            emit_xor_r32_r32(e, RDX, RDX);
            size_t jmp_done_patch = emit_offset(e) + 1;
            emit_jmp_rel32(e, 0); // JMP to done
            // divisor != 0
            emit_patch_rel32(e, jnz_patch + 2, emit_offset(e));

            // Guard: INT32_MIN / -1 overflow
            emit_cmp_r32_imm32(e, RAX, (int32_t)0x80000000u);
            size_t jne_ov = emit_offset(e);
            emit_byte(e, 0x0F); emit_byte(e, 0x85); emit_dword(e, 0); // JNE
            emit_cmp_r32_imm32(e, RCX, -1);
            size_t jne_ov2 = emit_offset(e);
            emit_byte(e, 0x0F); emit_byte(e, 0x85); emit_dword(e, 0); // JNE
            // overflow: quotient = INT32_MIN, remainder = 0
            emit_mov_r32_imm32(e, RAX, 0x80000000u);
            emit_xor_r32_r32(e, RDX, RDX);
            size_t jmp_done2 = emit_offset(e) + 1;
            emit_jmp_rel32(e, 0);

            emit_patch_rel32(e, jne_ov + 2, emit_offset(e));
            emit_patch_rel32(e, jne_ov2 + 2, emit_offset(e));

            // Normal path
            emit_cdq(e);
            emit_idiv_r32(e, RCX);

            emit_patch_rel32(e, jmp_done_patch, emit_offset(e));
            emit_patch_rel32(e, jmp_done2, emit_offset(e));

            // cond=0: quotient in EAX, cond=1: remainder in EDX
            x64_reg_t result_reg = (l->cond == 1) ? RDX : RAX;
            if (dst_h != X64_NOREG && dst_h != result_reg) {
                emit_mov_r32_r32(e, dst_h, result_reg);
            } else if (dst_h == X64_NOREG) {
                // Spilled — store from result_reg
                uint8_t gpr = cg->ssa->value_to_reg[l->dst_v];
                if (gpr != 0) emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(gpr), result_reg);
                return true; // skip cg_mark_dst
            }
            break;
        }

        // ---- Comparison ----

        case LIR_OP_CMP_RR: {
            x64_reg_t h1 = cg_resolve_val(cg, l->src_v[0], RAX);
            x64_reg_t h2 = cg_resolve_val(cg, l->src_v[1], RCX);
            emit_cmp_r32_r32(e, h1, h2);
            return true; // CMP sets flags, no dst to mark
        }

        case LIR_OP_CMP_RI: {
            x64_reg_t h1 = cg_resolve_val(cg, l->src_v[0], RAX);
            emit_cmp_r32_imm32(e, h1, l->imm);
            return true; // CMP sets flags, no dst to mark
        }

        case LIR_OP_SETCC: {
            // SETcc based on condition code in l->cond
            // The cond field holds the full setcc opcode byte (0x94=SETE, etc.)
            // For the emitter, we need to call the right emit_set* function.
            x64_reg_t out = (dst_h != X64_NOREG) ? dst_h : RAX;
            switch (l->cond) {
                case 0x94: emit_sete(e, out); break;   // EQ
                case 0x95: emit_setne(e, out); break;  // NE
                case 0x9C: emit_setl(e, out); break;   // LT (signed)
                case 0x92: emit_setb(e, out); break;   // LTU (unsigned below)
                case 0x9F: emit_setg(e, out); break;   // GT (signed)
                case 0x97: emit_seta(e, out); break;   // GTU (unsigned above)
                case 0x9E: emit_setle(e, out); break;  // LE (signed)
                case 0x96: emit_setbe(e, out); break;  // LEU (unsigned below-or-equal)
                case 0x9D: emit_setge(e, out); break;  // GE (signed)
                case 0x93: emit_setae(e, out); break;  // GEU (unsigned above-or-equal)
                default:
                    fprintf(stderr,
                            "FATAL: stage5_codegen: unsupported SETCC condition 0x%02X at pc=0x%08X\n",
                            l->cond, l->guest_pc);
                    abort();
            }
            emit_movzx_r32_r8(e, out, out);
            if (dst_h == X64_NOREG) {
                // Spilled
                uint8_t gpr = cg->ssa->value_to_reg[l->dst_v];
                if (gpr != 0) emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(gpr), out);
                return true;
            }
            break;
        }

        // ---- Memory Access (sub-word aware) ----

        case LIR_OP_MOV_RM: {
            // Load: dst = [R14 + addr + disp]
            x64_reg_t base = cg_resolve_val(cg, l->src_v[0], RAX);
            if (base != RAX) emit_mov_r32_r32(e, RAX, base);
            if (l->disp != 0) emit_add_r32_imm32(e, RAX, l->disp);
            if (dst_h == X64_NOREG) dst_h = RCX; // need somewhere to load into
            switch (l->size) {
                case 1:
                    if (l->is_signed) emit_movsx_r32_m8_idx(e, dst_h, R14, RAX);
                    else emit_movzx_r32_m8_idx(e, dst_h, R14, RAX);
                    break;
                case 2:
                    if (l->is_signed) emit_movsx_r32_m16_idx(e, dst_h, R14, RAX);
                    else emit_movzx_r32_m16_idx(e, dst_h, R14, RAX);
                    break;
                default: // 4
                    emit_mov_r32_m32_idx(e, dst_h, R14, RAX);
                    break;
            }
            break;
        }

        case LIR_OP_MOV_MR: {
            // Store: [R14 + addr + disp] = val
            x64_reg_t base = cg_resolve_val(cg, l->src_v[0], RAX);
            x64_reg_t val = cg_resolve_val(cg, l->src_v[1], RCX);
            if (base != RAX) emit_mov_r32_r32(e, RAX, base);
            if (l->disp != 0) emit_add_r32_imm32(e, RAX, l->disp);
            switch (l->size) {
                case 1: emit_mov_m8_r8_idx(e, R14, RAX, val); break;
                case 2: emit_mov_m16_r16_idx(e, R14, RAX, val); break;
                default: emit_mov_m32_r32_idx(e, R14, RAX, val); break;
            }
            return true; // stores have no dst to mark
        }

        // ---- Conditional Branch ----

        case LIR_OP_JCC: {
            x64_reg_t h1 = cg_resolve_val(cg, l->src_v[0], RAX);
            x64_reg_t h2 = cg_resolve_val(cg, l->src_v[1], RCX);
            emit_cmp_r32_r32(e, h1, h2);

            uint8_t cc = 0;
            switch (l->guest_opcode) {
                case 0x48: cc = 0x84; break; // BEQ -> JE
                case 0x49: cc = 0x85; break; // BNE -> JNE
                case 0x4A: cc = 0x8C; break; // BLT -> JL
                case 0x4B: cc = 0x8D; break; // BGE -> JGE
                case 0x4C: cc = 0x82; break; // BLTU -> JB
                case 0x4D: cc = 0x83; break; // BGEU -> JAE
                default: return false;
            }

            uint32_t taken_pc = l->guest_pc + 4 + l->imm;
            uint32_t fall_pc = l->guest_pc + 4;

            if (l->is_side_exit) {
                // Determine which direction the lifter traced by looking at the
                // next LIR node's guest_pc. The hot path (LIR continuation) is
                // whichever direction the lifter followed.
                uint32_t side_exit_pc = taken_pc;
                uint8_t side_cc = cc;

                // Check if lifter traced the taken path
                const stage5_lir_t *lir = cg->lir;
                if (lir_idx + 1 < lir->node_count &&
                    lir->nodes[lir_idx + 1].guest_pc == taken_pc) {
                    // Lifter traced taken: hot path = taken, cold = fall-through
                    // Invert condition so JCC fires on the cold (not-taken) path
                    side_cc = invert_x86_cc(cc);
                    side_exit_pc = fall_pc;
                }
                // else: lifter traced fall-through (default), side exit = taken_pc

                size_t jcc_patch = emit_offset(e);
                emit_byte(e, 0x0F);
                emit_byte(e, side_cc);
                emit_dword(e, 0); // placeholder rel32

                translate_ctx_t *ctx = cg->ctx;
                if (ctx->deferred_exit_count < MAX_BLOCK_EXITS) {
                    int di = ctx->deferred_exit_count++;
                    ctx->deferred_exits[di].jmp_patch_offset = jcc_patch + 2;
                    ctx->deferred_exits[di].target_pc = side_exit_pc;
                    ctx->deferred_exits[di].exit_idx = ctx->exit_idx++;
                    ctx->deferred_exits[di].branch_pc = l->guest_pc;
                    ctx->deferred_exits[di].force_full_flush = false;

                    // Snapshot current slot state for deferred stub emission.
                    // Slots may be repurposed between this side exit and the
                    // end of the block, so cg_sync_all at stub time would use
                    // stale guest_reg mappings.
                    for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
                        ctx->deferred_exits[di].dirty_snapshot[s] = cg->ssa_slot_dirty[s];
                        ctx->deferred_exits[di].guest_reg_snapshot[s] = cg->ssa_slot_guest_reg[s];
                    }
                    if (getenv("S5_TRACE")) {
                        fprintf(stderr, "[S5-SNAP] side_exit gpc=0x%08X target=0x%08X di=%d\n",
                                l->guest_pc, side_exit_pc, di);
                        for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
                            if (ctx->deferred_exits[di].dirty_snapshot[s]) {
                                fprintf(stderr, "  snap[%d] dirty=1 gpr=%u\n",
                                        s, ctx->deferred_exits[di].guest_reg_snapshot[s]);
                            }
                        }
                    }
                }
            } else {
                // Save dirty state before first exit (cg_sync_all clears dirty).
                // Both paths need the same flush sequence.
                bool saved_dirty[STAGE5_RA_HOST_SLOTS];
                uint8_t saved_gpr[STAGE5_RA_HOST_SLOTS];
                for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
                    saved_dirty[s] = cg->ssa_slot_dirty[s];
                    saved_gpr[s] = cg->ssa_slot_guest_reg[s];
                }

                size_t jcc_patch = emit_offset(e);
                emit_byte(e, 0x0F);
                emit_byte(e, cc);
                emit_dword(e, 0); // placeholder rel32
                // Terminal branch: emit both paths
                // Fall-through path
                cg_exit_chained(cg, fall_pc);

                // Restore dirty state for taken path
                for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
                    cg->ssa_slot_dirty[s] = saved_dirty[s];
                    cg->ssa_slot_guest_reg[s] = saved_gpr[s];
                }

                // Taken path
                size_t taken_start = emit_offset(e);
                cg_exit_chained(cg, taken_pc);
                emit_patch_rel32(e, jcc_patch + 2, taken_start);
            }
            return true;
        }

        // ---- JAL (Direct Call/Jump) ----

        case LIR_OP_CALL: {
            uint32_t return_pc = l->guest_pc + 4;
            uint32_t target_pc = l->guest_pc + l->imm;

            if (l->rd != 0) {
                // Save return address to link register
                emit_mov_r32_imm32(e, RAX, return_pc);
                // Write to guest register directly (bypass RA since we're about to exit)
                cg_sync_all(cg);
                emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(l->rd), RAX);

                // Push to RAS for return prediction
                // ras_top = (ras_top + 1) & RAS_MASK
                emit_mov_r32_m32(e, RCX, RBP, CPU_RAS_TOP_OFFSET);
                emit_add_r32_imm32(e, RCX, 1);
                emit_and_r32_imm32(e, RCX, RAS_MASK);
                emit_mov_m32_r32(e, RBP, CPU_RAS_TOP_OFFSET, RCX);
                // ras_stack[ras_top] = return_pc
                // Address: RBP + RAS_STACK_OFFSET + RCX*4
                emit_mov_r32_imm32(e, RAX, return_pc);
                emit_mov_m32_imm32_sib(e, RBP, RCX, 2, CPU_RAS_STACK_OFFSET, return_pc);
            } else {
                cg_sync_all(cg);
            }

            // Emit chained exit to target
            emit_mov_m32_imm32(e, RBP, CPU_PC_OFFSET, target_pc);

            uint8_t *patch_site = emit_ptr(e) + 1;
            translate_ctx_t *ctx = cg->ctx;
            translated_block_t *target = ctx->cache ? cache_lookup(ctx->cache, target_pc) : NULL;

            if (target && target->host_code) {
                int64_t rel = (int64_t)(target->host_code - (patch_site + 4));
                emit_jmp_rel32(e, (int32_t)rel);
            } else if (ctx->cache && ctx->cache->shared_branch_exit) {
                int64_t rel = (int64_t)(ctx->cache->shared_branch_exit - (patch_site + 4));
                emit_jmp_rel32(e, (int32_t)rel);
            } else {
                emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_BRANCH);
                emit_ret(e);
                return true;
            }

            int exit_idx = ctx->exit_idx++;
            if (exit_idx < MAX_BLOCK_EXITS && ctx->block) {
                cache_record_exit(ctx->cache, ctx->block, exit_idx, target_pc, patch_site);
                if (!target || !target->host_code)
                    cache_record_pending_chain(ctx->cache, ctx->block, exit_idx, target_pc);
            }
            return true;
        }

        // ---- JALR (Indirect Call/Jump/Return) ----

        case LIR_OP_RET: {
            uint32_t return_pc = l->guest_pc + 4;

            // Compute target: (rs1 + imm) & ~1
            x64_reg_t rs1_h = cg_resolve_val(cg, l->src_v[0], RAX);
            if (rs1_h != RAX) emit_mov_r32_r32(e, RAX, rs1_h);
            if (l->imm != 0) emit_add_r32_imm32(e, RAX, l->imm);
            emit_and_r32_imm32(e, RAX, (int32_t)~1u);

            if (l->rd != 0) {
                // Save target, write return address
                emit_mov_r32_r32(e, RCX, RAX);

                // Sync all cached registers
                cg_sync_all(cg);

                // Write link register
                emit_mov_m32_imm32(e, RBP, GUEST_REG_OFFSET(l->rd), return_pc);

                // Push to RAS
                emit_push_r64(e, RCX); // save target across RAS update
                emit_mov_r32_m32(e, RDX, RBP, CPU_RAS_TOP_OFFSET);
                emit_add_r32_imm32(e, RDX, 1);
                emit_and_r32_imm32(e, RDX, RAS_MASK);
                emit_mov_m32_r32(e, RBP, CPU_RAS_TOP_OFFSET, RDX);
                emit_mov_m32_imm32_sib(e, RBP, RDX, 2, CPU_RAS_STACK_OFFSET, return_pc);
                emit_pop_r64(e, RAX); // restore target
            } else {
                cg_sync_all(cg);
            }

            // Set PC and exit reason
            emit_mov_m32_r32(e, RBP, CPU_PC_OFFSET, RAX);
            emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_INDIRECT);
            emit_ret(e);
            return true;
        }

        // ---- SYSCALL (HALT/DEBUG/YIELD) ----

        case LIR_OP_SYSCALL: {
            uint32_t next_pc = l->guest_pc + 4;

            if (l->imm == 0x52) {
                // DEBUG: output character from rs1
                x64_reg_t val = cg_resolve_val(cg, l->src_v[0], RAX);
                if (val != RAX) emit_mov_r32_r32(e, RAX, val);
                cg_sync_all(cg);
                emit_mov_m32_r32(e, RBP, CPU_EXIT_INFO_OFFSET, RAX);
                emit_mov_m32_imm32(e, RBP, CPU_PC_OFFSET, next_pc);
                emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_DEBUG);
                emit_ret(e);
            } else if (l->imm == 0x51) {
                // YIELD
                cg_exit_native(cg, next_pc, EXIT_YIELD);
            } else {
                // HALT (0x7F)
                cg_sync_all(cg);
                emit_mov_m32_imm32(e, RBP, CPU_PC_OFFSET, l->guest_pc);
                emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_HALT);
                emit_ret(e);
            }
            return true;
        }

        // ---- FP Helper (call out to C function) ----

        case LIR_OP_FP_HELPER: {
            // Sync all registers, call C helper, reload
            cg_sync_all(cg);

            // x86-64 SysV ABI: rdi=cpu, esi=opcode, edx=rd, ecx=rs1, r8d=rs2
            emit_mov_r64_r64(e, RDI, RBP);  // cpu
            emit_mov_r32_imm32(e, RSI, (uint32_t)l->guest_opcode);
            emit_mov_r32_imm32(e, RDX, (uint32_t)l->rd);
            emit_mov_r32_imm32(e, RCX, (uint32_t)l->rs1);
            emit_mov_r32_imm32(e, R8, (uint32_t)l->rs2);

            // call dbt_fp_helper
            emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)dbt_fp_helper);
            emit_call_r64(e, RAX);

            // Reload any cached registers that the C call may have clobbered
            for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
                if (cg->ssa_slot_guest_reg[s] > 0) {
                    emit_mov_r32_m32(e, ra_hosts[s], RBP, GUEST_REG_OFFSET(cg->ssa_slot_guest_reg[s]));
                    cg->ssa_slot_dirty[s] = false;
                }
            }
            return true; // FP helper writes directly to cpu state
        }

        case LIR_OP_LEA: {
            x64_reg_t src = cg_resolve_val(cg, l->src_v[0], dst_h);
            if (dst_h != X64_NOREG) {
                emit_lea_r32_r32_disp(e, dst_h, src, l->disp);
            }
            break;
        }

        case LIR_OP_TEST_RR: {
            x64_reg_t h1 = cg_resolve_val(cg, l->src_v[0], RAX);
            x64_reg_t h2 = cg_resolve_val(cg, l->src_v[1], RCX);
            emit_test_r32_r32(e, h1, h2);
            return true;
        }

        case LIR_OP_JMP:
            cg_exit_chained(cg, l->guest_pc + l->imm);
            return true;

        default:
            fprintf(stderr, "stage5-codegen: unhandled LIR op %d at pc=0x%08X\n",
                    l->op, l->guest_pc);
            stage5_codegen_fallback_emit_node++;
            return false;
    }

    // Mark destination dirty
    cg_mark_dst(cg, l, dst_h, dst_slot);
    return true;
}

// Emit deferred side-exit stubs (cold paths placed after hot code).
// Uses per-exit slot snapshots instead of cg_sync_all, because slots
// may have been repurposed between the side exit and end of block.
static void cg_emit_deferred_exits(stage5_cg_t *cg) {
    translate_ctx_t *ctx = cg->ctx;
    emit_ctx_t *e = cg->e;

    for (int di = 0; di < ctx->deferred_exit_count; di++) {
        size_t stub_start = emit_offset(e);
        emit_patch_rel32(e, ctx->deferred_exits[di].jmp_patch_offset, stub_start);

        uint32_t target_pc = ctx->deferred_exits[di].target_pc;

        // Flush dirty slots using the snapshot captured at the side-exit point
        for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
            if (ctx->deferred_exits[di].dirty_snapshot[s] &&
                ctx->deferred_exits[di].guest_reg_snapshot[s] > 0) {
                emit_mov_m32_r32(e, RBP,
                    GUEST_REG_OFFSET(ctx->deferred_exits[di].guest_reg_snapshot[s]),
                    ra_hosts[s]);
            }
        }

        // Emit chained exit (PC store + JMP, no sync)
        emit_mov_m32_imm32(e, RBP, CPU_PC_OFFSET, target_pc);

        uint8_t *patch_site = emit_ptr(e) + 1;
        translated_block_t *target = ctx->cache ? cache_lookup(ctx->cache, target_pc) : NULL;

        if (target && target->host_code) {
            int64_t rel = (int64_t)(target->host_code - (patch_site + 4));
            if (rel >= INT32_MIN && rel <= INT32_MAX) {
                emit_jmp_rel32(e, (int32_t)rel);
            } else if (ctx->cache && ctx->cache->shared_branch_exit) {
                int64_t srel = (int64_t)(ctx->cache->shared_branch_exit - (patch_site + 4));
                emit_jmp_rel32(e, (int32_t)srel);
            } else {
                emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_BRANCH);
                emit_ret(e);
                continue;
            }
        } else if (ctx->cache && ctx->cache->shared_branch_exit) {
            int64_t rel = (int64_t)(ctx->cache->shared_branch_exit - (patch_site + 4));
            emit_jmp_rel32(e, (int32_t)rel);
        } else {
            emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_BRANCH);
            emit_ret(e);
            continue;
        }

        // Record exit for future chaining
        int exit_idx = ctx->deferred_exits[di].exit_idx;
        if (exit_idx < MAX_BLOCK_EXITS && ctx->block) {
            cache_record_exit(ctx->cache, ctx->block, exit_idx, target_pc, patch_site);
            if (!target || !target->host_code)
                cache_record_pending_chain(ctx->cache, ctx->block, exit_idx, target_pc);
        }
    }
}

// ============================================================================
// Pipeline Driver
// ============================================================================

extern void stage5_lir_optimize(stage5_lir_t *lir, const stage5_ssa_overlay_t *ssa);

bool stage5_codegen(translate_ctx_t *ctx, const stage5_lift_region_t *region, uint32_t guest_pc,
                    int terminal_idx, int fuse_cmp_idx, int emitted_pattern,
                    bool synth_block_end, bool side_exit_enabled) {
    (void)terminal_idx; (void)fuse_cmp_idx; (void)emitted_pattern;
    (void)synth_block_end; (void)side_exit_enabled;

    stage5_codegen_attempted++;
    stage5_cg_t cg = {0};
    cg.ctx = ctx; cg.e = &ctx->emit; cg.region = region;
    memset(cg.ssa_value_slot, -1, sizeof(cg.ssa_value_slot));

    // Reset deferred exit count for this block
    ctx->deferred_exit_count = 0;

    // 1. SSA overlay
    stage5_ssa_overlay_t ssa;
    if (!stage5_ssa_build_overlay(region, &ssa)) {
        stage5_codegen_fallback++;
        return false;
    }
    cg.ssa = &ssa;

    // 2. MIR
    stage5_mir_t mir;
    stage5_mir_build(region, &ssa, &mir);

    // 3. BURG lowering (MIR → LIR)
    stage5_lir_t lir;
    if (!stage5_burg_lower(&mir, &ssa, &lir)) {
        stage5_codegen_fallback++;
        return false;
    }
    cg.lir = &lir;

    // 4. LIR optimization
    stage5_lir_optimize(&lir, &ssa);

    // 5. Register allocation
    stage5_ra_plan_t ra_plan;
    if (!stage5_ra_build_plan_lir(&lir, &ssa, &ra_plan)) {
        stage5_codegen_fallback++;
        return false;
    }
    cg.ra_plan = &ra_plan;

    // Map SSA values to host slots
    for (uint16_t i = 0; i < ra_plan.interval_count; i++) {
        if (!ra_plan.intervals[i].spilled) {
            cg.ssa_value_slot[ra_plan.intervals[i].value_id] = ra_plan.intervals[i].assigned_slot;
        }
    }

    // 6. Load live-ins from guest memory
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

    // 7. Emit LIR nodes
    static bool s5_trace = false;
    static int s5_trace_init = 0;
    if (!s5_trace_init) { s5_trace = getenv("S5_TRACE") != NULL; s5_trace_init = 1; }
    if (s5_trace) {
        fprintf(stderr, "[S5] block pc=0x%08X insts=%u lir=%u ra_intervals=%u spilled=%u\n",
                guest_pc, region->guest_inst_count, lir.node_count,
                ra_plan.interval_count, ra_plan.spilled_count);
        for (uint32_t i = 0; i < lir.node_count; i++) {
            lir_node_t *l = &lir.nodes[i];
            fprintf(stderr, "  [%u] op=%d gpc=0x%X dst_v=%u src0=%u src1=%u imm=%d disp=%d sz=%u rd=%u rs1=%u rs2=%u side=%d\n",
                    i, l->op, l->guest_pc, l->dst_v, l->src_v[0], l->src_v[1],
                    l->imm, l->disp, l->size, l->rd, l->rs1, l->rs2, l->is_side_exit);
        }
        for (uint16_t i = 0; i < ra_plan.interval_count; i++) {
            fprintf(stderr, "  ra[%u] v=%u slot=%d start=%u end=%u spilled=%d gpr=%u\n",
                    i, ra_plan.intervals[i].value_id, ra_plan.intervals[i].assigned_slot,
                    ra_plan.intervals[i].start_idx, ra_plan.intervals[i].end_idx,
                    ra_plan.intervals[i].spilled, ssa.value_to_reg[ra_plan.intervals[i].value_id]);
        }
        for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
            if (cg.ssa_slot_value[s]) {
                fprintf(stderr, "  slot[%d] val=%u gpr=%u dirty=%d\n",
                        s, cg.ssa_slot_value[s], cg.ssa_slot_guest_reg[s], cg.ssa_slot_dirty[s]);
            }
        }
    }
    bool ended = false;
    for (uint32_t i = 0; i < lir.node_count; i++) {
        if (!cg_emit_lir_node(&cg, &lir.nodes[i], i)) {
            stage5_codegen_fallback++;
            return false;
        }
        lir_op_t op = lir.nodes[i].op;
        if (!lir.nodes[i].is_side_exit &&
            (op == LIR_OP_JCC || op == LIR_OP_SYSCALL ||
             op == LIR_OP_RET || op == LIR_OP_CALL || op == LIR_OP_JMP)) {
            ended = true;
            break;
        }
    }

    // 8. Synthesize block-end exit if no explicit terminal
    if (!ended) {
        uint32_t end_pc = guest_pc + region->guest_inst_count * 4;
        cg_exit_chained(&cg, end_pc);
    }

    // 9. Emit deferred side-exit stubs (cold code after hot path)
    cg_emit_deferred_exits(&cg);

    // Mark block as Stage 5 and set guest size directly.
    // Stage 5 owns its own block finalization — translate_block_cached
    // does NOT run any Stage 4 post-processing on Stage 5 blocks.
    if (ctx->block) {
        ctx->block->flags |= BLOCK_FLAG_STAGE5 | BLOCK_FLAG_DIRECT;
        ctx->block->guest_size = region->guest_inst_count * 4;
    }

    stage5_codegen_success++;
    return true;
}
