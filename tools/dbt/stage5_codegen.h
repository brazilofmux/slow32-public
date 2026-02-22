// SLOW-32 DBT: Stage 5 Native Codegen
// Region-level register allocation + direct x86-64 emission
//
// Emits native x86-64 for all prefix and most terminal instructions.
// Delegates only FP (translate_fp_r_type).

#ifndef STAGE5_CODEGEN_H
#define STAGE5_CODEGEN_H

#include "stage5_lift.h"
#include "translate.h"

// Try native codegen for a lifted region.
// On success: emits prefix + terminal into ctx->emit, returns true.
// On failure: returns false (caller falls through to Family C dispatch).
//
// Preconditions:
//   - ctx->reg_alloc[] already populated by reg_alloc_prescan() + prologue emitted
//   - region is a valid lifted region with IR nodes
//   - BURG pattern and terminal_idx already determined by caller
//
// The codegen uses ctx->reg_alloc as-is (no re-planning). It emits x86-64
// directly for ALU/shift/memory/compare prefix instructions, then delegates
// terminal emission to existing stage5_emit_cmp_branch_fused() or
// translate_jal()/translate_jalr()/etc.
bool stage5_codegen(translate_ctx_t *ctx,
                    const stage5_lift_region_t *region,
                    uint32_t guest_pc,
                    int terminal_idx,
                    int fuse_cmp_idx,
                    int emitted_pattern,
                    bool synth_block_end,
                    bool side_exit_emit_enabled,
                    const void *side_exit_family_cfg_ptr);

// Telemetry counters (defined in stage5_codegen.c)
extern uint32_t stage5_codegen_attempted;
extern uint32_t stage5_codegen_success;
extern uint32_t stage5_codegen_fallback;
extern uint32_t stage5_codegen_fallback_unsupported_op;
extern uint32_t stage5_codegen_fallback_preflight;
extern uint32_t stage5_codegen_fallback_preflight_fused_branch;
extern uint32_t stage5_codegen_fallback_preflight_missing_terminal;
extern uint32_t stage5_codegen_fallback_preflight_bad_terminal_index;
extern uint32_t stage5_codegen_fallback_preflight_bad_fuse_index;
extern uint32_t stage5_codegen_fallback_preflight_opcode;
extern uint32_t stage5_codegen_fallback_preflight_opcode_hist[128];
extern uint32_t stage5_codegen_fallback_preflight_side_exit;
extern uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem;
extern uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_load;
extern uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store;
extern uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_stb;
extern uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_stb_beq;
extern uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_sth;
extern uint32_t stage5_codegen_fallback_preflight_side_exit_cmpdep_mem_store_stw;
extern uint32_t stage5_codegen_fallback_preflight_terminal;
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix;
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_opcode_hist[128];
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_noncanonical_term;
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_noncanonical_term_opcode_hist[128];
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_noncanonical_cmp_opcode_hist[128];
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_nonadjacent;
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_rd_mismatch;
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_rd_mismatch_opcode_hist[128];
extern uint32_t stage5_codegen_branch_cmp_mix_passthrough_rd_mismatch_xor;
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_predicate_policy;
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_predicate_policy_pattern_hist[64];
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_opcode;
extern uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix_reason_multi_cmp;
extern uint32_t stage5_codegen_fallback_side_exit;
extern uint32_t stage5_codegen_fallback_emit_node;
extern uint32_t stage5_codegen_fallback_terminal;
extern uint32_t stage5_codegen_fallback_terminal_opcode_hist[128];
extern uint64_t stage5_codegen_guest_insts;
extern uint64_t stage5_codegen_host_bytes;
extern uint32_t stage5_codegen_regs_allocated_hist[REG_ALLOC_SLOTS + 1];
extern uint32_t stage5_codegen_boolpair_native_attempted;
extern uint32_t stage5_codegen_boolpair_native_success;
extern uint32_t stage5_codegen_boolpair_native_fallback;
extern uint32_t stage5_codegen_predicate_native_terminal_success;
extern uint32_t stage5_codegen_predicate_native_terminal_fallback;
extern uint32_t stage5_codegen_predicate_native_terminal_opcode_hist[128];
extern uint64_t stage5_codegen_predicate_native_guest_insts;
extern uint64_t stage5_codegen_predicate_native_host_bytes;
extern uint32_t stage5_codegen_fused_addi_mem;
extern uint32_t stage5_codegen_evictions;
extern uint64_t stage5_codegen_native_loads;
extern uint64_t stage5_codegen_native_stores;

// Bridge functions (defined in translate.c, expose static functions to codegen)
void emit_exit_chained_for_codegen(translate_ctx_t *ctx, uint32_t target_pc, int exit_idx);
void emit_exit_for_codegen(translate_ctx_t *ctx, exit_reason_t reason, uint32_t next_pc);
void emit_fault_exit_for_codegen(translate_ctx_t *ctx, exit_reason_t reason,
                                  uint32_t pc, x64_reg_t info_reg);
void reg_cache_flush_for_codegen(translate_ctx_t *ctx);
void flush_cached_host_regs_for_codegen(translate_ctx_t *ctx, x64_reg_t r1, x64_reg_t r2);
void emit_indirect_lookup_for_codegen(translate_ctx_t *ctx, x64_reg_t target_reg);
void emit_ras_predict_for_codegen(translate_ctx_t *ctx, x64_reg_t target_reg);

bool stage5_emit_cmp_branch_fused_for_codegen(translate_ctx_t *ctx,
    uint8_t cmp_opcode, uint8_t cmp_rs1, uint8_t cmp_rs2,
    int32_t cmp_imm, bool cmp_is_imm,
    uint8_t branch_opcode, int32_t branch_imm, uint32_t branch_pc);

bool stage5_translate_branch_terminal_for_codegen(translate_ctx_t *ctx,
    uint8_t opcode, uint8_t rs1, uint8_t rs2, int32_t imm);

bool stage5_translate_branch_direct_for_codegen(translate_ctx_t *ctx,
    uint8_t opcode, uint8_t rs1, uint8_t rs2, int32_t imm);

void stage5_flush_pending_for_codegen(translate_ctx_t *ctx);
bool stage5_side_exit_opcode_supported_for_codegen(uint8_t opcode);
bool stage5_emit_side_exit_for_codegen(translate_ctx_t *ctx,
    uint8_t opcode, uint8_t rs1, uint8_t rs2, int32_t imm, uint32_t branch_pc);

void stage5_translate_jal_jump_compact_for_codegen(translate_ctx_t *ctx,
    uint8_t rd, int32_t imm);

// Side-exit counters (defined in translate.c)
extern uint32_t stage5_emit_side_exits;

#endif // STAGE5_CODEGEN_H
