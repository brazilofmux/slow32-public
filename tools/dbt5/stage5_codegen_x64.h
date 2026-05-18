// SLOW-32 DBT5 — Clean x86-64 Native Codegen
//
// This is the dbt5 (clean-room) home for the x86-64 Stage 5 backend.
//
// Compared to the legacy dbt/stage5_codegen.{c,h} (which still pulls
// translate_ctx_t from the production Stage 1-4 runtime), this module
// is independent: it operates on a small, codegen-only context.
//
// Allowed dependencies:
//   - emit_x64.* primitives
//   - block_cache.h (chained-exit recording)
//   - cpu_state.h  (CPU offsets, exit reasons)
//   - dbt_limits.h (MAX_BLOCK_EXITS, etc.)
//   - the clean Stage 5 pipeline (lift -> SSA -> MIR -> BURG -> LIR -> RA)
//
// What is intentionally NOT here:
//   - translate_ctx_t and the Stage 4 reg-cache / fusion / peephole state
//   - register-cache snapshots in deferred exits (Stage 5 reconstructs from RA)
//   - any AArch64-only pending_write / shifted-fold scaffolding
//
// Used only on x86-64 hosts (see tools/dbt5/Makefile).

#ifndef DBT5_STAGE5_CODEGEN_X64_H
#define DBT5_STAGE5_CODEGEN_X64_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "emit_x64.h"
#include "block_cache.h"
#include "cpu_state.h"
#include "dbt_limits.h"
#include "stage5_lift.h"
#include "stage5_lir.h"
#include "stage5_ra.h"

// Minimal deferred-exit record for Stage 5 side-exits.
//
// Stage 4 carries a full register-cache snapshot per deferred exit; Stage 5
// does not need that — the RA plan is immutable and the codegen tracks
// per-exit slot_value / spilled_def_for_gpr snapshots itself in its own
// state (next to the cg pipeline), not in this public struct.
typedef struct {
    size_t   jmp_patch_offset;  // offset of rel32 inside the inline JCC
    uint32_t target_pc;         // guest PC the side exit jumps to
    int      exit_idx;          // exit index for chaining
    uint32_t branch_pc;         // guest PC of the originating branch
    bool     force_full_flush;  // reserved for future use
} stage5_cg_x64_deferred_exit_t;

// Thin codegen context — everything the x86-64 emitter actually needs.
typedef struct {
    dbt_cpu_state_t    *cpu;    // guest CPU state (for offset computation)
    emit_ctx_t          emit;   // host code emit buffer (raw x64)
    block_cache_t      *cache;  // block cache (for chaining)
    translated_block_t *block;  // current block (for exit recording)

    int   exit_idx;             // running exit index for chaining
    int   deferred_exit_count;
    stage5_cg_x64_deferred_exit_t deferred_exits[MAX_BLOCK_EXITS];
} stage5_cg_x64_ctx_t;

// Run the full clean-room pipeline (SSA -> MIR -> BURG -> LIR -> RA -> emit)
// against `region`, writing native x86-64 into ctx->emit.
//
// Returns true on success.  On failure the emit buffer state is undefined
// and the caller must NOT execute it.  There is no Stage 4 fallback —
// dbt5 has no Stage 1-4 code to fall back to.
bool stage5_codegen_x64(stage5_cg_x64_ctx_t *ctx,
                        const stage5_lift_region_t *region,
                        uint32_t guest_pc);

// Telemetry
extern uint32_t stage5_codegen_x64_attempted;
extern uint32_t stage5_codegen_x64_success;
extern uint32_t stage5_codegen_x64_fallback;
extern uint32_t stage5_codegen_x64_fallback_emit_node;

#endif // DBT5_STAGE5_CODEGEN_X64_H
