// SLOW-32 DBT: Stage 5 Native Codegen
// Self-contained compiler pipeline: LIFT -> SSA -> MIR -> BURG -> LIR -> RA -> EMIT
// ZERO Stage 4 dependencies. All emission uses emit_x64.h + block_cache.h directly.

#ifndef STAGE5_CODEGEN_H
#define STAGE5_CODEGEN_H

#include "stage5_lift.h"
// NOTE (dbt5 clean room): We deliberately do NOT include translate.h here.
// The x86 codegen (stage5_codegen.c) still references translate_ctx_t today
// because it was written against the old coupled runtime. On AArch64 hosts
// we do not compile that .c file; a future stage5_codegen_a64.c will use a
// dbt5-native thin context instead (no reg cache, no Stage 4 state).
// For now the public API below is only available on x86-64 hosts.

// Try native codegen for a lifted region.
// On success: emits complete x86-64 block into ctx->emit, returns true.
// On failure: returns false (caller should abort — no Stage 4 fallback).
//
// Preconditions:
//   - region is a valid lifted region with IR nodes
//   - ctx->cache and ctx->block set up for block chaining
//
// The codegen runs the full pipeline internally:
//   SSA overlay -> MIR -> BURG -> LIR -> RA -> native x86-64
bool stage5_codegen(translate_ctx_t *ctx,
                    const stage5_lift_region_t *region,
                    uint32_t guest_pc,
                    int terminal_idx,
                    int fuse_cmp_idx,
                    int emitted_pattern,
                    bool synth_block_end,
                    bool side_exit_enabled);

// Telemetry counters (defined in stage5_codegen.c)
extern uint32_t stage5_codegen_attempted;
extern uint32_t stage5_codegen_success;
extern uint32_t stage5_codegen_fallback;
extern uint32_t stage5_codegen_fallback_emit_node;

#endif // STAGE5_CODEGEN_H
