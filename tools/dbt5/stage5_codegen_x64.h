// SLOW-32 DBT5 — Clean x86-64 Native Codegen (Target for Migration)
//
// This is the intended home for the *independent* x86-64 Stage 5 backend.
//
// GOAL
// ----
// Eventually this file (and its .c) will contain a full native x86-64
// emitter that works only with:
//
//   - The lifted region + LIR produced by the clean pipeline
//   - The RA plan from stage5_ra
//   - A thin dbt5-native codegen context (no translate_ctx_t, no reg cache,
//     no Stage 1-4 fusion/peephole state)
//
// CURRENT STATE
// -------------
// This is currently a placeholder / migration target.
//
// The real implementation that exists today lives in the legacy file
// stage5_codegen.c, which is still coupled to the old translate_ctx_t and
// the production Stage 1-4 runtime.  That code will be gradually ported
// (or rewritten) into this new clean interface.
//
// When the port is complete, we expect to be able to delete or heavily
// deprecate the old stage5_codegen.c from the production ./tools/dbt tree.
//
// DO NOT extend the legacy stage5_codegen.c with new features.
// New x86-64 work should go here (or in supporting files under a
// stage5_cg_x64_* naming convention).

#ifndef DBT5_STAGE5_CODEGEN_X64_H
#define DBT5_STAGE5_CODEGEN_X64_H

#include <stdbool.h>
#include <stdint.h>

#include "stage5_lift.h"
#include "stage5_lir.h"
#include "stage5_ra.h"

// ============================================================================
// Thin codegen context (to be designed properly during the real port)
//
// For now this is just a sketch so the interface can be discussed.
// A real x86-64 agent should replace this with whatever minimal state
// the emitter actually needs (emit buffer, exit records, host slot
// mapping from the RA plan, etc.).
// ============================================================================

typedef struct {
    // Placeholder — real fields will be added during the port
    uint8_t *code_buffer;
    size_t   code_capacity;
    size_t   code_size;

    // Future: reference to the RA-assigned host slots, exit table, etc.
} stage5_cg_x64_ctx_t;

// ============================================================================
// Entry point (placeholder)
//
// This is the function that will eventually own a whole region natively
// on x86-64 using only the clean Stage 5 pipeline output.
// ============================================================================

bool stage5_codegen_x64(stage5_cg_x64_ctx_t *cg,
                        const stage5_lift_region_t *region,
                        const stage5_lir_t *lir,
                        const stage5_ra_plan_t *ra_plan);

// Telemetry (stubs for now)
extern uint32_t stage5_codegen_x64_attempted;
extern uint32_t stage5_codegen_x64_success;

#endif // DBT5_STAGE5_CODEGEN_X64_H
