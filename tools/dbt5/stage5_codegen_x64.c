// SLOW-32 DBT5 — x86-64 Codegen Implementation (Placeholder)
//
// This file is intentionally a skeleton.
//
// See stage5_codegen_x64.h for the migration plan and rationale.
//
// The actual working (but coupled) x86-64 emitter currently lives in
// the legacy stage5_codegen.c.  The long-term goal is to move all
// independent x86-64 emission logic into this file and its supporting
// modules, using only:
//
//   - emit_x64.* primitives
//   - block_cache
//   - the clean LIR + RA output from the Stage 5 pipeline
//
// Until a real port happens, everything here is a no-op stub.

#include "stage5_codegen_x64.h"

uint32_t stage5_codegen_x64_attempted = 0;
uint32_t stage5_codegen_x64_success   = 0;

bool stage5_codegen_x64(stage5_cg_x64_ctx_t *cg,
                        const stage5_lift_region_t *region,
                        const stage5_lir_t *lir,
                        const stage5_ra_plan_t *ra_plan)
{
    (void)cg;
    (void)region;
    (void)lir;
    (void)ra_plan;

    // Placeholder — real implementation will go here during the x86-64 port.
    // An x86-64 agent should replace this body with a clean emitter that
    // walks the LIR using the RA plan and emits only via emit_x64.h.
    return false;
}
