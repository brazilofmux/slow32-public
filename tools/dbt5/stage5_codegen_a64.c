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

bool stage5_codegen_a64(stage5_cg_a64_ctx_t *cg,
                        const stage5_lift_region_t *region,
                        const stage5_lir_t *lir,
                        const stage5_ra_plan_t *ra_plan)
{
    (void)cg;
    (void)region;
    (void)lir;
    (void)ra_plan;

    // This is the very first stub.
    // Real implementation will go here:
    //   1. Initialize the 8 host slots from the RA plan
    //   2. Walk the LIR
    //   3. Emit using only emit_mov_*, emit_add_*, emit_b_*, etc.
    //   4. Record exits for later patching
    //
    // The first working version will deliberately be narrow (no FP,
    // simple memory, final terminal only) so we can get something
    // emitting real A64 instructions without the old helpers.

    stage5_codegen_a64_attempted++;
    return false;   // not implemented yet
}
