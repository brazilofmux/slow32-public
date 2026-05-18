// SLOW-32 DBT Stage 5: Register Allocator Headers

#ifndef DBT_STAGE5_RA_H
#define DBT_STAGE5_RA_H

#include <stdbool.h>
#include <stdint.h>
#include "target/a64/lir/stage5_lir_a64.h"
#include "pre/ssa/stage5_ssa.h"

#define STAGE5_RA_MAX_INTERVALS 1024

// A64 grows the slot pool beyond the Stage-4 reg-cache ceiling (8).
// Slots 0–7 are caller-saved (W8–W15), free for the JIT body.
// Slots 8–13 are callee-saved (W19, W22–W26); the codegen saves/restores
// them in the JIT prologue/epilogue when any of them are used. The cost
// is a stp triple at entry and an ldp triple before each exit — paid only
// when register pressure actually demands the extra slots.
#define STAGE5_RA_HOST_SLOTS 14

typedef struct {
    uint16_t value_id;
    uint16_t start_idx;
    uint16_t end_idx;
    int8_t assigned_slot;
    bool spilled;
} stage5_ra_interval_t;

typedef struct {
    uint16_t interval_count;
    uint16_t spilled_count;
    stage5_ra_interval_t intervals[STAGE5_RA_MAX_INTERVALS];
} stage5_ra_plan_t;

// region may be NULL. If provided, intervals corresponding to guest registers
// with live_across_edge set will be biased against spilling (E3).
bool stage5_ra_build_plan_lir(const stage5_lir_t *lir, const stage5_ssa_overlay_t *ssa,
                              stage5_ra_plan_t *plan, const stage5_lift_region_t *region);
bool stage5_ra_enabled(void);

#endif // DBT_STAGE5_RA_H
