// SLOW-32 DBT: Stage 5 SSA-value register allocation (preliminary)

#ifndef DBT_STAGE5_RA_H
#define DBT_STAGE5_RA_H

#include <stdbool.h>
#include <stdint.h>

#include "stage5_lift.h"
#include "stage5_ssa.h"

#define STAGE5_RA_MAX_INTERVALS STAGE5_SSA_MAX_VALUES
#define STAGE5_RA_HOST_SLOTS 8

typedef struct {
    uint16_t value_id;
    uint16_t start_idx;
    uint16_t end_idx;
    int8_t assigned_slot;  // 0..STAGE5_RA_HOST_SLOTS-1, -1 => spilled
    bool spilled;
} stage5_ra_interval_t;

typedef struct {
    uint16_t interval_count;
    uint16_t spilled_count;
    stage5_ra_interval_t intervals[STAGE5_RA_MAX_INTERVALS];
} stage5_ra_plan_t;

extern uint32_t stage5_ra_regions_attempted;
extern uint32_t stage5_ra_regions_planned;
extern uint64_t stage5_ra_intervals_total;
extern uint64_t stage5_ra_spills_total;

bool stage5_ra_enabled(void);
bool stage5_ra_build_plan(const stage5_lift_region_t *region,
                          const stage5_ssa_overlay_t *ssa,
                          stage5_ra_plan_t *plan);

#endif  // DBT_STAGE5_RA_H
