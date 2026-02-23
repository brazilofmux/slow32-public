// SLOW-32 DBT Stage 5: Register Allocator Headers

#ifndef DBT_STAGE5_RA_H
#define DBT_STAGE5_RA_H

#include <stdbool.h>
#include <stdint.h>
#include "stage5_lir.h"
#include "stage5_ssa.h"

#define STAGE5_RA_MAX_INTERVALS 1024
#define STAGE5_RA_HOST_SLOTS 8

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

bool stage5_ra_build_plan_lir(const stage5_lir_t *lir, const stage5_ssa_overlay_t *ssa, stage5_ra_plan_t *plan);
bool stage5_ra_enabled(void);

#endif // DBT_STAGE5_RA_H
