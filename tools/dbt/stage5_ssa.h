// SLOW-32 DBT: Stage 5 SSA overlay (region-local)

#ifndef DBT_STAGE5_SSA_H
#define DBT_STAGE5_SSA_H

#include <stdbool.h>
#include <stdint.h>

#include "stage5_lift.h"

#define STAGE5_SSA_MAX_VALUES (STAGE5_MAX_IR_NODES * 3 + 64)

typedef struct {
    uint16_t value_count;
    uint8_t join_block_count;
    bool needs_phi;
    uint16_t node_def_value[STAGE5_MAX_IR_NODES];
    uint16_t node_use0_value[STAGE5_MAX_IR_NODES];
    uint16_t node_use1_value[STAGE5_MAX_IR_NODES];
    uint8_t node_def_reg[STAGE5_MAX_IR_NODES];
    uint8_t node_use0_reg[STAGE5_MAX_IR_NODES];
    uint8_t node_use1_reg[STAGE5_MAX_IR_NODES];
    bool node_use0_valid[STAGE5_MAX_IR_NODES];
    bool node_use1_valid[STAGE5_MAX_IR_NODES];
} stage5_ssa_overlay_t;

extern uint32_t stage5_ssa_regions_attempted;
extern uint32_t stage5_ssa_regions_built;
extern uint32_t stage5_ssa_regions_needs_phi;
extern uint64_t stage5_ssa_values_total;

bool stage5_ssa_enabled(void);
bool stage5_ssa_build_overlay(const stage5_lift_region_t *region,
                              stage5_ssa_overlay_t *overlay);

#endif  // DBT_STAGE5_SSA_H
