// SLOW-32 DBT Stage 5: SSA Management
// Provides Value ID mappings and SSA-specific metadata for regions.

#ifndef DBT_STAGE5_SSA_H
#define DBT_STAGE5_SSA_H

#include <stdbool.h>
#include <stdint.h>
#include "stage5_lift.h"

#define STAGE5_SSA_MAX_VALUES (STAGE5_MAX_IR_NODES * 3 + 64)

typedef struct {
    uint16_t value_count;
    bool needs_phi;
    
    // Mapping from Linear IR Node -> SSA Value IDs
    uint16_t node_def_value[STAGE5_MAX_IR_NODES];
    uint16_t node_use0_value[STAGE5_MAX_IR_NODES];
    uint16_t node_use1_value[STAGE5_MAX_IR_NODES];
    
    // Metadata for each Value ID
    uint8_t value_to_reg[STAGE5_SSA_MAX_VALUES];   // Original Guest GPR
    uint8_t value_use_count[STAGE5_SSA_MAX_VALUES];
    bool value_is_const[STAGE5_SSA_MAX_VALUES];
    uint32_t value_const_val[STAGE5_SSA_MAX_VALUES];
    uint16_t value_def_node[STAGE5_MAX_IR_NODES]; // Index into region->ir
} stage5_ssa_overlay_t;

// PHI elimination plan (kept simple for regions)
typedef struct {
    uint8_t guest_reg;
    uint16_t src_value;
    uint16_t dst_value;
} stage5_phi_copy_t;

#define STAGE5_MAX_PHI_COPIES 64
typedef struct {
    uint16_t copy_count;
    stage5_phi_copy_t copies[STAGE5_MAX_PHI_COPIES];
} stage5_phi_elim_plan_t;

bool stage5_ssa_build_overlay(const stage5_lift_region_t *region, stage5_ssa_overlay_t *ssa);
bool stage5_ssa_build_phi_elim_plan(const stage5_lift_region_t *region, stage5_ssa_overlay_t *ssa, stage5_phi_elim_plan_t *plan);

#endif // DBT_STAGE5_SSA_H
