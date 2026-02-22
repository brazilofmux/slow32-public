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

typedef struct {
    uint8_t guest_reg;
    uint16_t src_value;
    uint16_t dst_value;
} stage5_phi_copy_t;

#define STAGE5_MAX_PHI_EDGE_PLANS (STAGE5_MAX_CFG_BLOCKS * 2)
#define STAGE5_MAX_PHI_COPIES_PER_EDGE 32

typedef struct {
    uint16_t pred_block;
    uint16_t succ_block;
    uint8_t copy_count;
    stage5_phi_copy_t copies[STAGE5_MAX_PHI_COPIES_PER_EDGE];
} stage5_phi_edge_plan_t;

typedef struct {
    uint16_t edge_plan_count;
    uint16_t total_copies;
    stage5_phi_edge_plan_t edges[STAGE5_MAX_PHI_EDGE_PLANS];
} stage5_phi_elim_plan_t;

extern uint32_t stage5_ssa_regions_attempted;
extern uint32_t stage5_ssa_regions_built;
extern uint32_t stage5_ssa_regions_needs_phi;
extern uint64_t stage5_ssa_values_total;
extern uint32_t stage5_ssa_phi_plans_built;
extern uint64_t stage5_ssa_phi_edge_plans_total;
extern uint64_t stage5_ssa_phi_copies_total;

bool stage5_ssa_enabled(void);
bool stage5_ssa_build_overlay(const stage5_lift_region_t *region,
                              stage5_ssa_overlay_t *overlay);
bool stage5_ssa_build_phi_elim_plan(const stage5_lift_region_t *region,
                                    stage5_ssa_overlay_t *overlay,
                                    stage5_phi_elim_plan_t *plan);

#endif  // DBT_STAGE5_SSA_H
