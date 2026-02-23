// SLOW-32 DBT: Stage 5 SSA-value register allocation (preliminary)

#include "stage5_ra.h"

#include <string.h>
#include <stdlib.h>

uint32_t stage5_ra_regions_attempted;
uint32_t stage5_ra_regions_planned;
uint64_t stage5_ra_intervals_total;
uint64_t stage5_ra_spills_total;

bool stage5_ra_enabled(void) {
    return true;
}

static int cmp_interval_start(const void *a, const void *b) {
    const stage5_ra_interval_t *ia = (const stage5_ra_interval_t *)a;
    const stage5_ra_interval_t *ib = (const stage5_ra_interval_t *)b;
    if (ia->start_idx < ib->start_idx) return -1;
    if (ia->start_idx > ib->start_idx) return 1;
    if (ia->end_idx < ib->end_idx) return -1;
    if (ia->end_idx > ib->end_idx) return 1;
    return 0;
}

bool stage5_ra_build_plan(const stage5_lift_region_t *region,
                          const stage5_ssa_overlay_t *ssa,
                          stage5_ra_plan_t *plan) {
    stage5_ra_regions_attempted++;
    if (!region || !ssa || !plan) return false;
    memset(plan, 0, sizeof(*plan));

    if (ssa->value_count == 0 || region->ir_count == 0) {
        stage5_ra_regions_planned++;
        return true;
    }

    uint16_t first_use[STAGE5_SSA_MAX_VALUES];
    uint16_t last_use[STAGE5_SSA_MAX_VALUES];
    bool is_def[STAGE5_SSA_MAX_VALUES];
    memset(first_use, 0xFF, sizeof(first_use));
    memset(last_use, 0xFF, sizeof(last_use));
    memset(is_def, 0, sizeof(is_def));

    // First pass: identify which values are defined in the region.
    for (uint32_t i = 0; i < region->ir_count; i++) {
        uint16_t def = ssa->node_def_value[i];
        if (def > 0 && def < STAGE5_SSA_MAX_VALUES) {
            is_def[def] = true;
        }
    }

    // Second pass: compute live ranges.
    // Live-in values (used but never defined in the region) get start_idx=0
    // because they are loaded into host registers at region entry.
    for (uint32_t i = 0; i < region->ir_count; i++) {
        uint16_t def = ssa->node_def_value[i];
        uint16_t u0 = ssa->node_use0_valid[i] ? ssa->node_use0_value[i] : 0;
        uint16_t u1 = ssa->node_use1_valid[i] ? ssa->node_use1_value[i] : 0;

        if (def > 0 && def < STAGE5_SSA_MAX_VALUES) {
            if (first_use[def] == 0xFFFFu) first_use[def] = (uint16_t)i;
            if (last_use[def] == 0xFFFFu) last_use[def] = (uint16_t)i;
        }
        if (u0 > 0 && u0 < STAGE5_SSA_MAX_VALUES) {
            if (first_use[u0] == 0xFFFFu) {
                // Live-in: start at 0 so the entry load doesn't get clobbered.
                first_use[u0] = is_def[u0] ? (uint16_t)i : 0;
            }
            last_use[u0] = (uint16_t)i;
        }
        if (u1 > 0 && u1 < STAGE5_SSA_MAX_VALUES) {
            if (first_use[u1] == 0xFFFFu) {
                first_use[u1] = is_def[u1] ? (uint16_t)i : 0;
            }
            last_use[u1] = (uint16_t)i;
        }
    }

    for (uint16_t v = 1; v <= ssa->value_count && v < STAGE5_SSA_MAX_VALUES; v++) {
        if (first_use[v] == 0xFFFFu || last_use[v] == 0xFFFFu) continue;
        if (plan->interval_count >= STAGE5_RA_MAX_INTERVALS) return false;
        stage5_ra_interval_t *it = &plan->intervals[plan->interval_count++];
        it->value_id = v;
        it->start_idx = first_use[v];
        it->end_idx = last_use[v];
        it->assigned_slot = -1;
        it->spilled = false;
    }

    qsort(plan->intervals, plan->interval_count, sizeof(plan->intervals[0]), cmp_interval_start);

    uint16_t active[STAGE5_RA_MAX_INTERVALS];
    uint16_t active_count = 0;
    bool slot_used[STAGE5_RA_HOST_SLOTS];
    memset(slot_used, 0, sizeof(slot_used));

    for (uint16_t i = 0; i < plan->interval_count; i++) {
        stage5_ra_interval_t *cur = &plan->intervals[i];

        uint16_t w = 0;
        memset(slot_used, 0, sizeof(slot_used));
        for (uint16_t k = 0; k < active_count; k++) {
            stage5_ra_interval_t *a = &plan->intervals[active[k]];
            if (a->end_idx < cur->start_idx) {
                continue;
            }
            active[w++] = active[k];
            if (!a->spilled && a->assigned_slot >= 0 && a->assigned_slot < STAGE5_RA_HOST_SLOTS) {
                slot_used[a->assigned_slot] = true;
            }
        }
        active_count = w;

        int slot = -1;
        for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
            if (!slot_used[s]) {
                slot = s;
                break;
            }
        }

        if (slot >= 0) {
            cur->assigned_slot = (int8_t)slot;
            cur->spilled = false;
        } else {
            cur->assigned_slot = -1;
            cur->spilled = true;
            plan->spilled_count++;
        }

        if (active_count >= STAGE5_RA_MAX_INTERVALS) return false;
        active[active_count++] = i;
    }

    stage5_ra_regions_planned++;
    stage5_ra_intervals_total += plan->interval_count;
    stage5_ra_spills_total += plan->spilled_count;
    return true;
}
