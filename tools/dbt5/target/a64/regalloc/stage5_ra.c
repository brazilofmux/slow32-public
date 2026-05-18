// SLOW-32 DBT Stage 5: Linear Scan Register Allocator
// Operates on LIR nodes using SSA Value IDs as virtual registers.

#include "stage5_ra.h"
#include <string.h>
#include <stdlib.h>

static int cmp_interval_start(const void *a, const void *b) {
    const stage5_ra_interval_t *ia = (const stage5_ra_interval_t *)a;
    const stage5_ra_interval_t *ib = (const stage5_ra_interval_t *)b;
    if (ia->start_idx != ib->start_idx) return ia->start_idx - ib->start_idx;
    return ia->end_idx - ib->end_idx;
}

bool stage5_ra_build_plan_lir(const stage5_lir_t *lir, const stage5_ssa_overlay_t *ssa,
                              stage5_ra_plan_t *plan, const stage5_lift_region_t *region) {
    if (!lir || !ssa || !plan) return false;
    memset(plan, 0, sizeof(*plan));

    uint16_t first_use[STAGE5_SSA_MAX_VALUES];
    uint16_t last_use[STAGE5_SSA_MAX_VALUES];
    bool is_def[STAGE5_SSA_MAX_VALUES];
    memset(first_use, 0xFF, sizeof(first_use));
    memset(last_use, 0xFF, sizeof(last_use));
    memset(is_def, 0, sizeof(is_def));

    // Compute live ranges
    for (uint32_t i = 0; i < lir->node_count; i++) {
        const lir_node_t *l = &lir->nodes[i];
        if (l->dst_v != 0 && l->dst_v < STAGE5_SSA_MAX_VALUES) {
            is_def[l->dst_v] = true;
            if (first_use[l->dst_v] == 0xFFFFu) first_use[l->dst_v] = (uint16_t)i;
            last_use[l->dst_v] = (uint16_t)i;
        }
        for (int k = 0; k < 2; k++) {
            uint16_t u = l->src_v[k];
            if (u != 0 && u < STAGE5_SSA_MAX_VALUES) {
                if (first_use[u] == 0xFFFFu) first_use[u] = 0; // Live-in
                last_use[u] = (uint16_t)i;
            }
        }
    }

    for (uint16_t v = 1; v <= ssa->value_count; v++) {
        if (first_use[v] == 0xFFFFu) continue;
        stage5_ra_interval_t *it = &plan->intervals[plan->interval_count++];
        it->value_id = v;
        it->start_idx = first_use[v];
        it->end_idx = last_use[v];
        it->assigned_slot = -1;
    }

    qsort(plan->intervals, plan->interval_count, sizeof(plan->intervals[0]), cmp_interval_start);

    uint16_t active[STAGE5_RA_MAX_INTERVALS];
    uint16_t active_count = 0;
    bool slot_used[STAGE5_RA_HOST_SLOTS];

    for (uint16_t i = 0; i < plan->interval_count; i++) {
        stage5_ra_interval_t *cur = &plan->intervals[i];
        
        // Expire old intervals
        uint16_t w = 0;
        memset(slot_used, 0, sizeof(slot_used));
        for (uint16_t k = 0; k < active_count; k++) {
            stage5_ra_interval_t *a = &plan->intervals[active[k]];
            if (a->end_idx < cur->start_idx) continue;
            active[w++] = active[k];
            if (!a->spilled && a->assigned_slot >= 0) slot_used[a->assigned_slot] = true;
        }
        active_count = w;

        // Allocate slot
        int slot = -1;
        for (int s = 0; s < STAGE5_RA_HOST_SLOTS; s++) {
            if (!slot_used[s]) { slot = s; break; }
        }

        if (slot >= 0) {
            cur->assigned_slot = (int8_t)slot;
        } else {
            // Spill the one that ends furthest.
            // E3: bias against spilling intervals that carry live-across-edge
            // guest registers (loop invariants / hot carriers). These are much
            // more painful to reload on every back-edge.
            int victim = -1;
            uint16_t max_end = cur->end_idx;
            int best_cost = -1;

            for (uint16_t k = 0; k < active_count; k++) {
                stage5_ra_interval_t *a = &plan->intervals[active[k]];
                if (a->spilled) continue;

                int cost = a->end_idx;

                if (region) {
                    uint8_t gpr = (a->value_id < STAGE5_SSA_MAX_VALUES)
                                  ? ssa->value_to_reg[a->value_id] : 0;
                    if (gpr != 0 && gpr < 32 &&
                        region->reg_flow[gpr].live_across_edge) {
                        cost -= 10000; // strongly prefer not to spill loop carriers
                    }
                }

                if (cost > best_cost) {
                    best_cost = cost;
                    victim = k;
                }
            }

            if (victim >= 0) {
                stage5_ra_interval_t *v_it = &plan->intervals[active[victim]];
                cur->assigned_slot = v_it->assigned_slot;
                v_it->assigned_slot = -1;
                v_it->spilled = true;
                plan->spilled_count++;
            } else {
                cur->spilled = true;
                plan->spilled_count++;
            }
        }
        active[active_count++] = i;
    }

    return true;
}

bool stage5_ra_enabled(void) { return true; }
bool stage5_ra_build_plan(const stage5_lift_region_t *region, const stage5_ssa_overlay_t *ssa, stage5_ra_plan_t *plan) { (void)region; (void)ssa; (void)plan; return false; }
