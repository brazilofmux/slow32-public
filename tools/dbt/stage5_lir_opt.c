// SLOW-32 DBT Stage 5: LIR Optimizer
// Peephole and simple machine-independent optimizations on LIR.

#include "stage5_lir.h"
#include "stage5_ssa.h"
#include <string.h>

void stage5_lir_optimize(stage5_lir_t *lir, const stage5_ssa_overlay_t *ssa) {
    if (!lir || !ssa) return;

    for (uint32_t i = 0; i < lir->node_count; i++) {
        lir_node_t *l = &lir->nodes[i];
        if (l->op == LIR_OP_NOP) continue;

        // 1. Redundant MOV RR
        if (l->op == LIR_OP_MOV_RR && l->dst_v == l->src_v[0]) {
            l->op = LIR_OP_NOP;
            continue;
        }

        // 2. ADD/SUB/OR/XOR RI 0 → copy (identity ops)
        if (l->imm == 0 && (l->op == LIR_OP_ADD_RI || l->op == LIR_OP_SUB_RI ||
                            l->op == LIR_OP_OR_RI  || l->op == LIR_OP_XOR_RI)) {
            if (l->dst_v == l->src_v[0]) l->op = LIR_OP_NOP;
            else l->op = LIR_OP_MOV_RR;
            continue;
        }

        // 3. AND RI 0 → zero (AND with 0 clears register)
        if (l->op == LIR_OP_AND_RI && l->imm == 0) {
            l->op = LIR_OP_MOV_RI;
            l->imm = 0;
            continue;
        }

        // 4. SHL/SHR/SAR RI 0 → copy (shift by 0 = identity)
        if (l->imm == 0 && (l->op == LIR_OP_SHL_RI || l->op == LIR_OP_SHR_RI ||
                            l->op == LIR_OP_SAR_RI)) {
            if (l->dst_v == l->src_v[0]) l->op = LIR_OP_NOP;
            else l->op = LIR_OP_MOV_RR;
            continue;
        }
    }
}
