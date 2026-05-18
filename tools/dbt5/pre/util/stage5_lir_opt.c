// SLOW-32 DBT Stage 5: LIR Optimizer
// Peephole and simple machine-independent optimizations on LIR.

#include "target/a64/lir/stage5_lir_a64.h"  // temporary during aggressive split; will be cleaned
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

// Human-readable names for LIR opcodes (diagnostics only)
const char *lir_op_name(lir_op_t op) {
    switch (op) {
        case LIR_OP_NOP:            return "nop";
        case LIR_OP_MOV_RR:         return "mov_rr";
        case LIR_OP_MOV_RI:         return "mov_ri";
        case LIR_OP_MOV_RM:         return "mov_rm";
        case LIR_OP_MOV_MR:         return "mov_mr";
        case LIR_OP_LEA:            return "lea";
        case LIR_OP_ADD_RR:         return "add_rr";
        case LIR_OP_ADD_RI:         return "add_ri";
        case LIR_OP_SUB_RR:         return "sub_rr";
        case LIR_OP_SUB_RI:         return "sub_ri";
        case LIR_OP_AND_RR:         return "and_rr";
        case LIR_OP_AND_RI:         return "and_ri";
        case LIR_OP_OR_RR:          return "or_rr";
        case LIR_OP_OR_RI:          return "or_ri";
        case LIR_OP_XOR_RR:         return "xor_rr";
        case LIR_OP_XOR_RI:         return "xor_ri";
        case LIR_OP_SHL_RI:         return "shl_ri";
        case LIR_OP_SHR_RI:         return "shr_ri";
        case LIR_OP_SAR_RI:         return "sar_ri";
        case LIR_OP_SHL_RR:         return "shl_rr";
        case LIR_OP_SHR_RR:         return "shr_rr";
        case LIR_OP_SAR_RR:         return "sar_rr";
        case LIR_OP_IMUL_RR:        return "imul_rr";
        case LIR_OP_MULH_RR:        return "mulh_rr";
        case LIR_OP_MULHU_RR:       return "mulhu_rr";
        case LIR_OP_IDIV:           return "idiv";
        case LIR_OP_UDIV:           return "udiv";
        case LIR_OP_CMP_RR:         return "cmp_rr";
        case LIR_OP_CMP_RI:         return "cmp_ri";
        case LIR_OP_TEST_RR:        return "test_rr";
        case LIR_OP_SETCC:          return "setcc";
        case LIR_OP_JCC:            return "jcc";
        case LIR_OP_CMP_JCC:        return "cmp_jcc";
        case LIR_OP_CMP_RI_JCC:     return "cmp_ri_jcc";
        case LIR_OP_TEST_JCC:       return "test_jcc";
        case LIR_OP_CBZ:            return "cbz";
        case LIR_OP_CBNZ:           return "cbnz";
        case LIR_OP_JMP:            return "jmp";
        case LIR_OP_CALL:           return "call";
        case LIR_OP_RET:            return "ret";
        case LIR_OP_SYSCALL:        return "syscall";
        case LIR_OP_FP_HELPER:      return "fp_helper";
        default:                    return "???";
    }
}
