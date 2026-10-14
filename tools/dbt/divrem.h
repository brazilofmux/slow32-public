// SLOW-32 DBT: DIV/REM pairing, shared by the x86-64 and AArch64 translators.
//
// MOD(X,Y) and X/Y in one source line become REM and DIV of the same two
// registers.  x86's IDIV yields quotient and remainder at once, and
// AArch64's remainder is SDIV then MSUB, so the second divide is waste --
// and worse than waste: an integer divider takes one divide at a time,
// and when the remainder's divide goes first the quotient (often the
// loop-carried value) waits behind it.  A pi spigot posted to H390-MVS
// ran 1.22x gfortran's time on a Cascade Lake for want of this.
//
// The first of the pair computes both halves and leaves its partner's in
// a host register no translation otherwise uses.  The partner becomes a
// move.  Between the two only instructions whose translation is known
// to leave that register alone may appear (integer ALU, compares,
// multiplies, LUI, loads, stores: none calls out), and neither operand
// may be written.  Checked twice: a lookahead decides whether to pair
// at all (divrem_partner_ahead), and the translation loop drops the
// pairing at anything the lookahead should have stopped at (divrem_note)
// -- a side exit leaves the block, a back-edge target is entered from
// elsewhere -- so the partner's move is only emitted where the held
// value is certainly live.  S32_DBT_NO_DIVREM=1 turns pairing off.

#ifndef SLOW32_DBT_DIVREM_H
#define SLOW32_DBT_DIVREM_H

#include <stdlib.h>

#define DIVREM_WINDOW 16

uint32_t divrem_fusion_count;   // partners turned into a move (-s stats)

static int divrem_enabled = -1;
static inline bool divrem_on(void) {
    if (divrem_enabled < 0) {
        const char *v = getenv("S32_DBT_NO_DIVREM");
        divrem_enabled = (v && v[0] && v[0] != '0') ? 0 : 1;
    }
    return divrem_enabled != 0;
}

// May this instruction sit between a pair?  (Its translation must not
// touch the holding register: nothing that calls a helper or leaves.)
static inline bool divrem_transparent(uint8_t op) {
    if (op <= OP_MULHU) return true;          // ALU, compares, MUL*, DIV, REM
    if (op == OP_LUI) return true;
    if (op >= OP_LDB && op <= OP_LDHU) return true;
    if (op >= OP_STB && op <= OP_STW) return true;
    return false;
}

static inline bool divrem_is_store(uint8_t op) {
    return op >= OP_STB && op <= OP_STW;
}

// Does `inst` write guest register r?
static inline bool divrem_writes(const decoded_inst_t *inst, uint8_t r) {
    return !divrem_is_store(inst->opcode) && inst->rd != 0 && inst->rd == r;
}

// Is the partner (opcode `want`, same rs1/rs2) ahead of the DIV/REM at
// `pc` within the window, with nothing in between that would lose it?
static inline bool divrem_partner_ahead(dbt_cpu_state_t *cpu, uint32_t pc,
                                        uint8_t want, uint8_t rd,
                                        uint8_t rs1, uint8_t rs2) {
    if (!divrem_on()) return false;
    if (rd == rs1 || rd == rs2) return false;   // the first changes an operand
    for (int k = 1; k <= DIVREM_WINDOW; k++) {
        uint32_t p = pc + 4u * (uint32_t)k;
        if (p >= cpu->code_limit || cpu->code_limit - p < 4) return false;
        decoded_inst_t in = decode_instruction(*(uint32_t *)(cpu->mem_base + p));
        if (in.opcode == want && in.rs1 == rs1 && in.rs2 == rs2) return in.rd != 0;
        if (!divrem_transparent(in.opcode)) return false;
        if (divrem_writes(&in, rs1) || divrem_writes(&in, rs2)) return false;
    }
    return false;
}

// Before translating `inst`: drop a pending pairing it could break.
static inline void divrem_note(translate_ctx_t *ctx, const decoded_inst_t *inst,
                               bool backedge_target) {
    if (!ctx->divrem.valid) return;
    if (backedge_target || !divrem_transparent(inst->opcode)) {
        ctx->divrem.valid = false;
        return;
    }
    if (inst->opcode == ctx->divrem.consumer &&
        inst->rs1 == ctx->divrem.rs1 && inst->rs2 == ctx->divrem.rs2)
        return;                                  // the partner: it consumes
    if (divrem_writes(inst, ctx->divrem.rs1) || divrem_writes(inst, ctx->divrem.rs2))
        ctx->divrem.valid = false;
}

// At a DIV/REM: is the other half already held for it?
static inline bool divrem_take(translate_ctx_t *ctx, uint8_t op,
                               uint8_t rs1, uint8_t rs2) {
    if (!ctx->divrem.valid || ctx->divrem.consumer != op ||
        ctx->divrem.rs1 != rs1 || ctx->divrem.rs2 != rs2) return false;
    ctx->divrem.valid = false;
    divrem_fusion_count++;
    return true;
}

static inline void divrem_hold(translate_ctx_t *ctx, uint8_t consumer,
                               uint8_t rs1, uint8_t rs2) {
    ctx->divrem.valid = true;
    ctx->divrem.consumer = consumer;
    ctx->divrem.rs1 = rs1;
    ctx->divrem.rs2 = rs2;
}

#endif
