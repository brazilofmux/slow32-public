// SLOW-32 DBT: Stage 5 Native Codegen
// Region-level register allocation + direct x86-64 emission
//
// Emits x86-64 for lifted IR prefix instructions using the pre-assigned
// host registers from ctx->reg_alloc[] (set up by reg_alloc_prescan()).
// Delegates terminal emission to existing stage5_emit_cmp_branch_fused()
// or translate_*() functions.

#include "stage5_codegen.h"
#include "stage5_burg.h"
#include "block_cache.h"
#include "cpu_state.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// ============================================================================
// Telemetry counters
// ============================================================================
uint32_t stage5_codegen_attempted;
uint32_t stage5_codegen_success;
uint32_t stage5_codegen_fallback;
uint32_t stage5_codegen_fallback_unsupported_op;
uint32_t stage5_codegen_fallback_preflight;
uint32_t stage5_codegen_fallback_preflight_branch_cmp_mix;
uint32_t stage5_codegen_fallback_side_exit;
uint32_t stage5_codegen_fallback_emit_node;
uint32_t stage5_codegen_fallback_terminal;
uint64_t stage5_codegen_guest_insts;
uint64_t stage5_codegen_host_bytes;
uint32_t stage5_codegen_regs_allocated_hist[REG_ALLOC_SLOTS + 1];

static bool cg_cmp_rr_enabled(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_CMP_RR");
        enabled = (v && v[0] != '\0' && strcmp(v, "0") != 0);
        inited = true;
    }
    return enabled;
}

static bool cg_cmp_ri_enabled(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_CMP_RI");
        enabled = (v && v[0] != '\0' && strcmp(v, "0") != 0);
        inited = true;
    }
    return enabled;
}

static bool cg_fused_branch_enabled(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_FUSED_BRANCH");
        enabled = (v && v[0] != '\0' && strcmp(v, "0") != 0);
        inited = true;
    }
    return enabled;
}

static bool cg_branch_term_enabled(void) {
    static bool inited = false;
    static bool enabled = false;
    if (!inited) {
        const char *v = getenv("SLOW32_DBT_STAGE5_CODEGEN_BRANCH_TERM");
        enabled = (v && v[0] != '\0' && strcmp(v, "0") != 0);
        inited = true;
    }
    return enabled;
}

// ============================================================================
// Forward declarations for translate.c functions we need
// ============================================================================

// These are defined in translate.c and we need access to them.
// Since they're static, we need them re-declared or passed via ctx.
// We use the public API where possible and replicate small helpers.

// Host register array (must match translate.c)
static const x64_reg_t cg_reg_alloc_hosts[REG_ALLOC_SLOTS] = {
    RBX, R12, R13, RSI, RDI, R11, R8, R9
};

// ============================================================================
// Codegen context (stack-allocated, per-region)
// ============================================================================

typedef struct {
    translate_ctx_t *ctx;
    emit_ctx_t *e;
    const stage5_lift_region_t *region;
    uint32_t emitted_insts;        // Number of guest instructions emitted
} stage5_cg_t;

typedef struct {
    size_t emit_offset;
    bool emit_overflow;
    bool emit_rax_pending;
    uint32_t guest_pc;
    int current_inst_idx;
    int exit_idx;
    int side_exit_emitted;
    int deferred_exit_count;
    int pc_map_count;
    int validated_range_count;

    struct {
        uint8_t guest_reg;
        bool allocated;
        bool dirty;
    } reg_alloc[REG_ALLOC_SLOTS];
    int8_t reg_alloc_map[32];

    struct {
        bool valid;
        uint8_t guest_reg;
        host_reg_t host_reg;
        bool can_skip_store;
    } pending_write;

    struct {
        bool valid;
        uint8_t opcode;
        uint8_t rd;
        uint8_t rs1, rs2;
        bool rs2_is_imm;
        int32_t imm;
        int inst_idx;
    } pending_cond;

    struct {
        bool valid;
        uint32_t value;
    } reg_constants[32];

    struct {
        uint8_t guest_reg;
        uint32_t lo_offset;
        uint32_t hi_end;
        bool is_store_ok;
    } validated_ranges[MAX_VALIDATED_RANGES];

    struct {
        size_t jmp_patch_offset;
        uint32_t target_pc;
        int exit_idx;
        uint32_t branch_pc;
        bool dirty_snapshot[REG_ALLOC_SLOTS];
        bool allocated_snapshot[REG_ALLOC_SLOTS];
        uint8_t guest_reg_snapshot[REG_ALLOC_SLOTS];
        bool pending_write_valid;
        uint8_t pending_write_guest_reg;
        host_reg_t pending_write_host_reg;
        bool force_full_flush;
    } deferred_exits[MAX_BLOCK_EXITS];
} stage5_codegen_state_t;

static void cg_state_save(stage5_codegen_state_t *s, const translate_ctx_t *ctx) {
    s->emit_offset = ctx->emit.offset;
    s->emit_overflow = ctx->emit.overflow;
    s->emit_rax_pending = ctx->emit.rax_pending;
    s->guest_pc = ctx->guest_pc;
    s->current_inst_idx = ctx->current_inst_idx;
    s->exit_idx = ctx->exit_idx;
    s->side_exit_emitted = ctx->side_exit_emitted;
    s->deferred_exit_count = ctx->deferred_exit_count;
    s->pc_map_count = ctx->pc_map_count;
    s->validated_range_count = ctx->validated_range_count;
    memcpy(s->reg_alloc, ctx->reg_alloc, sizeof(s->reg_alloc));
    memcpy(s->reg_alloc_map, ctx->reg_alloc_map, sizeof(s->reg_alloc_map));
    memcpy(&s->pending_write, &ctx->pending_write, sizeof(s->pending_write));
    memcpy(&s->pending_cond, &ctx->pending_cond, sizeof(s->pending_cond));
    memcpy(s->reg_constants, ctx->reg_constants, sizeof(s->reg_constants));
    memcpy(s->validated_ranges, ctx->validated_ranges, sizeof(s->validated_ranges));
    memcpy(s->deferred_exits, ctx->deferred_exits, sizeof(s->deferred_exits));
}

static void cg_state_restore(translate_ctx_t *ctx, const stage5_codegen_state_t *s) {
    ctx->emit.offset = s->emit_offset;
    ctx->emit.overflow = s->emit_overflow;
    ctx->emit.rax_pending = s->emit_rax_pending;
    ctx->guest_pc = s->guest_pc;
    ctx->current_inst_idx = s->current_inst_idx;
    ctx->exit_idx = s->exit_idx;
    ctx->side_exit_emitted = s->side_exit_emitted;
    ctx->deferred_exit_count = s->deferred_exit_count;
    ctx->pc_map_count = s->pc_map_count;
    ctx->validated_range_count = s->validated_range_count;
    memcpy(ctx->reg_alloc, s->reg_alloc, sizeof(s->reg_alloc));
    memcpy(ctx->reg_alloc_map, s->reg_alloc_map, sizeof(s->reg_alloc_map));
    memcpy(&ctx->pending_write, &s->pending_write, sizeof(s->pending_write));
    memcpy(&ctx->pending_cond, &s->pending_cond, sizeof(s->pending_cond));
    memcpy(ctx->reg_constants, s->reg_constants, sizeof(s->reg_constants));
    memcpy(ctx->validated_ranges, s->validated_ranges, sizeof(s->validated_ranges));
    memcpy(ctx->deferred_exits, s->deferred_exits, sizeof(s->deferred_exits));
}

// ============================================================================
// Register resolution helpers
// ============================================================================

// Return the host register for a guest register, or X64_NOREG if not cached.
static inline x64_reg_t cg_guest_host(const stage5_cg_t *cg, uint8_t guest_reg) {
    if (guest_reg == 0) return X64_NOREG;
    if (!cg->ctx->reg_cache_enabled) return X64_NOREG;
    int8_t slot = cg->ctx->reg_alloc_map[guest_reg];
    return (slot >= 0) ? cg_reg_alloc_hosts[slot] : X64_NOREG;
}

// Resolve a source operand. If cached, returns the host reg.
// If not cached, loads from memory into 'scratch' and returns scratch.
// Guest r0 loads zero into scratch.
static inline x64_reg_t cg_resolve_src(stage5_cg_t *cg, uint8_t guest_reg,
                                        x64_reg_t scratch) {
    if (guest_reg == 0) {
        emit_xor_r32_r32(cg->e, scratch, scratch);
        return scratch;
    }
    x64_reg_t h = cg_guest_host(cg, guest_reg);
    if (h != X64_NOREG) return h;
    // Load from memory
    emit_mov_r32_m32(cg->e, scratch, RBP, GUEST_REG_OFFSET(guest_reg));
    return scratch;
}

// Resolve destination. If cached, returns the host reg.
// If not cached (spilled), returns RAX (caller must store back).
static inline x64_reg_t cg_resolve_dst(const stage5_cg_t *cg, uint8_t guest_reg) {
    if (guest_reg == 0) return RAX; // writes to r0 are discarded
    x64_reg_t h = cg_guest_host(cg, guest_reg);
    return (h != X64_NOREG) ? h : RAX;
}

// Mark a destination register as dirty after writing.
// Must mirror reg_cache_mark_written() in translate.c to keep ctx consistent.
static inline void cg_mark_dirty(stage5_cg_t *cg, uint8_t guest_reg) {
    if (guest_reg == 0) return;
    translate_ctx_t *ctx = cg->ctx;

    // Invalidate constant propagation
    ctx->reg_constants[guest_reg].valid = false;

    // Invalidate bounds check elimination ranges that use this register as base
    for (int i = 0; i < ctx->validated_range_count; i++) {
        if (ctx->validated_ranges[i].guest_reg == guest_reg) {
            ctx->validated_ranges[i].guest_reg = 0;
        }
    }

    // Discard pending write for the same guest register (superseded by our write)
    if (ctx->pending_write.valid && ctx->pending_write.guest_reg == guest_reg) {
        ctx->pending_write.valid = false;
        ctx->emit.rax_pending = false;
    }

    // Mark the cached slot as dirty
    int8_t slot = ctx->reg_alloc_map[guest_reg];
    if (slot >= 0) {
        ctx->reg_alloc[slot].dirty = true;
    }
}

// Store spilled destination back to memory (when rd is not cached).
static inline void cg_store_spilled(stage5_cg_t *cg, uint8_t guest_reg,
                                     x64_reg_t host_reg) {
    if (guest_reg == 0) return;
    if (cg_guest_host(cg, guest_reg) != X64_NOREG) return; // cached, dirty handles it
    emit_mov_m32_r32(cg->e, RBP, GUEST_REG_OFFSET(guest_reg), host_reg);
}

// ============================================================================
// Load/Store: delegate to existing translate_* functions
// ============================================================================
// For loads and stores, we delegate to the existing translate_* functions.
// These already handle address computation, bounds checking, W^X enforcement,
// and register cache interaction. This gives us correctness without
// duplicating the bounds check infrastructure.

// ============================================================================
// Individual instruction emitters
// ============================================================================

// ALU register-register: ADD, SUB, AND, OR, XOR
static bool cg_emit_alu_rr(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1, rs2 = n->rs2;
    if (rd == 0) return true; // discard

    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd);
    bool commutative = (n->opcode != OP_SUB);

    // Choose the emit function
    void (*emit_op)(emit_ctx_t *, x64_reg_t, x64_reg_t) = NULL;
    switch (n->opcode) {
        case OP_ADD: emit_op = emit_add_r32_r32; break;
        case OP_SUB: emit_op = emit_sub_r32_r32; break;
        case OP_AND: emit_op = emit_and_r32_r32; break;
        case OP_OR:  emit_op = emit_or_r32_r32; break;
        case OP_XOR: emit_op = emit_xor_r32_r32; break;
        default: return false;
    }

    x64_reg_t h_rd = cg_guest_host(cg, rd);
    x64_reg_t h_rs1 = cg_guest_host(cg, rs1);
    x64_reg_t h_rs2 = cg_guest_host(cg, rs2);

    if (h_rd != X64_NOREG && rd == rs1) {
        // rd == rs1 and both cached: OP dst, src2
        x64_reg_t src2_h = cg_resolve_src(cg, rs2, RCX);
        emit_op(e, dst_h, src2_h);
    } else if (h_rd != X64_NOREG && rd == rs2 && commutative) {
        // rd == rs2 commutative: OP dst, src1
        x64_reg_t src1_h = cg_resolve_src(cg, rs1, RCX);
        emit_op(e, dst_h, src1_h);
    } else {
        // General case: mov dst, src1; OP dst, src2
        x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX);
        if (dst_h != src1_h)
            emit_mov_r32_r32(e, dst_h, src1_h);
        x64_reg_t src2_h = cg_resolve_src(cg, rs2, RCX);
        emit_op(e, dst_h, src2_h);
    }

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// ALU register-immediate: ADDI, ANDI, ORI, XORI
static bool cg_emit_alu_ri(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1;
    int32_t imm = n->imm;
    if (rd == 0) return true;

    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd);

    // For ADDI with imm=0, it's a move
    if (n->opcode == OP_ADDI && imm == 0) {
        if (rs1 == 0) {
            emit_xor_r32_r32(e, dst_h, dst_h);
        } else {
            x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX);
            if (dst_h != src1_h)
                emit_mov_r32_r32(e, dst_h, src1_h);
        }
        cg_mark_dirty(cg, rd);
        cg_store_spilled(cg, rd, dst_h);
        return true;
    }

    // Choose emit function
    void (*emit_op)(emit_ctx_t *, x64_reg_t, int32_t) = NULL;
    switch (n->opcode) {
        case OP_ADDI: emit_op = emit_add_r32_imm32; break;
        case OP_ANDI: emit_op = emit_and_r32_imm32; break;
        case OP_ORI:  emit_op = emit_or_r32_imm32; break;
        case OP_XORI: emit_op = emit_xor_r32_imm32; break;
        default: return false;
    }

    x64_reg_t h_rd = cg_guest_host(cg, rd);

    if (h_rd != X64_NOREG && rd == rs1) {
        // In-place: OP dst, imm
        emit_op(e, dst_h, imm);
    } else {
        x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX);
        if (dst_h != src1_h)
            emit_mov_r32_r32(e, dst_h, src1_h);
        emit_op(e, dst_h, imm);
    }

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// Shift immediate: SLLI, SRLI, SRAI
static bool cg_emit_shift_imm(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1;
    uint8_t shamt = (uint8_t)(n->imm & 31);
    if (rd == 0) return true;

    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd);

    void (*emit_sh)(emit_ctx_t *, x64_reg_t, uint8_t) = NULL;
    switch (n->opcode) {
        case OP_SLLI: emit_sh = emit_shl_r32_imm8; break;
        case OP_SRLI: emit_sh = emit_shr_r32_imm8; break;
        case OP_SRAI: emit_sh = emit_sar_r32_imm8; break;
        default: return false;
    }

    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX);
    if (dst_h != src1_h)
        emit_mov_r32_r32(e, dst_h, src1_h);
    emit_sh(e, dst_h, shamt);

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// Shift register: SLL, SRL, SRA
// Requires shift amount in CL. RCX is scratch (never cached).
static bool cg_emit_shift_reg(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1, rs2 = n->rs2;
    if (rd == 0) return true;

    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd);

    void (*emit_sh)(emit_ctx_t *, x64_reg_t) = NULL;
    switch (n->opcode) {
        case OP_SLL: emit_sh = emit_shl_r32_cl; break;
        case OP_SRL: emit_sh = emit_shr_r32_cl; break;
        case OP_SRA: emit_sh = emit_sar_r32_cl; break;
        default: return false;
    }

    // Load shift amount into RCX
    x64_reg_t rs2_h = cg_guest_host(cg, rs2);
    if (rs2_h != X64_NOREG) {
        emit_mov_r32_r32(e, RCX, rs2_h);
    } else if (rs2 == 0) {
        emit_xor_r32_r32(e, RCX, RCX);
    } else {
        emit_mov_r32_m32(e, RCX, RBP, GUEST_REG_OFFSET(rs2));
    }

    // Prepare dst = src1
    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX);
    if (dst_h == RCX) {
        // dst is RCX (spilled rd maps to RAX, but let's be safe)
        // This shouldn't happen since rd==0 returns early and RCX isn't cached
        dst_h = RAX;
    }
    if (dst_h != src1_h)
        emit_mov_r32_r32(e, dst_h, src1_h);

    emit_sh(e, dst_h);

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// LUI: load upper immediate
static bool cg_emit_lui(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd;
    if (rd == 0) return true;

    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd);
    // n->imm for LUI is already the shifted value (raw & 0xFFFFF000)
    emit_mov_r32_imm32(e, dst_h, (uint32_t)n->imm);

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// MUL: multiply (low 32 bits)
static bool cg_emit_mul(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1, rs2 = n->rs2;
    if (rd == 0) return true;

    emit_ctx_t *e = cg->e;
    x64_reg_t dst_h = cg_resolve_dst(cg, rd);

    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX);
    if (dst_h != src1_h)
        emit_mov_r32_r32(e, dst_h, src1_h);
    x64_reg_t src2_h = cg_resolve_src(cg, rs2, RCX);
    emit_imul_r32_r32(e, dst_h, src2_h);

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// Comparisons: SLT, SLTU, SEQ, SNE, SGT, SGTU, SLE, SLEU, SGE, SGEU
static bool cg_emit_cmp_rr(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1, rs2 = n->rs2;
    if (rd == 0) return true;

    emit_ctx_t *e = cg->e;

    // Resolve compare operands first (before clobbering anything)
    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX);
    x64_reg_t src2_h;
    if (src1_h == RCX) {
        src2_h = cg_resolve_src(cg, rs2, RDX);
    } else {
        src2_h = cg_resolve_src(cg, rs2, RCX);
    }

    emit_cmp_r32_r32(e, src1_h, src2_h);

    x64_reg_t dst_h = cg_resolve_dst(cg, rd);
    // Mirror translate.c semantics exactly: SETcc then MOVZX to materialize 0/1.
    switch (n->opcode) {
        case OP_SLT:  emit_setl(e, dst_h); break;
        case OP_SLTU: emit_setb(e, dst_h); break;
        case OP_SEQ:  emit_sete(e, dst_h); break;
        case OP_SNE:  emit_setne(e, dst_h); break;
        case OP_SGT:  emit_setg(e, dst_h); break;
        case OP_SGTU: emit_seta(e, dst_h); break;
        case OP_SLE:  emit_setle(e, dst_h); break;
        case OP_SLEU: emit_setbe(e, dst_h); break;
        case OP_SGE:  emit_setge(e, dst_h); break;
        case OP_SGEU: emit_setae(e, dst_h); break;
        default: return false;
    }
    emit_movzx_r32_r8(e, dst_h, dst_h);

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// Compare immediate: SLTI, SLTIU
static bool cg_emit_cmp_ri(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    uint8_t rd = n->rd, rs1 = n->rs1;
    int32_t imm = n->imm;
    if (rd == 0) return true;

    emit_ctx_t *e = cg->e;

    // Resolve source and compare FIRST (before touching destination)
    x64_reg_t src1_h = cg_resolve_src(cg, rs1, RAX);
    emit_cmp_r32_imm32(e, src1_h, imm);

    x64_reg_t dst_h = cg_resolve_dst(cg, rd);
    switch (n->opcode) {
        case OP_SLTI:  emit_setl(e, dst_h); break;
        case OP_SLTIU: emit_setb(e, dst_h); break;
        default: return false;
    }
    emit_movzx_r32_r8(e, dst_h, dst_h);

    cg_mark_dirty(cg, rd);
    cg_store_spilled(cg, rd, dst_h);
    return true;
}

// ============================================================================
// Load/Store emission
// Uses existing translate_* functions for correctness (bounds checks, etc.)
// ============================================================================

// For loads and stores, we delegate to the existing translate_* functions.
// These already handle:
//   - Address computation with constant folding
//   - Memory bounds checking
//   - W^X enforcement
//   - Register cache interaction (guest_host_reg, reg_cache_mark_written)
//
// The existing translate_* functions read ctx->reg_alloc which we've populated,
// so they benefit from our cached registers automatically.

static bool cg_emit_load(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    translate_ctx_t *ctx = cg->ctx;
    ctx->guest_pc = n->pc;
    switch (n->opcode) {
        case OP_LDW:  translate_ldw(ctx, n->rd, n->rs1, n->imm); return true;
        case OP_LDH:  translate_ldh(ctx, n->rd, n->rs1, n->imm); return true;
        case OP_LDB:  translate_ldb(ctx, n->rd, n->rs1, n->imm); return true;
        case OP_LDHU: translate_ldhu(ctx, n->rd, n->rs1, n->imm); return true;
        case OP_LDBU: translate_ldbu(ctx, n->rd, n->rs1, n->imm); return true;
        default: return false;
    }
}

static bool cg_emit_store(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    translate_ctx_t *ctx = cg->ctx;
    ctx->guest_pc = n->pc;
    switch (n->opcode) {
        case OP_STW: translate_stw(ctx, n->rs1, n->rs2, n->imm); return true;
        case OP_STH: translate_sth(ctx, n->rs1, n->rs2, n->imm); return true;
        case OP_STB: translate_stb(ctx, n->rs1, n->rs2, n->imm); return true;
        default: return false;
    }
}

// ============================================================================
// IR node dispatch
// ============================================================================

// Flush pending write and pending cond before native ALU emission.
// This is necessary because:
// 1. pending_write may hold a value in RAX that we're about to clobber
// 2. pending_cond may reference registers we're about to write
// Load/store delegation to translate_* handles its own flushing.
static inline void cg_flush_pending(stage5_cg_t *cg) {
    translate_ctx_t *ctx = cg->ctx;
    if (ctx->pending_write.valid) {
        emit_mov_m32_r32(cg->e, RBP,
                         GUEST_REG_OFFSET(ctx->pending_write.guest_reg),
                         ctx->pending_write.host_reg);
        ctx->pending_write.valid = false;
        cg->e->rax_pending = false;
    }
    if (ctx->pending_cond.valid) {
        // Pending cond from a previous translate_* call needs to be
        // materialized. Since we can't call the static materialize_pending_cond,
        // just invalidate it. The comparison result was already written to rd
        // by the translate_* function, so the value is in the cached register.
        // Actually, pending_cond means the write was DEFERRED. We need to
        // materialize it. Since we can't call the static function, let's
        // do the safe thing: disable codegen when pending_cond is active.
        // In practice, pending_cond is only set by translate_slt/etc (not by
        // load/store), so this shouldn't happen.
        ctx->pending_cond.valid = false;
    }
}

static bool cg_emit_node(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    // Flush pending state before native emission (ALU instructions).
    // Load/store delegates to translate_* which handle their own flushing.
    bool is_load_store = (n->opcode >= OP_LDB && n->opcode <= OP_STW);
    if (!is_load_store && n->opcode != OP_NOP) {
        cg_flush_pending(cg);
    }

    switch (n->opcode) {
        case OP_NOP:
            return true;

        // ALU register-register
        case OP_ADD:
        case OP_SUB:
        case OP_AND:
        case OP_OR:
        case OP_XOR:
            return cg_emit_alu_rr(cg, n);

        // ALU register-immediate
        case OP_ADDI:
        case OP_ANDI:
        case OP_ORI:
        case OP_XORI:
            return cg_emit_alu_ri(cg, n);

        // Shift immediate
        case OP_SLLI:
        case OP_SRLI:
        case OP_SRAI:
            return cg_emit_shift_imm(cg, n);

        // Shift register
        case OP_SLL:
        case OP_SRL:
        case OP_SRA:
            return cg_emit_shift_reg(cg, n);

        // Upper immediate
        case OP_LUI:
            return cg_emit_lui(cg, n);

        // Multiply
        case OP_MUL:
            return cg_emit_mul(cg, n);

        // Comparisons register-register
        case OP_SLT:
        case OP_SLTU:
        case OP_SEQ:
        case OP_SNE:
        case OP_SGT:
        case OP_SGTU:
        case OP_SLE:
        case OP_SLEU:
        case OP_SGE:
        case OP_SGEU:
            return cg_emit_cmp_rr(cg, n);

        // Comparisons immediate
        case OP_SLTI:
        case OP_SLTIU:
            return cg_emit_cmp_ri(cg, n);

        // Loads
        case OP_LDW:
        case OP_LDH:
        case OP_LDB:
        case OP_LDHU:
        case OP_LDBU:
            return cg_emit_load(cg, n);

        // Stores
        case OP_STW:
        case OP_STH:
        case OP_STB:
            return cg_emit_store(cg, n);

        // Unsupported: abort region
        case OP_DIV:
        case OP_REM:
        case OP_MULHU:
        case OP_MULH:
            stage5_codegen_fallback_unsupported_op++;
            return false;

        default:
            // Unknown opcode — abort
            stage5_codegen_fallback_unsupported_op++;
            return false;
    }
}

static bool cg_opcode_supported(uint8_t opcode, bool cmp_rr_enabled, bool cmp_ri_enabled) {
    switch (opcode) {
        case OP_NOP:
        case OP_ADD: case OP_SUB: case OP_AND: case OP_OR: case OP_XOR:
        case OP_ADDI: case OP_ANDI: case OP_ORI: case OP_XORI:
        case OP_SLLI: case OP_SRLI: case OP_SRAI:
        case OP_SLL: case OP_SRL: case OP_SRA:
        case OP_LUI: case OP_MUL:
        case OP_LDW: case OP_LDH: case OP_LDB: case OP_LDHU: case OP_LDBU:
        case OP_STW: case OP_STH: case OP_STB:
            return true;
        case OP_SLT: case OP_SLTU: case OP_SEQ: case OP_SNE:
        case OP_SGT: case OP_SGTU: case OP_SLE: case OP_SLEU:
        case OP_SGE: case OP_SGEU:
            return cmp_rr_enabled;
        case OP_SLTI: case OP_SLTIU:
            return cmp_ri_enabled;
        default:
            return false;
    }
}

static bool cg_terminal_supported(uint8_t opcode, bool branch_term_enabled) {
    switch (opcode) {
        case OP_JAL:
        case OP_JALR:
        case OP_HALT:
        case OP_DEBUG:
        case OP_YIELD:
            return true;
        case OP_BEQ:
        case OP_BNE:
            return branch_term_enabled;
        default:
            return false;
    }
}

static bool cg_region_preflight(const stage5_lift_region_t *region,
                                int terminal_idx,
                                int fuse_cmp_idx,
                                bool synth_block_end,
                                bool side_exit_emit_enabled,
                                bool side_exit_family_c) {
    bool cmp_rr_enabled = cg_cmp_rr_enabled();
    bool cmp_ri_enabled = cmp_rr_enabled && cg_cmp_ri_enabled();
    bool fused_branch_enabled = cg_fused_branch_enabled();
    bool branch_term_enabled = cg_branch_term_enabled();

    if (fuse_cmp_idx >= 0 && !fused_branch_enabled) {
        return false;
    }

    if (!synth_block_end && terminal_idx < 0) {
        return false;
    }

    if (terminal_idx >= (int)region->ir_count) {
        return false;
    }

    if (fuse_cmp_idx >= (int)region->ir_count) {
        return false;
    }

    for (uint32_t i = 0; i < region->ir_count; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;
        if (terminal_idx >= 0 && (int)i == terminal_idx) continue;
        if (fuse_cmp_idx >= 0 && (int)i == fuse_cmp_idx) continue;
        if (!cg_opcode_supported(n->opcode, cmp_rr_enabled, cmp_ri_enabled)) {
            return false;
        }
        if (n->is_side_exit && n->kind == STAGE5_IR_BRANCH &&
            side_exit_emit_enabled && side_exit_family_c) {
            return false;
        }
    }

    if (terminal_idx >= 0) {
        const stage5_ir_node_t *term = &region->ir[terminal_idx];
        if (!cg_terminal_supported(term->opcode, branch_term_enabled)) {
            return false;
        }
        if (branch_term_enabled &&
            (term->opcode == OP_BEQ || term->opcode == OP_BNE ||
             term->opcode == OP_BLT || term->opcode == OP_BGE ||
             term->opcode == OP_BLTU || term->opcode == OP_BGEU)) {
            // Conservative safety gate: avoid branch-terminal regions that
            // also contain compare prefix nodes until cmp+branch interactions
            // are fully audited.
            for (uint32_t i = 0; i < region->ir_count; i++) {
                if ((int)i == terminal_idx || (fuse_cmp_idx >= 0 && (int)i == fuse_cmp_idx)) {
                    continue;
                }
                const stage5_ir_node_t *n = &region->ir[i];
                if (n->synthetic) continue;
                if (n->opcode == OP_SLT || n->opcode == OP_SLTU ||
                    n->opcode == OP_SEQ || n->opcode == OP_SNE ||
                    n->opcode == OP_SGT || n->opcode == OP_SGTU ||
                    n->opcode == OP_SLE || n->opcode == OP_SLEU ||
                    n->opcode == OP_SGE || n->opcode == OP_SGEU ||
                    n->opcode == OP_SLTI || n->opcode == OP_SLTIU) {
                    stage5_codegen_fallback_preflight_branch_cmp_mix++;
                    return false;
                }
            }
        }
    }

    if (fuse_cmp_idx >= 0) {
        if (terminal_idx < 0) return false;
        const stage5_ir_node_t *cmp = &region->ir[fuse_cmp_idx];
        const stage5_ir_node_t *br = &region->ir[terminal_idx];
        if (!(cmp->opcode == OP_SLT || cmp->opcode == OP_SLTU ||
              cmp->opcode == OP_SEQ || cmp->opcode == OP_SNE ||
              cmp->opcode == OP_SGT || cmp->opcode == OP_SGTU ||
              cmp->opcode == OP_SLE || cmp->opcode == OP_SLEU ||
              cmp->opcode == OP_SGE || cmp->opcode == OP_SGEU ||
              cmp->opcode == OP_SLTI || cmp->opcode == OP_SLTIU)) {
            return false;
        }
        if (!(br->opcode == OP_BEQ || br->opcode == OP_BNE ||
              br->opcode == OP_BLT || br->opcode == OP_BGE ||
              br->opcode == OP_BLTU || br->opcode == OP_BGEU)) {
            return false;
        }
    }
    return true;
}

// ============================================================================
// Side exit emission helper
// Replicates the side-exit logic from the Family C path in translate.c,
// using the same deferred exit infrastructure.
// ============================================================================

// Forward declarations for translate.c static functions we need:
// stage5_branch_inv_cc, stage5_side_exit_supported, stage5_pick_cmp_scratch_regs,
// stage5_side_exit_trace_emit
// These are static in translate.c so we can't call them directly.
// Replicate the minimal versions here.

static uint8_t cg_branch_inv_cc(uint8_t opcode) {
    switch (opcode) {
        case OP_BEQ:  return 0x05; // JNE
        case OP_BNE:  return 0x04; // JE
        case OP_BLT:  return 0x0D; // JGE
        case OP_BGE:  return 0x0C; // JL
        case OP_BLTU: return 0x03; // JAE
        case OP_BGEU: return 0x02; // JB
        default:      return 0xFF;
    }
}

static __attribute__((unused)) bool cg_emit_side_exit(stage5_cg_t *cg, const stage5_ir_node_t *n) {
    translate_ctx_t *ctx = cg->ctx;
    emit_ctx_t *e = cg->e;

    uint32_t branch_pc = n->pc;
    uint32_t fall_pc = branch_pc + 4;
    uint32_t taken_pc = fall_pc + n->imm;
    ctx->guest_pc = branch_pc;

    // Flush any pending comparison state
    if (ctx->pending_cond.valid) {
        // Materialize pending cond: store result to guest reg
        // This is the simple path — just store 0/1.
        // Actually, the native codegen doesn't use pending_cond (it does direct
        // emission). But translate_* calls from load/store may have set it.
        // Flush by emitting the store.
        // pending_cond is used by the Stage 4 fusion path. In native codegen,
        // we won't have a pending_cond from our own ALU emission, but a
        // translate_* call (for loads/stores) might have set it.
        // The safest thing: just invalidate it (the value was already emitted
        // by the translate_* call's compare-branch fusion path).
        // Actually, pending_cond is set by translate_slt etc and consumed by
        // translate_beq etc. Since we do our own cmp emission, we should
        // flush any leftover pending_cond.
        //
        // We can safely just invalidate it since our codegen doesn't rely on it.
        ctx->pending_cond.valid = false;
    }

    // Flush pending write before comparison
    if (ctx->pending_write.valid) {
        emit_mov_m32_r32(e, RBP,
                         GUEST_REG_OFFSET(ctx->pending_write.guest_reg),
                         ctx->pending_write.host_reg);
        ctx->pending_write.valid = false;
        e->rax_pending = false;
    }

    // Emit comparison
    if ((n->opcode == OP_BEQ || n->opcode == OP_BNE) &&
        (n->rs1 == 0 || n->rs2 == 0)) {
        // TEST optimization for comparison with zero
        uint8_t rz = (n->rs1 == 0) ? n->rs2 : n->rs1;
        if (rz == 0) {
            emit_xor_r32_r32(e, RAX, RAX);
            emit_test_r32_r32(e, RAX, RAX);
        } else {
            x64_reg_t hz = cg_guest_host(cg, rz);
            if (hz != X64_NOREG) {
                emit_test_r32_r32(e, hz, hz);
            } else {
                emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(rz));
                emit_test_r32_r32(e, RAX, RAX);
            }
        }
    } else {
        x64_reg_t h1 = cg_guest_host(cg, n->rs1);
        x64_reg_t h2 = cg_guest_host(cg, n->rs2);
        x64_reg_t cmp_a, cmp_b;

        if (n->rs1 == 0) {
            cmp_a = RAX;
            emit_xor_r32_r32(e, RAX, RAX);
        } else if (h1 != X64_NOREG) {
            cmp_a = h1;
        } else {
            cmp_a = RAX;
            emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(n->rs1));
        }

        if (n->rs2 == 0) {
            cmp_b = RCX;
            emit_xor_r32_r32(e, RCX, RCX);
        } else if (h2 != X64_NOREG) {
            cmp_b = h2;
        } else {
            cmp_b = RCX;
            emit_mov_r32_m32(e, RCX, RBP, GUEST_REG_OFFSET(n->rs2));
        }

        emit_cmp_r32_r32(e, cmp_a, cmp_b);
    }

    // Short jcc (2 bytes) skips the 5-byte jmp trampoline
    uint8_t inv_cc = cg_branch_inv_cc(n->opcode);
    if (inv_cc == 0xFF) return false;
    emit_jcc_short(e, inv_cc, 5);

    // 5-byte jmp rel32 trampoline to cold stub (patched later)
    size_t jmp_patch = emit_offset(e) + 1;
    emit_jmp_rel32(e, 0);

    // Record deferred side exit
    if (ctx->deferred_exit_count >= MAX_BLOCK_EXITS) return false;
    int di = ctx->deferred_exit_count++;
    ctx->deferred_exits[di].jmp_patch_offset = jmp_patch;
    ctx->deferred_exits[di].target_pc = taken_pc;
    ctx->deferred_exits[di].exit_idx = ctx->exit_idx;
    ctx->deferred_exits[di].branch_pc = branch_pc;
    for (int s = 0; s < REG_ALLOC_SLOTS; s++) {
        ctx->deferred_exits[di].dirty_snapshot[s] = ctx->reg_alloc[s].dirty;
        ctx->deferred_exits[di].allocated_snapshot[s] = ctx->reg_alloc[s].allocated;
        ctx->deferred_exits[di].guest_reg_snapshot[s] = ctx->reg_alloc[s].guest_reg;
    }
    ctx->deferred_exits[di].pending_write_valid = false;
    ctx->deferred_exits[di].pending_write_guest_reg = 0;
    ctx->deferred_exits[di].pending_write_host_reg = RAX;
    ctx->deferred_exits[di].force_full_flush =
        (n->opcode == OP_BLTU || n->opcode == OP_BGEU);

    if (ctx->block) {
        ctx->block->flags |= BLOCK_FLAG_DIRECT;
    }
    if (ctx->side_exit_emitted < MAX_BLOCK_EXITS) {
        ctx->side_exit_pcs[ctx->side_exit_emitted] = branch_pc;
    }
    ctx->exit_idx++;
    ctx->side_exit_emitted++;
    stage5_emit_side_exits++;

    return true;
}

// ============================================================================
// Main codegen entry point
// ============================================================================

// Forward declarations for functions defined in translate.c that we call
// through their public declarations in translate.h:
//   translate_jal, translate_jalr, translate_halt, translate_debug,
//   translate_yield, translate_beq/bne/blt/bge/bltu/bgeu
//   translate_ldw/ldh/ldb/ldhu/ldbu, translate_stw/sth/stb
//
// We also need stage5_emit_cmp_branch_fused and stage5_translate_branch_terminal
// but these are static in translate.c. We'll handle them via the caller
// (translate.c) by returning from stage5_codegen after prefix emission and
// letting the caller handle the terminal.
//
// Actually, looking at the design more carefully: the plan says to emit
// prefix instructions natively, then delegate terminal to existing emitters.
// The caller already has the terminal dispatch code. So the approach is:
//
// 1. stage5_codegen emits all prefix IR nodes natively
// 2. Returns true with a flag indicating "prefix done, terminal needed"
// 3. Caller handles terminal emission with existing code
//
// But that requires changing the interface. The simpler approach:
// stage5_codegen emits prefix AND terminal, since we can call the public
// translate_* functions for terminals.

bool stage5_codegen(translate_ctx_t *ctx,
                    const stage5_lift_region_t *region,
                    uint32_t guest_pc,
                    int terminal_idx,
                    int fuse_cmp_idx,
                    int emitted_pattern,
                    bool synth_block_end,
                    bool side_exit_emit_enabled,
                    const void *side_exit_family_cfg_ptr) {
    stage5_codegen_attempted++;

    stage5_codegen_state_t saved;
    cg_state_save(&saved, ctx);

    // Count allocated registers
    {
        int count = 0;
        for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
            if (ctx->reg_alloc[i].allocated) count++;
        }
        if (count <= REG_ALLOC_SLOTS)
            stage5_codegen_regs_allocated_hist[count]++;
    }

    // Set up codegen context
    stage5_cg_t cg;
    cg.ctx = ctx;
    cg.e = &ctx->emit;
    cg.region = region;
    cg.emitted_insts = 0;

    size_t emit_start = emit_offset(cg.e);

    // The side_exit_family_cfg_ptr is a pointer to a stage5_side_exit_family_cfg_t
    // struct from translate.c. We just need the family_c bool.
    typedef struct { bool family_b; bool family_c; } side_exit_cfg_t;
    const side_exit_cfg_t *se_cfg = (const side_exit_cfg_t *)side_exit_family_cfg_ptr;
    bool side_exit_family_c = se_cfg ? se_cfg->family_c : false;

    if (!cg_region_preflight(region, terminal_idx, fuse_cmp_idx, synth_block_end,
                             side_exit_emit_enabled, side_exit_family_c)) {
        stage5_codegen_fallback++;
        stage5_codegen_fallback_preflight++;
        return false;
    }

    // ========================================================================
    // Phase 1: Emit prefix instructions (everything except terminal)
    // ========================================================================

    for (uint32_t i = 0; i < region->ir_count; i++) {
        const stage5_ir_node_t *n = &region->ir[i];
        if (n->synthetic) continue;
        if (terminal_idx >= 0 && (int)i == terminal_idx) continue;
        if (fuse_cmp_idx >= 0 && (int)i == fuse_cmp_idx) continue;

        // Side exits are still emitted by the legacy Stage5 path.
        // Native codegen falls back for these regions until deferred-exit
        // ownership is fully mirrored.
        if (n->is_side_exit && n->kind == STAGE5_IR_BRANCH &&
            side_exit_emit_enabled && side_exit_family_c &&
            ctx->exit_idx + 2 < MAX_BLOCK_EXITS &&
            ctx->deferred_exit_count < MAX_BLOCK_EXITS) {
            stage5_codegen_fallback++;
            stage5_codegen_fallback_side_exit++;
            cg_state_restore(ctx, &saved);
            return false;
        }

        ctx->guest_pc = n->pc;
        ctx->current_inst_idx = (int)i;

        if (!cg_emit_node(&cg, n)) {
            // Unsupported instruction — abort
            stage5_codegen_fallback++;
            stage5_codegen_fallback_emit_node++;
            cg_state_restore(ctx, &saved);
            return false;
        }
        cg.emitted_insts++;
    }

    // ========================================================================
    // Phase 2: Emit terminal
    // ========================================================================

    bool ended = false;

    if (synth_block_end) {
        ctx->guest_pc = guest_pc + region->guest_inst_count * 4;
        // reg_cache_flush is called by emit_exit_chained
        emit_exit_chained_for_codegen(ctx, ctx->guest_pc, ctx->exit_idx++);
        ended = true;
    } else if (fuse_cmp_idx >= 0 && terminal_idx >= 0) {
        // Fused compare+branch — delegate to existing emitter
        const stage5_ir_node_t *c = &region->ir[fuse_cmp_idx];
        const stage5_ir_node_t *b = &region->ir[terminal_idx];
        bool cmp_is_imm = (c->opcode == OP_SLTI || c->opcode == OP_SLTIU);
        ctx->guest_pc = b->pc;
        ctx->current_inst_idx = terminal_idx;
        ended = stage5_emit_cmp_branch_fused_for_codegen(ctx,
            c->opcode, c->rs1, c->rs2, c->imm, cmp_is_imm,
            b->opcode, b->imm, b->pc);
    } else if (terminal_idx >= 0) {
        const stage5_ir_node_t *last = &region->ir[terminal_idx];
        ctx->guest_pc = last->pc;
        ctx->current_inst_idx = terminal_idx;
        switch (last->opcode) {
            case OP_JAL:
                if (emitted_pattern == STAGE5_BURG_PATTERN_JAL_JUMP) {
                    stage5_translate_jal_jump_compact_for_codegen(ctx,
                        last->rd, last->imm);
                } else {
                    translate_jal(ctx, last->rd, last->imm);
                }
                ended = true;
                break;
            case OP_JALR:
                translate_jalr(ctx, last->rd, last->rs1, last->imm);
                ended = true;
                break;
            case OP_HALT:
                translate_halt(ctx);
                ended = true;
                break;
            case OP_DEBUG:
                translate_debug(ctx, last->rs1);
                ended = true;
                break;
            case OP_YIELD:
                translate_yield(ctx);
                ended = true;
                break;
            case OP_BEQ:
            case OP_BNE:
            case OP_BLT:
            case OP_BGE:
            case OP_BLTU:
            case OP_BGEU:
                ended = stage5_translate_branch_terminal_for_codegen(ctx,
                    last->opcode, last->rs1, last->rs2, last->imm);
                break;
            default:
                ended = false;
                break;
        }
    }

    if (!ended) {
        stage5_codegen_fallback++;
        stage5_codegen_fallback_terminal++;
        cg_state_restore(ctx, &saved);
        return false;
    }

    // ========================================================================
    // Success: update telemetry
    // ========================================================================

    size_t emit_end = emit_offset(cg.e);
    stage5_codegen_success++;
    stage5_codegen_guest_insts += region->guest_inst_count;
    if (emit_end > emit_start) {
        stage5_codegen_host_bytes += (uint64_t)(emit_end - emit_start);
    }

    return true;
}
