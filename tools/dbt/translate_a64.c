// SLOW-32 DBT: AArch64 Block Translator Implementation
// Stage 1 - Correctness-first dynamic binary translation
//
// AArch64 register convention:
//   X20 = cpu_state pointer (callee-saved)
//   X21 = mem_base pointer  (callee-saved)
//   X22 = lookup_table pointer (callee-saved, Stage 3)
//   W0  = primary scratch
//   W1  = secondary scratch
//   W2  = tertiary scratch
//   W3-W15 = general scratch

#include "translate.h"
#include "block_cache.h"
#include <stdio.h>
#include <string.h>

// Debug flag - set to 1 to enable translation tracing
#ifndef DBT_TRACE
#define DBT_TRACE 0
#endif

uint32_t superblock_profile_min_samples = SUPERBLOCK_PROFILE_MIN_SAMPLES;
uint32_t superblock_taken_pct_threshold = SUPERBLOCK_TAKEN_PCT_THRESHOLD;

// ============================================================================
// Instruction decoding (shared with x86-64 translator)
// ============================================================================

decoded_inst_t decode_instruction(uint32_t raw) {
    decoded_inst_t inst = {0};
    inst.raw = raw;
    inst.opcode = raw & 0x7F;

    switch (inst.opcode) {
        // R-type: register-register
        case OP_ADD: case OP_SUB: case OP_XOR: case OP_OR: case OP_AND:
        case OP_SLL: case OP_SRL: case OP_SRA:
        case OP_SLT: case OP_SLTU: case OP_MUL: case OP_MULH:
        case OP_DIV: case OP_REM: case OP_SEQ: case OP_SNE:
        case OP_SGT: case OP_SGTU: case OP_SLE: case OP_SLEU:
        case OP_SGE: case OP_SGEU: case OP_MULHU:
        // Floating-point R-type
        case OP_FADD_S: case OP_FSUB_S: case OP_FMUL_S: case OP_FDIV_S:
        case OP_FSQRT_S: case OP_FEQ_S: case OP_FLT_S: case OP_FLE_S:
        case OP_FCVT_W_S: case OP_FCVT_WU_S: case OP_FCVT_S_W: case OP_FCVT_S_WU:
        case OP_FNEG_S: case OP_FABS_S:
        case OP_FADD_D: case OP_FSUB_D: case OP_FMUL_D: case OP_FDIV_D:
        case OP_FSQRT_D: case OP_FEQ_D: case OP_FLT_D: case OP_FLE_D:
        case OP_FCVT_W_D: case OP_FCVT_WU_D: case OP_FCVT_D_W: case OP_FCVT_D_WU:
        case OP_FCVT_D_S: case OP_FCVT_S_D: case OP_FNEG_D: case OP_FABS_D:
        case OP_FCVT_L_S: case OP_FCVT_LU_S: case OP_FCVT_S_L: case OP_FCVT_S_LU:
        case OP_FCVT_L_D: case OP_FCVT_LU_D: case OP_FCVT_D_L: case OP_FCVT_D_LU:
            inst.format = FMT_R;
            inst.rd = (raw >> 7) & 0x1F;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.rs2 = (raw >> 20) & 0x1F;
            break;

        // I-type with zero-extended immediate
        case OP_ORI: case OP_ANDI: case OP_XORI: case OP_SLTIU:
            inst.format = FMT_I;
            inst.rd = (raw >> 7) & 0x1F;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.imm = (raw >> 20) & 0xFFF;  // Zero-extend
            break;

        // I-type with sign-extended immediate
        case OP_ADDI: case OP_SLLI: case OP_SRLI: case OP_SRAI: case OP_SLTI:
        case OP_LDB: case OP_LDH: case OP_LDW: case OP_LDBU: case OP_LDHU:
        case OP_JALR:
            inst.format = FMT_I;
            inst.rd = (raw >> 7) & 0x1F;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.imm = ((int32_t)raw) >> 20;  // Sign-extend
            break;

        // S-type (store)
        case OP_STB: case OP_STH: case OP_STW:
            inst.format = FMT_S;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.rs2 = (raw >> 20) & 0x1F;
            inst.imm = (int32_t)(((raw >> 7) & 0x1F) | ((raw >> 20) & 0xFE0));
            if (inst.imm & 0x800) inst.imm |= 0xFFFFF000;
            break;

        // B-type (branch)
        case OP_BEQ: case OP_BNE: case OP_BLT: case OP_BGE:
        case OP_BLTU: case OP_BGEU:
            inst.format = FMT_B;
            inst.rs1 = (raw >> 15) & 0x1F;
            inst.rs2 = (raw >> 20) & 0x1F;
            inst.imm = (((raw >> 8) & 0xF) << 1) |
                       (((raw >> 25) & 0x3F) << 5) |
                       (((raw >> 7) & 0x1) << 11) |
                       (((int32_t)raw >> 31) << 12);
            break;

        // U-type (upper immediate)
        case OP_LUI:
            inst.format = FMT_U;
            inst.rd = (raw >> 7) & 0x1F;
            inst.imm = raw & 0xFFFFF000;
            break;

        // J-type (jump)
        case OP_JAL: {
            inst.format = FMT_J;
            inst.rd = (raw >> 7) & 0x1F;
            uint32_t imm20 = (raw >> 31) & 0x1;
            uint32_t imm10_1 = (raw >> 21) & 0x3FF;
            uint32_t imm11 = (raw >> 20) & 0x1;
            uint32_t imm19_12 = (raw >> 12) & 0xFF;
            inst.imm = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
            if (inst.imm & 0x100000) inst.imm |= 0xFFE00000;
            break;
        }

        // Special instructions
        case OP_NOP: case OP_YIELD: case OP_DEBUG: case OP_HALT: case OP_ASSERT_EQ:
            inst.format = FMT_R;
            inst.rs1 = (raw >> 15) & 0x1F;
            if (inst.opcode == OP_ASSERT_EQ) {
                inst.rs2 = (raw >> 20) & 0x1F;
            }
            break;

        default:
            break;
    }

    return inst;
}

// ============================================================================
// Helper functions
// ============================================================================

void translate_init(translate_ctx_t *ctx, dbt_cpu_state_t *cpu) {
    memset(ctx, 0, sizeof(*ctx));
    ctx->cpu = cpu;
    ctx->cache = NULL;  // Stage 1 mode
    ctx->block = NULL;
    ctx->side_exit_info_enabled = false;
    ctx->avoid_backedge_extend = false;
    ctx->peephole_enabled = false;
}

void translate_init_cached(translate_ctx_t *ctx, dbt_cpu_state_t *cpu, block_cache_t *cache) {
    memset(ctx, 0, sizeof(*ctx));
    ctx->cpu = cpu;
    ctx->cache = cache;  // Stage 2 mode
    ctx->block = NULL;
    ctx->inline_lookup_enabled = false;
    ctx->ras_enabled = false;
    ctx->superblock_enabled = false;
    ctx->superblock_depth = 0;
    ctx->side_exit_info_enabled = false;
    ctx->avoid_backedge_extend = false;
    ctx->peephole_enabled = false;
    ctx->reg_cache_enabled = true;  // Stage 5: enable register caching
}

// ============================================================================
// Stage 5: Per-block register allocation (7-slot, callee-saved)
// ============================================================================

// AArch64 callee-saved registers available for guest reg caching.
// W20/W21/W22 are reserved for cpu_state/mem_base/lookup_table.
// W29 (FP) and W30 (LR) are preserved by ABI convention.
static const a64_reg_t reg_alloc_hosts[REG_ALLOC_SLOTS] = {
    W19, W23, W24, W25, W26, W27, W28, A64_NOREG
};
// Note: REG_ALLOC_SLOTS=8 but we only have 7 usable AArch64 callee-saved regs.
// Slot 7 is A64_NOREG and will never be allocated.

static void reg_alloc_reset(translate_ctx_t *ctx) {
    memset(ctx->reg_alloc, 0, sizeof(ctx->reg_alloc));
    memset(ctx->reg_alloc_map, -1, sizeof(ctx->reg_alloc_map));
    ctx->reg_cache_hits = 0;
    ctx->reg_cache_misses = 0;
}

// Forward declarations
static void bounds_elim_invalidate(translate_ctx_t *ctx, uint8_t guest_reg);

// Return the host register for a cached guest register, or A64_NOREG if not cached.
static inline a64_reg_t guest_host_reg(translate_ctx_t *ctx, uint8_t guest_reg) {
    if (!ctx->reg_cache_enabled || guest_reg == 0) return A64_NOREG;
    int8_t slot = ctx->reg_alloc_map[guest_reg];
    return (slot >= 0) ? reg_alloc_hosts[slot] : A64_NOREG;
}

// Mark a cached guest register as written (dirty).
// Used when the translate function writes directly into the cached host register.
static inline void reg_cache_mark_written(translate_ctx_t *ctx, uint8_t guest_reg) {
    ctx->reg_constants[guest_reg].valid = false;
    bounds_elim_invalidate(ctx, guest_reg);
    if (ctx->pending_write.valid && ctx->pending_write.guest_reg == guest_reg) {
        ctx->pending_write.valid = false;
        ctx->emit.rax_pending = false;
    }
    ctx->reg_cache_hits++;
    int8_t slot = ctx->reg_alloc_map[guest_reg];
    ctx->reg_alloc[slot].dirty = true;
}

// Pre-scan block: tally register usage, dead temporary analysis, back-edge detection,
// and select top-used guest registers for caching in host callee-saved registers.
static void reg_alloc_prescan(translate_ctx_t *ctx, uint32_t start_pc) {
    dbt_cpu_state_t *cpu = ctx->cpu;
    uint32_t use_count[32] = {0};

    memset(ctx->dead_temp_skip, 0, sizeof(ctx->dead_temp_skip));
    ctx->loop_written_regs = 0;
    ctx->backedge_target_count = 0;
    ctx->has_backedge = false;
    ctx->backedge_target_pc = 0;

    decoded_inst_t decoded[MAX_BLOCK_INSTS];
    uint32_t pc = start_pc;
    int inst_count = 0;

    while (inst_count < MAX_BLOCK_INSTS) {
        if (pc >= cpu->code_limit || (cpu->code_limit - pc) < 4)
            break;

        uint32_t raw = *(uint32_t *)(cpu->mem_base + pc);
        decoded_inst_t inst = decode_instruction(raw);
        decoded[inst_count] = inst;

        // Tally register usage (skip r0)
        switch (inst.format) {
            case FMT_R:
                if (inst.rd != 0)  use_count[inst.rd]++;
                if (inst.rs1 != 0) use_count[inst.rs1]++;
                if (inst.rs2 != 0) use_count[inst.rs2]++;
                break;
            case FMT_I:
                if (inst.rd != 0)  use_count[inst.rd]++;
                if (inst.rs1 != 0) use_count[inst.rs1]++;
                break;
            case FMT_S:
                if (inst.rs1 != 0) use_count[inst.rs1]++;
                if (inst.rs2 != 0) use_count[inst.rs2]++;
                break;
            case FMT_B:
                if (inst.rs1 != 0) use_count[inst.rs1]++;
                if (inst.rs2 != 0) use_count[inst.rs2]++;
                break;
            case FMT_U:
                if (inst.rd != 0) use_count[inst.rd]++;
                break;
            case FMT_J:
                if (inst.rd != 0) use_count[inst.rd]++;
                break;
        }

        // Stop at block-ending instructions
        bool is_block_end = false;
        switch (inst.opcode) {
            case OP_JAL: case OP_JALR:
            case OP_HALT: case OP_DEBUG: case OP_YIELD:
                is_block_end = true;
                break;
            case OP_BEQ: case OP_BNE: case OP_BLT: case OP_BGE:
            case OP_BLTU: case OP_BGEU:
                // Detect in-block back-edge for flush safety
                if (inst.imm < 0) {
                    uint32_t target = pc + 4 + inst.imm;
                    if (target >= start_pc) {
                        ctx->has_backedge = true;
                        ctx->backedge_target_pc = target;
                        bool seen = false;
                        for (int i = 0; i < ctx->backedge_target_count; i++) {
                            if (ctx->backedge_targets[i] == target) {
                                seen = true;
                                break;
                            }
                        }
                        if (!seen && ctx->backedge_target_count < MAX_BLOCK_INSTS) {
                            ctx->backedge_targets[ctx->backedge_target_count++] = target;
                        }

                        // Compute registers written in the loop body
                        uint32_t target_idx = (target - start_pc) / 4;
                        for (int k = (int)target_idx; k <= inst_count; k++) {
                            uint8_t wr = 0;
                            switch (decoded[k].format) {
                                case FMT_R: case FMT_I: case FMT_U: case FMT_J:
                                    wr = decoded[k].rd; break;
                                default: break;
                            }
                            if (wr != 0) ctx->loop_written_regs |= (1u << wr);
                        }
                    }
                }
                is_block_end = true;
                break;
            default:
                break;
        }
        if (is_block_end) {
            inst_count++;
            break;
        }

        pc += 4;
        inst_count++;
    }

    // Dead temporary elimination: backward liveness scan
    {
        int read_count_before_write[32];
        for (int r = 0; r < 32; r++)
            read_count_before_write[r] = 2;  // Conservative: assume live at block end

        for (int i = inst_count - 1; i >= 0; i--) {
            decoded_inst_t *dinst = &decoded[i];
            uint8_t wr = 0;
            uint8_t r1 = 0, r2 = 0;

            switch (dinst->format) {
                case FMT_R: wr = dinst->rd; r1 = dinst->rs1; r2 = dinst->rs2; break;
                case FMT_I: wr = dinst->rd; r1 = dinst->rs1; break;
                case FMT_S: r1 = dinst->rs1; r2 = dinst->rs2; break;
                case FMT_B: r1 = dinst->rs1; r2 = dinst->rs2; break;
                case FMT_U: wr = dinst->rd; break;
                case FMT_J: wr = dinst->rd; break;
            }

            if (wr != 0) {
                ctx->dead_temp_skip[i] = (read_count_before_write[wr] <= 1);
                read_count_before_write[wr] = 0;
            }

            if (r1 != 0) read_count_before_write[r1]++;
            if (r2 != 0) read_count_before_write[r2]++;
        }
    }

    // Select top-N guest registers for caching (only when reg_cache_enabled)
    if (ctx->reg_cache_enabled) {
        // 7 usable slots (slot 7 = A64_NOREG, skip)
        for (int s = 0; s < REG_ALLOC_SLOTS; s++) {
            if (reg_alloc_hosts[s] == A64_NOREG) break;
            int best = -1;
            uint32_t best_count = 0;
            for (int r = 1; r < 32; r++) {
                if (use_count[r] > best_count) {
                    best_count = use_count[r];
                    best = r;
                }
            }
            if (best < 0 || best_count == 0) break;

            ctx->reg_alloc[s].guest_reg = (uint8_t)best;
            ctx->reg_alloc[s].allocated = true;
            ctx->reg_alloc[s].dirty = false;
            ctx->reg_alloc_map[best] = (int8_t)s;
            use_count[best] = 0;  // Remove from consideration
        }
    }
}

// Emit prologue: load allocated guest registers into host callee-saved registers
static void reg_alloc_emit_prologue(translate_ctx_t *ctx) {
    emit_ctx_t *e = &ctx->emit;
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!ctx->reg_alloc[i].allocated) continue;
        // LDR Wd, [X20, #offset]
        emit_ldr_w32_imm(e, reg_alloc_hosts[i], W20,
                         GUEST_REG_OFFSET(ctx->reg_alloc[i].guest_reg));
    }
}

// Constant propagation helpers
static inline void const_prop_reset(translate_ctx_t *ctx) {
    for (int i = 0; i < 32; i++)
        ctx->reg_constants[i].valid = false;
    ctx->reg_constants[0].valid = true;
    ctx->reg_constants[0].value = 0;
}

// Bounds check elimination helpers
static inline void bounds_elim_reset(translate_ctx_t *ctx) {
    ctx->validated_range_count = 0;
}

static void bounds_elim_invalidate(translate_ctx_t *ctx, uint8_t guest_reg) {
    for (int i = 0; i < ctx->validated_range_count; i++) {
        if (ctx->validated_ranges[i].guest_reg == guest_reg) {
            ctx->validated_ranges[i] = ctx->validated_ranges[--ctx->validated_range_count];
            i--;
        }
    }
}

static int bounds_elim_lookup(translate_ctx_t *ctx, uint8_t base_reg,
                              int32_t offset, uint32_t access_size,
                              bool is_store) {
    if (base_reg == 0) return 0;
    uint32_t access_lo = (uint32_t)offset;
    uint32_t access_hi = (uint32_t)offset + access_size;
    for (int i = 0; i < ctx->validated_range_count; i++) {
        if (ctx->validated_ranges[i].guest_reg == base_reg) {
            if (access_lo >= ctx->validated_ranges[i].lo_offset &&
                access_hi <= ctx->validated_ranges[i].hi_end) {
                if (!is_store) return 2;
                if (ctx->validated_ranges[i].is_store_ok) return 2;
                return 1;
            }
        }
    }
    return 0;
}

static void bounds_elim_record(translate_ctx_t *ctx, uint8_t base_reg,
                               int32_t offset, uint32_t access_size,
                               bool is_store) {
    if (base_reg == 0) return;
    uint32_t new_lo = (uint32_t)offset;
    uint32_t new_hi = (uint32_t)offset + access_size;

    for (int i = 0; i < ctx->validated_range_count; i++) {
        if (ctx->validated_ranges[i].guest_reg == base_reg) {
            if (new_lo < ctx->validated_ranges[i].lo_offset)
                ctx->validated_ranges[i].lo_offset = new_lo;
            if (new_hi > ctx->validated_ranges[i].hi_end)
                ctx->validated_ranges[i].hi_end = new_hi;
            if (is_store)
                ctx->validated_ranges[i].is_store_ok = true;
            return;
        }
    }
    if (ctx->validated_range_count < MAX_VALIDATED_RANGES) {
        int idx = ctx->validated_range_count++;
        ctx->validated_ranges[idx].guest_reg = base_reg;
        ctx->validated_ranges[idx].lo_offset = new_lo;
        ctx->validated_ranges[idx].hi_end = new_hi;
        ctx->validated_ranges[idx].is_store_ok = is_store;
    }
}

static inline bool is_backedge_target(translate_ctx_t *ctx, uint32_t pc) {
    if (!ctx->has_backedge) return false;
    for (int i = 0; i < ctx->backedge_target_count; i++) {
        if (ctx->backedge_targets[i] == pc) return true;
    }
    return false;
}

// ============================================================================
// Pending write management (dead temporary elimination)
// ============================================================================

static inline void flush_pending_write(translate_ctx_t *ctx) {
    if (!ctx->pending_write.valid) return;
    // STR Wd, [X20, #offset]
    emit_str_w32_imm(&ctx->emit, ctx->pending_write.host_reg,
                     W20, GUEST_REG_OFFSET(ctx->pending_write.guest_reg));
    ctx->pending_write.valid = false;
    ctx->emit.rax_pending = false;
}

// Stage 1: No pending_cond (compare-branch fusion is Stage 2+)
static inline void flush_pending_cond(translate_ctx_t *ctx) {
    (void)ctx;
}

// Flush all dirty cached registers to memory, plus pending write/cond.
static void reg_cache_flush(translate_ctx_t *ctx) {
    flush_pending_cond(ctx);
    flush_pending_write(ctx);
    if (!ctx->reg_cache_enabled) return;
    emit_ctx_t *e = &ctx->emit;
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!ctx->reg_alloc[i].allocated || !ctx->reg_alloc[i].dirty) continue;
        // STR Wd, [X20, #offset]
        emit_str_w32_imm(e, reg_alloc_hosts[i], W20,
                         GUEST_REG_OFFSET(ctx->reg_alloc[i].guest_reg));
        // NOTE: Do NOT clear dirty here! Multiple code paths (branch taken/fall-through)
        // all call flush, but only one executes at runtime. Each path must emit its own.
    }
}

// Flush using an allocation snapshot (for deferred side exits).
static void reg_alloc_flush_snapshot(translate_ctx_t *ctx,
                                      const bool *dirty_snapshot,
                                      const bool *allocated_snapshot,
                                      const uint8_t *guest_reg_snapshot) {
    emit_ctx_t *e = &ctx->emit;
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!allocated_snapshot[i] || !dirty_snapshot[i]) continue;
        emit_str_w32_imm(e, reg_alloc_hosts[i], W20,
                         GUEST_REG_OFFSET(guest_reg_snapshot[i]));
    }
}

// Flush and evict specific host registers being repurposed as scratch.
static void flush_cached_host_regs(translate_ctx_t *ctx, a64_reg_t r1, a64_reg_t r2) {
    if (!ctx->reg_cache_enabled) return;
    emit_ctx_t *e = &ctx->emit;
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!ctx->reg_alloc[i].allocated) continue;
        if (reg_alloc_hosts[i] == r1 || reg_alloc_hosts[i] == r2) {
            if (ctx->reg_alloc[i].dirty) {
                emit_str_w32_imm(e, reg_alloc_hosts[i], W20,
                                 GUEST_REG_OFFSET(ctx->reg_alloc[i].guest_reg));
            }
            ctx->reg_alloc_map[ctx->reg_alloc[i].guest_reg] = -1;
            ctx->reg_alloc[i].allocated = false;
            ctx->reg_alloc[i].dirty = false;
        }
    }
}

// Emit trace control (stub — tracing infrastructure unchanged)
void dbt_set_emit_trace(bool enabled, uint32_t pc) {
    (void)enabled;
    (void)pc;
}

// ============================================================================
// Guest register load/store (with optional register caching)
// ============================================================================

// Load guest register into AArch64 register (handles r0 = 0)
void emit_load_guest_reg(translate_ctx_t *ctx, a64_reg_t dst, uint8_t guest_reg) {
    emit_ctx_t *e = &ctx->emit;

    if (guest_reg == 0) {
        // r0 is always 0
        if (ctx->pending_write.valid && dst == ctx->pending_write.host_reg) {
            flush_pending_write(ctx);
        }
        // MOVZ Wd, #0
        emit_movz_w32(e, dst, 0, 0);
        return;
    }

    // Dead temporary elimination: check pending write
    if (ctx->pending_write.valid) {
        if (guest_reg == ctx->pending_write.guest_reg) {
            // Value is already in pending.host_reg
            a64_reg_t preg = ctx->pending_write.host_reg;
            if (!ctx->pending_write.can_skip_store) {
                // Level 1: still need memory store for later reads
                emit_str_w32_imm(e, preg, W20, GUEST_REG_OFFSET(guest_reg));
            }
            ctx->pending_write.valid = false;
            ctx->emit.rax_pending = false;
            if (dst != preg) {
                emit_mov_w32_w32(e, dst, preg);
            }
            ctx->reg_cache_hits++;
            return;
        }
        if (dst == ctx->pending_write.host_reg) {
            // About to clobber the pending register — flush first
            flush_pending_write(ctx);
        }
    }

    // Register caching: check if guest_reg is in a cached host register
    if (ctx->reg_cache_enabled) {
        int8_t slot = ctx->reg_alloc_map[guest_reg];
        if (slot >= 0) {
            ctx->reg_cache_hits++;
            a64_reg_t host_reg = reg_alloc_hosts[slot];
            if (dst != host_reg) {
                emit_mov_w32_w32(e, dst, host_reg);
            }
            return;
        }
        ctx->reg_cache_misses++;
    }

    // Load from CPU state: LDR Wd, [X20, #offset]
    emit_ldr_w32_imm(e, dst, W20, GUEST_REG_OFFSET(guest_reg));
}

// Store AArch64 register to guest register (handles r0 = discard)
void emit_store_guest_reg(translate_ctx_t *ctx, uint8_t guest_reg, a64_reg_t src) {
    emit_ctx_t *e = &ctx->emit;
    if (guest_reg == 0) return;

    // Constant propagation: non-constant write invalidates known value
    ctx->reg_constants[guest_reg].valid = false;
    bounds_elim_invalidate(ctx, guest_reg);

    // Discard pending write for same register
    if (ctx->pending_write.valid && ctx->pending_write.guest_reg == guest_reg) {
        ctx->pending_write.valid = false;
        ctx->emit.rax_pending = false;
    }

    // Register caching: if allocated, move into host register and mark dirty
    if (ctx->reg_cache_enabled) {
        int8_t slot = ctx->reg_alloc_map[guest_reg];
        if (slot >= 0) {
            ctx->reg_cache_hits++;
            a64_reg_t host_reg = reg_alloc_hosts[slot];
            if (src != host_reg) {
                emit_mov_w32_w32(e, host_reg, src);
            }
            ctx->reg_alloc[slot].dirty = true;
            return;
        }
    }

    // Dead temporary elimination: defer W0 stores as pending writes
    if (src == W0) {
        flush_pending_write(ctx);
        ctx->pending_write.guest_reg = guest_reg;
        ctx->pending_write.host_reg = W0;
        ctx->pending_write.valid = true;
        ctx->emit.rax_pending = true;
        ctx->pending_write.can_skip_store = ctx->dead_temp_skip[ctx->current_inst_idx];
        return;
    }

    // Non-W0, non-cached stores — store immediately
    ctx->reg_cache_misses++;
    emit_str_w32_imm(e, src, W20, GUEST_REG_OFFSET(guest_reg));
}

// Store immediate to guest register
void emit_store_guest_reg_imm32(translate_ctx_t *ctx, uint8_t guest_reg, uint32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    if (guest_reg == 0) return;

    ctx->reg_constants[guest_reg].valid = true;
    ctx->reg_constants[guest_reg].value = imm;
    bounds_elim_invalidate(ctx, guest_reg);

    if (ctx->pending_write.valid && ctx->pending_write.guest_reg == guest_reg) {
        ctx->pending_write.valid = false;
        ctx->emit.rax_pending = false;
    }

    // Register caching: if allocated, load immediate into host register
    if (ctx->reg_cache_enabled) {
        int8_t slot = ctx->reg_alloc_map[guest_reg];
        if (slot >= 0) {
            ctx->reg_cache_hits++;
            emit_mov_w32_imm32(e, reg_alloc_hosts[slot], imm);
            ctx->reg_alloc[slot].dirty = true;
            return;
        }
        ctx->reg_cache_misses++;
    }

    // AArch64: no memory-immediate store — load imm into W1, then store
    // Use W1 to avoid clobbering pending W0
    emit_mov_w32_imm32(e, W1, imm);
    emit_str_w32_imm(e, W1, W20, GUEST_REG_OFFSET(guest_reg));
}

// ============================================================================
// Exit sequences
// ============================================================================

void emit_exit(translate_ctx_t *ctx, exit_reason_t reason, uint32_t next_pc) {
    emit_ctx_t *e = &ctx->emit;

    reg_cache_flush(ctx);

    // Store next PC: MOV W1, #next_pc; STR W1, [X20, #PC_OFFSET]
    emit_mov_w32_imm32(e, W1, next_pc);
    emit_str_w32_imm(e, W1, W20, CPU_PC_OFFSET);

    // Store exit reason: MOV W1, #reason; STR W1, [X20, #EXIT_REASON_OFFSET]
    emit_mov_w32_imm32(e, W1, (uint32_t)reason);
    emit_str_w32_imm(e, W1, W20, CPU_EXIT_REASON_OFFSET);

    // Return to dispatcher
    emit_ret_lr(e);
}

void emit_exit_with_info(translate_ctx_t *ctx, exit_reason_t reason,
                         uint32_t next_pc, uint32_t info) {
    emit_ctx_t *e = &ctx->emit;

    reg_cache_flush(ctx);

    emit_mov_w32_imm32(e, W1, next_pc);
    emit_str_w32_imm(e, W1, W20, CPU_PC_OFFSET);

    emit_mov_w32_imm32(e, W1, (uint32_t)reason);
    emit_str_w32_imm(e, W1, W20, CPU_EXIT_REASON_OFFSET);

    emit_mov_w32_imm32(e, W1, info);
    emit_str_w32_imm(e, W1, W20, CPU_EXIT_INFO_OFFSET);

    emit_ret_lr(e);
}

// Exit with info from register (e.g., fault address in W0)
static void emit_exit_with_info_reg(translate_ctx_t *ctx, exit_reason_t reason,
                                    uint32_t next_pc, a64_reg_t info_reg) {
    emit_ctx_t *e = &ctx->emit;

    reg_cache_flush(ctx);

    emit_mov_w32_imm32(e, W1, next_pc);
    emit_str_w32_imm(e, W1, W20, CPU_PC_OFFSET);

    emit_mov_w32_imm32(e, W1, (uint32_t)reason);
    emit_str_w32_imm(e, W1, W20, CPU_EXIT_REASON_OFFSET);

    // Store info register (may be W0 or other)
    emit_str_w32_imm(e, info_reg, W20, CPU_EXIT_INFO_OFFSET);

    emit_ret_lr(e);
}

// ============================================================================
// Chained exit (Stage 1: always return to dispatcher)
// ============================================================================

static void emit_exit_chained(translate_ctx_t *ctx, uint32_t target_pc, int exit_idx) {
    emit_ctx_t *e = &ctx->emit;
    (void)exit_idx;

    reg_cache_flush(ctx);

    // Store target PC
    emit_mov_w32_imm32(e, W1, target_pc);
    emit_str_w32_imm(e, W1, W20, CPU_PC_OFFSET);

    if (ctx->cache == NULL) {
        // Stage 1: always return to dispatcher
        emit_mov_w32_imm32(e, W1, EXIT_BRANCH);
        emit_str_w32_imm(e, W1, W20, CPU_EXIT_REASON_OFFSET);
        emit_ret_lr(e);
        return;
    }

    // Stage 2: chain to target if already translated
    translated_block_t *target = cache_lookup(ctx->cache, target_pc);

    if (target && target->host_code) {
        // Target already translated - emit direct B
        int64_t rel = (int64_t)((uint8_t *)target->host_code - emit_ptr(e));
        emit_b(e, (int32_t)rel);

        if (ctx->block && exit_idx < MAX_BLOCK_EXITS) {
            cache_record_exit(ctx->cache, ctx->block, exit_idx, target_pc,
                              emit_ptr(e) - 4);  // patch_site is the B instruction
            ctx->block->exits[exit_idx].chained = true;
        }
    } else {
        // Target not yet translated — jump to shared_branch_exit
        uint8_t *patch_site = emit_ptr(e);
        int64_t rel = (int64_t)(ctx->cache->shared_branch_exit - patch_site);
        emit_b(e, (int32_t)rel);

        if (ctx->block && exit_idx < MAX_BLOCK_EXITS) {
            cache_record_exit(ctx->cache, ctx->block, exit_idx, target_pc, patch_site);
            cache_record_pending_chain(ctx->cache, ctx->block, exit_idx, target_pc);
        }
    }
}

// ============================================================================
// Memory access helpers
// ============================================================================

// Compute guest address into W0: W0 = guest_regs[rs1] + imm
static void emit_compute_addr(translate_ctx_t *ctx, uint8_t rs1, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        flush_pending_write(ctx);
        emit_mov_w32_imm32(e, W0, (uint32_t)imm);
        return;
    }

    // Constant folding
    if (ctx->reg_constants[rs1].valid) {
        uint32_t addr = ctx->reg_constants[rs1].value + (uint32_t)imm;
        flush_pending_write(ctx);
        emit_mov_w32_imm32(e, W0, addr);
        return;
    }

    emit_load_guest_reg(ctx, W0, rs1);
    if (imm != 0) {
        if (imm > 0 && (uint32_t)imm < 4096) {
            emit_add_w32_imm(e, W0, W0, (uint32_t)imm);
        } else if (imm < 0 && (uint32_t)(-imm) < 4096) {
            emit_sub_w32_imm(e, W0, W0, (uint32_t)(-imm));
        } else {
            emit_mov_w32_imm32(e, W2, (uint32_t)imm);
            emit_add_w32(e, W0, W0, W2);
        }
    }
}

// Memory access check: bounds, alignment, MMIO, W^X
// addr_reg holds the guest address (typically W0)
static void emit_mem_access_check(translate_ctx_t *ctx, a64_reg_t addr_reg,
                                  uint32_t access_size,
                                  exit_reason_t reason, bool is_store,
                                  uint8_t base_guest_reg, int32_t offset) {
    emit_ctx_t *e = &ctx->emit;
    dbt_cpu_state_t *cpu = ctx->cpu;

    if (cpu->bounds_checks_disabled) return;

    int coverage = bounds_elim_lookup(ctx, base_guest_reg, offset, access_size, is_store);
    if (coverage == 2) return;
    bool skip_bounds = (coverage == 1);

    // On AArch64, we use forward branches to skip over fault paths.
    // Collect patch sites for B.cond instructions.
    size_t fault_patches[8];
    int fault_count = 0;
    size_t ok_patches[8];
    int ok_count = 0;

    if (cpu->mem_size < access_size) {
        emit_exit_with_info_reg(ctx, reason, ctx->guest_pc, addr_reg);
        return;
    }

    // 1. Bounds check
    if (!skip_bounds) {
        uint32_t limit = cpu->mem_size - access_size;
        // CMP addr, #limit; B.HI fault
        emit_mov_w32_imm32(e, W3, limit);
        emit_cmp_w32_w32(e, addr_reg, W3);
        fault_patches[fault_count++] = emit_offset(e);
        emit_b_cond(e, COND_HI, 0);  // Patch later

        // 2. Alignment check
        if (cpu->align_traps_enabled && access_size > 1) {
            // TST addr, #(access_size-1); B.NE fault
            emit_mov_w32_imm32(e, W3, access_size - 1);
            emit_tst_w32(e, addr_reg, W3);
            fault_patches[fault_count++] = emit_offset(e);
            emit_b_cond(e, COND_NE, 0);
        }

        // 3. MMIO disabled check
        if (!cpu->mmio_enabled && cpu->mmio_base != 0) {
            // if addr < mmio_base -> ok (skip MMIO check)
            emit_mov_w32_imm32(e, W3, cpu->mmio_base);
            emit_cmp_w32_w32(e, addr_reg, W3);
            ok_patches[ok_count++] = emit_offset(e);
            emit_b_cond(e, COND_LO, 0);  // addr < mmio_base -> ok

            // if addr < mmio_base + 0x10000 -> fault (in MMIO window)
            emit_mov_w32_imm32(e, W3, cpu->mmio_base + 0x10000);
            emit_cmp_w32_w32(e, addr_reg, W3);
            fault_patches[fault_count++] = emit_offset(e);
            emit_b_cond(e, COND_LO, 0);  // addr < mmio_end -> fault
        }
    }

    // 4. W^X check — stores only
    if (is_store && cpu->wxorx_enabled) {
        uint32_t wx_limit = cpu->rodata_limit ? cpu->rodata_limit : cpu->code_limit;
        if (wx_limit > 0) {
            emit_mov_w32_imm32(e, W3, wx_limit);
            emit_cmp_w32_w32(e, addr_reg, W3);
            fault_patches[fault_count++] = emit_offset(e);
            emit_b_cond(e, COND_LO, 0);  // addr < wx_limit -> fault
        }
    }

    if (fault_count == 0 && ok_count == 0) {
        if (!skip_bounds) {
            bounds_elim_record(ctx, base_guest_reg, offset, access_size, is_store);
        } else if (is_store) {
            bounds_elim_record(ctx, base_guest_reg, offset, access_size, true);
        }
        return;
    }

    // Success path: B over fault handler
    size_t ok_branch = emit_offset(e);
    emit_b(e, 0);  // Patch later

    // Fault path
    size_t fault_offset = emit_offset(e);
    if (addr_reg != W0) {
        emit_mov_w32_w32(e, W0, addr_reg);
    }
    emit_exit_with_info_reg(ctx, reason, ctx->guest_pc, W0);

    // Patch all branches
    size_t ok_offset = emit_offset(e);

    // Patch ok_branch (unconditional B)
    {
        int32_t rel = (int32_t)(ok_offset - ok_branch);
        emit_patch_rel32(e, ok_branch, ok_offset);
    }

    // Patch fault branches (B.cond)
    for (int i = 0; i < fault_count; i++) {
        size_t patch = fault_patches[i];
        int32_t rel = (int32_t)(fault_offset - patch);
        // Patch B.cond: bits [23:5] = imm19 = rel/4
        uint32_t *inst = (uint32_t *)(e->buf + patch);
        int32_t imm19 = rel >> 2;
        *inst = (*inst & ~(0x7FFFF << 5)) | ((imm19 & 0x7FFFF) << 5);
    }

    // Patch ok branches (B.cond)
    for (int i = 0; i < ok_count; i++) {
        size_t patch = ok_patches[i];
        int32_t rel = (int32_t)(ok_offset - patch);
        uint32_t *inst = (uint32_t *)(e->buf + patch);
        int32_t imm19 = rel >> 2;
        *inst = (*inst & ~(0x7FFFF << 5)) | ((imm19 & 0x7FFFF) << 5);
    }

    // Record validated range
    if (!skip_bounds) {
        bounds_elim_record(ctx, base_guest_reg, offset, access_size, is_store);
    } else if (is_store) {
        bounds_elim_record(ctx, base_guest_reg, offset, access_size, true);
    }
}

// ============================================================================
// 3-operand register resolution helper
// Returns the host register holding guest_reg:
//   - If cached, returns the cached host register (zero-cost)
//   - Otherwise, loads into the given scratch register and returns scratch
// ============================================================================

static inline a64_reg_t resolve_src(translate_ctx_t *ctx, uint8_t guest_reg, a64_reg_t scratch) {
    a64_reg_t h = guest_host_reg(ctx, guest_reg);
    if (h != A64_NOREG) return h;
    emit_load_guest_reg(ctx, scratch, guest_reg);
    return scratch;
}

// ============================================================================
// Arithmetic translations
// ============================================================================

void translate_add(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // Constant folding
    if (ctx->reg_constants[rs1].valid && ctx->reg_constants[rs2].valid) {
        uint32_t result = ctx->reg_constants[rs1].value + ctx->reg_constants[rs2].value;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    a64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != A64_NOREG) {
        // 3-operand cached path: ADD hd, src1, src2
        a64_reg_t s1 = resolve_src(ctx, rs1, W0);
        if (rs2 == 0) {
            if (s1 != hd) emit_mov_w32_w32(e, hd, s1);
        } else {
            a64_reg_t s2 = resolve_src(ctx, rs2, W1);
            emit_add_w32(e, hd, s1, s2);
        }
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_load_guest_reg(ctx, W0, rs1);
        if (rs2 != 0) {
            a64_reg_t s2 = resolve_src(ctx, rs2, W1);
            emit_add_w32(e, W0, W0, s2);
        }
        emit_store_guest_reg(ctx, rd, W0);
    }
}

void translate_sub(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (ctx->reg_constants[rs1].valid && ctx->reg_constants[rs2].valid) {
        uint32_t result = ctx->reg_constants[rs1].value - ctx->reg_constants[rs2].value;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    a64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != A64_NOREG) {
        a64_reg_t s1 = resolve_src(ctx, rs1, W0);
        if (rs2 == 0) {
            if (s1 != hd) emit_mov_w32_w32(e, hd, s1);
        } else {
            a64_reg_t s2 = resolve_src(ctx, rs2, W1);
            emit_sub_w32(e, hd, s1, s2);
        }
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_load_guest_reg(ctx, W0, rs1);
        if (rs2 != 0) {
            a64_reg_t s2 = resolve_src(ctx, rs2, W1);
            emit_sub_w32(e, W0, W0, s2);
        }
        emit_store_guest_reg(ctx, rd, W0);
    }
}

void translate_addi(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, (uint32_t)imm);
        return;
    }

    if (ctx->reg_constants[rs1].valid) {
        uint32_t result = ctx->reg_constants[rs1].value + (uint32_t)imm;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    a64_reg_t hd = guest_host_reg(ctx, rd);
    a64_reg_t dst = (hd != A64_NOREG) ? hd : W0;
    a64_reg_t s1 = resolve_src(ctx, rs1, W0);

    if (imm == 0) {
        if (s1 != dst) emit_mov_w32_w32(e, dst, s1);
    } else if (imm > 0 && (uint32_t)imm < 4096) {
        emit_add_w32_imm(e, dst, s1, (uint32_t)imm);
    } else if (imm < 0 && (uint32_t)(-imm) < 4096) {
        emit_sub_w32_imm(e, dst, s1, (uint32_t)(-imm));
    } else {
        emit_mov_w32_imm32(e, W1, (uint32_t)imm);
        emit_add_w32(e, dst, s1, W1);
    }

    if (hd != A64_NOREG) {
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_store_guest_reg(ctx, rd, W0);
    }
}

// ============================================================================
// Logical translations
// ============================================================================

void translate_and(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (ctx->reg_constants[rs1].valid && ctx->reg_constants[rs2].valid) {
        emit_store_guest_reg_imm32(ctx, rd, ctx->reg_constants[rs1].value & ctx->reg_constants[rs2].value);
        return;
    }

    {
        a64_reg_t hd = guest_host_reg(ctx, rd);
        if (hd != A64_NOREG) {
            a64_reg_t s1 = resolve_src(ctx, rs1, W0);
            a64_reg_t s2 = resolve_src(ctx, rs2, W1);
            emit_and_w32(e, hd, s1, s2);
            reg_cache_mark_written(ctx, rd);
        } else {
            emit_load_guest_reg(ctx, W0, rs1);
            a64_reg_t s2 = resolve_src(ctx, rs2, W1);
            emit_and_w32(e, W0, W0, s2);
            emit_store_guest_reg(ctx, rd, W0);
        }
    }
}

void translate_or(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (ctx->reg_constants[rs1].valid && ctx->reg_constants[rs2].valid) {
        emit_store_guest_reg_imm32(ctx, rd, ctx->reg_constants[rs1].value | ctx->reg_constants[rs2].value);
        return;
    }

    a64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != A64_NOREG) {
        a64_reg_t s1 = resolve_src(ctx, rs1, W0);
        if (rs2 == 0) {
            if (s1 != hd) emit_mov_w32_w32(e, hd, s1);
        } else {
            a64_reg_t s2 = resolve_src(ctx, rs2, W1);
            emit_orr_w32(e, hd, s1, s2);
        }
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_load_guest_reg(ctx, W0, rs1);
        if (rs2 != 0) {
            a64_reg_t s2 = resolve_src(ctx, rs2, W1);
            emit_orr_w32(e, W0, W0, s2);
        }
        emit_store_guest_reg(ctx, rd, W0);
    }
}

void translate_xor(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (ctx->reg_constants[rs1].valid && ctx->reg_constants[rs2].valid) {
        emit_store_guest_reg_imm32(ctx, rd, ctx->reg_constants[rs1].value ^ ctx->reg_constants[rs2].value);
        return;
    }

    a64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != A64_NOREG) {
        a64_reg_t s1 = resolve_src(ctx, rs1, W0);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_eor_w32(e, hd, s1, s2);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_load_guest_reg(ctx, W0, rs1);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_eor_w32(e, W0, W0, s2);
        emit_store_guest_reg(ctx, rd, W0);
    }
}

void translate_andi(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, 0);
        return;
    }

    if (ctx->reg_constants[rs1].valid) {
        emit_store_guest_reg_imm32(ctx, rd, ctx->reg_constants[rs1].value & (uint32_t)imm);
        return;
    }

    a64_reg_t hd = guest_host_reg(ctx, rd);
    a64_reg_t dst = (hd != A64_NOREG) ? hd : W0;
    a64_reg_t s1 = resolve_src(ctx, rs1, W0);
    if (!emit_and_w32_imm(e, dst, s1, (uint32_t)imm)) {
        emit_mov_w32_imm32(e, W1, (uint32_t)imm);
        emit_and_w32(e, dst, s1, W1);
    }
    if (hd != A64_NOREG) reg_cache_mark_written(ctx, rd);
    else emit_store_guest_reg(ctx, rd, W0);
}

void translate_ori(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, (uint32_t)imm);
        return;
    }

    if (ctx->reg_constants[rs1].valid) {
        emit_store_guest_reg_imm32(ctx, rd, ctx->reg_constants[rs1].value | (uint32_t)imm);
        return;
    }

    a64_reg_t hd = guest_host_reg(ctx, rd);
    a64_reg_t dst = (hd != A64_NOREG) ? hd : W0;
    a64_reg_t s1 = resolve_src(ctx, rs1, W0);
    if (imm == 0) {
        if (s1 != dst) emit_mov_w32_w32(e, dst, s1);
    } else if (!emit_orr_w32_imm(e, dst, s1, (uint32_t)imm)) {
        emit_mov_w32_imm32(e, W1, (uint32_t)imm);
        emit_orr_w32(e, dst, s1, W1);
    }
    if (hd != A64_NOREG) reg_cache_mark_written(ctx, rd);
    else emit_store_guest_reg(ctx, rd, W0);
}

void translate_xori(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, (uint32_t)imm);
        return;
    }

    if (ctx->reg_constants[rs1].valid) {
        emit_store_guest_reg_imm32(ctx, rd, ctx->reg_constants[rs1].value ^ (uint32_t)imm);
        return;
    }

    a64_reg_t hd = guest_host_reg(ctx, rd);
    a64_reg_t dst = (hd != A64_NOREG) ? hd : W0;
    a64_reg_t s1 = resolve_src(ctx, rs1, W0);
    if (imm == 0) {
        if (s1 != dst) emit_mov_w32_w32(e, dst, s1);
    } else if (!emit_eor_w32_imm(e, dst, s1, (uint32_t)imm)) {
        emit_mov_w32_imm32(e, W1, (uint32_t)imm);
        emit_eor_w32(e, dst, s1, W1);
    }
    if (hd != A64_NOREG) reg_cache_mark_written(ctx, rd);
    else emit_store_guest_reg(ctx, rd, W0);
}

// ============================================================================
// Shift translations
// ============================================================================

void translate_sll(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    a64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != A64_NOREG) {
        a64_reg_t s1 = resolve_src(ctx, rs1, W0);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_lslv_w32(e, hd, s1, s2);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_load_guest_reg(ctx, W0, rs1);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_lslv_w32(e, W0, W0, s2);
        emit_store_guest_reg(ctx, rd, W0);
    }
}

void translate_srl(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    a64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != A64_NOREG) {
        a64_reg_t s1 = resolve_src(ctx, rs1, W0);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_lsrv_w32(e, hd, s1, s2);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_load_guest_reg(ctx, W0, rs1);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_lsrv_w32(e, W0, W0, s2);
        emit_store_guest_reg(ctx, rd, W0);
    }
}

void translate_sra(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    a64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != A64_NOREG) {
        a64_reg_t s1 = resolve_src(ctx, rs1, W0);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_asrv_w32(e, hd, s1, s2);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_load_guest_reg(ctx, W0, rs1);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_asrv_w32(e, W0, W0, s2);
        emit_store_guest_reg(ctx, rd, W0);
    }
}

void translate_slli(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (ctx->reg_constants[rs1].valid) {
        emit_store_guest_reg_imm32(ctx, rd, ctx->reg_constants[rs1].value << (imm & 31));
        return;
    }

    a64_reg_t hd = guest_host_reg(ctx, rd);
    a64_reg_t dst = (hd != A64_NOREG) ? hd : W0;
    a64_reg_t s1 = resolve_src(ctx, rs1, W0);
    if ((imm & 31) != 0) {
        emit_lsl_w32_imm(e, dst, s1, imm & 31);
    } else if (s1 != dst) {
        emit_mov_w32_w32(e, dst, s1);
    }
    if (hd != A64_NOREG) reg_cache_mark_written(ctx, rd);
    else emit_store_guest_reg(ctx, rd, W0);
}

void translate_srli(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (ctx->reg_constants[rs1].valid) {
        emit_store_guest_reg_imm32(ctx, rd, ctx->reg_constants[rs1].value >> (imm & 31));
        return;
    }

    a64_reg_t hd = guest_host_reg(ctx, rd);
    a64_reg_t dst = (hd != A64_NOREG) ? hd : W0;
    a64_reg_t s1 = resolve_src(ctx, rs1, W0);
    if ((imm & 31) != 0) {
        emit_lsr_w32_imm(e, dst, s1, imm & 31);
    } else if (s1 != dst) {
        emit_mov_w32_w32(e, dst, s1);
    }
    if (hd != A64_NOREG) reg_cache_mark_written(ctx, rd);
    else emit_store_guest_reg(ctx, rd, W0);
}

void translate_srai(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (ctx->reg_constants[rs1].valid) {
        emit_store_guest_reg_imm32(ctx, rd, (uint32_t)((int32_t)ctx->reg_constants[rs1].value >> (imm & 31)));
        return;
    }

    a64_reg_t hd = guest_host_reg(ctx, rd);
    a64_reg_t dst = (hd != A64_NOREG) ? hd : W0;
    a64_reg_t s1 = resolve_src(ctx, rs1, W0);
    if ((imm & 31) != 0) {
        emit_asr_w32_imm(e, dst, s1, imm & 31);
    } else if (s1 != dst) {
        emit_mov_w32_w32(e, dst, s1);
    }
    if (hd != A64_NOREG) reg_cache_mark_written(ctx, rd);
    else emit_store_guest_reg(ctx, rd, W0);
}

// ============================================================================
// Comparison translations
// AArch64: CMP + CSET replaces x86 CMP + SETcc + MOVZX
// ============================================================================

static a64_cond_t opcode_to_cond(uint8_t opcode) {
    switch (opcode) {
        case OP_SLT:  case OP_SLTI:  return COND_LT;
        case OP_SLTU: case OP_SLTIU: return COND_LO;  // unsigned <
        case OP_SEQ:  return COND_EQ;
        case OP_SNE:  return COND_NE;
        case OP_SGT:  return COND_GT;
        case OP_SGTU: return COND_HI;  // unsigned >
        case OP_SLE:  return COND_LE;
        case OP_SLEU: return COND_LS;  // unsigned <=
        case OP_SGE:  return COND_GE;
        case OP_SGEU: return COND_HS;  // unsigned >=
        default:      return COND_AL;
    }
}

static void translate_cmp_set(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1,
                              uint8_t rs2, uint8_t opcode) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // Constant folding
    if (ctx->reg_constants[rs1].valid && ctx->reg_constants[rs2].valid) {
        int32_t v1 = (int32_t)ctx->reg_constants[rs1].value;
        int32_t v2 = (int32_t)ctx->reg_constants[rs2].value;
        uint32_t u1 = ctx->reg_constants[rs1].value;
        uint32_t u2 = ctx->reg_constants[rs2].value;
        uint32_t result = 0;
        switch (opcode) {
            case OP_SLT:  result = (v1 < v2) ? 1 : 0; break;
            case OP_SLTU: result = (u1 < u2) ? 1 : 0; break;
            case OP_SEQ:  result = (u1 == u2) ? 1 : 0; break;
            case OP_SNE:  result = (u1 != u2) ? 1 : 0; break;
            case OP_SGT:  result = (v1 > v2) ? 1 : 0; break;
            case OP_SGTU: result = (u1 > u2) ? 1 : 0; break;
            case OP_SLE:  result = (v1 <= v2) ? 1 : 0; break;
            case OP_SLEU: result = (u1 <= u2) ? 1 : 0; break;
            case OP_SGE:  result = (v1 >= v2) ? 1 : 0; break;
            case OP_SGEU: result = (u1 >= u2) ? 1 : 0; break;
        }
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    a64_cond_t cond = opcode_to_cond(opcode);
    a64_reg_t s1 = resolve_src(ctx, rs1, W0);
    a64_reg_t s2 = resolve_src(ctx, rs2, W1);
    emit_cmp_w32_w32(e, s1, s2);
    a64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != A64_NOREG) {
        emit_cset_w32(e, hd, cond);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_cset_w32(e, W0, cond);
        emit_store_guest_reg(ctx, rd, W0);
    }
}

void translate_slt(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, OP_SLT);
}
void translate_sltu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, OP_SLTU);
}
void translate_seq(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, OP_SEQ);
}
void translate_sne(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, OP_SNE);
}
void translate_sgt(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, OP_SGT);
}
void translate_sgtu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, OP_SGTU);
}
void translate_sle(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, OP_SLE);
}
void translate_sleu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, OP_SLEU);
}
void translate_sge(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, OP_SGE);
}
void translate_sgeu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, OP_SGEU);
}

void translate_slti(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (ctx->reg_constants[rs1].valid) {
        uint32_t result = ((int32_t)ctx->reg_constants[rs1].value < imm) ? 1 : 0;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    {
        a64_reg_t s1 = resolve_src(ctx, rs1, W0);
        if (imm >= 0 && (uint32_t)imm < 4096) {
            emit_cmp_w32_imm(e, s1, (uint32_t)imm);
        } else if (imm < 0 && (uint32_t)(-imm) < 4096) {
            emit_cmn_w32_imm(e, s1, (uint32_t)(-imm));
        } else {
            emit_mov_w32_imm32(e, W1, (uint32_t)imm);
            emit_cmp_w32_w32(e, s1, W1);
        }
        a64_reg_t hd = guest_host_reg(ctx, rd);
        if (hd != A64_NOREG) {
            emit_cset_w32(e, hd, COND_LT);
            reg_cache_mark_written(ctx, rd);
        } else {
            emit_cset_w32(e, W0, COND_LT);
            emit_store_guest_reg(ctx, rd, W0);
        }
    }
}

void translate_sltiu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (ctx->reg_constants[rs1].valid) {
        uint32_t result = (ctx->reg_constants[rs1].value < (uint32_t)imm) ? 1 : 0;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    {
        a64_reg_t s1 = resolve_src(ctx, rs1, W0);
        if ((uint32_t)imm < 4096) {
            emit_cmp_w32_imm(e, s1, (uint32_t)imm);
        } else {
            emit_mov_w32_imm32(e, W1, (uint32_t)imm);
            emit_cmp_w32_w32(e, s1, W1);
        }
        a64_reg_t hd = guest_host_reg(ctx, rd);
        if (hd != A64_NOREG) {
            emit_cset_w32(e, hd, COND_LO);
            reg_cache_mark_written(ctx, rd);
        } else {
            emit_cset_w32(e, W0, COND_LO);
            emit_store_guest_reg(ctx, rd, W0);
        }
    }
}

// ============================================================================
// Multiply/Divide translations
// AArch64 advantages: 3-operand MUL/SDIV/UDIV, no implicit registers,
// no INT32_MIN/-1 guard needed (SDIV returns INT32_MIN on AArch64)
// ============================================================================

void translate_mul(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    a64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != A64_NOREG) {
        a64_reg_t s1 = resolve_src(ctx, rs1, W0);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_mul_w32(e, hd, s1, s2);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_load_guest_reg(ctx, W0, rs1);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_mul_w32(e, W0, W0, s2);
        emit_store_guest_reg(ctx, rd, W0);
    }
}

void translate_mulh(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // SMULL uses X-register result, must go through W0/W1 scratch
    emit_load_guest_reg(ctx, W0, rs1);
    emit_load_guest_reg(ctx, W1, rs2);
    emit_smull(e, W0, W0, W1);
    emit_asr_x64_imm(e, W0, W0, 32);
    emit_store_guest_reg(ctx, rd, W0);
}

// Helper to emit 64-bit logical shift right (LSR Xd, Xn, #shift)
static void emit_lsr_x64_imm(emit_ctx_t *e, a64_reg_t rd, a64_reg_t rn, uint32_t shift) {
    // UBFM Xd, Xn, #shift, #63
    uint32_t inst = 0xD340FC00 | ((shift & 0x3F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
    emit_inst(e, inst);
}

void translate_mulhu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // UMULL uses X-register result, must go through scratch
    emit_load_guest_reg(ctx, W0, rs1);
    emit_load_guest_reg(ctx, W1, rs2);
    emit_umull(e, W0, W0, W1);
    emit_lsr_x64_imm(e, W0, W0, 32);
    emit_store_guest_reg(ctx, rd, W0);
}

// ============================================================================
// Divide/Remainder translations
// AArch64 SDIV/UDIV are direct — no INT32_MIN/-1 guard needed
// ============================================================================

void translate_div(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    a64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != A64_NOREG) {
        a64_reg_t s1 = resolve_src(ctx, rs1, W0);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_sdiv_w32(e, hd, s1, s2);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_load_guest_reg(ctx, W0, rs1);
        a64_reg_t s2 = resolve_src(ctx, rs2, W1);
        emit_sdiv_w32(e, W0, W0, s2);
        emit_store_guest_reg(ctx, rd, W0);
    }
}

void translate_rem(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // REM = rs1 - (rs1/rs2)*rs2 = SDIV + MSUB
    // Uses W0/W1/W2 as scratch since MSUB needs 4 operands
    emit_load_guest_reg(ctx, W0, rs1);
    a64_reg_t s2 = resolve_src(ctx, rs2, W1);
    emit_sdiv_w32(e, W2, W0, s2);
    emit_msub_w32(e, W0, W2, s2, W0);
    emit_store_guest_reg(ctx, rd, W0);
}

// DIVU not defined as a separate opcode in SLOW-32 (no OP_DIVU),
// but included for completeness if needed later.

// ============================================================================
// Upper immediate
// ============================================================================

void translate_lui(translate_ctx_t *ctx, uint8_t rd, int32_t imm) {
    if (rd == 0) return;
    emit_store_guest_reg_imm32(ctx, rd, (uint32_t)imm);
}

// ============================================================================
// Memory translations
// Guest memory: mem_base (X21) + guest_addr (Wn, zero-extended via UXTW)
// ============================================================================

void translate_ldw(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    emit_compute_addr(ctx, rs1, imm);  // Result in W0
    emit_mem_access_check(ctx, W0, 4, EXIT_FAULT_LOAD, false, base_reg, imm);
    // LDR W1, [X21, W0, UXTW]
    emit_ldr_w32_reg_uxtw(e, W1, W21, W0);
    emit_store_guest_reg(ctx, rd, W1);
}

void translate_ldh(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, W0, 2, EXIT_FAULT_LOAD, false, base_reg, imm);
    // LDRSH W1, [X21, W0, UXTW] — sign-extend halfword
    emit_ldrsh_w32_reg_uxtw(e, W1, W21, W0);
    emit_store_guest_reg(ctx, rd, W1);
}

void translate_ldb(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, W0, 1, EXIT_FAULT_LOAD, false, base_reg, imm);
    // LDRSB W1, [X21, W0, UXTW] — sign-extend byte
    emit_ldrsb_w32_reg_uxtw(e, W1, W21, W0);
    emit_store_guest_reg(ctx, rd, W1);
}

void translate_ldhu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, W0, 2, EXIT_FAULT_LOAD, false, base_reg, imm);
    // LDRH W1, [X21, W0, UXTW] — zero-extend halfword
    emit_ldrh_reg_uxtw(e, W1, W21, W0);
    emit_store_guest_reg(ctx, rd, W1);
}

void translate_ldbu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, W0, 1, EXIT_FAULT_LOAD, false, base_reg, imm);
    // LDRB W1, [X21, W0, UXTW] — zero-extend byte
    emit_ldrb_reg_uxtw(e, W1, W21, W0);
    emit_store_guest_reg(ctx, rd, W1);
}

void translate_stw(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    emit_compute_addr(ctx, rs1, imm);  // W0 = address
    emit_mem_access_check(ctx, W0, 4, EXIT_FAULT_STORE, true, base_reg, imm);
    // Load value to store into W1
    emit_load_guest_reg(ctx, W1, rs2);
    // STR W1, [X21, W0, UXTW]
    emit_str_w32_reg_uxtw(e, W1, W21, W0);
}

void translate_sth(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, W0, 2, EXIT_FAULT_STORE, true, base_reg, imm);
    emit_load_guest_reg(ctx, W1, rs2);
    emit_strh_reg_uxtw(e, W1, W21, W0);
}

void translate_stb(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, W0, 1, EXIT_FAULT_STORE, true, base_reg, imm);
    emit_load_guest_reg(ctx, W1, rs2);
    emit_strb_reg_uxtw(e, W1, W21, W0);
}

// ============================================================================
// Control flow translations
// ============================================================================

void translate_jal(translate_ctx_t *ctx, uint8_t rd, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint32_t return_pc = ctx->guest_pc + 4;
    uint32_t target_pc = ctx->guest_pc + imm;

    // Store return address
    if (rd != 0) {
        emit_store_guest_reg_imm32(ctx, rd, return_pc);
    }

    // Exit with branch to target (chainable in Stage 2)
    emit_exit_chained(ctx, target_pc, ctx->exit_idx++);
}

void translate_jalr(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint32_t return_pc = ctx->guest_pc + 4;

    // Calculate target: (rs1 + imm) & ~1
    emit_load_guest_reg(ctx, W0, rs1);
    if (imm != 0) {
        if (imm > 0 && (uint32_t)imm < 4096) {
            emit_add_w32_imm(e, W0, W0, (uint32_t)imm);
        } else if (imm < 0 && (uint32_t)(-imm) < 4096) {
            emit_sub_w32_imm(e, W0, W0, (uint32_t)(-imm));
        } else {
            emit_mov_w32_imm32(e, W2, (uint32_t)imm);
            emit_add_w32(e, W0, W0, W2);
        }
    }
    // AND W0, W0, #~1 (clear low bit)
    if (!emit_and_w32_imm(e, W0, W0, ~1u)) {
        emit_mov_w32_imm32(e, W2, ~1u);
        emit_and_w32(e, W0, W0, W2);
    }

    // Store return address (use W2 to preserve W0=target)
    if (rd != 0) {
        a64_reg_t hd = guest_host_reg(ctx, rd);
        if (hd != A64_NOREG) {
            emit_mov_w32_imm32(e, hd, return_pc);
            ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        } else {
            emit_mov_w32_imm32(e, W2, return_pc);
            emit_str_w32_imm(e, W2, W20, GUEST_REG_OFFSET(rd));
        }
        ctx->reg_constants[rd].valid = false;
        bounds_elim_invalidate(ctx, rd);
    }

    // Store target PC and exit
    reg_cache_flush(ctx);
    emit_str_w32_imm(e, W0, W20, CPU_PC_OFFSET);
    emit_mov_w32_imm32(e, W1, EXIT_INDIRECT);
    emit_str_w32_imm(e, W1, W20, CPU_EXIT_REASON_OFFSET);
    emit_ret_lr(e);
}

// ============================================================================
// Branch translations
// ============================================================================

static bool translate_branch_common(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2,
                                    int32_t imm, a64_cond_t cond) {
    emit_ctx_t *e = &ctx->emit;
    uint32_t fall_pc = ctx->guest_pc + 4;
    uint32_t taken_pc = fall_pc + imm;  // PC+4 + imm

    // Compare rs1, rs2 (use cached regs if available)
    a64_reg_t s1 = resolve_src(ctx, rs1, W0);
    a64_reg_t s2 = resolve_src(ctx, rs2, W1);
    emit_cmp_w32_w32(e, s1, s2);

    // B.cond to taken path (jump over not-taken exit)
    size_t bcond_patch = emit_offset(e);
    emit_b_cond(e, cond, 0);  // Patch later

    // Not-taken path: exit with fall_pc
    emit_exit_chained(ctx, fall_pc, ctx->exit_idx++);

    // Taken path: patch B.cond to here
    size_t taken_offset = emit_offset(e);
    {
        int32_t rel = (int32_t)(taken_offset - bcond_patch);
        uint32_t *inst = (uint32_t *)(e->buf + bcond_patch);
        int32_t imm19 = rel >> 2;
        *inst = (*inst & ~(0x7FFFF << 5)) | ((imm19 & 0x7FFFF) << 5);
    }

    // Exit with taken_pc
    emit_exit_chained(ctx, taken_pc, ctx->exit_idx++);

    return true;  // Block always ends at branches in Stage 1
}

bool translate_beq(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, COND_EQ);
}
bool translate_bne(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, COND_NE);
}
bool translate_blt(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, COND_LT);
}
bool translate_bge(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, COND_GE);
}
bool translate_bltu(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, COND_LO);
}
bool translate_bgeu(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, COND_HS);
}

// ============================================================================
// Special instructions
// ============================================================================

void translate_nop(translate_ctx_t *ctx) {
    (void)ctx;
}

void translate_halt(translate_ctx_t *ctx) {
    emit_exit(ctx, EXIT_HALT, ctx->guest_pc + 4);
}

void translate_debug(translate_ctx_t *ctx, uint8_t rs1) {
    emit_ctx_t *e = &ctx->emit;

    // Load character to output
    emit_load_guest_reg(ctx, W0, rs1);
    emit_str_w32_imm(e, W0, W20, CPU_EXIT_INFO_OFFSET);

    emit_exit(ctx, EXIT_DEBUG, ctx->guest_pc + 4);
}

void translate_yield(translate_ctx_t *ctx) {
    emit_exit(ctx, EXIT_YIELD, ctx->guest_pc + 4);
}

void translate_assert_eq(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2) {
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, W0, rs1);
    emit_load_guest_reg(ctx, W1, rs2);
    emit_cmp_w32_w32(e, W0, W1);

    // B.EQ over fail path
    size_t beq_patch = emit_offset(e);
    emit_b_cond(e, COND_EQ, 0);

    // Fail path
    emit_exit(ctx, EXIT_ASSERT_FAIL, ctx->guest_pc);

    // Success: patch B.EQ to here
    size_t success_offset = emit_offset(e);
    {
        int32_t rel = (int32_t)(success_offset - beq_patch);
        uint32_t *inst = (uint32_t *)(e->buf + beq_patch);
        int32_t imm19 = rel >> 2;
        *inst = (*inst & ~(0x7FFFF << 5)) | ((imm19 & 0x7FFFF) << 5);
    }
}

// ============================================================================
// Floating-point (helper call fallback — all FP ops exit to dispatcher)
// ============================================================================

// FP helper function prototype (called from C, not from JIT code)
// On AArch64 Stage 1, we just exit to the interpreter for FP ops.
void translate_fp_r_type(translate_ctx_t *ctx, uint8_t opcode, uint8_t rd,
                         uint8_t rs1, uint8_t rs2) {
    (void)opcode; (void)rd; (void)rs1; (void)rs2;
    // For Stage 1, exit to interpreter to handle FP
    // The dispatcher will fall back to interpretation for this instruction
    emit_exit(ctx, EXIT_HALT, ctx->guest_pc);
}

// ============================================================================
// Main translation function
// ============================================================================

translated_block_fn translate_block(translate_ctx_t *ctx) {
    dbt_cpu_state_t *cpu = ctx->cpu;
    emit_ctx_t *e = &ctx->emit;

    // Reset emitter
    emit_init(e, cpu->code_buffer, cpu->code_buffer_size);

    // Save entry point
    void *entry = emit_ptr(e);

    ctx->guest_pc = cpu->pc;
    ctx->block_start_pc = cpu->pc;
    ctx->inst_count = 0;
    ctx->side_exit_emitted = 0;
    ctx->deferred_exit_count = 0;
    ctx->pc_map_count = 0;
    ctx->pending_write.valid = false;
    ctx->emit.rax_pending = false;
    ctx->pending_cond.valid = false;
    ctx->current_inst_idx = 0;
    ctx->has_backedge = false;
    ctx->backedge_target_pc = 0;
    ctx->backedge_target_count = 0;
    ctx->exit_idx = 0;
    memset(ctx->side_exit_pcs, 0, sizeof(ctx->side_exit_pcs));
    memset(ctx->dead_temp_skip, 0, sizeof(ctx->dead_temp_skip));
    reg_alloc_reset(ctx);
    const_prop_reset(ctx);
    bounds_elim_reset(ctx);

    // Prescan for dead temporary elimination
    reg_alloc_prescan(ctx, cpu->pc);

    if (DBT_TRACE) {
        fprintf(stderr, "DBT: Translating block at PC=0x%08X (AArch64)\n", cpu->pc);
    }

    while (ctx->inst_count < MAX_BLOCK_INSTS) {
        if (ctx->guest_pc >= cpu->code_limit ||
            (cpu->code_limit - ctx->guest_pc) < 4) {
            emit_exit_with_info(ctx, EXIT_FAULT_FETCH, ctx->guest_pc, ctx->guest_pc);
            goto block_done;
        }
        if (cpu->align_traps_enabled && (ctx->guest_pc & 3u) != 0) {
            emit_exit_with_info(ctx, EXIT_FAULT_FETCH, ctx->guest_pc, ctx->guest_pc);
            goto block_done;
        }

        // Flush pending writes at back-edge targets
        if (is_backedge_target(ctx, ctx->guest_pc)) {
            flush_pending_write(ctx);
            const_prop_reset(ctx);
            bounds_elim_reset(ctx);
        }

        // Record guest PC → host offset
        if (ctx->pc_map_count < MAX_BLOCK_INSTS) {
            ctx->pc_map[ctx->pc_map_count].guest_pc = ctx->guest_pc;
            ctx->pc_map[ctx->pc_map_count].host_offset = emit_offset(e);
            ctx->pc_map_count++;
        }

        // Fetch and decode
        uint32_t raw = *(uint32_t *)(cpu->mem_base + ctx->guest_pc);
        decoded_inst_t inst = decode_instruction(raw);

        if (DBT_TRACE) {
            fprintf(stderr, "  [%d] PC=0x%08X: raw=0x%08X op=0x%02X rd=%d rs1=%d rs2=%d imm=%d\n",
                    ctx->inst_count, ctx->guest_pc, raw, inst.opcode,
                    inst.rd, inst.rs1, inst.rs2, inst.imm);
        }

        ctx->current_inst_idx = ctx->inst_count;

        // Translate based on opcode
        switch (inst.opcode) {
            // Arithmetic
            case OP_ADD:  translate_add(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SUB:  translate_sub(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_ADDI: translate_addi(ctx, inst.rd, inst.rs1, inst.imm); break;

            // Logical
            case OP_AND:  translate_and(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_OR:   translate_or(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_XOR:  translate_xor(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_ANDI: translate_andi(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_ORI:  translate_ori(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_XORI: translate_xori(ctx, inst.rd, inst.rs1, inst.imm); break;

            // Shifts
            case OP_SLL:  translate_sll(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SRL:  translate_srl(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SRA:  translate_sra(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SLLI: translate_slli(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_SRLI: translate_srli(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_SRAI: translate_srai(ctx, inst.rd, inst.rs1, inst.imm); break;

            // Comparisons
            case OP_SLT:  translate_slt(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SLTU: translate_sltu(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SEQ:  translate_seq(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SNE:  translate_sne(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SGT:  translate_sgt(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SGTU: translate_sgtu(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SLE:  translate_sle(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SLEU: translate_sleu(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SGE:  translate_sge(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SGEU: translate_sgeu(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SLTI: translate_slti(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_SLTIU:translate_sltiu(ctx, inst.rd, inst.rs1, inst.imm); break;

            // Multiply/Divide
            case OP_MUL:  translate_mul(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_MULH: translate_mulh(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_MULHU: translate_mulhu(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_DIV:  translate_div(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_REM:  translate_rem(ctx, inst.rd, inst.rs1, inst.rs2); break;

            // All floating-point → interpreter fallback
            case OP_FADD_S: case OP_FSUB_S: case OP_FMUL_S: case OP_FDIV_S:
            case OP_FSQRT_S: case OP_FEQ_S: case OP_FLT_S: case OP_FLE_S:
            case OP_FCVT_W_S: case OP_FCVT_WU_S: case OP_FCVT_S_W: case OP_FCVT_S_WU:
            case OP_FNEG_S: case OP_FABS_S:
            case OP_FADD_D: case OP_FSUB_D: case OP_FMUL_D: case OP_FDIV_D:
            case OP_FSQRT_D: case OP_FEQ_D: case OP_FLT_D: case OP_FLE_D:
            case OP_FCVT_W_D: case OP_FCVT_WU_D: case OP_FCVT_D_W: case OP_FCVT_D_WU:
            case OP_FCVT_D_S: case OP_FCVT_S_D: case OP_FNEG_D: case OP_FABS_D:
            case OP_FCVT_L_S: case OP_FCVT_LU_S: case OP_FCVT_S_L: case OP_FCVT_S_LU:
            case OP_FCVT_L_D: case OP_FCVT_LU_D: case OP_FCVT_D_L: case OP_FCVT_D_LU:
                translate_fp_r_type(ctx, inst.opcode, inst.rd, inst.rs1, inst.rs2);
                goto block_done;

            // Upper immediate
            case OP_LUI:  translate_lui(ctx, inst.rd, inst.imm); break;

            // Memory
            case OP_LDW:  translate_ldw(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_LDH:  translate_ldh(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_LDB:  translate_ldb(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_LDHU: translate_ldhu(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_LDBU: translate_ldbu(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_STW:  translate_stw(ctx, inst.rs1, inst.rs2, inst.imm); break;
            case OP_STH:  translate_sth(ctx, inst.rs1, inst.rs2, inst.imm); break;
            case OP_STB:  translate_stb(ctx, inst.rs1, inst.rs2, inst.imm); break;

            // Control flow (block-ending)
            case OP_JAL:
                translate_jal(ctx, inst.rd, inst.imm);
                goto block_done;

            case OP_JALR:
                translate_jalr(ctx, inst.rd, inst.rs1, inst.imm);
                goto block_done;

            case OP_BEQ:
                if (translate_beq(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto block_done;
                continue;

            case OP_BNE:
                if (translate_bne(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto block_done;
                continue;

            case OP_BLT:
                if (translate_blt(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto block_done;
                continue;

            case OP_BGE:
                if (translate_bge(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto block_done;
                continue;

            case OP_BLTU:
                if (translate_bltu(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto block_done;
                continue;

            case OP_BGEU:
                if (translate_bgeu(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto block_done;
                continue;

            // Special (block-ending)
            case OP_HALT:
                translate_halt(ctx);
                goto block_done;

            case OP_DEBUG:
                translate_debug(ctx, inst.rs1);
                goto block_done;

            case OP_YIELD:
                translate_yield(ctx);
                goto block_done;

            case OP_ASSERT_EQ:
                translate_assert_eq(ctx, inst.rs1, inst.rs2);
                break;

            case OP_NOP:
                translate_nop(ctx);
                break;

            default:
                fprintf(stderr, "DBT: Unknown opcode 0x%02X at PC 0x%08X\n",
                        inst.opcode, ctx->guest_pc);
                emit_exit(ctx, EXIT_HALT, ctx->guest_pc);
                goto block_done;
        }

        ctx->guest_pc += 4;
        ctx->inst_count++;
    }

    // Reached max instructions
    emit_exit(ctx, EXIT_BLOCK_END, ctx->guest_pc);

block_done:
    if (ctx->block) {
        ctx->block->reg_cache_hits = ctx->reg_cache_hits;
        ctx->block->reg_cache_misses = ctx->reg_cache_misses;
    }
    cpu->code_buffer_used = emit_offset(e);

#ifdef __aarch64__
    // ARM I-cache is not coherent with D-cache — must flush after writing code
    __builtin___clear_cache((char *)entry, (char *)entry + cpu->code_buffer_used);
#endif

    return (translated_block_fn)entry;
}

// ============================================================================
// Stage 2: translate_block_cached (stub — to be implemented)
// ============================================================================

translated_block_t *translate_block_cached(translate_ctx_t *ctx, uint32_t guest_pc) {
    dbt_cpu_state_t *cpu = ctx->cpu;
    block_cache_t *cache = ctx->cache;
    emit_ctx_t *e = &ctx->emit;

    // Allocate a block
    translated_block_t *block = cache_alloc_block(cache, guest_pc);
    if (!block) return NULL;

    ctx->block = block;
    ctx->exit_idx = 0;
    ctx->guest_pc = guest_pc;
    cpu->pc = guest_pc;

    // Translate into cache code buffer (aligned to 16 bytes)
    uint8_t *code_start = cache_get_code_ptr(cache);
    uint32_t code_avail = cache->code_buffer_size -
                          (uint32_t)(code_start - cache->code_buffer);
    emit_init(e, code_start, code_avail);

    void *entry = emit_ptr(e);

    ctx->block_start_pc = guest_pc;
    ctx->inst_count = 0;
    ctx->side_exit_emitted = 0;
    ctx->deferred_exit_count = 0;
    ctx->pc_map_count = 0;
    ctx->pending_write.valid = false;
    ctx->emit.rax_pending = false;
    ctx->pending_cond.valid = false;
    ctx->current_inst_idx = 0;
    ctx->has_backedge = false;
    ctx->backedge_target_pc = 0;
    ctx->backedge_target_count = 0;
    memset(ctx->side_exit_pcs, 0, sizeof(ctx->side_exit_pcs));
    memset(ctx->dead_temp_skip, 0, sizeof(ctx->dead_temp_skip));
    reg_alloc_reset(ctx);
    const_prop_reset(ctx);
    bounds_elim_reset(ctx);
    reg_alloc_prescan(ctx, guest_pc);

    // Emit prologue: load cached guest registers into host callee-saved registers
    if (ctx->reg_cache_enabled) {
        reg_alloc_emit_prologue(ctx);
    }

    if (DBT_TRACE) {
        fprintf(stderr, "DBT: Translating cached block at PC=0x%08X (AArch64)\n", guest_pc);
    }

    while (ctx->inst_count < MAX_BLOCK_INSTS) {
        if (ctx->guest_pc >= cpu->code_limit ||
            (cpu->code_limit - ctx->guest_pc) < 4) {
            emit_exit_with_info(ctx, EXIT_FAULT_FETCH, ctx->guest_pc, ctx->guest_pc);
            break;
        }
        if (cpu->align_traps_enabled && (ctx->guest_pc & 3u) != 0) {
            emit_exit_with_info(ctx, EXIT_FAULT_FETCH, ctx->guest_pc, ctx->guest_pc);
            break;
        }

        if (is_backedge_target(ctx, ctx->guest_pc)) {
            // At back-edge targets, flush all state so the loop iteration
            // sees consistent memory values for non-cached register accesses.
            reg_cache_flush(ctx);
            const_prop_reset(ctx);
            bounds_elim_reset(ctx);
        }

        if (ctx->pc_map_count < MAX_BLOCK_INSTS) {
            ctx->pc_map[ctx->pc_map_count].guest_pc = ctx->guest_pc;
            ctx->pc_map[ctx->pc_map_count].host_offset = emit_offset(e);
            ctx->pc_map_count++;
        }

        uint32_t raw = *(uint32_t *)(cpu->mem_base + ctx->guest_pc);
        decoded_inst_t inst = decode_instruction(raw);

        ctx->current_inst_idx = ctx->inst_count;

        switch (inst.opcode) {
            case OP_ADD:  translate_add(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SUB:  translate_sub(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_ADDI: translate_addi(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_AND:  translate_and(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_OR:   translate_or(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_XOR:  translate_xor(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_ANDI: translate_andi(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_ORI:  translate_ori(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_XORI: translate_xori(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_SLL:  translate_sll(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SRL:  translate_srl(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SRA:  translate_sra(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SLLI: translate_slli(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_SRLI: translate_srli(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_SRAI: translate_srai(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_SLT:  translate_slt(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SLTU: translate_sltu(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SEQ:  translate_seq(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SNE:  translate_sne(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SGT:  translate_sgt(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SGTU: translate_sgtu(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SLE:  translate_sle(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SLEU: translate_sleu(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SGE:  translate_sge(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SGEU: translate_sgeu(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_SLTI: translate_slti(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_SLTIU:translate_sltiu(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_MUL:  translate_mul(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_MULH: translate_mulh(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_MULHU: translate_mulhu(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_DIV:  translate_div(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_REM:  translate_rem(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_LUI:  translate_lui(ctx, inst.rd, inst.imm); break;
            case OP_LDW:  translate_ldw(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_LDH:  translate_ldh(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_LDB:  translate_ldb(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_LDHU: translate_ldhu(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_LDBU: translate_ldbu(ctx, inst.rd, inst.rs1, inst.imm); break;
            case OP_STW:  translate_stw(ctx, inst.rs1, inst.rs2, inst.imm); break;
            case OP_STH:  translate_sth(ctx, inst.rs1, inst.rs2, inst.imm); break;
            case OP_STB:  translate_stb(ctx, inst.rs1, inst.rs2, inst.imm); break;
            case OP_JAL:  translate_jal(ctx, inst.rd, inst.imm); goto cached_done;
            case OP_JALR: translate_jalr(ctx, inst.rd, inst.rs1, inst.imm); goto cached_done;
            case OP_BEQ:  if (translate_beq(ctx, inst.rs1, inst.rs2, inst.imm)) goto cached_done; continue;
            case OP_BNE:  if (translate_bne(ctx, inst.rs1, inst.rs2, inst.imm)) goto cached_done; continue;
            case OP_BLT:  if (translate_blt(ctx, inst.rs1, inst.rs2, inst.imm)) goto cached_done; continue;
            case OP_BGE:  if (translate_bge(ctx, inst.rs1, inst.rs2, inst.imm)) goto cached_done; continue;
            case OP_BLTU: if (translate_bltu(ctx, inst.rs1, inst.rs2, inst.imm)) goto cached_done; continue;
            case OP_BGEU: if (translate_bgeu(ctx, inst.rs1, inst.rs2, inst.imm)) goto cached_done; continue;
            case OP_HALT:  translate_halt(ctx); goto cached_done;
            case OP_DEBUG: translate_debug(ctx, inst.rs1); goto cached_done;
            case OP_YIELD: translate_yield(ctx); goto cached_done;
            case OP_ASSERT_EQ: translate_assert_eq(ctx, inst.rs1, inst.rs2); break;
            case OP_NOP:   translate_nop(ctx); break;

            // Floating-point → halt
            case OP_FADD_S: case OP_FSUB_S: case OP_FMUL_S: case OP_FDIV_S:
            case OP_FSQRT_S: case OP_FEQ_S: case OP_FLT_S: case OP_FLE_S:
            case OP_FCVT_W_S: case OP_FCVT_WU_S: case OP_FCVT_S_W: case OP_FCVT_S_WU:
            case OP_FNEG_S: case OP_FABS_S:
            case OP_FADD_D: case OP_FSUB_D: case OP_FMUL_D: case OP_FDIV_D:
            case OP_FSQRT_D: case OP_FEQ_D: case OP_FLT_D: case OP_FLE_D:
            case OP_FCVT_W_D: case OP_FCVT_WU_D: case OP_FCVT_D_W: case OP_FCVT_D_WU:
            case OP_FCVT_D_S: case OP_FCVT_S_D: case OP_FNEG_D: case OP_FABS_D:
            case OP_FCVT_L_S: case OP_FCVT_LU_S: case OP_FCVT_S_L: case OP_FCVT_S_LU:
            case OP_FCVT_L_D: case OP_FCVT_LU_D: case OP_FCVT_D_L: case OP_FCVT_D_LU:
                translate_fp_r_type(ctx, inst.opcode, inst.rd, inst.rs1, inst.rs2);
                goto cached_done;

            default:
                fprintf(stderr, "DBT: Unknown opcode 0x%02X at PC 0x%08X\n",
                        inst.opcode, ctx->guest_pc);
                emit_exit(ctx, EXIT_HALT, ctx->guest_pc);
                goto cached_done;
        }

        ctx->guest_pc += 4;
        ctx->inst_count++;
    }

    emit_exit(ctx, EXIT_BLOCK_END, ctx->guest_pc);

cached_done:
    block->host_code = (uint8_t *)entry;
    block->host_size = emit_offset(e);
    block->guest_size = (ctx->inst_count + 1) * 4;
    block->reg_cache_hits = ctx->reg_cache_hits;
    block->reg_cache_misses = ctx->reg_cache_misses;

    cache_commit_code(cache, block->host_size);

    // Insert into hash table so cache_lookup can find it
    cache_insert(cache, block);

    // Chain any pending blocks that were waiting for this one
    cache_chain_pending(cache, block);

    ctx->block = NULL;
    return block;
}
