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
uint32_t cmp_branch_fusion_count = 0;  // Global fusion counter for stats
uint32_t cbz_peephole_count = 0;      // CBZ/CBNZ peephole counter for stats
uint32_t native_stub_count = 0;       // Native intrinsic stub counter

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
    ctx->inline_lookup_enabled = true;   // Stage 3: inline hash-probe lookup
    ctx->ras_enabled = true;              // Stage 3: return address stack
    ctx->superblock_enabled = true;
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
static inline a64_reg_t resolve_src(translate_ctx_t *ctx, uint8_t guest_reg, a64_reg_t scratch);

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

// ============================================================================
// Compare-Branch Fusion: pending condition management
// ============================================================================

// Invert an AArch64 condition code (flip bit 0)
static inline a64_cond_t invert_cond(a64_cond_t cond) {
    return (a64_cond_t)(cond ^ 1);
}

// Map comparison opcode to AArch64 condition code
static a64_cond_t cmp_opcode_to_cond(uint8_t opcode) {
    switch (opcode) {
        case OP_SLT:  case OP_SLTI:  return COND_LT;
        case OP_SLTU: case OP_SLTIU: return COND_LO;
        case OP_SEQ:  return COND_EQ;
        case OP_SNE:  return COND_NE;
        case OP_SGT:  return COND_GT;
        case OP_SGTU: return COND_HI;
        case OP_SLE:  return COND_LE;
        case OP_SLEU: return COND_LS;
        case OP_SGE:  return COND_GE;
        case OP_SGEU: return COND_HS;
        default:      return COND_AL;
    }
}

// Look ahead: can the comparison result be fused with the next branch?
// Pattern: Sxx rd, rs1, rs2; BEQ/BNE rd, r0, target
static bool can_fuse_with_next(translate_ctx_t *ctx, uint8_t rd) {
    dbt_cpu_state_t *cpu = ctx->cpu;
    uint32_t next_pc = ctx->guest_pc + 4;

    if (next_pc + 4 > cpu->code_limit) return false;

    uint32_t raw = *(uint32_t *)(cpu->mem_base + next_pc);
    uint8_t opcode = raw & 0x7F;

    // Only BEQ and BNE can be fused
    if (opcode != OP_BEQ && opcode != OP_BNE) return false;

    uint8_t rs1 = (raw >> 15) & 0x1F;
    uint8_t rs2 = (raw >> 20) & 0x1F;

    // Pattern: Bxx rd, r0, target (testing comparison result against zero)
    return (rs1 == rd && rs2 == 0) || (rs2 == rd && rs1 == 0);
}

// Materialize a deferred comparison into its destination register.
// Emits CMP + CSET + store. On AArch64, none of these modify flags,
// but we only call this when the comparison is NOT being fused with a branch.
static void materialize_pending_cond(translate_ctx_t *ctx) {
    if (!ctx->pending_cond.valid) return;

    emit_ctx_t *e = &ctx->emit;
    uint8_t opcode = ctx->pending_cond.opcode;
    uint8_t rd = ctx->pending_cond.rd;
    uint8_t c_rs1 = ctx->pending_cond.rs1;
    uint8_t c_rs2 = ctx->pending_cond.rs2;
    bool rs2_is_imm = ctx->pending_cond.rs2_is_imm;
    int32_t imm = ctx->pending_cond.imm;

    // Clear valid BEFORE emitting to avoid re-entrant flush
    ctx->pending_cond.valid = false;

    // Emit the comparison
    a64_reg_t s1 = resolve_src(ctx, c_rs1, W0);

    if (rs2_is_imm) {
        if (imm >= 0 && (uint32_t)imm < 4096) {
            emit_cmp_w32_imm(e, s1, (uint32_t)imm);
        } else if (imm < 0 && (uint32_t)(-imm) < 4096) {
            emit_cmn_w32_imm(e, s1, (uint32_t)(-imm));
        } else {
            emit_mov_w32_imm32(e, W1, (uint32_t)imm);
            emit_cmp_w32_w32(e, s1, W1);
        }
    } else {
        a64_reg_t s2 = resolve_src(ctx, c_rs2, W1);
        emit_cmp_w32_w32(e, s1, s2);
    }

    // Emit CSET + store
    a64_cond_t cond = cmp_opcode_to_cond(opcode);
    a64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != A64_NOREG) {
        emit_cset_w32(e, hd, cond);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_cset_w32(e, W0, cond);
        emit_store_guest_reg(ctx, rd, W0);
    }
}

static inline void flush_pending_cond(translate_ctx_t *ctx) {
    materialize_pending_cond(ctx);
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
// Compact chained exit (for superblock deferred side exits)
// Same as emit_exit_chained() but WITHOUT reg_cache_flush() — caller already
// flushed from the register allocation snapshot.
// ============================================================================

static void emit_exit_chained_compact(translate_ctx_t *ctx, uint32_t target_pc, int exit_idx) {
    emit_ctx_t *e = &ctx->emit;

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

    // Stage 2+: chain to target if already translated
    translated_block_t *target = cache_lookup(ctx->cache, target_pc);

    if (target && target->host_code) {
        // Target already translated - emit direct B
        int64_t rel = (int64_t)((uint8_t *)target->host_code - emit_ptr(e));
        emit_b(e, (int32_t)rel);

        if (ctx->block && exit_idx < MAX_BLOCK_EXITS) {
            cache_record_exit(ctx->cache, ctx->block, exit_idx, target_pc,
                              emit_ptr(e) - 4);
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
// Deferred side exit emission (superblock cold stubs)
// Called after the main translation loop to emit out-of-line exit stubs
// for each superblock side exit (taken branch path).
// ============================================================================

static void emit_deferred_side_exits(translate_ctx_t *ctx) {
    emit_ctx_t *e = &ctx->emit;

    for (int i = 0; i < ctx->deferred_exit_count; i++) {
        size_t cold_offset = emit_offset(e);

        // Patch B.cond imm19 to point here
        size_t bcond_off = ctx->deferred_exits[i].jmp_patch_offset;
        int32_t rel = (int32_t)(cold_offset - bcond_off);
        uint32_t *inst = (uint32_t *)(e->buf + bcond_off);
        int32_t imm19 = rel >> 2;
        *inst = (*inst & ~(0x7FFFF << 5)) | ((imm19 & 0x7FFFF) << 5);

        // Flush pending write from snapshot
        if (ctx->deferred_exits[i].pending_write_valid) {
            emit_str_w32_imm(e, ctx->deferred_exits[i].pending_write_host_reg, W20,
                             GUEST_REG_OFFSET(ctx->deferred_exits[i].pending_write_guest_reg));
        }

        // Flush dirty cached regs from snapshot
        reg_alloc_flush_snapshot(ctx,
            ctx->deferred_exits[i].dirty_snapshot,
            ctx->deferred_exits[i].allocated_snapshot,
            ctx->deferred_exits[i].guest_reg_snapshot);

        // Record branch_pc for profiling
        int exit_idx = ctx->deferred_exits[i].exit_idx;
        if (ctx->block && exit_idx < MAX_BLOCK_EXITS) {
            ctx->block->exits[exit_idx].branch_pc = ctx->deferred_exits[i].branch_pc;
        }

        // Emit compact exit (store PC + chain/dispatcher jump)
        emit_exit_chained_compact(ctx, ctx->deferred_exits[i].target_pc, exit_idx);
    }
}

// ============================================================================
// Inline indirect lookup (Stage 3)
// Emits a 4-probe hash lookup directly in generated code for JALR targets.
// Avoids returning to the C dispatcher when the target block is cached.
//
// Structure per probe:
//   [advance index] → load block ptr → CBZ null→miss
//   → load guest_pc → CMP → B.NE → [next probe]
//   → [HIT: flush + LDR host_code + BR]  (exits generated code)
// After all probes: [MISS: flush + store PC + jump to native dispatcher]
// ============================================================================

#define INLINE_LOOKUP_MAX_PROBES 4

static void emit_inline_lookup(translate_ctx_t *ctx, a64_reg_t target_reg) {
    emit_ctx_t *e = &ctx->emit;

    // Save target in W3 (preserved across probes)
    if (target_reg != W3) {
        emit_mov_w32_w32(e, W3, target_reg);
    }

    // Compute hash: ((target >> 2) ^ (target >> 12)) & mask
    emit_lsr_w32_imm(e, W4, W3, 2);              // W4 = target >> 2
    emit_eor_w32_lsr(e, W4, W4, W3, 12);         // W4 ^= (target >> 12)
    emit_ldr_w32_imm(e, W5, W20, CPU_LOOKUP_MASK_OFFSET);  // W5 = mask
    emit_and_w32(e, W4, W4, W5);                  // W4 = hash index

    // Track CBZ (null) patch locations — all patched to miss at end
    size_t null_patches[INLINE_LOOKUP_MAX_PROBES];
    int null_patch_count = 0;

    for (int probe = 0; probe < INLINE_LOOKUP_MAX_PROBES; probe++) {
        if (probe > 0) {
            emit_add_w32_imm(e, W4, W4, 1);      // W4++
            emit_and_w32(e, W4, W4, W5);          // W4 &= mask
        }

        // Load block pointer: X6 = lookup_table[hash] (8 bytes per entry)
        emit_ldr_x64_reg_lsl3(e, W6, W22, W4);   // X6 = [X22 + X4<<3]

        // Null check → miss
        null_patches[null_patch_count++] = emit_offset(e);
        emit_cbz_x64(e, W6, 0);                  // Patch later to miss

        // Compare block->guest_pc vs target
        emit_ldr_w32_imm(e, W7, W6, 0);          // W7 = block->guest_pc
        emit_cmp_w32_w32(e, W7, W3);             // target match?

        // B.NE → skip hit path to next probe (or miss for last)
        size_t bne_patch = emit_offset(e);
        emit_b_cond(e, COND_NE, 0);              // Patch after hit path

        // HIT: flush cached regs, load host_code, jump to translated block
        reg_cache_flush(ctx);
        emit_ldr_x64_imm(e, W6, W6, 8);          // X6 = block->host_code
        emit_br(e, W6);                           // BR X6 — exits block

        // Patch B.NE to here (start of next probe, or miss for last)
        {
            size_t here = emit_offset(e);
            int32_t rel = (int32_t)(here - bne_patch);
            uint32_t *inst = (uint32_t *)(e->buf + bne_patch);
            int32_t imm19 = rel >> 2;
            *inst = (*inst & ~(0x7FFFF << 5)) | ((imm19 & 0x7FFFF) << 5);
        }
    }

    // Miss path: patch all CBZ null branches here
    size_t miss_offset = emit_offset(e);
    for (int i = 0; i < null_patch_count; i++) {
        int32_t rel = (int32_t)(miss_offset - null_patches[i]);
        uint32_t *inst = (uint32_t *)(e->buf + null_patches[i]);
        int32_t imm19 = rel >> 2;
        *inst = (*inst & ~(0x7FFFF << 5)) | ((imm19 & 0x7FFFF) << 5);
    }

    // Miss fallback: flush + store PC + jump to native dispatcher
    reg_cache_flush(ctx);
    emit_str_w32_imm(e, W3, W20, CPU_PC_OFFSET);
    emit_mov_w32_imm32(e, W1, EXIT_INDIRECT);
    emit_str_w32_imm(e, W1, W20, CPU_EXIT_REASON_OFFSET);

    if (ctx->cache && ctx->cache->native_dispatcher) {
        int64_t rel = (int64_t)(ctx->cache->native_dispatcher - emit_ptr(e));
        emit_b(e, (int32_t)rel);
    } else {
        emit_ret_lr(e);
    }
}

// ============================================================================
// Return Address Stack (Stage 3 Phase 2)
// ============================================================================

// Push return address onto RAS at JAL call sites (rd == REG_LR)
static void emit_ras_push(translate_ctx_t *ctx, uint32_t return_pc) {
    emit_ctx_t *e = &ctx->emit;

    // Load ras_top index
    emit_ldr_w32_imm(e, W1, W20, CPU_RAS_TOP_OFFSET);      // W1 = ras_top

    // Compute byte offset: W2 = W1 << 2 (each entry is 4 bytes)
    emit_lsl_w32_imm(e, W2, W1, 2);                         // W2 = ras_top * 4

    // Store return_pc at ras_stack[ras_top]
    emit_mov_w32_imm32(e, W3, return_pc);                    // W3 = return_pc
    emit_add_w32_imm(e, W2, W2, CPU_RAS_STACK_OFFSET);      // W2 = byte offset into cpu_state
    emit_str_w32_reg_uxtw(e, W3, W20, W2);                  // [X20 + W2] = return_pc

    // Increment top with wrap: ras_top = (ras_top + 1) & RAS_MASK
    emit_add_w32_imm(e, W1, W1, 1);                         // W1++
    if (!emit_and_w32_imm(e, W1, W1, RAS_MASK)) {           // W1 &= RAS_MASK
        emit_mov_w32_imm32(e, W2, RAS_MASK);
        emit_and_w32(e, W1, W1, W2);
    }
    emit_str_w32_imm(e, W1, W20, CPU_RAS_TOP_OFFSET);       // store new ras_top
}

// Predict return address from RAS and perform inline lookup.
// target_reg holds the actual computed return target.
// If RAS prediction matches, we use it for inline lookup (should hit fast).
// If mismatch, use the actual target instead.
static void emit_ras_predict(translate_ctx_t *ctx, a64_reg_t target_reg) {
    emit_ctx_t *e = &ctx->emit;

    // Save actual target in W8 (higher scratch, survives RAS operations)
    if (target_reg != W8) {
        emit_mov_w32_w32(e, W8, target_reg);
    }

    // Decrement ras_top with wrap: ras_top = (ras_top - 1) & RAS_MASK
    emit_ldr_w32_imm(e, W1, W20, CPU_RAS_TOP_OFFSET);       // W1 = ras_top
    emit_sub_w32_imm(e, W1, W1, 1);                          // W1--
    if (!emit_and_w32_imm(e, W1, W1, RAS_MASK)) {            // W1 &= RAS_MASK
        emit_mov_w32_imm32(e, W2, RAS_MASK);
        emit_and_w32(e, W1, W1, W2);
    }
    emit_str_w32_imm(e, W1, W20, CPU_RAS_TOP_OFFSET);       // store new ras_top

    // Load predicted return address: W0 = ras_stack[ras_top]
    emit_lsl_w32_imm(e, W2, W1, 2);                          // W2 = ras_top * 4
    emit_add_w32_imm(e, W2, W2, CPU_RAS_STACK_OFFSET);       // byte offset into cpu_state
    emit_ldr_w32_reg_uxtw(e, W0, W20, W2);                   // W0 = predicted PC

    // Compare predicted vs actual
    emit_cmp_w32_w32(e, W0, W8);                              // predicted == actual?

    // If match → use predicted (W0) for inline lookup
    // If mismatch → use actual (W8) for inline lookup
    // CSEL W0, W0, W8, EQ  — select predicted if match, actual otherwise
    // Actually, both paths do inline lookup with the same value anyway (the actual target).
    // The RAS prediction matters because the inline lookup hash probe is more likely
    // to hit on probe 0 for predicted targets (they were recently translated).
    // So: always use W8 (actual target) for inline lookup.
    // The point of RAS is just to avoid the native dispatcher, not to predict differently.
    // Actually, looking at the x86 code again: if prediction matches, it uses the predicted
    // value for inline lookup because it's more likely to be a hot block.
    // If mismatch, it still does inline lookup with the actual target.
    // For simplicity, just do inline lookup with the actual target always.
    emit_inline_lookup(ctx, W8);
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
// With compare-branch fusion: defer CMP when next instruction is BEQ/BNE
// ============================================================================

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

    // Compare-branch fusion: defer if next instruction is BEQ/BNE testing rd
    if (can_fuse_with_next(ctx, rd)) {
        flush_pending_cond(ctx);
        ctx->pending_cond.opcode = opcode;
        ctx->pending_cond.rd = rd;
        ctx->pending_cond.rs1 = rs1;
        ctx->pending_cond.rs2 = rs2;
        ctx->pending_cond.rs2_is_imm = false;
        ctx->pending_cond.inst_idx = ctx->current_inst_idx;
        ctx->pending_cond.valid = true;
        ctx->reg_constants[rd].valid = false;
        return;
    }

    // Not fusible — emit immediately
    a64_cond_t cond = cmp_opcode_to_cond(opcode);
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

    // Compare-branch fusion: defer if next instruction is BEQ/BNE testing rd
    if (can_fuse_with_next(ctx, rd)) {
        flush_pending_cond(ctx);
        ctx->pending_cond.opcode = OP_SLTI;
        ctx->pending_cond.rd = rd;
        ctx->pending_cond.rs1 = rs1;
        ctx->pending_cond.rs2 = 0;
        ctx->pending_cond.rs2_is_imm = true;
        ctx->pending_cond.imm = imm;
        ctx->pending_cond.inst_idx = ctx->current_inst_idx;
        ctx->pending_cond.valid = true;
        ctx->reg_constants[rd].valid = false;
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

    // Compare-branch fusion: defer if next instruction is BEQ/BNE testing rd
    if (can_fuse_with_next(ctx, rd)) {
        flush_pending_cond(ctx);
        ctx->pending_cond.opcode = OP_SLTIU;
        ctx->pending_cond.rd = rd;
        ctx->pending_cond.rs1 = rs1;
        ctx->pending_cond.rs2 = 0;
        ctx->pending_cond.rs2_is_imm = true;
        ctx->pending_cond.imm = imm;
        ctx->pending_cond.inst_idx = ctx->current_inst_idx;
        ctx->pending_cond.valid = true;
        ctx->reg_constants[rd].valid = false;
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

    // RAS push: if this is a call (rd == REG_LR), push return address
    if (ctx->ras_enabled && rd == REG_LR) {
        emit_ras_push(ctx, return_pc);
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

    // Inline lookup for indirect jumps (Stage 3)
    bool is_return = (rd == 0 && rs1 == REG_LR && imm == 0);

    if (ctx->ras_enabled && is_return && ctx->inline_lookup_enabled && ctx->cache) {
        // RAS-predicted return: pop predicted PC, do inline lookup
        emit_ras_predict(ctx, W0);
    } else if (ctx->inline_lookup_enabled && ctx->cache) {
        // Generic indirect: inline 4-probe hash lookup
        emit_inline_lookup(ctx, W0);
    } else {
        // Stage 1-2 fallback: store PC + exit to dispatcher
        reg_cache_flush(ctx);
        emit_str_w32_imm(e, W0, W20, CPU_PC_OFFSET);
        emit_mov_w32_imm32(e, W1, EXIT_INDIRECT);
        emit_str_w32_imm(e, W1, W20, CPU_EXIT_REASON_OFFSET);
        emit_ret_lr(e);
    }
}

// ============================================================================
// Branch translations
// ============================================================================

static bool translate_branch_common(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2,
                                    int32_t imm, a64_cond_t cond) {
    emit_ctx_t *e = &ctx->emit;
    uint32_t fall_pc = ctx->guest_pc + 4;
    uint32_t taken_pc = fall_pc + imm;  // PC+4 + imm
    bool fused = false;
    a64_reg_t cbz_reg = A64_NOREG;
    bool cbz_is_nz = false;

    // Compare-branch fusion: consume pending_cond if this branch tests its rd
    if (ctx->pending_cond.valid) {
        uint8_t prd = ctx->pending_cond.rd;
        // Pattern: BEQ/BNE prd, r0, target — one operand is prd, other is r0
        if ((rs1 == prd && rs2 == 0) || (rs2 == prd && rs1 == 0)) {
            // Extract deferred comparison state
            uint8_t saved_opcode = ctx->pending_cond.opcode;
            uint8_t c_rs1 = ctx->pending_cond.rs1;
            uint8_t c_rs2 = ctx->pending_cond.rs2;
            bool rs2_is_imm = ctx->pending_cond.rs2_is_imm;
            int32_t saved_imm = ctx->pending_cond.imm;
            int saved_idx = ctx->pending_cond.inst_idx;

            // Clear pending before emitting to avoid re-entrant flush
            ctx->pending_cond.valid = false;

            // Emit the deferred comparison
            a64_reg_t s1 = resolve_src(ctx, c_rs1, W0);
            if (rs2_is_imm) {
                if (saved_imm >= 0 && (uint32_t)saved_imm < 4096) {
                    emit_cmp_w32_imm(e, s1, (uint32_t)saved_imm);
                } else if (saved_imm < 0 && (uint32_t)(-saved_imm) < 4096) {
                    emit_cmn_w32_imm(e, s1, (uint32_t)(-saved_imm));
                } else {
                    emit_mov_w32_imm32(e, W1, (uint32_t)saved_imm);
                    emit_cmp_w32_w32(e, s1, W1);
                }
            } else {
                a64_reg_t s2 = resolve_src(ctx, c_rs2, W1);
                emit_cmp_w32_w32(e, s1, s2);
            }

            // If rd is NOT dead, materialize the comparison result.
            // On AArch64, CSET/STR don't modify flags, so this is safe
            // between CMP and B.cond.
            bool rd_is_dead = ctx->dead_temp_skip[saved_idx];
            if (!rd_is_dead) {
                a64_cond_t cmp_cond = cmp_opcode_to_cond(saved_opcode);
                a64_reg_t hd = guest_host_reg(ctx, prd);
                if (hd != A64_NOREG) {
                    emit_cset_w32(e, hd, cmp_cond);
                    reg_cache_mark_written(ctx, prd);
                } else {
                    emit_cset_w32(e, W2, cmp_cond);
                    emit_store_guest_reg(ctx, prd, W2);
                }
            }

            // Determine the fused branch condition:
            // The comparison set cond (e.g., SLT → COND_LT).
            // BNE rd, r0 = "branch if comparison was true" → use cond directly
            // BEQ rd, r0 = "branch if comparison was false" → invert cond
            a64_cond_t fused_cond = cmp_opcode_to_cond(saved_opcode);
            if (cond == COND_EQ) {
                fused_cond = invert_cond(fused_cond);
            }
            cond = fused_cond;
            fused = true;
            cmp_branch_fusion_count++;
        }
    }

    if (!fused) {
        // Standard comparison: flush any stale pending_cond
        flush_pending_cond(ctx);

        if ((cond == COND_EQ || cond == COND_NE) && (rs1 == 0 || rs2 == 0)) {
            // CBZ/CBNZ: skip CMP entirely, saves 2 instructions
            uint8_t nonzero_reg = (rs1 == 0) ? rs2 : rs1;
            cbz_reg = resolve_src(ctx, nonzero_reg, W0);
            cbz_is_nz = (cond == COND_NE);
            cbz_peephole_count++;
        } else if (rs1 == 0 || rs2 == 0) {
            // CMP-against-zero: saves 1 instruction (no MOVZ needed)
            uint8_t nonzero_reg = (rs1 == 0) ? rs2 : rs1;
            a64_reg_t s = resolve_src(ctx, nonzero_reg, W0);
            if (rs1 == 0) {
                // Comparing 0 vs rs2: flags must reflect (0 - rs2)
                emit_cmp_w32_w32(e, WZR, s);
            } else {
                // Comparing rs1 vs 0
                emit_cmp_w32_imm(e, s, 0);
            }
        } else {
            a64_reg_t s1 = resolve_src(ctx, rs1, W0);
            a64_reg_t s2 = resolve_src(ctx, rs2, W1);
            emit_cmp_w32_w32(e, s1, s2);
        }
    }

    // ---- Superblock extension: inline fall-through, defer taken path ----
    {
        bool can_extend = ctx->superblock_enabled &&
                          ctx->superblock_depth < MAX_SUPERBLOCK_DEPTH &&
                          imm > 0 &&  // Forward branch only (fall-through is hot)
                          ctx->exit_idx < MAX_SUPERBLOCK_EXITS - 1 &&
                          ctx->deferred_exit_count < MAX_BLOCK_EXITS;
        if (ctx->avoid_backedge_extend && imm < 0) can_extend = false;

        if (can_extend) {
            int de_idx = ctx->deferred_exit_count;

            // Flush pending write before snapshot so cold stub doesn't need it
            flush_pending_write(ctx);

            // Emit B.cond/CBZ/CBNZ to cold stub (placeholder, patched in emit_deferred_side_exits)
            size_t bcond_patch = emit_offset(e);
            if (cbz_reg != A64_NOREG) {
                if (cbz_is_nz) emit_cbnz_w32(e, cbz_reg, 0);
                else            emit_cbz_w32(e, cbz_reg, 0);
            } else {
                emit_b_cond(e, cond, 0);  // imm19=0 placeholder
            }

            // Snapshot register allocation state for the deferred cold exit
            ctx->deferred_exits[de_idx].jmp_patch_offset = bcond_patch;
            ctx->deferred_exits[de_idx].target_pc = taken_pc;
            ctx->deferred_exits[de_idx].exit_idx = ctx->exit_idx++;
            ctx->deferred_exits[de_idx].branch_pc = ctx->guest_pc;
            ctx->deferred_exits[de_idx].pending_write_valid = false;

            for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
                ctx->deferred_exits[de_idx].dirty_snapshot[i] = ctx->reg_alloc[i].dirty;
                ctx->deferred_exits[de_idx].allocated_snapshot[i] = ctx->reg_alloc[i].allocated;
                ctx->deferred_exits[de_idx].guest_reg_snapshot[i] = ctx->reg_alloc[i].guest_reg;
            }

            // Record side exit PC for profiling
            if (ctx->side_exit_emitted < MAX_BLOCK_EXITS) {
                ctx->side_exit_pcs[ctx->side_exit_emitted] = ctx->guest_pc;
            }
            ctx->side_exit_emitted++;
            ctx->deferred_exit_count++;

            // Continue translating fall-through (not-taken) path
            ctx->superblock_depth++;
            ctx->guest_pc = fall_pc;
            ctx->inst_count++;
            return false;  // Block continues (superblock extension)
        }
    }

    // ---- Standard dual-exit (no superblock) ----

    // B.cond/CBZ/CBNZ to taken path (jump over not-taken exit)
    size_t bcond_patch = emit_offset(e);
    if (cbz_reg != A64_NOREG) {
        if (cbz_is_nz) emit_cbnz_w32(e, cbz_reg, 0);
        else            emit_cbz_w32(e, cbz_reg, 0);
    } else {
        emit_b_cond(e, cond, 0);  // Patch later
    }

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

    return true;  // Block ends at branch
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
// Intrinsic recognition: emit native stubs for known functions (AArch64)
// ============================================================================

// Helper: emit stub prologue (save LR around host BLR calls)
// STP X29, X30, [SP, #-16]!
static void emit_a64_stub_prologue(emit_ctx_t *e) {
    // In STP/LDP, register 31 in the Rn field = SP (not XZR)
    emit_stp_x64_pre(e, W29, W30, WZR, -16);  // WZR=31=SP in load/store context
}

// Helper: emit stub epilogue (restore LR, set PC = guest LR, exit indirect)
static void emit_a64_stub_epilogue(emit_ctx_t *e) {
    // LDP X29, X30, [SP], #16
    emit_ldp_x64_post(e, W29, W30, WZR, 16);
    // PC = guest r31 (link register)
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(REG_LR));
    emit_str_w32_imm(e, W0, W20, CPU_PC_OFFSET);
    // exit_reason = EXIT_INDIRECT
    emit_mov_w32_imm32(e, W0, EXIT_INDIRECT);
    emit_str_w32_imm(e, W0, W20, CPU_EXIT_REASON_OFFSET);
    emit_ret_lr(e);
}

// Helper: emit BLR to a host function via X8
static void emit_a64_call_host(emit_ctx_t *e, void *fn) {
    emit_mov_x64_imm64(e, W8, (uint64_t)(uintptr_t)fn);
    emit_blr(e, W8);
}

// Helper: combine two 32-bit guest regs (lo, hi) into X0 as a 64-bit double
// Loads regs[lo_reg] → W0, regs[hi_reg] → W1, combines: X0 = W0 | (X1 << 32)
static void emit_a64_load_f64_args(emit_ctx_t *e, int lo_reg, int hi_reg) {
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(lo_reg));
    emit_ldr_w32_imm(e, W1, W20, GUEST_REG_OFFSET(hi_reg));
    // ORR X0, X0, X1, LSL #32
    emit_orr_x64_lsl(e, W0, W0, W1, 32);
    // FMOV D0, X0
    emit_fmov_d_x64(e, 0, W0);
}

// Helper: store D0 result to guest r1:r2 (lo:hi)
static void emit_a64_store_f64_result(emit_ctx_t *e) {
    // FMOV X0, D0
    emit_fmov_x64_d(e, W0, 0);
    // Store low 32 bits to r1
    emit_str_w32_imm(e, W0, W20, GUEST_REG_OFFSET(1));
    // LSR X0, X0, #32 → high 32 bits
    emit_lsr_x64_imm(e, W0, W0, 32);
    // Store to r2
    emit_str_w32_imm(e, W0, W20, GUEST_REG_OFFSET(2));
}

// ---- Shared helpers for intrinsic stubs ----

// Patch an array of CBZ/B.cond sites. Both use imm19 at bits[23:5].
static void patch_imm19_branches(emit_ctx_t *e, size_t *offsets, int count, size_t target) {
    for (int i = 0; i < count; i++) {
        int32_t rel = (int32_t)(target - offsets[i]);
        uint32_t *patch = (uint32_t *)(e->buf + offsets[i]);
        int32_t imm19 = rel >> 2;
        *patch = (*patch & ~(0x7FFFF << 5)) | ((imm19 & 0x7FFFF) << 5);
    }
}

// Emit a fault return sequence for intrinsic stubs (restores stack frame first).
static void emit_a64_stub_fault_exit(translate_ctx_t *ctx, exit_reason_t reason, a64_reg_t info_reg) {
    emit_ctx_t *e = &ctx->emit;
    emit_ldp_x64_post(e, W29, W30, WZR, 16);
    emit_mov_w32_imm32(e, W0, ctx->guest_pc);
    emit_str_w32_imm(e, W0, W20, CPU_PC_OFFSET);
    emit_mov_w32_imm32(e, W0, (uint32_t)reason);
    emit_str_w32_imm(e, W0, W20, CPU_EXIT_REASON_OFFSET);
    emit_str_w32_imm(e, info_reg, W20, CPU_EXIT_INFO_OFFSET);
    emit_ret_lr(e);
}

// Dynamic access check for intrinsic stubs.
// Emits branches to caller-provided fault target patch list.
// Semantics match x86 DBT dynamic checks:
//   - size == 0 => success
//   - bounds check on [addr, addr+size)
//   - MMIO-disabled overlap check
//   - W^X check for stores
static void emit_stub_range_check_a64(translate_ctx_t *ctx,
                                      a64_reg_t addr_reg,
                                      a64_reg_t size_reg,
                                      bool is_store,
                                      size_t *fault_patches,
                                      int *fault_count) {
    emit_ctx_t *e = &ctx->emit;
    dbt_cpu_state_t *cpu = ctx->cpu;

    if (cpu->bounds_checks_disabled) return;

    size_t ok_patches[4];
    int ok_count = 0;

    // size == 0 => ok
    ok_patches[ok_count++] = emit_offset(e);
    emit_cbz_w32(e, size_reg, 0);

    // size > mem_size => fault
    emit_ldr_w32_imm(e, W3, W20, CPU_MEM_SIZE_OFFSET);
    emit_cmp_w32_w32(e, size_reg, W3);
    fault_patches[(*fault_count)++] = emit_offset(e);
    emit_b_cond(e, COND_HI, 0);

    // addr > mem_size - size => fault
    emit_sub_w32(e, W4, W3, size_reg);
    emit_cmp_w32_w32(e, addr_reg, W4);
    fault_patches[(*fault_count)++] = emit_offset(e);
    emit_b_cond(e, COND_HI, 0);

    // MMIO disabled window overlap check for dynamic ranges
    if (!cpu->mmio_enabled && cpu->mmio_base != 0) {
        // end = addr + size
        emit_add_w32(e, W5, addr_reg, size_reg);

        // if end <= mmio_base => ok
        emit_mov_w32_imm32(e, W4, cpu->mmio_base);
        emit_cmp_w32_w32(e, W5, W4);
        ok_patches[ok_count++] = emit_offset(e);
        emit_b_cond(e, COND_LS, 0);

        // if addr >= mmio_base + 0x10000 => ok
        emit_mov_w32_imm32(e, W4, cpu->mmio_base + 0x10000);
        emit_cmp_w32_w32(e, addr_reg, W4);
        ok_patches[ok_count++] = emit_offset(e);
        emit_b_cond(e, COND_HS, 0);

        // overlap => fault
        emit_cmp_w32_w32(e, WZR, WZR);  // Always EQ
        fault_patches[(*fault_count)++] = emit_offset(e);
        emit_b_cond(e, COND_EQ, 0);
    }

    // W^X check — stores only
    if (is_store && cpu->wxorx_enabled) {
        uint32_t wx_limit = cpu->rodata_limit ? cpu->rodata_limit : cpu->code_limit;
        if (wx_limit > 0) {
            emit_mov_w32_imm32(e, W4, wx_limit);
            emit_cmp_w32_w32(e, addr_reg, W4);
            fault_patches[(*fault_count)++] = emit_offset(e);
            emit_b_cond(e, COND_LO, 0);
        }
    }

    patch_imm19_branches(e, ok_patches, ok_count, emit_offset(e));
}

// memcpy / memmove stub
// Guest: r3=dest, r4=src, r5=count, r1=return(dest)
static bool emit_native_memcpy_stub_a64(translate_ctx_t *ctx, translated_block_t *block,
                                         bool is_memmove) {
    emit_ctx_t *e = &ctx->emit;

    emit_a64_stub_prologue(e);

    // Load guest regs
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(3));   // dest
    emit_ldr_w32_imm(e, W1, W20, GUEST_REG_OFFSET(4));   // src
    emit_ldr_w32_imm(e, W2, W20, GUEST_REG_OFFSET(5));   // count

    // Store dest as return value in r1
    emit_str_w32_imm(e, W0, W20, GUEST_REG_OFFSET(1));

    // count == 0 => no-op
    size_t done_patch = emit_offset(e);
    emit_cbz_w32(e, W2, 0);

    // Dynamic checks: src (load) and dest (store)
    size_t fault_load_patches[8];
    int fault_load_count = 0;
    size_t fault_store_patches[8];
    int fault_store_count = 0;
    emit_stub_range_check_a64(ctx, W1, W2, false, fault_load_patches, &fault_load_count);
    emit_stub_range_check_a64(ctx, W0, W2, true, fault_store_patches, &fault_store_count);

    // Bounds OK — convert guest addresses to host pointers
    emit_add_x64_x64_w32_uxtw(e, W0, W21, W0);
    emit_add_x64_x64_w32_uxtw(e, W1, W21, W1);

    // Call host memcpy/memmove
    if (is_memmove) {
        emit_a64_call_host(e, (void *)memmove);
    } else {
        emit_a64_call_host(e, (void *)memcpy);
    }

    size_t done_offset = emit_offset(e);
    patch_imm19_branches(e, &done_patch, 1, done_offset);
    emit_a64_stub_epilogue(e);

    size_t fault_load_offset = emit_offset(e);
    emit_a64_stub_fault_exit(ctx, EXIT_FAULT_LOAD, W1);

    size_t fault_store_offset = emit_offset(e);
    emit_a64_stub_fault_exit(ctx, EXIT_FAULT_STORE, W0);

    patch_imm19_branches(e, fault_load_patches, fault_load_count, fault_load_offset);
    patch_imm19_branches(e, fault_store_patches, fault_store_count, fault_store_offset);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// memset stub
// Guest: r3=dest, r4=value(byte), r5=count, r1=return(dest)
static bool emit_native_memset_stub_a64(translate_ctx_t *ctx, translated_block_t *block) {
    emit_ctx_t *e = &ctx->emit;

    emit_a64_stub_prologue(e);

    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(3));   // dest
    emit_ldr_w32_imm(e, W1, W20, GUEST_REG_OFFSET(4));   // value
    emit_ldr_w32_imm(e, W2, W20, GUEST_REG_OFFSET(5));   // count

    // Store dest as return value
    emit_str_w32_imm(e, W0, W20, GUEST_REG_OFFSET(1));

    // count == 0 => no-op
    size_t done_patch = emit_offset(e);
    emit_cbz_w32(e, W2, 0);

    size_t fault_store_patches[8];
    int fault_store_count = 0;
    emit_stub_range_check_a64(ctx, W0, W2, true, fault_store_patches, &fault_store_count);

    // Bounds OK
    emit_add_x64_x64_w32_uxtw(e, W0, W21, W0);
    // W1 = value, W2 = count already set

    emit_a64_call_host(e, (void *)memset);

    size_t done_offset = emit_offset(e);
    patch_imm19_branches(e, &done_patch, 1, done_offset);
    emit_a64_stub_epilogue(e);

    size_t fault_store_offset = emit_offset(e);
    emit_a64_stub_fault_exit(ctx, EXIT_FAULT_STORE, W0);

    patch_imm19_branches(e, fault_store_patches, fault_store_count, fault_store_offset);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// strlen stub
// Guest: r3=str → r1=length
static bool emit_native_strlen_stub_a64(translate_ctx_t *ctx, translated_block_t *block) {
    emit_ctx_t *e = &ctx->emit;

    emit_a64_stub_prologue(e);

    // Load guest r3 (string pointer)
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(3));
    // Save guest addr in callee-saved reg for fault reporting.
    emit_mov_w32_w32(e, W23, W0);

    // Bounds check: addr < mem_size
    emit_ldr_w32_imm(e, W3, W20, CPU_MEM_SIZE_OFFSET);
    emit_cmp_w32_w32(e, W0, W3);
    // B.HS → fault (unsigned >=)
    size_t fault_patch_offset = emit_offset(e);
    emit_b_cond(e, COND_HS, 0);  // patch later

    // X0 = X21 + W0, UXTW (host pointer to string start)
    emit_add_x64_x64_w32_uxtw(e, W0, W21, W0);
    // Save host start pointer in callee-saved X19 across the host call.
    emit_mov_x64_x64(e, W19, W0);

    // memchr(str, 0, mem_size - addr)
    emit_mov_w32_imm32(e, W1, 0);          // search for null
    emit_sub_w32(e, W2, W3, W23);          // W2 = mem_size - addr

    emit_a64_call_host(e, (void *)memchr);

    // If NULL (not found) → fault
    emit_cbz_x64(e, W0, 0);  // patch later
    size_t null_patch_offset = emit_offset(e) - 4;

    // length = result - start (both host pointers, 64-bit subtraction)
    emit_sub_x64(e, W0, W0, W19);

    // Store length to guest r1
    emit_str_w32_imm(e, W0, W20, GUEST_REG_OFFSET(1));

    emit_a64_stub_epilogue(e);

    // Fault path (bounds check or null not found)
    size_t fault_offset = emit_offset(e);

    // Patch the B.HS to jump here
    int32_t fault_rel1 = (int32_t)(fault_offset - fault_patch_offset);
    uint32_t *patch1 = (uint32_t *)(e->buf + fault_patch_offset);
    int32_t imm19_1 = fault_rel1 >> 2;
    *patch1 = (*patch1 & ~(0x7FFFF << 5)) | ((imm19_1 & 0x7FFFF) << 5);

    // Patch the CBZ to jump here
    int32_t fault_rel2 = (int32_t)(fault_offset - null_patch_offset);
    uint32_t *patch2 = (uint32_t *)(e->buf + null_patch_offset);
    int32_t imm19_2 = fault_rel2 >> 2;
    *patch2 = (*patch2 & ~(0x7FFFF << 5)) | ((imm19_2 & 0x7FFFF) << 5);

    emit_a64_stub_fault_exit(ctx, EXIT_FAULT_LOAD, W23);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// memswap stub (byte-by-byte swap loop using host pointers)
// Guest: r3=a, r4=b, r5=count, no return value
static bool emit_native_memswap_stub_a64(translate_ctx_t *ctx, translated_block_t *block) {
    emit_ctx_t *e = &ctx->emit;

    emit_a64_stub_prologue(e);

    // Load guest registers
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(3));   // a
    emit_ldr_w32_imm(e, W1, W20, GUEST_REG_OFFSET(4));   // b
    emit_ldr_w32_imm(e, W2, W20, GUEST_REG_OFFSET(5));   // count

    // count == 0 => no-op
    size_t done_patch = emit_offset(e);
    emit_cbz_w32(e, W2, 0);

    // Dynamic checks: both ranges are stores.
    size_t fault_a_patches[8];
    int fault_a_count = 0;
    size_t fault_b_patches[8];
    int fault_b_count = 0;
    emit_stub_range_check_a64(ctx, W0, W2, true, fault_a_patches, &fault_a_count);
    emit_stub_range_check_a64(ctx, W1, W2, true, fault_b_patches, &fault_b_count);

    // Bounds OK — convert to host pointers
    // X9 = X21 + W0, UXTW (host ptr a)
    emit_add_x64_x64_w32_uxtw(e, W9, W21, W0);
    // X10 = X21 + W1, UXTW (host ptr b)
    emit_add_x64_x64_w32_uxtw(e, W10, W21, W1);
    // W2 = count (loop counter)

    // Byte swap loop:
    //   .Lloop:
    //     CBZ W2, .Ldone
    //     LDRB W3, [X9]
    //     LDRB W4, [X10]
    //     STRB W4, [X9]
    //     STRB W3, [X10]
    //     ADD X9, X9, #1
    //     ADD X10, X10, #1
    //     SUB W2, W2, #1
    //     B .Lloop
    //   .Ldone:

    size_t loop_top = emit_offset(e);

    // CBZ W2, .Ldone (patch later)
    size_t cbz_offset = emit_offset(e);
    emit_cbz_w32(e, W2, 0);  // placeholder

    // LDRB W3, [X9, #0]
    emit_ldrb_imm(e, W3, W9, 0);
    // LDRB W4, [X10, #0]
    emit_ldrb_imm(e, W4, W10, 0);
    // STRB W4, [X9, #0]
    emit_strb_imm(e, W4, W9, 0);
    // STRB W3, [X10, #0]
    emit_strb_imm(e, W3, W10, 0);

    // Advance pointers
    emit_add_x64_imm(e, W9, W9, 1);
    emit_add_x64_imm(e, W10, W10, 1);
    // Decrement counter
    emit_sub_w32_imm(e, W2, W2, 1);
    // B .Lloop
    int32_t loop_rel = (int32_t)(loop_top - emit_offset(e));
    emit_b(e, loop_rel);

    // .Ldone: patch CBZ
    size_t done_offset = emit_offset(e);
    int32_t cbz_rel = (int32_t)(done_offset - cbz_offset);
    uint32_t *cbz_patch = (uint32_t *)(e->buf + cbz_offset);
    int32_t cbz_imm19 = cbz_rel >> 2;
    *cbz_patch = (*cbz_patch & ~(0x7FFFF << 5)) | ((cbz_imm19 & 0x7FFFF) << 5);

    patch_imm19_branches(e, &done_patch, 1, done_offset);
    emit_a64_stub_epilogue(e);

    size_t fault_a_offset = emit_offset(e);
    emit_a64_stub_fault_exit(ctx, EXIT_FAULT_STORE, W0);

    size_t fault_b_offset = emit_offset(e);
    emit_a64_stub_fault_exit(ctx, EXIT_FAULT_STORE, W1);

    patch_imm19_branches(e, fault_a_patches, fault_a_count, fault_a_offset);
    patch_imm19_branches(e, fault_b_patches, fault_b_count, fault_b_offset);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// ---- Math function stubs (11 signatures) ----

// SIG_F32_F32: float fn(float)
// Guest: r3=float_bits → r1=float_bits
static bool emit_native_math_f32_unary_a64(translate_ctx_t *ctx, translated_block_t *block,
                                            void *host_fn) {
    emit_ctx_t *e = &ctx->emit;
    emit_a64_stub_prologue(e);

    // Load r3 → W0, then FMOV S0, W0
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(3));
    emit_fmov_s_w32(e, 0, W0);

    emit_a64_call_host(e, host_fn);

    // FMOV W0, S0 → store to r1
    emit_fmov_w32_s(e, W0, 0);
    emit_str_w32_imm(e, W0, W20, GUEST_REG_OFFSET(1));

    emit_a64_stub_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// SIG_F64_F64: double fn(double)
// Guest: r3=low32, r4=high32 → r1=low32, r2=high32
static bool emit_native_math_f64_unary_a64(translate_ctx_t *ctx, translated_block_t *block,
                                            void *host_fn) {
    emit_ctx_t *e = &ctx->emit;
    emit_a64_stub_prologue(e);

    emit_a64_load_f64_args(e, 3, 4);  // r3:r4 → D0

    emit_a64_call_host(e, host_fn);

    emit_a64_store_f64_result(e);  // D0 → r1:r2

    emit_a64_stub_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// SIG_F32_F32_F32: float fn(float, float)
// Guest: r3=x, r4=y → r1=result
static bool emit_native_math_f32_binary_a64(translate_ctx_t *ctx, translated_block_t *block,
                                             void *host_fn) {
    emit_ctx_t *e = &ctx->emit;
    emit_a64_stub_prologue(e);

    // r3 → S0
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(3));
    emit_fmov_s_w32(e, 0, W0);
    // r4 → S1
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(4));
    emit_fmov_s_w32(e, 1, W0);

    emit_a64_call_host(e, host_fn);

    // S0 → r1
    emit_fmov_w32_s(e, W0, 0);
    emit_str_w32_imm(e, W0, W20, GUEST_REG_OFFSET(1));

    emit_a64_stub_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// SIG_F64_F64_F64: double fn(double, double)
// Guest: r3:r4=x, r5:r6=y → r1:r2=result
static bool emit_native_math_f64_binary_a64(translate_ctx_t *ctx, translated_block_t *block,
                                             void *host_fn) {
    emit_ctx_t *e = &ctx->emit;
    emit_a64_stub_prologue(e);

    // r3:r4 → D0
    emit_a64_load_f64_args(e, 3, 4);
    // r5:r6 → D1
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(5));
    emit_ldr_w32_imm(e, W1, W20, GUEST_REG_OFFSET(6));
    emit_orr_x64_lsl(e, W0, W0, W1, 32);
    emit_fmov_d_x64(e, 1, W0);

    emit_a64_call_host(e, host_fn);

    emit_a64_store_f64_result(e);

    emit_a64_stub_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// SIG_F64_F64_I32: double fn(double, int)  — ldexp
// Guest: r3:r4=double, r5=int → r1:r2=result
static bool emit_native_math_f64_i32_a64(translate_ctx_t *ctx, translated_block_t *block,
                                          void *host_fn) {
    emit_ctx_t *e = &ctx->emit;
    emit_a64_stub_prologue(e);

    // r3:r4 → D0
    emit_a64_load_f64_args(e, 3, 4);
    // r5 → W0 (AAPCS64: int arg in W0)
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(5));

    emit_a64_call_host(e, host_fn);

    emit_a64_store_f64_result(e);

    emit_a64_stub_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// SIG_F32_F32_I32: float fn(float, int)  — ldexpf
// Guest: r3=float, r4=int → r1=result
static bool emit_native_math_f32_i32_a64(translate_ctx_t *ctx, translated_block_t *block,
                                          void *host_fn) {
    emit_ctx_t *e = &ctx->emit;
    emit_a64_stub_prologue(e);

    // r3 → S0
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(3));
    emit_fmov_s_w32(e, 0, W0);
    // r4 → W0 (int arg)
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(4));

    emit_a64_call_host(e, host_fn);

    // S0 → r1
    emit_fmov_w32_s(e, W0, 0);
    emit_str_w32_imm(e, W0, W20, GUEST_REG_OFFSET(1));

    emit_a64_stub_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// SIG_F64_F64_DPTR: double fn(double, double*)  — modf
// Guest: r3:r4=double, r5=guest_ptr → r1:r2=result, *ptr written
static bool emit_native_math_f64_dptr_a64(translate_ctx_t *ctx, translated_block_t *block,
                                           void *host_fn) {
    emit_ctx_t *e = &ctx->emit;
    emit_a64_stub_prologue(e);

    // r3:r4 → D0
    emit_a64_load_f64_args(e, 3, 4);
    // r5 -> guest pointer (store 8 bytes)
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(5));
    emit_mov_w32_imm32(e, W2, 8);
    size_t fault_patches[8];
    int fault_count = 0;
    emit_stub_range_check_a64(ctx, W0, W2, true, fault_patches, &fault_count);

    // guest ptr -> host ptr in X0
    emit_add_x64_x64_w32_uxtw(e, W0, W21, W0);

    emit_a64_call_host(e, host_fn);

    emit_a64_store_f64_result(e);

    emit_a64_stub_epilogue(e);

    size_t fault_offset = emit_offset(e);
    emit_a64_stub_fault_exit(ctx, EXIT_FAULT_STORE, W0);
    patch_imm19_branches(e, fault_patches, fault_count, fault_offset);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// SIG_F64_F64_IPTR: double fn(double, int*)  — frexp
// Guest: r3:r4=double, r5=guest_ptr → r1:r2=result, *ptr written
static bool emit_native_math_f64_iptr_a64(translate_ctx_t *ctx, translated_block_t *block,
                                           void *host_fn) {
    emit_ctx_t *e = &ctx->emit;
    emit_a64_stub_prologue(e);

    emit_a64_load_f64_args(e, 3, 4);
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(5));
    emit_mov_w32_imm32(e, W2, 4);
    size_t fault_patches[8];
    int fault_count = 0;
    emit_stub_range_check_a64(ctx, W0, W2, true, fault_patches, &fault_count);

    emit_add_x64_x64_w32_uxtw(e, W0, W21, W0);

    emit_a64_call_host(e, host_fn);

    emit_a64_store_f64_result(e);

    emit_a64_stub_epilogue(e);

    size_t fault_offset = emit_offset(e);
    emit_a64_stub_fault_exit(ctx, EXIT_FAULT_STORE, W0);
    patch_imm19_branches(e, fault_patches, fault_count, fault_offset);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// SIG_F32_F32_FPTR: float fn(float, float*)  — modff
// Guest: r3=float, r4=guest_ptr → r1=result, *ptr written
static bool emit_native_math_f32_fptr_a64(translate_ctx_t *ctx, translated_block_t *block,
                                           void *host_fn) {
    emit_ctx_t *e = &ctx->emit;
    emit_a64_stub_prologue(e);

    // r3 → S0
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(3));
    emit_fmov_s_w32(e, 0, W0);
    // r4 -> guest pointer (store 4 bytes)
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(4));
    emit_mov_w32_imm32(e, W2, 4);
    size_t fault_patches[8];
    int fault_count = 0;
    emit_stub_range_check_a64(ctx, W0, W2, true, fault_patches, &fault_count);

    // guest ptr -> host ptr in X0
    emit_add_x64_x64_w32_uxtw(e, W0, W21, W0);

    emit_a64_call_host(e, host_fn);

    // S0 → r1
    emit_fmov_w32_s(e, W0, 0);
    emit_str_w32_imm(e, W0, W20, GUEST_REG_OFFSET(1));

    emit_a64_stub_epilogue(e);

    size_t fault_offset = emit_offset(e);
    emit_a64_stub_fault_exit(ctx, EXIT_FAULT_STORE, W0);
    patch_imm19_branches(e, fault_patches, fault_count, fault_offset);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// SIG_F32_F32_IPTR2: float fn(float, int*)  — frexpf
// Guest: r3=float, r4=guest_ptr → r1=result, *ptr written
static bool emit_native_math_f32_iptr_a64(translate_ctx_t *ctx, translated_block_t *block,
                                           void *host_fn) {
    emit_ctx_t *e = &ctx->emit;
    emit_a64_stub_prologue(e);

    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(3));
    emit_fmov_s_w32(e, 0, W0);
    emit_ldr_w32_imm(e, W0, W20, GUEST_REG_OFFSET(4));
    emit_mov_w32_imm32(e, W2, 4);
    size_t fault_patches[8];
    int fault_count = 0;
    emit_stub_range_check_a64(ctx, W0, W2, true, fault_patches, &fault_count);

    emit_add_x64_x64_w32_uxtw(e, W0, W21, W0);

    emit_a64_call_host(e, host_fn);

    emit_fmov_w32_s(e, W0, 0);
    emit_str_w32_imm(e, W0, W20, GUEST_REG_OFFSET(1));

    emit_a64_stub_epilogue(e);

    size_t fault_offset = emit_offset(e);
    emit_a64_stub_fault_exit(ctx, EXIT_FAULT_STORE, W0);
    patch_imm19_branches(e, fault_patches, fault_count, fault_offset);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// SIG_I32_F64: int fn(double)  — isnan, isinf, isfinite
// Guest: r3:r4=double → r1=int result
static bool emit_native_math_i32_f64_a64(translate_ctx_t *ctx, translated_block_t *block,
                                          void *host_fn) {
    emit_ctx_t *e = &ctx->emit;
    emit_a64_stub_prologue(e);

    emit_a64_load_f64_args(e, 3, 4);

    emit_a64_call_host(e, host_fn);

    // Result is in W0 (int), store to r1
    emit_str_w32_imm(e, W0, W20, GUEST_REG_OFFSET(1));

    emit_a64_stub_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// Dispatch to the correct math emitter based on signature type
static bool emit_native_math_stub_a64(translate_ctx_t *ctx, translated_block_t *block,
                                       void *host_fn, uint8_t sig) {
    switch (sig) {
        case SIG_F32_F32:      return emit_native_math_f32_unary_a64(ctx, block, host_fn);
        case SIG_F64_F64:      return emit_native_math_f64_unary_a64(ctx, block, host_fn);
        case SIG_F32_F32_F32:  return emit_native_math_f32_binary_a64(ctx, block, host_fn);
        case SIG_F64_F64_F64:  return emit_native_math_f64_binary_a64(ctx, block, host_fn);
        case SIG_F64_F64_I32:  return emit_native_math_f64_i32_a64(ctx, block, host_fn);
        case SIG_F32_F32_I32:  return emit_native_math_f32_i32_a64(ctx, block, host_fn);
        case SIG_F64_F64_DPTR: return emit_native_math_f64_dptr_a64(ctx, block, host_fn);
        case SIG_F64_F64_IPTR: return emit_native_math_f64_iptr_a64(ctx, block, host_fn);
        case SIG_F32_F32_FPTR: return emit_native_math_f32_fptr_a64(ctx, block, host_fn);
        case SIG_F32_F32_IPTR2:return emit_native_math_f32_iptr_a64(ctx, block, host_fn);
        case SIG_I32_F64:      return emit_native_math_i32_f64_a64(ctx, block, host_fn);
        default: return false;
    }
}

// ---- try_emit_intrinsic_a64: main entry point ----

static translated_block_t *try_emit_intrinsic_a64(translate_ctx_t *ctx, uint32_t guest_pc) {
    dbt_cpu_state_t *cpu = ctx->cpu;
    if (!cpu->intrinsics_enabled) return NULL;

    bool (*emitter)(translate_ctx_t *, translated_block_t *) = NULL;
    bool is_memmove = false;
    void *math_fn = NULL;
    uint8_t math_sig = 0;

    if (cpu->intrinsic_memcpy && guest_pc == cpu->intrinsic_memcpy) {
        // Use emit_native_memcpy_stub_a64 with is_memmove=false
    } else if (cpu->intrinsic_memset && guest_pc == cpu->intrinsic_memset) {
        emitter = emit_native_memset_stub_a64;
    } else if (cpu->intrinsic_memmove && guest_pc == cpu->intrinsic_memmove) {
        is_memmove = true;
    } else if (cpu->intrinsic_strlen && guest_pc == cpu->intrinsic_strlen) {
        emitter = emit_native_strlen_stub_a64;
    } else if (cpu->intrinsic_memswap && guest_pc == cpu->intrinsic_memswap) {
        emitter = emit_native_memswap_stub_a64;
    } else {
        // Check math intercept table
        for (int i = 0; i < cpu->num_intercepts; i++) {
            if (cpu->intercepts[i].guest_addr == guest_pc) {
                math_fn = cpu->intercepts[i].host_fn;
                math_sig = cpu->intercepts[i].sig;
                break;
            }
        }
        if (!math_fn) return NULL;
    }

    block_cache_t *cache = ctx->cache;
    if (!cache) return NULL;

    // Allocate block
    translated_block_t *block = cache_alloc_block(cache, guest_pc);
    if (!block) {
        cache_flush(cache);
        block = cache_alloc_block(cache, guest_pc);
        if (!block) return NULL;
    }

    uint8_t *code_start = cache_get_code_ptr(cache);
    if (!code_start) {
        cache_flush(cache);
        block = cache_alloc_block(cache, guest_pc);
        code_start = cache_get_code_ptr(cache);
        if (!code_start || !block) return NULL;
    }

    emit_ctx_t *e = &ctx->emit;
    emit_init(e, code_start, cache->code_buffer_size - cache->code_buffer_used);
    memset(block, 0, sizeof(*block));
    block->guest_pc = guest_pc;
    block->host_code = code_start;

    const char *name = "unknown";
    if (math_fn) name = "math";
    else if (emitter == emit_native_memset_stub_a64) name = "memset";
    else if (emitter == emit_native_strlen_stub_a64) name = "strlen";
    else if (emitter == emit_native_memswap_stub_a64) name = "memswap";
    else if (is_memmove) name = "memmove";
    else name = "memcpy";

    bool ok;
    if (math_fn) {
        ok = emit_native_math_stub_a64(ctx, block, math_fn, math_sig);
    } else if (emitter) {
        ok = emitter(ctx, block);
    } else {
        ok = emit_native_memcpy_stub_a64(ctx, block, is_memmove);
    }

    if (!ok) return NULL;

    native_stub_count++;
    block->host_size = emit_offset(e);
    if (DBT_TRACE) {
        fprintf(stderr, "DBT: Emitted intrinsic stub '%s' at PC=0x%08X (%u bytes)\n",
                name, guest_pc, block->host_size);
    }
    cache_commit_code(cache, block->host_size);
    cache_insert(cache, block);
    cache_chain_incoming(cache, block);
    return block;
}

// ============================================================================
// Stage 2: translate_block_cached
// ============================================================================

translated_block_t *translate_block_cached(translate_ctx_t *ctx, uint32_t guest_pc) {
    // Check for intrinsic recognition before normal translation
    translated_block_t *intrinsic_block = try_emit_intrinsic_a64(ctx, guest_pc);
    if (intrinsic_block) return intrinsic_block;

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
    // Emit deferred side exits (superblock cold stubs)
    if (ctx->deferred_exit_count > 0) {
        emit_deferred_side_exits(ctx);
    }

    block->host_code = (uint8_t *)entry;
    block->host_size = emit_offset(e);
    block->guest_size = (ctx->inst_count + 1) * 4;
    block->reg_cache_hits = ctx->reg_cache_hits;
    block->reg_cache_misses = ctx->reg_cache_misses;

    cache_commit_code(cache, block->host_size);

    // Update superblock metadata
    if (ctx->side_exit_emitted > 0) {
        uint32_t stored = ctx->side_exit_emitted;
        if (stored > MAX_BLOCK_EXITS) stored = MAX_BLOCK_EXITS;
        block->side_exit_count = (uint8_t)stored;
        memcpy(block->side_exit_pcs, ctx->side_exit_pcs, sizeof(uint32_t) * stored);
        cache->superblock_count++;
        cache->side_exit_emitted += ctx->side_exit_emitted;
    }

    // Insert into hash table so cache_lookup can find it
    cache_insert(cache, block);

    // Chain any pending blocks that were waiting for this one
    cache_chain_pending(cache, block);

    ctx->block = NULL;
    return block;
}
