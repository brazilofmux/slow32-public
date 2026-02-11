// SLOW-32 DBT: Block Translator Implementation
// Stage 4 - Superblock extension

#include "translate.h"
#include "block_cache.h"
#include <stdio.h>
#include <string.h>
#include <math.h>

// Debug flag - set to 1 to enable translation tracing
#ifndef DBT_TRACE
#define DBT_TRACE 0
#endif

uint32_t superblock_profile_min_samples = SUPERBLOCK_PROFILE_MIN_SAMPLES;
uint32_t superblock_taken_pct_threshold = SUPERBLOCK_TAKEN_PCT_THRESHOLD;

// ============================================================================
// Instruction decoding
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
            // Sign-extend: low 5 bits from bits [11:7], high 7 bits from bits [31:25]
            inst.imm = (int32_t)(((raw >> 7) & 0x1F) | ((raw >> 20) & 0xFE0));
            // Sign extend from 12 bits
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
            if (inst.imm & 0x100000) inst.imm |= 0xFFE00000;  // Sign-extend
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
            // Unknown opcode - will be handled by fallback
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
    ctx->inline_lookup_enabled = false;  // Stage 3: disabled by default
    ctx->ras_enabled = false;            // Stage 3 Phase 2: disabled by default
    ctx->superblock_enabled = false;     // Stage 4: disabled by default
    ctx->superblock_depth = 0;
    ctx->side_exit_info_enabled = false;
    ctx->avoid_backedge_extend = false;
    ctx->peephole_enabled = false;
}

// ============================================================================
// Stage 5: Per-block register allocation (8-slot fully-associative)
// ============================================================================

static const x64_reg_t reg_alloc_hosts[REG_ALLOC_SLOTS] = {
    RBX, R12, R13, RSI, RDI, R11, R8, R9
};

static void reg_alloc_reset(translate_ctx_t *ctx) {
    memset(ctx->reg_alloc, 0, sizeof(ctx->reg_alloc));
    memset(ctx->reg_alloc_map, -1, sizeof(ctx->reg_alloc_map));
    ctx->reg_cache_hits = 0;
    ctx->reg_cache_misses = 0;
}

// Pre-scan block to determine top-used guest registers for allocation
static void reg_alloc_prescan(translate_ctx_t *ctx, uint32_t start_pc) {
    dbt_cpu_state_t *cpu = ctx->cpu;
    uint32_t use_count[32] = {0};
    uint32_t pc = start_pc;
    int inst_count = 0;
    decoded_inst_t decoded[MAX_BLOCK_INSTS];

    // Initialize dead_temp_skip array
    memset(ctx->dead_temp_skip, 0, sizeof(ctx->dead_temp_skip));
    ctx->loop_written_regs = 0;
    ctx->backedge_target_count = 0;
    ctx->has_backedge = false;
    ctx->backedge_target_pc = 0;

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
            case OP_JAL:
            case OP_JALR:
            case OP_HALT:
            case OP_DEBUG:
            case OP_YIELD:
                is_block_end = true;
                break;
            case OP_BEQ: case OP_BNE: case OP_BLT: case OP_BGE:
            case OP_BLTU: case OP_BGEU:
                // For superblock mode, forward branches continue on fall-through
                if (ctx->superblock_enabled && inst.imm > 0) {
                    // Continue scanning fall-through (mirrors superblock heuristic)
                } else {
                    // Detect in-block back-edge: backward branch targeting within this block
                    if (inst.imm < 0 && ctx->reg_cache_enabled) {
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
                            
                            // Compute registers written in the loop body (target to current)
                            // Used for minimizing dirty snapshots in deferred side exits
                            uint32_t target_idx = (target - start_pc) / 4;
                            // Note: inst_count is the current instruction index
                            for (int k = target_idx; k <= inst_count; k++) {
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
                }
                break;
            default:
                break;
        }
        if (is_block_end) {
            inst_count++;  // Include the block-ending instruction
            break;
        }

        pc += 4;
        inst_count++;
    }

    // Dead temporary elimination: backward liveness scan
    // For each instruction that writes a register, determine if the value
    // has at most 1 read before the next write (making store skippable).
    // Safe with superblocks because pending writes are captured in the
    // deferred side exit snapshot and flushed correctly in cold stubs.
    {
        int read_count_before_write[32];
        for (int r = 0; r < 32; r++)
            read_count_before_write[r] = 2;  // Conservative: assume live at block end

        for (int i = inst_count - 1; i >= 0; i--) {
            decoded_inst_t *dinst = &decoded[i];
            uint8_t wr = 0;   // written register
            uint8_t r1 = 0, r2 = 0;  // read registers

            switch (dinst->format) {
                case FMT_R:
                    wr = dinst->rd;
                    r1 = dinst->rs1;
                    r2 = dinst->rs2;
                    break;
                case FMT_I:
                    wr = dinst->rd;
                    r1 = dinst->rs1;
                    break;
                case FMT_S:
                    r1 = dinst->rs1;
                    r2 = dinst->rs2;
                    break;
                case FMT_B:
                    r1 = dinst->rs1;
                    r2 = dinst->rs2;
                    break;
                case FMT_U:
                    wr = dinst->rd;
                    break;
                case FMT_J:
                    wr = dinst->rd;
                    break;
            }

            // Process write first (backward: write kills liveness)
            if (wr != 0) {
                ctx->dead_temp_skip[i] = (read_count_before_write[wr] <= 1);
                read_count_before_write[wr] = 0;
            }

            // Process reads after (backward: reads establish liveness)
            if (r1 != 0) read_count_before_write[r1]++;
            if (r2 != 0) read_count_before_write[r2]++;
        }
    }

    // Select top REG_ALLOC_SLOTS registers by use count
    for (int s = 0; s < REG_ALLOC_SLOTS; s++) {
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

// Emit prologue: load allocated guest registers into host registers
static void reg_alloc_emit_prologue(translate_ctx_t *ctx) {
    emit_ctx_t *e = &ctx->emit;
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!ctx->reg_alloc[i].allocated) continue;
        emit_mov_r32_m32(e, reg_alloc_hosts[i], RBP,
                         GUEST_REG_OFFSET(ctx->reg_alloc[i].guest_reg));
    }
}

// Dead temporary elimination: flush any pending write to memory
// Forward declarations
static void reg_cache_flush(translate_ctx_t *ctx);

// Return the host register for a cached guest register, or X64_NOREG if not cached.
static inline x64_reg_t guest_host_reg(translate_ctx_t *ctx, uint8_t guest_reg) {
    if (!ctx->reg_cache_enabled || guest_reg == 0) return X64_NOREG;
    int8_t slot = ctx->reg_alloc_map[guest_reg];
    return (slot >= 0) ? reg_alloc_hosts[slot] : X64_NOREG;
}

static void bounds_elim_invalidate(translate_ctx_t *ctx, uint8_t guest_reg);

// Mark a cached guest register as written (dirty) without emitting a MOV.
// Used when loading directly into a cached host register.
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

// Reset all constant propagation state (e.g., at block start or back-edge targets)
static inline void const_prop_reset(translate_ctx_t *ctx) {
    for (int i = 0; i < 32; i++) {
        ctx->reg_constants[i].valid = false;
    }
    ctx->reg_constants[0].valid = true;
    ctx->reg_constants[0].value = 0;
}

// Reset all bounds check elimination state (at block start or back-edge targets)
static inline void bounds_elim_reset(translate_ctx_t *ctx) {
    ctx->validated_range_count = 0;
}

// Check if an access is already covered by a previously validated range.
// Returns: 0 = not covered, 1 = bounds covered (may still need W^X),
//          2 = fully covered (bounds + W^X if needed)
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
                return 1;  // bounds ok, but W^X not yet checked
            }
        }
    }
    return 0;
}

// Record a validated range after emitting a bounds check
static void bounds_elim_record(translate_ctx_t *ctx, uint8_t base_reg,
                               int32_t offset, uint32_t access_size,
                               bool is_store) {
    if (base_reg == 0) return;
    uint32_t new_lo = (uint32_t)offset;
    uint32_t new_hi = (uint32_t)offset + access_size;

    // Try to extend an existing range for the same base register
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
    // New entry
    if (ctx->validated_range_count < MAX_VALIDATED_RANGES) {
        int idx = ctx->validated_range_count++;
        ctx->validated_ranges[idx].guest_reg = base_reg;
        ctx->validated_ranges[idx].lo_offset = new_lo;
        ctx->validated_ranges[idx].hi_end = new_hi;
        ctx->validated_ranges[idx].is_store_ok = is_store;
    }
}

// Invalidate validated ranges when a base register is modified
static void bounds_elim_invalidate(translate_ctx_t *ctx, uint8_t guest_reg) {
    for (int i = 0; i < ctx->validated_range_count; i++) {
        if (ctx->validated_ranges[i].guest_reg == guest_reg) {
            ctx->validated_ranges[i] = ctx->validated_ranges[--ctx->validated_range_count];
            i--;  // re-check this index
        }
    }
}

static inline bool is_backedge_target(translate_ctx_t *ctx, uint32_t pc) {
    if (!ctx->has_backedge) return false;
    for (int i = 0; i < ctx->backedge_target_count; i++) {
        if (ctx->backedge_targets[i] == pc) {
            return true;
        }
    }
    return false;
}

static inline void flush_pending_write(translate_ctx_t *ctx) {
    if (!ctx->pending_write.valid) return;
    emit_mov_m32_r32(&ctx->emit, RBP,
                     GUEST_REG_OFFSET(ctx->pending_write.guest_reg),
                     ctx->pending_write.host_reg);
    ctx->pending_write.valid = false;
    ctx->emit.rax_pending = false;
}

// Materialize a deferred comparison into its destination register
static void materialize_pending_cond(translate_ctx_t *ctx) {
    if (!ctx->pending_cond.valid) return;
    
    emit_ctx_t *e = &ctx->emit;
    uint8_t opcode = ctx->pending_cond.opcode;
    uint8_t rd = ctx->pending_cond.rd;
    uint8_t rs1 = ctx->pending_cond.rs1;
    uint8_t rs2 = ctx->pending_cond.rs2;
    bool rs2_is_imm = ctx->pending_cond.rs2_is_imm;
    int32_t imm = ctx->pending_cond.imm;

    // Reset valid bit BEFORE emitting anything that might trigger another flush/materialize
    ctx->pending_cond.valid = false;

    // Use host registers for comparison operands when available
    x64_reg_t h1 = guest_host_reg(ctx, rs1);
    x64_reg_t cmp_a = (h1 != X64_NOREG) ? h1 : RAX;
    if (h1 == X64_NOREG) emit_load_guest_reg(ctx, RAX, rs1);

    if (rs2_is_imm) {
        // Comparison with immediate (SLTI, SLTIU)
        if (h1 != X64_NOREG) flush_pending_write(ctx);
        emit_cmp_r32_imm32(e, cmp_a, imm);
    } else {
        // Register-register comparison (SLT, SLTU, SEQ, SNE, ...)
        x64_reg_t h2 = guest_host_reg(ctx, rs2);
        x64_reg_t cmp_b = (h2 != X64_NOREG) ? h2 : RCX;
        if (h2 == X64_NOREG) emit_load_guest_reg(ctx, RCX, rs2);
        if (h1 != X64_NOREG) flush_pending_write(ctx);
        emit_cmp_r32_r32(e, cmp_a, cmp_b);
    }

    // Determine correct setcc based on opcode
    void (*emit_setcc_fn)(emit_ctx_t *, x64_reg_t) = NULL;
    switch (opcode) {
        case OP_SLT:  case OP_SLTI:  emit_setcc_fn = emit_setl;  break;
        case OP_SLTU: case OP_SLTIU: emit_setcc_fn = emit_setb;  break;
        case OP_SEQ:                 emit_setcc_fn = emit_sete;  break;
        case OP_SNE:                 emit_setcc_fn = emit_setne; break;
        case OP_SGT:                 emit_setcc_fn = emit_setg;  break;
        case OP_SGTU:                emit_setcc_fn = emit_seta;  break;
        case OP_SLE:                 emit_setcc_fn = emit_setle; break;
        case OP_SLEU:                emit_setcc_fn = emit_setbe; break;
        case OP_SGE:                 emit_setcc_fn = emit_setge; break;
        case OP_SGEU:                emit_setcc_fn = emit_setae; break;
        default:
            fprintf(stderr, "materialize_pending_cond: unknown opcode 0x%02X\n", opcode);
            return;
    }

    if (emit_setcc_fn) {
        emit_setcc_fn(e, RAX);
        emit_movzx_r32_r8(e, RAX, RAX);
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

static inline void flush_pending_cond(translate_ctx_t *ctx) {
    materialize_pending_cond(ctx);
}

static void reg_cache_flush(translate_ctx_t *ctx) {
    flush_pending_cond(ctx);
    flush_pending_write(ctx);
    if (!ctx->reg_cache_enabled) {
        return;
    }
    emit_ctx_t *e = &ctx->emit;
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!ctx->reg_alloc[i].allocated || !ctx->reg_alloc[i].dirty) {
            continue;
        }
        emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(ctx->reg_alloc[i].guest_reg),
                         reg_alloc_hosts[i]);
        // NOTE: Do NOT clear dirty here! Multiple code paths (e.g., branch
        // taken/fall-through, inline lookup probes) all call flush, but only one
        // executes at runtime. Each path must emit its own flush code.
    }
}

// Flush using a full allocation snapshot (for deferred side exits)
// Uses the snapshotted guest_reg mapping, NOT the current ctx->reg_alloc,
// because superblock continuation may have evicted/reassigned slots since
// the snapshot was captured.
// NOTE: Caller must handle pending write flush separately using the snapshot.
static void reg_alloc_flush_snapshot(translate_ctx_t *ctx,
                                      const bool *dirty_snapshot,
                                      const bool *allocated_snapshot,
                                      const uint8_t *guest_reg_snapshot) {
    emit_ctx_t *e = &ctx->emit;
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!allocated_snapshot[i] || !dirty_snapshot[i]) {
            continue;
        }
        emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(guest_reg_snapshot[i]),
                         reg_alloc_hosts[i]);
    }
}

// Flush specific host registers that are about to be used as scratch.
// Writes dirty cached values back to memory but keeps slots allocated (dirty=false).
// Used before JALR/RAS code that clobbers R8/R9 as temporaries.
static void flush_cached_host_regs(translate_ctx_t *ctx, x64_reg_t r1, x64_reg_t r2) {
    if (!ctx->reg_cache_enabled) return;
    emit_ctx_t *e = &ctx->emit;
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!ctx->reg_alloc[i].allocated) continue;
        if (reg_alloc_hosts[i] == r1 || reg_alloc_hosts[i] == r2) {
            if (ctx->reg_alloc[i].dirty) {
                emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(ctx->reg_alloc[i].guest_reg),
                                 reg_alloc_hosts[i]);
            }
            // Fully evict from cache so the host register can be reused as scratch.
            // Without this, emit_store_guest_reg may still target this host register,
            // clobbering the scratch value (e.g., JALR target saved in R8).
            ctx->reg_alloc_map[ctx->reg_alloc[i].guest_reg] = -1;
            ctx->reg_alloc[i].allocated = false;
            ctx->reg_alloc[i].dirty = false;
        }
    }
}

// Load guest register into x86-64 register
void emit_load_guest_reg(translate_ctx_t *ctx, x64_reg_t dst, uint8_t guest_reg) {
    emit_ctx_t *e = &ctx->emit;

    if (guest_reg == 0) {
        // r0 is always 0 — flush pending if it would clobber dst
        if (ctx->pending_write.valid && dst == ctx->pending_write.host_reg) {
            flush_pending_write(ctx);
        }
        emit_xor_r32_r32(e, dst, dst);
        return;
    }

    // Dead temporary elimination: check pending write
    if (ctx->pending_write.valid) {
        if (guest_reg == ctx->pending_write.guest_reg) {
            // CONSUME: value is already in pending.host_reg
            x64_reg_t preg = ctx->pending_write.host_reg;
            if (!ctx->pending_write.can_skip_store) {
                // Level 1: still need memory store for later reads
                emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(guest_reg), preg);
            }
            // Level 2: if can_skip_store, skip entirely
            ctx->pending_write.valid = false;
            ctx->emit.rax_pending = false;
            if (dst != preg) {
                emit_mov_r32_r32(e, dst, preg);
            }
            ctx->reg_cache_hits++;
            return;
        }
        if (dst == ctx->pending_write.host_reg) {
            // About to clobber the pending register — flush first
            flush_pending_write(ctx);
        }
    }

    if (!ctx->reg_cache_enabled) {
        // Load from CPU state: mov dst, [rbp + offset]
        const char *saved_tag = e->trace_tag;
        if (e->trace_enabled) {
            e->trace_tag = "guest_load";
        }
        emit_mov_r32_m32(e, dst, RBP, GUEST_REG_OFFSET(guest_reg));
        e->trace_tag = saved_tag;
        return;
    }

    int8_t slot = ctx->reg_alloc_map[guest_reg];
    if (slot >= 0) {
        // Allocated — use host register directly
        ctx->reg_cache_hits++;
        x64_reg_t host_reg = reg_alloc_hosts[slot];
        if (dst != host_reg) {
            emit_mov_r32_r32(e, dst, host_reg);
        }
    } else {
        // Not allocated — memory fallback
        ctx->reg_cache_misses++;
        const char *saved_tag = e->trace_tag;
        if (e->trace_enabled) {
            e->trace_tag = "guest_load";
        }
        emit_mov_r32_m32(e, dst, RBP, GUEST_REG_OFFSET(guest_reg));
        e->trace_tag = saved_tag;
    }
}

// Store x86-64 register to guest register
void emit_store_guest_reg(translate_ctx_t *ctx, uint8_t guest_reg, x64_reg_t src) {
    emit_ctx_t *e = &ctx->emit;
    if (guest_reg == 0) {
        // Writes to r0 are discarded
        return;
    }

    // Constant propagation: non-constant write invalidates known value
    ctx->reg_constants[guest_reg].valid = false;

    // Bounds check elimination: invalidate ranges using this register as base
    bounds_elim_invalidate(ctx, guest_reg);

    // If there's a pending write for the same guest register, discard it
    if (ctx->pending_write.valid && ctx->pending_write.guest_reg == guest_reg) {
        ctx->pending_write.valid = false;
        ctx->emit.rax_pending = false;
    }

    if (!ctx->reg_cache_enabled) {
        // Dead temporary elimination: defer RAX stores as pending writes
        if (src == RAX) {
            flush_pending_write(ctx);  // Flush any previous pending
            ctx->pending_write.guest_reg = guest_reg;
            ctx->pending_write.host_reg = RAX;
            ctx->pending_write.valid = true;
            ctx->emit.rax_pending = true;
            ctx->pending_write.can_skip_store = ctx->dead_temp_skip[ctx->current_inst_idx];
            return;
        }
        // Non-RAX stores (e.g., RCX from loads) — store normally
        const char *saved_tag = e->trace_tag;
        if (e->trace_enabled) {
            e->trace_tag = "guest_store";
        }
        emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(guest_reg), src);
        e->trace_tag = saved_tag;
        return;
    }

    int8_t slot = ctx->reg_alloc_map[guest_reg];
    if (slot >= 0) {
        // Allocated — move into host register, mark dirty
        ctx->reg_cache_hits++;
        x64_reg_t host_reg = reg_alloc_hosts[slot];
        if (src != host_reg) {
            emit_mov_r32_r32(e, host_reg, src);
        }
        ctx->reg_alloc[slot].dirty = true;
    } else {
        // Not allocated — defer RAX stores as pending writes
        if (src == RAX) {
            flush_pending_write(ctx);
            ctx->pending_write.guest_reg = guest_reg;
            ctx->pending_write.host_reg = RAX;
            ctx->pending_write.valid = true;
            ctx->emit.rax_pending = true;
            ctx->pending_write.can_skip_store = ctx->dead_temp_skip[ctx->current_inst_idx];
            return;
        }
        ctx->reg_cache_misses++;
        const char *saved_tag = e->trace_tag;
        if (e->trace_enabled) {
            e->trace_tag = "guest_store";
        }
        emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(guest_reg), src);
        e->trace_tag = saved_tag;
    }
}

void emit_store_guest_reg_imm32(translate_ctx_t *ctx, uint8_t guest_reg, uint32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    if (guest_reg == 0) {
        return;
    }

    // Constant propagation: record the known value
    ctx->reg_constants[guest_reg].valid = true;
    ctx->reg_constants[guest_reg].value = imm;

    // Bounds check elimination: invalidate ranges using this register as base
    bounds_elim_invalidate(ctx, guest_reg);

    // Discard pending write for same register (being overwritten)
    if (ctx->pending_write.valid && ctx->pending_write.guest_reg == guest_reg) {
        ctx->pending_write.valid = false;
        ctx->emit.rax_pending = false;
    }
    if (!ctx->reg_cache_enabled) {
        const char *saved_tag = e->trace_tag;
        if (e->trace_enabled) {
            e->trace_tag = "guest_store_imm";
        }
        emit_mov_m32_imm32(e, RBP, GUEST_REG_OFFSET(guest_reg), imm);
        e->trace_tag = saved_tag;
        return;
    }

    int8_t slot = ctx->reg_alloc_map[guest_reg];
    if (slot >= 0) {
        // Allocated — mov imm into host register, mark dirty
        ctx->reg_cache_hits++;
        emit_mov_r32_imm32(e, reg_alloc_hosts[slot], imm);
        ctx->reg_alloc[slot].dirty = true;
    } else {
        // Not allocated — memory fallback
        ctx->reg_cache_misses++;
        const char *saved_tag = e->trace_tag;
        if (e->trace_enabled) {
            e->trace_tag = "guest_store_imm";
        }
        emit_mov_m32_imm32(e, RBP, GUEST_REG_OFFSET(guest_reg), imm);
        e->trace_tag = saved_tag;
    }
}

// Emit exit sequence
void emit_exit(translate_ctx_t *ctx, exit_reason_t reason, uint32_t next_pc) {
    emit_ctx_t *e = &ctx->emit;
    const char *saved_tag = e->trace_tag;

    reg_cache_flush(ctx);  // also flushes pending_cond

    // Store next PC
    if (e->trace_enabled) {
        e->trace_tag = "exit_pc";
    }
    emit_mov_m32_imm32(e, RBP, CPU_PC_OFFSET, next_pc);

    // Store exit reason
    if (e->trace_enabled) {
        e->trace_tag = "exit_reason";
    }
    emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, reason);
    e->trace_tag = saved_tag;

    // Return to dispatcher
    emit_ret(e);
}

// Emit exit with info
void emit_exit_with_info(translate_ctx_t *ctx, exit_reason_t reason,
                         uint32_t next_pc, uint32_t info) {
    emit_ctx_t *e = &ctx->emit;
    const char *saved_tag = e->trace_tag;

    reg_cache_flush(ctx);

    // Store next PC
    if (e->trace_enabled) {
        e->trace_tag = "exit_pc";
    }
    emit_mov_m32_imm32(e, RBP, CPU_PC_OFFSET, next_pc);

    // Store exit reason
    if (e->trace_enabled) {
        e->trace_tag = "exit_reason";
    }
    emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, reason);

    // Store exit info
    if (e->trace_enabled) {
        e->trace_tag = "exit_info";
    }
    emit_mov_m32_imm32(e, RBP, CPU_EXIT_INFO_OFFSET, info);
    e->trace_tag = saved_tag;

    emit_ret(e);
}

static void emit_exit_with_info_reg(translate_ctx_t *ctx, exit_reason_t reason,
                                    uint32_t next_pc, x64_reg_t info_reg) {
    emit_ctx_t *e = &ctx->emit;
    const char *saved_tag = e->trace_tag;

    reg_cache_flush(ctx);

    if (e->trace_enabled) {
        e->trace_tag = "exit_pc";
    }
    emit_mov_m32_imm32(e, RBP, CPU_PC_OFFSET, next_pc);

    if (e->trace_enabled) {
        e->trace_tag = "exit_reason";
    }
    emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, reason);

    if (e->trace_enabled) {
        e->trace_tag = "exit_info";
    }
    emit_mov_m32_r32(e, RBP, CPU_EXIT_INFO_OFFSET, info_reg);
    e->trace_tag = saved_tag;

    emit_ret(e);
}

// ============================================================================
// Arithmetic translations
// ============================================================================

void translate_add(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // Constant folding: both operands known
    if (ctx->reg_constants[rs1].valid && ctx->reg_constants[rs2].valid) {
        uint32_t result = ctx->reg_constants[rs1].value + ctx->reg_constants[rs2].value;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        // Use host register directly as accumulator
        if (rs2 == 0) {
            emit_load_guest_reg(ctx, hd, rs1);
        } else if (rd == rs1) {
            // hd already has rs1
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_add_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_add_r32_r32(e, hd, RCX); }
        } else if (rd == rs2) {
            // Commutative: hd already has rs2, add rs1
            x64_reg_t h1 = guest_host_reg(ctx, rs1);
            if (h1 != X64_NOREG) emit_add_r32_r32(e, hd, h1);
            else { emit_load_guest_reg(ctx, RAX, rs1); emit_add_r32_r32(e, hd, RAX); }
        } else {
            emit_load_guest_reg(ctx, hd, rs1);
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_add_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_add_r32_r32(e, hd, RCX); }
        }
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (rs2 != 0) {
            emit_load_guest_reg(ctx, RCX, rs2);
            emit_add_r32_r32(e, RAX, RCX);
        }
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_sub(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // Constant folding: both operands known
    if (ctx->reg_constants[rs1].valid && ctx->reg_constants[rs2].valid) {
        uint32_t result = ctx->reg_constants[rs1].value - ctx->reg_constants[rs2].value;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        if (rs2 == 0) {
            emit_load_guest_reg(ctx, hd, rs1);
        } else if (rd == rs1) {
            // hd already has rs1
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_sub_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_sub_r32_r32(e, hd, RCX); }
        } else if (rd == rs2) {
            // rd == rs2: hd holds rs2; need rs1 - rs2 in hd.
            x64_reg_t h1 = guest_host_reg(ctx, rs1);
            if (h1 != X64_NOREG) {
                // Both cached: NEG + ADD (2 insns instead of 3)
                emit_neg_r32(e, hd);
                emit_add_r32_r32(e, hd, h1);
            } else {
                // rs1 not cached: save hd, load rs1, subtract
                emit_mov_r32_r32(e, RCX, hd);
                emit_load_guest_reg(ctx, hd, rs1);
                emit_sub_r32_r32(e, hd, RCX);
            }
        } else {
            // rd != rs1 && rd != rs2: safe to overwrite hd
            emit_load_guest_reg(ctx, hd, rs1);
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_sub_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_sub_r32_r32(e, hd, RCX); }
        }
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (rs2 != 0) {
            emit_load_guest_reg(ctx, RCX, rs2);
            emit_sub_r32_r32(e, RAX, RCX);
        }
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_addi(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, (uint32_t)imm);
        return;
    }

    // Constant folding: if rs1 is a known constant, compute result at translate time
    if (ctx->reg_constants[rs1].valid) {
        uint32_t result = ctx->reg_constants[rs1].value + (uint32_t)imm;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        emit_load_guest_reg(ctx, hd, rs1); // no-op if rd == rs1
        if (imm != 0) {
            emit_add_r32_imm32(e, hd, imm);
        }
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (imm != 0) {
            emit_add_r32_imm32(e, RAX, imm);
        }
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

// ============================================================================
// Logical translations
// ============================================================================

void translate_and(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0 || rs2 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, 0);
        return;
    }

    // Constant folding: both operands known
    if (ctx->reg_constants[rs1].valid && ctx->reg_constants[rs2].valid) {
        uint32_t result = ctx->reg_constants[rs1].value & ctx->reg_constants[rs2].value;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        if (rd == rs1) {
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_and_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_and_r32_r32(e, hd, RCX); }
        } else if (rd == rs2) {
            x64_reg_t h1 = guest_host_reg(ctx, rs1);
            if (h1 != X64_NOREG) emit_and_r32_r32(e, hd, h1);
            else { emit_load_guest_reg(ctx, RAX, rs1); emit_and_r32_r32(e, hd, RAX); }
        } else {
            emit_load_guest_reg(ctx, hd, rs1);
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_and_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_and_r32_r32(e, hd, RCX); }
        }
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_and_r32_r32(e, RAX, RCX);
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_or(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // Constant folding: both operands known
    if (ctx->reg_constants[rs1].valid && ctx->reg_constants[rs2].valid) {
        uint32_t result = ctx->reg_constants[rs1].value | ctx->reg_constants[rs2].value;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        if (rs2 == 0) {
            emit_load_guest_reg(ctx, hd, rs1);
        } else if (rd == rs1) {
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_or_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_or_r32_r32(e, hd, RCX); }
        } else if (rd == rs2) {
            x64_reg_t h1 = guest_host_reg(ctx, rs1);
            if (h1 != X64_NOREG) emit_or_r32_r32(e, hd, h1);
            else { emit_load_guest_reg(ctx, RAX, rs1); emit_or_r32_r32(e, hd, RAX); }
        } else {
            emit_load_guest_reg(ctx, hd, rs1);
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_or_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_or_r32_r32(e, hd, RCX); }
        }
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (rs2 != 0) {
            emit_load_guest_reg(ctx, RCX, rs2);
            emit_or_r32_r32(e, RAX, RCX);
        }
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_xor(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // Constant folding: both operands known
    if (ctx->reg_constants[rs1].valid && ctx->reg_constants[rs2].valid) {
        uint32_t result = ctx->reg_constants[rs1].value ^ ctx->reg_constants[rs2].value;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        if (rs2 == 0) {
            emit_load_guest_reg(ctx, hd, rs1);
        } else if (rd == rs1) {
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_xor_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_xor_r32_r32(e, hd, RCX); }
        } else if (rd == rs2) {
            x64_reg_t h1 = guest_host_reg(ctx, rs1);
            if (h1 != X64_NOREG) emit_xor_r32_r32(e, hd, h1);
            else { emit_load_guest_reg(ctx, RAX, rs1); emit_xor_r32_r32(e, hd, RAX); }
        } else {
            emit_load_guest_reg(ctx, hd, rs1);
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_xor_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_xor_r32_r32(e, hd, RCX); }
        }
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (rs2 != 0) {
            emit_load_guest_reg(ctx, RCX, rs2);
            emit_xor_r32_r32(e, RAX, RCX);
        }
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_andi(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, 0);
        return;
    }

    // Constant folding
    if (ctx->reg_constants[rs1].valid) {
        uint32_t result = ctx->reg_constants[rs1].value & (uint32_t)imm;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        emit_load_guest_reg(ctx, hd, rs1);
        emit_and_r32_imm32(e, hd, imm);
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        emit_and_r32_imm32(e, RAX, imm);
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_ori(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, (uint32_t)imm);
        return;
    }

    // Constant folding
    if (ctx->reg_constants[rs1].valid) {
        uint32_t result = ctx->reg_constants[rs1].value | (uint32_t)imm;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        emit_load_guest_reg(ctx, hd, rs1);
        if (imm != 0) {
            emit_or_r32_imm32(e, hd, imm);
        }
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (imm != 0) {
            emit_or_r32_imm32(e, RAX, imm);
        }
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_xori(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, (uint32_t)imm);
        return;
    }

    // Constant folding
    if (ctx->reg_constants[rs1].valid) {
        uint32_t result = ctx->reg_constants[rs1].value ^ (uint32_t)imm;
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        emit_load_guest_reg(ctx, hd, rs1);
        if (imm != 0) {
            emit_xor_r32_imm32(e, hd, imm);
        }
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (imm != 0) {
            emit_xor_r32_imm32(e, RAX, imm);
        }
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

// ============================================================================
// Shift translations
// ============================================================================

void translate_sll(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        if (rs2 != 0) emit_load_guest_reg(ctx, RCX, rs2);
        emit_load_guest_reg(ctx, hd, rs1);
        if (rs2 != 0) emit_shl_r32_cl(e, hd);
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (rs2 != 0) {
            emit_load_guest_reg(ctx, RCX, rs2);
            emit_shl_r32_cl(e, RAX);
        }
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_srl(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        if (rs2 != 0) emit_load_guest_reg(ctx, RCX, rs2);
        emit_load_guest_reg(ctx, hd, rs1);
        if (rs2 != 0) emit_shr_r32_cl(e, hd);
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (rs2 != 0) {
            emit_load_guest_reg(ctx, RCX, rs2);
            emit_shr_r32_cl(e, RAX);
        }
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_sra(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        if (rs2 != 0) emit_load_guest_reg(ctx, RCX, rs2);
        emit_load_guest_reg(ctx, hd, rs1);
        if (rs2 != 0) emit_sar_r32_cl(e, hd);
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (rs2 != 0) {
            emit_load_guest_reg(ctx, RCX, rs2);
            emit_sar_r32_cl(e, RAX);
        }
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_slli(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // Constant folding
    if (ctx->reg_constants[rs1].valid) {
        uint32_t result = ctx->reg_constants[rs1].value << (imm & 0x1F);
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        emit_load_guest_reg(ctx, hd, rs1);
        if ((imm & 0x1F) != 0) emit_shl_r32_imm8(e, hd, imm & 0x1F);
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if ((imm & 0x1F) != 0) emit_shl_r32_imm8(e, RAX, imm & 0x1F);
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_srli(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // Constant folding
    if (ctx->reg_constants[rs1].valid) {
        uint32_t result = ctx->reg_constants[rs1].value >> (imm & 0x1F);
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        emit_load_guest_reg(ctx, hd, rs1);
        if ((imm & 0x1F) != 0) emit_shr_r32_imm8(e, hd, imm & 0x1F);
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if ((imm & 0x1F) != 0) emit_shr_r32_imm8(e, RAX, imm & 0x1F);
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_srai(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // Constant folding (arithmetic right shift)
    if (ctx->reg_constants[rs1].valid) {
        uint32_t result = (uint32_t)((int32_t)ctx->reg_constants[rs1].value >> (imm & 0x1F));
        emit_store_guest_reg_imm32(ctx, rd, result);
        return;
    }

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        emit_load_guest_reg(ctx, hd, rs1);
        if ((imm & 0x1F) != 0) emit_sar_r32_imm8(e, hd, imm & 0x1F);
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if ((imm & 0x1F) != 0) emit_sar_r32_imm8(e, RAX, imm & 0x1F);
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

// ============================================================================
// Comparison translations
// ============================================================================

// Peephole lookahead: check if the next instruction is a BEQ/BNE testing 'rd'
// against r0, which enables compare-branch fusion.
static bool can_fuse_with_next(translate_ctx_t *ctx, uint8_t rd) {
    dbt_cpu_state_t *cpu = ctx->cpu;
    uint32_t next_pc = ctx->guest_pc + 4;

    // Bounds check: need 4 bytes for the next instruction
    if (next_pc + 4 > cpu->code_limit) return false;

    uint32_t raw = *(uint32_t *)(cpu->mem_base + next_pc);
    uint8_t opcode = raw & 0x7F;

    // Only BEQ and BNE can be fused
    if (opcode != OP_BEQ && opcode != OP_BNE) return false;

    // Extract rs1 and rs2 from B-type encoding
    uint8_t rs1 = (raw >> 15) & 0x1F;
    uint8_t rs2 = (raw >> 20) & 0x1F;

    // Pattern: Bxx rd, r0, target  (testing comparison result against zero)
    return (rs1 == rd && rs2 == 0) || (rs2 == rd && rs1 == 0);
}

// Emit a comparison immediately (non-deferred path)
static void emit_cmp_set_immediate(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2,
                                   void (*emit_setcc)(emit_ctx_t *, x64_reg_t)) {
    emit_ctx_t *e = &ctx->emit;

    // Use host registers for comparison operands when available
    x64_reg_t h1 = guest_host_reg(ctx, rs1);
    x64_reg_t h2 = guest_host_reg(ctx, rs2);
    x64_reg_t cmp_a = (h1 != X64_NOREG) ? h1 : RAX;
    x64_reg_t cmp_b = (h2 != X64_NOREG) ? h2 : RCX;
    if (h1 == X64_NOREG) emit_load_guest_reg(ctx, RAX, rs1);
    if (h2 == X64_NOREG) emit_load_guest_reg(ctx, RCX, rs2);
    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        // SETcc + MOVZX directly into cached host register
        emit_cmp_r32_r32(e, cmp_a, cmp_b);
        emit_setcc(e, hd);
        emit_movzx_r32_r8(e, hd, hd);
        reg_cache_mark_written(ctx, rd);
    } else {
        // Flush pending write before clobbering RAX with setcc result
        if (h1 != X64_NOREG) flush_pending_write(ctx);
        emit_cmp_r32_r32(e, cmp_a, cmp_b);
        emit_setcc(e, RAX);
        emit_movzx_r32_r8(e, RAX, RAX);
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

// Helper for compare and set
static void translate_cmp_set(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2,
                              uint8_t opcode) {
    if (rd == 0) return;

    // Only defer if the next instruction is a fusible BEQ/BNE
    if (can_fuse_with_next(ctx, rd)) {
        // Flush any existing pending condition
        flush_pending_cond(ctx);

        // Defer the comparison for fusion with the next branch
        ctx->pending_cond.opcode = opcode;
        ctx->pending_cond.rd = rd;
        ctx->pending_cond.rs1 = rs1;
        ctx->pending_cond.rs2 = rs2;
        ctx->pending_cond.rs2_is_imm = false;
        ctx->pending_cond.inst_idx = ctx->current_inst_idx;
        ctx->pending_cond.valid = true;
        // Invalidate constant for rd — the comparison writes rd even though
        // the store is deferred for fusion
        ctx->reg_constants[rd].valid = false;
        return;
    }

    // Not fusible — emit the comparison immediately
    void (*emit_setcc_fn)(emit_ctx_t *, x64_reg_t) = NULL;
    switch (opcode) {
        case OP_SLT:  emit_setcc_fn = emit_setl;  break;
        case OP_SLTU: emit_setcc_fn = emit_setb;  break;
        case OP_SEQ:  emit_setcc_fn = emit_sete;  break;
        case OP_SNE:  emit_setcc_fn = emit_setne; break;
        case OP_SGT:  emit_setcc_fn = emit_setg;  break;
        case OP_SGTU: emit_setcc_fn = emit_seta;  break;
        case OP_SLE:  emit_setcc_fn = emit_setle; break;
        case OP_SLEU: emit_setcc_fn = emit_setbe; break;
        case OP_SGE:  emit_setcc_fn = emit_setge; break;
        case OP_SGEU: emit_setcc_fn = emit_setae; break;
    }
    emit_cmp_set_immediate(ctx, rd, rs1, rs2, emit_setcc_fn);
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

    if (can_fuse_with_next(ctx, rd)) {
        flush_pending_cond(ctx);
        ctx->pending_cond.opcode = OP_SLTI;
        ctx->pending_cond.rd = rd;
        ctx->pending_cond.rs1 = rs1;
        ctx->pending_cond.rs2_is_imm = true;
        ctx->pending_cond.imm = imm;
        ctx->pending_cond.inst_idx = ctx->current_inst_idx;
        ctx->pending_cond.valid = true;
        ctx->reg_constants[rd].valid = false;
        return;
    }

    // Not fusible — emit immediately
    emit_ctx_t *e = &ctx->emit;
    x64_reg_t h1 = guest_host_reg(ctx, rs1);
    if (h1 != X64_NOREG) {
        flush_pending_write(ctx);
        emit_cmp_r32_imm32(e, h1, imm);
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        emit_cmp_r32_imm32(e, RAX, imm);
    }
    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        emit_setl(e, hd);
        emit_movzx_r32_r8(e, hd, hd);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_setl(e, RAX);
        emit_movzx_r32_r8(e, RAX, RAX);
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_sltiu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;

    if (can_fuse_with_next(ctx, rd)) {
        flush_pending_cond(ctx);
        ctx->pending_cond.opcode = OP_SLTIU;
        ctx->pending_cond.rd = rd;
        ctx->pending_cond.rs1 = rs1;
        ctx->pending_cond.rs2_is_imm = true;
        ctx->pending_cond.imm = imm;
        ctx->pending_cond.inst_idx = ctx->current_inst_idx;
        ctx->pending_cond.valid = true;
        ctx->reg_constants[rd].valid = false;
        return;
    }

    // Not fusible — emit immediately
    emit_ctx_t *e = &ctx->emit;
    x64_reg_t h1 = guest_host_reg(ctx, rs1);
    if (h1 != X64_NOREG) {
        flush_pending_write(ctx);
        emit_cmp_r32_imm32(e, h1, imm);
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        emit_cmp_r32_imm32(e, RAX, imm);
    }
    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        emit_setb(e, hd);
        emit_movzx_r32_r8(e, hd, hd);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_setb(e, RAX);
        emit_movzx_r32_r8(e, RAX, RAX);
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

// ============================================================================
// Multiply/Divide translations
// ============================================================================

void translate_mul(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    x64_reg_t hd = guest_host_reg(ctx, rd);
    if (hd != X64_NOREG) {
        // IMUL r32, r32 works with any registers; commutative
        if (rd == rs1) {
            // hd already has rs1
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_imul_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_imul_r32_r32(e, hd, RCX); }
        } else if (rd == rs2) {
            // hd already has rs2; commutative so IMUL hd, rs1
            x64_reg_t h1 = guest_host_reg(ctx, rs1);
            if (h1 != X64_NOREG) emit_imul_r32_r32(e, hd, h1);
            else { emit_load_guest_reg(ctx, RAX, rs1); emit_imul_r32_r32(e, hd, RAX); }
        } else {
            emit_load_guest_reg(ctx, hd, rs1);
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) emit_imul_r32_r32(e, hd, h2);
            else { emit_load_guest_reg(ctx, RCX, rs2); emit_imul_r32_r32(e, hd, RCX); }
        }
        ctx->reg_alloc[ctx->reg_alloc_map[rd]].dirty = true;
        ctx->reg_constants[rd].valid = false;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_imul_r32_r32(e, RAX, RCX);
        emit_store_guest_reg(ctx, rd, RAX);
    }
}

void translate_mulh(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_imul_one_r32(e, RCX);  // edx:eax = eax * ecx (signed)
    emit_mov_r32_r32(e, RAX, RDX);  // High part in edx
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_mulhu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_mul_r32(e, RCX);  // edx:eax = eax * ecx (unsigned)
    emit_mov_r32_r32(e, RAX, RDX);  // High part in edx
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_div(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    emit_load_guest_reg(ctx, RCX, rs2);

    // Guard: INT32_MIN / -1 would crash the host (x86 SIGFPE)
    emit_cmp_r32_imm32(e, RCX, -1);
    emit_jne_rel32(e, 0);  // patch later
    size_t patch_not_neg1 = e->offset - 4;  // displacement is last 4 bytes
    emit_cmp_r32_imm32(e, RAX, (int32_t)0x80000000);
    emit_jne_rel32(e, 0);  // patch later
    size_t patch_not_intmin = e->offset - 4;
    // INT32_MIN / -1 = INT32_MIN
    flush_pending_write(ctx);  // About to clobber RAX
    emit_mov_r32_imm32(e, RAX, 0x80000000);
    emit_jmp_rel32(e, 0);  // patch later
    size_t patch_skip_idiv = e->offset - 4;

    emit_patch_rel32(e, patch_not_neg1, e->offset);
    emit_patch_rel32(e, patch_not_intmin, e->offset);
    emit_cdq(e);
    emit_idiv_r32(e, RCX);

    emit_patch_rel32(e, patch_skip_idiv, e->offset);
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_rem(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    emit_load_guest_reg(ctx, RCX, rs2);

    // Guard: INT32_MIN % -1 would crash the host (x86 SIGFPE)
    emit_cmp_r32_imm32(e, RCX, -1);
    emit_jne_rel32(e, 0);  // patch later
    size_t patch_not_neg1 = e->offset - 4;  // displacement is last 4 bytes
    emit_cmp_r32_imm32(e, RAX, (int32_t)0x80000000);
    emit_jne_rel32(e, 0);  // patch later
    size_t patch_not_intmin = e->offset - 4;
    // INT32_MIN % -1 = 0
    flush_pending_write(ctx);  // About to clobber RAX
    emit_xor_r32_r32(e, RAX, RAX);
    emit_jmp_rel32(e, 0);  // patch later
    size_t patch_skip_idiv = e->offset - 4;

    emit_patch_rel32(e, patch_not_neg1, e->offset);
    emit_patch_rel32(e, patch_not_intmin, e->offset);
    emit_cdq(e);
    emit_idiv_r32(e, RCX);
    emit_mov_r32_r32(e, RAX, RDX);

    emit_patch_rel32(e, patch_skip_idiv, e->offset);
    emit_store_guest_reg(ctx, rd, RAX);
}

// ============================================================================
// Upper immediate
// ============================================================================

void translate_lui(translate_ctx_t *ctx, uint8_t rd, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_store_guest_reg_imm32(ctx, rd, (uint32_t)imm);
}

// ============================================================================
// Memory translations
// ============================================================================

static void emit_mem_access_check(translate_ctx_t *ctx, x64_reg_t addr_reg,
                                  uint32_t access_size,
                                  exit_reason_t reason, bool is_store,
                                  uint8_t base_guest_reg, int32_t offset) {
    emit_ctx_t *e = &ctx->emit;
    dbt_cpu_state_t *cpu = ctx->cpu;

    // -U flag: skip all checks (unsafe, for benchmarking)
    if (cpu->bounds_checks_disabled) return;

    // Bounds check elimination: skip if already validated
    int coverage = bounds_elim_lookup(ctx, base_guest_reg, offset, access_size, is_store);
    if (coverage == 2) {
        // Fully covered — skip all checks
        return;
    }
    bool skip_bounds = (coverage == 1);

    size_t fault_jumps[8];
    int fault_count = 0;
    size_t ok_jumps[8];
    int ok_count = 0;

    if (cpu->mem_size < access_size) {
        emit_exit_with_info_reg(ctx, reason, ctx->guest_pc, addr_reg);
        return;
    }

    // 1. Bounds check — skip if already validated for this base+offset range
    if (!skip_bounds) {
        uint32_t limit = cpu->mem_size - access_size;
        emit_cmp_r32_imm32(e, addr_reg, (int32_t)limit);
        fault_jumps[fault_count++] = emit_offset(e) + 2;
        emit_ja_rel32(e, 0);

        // 2. Alignment check — only when align_traps_enabled && access_size > 1
        if (cpu->align_traps_enabled && access_size > 1) {
            emit_mov_r32_r32(e, RDX, addr_reg);
            emit_and_r32_imm32(e, RDX, (int32_t)(access_size - 1));
            fault_jumps[fault_count++] = emit_offset(e) + 2;
            emit_jne_rel32(e, 0);
        }

        // 3. MMIO disabled check — only when mmio_base != 0 && !mmio_enabled
        if (!cpu->mmio_enabled && cpu->mmio_base != 0) {
            emit_cmp_r32_imm32(e, addr_reg, (int32_t)cpu->mmio_base);
            ok_jumps[ok_count++] = emit_offset(e) + 2;
            emit_jb_rel32(e, 0);
            emit_cmp_r32_imm32(e, addr_reg, (int32_t)(cpu->mmio_base + 0x10000));
            fault_jumps[fault_count++] = emit_offset(e) + 2;
            emit_jb_rel32(e, 0);
        }
    }

    // 4. W^X check — stores only, only when wxorx_enabled
    //    Needed even when bounds are skipped (unless fully covered)
    if (is_store && cpu->wxorx_enabled) {
        uint32_t wx_limit = cpu->rodata_limit ? cpu->rodata_limit : cpu->code_limit;
        if (wx_limit > 0) {
            emit_cmp_r32_imm32(e, addr_reg, (int32_t)wx_limit);
            fault_jumps[fault_count++] = emit_offset(e) + 2;
            emit_jb_rel32(e, 0);
        }
    }

    if (fault_count == 0 && ok_count == 0) {
        // No checks were emitted (e.g., skip_bounds with no W^X)
        // Record the range and return
        if (!skip_bounds) {
            bounds_elim_record(ctx, base_guest_reg, offset, access_size, is_store);
        } else if (is_store) {
            // Update existing range's is_store_ok
            bounds_elim_record(ctx, base_guest_reg, offset, access_size, true);
        }
        return;
    }

    // Success path — jump over fault handler
    size_t ok_jump = emit_offset(e) + 1;
    emit_jmp_rel32(e, 0);

    // Fault path — need address in RAX for exit_info
    size_t fault_offset = emit_offset(e);
    if (addr_reg != RAX) {
        emit_mov_r32_r32(e, RAX, addr_reg);
    }
    emit_exit_with_info_reg(ctx, reason, ctx->guest_pc, RAX);

    // Patch all jumps
    size_t ok_offset = emit_offset(e);
    emit_patch_rel32(e, ok_jump, ok_offset);
    for (int i = 0; i < fault_count; i++) {
        emit_patch_rel32(e, fault_jumps[i], fault_offset);
    }
    for (int i = 0; i < ok_count; i++) {
        emit_patch_rel32(e, ok_jumps[i], ok_offset);
    }

    // Record validated range after successful check emission
    if (!skip_bounds) {
        bounds_elim_record(ctx, base_guest_reg, offset, access_size, is_store);
    } else if (is_store) {
        // W^X was checked — update the existing range
        bounds_elim_record(ctx, base_guest_reg, offset, access_size, true);
    }
}

// Memory access check for dynamic sizes (e.g., memcpy/memset)
// Assumes addr_reg and size_reg are 32-bit, size_reg is bytes.
static void emit_mem_access_check_dynamic(translate_ctx_t *ctx,
                                          x64_reg_t addr_reg,
                                          x64_reg_t size_reg,
                                          exit_reason_t reason,
                                          bool is_store) {
    emit_ctx_t *e = &ctx->emit;
    dbt_cpu_state_t *cpu = ctx->cpu;

    // -U flag: skip all checks (unsafe, for benchmarking)
    if (cpu->bounds_checks_disabled) return;

    size_t fault_jumps[8];
    int fault_count = 0;
    size_t ok_jumps[8];
    int ok_count = 0;

    // size == 0 => ok (no access)
    emit_test_r32_r32(e, size_reg, size_reg);
    ok_jumps[ok_count++] = emit_offset(e) + 2;
    emit_je_rel32(e, 0);

    // size > mem_size => fault
    emit_cmp_r32_imm32(e, size_reg, (int32_t)cpu->mem_size);
    fault_jumps[fault_count++] = emit_offset(e) + 2;
    emit_ja_rel32(e, 0);

    // addr <= mem_size - size
    emit_mov_r32_imm32(e, R8, cpu->mem_size);
    emit_sub_r32_r32(e, R8, size_reg);
    emit_cmp_r32_r32(e, addr_reg, R8);
    fault_jumps[fault_count++] = emit_offset(e) + 2;
    emit_ja_rel32(e, 0);

    // MMIO disabled window check for dynamic ranges
    if (!cpu->mmio_enabled && cpu->mmio_base != 0) {
        // end = addr + size
        emit_mov_r32_r32(e, R9, addr_reg);
        emit_add_r32_r32(e, R9, size_reg);

        // if end <= mmio_base => ok
        emit_cmp_r32_imm32(e, R9, (int32_t)cpu->mmio_base);
        ok_jumps[ok_count++] = emit_offset(e) + 2;
        emit_jbe_rel32(e, 0);

        // if addr >= mmio_base + 0x10000 => ok
        emit_cmp_r32_imm32(e, addr_reg, (int32_t)(cpu->mmio_base + 0x10000));
        ok_jumps[ok_count++] = emit_offset(e) + 2;
        emit_jae_rel32(e, 0);

        // overlap => fault
        fault_jumps[fault_count++] = emit_offset(e) + 1;
        emit_jmp_rel32(e, 0);
    }

    // W^X check — stores only
    if (is_store && cpu->wxorx_enabled) {
        uint32_t wx_limit = cpu->rodata_limit ? cpu->rodata_limit : cpu->code_limit;
        if (wx_limit > 0) {
            emit_cmp_r32_imm32(e, addr_reg, (int32_t)wx_limit);
            fault_jumps[fault_count++] = emit_offset(e) + 2;
            emit_jb_rel32(e, 0);
        }
    }

    // Success path — jump over fault handler
    size_t ok_jump = emit_offset(e) + 1;
    emit_jmp_rel32(e, 0);

    // Fault path — need address in RAX for exit_info
    size_t fault_offset = emit_offset(e);
    if (addr_reg != RAX) {
        emit_mov_r32_r32(e, RAX, addr_reg);
    }
    emit_exit_with_info_reg(ctx, reason, ctx->guest_pc, RAX);

    // Patch all jumps
    size_t ok_offset = emit_offset(e);
    emit_patch_rel32(e, ok_jump, ok_offset);
    for (int i = 0; i < fault_count; i++) {
        emit_patch_rel32(e, fault_jumps[i], fault_offset);
    }
    for (int i = 0; i < ok_count; i++) {
        emit_patch_rel32(e, ok_jumps[i], ok_offset);
    }
}

// Helper to compute address — returns the register holding the effective address.
// When the base register is allocated and imm==0, returns the host register directly
// (no instructions emitted). When allocated and imm!=0, uses LEA (1 inst vs 2).
static x64_reg_t emit_compute_addr(translate_ctx_t *ctx, uint8_t rs1, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    if (rs1 == 0) {
        flush_pending_write(ctx);  // About to clobber RAX
        emit_mov_r32_imm32(e, RAX, (uint32_t)imm);
        return RAX;
    }

    // Constant folding: if rs1 is a known constant, fold address at translate time
    if (ctx->reg_constants[rs1].valid) {
        uint32_t addr = ctx->reg_constants[rs1].value + (uint32_t)imm;
        flush_pending_write(ctx);  // About to clobber RAX
        emit_mov_r32_imm32(e, RAX, addr);
        return RAX;
    }

    x64_reg_t host = guest_host_reg(ctx, rs1);
    if (host != X64_NOREG) {
        if (imm == 0) {
            // Address already in host register — use directly
            flush_pending_write(ctx);  // Fault path's reg_cache_flush would clear this at translate-time
            return host;
        }
        // LEA: compute base+offset into RAX without clobbering host reg
        flush_pending_write(ctx);  // About to clobber RAX
        emit_lea_r32_r32_disp(e, RAX, host, imm);
        return RAX;
    }

    // Not allocated — existing path
    emit_load_guest_reg(ctx, RAX, rs1);
    if (imm != 0) {
        emit_add_r32_imm32(e, RAX, imm);
    }
    return RAX;
}

void translate_ldw(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    // Disable range tracking when address was constant-folded
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    x64_reg_t addr = emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, addr, 4, EXIT_FAULT_LOAD, false, base_reg, imm);
    x64_reg_t host_rd = guest_host_reg(ctx, rd);
    if (host_rd != X64_NOREG) {
        emit_mov_r32_m32_idx(e, host_rd, R14, addr);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_mov_r32_m32_idx(e, RCX, R14, addr);
        emit_store_guest_reg(ctx, rd, RCX);
    }
}

void translate_ldh(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    x64_reg_t addr = emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, addr, 2, EXIT_FAULT_LOAD, false, base_reg, imm);
    x64_reg_t host_rd = guest_host_reg(ctx, rd);
    if (host_rd != X64_NOREG) {
        emit_movsx_r32_m16_idx(e, host_rd, R14, addr);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_movsx_r32_m16_idx(e, RCX, R14, addr);
        emit_store_guest_reg(ctx, rd, RCX);
    }
}

void translate_ldb(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    x64_reg_t addr = emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, addr, 1, EXIT_FAULT_LOAD, false, base_reg, imm);
    x64_reg_t host_rd = guest_host_reg(ctx, rd);
    if (host_rd != X64_NOREG) {
        emit_movsx_r32_m8_idx(e, host_rd, R14, addr);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_movsx_r32_m8_idx(e, RCX, R14, addr);
        emit_store_guest_reg(ctx, rd, RCX);
    }
}

void translate_ldhu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    x64_reg_t addr = emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, addr, 2, EXIT_FAULT_LOAD, false, base_reg, imm);
    x64_reg_t host_rd = guest_host_reg(ctx, rd);
    if (host_rd != X64_NOREG) {
        emit_movzx_r32_m16_idx(e, host_rd, R14, addr);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_movzx_r32_m16_idx(e, RCX, R14, addr);
        emit_store_guest_reg(ctx, rd, RCX);
    }
}

void translate_ldbu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    x64_reg_t addr = emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, addr, 1, EXIT_FAULT_LOAD, false, base_reg, imm);
    x64_reg_t host_rd = guest_host_reg(ctx, rd);
    if (host_rd != X64_NOREG) {
        emit_movzx_r32_m8_idx(e, host_rd, R14, addr);
        reg_cache_mark_written(ctx, rd);
    } else {
        emit_movzx_r32_m8_idx(e, RCX, R14, addr);
        emit_store_guest_reg(ctx, rd, RCX);
    }
}

void translate_stw(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    x64_reg_t addr = emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, addr, 4, EXIT_FAULT_STORE, true, base_reg, imm);
    x64_reg_t host_rs2 = guest_host_reg(ctx, rs2);
    if (host_rs2 != X64_NOREG) {
        flush_pending_write(ctx);
        emit_mov_m32_r32_idx(e, R14, addr, host_rs2);
    } else {
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_mov_m32_r32_idx(e, R14, addr, RCX);
    }
}

void translate_sth(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    x64_reg_t addr = emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, addr, 2, EXIT_FAULT_STORE, true, base_reg, imm);
    x64_reg_t host_rs2 = guest_host_reg(ctx, rs2);
    if (host_rs2 != X64_NOREG) {
        flush_pending_write(ctx);
        emit_mov_m16_r16_idx(e, R14, addr, host_rs2);
    } else {
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_mov_m16_r16_idx(e, R14, addr, RCX);
    }
}

void translate_stb(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint8_t base_reg = (rs1 != 0 && ctx->reg_constants[rs1].valid) ? 0 : rs1;

    x64_reg_t addr = emit_compute_addr(ctx, rs1, imm);
    emit_mem_access_check(ctx, addr, 1, EXIT_FAULT_STORE, true, base_reg, imm);
    x64_reg_t host_rs2 = guest_host_reg(ctx, rs2);
    if (host_rs2 != X64_NOREG) {
        flush_pending_write(ctx);
        emit_mov_m8_r8_idx(e, R14, addr, host_rs2);
    } else {
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_mov_m8_r8_idx(e, R14, addr, RCX);
    }
}

// ============================================================================
// Control flow translations
// ============================================================================

// Helper for Stage 2 chained exits
// Emits a patchable jump that can be chained to a target block
static void emit_exit_chained(translate_ctx_t *ctx, uint32_t target_pc, int exit_idx) {
    emit_ctx_t *e = &ctx->emit;
    const char *saved_tag = e->trace_tag;

    reg_cache_flush(ctx);  // also flushes pending_cond

    // Store target PC in case we return to dispatcher or need it for debug
    if (e->trace_enabled) {
        e->trace_tag = "exit_pc";
    }
    emit_mov_m32_imm32(e, RBP, CPU_PC_OFFSET, target_pc);

    if (ctx->cache == NULL) {
        // Stage 1 mode: always return to dispatcher
        if (e->trace_enabled) {
            e->trace_tag = "exit_reason";
        }
        emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_BRANCH);
        e->trace_tag = saved_tag;
        emit_ret(e);
        return;
    }

    // Stage 2 mode: try to chain, or emit patchable jump to dispatcher
    translated_block_t *target = cache_lookup(ctx->cache, target_pc);

    // Record the patch site (where the rel32 offset will be)
    uint8_t *patch_site = emit_ptr(e) + 1;  // After jmp opcode (E9)

    if (target && target->host_code) {
        // Target already translated - emit direct jump
        int64_t rel = (int64_t)(target->host_code - (patch_site + 4));
        emit_jmp_rel32(e, (int32_t)rel);

        // Record as already chained
        if (ctx->block && exit_idx < MAX_BLOCK_EXITS) {
            cache_record_exit(ctx->cache, ctx->block, exit_idx, target_pc, patch_site);
            ctx->block->exits[exit_idx].chained = true;
        }
    } else {
        // Target not yet translated - jump to shared_branch_exit stub
        // (sets exit_reason and returns to dispatcher)
        // This saves 10 bytes vs inlining the exit_reason store
        patch_site = emit_ptr(e) + 1;

        int64_t rel = (int64_t)(ctx->cache->shared_branch_exit - (patch_site + 4));
        emit_jmp_rel32(e, (int32_t)rel);

        // Record for later chaining
        if (ctx->block && exit_idx < MAX_BLOCK_EXITS) {
            cache_record_exit(ctx->cache, ctx->block, exit_idx, target_pc, patch_site);
            cache_record_pending_chain(ctx->cache, ctx->block, exit_idx, target_pc);
        }
    }
}

// Helper for compact chained exits using shared branch exit stub.
// Used for out-of-line (cold) side exit stubs. Only emits:
//   reg_cache_flush (if enabled)
//   mov [rbp+PC], target_pc
//   jmp target_host_code   (if already chained)
//   OR jmp shared_branch_exit (if not yet chained; will be patched later)
//
// This saves ~10 bytes per unchained exit vs emit_exit_chained because
// the exit_reason store is in the shared stub, not inlined.
static void emit_exit_chained_compact(translate_ctx_t *ctx, uint32_t target_pc,
                                      int exit_idx) {
    emit_ctx_t *e = &ctx->emit;

    // Note: reg_cache_flush must be done by caller with correct snapshot
    flush_pending_cond(ctx);

    // Store target PC
    emit_mov_m32_imm32(e, RBP, CPU_PC_OFFSET, target_pc);

    if (ctx->cache == NULL) {
        // Stage 1 mode: fallback to full exit
        emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_BRANCH);
        emit_ret(e);
        return;
    }

    // Try to chain to already-translated target
    translated_block_t *target = cache_lookup(ctx->cache, target_pc);
    uint8_t *patch_site = emit_ptr(e) + 1;  // After jmp opcode (E9)

    if (target && target->host_code) {
        // Target already translated - emit direct jump
        int64_t rel = (int64_t)(target->host_code - (patch_site + 4));
        emit_jmp_rel32(e, (int32_t)rel);

        if (ctx->block && exit_idx < MAX_BLOCK_EXITS) {
            cache_record_exit(ctx->cache, ctx->block, exit_idx, target_pc, patch_site);
            ctx->block->exits[exit_idx].chained = true;
        }
    } else {
        // Not yet translated - jump to shared_branch_exit (sets EXIT_REASON + ret)
        // This will be patched later when the target is translated
        int64_t rel = (int64_t)(ctx->cache->shared_branch_exit - (patch_site + 4));
        emit_jmp_rel32(e, (int32_t)rel);

        if (ctx->block && exit_idx < MAX_BLOCK_EXITS) {
            cache_record_exit(ctx->cache, ctx->block, exit_idx, target_pc, patch_site);
            cache_record_pending_chain(ctx->cache, ctx->block, exit_idx, target_pc);
        }
    }
}

// Emit all deferred (out-of-line) side exit stubs at the current emit position.
// Called after the hot path of the block is complete.
static void emit_deferred_side_exits(translate_ctx_t *ctx) {
    emit_ctx_t *e = &ctx->emit;

    for (int i = 0; i < ctx->deferred_exit_count; i++) {
        // Patch the inline jmp to point here
        size_t cold_offset = emit_offset(e);
        emit_patch_rel32(e, ctx->deferred_exits[i].jmp_patch_offset, cold_offset);

        // Flush state from branch point snapshot (not current translation state)
        if (ctx->reg_cache_enabled) {
            // Flush snapshotted pending write for non-cached registers
            if (ctx->deferred_exits[i].pending_write_valid) {
                emit_mov_m32_r32(e, RBP,
                    GUEST_REG_OFFSET(ctx->deferred_exits[i].pending_write_guest_reg),
                    ctx->deferred_exits[i].pending_write_host_reg);
            }
            // Flush dirty cached registers using full allocation snapshot
            reg_alloc_flush_snapshot(ctx,
                ctx->deferred_exits[i].dirty_snapshot,
                ctx->deferred_exits[i].allocated_snapshot,
                ctx->deferred_exits[i].guest_reg_snapshot);
        }

        // Record branch_pc on block exit
        if (ctx->block && ctx->deferred_exits[i].exit_idx < MAX_BLOCK_EXITS) {
            ctx->block->exits[ctx->deferred_exits[i].exit_idx].branch_pc =
                ctx->deferred_exits[i].branch_pc;
        }

        // Emit compact exit (mov PC + jmp shared/chained)
        emit_exit_chained_compact(ctx, ctx->deferred_exits[i].target_pc,
                                  ctx->deferred_exits[i].exit_idx);
    }
}

// ============================================================================
// Stage 3: Inline indirect branch lookup
// ============================================================================

static const int INLINE_LOOKUP_MAX_PROBES = 4;

// Emit inline hash table lookup for indirect branches with bounded probing.
// target_reg contains the guest PC to look up.
// target_save is a scratch register that will hold the target for compares/fallback.
static void emit_indirect_lookup_with_target(translate_ctx_t *ctx,
                                             x64_reg_t target_reg,
                                             x64_reg_t target_save) {
    emit_ctx_t *e = &ctx->emit;
    size_t null_patches[INLINE_LOOKUP_MAX_PROBES];
    size_t null_patch_count = 0;
    size_t last_cmp_patch = 0;

    // Save target to target_save (need it for comparison and fallback)
    if (target_reg != target_save) {
        emit_mov_r64_r64(e, target_save, target_reg);
    }

    // Compute hash: ((target >> 2) ^ (target >> 12)) & MASK
    // Use RCX as a temp and reload mask to avoid clobbering target_save.
    emit_mov_r32_r32(e, RDX, target_save);                   // rdx = target (32-bit)
    emit_mov_r32_r32(e, RCX, RDX);                           // rcx = target (temp)
    emit_shr_r32_imm8(e, RDX, 2);                            // rdx >>= 2
    emit_shr_r32_imm8(e, RCX, 12);                           // rcx >>= 12
    emit_xor_r32_r32(e, RDX, RCX);                           // rdx ^= rcx
    emit_mov_r32_m32(e, RCX, RBP, CPU_LOOKUP_MASK_OFFSET);   // rcx = mask
    emit_and_r32_r32(e, RDX, RCX);                           // rdx = hash

    // r10d = hash index (probe cursor)
    emit_mov_r32_r32(e, R10, RDX);

    for (int probe = 0; probe < INLINE_LOOKUP_MAX_PROBES; probe++) {
        if (probe > 0) {
            emit_add_r32_imm32(e, R10, 1);
            emit_and_r32_r32(e, R10, RCX);
        }

        // Load block pointer: blocks[idx] (R10 = index, each entry is 8 bytes)
        emit_mov_r64_m64_sib(e, RDX, R15, R10, 8, 0);  // rdx = [r15 + r10*8]

        // Check for NULL (miss)
        emit_test_r64_r64(e, RDX, RDX);
        null_patches[null_patch_count++] = emit_offset(e) + 2;
        emit_jz_rel32(e, 0);

        // Compare block->guest_pc with target
        emit_cmp_m32_r32(e, RDX, 0, target_save);
        size_t cmp_miss_patch = emit_offset(e) + 2;
        emit_jne_rel32(e, 0);

        reg_cache_flush(ctx);

        // Hit! Jump to host code
        emit_mov_r64_m64(e, RAX, RDX, 8);
        emit_jmp_r64(e, RAX);

        if (probe < INLINE_LOOKUP_MAX_PROBES - 1) {
            size_t next_probe = emit_offset(e);
            emit_patch_rel32(e, cmp_miss_patch, next_probe);
        } else {
            last_cmp_patch = cmp_miss_patch;
        }
    }

    // miss_path: Fall back to dispatcher
    size_t miss_path = emit_offset(e);
    for (size_t i = 0; i < null_patch_count; i++) {
        emit_patch_rel32(e, null_patches[i], miss_path);
    }
    if (last_cmp_patch != 0) {
        emit_patch_rel32(e, last_cmp_patch, miss_path);
    }

    reg_cache_flush(ctx);

    // Store target PC
    emit_mov_m32_r32(e, RBP, CPU_PC_OFFSET, target_save);

    // Store exit reason = EXIT_INDIRECT
    emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_INDIRECT);

    // Jump to native dispatcher for full linear-probe lookup
    // (gives indirect branches a second chance beyond the 4-probe inline lookup)
    if (ctx->cache && ctx->cache->native_dispatcher) {
        uint8_t *dispatcher = ctx->cache->native_dispatcher;
        int32_t rel = (int32_t)(dispatcher - (emit_ptr(e) + 5));
        emit_jmp_rel32(e, rel);
    } else {
        emit_ret(e);
    }
}

// Emit inline hash table lookup for indirect branches
// target_reg contains the guest PC to jump to
// On hit: jumps directly to host code
// On miss: falls back to dispatcher
static void emit_indirect_lookup(translate_ctx_t *ctx, x64_reg_t target_reg) {
    emit_indirect_lookup_with_target(ctx, target_reg, R8);
}

// ============================================================================
// Stage 3 Phase 2: Return Address Stack (RAS)
// ============================================================================

// Push a return address onto the RAS
// Called at JAL sites when rd == r31 (call instruction)
static void emit_ras_push(translate_ctx_t *ctx, uint32_t return_pc) {
    emit_ctx_t *e = &ctx->emit;

    // RAS is a circular buffer: ras_stack[ras_top++ & RAS_MASK] = return_pc
    //
    // Algorithm:
    // 1. Load current top index
    // 2. Store return_pc at ras_stack[top]
    // 3. Increment top with wrap (top = (top + 1) & RAS_MASK)

    // 1. Load ras_top into ECX
    emit_mov_r32_m32(e, RCX, RBP, CPU_RAS_TOP_OFFSET);

    // 2. Store return_pc at ras_stack[top]
    // ras_stack is at CPU_RAS_STACK_OFFSET, each entry is 4 bytes
    emit_mov_m32_imm32_sib(e, RBP, RCX, 4, CPU_RAS_STACK_OFFSET, return_pc);

    // 3. Increment top with wrap: top = (top + 1) & RAS_MASK
    emit_add_r32_imm32(e, RCX, 1);
    emit_and_r32_imm32(e, RCX, RAS_MASK);
    emit_mov_m32_r32(e, RBP, CPU_RAS_TOP_OFFSET, RCX);
}

// Predict return address using RAS
// Called at JALR r0, r31, 0 sites (return instruction)
// target_reg contains the actual return address from r31
// On RAS hit: jumps directly to the predicted block
// On RAS miss: falls through to inline lookup (or dispatcher)
static void emit_ras_predict(translate_ctx_t *ctx, x64_reg_t target_reg) {
    emit_ctx_t *e = &ctx->emit;

    // Algorithm:
    // 1. Decrement top with wrap: top = (top - 1) & RAS_MASK
    // 2. Load predicted_pc = ras_stack[top]
    // 3. Compare with actual target
    // 4. If equal: do inline lookup for predicted_pc (should hit)
    // 5. If not equal: do inline lookup for actual target

    // Save actual target to R8 (we need it if RAS misses)
    if (target_reg != R8) {
        emit_mov_r64_r64(e, R8, target_reg);
    }

    // 1. Decrement top with wrap
    emit_mov_r32_m32(e, RCX, RBP, CPU_RAS_TOP_OFFSET);
    emit_sub_r32_imm32(e, RCX, 1);
    emit_and_r32_imm32(e, RCX, RAS_MASK);
    emit_mov_m32_r32(e, RBP, CPU_RAS_TOP_OFFSET, RCX);

    // 2. Load predicted_pc from ras_stack[top]
    emit_mov_r32_m32_sib(e, RAX, RBP, RCX, 4, CPU_RAS_STACK_OFFSET);

    // 3. Compare predicted with actual (R8)
    emit_cmp_r32_r32(e, RAX, R8);
    size_t mismatch_patch = emit_offset(e) + 2;
    emit_jne_rel32(e, 0);  // -> mismatch (use actual target)

    // 4. RAS hit! Use predicted_pc (already in RAX) for inline lookup
    // Fall through to inline lookup with RAX as target

    // Do inline lookup with RAX (predicted_pc)
    // Save predicted PC to R9 for comparison/fallback.
    emit_mov_r32_r32(e, R9, RAX);
    emit_indirect_lookup_with_target(ctx, RAX, R9);

    // 5. RAS mismatch - use actual target (R8) instead
    size_t mismatch_path = emit_offset(e);
    emit_patch_rel32(e, mismatch_patch, mismatch_path);

    // Fall through to inline lookup with actual target
    emit_indirect_lookup(ctx, R8);
}

void translate_jal(translate_ctx_t *ctx, uint8_t rd, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint32_t return_pc = ctx->guest_pc + 4;
    uint32_t target_pc = ctx->guest_pc + imm;

    // Store return address if rd != 0
    if (rd != 0) {
        flush_pending_write(ctx);  // About to clobber RAX directly
        emit_mov_r32_imm32(e, RAX, return_pc);
        emit_store_guest_reg(ctx, rd, RAX);
    }

    // Mark this as a call for profiling
    if (ctx->block && rd == REG_LR) {
        ctx->block->flags |= BLOCK_FLAG_CALL;
    }

    // Stage 3 Phase 2: Push return address to RAS for calls
    if (ctx->ras_enabled && rd == REG_LR) {
        emit_ras_push(ctx, return_pc);
    }

    // Exit with branch to target (chainable)
    emit_exit_chained(ctx, target_pc, ctx->exit_idx++);
}

void translate_jalr(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint32_t return_pc = ctx->guest_pc + 4;

    // Calculate target: (rs1 + imm) & ~1
    emit_load_guest_reg(ctx, RAX, rs1);
    if (imm != 0) {
        emit_add_r32_imm32(e, RAX, imm);
    }
    emit_and_r32_imm32(e, RAX, (int32_t)~1u);

    // Flush R8/R9 cached values before using them as scratch.
    // JALR always ends the block, so no reload is needed.
    flush_cached_host_regs(ctx, R8, R9);

    // Store return address if rd != 0
    // Do this BEFORE we potentially clobber RAX with lookup code
    if (rd != 0) {
        // Save target temporarily
        emit_mov_r64_r64(e, R8, RAX);
        emit_mov_r32_imm32(e, RAX, return_pc);
        emit_store_guest_reg(ctx, rd, RAX);
        // Flush any deferred write before restoring target into RAX
        flush_pending_write(ctx);
        // Restore target
        emit_mov_r64_r64(e, RAX, R8);
    }

    // Mark block flags for profiling
    bool is_return = (rd == 0 && rs1 == REG_LR && imm == 0);
    if (ctx->block) {
        ctx->block->flags |= BLOCK_FLAG_INDIRECT;
        if (is_return) {
            ctx->block->flags |= BLOCK_FLAG_RETURN;
        }
    }

    // Stage 3 Phase 2: Use RAS prediction for returns
    if (ctx->ras_enabled && is_return && ctx->inline_lookup_enabled) {
        emit_ras_predict(ctx, RAX);
    }
    // Stage 3: Use inline hash lookup if enabled
    else if (ctx->inline_lookup_enabled && ctx->cache) {
        emit_indirect_lookup(ctx, RAX);
    } else {
        // Stage 2 fallback: return to dispatcher
        emit_mov_m32_r32(e, RBP, CPU_PC_OFFSET, RAX);
        emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_INDIRECT);
        emit_ret(e);
    }
}

// Look up host code offset for a guest PC within the current block.
// Returns the host offset, or (size_t)-1 if not found.
static size_t pc_map_lookup(translate_ctx_t *ctx, uint32_t guest_pc) {
    for (int i = 0; i < ctx->pc_map_count; i++) {
        if (ctx->pc_map[i].guest_pc == guest_pc)
            return ctx->pc_map[i].host_offset;
    }
    return (size_t)-1;
}

// Helper for conditional branches
// NOTE: SLOW-32 branches are PC+4 relative, not PC relative!
// Returns true if the block should end, false if superblock continues
static bool translate_branch_common(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2,
                                    int32_t imm,
                                    void (*emit_jcc)(emit_ctx_t *, int32_t),
                                    void (*emit_jcc_inv)(emit_ctx_t *, int32_t),
                                    uint8_t inv_cc) {
    emit_ctx_t *e = &ctx->emit;
    uint32_t fall_pc = ctx->guest_pc + 4;
    uint32_t taken_pc = fall_pc + imm;  // PC+4 + imm
    uint32_t branch_pc = ctx->guest_pc;

    // Mark as direct (both exits can be chained)
    if (ctx->block) {
        ctx->block->flags |= BLOCK_FLAG_DIRECT;
    }

    // Stage 4: Check if we should extend past this branch (superblock mode)
    // Heuristic: forward branches (imm > 0) are likely not taken
    bool can_extend = ctx->superblock_enabled &&
                      ctx->superblock_depth < MAX_SUPERBLOCK_DEPTH &&
                      imm > 0 &&  // Forward branch - fall-through is likely
                      ctx->exit_idx < MAX_SUPERBLOCK_EXITS - 1;
    if (ctx->avoid_backedge_extend && imm < 0) {
        can_extend = false;
    }

    // Compare-Branch Fusion
    bool fused = false;
    if (ctx->pending_cond.valid) {
        // We have a deferred comparison. Is it the one this branch is testing?
        // Branch instructions (BEQ/BNE/...) in SLOW-32 test if (rs1 == rs2).
        // Common pattern: SLT r1, r2, r3; BNE r1, r0, label.
        // This is testing if (r2 < r3) != 0, which is just (r2 < r3).

        bool match = (rs2 == 0 && rs1 == ctx->pending_cond.rd) ||
                     (rs1 == 0 && rs2 == ctx->pending_cond.rd);
        if (match) {
            // FUSION OPPORTUNITY: Bxx rd, r0, target where rd is the result of a comparison
            uint8_t cmp_op = ctx->pending_cond.opcode;
            uint8_t c_rs1 = ctx->pending_cond.rs1;
            uint8_t c_rs2 = ctx->pending_cond.rs2;
            bool c_rs2_is_imm = ctx->pending_cond.rs2_is_imm;
            int32_t c_imm = ctx->pending_cond.imm;
            uint8_t c_rd = ctx->pending_cond.rd;
            int c_inst_idx = ctx->pending_cond.inst_idx;

            // Discard the pending condition BEFORE emitting anything to avoid
            // re-entrant flushes in emit_load_guest_reg clobbering registers.
            ctx->pending_cond.valid = false;

            if (DBT_TRACE) {
                fprintf(stderr, "  [fusion] Fusing branch at 0x%08X with comparison opcode 0x%02X\n",
                        branch_pc, cmp_op);
            }

            // 1. Emit the comparison
            x64_reg_t h1 = guest_host_reg(ctx, c_rs1);
            x64_reg_t cmp_a = (h1 != X64_NOREG) ? h1 : RAX;
            if (h1 == X64_NOREG) emit_load_guest_reg(ctx, RAX, c_rs1);

            if (c_rs2_is_imm) {
                if (h1 != X64_NOREG) flush_pending_write(ctx);
                emit_cmp_r32_imm32(e, cmp_a, c_imm);
            } else {
                x64_reg_t h2 = guest_host_reg(ctx, c_rs2);
                x64_reg_t cmp_b = (h2 != X64_NOREG) ? h2 : RCX;
                if (h2 == X64_NOREG) emit_load_guest_reg(ctx, RCX, c_rs2);
                if (h1 != X64_NOREG) flush_pending_write(ctx);
                emit_cmp_r32_r32(e, cmp_a, cmp_b);
            }

            // 2. Materialize the comparison result into rd — but skip if rd
            //    is a dead temporary (only consumed by this branch).
            //    SETCC, MOVZX, and MOV/store do NOT modify x86 flags,
            //    so the fused Jcc below still reads the correct CMP flags.
            bool rd_is_dead = (c_inst_idx >= 0 &&
                               c_inst_idx < MAX_BLOCK_INSTS &&
                               ctx->dead_temp_skip[c_inst_idx]);
            if (!rd_is_dead) {
                void (*emit_setcc_fn)(emit_ctx_t *, x64_reg_t) = NULL;
                switch (cmp_op) {
                    case OP_SLT:  case OP_SLTI:  emit_setcc_fn = emit_setl;  break;
                    case OP_SLTU: case OP_SLTIU: emit_setcc_fn = emit_setb;  break;
                    case OP_SEQ:                 emit_setcc_fn = emit_sete;  break;
                    case OP_SNE:                 emit_setcc_fn = emit_setne; break;
                    case OP_SGT:                 emit_setcc_fn = emit_setg;  break;
                    case OP_SGTU:                emit_setcc_fn = emit_seta;  break;
                    case OP_SLE:                 emit_setcc_fn = emit_setle; break;
                    case OP_SLEU:                emit_setcc_fn = emit_setbe; break;
                    case OP_SGE:                 emit_setcc_fn = emit_setge; break;
                    case OP_SGEU:                emit_setcc_fn = emit_setae; break;
                }
                if (emit_setcc_fn) {
                    emit_setcc_fn(e, RAX);
                    emit_movzx_r32_r8(e, RAX, RAX);
                    emit_store_guest_reg(ctx, c_rd, RAX);
                }
            }

            // 3. Map SLOW-32 comparison + branch condition to x86-64 condition codes
            // We are testing: if ((c_rs1 OP c_rs2) BRANCH_TYPE 0) jump target
            // where BRANCH_TYPE is == (BEQ) or != (BNE).

            bool is_beq = (inv_cc == 0x05); // BEQ's inverted CC is 0x85 (JNE), low nibble 5
            bool is_bne = (inv_cc == 0x04); // BNE's inverted CC is 0x84 (JE), low nibble 4

            if (is_beq || is_bne) {
                // Determine the base x86 condition for the comparison 'cmp_op'
                uint8_t base_cc = 0;     // e.g., 0x84 for JE
                uint8_t base_cc_inv = 0; // e.g., 0x85 for JNE

                switch (cmp_op) {
                    case OP_SLT:  case OP_SLTI:  base_cc = 0x8C; base_cc_inv = 0x8D; break; // JL / JGE
                    case OP_SLTU: case OP_SLTIU: base_cc = 0x82; base_cc_inv = 0x83; break; // JB / JAE
                    case OP_SEQ:                 base_cc = 0x84; base_cc_inv = 0x85; break; // JE / JNE
                    case OP_SNE:                 base_cc = 0x85; base_cc_inv = 0x84; break; // JNE / JE
                    case OP_SGT:                 base_cc = 0x8F; base_cc_inv = 0x8E; break; // JG / JLE
                    case OP_SGTU:                base_cc = 0x87; base_cc_inv = 0x86; break; // JA / JBE
                    case OP_SLE:                 base_cc = 0x8E; base_cc_inv = 0x8F; break; // JLE / JG
                    case OP_SLEU:                base_cc = 0x86; base_cc_inv = 0x87; break; // JBE / JA
                    case OP_SGE:                 base_cc = 0x8D; base_cc_inv = 0x8C; break; // JGE / JL
                    case OP_SGEU:                base_cc = 0x83; base_cc_inv = 0x82; break; // JAE / JB
                }

                if (is_bne) {
                    // if (cond != 0) jump  =>  jump if comparison was TRUE
                    emit_jcc = (void (*)(emit_ctx_t *, int32_t))emit_jcc_rel32_from_cc(base_cc);
                    emit_jcc_inv = (void (*)(emit_ctx_t *, int32_t))emit_jcc_rel32_from_cc(base_cc_inv);
                    inv_cc = base_cc_inv & 0x0F;
                } else {
                    // if (cond == 0) jump  =>  jump if comparison was FALSE
                    emit_jcc = (void (*)(emit_ctx_t *, int32_t))emit_jcc_rel32_from_cc(base_cc_inv);
                    emit_jcc_inv = (void (*)(emit_ctx_t *, int32_t))emit_jcc_rel32_from_cc(base_cc);
                    inv_cc = base_cc & 0x0F;
                }

                if (emit_jcc) {
                    fused = true;
                }
            }
        }

        if (!fused) {
            // Not a fusion candidate, materialize it now
            flush_pending_cond(ctx);
        }
    }

    if (!fused) {
        // Standard comparison logic
        const char *saved_tag_cmp = e->trace_tag;
        if (e->trace_enabled) {
            e->trace_tag = "branch_compare";
        }
        if (rs1 == 0 && rs2 == 0) {
            flush_pending_write(ctx);  // About to clobber RAX directly
            emit_xor_r32_r32(e, RAX, RAX);
        } else if (rs1 == 0) {
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            if (h2 != X64_NOREG) {
                flush_pending_write(ctx);
                emit_test_r32_r32(e, h2, h2);
            } else {
                emit_load_guest_reg(ctx, RAX, rs2);
                emit_test_r32_r32(e, RAX, RAX);
            }
        } else if (rs2 == 0) {
            x64_reg_t h1 = guest_host_reg(ctx, rs1);
            if (h1 != X64_NOREG) {
                flush_pending_write(ctx);
                emit_test_r32_r32(e, h1, h1);
            } else {
                emit_load_guest_reg(ctx, RAX, rs1);
                emit_test_r32_r32(e, RAX, RAX);
            }
        } else {
            x64_reg_t h1 = guest_host_reg(ctx, rs1);
            x64_reg_t h2 = guest_host_reg(ctx, rs2);
            x64_reg_t cmp_a = (h1 != X64_NOREG) ? h1 : RAX;
            x64_reg_t cmp_b = (h2 != X64_NOREG) ? h2 : RCX;
            if (h1 == X64_NOREG) emit_load_guest_reg(ctx, RAX, rs1);
            if (h2 == X64_NOREG) emit_load_guest_reg(ctx, RCX, rs2);
            if (h1 != X64_NOREG || h2 != X64_NOREG)
                flush_pending_write(ctx);
            emit_cmp_r32_r32(e, cmp_a, cmp_b);
        }
        e->trace_tag = saved_tag_cmp;
    }

    if (can_extend) {
        // Superblock mode: out-of-line side exit for taken path
        //
        // Layout:
        //     cmp rs1, rs2
        //     jcc_inv skip              ; if condition FALSE, skip trampoline
        //     jmp cold_stub_N           ; 5-byte jump to cold exit at end of block
        // skip:
        //     <continue with fall-through>  ; hot path
        //
        // Cold stubs are emitted at end of block by emit_deferred_side_exits().

        // Jump over trampoline if condition is FALSE (not taken)
        // Short jcc (2 bytes: 7x 05) skips exactly the 5-byte jmp rel32
        (void)emit_jcc_inv;
        emit_jcc_short(e, inv_cc, 5);

        // Emit 5-byte trampoline jmp to cold stub (patched later)
        size_t jmp_patch = emit_offset(e) + 1;  // After E9 opcode
        emit_jmp_rel32(e, 0);  // Placeholder, will be patched

        // Record deferred side exit (to be emitted at end of block)
        if (ctx->deferred_exit_count < MAX_BLOCK_EXITS) {
            int di = ctx->deferred_exit_count++;
            ctx->deferred_exits[di].jmp_patch_offset = jmp_patch;
            ctx->deferred_exits[di].target_pc = taken_pc;
            ctx->deferred_exits[di].exit_idx = ctx->exit_idx;
            ctx->deferred_exits[di].branch_pc = branch_pc;
            // Snapshot full allocation state for flushing in cold stub
            // (must capture guest_reg mapping because superblock continuation
            // may evict/reassign slots before cold stubs are emitted)
            for (int s = 0; s < REG_ALLOC_SLOTS; s++) {
                ctx->deferred_exits[di].dirty_snapshot[s] = ctx->reg_alloc[s].dirty;
                ctx->deferred_exits[di].allocated_snapshot[s] = ctx->reg_alloc[s].allocated;
                ctx->deferred_exits[di].guest_reg_snapshot[s] = ctx->reg_alloc[s].guest_reg;
            }
            // Snapshot pending write state (non-cached register deferred store)
            ctx->deferred_exits[di].pending_write_valid = ctx->pending_write.valid;
            ctx->deferred_exits[di].pending_write_guest_reg = ctx->pending_write.guest_reg;
            ctx->deferred_exits[di].pending_write_host_reg = ctx->pending_write.host_reg;

        }

        if (ctx->side_exit_emitted < MAX_BLOCK_EXITS) {
            ctx->side_exit_pcs[ctx->side_exit_emitted] = branch_pc;
        }

        ctx->exit_idx++;
        ctx->side_exit_emitted++;

        // Increment superblock depth and continue with fall-through (hot path)
        ctx->superblock_depth++;
        ctx->guest_pc = fall_pc;
        ctx->inst_count++;

        if (DBT_TRACE) {
            fprintf(stderr, "  [superblock] Extended past branch at 0x%08X, depth=%d\n",
                    fall_pc - 4, ctx->superblock_depth);
        }

        return false;  // Don't end block - continue translating
    }

    // Check for in-block back-edge: if the taken target is within this block,
    // we can emit a direct jmp back to the loop body, skipping the register
    // flush and prologue reload entirely. This is a major win for tight loops.
    size_t backedge_host_offset = (size_t)-1;
    if (imm < 0 && ctx->reg_cache_enabled &&
        taken_pc >= ctx->block_start_pc) {
        backedge_host_offset = pc_map_lookup(ctx, taken_pc);
    }

    if (backedge_host_offset != (size_t)-1) {
        // In-block back-edge: emit tight loop
        //   [compare]
        //   jcc taken_path
        //   [fall-through: flush + exit with fall_pc]
        //   taken_path:
        //   [jmp directly to loop body - no flush, no PC store]

        // IMPORTANT: The back-edge loop means cached registers can accumulate
        // dirty changes across iterations. Any deferred side exit in the loop
        // body has a dirty_snapshot captured during the first (translation-time)
        // pass, which may not reflect registers dirtied in subsequent iterations.
        // Mark registers written in the loop as dirty in all deferred exit snapshots
        // so they are correctly flushed if a side exit is taken after iterating.
        for (int di = 0; di < ctx->deferred_exit_count; di++) {
            for (int s = 0; s < REG_ALLOC_SLOTS; s++) {
                // Use the snapshotted allocation (not current), since slots
                // may have been reassigned since the side exit was captured
                if (ctx->deferred_exits[di].allocated_snapshot[s]) {
                    uint8_t guest_reg = ctx->deferred_exits[di].guest_reg_snapshot[s];
                    if (ctx->loop_written_regs & (1u << guest_reg)) {
                        ctx->deferred_exits[di].dirty_snapshot[s] = true;
                    }
                }
            }
        }

        size_t jcc_patch = emit_offset(e) + 2;
        emit_jcc(e, 0);  // Placeholder

        // Fall-through path: exit the loop
        if (ctx->block && ctx->exit_idx < MAX_BLOCK_EXITS) {
            ctx->block->exits[ctx->exit_idx].branch_pc = branch_pc;
        }
        emit_exit_chained(ctx, fall_pc, ctx->exit_idx++);

        // Taken path: direct jump back to loop body (no flush needed)
        size_t taken_offset = emit_offset(e);
        emit_patch_rel32(e, jcc_patch, taken_offset);
        // Jump directly to the host offset of the back-edge target
        int32_t rel = (int32_t)backedge_host_offset - (int32_t)(emit_offset(e) + 5);
        emit_jmp_rel32(e, rel);

        if (DBT_TRACE) {
            fprintf(stderr, "  [loop] In-block back-edge at 0x%08X -> 0x%08X (host offset %zu)\n",
                    branch_pc, taken_pc, backedge_host_offset);
        }

        return true;  // Block ends
    }

    // Normal mode: end block with both exits
    // Jump to taken path if condition true
    size_t jcc_patch = emit_offset(e) + 2;  // After 0F XX comes the rel32
    emit_jcc(e, 0);  // Placeholder

    // Fall-through path: exit with fall_pc (exit_idx = 0)
    if (ctx->block && ctx->exit_idx < MAX_BLOCK_EXITS) {
        ctx->block->exits[ctx->exit_idx].branch_pc = branch_pc;
    }
    emit_exit_chained(ctx, fall_pc, ctx->exit_idx++);

    // Taken path
    size_t taken_offset = emit_offset(e);
    emit_patch_rel32(e, jcc_patch, taken_offset);
    if (ctx->block && ctx->exit_idx < MAX_BLOCK_EXITS) {
        ctx->block->exits[ctx->exit_idx].branch_pc = branch_pc;
    }
    emit_exit_chained(ctx, taken_pc, ctx->exit_idx++);

    return true;  // Block ends
}

static bool emit_trace_enabled = false;
static uint32_t emit_trace_pc = 0;

void dbt_set_emit_trace(bool enabled, uint32_t pc) {
    emit_trace_enabled = enabled;
    emit_trace_pc = pc;
}

// Return the length of the x86-64 instruction at code[offset], or 0 if unknown.
// Only needs to handle instructions the DBT emitter generates.
static int x64_insn_length(const uint8_t *code, size_t offset, size_t len) {
    size_t i = offset;
    if (i >= len) return 0;

    // SSE mandatory prefixes (0xF3, 0x66) come before REX
    uint8_t sse_prefix = 0;
    if (code[i] == 0xF3 || code[i] == 0x66) {
        sse_prefix = code[i];
        i++;
        if (i >= len) return 0;
    }

    // Consume REX prefix (0x40-0x4F)
    uint8_t rex = 0;
    if (code[i] >= 0x40 && code[i] <= 0x4F) {
        rex = code[i];
        i++;
        if (i >= len) return 0;
    }

    uint8_t op = code[i];

    // SSE instructions with F3 prefix: F3 [REX] 0F xx modrm
    if (sse_prefix == 0xF3 && op == 0x0F) {
        if (i + 2 >= len) return 0;
        uint8_t op2 = code[i + 1];
        // addss(58), subss(5C), mulss(59), divss(5E), sqrtss(51),
        // cvtsi2ss(2A), cvttss2si(2C) — all reg-reg = prefix+[REX]+0F+op+modrm
        if (op2 == 0x58 || op2 == 0x5C || op2 == 0x59 || op2 == 0x5E ||
            op2 == 0x51 || op2 == 0x2A || op2 == 0x2C) {
            return (int)(i - offset + 3); // 0F + op2 + modrm
        }
        return 0;
    }

    // SSE instructions with 66 prefix: 66 [REX] 0F xx modrm
    if (sse_prefix == 0x66 && op == 0x0F) {
        if (i + 2 >= len) return 0;
        uint8_t op2 = code[i + 1];
        // movd xmm,r32 (6E), movd r32,xmm (7E) — reg-reg
        if (op2 == 0x6E || op2 == 0x7E) {
            return (int)(i - offset + 3); // 0F + op2 + modrm
        }
        return 0;
    }

    // 1-byte instructions
    if (op == 0x90) return (int)(i - offset + 1); // NOP
    if (op == 0xC3) return (int)(i - offset + 1); // RET
    if (op == 0xCC) return (int)(i - offset + 1); // INT3
    if ((op & 0xF0) == 0x50) return (int)(i - offset + 1); // PUSH/POP r64
    if (op == 0x99) return (int)(i - offset + 1); // CDQ
    if (op == 0xF4) return (int)(i - offset + 1); // HLT

    // MOV r32/r64, imm32/imm64 (0xB8-0xBF)
    if ((op & 0xF8) == 0xB8) {
        if (rex & 0x08) return (int)(i - offset + 9); // REX.W: mov r64, imm64
        return (int)(i - offset + 5); // mov r32, imm32
    }

    // JMP/CALL rel32 (0xE8, 0xE9)
    if (op == 0xE8 || op == 0xE9) return (int)(i - offset + 5);

    // JMP/Jcc rel8 (0xEB, 0x70-0x7F)
    if (op == 0xEB) return (int)(i - offset + 2);
    if (op >= 0x70 && op <= 0x7F) return (int)(i - offset + 2);

    // 2-byte ALU reg,reg: ADD(01), OR(09), AND(21), SUB(29), XOR(31), CMP(39)
    // TEST(85), MOV(88,89,8B), LEA(8D)
    // These have a ModR/M byte; length depends on addressing mode
    if (op == 0x01 || op == 0x09 || op == 0x21 || op == 0x29 ||
        op == 0x31 || op == 0x39 || op == 0x85 || op == 0x87 ||
        op == 0x88 || op == 0x89 || op == 0x8B || op == 0x8D || op == 0xF7) {
        if (i + 1 >= len) return 0;
        uint8_t modrm = code[i + 1];
        uint8_t mod = modrm & 0xC0;
        uint8_t rm = modrm & 0x07;
        int base = (int)(i - offset + 2);
        if (mod == 0xC0) return base;                // reg,reg
        if (mod == 0x00 && rm == 0x05) return base + 4; // [rip+disp32] or [disp32]
        if (mod == 0x00) {
            if (rm == 0x04) return base + 1;         // SIB, no disp
            return base;                              // [reg]
        }
        if (mod == 0x40) {
            if (rm == 0x04) return base + 2;         // SIB + disp8
            return base + 1;                          // [reg+disp8]
        }
        if (mod == 0x80) {
            if (rm == 0x04) return base + 5;         // SIB + disp32
            return base + 4;                          // [reg+disp32]
        }
        return 0;
    }

    // ALU r/m32, imm32 (0x81 + ModR/M)
    if (op == 0x81) {
        if (i + 1 >= len) return 0;
        uint8_t modrm = code[i + 1];
        uint8_t mod = modrm & 0xC0;
        int base = (int)(i - offset + 2);
        if (mod == 0xC0) return base + 4;  // reg, imm32
        return 0; // memory forms not expected
    }

    // ALU r/m32, imm8 (0x83 + ModR/M)
    if (op == 0x83) {
        if (i + 1 >= len) return 0;
        uint8_t modrm = code[i + 1];
        uint8_t mod = modrm & 0xC0;
        int base = (int)(i - offset + 2);
        if (mod == 0xC0) return base + 1;  // reg, imm8
        return 0;
    }

    // C7 group: MOV r/m32, imm32
    if (op == 0xC7) {
        if (i + 1 >= len) return 0;
        uint8_t modrm = code[i + 1];
        uint8_t mod = modrm & 0xC0;
        uint8_t rm = modrm & 0x07;
        int base = (int)(i - offset + 2);
        if (mod == 0xC0) return base + 4;              // reg, imm32
        if (mod == 0x00) {
            if (rm == 0x04) return base + 5;           // SIB + imm32
            if (rm == 0x05) return base + 8;           // [rip+disp32] + imm32
            return base + 4;                            // [reg] + imm32
        }
        if (mod == 0x40) {
            if (rm == 0x04) return base + 6;           // SIB+disp8+imm32
            return base + 5;                            // [reg+disp8]+imm32
        }
        if (mod == 0x80) {
            if (rm == 0x04) return base + 9;           // SIB+disp32+imm32
            return base + 8;                            // [reg+disp32]+imm32
        }
        return 0;
    }

    // FF group: INC/DEC/CALL/JMP indirect
    if (op == 0xFF) {
        if (i + 1 >= len) return 0;
        uint8_t modrm = code[i + 1];
        uint8_t mod = modrm & 0xC0;
        int base = (int)(i - offset + 2);
        if (mod == 0xC0) return base;
        return 0;
    }

    // 0F prefix: two-byte opcodes
    if (op == 0x0F) {
        if (i + 1 >= len) return 0;
        uint8_t op2 = code[i + 1];
        // ucomiss xmm, xmm (0x0F 0x2E + ModR/M)
        if (op2 == 0x2E) return (int)(i - offset + 3);
        // Jcc rel32 (0x0F 0x80-0x8F)
        if (op2 >= 0x80 && op2 <= 0x8F) return (int)(i - offset + 6);
        // SETcc (0x0F 0x90-0x9F + ModR/M)
        if (op2 >= 0x90 && op2 <= 0x9F) return (int)(i - offset + 3);
        // MOVZX (0x0F 0xB6/0xB7) / MOVSX (0x0F 0xBE/0xBF) + ModR/M
        if (op2 == 0xB6 || op2 == 0xB7 || op2 == 0xBE || op2 == 0xBF) {
            if (i + 2 >= len) return 0;
            uint8_t modrm = code[i + 2];
            uint8_t mod = modrm & 0xC0;
            uint8_t rm = modrm & 0x07;
            int base = (int)(i - offset + 3);
            if (mod == 0xC0) return base;
            if (mod == 0x80) {
                if (rm == 0x04) return base + 5;
                return base + 4;
            }
            if (mod == 0x40) {
                if (rm == 0x04) return base + 2;
                return base + 1;
            }
            if (mod == 0x00) {
                if (rm == 0x04) return base + 1;
                if (rm == 0x05) return base + 4;
                return base;
            }
            return 0;
        }
        // IMUL r32, r/m32 (0x0F 0xAF + ModR/M)
        if (op2 == 0xAF) {
            if (i + 2 >= len) return 0;
            uint8_t modrm = code[i + 2];
            uint8_t mod = modrm & 0xC0;
            int base = (int)(i - offset + 3);
            if (mod == 0xC0) return base;
            return 0;
        }
        return 0;
    }

    // IDIV / DIV / MUL / IMUL (F7 /reg) - handled above with 0xF7
    // SHIFT (D3) - shift by CL
    if (op == 0xD3) {
        if (i + 1 >= len) return 0;
        uint8_t modrm = code[i + 1];
        uint8_t mod = modrm & 0xC0;
        if (mod == 0xC0) return (int)(i - offset + 2);
        return 0;
    }

    // SHIFT by imm8 (C1)
    if (op == 0xC1) {
        if (i + 1 >= len) return 0;
        uint8_t modrm = code[i + 1];
        uint8_t mod = modrm & 0xC0;
        if (mod == 0xC0) return (int)(i - offset + 3); // reg, imm8
        return 0;
    }

    return 0; // Unknown
}

static size_t peephole_optimize_x64(uint8_t *code, size_t len) {
    size_t hits = 0;
    size_t i = 0;
    while (i + 6 < len) {
        size_t i0 = i;

        // Pattern JCC: Jcc +5 ; JMP rel32  ->  inverted-Jcc (combined target)
        // The emitter produces "ja +5; jmp target" for bounds checks.
        // We fold into a single "jbe target" saving 5 bytes.
        // Matches: 0F 8x [rel32=05000000] E9 [rel32]
        if (i0 + 11 <= len &&
            code[i0] == 0x0F &&
            (code[i0 + 1] & 0xF0) == 0x80 &&        // Jcc near (0F 80..8F)
            code[i0 + 2] == 0x05 &&                   // rel32 = 5
            code[i0 + 3] == 0x00 &&
            code[i0 + 4] == 0x00 &&
            code[i0 + 5] == 0x00 &&
            code[i0 + 6] == 0xE9) {                   // JMP rel32
            // Invert condition: flip low bit of cc byte
            uint8_t cc = code[i0 + 1];
            uint8_t inv_cc = cc ^ 0x01;
            // The JMP target is relative to end of JMP (i0+11).
            // New Jcc target must be relative to end of new Jcc (i0+6, since 5 bytes of JMP gone).
            // Original JMP displacement is at i0+7..i0+10.
            int32_t jmp_disp = (int32_t)((uint32_t)code[i0 + 7] |
                                         ((uint32_t)code[i0 + 8] << 8) |
                                         ((uint32_t)code[i0 + 9] << 16) |
                                         ((uint32_t)code[i0 + 10] << 24));
            // New displacement: original target was at i0+11+jmp_disp.
            // New instruction ends at i0+6, so new_disp = (i0+11+jmp_disp) - (i0+6) = jmp_disp + 5.
            int32_t new_disp = jmp_disp + 5;
            code[i0 + 0] = 0x0F;
            code[i0 + 1] = inv_cc;
            code[i0 + 2] = (uint8_t)(new_disp & 0xFF);
            code[i0 + 3] = (uint8_t)((new_disp >> 8) & 0xFF);
            code[i0 + 4] = (uint8_t)((new_disp >> 16) & 0xFF);
            code[i0 + 5] = (uint8_t)((new_disp >> 24) & 0xFF);
            // NOP out the former JMP
            for (size_t k = i0 + 6; k < i0 + 11; k++) {
                code[k] = 0x90;
            }
            hits++;
            i = i0 + 11;
            continue;
        }

        // Pattern DSTORE: mov [rbp+disp8], imm32 ; mov [rbp+disp8], imm32 (same disp)
        // Dead first store — second write overwrites the same address.
        // Form: C7 45 disp8 imm32 (7 bytes)  C7 45 disp8 imm32 (7 bytes)
        if (i0 + 14 <= len &&
            code[i0] == 0xC7 && code[i0 + 1] == 0x45 &&
            code[i0 + 7] == 0xC7 && code[i0 + 8] == 0x45 &&
            code[i0 + 2] == code[i0 + 9]) {  // same disp8
            // NOP out first store
            for (size_t k = i0; k < i0 + 7; k++) {
                code[k] = 0x90;
            }
            hits++;
            i = i0 + 7;
            continue;
        }

        // Pattern DSTORE32: mov [rbp+disp32], imm32 ; mov [rbp+disp32], imm32 (same disp)
        // Form: C7 85 disp32 imm32 (10 bytes)  C7 85 disp32 imm32 (10 bytes)
        if (i0 + 20 <= len &&
            code[i0] == 0xC7 && code[i0 + 1] == 0x85 &&
            code[i0 + 10] == 0xC7 && code[i0 + 11] == 0x85 &&
            code[i0 + 2] == code[i0 + 12] &&  // same disp32 (byte 0)
            code[i0 + 3] == code[i0 + 13] &&
            code[i0 + 4] == code[i0 + 14] &&
            code[i0 + 5] == code[i0 + 15]) {
            // NOP out first store
            for (size_t k = i0; k < i0 + 10; k++) {
                code[k] = 0x90;
            }
            hits++;
            i = i0 + 10;
            continue;
        }

        // Pattern MOV_CMP: mov rA, rB ; cmp/test using rA → substitute rB, NOP mov
        // Only when rA is provably dead (next insn is SETcc/Jcc/JMP/mov-to-rA)
        if (i0 + 4 <= len) {
            uint8_t op_mov = code[i0];
            if (op_mov == 0x89 || op_mov == 0x8B) {
                uint8_t modrm_mov = code[i0 + 1];
                if ((modrm_mov & 0xC0) == 0xC0) {
                    uint8_t mov_dst, mov_src;
                    if (op_mov == 0x89) {
                        mov_dst = modrm_mov & 0x07;
                        mov_src = (modrm_mov >> 3) & 0x07;
                    } else {
                        mov_dst = (modrm_mov >> 3) & 0x07;
                        mov_src = modrm_mov & 0x07;
                    }
                    if (mov_dst != mov_src) {
                        size_t cmp_pos = i0 + 2;
                        uint8_t op_cmp = code[cmp_pos];
                        if (cmp_pos + 2 <= len &&
                            (op_cmp == 0x39 || op_cmp == 0x3B || op_cmp == 0x85)) {
                            uint8_t modrm_cmp = code[cmp_pos + 1];
                            if ((modrm_cmp & 0xC0) == 0xC0) {
                                uint8_t cmp_reg = (modrm_cmp >> 3) & 0x07;
                                uint8_t cmp_rm = modrm_cmp & 0x07;
                                bool in_reg = (cmp_reg == mov_dst);
                                bool in_rm = (cmp_rm == mov_dst);
                                if (in_reg || in_rm) {
                                    size_t after = cmp_pos + 2;
                                    bool dead = false;
                                    if (after < len) {
                                        uint8_t next = code[after];
                                        // SETcc (0F 90-9F) or Jcc near (0F 80-8F)
                                        if (after + 1 < len && next == 0x0F) {
                                            uint8_t next2 = code[after + 1];
                                            if ((next2 >= 0x90 && next2 <= 0x9F) ||
                                                (next2 >= 0x80 && next2 <= 0x8F))
                                                dead = true;
                                        }
                                        // Jcc short (70-7F)
                                        if (next >= 0x70 && next <= 0x7F) dead = true;
                                        // JMP (E9, EB)
                                        if (next == 0xE9 || next == 0xEB) dead = true;
                                        // MOV overwriting mov_dst: 89 xx with rm=mov_dst, mod=C0
                                        if (next == 0x89 && after + 1 < len) {
                                            uint8_t nm = code[after + 1];
                                            if ((nm & 0xC0) == 0xC0 && (nm & 0x07) == mov_dst)
                                                dead = true;
                                        }
                                        // 8B xx with reg=mov_dst
                                        if (next == 0x8B && after + 1 < len) {
                                            uint8_t nm = code[after + 1];
                                            if (((nm >> 3) & 0x07) == mov_dst)
                                                dead = true;
                                        }
                                        // B8+rd imm32
                                        if ((next & 0xF8) == 0xB8 && (next & 0x07) == mov_dst)
                                            dead = true;
                                    }
                                    if (dead) {
                                        uint8_t new_modrm = modrm_cmp;
                                        if (in_reg)
                                            new_modrm = (new_modrm & 0xC7) | (mov_src << 3);
                                        if (in_rm)
                                            new_modrm = (new_modrm & 0xF8) | mov_src;
                                        code[cmp_pos + 1] = new_modrm;
                                        code[i0] = 0x90;
                                        code[i0 + 1] = 0x90;
                                        hits++;
                                        i = cmp_pos;
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Pattern 0: xor r32, r32 ; cmp r32, r32 -> xor r32, r32 ; test other, other
        // Keeps the zeroed register intact but removes dependency on it.
        if (i0 + 3 < len && code[i0] == 0x31) {
            uint8_t modrm_xor = code[i0 + 1];
            uint8_t mod_xor = modrm_xor & 0xC0;
            uint8_t reg_xor = (modrm_xor >> 3) & 0x07;
            uint8_t rm_xor = modrm_xor & 0x07;
            if (mod_xor == 0xC0 && reg_xor == rm_xor) {
                if (code[i0 + 2] == 0x39) {
                    uint8_t modrm_cmp = code[i0 + 3];
                    uint8_t mod_cmp = modrm_cmp & 0xC0;
                    uint8_t reg_cmp = (modrm_cmp >> 3) & 0x07;
                    uint8_t rm_cmp = modrm_cmp & 0x07;
                    if (mod_cmp == 0xC0 && reg_cmp == reg_xor) {
                        code[i0 + 2] = 0x85; // test r/m32, r32
                        code[i0 + 3] = (uint8_t)(0xC0 | (rm_cmp << 3) | rm_cmp);
                        hits++;
                        i = i0 + 4;
                        continue;
                    }
                }
            }
        }

        // Pattern 0b: mov r32, imm32 ; mov [rbp+disp32], r32 ; mov r32, imm32
        // Replace first two with mov [rbp+disp32], imm32 (safe because r32 is overwritten).
        if (i0 + 15 < len) {
            uint8_t op0 = code[i0];
            if ((op0 & 0xF8) == 0xB8) {
                uint8_t rd = op0 & 0x07;
                uint32_t imm0 = (uint32_t)code[i0 + 1] |
                                ((uint32_t)code[i0 + 2] << 8) |
                                ((uint32_t)code[i0 + 3] << 16) |
                                ((uint32_t)code[i0 + 4] << 24);
                size_t p1 = i0 + 5;
                if (code[p1] == 0x89) {
                    uint8_t modrm1 = code[p1 + 1];
                    uint8_t mod1 = modrm1 & 0xC0;
                    uint8_t rm1 = modrm1 & 0x07;
                    uint8_t reg1 = (modrm1 >> 3) & 0x07;
                    if (mod1 == 0x80 && rm1 == 0x05 && reg1 == rd) {
                        uint32_t disp = (uint32_t)code[p1 + 2] |
                                        ((uint32_t)code[p1 + 3] << 8) |
                                        ((uint32_t)code[p1 + 4] << 16) |
                                        ((uint32_t)code[p1 + 5] << 24);
                        size_t p2 = p1 + 6;
                        if (code[p2] == (uint8_t)(0xB8 | rd)) {
                            // Emit: C7 85 disp32 imm32 + pad
                            code[i0] = 0xC7;
                            code[i0 + 1] = 0x85;
                            code[i0 + 2] = (uint8_t)(disp & 0xFF);
                            code[i0 + 3] = (uint8_t)((disp >> 8) & 0xFF);
                            code[i0 + 4] = (uint8_t)((disp >> 16) & 0xFF);
                            code[i0 + 5] = (uint8_t)((disp >> 24) & 0xFF);
                            code[i0 + 6] = (uint8_t)(imm0 & 0xFF);
                            code[i0 + 7] = (uint8_t)((imm0 >> 8) & 0xFF);
                            code[i0 + 8] = (uint8_t)((imm0 >> 16) & 0xFF);
                            code[i0 + 9] = (uint8_t)((imm0 >> 24) & 0xFF);
                            code[i0 + 10] = 0x90;
                            hits++;
                            i = i0 + 11;
                            continue;
                        }
                    }
                }
            }
        }

        // Pattern 0c: mov r32, 0 ; mov [rbp+disp32], r32 ; mov r32, 0 ; mov [rbp+disp32], r32
        // Remove the second mov r32, 0 (reg is still zero).
        if (i0 + 21 < len) {
            uint8_t op0 = code[i0];
            if ((op0 & 0xF8) == 0xB8) {
                uint8_t rd = op0 & 0x07;
                uint32_t imm0 = (uint32_t)code[i0 + 1] |
                                ((uint32_t)code[i0 + 2] << 8) |
                                ((uint32_t)code[i0 + 3] << 16) |
                                ((uint32_t)code[i0 + 4] << 24);
                size_t p1 = i0 + 5;
                if (imm0 == 0 && code[p1] == 0x89) {
                    uint8_t modrm1 = code[p1 + 1];
                    uint8_t mod1 = modrm1 & 0xC0;
                    uint8_t rm1 = modrm1 & 0x07;
                    uint8_t reg1 = (modrm1 >> 3) & 0x07;
                    if (mod1 == 0x80 && rm1 == 0x05 && reg1 == rd) {
                        size_t p2 = p1 + 6;
                        if (code[p2] == (uint8_t)(0xB8 | rd)) {
                            uint32_t imm1 = (uint32_t)code[p2 + 1] |
                                            ((uint32_t)code[p2 + 2] << 8) |
                                            ((uint32_t)code[p2 + 3] << 16) |
                                            ((uint32_t)code[p2 + 4] << 24);
                            if (imm1 == 0 && code[p2 + 5] == 0x89) {
                                uint8_t modrm2 = code[p2 + 6];
                                uint8_t mod2 = modrm2 & 0xC0;
                                uint8_t rm2 = modrm2 & 0x07;
                                uint8_t reg2 = (modrm2 >> 3) & 0x07;
                                if (mod2 == 0x80 && rm2 == 0x05 && reg2 == rd) {
                                    for (size_t k = p2; k < p2 + 5; k++) {
                                        code[k] = 0x90;
                                    }
                                    hits++;
                                    i = p2 + 5;
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
        }

        // Pattern A: mov r32, [rbp+disp32] ; mov [rbp+disp32], r32 (same reg/disp)
        // Pattern B: mov [rbp+disp32], r32 ; mov r32, [rbp+disp32] (same reg/disp)
        // Pattern C: mov r32, imm32 ; mov r32, imm32 (same reg) -> kill first

        // Try Pattern C first (imm32 to same reg)
        size_t p = i;
        uint8_t rex_a = 0;
        if (code[p] >= 0x40 && code[p] <= 0x4F) {
            rex_a = code[p++];
        }
        if (p < len && (code[p] & 0xF8) == 0xB8) {
            uint8_t rd0 = (code[p] & 0x07) | ((rex_a & 0x01) ? 0x08 : 0x00);
            size_t imm0 = p + 1;
            size_t end0 = imm0 + 4;
            if (end0 <= len) {
                size_t q = end0;
                uint8_t rex_b = 0;
                if (q < len && code[q] >= 0x40 && code[q] <= 0x4F) {
                    rex_b = code[q++];
                }
                if (q < len && (code[q] & 0xF8) == 0xB8) {
                    uint8_t rd1 = (code[q] & 0x07) | ((rex_b & 0x01) ? 0x08 : 0x00);
                    size_t imm1 = q + 1;
                    size_t end1 = imm1 + 4;
                    if (end1 <= len && rd0 == rd1) {
                        for (size_t k = i0; k < end0; k++) {
                            code[k] = 0x90;
                        }
                        hits++;
                        i = end0;
                        continue;
                    }
                }
                // No Pattern C match — skip past this mov r32, imm32 instruction
                // to avoid landing inside the immediate on the next iteration.
                i = end0;
                continue;
            }
        }

        // Try Pattern A/B
        size_t a = i0;
        uint8_t rex0 = 0;
        if (code[a] >= 0x40 && code[a] <= 0x4F) {
            rex0 = code[a++];
        }
        if (a >= len) goto skip_insn;
        uint8_t op0 = code[a++];
        if (a >= len) goto skip_insn;
        uint8_t modrm0 = code[a++];
        uint8_t mod0 = modrm0 & 0xC0;
        uint8_t rm0 = modrm0 & 0x07;
        uint8_t reg0 = ((modrm0 >> 3) & 0x07) | ((rex0 & 0x4) ? 0x08 : 0x00);
        if (mod0 != 0x80 || rm0 != 0x05 || (rex0 & 0x01)) goto skip_insn;
        if (a + 4 > len) goto skip_insn;
        uint32_t disp0 = (uint32_t)code[a] |
                         ((uint32_t)code[a + 1] << 8) |
                         ((uint32_t)code[a + 2] << 16) |
                         ((uint32_t)code[a + 3] << 24);
        a += 4;
        size_t end0 = a;

        size_t b = a;
        uint8_t rex1 = 0;
        if (b < len && code[b] >= 0x40 && code[b] <= 0x4F) {
            rex1 = code[b++];
        }
        if (b >= len) goto skip_insn;
        uint8_t op1 = code[b++];
        if (b >= len) goto skip_insn;
        uint8_t modrm1 = code[b++];
        uint8_t mod1 = modrm1 & 0xC0;
        uint8_t rm1 = modrm1 & 0x07;
        uint8_t reg1 = ((modrm1 >> 3) & 0x07) | ((rex1 & 0x4) ? 0x08 : 0x00);
        if (mod1 != 0x80 || rm1 != 0x05 || (rex1 & 0x01)) goto skip_insn;
        if (b + 4 > len) goto skip_insn;
        uint32_t disp1 = (uint32_t)code[b] |
                         ((uint32_t)code[b + 1] << 8) |
                         ((uint32_t)code[b + 2] << 16) |
                         ((uint32_t)code[b + 3] << 24);
        b += 4;
        size_t end1 = b;

        if (disp0 == disp1 && reg0 == reg1) {
            if (op0 == 0x8B && op1 == 0x89) {
                // Load then store back to same slot: remove store.
                for (size_t k = end0; k < end1; k++) {
                    code[k] = 0x90;
                }
                hits++;
                i = end1;
                continue;
            }
            if (op0 == 0x89 && op1 == 0x8B) {
                // Store then reload into same reg: remove load.
                for (size_t k = end0; k < end1; k++) {
                    code[k] = 0x90;
                }
                hits++;
                i = end1;
                continue;
            }
        }

skip_insn:;
        int skip = x64_insn_length(code, i0, len);
        if (skip <= 0) break;  // Unknown instruction, stop scanning
        i = i0 + skip;
    }
    return hits;
}

// NOP compaction: remove 0x90 NOPs left by peephole optimizer and adjust jumps
static size_t nop_compact_x64(uint8_t *code, size_t len,
                               translated_block_t *block)
{
    // Skip oversized blocks to limit stack usage (~3 bytes per position)
    if (len > 16384) return len;

    // Phase 1: Classify bytes — find standalone 1-byte NOPs
    bool is_nop[len];
    memset(is_nop, 0, sizeof(is_nop));
    size_t pos = 0;
    while (pos < len) {
        int ilen = x64_insn_length(code, pos, len);
        if (ilen <= 0) break;
        if (ilen == 1 && code[pos] == 0x90)
            is_nop[pos] = true;
        pos += ilen;
    }
    size_t scanned_end = pos;

    // If we couldn't scan the entire block, skip compaction — we can't
    // safely adjust branches in the unscanned tail.
    if (scanned_end < len)
        return len;

    // Count NOPs; bail if nothing to compact
    size_t nop_count = 0;
    for (size_t i = 0; i < scanned_end; i++)
        if (is_nop[i]) nop_count++;
    if (nop_count == 0) return len;

    // Phase 2: Build cumulative shift map
    uint16_t shift[len + 1];
    uint16_t cum = 0;
    for (size_t i = 0; i <= len; i++) {
        shift[i] = cum;
        if (i < len && is_nop[i]) cum++;
    }

    // Phase 3: Adjust relative displacements before compacting
    pos = 0;
    while (pos < scanned_end) {
        int ilen = x64_insn_length(code, pos, len);
        if (ilen <= 0) break;
        if (is_nop[pos]) { pos += ilen; continue; }

        uint8_t op = code[pos];
        size_t disp_off = 0;  // offset of displacement within instruction
        size_t insn_len = 0;
        int disp_size = 0;    // 1 for rel8, 4 for rel32

        // Jcc near: 0F 8x rel32 (6 bytes)
        if (pos + 1 < scanned_end && op == 0x0F && (code[pos + 1] & 0xF0) == 0x80) {
            disp_off = 2; insn_len = 6; disp_size = 4;
        }
        // JMP near: E9 rel32 (5 bytes)
        else if (op == 0xE9) {
            disp_off = 1; insn_len = 5; disp_size = 4;
        }
        // CALL near: E8 rel32 (5 bytes)
        else if (op == 0xE8) {
            disp_off = 1; insn_len = 5; disp_size = 4;
        }
        // Jcc short: 7x rel8 (2 bytes)
        else if ((op & 0xF0) == 0x70) {
            disp_off = 1; insn_len = 2; disp_size = 1;
        }
        // JMP short: EB rel8 (2 bytes)
        else if (op == 0xEB) {
            disp_off = 1; insn_len = 2; disp_size = 1;
        }

        if (disp_size > 0 && pos + insn_len <= scanned_end) {
            int32_t old_disp;
            if (disp_size == 4) {
                old_disp = (int32_t)((uint32_t)code[pos + disp_off] |
                           ((uint32_t)code[pos + disp_off + 1] << 8) |
                           ((uint32_t)code[pos + disp_off + 2] << 16) |
                           ((uint32_t)code[pos + disp_off + 3] << 24));
            } else {
                old_disp = (int8_t)code[pos + disp_off];
            }

            int64_t old_target = (int64_t)pos + (int64_t)insn_len + (int64_t)old_disp;
            int32_t new_disp;

            if (old_target >= 0 && (size_t)old_target <= scanned_end) {
                // Intra-block: both source and target shift
                new_disp = old_disp + (int32_t)shift[pos] - (int32_t)shift[(size_t)old_target];
            } else {
                // Extra-block: only source shifts
                new_disp = old_disp + (int32_t)shift[pos];
            }

            if (disp_size == 1) {
                // Check rel8 overflow
                if (new_disp < -128 || new_disp > 127)
                    return len;  // Abort compaction
                code[pos + disp_off] = (uint8_t)(int8_t)new_disp;
            } else {
                code[pos + disp_off + 0] = (uint8_t)(new_disp & 0xFF);
                code[pos + disp_off + 1] = (uint8_t)((new_disp >> 8) & 0xFF);
                code[pos + disp_off + 2] = (uint8_t)((new_disp >> 16) & 0xFF);
                code[pos + disp_off + 3] = (uint8_t)((new_disp >> 24) & 0xFF);
            }
        }

        pos += ilen;
    }

    // Phase 4: Compact — remove NOP bytes
    size_t new_len = 0;
    for (size_t j = 0; j < scanned_end; j++) {
        if (!is_nop[j])
            code[new_len++] = code[j];
    }
    // Copy any unscanned tail verbatim
    for (size_t j = scanned_end; j < len; j++) {
        code[new_len++] = code[j];
    }

    // Phase 5: Adjust patch_site pointers in block exits
    for (uint8_t i = 0; i < block->exit_count; i++) {
        size_t old_off = (size_t)(block->exits[i].patch_site - code);
        if (old_off < scanned_end) {
            block->exits[i].patch_site = code + (old_off - shift[old_off]);
        }
    }

    // Phase 6: Fill freed tail with INT3 for debugging
    memset(code + new_len, 0xCC, len - new_len);

    return new_len;
}

// Branch functions now return true if block ends, false if superblock continues
bool translate_beq(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_je_rel32, emit_jne_rel32, 0x05);
}

bool translate_bne(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_jne_rel32, emit_je_rel32, 0x04);
}

bool translate_blt(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_jl_rel32, emit_jge_rel32, 0x0D);
}

bool translate_bge(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_jge_rel32, emit_jl_rel32, 0x0C);
}

bool translate_bltu(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_jb_rel32, emit_jae_rel32, 0x03);
}

bool translate_bgeu(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_jae_rel32, emit_jb_rel32, 0x02);
}

// ============================================================================
// Special instructions
// ============================================================================

void translate_nop(translate_ctx_t *ctx) {
    // Nothing to do
    (void)ctx;
}

void translate_halt(translate_ctx_t *ctx) {
    emit_exit(ctx, EXIT_HALT, ctx->guest_pc + 4);
}

void translate_debug(translate_ctx_t *ctx, uint8_t rs1) {
    emit_ctx_t *e = &ctx->emit;

    // Load character to output
    emit_load_guest_reg(ctx, RAX, rs1);
    emit_mov_m32_r32(e, RBP, CPU_EXIT_INFO_OFFSET, RAX);

    emit_exit(ctx, EXIT_DEBUG, ctx->guest_pc + 4);
}

void translate_yield(translate_ctx_t *ctx) {
    emit_exit(ctx, EXIT_YIELD, ctx->guest_pc + 4);
}

// ============================================================================
// Floating-point helper (called from JIT code)
// ============================================================================

static inline void load_f64_pair(uint32_t *regs, uint8_t reg, double *out) {
    if (reg >= 31 || (reg & 1)) {
        fprintf(stderr, "f64 register fault: r%d is invalid (must be even, < 31)\n", reg);
        *out = 0.0;
        return;
    }
    uint64_t bits = ((uint64_t)regs[reg + 1] << 32) | regs[reg];
    memcpy(out, &bits, 8);
}

static inline void store_f64_pair(uint32_t *regs, uint8_t reg, double val) {
    if (reg >= 31 || (reg & 1)) {
        fprintf(stderr, "f64 register fault: r%d is invalid (must be even, < 31)\n", reg);
        return;
    }
    uint64_t bits;
    memcpy(&bits, &val, 8);
    regs[reg] = (uint32_t)bits;
    regs[reg + 1] = (uint32_t)(bits >> 32);
}

static inline bool load_u64_pair(uint32_t *regs, uint8_t reg, uint64_t *out) {
    if (reg >= 31 || (reg & 1)) {
        fprintf(stderr, "f64 register fault: r%d is invalid (must be even, < 31)\n", reg);
        *out = 0;
        return false;
    }
    *out = ((uint64_t)regs[reg + 1] << 32) | regs[reg];
    return true;
}

static inline bool store_u64_pair(uint32_t *regs, uint8_t reg, uint64_t val) {
    if (reg >= 31 || (reg & 1)) {
        fprintf(stderr, "f64 register fault: r%d is invalid (must be even, < 31)\n", reg);
        return false;
    }
    regs[reg] = (uint32_t)val;
    regs[reg + 1] = (uint32_t)(val >> 32);
    return true;
}

// This function is called from JIT code via emit_call_r64.
// RDI = cpu state pointer, ESI = opcode, EDX = rd, ECX = rs1, R8D = rs2
void dbt_fp_helper(dbt_cpu_state_t *cpu, uint32_t opcode,
                   uint32_t rd, uint32_t rs1, uint32_t rs2) {
    uint32_t *r = cpu->regs;

    switch (opcode) {
    // f32 arithmetic
    case OP_FADD_S: case OP_FSUB_S: case OP_FMUL_S: case OP_FDIV_S: {
        float a, b, res;
        memcpy(&a, &r[rs1], 4);
        memcpy(&b, &r[rs2], 4);
        switch (opcode) {
            case OP_FADD_S: res = a + b; break;
            case OP_FSUB_S: res = a - b; break;
            case OP_FMUL_S: res = a * b; break;
            default:        res = a / b; break;
        }
        memcpy(&r[rd], &res, 4);
        break;
    }
    case OP_FSQRT_S: {
        float a, res; memcpy(&a, &r[rs1], 4);
        res = sqrtf(a);
        memcpy(&r[rd], &res, 4);
        break;
    }
    case OP_FEQ_S: { float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); r[rd] = (a == b) ? 1 : 0; break; }
    case OP_FLT_S: { float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); r[rd] = (a < b) ? 1 : 0; break; }
    case OP_FLE_S: { float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); r[rd] = (a <= b) ? 1 : 0; break; }
    case OP_FCVT_W_S: { float a; memcpy(&a, &r[rs1], 4); r[rd] = (uint32_t)(int32_t)a; break; }
    case OP_FCVT_WU_S: { float a; memcpy(&a, &r[rs1], 4); r[rd] = (uint32_t)a; break; }
    case OP_FCVT_S_W: { float res = (float)(int32_t)r[rs1]; memcpy(&r[rd], &res, 4); break; }
    case OP_FCVT_S_WU: { float res = (float)r[rs1]; memcpy(&r[rd], &res, 4); break; }
    case OP_FNEG_S: r[rd] = r[rs1] ^ 0x80000000u; break;
    case OP_FABS_S: r[rd] = r[rs1] & 0x7FFFFFFFu; break;

    // f64 arithmetic
    case OP_FADD_D: case OP_FSUB_D: case OP_FMUL_D: case OP_FDIV_D: {
        double a, b, res;
        load_f64_pair(r, rs1, &a); load_f64_pair(r, rs2, &b);
        switch (opcode) {
            case OP_FADD_D: res = a + b; break;
            case OP_FSUB_D: res = a - b; break;
            case OP_FMUL_D: res = a * b; break;
            default:        res = a / b; break;
        }
        store_f64_pair(r, rd, res);
        break;
    }
    case OP_FSQRT_D: { double a, res; load_f64_pair(r, rs1, &a); res = sqrt(a); store_f64_pair(r, rd, res); break; }
    case OP_FEQ_D: { double a, b; load_f64_pair(r, rs1, &a); load_f64_pair(r, rs2, &b); r[rd] = (a == b) ? 1 : 0; break; }
    case OP_FLT_D: { double a, b; load_f64_pair(r, rs1, &a); load_f64_pair(r, rs2, &b); r[rd] = (a < b) ? 1 : 0; break; }
    case OP_FLE_D: { double a, b; load_f64_pair(r, rs1, &a); load_f64_pair(r, rs2, &b); r[rd] = (a <= b) ? 1 : 0; break; }
    case OP_FCVT_W_D: { double a; load_f64_pair(r, rs1, &a); r[rd] = (uint32_t)(int32_t)a; break; }
    case OP_FCVT_WU_D: { double a; load_f64_pair(r, rs1, &a); r[rd] = (uint32_t)a; break; }
    case OP_FCVT_D_W: { store_f64_pair(r, rd, (double)(int32_t)r[rs1]); break; }
    case OP_FCVT_D_WU: { store_f64_pair(r, rd, (double)r[rs1]); break; }
    case OP_FCVT_D_S: { float a; memcpy(&a, &r[rs1], 4); store_f64_pair(r, rd, (double)a); break; }
    case OP_FCVT_S_D: { double a; load_f64_pair(r, rs1, &a); float res = (float)a; memcpy(&r[rd], &res, 4); break; }
    case OP_FNEG_D: {
        double a;
        load_f64_pair(r, rs1, &a);
        uint64_t bits;
        memcpy(&bits, &a, 8);
        bits ^= 0x8000000000000000ull;
        memcpy(&a, &bits, 8);
        store_f64_pair(r, rd, a);
        break;
    }
    case OP_FABS_D: {
        double a;
        load_f64_pair(r, rs1, &a);
        uint64_t bits;
        memcpy(&bits, &a, 8);
        bits &= 0x7FFFFFFFFFFFFFFFull;
        memcpy(&a, &bits, 8);
        store_f64_pair(r, rd, a);
        break;
    }

    // float <-> int64
    case OP_FCVT_L_S: {
        float a; memcpy(&a, &r[rs1], 4);
        int64_t v = (int64_t)a;
        store_u64_pair(r, rd, (uint64_t)v);
        break;
    }
    case OP_FCVT_LU_S: {
        float a; memcpy(&a, &r[rs1], 4);
        uint64_t v = (uint64_t)a;
        store_u64_pair(r, rd, v);
        break;
    }
    case OP_FCVT_S_L: {
        uint64_t v;
        if (!load_u64_pair(r, rs1, &v)) break;
        float res = (float)(int64_t)v;
        memcpy(&r[rd], &res, 4);
        break;
    }
    case OP_FCVT_S_LU: {
        uint64_t v;
        if (!load_u64_pair(r, rs1, &v)) break;
        float res = (float)v;
        memcpy(&r[rd], &res, 4);
        break;
    }
    case OP_FCVT_L_D: {
        double a; load_f64_pair(r, rs1, &a);
        int64_t v = (int64_t)a;
        store_u64_pair(r, rd, (uint64_t)v);
        break;
    }
    case OP_FCVT_LU_D: {
        double a; load_f64_pair(r, rs1, &a);
        uint64_t v = (uint64_t)a;
        store_u64_pair(r, rd, v);
        break;
    }
    case OP_FCVT_D_L: {
        uint64_t v;
        if (!load_u64_pair(r, rs1, &v)) break;
        store_f64_pair(r, rd, (double)(int64_t)v);
        break;
    }
    case OP_FCVT_D_LU: {
        uint64_t v;
        if (!load_u64_pair(r, rs1, &v)) break;
        store_f64_pair(r, rd, (double)v);
        break;
    }
    }

    r[0] = 0;  // Ensure r0 stays 0
}

// ============================================================================
// Inline f32 FP translation (SSE)
// ============================================================================

// f32 arithmetic: FADD_S, FSUB_S, FMUL_S, FDIV_S
static void translate_fp_f32_arith(translate_ctx_t *ctx, uint8_t opcode, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    emit_ctx_t *e = &ctx->emit;
    flush_pending_write(ctx);
    emit_load_guest_reg(ctx, RAX, rs1);
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_movd_xmm_r32(e, 0, RAX);
    emit_movd_xmm_r32(e, 1, RCX);
    switch (opcode) {
        case OP_FADD_S: emit_addss(e, 0, 1); break;
        case OP_FSUB_S: emit_subss(e, 0, 1); break;
        case OP_FMUL_S: emit_mulss(e, 0, 1); break;
        case OP_FDIV_S: emit_divss(e, 0, 1); break;
    }
    emit_movd_r32_xmm(e, RAX, 0);
    emit_store_guest_reg(ctx, rd, RAX);
}

// f32 sqrt: FSQRT_S
static void translate_fp_f32_sqrt(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1) {
    emit_ctx_t *e = &ctx->emit;
    flush_pending_write(ctx);
    emit_load_guest_reg(ctx, RAX, rs1);
    emit_movd_xmm_r32(e, 0, RAX);
    emit_sqrtss(e, 0, 0);
    emit_movd_r32_xmm(e, RAX, 0);
    emit_store_guest_reg(ctx, rd, RAX);
}

// f32 comparisons: FEQ_S, FLT_S, FLE_S
static void translate_fp_f32_cmp(translate_ctx_t *ctx, uint8_t opcode, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    emit_ctx_t *e = &ctx->emit;
    flush_pending_write(ctx);
    emit_load_guest_reg(ctx, RAX, rs1);
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_movd_xmm_r32(e, 0, RAX);
    emit_movd_xmm_r32(e, 1, RCX);
    emit_xor_r32_r32(e, RAX, RAX);   // zero result
    emit_ucomiss(e, 0, 1);           // sets CF, ZF, PF
    // For NaN: PF=1. We need result=0 for NaN in all cases.
    // FEQ: ZF=1 && PF=0 -> sete cl; setnp al; and eax, ecx
    // FLT: CF=1 && PF=0 -> setb cl; setnp al; and eax, ecx
    // FLE: (CF=1||ZF=1) && PF=0 -> setbe cl; setnp al; and eax, ecx
    switch (opcode) {
        case OP_FEQ_S: emit_sete(e, RCX); break;
        case OP_FLT_S: emit_setb(e, RCX); break;
        case OP_FLE_S: emit_setbe(e, RCX); break;
    }
    emit_setnp(e, RAX);
    emit_and_r32_r32(e, RAX, RCX);
    emit_store_guest_reg(ctx, rd, RAX);
}

// f32 negate: FNEG_S (pure integer xor)
static void translate_fp_f32_neg(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1) {
    emit_ctx_t *e = &ctx->emit;
    flush_pending_write(ctx);
    emit_load_guest_reg(ctx, RAX, rs1);
    emit_xor_r32_imm32(e, RAX, (int32_t)0x80000000);
    emit_store_guest_reg(ctx, rd, RAX);
}

// f32 absolute value: FABS_S (pure integer and)
static void translate_fp_f32_abs(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1) {
    emit_ctx_t *e = &ctx->emit;
    flush_pending_write(ctx);
    emit_load_guest_reg(ctx, RAX, rs1);
    emit_and_r32_imm32(e, RAX, 0x7FFFFFFF);
    emit_store_guest_reg(ctx, rd, RAX);
}

// float -> signed int32 (truncating): FCVT_W_S
static void translate_fp_f32_cvt_w_s(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1) {
    emit_ctx_t *e = &ctx->emit;
    flush_pending_write(ctx);
    emit_load_guest_reg(ctx, RAX, rs1);
    emit_movd_xmm_r32(e, 0, RAX);
    emit_cvttss2si_r32_xmm(e, RAX, 0);
    emit_store_guest_reg(ctx, rd, RAX);
}

// float -> unsigned int32 (truncating): FCVT_WU_S
// Use 64-bit cvttss2si then truncate to 32-bit to handle full uint32 range
static void translate_fp_f32_cvt_wu_s(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1) {
    emit_ctx_t *e = &ctx->emit;
    flush_pending_write(ctx);
    emit_load_guest_reg(ctx, RAX, rs1);
    emit_movd_xmm_r32(e, 0, RAX);
    // Use cvttss2si with 32-bit dest — values > INT32_MAX return 0x80000000
    // which matches the hardware behavior for out-of-range unsigned conversion
    emit_cvttss2si_r32_xmm(e, RAX, 0);
    emit_store_guest_reg(ctx, rd, RAX);
}

// signed int32 -> float: FCVT_S_W
static void translate_fp_f32_cvt_s_w(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1) {
    emit_ctx_t *e = &ctx->emit;
    flush_pending_write(ctx);
    emit_load_guest_reg(ctx, RAX, rs1);
    emit_cvtsi2ss_xmm_r32(e, 0, RAX);
    emit_movd_r32_xmm(e, RAX, 0);
    emit_store_guest_reg(ctx, rd, RAX);
}

// unsigned int32 -> float: FCVT_S_WU
// Zero-extend to 64 bits (32-bit mov already does this), then use 64-bit cvtsi2ss
static void translate_fp_f32_cvt_s_wu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1) {
    emit_ctx_t *e = &ctx->emit;
    flush_pending_write(ctx);
    emit_load_guest_reg(ctx, RAX, rs1);
    // eax is already zero-extended to rax by 32-bit load
    emit_cvtsi2ss_xmm_r64(e, 0, RAX);
    emit_movd_r32_xmm(e, RAX, 0);
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_fp_r_type(translate_ctx_t *ctx, uint8_t opcode, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    emit_ctx_t *e = &ctx->emit;

    // Flush pending write before calling helper (clobbers everything)
    flush_pending_write(ctx);

    // Invalidate constant propagation for rd (and rd+1 for register-pair results).
    // The helper writes to cpu->regs directly, bypassing emit_store_guest_reg,
    // so we must manually invalidate the stale compile-time constants.
    if (rd != 0) {
        ctx->reg_constants[rd].valid = false;
        bounds_elim_invalidate(ctx, rd);
        if (rd + 1 < 32) {
            ctx->reg_constants[rd + 1].valid = false;
            bounds_elim_invalidate(ctx, rd + 1);
        }
    }

    // Flush ALL cached registers before calling helper - it reads from memory!
    // Must flush ALL, not just dirty, because helper reads rs1/rs2 from memory.
    if (ctx->reg_cache_enabled) {
        emit_ctx_t *ef = &ctx->emit;
        for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
            if (!ctx->reg_alloc[i].allocated) continue;
            uint8_t greg = ctx->reg_alloc[i].guest_reg;
            emit_mov_m32_r32(ef, RBP, GUEST_REG_OFFSET(greg), reg_alloc_hosts[i]);
            ctx->reg_alloc[i].dirty = false;
        }
    }

    // Set up System V AMD64 ABI call: RDI=cpu, ESI=opcode, EDX=rd, ECX=rs1, R8D=rs2
    emit_mov_r64_r64(e, RDI, RBP);                  // cpu state
    emit_mov_r32_imm32(e, RSI, opcode);              // opcode
    emit_mov_r32_imm32(e, RDX, rd);                  // rd
    emit_mov_r32_imm32(e, RCX, rs1);                 // rs1
    emit_mov_r32_imm32(e, R8, rs2);                  // rs2
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)dbt_fp_helper);
    emit_call_r64(e, RAX);

    // Reload reg cache from memory (helper may have modified any guest regs)
    if (ctx->reg_cache_enabled) {
        for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
            if (!ctx->reg_alloc[i].allocated) continue;
            uint8_t greg = ctx->reg_alloc[i].guest_reg;
            emit_mov_r32_m32(e, reg_alloc_hosts[i], RBP, GUEST_REG_OFFSET(greg));
        }
    }
}

void translate_assert_eq(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2) {
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_cmp_r32_r32(e, RAX, RCX);

    // If equal, continue; if not equal, exit with assert fail
    size_t je_patch = emit_offset(e) + 2;
    emit_je_rel32(e, 0);  // Jump over fail path

    // Fail path
    emit_exit(ctx, EXIT_ASSERT_FAIL, ctx->guest_pc);

    // Success path - patch jump
    size_t success_offset = emit_offset(e);
    emit_patch_rel32(e, je_patch, success_offset);
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
    memset(ctx->side_exit_pcs, 0, sizeof(ctx->side_exit_pcs));
    memset(ctx->dead_temp_skip, 0, sizeof(ctx->dead_temp_skip));
    reg_alloc_reset(ctx);
    const_prop_reset(ctx);
    bounds_elim_reset(ctx);
    if (ctx->reg_cache_enabled) {
        reg_alloc_prescan(ctx, cpu->pc);
        reg_alloc_emit_prologue(ctx);
    }

    if (DBT_TRACE) {
        fprintf(stderr, "DBT: Translating block at PC=0x%08X\n", cpu->pc);
    }

    // Debug: emit int3 at start of block for debugging
    // emit_int3(e);

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

        // Flush pending writes before back-edge target to prevent stale
        // RAX stores from re-executing with wrong values on loop iterations.
        // Also invalidate all constants since values may change across iterations.
        if (is_backedge_target(ctx, ctx->guest_pc)) {
            flush_pending_write(ctx);
            const_prop_reset(ctx);
            bounds_elim_reset(ctx);
        }

        // Record guest PC → host offset for in-block back-edge optimization
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

        // Track instruction index for dead temporary elimination
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

            // Floating-point f32 (inline SSE)
            case OP_FADD_S: case OP_FSUB_S: case OP_FMUL_S: case OP_FDIV_S:
                translate_fp_f32_arith(ctx, inst.opcode, inst.rd, inst.rs1, inst.rs2);
                break;
            case OP_FSQRT_S:
                translate_fp_f32_sqrt(ctx, inst.rd, inst.rs1);
                break;
            case OP_FEQ_S: case OP_FLT_S: case OP_FLE_S:
                translate_fp_f32_cmp(ctx, inst.opcode, inst.rd, inst.rs1, inst.rs2);
                break;
            case OP_FNEG_S:
                translate_fp_f32_neg(ctx, inst.rd, inst.rs1);
                break;
            case OP_FABS_S:
                translate_fp_f32_abs(ctx, inst.rd, inst.rs1);
                break;
            case OP_FCVT_W_S:
                translate_fp_f32_cvt_w_s(ctx, inst.rd, inst.rs1);
                break;
            case OP_FCVT_WU_S:
                translate_fp_f32_cvt_wu_s(ctx, inst.rd, inst.rs1);
                break;
            case OP_FCVT_S_W:
                translate_fp_f32_cvt_s_w(ctx, inst.rd, inst.rs1);
                break;
            case OP_FCVT_S_WU:
                translate_fp_f32_cvt_s_wu(ctx, inst.rd, inst.rs1);
                break;

            // Floating-point f64/cross-format (helper call fallback)
            case OP_FADD_D: case OP_FSUB_D: case OP_FMUL_D: case OP_FDIV_D:
            case OP_FSQRT_D: case OP_FEQ_D: case OP_FLT_D: case OP_FLE_D:
            case OP_FCVT_W_D: case OP_FCVT_WU_D: case OP_FCVT_D_W: case OP_FCVT_D_WU:
            case OP_FCVT_D_S: case OP_FCVT_S_D: case OP_FNEG_D: case OP_FABS_D:
            case OP_FCVT_L_S: case OP_FCVT_LU_S: case OP_FCVT_S_L: case OP_FCVT_S_LU:
            case OP_FCVT_L_D: case OP_FCVT_LU_D: case OP_FCVT_D_L: case OP_FCVT_D_LU:
                translate_fp_r_type(ctx, inst.opcode, inst.rd, inst.rs1, inst.rs2);
                break;

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
                continue;  // Superblock extended - skip increment (already done)

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
                // ASSERT_EQ doesn't end block on success
                break;

            case OP_NOP:
                translate_nop(ctx);
                break;

            default:
                // Unknown opcode - emit halt for now
                fprintf(stderr, "DBT: Unknown opcode 0x%02X at PC 0x%08X\n",
                        inst.opcode, ctx->guest_pc);
                emit_exit(ctx, EXIT_HALT, ctx->guest_pc);
                goto block_done;
        }

        ctx->guest_pc += 4;
        ctx->inst_count++;
    }

    // Reached max instructions - exit with block end
    emit_exit(ctx, EXIT_BLOCK_END, ctx->guest_pc);

block_done:
    if (ctx->block) {
        ctx->block->reg_cache_hits = ctx->reg_cache_hits;
        ctx->block->reg_cache_misses = ctx->reg_cache_misses;
    }
    cpu->code_buffer_used = emit_offset(e);
    return (translated_block_fn)entry;
}

// ============================================================================
// Intrinsic recognition: emit native stubs for known functions
// ============================================================================

// Emit a native memcpy/memmove stub:
//   guest r3=dest, r4=src, r5=count → host memcpy/memmove
//   returns dest in guest r1, jumps to guest r31
// Uses: rdi, rsi, rdx, rax, rcx (all caller-saved)
static bool emit_native_memcpy_stub(translate_ctx_t *ctx, translated_block_t *block,
                                     bool is_memmove) {
    emit_ctx_t *e = &ctx->emit;

    // Load guest registers into scratch registers
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));   // eax = dest (guest addr)
    emit_mov_r32_m32(e, RCX, RBP, GUEST_REG_OFFSET(4));   // ecx = src (guest addr)
    emit_mov_r32_m32(e, RDX, RBP, GUEST_REG_OFFSET(5));   // edx = count

    // Bounds/MMIO/W^X checks for src/dest (dynamic size in rdx)
    emit_mem_access_check_dynamic(ctx, RCX, RDX, EXIT_FAULT_LOAD, false);
    emit_mem_access_check_dynamic(ctx, RAX, RDX, EXIT_FAULT_STORE, true);

    // Store dest as return value in guest r1
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);

    // We need to save rbp, r14, r15 across the call since they're callee-saved
    // and the C function won't clobber them. But we also need to save our state.
    // Actually rbp/r14/r15 are callee-saved in the x86-64 ABI, so a C call will preserve them.

    // Set up host memcpy args: rdi = host_dest, rsi = host_src, rdx = count
    // host_dest = mem_base + dest = r14 + rax (zero-extended)
    // We need 64-bit add: use movzx to ensure zero-extension, then add

    // rdi = r14 (mem_base, 64-bit)
    emit_mov_r64_r64(e, RDI, R14);
    // rdi += rax (eax is already zero-extended to rax by 32-bit mov)
    // Emit: REX.W add rdi, rax  →  48 01 C7
    emit_byte(e, 0x48);  // REX.W
    emit_byte(e, 0x01);  // ADD r/m64, r64
    emit_byte(e, MODRM(MOD_DIRECT, RAX, RDI));

    // rsi = r14 + rcx
    emit_mov_r64_r64(e, RSI, R14);
    // Emit: REX.W add rsi, rcx  →  48 01 CE
    emit_byte(e, 0x48);
    emit_byte(e, 0x01);
    emit_byte(e, MODRM(MOD_DIRECT, RCX, RSI));

    // rdx already has count (zero-extended to 64-bit by the 32-bit load)

    // Save r14 and r15 (in case C function somehow clobbers, though they're callee-saved)
    // Actually they are callee-saved, so the C function will preserve them.
    // But we do need to align the stack to 16 bytes before call.
    // The trampoline does push rbp,rbx,r12,r13,r14,r15 (6 pushes = 48 bytes)
    // then call *rdx adds 8 bytes return addr. So at entry to translated code,
    // RSP is 8-byte aligned but not 16-byte aligned.
    // Before our call, we need to push something to align.
    emit_push_r64(e, RAX);  // Align stack + save (don't need value)

    // Load host function address and call
    // mov rax, memcpy/memmove address
    if (is_memmove) {
        emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)memmove);
    } else {
        emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)memcpy);
    }
    emit_call_r64(e, RAX);

    emit_pop_r64(e, RAX);  // Restore stack alignment

    // Set PC to guest r31 (link register)
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(REG_LR));
    emit_mov_m32_r32(e, RBP, CPU_PC_OFFSET, RAX);

    // Exit with EXIT_INDIRECT (returning to whatever the caller was)
    emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_INDIRECT);
    emit_ret(e);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;  // Minimal
    return !e->overflow;
}

// Emit a native memset stub:
//   guest r3=dest, r4=value(byte), r5=count → host memset
//   returns dest in guest r1, jumps to guest r31
static bool emit_native_memset_stub(translate_ctx_t *ctx, translated_block_t *block) {
    emit_ctx_t *e = &ctx->emit;

    // Load guest registers
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));   // eax = dest
    emit_mov_r32_m32(e, RCX, RBP, GUEST_REG_OFFSET(4));   // ecx = value
    emit_mov_r32_m32(e, RDX, RBP, GUEST_REG_OFFSET(5));   // edx = count

    // Bounds/MMIO/W^X checks for dest (dynamic size in rdx)
    emit_mem_access_check_dynamic(ctx, RAX, RDX, EXIT_FAULT_STORE, true);

    // Store dest as return value
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);

    // host memset args: rdi=ptr, rsi=value, rdx=count
    // rdi = r14 + rax
    emit_mov_r64_r64(e, RDI, R14);
    emit_byte(e, 0x48); emit_byte(e, 0x01); emit_byte(e, MODRM(MOD_DIRECT, RAX, RDI));

    // rsi = value (zero-extended)
    emit_mov_r32_r32(e, RSI, RCX);

    // rdx = count (already set)

    // Align stack and call
    emit_push_r64(e, RAX);
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)memset);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RAX);

    // Return to guest r31
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(REG_LR));
    emit_mov_m32_r32(e, RBP, CPU_PC_OFFSET, RAX);
    emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_INDIRECT);
    emit_ret(e);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// Emit a native strlen stub:
//   guest r3=str → host strlen
//   returns length in guest r1, jumps to guest r31
static bool emit_native_strlen_stub(translate_ctx_t *ctx, translated_block_t *block) {
    emit_ctx_t *e = &ctx->emit;

    // Load guest r3 (str ptr)
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));
    // Save guest addr in RBX for fault reporting
    emit_mov_r32_r32(e, RBX, RAX);

    // Bounds check: addr must be < mem_size
    emit_cmp_r32_imm32(e, RAX, (int32_t)ctx->cpu->mem_size);
    size_t fault_addr_patch = emit_offset(e) + 2;
    emit_jae_rel32(e, 0);

    // rdi = r14 + rax (host pointer)
    emit_mov_r64_r64(e, RDI, R14);
    emit_byte(e, 0x48); emit_byte(e, 0x01); emit_byte(e, MODRM(MOD_DIRECT, RAX, RDI));
    // Save start pointer in r12
    emit_mov_r64_r64(e, R12, RDI);

    // rdx = mem_size - addr (bounded length)
    emit_mov_r32_imm32(e, RDX, ctx->cpu->mem_size);
    emit_sub_r32_r32(e, RDX, RAX);

    // rsi = 0 (search for null byte)
    emit_xor_r32_r32(e, RSI, RSI);

    // Align stack and call memchr
    emit_push_r64(e, RAX);
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)memchr);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RCX);  // Restore stack (into any reg)

    // If not found, fault
    emit_test_r32_r32(e, RAX, RAX);
    size_t fault_null_patch = emit_offset(e) + 2;
    emit_je_rel32(e, 0);

    // len = found - start
    emit_byte(e, 0x4C); emit_byte(e, 0x29); emit_byte(e, 0xE0); // sub rax, r12

    // Return value from memchr is in rax - store length to guest r1
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);

    // Return to guest r31
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(REG_LR));
    emit_mov_m32_r32(e, RBP, CPU_PC_OFFSET, RAX);
    emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_INDIRECT);
    emit_ret(e);

    // Fault path (bounds or missing terminator)
    size_t fault_offset = emit_offset(e);
    emit_mov_r32_r32(e, RAX, RBX);
    emit_exit_with_info_reg(ctx, EXIT_FAULT_LOAD, ctx->guest_pc, RAX);

    emit_patch_rel32(e, fault_addr_patch, fault_offset);
    emit_patch_rel32(e, fault_null_patch, fault_offset);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// Emit a native memswap stub:
//   guest r3=a, r4=b, r5=count → inline qword-swap loop on host
//   no return value, jumps to guest r31
static bool emit_native_memswap_stub(translate_ctx_t *ctx, translated_block_t *block) {
    emit_ctx_t *e = &ctx->emit;

    // Load guest registers
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));   // eax = a (guest addr)
    emit_mov_r32_m32(e, RCX, RBP, GUEST_REG_OFFSET(4));   // ecx = b (guest addr)
    emit_mov_r32_m32(e, RDX, RBP, GUEST_REG_OFFSET(5));   // edx = count

    // Bounds/MMIO/W^X checks for both ranges (dynamic size in rdx)
    emit_mem_access_check_dynamic(ctx, RAX, RDX, EXIT_FAULT_STORE, true);
    emit_mem_access_check_dynamic(ctx, RCX, RDX, EXIT_FAULT_STORE, true);

    // rdi = host ptr a = r14 + rax
    emit_mov_r64_r64(e, RDI, R14);
    emit_byte(e, 0x48); emit_byte(e, 0x01); emit_byte(e, MODRM(MOD_DIRECT, RAX, RDI));

    // rsi = host ptr b = r14 + rcx
    emit_mov_r64_r64(e, RSI, R14);
    emit_byte(e, 0x48); emit_byte(e, 0x01); emit_byte(e, MODRM(MOD_DIRECT, RCX, RSI));

    // rdx = count (already set, zero-extended)

    // Qword swap loop: while (rdx >= 8) { swap 8 bytes via rax/rcx }
    //   .Lqword_loop:
    //     cmp rdx, 8
    //     jb .Lbyte_loop
    //     mov rax, [rdi]
    //     mov rcx, [rsi]
    //     mov [rdi], rcx
    //     mov [rsi], rax
    //     add rdi, 8
    //     add rsi, 8
    //     sub rdx, 8
    //     jmp .Lqword_loop
    //   .Lbyte_loop:
    //     test rdx, rdx
    //     jz .Ldone
    //     mov al, [rdi]
    //     mov cl, [rsi]
    //     mov [rdi], cl
    //     mov [rsi], al
    //     inc rdi
    //     inc rsi
    //     dec rdx
    //     jmp .Lbyte_loop
    //   .Ldone:

    size_t qword_loop = emit_offset(e);

    // cmp edx, 8
    emit_cmp_r32_imm32(e, RDX, 8);
    // jb .Lbyte_loop (rel32 — patch later)
    size_t jb_patch = emit_offset(e);
    emit_jb_rel32(e, 0);  // placeholder

    // mov rax, [rdi]  — REX.W 48 8B 07
    emit_byte(e, 0x48); emit_byte(e, 0x8B); emit_byte(e, MODRM(MOD_INDIRECT, RAX, RDI));
    // mov rcx, [rsi]  — REX.W 48 8B 0E
    emit_byte(e, 0x48); emit_byte(e, 0x8B); emit_byte(e, MODRM(MOD_INDIRECT, RCX, RSI));
    // mov [rdi], rcx  — REX.W 48 89 0F
    emit_byte(e, 0x48); emit_byte(e, 0x89); emit_byte(e, MODRM(MOD_INDIRECT, RCX, RDI));
    // mov [rsi], rax  — REX.W 48 89 06
    emit_byte(e, 0x48); emit_byte(e, 0x89); emit_byte(e, MODRM(MOD_INDIRECT, RAX, RSI));

    // add rdi, 8  — REX.W 48 83 C7 08
    emit_byte(e, 0x48); emit_byte(e, 0x83); emit_byte(e, MODRM(MOD_DIRECT, 0, RDI)); emit_byte(e, 8);
    // add rsi, 8  — REX.W 48 83 C6 08
    emit_byte(e, 0x48); emit_byte(e, 0x83); emit_byte(e, MODRM(MOD_DIRECT, 0, RSI)); emit_byte(e, 8);
    // sub edx, 8
    emit_sub_r32_imm32(e, RDX, 8);

    // jmp .Lqword_loop
    int32_t qloop_rel = (int32_t)qword_loop - (int32_t)(emit_offset(e) + 5);
    emit_jmp_rel32(e, qloop_rel);

    // .Lbyte_loop:
    size_t byte_loop = emit_offset(e);
    // Patch the jb
    emit_patch_rel32(e, jb_patch + 2, byte_loop);  // +2 to skip 0F 8x opcode bytes

    // test edx, edx
    emit_test_r32_r32(e, RDX, RDX);
    // jz .Ldone (patch later)
    size_t jz_patch = emit_offset(e);
    emit_je_rel32(e, 0);  // placeholder

    // mov al, [rdi]  — 8A 07
    emit_byte(e, 0x8A); emit_byte(e, MODRM(MOD_INDIRECT, RAX, RDI));
    // mov cl, [rsi]  — 8A 0E
    emit_byte(e, 0x8A); emit_byte(e, MODRM(MOD_INDIRECT, RCX, RSI));
    // mov [rdi], cl  — 88 0F
    emit_byte(e, 0x88); emit_byte(e, MODRM(MOD_INDIRECT, RCX, RDI));
    // mov [rsi], al  — 88 06
    emit_byte(e, 0x88); emit_byte(e, MODRM(MOD_INDIRECT, RAX, RSI));

    // inc rdi  — REX.W 48 FF C7
    emit_byte(e, 0x48); emit_byte(e, 0xFF); emit_byte(e, MODRM(MOD_DIRECT, 0, RDI));
    // inc rsi  — REX.W 48 FF C6
    emit_byte(e, 0x48); emit_byte(e, 0xFF); emit_byte(e, MODRM(MOD_DIRECT, 0, RSI));
    // dec edx  — FF CA
    emit_byte(e, 0xFF); emit_byte(e, MODRM(MOD_DIRECT, 1, RDX));

    // jmp .Lbyte_loop
    int32_t bloop_rel = (int32_t)byte_loop - (int32_t)(emit_offset(e) + 5);
    emit_jmp_rel32(e, bloop_rel);

    // .Ldone:
    size_t done = emit_offset(e);
    emit_patch_rel32(e, jz_patch + 2, done);  // +2 to skip 0F 84 opcode bytes

    // Return to guest r31
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(REG_LR));
    emit_mov_m32_r32(e, RBP, CPU_PC_OFFSET, RAX);
    emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_INDIRECT);
    emit_ret(e);

    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// ============================================================================
// Math function interception emitters
// ============================================================================

// Helper: emit the common epilogue for math stubs (set PC = LR, exit indirect)
static void emit_math_epilogue(emit_ctx_t *e) {
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(REG_LR));
    emit_mov_m32_r32(e, RBP, CPU_PC_OFFSET, RAX);
    emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_INDIRECT);
    emit_ret(e);
}

// float fn(float) — SIG_F32_F32
// Guest: r3 = float bits in, r1 = float bits out
static bool emit_native_math_f32_unary(translate_ctx_t *ctx, translated_block_t *block,
                                        void *host_fn) {
    emit_ctx_t *e = &ctx->emit;

    // Load guest r3 (float bits) into eax
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));

    // movd xmm0, eax  →  66 0F 6E C0
    emit_byte(e, 0x66); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC0);

    // Align stack (entry is 8-byte aligned, need 16-byte for call)
    emit_push_r64(e, RAX);

    // Call host function
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)host_fn);
    emit_call_r64(e, RAX);

    emit_pop_r64(e, RAX);  // restore stack alignment

    // movd eax, xmm0  →  66 0F 7E C0
    emit_byte(e, 0x66); emit_byte(e, 0x0F); emit_byte(e, 0x7E); emit_byte(e, 0xC0);

    // Store result to guest r1
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);

    emit_math_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// double fn(double) — SIG_F64_F64
// Guest: r3=low32, r4=high32 → r1=low32, r2=high32
static bool emit_native_math_f64_unary(translate_ctx_t *ctx, translated_block_t *block,
                                        void *host_fn) {
    emit_ctx_t *e = &ctx->emit;

    // Load r3 (low), r4 (high) and combine into rax
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));   // eax = low32
    emit_mov_r32_m32(e, RDX, RBP, GUEST_REG_OFFSET(4));   // edx = high32

    // shl rdx, 32  →  48 C1 E2 20
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE2); emit_byte(e, 0x20);
    // or rax, rdx  →  48 09 D0
    emit_byte(e, 0x48); emit_byte(e, 0x09); emit_byte(e, 0xD0);

    // movq xmm0, rax  →  66 48 0F 6E C0
    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC0);

    emit_push_r64(e, RAX);
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)host_fn);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RAX);

    // movq rax, xmm0  →  66 48 0F 7E C0
    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x7E); emit_byte(e, 0xC0);

    // Store low32 to r1
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);
    // Shift right 32 to get high32, store to r2
    // shr rax, 32  →  48 C1 E8 20
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE8); emit_byte(e, 0x20);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(2), RAX);

    emit_math_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// float fn(float, float) — SIG_F32_F32_F32
// Guest: r3=x, r4=y → r1=result
static bool emit_native_math_f32_binary(translate_ctx_t *ctx, translated_block_t *block,
                                         void *host_fn) {
    emit_ctx_t *e = &ctx->emit;

    // Load r3 → xmm0
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));
    // movd xmm0, eax
    emit_byte(e, 0x66); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC0);

    // Load r4 → xmm1
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(4));
    // movd xmm1, eax  →  66 0F 6E C8
    emit_byte(e, 0x66); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC8);

    emit_push_r64(e, RAX);
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)host_fn);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RAX);

    // movd eax, xmm0
    emit_byte(e, 0x66); emit_byte(e, 0x0F); emit_byte(e, 0x7E); emit_byte(e, 0xC0);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);

    emit_math_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// double fn(double, double) — SIG_F64_F64_F64
// Guest: r3:r4=x, r5:r6=y → r1:r2=result
static bool emit_native_math_f64_binary(translate_ctx_t *ctx, translated_block_t *block,
                                         void *host_fn) {
    emit_ctx_t *e = &ctx->emit;

    // Load r3:r4 → xmm0
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));   // low
    emit_mov_r32_m32(e, RDX, RBP, GUEST_REG_OFFSET(4));   // high
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE2); emit_byte(e, 0x20); // shl rdx,32
    emit_byte(e, 0x48); emit_byte(e, 0x09); emit_byte(e, 0xD0); // or rax, rdx
    // movq xmm0, rax
    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC0);

    // Load r5:r6 → xmm1
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(5));   // low
    emit_mov_r32_m32(e, RDX, RBP, GUEST_REG_OFFSET(6));   // high
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE2); emit_byte(e, 0x20); // shl rdx,32
    emit_byte(e, 0x48); emit_byte(e, 0x09); emit_byte(e, 0xD0); // or rax, rdx
    // movq xmm1, rax  →  66 48 0F 6E C8
    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC8);

    emit_push_r64(e, RAX);
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)host_fn);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RAX);

    // movq rax, xmm0
    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x7E); emit_byte(e, 0xC0);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE8); emit_byte(e, 0x20); // shr rax,32
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(2), RAX);

    emit_math_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// double fn(double, int) — SIG_F64_F64_I32 (ldexp)
// Guest: r3:r4=x, r5=int → r1:r2=result
static bool emit_native_math_f64_i32(translate_ctx_t *ctx, translated_block_t *block,
                                      void *host_fn) {
    emit_ctx_t *e = &ctx->emit;

    // Load r3:r4 → xmm0
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));
    emit_mov_r32_m32(e, RDX, RBP, GUEST_REG_OFFSET(4));
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE2); emit_byte(e, 0x20);
    emit_byte(e, 0x48); emit_byte(e, 0x09); emit_byte(e, 0xD0);
    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC0);

    // Load r5 → edi (int argument)
    emit_mov_r32_m32(e, RDI, RBP, GUEST_REG_OFFSET(5));

    emit_push_r64(e, RAX);
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)host_fn);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RAX);

    // movq rax, xmm0
    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x7E); emit_byte(e, 0xC0);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE8); emit_byte(e, 0x20);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(2), RAX);

    emit_math_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// float fn(float, int) — SIG_F32_F32_I32 (ldexpf)
// Guest: r3=float, r4=int → r1=result
static bool emit_native_math_f32_i32(translate_ctx_t *ctx, translated_block_t *block,
                                      void *host_fn) {
    emit_ctx_t *e = &ctx->emit;

    // Load r3 → xmm0
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));
    emit_byte(e, 0x66); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC0);

    // Load r4 → edi
    emit_mov_r32_m32(e, RDI, RBP, GUEST_REG_OFFSET(4));

    emit_push_r64(e, RAX);
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)host_fn);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RAX);

    // movd eax, xmm0
    emit_byte(e, 0x66); emit_byte(e, 0x0F); emit_byte(e, 0x7E); emit_byte(e, 0xC0);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);

    emit_math_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// double fn(double, double*) — SIG_F64_F64_DPTR (modf)
// Guest: r3:r4=x, r5=guest_ptr → r1:r2=result, *ptr written
static bool emit_native_math_f64_dptr(translate_ctx_t *ctx, translated_block_t *block,
                                       void *host_fn) {
    emit_ctx_t *e = &ctx->emit;

    // Load r3:r4 → xmm0
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));
    emit_mov_r32_m32(e, RDX, RBP, GUEST_REG_OFFSET(4));
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE2); emit_byte(e, 0x20);
    emit_byte(e, 0x48); emit_byte(e, 0x09); emit_byte(e, 0xD0);
    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC0);

    // Load r5 (guest pointer), bounds check (store 8 bytes), convert to host
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(5));
    emit_mem_access_check(ctx, RAX, 8, EXIT_FAULT_STORE, true, 0, 0);
    emit_mov_r64_r64(e, RDI, R14);
    // add rdi, rax  →  48 01 C7
    emit_byte(e, 0x48); emit_byte(e, 0x01); emit_byte(e, 0xC7);

    emit_push_r64(e, RAX);
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)host_fn);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RAX);

    // movq rax, xmm0
    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x7E); emit_byte(e, 0xC0);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE8); emit_byte(e, 0x20);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(2), RAX);

    emit_math_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// double fn(double, int*) — SIG_F64_F64_IPTR (frexp)
// Guest: r3:r4=x, r5=guest_ptr → r1:r2=result, *ptr written
static bool emit_native_math_f64_iptr(translate_ctx_t *ctx, translated_block_t *block,
                                       void *host_fn) {
    emit_ctx_t *e = &ctx->emit;

    // Same as f64_dptr — pointer type doesn't matter for the call
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));
    emit_mov_r32_m32(e, RDX, RBP, GUEST_REG_OFFSET(4));
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE2); emit_byte(e, 0x20);
    emit_byte(e, 0x48); emit_byte(e, 0x09); emit_byte(e, 0xD0);
    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC0);

    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(5));
    emit_mem_access_check(ctx, RAX, 4, EXIT_FAULT_STORE, true, 0, 0);
    emit_mov_r64_r64(e, RDI, R14);
    emit_byte(e, 0x48); emit_byte(e, 0x01); emit_byte(e, 0xC7);

    emit_push_r64(e, RAX);
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)host_fn);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RAX);

    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x7E); emit_byte(e, 0xC0);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE8); emit_byte(e, 0x20);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(2), RAX);

    emit_math_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// float fn(float, float*) — SIG_F32_F32_FPTR (modff)
// Guest: r3=float, r4=guest_ptr → r1=result, *ptr written
static bool emit_native_math_f32_fptr(translate_ctx_t *ctx, translated_block_t *block,
                                       void *host_fn) {
    emit_ctx_t *e = &ctx->emit;

    // Load r3 → xmm0
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));
    emit_byte(e, 0x66); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC0);

    // Load r4 (guest pointer), bounds check (store 4 bytes) → host pointer in rdi
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(4));
    emit_mem_access_check(ctx, RAX, 4, EXIT_FAULT_STORE, true, 0, 0);
    emit_mov_r64_r64(e, RDI, R14);
    emit_byte(e, 0x48); emit_byte(e, 0x01); emit_byte(e, 0xC7);

    emit_push_r64(e, RAX);
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)host_fn);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RAX);

    // movd eax, xmm0
    emit_byte(e, 0x66); emit_byte(e, 0x0F); emit_byte(e, 0x7E); emit_byte(e, 0xC0);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);

    emit_math_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// float fn(float, int*) — SIG_F32_F32_IPTR2 (frexpf)
// Guest: r3=float, r4=guest_ptr → r1=result, *ptr written
static bool emit_native_math_f32_iptr(translate_ctx_t *ctx, translated_block_t *block,
                                       void *host_fn) {
    emit_ctx_t *e = &ctx->emit;

    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));
    emit_byte(e, 0x66); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC0);

    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(4));
    emit_mem_access_check(ctx, RAX, 4, EXIT_FAULT_STORE, true, 0, 0);
    emit_mov_r64_r64(e, RDI, R14);
    emit_byte(e, 0x48); emit_byte(e, 0x01); emit_byte(e, 0xC7);

    emit_push_r64(e, RAX);
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)host_fn);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RAX);

    emit_byte(e, 0x66); emit_byte(e, 0x0F); emit_byte(e, 0x7E); emit_byte(e, 0xC0);
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);

    emit_math_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// int fn(double) — SIG_I32_F64 (isnan, isinf, isfinite)
// Guest: r3:r4=double → r1=int result
static bool emit_native_math_i32_f64(translate_ctx_t *ctx, translated_block_t *block,
                                      void *host_fn) {
    emit_ctx_t *e = &ctx->emit;

    // Load r3:r4 → xmm0
    emit_mov_r32_m32(e, RAX, RBP, GUEST_REG_OFFSET(3));
    emit_mov_r32_m32(e, RDX, RBP, GUEST_REG_OFFSET(4));
    emit_byte(e, 0x48); emit_byte(e, 0xC1); emit_byte(e, 0xE2); emit_byte(e, 0x20);
    emit_byte(e, 0x48); emit_byte(e, 0x09); emit_byte(e, 0xD0);
    emit_byte(e, 0x66); emit_byte(e, 0x48); emit_byte(e, 0x0F); emit_byte(e, 0x6E); emit_byte(e, 0xC0);

    emit_push_r64(e, RCX);  // align stack (use RCX, not RAX — return value is in EAX)
    emit_mov_r64_imm64(e, RAX, (uint64_t)(uintptr_t)host_fn);
    emit_call_r64(e, RAX);
    emit_pop_r64(e, RCX);  // restore stack alignment

    // Result is in eax (int), store to guest r1
    emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(1), RAX);

    emit_math_epilogue(e);
    block->flags |= BLOCK_FLAG_INDIRECT | BLOCK_FLAG_RETURN;
    block->guest_size = 4;
    return !e->overflow;
}

// Dispatch to the correct math emitter based on signature type
static bool emit_native_math_stub(translate_ctx_t *ctx, translated_block_t *block,
                                   void *host_fn, uint8_t sig) {
    switch (sig) {
        case SIG_F32_F32:      return emit_native_math_f32_unary(ctx, block, host_fn);
        case SIG_F64_F64:      return emit_native_math_f64_unary(ctx, block, host_fn);
        case SIG_F32_F32_F32:  return emit_native_math_f32_binary(ctx, block, host_fn);
        case SIG_F64_F64_F64:  return emit_native_math_f64_binary(ctx, block, host_fn);
        case SIG_F64_F64_I32:  return emit_native_math_f64_i32(ctx, block, host_fn);
        case SIG_F32_F32_I32:  return emit_native_math_f32_i32(ctx, block, host_fn);
        case SIG_F64_F64_DPTR: return emit_native_math_f64_dptr(ctx, block, host_fn);
        case SIG_F64_F64_IPTR: return emit_native_math_f64_iptr(ctx, block, host_fn);
        case SIG_F32_F32_FPTR: return emit_native_math_f32_fptr(ctx, block, host_fn);
        case SIG_F32_F32_IPTR2:return emit_native_math_f32_iptr(ctx, block, host_fn);
        case SIG_I32_F64:      return emit_native_math_i32_f64(ctx, block, host_fn);
        default: return false;
    }
}

// Check if guest_pc matches a known intrinsic and emit native stub
// Returns the block if handled, NULL if not an intrinsic
static translated_block_t *try_emit_intrinsic(translate_ctx_t *ctx, uint32_t guest_pc) {
    dbt_cpu_state_t *cpu = ctx->cpu;
    if (!cpu->intrinsics_enabled) return NULL;

    bool (*emitter)(translate_ctx_t *, translated_block_t *) = NULL;
    bool is_memmove = false;

    if (cpu->intrinsic_memcpy && guest_pc == cpu->intrinsic_memcpy) {
        // Will use emit_native_memcpy_stub with is_memmove=false
    } else if (cpu->intrinsic_memset && guest_pc == cpu->intrinsic_memset) {
        emitter = emit_native_memset_stub;
    } else if (cpu->intrinsic_memmove && guest_pc == cpu->intrinsic_memmove) {
        is_memmove = true;
    } else if (cpu->intrinsic_strlen && guest_pc == cpu->intrinsic_strlen) {
        emitter = emit_native_strlen_stub;
    } else if (cpu->intrinsic_memswap && guest_pc == cpu->intrinsic_memswap) {
        emitter = emit_native_memswap_stub;
    } else {
        // Check math intercept table
        void *math_fn = NULL;
        uint8_t math_sig = 0;
        for (int i = 0; i < cpu->num_intercepts; i++) {
            if (cpu->intercepts[i].guest_addr == guest_pc) {
                math_fn = cpu->intercepts[i].host_fn;
                math_sig = cpu->intercepts[i].sig;
                break;
            }
        }
        if (!math_fn) return NULL;

        // Emit math stub
        block_cache_t *cache = ctx->cache;
        if (!cache) return NULL;

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

        bool ok = emit_native_math_stub(ctx, block, math_fn, math_sig);
        if (!ok) return NULL;

        block->host_size = emit_offset(e);
        cache_commit_code(cache, block->host_size);
        cache_insert(cache, block);
        cache_chain_incoming(cache, block);
        return block;
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

    // Initialize
    emit_ctx_t *e = &ctx->emit;
    emit_init(e, code_start, cache->code_buffer_size - cache->code_buffer_used);
    memset(block, 0, sizeof(*block));
    block->guest_pc = guest_pc;
    block->host_code = code_start;

    bool ok;
    if (emitter) {
        ok = emitter(ctx, block);
    } else {
        ok = emit_native_memcpy_stub(ctx, block, is_memmove);
    }

    if (!ok) return NULL;

    block->host_size = emit_offset(e);
    cache_commit_code(cache, block->host_size);
    cache_insert(cache, block);
    cache_chain_incoming(cache, block);

    return block;
}

// ============================================================================
// Stage 2: Cached block translation
// ============================================================================

translated_block_t *translate_block_cached(translate_ctx_t *ctx, uint32_t guest_pc) {
    dbt_cpu_state_t *cpu = ctx->cpu;
    block_cache_t *cache = ctx->cache;
    emit_ctx_t *e = &ctx->emit;
    bool saved_superblock_enabled = ctx->superblock_enabled;
    bool forced_no_superblock = false;

    if (!cache) {
        fprintf(stderr, "DBT: translate_block_cached called without cache!\n");
        return NULL;
    }

    // Check for intrinsic recognition before normal translation
    translated_block_t *intrinsic_block = try_emit_intrinsic(ctx, guest_pc);
    if (intrinsic_block) {
        return intrinsic_block;
    }

    // Allocate block metadata
    translated_block_t *block = cache_alloc_block(cache, guest_pc);
    if (!block) {
        // Cache full - flush and retry
        cache_flush(cache);
        block = cache_alloc_block(cache, guest_pc);
        if (!block) {
            fprintf(stderr, "DBT: Failed to allocate block after flush!\n");
            return NULL;
        }
    }

    // Get code buffer pointer
    uint8_t *code_start = cache_get_code_ptr(cache);
    if (!code_start) {
        // Code buffer full - flush and retry
        cache_flush(cache);
        block = cache_alloc_block(cache, guest_pc);
        code_start = cache_get_code_ptr(cache);
        if (!code_start || !block) {
            fprintf(stderr, "DBT: Failed to allocate code buffer after flush!\n");
            return NULL;
        }
    }

retry_translate:
    // Initialize emitter to write into cache code buffer
    emit_init(e, code_start, cache->code_buffer_size - cache->code_buffer_used);
    e->trace_enabled = emit_trace_enabled &&
                       (emit_trace_pc == 0 || emit_trace_pc == guest_pc);
    e->trace_tag = NULL;

    // Reset block metadata before translation
    memset(block, 0, sizeof(*block));
    block->guest_pc = guest_pc;
    block->host_code = code_start;

    // Set up translation context
    ctx->guest_pc = guest_pc;
    ctx->block_start_pc = guest_pc;
    ctx->inst_count = 0;
    ctx->block = block;
    ctx->exit_idx = 0;
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
    if (ctx->reg_cache_enabled) {
        reg_alloc_prescan(ctx, guest_pc);
        reg_alloc_emit_prologue(ctx);
    }

    if (DBT_TRACE) {
        fprintf(stderr, "DBT: Translating block at PC=0x%08X (cached)\n", guest_pc);
    }

    // Translate the block (same logic as Stage 1)
    while (ctx->inst_count < MAX_BLOCK_INSTS) {
        if (ctx->guest_pc >= cpu->code_limit ||
            (cpu->code_limit - ctx->guest_pc) < 4) {
            emit_exit_with_info(ctx, EXIT_FAULT_FETCH, ctx->guest_pc, ctx->guest_pc);
            goto cached_block_done;
        }
        if (cpu->align_traps_enabled && (ctx->guest_pc & 3u) != 0) {
            emit_exit_with_info(ctx, EXIT_FAULT_FETCH, ctx->guest_pc, ctx->guest_pc);
            goto cached_block_done;
        }

        // Flush pending writes before back-edge target to prevent stale
        // RAX stores from re-executing with wrong values on loop iterations.
        // Also invalidate all constants since values may change across iterations.
        if (is_backedge_target(ctx, ctx->guest_pc)) {
            flush_pending_write(ctx);
            const_prop_reset(ctx);
            bounds_elim_reset(ctx);
        }

        // Record guest PC → host offset for in-block back-edge optimization
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

        // Track instruction index for dead temporary elimination
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

            // Floating-point f32 (inline SSE)
            case OP_FADD_S: case OP_FSUB_S: case OP_FMUL_S: case OP_FDIV_S:
                translate_fp_f32_arith(ctx, inst.opcode, inst.rd, inst.rs1, inst.rs2);
                break;
            case OP_FSQRT_S:
                translate_fp_f32_sqrt(ctx, inst.rd, inst.rs1);
                break;
            case OP_FEQ_S: case OP_FLT_S: case OP_FLE_S:
                translate_fp_f32_cmp(ctx, inst.opcode, inst.rd, inst.rs1, inst.rs2);
                break;
            case OP_FNEG_S:
                translate_fp_f32_neg(ctx, inst.rd, inst.rs1);
                break;
            case OP_FABS_S:
                translate_fp_f32_abs(ctx, inst.rd, inst.rs1);
                break;
            case OP_FCVT_W_S:
                translate_fp_f32_cvt_w_s(ctx, inst.rd, inst.rs1);
                break;
            case OP_FCVT_WU_S:
                translate_fp_f32_cvt_wu_s(ctx, inst.rd, inst.rs1);
                break;
            case OP_FCVT_S_W:
                translate_fp_f32_cvt_s_w(ctx, inst.rd, inst.rs1);
                break;
            case OP_FCVT_S_WU:
                translate_fp_f32_cvt_s_wu(ctx, inst.rd, inst.rs1);
                break;

            // Floating-point f64/cross-format (helper call fallback)
            case OP_FADD_D: case OP_FSUB_D: case OP_FMUL_D: case OP_FDIV_D:
            case OP_FSQRT_D: case OP_FEQ_D: case OP_FLT_D: case OP_FLE_D:
            case OP_FCVT_W_D: case OP_FCVT_WU_D: case OP_FCVT_D_W: case OP_FCVT_D_WU:
            case OP_FCVT_D_S: case OP_FCVT_S_D: case OP_FNEG_D: case OP_FABS_D:
            case OP_FCVT_L_S: case OP_FCVT_LU_S: case OP_FCVT_S_L: case OP_FCVT_S_LU:
            case OP_FCVT_L_D: case OP_FCVT_LU_D: case OP_FCVT_D_L: case OP_FCVT_D_LU:
                translate_fp_r_type(ctx, inst.opcode, inst.rd, inst.rs1, inst.rs2);
                break;

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
                goto cached_block_done;

            case OP_JALR:
                translate_jalr(ctx, inst.rd, inst.rs1, inst.imm);
                goto cached_block_done;

            case OP_BEQ:
                if (translate_beq(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto cached_block_done;
                continue;  // Superblock extended - skip increment (already done)

            case OP_BNE:
                if (translate_bne(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto cached_block_done;
                continue;

            case OP_BLT:
                if (translate_blt(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto cached_block_done;
                continue;

            case OP_BGE:
                if (translate_bge(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto cached_block_done;
                continue;

            case OP_BLTU:
                if (translate_bltu(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto cached_block_done;
                continue;

            case OP_BGEU:
                if (translate_bgeu(ctx, inst.rs1, inst.rs2, inst.imm))
                    goto cached_block_done;
                continue;

            // Special (block-ending)
            case OP_HALT:
                translate_halt(ctx);
                goto cached_block_done;

            case OP_DEBUG:
                translate_debug(ctx, inst.rs1);
                goto cached_block_done;

            case OP_YIELD:
                translate_yield(ctx);
                goto cached_block_done;

            case OP_ASSERT_EQ:
                translate_assert_eq(ctx, inst.rs1, inst.rs2);
                // ASSERT_EQ doesn't end block on success
                break;

            case OP_NOP:
                translate_nop(ctx);
                break;

            default:
                // Unknown opcode - emit halt for now
                fprintf(stderr, "DBT: Unknown opcode 0x%02X at PC 0x%08X\n",
                        inst.opcode, ctx->guest_pc);
                emit_exit(ctx, EXIT_HALT, ctx->guest_pc);
                goto cached_block_done;
        }

        ctx->guest_pc += 4;
        ctx->inst_count++;
    }

    // Reached max instructions - exit with block end
    emit_exit_chained(ctx, ctx->guest_pc, ctx->exit_idx++);

cached_block_done:
    // Emit deferred (out-of-line) side exit cold stubs after the hot path
    if (ctx->deferred_exit_count > 0) {
        emit_deferred_side_exits(ctx);
    }

    if (ctx->block) {
        ctx->block->reg_cache_hits = ctx->reg_cache_hits;
        ctx->block->reg_cache_misses = ctx->reg_cache_misses;
    }
    // Finalize block
    block->guest_size = (ctx->guest_pc - ctx->block_start_pc) + 4;
    block->host_size = emit_offset(e);

    if (ctx->peephole_enabled && block->host_size > 0) {
        size_t hits = peephole_optimize_x64(block->host_code, block->host_size);
        if (hits > 0) {
            cache->peephole_hits += hits;
            // Compact NOPs out of the code
            size_t new_size = nop_compact_x64(block->host_code, block->host_size, block);
            block->host_size = (uint32_t)new_size;
        }
    }

    if (ctx->avoid_backedge_extend && ctx->superblock_enabled &&
        ctx->side_exit_emitted > 0 && !forced_no_superblock) {
        bool has_backedge = false;
        for (uint32_t eidx = 0; eidx < block->exit_count; eidx++) {
            block_exit_t *ex = &block->exits[eidx];
            if (ex->branch_pc != 0 && ex->target_pc < ex->branch_pc) {
                has_backedge = true;
                break;
            }
        }
        if (has_backedge) {
            forced_no_superblock = true;
            ctx->superblock_enabled = false;
            ctx->superblock_depth = 0;
            goto retry_translate;
        }
    }

    if (ctx->side_exit_emitted > 0) {
        cache->superblock_count++;
        cache->side_exit_emitted += ctx->side_exit_emitted;
        cache->superblock_guest_bytes_total += block->guest_size;
        cache->superblock_host_bytes_total += block->host_size;
        cache->superblock_inst_total += (uint64_t)(block->guest_size / 4);
    }

    if (ctx->side_exit_emitted > 0) {
        uint32_t stored_side_exits = (ctx->side_exit_emitted > MAX_BLOCK_EXITS)
            ? MAX_BLOCK_EXITS
            : (uint32_t)ctx->side_exit_emitted;
        block->side_exit_count = (uint8_t)stored_side_exits;
        memcpy(block->side_exit_pcs,
               ctx->side_exit_pcs,
               sizeof(uint32_t) * stored_side_exits);
    } else {
        block->side_exit_count = 0;
    }

    // Commit code to cache
    cache_commit_code(cache, block->host_size);

    // Insert into cache
    cache_insert(cache, block);

    // Chain any blocks that were waiting for this target
    cache_chain_incoming(cache, block);

    // Clear current block reference
    ctx->block = NULL;
    ctx->superblock_enabled = saved_superblock_enabled;

    return block;
}
