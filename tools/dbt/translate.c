// SLOW-32 DBT: Block Translator Implementation
// Stage 4 - Superblock extension

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
        case OP_SGE: case OP_SGEU:
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
// Stage 5: Simple register cache (fixed mapping)
// ============================================================================

#define REG_CACHE_SLOTS 4

static const x64_reg_t reg_cache_hosts[REG_CACHE_SLOTS] = { R12, R13, RBX, R11 };

static inline int reg_cache_slot(uint8_t guest_reg) {
    // Simple XOR hash to separate common colliding registers:
    // r1 (0x01) & 3 = 1
    // r29 (SP, 0x1D) & 3 = 1  <- COLLISION in Slot 1
    //
    // New hash: (reg ^ (reg >> 2)) & 3
    // r1  (00001) -> 1 ^ 0 = 1 (Slot 1)
    // r29 (11101) -> 1 ^ 7 = 6 -> 2 (Slot 2) -> SP moves to Slot 2
    // r31 (11111) -> 3 ^ 7 = 4 -> 0 (Slot 0) -> LR moves to Slot 0
    return (guest_reg ^ (guest_reg >> 2)) & (REG_CACHE_SLOTS - 1);
}

static void reg_cache_reset(translate_ctx_t *ctx) {
    memset(ctx->reg_cache, 0, sizeof(ctx->reg_cache));
    ctx->reg_cache_hits = 0;
    ctx->reg_cache_misses = 0;
}

static void reg_cache_flush(translate_ctx_t *ctx) {
    if (!ctx->reg_cache_enabled) {
        return;
    }
    emit_ctx_t *e = &ctx->emit;
    for (int i = 0; i < REG_CACHE_SLOTS; i++) {
        if (!ctx->reg_cache[i].valid || !ctx->reg_cache[i].dirty) {
            continue;
        }
        emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(ctx->reg_cache[i].guest_reg),
                         reg_cache_hosts[i]);
        // NOTE: Do NOT set dirty=false here! Multiple code paths (e.g., branch
        // taken/fall-through, inline lookup probes) all call flush, but only one
        // executes at runtime. Each path must emit its own flush code.
    }
}

// Load guest register into x86-64 register
void emit_load_guest_reg(translate_ctx_t *ctx, x64_reg_t dst, uint8_t guest_reg) {
    emit_ctx_t *e = &ctx->emit;
    if (guest_reg == 0) {
        // r0 is always 0
        emit_xor_r32_r32(e, dst, dst);
        return;
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

    int slot = reg_cache_slot(guest_reg);
    x64_reg_t host_reg = reg_cache_hosts[slot];
    if (!ctx->reg_cache[slot].valid || ctx->reg_cache[slot].guest_reg != guest_reg) {
        ctx->reg_cache_misses++;
        if (ctx->reg_cache[slot].valid && ctx->reg_cache[slot].dirty) {
            emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(ctx->reg_cache[slot].guest_reg),
                             host_reg);
        }
        const char *saved_tag = e->trace_tag;
        if (e->trace_enabled) {
            e->trace_tag = "guest_load";
        }
        emit_mov_r32_m32(e, host_reg, RBP, GUEST_REG_OFFSET(guest_reg));
        e->trace_tag = saved_tag;
        ctx->reg_cache[slot].valid = true;
        ctx->reg_cache[slot].dirty = false;
        ctx->reg_cache[slot].guest_reg = guest_reg;
    } else {
        ctx->reg_cache_hits++;
    }
    if (dst != host_reg) {
        emit_mov_r32_r32(e, dst, host_reg);
    }
}

// Store x86-64 register to guest register
void emit_store_guest_reg(translate_ctx_t *ctx, uint8_t guest_reg, x64_reg_t src) {
    emit_ctx_t *e = &ctx->emit;
    if (guest_reg == 0) {
        // Writes to r0 are discarded
        return;
    }

    if (!ctx->reg_cache_enabled) {
        // Store to CPU state: mov [rbp + offset], src
        const char *saved_tag = e->trace_tag;
        if (e->trace_enabled) {
            e->trace_tag = "guest_store";
        }
        emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(guest_reg), src);
        e->trace_tag = saved_tag;
        return;
    }

    int slot = reg_cache_slot(guest_reg);
    x64_reg_t host_reg = reg_cache_hosts[slot];
    if (ctx->reg_cache[slot].valid && ctx->reg_cache[slot].guest_reg != guest_reg) {
        ctx->reg_cache_misses++;
        if (ctx->reg_cache[slot].dirty) {
            emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(ctx->reg_cache[slot].guest_reg),
                             host_reg);
        }
        ctx->reg_cache[slot].dirty = false;
    } else if (ctx->reg_cache[slot].valid && ctx->reg_cache[slot].guest_reg == guest_reg) {
        ctx->reg_cache_hits++;
    } else {
        ctx->reg_cache_misses++;
    }
    if (src != host_reg) {
        emit_mov_r32_r32(e, host_reg, src);
    }
    ctx->reg_cache[slot].valid = true;
    ctx->reg_cache[slot].dirty = true;
    ctx->reg_cache[slot].guest_reg = guest_reg;
}

void emit_store_guest_reg_imm32(translate_ctx_t *ctx, uint8_t guest_reg, uint32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    if (guest_reg == 0) {
        return;
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

    int slot = reg_cache_slot(guest_reg);
    x64_reg_t host_reg = reg_cache_hosts[slot];
    if (ctx->reg_cache[slot].valid && ctx->reg_cache[slot].guest_reg != guest_reg) {
        ctx->reg_cache_misses++;
        if (ctx->reg_cache[slot].dirty) {
            emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(ctx->reg_cache[slot].guest_reg),
                             host_reg);
        }
        ctx->reg_cache[slot].dirty = false;
    } else if (ctx->reg_cache[slot].valid && ctx->reg_cache[slot].guest_reg == guest_reg) {
        ctx->reg_cache_hits++;
    } else {
        ctx->reg_cache_misses++;
    }

    emit_mov_r32_imm32(e, host_reg, imm);
    ctx->reg_cache[slot].valid = true;
    ctx->reg_cache[slot].dirty = true;
    ctx->reg_cache[slot].guest_reg = guest_reg;
}

// Emit exit sequence
void emit_exit(translate_ctx_t *ctx, exit_reason_t reason, uint32_t next_pc) {
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

// ============================================================================
// Arithmetic translations
// ============================================================================

void translate_add(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    if (rs2 != 0) {
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_add_r32_r32(e, RAX, RCX);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_sub(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    if (rs2 != 0) {
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_sub_r32_r32(e, RAX, RCX);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_addi(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        // rd = 0 + imm = imm
        emit_store_guest_reg_imm32(ctx, rd, (uint32_t)imm);
        return;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (imm != 0) {
            emit_add_r32_imm32(e, RAX, imm);
        }
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

// ============================================================================
// Logical translations
// ============================================================================

void translate_and(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0 || rs2 == 0) {
        // Anything AND 0 = 0
        emit_store_guest_reg_imm32(ctx, rd, 0);
        return;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_and_r32_r32(e, RAX, RCX);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_or(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    if (rs2 != 0) {
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_or_r32_r32(e, RAX, RCX);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_xor(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    if (rs2 != 0) {
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_xor_r32_r32(e, RAX, RCX);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_andi(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, 0);
        return;
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        emit_and_r32_imm32(e, RAX, imm);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_ori(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, (uint32_t)imm);
        return;
    }
    emit_load_guest_reg(ctx, RAX, rs1);
    if (imm != 0) {
        emit_or_r32_imm32(e, RAX, imm);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_xori(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    if (rs1 == 0) {
        emit_store_guest_reg_imm32(ctx, rd, (uint32_t)imm);
        return;
    }
    emit_load_guest_reg(ctx, RAX, rs1);
    if (imm != 0) {
        emit_xor_r32_imm32(e, RAX, imm);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

// ============================================================================
// Shift translations
// ============================================================================

void translate_sll(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    if (rs2 != 0) {
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_shl_r32_cl(e, RAX);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_srl(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    if (rs2 != 0) {
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_shr_r32_cl(e, RAX);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_sra(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    if (rs2 != 0) {
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_sar_r32_cl(e, RAX);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_slli(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    if ((imm & 0x1F) != 0) {
        emit_shl_r32_imm8(e, RAX, imm & 0x1F);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_srli(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    if ((imm & 0x1F) != 0) {
        emit_shr_r32_imm8(e, RAX, imm & 0x1F);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_srai(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    if ((imm & 0x1F) != 0) {
        emit_sar_r32_imm8(e, RAX, imm & 0x1F);
    }
    emit_store_guest_reg(ctx, rd, RAX);
}

// ============================================================================
// Comparison translations
// ============================================================================

// Helper for compare and set
static void translate_cmp_set(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2,
                              void (*emit_setcc)(emit_ctx_t *, x64_reg_t)) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_cmp_r32_r32(e, RAX, RCX);
    emit_setcc(e, RAX);
    emit_movzx_r32_r8(e, RAX, RAX);
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_slt(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, emit_setl);
}

void translate_sltu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, emit_setb);
}

void translate_seq(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, emit_sete);
}

void translate_sne(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, emit_setne);
}

void translate_sgt(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, emit_setg);
}

void translate_sgtu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, emit_seta);
}

void translate_sle(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, emit_setle);
}

void translate_sleu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, emit_setbe);
}

void translate_sge(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, emit_setge);
}

void translate_sgeu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    translate_cmp_set(ctx, rd, rs1, rs2, emit_setae);
}

void translate_slti(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    emit_cmp_r32_imm32(e, RAX, imm);
    emit_setl(e, RAX);
    emit_movzx_r32_r8(e, RAX, RAX);
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_sltiu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    emit_cmp_r32_imm32(e, RAX, imm);
    emit_setb(e, RAX);
    emit_movzx_r32_r8(e, RAX, RAX);
    emit_store_guest_reg(ctx, rd, RAX);
}

// ============================================================================
// Multiply/Divide translations
// ============================================================================

void translate_mul(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_load_guest_reg(ctx, RAX, rs1);
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_imul_r32_r32(e, RAX, RCX);
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_mulh(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
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

    // TODO: Handle division by zero
    emit_load_guest_reg(ctx, RAX, rs1);
    emit_cdq(e);  // Sign-extend eax into edx:eax
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_idiv_r32(e, RCX);  // eax = edx:eax / ecx
    emit_store_guest_reg(ctx, rd, RAX);
}

void translate_rem(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    // TODO: Handle division by zero
    emit_load_guest_reg(ctx, RAX, rs1);
    emit_cdq(e);
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_idiv_r32(e, RCX);  // edx = edx:eax % ecx
    emit_mov_r32_r32(e, RAX, RDX);
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

// Helper to compute address into RAX
static void emit_compute_addr(translate_ctx_t *ctx, uint8_t rs1, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    if (rs1 == 0) {
        emit_mov_r32_imm32(e, RAX, (uint32_t)imm);
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        if (imm != 0) {
            emit_add_r32_imm32(e, RAX, imm);
        }
    }
}

void translate_ldw(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_compute_addr(ctx, rs1, imm);
    // Load: ecx = [r14 + rax] where r14 = mem_base
    emit_mov_r32_m32_idx(e, RCX, R14, RAX);
    emit_store_guest_reg(ctx, rd, RCX);
}

void translate_ldh(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_compute_addr(ctx, rs1, imm);
    emit_movsx_r32_m16_idx(e, RCX, R14, RAX);
    emit_store_guest_reg(ctx, rd, RCX);
}

void translate_ldb(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_compute_addr(ctx, rs1, imm);
    emit_movsx_r32_m8_idx(e, RCX, R14, RAX);
    emit_store_guest_reg(ctx, rd, RCX);
}

void translate_ldhu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_compute_addr(ctx, rs1, imm);
    emit_movzx_r32_m16_idx(e, RCX, R14, RAX);
    emit_store_guest_reg(ctx, rd, RCX);
}

void translate_ldbu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    if (rd == 0) return;
    emit_ctx_t *e = &ctx->emit;

    emit_compute_addr(ctx, rs1, imm);
    emit_movzx_r32_m8_idx(e, RCX, R14, RAX);
    emit_store_guest_reg(ctx, rd, RCX);
}

void translate_stw(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;

    emit_compute_addr(ctx, rs1, imm);
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_mov_m32_r32_idx(e, R14, RAX, RCX);
}

void translate_sth(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;

    emit_compute_addr(ctx, rs1, imm);
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_mov_m16_r16_idx(e, R14, RAX, RCX);
}

void translate_stb(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;

    emit_compute_addr(ctx, rs1, imm);
    emit_load_guest_reg(ctx, RCX, rs2);
    emit_mov_m8_r8_idx(e, R14, RAX, RCX);
}

// ============================================================================
// Control flow translations
// ============================================================================

// Helper for Stage 2 chained exits
// Emits a patchable jump that can be chained to a target block
static void emit_exit_chained(translate_ctx_t *ctx, uint32_t target_pc, int exit_idx) {
    emit_ctx_t *e = &ctx->emit;
    const char *saved_tag = e->trace_tag;

    reg_cache_flush(ctx);

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
        // Target not yet translated - jump to dispatcher stub (will be patched later)
        // First set exit_reason so dispatcher knows this is a normal branch
        if (e->trace_enabled) {
            e->trace_tag = "exit_reason";
        }
        emit_mov_m32_imm32(e, RBP, CPU_EXIT_REASON_OFFSET, EXIT_BRANCH);
        e->trace_tag = saved_tag;

        // Get new patch site (after the exit_reason stores)
        patch_site = emit_ptr(e) + 1;

        int64_t rel = (int64_t)(ctx->cache->dispatcher_stub - (patch_site + 4));
        emit_jmp_rel32(e, (int32_t)rel);

        // Record for later chaining
        if (ctx->block && exit_idx < MAX_BLOCK_EXITS) {
            cache_record_exit(ctx->cache, ctx->block, exit_idx, target_pc, patch_site);
        }
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

    // Compute hash: (target >> 2) & MASK
    emit_mov_r32_m32(e, RCX, RBP, CPU_LOOKUP_MASK_OFFSET);  // rcx = mask
    emit_mov_r32_r32(e, RDX, target_save);                   // rdx = target (32-bit)
    emit_shr_r32_imm8(e, RDX, 2);                            // rdx >>= 2
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
    emit_mov_r32_imm32(e, RAX, EXIT_INDIRECT);
    emit_mov_m32_r32(e, RBP, CPU_EXIT_REASON_OFFSET, RAX);

    // Return to dispatcher
    emit_ret(e);
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
    emit_mov_r32_imm32(e, RAX, return_pc);
    emit_mov_m32_r32_sib(e, RBP, RCX, 4, CPU_RAS_STACK_OFFSET, RAX);

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
    emit_exit_chained(ctx, target_pc, 0);
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

    // Store return address if rd != 0
    // Do this BEFORE we potentially clobber RAX with lookup code
    if (rd != 0) {
        // Save target temporarily
        emit_mov_r64_r64(e, R8, RAX);
        emit_mov_r32_imm32(e, RAX, return_pc);
        emit_store_guest_reg(ctx, rd, RAX);
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
        emit_mov_r32_imm32(e, RAX, EXIT_INDIRECT);
        emit_mov_m32_r32(e, RBP, CPU_EXIT_REASON_OFFSET, RAX);
        emit_ret(e);
    }
}

// Helper for conditional branches
// NOTE: SLOW-32 branches are PC+4 relative, not PC relative!
// Returns true if the block should end, false if superblock continues
static bool translate_branch_common(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2,
                                    int32_t imm,
                                    void (*emit_jcc)(emit_ctx_t *, int32_t),
                                    void (*emit_jcc_inv)(emit_ctx_t *, int32_t)) {
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

    // Compare (special-case zero register to emit test)
    const char *saved_tag = e->trace_tag;
    if (e->trace_enabled) {
        e->trace_tag = "branch_compare";
    }
    if (rs1 == 0 && rs2 == 0) {
        emit_xor_r32_r32(e, RAX, RAX);
    } else if (rs1 == 0) {
        emit_load_guest_reg(ctx, RAX, rs2);
        emit_test_r32_r32(e, RAX, RAX);
    } else if (rs2 == 0) {
        emit_load_guest_reg(ctx, RAX, rs1);
        emit_test_r32_r32(e, RAX, RAX);
    } else {
        emit_load_guest_reg(ctx, RAX, rs1);
        emit_load_guest_reg(ctx, RCX, rs2);
        emit_cmp_r32_r32(e, RAX, RCX);
    }
    e->trace_tag = saved_tag;

    if (can_extend) {
        // Superblock mode: emit inline side exit for taken path, continue with fall-through
        //
        // Layout:
        //     cmp rs1, rs2
        //     jcc_inv skip_side_exit  ; if condition FALSE, skip side exit
        //     <side exit code for taken_pc>
        // skip_side_exit:
        //     <continue with fall-through>
        //
        // The inverted condition skips the side exit when we want to fall through

        // Jump over side exit if condition is FALSE (i.e., don't take branch)
        size_t skip_patch = emit_offset(e) + 2;
        emit_jcc_inv(e, 0);  // Placeholder - inverted condition

        // Emit side exit for taken path (branch was taken)
        // CRITICAL: Save cache state because side exit will flush (clear dirty),
        // but we are jumping over it for the fallthrough path!
        uint8_t saved_cache[sizeof(ctx->reg_cache)];
        memcpy(saved_cache, ctx->reg_cache, sizeof(saved_cache));

        if (ctx->profile_side_exits || ctx->side_exit_info_enabled) {
            const char *saved_exit_tag = e->trace_tag;

            // Track runtime side-exit usage
            if (ctx->profile_side_exits) {
                if (e->trace_enabled) {
                    e->trace_tag = "side_exit_taken";
                }
                emit_mov_r32_m32(e, R8, RBP, CPU_SIDE_EXIT_TAKEN_OFFSET);
                emit_add_r32_imm32(e, R8, 1);
                emit_mov_m32_r32(e, RBP, CPU_SIDE_EXIT_TAKEN_OFFSET, R8);
            }

            // Record branch PC for diagnostics/profiling in dispatcher
            if (e->trace_enabled) {
                e->trace_tag = "exit_info";
            }
            emit_mov_m32_imm32(e, RBP, CPU_EXIT_INFO_OFFSET, branch_pc);
            e->trace_tag = saved_exit_tag;
        }

        if (ctx->side_exit_emitted < MAX_BLOCK_EXITS) {
            ctx->side_exit_pcs[ctx->side_exit_emitted] = branch_pc;
        }

        if (ctx->block && ctx->exit_idx < MAX_BLOCK_EXITS) {
            ctx->block->exits[ctx->exit_idx].branch_pc = branch_pc;
        }
        emit_exit_chained(ctx, taken_pc, ctx->exit_idx++);
        ctx->side_exit_emitted++;

        // Restore cache state (dirty flags) for the fallthrough path
        memcpy(ctx->reg_cache, saved_cache, sizeof(saved_cache));

        // Patch the skip jump to here
        size_t skip_target = emit_offset(e);
        emit_patch_rel32(e, skip_patch, skip_target);

        // Increment superblock depth and continue
        ctx->superblock_depth++;
        ctx->guest_pc = fall_pc;
        ctx->inst_count++;

        if (DBT_TRACE) {
            fprintf(stderr, "  [superblock] Extended past branch at 0x%08X, depth=%d\n",
                    fall_pc - 4, ctx->superblock_depth);
        }

        return false;  // Don't end block - continue translating
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

static size_t peephole_optimize_x64(uint8_t *code, size_t len) {
    size_t hits = 0;
    size_t i = 0;
    while (i + 6 < len) {
        size_t i0 = i;

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
            }
        }

        // Try Pattern A/B
        size_t a = i0;
        uint8_t rex0 = 0;
        if (code[a] >= 0x40 && code[a] <= 0x4F) {
            rex0 = code[a++];
        }
        if (a >= len) {
            i = i0 + 1;
            continue;
        }
        uint8_t op0 = code[a++];
        if (a >= len) {
            i = i0 + 1;
            continue;
        }
        uint8_t modrm0 = code[a++];
        uint8_t mod0 = modrm0 & 0xC0;
        uint8_t rm0 = modrm0 & 0x07;
        uint8_t reg0 = ((modrm0 >> 3) & 0x07) | ((rex0 & 0x4) ? 0x08 : 0x00);
        if (mod0 != 0x80 || rm0 != 0x05 || (rex0 & 0x01)) {
            i = i0 + 1;
            continue;
        }
        if (a + 4 > len) {
            i = i0 + 1;
            continue;
        }
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
        if (b >= len) {
            i = i0 + 1;
            continue;
        }
        uint8_t op1 = code[b++];
        if (b >= len) {
            i = i0 + 1;
            continue;
        }
        uint8_t modrm1 = code[b++];
        uint8_t mod1 = modrm1 & 0xC0;
        uint8_t rm1 = modrm1 & 0x07;
        uint8_t reg1 = ((modrm1 >> 3) & 0x07) | ((rex1 & 0x4) ? 0x08 : 0x00);
        if (mod1 != 0x80 || rm1 != 0x05 || (rex1 & 0x01)) {
            i = i0 + 1;
            continue;
        }
        if (b + 4 > len) {
            i = i0 + 1;
            continue;
        }
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

        i = i0 + 1;
    }
    return hits;
}

// Branch functions now return true if block ends, false if superblock continues
bool translate_beq(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_je_rel32, emit_jne_rel32);
}

bool translate_bne(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_jne_rel32, emit_je_rel32);
}

bool translate_blt(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_jl_rel32, emit_jge_rel32);
}

bool translate_bge(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_jge_rel32, emit_jl_rel32);
}

bool translate_bltu(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_jb_rel32, emit_jae_rel32);
}

bool translate_bgeu(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm) {
    return translate_branch_common(ctx, rs1, rs2, imm, emit_jae_rel32, emit_jb_rel32);
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
    memset(ctx->side_exit_pcs, 0, sizeof(ctx->side_exit_pcs));
    reg_cache_reset(ctx);

    if (DBT_TRACE) {
        fprintf(stderr, "DBT: Translating block at PC=0x%08X\n", cpu->pc);
    }

    // Debug: emit int3 at start of block for debugging
    // emit_int3(e);

    while (ctx->inst_count < MAX_BLOCK_INSTS) {
        // Fetch and decode
        uint32_t raw = *(uint32_t *)(cpu->mem_base + ctx->guest_pc);
        decoded_inst_t inst = decode_instruction(raw);

        if (DBT_TRACE) {
            fprintf(stderr, "  [%d] PC=0x%08X: raw=0x%08X op=0x%02X rd=%d rs1=%d rs2=%d imm=%d\n",
                    ctx->inst_count, ctx->guest_pc, raw, inst.opcode,
                    inst.rd, inst.rs1, inst.rs2, inst.imm);
        }

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
            case OP_DIV:  translate_div(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_REM:  translate_rem(ctx, inst.rd, inst.rs1, inst.rs2); break;

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
    memset(ctx->side_exit_pcs, 0, sizeof(ctx->side_exit_pcs));
    reg_cache_reset(ctx);

    if (DBT_TRACE) {
        fprintf(stderr, "DBT: Translating block at PC=0x%08X (cached)\n", guest_pc);
    }

    // Translate the block (same logic as Stage 1)
    while (ctx->inst_count < MAX_BLOCK_INSTS) {
        // Fetch and decode
        uint32_t raw = *(uint32_t *)(cpu->mem_base + ctx->guest_pc);
        decoded_inst_t inst = decode_instruction(raw);

        if (DBT_TRACE) {
            fprintf(stderr, "  [%d] PC=0x%08X: raw=0x%08X op=0x%02X rd=%d rs1=%d rs2=%d imm=%d\n",
                    ctx->inst_count, ctx->guest_pc, raw, inst.opcode,
                    inst.rd, inst.rs1, inst.rs2, inst.imm);
        }

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
            case OP_DIV:  translate_div(ctx, inst.rd, inst.rs1, inst.rs2); break;
            case OP_REM:  translate_rem(ctx, inst.rd, inst.rs1, inst.rs2); break;

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
    emit_exit_chained(ctx, ctx->guest_pc, 0);

cached_block_done:
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
