/*
 * Slow32 Tiny Code Generator backend
 *
 * Minimal scaffolding to fetch, decode, and translate a subset of the ISA.
 * Each translation block currently exits after handling sequential flow;
 * conditional branches and halts emit explicit exits as well. Additional
 * opcodes will be added incrementally.
 */

#include "qemu/osdep.h"

#include <inttypes.h>

#include "cpu.h"
#include "accel/tcg/cpu-ldst.h"
#include "exec/memop.h"
#include "exec/helper-proto.h"
#include "exec/helper-gen.h"
#include "exec/translator.h"
#include "exec/translation-block.h"
#include "exec/plugin-gen.h"
#include "tcg/tcg.h"
#include "tcg/tcg-op.h"
#include "qemu/log.h"
#include "qemu/qemu-print.h"
#define HELPER_H "helper.h"
#include "exec/helper-info.c.inc"
#undef HELPER_H

#define DISAS_NEXT   0
#define DISAS_BRANCH 1
#define DISAS_EXIT   2

#define SLOW32_TB_MAX_INSNS 128

typedef struct Slow32PendingCmp {
    bool valid;
    bool rhs_is_imm;
    uint8_t rd;
    uint8_t lhs;
    uint8_t rhs;
    int32_t imm;
    TCGCond cond;
} Slow32PendingCmp;

typedef struct DisasContext {
    DisasContextBase base;
    CPUSlow32State *env;
    target_ulong pc;
    target_ulong next_pc;
    int is_jmp;
    int insn_count;
    Slow32PendingCmp pending_cmp;
} DisasContext;

enum {
    OP_ADD    = 0x00,
    OP_SUB    = 0x01,
    OP_XOR    = 0x02,
    OP_OR     = 0x03,
    OP_AND    = 0x04,
    OP_SLL    = 0x05,
    OP_SRL    = 0x06,
    OP_SRA    = 0x07,
    OP_SLT    = 0x08,
    OP_SLTU   = 0x09,
    OP_MUL    = 0x0A,
    OP_MULH   = 0x0B,
    OP_DIV    = 0x0C,
    OP_REM    = 0x0D,
    OP_SEQ    = 0x0E,
    OP_SNE    = 0x0F,

    OP_ADDI   = 0x10,
    OP_ORI    = 0x11,
    OP_ANDI   = 0x12,
    OP_SLLI   = 0x13,
    OP_SRLI   = 0x14,
    OP_SRAI   = 0x15,
    OP_SLTI   = 0x16,
    OP_SLTIU  = 0x17,
    OP_SGT    = 0x18,
    OP_SGTU   = 0x19,
    OP_SLE    = 0x1A,
    OP_SLEU   = 0x1B,
    OP_SGE    = 0x1C,
    OP_SGEU   = 0x1D,
    OP_XORI   = 0x1E,

    OP_LUI    = 0x20,

    OP_LDB    = 0x30,
    OP_LDH    = 0x31,
    OP_LDW    = 0x32,
    OP_LDBU   = 0x33,
    OP_LDHU   = 0x34,

    OP_STB    = 0x38,
    OP_STH    = 0x39,
    OP_STW    = 0x3A,

    OP_ASSERT_EQ = 0x3F,

    OP_JAL    = 0x40,
    OP_JALR   = 0x41,

    OP_BEQ    = 0x48,
    OP_BNE    = 0x49,
    OP_BLT    = 0x4A,
    OP_BGE    = 0x4B,
    OP_BLTU   = 0x4C,
    OP_BGEU   = 0x4D,

    OP_NOP    = 0x50,
    OP_YIELD  = 0x51,
    OP_DEBUG  = 0x52,

    // f32 instructions
    OP_FADD_S    = 0x53,
    OP_FSUB_S    = 0x54,
    OP_FMUL_S    = 0x55,
    OP_FDIV_S    = 0x56,
    OP_FSQRT_S   = 0x57,
    OP_FEQ_S     = 0x58,
    OP_FLT_S     = 0x59,
    OP_FLE_S     = 0x5A,
    OP_FCVT_W_S  = 0x5B,
    OP_FCVT_WU_S = 0x5C,
    OP_FCVT_S_W  = 0x5D,
    OP_FCVT_S_WU = 0x5E,
    OP_FNEG_S    = 0x5F,
    OP_FABS_S    = 0x60,

    // f64 instructions
    OP_FADD_D    = 0x61,
    OP_FSUB_D    = 0x62,
    OP_FMUL_D    = 0x63,
    OP_FDIV_D    = 0x64,
    OP_FSQRT_D   = 0x65,
    OP_FEQ_D     = 0x66,
    OP_FLT_D     = 0x67,
    OP_FLE_D     = 0x68,
    OP_FCVT_W_D  = 0x69,
    OP_FCVT_WU_D = 0x6A,
    OP_FCVT_D_W  = 0x6B,
    OP_FCVT_D_WU = 0x6C,
    OP_FCVT_D_S  = 0x6D,
    OP_FCVT_S_D  = 0x6E,
    OP_FNEG_D    = 0x6F,
    OP_FABS_D    = 0x70,

    // float <-> int64 conversions
    OP_FCVT_L_S  = 0x71,
    OP_FCVT_LU_S = 0x72,
    OP_FCVT_S_L  = 0x73,
    OP_FCVT_S_LU = 0x74,
    OP_FCVT_L_D  = 0x75,
    OP_FCVT_LU_D = 0x76,
    OP_FCVT_D_L  = 0x77,
    OP_FCVT_D_LU = 0x78,

    OP_HALT   = 0x7F,
};

static TCGv_i32 cpu_regs[SLOW32_NUM_GPRS];
static TCGv_i32 cpu_pc;
static TCGv_i32 cpu_next_pc;
static TCGv_i32 cpu_halted;

static inline TCGv_i32 load_gpr(int reg)
{
    return reg == SLOW32_REG_ZERO ? tcg_constant_i32(0) : cpu_regs[reg];
}

static inline void store_gpr(int reg, TCGv_i32 value)
{
    if (reg == SLOW32_REG_ZERO) {
        return;
    }
    tcg_gen_mov_i32(cpu_regs[reg], value);
}

static inline void store_gpr_imm(int reg, target_ulong value)
{
    if (reg == SLOW32_REG_ZERO) {
        return;
    }
    tcg_gen_movi_i32(cpu_regs[reg], value);
}

static inline void store_shift(int rd, int rs1, TCGv_i32 shift,
                               void (*op)(TCGv_i32, TCGv_i32, TCGv_i32))
{
    TCGv_i32 t = tcg_temp_new_i32();
    op(t, load_gpr(rs1), shift);
    store_gpr(rd, t);
}

static inline void store_cond(int rd, TCGCond cond, TCGv_i32 lhs, TCGv_i32 rhs)
{
    TCGv_i32 t = tcg_temp_new_i32();
    tcg_gen_setcond_i32(cond, t, lhs, rhs);
    store_gpr(rd, t);
}

static inline void store_condi(int rd, TCGCond cond, TCGv_i32 lhs, int32_t imm)
{
    TCGv_i32 t = tcg_temp_new_i32();
    tcg_gen_setcondi_i32(cond, t, lhs, imm);
    store_gpr(rd, t);
}

static inline void slow32_emit_tb_enter(DisasContext *ctx)
{
}

static inline void slow32_count_insns(DisasContext *ctx)
{
}

static void slow32_clear_pending_cmp(DisasContext *ctx)
{
    ctx->pending_cmp.valid = false;
}

static void slow32_flush_pending_cmp(DisasContext *ctx)
{
    Slow32PendingCmp *cmp = &ctx->pending_cmp;

    if (!cmp->valid) {
        return;
    }

    if (cmp->rd != SLOW32_REG_ZERO) {
        TCGv_i32 lhs = load_gpr(cmp->lhs);
        if (cmp->rhs_is_imm) {
            store_condi(cmp->rd, cmp->cond, lhs, cmp->imm);
        } else {
            store_cond(cmp->rd, cmp->cond, lhs, load_gpr(cmp->rhs));
        }
    }

    slow32_clear_pending_cmp(ctx);
}

static void slow32_queue_cmp_reg(DisasContext *ctx, int rd, TCGCond cond,
                                 int rs1, int rs2)
{
    slow32_flush_pending_cmp(ctx);

    if (rd == SLOW32_REG_ZERO) {
        return;
    }

    ctx->pending_cmp = (Slow32PendingCmp) {
        .valid = true,
        .rhs_is_imm = false,
        .rd = rd,
        .lhs = rs1,
        .rhs = rs2,
        .cond = cond,
    };
}

static void slow32_queue_cmp_imm(DisasContext *ctx, int rd, TCGCond cond,
                                 int rs1, int32_t imm)
{
    slow32_flush_pending_cmp(ctx);

    if (rd == SLOW32_REG_ZERO) {
        return;
    }

    ctx->pending_cmp = (Slow32PendingCmp) {
        .valid = true,
        .rhs_is_imm = true,
        .rd = rd,
        .lhs = rs1,
        .imm = imm,
        .cond = cond,
    };
}

static inline TCGv_i32 gen_addr(int rs1, int32_t imm)
{
    TCGv_i32 addr = tcg_temp_new_i32();
    if (imm == 0) {
        tcg_gen_mov_i32(addr, load_gpr(rs1));
    } else {
        tcg_gen_addi_i32(addr, load_gpr(rs1), imm);
    }
    return addr;
}

static inline void gen_load_i32(int rd, int rs1, int32_t imm, MemOp mop)
{
    TCGv_i32 addr = gen_addr(rs1, imm);
    TCGv_i32 val = tcg_temp_new_i32();

    tcg_gen_qemu_ld_i32(val, addr, 0, mop);
    store_gpr(rd, val);
}

static inline void gen_store_i32(int rs1, int rs2, int32_t imm, MemOp mop)
{
    TCGv_i32 addr = gen_addr(rs1, imm);

    tcg_gen_qemu_st_i32(load_gpr(rs2), addr, 0, mop);
}

static inline uint8_t decode_rd(uint32_t raw)
{
    return (raw >> 7) & 0x1F;
}

static inline uint8_t decode_rs1(uint32_t raw)
{
    return (raw >> 15) & 0x1F;
}

static inline uint8_t decode_rs2(uint32_t raw)
{
    return (raw >> 20) & 0x1F;
}

static inline int32_t sext(uint32_t value, unsigned bits)
{
    uint32_t mask = 1u << (bits - 1);
    return (value ^ mask) - mask;
}

static inline int32_t decode_i_imm(uint32_t raw)
{
    return (int32_t)raw >> 20;
}

static inline uint32_t decode_i_uimm(uint32_t raw)
{
    return (raw >> 20) & 0xFFF;
}

static inline int32_t decode_s_imm(uint32_t raw)
{
    uint32_t lo = (raw >> 7) & 0x1F;
    uint32_t hi = raw >> 25;
    return sext((hi << 5) | lo, 12);
}

static inline int32_t decode_b_imm(uint32_t raw)
{
    uint32_t bit11 = (raw >> 7) & 0x1;
    uint32_t bits4_1 = (raw >> 8) & 0xF;
    uint32_t bits10_5 = (raw >> 25) & 0x3F;
    uint32_t bit12 = raw >> 31;
    uint32_t value = (bit12 << 12) | (bit11 << 11) |
                     (bits10_5 << 5) | (bits4_1 << 1);
    return sext(value, 13);
}

static inline int32_t decode_j_imm(uint32_t raw)
{
    uint32_t bit20 = raw >> 31;
    uint32_t bits10_1 = (raw >> 21) & 0x3FF;
    uint32_t bit11 = (raw >> 20) & 0x1;
    uint32_t bits19_12 = (raw >> 12) & 0xFF;
    uint32_t value = (bit20 << 20) | (bits19_12 << 12) |
                     (bit11 << 11) | (bits10_1 << 1);
    return sext(value, 21);
}

static inline void commit_pc_const(target_ulong value)
{
    tcg_gen_movi_i32(cpu_pc, value);
    tcg_gen_movi_i32(cpu_next_pc, value);
}

static inline void commit_pc_tcg(TCGv_i32 value)
{
    tcg_gen_mov_i32(cpu_pc, value);
    tcg_gen_mov_i32(cpu_next_pc, value);
}

static inline void lookup_and_goto_ptr(DisasContext *ctx)
{
    slow32_emit_tb_enter(ctx);
    if (tb_cflags(ctx->base.tb) & CF_NO_GOTO_PTR) {
        tcg_gen_exit_tb(NULL, 0);
    } else {
        tcg_gen_lookup_and_goto_ptr();
    }
}

static inline void gen_goto_tb(DisasContext *ctx, int tb_num,
                               target_ulong dest)
{
    slow32_emit_tb_enter(ctx);
    commit_pc_const(dest);

    if (translator_use_goto_tb(&ctx->base, dest)) {
        tcg_gen_goto_tb(tb_num);
        tcg_gen_exit_tb(ctx->base.tb, tb_num);
    } else {
        lookup_and_goto_ptr(ctx);
    }
}

static inline void slow32_exit_tb(DisasContext *ctx)
{
    slow32_emit_tb_enter(ctx);
    tcg_gen_exit_tb(NULL, 0);
}

static void gen_cond_branch(DisasContext *ctx, TCGCond cond,
                            TCGv_i32 lhs, TCGv_i32 rhs, target_ulong target)
{
    TCGLabel *label_taken = gen_new_label();

    tcg_gen_brcond_i32(cond, lhs, rhs, label_taken);

    /* Not taken path */
    slow32_count_insns(ctx);
    gen_goto_tb(ctx, 1, ctx->next_pc);

    /* Taken path */
    gen_set_label(label_taken);
    slow32_count_insns(ctx);
    gen_goto_tb(ctx, 0, target);
    ctx->is_jmp = DISAS_BRANCH;
}

static void slow32_emit_cmp_and_branch(DisasContext *ctx, TCGCond cond,
                                       target_ulong target)
{
    Slow32PendingCmp *cmp = &ctx->pending_cmp;
    TCGv_i32 lhs = load_gpr(cmp->lhs);
    TCGv_i32 rhs_val;

    if (cmp->rhs_is_imm) {
        rhs_val = tcg_constant_i32(cmp->imm);
    } else {
        rhs_val = load_gpr(cmp->rhs);
    }

    if (cmp->rd != SLOW32_REG_ZERO) {
        if (cmp->rhs_is_imm) {
            store_condi(cmp->rd, cmp->cond, lhs, cmp->imm);
        } else {
            store_cond(cmp->rd, cmp->cond, lhs, rhs_val);
        }
    }

    gen_cond_branch(ctx, cond, lhs, rhs_val, target);
}

static bool slow32_is_cmp_branch_pair(DisasContext *ctx, uint32_t raw)
{
    Slow32PendingCmp *cmp = &ctx->pending_cmp;

    if (!cmp->valid) {
        return false;
    }

    uint8_t opcode = raw & 0x7F;
    if (opcode != OP_BEQ && opcode != OP_BNE) {
        return false;
    }

    uint8_t rs1 = decode_rs1(raw);
    uint8_t rs2 = decode_rs2(raw);

    return (rs1 == cmp->rd && rs2 == SLOW32_REG_ZERO) ||
           (rs2 == cmp->rd && rs1 == SLOW32_REG_ZERO);
}

static void slow32_do_collapse_cmp_branch(DisasContext *ctx, uint32_t raw)
{
    Slow32PendingCmp *cmp = &ctx->pending_cmp;
    uint8_t opcode = raw & 0x7F;
    target_ulong target = ctx->next_pc + decode_b_imm(raw);
    TCGCond cond = cmp->cond;

    if (opcode == OP_BEQ) {
        cond = tcg_invert_cond(cond);
    }

    slow32_emit_cmp_and_branch(ctx, cond, target);
    slow32_clear_pending_cmp(ctx);
}

static void gen_set_halted(int value)
{
    tcg_gen_movi_i32(cpu_halted, value);
}

/*
 * Try to emit a native intrinsic for the current PC.  If the PC matches
 * a known intrinsic address, emit a single-instruction TB that calls the
 * corresponding helper and returns via r31 (link register).  Returns true
 * if the intrinsic was handled.
 */
static bool try_emit_intrinsic(DisasContext *ctx, CPUSlow32State *env)
{
    typedef void (*gen_helper_fn)(TCGv_env);
    struct {
        uint32_t addr;
        gen_helper_fn gen;
    } intrinsics[] = {
        { env->intrinsic_memcpy,  gen_helper_slow32_native_memcpy },
        { env->intrinsic_memset,  gen_helper_slow32_native_memset },
        { env->intrinsic_memmove, gen_helper_slow32_native_memmove },
        { env->intrinsic_strlen,  gen_helper_slow32_native_strlen },
        { env->intrinsic_memswap, gen_helper_slow32_native_memswap },
    };

    for (int i = 0; i < ARRAY_SIZE(intrinsics); i++) {
        if (intrinsics[i].addr && ctx->pc == intrinsics[i].addr) {
            intrinsics[i].gen(tcg_env);
            commit_pc_tcg(load_gpr(31));
            lookup_and_goto_ptr(ctx);
            ctx->is_jmp = DISAS_BRANCH;
            return true;
        }
    }

    return false;
}

static bool translate_one(DisasContext *ctx, uint32_t raw)
{
    uint8_t opcode = raw & 0x7F;
    uint8_t rd = decode_rd(raw);
    uint8_t rs1 = decode_rs1(raw);
    uint8_t rs2 = decode_rs2(raw);

    switch (opcode) {
    case OP_NOP:
        break;

    case OP_ADD: {
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_add_i32(t, load_gpr(rs1), load_gpr(rs2));
        store_gpr(rd, t);
        break;
    }
    case OP_SUB: {
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_sub_i32(t, load_gpr(rs1), load_gpr(rs2));
        store_gpr(rd, t);
        break;
    }
    case OP_AND: {
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_and_i32(t, load_gpr(rs1), load_gpr(rs2));
        store_gpr(rd, t);
        break;
    }
    case OP_SLL: {
        TCGv_i32 sh = tcg_temp_new_i32();
        tcg_gen_andi_i32(sh, load_gpr(rs2), 31);
        store_shift(rd, rs1, sh, tcg_gen_shl_i32);
        break;
    }
    case OP_SRL: {
        TCGv_i32 sh = tcg_temp_new_i32();
        tcg_gen_andi_i32(sh, load_gpr(rs2), 31);
        store_shift(rd, rs1, sh, tcg_gen_shr_i32);
        break;
    }
    case OP_SRA: {
        TCGv_i32 sh = tcg_temp_new_i32();
        tcg_gen_andi_i32(sh, load_gpr(rs2), 31);
        store_shift(rd, rs1, sh, tcg_gen_sar_i32);
        break;
    }
    case OP_OR: {
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_or_i32(t, load_gpr(rs1), load_gpr(rs2));
        store_gpr(rd, t);
        break;
    }
    case OP_XOR: {
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_xor_i32(t, load_gpr(rs1), load_gpr(rs2));
        store_gpr(rd, t);
        break;
    }
    case OP_SEQ:
        slow32_queue_cmp_reg(ctx, rd, TCG_COND_EQ, rs1, rs2);
        break;
    case OP_SNE:
        slow32_queue_cmp_reg(ctx, rd, TCG_COND_NE, rs1, rs2);
        break;
    case OP_SLT:
        slow32_queue_cmp_reg(ctx, rd, TCG_COND_LT, rs1, rs2);
        break;
    case OP_SLTU:
        slow32_queue_cmp_reg(ctx, rd, TCG_COND_LTU, rs1, rs2);
        break;
    case OP_SGT:
        slow32_queue_cmp_reg(ctx, rd, TCG_COND_GT, rs1, rs2);
        break;
    case OP_SGTU:
        slow32_queue_cmp_reg(ctx, rd, TCG_COND_GTU, rs1, rs2);
        break;
    case OP_SLE:
        slow32_queue_cmp_reg(ctx, rd, TCG_COND_LE, rs1, rs2);
        break;
    case OP_SLEU:
        slow32_queue_cmp_reg(ctx, rd, TCG_COND_LEU, rs1, rs2);
        break;
    case OP_SGE:
        slow32_queue_cmp_reg(ctx, rd, TCG_COND_GE, rs1, rs2);
        break;
    case OP_SGEU:
        slow32_queue_cmp_reg(ctx, rd, TCG_COND_GEU, rs1, rs2);
        break;

    case OP_ADDI: {
        int32_t imm = decode_i_imm(raw);
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_addi_i32(t, load_gpr(rs1), imm);
        store_gpr(rd, t);
        break;
    }
    case OP_ORI: {
        uint32_t imm = decode_i_uimm(raw);
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_ori_i32(t, load_gpr(rs1), imm);
        store_gpr(rd, t);
        break;
    }
    case OP_ANDI: {
        uint32_t imm = decode_i_uimm(raw);
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_andi_i32(t, load_gpr(rs1), imm);
        store_gpr(rd, t);
        break;
    }
    case OP_XORI: {
        uint32_t imm = decode_i_uimm(raw);
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_xori_i32(t, load_gpr(rs1), imm);
        store_gpr(rd, t);
        break;
    }
    case OP_SLLI: {
        int32_t imm = decode_i_imm(raw) & 0x1F;
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_shli_i32(t, load_gpr(rs1), imm);
        store_gpr(rd, t);
        break;
    }
    case OP_SRLI: {
        int32_t imm = decode_i_imm(raw) & 0x1F;
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_shri_i32(t, load_gpr(rs1), imm);
        store_gpr(rd, t);
        break;
    }
    case OP_SRAI: {
        int32_t imm = decode_i_imm(raw) & 0x1F;
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_sari_i32(t, load_gpr(rs1), imm);
        store_gpr(rd, t);
        break;
    }
    case OP_SLTI:
        slow32_queue_cmp_imm(ctx, rd, TCG_COND_LT, rs1, decode_i_imm(raw));
        break;
    case OP_SLTIU:
        slow32_queue_cmp_imm(ctx, rd, TCG_COND_LTU, rs1, decode_i_imm(raw));
        break;
    case OP_LUI:
        store_gpr_imm(rd, raw & 0xFFFFF000);
        break;

    case OP_LDB:
        gen_load_i32(rd, rs1, decode_i_imm(raw), MO_SB);
        break;
    case OP_LDBU:
        gen_load_i32(rd, rs1, decode_i_imm(raw), MO_UB);
        break;
    case OP_LDH:
        gen_load_i32(rd, rs1, decode_i_imm(raw), MO_TESW);
        break;
    case OP_LDHU:
        gen_load_i32(rd, rs1, decode_i_imm(raw), MO_TEUW);
        break;
    case OP_LDW:
        gen_load_i32(rd, rs1, decode_i_imm(raw), MO_TEUL);
        break;

    case OP_STB:
        gen_store_i32(rs1, rs2, decode_s_imm(raw), MO_8);
        break;
    case OP_STH:
        gen_store_i32(rs1, rs2, decode_s_imm(raw), MO_TEUW);
        break;
    case OP_STW:
        gen_store_i32(rs1, rs2, decode_s_imm(raw), MO_TEUL);
        break;

    case OP_MUL: {
        TCGv_i32 t = tcg_temp_new_i32();
        tcg_gen_mul_i32(t, load_gpr(rs1), load_gpr(rs2));
        store_gpr(rd, t);
        break;
    }
    case OP_MULH: {
        TCGv_i32 lo = tcg_temp_new_i32();
        TCGv_i32 hi = tcg_temp_new_i32();
        tcg_gen_muls2_i32(lo, hi, load_gpr(rs1), load_gpr(rs2));
        store_gpr(rd, hi);
        break;
    }
    case OP_DIV: {
        TCGv_i32 denom = load_gpr(rs2);
        TCGv_i32 numer = load_gpr(rs1);
        TCGv_i32 result = tcg_temp_new_i32();
        TCGLabel *div_by_zero = gen_new_label();
        TCGLabel *done = gen_new_label();

        tcg_gen_brcondi_i32(TCG_COND_EQ, denom, 0, div_by_zero);
        tcg_gen_div_i32(result, numer, denom);
        tcg_gen_br(done);

        gen_set_label(div_by_zero);
        tcg_gen_movi_i32(result, -1);

        gen_set_label(done);
        store_gpr(rd, result);
        break;
    }
    case OP_REM: {
        TCGv_i32 denom = load_gpr(rs2);
        TCGv_i32 numer = load_gpr(rs1);
        TCGv_i32 result = tcg_temp_new_i32();
        TCGLabel *mod_by_zero = gen_new_label();
        TCGLabel *done = gen_new_label();

        tcg_gen_brcondi_i32(TCG_COND_EQ, denom, 0, mod_by_zero);
        tcg_gen_rem_i32(result, numer, denom);
        tcg_gen_br(done);

        gen_set_label(mod_by_zero);
        tcg_gen_mov_i32(result, numer);

        gen_set_label(done);
        store_gpr(rd, result);
        break;
    }

    case OP_BEQ: {
        int32_t imm = decode_b_imm(raw);
        target_ulong target = ctx->next_pc + imm;
        gen_cond_branch(ctx, TCG_COND_EQ,
                        load_gpr(rs1), load_gpr(rs2), target);
        return false;
    }
    case OP_BNE: {
        int32_t imm = decode_b_imm(raw);
        target_ulong target = ctx->next_pc + imm;
        gen_cond_branch(ctx, TCG_COND_NE,
                        load_gpr(rs1), load_gpr(rs2), target);
        return false;
    }
    case OP_BLT: {
        int32_t imm = decode_b_imm(raw);
        target_ulong target = ctx->next_pc + imm;
        gen_cond_branch(ctx, TCG_COND_LT,
                        load_gpr(rs1), load_gpr(rs2), target);
        return false;
    }
    case OP_BGE: {
        int32_t imm = decode_b_imm(raw);
        target_ulong target = ctx->next_pc + imm;
        gen_cond_branch(ctx, TCG_COND_GE,
                        load_gpr(rs1), load_gpr(rs2), target);
        return false;
    }
    case OP_BLTU: {
        int32_t imm = decode_b_imm(raw);
        target_ulong target = ctx->next_pc + imm;
        gen_cond_branch(ctx, TCG_COND_LTU,
                        load_gpr(rs1), load_gpr(rs2), target);
        return false;
    }
    case OP_BGEU: {
        int32_t imm = decode_b_imm(raw);
        target_ulong target = ctx->next_pc + imm;
        gen_cond_branch(ctx, TCG_COND_GEU,
                        load_gpr(rs1), load_gpr(rs2), target);
        return false;
    }

    case OP_JAL: {
        int32_t imm = decode_j_imm(raw);
        target_ulong target = ctx->pc + imm;
        store_gpr_imm(rd, ctx->pc + 4);
        slow32_count_insns(ctx);
        gen_goto_tb(ctx, 0, target);
        ctx->is_jmp = DISAS_BRANCH;
        return false;
    }
    case OP_JALR: {
        int32_t imm = decode_i_imm(raw);
        TCGv_i32 target = tcg_temp_new_i32();
        tcg_gen_addi_i32(target, load_gpr(rs1), imm);
        tcg_gen_andi_i32(target, target, ~1);
        store_gpr_imm(rd, ctx->pc + 4);
        slow32_count_insns(ctx);
        commit_pc_tcg(target);
        lookup_and_goto_ptr(ctx);
        ctx->is_jmp = DISAS_BRANCH;
        return false;
    }

    case OP_HALT:
        gen_set_halted(1);
        commit_pc_const(ctx->next_pc);
        slow32_count_insns(ctx);
        gen_helper_slow32_halt(tcg_env);
        slow32_exit_tb(ctx);
        ctx->is_jmp = DISAS_EXIT;
        return false;

    case OP_ASSERT_EQ: {
        /* For now, treat as HALT on mismatch and fall-through otherwise. */
        TCGLabel *ok = gen_new_label();
        tcg_gen_brcond_i32(TCG_COND_EQ, load_gpr(rs1), load_gpr(rs2), ok);
        gen_set_halted(1);
        commit_pc_const(ctx->pc);
        slow32_count_insns(ctx);
        gen_helper_slow32_halt(tcg_env);
        slow32_exit_tb(ctx);
        gen_set_label(ok);
        break;
    }
    case OP_YIELD:
        gen_helper_slow32_yield(tcg_env);
        break;
    case OP_DEBUG:
        gen_helper_slow32_debug(tcg_env, load_gpr(rs1));
        break;

    /* Floating-point instructions -- dispatch to single helper */
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
        gen_helper_slow32_fp_op(tcg_env, tcg_constant_i32(raw));
        break;

    default:
        qemu_log_mask(LOG_GUEST_ERROR,
                      "slow32: unknown opcode 0x%02x at PC=0x%08" PRIx64 "\n",
                      opcode, (uint64_t)ctx->pc);
        gen_set_halted(1);
        commit_pc_const(ctx->pc);
        slow32_count_insns(ctx);
        slow32_exit_tb(ctx);
        ctx->is_jmp = DISAS_EXIT;
        return false;
    }

    return true;
}

void slow32_translate_code(CPUState *cs, TranslationBlock *tb, int *max_insns,
                           vaddr pc, void *host_pc)
{
    CPUSlow32State *env = cpu_env(cs);
    /* Aim for bigger TBs to reduce dispatch overhead. */
    int limit = SLOW32_TB_MAX_INSNS;
    *max_insns = limit;
    (void)host_pc;
    DisasContext ctx = {
        .base = {
            .tb = tb,
            .pc_first = pc,
            .pc_next = pc,
            .is_jmp = DISAS_NEXT,
            .max_insns = limit,
        },
        .env = env,
        .pc = pc,
        .next_pc = pc,
        .is_jmp = DISAS_NEXT,
        .insn_count = 0,
    };

    /* TB chaining hook: start - enables plugin instrumentation and chaining */
    bool plugin_enabled = plugin_gen_tb_start(cs, &ctx.base);

    while (ctx.insn_count < limit) {
        ctx.pc = ctx.next_pc;
        ctx.next_pc = ctx.pc + 4;
        ctx.base.pc_next = ctx.next_pc;

        uint32_t raw = cpu_ldl_code(env, ctx.pc);
        bool collapse_branch = slow32_is_cmp_branch_pair(&ctx, raw);

        tcg_gen_insn_start(ctx.pc, ctx.next_pc);

        /* TB chaining hook: per-instruction start */
        if (plugin_enabled) {
            plugin_gen_insn_start(cs, &ctx.base);
        }

        ctx.insn_count++;
        ctx.base.num_insns = ctx.insn_count;

        if (try_emit_intrinsic(&ctx, env)) {
            if (plugin_enabled) {
                plugin_gen_insn_end();
            }
            break;
        }

        if (collapse_branch) {
            slow32_do_collapse_cmp_branch(&ctx, raw);
            if (plugin_enabled) {
                plugin_gen_insn_end();
            }
            break;
        }

        if (!translate_one(&ctx, raw)) {
            if (plugin_enabled) {
                plugin_gen_insn_end();
            }
            break;
        }

        bool keep_pending = false;
        if (ctx.pending_cmp.valid && ctx.is_jmp == DISAS_NEXT &&
            ctx.insn_count < limit) {
            uint32_t lookahead = cpu_ldl_code(env, ctx.next_pc);
            keep_pending = slow32_is_cmp_branch_pair(&ctx, lookahead);
        }

        if (!keep_pending) {
            slow32_flush_pending_cmp(&ctx);
        }

        /* TB chaining hook: per-instruction end */
        if (plugin_enabled) {
            plugin_gen_insn_end();
        }

        if (ctx.is_jmp != DISAS_NEXT) {
            break;
        }
    }

    slow32_flush_pending_cmp(&ctx);

    if (ctx.is_jmp == DISAS_NEXT) {
        slow32_count_insns(&ctx);
        gen_goto_tb(&ctx, 0, ctx.next_pc);
    }

    /* TB chaining hook: end (only if plugins were enabled) */
    if (plugin_enabled) {
        plugin_gen_tb_end(cs, ctx.insn_count);
    }

    tb->size = ctx.next_pc - pc;
    tb->icount = ctx.insn_count;
    *max_insns = ctx.insn_count;
}

void slow32_translate_init(void)
{
    for (int i = 0; i < SLOW32_NUM_GPRS; ++i) {
        char name[8];
        g_snprintf(name, sizeof(name), "r%d", i);
        cpu_regs[i] = tcg_global_mem_new_i32(tcg_env,
                                             offsetof(CPUSlow32State, regs[i]),
                                             name);
    }

    cpu_pc = tcg_global_mem_new_i32(tcg_env,
                                    offsetof(CPUSlow32State, pc), "pc");
    cpu_next_pc = tcg_global_mem_new_i32(tcg_env,
                                         offsetof(CPUSlow32State, next_pc),
                                         "next_pc");
    cpu_halted = tcg_global_mem_new_i32(tcg_env,
                                        offsetof(CPUSlow32State, halted),
                                        "halted");
}

void slow32_cpu_dump_state(CPUState *cs, FILE *f, int flags)
{
    CPUSlow32State *env = cpu_env(cs);

    qemu_fprintf(f, "pc=0x%08x halted=%d\n", env->pc, env->halted);
    for (int i = 0; i < SLOW32_NUM_GPRS; i += 4) {
        qemu_fprintf(f, "r%-2d=0x%08x r%-2d=0x%08x r%-2d=0x%08x r%-2d=0x%08x\n",
                     i, env->regs[i], i + 1, env->regs[i + 1],
                     i + 2, env->regs[i + 2], i + 3, env->regs[i + 3]);
    }
}

void slow32_disas_set_info(CPUState *cs, disassemble_info *info)
{
    info->print_insn = NULL;
}
