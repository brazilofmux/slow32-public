/*
 * s32_lift.c - SLOW-32 binary lifter to LLVM IR
 *
 * Stage 2: Per-function lifting with global register file.
 * Functions detected from JAL rd!=0 targets. Each function gets its own
 * LLVM function with alloca-based registers; inter-function state passes
 * through @regs global array.
 *
 * Usage: s32-lift input.s32x [-o output.ll]
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include "../../common/s32_formats.h"

/* ========================================================================
 * Opcode definitions (from translate.h)
 * ======================================================================== */

#define OP_ADD    0x00
#define OP_SUB    0x01
#define OP_XOR    0x02
#define OP_OR     0x03
#define OP_AND    0x04
#define OP_SLL    0x05
#define OP_SRL    0x06
#define OP_SRA    0x07
#define OP_SLT    0x08
#define OP_SLTU   0x09
#define OP_MUL    0x0A
#define OP_MULH   0x0B
#define OP_DIV    0x0C
#define OP_REM    0x0D
#define OP_SEQ    0x0E
#define OP_SNE    0x0F
#define OP_ADDI   0x10
#define OP_ORI    0x11
#define OP_ANDI   0x12
#define OP_SLLI   0x13
#define OP_SRLI   0x14
#define OP_SRAI   0x15
#define OP_SLTI   0x16
#define OP_SLTIU  0x17
#define OP_SGT    0x18
#define OP_SGTU   0x19
#define OP_SLE    0x1A
#define OP_SLEU   0x1B
#define OP_SGE    0x1C
#define OP_SGEU   0x1D
#define OP_XORI   0x1E
#define OP_MULHU  0x1F
#define OP_LUI    0x20
#define OP_LDB    0x30
#define OP_LDH    0x31
#define OP_LDW    0x32
#define OP_LDBU   0x33
#define OP_LDHU   0x34
#define OP_STB    0x38
#define OP_STH    0x39
#define OP_STW    0x3A
#define OP_ASSERT_EQ 0x3F
#define OP_JAL    0x40
#define OP_JALR   0x41
#define OP_BEQ    0x48
#define OP_BNE    0x49
#define OP_BLT    0x4A
#define OP_BGE    0x4B
#define OP_BLTU   0x4C
#define OP_BGEU   0x4D
#define OP_NOP    0x50
#define OP_YIELD  0x51
#define OP_DEBUG  0x52
#define OP_HALT   0x7F

/* ========================================================================
 * Instruction field extraction
 * ======================================================================== */

#define OPCODE(inst) ((inst) & 0x7F)
#define RD(inst)     (((inst) >> 7) & 0x1F)
#define RS1(inst)    (((inst) >> 15) & 0x1F)
#define RS2(inst)    (((inst) >> 20) & 0x1F)

static int32_t imm_i(uint32_t inst) {
    return ((int32_t)inst) >> 20;
}

static uint32_t imm_i_unsigned(uint32_t inst) {
    return (inst >> 20) & 0xFFF;
}

static int32_t imm_s(uint32_t inst) {
    return (int32_t)(((inst >> 7) & 0x1F) | (((int32_t)inst >> 25) << 5));
}

static int32_t imm_b(uint32_t inst) {
    return (int32_t)((((inst >> 8) & 0xF) << 1) |
                     (((inst >> 25) & 0x3F) << 5) |
                     (((inst >> 7) & 0x1) << 11) |
                     (((int32_t)inst >> 31) << 12));
}

static int32_t imm_j(uint32_t inst) {
    uint32_t imm20   = (inst >> 31) & 0x1;
    uint32_t imm10_1 = (inst >> 21) & 0x3FF;
    uint32_t imm11   = (inst >> 20) & 0x1;
    uint32_t imm19_12 = (inst >> 12) & 0xFF;
    int32_t imm = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
    if (imm & 0x100000) imm |= (int32_t)0xFFE00000;
    return imm;
}

static int32_t imm_u(uint32_t inst) {
    return (int32_t)(inst & 0xFFFFF000);
}

/* ========================================================================
 * Global state
 * ======================================================================== */

static FILE *out;

/* Code section */
static uint32_t *code;
static uint32_t code_base;
static uint32_t code_size;
static uint32_t code_limit_addr;
static int num_inst;

/* Data sections */
#define MAX_DATA_SECTIONS 32
static struct {
    uint32_t vaddr;
    uint32_t size;
    uint8_t *data;
} data_secs[MAX_DATA_SECTIONS];
static int num_data_secs;

/* Header info */
static uint32_t entry_point;
static uint32_t mem_size;
static uint32_t stack_base;

/* Basic block tracking */
static bool *block_starts;  /* indexed by instruction index: (pc - code_base)/4 */

/* Function tracking */
#define MAX_FUNCTIONS 4096
static uint32_t func_entries[MAX_FUNCTIONS]; /* sorted by PC */
static int num_funcs;

/* Temp variable counter for unique SSA names */
static int temp_ctr;

/* Which function we're currently emitting (index into func_entries) */
static int cur_func_idx;

/* ========================================================================
 * .s32x file loading
 * ======================================================================== */

static void load_s32x(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) { perror(path); exit(1); }

    s32x_header_t hdr;
    if (fread(&hdr, sizeof(hdr), 1, f) != 1) {
        fprintf(stderr, "Error: failed to read header\n");
        exit(1);
    }
    if (hdr.magic != S32X_MAGIC) {
        fprintf(stderr, "Error: not a .s32x file (magic=0x%08x)\n", hdr.magic);
        exit(1);
    }

    entry_point = hdr.entry;
    mem_size = hdr.mem_size;
    stack_base = hdr.stack_base;

    /* Read section table */
    s32x_section_t *sections = malloc(hdr.nsections * sizeof(s32x_section_t));
    fseek(f, hdr.sec_offset, SEEK_SET);
    if (fread(sections, sizeof(s32x_section_t), hdr.nsections, f) != hdr.nsections) {
        fprintf(stderr, "Error: failed to read section table\n");
        exit(1);
    }

    for (uint32_t i = 0; i < hdr.nsections; i++) {
        s32x_section_t *s = &sections[i];
        if (s->type == S32_SEC_CODE && s->size > 0 && !code) {
            code_base = s->vaddr;
            code_size = s->size;
            code = malloc(code_size);
            fseek(f, s->offset, SEEK_SET);
            fread(code, 1, code_size, f);
        } else if ((s->type == S32_SEC_DATA || s->type == S32_SEC_RODATA) && s->size > 0) {
            if (num_data_secs >= MAX_DATA_SECTIONS) {
                fprintf(stderr, "Warning: too many data sections, skipping\n");
                continue;
            }
            int idx = num_data_secs++;
            data_secs[idx].vaddr = s->vaddr;
            data_secs[idx].size = s->size;
            data_secs[idx].data = malloc(s->size);
            fseek(f, s->offset, SEEK_SET);
            fread(data_secs[idx].data, 1, s->size, f);
        }
    }

    if (!code) {
        fprintf(stderr, "Error: no code section found\n");
        exit(1);
    }

    num_inst = code_size / 4;
    code_limit_addr = code_base + code_size;

    free(sections);
    fclose(f);

    fprintf(stderr, "Loaded: %d instructions, entry=0x%x, stack=0x%x, mem=%uMB\n",
            num_inst, entry_point, stack_base, mem_size / (1024*1024));
    for (int i = 0; i < num_data_secs; i++) {
        fprintf(stderr, "  data section: vaddr=0x%x size=%u\n",
                data_secs[i].vaddr, data_secs[i].size);
    }
}

/* ========================================================================
 * Basic block and function analysis
 * ======================================================================== */

static inline bool pc_in_code(uint32_t pc) {
    return pc >= code_base && pc < code_limit_addr && ((pc - code_base) % 4 == 0);
}

static inline int pc_to_idx(uint32_t pc) {
    return (pc - code_base) / 4;
}

static void mark_block_start(uint32_t pc) {
    if (pc_in_code(pc))
        block_starts[pc_to_idx(pc)] = true;
}

static int cmp_u32(const void *a, const void *b) {
    uint32_t va = *(const uint32_t *)a;
    uint32_t vb = *(const uint32_t *)b;
    return (va > vb) - (va < vb);
}

static void add_func_entry(uint32_t pc) {
    if (!pc_in_code(pc)) return;
    /* Check for duplicates */
    for (int i = 0; i < num_funcs; i++)
        if (func_entries[i] == pc) return;
    if (num_funcs >= MAX_FUNCTIONS) {
        fprintf(stderr, "Warning: too many functions\n");
        return;
    }
    func_entries[num_funcs++] = pc;
}

/* Is the given PC a known function entry? (binary search, array must be sorted) */
static bool is_func_entry(uint32_t pc) {
    int lo = 0, hi = num_funcs - 1;
    while (lo <= hi) {
        int mid = (lo + hi) / 2;
        if (func_entries[mid] == pc) return true;
        if (func_entries[mid] < pc) lo = mid + 1;
        else hi = mid - 1;
    }
    return false;
}

/* Get the end address of a function (start of next function, or code_limit_addr) */
static uint32_t func_end(int func_idx) {
    if (func_idx + 1 < num_funcs)
        return func_entries[func_idx + 1];
    return code_limit_addr;
}

/* Check if a PC is within the current function's range */
static bool pc_in_cur_func(uint32_t pc) {
    return pc >= func_entries[cur_func_idx] && pc < func_end(cur_func_idx);
}

static void find_block_starts_and_functions(void) {
    block_starts = calloc(num_inst, sizeof(bool));

    /* Entry point is a function */
    add_func_entry(entry_point);
    mark_block_start(entry_point);
    mark_block_start(code_base);

    /* First pass: scan for JAL rd!=0 to find function entries */
    for (int i = 0; i < num_inst; i++) {
        uint32_t pc = code_base + i * 4;
        uint32_t inst = code[i];
        uint32_t op = OPCODE(inst);

        if (op == OP_JAL && RD(inst) != 0) {
            int32_t offset = imm_j(inst);
            uint32_t target = pc + offset;
            add_func_entry(target);
        }
    }

    /* Second pass: detect function entries by prologue pattern.
     * After a return (jalr r0, r31, 0), if the next instruction is
     * addi sp, sp, -N (N > 0), it's a function entry. */
    for (int i = 0; i < num_inst - 1; i++) {
        uint32_t inst = code[i];
        uint32_t op = OPCODE(inst);
        /* Look for jalr r0, r31, 0 (return) */
        if (op == OP_JALR && RD(inst) == 0 && RS1(inst) == 31 && imm_i(inst) == 0) {
            /* Check next non-return instruction */
            int j = i + 1;
            while (j < num_inst && OPCODE(code[j]) == OP_JALR &&
                   RD(code[j]) == 0 && RS1(code[j]) == 31 && imm_i(code[j]) == 0)
                j++;
            if (j < num_inst) {
                uint32_t next = code[j];
                /* addi sp, sp, -N → op=ADDI, rd=29, rs1=29, imm < 0 */
                if (OPCODE(next) == OP_ADDI && RD(next) == 29 && RS1(next) == 29 &&
                    imm_i(next) < 0) {
                    add_func_entry(code_base + j * 4);
                }
            }
        }
    }

    /* Sort function entries by PC */
    qsort(func_entries, num_funcs, sizeof(uint32_t), cmp_u32);

    /* All function entries are block starts */
    for (int i = 0; i < num_funcs; i++)
        mark_block_start(func_entries[i]);

    /* Second pass: find basic block boundaries */
    for (int i = 0; i < num_inst; i++) {
        uint32_t pc = code_base + i * 4;
        uint32_t inst = code[i];
        uint32_t op = OPCODE(inst);

        switch (op) {
        case OP_BEQ: case OP_BNE: case OP_BLT: case OP_BGE:
        case OP_BLTU: case OP_BGEU: {
            int32_t offset = imm_b(inst);
            uint32_t target = pc + 4 + offset;  /* branches are PC+4 relative */
            mark_block_start(target);
            if (i + 1 < num_inst) mark_block_start(pc + 4);
            break;
        }
        case OP_JAL: {
            int32_t offset = imm_j(inst);
            uint32_t target = pc + offset;
            mark_block_start(target);
            if (i + 1 < num_inst) mark_block_start(pc + 4);
            break;
        }
        case OP_JALR:
        case OP_HALT:
            if (i + 1 < num_inst) mark_block_start(pc + 4);
            break;
        default:
            break;
        }
    }

    int nblocks = 0;
    for (int i = 0; i < num_inst; i++)
        if (block_starts[i]) nblocks++;
    fprintf(stderr, "Found %d functions, %d basic blocks\n", num_funcs, nblocks);
}

/* ========================================================================
 * IR emission helpers
 * ======================================================================== */

static int T(void) { return temp_ctr++; }

/* Read a guest register. Emits a load from the alloca. Returns temp ID. */
static int read_reg(int reg) {
    int t = T();
    if (reg == 0) {
        fprintf(out, "  %%t%d = add i32 0, 0\n", t);
    } else {
        fprintf(out, "  %%t%d = load i32, ptr %%r%d\n", t, reg);
    }
    return t;
}

/* Write a temp value to a guest register. No-op for r0. */
static void write_reg(int reg, int t) {
    if (reg == 0) return;
    fprintf(out, "  store i32 %%t%d, ptr %%r%d\n", t, reg);
}

/* Emit a memory address computation: base_temp + offset → ptr */
static int emit_mem_addr(int base_temp, int32_t offset) {
    int addr = T();
    fprintf(out, "  %%t%d = add i32 %%t%d, %d\n", addr, base_temp, offset);
    int addr64 = T();
    fprintf(out, "  %%t%d = zext i32 %%t%d to i64\n", addr64, addr);
    int ptr = T();
    fprintf(out, "  %%t%d = getelementptr i8, ptr %%mem, i64 %%t%d\n", ptr, addr64);
    return ptr;
}

/* Flush local allocas (r1-r31) to @regs global */
static void emit_flush_regs(void) {
    for (int i = 1; i < 32; i++) {
        int t = T();
        fprintf(out, "  %%t%d = load i32, ptr %%r%d\n", t, i);
        int gep = T();
        fprintf(out, "  %%t%d = getelementptr [32 x i32], ptr @regs, i64 0, i64 %d\n", gep, i);
        fprintf(out, "  store i32 %%t%d, ptr %%t%d\n", t, gep);
    }
}

/* Reload local allocas (r1-r31) from @regs global */
static void emit_reload_regs(void) {
    for (int i = 1; i < 32; i++) {
        int gep = T();
        fprintf(out, "  %%t%d = getelementptr [32 x i32], ptr @regs, i64 0, i64 %d\n", gep, i);
        int t = T();
        fprintf(out, "  %%t%d = load i32, ptr %%t%d\n", t, gep);
        fprintf(out, "  store i32 %%t%d, ptr %%r%d\n", t, i);
    }
}

/* ========================================================================
 * Opcode name tables
 * ======================================================================== */

static const char *r_type_ir_op(uint32_t op) {
    switch (op) {
    case OP_ADD: return "add";
    case OP_SUB: return "sub";
    case OP_XOR: return "xor";
    case OP_OR:  return "or";
    case OP_AND: return "and";
    case OP_SLL: return "shl";
    case OP_SRL: return "lshr";
    case OP_SRA: return "ashr";
    case OP_MUL: return "mul";
    default:     return NULL;
    }
}

static const char *cmp_pred(uint32_t op) {
    switch (op) {
    case OP_SLT:  return "slt";
    case OP_SLTU: return "ult";
    case OP_SEQ:  return "eq";
    case OP_SNE:  return "ne";
    case OP_SGT:  return "sgt";
    case OP_SGTU: return "ugt";
    case OP_SLE:  return "sle";
    case OP_SLEU: return "ule";
    case OP_SGE:  return "sge";
    case OP_SGEU: return "uge";
    default:      return "eq";
    }
}

static const char *br_pred(uint32_t op) {
    switch (op) {
    case OP_BEQ:  return "eq";
    case OP_BNE:  return "ne";
    case OP_BLT:  return "slt";
    case OP_BGE:  return "sge";
    case OP_BLTU: return "ult";
    case OP_BGEU: return "uge";
    default:      return "eq";
    }
}

/* ========================================================================
 * Per-instruction IR emission
 * Returns true if the instruction is a block terminator.
 * ======================================================================== */

static bool emit_instruction(uint32_t pc, uint32_t inst) {
    uint32_t op = OPCODE(inst);
    int rd = RD(inst), rs1 = RS1(inst), rs2 = RS2(inst);

    /* ---- Simple R-type arithmetic ---- */
    const char *ir_op = r_type_ir_op(op);
    if (ir_op) {
        int a = read_reg(rs1), b = read_reg(rs2);
        /* For shifts, mask amount to 5 bits (SLOW-32 behavior).
         * Without this, LLVM treats shift >= 32 as UB and miscompiles. */
        if (op == OP_SLL || op == OP_SRL || op == OP_SRA) {
            int masked = T();
            fprintf(out, "  %%t%d = and i32 %%t%d, 31\n", masked, b);
            b = masked;
        }
        int t = T();
        fprintf(out, "  %%t%d = %s i32 %%t%d, %%t%d\n", t, ir_op, a, b);
        write_reg(rd, t);
        return false;
    }

    switch (op) {

    /* ---- Signed division/remainder ---- */
    case OP_DIV: {
        int a = read_reg(rs1), b = read_reg(rs2);
        int t = T();
        fprintf(out, "  %%t%d = sdiv i32 %%t%d, %%t%d\n", t, a, b);
        write_reg(rd, t);
        return false;
    }
    case OP_REM: {
        int a = read_reg(rs1), b = read_reg(rs2);
        int t = T();
        fprintf(out, "  %%t%d = srem i32 %%t%d, %%t%d\n", t, a, b);
        write_reg(rd, t);
        return false;
    }

    /* ---- Multiply high (signed) ---- */
    case OP_MULH: {
        int a = read_reg(rs1), b = read_reg(rs2);
        int a64 = T(), b64 = T();
        fprintf(out, "  %%t%d = sext i32 %%t%d to i64\n", a64, a);
        fprintf(out, "  %%t%d = sext i32 %%t%d to i64\n", b64, b);
        int prod = T();
        fprintf(out, "  %%t%d = mul i64 %%t%d, %%t%d\n", prod, a64, b64);
        int hi = T();
        fprintf(out, "  %%t%d = lshr i64 %%t%d, 32\n", hi, prod);
        int t = T();
        fprintf(out, "  %%t%d = trunc i64 %%t%d to i32\n", t, hi);
        write_reg(rd, t);
        return false;
    }

    /* ---- Multiply high (unsigned) ---- */
    case OP_MULHU: {
        int a = read_reg(rs1), b = read_reg(rs2);
        int a64 = T(), b64 = T();
        fprintf(out, "  %%t%d = zext i32 %%t%d to i64\n", a64, a);
        fprintf(out, "  %%t%d = zext i32 %%t%d to i64\n", b64, b);
        int prod = T();
        fprintf(out, "  %%t%d = mul i64 %%t%d, %%t%d\n", prod, a64, b64);
        int hi = T();
        fprintf(out, "  %%t%d = lshr i64 %%t%d, 32\n", hi, prod);
        int t = T();
        fprintf(out, "  %%t%d = trunc i64 %%t%d to i32\n", t, hi);
        write_reg(rd, t);
        return false;
    }

    /* ---- R-type comparisons ---- */
    case OP_SLT: case OP_SLTU: case OP_SEQ: case OP_SNE:
    case OP_SGT: case OP_SGTU: case OP_SLE: case OP_SLEU:
    case OP_SGE: case OP_SGEU: {
        int a = read_reg(rs1), b = read_reg(rs2);
        int c = T();
        fprintf(out, "  %%t%d = icmp %s i32 %%t%d, %%t%d\n", c, cmp_pred(op), a, b);
        int t = T();
        fprintf(out, "  %%t%d = zext i1 %%t%d to i32\n", t, c);
        write_reg(rd, t);
        return false;
    }

    /* ---- ADDI (sign-extended immediate) ---- */
    case OP_ADDI: {
        int a = read_reg(rs1);
        int32_t imm = imm_i(inst);
        int t = T();
        fprintf(out, "  %%t%d = add i32 %%t%d, %d\n", t, a, imm);
        write_reg(rd, t);
        return false;
    }

    /* ---- ORI, ANDI, XORI (zero-extended immediate) ---- */
    case OP_ORI: {
        int a = read_reg(rs1);
        uint32_t imm = imm_i_unsigned(inst);
        int t = T();
        fprintf(out, "  %%t%d = or i32 %%t%d, %u\n", t, a, imm);
        write_reg(rd, t);
        return false;
    }
    case OP_ANDI: {
        int a = read_reg(rs1);
        uint32_t imm = imm_i_unsigned(inst);
        int t = T();
        fprintf(out, "  %%t%d = and i32 %%t%d, %u\n", t, a, imm);
        write_reg(rd, t);
        return false;
    }
    case OP_XORI: {
        int a = read_reg(rs1);
        uint32_t imm = imm_i_unsigned(inst);
        int t = T();
        fprintf(out, "  %%t%d = xor i32 %%t%d, %u\n", t, a, imm);
        write_reg(rd, t);
        return false;
    }

    /* ---- Shift immediates ---- */
    case OP_SLLI: {
        int a = read_reg(rs1);
        uint32_t shamt = imm_i_unsigned(inst) & 0x1F;
        int t = T();
        fprintf(out, "  %%t%d = shl i32 %%t%d, %u\n", t, a, shamt);
        write_reg(rd, t);
        return false;
    }
    case OP_SRLI: {
        int a = read_reg(rs1);
        uint32_t shamt = imm_i_unsigned(inst) & 0x1F;
        int t = T();
        fprintf(out, "  %%t%d = lshr i32 %%t%d, %u\n", t, a, shamt);
        write_reg(rd, t);
        return false;
    }
    case OP_SRAI: {
        int a = read_reg(rs1);
        uint32_t shamt = imm_i_unsigned(inst) & 0x1F;
        int t = T();
        fprintf(out, "  %%t%d = ashr i32 %%t%d, %u\n", t, a, shamt);
        write_reg(rd, t);
        return false;
    }

    /* ---- SLTI (sign-extended immediate, signed comparison) ---- */
    case OP_SLTI: {
        int a = read_reg(rs1);
        int32_t imm = imm_i(inst);
        int c = T();
        fprintf(out, "  %%t%d = icmp slt i32 %%t%d, %d\n", c, a, imm);
        int t = T();
        fprintf(out, "  %%t%d = zext i1 %%t%d to i32\n", t, c);
        write_reg(rd, t);
        return false;
    }

    /* ---- SLTIU (zero-extended immediate, unsigned comparison) ---- */
    case OP_SLTIU: {
        int a = read_reg(rs1);
        uint32_t imm = imm_i_unsigned(inst);
        int c = T();
        fprintf(out, "  %%t%d = icmp ult i32 %%t%d, %u\n", c, a, imm);
        int t = T();
        fprintf(out, "  %%t%d = zext i1 %%t%d to i32\n", t, c);
        write_reg(rd, t);
        return false;
    }

    /* ---- LUI ---- */
    case OP_LUI: {
        int32_t imm = imm_u(inst);
        int t = T();
        fprintf(out, "  %%t%d = add i32 0, %d\n", t, imm);
        write_reg(rd, t);
        return false;
    }

    /* ---- Loads ---- */
    case OP_LDW: {
        int base = read_reg(rs1);
        int ptr = emit_mem_addr(base, imm_i(inst));
        int val = T();
        fprintf(out, "  %%t%d = load i32, ptr %%t%d, align 1\n", val, ptr);
        write_reg(rd, val);
        return false;
    }
    case OP_LDH: {
        int base = read_reg(rs1);
        int ptr = emit_mem_addr(base, imm_i(inst));
        int half = T();
        fprintf(out, "  %%t%d = load i16, ptr %%t%d, align 1\n", half, ptr);
        int val = T();
        fprintf(out, "  %%t%d = sext i16 %%t%d to i32\n", val, half);
        write_reg(rd, val);
        return false;
    }
    case OP_LDB: {
        int base = read_reg(rs1);
        int ptr = emit_mem_addr(base, imm_i(inst));
        int byte_val = T();
        fprintf(out, "  %%t%d = load i8, ptr %%t%d, align 1\n", byte_val, ptr);
        int val = T();
        fprintf(out, "  %%t%d = sext i8 %%t%d to i32\n", val, byte_val);
        write_reg(rd, val);
        return false;
    }
    case OP_LDHU: {
        int base = read_reg(rs1);
        int ptr = emit_mem_addr(base, imm_i(inst));
        int half = T();
        fprintf(out, "  %%t%d = load i16, ptr %%t%d, align 1\n", half, ptr);
        int val = T();
        fprintf(out, "  %%t%d = zext i16 %%t%d to i32\n", val, half);
        write_reg(rd, val);
        return false;
    }
    case OP_LDBU: {
        int base = read_reg(rs1);
        int ptr = emit_mem_addr(base, imm_i(inst));
        int byte_val = T();
        fprintf(out, "  %%t%d = load i8, ptr %%t%d, align 1\n", byte_val, ptr);
        int val = T();
        fprintf(out, "  %%t%d = zext i8 %%t%d to i32\n", val, byte_val);
        write_reg(rd, val);
        return false;
    }

    /* ---- Stores ---- */
    case OP_STW: {
        int base = read_reg(rs1);
        int val = read_reg(rs2);
        int ptr = emit_mem_addr(base, imm_s(inst));
        fprintf(out, "  store i32 %%t%d, ptr %%t%d, align 1\n", val, ptr);
        return false;
    }
    case OP_STH: {
        int base = read_reg(rs1);
        int val = read_reg(rs2);
        int ptr = emit_mem_addr(base, imm_s(inst));
        int half = T();
        fprintf(out, "  %%t%d = trunc i32 %%t%d to i16\n", half, val);
        fprintf(out, "  store i16 %%t%d, ptr %%t%d, align 1\n", half, ptr);
        return false;
    }
    case OP_STB: {
        int base = read_reg(rs1);
        int val = read_reg(rs2);
        int ptr = emit_mem_addr(base, imm_s(inst));
        int byte_val = T();
        fprintf(out, "  %%t%d = trunc i32 %%t%d to i8\n", byte_val, val);
        fprintf(out, "  store i8 %%t%d, ptr %%t%d, align 1\n", byte_val, ptr);
        return false;
    }

    /* ---- JAL (direct jump and link) ---- */
    case OP_JAL: {
        int32_t offset = imm_j(inst);
        uint32_t target = pc + offset;

        if (rd != 0 && is_func_entry(target)) {
            /* Function call: JAL rd, target where target is a function entry */
            /* Store return address in rd */
            int t = T();
            fprintf(out, "  %%t%d = add i32 0, %u\n", t, pc + 4);
            write_reg(rd, t);
            /* Flush registers, call function, reload */
            emit_flush_regs();
            fprintf(out, "  call void @func_0x%x(ptr %%mem)\n", target);
            emit_reload_regs();
            /* Fall through to next instruction (not a terminator) */
            return false;
        }

        if (rd == 0 && is_func_entry(target) && !pc_in_cur_func(target)) {
            /* Tail call to another function: JAL r0, target */
            emit_flush_regs();
            fprintf(out, "  call void @func_0x%x(ptr %%mem)\n", target);
            fprintf(out, "  ret void\n");
            return true;
        }

        /* Local jump or cross-function jump */
        if (rd != 0) {
            int t = T();
            fprintf(out, "  %%t%d = add i32 0, %u\n", t, pc + 4);
            write_reg(rd, t);
        }
        if (pc_in_code(target) && pc_in_cur_func(target)) {
            fprintf(out, "  br label %%pc_0x%x\n", target);
        } else if (pc_in_code(target)) {
            /* Cross-function jump: flush and dispatch */
            emit_flush_regs();
            fprintf(out, "  call void @s32_dispatch(ptr %%mem, i32 %u)\n", target);
            fprintf(out, "  ret void\n");
        } else {
            fprintf(out, "  call void @llvm.trap()\n");
            fprintf(out, "  unreachable\n");
        }
        return true;
    }

    /* ---- JALR (indirect jump and link) ---- */
    case OP_JALR: {
        int32_t offset = imm_i(inst);
        int base = read_reg(rs1);
        int target = T();
        fprintf(out, "  %%t%d = add i32 %%t%d, %d\n", target, base, offset);

        /* JALR r0, r31, 0 → function return */
        if (rd == 0 && rs1 == 31 && offset == 0) {
            emit_flush_regs();
            fprintf(out, "  ret void\n");
            return true;
        }

        /* JALR with link register → indirect call */
        if (rd != 0) {
            int t = T();
            fprintf(out, "  %%t%d = add i32 0, %u\n", t, pc + 4);
            write_reg(rd, t);
            emit_flush_regs();
            fprintf(out, "  call void @s32_dispatch(ptr %%mem, i32 %%t%d)\n", target);
            emit_reload_regs();
            return false;
        }

        /* JALR r0, rs, offset → indirect jump (jump table within function, or tail call) */
        /* First, try local switch for blocks within this function */
        fprintf(out, "  switch i32 %%t%d, label %%jalr_tail_%x [\n", target, pc);
        uint32_t fstart = func_entries[cur_func_idx];
        uint32_t fend = func_end(cur_func_idx);
        for (uint32_t bpc = fstart; bpc < fend; bpc += 4) {
            int idx = pc_to_idx(bpc);
            if (block_starts[idx]) {
                fprintf(out, "    i32 %u, label %%pc_0x%x\n", bpc, bpc);
            }
        }
        fprintf(out, "  ]\n");
        /* Default: tail call via dispatch */
        fprintf(out, "\njalr_tail_%x:\n", pc);
        emit_flush_regs();
        fprintf(out, "  call void @s32_dispatch(ptr %%mem, i32 %%t%d)\n", target);
        fprintf(out, "  ret void\n");
        return true;
    }

    /* ---- Branches ---- */
    case OP_BEQ: case OP_BNE: case OP_BLT: case OP_BGE:
    case OP_BLTU: case OP_BGEU: {
        int a = read_reg(rs1), b = read_reg(rs2);
        int c = T();
        fprintf(out, "  %%t%d = icmp %s i32 %%t%d, %%t%d\n", c, br_pred(op), a, b);
        uint32_t target = pc + 4 + imm_b(inst);
        uint32_t fall = pc + 4;
        bool target_local = pc_in_code(target) && pc_in_cur_func(target);
        bool fall_local = pc_in_code(fall) && pc_in_cur_func(fall);

        if (target_local && fall_local) {
            /* Both targets in current function - simple branch */
            fprintf(out, "  br i1 %%t%d, label %%pc_0x%x, label %%pc_0x%x\n",
                    c, target, fall);
        } else {
            /* At least one target is cross-function: use conditional blocks */
            fprintf(out, "  br i1 %%t%d, label %%br_taken_%x, label %%br_fall_%x\n",
                    c, pc, pc);
            fprintf(out, "\nbr_taken_%x:\n", pc);
            if (target_local) {
                fprintf(out, "  br label %%pc_0x%x\n", target);
            } else if (pc_in_code(target)) {
                emit_flush_regs();
                fprintf(out, "  call void @s32_dispatch(ptr %%mem, i32 %u)\n", target);
                fprintf(out, "  ret void\n");
            } else {
                fprintf(out, "  br label %%trap\n");
            }
            fprintf(out, "\nbr_fall_%x:\n", pc);
            if (fall_local) {
                fprintf(out, "  br label %%pc_0x%x\n", fall);
            } else if (pc_in_code(fall)) {
                emit_flush_regs();
                fprintf(out, "  call void @s32_dispatch(ptr %%mem, i32 %u)\n", fall);
                fprintf(out, "  ret void\n");
            } else {
                fprintf(out, "  br label %%trap\n");
            }
        }
        return true;
    }

    /* ---- NOP / YIELD ---- */
    case OP_NOP:
    case OP_YIELD:
        return false;

    /* ---- DEBUG ---- */
    case OP_DEBUG: {
        int val = read_reg(rs1);
        fprintf(out, "  call void @s32_debug(i32 %%t%d)\n", val);
        return false;
    }

    /* ---- HALT ---- */
    case OP_HALT:
        fprintf(out, "  call void @s32_halt()\n");
        fprintf(out, "  unreachable\n");
        return true;

    /* ---- ASSERT_EQ ---- */
    case OP_ASSERT_EQ:
        return false;

    /* ---- Unknown ---- */
    default:
        fprintf(stderr, "Warning: unimplemented opcode 0x%02x at pc=0x%x\n", op, pc);
        fprintf(out, "  ; UNIMPLEMENTED opcode 0x%02x\n", op);
        return false;
    }
}

/* ========================================================================
 * Top-level IR emission
 * ======================================================================== */

static void emit_header(void) {
    fprintf(out, "; SLOW-32 lifted binary - generated by s32-lift (Stage 2)\n");
    fprintf(out, "target triple = \"x86_64-unknown-linux-gnu\"\n\n");

    /* Runtime function declarations */
    fprintf(out, "declare void @s32_debug(i32)\n");
    fprintf(out, "declare void @s32_halt()\n");
    fprintf(out, "declare void @s32_dispatch_fail(i32)\n");
    fprintf(out, "declare void @llvm.trap() nounwind noreturn\n");
    fprintf(out, "declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)\n");
    fprintf(out, "\n");

    /* Memory layout constants */
    fprintf(out, "@__s32_mem_size = global i32 %u\n", mem_size);
    fprintf(out, "@__s32_stack_base = global i32 %u\n", stack_base);
    fprintf(out, "\n");

    /* Global register file for inter-function communication */
    fprintf(out, "@regs = internal global [32 x i32] zeroinitializer\n\n");
}

static void emit_data_constants(void) {
    for (int i = 0; i < num_data_secs; i++) {
        fprintf(out, "@__s32_data_%d = private constant [%u x i8] c\"",
                i, data_secs[i].size);
        for (uint32_t j = 0; j < data_secs[i].size; j++) {
            fprintf(out, "\\%02X", data_secs[i].data[j]);
        }
        fprintf(out, "\"\n");
    }
    if (num_data_secs > 0) fprintf(out, "\n");
}

static void emit_init_memory(void) {
    fprintf(out, "define void @s32_init_memory(ptr %%mem) {\n");
    fprintf(out, "entry:\n");
    for (int i = 0; i < num_data_secs; i++) {
        int dst = T();
        fprintf(out, "  %%t%d = getelementptr i8, ptr %%mem, i64 %u\n",
                dst, data_secs[i].vaddr);
        fprintf(out, "  call void @llvm.memcpy.p0.p0.i64(ptr %%t%d, ptr @__s32_data_%d, "
                "i64 %u, i1 false)\n", dst, i, data_secs[i].size);
    }
    fprintf(out, "  ret void\n");
    fprintf(out, "}\n\n");
}

/* Emit one lifted function */
static void emit_function(int func_idx) {
    uint32_t fstart = func_entries[func_idx];
    uint32_t fend = func_end(func_idx);
    cur_func_idx = func_idx;
    temp_ctr = 0;

    fprintf(out, "define void @func_0x%x(ptr noalias %%mem) {\n", fstart);
    fprintf(out, "entry:\n");

    /* Register allocas */
    for (int i = 0; i < 32; i++) {
        fprintf(out, "  %%r%d = alloca i32, align 4\n", i);
    }

    /* Initialize r0 = 0, load r1-r31 from @regs */
    fprintf(out, "  store i32 0, ptr %%r0\n");
    for (int i = 1; i < 32; i++) {
        int gep = T();
        fprintf(out, "  %%t%d = getelementptr [32 x i32], ptr @regs, i64 0, i64 %d\n", gep, i);
        int val = T();
        fprintf(out, "  %%t%d = load i32, ptr %%t%d\n", val, gep);
        fprintf(out, "  store i32 %%t%d, ptr %%r%d\n", val, i);
    }

    /* Jump to function entry */
    fprintf(out, "  br label %%pc_0x%x\n\n", fstart);

    /* Emit basic blocks for this function's range */
    int istart = pc_to_idx(fstart);
    int iend = pc_to_idx(fend);
    bool in_block = false;

    for (int i = istart; i < iend; i++) {
        uint32_t pc = code_base + i * 4;
        uint32_t inst = code[i];

        if (block_starts[i]) {
            if (in_block) {
                fprintf(out, "  br label %%pc_0x%x\n", pc);
            }
            fprintf(out, "\npc_0x%x:\n", pc);
            in_block = true;
        }

        if (!in_block) {
            fprintf(out, "\npc_0x%x:\n", pc);
            in_block = true;
        }

        fprintf(out, "  ; [0x%04x] %08x\n", pc, inst);

        bool is_term = emit_instruction(pc, inst);
        if (is_term) {
            in_block = false;
        }
    }

    /* If the last block doesn't have a terminator, add unreachable */
    if (in_block) {
        fprintf(out, "  call void @llvm.trap()\n");
        fprintf(out, "  unreachable\n");
    }

    /* Trap block */
    fprintf(out, "\ntrap:\n");
    fprintf(out, "  call void @llvm.trap()\n");
    fprintf(out, "  unreachable\n");

    fprintf(out, "}\n\n");
}

/* Emit dispatch function for indirect calls */
static void emit_dispatch_func(void) {
    fprintf(out, "define void @s32_dispatch(ptr %%mem, i32 %%target) {\n");
    fprintf(out, "entry:\n");
    fprintf(out, "  switch i32 %%target, label %%trap [\n");
    for (int i = 0; i < num_funcs; i++) {
        fprintf(out, "    i32 %u, label %%call_%d\n", func_entries[i], i);
    }
    fprintf(out, "  ]\n\n");

    for (int i = 0; i < num_funcs; i++) {
        fprintf(out, "call_%d:\n", i);
        fprintf(out, "  call void @func_0x%x(ptr %%mem)\n", func_entries[i]);
        fprintf(out, "  ret void\n\n");
    }

    fprintf(out, "trap:\n");
    fprintf(out, "  call void @s32_dispatch_fail(i32 %%target)\n");
    fprintf(out, "  call void @llvm.trap()\n");
    fprintf(out, "  unreachable\n");
    fprintf(out, "}\n\n");
}

/* Emit entry wrapper that initializes @regs and calls the entry function */
static void emit_entry_wrapper(void) {
    fprintf(out, "define void @s32_entry(ptr %%mem) {\n");
    fprintf(out, "entry:\n");

    /* Initialize all regs to 0 */
    for (int i = 0; i < 32; i++) {
        int gep = T();
        fprintf(out, "  %%t%d = getelementptr [32 x i32], ptr @regs, i64 0, i64 %d\n", gep, i);
        fprintf(out, "  store i32 0, ptr %%t%d\n", gep);
    }

    /* Set r29 (SP) = stack_base */
    int sp_gep = T();
    fprintf(out, "  %%t%d = getelementptr [32 x i32], ptr @regs, i64 0, i64 29\n", sp_gep);
    fprintf(out, "  store i32 %u, ptr %%t%d\n", stack_base, sp_gep);

    /* Call entry function */
    fprintf(out, "  call void @func_0x%x(ptr %%mem)\n", entry_point);
    fprintf(out, "  ret void\n");
    fprintf(out, "}\n");
}

/* ========================================================================
 * Main
 * ======================================================================== */

static void usage(const char *prog) {
    fprintf(stderr, "Usage: %s input.s32x [-o output.ll]\n", prog);
    fprintf(stderr, "  Lifts a SLOW-32 binary to LLVM IR\n");
    exit(1);
}

int main(int argc, char **argv) {
    const char *input = NULL;
    const char *output = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            output = argv[++i];
        } else if (argv[i][0] == '-') {
            usage(argv[0]);
        } else {
            input = argv[i];
        }
    }

    if (!input) usage(argv[0]);

    /* Load the binary */
    load_s32x(input);

    /* Analyze control flow and detect functions */
    find_block_starts_and_functions();

    /* Open output */
    if (output) {
        out = fopen(output, "w");
        if (!out) { perror(output); exit(1); }
    } else {
        out = stdout;
    }

    /* Emit IR */
    temp_ctr = 0;
    emit_header();
    emit_data_constants();

    temp_ctr = 0;
    emit_init_memory();

    /* Forward declarations (will be replaced by actual definitions) */
    /* We don't need forward declares since we'll emit all definitions */

    /* Emit all functions */
    for (int i = 0; i < num_funcs; i++) {
        emit_function(i);
    }

    /* Emit dispatch function for indirect calls */
    temp_ctr = 0;
    emit_dispatch_func();

    /* Emit entry wrapper */
    temp_ctr = 0;
    emit_entry_wrapper();

    if (output) fclose(out);

    fprintf(stderr, "Lifted %d instructions into %d functions\n", num_inst, num_funcs);
    return 0;
}
