// SLOW-32 Instruction Set Definitions (clean-room copy for Stage 5)
// This file is intentionally standalone. It contains only the guest ISA
// encoding, decoded instruction struct, and decoder.  No Stage 1-4
// translator types (translate_ctx_t, reg cache, emit helpers, etc.).
//
// Stage 5 (dbt5) and any future independent backends must use this or an
// equivalent, never pull in the production translate.h from ../dbt/.

#ifndef DBT5_SLOW32_INST_H
#define DBT5_SLOW32_INST_H

#include <stdint.h>

// ============================================================================
// SLOW-32 opcode definitions
// ============================================================================

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
// f32 instructions
#define OP_FADD_S    0x53
#define OP_FSUB_S    0x54
#define OP_FMUL_S    0x55
#define OP_FDIV_S    0x56
#define OP_FSQRT_S   0x57
#define OP_FEQ_S     0x58
#define OP_FLT_S     0x59
#define OP_FLE_S     0x5A
#define OP_FCVT_W_S  0x5B
#define OP_FCVT_WU_S 0x5C
#define OP_FCVT_S_W  0x5D
#define OP_FCVT_S_WU 0x5E
#define OP_FNEG_S    0x5F
#define OP_FABS_S    0x60
// f64 instructions
#define OP_FADD_D    0x61
#define OP_FSUB_D    0x62
#define OP_FMUL_D    0x63
#define OP_FDIV_D    0x64
#define OP_FSQRT_D   0x65
#define OP_FEQ_D     0x66
#define OP_FLT_D     0x67
#define OP_FLE_D     0x68
#define OP_FCVT_W_D  0x69
#define OP_FCVT_WU_D 0x6A
#define OP_FCVT_D_W  0x6B
#define OP_FCVT_D_WU 0x6C
#define OP_FCVT_D_S  0x6D
#define OP_FCVT_S_D  0x6E
#define OP_FNEG_D    0x6F
#define OP_FABS_D    0x70
// float <-> int64 conversions
#define OP_FCVT_L_S  0x71
#define OP_FCVT_LU_S 0x72
#define OP_FCVT_S_L  0x73
#define OP_FCVT_S_LU 0x74
#define OP_FCVT_L_D  0x75
#define OP_FCVT_LU_D 0x76
#define OP_FCVT_D_L  0x77
#define OP_FCVT_D_LU 0x78
#define OP_HALT   0x7F

// ============================================================================
// Instruction format types
// ============================================================================

typedef enum {
    FMT_R,      // Register-register: rd, rs1, rs2
    FMT_I,      // Immediate: rd, rs1, imm12
    FMT_S,      // Store: rs1, rs2, imm12
    FMT_B,      // Branch: rs1, rs2, imm13
    FMT_U,      // Upper immediate: rd, imm20
    FMT_J,      // Jump: rd, imm21
} inst_format_t;

// Decoded instruction (guest)
typedef struct {
    uint32_t raw;
    uint8_t opcode;
    uint8_t rd;
    uint8_t rs1;
    uint8_t rs2;
    int32_t imm;
    inst_format_t format;
} decoded_inst_t;

// Decode a raw 32-bit SLOW-32 instruction word.
// The implementation is host-architecture independent.
decoded_inst_t decode_instruction(uint32_t raw);

#endif // DBT5_SLOW32_INST_H
