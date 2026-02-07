// SLOW-32 DBT: Block Translator
// Stage 2 - Block caching and direct chaining

#ifndef DBT_TRANSLATE_H
#define DBT_TRANSLATE_H

#include "cpu_state.h"
#include "emit_x64.h"

// Forward declarations
typedef struct block_cache block_cache_t;
typedef struct translated_block translated_block_t;

// Maximum instructions per basic block
#define MAX_BLOCK_INSTS 64

#include "dbt_limits.h"

// Stage 4: Superblock limits
#define MAX_SUPERBLOCK_DEPTH 4      // Max conditional branches to extend past
#define SUPERBLOCK_PROFILE_MIN_SAMPLES 1000
#define SUPERBLOCK_TAKEN_PCT_THRESHOLD 3  // Allow extension if taken <= 3%

// Runtime-tunable thresholds (initialized from defaults above)
extern uint32_t superblock_profile_min_samples;
extern uint32_t superblock_taken_pct_threshold;

// Translation context
typedef struct {
    dbt_cpu_state_t *cpu;
    emit_ctx_t emit;
    uint32_t guest_pc;          // Current guest PC being translated
    uint32_t block_start_pc;    // Start of current block
    int inst_count;             // Instructions translated so far

    // Stage 2: Block cache integration
    block_cache_t *cache;       // Block cache (NULL for Stage 1 mode)
    translated_block_t *block;  // Current block being translated
    int exit_idx;               // Current exit index for chaining

    // Stage 3: Inline indirect branch lookup
    bool inline_lookup_enabled; // Emit inline hash lookup for JALR

    // Stage 3 Phase 2: Return Address Stack
    bool ras_enabled;           // Emit RAS push/predict for calls/returns

    // Stage 4: Superblock extension
    bool superblock_enabled;    // Extend past forward conditional branches
    int superblock_depth;       // Number of branches extended past so far
    int side_exit_emitted;      // Superblock side exits emitted in this block
    bool profile_side_exits;    // Emit side-exit profile counters
    uint32_t side_exit_pcs[MAX_BLOCK_EXITS];
    bool side_exit_info_enabled; // Emit branch_pc info for diagnostics
    bool avoid_backedge_extend;  // Study-only guard: don't extend across back-edges
    bool peephole_enabled;       // Peephole optimize emitted host code

    // Stage 5: Per-block register allocation (8-slot fully-associative)
    bool reg_cache_enabled;
    uint32_t reg_cache_hits;
    uint32_t reg_cache_misses;

    #define REG_ALLOC_SLOTS 8
    struct {
        uint8_t guest_reg;
        bool    allocated;
        bool    dirty;
    } reg_alloc[REG_ALLOC_SLOTS];
    int8_t reg_alloc_map[32];   // guest_reg -> slot index, or -1

    // Guest PC → host code offset map (for in-block back-edge loops)
    int pc_map_count;
    struct {
        uint32_t guest_pc;
        size_t   host_offset;
    } pc_map[MAX_BLOCK_INSTS];

    // Back-edge detection (from prescan): prevents stale pending write flushes
    bool has_backedge;
    uint32_t backedge_target_pc;
    uint32_t backedge_targets[MAX_BLOCK_INSTS];
    int backedge_target_count;
    uint32_t loop_written_regs; // Bitmask of registers written within the detected loop

    // Dead temporary elimination: pending write tracker
    struct {
        uint8_t guest_reg;    // Which guest register
        x64_reg_t host_reg;   // Which x64 reg holds the value (RAX)
        bool valid;            // Is there a pending write?
        bool can_skip_store;   // Level 2: true if store can be skipped entirely
    } pending_write;

    // Prescan liveness: can_skip_store[i] for each instruction
    bool dead_temp_skip[MAX_BLOCK_INSTS];

    // Current instruction index (for dead_temp_skip lookup)
    int current_inst_idx;

    // Constant propagation: track registers with known values at translate time
    struct {
        bool valid;
        uint32_t value;
    } reg_constants[32];

    // Bounds check elimination: track validated address ranges per base register
    #define MAX_VALIDATED_RANGES 8
    struct {
        uint8_t guest_reg;   // Base guest register (0 = unused)
        uint32_t lo_offset;  // Lowest validated offset (base + lo_offset)
        uint32_t hi_end;     // Highest validated end (base + hi_end), exclusive
        bool is_store_ok;    // W^X has been checked for this range
    } validated_ranges[MAX_VALIDATED_RANGES];
    int validated_range_count;

    // Compare-Branch fusion state
    struct {
        bool valid;
        uint8_t opcode;       // Original comparison opcode (SLT, SEQ, etc.)
        uint8_t rd;           // Destination register of the comparison
        uint8_t rs1, rs2;     // Operands of the comparison
        bool rs2_is_imm;      // True if rs2 is an immediate (for SLTI, etc.)
        int32_t imm;          // Immediate value if rs2_is_imm is true
        int inst_idx;         // Instruction index (for dead_temp_skip lookup)
    } pending_cond;

    // Out-of-line side exit stubs (deferred to end of block)
    int deferred_exit_count;
    struct {
        size_t jmp_patch_offset;    // offset of rel32 in the inline jmp to cold stub
        uint32_t target_pc;         // guest PC for this side exit
        int exit_idx;               // exit index for chaining
        uint32_t branch_pc;         // guest PC of the branch instruction
        // Snapshot of dirty bits at the point of the branch (for flushing in cold stub)
        bool dirty_snapshot[REG_ALLOC_SLOTS];
    } deferred_exits[MAX_BLOCK_EXITS];
} translate_ctx_t;

// Translated block function signature
// Called with rbp = &cpu, r14 = cpu->mem_base
// Returns via 'ret' with exit_reason set in cpu->exit_reason
typedef void (*translated_block_fn)(void);

// Initialize translation context (Stage 1 mode - no caching)
void translate_init(translate_ctx_t *ctx, dbt_cpu_state_t *cpu);

// Initialize translation context with cache (Stage 2 mode)
void translate_init_cached(translate_ctx_t *ctx, dbt_cpu_state_t *cpu, block_cache_t *cache);

// Translate a basic block starting at cpu->pc (Stage 1)
// Returns pointer to executable code
translated_block_fn translate_block(translate_ctx_t *ctx);

// Translate and cache a block (Stage 2)
// Returns the translated_block_t with host code and exit info
translated_block_t *translate_block_cached(translate_ctx_t *ctx, uint32_t guest_pc);

// ============================================================================
// SLOW-32 opcode definitions (from slow32.h)
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

// Decoded instruction
typedef struct {
    uint32_t raw;
    uint8_t opcode;
    uint8_t rd;
    uint8_t rs1;
    uint8_t rs2;
    int32_t imm;
    inst_format_t format;
} decoded_inst_t;

// Decode a raw instruction word
decoded_inst_t decode_instruction(uint32_t raw);

// ============================================================================
// Individual instruction translators
// ============================================================================

// Arithmetic
void translate_add(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_sub(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_addi(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);

// Logical
void translate_and(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_or(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_xor(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_andi(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);
void translate_ori(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);
void translate_xori(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);

// Shifts
void translate_sll(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_srl(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_sra(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_slli(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);
void translate_srli(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);
void translate_srai(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);

// Comparisons
void translate_slt(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_sltu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_seq(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_sne(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_sgt(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_sgtu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_sle(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_sleu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_sge(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_sgeu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_slti(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);
void translate_sltiu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);

// Multiply/divide
void translate_mul(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_mulh(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_div(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);
void translate_rem(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, uint8_t rs2);

// Upper immediate
void translate_lui(translate_ctx_t *ctx, uint8_t rd, int32_t imm);

// Memory
void translate_ldw(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);
void translate_ldh(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);
void translate_ldb(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);
void translate_ldhu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);
void translate_ldbu(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);
void translate_stw(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm);
void translate_sth(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm);
void translate_stb(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm);

// Control flow
void translate_jal(translate_ctx_t *ctx, uint8_t rd, int32_t imm);
void translate_jalr(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm);
// Branch functions return true if block ends, false if superblock continues
bool translate_beq(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm);
bool translate_bne(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm);
bool translate_blt(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm);
bool translate_bge(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm);
bool translate_bltu(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm);
bool translate_bgeu(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2, int32_t imm);

// Floating-point (all use helper-call fallback for now)
void translate_fp_r_type(translate_ctx_t *ctx, uint8_t opcode, uint8_t rd, uint8_t rs1, uint8_t rs2);

// Special
void translate_nop(translate_ctx_t *ctx);
void translate_halt(translate_ctx_t *ctx);
void translate_debug(translate_ctx_t *ctx, uint8_t rs1);
void translate_yield(translate_ctx_t *ctx);
void translate_assert_eq(translate_ctx_t *ctx, uint8_t rs1, uint8_t rs2);

// ============================================================================
// Helper functions
// ============================================================================

// Load guest register into x64 register (handles r0 = 0)
void emit_load_guest_reg(translate_ctx_t *ctx, x64_reg_t dst, uint8_t guest_reg);

// Store x64 register to guest register (handles r0 = discard)
void emit_store_guest_reg(translate_ctx_t *ctx, uint8_t guest_reg, x64_reg_t src);

// Store immediate to guest register (handles r0 = discard)
void emit_store_guest_reg_imm32(translate_ctx_t *ctx, uint8_t guest_reg, uint32_t imm);

// Emit trace control (for mapping codegen to host bytes)
void dbt_set_emit_trace(bool enabled, uint32_t pc);

// Emit exit sequence (set exit_reason and return)
void emit_exit(translate_ctx_t *ctx, exit_reason_t reason, uint32_t next_pc);

// Emit exit with info value (e.g., for DEBUG character)
void emit_exit_with_info(translate_ctx_t *ctx, exit_reason_t reason, uint32_t next_pc, uint32_t info);

#endif // DBT_TRANSLATE_H
