#ifndef SLOW32_H
#define SLOW32_H

#include <stdint.h>
#include <stdbool.h>
#include "memory_manager.h"
#include "mmio_ring.h"

#define NUM_REGS 32
#define DEFAULT_MEM_SIZE (256 * 1024 * 1024)  // Default 256MB to match architecture spec
#define MAX_MEM_SIZE (256 * 1024 * 1024)      // Maximum 256MB
#define CODE_SIZE (1 * 1024 * 1024)           // First 1MB is code (execute-only)
#define DATA_START CODE_SIZE                   // Data starts after code

#define REG_ZERO 0
#define REG_SP   29
#define REG_FP   30
#define REG_LR   31

#define MMIO_BASE     0x10000000
#define REQ_HEAD      (MMIO_BASE + 0x0000)
#define REQ_TAIL      (MMIO_BASE + 0x0004)
#define REQ_RING      (MMIO_BASE + 0x1000)
#define RESP_HEAD     (MMIO_BASE + 0x2000)
#define RESP_TAIL     (MMIO_BASE + 0x2004)
#define RESP_RING     (MMIO_BASE + 0x3000)

typedef enum {
    OP_ADD    = 0x00,
    OP_SUB    = 0x01,
    OP_XOR    = 0x02,
    OP_OR     = 0x03,
    OP_AND    = 0x04,
    OP_SLL    = 0x05,
    OP_SRL    = 0x06,
    OP_SRA    = 0x07,
    OP_SLT    = 0x08,  // Set less than (signed)
    OP_SLTU   = 0x09,  // Set less than (unsigned)
    OP_MUL    = 0x0A,
    OP_MULH   = 0x0B,
    OP_DIV    = 0x0C,
    OP_REM    = 0x0D,
    OP_SEQ    = 0x0E,  // Set equal (rd = rs1 == rs2 ? 1 : 0)
    OP_SNE    = 0x0F,  // Set not equal (rd = rs1 != rs2 ? 1 : 0)
    
    OP_ADDI   = 0x10,
    OP_ORI    = 0x11,
    OP_ANDI   = 0x12,
    OP_SLLI   = 0x13,
    OP_SRLI   = 0x14,
    OP_SRAI   = 0x15,
    OP_SLTI   = 0x16,
    OP_SLTIU  = 0x17,
    OP_SGT    = 0x18,  // Set greater than (signed)
    OP_SGTU   = 0x19,  // Set greater than (unsigned)
    OP_SLE    = 0x1A,  // Set less or equal (signed)
    OP_SLEU   = 0x1B,  // Set less or equal (unsigned)
    OP_SGE    = 0x1C,  // Set greater or equal (signed)
    OP_SGEU   = 0x1D,  // Set greater or equal (unsigned)
    OP_XORI   = 0x1E,  // XOR immediate
    
    OP_LUI    = 0x20,
    
    OP_LDB    = 0x30,
    OP_LDH    = 0x31,
    OP_LDW    = 0x32,
    OP_LDBU   = 0x33,
    OP_LDHU   = 0x34,
    
    OP_STB    = 0x38,
    OP_STH    = 0x39,
    OP_STW    = 0x3A,
    
    OP_ASSERT_EQ = 0x3F,  // Test instruction for self-checking
    
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
    OP_DEBUG  = 0x52,  // Debug output: print char in rs1
    OP_HALT   = 0x7F,
} opcode_t;

typedef enum {
    FMT_R,   // Register format: op rd, rs1, rs2
    FMT_I,   // Immediate format: op rd, rs1, imm12
    FMT_S,   // Store format: op rs1, rs2, imm12
    FMT_B,   // Branch format: op rs1, rs2, imm12
    FMT_U,   // Upper immediate: op rd, imm20
    FMT_J,   // Jump format: op rd, imm20
} inst_format_t;

typedef struct {
    uint32_t raw;
    opcode_t opcode;
    inst_format_t format;
    uint8_t rd;
    uint8_t rs1;
    uint8_t rs2;
    int32_t imm;
} instruction_t;

typedef struct cpu_state {
    uint32_t regs[NUM_REGS];
    uint32_t pc;
    memory_manager_t mm;    // Memory manager instead of raw pointer
    memory_region_t *code_region; // Cached pointer to code region for fast fetch
    uint64_t cycle_count;
    uint64_t inst_count;    // Instructions executed
    bool halted;
    
    // Memory protection limits (from .s32x executable)
    uint32_t code_limit;    // End of execute-only region
    uint32_t rodata_limit;  // End of read-only region
    uint32_t data_limit;    // End of writable region
    bool wxorx_enabled;     // W^X protection enabled
    
    // MMIO support (auto-detected)
    bool mmio_enabled;      // MMIO detected/enabled
    bool mmio_initialized;  // MMIO actually set up
    mmio_ring_state_t *mmio; // MMIO state (allocated on demand)
    void *mmio_mem;         // Host pointer to MMIO memory
    
    struct {
        bool enabled;
        bool step_mode;
        bool trace_instructions;      // Print each instruction as it executes
        bool show_reg_changes;        // Show register changes
        uint64_t max_cycles;          // Maximum cycles before stopping (0 = unlimited)
        uint32_t breakpoints[16];
        int num_breakpoints;
        uint32_t watch_start;         // Memory watch region start
        uint32_t watch_end;           // Memory watch region end
        bool watch_enabled;           // Memory watch active
        uint32_t prev_regs[NUM_REGS]; // Previous register values for change detection
    } debug;
} cpu_state_t;

void cpu_init(cpu_state_t *cpu);
void cpu_destroy(cpu_state_t *cpu);
bool cpu_load_binary(cpu_state_t *cpu, const char *filename);
void cpu_reset(cpu_state_t *cpu, uint32_t entry_point);
void cpu_step(cpu_state_t *cpu);
void cpu_run(cpu_state_t *cpu);
void cpu_dump_regs(cpu_state_t *cpu);
void cpu_dump_mem(cpu_state_t *cpu, uint32_t addr, uint32_t size);

#endif