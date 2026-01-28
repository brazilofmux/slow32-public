// SLOW-32 DBT: Guest CPU State
// Stage 1 - Correctness-first dynamic binary translation

#ifndef DBT_CPU_STATE_H
#define DBT_CPU_STATE_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

// Exit reasons (set by translated code before returning to dispatcher)
typedef enum {
    EXIT_BRANCH         = 0,    // Normal branch, next_pc in cpu->pc
    EXIT_INDIRECT       = 1,    // Indirect branch (JALR), next_pc in cpu->pc
    EXIT_HALT           = 2,    // HALT instruction
    EXIT_DEBUG          = 3,    // DEBUG instruction (needs host I/O)
    EXIT_YIELD          = 4,    // YIELD instruction (MMIO)
    EXIT_FAULT_FETCH    = 5,    // Instruction fetch fault
    EXIT_FAULT_LOAD     = 6,    // Load fault
    EXIT_FAULT_STORE    = 7,    // Store fault
    EXIT_ASSERT_FAIL    = 8,    // ASSERT_EQ failed
    EXIT_BLOCK_END      = 9,    // Reached max instructions in block
} exit_reason_t;

// Guest CPU state structure
// Translated code accesses this via a base pointer in rbp
typedef struct {
    // Guest registers (r0 is always 0, enforced by generated code)
    // Offset: 0x00
    uint32_t regs[32];

    // Program counter
    // Offset: 0x80
    uint32_t pc;

    // Exit information (set by translated code before returning)
    // Offset: 0x84
    uint32_t exit_reason;
    // Offset: 0x88
    uint32_t exit_info;     // e.g., fault address, DEBUG char

    // Padding for alignment
    uint32_t _pad0;

    // Cycle counter (unused in DBT)
    // Offset: 0x90
    uint64_t cycle_count;

    // Halt flag
    // Offset: 0x98
    bool halted;
    uint8_t _pad1[7];

    // Guest memory (simple flat model for Stage 1)
    // Offset: 0xA8
    uint8_t *mem_base;
    // Offset: 0xB0
    uint32_t mem_size;

    // Code buffer for translated blocks
    // Offset: 0xB8
    uint8_t *code_buffer;
    // Offset: 0xC0
    size_t code_buffer_size;
    // Offset: 0xC8
    size_t code_buffer_used;

    // Memory region limits (from s32x header)
    // Offset: 0xD0
    uint32_t code_limit;
    uint32_t rodata_limit;
    uint32_t data_limit;
    uint32_t stack_base;

    // MMIO
    // Offset: 0xE0
    uint32_t mmio_base;
    bool mmio_enabled;
    bool wxorx_enabled;
    bool align_traps_enabled;
    uint8_t _pad2[1];

    // Stage 3: Inline indirect branch lookup
    // Offset: 0xE8
    void *lookup_table;         // Pointer to block_cache->blocks array
    uint32_t lookup_mask;       // BLOCK_CACHE_SIZE - 1
    uint32_t _pad3;

    // Stage 3 Phase 2: Return Address Stack (RAS)
    // Offset: 0xF0
    #define RAS_SIZE 32         // Power of 2 for easy masking
    #define RAS_MASK (RAS_SIZE - 1)
    uint32_t ras_stack[RAS_SIZE];   // Predicted return guest PCs
    uint32_t ras_top;               // Stack top index (0 to RAS_SIZE-1)

    // Stage 4 profiling: superblock side exits taken
    // Offset: 0x174
    uint32_t side_exit_taken;
    uint32_t _pad4;

} dbt_cpu_state_t;

// Offset macros for generated code
#define CPU_REGS_OFFSET         offsetof(dbt_cpu_state_t, regs)
#define CPU_PC_OFFSET           offsetof(dbt_cpu_state_t, pc)
#define CPU_EXIT_REASON_OFFSET  offsetof(dbt_cpu_state_t, exit_reason)
#define CPU_EXIT_INFO_OFFSET    offsetof(dbt_cpu_state_t, exit_info)
#define CPU_HALTED_OFFSET       offsetof(dbt_cpu_state_t, halted)
#define CPU_MEM_BASE_OFFSET     offsetof(dbt_cpu_state_t, mem_base)
#define CPU_MEM_SIZE_OFFSET     offsetof(dbt_cpu_state_t, mem_size)
#define CPU_CODE_LIMIT_OFFSET   offsetof(dbt_cpu_state_t, code_limit)
#define CPU_RODATA_LIMIT_OFFSET offsetof(dbt_cpu_state_t, rodata_limit)
#define CPU_MMIO_BASE_OFFSET    offsetof(dbt_cpu_state_t, mmio_base)
#define CPU_MMIO_ENABLED_OFFSET offsetof(dbt_cpu_state_t, mmio_enabled)
#define CPU_WXORX_ENABLED_OFFSET offsetof(dbt_cpu_state_t, wxorx_enabled)
#define CPU_ALIGN_TRAPS_OFFSET  offsetof(dbt_cpu_state_t, align_traps_enabled)
#define CPU_LOOKUP_TABLE_OFFSET offsetof(dbt_cpu_state_t, lookup_table)
#define CPU_LOOKUP_MASK_OFFSET  offsetof(dbt_cpu_state_t, lookup_mask)
#define CPU_RAS_STACK_OFFSET    offsetof(dbt_cpu_state_t, ras_stack)
#define CPU_RAS_TOP_OFFSET      offsetof(dbt_cpu_state_t, ras_top)
#define CPU_SIDE_EXIT_TAKEN_OFFSET offsetof(dbt_cpu_state_t, side_exit_taken)

// Helper to get guest register offset
#define GUEST_REG_OFFSET(n)     (CPU_REGS_OFFSET + (n) * 4)

// Register indices (for clarity in code)
#define REG_ZERO    0
#define REG_RV      1   // Return value
#define REG_T0      2   // Temporary
#define REG_A0      3   // First argument
#define REG_A1      4
#define REG_A2      5
#define REG_A3      6
#define REG_A4      7
#define REG_A5      8
#define REG_A6      9
#define REG_A7      10
#define REG_SP      29  // Stack pointer
#define REG_FP      30  // Frame pointer
#define REG_LR      31  // Link register

// Initialize CPU state
void dbt_cpu_init(dbt_cpu_state_t *cpu);

// Clean up CPU state
void dbt_cpu_destroy(dbt_cpu_state_t *cpu);

// Load s32x executable
bool dbt_load_s32x(dbt_cpu_state_t *cpu, const char *filename);

#endif // DBT_CPU_STATE_H
