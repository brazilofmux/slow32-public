// SLOW-32 DBT: Paranoid Mode — Lockstep Shadow Interpreter
// Embeds a pure-C interpreter that runs in lockstep with the DBT.
// After each block executes via native code, the shadow re-executes the same
// guest instructions and compares results.  First divergence is reported with
// full diagnostics.

#ifndef DBT_SHADOW_INTERP_H
#define DBT_SHADOW_INTERP_H

#include <stdint.h>
#include <stdbool.h>
#include "cpu_state.h"
#include "block_cache.h"

// Shadow store buffer: captures stores made by the shadow interpreter
// so we don't write to real guest memory.  Loads check the buffer first
// (store forwarding) before falling back to real memory.
#define SHADOW_STORE_BUF_SIZE 65536

typedef struct {
    uint32_t addr;
    uint8_t  value;     // byte-granularity
} shadow_store_entry_t;

typedef struct {
    // Shadow register file and PC
    uint32_t regs[32];
    uint32_t pc;

    // Pre-block snapshot (taken before DBT executes)
    uint32_t snap_regs[32];
    uint32_t snap_pc;

    // Shadow store buffer (intra-block stores)
    shadow_store_entry_t store_buf[SHADOW_STORE_BUF_SIZE];
    int store_buf_count;

    // Guest memory pointer (read-only reference to real memory)
    uint8_t *mem_base;
    uint32_t mem_size;

    // Block-ending flags set by shadow step
    bool hit_debug;
    bool hit_yield;
    bool hit_halt;
    bool hit_assert_fail;
    uint32_t debug_char;    // character from DEBUG instruction

    // Statistics
    uint64_t blocks_verified;
    uint64_t blocks_skipped;    // intrinsic/intercept blocks
    uint64_t instructions_verified;

    // Configuration
    bool enabled;
    bool verbose;           // print per-block status
    bool check_memory;      // compare store buffer vs real memory at block end
    uint32_t pc_filter;     // only check blocks starting at this PC (0 = all)
    uint64_t skip_count;    // skip first N blocks before checking
    uint64_t skip_remaining;

    // Intrinsic addresses to skip (copied from cpu state)
    uint32_t intrinsic_memcpy;
    uint32_t intrinsic_memset;
    uint32_t intrinsic_memmove;
    uint32_t intrinsic_strlen;
    uint32_t intrinsic_memswap;

    // Math intercept addresses to skip
    uint32_t intercept_addrs[MAX_INTERCEPTS];
    int num_intercepts;
} shadow_state_t;

// Global paranoid mode flag (checked by block_cache.c to disable chaining)
extern bool paranoid_mode;
// Global debug flag: force stage2+ exits through dispatcher (no direct chaining).
extern bool dbt_no_chain;

// Initialize the shadow state.  Call after dbt_cpu_init + dbt_load_s32x.
void shadow_init(shadow_state_t *s, dbt_cpu_state_t *cpu);

// Snapshot current CPU state before block execution.
void shadow_snapshot(shadow_state_t *s, dbt_cpu_state_t *cpu);

// Pre-execute: run the shadow interpreter from snapshot state BEFORE the DBT
// executes.  Must be called while guest memory is still in pre-block state.
// Stores the shadow's final registers/PC/store-buffer for later comparison.
void shadow_pre_execute(shadow_state_t *s, translated_block_t *block);

// Verify: compare shadow results (from pre_execute) with DBT results.
// Returns true if OK, false (and prints diagnostics) on divergence.
// On divergence, aborts the process.
bool shadow_verify(shadow_state_t *s, dbt_cpu_state_t *cpu,
                   translated_block_t *block, uint64_t exec_num);

// Print summary statistics.
void shadow_print_stats(shadow_state_t *s);

#endif // DBT_SHADOW_INTERP_H
