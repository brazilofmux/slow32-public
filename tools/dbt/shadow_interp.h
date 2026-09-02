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
// Open-addressing index over the buffer (2x entries). Generation-stamped so
// it never needs clearing between executions.
#define SHADOW_STORE_HASH_SIZE (SHADOW_STORE_BUF_SIZE * 2)
#define SHADOW_STORE_HASH_MASK (SHADOW_STORE_HASH_SIZE - 1)

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

    // O(1) addr -> buffer-index map: entry = (generation << 20) | (idx + 1).
    // An entry whose generation doesn't match store_gen is an empty slot.
    uint64_t store_hash[SHADOW_STORE_HASH_SIZE];
    uint64_t store_gen;

    // Guest memory pointer (read-only reference to real memory)
    uint8_t *mem_base;
    uint32_t mem_size;
    uint32_t code_limit;
    uint32_t rodata_limit;
    uint32_t mmio_base;
    bool mmio_enabled;
    bool wxorx_enabled;
    bool align_traps_enabled;
    bool bounds_checks_disabled;

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

    // Paranoid-lite bookkeeping
    bool store_buf_overflow;     // stores exceeded buffer this execution
    bool lite_skip_this;         // pre-execute couldn't follow: skip verify
    uint64_t lite_budget;        // max shadow steps per dispatch
    uint64_t lite_budget_skips;  // executions skipped: step budget exhausted
    uint64_t lite_mem_skips;     // memory compares skipped: buffer overflow
    uint64_t lite_nofootprint_skips; // blocks without a footprint (stage<4 etc.)
    // When the shadow falls sequentially out of the footprint onto plain
    // `jal r0` jumps the DBT elided (jump-over inlining), it records each
    // jump's PC here while following the chain; verify accepts the DBT
    // stopping at any of them (pure jumps change nothing but the PC).
    uint32_t lite_jump_chain[8];
    int lite_jump_chain_n;
    // Lite runs the shadow REGISTER FILE continuously: cpu->regs is only
    // copied in when the shadow couldn't follow (intrinsic block, skip,
    // budget, PC desync). Re-snapshotting every dispatch would launder
    // DBT register corruption into the shadow (dead-temp writeback skips
    // force register-only mismatches to be soft), hiding exactly the bug
    // class this mode exists to catch: with a continuous shadow, corrupt
    // registers propagate into a hard PC/memory divergence instead.
    bool lite_synced;
    uint64_t lite_resyncs;

    // Configuration
    bool enabled;
    bool verbose;           // print per-block status
    bool check_memory;      // compare store buffer vs real memory at block end
    uint32_t pc_filter;     // only check blocks starting at this PC (0 = all)
    uint64_t skip_count;    // skip first N blocks before checking
    uint64_t skip_remaining;

    // Follow taken backward conditional branches whose target lies inside the
    // block, mirroring the translator's in-block back-edge fast path (emitted
    // only when the register cache is enabled). Without this the shadow does
    // one linear pass while the DBT runs the whole loop in-block, and every
    // loop block reports a false PC/register divergence.
    bool follow_backedges;
    bool chase_abort;       // step budget exhausted mid-loop; skip this verify

    // Intrinsic addresses to skip (copied from cpu state)
    uint32_t intrinsic_memcpy;
    uint32_t intrinsic_memset;
    uint32_t intrinsic_memmove;
    uint32_t intrinsic_strlen;
    uint32_t intrinsic_memswap;
    uint32_t intrinsic_memcmp;

    // Math intercept addresses to skip
    uint32_t intercept_addrs[MAX_INTERCEPTS];
    int num_intercepts;
} shadow_state_t;

// Global paranoid mode flag (checked by block_cache.c to disable chaining)
extern bool paranoid_mode;
// Paranoid-lite: verify the PRODUCTION translation (superblocks, reg cache,
// peephole all ON) instead of --paranoid's de-optimized one. Chaining is
// disabled for per-dispatch granularity; the shadow follows each block's
// exact guest-PC footprint (translated_block_t.lite_pcs), so it can track
// jump-over inlining and in-block back-edge loops.
extern bool paranoid_lite_mode;
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

// Paranoid-lite pre-execute: like shadow_pre_execute, but follows the block's
// exact guest-PC footprint (block->lite_pcs) instead of a linear range, so it
// can track Stage-4 superblock inlining and in-block back-edge loops.
void shadow_lite_pre_execute(shadow_state_t *s, translated_block_t *block);

// Record a block's guest-PC footprint for paranoid-lite (sorted copy is
// attached to the block). No-op unless paranoid_lite_mode is set.
void shadow_lite_attach_footprint(translated_block_t *block,
                                  const uint32_t *pcs, int count);

// Verify: compare shadow results (from pre_execute) with DBT results.
// Returns true if OK, false (and prints diagnostics) on divergence.
// On divergence, aborts the process.
bool shadow_verify(shadow_state_t *s, dbt_cpu_state_t *cpu,
                   translated_block_t *block, uint64_t exec_num);

// Print summary statistics.
void shadow_print_stats(shadow_state_t *s);

#endif // DBT_SHADOW_INTERP_H
