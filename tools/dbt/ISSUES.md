# SLOW-32 DBT Emulator — Issues & Recommendations

This document tracks bugs, architectural limitations, and opportunities for improvement in the SLOW-32 Dynamic Binary Translator (`slow32-dbt`).

## Critical Bugs & Safety Issues

### 1. Out-of-Bounds Register Access for `f64` (Resolved)
The DBT's `load_f64_pair`/`store_f64_pair` and `load_u64_pair`/`store_u64_pair` helper functions access `regs[r]` and `regs[r+1]`.
- **Status**: Fixed in `64bb59b`. All register-pair access paths in `dbt_fp_helper` (including `FNEG.D`, `FABS.D`, and all 64-bit integer conversions) now use validated helper functions that check if the register index is even and < 31.

### 2. Unchecked Memory Allocations
Host-side allocations in `dbt_cpu_init`, `dbt_init_mmio`, and `translate_block` are often unchecked or only partially checked.
- **Problem**: `malloc`, `calloc`, and `mmap` failures can lead to null pointer dereferences or undefined behavior.
- **Recommendation**: Audit all allocation sites and ensure robust error handling (e.g., `exit(1)` or clean shutdown).

### 3. `dbt_write_callback` Bounds Check Overflow Risk
The check `if (size > cpu->mem_size || addr > cpu->mem_size - size)` protects against basic overflow.
- **Problem**: While correct, it's called for every section load. A large `size` close to `UINT32_MAX` could theoretically wrap in the subtraction if not careful (though `addr > ...` usually handles it).
- **Recommendation**: Use `addr + size < addr` or cast to `uint64_t` for robust overflow detection.

### 4. `yield_spin_count` False Positives
The `dbt_handle_yield` function detects a spin loop if `req_head` and `req_tail` don't change for 3 consecutive yields.
- **Problem**: Legitimate polling loops (e.g., waiting for console input or a timer) might yield frequently without new requests, triggering the "spin detected" warning incorrectly.
- **Recommendation**: Increase the threshold or only warn if `total_requests` hasn't changed over a longer period.

### 5. `lookup_table` Indexing Risk
The generated code masks the PC hash with `lookup_mask`.
- **Problem**: If `lookup_mask` is not a power of 2 minus 1, the masking logic fails. `block_cache_init` ensures power-of-2 size, but defensive programming would assert this relationship.
- **Recommendation**: Add a runtime assertion that `(size & (size - 1)) == 0`.

---

## Architectural Limitations & Performance

### 6. Single-Page `mmap` for Code Buffer
`dbt_cpu_init` allocates a fixed `STAGE1_CODE_BUFFER_SIZE` (64KB).
- **Problem**: For large programs or long-running sessions with many unique code paths, this buffer will fill up. There is no logic to flush the cache or grow the buffer.
- **Recommendation**: Implement a code buffer management strategy (e.g., ring buffer, flush-on-full, or `mremap` growth).

### 7. Global `mmio_state` Limits Concurrency
`mmio_state` is a static global variable in `dbt.c`.
- **Problem**: This prevents running multiple DBT instances in the same process (e.g., for testing or threaded simulation).
- **Recommendation**: Move `mmio_state` into `dbt_cpu_state_t`.

### 8. `rdtsc` Portability
The profiling code uses inline assembly `rdtsc`, which is x86-specific.
- **Problem**: The DBT won't compile on ARM64 (e.g., Apple Silicon) or other hosts.
- **Recommendation**: Use `clock_gettime` or compiler intrinsics for portable high-resolution timing.

---

## Quality of Life

### 9. Hardcoded Guest Memory Size
`GUEST_MEM_SIZE` is fixed at 256MB.
- **Problem**: This limits the simulation of smaller or larger systems and wastes host memory for small programs.
- **Recommendation**: Allow configuration via command-line flags, similar to the linker's `--mem-size`.

### 10. `math_intercepts` Table Maintenance
The table manually maps string names to host function pointers.
- **Problem**: Adding new intrinsics requires manual updates to this table, `slow32-tcg.c`, and potentially other places.
- **Recommendation**: Centralize intrinsic definitions in a shared header or generate them from a macro list.

---

## Latent Correctness Bug (from RV32IM sister project)

### 11. Self-Loop Back-Edge Register Mapping Corruption

**Severity**: Data corruption — silently produces wrong results.

**File**: `stage5_codegen.c`, function `cg_emit_self_loop_branch()` (line 204).

**Summary**: The self-loop optimization emits a back-edge that jumps directly to the loop body, bypassing the prologue register reload. It builds a "shuffle plan" to move registers to their expected entry slots. The bail-out guard at line 277 (`if (shuffle_count > 6) return false`) only checks shuffle **operations**, not total **distinct guest registers** used in the loop body. When a loop uses more guest registers than `STAGE5_RA_HOST_SLOTS` (8), the register allocator spills and repurposes slots during the block. The shuffle plan accounts for the final slot state but may not correctly handle all the intermediate slot reuse that occurred — particularly when a guest register was evicted mid-block, its slot reused for another guest register, and the shuffle plan must now reconstruct the original entry mapping from a state that has diverged in ways the slot-tracking can't fully represent.

**Trigger pattern**: GCC `-O2` struct copy loops. A 260-byte struct copy (e.g., dBASE's `value_t`: 4-byte type + 256-byte union) gets unrolled into a loop copying 24 bytes/iteration with 6 LW + 6 SW, using 9 distinct guest registers:
- 6 data temporaries (t3, t1, a7, a6, a0, a1)
- 2 pointer registers (source, dest)
- 1 end-of-range marker

With only 8 host register slots, the 9th register forces an eviction. The loop appears to work but silently truncates the copy.

**Observed symptom**: Large struct copies corrupted when compiled at `-O2`. The identical binary runs correctly in the interpreter. Compiling at `-Os` (which calls `memcpy` instead of inlining the loop) works around it.

**How this was found and fixed in the sister project**: The RV32IM DBT at `~/riscv/` had an equivalent bug in its simpler LRU register cache. The self-loop pre-scan collected source registers used in the loop body. When the count exceeded `RC_NUM_SLOTS` (8), LRU evictions during translation reshuffled the guest-to-host mapping. The back-edge jumped to `warm_entry` expecting the original mapping, but host registers now held wrong guest values.

**The RV32IM fix** (commit `a5ab60e` in `~/riscv/`): After the self-loop pre-scan, count distinct source registers. If `nused > RC_NUM_SLOTS`, disable the self-loop optimization entirely and fall back to normal flush-and-exit at the back-edge. The relevant code is in `~/riscv/dbt/dbt.c` around line 1092:

```c
if (self_loop) {
    int nused = 0;
    for (int r = 1; r < 32; r++)
        if (used[r]) nused++;
    if (past_first_branch || nused > RC_NUM_SLOTS)
        self_loop = 0;
}
```

**Recommended fix for SLOW-32**: The SLOW-32 codegen is SSA-based and more sophisticated than the RV32IM LRU cache, so the fix needs to be adapted. In `cg_emit_self_loop_branch()`, before or alongside the `shuffle_count > 6` check, count the total distinct guest registers that appear in `entry_gpr_for_slot[]` plus any guest registers that were allocated to slots during the block but are NOT in the entry mapping. If that total exceeds `STAGE5_RA_HOST_SLOTS`, return false (bail out to normal two-exit translation). The existing shuffle plan handles slot-to-slot moves and memory reloads, but it may not correctly reconstruct the entry state when heavy slot reuse has occurred.

**Reproduction**: Compile a C program with a 260-byte struct returned by value, with chained `identity(identity(make_str(buf)))` calls, at `-O2`. The test program `~/riscv/examples/test_struct_copy.c` demonstrates the pattern and can be adapted for SLOW-32's toolchain.
