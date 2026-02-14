# SLOW-32 DBT Stage 4: Trace Compilation

## Overview

Stages 1-3 established a working DBT with block caching, direct chaining, and indirect branch optimization. Stage 4 explores **trace compilation** - building longer sequences of translated code that follow hot execution paths across multiple basic blocks.

**Current State (Stage 3):**

- Translation unit: Single basic block
- Block boundaries: Every branch ends a block
- Chaining: Direct jumps patched between blocks
- Overhead: Block transitions still have some cost

**Stage 4 Goal:**

- Translation unit: Traces spanning multiple basic blocks
- Fewer transitions, better code locality, more optimization opportunity

---

## What is a Trace?

A **trace** is a linear sequence of instructions representing one path through the program. Unlike basic blocks, traces can:

1. **Cross conditional branches** - Follow the "likely" path, emit side exits for unlikely paths
2. **Inline function calls** - Expand small callees into the caller's trace
3. **Span loop iterations** - Unroll hot loops or create self-looping traces

### Example: Basic Blocks vs Trace

```
Guest code:
    loop:
        add  r1, r1, r2      ; BB1 starts
        blt  r1, r3, loop    ; BB1 ends (conditional)
        add  r4, r1, r0      ; BB2 starts (fall-through)
        ret                  ; BB2 ends
```

**Stage 3 (Basic Blocks):**
```
BB1: add r1,r1,r2 → blt → [exit to dispatcher, check target]
BB2: add r4,r1,r0 → ret → [exit to dispatcher]

Chained: BB1 branches to BB1 or BB2 directly
But still: Each iteration = BB1 translation overhead
```

**Stage 4 (Trace):**
```
Trace (loop unrolled 4x):
    add r1,r1,r2
    cmp r1,r3; jge side_exit_1  ; iteration 1
    add r1,r1,r2
    cmp r1,r3; jge side_exit_2  ; iteration 2
    add r1,r1,r2
    cmp r1,r3; jge side_exit_3  ; iteration 3
    add r1,r1,r2
    cmp r1,r3; jge side_exit_4  ; iteration 4
    jmp trace_start             ; loop back

side_exit_N: ... exit to BB2 or dispatcher
```

---

## Trade-offs: Is Stage 4 Worth It?

### Arguments For Traces

1. **Fewer block transitions** - Hot loops stay in native code longer
2. **Cross-block optimization** - Eliminate redundant guest register loads
3. **Branch prediction** - x86 branch predictors work better on traces
4. **Code locality** - Hot path is contiguous in icache

### Arguments Against (for SLOW-32)

1. **Complexity** - Trace formation is significantly more complex than block translation
2. **Diminishing returns** - Stage 3 already handles most overhead (chaining, inline lookup)
3. **Small programs** - SLOW-32 test programs are tiny; traces help most on larger code
4. **Debug difficulty** - Harder to correlate native code to guest code
5. **Code bloat** - Traces are larger; may pressure code cache

### The Question

> Is the added complexity worth the performance gain for SLOW-32's use case?

Probably not for correctness testing. But traces are educationally valuable and could be useful if SLOW-32 ever runs larger programs.

---

## Trace Formation Approaches

### Approach 1: Superblocks (Simpler)

**Idea:** Extend basic blocks by following fall-through paths of conditional branches.

```
Original:          Superblock:
    BB1 → blt         BB1 code
      ↓     ↘         cmp; jl side_exit
    BB2     BB3       BB2 code (inlined)
                      ... continues until call/ret/indirect
```

**Pros:** Simple extension of current model
**Cons:** Limited gains; doesn't help loops much

### Approach 2: Trace Recording (Classic)

**Idea:** When a block becomes hot, start recording instructions as they execute until we see a backwards branch (loop) or exceed a limit.

```
1. Profile blocks (exec_count)
2. When block exceeds threshold: start trace recording
3. Execute guest code, record each instruction
4. Stop at: loop header, indirect branch, call depth exceeded, length limit
5. Compile recorded trace
```

**Pros:** Captures actual hot paths
**Cons:** Requires interpreter mode for recording, more complex

### Approach 3: Next-Executing-Tail (NET) Traces

**Idea:** Chain blocks greedily at translation time based on likely targets.

```
When translating BB1 that ends with conditional branch:
  - If fall-through BB2 is already translated, chain normally
  - If not, speculatively translate BB2 and embed it
  - Repeat until limit
```

**Pros:** No profiling needed
**Cons:** May trace cold paths

### Approach 4: Loop Detection

**Idea:** Detect loops in the guest CFG and create specialized loop traces.

```
1. Detect backedge (branch to earlier PC)
2. Analyze loop: single entry, single backedge?
3. If simple loop: unroll N iterations
4. Emit: unrolled body + exit checks + loop-back jump
```

**Pros:** Targets the highest-value optimization
**Cons:** Limited scope; only helps loops

---

## Recommended Approach for Stage 4

Given SLOW-32's goals, I recommend **Loop Detection + Simple Superblocks**:

### Phase 1: Superblock Extension

Extend blocks past conditional branches when the target is adjacent:

```c
// During translation, when we hit a conditional branch:
if (branch_target == current_pc + 4) {
    // Fall-through is likely path - extend the trace
    emit_branch_with_side_exit();
    continue translating fall-through;
}
```

### Phase 2: Loop Traces

Detect and optimize simple loops:

```c
typedef struct {
    uint32_t header_pc;      // Loop entry point
    uint32_t backedge_pc;    // Branch back to header
    uint32_t exit_pc;        // Fall-through exit
    uint32_t body_size;      // Instructions in loop body
} loop_info_t;

// Translate loop as self-contained unit with optional unrolling
```

### Phase 3: Cross-Block Register Optimization

Track which guest registers are "live" in host registers across block boundaries:

```c
typedef struct {
    uint8_t host_reg;        // Which x86 reg holds this
    uint8_t guest_reg;       // Which SLOW-32 reg it is
    bool dirty;              // Modified, needs writeback
} reg_binding_t;
```

This allows eliminating redundant loads like:
```
; Before (Stage 3)
mov eax, [rbp + r1_offset]   ; load r1
add eax, ecx
mov [rbp + r1_offset], eax   ; store r1
mov eax, [rbp + r1_offset]   ; load r1 AGAIN (next block)

; After (Stage 4)
mov eax, [rbp + r1_offset]   ; load r1 once
add eax, ecx
; keep r1 in eax, no store until needed
```

---

## Data Structures

### Trace Descriptor

```c
typedef struct {
    uint32_t entry_pc;           // Trace entry point (guest PC)
    uint8_t *host_code;          // Compiled trace
    uint32_t host_size;

    // Exit points (side exits)
    struct {
        uint32_t guest_pc;       // Where to go on side exit
        uint8_t *patch_site;     // Location of jmp rel32 in trace
    } exits[MAX_TRACE_EXITS];
    uint8_t exit_count;

    // Statistics
    uint64_t exec_count;
    uint64_t exit_counts[MAX_TRACE_EXITS];  // Per-exit counters
} trace_t;
```

### Loop Info

```c
typedef struct {
    uint32_t header_pc;
    uint32_t body_end_pc;        // Last instruction before backedge
    uint32_t trip_count_estimate; // If known
    bool single_exit;            // Can we fully optimize?
} loop_info_t;
```

---

## Implementation Sketch

### Phase 1: Superblock Extension

```c
// In translate_block_cached(), after translating a conditional branch:

void translate_bxx_extended(translate_ctx_t *ctx, ...) {
    uint32_t taken_pc = ctx->guest_pc + imm;
    uint32_t fall_pc = ctx->guest_pc + 4;

    // Check if fall-through is "likely" (heuristic: forward branches often not taken)
    bool extend_fall_through = (imm > 0);  // Branch is forward

    if (extend_fall_through && ctx->trace_depth < MAX_TRACE_DEPTH) {
        // Emit conditional jump to side exit
        emit_cmp_and_branch(ctx, rs1, rs2, taken_pc);  // side exit if taken

        // Continue translating fall-through inline
        ctx->trace_depth++;
        ctx->guest_pc = fall_pc;
        // ... continue main translation loop
    } else {
        // Normal Stage 3 behavior: end block here
        emit_conditional_exit(ctx, ...);
    }
}
```

### Phase 2: Loop Detection

```c
// Detect loop when we see a backward branch
bool detect_loop(translate_ctx_t *ctx, uint32_t branch_target) {
    if (branch_target >= ctx->block_start_pc) {
        return false;  // Not a backedge
    }

    // Simple heuristic: branch_target is our loop header
    // Current PC is the backedge location

    loop_info_t loop = {
        .header_pc = branch_target,
        .body_end_pc = ctx->guest_pc,
        .single_exit = true,  // Assume for now
    };

    return compile_loop_trace(ctx, &loop);
}
```

### Phase 3: Register Caching

```c
// Track register state across trace
typedef struct {
    int8_t host_for_guest[32];   // -1 = not cached, else x86 reg
    int8_t guest_in_host[16];    // -1 = free, else guest reg
    uint32_t dirty_mask;          // Which cached regs need writeback
} reg_state_t;

void emit_load_guest_reg_cached(translate_ctx_t *ctx, x64_reg_t dst, uint8_t guest) {
    if (ctx->reg_state.host_for_guest[guest] >= 0) {
        // Already in a host register - move or reuse
        x64_reg_t cached = ctx->reg_state.host_for_guest[guest];
        if (cached != dst) {
            emit_mov_r32_r32(&ctx->emit, dst, cached);
        }
    } else {
        // Load from memory
        emit_mov_r32_m32(&ctx->emit, dst, RBP, guest_reg_offset(guest));
        // Maybe cache it
        cache_guest_reg(ctx, guest, dst);
    }
}

void emit_trace_exit(translate_ctx_t *ctx, uint32_t target_pc) {
    // Writeback all dirty registers before exit
    for (int i = 0; i < 32; i++) {
        if (ctx->reg_state.dirty_mask & (1 << i)) {
            x64_reg_t host = ctx->reg_state.host_for_guest[i];
            emit_mov_m32_r32(&ctx->emit, RBP, guest_reg_offset(i), host);
        }
    }
    ctx->reg_state.dirty_mask = 0;

    emit_exit(ctx, EXIT_BRANCH, target_pc);
}
```

---

## Side Exits

A key concept in trace compilation is the **side exit** - when execution deviates from the predicted trace path.

```
Trace for path A→B→C:
    ; A code
    cmp ...; jne side_exit_1   ; if not B, exit
    ; B code
    cmp ...; jne side_exit_2   ; if not C, exit
    ; C code
    jmp next_trace_or_dispatch

side_exit_1:
    writeback dirty regs
    mov [cpu->pc], actual_target
    ret  ; back to dispatcher

side_exit_2:
    writeback dirty regs
    mov [cpu->pc], actual_target
    ret
```

Side exits must:

1. Writeback any dirty cached registers
2. Store the correct guest PC
3. Return to dispatcher (or chain to target block)

---

## What Stage 4 Does NOT Include

To keep scope manageable:

1. **No JIT IR** - We translate directly, no intermediate representation
2. **No SSA** - Keep the simple register model
3. **No interprocedural optimization** - Traces don't span calls (yet)
4. **No speculative optimization** - No type guards, no deoptimization
5. **No profile-guided recompilation** - One compilation level only

These would be Stage 5+ if ever needed.

---

## Performance Expectations

| Workload | Stage 3 | Stage 4 Target | Notes |
|----------|---------|----------------|-------|
| Tight loop (no calls) | Fast | Faster | Superblock helps |
| Loop with branches | Good | Better | Trace unrolling helps |
| Deep call tree | Good (RAS) | Same | No change |
| Switch statement | OK | Same | Still indirect |

**Realistic estimate:** 10-30% improvement on loop-heavy code, minimal change elsewhere.

---

## Testing Strategy

### Correctness

1. All Stage 3 tests must still pass
2. Differential testing: trace vs non-trace must produce same results
3. Stress test side exits with adversarial branching patterns

### New Benchmarks

```c
// Loop-heavy: should benefit from trace
int sum_array(int *arr, int n) {
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

// Conditional-heavy: tests superblock
int count_positive(int *arr, int n) {
    int count = 0;
    for (int i = 0; i < n; i++) {
        if (arr[i] > 0) count++;
    }
    return count;
}

// Nested loops: tests trace depth limits
void matrix_add(int *c, int *a, int *b, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i*n+j] = a[i*n+j] + b[i*n+j];
        }
    }
}
```

---

## Open Questions

1. **Is this worth it?** Stage 3 already handles most hot-path issues. Traces add significant complexity for uncertain gain on small programs.

2. **Trace selection heuristics?** How do we decide when to form a trace vs. use normal blocks?

3. **Trace invalidation?** Not an issue for SLOW-32 (guest code is immutable), but would matter for self-modifying code.

4. **Register allocator?** Current approach is simple load/op/store. A proper allocator would help but adds much complexity.

5. **Code cache pressure?** Traces are larger than blocks. Do we need smarter eviction?

---

## Implementation Order (If Proceeding)

### Phase 1: Superblock Extension (Low-hanging fruit)

- Extend blocks past forward conditional branches
- Simple side exits
- Minimal change to existing code
- Test: loops with exit conditions

### Phase 2: Register Caching (Medium effort)

- Track registers across trace
- Lazy writeback at side exits
- Significant code generation changes
- Test: loops with accumulator variables

### Phase 3: Loop Optimization (Higher effort)

- Loop detection
- Optional unrolling
- Loop-back optimization
- Test: tight inner loops, matrix operations

---

## Implementation Status

### Phase 1: Superblock Extension - COMPLETE

**Implemented:**

- ✅ Added `superblock_enabled` and `superblock_depth` to `translate_ctx_t`
- ✅ Added `MAX_SUPERBLOCK_DEPTH` (4) and `MAX_SUPERBLOCK_EXITS` (8) limits
- ✅ Modified `translate_branch_common()` to extend past forward branches
- ✅ Branch functions now return bool (true = block ends, false = continue)
- ✅ Side exits emitted inline with inverted conditional jump
- ✅ Added `-4` flag and `run_dbt_stage4()` dispatcher
- ✅ Stage 4 is now the default mode
- ✅ All 17 regression tests pass

**How it works:**

- Forward conditional branches (imm > 0) are extended rather than ending the block
- The "taken" path becomes a side exit (inline exit code)
- The "fall-through" path continues inline in the superblock
- Up to 4 branches can be extended per superblock

**Results on feature-loops test:**
| Metric | Stage 3 | Stage 4 |
|--------|---------|---------|
| Blocks cached | 39 | 33 |
| Cache hit rate | 18.0% | 21.8% |
| Chains made | 28 | 18 |

### Phase 2: Register Caching - NOT STARTED

### Phase 3: Loop Optimization - NOT STARTED

---

## Alternative: Skip Stage 4?

Given the complexity vs. benefit trade-off, alternatives to full trace compilation:

1. **Better profiling** - Just add statistics to understand where time goes
2. **Targeted peepholes** - Optimize specific hot patterns (load-op-store fusion)
3. **Larger benchmarks** - May reveal Stage 3 is "good enough"
4. **Move to Stage 5** - Something completely different (e.g., QEMU TCG backend integration)

The best next step might be: **Create larger benchmarks and measure before committing to Stage 4 complexity.**

---

## Conclusion

Stage 4 trace compilation offers modest performance gains at significant implementation cost. For SLOW-32's educational/testing purpose, it may be over-engineering.

**Recommendation:**

- Implement Phase 1 (superblock extension) as a low-risk experiment
- Measure actual impact on benchmarks
- Decide whether to continue based on results

This keeps Stage 4 exploratory rather than committing to full trace compilation.
