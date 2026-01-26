# SLOW-32 DBT Stage 3: Indirect Branch Optimization

## Overview

Stage 2 established caching and direct branch chaining. Stage 3 optimizes the remaining performance bottleneck: **indirect branches**.

**Current Problem:**
Every `JALR` (indirect jump/call/return) returns to the C dispatcher, which:
1. Does a full hash table lookup via C code
2. Has function call overhead
3. Breaks the native execution flow

**Stage 3 Goals:**
1. **Inline Hash Lookup** - Probe the block cache directly in generated code
2. **Return Address Stack (RAS)** - Predict return addresses for call/return pairs
3. **Statistics & Tuning** - Better profiling to guide optimizations

**Expected Speedup:** 2-5x for code with many function calls (common in C programs).

---

## Performance Analysis

### Current Indirect Branch Cost (Stage 2)

```
JALR rd, rs1, imm:
    1. Compute target = (rs1 + imm) & ~1        ; ~5 x86 instructions
    2. Store to cpu->pc                          ; 2 instructions
    3. Store exit_reason = EXIT_INDIRECT         ; 3 instructions
    4. ret                                       ; 1 instruction

    [C Dispatcher]
    5. Read cpu->pc                              ; memory load
    6. Call cache_lookup()                       ; function call overhead
    7. Hash, probe loop, compare                 ; ~20+ instructions
    8. Call execute_translated()                 ; more overhead
    9. Jump to block                             ; finally!
```

Total: **~40-50 cycles** for each indirect branch.

### Target: Inline Hash Lookup

```
JALR rd, rs1, imm:
    1. Compute target = (rs1 + imm) & ~1        ; ~5 x86 instructions
    2. Hash: (target >> 2) & MASK               ; 3 instructions
    3. Load block ptr from cache->blocks[idx]   ; 1 memory load
    4. if (ptr == NULL) goto slow_path          ; 1 branch
    5. if (ptr->guest_pc != target) goto slow   ; 1 compare, 1 branch
    6. jmp ptr->host_code                       ; 1 indirect jump

    slow_path:
    7. Store pc, exit_reason                    ; 5 instructions
    8. ret                                      ; return to dispatcher
```

Hit path: **~15-20 cycles** (3x improvement).

### Target: Return Address Stack

For call/return pairs, we can do even better:

```
Call site (JAL r31, func):
    1. Push return_pc onto RAS                  ; 2 instructions
    2. Jump to callee                           ; (already chained)

Return site (JALR r0, r31, 0):
    1. Pop predicted_pc from RAS               ; 2 instructions
    2. if (predicted_pc == actual_pc) goto *ras_target  ; 1 compare, 1 branch
    3. else: do inline hash lookup             ; fallback
```

RAS hit: **~8-10 cycles** (5x improvement over inline lookup).

---

## Data Structures

### Inline Lookup Table

The existing `block_cache_t` works, but we need fast access from generated code:

```c
// Exposed to generated code via fixed registers
// r15 = pointer to lookup table base (cache->blocks)
// This allows: mov rax, [r15 + idx*8] to get block pointer

// In cpu_state.h or block_cache.h:
typedef struct {
    translated_block_t **lookup_table;  // Same as cache->blocks
    uint32_t lookup_mask;               // BLOCK_CACHE_SIZE - 1
} inline_lookup_t;
```

### Return Address Stack

```c
#define RAS_SIZE 32  // Power of 2, typical call depth

typedef struct {
    uint32_t guest_pc[RAS_SIZE];       // Predicted return addresses
    uint8_t *host_code[RAS_SIZE];      // Corresponding host addresses
    uint8_t top;                        // Stack pointer (wraps)
} return_stack_t;

// Add to dbt_cpu_state_t:
typedef struct dbt_cpu_state {
    // ... existing fields ...

    // Stage 3: Indirect branch optimization
    translated_block_t **lookup_table;  // Fast path for generated code
    uint32_t lookup_mask;
    return_stack_t ras;
} dbt_cpu_state_t;
```

### Block Metadata Extension

```c
// Add to translated_block_t:
typedef struct translated_block {
    // ... existing fields ...

    // Stage 3: RAS support
    uint8_t *ras_entry;     // Entry point for RAS prediction hit
                            // (skips the inline lookup code)
} translated_block_t;
```

---

## Implementation Details

### Phase 1: Inline Hash Lookup for JALR

#### Generated Code Pattern

```c
void emit_indirect_lookup(translate_ctx_t *ctx, x64_reg_t target_reg) {
    emit_ctx_t *e = &ctx->emit;

    // target_reg (e.g., RAX) contains the guest PC to look up
    // RBP = cpu state pointer
    // We'll use: RCX = hash, RDX = block pointer, R8 = saved target

    // 1. Save target (we'll need it for comparison and fallback)
    emit_mov_r64_r64(e, R8, target_reg);  // r8 = target

    // 2. Compute hash: (target >> 2) & MASK
    emit_mov_r32_r32(e, ECX, target_reg);
    emit_shr_r32_imm8(e, ECX, 2);
    emit_and_r32_imm32(e, ECX, BLOCK_CACHE_MASK);

    // 3. Load lookup table base from CPU state
    emit_mov_r64_m64(e, RDX, RBP, CPU_LOOKUP_TABLE_OFFSET);

    // 4. Load block pointer: blocks[idx]
    // RDX = RDX + RCX*8
    emit_mov_r64_m64_sib(e, RDX, RDX, RCX, 8, 0);

    // 5. Check for NULL
    emit_test_r64_r64(e, RDX, RDX);
    size_t miss_patch1 = emit_offset(e) + 2;
    emit_jz_rel32(e, 0);  // -> slow_path

    // 6. Compare guest_pc (first field of translated_block_t)
    emit_cmp_m32_r32(e, RDX, 0, R8D);  // compare block->guest_pc with target
    size_t miss_patch2 = emit_offset(e) + 2;
    emit_jne_rel32(e, 0);  // -> slow_path

    // 7. Hit! Load host_code and jump
    emit_mov_r64_m64(e, RAX, RDX, offsetof(translated_block_t, host_code));
    emit_jmp_r64(e, RAX);

    // slow_path: Fall back to dispatcher
    size_t slow_path = emit_offset(e);
    emit_patch_rel32(e, miss_patch1, slow_path);
    emit_patch_rel32(e, miss_patch2, slow_path);

    // Store target PC
    emit_mov_m32_r32(e, RBP, CPU_PC_OFFSET, R8D);

    // Store exit reason
    emit_mov_r32_imm32(e, EAX, EXIT_INDIRECT);
    emit_mov_m32_r32(e, RBP, CPU_EXIT_REASON_OFFSET, EAX);

    emit_ret(e);
}
```

#### Code Size

Inline lookup adds ~40-50 bytes per JALR. Trade-off:
- More code buffer usage
- But avoids dispatcher overhead on hits

### Phase 2: Return Address Stack

#### RAS Push (at JAL sites)

```c
void emit_ras_push(translate_ctx_t *ctx, uint32_t return_pc, uint8_t *return_host) {
    emit_ctx_t *e = &ctx->emit;

    // Load RAS pointer and top index
    // cpu->ras.top is at CPU_RAS_TOP_OFFSET
    // cpu->ras.guest_pc is at CPU_RAS_GUEST_OFFSET
    // cpu->ras.host_code is at CPU_RAS_HOST_OFFSET

    // 1. Load top index
    emit_movzx_r32_m8(e, ECX, RBP, CPU_RAS_TOP_OFFSET);

    // 2. Store return PC
    emit_lea_r64_m64(e, RDX, RBP, CPU_RAS_GUEST_OFFSET);
    emit_mov_r32_imm32(e, EAX, return_pc);
    emit_mov_m32_r32_sib(e, RDX, RCX, 4, 0, EAX);

    // 3. Store return host code (will patch later if not yet known)
    emit_lea_r64_m64(e, RDX, RBP, CPU_RAS_HOST_OFFSET);
    emit_mov_r64_imm64(e, RAX, (uint64_t)return_host);
    emit_mov_m64_r64_sib(e, RDX, RCX, 8, 0, RAX);

    // 4. Increment top (with wrap)
    emit_add_r32_imm8(e, ECX, 1);
    emit_and_r32_imm8(e, ECX, RAS_SIZE - 1);
    emit_mov_m8_r8(e, RBP, CPU_RAS_TOP_OFFSET, CL);
}
```

#### RAS Pop & Predict (at JALR r0, r31, 0 sites)

```c
void emit_ras_predict(translate_ctx_t *ctx, x64_reg_t actual_target) {
    emit_ctx_t *e = &ctx->emit;

    // actual_target contains the actual return address (rs1 + imm)

    // 1. Decrement top (with wrap)
    emit_movzx_r32_m8(e, ECX, RBP, CPU_RAS_TOP_OFFSET);
    emit_sub_r32_imm8(e, ECX, 1);
    emit_and_r32_imm8(e, ECX, RAS_SIZE - 1);
    emit_mov_m8_r8(e, RBP, CPU_RAS_TOP_OFFSET, CL);

    // 2. Load predicted guest PC
    emit_lea_r64_m64(e, RDX, RBP, CPU_RAS_GUEST_OFFSET);
    emit_mov_r32_m32_sib(e, EAX, RDX, RCX, 4, 0);

    // 3. Compare with actual
    emit_cmp_r32_r32(e, EAX, actual_target);
    size_t mismatch_patch = emit_offset(e) + 2;
    emit_jne_rel32(e, 0);  // -> inline_lookup

    // 4. Prediction hit! Load host code and jump
    emit_lea_r64_m64(e, RDX, RBP, CPU_RAS_HOST_OFFSET);
    emit_mov_r64_m64_sib(e, RAX, RDX, RCX, 8, 0);
    emit_jmp_r64(e, RAX);

    // mismatch: Fall through to inline lookup
    size_t mismatch_target = emit_offset(e);
    emit_patch_rel32(e, mismatch_patch, mismatch_target);

    // Continue with inline lookup using actual_target...
    emit_indirect_lookup_inline(ctx, actual_target);
}
```

#### Handling RAS Host Code Patching

When we emit a JAL, the return block might not be translated yet. We need to:

1. Initially store `return_host = NULL` or a sentinel
2. When the return block is translated, scan RAS entries (or maintain a patch list)

**Simpler approach:** Always store the guest PC; do an inline lookup if host code is unknown.

```c
// At JAL emission time:
uint32_t return_pc = ctx->guest_pc + 4;
translated_block_t *return_block = cache_lookup(cache, return_pc);
uint8_t *return_host = return_block ? return_block->host_code : NULL;

emit_ras_push(ctx, return_pc, return_host);
```

At return time, if RAS host code is NULL, fall through to inline lookup.

---

## Register Allocation

Stage 3 needs more registers in generated code:

| Register | Purpose |
|----------|---------|
| RBP | CPU state pointer (unchanged) |
| R14 | Guest memory base (unchanged) |
| R15 | Block lookup table pointer (NEW) |
| RAX, RCX, RDX | Scratch (unchanged) |
| R8, R9 | Additional scratch for inline lookup |

### Trampoline Update

```c
static void execute_translated(dbt_cpu_state_t *cpu, translated_block_fn block) {
    uint8_t *mem_base = cpu->mem_base;
    translated_block_t **lookup_table = cpu->lookup_table;

    __asm__ __volatile__(
        "push %%rbp\n\t"
        "push %%rbx\n\t"
        "push %%r12\n\t"
        "push %%r13\n\t"
        "push %%r14\n\t"
        "push %%r15\n\t"

        "mov %%rax, %%rbp\n\t"   // rbp = cpu
        "mov %%rcx, %%r14\n\t"   // r14 = mem_base
        "mov %%rsi, %%r15\n\t"   // r15 = lookup_table (NEW)

        "call *%%rdx\n\t"

        "pop %%r15\n\t"
        "pop %%r14\n\t"
        "pop %%r13\n\t"
        "pop %%r12\n\t"
        "pop %%rbx\n\t"
        "pop %%rbp\n\t"

        :
        : "a" (cpu), "c" (mem_base), "S" (lookup_table), "d" (block)
        : "rdi", "r8", "r9", "r10", "r11", "memory", "cc"
    );
}
```

---

## Modified Translation

### translate_jalr (Stage 3)

```c
void translate_jalr(translate_ctx_t *ctx, uint8_t rd, uint8_t rs1, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint32_t return_pc = ctx->guest_pc + 4;

    // Calculate target: (rs1 + imm) & ~1
    emit_load_guest_reg(e, RAX, rs1);
    if (imm != 0) {
        emit_add_r32_imm32(e, RAX, imm);
    }
    emit_and_r32_imm32(e, RAX, ~1u);

    // Store return address if rd != 0
    if (rd != 0) {
        emit_mov_r32_imm32(e, RCX, return_pc);
        emit_store_guest_reg(e, rd, RCX);
    }

    // Mark block flags
    if (ctx->block) {
        ctx->block->flags |= BLOCK_FLAG_INDIRECT;
    }

    // Check for return pattern: JALR r0, r31, 0
    bool is_return = (rd == 0 && rs1 == REG_LR && imm == 0);

    if (is_return && ctx->cache && ctx->cache->ras_enabled) {
        // Use RAS prediction
        ctx->block->flags |= BLOCK_FLAG_RETURN;
        emit_ras_predict(ctx, RAX);
    } else if (ctx->cache && ctx->cache->inline_lookup_enabled) {
        // Use inline hash lookup
        emit_indirect_lookup(ctx, RAX);
    } else {
        // Stage 2 fallback: return to dispatcher
        emit_mov_m32_r32(e, RBP, CPU_PC_OFFSET, RAX);
        emit_mov_r32_imm32(e, RAX, EXIT_INDIRECT);
        emit_mov_m32_r32(e, RBP, CPU_EXIT_REASON_OFFSET, RAX);
        emit_ret(e);
    }
}
```

### translate_jal (Stage 3)

```c
void translate_jal(translate_ctx_t *ctx, uint8_t rd, int32_t imm) {
    emit_ctx_t *e = &ctx->emit;
    uint32_t return_pc = ctx->guest_pc + 4;
    uint32_t target_pc = ctx->guest_pc + imm;

    // Store return address if rd != 0
    if (rd != 0) {
        emit_mov_r32_imm32(e, RAX, return_pc);
        emit_store_guest_reg(e, rd, RAX);
    }

    // Push to RAS if this is a call (rd == r31)
    if (rd == REG_LR && ctx->cache && ctx->cache->ras_enabled) {
        ctx->block->flags |= BLOCK_FLAG_CALL;

        // Try to get return block's host code
        translated_block_t *return_block = cache_lookup(ctx->cache, return_pc);
        uint8_t *return_host = return_block ? return_block->host_code : NULL;

        emit_ras_push(ctx, return_pc, return_host);
    }

    // Exit with branch to target (chainable) - unchanged from Stage 2
    emit_exit_chained(ctx, target_pc, 0);
}
```

---

## Performance Tracking

### Enhanced Statistics

```c
typedef struct {
    // ... existing stats ...

    // Stage 3 stats
    uint64_t indirect_count;      // Total indirect branches
    uint64_t inline_hit_count;    // Inline lookup hits
    uint64_t inline_miss_count;   // Inline lookup misses (to dispatcher)
    uint64_t ras_push_count;      // RAS pushes
    uint64_t ras_hit_count;       // RAS prediction hits
    uint64_t ras_miss_count;      // RAS prediction misses
} cache_stats_t;
```

### Profiling Infrastructure

For future optimization (hot path compilation, etc.):

```c
// Increment exec_count atomically in generated code
// Add at block entry:
void emit_block_profile(translate_ctx_t *ctx) {
    if (!ctx->profiling_enabled) return;

    emit_ctx_t *e = &ctx->emit;

    // block->exec_count is at known offset from block pointer
    // We can store block pointer in a fixed location in cpu state
    emit_mov_r64_imm64(e, RAX, (uint64_t)ctx->block);
    emit_lock_inc_m32(e, RAX, offsetof(translated_block_t, exec_count));
}
```

---

## File Structure Changes

```
tools/dbt/
├── dbt.c              # Update execute_translated(), add Stage 3 mode
├── cpu_state.h        # Add lookup_table, ras fields
├── block_cache.h      # Add inline lookup helpers, ras_enabled flag
├── block_cache.c      # Add RAS management
├── translate.h        # Add emit_indirect_lookup, emit_ras_* declarations
├── translate.c        # Implement Stage 3 JALR/JAL handling
├── emit_x64.h         # Add new emit primitives
├── emit_x64.c         # Implement SIB addressing, etc.
└── Makefile           # No changes needed
```

---

## Implementation Order

### Phase 1: Inline Hash Lookup (Core Feature)
1. Add `lookup_table` pointer to `dbt_cpu_state_t`
2. Update `execute_translated()` to pass lookup table in R15
3. Add necessary emit primitives for SIB addressing
4. Implement `emit_indirect_lookup()`
5. Update `translate_jalr()` to use inline lookup
6. Test: Function-heavy programs should show improvement

### Phase 2: Return Address Stack (Optimization)
1. Add `return_stack_t` to `dbt_cpu_state_t`
2. Implement `emit_ras_push()` and `emit_ras_predict()`
3. Update `translate_jal()` to push RAS on calls
4. Update `translate_jalr()` to use RAS for returns
5. Test: Deep call stacks should show additional improvement

### Phase 3: Statistics & Tuning
1. Add Stage 3 statistics
2. Add `-3` command-line flag
3. Profile and tune hash table size, RAS size
4. Optional: Linear probing in inline lookup

---

## Testing Strategy

### Correctness
- All Stage 2 regression tests must pass
- Differential testing against interpreter

### Performance Benchmarks

| Test | Stage 2 | Stage 3 Target |
|------|---------|----------------|
| Recursive factorial(20) | ~X MIPS | ~2X MIPS |
| Fibonacci(30) | ~X MIPS | ~2.5X MIPS |
| printf-heavy test | ~X MIPS | ~3X MIPS |
| Tight loop (no calls) | ~Y MIPS | ~Y MIPS (unchanged) |

### New Test Cases
1. **Deep recursion** - Tests RAS depth handling
2. **Indirect call table** - Tests inline lookup (not RAS)
3. **Longjmp/setjmp-like** - Non-local returns, RAS misprediction

---

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Inline lookup code size bloat | Monitor code buffer usage; fallback to dispatcher if critical |
| RAS misprediction overhead | Inline lookup fallback ensures correctness, minimal overhead |
| Register pressure | R15 dedication is acceptable; R8/R9 scratch doesn't conflict |
| Hash collision chains | Single-probe inline, fall back to dispatcher for misses |

---

## Alternative Approaches Considered

### 1. Polymorphic Inline Caches (PICs)
- Cache last N targets at each call site
- More complex, better for highly polymorphic calls
- **Deferred:** SLOW-32 programs don't have polymorphic dispatch

### 2. Return Address Buffer (RAB) like x86
- Hardware-style return prediction
- Requires more state management
- **Using RAS instead:** Simpler software equivalent

### 3. Trace Compilation
- Build superblocks across multiple BBs
- More complex, better for very hot paths
- **Deferred to Stage 4:** Focus on indirect branches first

---

## Implementation Status

### Phase 1: Inline Hash Lookup - COMPLETE

**Implemented:**
- ✅ New emit primitives: `emit_mov_r64_m64`, `emit_mov_r64_m64_sib`, `emit_cmp_m32_r32`, `emit_test_r64_r64`
- ✅ CPU state extended with `lookup_table` and `lookup_mask` pointers
- ✅ `emit_indirect_lookup()` generates inline hash probe for JALR
- ✅ Stage 3 mode (`-3` flag, now default)
- ✅ All 17 regression tests pass

**Results:**
- Inline lookup reduces dispatcher calls (e.g., 1056 → 734 for printf test)
- Code size increase: ~60 bytes per JALR instruction
- Performance: Mixed results on small tests (overhead vs savings trade-off)
- Benefits expected on larger programs with hot indirect branches

**Known Limitations:**
- Instruction count may be inaccurate when inline lookup succeeds (count only updates on dispatcher return)
- No linear probing on collision (single-probe, falls back to dispatcher on miss)

### Phase 2: Return Address Stack - COMPLETE

**Implemented:**
- ✅ RAS data structure in cpu_state.h (32-entry circular buffer)
- ✅ `emit_mov_r32_m32_sib` and `emit_mov_m32_r32_sib` for array access
- ✅ `emit_ras_push()` for JAL r31 (call) sites
- ✅ `emit_ras_predict()` for JALR r0, r31, 0 (return) sites
- ✅ translate_jal() pushes to RAS on calls
- ✅ translate_jalr() uses RAS prediction for returns
- ✅ All 17 regression tests pass

**How it works:**
- At call sites (JAL r31), the return address is pushed onto the RAS
- At return sites (JALR r0, r31, 0), RAS predicts the return address
- If prediction matches actual: inline lookup for predicted address (fast path)
- If prediction misses: falls through to regular inline lookup

---

## Success Criteria

Stage 3 is complete when:

1. ✅ All regression tests pass
2. ✅ Inline lookup implemented and working
3. ✅ RAS prediction working for call/return pairs
4. ⏳ 2x+ speedup on function-call-heavy benchmarks (needs larger tests)
5. ✅ No regressions on loop-heavy benchmarks
6. ✅ RAS integrated - hits flow through inline lookup fast path

---

## New Emit Primitives Required

Stage 3 needs these additional primitives in emit_x64.h/c:

### 64-bit Memory Operations

```c
// mov r64, [base + disp32] - load 64-bit pointer from CPU state
void emit_mov_r64_m64(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp);

// mov r64, [base + index*scale] - load from pointer array
// scale: 0=1, 1=2, 2=4, 3=8
void emit_mov_r64_m64_sib(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base,
                          x64_reg_t index, uint8_t scale, int32_t disp);

// cmp [base + disp], r32 - compare memory with register
void emit_cmp_m32_r32(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, x64_reg_t src);
```

### 64-bit Test

```c
// test r64, r64 - for NULL checks
void emit_test_r64_r64(emit_ctx_t *ctx, x64_reg_t a, x64_reg_t b);
```

### Byte Operations (for RAS)

```c
// movzx r32, byte [base + disp] - load RAS top index
void emit_movzx_r32_m8(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp);

// mov byte [base + disp], r8 - store RAS top index
void emit_mov_m8_r8(emit_ctx_t *ctx, x64_reg_t base, int32_t disp, x64_reg_t src);

// add/sub r32, imm8 - for RAS index manipulation
// (already have add_r32_imm32 which handles imm8 automatically)
```

### LEA (Load Effective Address)

```c
// lea r64, [base + disp] - compute address
void emit_lea_r64_m64(emit_ctx_t *ctx, x64_reg_t dst, x64_reg_t base, int32_t disp);
```

---

## Appendix: x86-64 Encoding Reference

### SIB Addressing

For `mov rax, [base + index*scale + disp]`:

```
REX.W prefix: 48 (for 64-bit operand)
Opcode: 8B (mov r64, r/m64)
ModR/M: mod=00/01/10, reg=dst, r/m=100 (SIB follows)
SIB: scale|index|base
  scale: 00=1, 01=2, 10=4, 11=8
  index: register index (4=none for ESP/R12)
  base: register index
disp: 0/8/32 bytes depending on mod
```

### Indirect Jump

```
jmp *rax: FF E0
jmp *[rax]: FF 20
```

### Lock Prefix (for atomic increment)

```
lock incl [rax]: F0 FF 00
```
