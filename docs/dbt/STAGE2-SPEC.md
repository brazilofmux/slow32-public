# SLOW-32 DBT Stage 2: Block Cache & Direct Chaining

## Overview

Stage 1 established correctness: translate a block, execute it, return to dispatcher.
Stage 2 adds performance: cache translated blocks and chain them together.

**Goals:**

1. **Block Cache** - Avoid re-translating the same guest code
2. **Direct Chaining** - Avoid returning to dispatcher for known branches
3. **Indirect Branch Lookup** - Fast hash lookup for computed jumps

## Performance Analysis

Stage 1 bottleneck: Every block returns to C dispatcher, which:

1. Looks at exit_reason
2. Calls translate_block() (re-decodes even if seen before)
3. Calls execute_translated() (inline asm overhead)

Expected speedup from Stage 2: 5-20x depending on branch density.

---

## Block Cache Design

### Data Structure

```c
typedef struct translated_block {
    uint32_t guest_pc;           // Guest address (key)
    uint32_t guest_size;         // Bytes of guest code
    uint8_t *host_code;          // Pointer into code buffer
    uint32_t host_size;          // Bytes of host code
    uint32_t flags;              // BLOCK_FLAG_*

    // For chain management
    struct chain_slot *incoming; // List of blocks that jump here
    uint32_t exit_count;         // Number of exits (1 or 2)
    struct {
        uint32_t target_pc;      // Guest target (or 0 if indirect)
        uint8_t *patch_site;     // Where to patch in host code
        bool chained;            // Already patched?
    } exits[2];                  // Most blocks have 1-2 exits
} translated_block_t;

#define BLOCK_CACHE_SIZE 4096    // Power of 2 for fast modulo
#define BLOCK_FLAG_DIRECT   0x01 // All exits are direct branches
#define BLOCK_FLAG_INDIRECT 0x02 // Has indirect branch exit
#define BLOCK_FLAG_CALL     0x04 // Ends with JAL (potential for return prediction)

typedef struct {
    translated_block_t *blocks[BLOCK_CACHE_SIZE];  // Hash table (open addressing)
    uint32_t count;

    // Code buffer management
    uint8_t *code_buffer;
    uint32_t code_buffer_size;
    uint32_t code_buffer_used;
} block_cache_t;
```

### Hash Function

Simple and fast - guest PC is already well-distributed:

```c
static inline uint32_t block_hash(uint32_t guest_pc) {
    // SLOW-32 instructions are 4-byte aligned, shift out low bits
    return (guest_pc >> 2) & (BLOCK_CACHE_SIZE - 1);
}
```

### Lookup

```c
translated_block_t *cache_lookup(block_cache_t *cache, uint32_t guest_pc) {
    uint32_t idx = block_hash(guest_pc);
    uint32_t start = idx;

    do {
        translated_block_t *b = cache->blocks[idx];
        if (b == NULL) return NULL;           // Empty slot
        if (b->guest_pc == guest_pc) return b; // Found
        idx = (idx + 1) & (BLOCK_CACHE_SIZE - 1); // Linear probe
    } while (idx != start);

    return NULL; // Table full (shouldn't happen)
}
```

### Insertion

```c
void cache_insert(block_cache_t *cache, translated_block_t *block) {
    uint32_t idx = block_hash(block->guest_pc);

    while (cache->blocks[idx] != NULL) {
        idx = (idx + 1) & (BLOCK_CACHE_SIZE - 1);
    }

    cache->blocks[idx] = block;
    cache->count++;
}
```

---

## Direct Chaining

### Concept

When Block A ends with `BEQ r1, r2, BlockB`:

- Stage 1: Generate code that sets `exit_reason = EXIT_BRANCH`, `pc = BlockB`, then `ret`
- Stage 2: Generate a `jmp <placeholder>`, later patch to jump directly to BlockB's host code

### Exit Types

```
Direct Branch (BEQ, BNE, BLT, BGE, etc.):
  - Two exits: taken path, fall-through path
  - Both can be chained once targets are translated

Direct Jump (J):
  - One exit: always taken
  - Can be chained immediately if target exists

Call (JAL):
  - One exit + implicit return expectation
  - Chain the call; return uses indirect mechanism

Indirect Jump (JALR, JR):
  - Cannot be chained statically
  - Use lookup stub (see below)
```

### Chaining Protocol

**At translation time:**

```c
void emit_direct_exit(translate_ctx_t *ctx, uint32_t target_pc, int exit_idx) {
    translated_block_t *target = cache_lookup(ctx->cache, target_pc);

    // Record exit info
    ctx->current_block->exits[exit_idx].target_pc = target_pc;
    ctx->current_block->exits[exit_idx].patch_site = ctx->code_ptr + 1; // After opcode

    if (target) {
        // Target already translated - emit direct jump
        emit_jmp_rel32(ctx, target->host_code);
        ctx->current_block->exits[exit_idx].chained = true;
    } else {
        // Target not yet translated - emit jump to dispatcher stub
        emit_jmp_rel32(ctx, ctx->dispatcher_stub);
        ctx->current_block->exits[exit_idx].chained = false;
    }
}
```

**After translating a new block:**

```c
void chain_incoming_blocks(block_cache_t *cache, translated_block_t *new_block) {
    // Find all blocks that have an unchained exit to new_block->guest_pc
    for (int i = 0; i < BLOCK_CACHE_SIZE; i++) {
        translated_block_t *b = cache->blocks[i];
        if (!b) continue;

        for (int e = 0; e < b->exit_count; e++) {
            if (!b->exits[e].chained &&
                b->exits[e].target_pc == new_block->guest_pc) {
                // Patch the jump
                patch_jmp_rel32(b->exits[e].patch_site, new_block->host_code);
                b->exits[e].chained = true;
            }
        }
    }
}
```

### Dispatcher Stub

When a block exits to an unchained target, it jumps to this stub:

```asm
dispatcher_stub:
    ; PC was already set by the block epilogue
    ; Just return to C dispatcher
    ret
```

The C dispatcher then:

1. Looks up or translates the target block
2. Patches the original exit if found
3. Executes the target block

---

## Indirect Branch Lookup

For `JALR rd, rs, imm` and `JR rs`:

### Option A: Return to Dispatcher (Simple)

Same as Stage 1 - just set `exit_reason = EXIT_INDIRECT` and return.

### Option B: Inline Hash Lookup (Faster)

Generate inline code that:

1. Computes hash of target PC
2. Probes the hash table
3. On hit: jumps directly to host code
4. On miss: returns to dispatcher

```c
void emit_indirect_lookup(translate_ctx_t *ctx, x86_reg target_reg) {
    // target_reg contains the guest PC to look up

    // Hash: (target >> 2) & (CACHE_SIZE - 1)
    emit_mov_r32_r32(ctx, X86_EAX, target_reg);
    emit_shr_r32_imm8(ctx, X86_EAX, 2);
    emit_and_r32_imm32(ctx, X86_EAX, BLOCK_CACHE_SIZE - 1);

    // Load hash table base
    emit_mov_r64_imm64(ctx, X86_RCX, (uint64_t)ctx->cache->blocks);

    // blocks[idx] - each entry is 8 bytes (pointer)
    emit_mov_r64_m64_indexed(ctx, X86_RDX, X86_RCX, X86_RAX, 8);

    // Check if NULL
    emit_test_r64_r64(ctx, X86_RDX, X86_RDX);
    emit_jz_rel8(ctx, /* miss_label */);

    // Check if guest_pc matches (first field of translated_block_t)
    emit_cmp_m32_r32(ctx, X86_RDX, 0, target_reg);
    emit_jne_rel8(ctx, /* miss_label */);

    // Hit! Load host_code pointer (offset 8 in struct)
    emit_mov_r64_m64(ctx, X86_RAX, X86_RDX, offsetof(translated_block_t, host_code));
    emit_jmp_r64(ctx, X86_RAX);

    // miss_label:
    // Store target PC and return to dispatcher
    emit_mov_m32_r32(ctx, X86_RBP, CPU_PC_OFFSET, target_reg);
    emit_mov_m32_imm32(ctx, X86_RBP, CPU_EXIT_REASON_OFFSET, EXIT_INDIRECT);
    emit_ret(ctx);
}
```

---

## Code Buffer Management

### Problem

Stage 1 reuses the same buffer for each block. Stage 2 needs persistent storage.

### Solution: Simple Linear Allocator

```c
uint8_t *code_buffer_alloc(block_cache_t *cache, uint32_t size) {
    // Align to 16 bytes
    uint32_t aligned = (cache->code_buffer_used + 15) & ~15;

    if (aligned + size > cache->code_buffer_size) {
        // Buffer full - need to flush cache
        return NULL;
    }

    uint8_t *ptr = cache->code_buffer + aligned;
    cache->code_buffer_used = aligned + size;
    return ptr;
}
```

### Buffer Overflow

When buffer fills up:

**Option A: Flush Everything**

- Clear cache, reset buffer pointer
- Simple, but loses all translations

**Option B: Multiple Buffers**

- Allocate new buffer, keep old one read-only
- More complex memory management

**Option C: LRU Eviction**

- Track access times, evict least-recently-used blocks
- Complex, probably not worth it for Stage 2

For Stage 2, use Option A. It's simple and adequate.

---

## Modified Dispatcher Loop

```c
static void run_dbt(dbt_cpu_state_t *cpu, block_cache_t *cache) {
    while (!cpu->halted) {
        // Look up in cache first
        translated_block_t *block = cache_lookup(cache, cpu->pc);

        if (!block) {
            // Translate new block
            block = translate_and_cache(cache, cpu);
        }

        // Execute
        execute_translated(cpu, block->host_code);
        cpu->inst_count += block->guest_size / 4;

        // Handle exits
        switch (cpu->exit_reason) {
            case EXIT_BRANCH:
            case EXIT_INDIRECT:
                // Continue - PC already set
                break;

            case EXIT_CHAINED:
                // Block chained directly - shouldn't reach here
                // (This would mean chaining is working!)
                break;

            // ... other cases same as Stage 1
        }
    }
}
```

Actually, with proper chaining, the hot path never returns to C!

```c
static void run_dbt_v2(dbt_cpu_state_t *cpu, block_cache_t *cache) {
    while (!cpu->halted) {
        translated_block_t *block = cache_lookup(cache, cpu->pc);
        if (!block) {
            block = translate_and_cache(cache, cpu);
        }

        // Execute - this will chain through multiple blocks
        // Only returns when hitting an untranslated target or exception
        execute_translated(cpu, block->host_code);

        // If we get here, something needs handling
        handle_exit(cpu, cache);
    }
}
```

---

## File Structure

```
tools/dbt/
├── dbt.c              # Main + dispatcher (update)
├── cpu_state.h        # Add block_cache_t (update)
├── block_cache.c      # NEW: Cache implementation
├── block_cache.h      # NEW: Cache API
├── translate.c        # Update for chaining
├── translate.h        # Update
├── emit_x64.c         # Add new primitives (update)
├── emit_x64.h         # Update
└── Makefile           # Update
```

---

## Implementation Order

### Phase 1: Block Cache (No Chaining)

1. Add `block_cache.h/c` with hash table
2. Modify `translate_block()` to return persistent blocks
3. Update dispatcher to lookup before translate
4. Test: Should work, same speed as Stage 1 (avoids retranslation)

### Phase 2: Direct Exit Chaining

1. Add exit tracking to `translated_block_t`
2. Modify branch translation to emit patchable jumps
3. Implement `chain_incoming_blocks()`
4. Test: Tight loops should stay in native code

### Phase 3: Conditional Branch Chaining

1. Handle BEQ/BNE etc. with two exits
2. Emit conditional jumps with patchable targets
3. Test: Both paths chain correctly

### Phase 4: Indirect Lookup (Optional for Stage 2)

1. Implement inline hash lookup
2. Measure improvement vs. dispatcher return
3. May defer to Stage 3 if complex

---

## Testing Strategy

### Correctness

- Differential testing against interpreter (same as Stage 1)
- All regression tests must pass

### Performance Metrics

```
Metric                  Stage 1     Stage 2 Target
------                  -------     --------------
Simple loop             ~12 MIPS    ~100 MIPS
Function calls          ~10 MIPS    ~50 MIPS
Cache hit rate          N/A         >95%
Avg chain length        1           >10
```

### Test Cases

1. **Tight loop** - Should chain completely, never return to dispatcher
2. **Function with multiple calls** - Return address handling
3. **Recursive function** - Deep call stack
4. **Switch statement** - Jump table (indirect)

---

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Chaining breaks with code invalidation | SLOW-32 has W^X, code is immutable |
| Hash collisions slow lookup | Use larger table, monitor collision rate |
| Code buffer overflow | Simple flush-and-restart for now |
| Indirect branches dominate | Inline lookup in Phase 4 |

---

## Implementation Status

Stage 2 has been implemented with the following results:

### Implemented Features:

- ✅ Block cache with hash table lookup
- ✅ Block metadata pool
- ✅ 4MB code buffer for translated blocks
- ✅ Direct branch chaining (JAL, conditional branches)
- ✅ Exit tracking and patching
- ✅ Cache statistics and profiling

### Test Results:

- ✅ All 17 regression tests pass (1 skipped: MMIO)
- ✅ Exit codes match interpreter

### Performance Observations:

- Small tests (~200-1000 instructions): Similar MIPS to Stage 1
- Cache overhead roughly equals translation savings for small programs
- Real benefits expected in larger programs with hot loops

### Files Changed:

- `block_cache.h` - Cache data structures and API
- `block_cache.c` - Cache implementation
- `translate.h/c` - Added chaining support
- `dbt.c` - Stage 2 dispatcher, command-line options
- `scripts/diff-test.sh` - Updated for cleaner testing

### Usage:
```bash
# Stage 1 (no caching)
./slow32-dbt -1 program.s32x

# Stage 2 (caching + chaining, default)
./slow32-dbt -2 program.s32x

# Show statistics
./slow32-dbt -s program.s32x
```

---

## Success Criteria

Stage 2 is complete when:

1. ✅ All regression tests pass
2. ⚠️ Block cache hit rate varies (2-45% depending on test)
3. ✅ Direct branches chain successfully
4. ⚠️ Performance benefit minimal on small tests (need larger benchmarks)
5. ⏳ No memory leaks under valgrind (not yet tested)

---

## Code Examples

### Before (Stage 1)

```
Block at 0x100 (loop body):
    ... compute ...
    set pc = 0x100
    set exit_reason = EXIT_BRANCH
    ret                 ; Return to C dispatcher

C dispatcher:
    translate_block()   ; Re-decode 0x100 every iteration!
    execute_translated()
```

### After (Stage 2)

```
Block at 0x100 (loop body):
    ... compute ...
    jmp block_0x100     ; Direct jump, no dispatcher!
```

The loop stays entirely in native x86-64 code until something breaks the pattern.
