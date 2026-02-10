# The Deferred Side Exit Allocation Snapshot Bug

**Date**: February 2026
**Impact**: 29 of 53 dBase III test failures on DBT Stage 4
**Root cause**: 4 missing lines of code in the side exit snapshot mechanism
**Time to diagnosis**: ~3 sessions of deep debugging
**Fix**: 10 lines changed across 2 files

---

## The Symptom

```
Expected: Width (1-254):
Got:      Width (1- 254):
```

A single spurious space before `254`. Every `printf("%d", ...)` call in the
dBase III clone produced a space-prefixed number when running on the DBT with
superblocks and register caching enabled (Stage 4). The interpreter produced
correct output. The same printf code in a small test binary produced correct
output on the DBT. Only the 140KB dBase binary triggered the bug.

## The Architecture

The SLOW-32 DBT (Dynamic Binary Translator) translates guest RISC instructions
into native x86-64 code at runtime. Stage 4 combines two optimizations:

**Superblocks**: When a forward conditional branch is encountered during
translation, instead of ending the block, the translator converts it to a
"side exit" and continues translating the fall-through path. The taken path
becomes a "cold stub" emitted at the end of the block. This keeps the hot
path contiguous in memory.

**Register caching**: The 8 most frequently used guest registers are kept in
dedicated x86-64 host registers (RBX, R12, R13, RSI, RDI, R11, R8, R9)
instead of being loaded from and stored to memory on every access. When a
register is modified, it's marked "dirty" and flushed to memory at block
boundaries.

These two features interact at side exits. When a side exit fires at runtime,
the cold stub must flush any dirty cached registers back to memory before
transferring control to the target block. To do this correctly, the translator
captures a "dirty snapshot" at each side exit point during translation.

## The Bug

The dirty snapshot stored only one thing: **which slots were dirty**.

```c
// At each side exit point during translation:
for (int s = 0; s < REG_ALLOC_SLOTS; s++) {
    deferred_exits[di].dirty_snapshot[s] = ctx->reg_alloc[s].dirty;
}
```

When emitting the cold stub at the end of the block, the flush function used
the dirty snapshot to decide *which* slots to flush, but read the **current**
register allocation to determine *where* to write:

```c
static void reg_alloc_flush_snapshot(ctx, dirty_snapshot) {
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!ctx->reg_alloc[i].allocated || !dirty_snapshot[i])
            continue;
        // BUG: ctx->reg_alloc[i].guest_reg is the END-OF-BLOCK mapping,
        // not the mapping at the side exit point!
        emit_mov(memory[ctx->reg_alloc[i].guest_reg], host_reg[i]);
    }
}
```

In a superblock, the translator continues past side exits. Later instructions
may **evict** a guest register from a cache slot and **reassign** that slot to
a different guest register. By the time the cold stubs are emitted at the end
of the block, the slot-to-guest-register mapping may have changed.

### What Happens at Runtime

Consider a superblock where slot 6 (host register R8) is used as follows:

```
Translation time:
  [instruction 10]  slot 6 = guest r9, set to 0, marked dirty
  [instruction 15]  SIDE EXIT A captured: slot 6 dirty = true
  [instruction 20]  r9 evicted from slot 6
  [instruction 22]  slot 6 = guest r22, loaded from memory
  [instruction 30]  END OF BLOCK

Cold stub for side exit A (emitted at instruction 30):
  // dirty_snapshot[6] = true (correct - r9 was dirty)
  // ctx->reg_alloc[6].guest_reg = r22 (WRONG - was r9 at exit point!)
  MOV [memory + r22_offset], R8    // Writes r9's value to r22's location!
```

When side exit A fires at runtime:
1. The conditional jump goes directly to the cold stub (the superblock code
   after instruction 15 does NOT execute)
2. Host register R8 contains guest r9's value (0) from the side exit point
3. The cold stub writes R8 to **r22's** memory location instead of r9's
4. r9's memory location retains its stale value from before the block

The target block then loads r9 from memory and gets the stale value instead
of 0.

## Why Only the dBase Binary

The bug requires slot reassignment *within a single superblock* between a side
exit and the end of the block. This only happens when:

1. The superblock is long enough to exhaust all 8 cache slots
2. A guest register is evicted to make room for a different one
3. The eviction happens after a side exit that dirtied the original register

Small test binaries produce short superblocks with few register pressure
points. The dBase binary's printf implementation (`vsnprintf_enhanced`, ~2000
instructions) creates superblocks with dozens of side exits and heavy register
pressure, making slot reassignment inevitable.

The printf code was byte-for-byte identical between the test binary and the
dBase binary. The only difference was the *surrounding code* that created
longer superblock chains with more register pressure.

## Why Only Stage 4

- Superblocks alone (Stage 3, `-S`): No register cache, so no slots to corrupt.
  All register values live in memory. Correct.
- Register cache alone (`-R`): No superblocks, so no deferred side exits. Every
  block boundary does a full flush with the current (correct) allocation. Correct.
- Both together (Stage 4, default): Deferred side exits + register cache = the
  snapshot uses stale allocation data. **Broken.**

## The Manifestation

In the dBase binary's printf, guest register r9 serves as the `is_signed_conv`
flag. It's initialized to 0 at the top of `vsnprintf_enhanced` and stored to
the stack frame at `fp-136`. Later, when formatting a `%d` conversion, the
flag is loaded back and checked. If non-zero AND the conversion is signed,
a space character is prepended to positive numbers.

The snapshot bug caused the r9=0 store to be written to the wrong guest
register's memory location. The stack location `fp-136` retained a stale
non-zero value, so `is_signed_conv` appeared to be set, and every `%d`
conversion got a spurious space prefix.

## The Investigation

### What We Ruled Out (in order)

1. **f64 const_prop invalidation** (Session 1) - Found and fixed a real but
   separate bug. Improved results from 15/53 to 24/53.
2. **Dead temporary elimination** - Guarded with `if (!ctx->superblock_enabled)`.
   Not the root cause. Guard was unnecessary and later removed.
3. **Pending write snapshot** (Session 2) - Added pending write capture to
   deferred exits. Necessary but not sufficient.
4. **Force-all-dirty in snapshots** - Changed `dirty_snapshot[s] = dirty` to
   `dirty_snapshot[s] = allocated`. Bug persisted. Dirty flags were not the issue.
5. **r13/space_sign flag corruption** - Forced r13=0 at initialization. Bug
   persisted. The flag itself was fine.
6. **All flag register initialization** - Forced r21, r9, r13, r4, r20 = 0.
   Bug persisted. The initialization block was correct.
7. **Printf code differences** - Compared byte-for-byte between test and dBase
   binaries. Identical. Not a code generation issue.
8. **MMIO libc differences** - Built test with same libc. No bug. Same library.
9. **Block structure analysis** - Mapped every block in the printf code path,
   every side exit, every register allocation. This revealed that register
   allocations *differed between blocks* but didn't immediately reveal the
   snapshot was using end-of-block state.

### The Breakthrough (Session 3)

Reading `reg_alloc_flush_snapshot()`:

```c
emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(ctx->reg_alloc[i].guest_reg), ...);
//                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//                                         This is the CURRENT state,
//                                         not the state at snapshot time!
```

The snapshot captured `dirty[8]` but not `guest_reg[8]`. The flush function
dereferenced the live `ctx->reg_alloc` array, which had been mutated by the
superblock continuation after the snapshot was taken.

## The Fix

**translate.h** - Add allocation state to the snapshot:

```c
struct {
    // ... existing fields ...
    bool dirty_snapshot[REG_ALLOC_SLOTS];
    bool allocated_snapshot[REG_ALLOC_SLOTS];     // NEW
    uint8_t guest_reg_snapshot[REG_ALLOC_SLOTS];  // NEW
} deferred_exits[MAX_BLOCK_EXITS];
```

**translate.c** - Capture full state at each side exit:

```c
for (int s = 0; s < REG_ALLOC_SLOTS; s++) {
    ctx->deferred_exits[di].dirty_snapshot[s] = ctx->reg_alloc[s].dirty;
    ctx->deferred_exits[di].allocated_snapshot[s] = ctx->reg_alloc[s].allocated;
    ctx->deferred_exits[di].guest_reg_snapshot[s] = ctx->reg_alloc[s].guest_reg;
}
```

**translate.c** - Use snapshots in the flush:

```c
static void reg_alloc_flush_snapshot(ctx, dirty_snapshot,
                                      allocated_snapshot,
                                      guest_reg_snapshot) {
    for (int i = 0; i < REG_ALLOC_SLOTS; i++) {
        if (!allocated_snapshot[i] || !dirty_snapshot[i])
            continue;
        emit_mov_m32_r32(e, RBP, GUEST_REG_OFFSET(guest_reg_snapshot[i]),
                         reg_alloc_hosts[i]);
    }
}
```

## The Result

```
Before:  24/53 dBase tests passing on DBT
After:   53/53 dBase tests passing on DBT

Regression tests:  58/58
DBT diff tests:    58/58
```

## Performance

With the bug fixed and all optimizations working correctly:

```
slow32-fast (interpreter):  2.71s  (53 tests)
slow32-dbt (JIT):           0.59s  (53 tests)

Speedup: 4.6x
```

## Lessons

1. **Snapshot everything you dereference later.** If a deferred operation reads
   state that can change between capture and use, the snapshot must include all
   of that state. Capturing the dirty flags but not the register mapping is
   like remembering that you need to mail a letter but forgetting the address.

2. **Bugs that require two features to interact are the hardest to find.** Neither
   superblocks nor register caching was broken in isolation. The bug only existed
   in the four lines of code where they met: the deferred side exit flush.

3. **Large binaries find bugs that small tests cannot.** The printf code was
   identical. The bug was in how the JIT *compiled* that code in the context of
   a large binary with high register pressure. Unit tests for the JIT's
   individual features would never trigger this.

4. **When the same code produces different results at different addresses, the
   bug is in the translator, not the translated code.** This was the key clue:
   identical guest instructions, identical memory layout, different output. The
   only variable was the surrounding binary's effect on superblock length and
   register pressure.
