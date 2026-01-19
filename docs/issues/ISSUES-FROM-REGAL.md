# SLOW-32 Toolchain Issues Discovered During Regal Port

These issues were discovered while porting the Regal accounting system to SLOW-32. The application is a real-world C program with ~15K lines of code, making it a good stress test for the toolchain.

---

## Issue 1: LLVM Codegen Optimization Bug in Tight Loops

### Summary
The LLVM backend generates incorrect code for tight loops with switch statements at `-O2`. The compiler appears to incorrectly cache values across loop iterations, causing state machine logic to fail.

### Symptoms
- CSV parser state machine fails silently
- Adding `printf()` debug statements makes the code work (classic Heisenbug)
- The loop appears to exit early or skip iterations
- Fields are not accumulated correctly despite correct data being read

### Affected Code Pattern
```c
for (size_t i = 0; i < len; i++) {
    unsigned char ch = data[i];

    switch (state) {
    case STATE_A:
        if (ch == ',') {
            // state transitions
            state = STATE_B;
        }
        break;
    case STATE_B:
        // more state handling
        break;
    }
}
```

### Workaround
Adding a memory barrier inside the loop forces correct behavior:
```c
for (size_t i = 0; i < len; i++) {
    unsigned char ch = data[i];

    /* Memory barrier to prevent LLVM optimization issues */
    __asm__ volatile("" : : : "memory");

    switch (state) {
    // ...
    }
}
```

### Investigation Notes
- The bug manifests only at `-O2`; we couldn't test `-O1` because LLC crashes
- Volatile variables alone don't fix it
- The issue appears related to how the optimizer handles the loop iterator, character read, and state variable together
- Possibly related to loop-invariant code motion or value caching optimizations

### Files for Reproduction
- `/ztank/secret/sdennis/regal/src/regal/csv.c` - The CSV parser that exhibits the bug
- Test by building regal for SLOW-32 and running `regal stats`

---

## Issue 2: Hardcoded 1MB Heap Limit When MMIO is Enabled

### Summary
The linker (`s32-ld`) hardcodes a 1MB heap size when MMIO is enabled, regardless of available memory. This prevents applications from allocating more than ~1MB total heap space.

### Location
`tools/linker/s32-ld.c`, line 953:
```c
ld->mmio_base = ld->heap_base + 0x100000;  // 1MB heap space by default
```

### Problem
When `--mmio` is specified (required for file I/O):
1. `__heap_start` is set to `heap_base` (after BSS)
2. `__heap_end` is set to `mmio_base` (line 990)
3. `mmio_base` is calculated as `heap_base + 0x100000` (1MB)
4. Result: Only 1MB available for malloc, regardless of 200+ MB physical memory available

### Symptoms
- `malloc(1434891)` fails even with 200MB+ available memory
- Larger CSV files fail to load
- The executable header shows correct memory layout, but runtime heap is limited

### Suggested Fix
Add a `--heap-size` option to allow configuring heap space:
```c
// In argument parsing
} else if (strcmp(arg, "--heap-size") == 0) {
    ld->heap_size = parse_size(argv[++i]);  // Parse like --data-size
}

// In layout_sections(), around line 953
if (ld->mmio_size > 0) {
    size_t heap_space = ld->heap_size ? ld->heap_size : 0x100000;  // Default 1MB
    ld->mmio_base = ld->heap_base + heap_space;
    ld->mmio_base = (ld->mmio_base + 0xFFF) & ~0xFFF;  // Page align
}
```

### Alternative Approach
Calculate heap size based on available space:
```c
if (ld->mmio_size > 0) {
    // Place MMIO just before stack, leaving rest for heap
    ld->mmio_base = ld->stack_base - ld->stack_size - ld->mmio_size;
    ld->mmio_base = ld->mmio_base & ~0xFFF;  // Page align down
}
```

---

## Issue 3: LLC Crashes at -O1

### Summary
Attempting to compile with `-O1` instead of `-O2` causes LLC to crash.

### Error
```
LLVM ERROR: Cannot select: ...
(or similar crash during instruction selection)
```

### Impact
Cannot use lower optimization levels to work around Issue 1.

---

## Priority

1. **Issue 2 (Heap Limit)** - High priority, easy fix, completely blocks real applications
2. **Issue 1 (Codegen Bug)** - Medium priority, has workaround, but indicates deeper optimization issues
3. **Issue 3 (LLC -O1 crash)** - Low priority, would be nice for debugging

---

## Test Case

Build regal for SLOW-32 and run:
```bash
cd /ztank/secret/sdennis/regal
./src/regal/build-slow32.sh
~/slow-32/tools/emulator/slow32 ./src/regal/regal.s32x stats
```

Without the memory barrier fix in csv.c, CSV parsing fails silently.
With the 1MB heap limit, lines.csv (1.4MB) fails to load.
