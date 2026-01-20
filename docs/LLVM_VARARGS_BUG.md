# SLOW-32 LLVM Backend Bug: Incorrect Vararg Register Spill Address Calculation

## Summary

The SLOW-32 LLVM backend incorrectly uses `ORI` (bitwise OR) instead of `ADDI` (add immediate) when computing consecutive stack slot addresses for spilling vararg registers. This causes data corruption when the base address has certain bit patterns.

## Symptoms

- 64-bit values passed through varargs are corrupted
- The corruption is intermittent, depending on stack frame alignment
- Affects functions with mixed pointer/int fixed parameters before varargs
- Functions with uniform int-only fixed parameters often work correctly

## Reproduction

Compile and run this test:

```c
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>

typedef struct { int x; } Context;

// 6 fixed params with mixed types (ptr, int, ptr, ptr, int, ptr)
void mixed_6fixed(Context *ctx, int severity,
                  const char *check_name, const char *file,
                  int line_number, const char *fmt, ...)
{
    char buf[256];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    printf("Result: '%s'\n", buf);
}

int main() {
    Context ctx = {0};
    char code[] = "ABC";
    int64_t val = 123456789LL;

    // This produces WRONG output (corrupted value)
    mixed_6fixed(&ctx, 2, "check", NULL, 0, "%s: val %lld", code, (long long)val);
    // Expected: "Result: 'ABC: val 123456789'"
    // Actual:   "Result: 'ABC: val 268433984'" (or similar garbage)

    return 0;
}
```

## Root Cause Analysis

When the backend saves vararg registers r9 and r10 to the stack, it generates:

```asm
addi r6, fp, -8       # r6 = base of vararg save area
ori  r1, r6, 4        # BUG: trying to compute r6 + 4, but uses OR
stw r1+0, r10         # Store r10 at computed address
stw r6+0, r9          # Store r9 at base address
```

The problem: `ori r1, r6, 4` does NOT add 4 to r6. It performs bitwise OR.

- If `r6 = 0x0FFFFFE8` (bit 2 clear): `r6 | 4 = 0x0FFFFFEC` ✓ (happens to work)
- If `r6 = 0x0FFFFFC4` (bit 2 set): `r6 | 4 = 0x0FFFFFC4` ✗ (no change!)

When the OR is a no-op, r10 overwrites r9's slot, corrupting the vararg data.

### Correct Code Should Be

```asm
addi r6, fp, -8       # r6 = base of vararg save area
addi r1, r6, 4        # r1 = r6 + 4 (CORRECT: use add, not or)
stw r1+0, r10         # Store r10 at r6+4
stw r6+0, r9          # Store r9 at r6
```

## Where to Look

The bug is likely in one of these files in `llvm/lib/Target/SLOW32/`:

1. **SLOW32FrameLowering.cpp** - Frame setup code that spills vararg registers
2. **SLOW32ISelDAGToDAG.cpp** - Instruction selection that might fold frame offsets
3. **SLOW32ISelLowering.cpp** - Lowering of `ISD::VASTART` or frame index operations

Search for:
- Code that handles `FrameIndex` with small offsets
- Vararg register spilling logic
- Any use of `ORI` or `ISD::OR` for address computation

The pattern to find: anywhere that uses OR to "add" a small power-of-2 constant to an address, assuming alignment guarantees that don't actually hold.

## The Fix

Replace the instruction selection pattern that generates `ORI reg, 4` (or similar small constants) for frame offset calculations with `ADDI reg, 4`.

This might be:
1. A peephole optimization that incorrectly assumes alignment
2. An instruction selection pattern that matches `(add frameindex, 4)` but emits `ori`
3. Frame lowering code that manually constructs the wrong instruction

## Verification

After fixing, this test should produce correct output:

```bash
cd ~/slow-32
cat > /tmp/test-varargs.c << 'EOF'
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
typedef struct { int x; } Ctx;
void test(Ctx *c, int s, const char *n, const char *f, int l, const char *fmt, ...) {
    char buf[256];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    printf("%s\n", buf);
}
int main() {
    Ctx c = {0};
    int64_t v = 123456789LL;
    test(&c, 0, "x", NULL, 0, "val=%lld", (long long)v);
    return 0;
}
EOF
./scripts/compile.sh /tmp/test-varargs.c /tmp/test-varargs.s32x
./tools/emulator/slow32 /tmp/test-varargs.s32x
# Should print: val=123456789
```

## Related

This bug was discovered while investigating issues reported in `~/regal/docs/SLOW32_BUGS.md`. The "64-bit varargs" and "many fixed params varargs" bugs described there are both manifestations of this single underlying issue.
