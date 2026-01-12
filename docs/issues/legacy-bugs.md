# SLOW-32 LLVM Backend Bug List

## Bug #1: Function Arguments Clobbered by llvm.lifetime Intrinsics

**Status:** Open  
**Severity:** Critical  
**Affects:** All varargs functions, any function using llvm.lifetime intrinsics  

### Description
The LLVM backend incorrectly reuses argument registers (r3-r10) for llvm.lifetime.start/end calls without first preserving the original argument values. This causes function arguments to be overwritten before they can be used.

### Reproduction Steps

1. Create test file `test_varargs_bug.c`:
```c
#include <stdarg.h>

int sum(int count, ...) {
    va_list args;
    va_start(args, count);
    
    int total = 0;
    for (int i = 0; i < count; i++) {
        total += va_arg(args, int);
    }
    
    va_end(args);
    return total;
}

int main() {
    return sum(3, 10, 20, 30);  // Should return 60
}
```

2. Compile and examine assembly:
```bash
clang-19 -S -emit-llvm -O1 test_varargs_bug.c -o test_varargs_bug.ll
./tools/compiler-deprecated/slow32-compile test_varargs_bug.ll > test_varargs_bug.s
grep -A5 "^sum:" test_varargs_bug.s
```

3. Observe the bug at line ~12 of the sum function:
```asm
sum:
    addi sp, sp, -64
    stw sp+0, lr
    stw sp+4, fp
    add fp, sp, r0
    ...
    addi r3, r0, 24      # BUG: Overwrites first arg (count) with 24
    add r4, r20, r0
    jal llvm.lifetime.start.p0
```

### Expected Behavior
The function should preserve r3 (containing `count` parameter) before using it for the llvm.lifetime.start call.

### Actual Behavior
- r3 is overwritten with 24 (size for lifetime intrinsic)
- Original `count` value is lost
- Function uses 24 instead of actual count
- Causes infinite loops or incorrect results

### Workaround
None currently. The bug affects all varargs functions and potentially other functions that use stack allocations with lifetime markers.

### Root Cause Analysis
The issue appears to be in the instruction selection or register allocation phase where the backend doesn't recognize that r3-r10 contain live function arguments that need to be preserved before being reused for other purposes.

---

## Bug #2: Printf with Format String Loop

**Status:** Open  
**Severity:** High  
**Related to:** Bug #1  

### Description
Printf implementation enters infinite loop because the format string pointer (first argument) gets overwritten.

### Reproduction Steps

1. Create test file `test_printf_bug.c`:
```c
void debug_char(char c);

void printf(const char *format, ...) {
    // Implementation that uses va_list
    // See test_varargs.c for full code
}

int main() {
    printf("Hello, World!\n");
    return 0;
}
```

2. Compile and run:
```bash
clang-19 -S -emit-llvm -O1 test_printf_bug.c -o test_printf_bug.ll
./tools/compiler-deprecated/slow32-compile test_printf_bug.ll > test_printf_bug.s
./assembler/slow32asm test_printf_bug.s test_printf_bug.s32o
# Link with runtime
./linker/s32-ld -o test_printf_bug.s32x runtime/crt0.s32o test_printf_bug.s32o debug_char.s32o runtime/intrinsics.s32o
./emulator/slow32 test_printf_bug.s32x
# Results in infinite loop
```

### Root Cause
Same as Bug #1 - the format string pointer in r3 is overwritten with 24, causing the function to read from address 24 instead of the actual string, leading to an infinite loop.

---

## Test Suite for Verification

Save as `test_bugs.sh`:
```bash
#!/bin/bash

echo "Testing Bug #1: Simple varargs sum"
cat > test_sum.c << 'EOF'
#include <stdarg.h>
void debug_char(char c);

int sum(int count, ...) {
    va_list args;
    va_start(args, count);
    int total = 0;
    for (int i = 0; i < count; i++) {
        total += va_arg(args, int);
    }
    va_end(args);
    return total;
}

int main() {
    int result = sum(3, 10, 20, 30);
    if (result == 60) {
        debug_char('O'); debug_char('K'); debug_char('\n');
    } else {
        debug_char('E'); debug_char('R'); debug_char('R'); debug_char('\n');
    }
    return 0;
}
EOF

clang-19 -S -emit-llvm -O1 test_sum.c -o test_sum.ll
./tools/compiler-deprecated/slow32-compile test_sum.ll > test_sum.s
./assembler/slow32asm test_sum.s test_sum.s32o
./linker/s32-ld -o test_sum.s32x runtime/crt0.s32o test_sum.s32o debug_char.s32o runtime/intrinsics.s32o
echo "Expected: OK"
echo -n "Actual: "
timeout 1 ./emulator/slow32 test_sum.s32x 2>/dev/null || echo "TIMEOUT/ERROR"
```

---

---

## Bug #3: Static Variables Not Generated in Assembly

**Status:** Open  
**Severity:** Medium  
**Affects:** Functions with static local variables  

### Description
The LLVM backend doesn't generate .data or .bss sections for static local variables. References to static variables are generated but the actual storage is missing.

### Reproduction Steps

1. Create test file `test_static.c`:
```c
char* get_buffer() {
    static char buffer[256];
    return buffer;
}

int get_counter() {
    static int counter = 0;
    return ++counter;
}
```

2. Compile:
```bash
clang-19 -S -emit-llvm -O1 test_static.c -o test_static.ll
./tools/compiler-deprecated/slow32-compile test_static.ll > test_static.s
grep "buffer\|counter" test_static.s
# Shows references like: lui r24, %hi(get_buffer.buffer)
# But no actual .data or .bss definition
```

### Expected Behavior
Should generate:
```asm
.bss
get_buffer.buffer: .space 256

.data  
get_counter.counter: .word 0
```

### Actual Behavior
- References to static variables are generated using %hi/%lo
- No .data or .bss sections created
- Linker fails with undefined symbol errors

### Workaround
Avoid static local variables. Use stack allocation or pass buffers as parameters.

---

## Bug #4: Incorrect PHI Node Handling in Loops with Pointer Arithmetic

**Status:** Open  
**Severity:** High  
**Affects:** Inlined loops with pointer increments and PHI nodes  

### Description
When the optimizer inlines functions with loops that use pointer arithmetic, the LLVM backend incorrectly handles PHI nodes, causing the loop to repeatedly process the same value instead of advancing through data.

### Reproduction Steps

1. Create test file `test_loop_phi.c`:
```c
void debug_char(char c);

void puts(const char *str) {
    while (*str) {
        debug_char(*str++);
    }
}

int main() {
    puts("Hello\n");
    return 0;
}
```

2. Compile with optimization:
```bash
clang-19 -S -emit-llvm -O1 test_loop_phi.c -o test_loop_phi.ll
./tools/compiler-deprecated/slow32-compile test_loop_phi.ll > test_loop_phi.s
```

3. Run and observe:
```bash
./assembler/slow32asm test_loop_phi.s test_loop_phi.s32o
./linker/s32-ld -o test_loop_phi.s32x runtime/crt0.s32o test_loop_phi.s32o debug_char.s32o runtime/intrinsics.s32o
./emulator/slow32 test_loop_phi.s32x
# Outputs: "HHHHHH" instead of "Hello\n"
```

### Root Cause
The LLVM IR shows correct PHI nodes:
```llvm
%2 = phi i8 [ %6, %1 ], [ 72, %0 ]  ; Character PHI
%3 = phi i64 [ %4, %1 ], [ 0, %0 ]  ; Index PHI
```

But the generated assembly doesn't properly update the PHI values in the loop. It keeps using the initial value (72 = 'H') instead of loading subsequent characters.

### Workaround
Compile with `-O0` to avoid inlining, or modify code to prevent optimization.

---

---

## Bug #5: Getelementptr Not Generating Pointer Arithmetic

**Status:** Open  
**Severity:** CRITICAL  
**Affects:** ALL pointer arithmetic, array indexing, struct field access  

### Description
The LLVM backend is not generating ADD instructions for `getelementptr` operations. This completely breaks pointer arithmetic, making it impossible to iterate through arrays or strings.

### Reproduction Steps

1. Create test file `test_ptr_arith.c`:
```c
void debug_char(char c);

int main() {
    char str[] = "ABC";
    char *p = str;
    
    debug_char(*p);     // Should print 'A'
    p = p + 1;          // Increment pointer
    debug_char(*p);     // Should print 'B'
    p = p + 1;          // Increment pointer
    debug_char(*p);     // Should print 'C'
    
    return 0;
}
```

2. Compile and run:
```bash
clang-19 -S -emit-llvm -O0 test_ptr_arith.c -o test_ptr_arith.ll
./tools/compiler-deprecated/slow32-compile test_ptr_arith.ll > test_ptr_arith.s
# Check assembly - no ADD instruction for pointer increment!
grep -A3 "p = p + 1" test_ptr_arith.s
```

3. Result:
```asm
ldw r17, r13+0      # Load pointer
add r18, r17, r0    # Just copies pointer, NO INCREMENT!
stw r13+0, r18      # Store same pointer back
```

### Expected Behavior
For `getelementptr inbounds i8, ptr %6, i64 1`:
```asm
ldw r17, r13+0      # Load pointer
addi r18, r17, 1    # Add 1 to pointer
stw r13+0, r18      # Store incremented pointer
```

### Impact
- String operations broken (can't iterate through strings)
- Array indexing broken (can't access array[i] for i > 0)
- Struct field access likely broken
- ++ and -- operators don't work
- Any loop over arrays/strings fails

### Root Cause
The LLVM backend's handling of `getelementptr` instruction is missing the actual arithmetic. It's likely just returning the base pointer without adding the offset.

---

## Notes for Fix

The fix likely needs to be in one of these areas:

1. **SLOW32ISelLowering.cpp**: Ensure function arguments are copied to virtual registers before any calls
2. **SLOW32CallingConv.td**: May need to mark argument registers as needing preservation
3. **SLOW32RegisterInfo.cpp**: Ensure r3-r10 are marked as live-in for functions with parameters

The key insight is that llvm.lifetime.start is being lowered to a regular function call that uses r3 for its first parameter, but the backend doesn't realize r3 already contains a live value (the function's first argument).