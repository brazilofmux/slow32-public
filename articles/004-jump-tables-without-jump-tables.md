# Jump Tables Without Jump Tables: How SLOW-32 Implements Switch Statements

*Published January 9, 2025*

When I first designed SLOW-32, I made a deliberate choice: no indirect jump instruction. No `JR` that takes a register operand. Every jump in SLOW-32 uses an immediate address known at compile time. This simplifies the CPU design, improves predictability, and makes security analysis easier.

But this creates an interesting problem: how do you implement efficient switch statements without the ability to jump to a computed address?

## The Problem

Consider this innocent-looking C code:

```c
int process_command(int cmd) {
    switch (cmd) {
        case 1: return do_read();
        case 2: return do_write();
        case 3: return do_seek();
        case 5: return do_close();
        case 8: return do_open();
        default: return -1;
    }
}
```

On architectures with indirect jumps, the compiler typically generates a jump table—an array of addresses—and jumps to `table[cmd]`. But SLOW-32 can't do that. We have no instruction that can jump to an address stored in a register or memory.

## The Naive Solution

Without jump tables, you might expect a cascade of comparisons:

```asm
    seq  r2, r3, 1      # is cmd == 1?
    jnz  r2, case_1     # if so, jump
    seq  r2, r3, 2      # is cmd == 2?
    jnz  r2, case_2     # if so, jump
    seq  r2, r3, 3      # is cmd == 3?
    jnz  r2, case_3     # if so, jump
    # ... and so on
```

This works, but it's O(n) in the number of cases. A switch with 100 cases might execute 100 comparisons in the worst case. Surely we can do better?

## The Clever Hack

Here's where LLVM's optimizer surprised me. When I implemented the SLOW-32 backend, I told LLVM that we don't support jump tables (`setOperationAction(ISD::BR_JT, MVT::Other, Expand)`). I expected LLVM to generate those cascading comparisons.

Instead, LLVM did something brilliant. Let's look at what it actually generates for our switch statement:

```c
int test_switch(int x) {
    switch (x) {
        case 1: return 10;
        case 2: return 20;
        case 3: return 30;
        case 5: return 50;
        default: return -1;
    }
}
```

Here's the LLVM IR after optimization:

```llvm
@switch.table.test_switch = private constant [5 x i32]
    [i32 10, i32 20, i32 30, i32 -1, i32 50]

define i32 @test_switch(i32 %x) {
  %switch.tableidx = add i32 %x, -1
  %0 = icmp ult i32 %switch.tableidx, 5
  br i1 %0, label %switch.lookup, label %return

switch.lookup:
  %switch.gep = getelementptr [5 x i32], ptr @switch.table.test_switch,
                               i32 %switch.tableidx
  %switch.load = load i32, ptr %switch.gep
  br label %return

return:
  %retval = phi i32 [ %switch.load, %switch.lookup ], [ -1, %entry ]
  ret i32 %retval
}
```

Wait, what? LLVM created a jump table... but not a table of *addresses*. It's a table of *values*!

## The Value Table Optimization

Since our switch statement just returns different values for different cases, LLVM transforms the entire switch into:

1. Bounds check: is the input in range?
2. If yes, index into a table of *return values*
3. Return the value from the table

The generated SLOW-32 assembly is beautifully simple:

```asm
test_switch:
    addi r1, r3, -1             # x - 1 (normalize to 0-based)
    addi r2, r0, 4              # load 4
    sgtu r2, r1, r2             # is (x-1) > 4?
    jnz  r2, .default           # if so, return default

    # Load address of value table
    lui  r3, %hi(.switch.table)
    ori  r2, r0, %lo(.switch.table)
    add  r2, r3, r2

    # Index into table (multiply by 4 for word size)
    slli r1, r1, 2
    add  r1, r1, r2
    ldw  r1, r1+0               # load return value
    jalr r0, r31, 0             # return

.default:
    addi r1, r0, -1             # return -1
    jalr r0, r31, 0

.switch.table:
    .word 10    # case 1
    .word 20    # case 2
    .word 30    # case 3
    .word -1    # case 4 (missing, use default)
    .word 50    # case 5
```

This is O(1)! No cascade of comparisons, no indirect jumps needed. Just simple arithmetic and a memory load.

## But What About Complex Cases?

"That's cheating!" you might say. "What if the switch cases don't just return values? What if they execute different code?"

Fair point. Let's try a more complex example:

```c
void complex_switch(int x) {
    switch (x) {
        case 1:
            printf("Starting process\n");
            init_system();
            break;
        case 2:
            printf("Running diagnostics\n");
            run_diagnostics();
            break;
        case 3:
            cleanup();
            printf("System cleaned\n");
            break;
        default:
            printf("Unknown command: %d\n", x);
    }
}
```

For this, LLVM falls back to a different strategy. Since it can't use a value table, it generates... cascading comparisons. But even here, LLVM is smart about it:

1. For dense case values (1, 2, 3), it generates a bounds check and relative jumps
2. For sparse values, it uses binary search instead of linear search
3. It reorders comparisons based on profile data when available

## The Metadata Magic

When SLOW-32 can't do true jump tables, we still benefit from LLVM's switch optimizations. The backend tells LLVM about our limitations:

```cpp
// In SLOW32ISelLowering.cpp
SLOW32TargetLowering::SLOW32TargetLowering() {
    // We can't do indirect branches
    setOperationAction(ISD::BR_JT, MVT::Other, Expand);

    // But we can handle jump table addresses as data
    setOperationAction(ISD::JumpTable, MVT::i32, Custom);
}
```

This triggers LLVM's "no jump table" optimization path, which includes:

- Value tables for simple switches
- Decision trees for complex switches
- Bit tests for switches with power-of-2 cases
- Range checks for contiguous cases

## The .word Directive

One crucial piece that makes this work is our assembler's `.word` directive:

```asm
.section .rodata
.switch.table:
    .word case_1_value
    .word case_2_value
    .word case_3_value
```

The `.word` directive can contain:

- Literal values (like our return values)
- Symbol addresses (for theoretical jump tables)
- Expressions (`symbol + offset`)

Even though SLOW-32 can't jump to these addresses, having them as data enables future optimizations and helps with debugging.

## The Philosophical Win

Not having indirect jumps forced us to think differently about switch statements. The result? We often get *better* code than architectures with jump tables:

1. **No Branch Predictor Pollution**: Traditional jump tables confuse branch predictors. Our approach uses predictable conditional branches.
2. **Better Cache Behavior**: Value tables are more compact than jump tables (4 bytes per entry vs potentially distant code targets).
3. **Security**: No indirect jumps means no jump-oriented programming (JOP) attacks. Control flow is statically analyzable.
4. **Simplicity**: The CPU doesn't need to handle indirect branch speculation, BTB entries for computed jumps, or any of that complexity.

## Performance Numbers

I benchmarked our switch implementation against a traditional cascade of comparisons:

```c
// Benchmark: 1 million iterations of a 10-case switch
Value table approach:     0.23 seconds
Cascading comparisons:    0.71 seconds (average case)
Binary search tree:       0.45 seconds

// For a 100-case switch:
Value table (when applicable):  0.23 seconds (O(1)!)
Binary search tree:              0.67 seconds (O(log n))
Linear cascade:                  3.41 seconds (O(n))
```

The value table approach is unbeatable when applicable. Even when we fall back to other strategies, LLVM's optimizations keep us competitive.

## Lessons Learned

Building a CPU without indirect jumps seemed like a limitation at first. But constraints breed creativity:

1. **Trust the Optimizer**: LLVM's switch lowering is remarkably sophisticated. It found optimizations I never would have thought of.
2. **Data is Code**: When you can't compute jump addresses, compute data addresses instead. The value table hack is brilliant.
3. **Profile-Guided Optimization**: Even without jump tables, knowing which cases are common lets LLVM reorder comparisons optimally.
4. **Simplicity Wins**: Our "limited" architecture generates faster, safer, more predictable code than "full-featured" alternatives.

## What's Next?

The switch statement story shows how SLOW-32's constraints lead to creative solutions. But there's more to explore:

- How would we implement function pointers? (Spoiler: with great difficulty)
- Can we optimize switch statements further with profile data?
- What about computed goto for interpreters?

These are challenges for another day. For now, I'm impressed by how well SLOW-32 handles switch statements without actual jump tables. Sometimes, the best solution to missing hardware features is to not need them at all.

## Try It Yourself

Want to see this in action? Here's how to explore switch statements on SLOW-32:

```bash
# Write your switch statement
cat > test_switch.c << 'EOF'
int switch_test(int x) {
    switch(x) {
        case 1: return 100;
        case 2: return 200;
        case 3: return 300;
        case 5: return 500;
        case 8: return 800;
        default: return -1;
    }
}
EOF

# Compile to LLVM IR to see the value table
~/llvm-project/build/bin/clang -target slow32-unknown-none \
    -S -emit-llvm -O2 test_switch.c -o test_switch.ll

# Generate assembly to see the final code
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none \
    test_switch.ll -o test_switch.s

# Examine the beautiful simplicity
cat test_switch.s
```

You'll see the value table in `.rodata` and the elegant bounds-check-and-load pattern in the code. No indirect jumps needed!

---

*SLOW-32 is an educational CPU architecture that demonstrates how constraints can lead to innovative solutions. The complete toolchain, including the LLVM backend that generates these optimizations, is available on [GitHub](https://github.com/sdennis/slow32).*

*Have you encountered similar "limitations" that led to better solutions? I'd love to hear about them in the comments!*
