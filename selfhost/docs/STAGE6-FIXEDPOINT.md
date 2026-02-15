# Stage 6: Verification and Fixed-Point Proof

> **V2 cross-reference:** V2 has three fixed-point stages: **Stage 9** (Subset C), **Stage 13** (Full C), and **Stage 16** (Optimized) in [BOOTSTRAP-V2.md](BOOTSTRAP-V2.md). Each layer proves Gen2 == Gen3 independently.

## Goal

Prove that the self-hosted toolchain is correct by demonstrating a **fixed point**: the toolchain can compile itself and produce an identical binary.

## The Fixed-Point Test

A compiler reaches a fixed point when:

```
compiler₁ compiles compiler_source → compiler₂
compiler₂ compiles compiler_source → compiler₃
compiler₂ == compiler₃  (bit-for-bit identical)
```

If `compiler₂` and `compiler₃` are identical, the compiler is self-consistent — it produces the same output regardless of which copy compiled it.

## Procedure

### Step 1: Build Generation 1 (Gen1)

Use the stage 4 bootstrap compiler (running in Forth or cross-compiled by LLVM) to compile the toolchain:

```
bootstrap_cc  compiles  slow32asm.c → gen1/slow32asm.s32x
bootstrap_cc  compiles  s32-ld.c    → gen1/s32-ld.s32x
bootstrap_cc  compiles  cc.c        → gen1/cc.s32x
```

### Step 2: Build Generation 2 (Gen2)

Use the Gen1 toolchain to rebuild itself:

```
gen1/cc.s32x        compiles  slow32asm.c → gen2/slow32asm.s32x
gen1/slow32asm.s32x assembles ...
gen1/s32-ld.s32x    links     ...         → gen2/slow32asm.s32x

gen1/cc.s32x        compiles  s32-ld.c    → gen2/s32-ld.s32x
gen1/cc.s32x        compiles  cc.c        → gen2/cc.s32x
```

### Step 3: Build Generation 3 (Gen3)

Use the Gen2 toolchain to rebuild itself again:

```
gen2/cc.s32x        compiles  slow32asm.c → gen3/slow32asm.s32x
gen2/slow32asm.s32x assembles ...
gen2/s32-ld.s32x    links     ...         → gen3/slow32asm.s32x

gen2/cc.s32x        compiles  s32-ld.c    → gen3/s32-ld.s32x
gen2/cc.s32x        compiles  cc.c        → gen3/cc.s32x
```

### Step 4: Compare Gen2 and Gen3

```bash
diff gen2/slow32asm.s32x gen3/slow32asm.s32x  # must be identical
diff gen2/s32-ld.s32x    gen3/s32-ld.s32x     # must be identical
diff gen2/s32-ar.s32x    gen3/s32-ar.s32x     # must be identical
diff gen2/slow32dis.s32x gen3/slow32dis.s32x  # must be identical
diff gen2/slow32dump.s32x gen3/slow32dump.s32x # must be identical
diff gen2/cc.s32x        gen3/cc.s32x         # must be identical
```

If all comparisons pass, the toolchain has reached a fixed point.

**Note:** Gen1 and Gen2 will likely differ because Gen1 was compiled by a different compiler (the bootstrap). But Gen2 and Gen3 must match because they were compiled by identical compilers.

## Why Gen1 != Gen2

The bootstrap compiler and the self-hosted compiler may generate different code for the same source because:

- Different register allocation strategies
- Different instruction selection
- Different optimization levels
- Different string table ordering
- Different section layout

This is expected and not a problem. What matters is that from Gen2 onward, the output stabilizes.

## Additional Verification

### Full Test Suite

Run the complete regression test suite using the self-hosted toolchain:

```bash
cd regression
# Replace cross-compiler with self-hosted compiler in test runner
export CC="emulator gen2/cc.s32x"
export AS="emulator gen2/slow32asm.s32x"
export LD="emulator gen2/s32-ld.s32x"
./run-tests.sh
```

All 23 regression tests should pass.

### Cross-Verification

Compile a known program with both LLVM and the self-hosted compiler, then compare runtime behavior:

```bash
# LLVM path
clang -target slow32-unknown-none -O0 test.c -S -emit-llvm -o test.ll
llc -mtriple=slow32-unknown-none test.ll -o test.s
slow32asm test.s test.s32o
s32-ld crt0.s32o test.s32o libc.s32a -o test-llvm.s32x

# Self-hosted path
emulator gen2/cc.s32x < test.c > test-self.s
emulator gen2/slow32asm.s32x test-self.s test-self.s32o
emulator gen2/s32-ld.s32x crt0.s32o test-self.s32o libc.s32a -o test-self.s32x

# Compare behavior (not binaries — those will differ)
emulator test-llvm.s32x > output-llvm.txt
emulator test-self.s32x > output-self.txt
diff output-llvm.txt output-self.txt
```

### Diverse Double-Compiling (DDC)

For maximum trust, use a *third* independent compiler to verify. If a different compiler (e.g., a hand-written interpreter, or a compiler from a different project) produces the same Gen2 binary from the same source, it provides strong evidence against Thompson-style trust attacks.

```
CompilerA compiles cc.c → cc_A
CompilerB compiles cc.c → cc_B       (CompilerB is independent)
cc_A compiles cc.c → cc_AA
cc_B compiles cc.c → cc_BA
cc_AA == cc_BA                        (fixed point from different seeds)
```

## Thompson Trust Discussion

Ken Thompson's "Reflections on Trusting Trust" (1984) showed that a compiler can contain a self-reproducing backdoor invisible in source code. The bootstrap chain addresses this:

1. **Stage 0 (emulator)**: ~300 lines of C, fully auditable
2. **Stage 1 (Forth kernel)**: Hand-assembled machine code, fully auditable
3. **Stage 2 (assembler)**: Forth source, fully auditable
4. **Stage 3 (linker)**: Forth source, fully auditable
5. **Stage 4 (C compiler)**: Forth source, fully auditable

No binary in the chain is taken on trust — every stage's source is readable and the translation to the next stage is mechanical and verifiable. The only trust assumption is the host CPU executing the emulator correctly.

The fixed-point test proves internal consistency. DDC proves independence from the bootstrap path. Together, they provide strong assurance that the toolchain does what its source says.

## Success Criteria

The bootstrap is complete when:

1. Gen2 == Gen3 (fixed point reached)
2. All regression tests pass with self-hosted toolchain
3. The self-hosted toolchain can build the Forth kernel, SLOW BASIC, and all examples
4. Every stage's source code is available and auditable
5. The entire bootstrap can be reproduced from scratch on any machine with a C compiler (for the emulator)
