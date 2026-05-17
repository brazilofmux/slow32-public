# Stage01 — A Teaching Guide

This document is for readers who want to understand **what Stage01 is, why it exists, and what is actually hard about it**, whether you are studying the bootstrap in a class, doing a self-guided deep dive, or preparing to work on later stages.

It is deliberately not a line-by-line code walkthrough. The four Forth programs (`asm.fth`, `ar.fth`, `link.fth`, `cc.fth`) total ~7.7k lines. The value is in the *mental models* and in the carefully chosen test corpus, not in reading every word of Forth.

---

## 1. Why Stage01 Exists in the Bootstrap Story

Stage00 gives you a minimal emulator (`s32-emu`) and (via the Forth kernel) a way to run Forth on the bare SLOW-32 machine.

Stage01's job is to **produce the first real toolchain artifacts from source** using only that Forth environment:

- `asm.fth`  — turns `.s` into `.s32o` (object files with relocations and a symbol table)
- `ar.fth`   — creates and manipulates `.s32a` archives (static libraries)
- `link.fth` — turns collections of `.s32o` + `.s32a` into a final `.s32x` executable
- `cc.fth`   — the first C compiler; it emits the assembly that `asm.fth` can consume

After Stage01 you have a complete (if limited) C toolchain that runs on the machine it targets. Stage02 then rewrites the same four tools in a tiny C subset, at which point the Forth versions can be retired.

The "selfhost-kernel regeneration gate" (`run-selfhost-kernel.sh`) is the dramatic demonstration: we assemble and link `crt0_minimal.s`, `mmio_minimal.s`, and the entire `forth/kernel.s` using only the Stage01 tools, then boot the result. If that works, the layer is real.

---

## 2. The Four Tools — Mental Models

### Assembler (`asm.fth`)
- Two-pass design (collect symbols and sizes on pass 1, emit code + record relocations on pass 2).
- "Bootstrap Core" instruction set — deliberately narrow and consistent; no RISC-V-style aliases that would complicate the parser.
- Relocation types that matter: `REL-HI20`/`REL-LO12` pairs, `REL-JAL`, `REL-CALL`, plain `REL-32`.
- Symbol table is parallel arrays (name pointer, length, section, offset). No fancy hash tables — this is Forth.
- Output is a `.s32o` that the linker (not the assembler) is responsible for turning into final addresses.

Key subtlety: the assembler must get the *size* of every instruction right on pass 1 even when it does not yet know the final value of a symbol. That is why `la` / `call` / `li` large-immediate pseudo-instructions exist and why long-branch lowering appears later in the C compiler.

### Linker (`link.fth`)
- Reads multiple `.s32o` files and zero or more `.s32a` archives.
- Builds a global symbol table, resolves references, applies relocations, and writes a single `.s32x` with the proper header (W^X flags, optional MMIO base, stack size, etc.).
- The order `crt0` first, then program objects, then archives is significant (the kernel regeneration test depends on it).
- Relocation application is where HI20/LO12 pairs, JAL targets, and PC-relative forms are finally turned into concrete bits.

### Archiver (`ar.fth`)
- A faithful but minimal implementation of the `.s32a` format (magic, member table, symbol table, string table, member data).
- Supports the operations later stages actually need: create, list, extract, replace, delete, and the "verbose" variants used by build scripts.
- The symbol table in the archive is what lets the linker pull in only the members that satisfy unresolved references.

### C Compiler (`cc.fth`) — the 4.2 kLOC elephant
- **Accumulator machine**: every expression leaves its result in `r1`. `r2` is the only scratch register the compiler itself uses. Everything else (arguments, locals, temporaries) is handled via the stack or frame pointer.
- Very limited type system (int, char, unsigned variants, pointers, arrays, structs are barely there). Enough for the subset needed to write `s32-as.c`, `s32-ar.c`, `s32-ld.c`, and the early self-hosting C tools.
- Preprocessor is *not* a separate pass; it is integrated into the lexer in a way that makes certain macro-expansion edge cases (the "idiom" tests) surprisingly tricky.
- Long-branch lowering: because the assembler only guarantees short branches for conditional jumps, the compiler must detect when the target is too far and emit the classic `bXX skip; j target; skip:` pattern, counting how many such transformations it performed.

The accumulator model is the single most important thing to keep in your head when reading the code the compiler emits. Almost every expression sequence ends with a value in `r1`.

---

## 3. The Test Corpus — What Each Bucket Actually Proves

After the reorganization the layout is:

```
tests/
├── conformance/
│   ├── baseline/          (test1.c … test9.c)
│   ├── subset/            (13 files — the contract)
│   ├── subset-idioms/     (62 files — the "we actually needed this" cases)
│   └── known-gaps/
└── historical-debug/      (the bisect and reloc families — archaeology only)
```

### The 13 Subset Tests
These are the minimum that must pass for Stage01 to be considered a working C compiler for the bootstrap. They cover strings, pointer arithmetic, control flow, function calls with many arguments, globals, logic operators, etc.

`subset13_long_branch.c` is the one that forces the compiler's long-branch lowering logic to fire (the function body is deliberately padded with hundreds of increments so the backward branch target is far away).

### The 62 Subset-Idiom Tests
These are the crown jewels for anyone trying to understand what was actually difficult.

Each file name is a clue:

- `idiom29_reloc_patch_bitpack.c`, `idiom30_nested_reloc_pair_scan.c`, `idiom31_hi20_lo12_rounding.c` — the linker and assembler relocation machinery had to be exactly right.
- `idiom32_branch_reloc_pack.c`, `idiom33_jal_reloc_pack.c` — branch and call sites with relocations are special.
- `idiom34_archive_delete_shape.c`, `idiom35_archive_header_bounds.c` — the archiver's internal bookkeeping had off-by-one and size-limit bugs that only appeared on real workloads.
- Many of the `pp_*` and `ptr_*` files are macro-expansion or pointer-arithmetic shapes that the integrated preprocessor + parser in `cc.fth` got wrong the first N times.

When you look at one of these files and it feels "too clever" or "why would anyone write this?", the answer is almost always: "because the previous version of the compiler or linker produced wrong code or crashed on exactly this pattern when we tried to compile the real Stage02 tools."

### Historical Debug Material
Everything under `historical-debug/` was created during the port of the assembler/linker from an earlier version or while chasing a specific bug (LO12 packing, archive member replacement, long branches through certain instruction sequences, etc.). They are kept so that a future regression in those obscure paths can be detected, but they are not part of the recommended study surface.

---

## 4. A Small Guided Walk-Through

Take `subset05_call_args.c` (or any of the small subset tests) and do this:

1. Run the compiler by hand:
   ```
   selfhost/stage01/run-test-cc.fth subset05_call_args.c /tmp/out.s
   ```
2. Look at the generated assembly. Notice how every expression result lands in `r1`, how arguments are moved from `r1` into `a0`/`a1`… before the call, and how the call itself is usually a `call` pseudo (which the assembler will turn into the right HI20/LO12 or direct JAL sequence).
3. Assemble the `.s` with the Stage01 assembler and produce a `.s32o`.
4. Link it (with `crt0_minimal.s32o`) and run it under the emulator. The fact that it produces the expected output proves that the entire pipeline — compiler register convention, assembler relocation emission, linker resolution — is consistent.

Doing this for two or three different tests (one with interesting pointer arithmetic, one with a long backward branch, one with a macro that expands to a comma expression) is the fastest way to internalize what the Stage01 layer actually guarantees.

---

## 5. What Stage01 Deliberately Does *Not* Do

- It is not a standards-conforming C compiler. Many ISO features are absent or only partially implemented.
- Floating point, `long long`, full struct layout, `switch` with jump tables, etc. all come much later (or never, in the Forth-hosted version).
- The optimizer is nonexistent. The emitted code is straightforward accumulator style; later self-hosting compilers add HIR, SSA, instruction selection, and a real register allocator.
- Error reporting is minimal. When something goes wrong you usually just get a Forth backtrace or a "FAILED" message.

These limitations are features for the bootstrap: the smaller the compiler, the sooner you can write it in the previous stage's language and move on.

---

## 6. How to Use This Layer When Teaching

- **Week 1–2 goal**: Get students to successfully run the kernel regeneration gate and explain, at a high level, why each of the four tools is necessary.
- **Deeper exercise**: Pick 3–5 idiom tests, have students predict what will go wrong if the assembler or linker has a particular off-by-one or relocation bug, then actually break the tool and watch the test fail.
- **"Compare and contrast"**: Once Stage02 exists, compare the C implementation of `s32-as.c` / `s32-ld.c` against the Forth versions. The C versions are still simple, but they have real data structures and the contrast is illuminating.
- **Archaeology project** (advanced): Give a student one of the historical bisect files + the git history around the V2 relocation work and ask them to reconstruct what the bug was.

---

## 7. Next Steps for the Curious Reader

- Read the top-level `selfhost/docs/BOOTSTRAP.md` for the full multi-stage plan.
- Look at `selfhost/stage02/` — the first C rewrite of the same four tools. Seeing what becomes easier and what stays painful is the real payoff of having lived through Stage01.
- If you want to modify `cc.fth`, the `run-subset-conformance.sh` + `run-subset-gap-scan.sh` pair is your friend. Keep the subset + idioms suites green.

Stage01 is not pretty, but it is one of the cleanest "first self-hosting C toolchain" artifacts that exists in public. The test corpus, especially the idiom collection, is the part most worth studying deeply.

If you internalize the accumulator model, the reason long branches must be lowered in the compiler, and the pain that produced the 62 idiom tests, you will understand why the later, more sophisticated self-hosting compilers were worth building.