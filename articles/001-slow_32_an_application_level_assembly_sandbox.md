# SLOW-32: Building an Application-Level Assembly Sandbox (Without the Hardware Baggage)

*What if you could explore compilers, linkers, and emulators against a clean, modern ISA—without dragging along decades of hardware quirks? SLOW-32 is a tiny, fast, and friendly CPU architecture designed for learning, tooling, and experimentation at the **application-level**. No real/protected/flat modes, no MMU, no paging, no virtual memory, no carry bit, no interrupts, no DMA—just the essentials for writing code and building tools that run blisteringly fast in a host-managed sandbox.*

---

## Why build a CPU with *less*?

Most of us who tinker with compilers or emulators end up fighting the past: flag registers, ring levels, segment tables, trap machinations, memory-mapped chaos, and “just-so” historical accidents. SLOW-32 cuts that away to focus on the things that matter when you’re learning or building toolchains:

- **Clarity over legacy:** Fixed-width instructions, a simple register file, and a straightforward memory model keep mental overhead low.
- **Predictability over complexity:** No hidden state machines. No interrupts. No undefined behavior surprises from quirky modes.
- **Speed through simplicity:** Tight loops can hit \~350M instr/sec on a modern host in the current emulator. Speed isn’t the goal—but it’s a welcome side effect.
- **Crash-safe by design:** If a program wedges the sandbox, you just spawn a fresh one. Read-only code/.rodata are shareable across instances (planned), keeping instances lightweight.

The result is a “just-enough” computer that’s ideal for compiler backends, emulators, static tooling, and pedagogy.

**Toolchain strategy.** Many of us start with flex/bison/ANTLR and discover the learning and implementation curve is steep. By leaning on an LLVM backend, SLOW-32 reduces total work and inherits world-class SSA/PHI, optimization, and codegen infrastructure. The backend includes sophisticated features like jump tables for switch statements, varargs support, and inline expansion for small memcpy/memset operations. When the host zeroes writable memory and registers between calls, each external request can run in its own short-lived SLOW-32 instance—so crashes are naturally contained.

---

## What this series will cover

**Today’s post**: the big picture—what SLOW-32 is, how the toolchain pieces fit, and what makes it different.

**Upcoming posts** (draft plan):

1. **Toolchain Deep Dive**: Clang adjustments → LLVM backend → assembler → linker → loaders.
2. **Emulator Internals**: Two emulators (fast vs. debug/trace), determinism, and crash semantics.
3. **ABI & Calling Conventions**: Registers, stack, prolog/epilog, codegen patterns, varargs handling.
4. **I/O & Host Facilities**: From `DEBUG` to MMIO ring buffers and a TRAP interface.
5. **Object Format & Metadata**: `.s32x` layout, the Interrupt Service Table as *file metadata*, and host policies (W^X).
6. **Performance Notes**: Branch shaping, PHI moves, and why simplicity wins.
7. **Extensions Roadmap**: Floating point, atomics, SIMD, and optional privileged/trap space.
8. **JIT Options**: TCC via QEMU and other JIT experiments.
9. **Standard Library**: Strings, memory, and host-facilities shims.

---

## The high-level design

### Application-level ISA

- **No CPU modes, no MMU, no paging, no interrupts**. The emulator is the OS—for exactly as long as your code is running.
- **One W^X line** protects code and `.rodata`; long-term this role is delegated to the host’s memory controller.
- **Interrupt Service Table (IST)** is not memory-mapped; it lives as *metadata* in the `.s32x` file. The emulator/host consults it rather than the guest dereferencing a vector table.
- **I/O is explicit**: today via a `DEBUG` instruction; later via **MMIO ring buffers** in shared memory or a **TRAP** interface for structured host calls.
- **Zero boot baggage**: no device drivers, GRUB, boot sectors, or BIOS. The reset vector *is* the program entry point.
- **Faults are metadata-routed**: e.g., divide-by-zero transfers control to a handler declared in the `.s32x` Interrupt Service Table—no hardware interrupts involved.
- **Null page policy**: address 0 is write-protected (reads are permitted).

### Memory model (simple and bounded)

- Code executes from an execute-only segment; data is read/write only. No self-modifying code.
- Address spaces are tightly bounded for speed and safety; that’s why the emulator is fast and crash recovery is trivial.
- Read-only code/data will be shareable across instances (planned) to make “spawn another one” cheap.

### Tooling end-to-end (today and tomorrow)

**Compilation Pipeline:**
- **C/C++ source** → Clang with `--target slow32-unknown-none`
- **Clang** → LLVM IR (with SLOW‑32 data layout and ABI)
- **LLVM IR** → SLOW‑32 assembly (via custom backend)
- **Assembly** → Object files `.s32o` (via assembler)
- **Objects** → Executable `.s32x` (via linker)

**Analysis & Execution Tools:**
- **Binary inspection**: `s32-objdump` (objects), `s32-exedump` (executables), `slow32dis` (disassembly)
- **Execution**: Fast emulator (~350M inst/sec) or debug emulator (tracing, breakpoints, watchpoints)

**Key Components:**
- **Native Clang target & LLVM backend**: Generate SLOW‑32 code that favors simple compare+branch patterns and register moves over flags.
- **Assembler & Linker**: Produce `.s32x` files that bundle code/data plus metadata (including the IST) for the host/emulator.
- **Two emulators**: a *fast* one for throughput and a *debug* one with comprehensive debugging features.
- **Analysis utilities**: Tools for inspecting binaries at every stage of the pipeline.

---

## The instruction set (at a glance)

SLOW-32 is influenced by RISC-V (but not opcode-compatible). It uses 32-bit fixed-width encodings with familiar R/I/B/J formats, a conventional 32-register file (r0 is hard-zero), and a straightforward branch+compare style in lieu of a carry flag.

**Highlights**

- **Arithmetic/logic**: `ADD`, `SUB`, `MUL`, `DIV`, `REM`, bitwise ops, shifts.
- **Compare & branch**: `SLT/SLTU/SEQ/SNE/SGT/SGTU/SLE/SLEU` + `BEQ/BNE/BLT/BGE/BLTU/BGEU`—no flags register to juggle.
- **Control flow**: `JAL`, `JALR` (link register in `r31`).
- **Memory**: `LDW/LDH/LDHU/LDB/LDBU` and `STW/STH/STB`.
- **Special**: `LUI`, `NOP`, `HALT`, `DEBUG`, `YIELD` (timing).
- **Register conventions**: `r0=zero`, `r1`=return value, `r3–r10`=args, `r11–r28`=saved, `r29`=sp, `r30`=fp, `r31`=lr.

> Implementation status, memory map, and cycle costs are tracked in the SLOW-32 Instruction Set Reference. (Link in the post footer.)

---

## Hello, world (DEBUG edition)

Here’s a minimal program that writes a string by feeding bytes to the `DEBUG` instruction. The assembler provides pseudo-ops like `li` and `mv` for readability.

```asm
        .section .rodata
msg:    .ascii  "Hello, SLOW-32!\n\0"

        .section .text
        .globl  _start
_start:
        li      r3, msg        # r3 = ptr to string
.loop:
        ldbu    r2, r3+0       # r2 = *r3 (unsigned byte)
        beq     r2, r0, .done  # if byte == 0, finish
        debug   r2             # emit character to host
        addi    r3, r3, 1      # r3++
        jal     r0, .loop      # jump (no link)
.done:
        halt
```

**What this demonstrates**

- Explicit, byte-addressed I/O via `DEBUG`—no syscalls or interrupts required.
- Clean compare+branch flow without condition flags.
- A tiny, readable code footprint with predictable execution.

In later posts we’ll re-target this example to **MMIO ring buffers** and a **TRAP** ABI for richer host services (timers, files, sockets, etc.).

---

## Emulator behavior and safety model

- **Fast by construction**: Bounded addressing and simple decode help the emulator run hot loops near the metal.
- **OK to crash**: The sandbox is disposable. If you hit UB, the host tears it down and spins up a fresh one.
- **Read-only sharing (planned)**: Code and `.rodata` will be shared between instances to make crash-recovery effectively free.

---

## Roadmap & open questions

**Near-term**

- Finish 64-bit support in the LLVM backend (shifts, add/sub with carry/borrow, and constants work; multiply, divide, and comparisons remain).
- **Floating-point**: choose scalar encoding, calling convention, and NaN/rounding policy.
- Finalize the **MMIO ring buffer** ABI and host shims.
- Define a **TRAP** calling convention (registers, error model, reentrancy rules).
- Add a **lightweight standard library** (memcpy/memset/strings) tuned for SLOW-32.

*Note:* Early versions handled SSA/PHI in custom passes; with LLVM in the loop, that work now lives upstream. The backend properly handles PHI nodes, producing efficient code for loops and conditionals.

**Mid-term**

- **Atomics & concurrency**: minimal ops for lock-free structures (or host-mediated primitives first?). Multithreading is possible, but we may prefer host-mediated concurrency or a multi-process model for simplicity and determinism.
- **SIMD**: explore 64/128-bit lanes for memcpy/hash/tiny-DSP.
- **Privileged/TRAP space**: optional, scoped instructions that remain emulator-friendly.
- **JIT experiments**: integrate **QEMU’s TCC** (Tiny Code Compiler) as an optional backend.

**Design questions for readers**

- Would you prefer a small FP extension first, or an atomics subset for concurrency?
- What host facilities would you want exposed early (timers? files? sockets? RNG?)

---

## FAQ (starter set)

**Why no carry bit?**\
Carry flags tangle the pipeline and complicate codegen. Comparisons (`SLTU`, etc.) and wider types handle carry/borrow semantics cleanly without global flags.

**Why no interrupts?**\
They’re great for hardware, not for clean emulators. We’ll reach the host via TRAPs or ring-buffer MMIO where *you* control the flow.

**How fast is it?**\
In tight loops, around \~350M instr/sec on a typical modern host in the current emulator. It’s not a benchmark contest—the design optimizes for simplicity, clarity, and determinism.

**What happens on a crash?**\
The host tears down the sandbox and spawns a new instance. State is cheap, and (planned) read-only sharing makes recovery nearly free.

---

## How to follow along

In upcoming posts I’ll publish build/run snippets, disassembly walkthroughs, and trace screenshots as the toolchain firms up. If you’re into compilers, emulators, or teaching systems programming: this is for you. Comment with questions, wishlist features, or your favorite micro-benchmarks you’d like to see.

*Next up: Toolchain Deep Dive—mapping LLVM IR to SLOW-32 patterns, and why compare+branch beats status flags in a teaching ISA.*

