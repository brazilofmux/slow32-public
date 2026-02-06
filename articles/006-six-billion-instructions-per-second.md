# I Built a 6 BIPS JIT in Five Months (and Why That Matters)

**A stroll from a toy ISA to a production-grade dynamic binary translator: correctness-first, then ruthless specialization. Benchmarks, design choices, and how AI changed the clock speed of systems work.**

*This began as a curiosity. It turned into a test of whether a small, clean ISA and fast iteration could reach the performance knee of modern emulation. The results surprised and taught me a lot. I'm publishing the code and the numbers so other people can reproduce and build on it.*

---

## The Napkin

At the end of August I sketched a 32-bit RISC-y instruction set on a napkin. Five months later I'm sitting with a JIT backend that runs a real CSV validator at hundreds of megabytes per second and a compute microbenchmark at nearly six billion guest instructions per second. That feels surreal, because this kind of project — a custom ISA, a full LLVM-targeting compiler toolchain, a runtime, and a multi-stage dynamic binary translator — used to be a multi-year program inside a funded team.

The point of this essay isn't to brag. It's to explain the path, the engineering choices, and the sense in which the result is both a toy and a real machine. Because the interesting part isn't the peak number. It's what the peak number implies about how systems work is changing — both in what one person can build, and in what kind of execution environment we might want to build it *for*.

The artifact is real. The benchmarks are reproducible. The code is public. If I'm wrong about something, you can check the receipts.

---

## The Contract: SLOW-32

SLOW-32 is a 32-bit, orthogonal, 3-address RISC instruction set. It's inspired by RISC-V but simplified for a specific purpose: to be the thinnest possible contract between a compiler and an execution environment.

Here's what SLOW-32 *has*:

- Eighty-plus instructions. Fixed-width 32-bit encoding, little-endian. Thirty-two general-purpose registers (r0 hardwired to zero). The full complement of integer arithmetic, logical, shift, comparison, branch, jump, and load/store. IEEE 754 floating-point (f32 and f64) lives in GPRs — no separate FP register file. Doubles use register pairs. The emulators hook transcendental functions (sin, cos, sqrt, etc.) to host libm, and dtoa handles float-to-string conversion. The whole FP subsystem went from nothing to working in three days.
- A complete toolchain: a custom assembler with GNU-style directives and expression evaluation, a linker, a librarian, a disassembler, and an LLVM backend with Clang integration. You can write C or C++, compile it with Clang, and produce a SLOW-32 executable.
- A microcontroller-style memory model: code lives low (W^X enforced), data and stack live high, one MMIO device (a ring buffer accessed via YIELD), no interrupts. The linker controls the memory map. A binary can address anywhere from 4 KB to 256 MB depending on how you configure it.

Here's what SLOW-32 *doesn't* have:

- No exceptions. No RTTI. No virtual memory. No threads (yet). No self-modifying code. No signal handlers.

These are deliberate omissions. Every feature you leave out is an invariant you can exploit later. A W^X code region means the JIT can assume code pages are immutable — which makes block caching and direct branch chaining both safe and cheap. No exceptions means every instruction either completes or the machine halts. No self-modifying code means translated blocks never go stale.

The mental model is a microcontroller: ROM for code, RAM for data, a simple peripheral interface, deterministic execution. If you've ever programmed an embedded system, the contract will feel familiar.

---

## Why Correctness-First

Before writing any JIT, I built a plain interpreter. Before optimizing anything, I made the interpreter pass thousands of differential tests against hand-computed expected outputs. Before adding a second emulator, the first one was boring and correct.

This sounds slow. It's the opposite of slow. Every hour invested in correctness infrastructure paid back tenfold during optimization, because when a JIT stage introduced a subtle bug — and they all did — I had an oracle to test against. The debugging loop was: run the same binary on the interpreter and the JIT, diff the register/memory state at every block boundary, find the first divergence. That loop converges in minutes, not days.

The lesson is simple and old and still underappreciated: you don't need to be fast to be fast. You need to be *correct*, and then you can be fast in a hurry.

---

## The Stages

### Stage 1 — A Working Translator

The first dynamic binary translator was deliberately primitive. It pinned RBP to a pointer to the CPU state structure (`dbt_cpu_state_t`), pinned R14 to the base of guest memory, and emitted raw x86-64 bytes into an mmap'd code buffer. No register allocation, no block caching, no chaining. Each basic block was translated, executed, and then control returned to the dispatcher via `RET` with the exit reason written to the CPU state.

The goal was to get a thing that ran. A thing you could `diff` against the interpreter. A thing whose output you could trust before you made it fast.

```
// The fundamental contract: every translated block ends with
//   mov [rbp + offsetof(exit_reason)], IMM
//   ret
// The dispatcher reads exit_reason and decides what to do next.
```

### Stage 2 — Block Cache and Direct Tail Chaining

Once the translator was correct, the first optimization was obvious: don't re-translate blocks you've already seen. A hash table keyed by guest PC stores pointers to translated code. On a block exit, if the target is already in the cache, patch the exit to jump directly to the translated code instead of returning to the dispatcher.

This is where W^X pays off. Because guest code is immutable, you never need to invalidate the cache. A translated block is valid forever. Direct tail chaining means most block transitions are a single `JMP` instruction — no dispatcher, no hash lookup, no indirect branch. The control flow stays in JIT-compiled code.

### Stage 3 — Superblocks and Register Locality

A basic block ends at every branch. But most branches are predictable — a loop back-edge, a function's hot path. Superblocks extend translation across taken branches, producing longer sequences of straight-line code that the optimizer can work with.

The key insight: longer blocks mean more opportunity for register caching. If a guest register is loaded at the top of a superblock and used six times before the exit, you can keep it in a host register for the entire block instead of hitting memory on every access. On x86-64 with its generous register file, this is a massive win.

Instrumentation at side exits tracks which exits are taken and how often. The heuristics are conservative: don't extend across high-entropy branches (like indirect jumps through a function pointer table), but do extend across loop back-edges and predictable conditionals.

### Stage 4 — Hotness-Guided Extension and Peephole Optimization

Stage 3 had a problem: the act of measuring side-exit frequency affected the code being measured, because the instrumentation itself changed the block boundaries. The solution was a two-pass approach: profile first with extensions disabled, identify hot side exits, then retranslate hot blocks with extensions enabled based on the collected profile.

This is also where peephole optimization enters. The emitter recognizes common patterns in the x86-64 output — redundant loads, unnecessary register-to-register moves, flag computations that are never consumed — and eliminates them. The patterns are simple but the cumulative effect is significant, particularly in tight loops where the same guest registers are manipulated repeatedly.

The register cache became global across a superblock: guest registers are loaded into host GPRs at block entry, held across the entire translated sequence, and only materialized (written back to the CPU state struct) at exits and savepoints. This is where the performance went from hundreds of MIPS to multiple GIPS.

Getting this right was the hardest part of the project. MMIO accesses must see current register values. Block exits must leave the CPU state consistent. Register aliasing (two guest registers mapped to the same host register across a chain boundary) introduces subtle correctness hazards. The differential testing infrastructure — comparing register state against the interpreter at every block boundary — was the only thing that made this tractable.

---

## The Role of AI

I need to be direct about this because the hype cycle around AI-assisted coding is producing a lot of noise.

I built this with Claude — both the conversational version and the agentic coding version (Claude Code). The AI did enormous amounts of mechanical work: writing emitter functions for each instruction, generating test cases, producing delta patches against the codebase, iterating on build system changes. The agent could hold the entire codebase in context and produce correct, targeted edits at a pace I couldn't match by hand.

But the AI did not design the staged architecture. It did not decide to enforce W^X to enable safe block caching. It did not choose the two-pass profiling strategy. It did not identify that register caching across superblock boundaries was the critical optimization. Those were human decisions based on decades of reading about JIT compilers, CPU architecture, and systems design.

The right way to think about it: AI accelerated *execution*, not *design*. And execution speed only collapses timelines when the design boundaries are tight and the invariants are explicit. I could tell the agent "implement Stage 3 register caching with these host registers pinned to these guest registers, write-back on exit, and add differential checksums at block boundaries" — and it could do that, quickly and correctly. But I had to know to ask for it, and I had to know that the answer was right.

The human job, in this new arrangement, is architecture, invariants, staging, triage, and judgment. The agent types fast.

---

## Benchmarks

All numbers were collected on an AMD Ryzen 5 3600 (6-core, 3.6 GHz base) running Linux. Each measurement is best-of-three sequential runs. Two workloads:

- **benchmark_core**: Pure compute — integer arithmetic, branching, memory access, 10 million iterations, 285 million guest instructions.
- **validatecsv_ragel**: A real program — a Ragel-generated goto-driven CSV validator processing 103.6 MB of CSV data, 1.1 billion guest instructions.

Five emulators, plus native x86-64 for comparison. The two interpreters and the DBT are mine. The QEMU entry is a custom ~4,000-line TCG backend I wrote for SLOW-32 — a full system emulator that loads `.s32x` binaries, enforces W^X, and handles MMIO, built on top of QEMU's Tiny Code Generator framework. It's included here as an independent baseline: QEMU's TCG does competent block-at-a-time translation, but it doesn't do superblocks, register caching, or peephole optimization. It shows what a mature, general-purpose JIT framework achieves without the specialized work that makes the custom DBT fast.

### benchmark_core — Pure Compute (285M guest instructions)

| Emulator | Time (s) | MIPS | vs. Interpreter |
|----------|----------|------|-----------------|
| Interpreter | 1.907 | 149 | 1x |
| Optimized interpreter | 0.751 | 380 | 2.5x |
| QEMU TCG | 0.246 | 1,159 | 7.8x |
| **DBT Stage 4 (safe)** | **0.048** | **5,938** | **39.7x** |
| **DBT Stage 4 (unsafe)** | **0.047** | **6,064** | **40.6x** |

### validatecsv — Real-World I/O (1.1B guest instructions, 103.6 MB CSV)

| Emulator | Time (s) | MIPS | MB/s | vs. Interpreter |
|----------|----------|------|------|-----------------|
| Interpreter | 9.802 | 113 | 10.6 | 1x |
| Optimized interpreter | 3.479 | 319 | 29.8 | 2.8x |
| QEMU TCG | 0.952 | 1,164 | 108.9 | 10.3x |
| **DBT Stage 4 (safe)** | **0.318** | **3,486** | **326** | **30.8x** |
| **DBT Stage 4 (unsafe)** | **0.295** | **3,758** | **351** | **33.2x** |
| Native x86-64 (fread) | 0.202 | — | 513 | — |
| Native x86-64 (mmap) | 0.144 | — | 720 | — |

### What the Numbers Mean

The DBT runs pure compute at **5.9 billion guest instructions per second** — on a 3.6 GHz processor, that's roughly 1.6 guest instructions per host clock cycle. The JIT is generating tight enough x86-64 that the superscalar execution engine is retiring more than one guest operation per cycle.

The DBT is **5x faster than QEMU TCG** on compute and **3x faster** on the I/O-bound CSV workload. QEMU is a serious, battle-tested emulator; beating it by this margin on a custom ISA is not something I expected.

The "safe" versus "unsafe" difference is 2–8% depending on workload. "Safe" means W^X enforcement and bounds checking on every memory access. Those checks cost almost nothing because the peephole pass folds them into surrounding code. **You don't have to choose between safety and speed.**

On the real-world CSV workload, the DBT processes data at **351 MB/s** — that's **68% of native fread speed**. A program compiled for a custom 32-bit ISA, running through a JIT translator, processing real I/O through emulated syscalls, within striking distance of bare metal. The whole stack is working.

---

## What Surprised Me

I expected the JIT to be fast. I did not expect it to be *this* fast, and I did not expect the path to be so clean.

The biggest surprise was how cheap safety turned out to be. Before measuring, I assumed bounds checks would cost 15–25%. They cost 2%. The peephole pass is good enough that the checks disappear into the noise. This matters because it changes the engineering calculus: you can sandbox aggressively — every memory access validated, every code page immutable — and still operate at GIPS speeds.

The second surprise was how much mileage the staged approach provided. Each stage was individually simple. Stage 1 was almost trivial. But the *composition* of simple stages produced an emulator that competes with systems built by large teams. The correctness infrastructure meant each stage could be validated independently, so the complexity never compounded. I was never debugging the whole system — I was always debugging a delta.

The third surprise was emotional. I've been a systems programmer for decades. I've shipped compilers and interpreters and embedded systems. But I've never been able to hold an entire toolchain in my head and iterate on it at this speed. The AI collaboration didn't just make the project faster — it made it *possible*. I would not have attempted this project without it, because the mechanical effort would have been prohibitive.

---

## The Four Corners

Here's the bigger idea, and it's the reason I think this project matters beyond the benchmark numbers.

Every programming environment picks its tradeoffs, and the mainstream options occupy three corners of a design space:

**C/C++** gives you speed and simplicity — as simple or as complex as you want it. But it's dangerous. Buffer overflows, use-after-free, undefined behavior. You trade safety for control.

**C#/Java** gives you safety and a productive development experience. Garbage collection means you don't think about memory. But you lose predictability. The GC can pause you at any time, and memory behavior is opaque. For most applications this is fine. For some, it's disqualifying.

**Rust** gives you both speed and safety through the borrow checker. But it's not simple. The borrow checker is a powerful tool that requires training to use effectively, and not all problems decompose cleanly into ownership graphs. Some patterns that are natural in C or Java require significant contortion in Rust.

SLOW-32 occupies a **fourth corner**: simple, fast enough, sandboxed, but restricted. The sandbox is so small and so cheap that *crashing doesn't matter*. A SLOW-32 instance can be as small as 4 KB. Spawn a thousand of them. If one faults — a bad pointer, a runaway loop, a logic error — kill it and restart. No garbage collector pause, no borrow checker negotiation, no segfault postmortem. Just a disposable microcontroller that happens to run on your existing CPU at billions of instructions per second.

The restricted API is a feature, not a limitation. A SLOW-32 instance talks to the outside world through a single MMIO ring buffer. That's it. No filesystem access, no network sockets, no shared memory. The host decides what the guest can do. This is the security model of a microcontroller, and it's the simplest security model that actually works.

---

## The FPGA Bridge

There's a deeper idea buried in the benchmark numbers, and it has to do with hardware.

SLOW-32 was designed to be implementable on an FPGA. A simple pipelined implementation running on a Lattice or Xilinx part could clock at 50–150 MHz with four cycles per instruction. You'd get 12–37 MIPS. That's fine for embedded work — sensor processing, control loops, protocol handlers.

But look at what happens on the other end. On a modern x86-64 with a JIT, the same ISA runs at 6,000 MIPS. The same binary, the same semantics, three orders of magnitude difference in speed.

Now ask: what is a modern out-of-order x86 core, if not an FPGA with better CPU support? It has speculative execution, branch prediction, multiple execution units, deep caches, memory prefetching — all the machinery that makes simple instruction sequences run fast. When you JIT a clean RISC ISA onto that machinery, you're using the x86 core as what it is: a high-performance execution substrate for simple instruction streams.

This means SLOW-32 binaries live on a continuum. At one end, an FPGA running at tens of megahertz with deterministic timing — good for hard real-time constraints. At the other end, a Ryzen running at billions of instructions per second with nondeterministic timing — good for throughput. Same binary, same semantics, different execution substrate. And in the middle, RISC-V: the mapping from SLOW-32 to RISC-V is nearly one-to-one, because the ISA was designed with that correspondence in mind.

The question isn't "FPGA or CPU." The question is "what execution properties does this workload need?" and then you put the binary on the substrate that provides them.

---

## The Limits (and Why That's Fine)

Let me be honest about what I didn't solve.

The I/O path still involves copies. The MMIO ring buffer is a clean abstraction, but it means data passes through the guest-to-host boundary with a memcpy. Native code using mmap can process the same CSV file at 720 MB/s because the kernel maps the file pages directly into the process address space. The DBT tops out at 351 MB/s partly because the data has to traverse the ring buffer. A DMA-style fast path — where the host maps file data directly into guest memory — would narrow this gap but adds complexity.

The DBT cannot replace hardware determinism. If your workload needs guaranteed cycle-level timing — a motor controller, a safety-critical loop — you want the FPGA. The JIT is fast, but it's not *predictable* in the real-time sense. These are different virtues.

Getting beyond the current performance asymptote would require fundamentally different work: whole-program optimization, vectorization, or building an optimizing compiler at the LLVM level of sophistication. That's not a weekend project. Alternatively, you build hardware — which is the FPGA path.

And the toolchain, while functional, is not battle-hardened. The libc is minimal. There's no POSIX layer. The C++ support doesn't include exceptions. These are engineering tasks, not research problems, but they're real work.

None of this undermines the core result. The point was never to replace native code or to ship a production microcontroller. The point was to demonstrate that a clean ISA, a focused toolchain, and a staged JIT can reach a performance regime that makes the sandbox model viable for real workloads.

---

## Next Steps

For anyone who wants to follow or contribute, here's where the project is headed:

- **Fast-path MMIO**: The biggest remaining bottleneck is the I/O path. A DMA-style approach where the host maps file data directly into guest address space would bypass the ring buffer copy and close the gap to native.
- **Offline pre-translation**: Save profiles from JIT runs and use them to pre-warm the block cache on subsequent executions — a hybrid AOT/JIT approach.
- **RISC-V soft core**: A toy FPGA implementation of SLOW-32, not for speed, but to validate the ISA on real hardware and demonstrate the continuum from FPGA to JIT.
- **Interrupt support**: For embedded-style use cases where the guest needs to respond to asynchronous events.

It's worth noting what I investigated and decided *not* to pursue. Profiling showed that all hot loops already fit in single superblocks, so cross-block register allocation wouldn't help on current workloads. Dispatch overhead is effectively zero — block chaining keeps execution in JIT code without returning to C. And tuning superblock extension thresholds produced identical results across all configurations. The DBT appears to be at the point where further gains require either reducing I/O overhead or moving to hardware.

The repository is public: [github.com/brazilofmux/slow32-public](https://github.com/brazilofmux/slow32-public). The benchmark data, the toolchain, and the emulators are all there. If the numbers look wrong, check my work.

---

## The Larger Point

This essay describes a five-month, part-time project by one person working with an AI agent. The output is a complete instruction set architecture, a compiler toolchain, a runtime, and a JIT that executes real programs at billions of instructions per second. Five years ago, this would have been a funded research program. Ten years ago, it would have been a PhD thesis.

I don't say that to diminish the work that came before — the people who built QEMU, LLVM, and the JIT literature I learned from. I say it because something has genuinely changed. The unit of work in systems programming has shifted. The mechanical cost of implementation — writing boilerplate, generating test cases, iterating on builds — has collapsed. What remains, and what matters more than ever, is the human contribution: architectural clarity, correctness discipline, staged design, and the judgment to know which optimization matters next.

AI didn't design this system. I did. But AI made it *buildable* in a timeframe that let me actually do it, instead of filing it under "someday" with all the other projects that die on the vine.

If you're a systems programmer sitting on a stack of ideas you never had time to build — this is the moment. The constraint is no longer mechanical effort. The constraint is knowing what to build, and having the discipline to build it right.

---

## Reproducibility

**Repository**: [github.com/brazilofmux/slow32-public](https://github.com/brazilofmux/slow32-public)

**Hardware**: AMD Ryzen 5 3600 (6-core, 3.6 GHz base clock), Linux.

**Commit**: [`fcbe528`](https://github.com/brazilofmux/slow32-public/tree/fcbe528) — all emulators built with GCC `-O2`, SLOW-32 programs compiled with Clang/LLC `-O2` via the LLVM backend.

**Benchmark commands**:
```bash
# Compute benchmark (instruction counts from slow32-fast, timing from time)
time ./tools/emulator/slow32-fast benchmark_core.s32x
time ./tools/dbt/slow32-dbt -4 benchmark_core.s32x

# CSV validator (real-world I/O workload)
FILES=$(find csv2 -name '*.csv' -print)
time ./tools/dbt/slow32-dbt -4 validatecsv_ragel.s32x $FILES
```

**Methodology**: Best of 3 runs, executed sequentially to avoid interference. `slow32-fast` reports instruction counts and wall-clock timing directly. The `-U` flag disables bounds/W^X checks for the "unsafe" comparison. Correctness is validated by diffing output against the reference interpreter.

---

*Stephen is a retired electrical engineer who writes about systems, AI, and whatever else catches his attention at [Unlikely Emphasis](https://unlikelyemphasis.substack.com).*
