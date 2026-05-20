# Closing the Bootstrap: SLOW-32 Now Carries Itself to Any Host

**A 933-line C interpreter is the only piece of the system that the host has to compile. From there, SLOW-32 builds its own toolchain, generates its own native code on AArch64 and x86-64, and runs guest binaries at near-native speed without ever calling out to the host compiler again. ARM64 is closed today. AMD64 is days away.**

*Five months ago the headline number was speed: roughly six billion guest instructions per second on a Ryzen, beating QEMU on real workloads. That was a property of the JIT. This piece is about closure — and a new personal best of **7.2 billion instructions per second** on my MacBook Pro. The system no longer needs to ask the host for anything after the first 933 lines.*

---

## What "Closed" Means Here

A toolchain is closed when nothing outside of it has to be trusted to keep it working.

Every other compiler I've ever depended on is open in some specific sense. Clang depends on the system C++ runtime. GCC depends on you having a previous GCC. Even self-hosting languages — Go, Rust, OCaml — originally shipped as a binary blob bootstrapped from a compiler in another language. That isn't bad engineering; it's the only way to get started. But it does mean the chain of trust threads through a lot of other people's machines.

SLOW-32 today threads through one file: [`selfhost/stage00/s32-emu.c`](https://github.com/brazilofmux/slow32-public/blob/main/selfhost/stage00/s32-emu.c), a 933-line C interpreter for the SLOW-32 ISA. That's the only thing the host has to compile. Every other piece of the environment — the C compiler, the assembler, the linker, the runtime, the high-speed emulator, the dynamic binary translator — runs *inside* SLOW-32, gets compiled *by* SLOW-32, and emits the native code that makes SLOW-32 fast on whatever you're sitting in front of. (Full repo: [github.com/brazilofmux/slow32-public](https://github.com/brazilofmux/slow32-public). Open it in another tab; I'll wait.)

I shipped the AArch64 leg of that chain over the last few weeks. The x86-64 leg has been working a long time at the JIT level — that's where the 6 BIPS number from the previous article came from — but the corresponding cross-compiler closure is what's landing next. The detail of that work has its own section below.

This piece is about how the chain works, what closing it actually required, and why I think this changes what a 32-bit hobby ISA gets to be.

---

## The Bootstrap in Plain English

Here is the procedure on a fresh laptop, stripped to its essentials.

1. The host C compiler builds the 933-line interpreter `s32-emu`. That is the entire host-specific surface of the project.
2. The interpreter loads `stage07`, a precompiled SLOW-32 binary that contains the full self-hosted C compiler with both AArch64 and x86-64 back ends. *(If you're worried about the Ken Thompson hack here: don't. This blob can be strictly re-derived from portable C source, and that re-derivation is exercised regularly. More on that two paragraphs down.)*
3. `stage07` produces a native cross-compiler for the host: `cc-a64` on ARM64, `cc-x64` on x86-64.
4. The cross-compiler builds the runtime, the high-speed emulator (`slow32-fast`), and the JIT dynamic binary translator (`slow32-dbt`) as native ELF.
5. From here on, SLOW-32 guest programs run through the JIT — the dynamic binary translator that turns guest blocks into host machine code on the fly — at multi-GIPS speeds.

After step 1, the host compiler is gone from the chain. Everything else is the system compiling itself.

The interesting part is what's hiding inside step 3. `cc-a64` and `cc-x64` are not separate compilers; they're the same C front end with different back ends. The shared parts — lexer, parser, type checker, IR, optimizer — live in headers that both compilers `#include`. The architecture-specific parts — register allocation, instruction selection, ABI lowering — live in `hir_codegen_a64.h` and `hir_codegen_x64.h`. Adding a new host architecture is roughly the size of those two files: a few thousand lines of careful work, not a new compiler.

For practical day-to-day development I usually let the host compiler also build the cross-compiler directly from C source, because both are portable C and it's faster. That's a convenience, not a requirement. The closure is real either way: anything I've ever built with the convenience path can be re-derived from `s32-emu.c` and a precompiled `stage07` blob, and that re-derivation gets exercised regularly as a check.

The whole chain is also small enough to feel. A clean build of the entire self-hosted toolchain — from `cc -O2` on the trust root through every stage and out to the cross-compilers — finishes in about forty seconds on a modern laptop. The interpreter alone compiles in a fraction of a second. There is no point in the build where you have time to lose track of what's happening; the system that runs the system fits in the time it takes to read this paragraph.

---

## The Trust Root: 933 Lines

`s32-emu.c` is the part I want anyone skeptical to read. It's a textbook fetch-decode-execute interpreter for SLOW-32: 32 registers, a flat memory image, the W^X invariant, and a tiny MMIO ring buffer that handles all I/O. There is no JIT in it, no clever caching, no platform-specific code. It's slow on purpose. The whole file fits in a single sitting.

This is the only thing whose correctness has to be argued from inspection. Everything above it is checked against it: the fast interpreter and the DBT both treat `s32-emu` as the oracle, and any divergence in register or memory state on a regression test flags an immediate bug. The slowness of the root is what makes the rest of the stack tractable to debug.

I hold the line on its size deliberately. The interpreter has grown by a couple of hundred lines over the project's lifetime, mostly when the floating-point unit landed. Every feature request that would push it past a thousand lines gets pushed up the stack instead — into runtime libraries, into MMIO devices, into the compiler. Stage 0 is allowed to be boring.

---

## What Closing the AArch64 Loop Took

Most of the last six weeks was AArch64 work, and most of it was not glamorous.

When I started, `cc-a64` could already produce assembly that *looked* right for small functions. What it could not do was produce a working DBT. The AArch64 DBT is roughly four thousand lines of C with a lot of pointer arithmetic, struct manipulation, and inline cache-management code; it exercises the back end in ways that test programs do not. Each failure mode pointed at a different load-bearing assumption.

A short tour of the fixes that landed, in roughly the order they hurt:

- **Struct member alignment.** The original SLOW-32 layout rule was four-byte maximum alignment, which is fine on the guest. On a 64-bit host, struct members with pointer or `long` fields need natural alignment; otherwise stores tear and reads observe partial values. Fixing this required teaching `stage07` about host-driven alignment as a separate concept from guest layout.
- **`size_t` and pointer width.** The runtime's `stddef.h` was hardcoded for 32-bit hosts. After the cross-compiler started producing 64-bit code, every `size_t`-typed return path needed to know it was running on a 64-bit machine. One audited line in `stddef.h`, plus a tour of every callsite that consumed it.
- **Pointer returns from libc.** AArch64's ABI returns pointers in `x0` as 64-bit values; `cc-a64` was, in some cases, treating them as 32-bit and silently truncating them. Caught the hard way: a maddening memory-corruption bug in the DBT's block cache that vanished and reappeared depending on the malloc arena's high bits.
- **A codegen workaround for snapshot copies.** `cc-a64`'s common-subexpression eliminator had a bug that confused two distinct snapshot pointers. The fix in the back end is real but complicated; the fix that unblocked the DBT was a one-line hoist in the DBT itself, marked clearly as a workaround. Both will live until the back end gets a proper fix.
- **STORE base/value collision guard.** The DBT's edge table needed a guard against a case in which the base register and the value register of a store were the same physical register. AArch64 has no equivalent of x86's tolerance for this in the encoding — you have to detect it in software.
- **Icache + superblock exits.** The big one. JIT-emitted code on AArch64 is not visible to the instruction cache until you issue explicit `dc cvau` and `ic ivau` barriers. The DBT was emitting the right inline assembly, but `cc-a64` was dropping the asm on the floor. The fix taught the cross-compiler about cache-maintenance inline asm and routed DBT flushes through proper barriers, which closed a class of bugs where superblock exits would land in stale code and execute the previous tenant.

None of these are interesting in isolation. Collectively, they are the difference between "the cross-compiler emits code" and "the cross-compiler bootstraps a working JIT." That's what closure cost.

---

## What You Get on AArch64 Today

The fast interpreter compiled by `cc-a64` is correct and stable on real workloads. The regression suite passes end to end, the Forth kernel and dBase compatibility layer both run cleanly, and differential testing against the slow interpreter is silent across thousands of guest programs.

Wall-clock numbers compared against a GCC-compiled baseline of the same emulator are competitive. On the hot loops where the SLOW-32 emulator and JIT spend most of their time, the cross-compiler produces code as tight as `gcc -O2` — and on the patterns that matter most to us, sometimes tighter. `h_add`, the dispatch-loop function that handles the bulk of the guest's integer arithmetic, lowers to the same eight-instruction SIB-folded sequence under both compilers. The cross-compiler has the advantage of being shaped for our patterns; GCC has a vastly more sophisticated general optimizer with no such bias. The handoff falls out roughly where you'd expect.

This is not the gap I expected to close. Earlier in the spring the cross-compiler was emitting 37-instruction stack-spilled code where GCC emitted 8. Each round of work — graph-coloring register allocation, ALU+SIB fusion, compare-branch fusion, LICM, immediate folding — narrowed it. At some point the per-instruction quality stopped being the bottleneck. That is a strange and welcome thing to discover about a compiler I wrote inside the system it compiles.

The DBT is the lever that flattens that gap. Once the JIT is running, guest code no longer goes through compiled-by-`cc-a64` interpreter loops; it goes through native AArch64 instructions emitted by the JIT itself. The cost of a slightly less clever cross-compiler shows up only in the DBT's startup time, not in the steady-state guest speed.

On the x86-64 side, that delivered 6 BIPS in compute and 3.5 BIPS on real CSV I/O. The AArch64 DBT just stabilized and is being benchmarked now. I'll publish numbers when they're trustworthy. I expect them to be in the same neighborhood; M-series cores are more than capable of it, and the JIT's design is host-agnostic by construction.

---

## AMD64 Catching Up

The x86-64 path has had a working DBT for months — that's where the 6 BIPS number came from. What it didn't have until recently was the same closed loop. The mature DBT on x86-64 is built by host GCC. That's fine for performance, and it's fine for correctness, but it isn't *closure*. The whole point of the AArch64 work above was that the JIT running on my MacBook is now produced by the project itself, not by Apple's compiler.

Closing the x86-64 loop is what Claude has been finishing while I worked the AArch64 fixes. Most of the work is the same shape as the ARM64 list above, with a different cast: `cc-x64`'s back end gets the same scrutiny `cc-a64` got, the same struct-alignment and pointer-width audits run on the x86 side, and `cc-x64`-built DBTs have to pass the same differential tests against the interpreter. There's nothing exotic in the queue. There's just a lot of it, and I'd rather have the agent do the mechanical work while I think about the harder design issues.

When that lands — should be days — the same recipe runs end to end on either architecture from the same `s32-emu.c` root. A single 933-line file is the entire host-specific surface of the project on either ISA.

---

## What's Actually Being Claimed Here

Let me be careful about what this is and isn't.

**This is not a claim that SLOW-32 is faster than native code.** It isn't. Native compilation will always win on raw throughput; the DBT competes by being good enough that the gap is interesting rather than disqualifying. On the compute benchmark, the JIT runs at roughly 1.6 guest instructions per host clock cycle — excellent for an emulator and not so excellent compared to native vector loops on the same data.

**This is not a claim that the closed loop replaces a real OS.** SLOW-32 is still application-level. It has no virtual memory, no threads, no signals, and no filesystem semantics beyond what the MMIO ring buffer carries. The closure I'm describing is in the toolchain, not in the runtime model.

**This is not a claim that AI built the system.** Claude is doing real work — the AMD64 closure I just described is genuinely Claude's project right now — but the architecture, the staging, the invariants, and the judgment calls about which fixes were load-bearing this month are mine. The agent moves fast inside a tight design boundary. It does not draw the boundary.

**This is a claim that the host's identity has stopped mattering.** Once `s32-emu.c` compiles, every subsequent binary in the project — compiler, runtime, JIT — is produced by the project itself. The cross-compilers are first-class citizens of the SLOW-32 environment; they happen to emit native code, but they live and run inside SLOW-32. Whatever C compiler I had on the host disappears from the chain after the first step.

The practical consequence for anyone using SLOW-32 is portability with teeth. I can hand someone a SLOW-32 executable and tell them: this will run at multi-GIPS speeds on your laptop regardless of architecture, and the only thing your machine had to do was compile a 933-line C file once.

---

## Reproducibility

I don't expect you to take my word for it. Here is how you can verify the closure on your own machine today.

- **Repository:** [github.com/brazilofmux/slow32-public](https://github.com/brazilofmux/slow32-public)
- **Trust root:** [`selfhost/stage00/s32-emu.c`](https://github.com/brazilofmux/slow32-public/blob/main/selfhost/stage00/s32-emu.c) — 933 lines. Read it.
- **Hardware (AArch64):** MacBook Pro, Apple silicon.
- **Hardware (x86-64, prior article):** AMD Ryzen 5 3600. AMD64 numbers from the closed loop will publish when the cross-compiler closure lands.
- **Validation:** every stage in the chain is differential-tested against `s32-emu.c`. If any of them produces different register or memory state on a regression test, the build fails.

The chain end to end (AArch64 today; AMD64 days away):

```bash
# 1. Build the trust root.
cc -O2 -o s32-emu selfhost/stage00/s32-emu.c

# 2. Build the cross-compiler. cc-a64 on ARM64; cc-x64 on x86-64.
#    On ARM64:
cd selfhost/stage08-cross-a64 && make

# 3. Use the cross-compiler to build the runtime and the high-speed emulator.
make libc
make                       # produces out/cc-a64 and the AArch64 fast emulator

# 4. Run a SLOW-32 guest binary through the JIT.
./tools/dbt/slow32-dbt -4 myprogram.s32x
```

---

## Why This Matters

When I started SLOW-32 I thought of it as an educational ISA with a pleasant property — clean enough to JIT well. The 6 BIPS milestone made it an interesting performance artifact. Closing the bootstrap loop is what turns it into something else: a small, portable execution environment that owns its own toolchain.

This is the moment the physical chip’s ISA stopped mattering much. Someone still has to fabricate silicon, but whether that silicon speaks ARM or x86 is now almost cosmetic. It’s just a fast FPGA whose LUTs happen to implement someone else’s architecture. My orthogonal 32-bit RISC has become the real machine the software experiences. **The hardware is emulating *my* virtual machine, not the other way around.**

What I want from a 32-bit hobby ISA, in the end, is the property that a binary I produce today will still run at full speed on a machine I haven't bought yet, without depending on the continued existence and stability of any specific host compiler. With the AArch64 leg closed and the AMD64 leg closing, that's the property I have. The interpreter is the contract. Everything else is just code.

---

*If you're building your own toy ISA, compiler, or self-hosted runtime, how do you handle the bootstrap problem? What's your trust root? I'd love to hear about it in the comments.*

[Subscribe]

---

*Stephen is a retired electrical engineer who writes about systems, AI, and whatever else catches his attention at [Unlikely Emphasis](https://unlikelyemphasis.substack.com).*
