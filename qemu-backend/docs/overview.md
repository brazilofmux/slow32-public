# Slow32 TCG Port — Overview & Requirements

## Motivation

The SLOW-32 ISA already has a fast interpreter (`slow-32/tools/emulator/slow32-fast`, ~385 MIPS). The next milestone is a QEMU Tiny Code Generator backend that can execute `.s32x` binaries directly (`qemu-system-slow32 -kernel program.s32x`). This document captures the functional requirements before writing code, so we can implement the target incrementally and validate each step against the reference emulator.

## Architectural Snapshot

| Concern | Details | Source |
|---------|---------|--------|
| Registers | 32 GPRs. `r0` hardwired to zero; `r1–r2` return values; `r3–r10` arguments; `r29=sp`, `r30=fp`, `r31=lr`. No condition codes. | `slow-32/README.md`, `CLAUDE.md` |
| Instruction width | Fixed 32-bit words, RISC-V-like encoding with custom opcode assignments. | `slow-32/docs/INSTRUCTION-SET.md`, `tools/emulator/slow32.c` |
| Control flow | `jal`, `jalr`, `beq/bne`, `blt/bge`, `bltu/bgeu`. Comparisons produce 0/1 results directly in registers. | ISA doc |
| Special opcodes | `yield`, `debug`, `halt`. `debug` emits a character; `yield` wastes cycles; `halt` stops execution. | `slow32.c` |
| Memory map | Default `.s32x` layout: 1 MB execute-only code at 0x0000_0000, followed by up to 255 MB data/heap, optional MMIO window starting at linker-provided `__mmio_base`, stack near 0x0FFF_FFF0. Compact layouts shrink these regions but must respect page alignment. | `slow-32/tools/emulator/s32x_loader.h`, `runtime` docs |
| Binary formats | `.s32o` object files and `.s32x` executables share headers defined in `slow-32/tools/emulator/slow32_format.h`. | same |
| Toolchain | LLVM backend already built in `~/llvm-project`. Helper scripts expect `LLVM_BIN=~/llvm-project/build/bin`. | `slow-32/CLAUDE.md` |

## Loader Requirements

1. Parse `.s32x` headers exactly like `slow-32/tools/emulator/s32x_loader.h`.
2. Allocate QEMU `MemoryRegion`s for:
   - Execute-only code (map as ROM or RAM with `readonly = true` to enforce W^X).
   - Read/write data/heap.
   - Optional MMIO window sized per header; expose base to guest via `__mmio_base`.
3. Initialize stack pointer (`r29`) to the top of the allocated stack region (default 0x0FFF_FFF0 unless overridden).
4. Preserve compact layout metadata (code/data sizes, heap/stack adjustments) so small programs retain their footprint.
5. Hook into `hw/core/loader.c` or provide a dedicated loader (`hw/misc/slow32_loader.c`) invoked by `machine.c`.

## CPU State Definition

`target/slow32/cpu.h` should at minimum expose:

```c
typedef struct Slow32CPU {
    CPUState parent_obj;
    uint32_t regs[32];
    uint32_t pc;
    uint32_t mmio_base;
    uint32_t stack_top;
    uint32_t pending_debug_char;
    bool halted;
} Slow32CPU;
```

Future extensions (cycle counters, MMU flags) can be added later. The key is mirroring what the fast interpreter tracks today: register file, program counter, stack/MMIO metadata, and halt/debug state.

## Translation Strategy

- **Decoder**: Reuse opcode field extraction from `slow-32/tools/emulator/slow32.c`. Keep the tables close to the ISA spec for easy auditing.
- **ALU / Logical ops**: Implement directly with TCG helpers (`tcg_gen_add_i32`, `tcg_gen_xor_i32`, etc.).
- **Comparisons**: Emit boolean results (0/1) to match the ISA. Branches should evaluate these registers, not hidden flags.
- **Loads/Stores**: Use QEMU’s `tcg_gen_qemu_{ld,st}_i32` helpers. Enforce alignment semantics from the interpreter (fault or emulate as needed).
- **Multiply/Divide**: Initially call C helpers that mirror `slow32.c` behavior. We can inline later if profiling warrants it.
- **Special instructions**:
  - `debug`: Call a helper that writes to a QEMU chardev (or stderr placeholder).
  - `yield`: optional helper that currently no-ops but keeps a hook for future instrumentation.
  - `halt`: raise `EXCP_HLT` to terminate the vCPU loop.

## Memory & Devices

- The only mandatory device is the DEBUG console output. Implement it via `qemu_chr_fe_write_all` so it integrates with existing chardev plumbing.
- MMIO ring buffer support (from `slow32_mmio.c` and `mmio_ring.c`) can arrive later; the initial target can route DEBUG directly to host output.
- Sparse allocation: the interpreter lazily allocates pages. QEMU can approximate this by creating RAM sections sized per header; laziness is nice-to-have but not required for functional correctness.

## Dependencies & References

- Primary specs:
  - `slow-32/docs/INSTRUCTION-SET.md`
  - `slow-32/docs/file-formats.md`
  - `slow-32/tools/emulator/slow32.c` and `slow32_format.h`
  - `hw/slow32/slow32-tcg.c` - reference machine glue for early experiments
- QEMU examples:
  - `target/openrisc/` (small, no vector ISA)
  - `target/riscv/` (richer, but shares RISC-like structure)
- Existing helper docs:
  - `AGENTS.md` (high-level project context)
  - `slow-32/CLAUDE.md` (toolchain commands, regression expectations)

## Instrumentation & Benchmarking

- The translator increments a per-CPU instruction counter (`insn_retired`) and accumulates translation time for every TB. `helper.c` prints a single `Slow32 stats: guest_insns=… wall_ms=… translate_ms=… exec_ms=… tb_count=…` line whenever the guest halts so we can see how much time went into translation versus execution.
- Use `-machine slow32-tcg,stats=off` to disable all counter bookkeeping and suppress the `Slow32 stats` printouts when benchmarking peak performance. (Note: tooling such as `scripts/slow32/compare.py` requires stats to remain enabled.)
- `scripts/slow32/compare.py` sweeps one or more `.s32x` images and runs each under both `slow32-fast` and `qemu-system-slow32`. Use `--iterations N` to average multiple runs, `--suite DIR_OR_FILE` to point at a directory (all `*.s32x`) or manifest, and `--verbose` to echo the resolved workload list.
- Example run (smoke workload, 5 iterations):

```sh
$ python3 scripts/slow32/compare.py --iterations 5
slow32 comparison (lower is better)
-----------------------------------
 image: /tmp/s32_smoke.s32x
 iterations: 5

 qemu-system-slow32: 0.027720s (translate 0.000044s, exec 0.027675s, tb_count≈23)
 slow32-fast:        0.000024s
 guest instructions: 260 (qemu reported 260)
 speed ratio (fast/tcg): 0.001x
 translate share: 0.2% (relative to total qemu time)
```

- When multiple images are requested the script prints a compact table, making it easy to spot workloads dominated by translation/start-up tax versus steady-state execution time.

Keeping this overview up to date will help new contributors understand why each subsystem matters before we implement it.
