# AGENTS.md — QEMU + SLOW-32 Integration Guide

## 1. Repository Context
- Upstream QEMU tree plus a private copy of the `slow-32/` toolchain lives side-by-side in this workspace (`/ztank/secret/sdennis/qemu`). The immediate goal is to grow a `slow32-tcg` emulator by teaching QEMU’s Tiny Code Generator how to execute SLOW-32 binaries.
- `slow-32/tools/emulator/slow32-fast` already reaches ~385 MIPS; it exists as a behavioral reference whose ISA, loader, and MMIO semantics must match whatever the TCG backend produces.
- The LLVM SLOW-32 backend is already built under `~/llvm-project` (see `slow-32/LLVM_BACKEND_WORK_NEEDED.md` for the current status). Inline assembly and the full C toolchain are available, so you can generate fresh `.s32x` workloads without touching host compilers.

## 2. Immediate Points of Interest
- `slow-32/` — complete assembler, linker, runtime, emulators, docs, and regression inputs. Use its `AGENTS.md`, `README.md`, and `CLAUDE.md` for day-to-day toolchain workflows.
- `slow-32/docs/INSTRUCTION-SET.md` & `docs/file-formats.md` — authoritative ISA and executable/object format descriptions.
- `slow-32/tools/emulator/` — the existing C-based interpreter (`slow32.c`, `slow32-fast.c`, `slow32_mmio.c`, `mmio_ring.c`). Treat this directory as executable documentation for instruction semantics, MMIO handoffs, and W^X enforcement (`slow32_format.h`, `s32x_loader.h`).
- `tcg/`, `target/`, `include/tcg/`, `accel/tcg/` — QEMU subsystems you will extend when adding a `slow32` target. Follow the pattern used by other small softmmu targets (e.g., `target/riscv` or `target/openrisc`) rather than digging through unrelated devices.
- External dependency: `~/llvm-project` (already configured with the SLOW-32 backend). Clang lives in `~/llvm-project/build/bin`; scripts inside `slow-32/tools/` default to that path.

### Slow32 TCG documentation
- High-level requirements live in `docs/slow32-tcg/overview.md`.
- The incremental work checklist is tracked in `docs/slow32-tcg/todo.md`.

## 3. Building Tooling Versus QEMU
| Component | Command | Notes |
|-----------|---------|-------|
| Slow-32 toolchain | `cd slow-32 && make` | Builds assembler, linker, utilities, runtime objects, and both emulators. `make runtime`, `make emulator`, etc. are available for incremental work. |
| QEMU host binary | `./configure --target-list=x86_64-softmmu && ninja -C build` | Standard Meson/Ninja flow; adjust `--target-list` once a `slow32-softmmu` target exists. Keep builds in a dedicated `build/` directory to avoid polluting the repo. |
| Fresh SLOW-32 executable | `cd slow-32 && ./tools/compile-c.sh examples/hello.c hello.s32x` | Script stitches together clang → llc → assembler → linker using the runtime archives. |

## 4. SLOW-32 ISA Facts Needed for TCG
- 32 general-purpose registers (r0 hardwired to zero; r1–r2 return values; r3–r10 argument registers; r29=sp, r30=fp, r31=lr). No condition codes — comparisons produce 0/1 results instead.
- Fixed-width 32-bit instructions covering arithmetic, comparison, shifts, load/store, and straightforward control flow (`jal`, `jalr`, `beq/bne`, signed/unsigned branch variants). Special opcodes: `yield`, `debug`, `halt`, plus MMIO-backed syscalls described in `slow-32/docs/`.
- Memory layout: 1 MB execute-only code segment at 0x0000_0000, followed by a large data segment and optional MMIO window (linker injects `__mmio_base`). Stack starts near 0x0FFF_FFF0 and grows downward. The emulator enforces W^X and sparse allocation—mirror this in QEMU’s `MemoryRegion` setup.
- Instruction timings (for fast-versus-TCG validation) are encoded in `slow-32/tools/emulator/slow32.c`. Even if TCG will not perfectly match cycle counts, the functional behavior (including multi-cycle MUL/DIV and memory side effects) must match.

## 5. Binary & Loader Details
- Object files (`.s32o`) and executables (`.s32x`) have custom headers defined in `slow-32/tools/emulator/slow32_format.h`. The loader logic inside `s32x_loader.h` shows how sections are mapped, how the W^X policy is enforced, and how the MMIO heap is reserved.
- The linker (`slow-32/tools/linker/s32-ld.c`) emits compact layouts when programs are small; your QEMU loader must honor the header’s explicit sizes rather than assuming defaults.
- MMIO expectations: the runtime accesses UART/console through the special DEBUG instruction and optional ring-buffer MMIO implemented in `mmio_ring.c`. Preserve these addresses when wiring QEMU devices.

## 6. Existing Emulators & Debug Hooks
- `slow32` — straightforward interpreter with per-instruction tracing, stepping (`-s`), register diffs (`-r`), breakpoints (`-b`), and watchpoints (`-w`). Use it as the oracle when validating new TCG translations.
- `slow32-fast` — optimized interpreter emphasizing throughput (~385 MIPS). Useful when benchmarking parity against the forthcoming TCG backend.
- `slow32_mmio` plus `mmio_ring.c` — demonstrates how the MMIO console buffer integrates with the runtime; mirror the behavior inside QEMU’s device emulation or delegate to existing chardevs.

## 7. QEMU TCG Integration Checklist
1. **Target skeleton** — Create `target/slow32/` with `cpu.c`, `cpu.h`, `helper.h`, `helper.c`, `translate.c`, and `Kconfig`/`meson.build` entries mirroring other 32-bit RISCs. Define `Slow32CPU` state (GPR file, PC, pending exception flags, MMIO base, cycle counters if needed).
2. **Decoder & translator** — Port the instruction decode tables from `slow-32/tools/emulator/slow32.c` or the ISA doc into TCG helpers. Keep ALU ops inline in `translate.c`; use helper calls for complex sequences (e.g., 64-bit MUL/DIV expansions).
3. **Memory model** — Instantiate code/data RAM regions according to the `.s32x` header and enforce execute-only permissions on the text segment. Implement sparse allocation or reuse QEMU’s `memory_region_init_ram_nomigrate` plus `memory_region_init_alias` to match the emulator’s lazy paging.
4. **Device hooks** — Reuse QEMU’s chardev infrastructure for the DEBUG instruction output. If you need the MMIO ring buffer, bolt it onto an existing UART or implement a tiny char device.
5. **Loader** — Teach `hw/loader` (or a new `hw/misc/slow32_loader.c`) to parse `.s32x` images directly so `qemu-system-slow32 -kernel program.s32x` “just works.” Reference `slow-32/tools/emulator/s32x_loader.h`.
6. **CLI plumbing** — Add `slow32-softmmu` to `default_targets` once the backend builds. Update `docs/system/target` with a short description.
7. **Testing** — Run `slow-32/regression/run-tests.sh` to produce known-good outputs, then execute the resulting binaries under QEMU and diff traces against `slow32 -t`. Build automated acceptance tests once opcodes stabilize.

## 8. Testing & Validation Flow
- Toolchain smoke test: `cd slow-32 && ./scripts/test-quick.sh`.
- Full regression: `cd slow-32/regression && ./run-tests.sh` (expects LLVM tools in `~/llvm-project/build/bin` or exported via `LLVM_BIN`). Artifacts land in `slow-32/regression/results/<case>/`.
- Emulator comparison: run the same `.s32x` via `slow32-fast` and the TCG-powered `qemu-system-slow32`, capture traces (`-d in_asm,exec`) and compare with `slow32 -t`.
- When modifying the LLVM backend, follow the checklist in `slow-32/LLVM_BACKEND_WORK_NEEDED.md` (notably the 64-bit division bug and the `__udivdi3` safeguard).

## 9. Known Pitfalls & References
- The LLVM backend currently works around a 64-bit divide/modulo issue (see `slow-32/docs/issues/llvm-i64-division.md`). Do not assume optimizer-generated `div`/`rem` sequences are trustworthy until that fix lands.
- Some runtime objects (e.g., `printf_enhanced.s32o`) rely on MMIO syscalls introduced in the private tree—ensure your QEMU device model exposes the same addresses before claiming feature parity.
- Always link with `crt0.s32o` first, followed by program objects, `runtime/libs32.s32a`, and `runtime/libc.s32a`; the helper script `slow-32/tools/compile-c.sh` encodes the canonical order.
- Documentation map: `slow-32/README.md` (project overview), `slow-32/AGENTS.md` (repository etiquette inside the toolchain), `slow-32/CLAUDE.md` (assistant cheat sheet), `slow-32/docs/` (ISA, formats, release notes), `slow-32/examples/` (sample workloads). Consult these before touching generic QEMU subsystems.

Armed with the above, new agents can focus on SLOW-32 specifics instead of rediscovering how to build toolchains or where the ISA is documented, and can stand up the TCG backend incrementally without spelunking unrelated QEMU code.
