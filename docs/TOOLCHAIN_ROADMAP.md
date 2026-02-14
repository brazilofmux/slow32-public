# Toolchain Roadmap: Dual-Path Strategy

## Vision

SLOW-32 pursues two complementary toolchain paths simultaneously. They share
the same LLVM backend, the same emulators, and the same runtime libraries.
They diverge only in the narrow assembler/linker/object-format layer.

```
                         LLVM Backend
                        (shared by both)
                              |
                     llc -filetype=...
                        /           \
                   asm (text)     obj (binary)
                      |               |
               Path B: Self-Host   Path A: Production
                      |               |
                  slow32asm         (ELF .o)
                      |               |
                   .s32o            lld / ld
                      |               |
                   s32-ld           ELF executable
                      |               |
                   .s32x              |
                      \              /
                       \            /
                     Emulators / Hardware
                    (load either format)
```

## Path A: Production (ELF)

**Goal:** Standard toolchain integration. GDB support. Credibility.

**What it gives us:**

- Standard ELF object files and executables
- GDB debugging via GDB stub in emulators
- lld or GNU ld for linking (no custom linker needed)
- Potential binutils/objdump/readelf compatibility
- Path to Linux kernel port (long term)

**What already exists:**

- `SLOW32ELFObjectWriter.cpp` -- ELF relocation emission (MC layer)
- `SLOW32MCCodeEmitter.cpp` -- binary instruction encoding
- `SLOW32AsmBackend.cpp` -- fixup/relaxation handling
- `SLOW32TargetMachine.cpp` creates `TargetLoweringObjectFileELF`
- `llc -filetype=obj` should already attempt ELF .o emission

**What's needed:**

1. **Validate/complete MC layer** -- test `llc -filetype=obj`, fix any

   missing relocations or encoding bugs

2. **Linker script for lld** -- memory layout (code 0x0-0xFFFFF, data,

   heap, stack at 0x0FFFFFF0)

3. **ELF loader in emulators** -- read ELF program headers, extract

   entry point, map PT_LOAD segments

4. **Metadata via symbols** -- emulators already resolve `__mmio_base`;

   extend to `__heap_base`, `__stack_base`, `__stack_end`; derive
   code_limit/rodata_limit from ELF segment boundaries

5. **GDB stub** -- RSP protocol in emulators, register read/write,

   memory read/write, breakpoints, single-step

**MMIO in ELF world:**
The MMIO region can be a PT_LOAD segment at a fixed high address, or
the emulator can detect `__mmio_base` in the symbol table (already works).
No custom ELF extensions needed.

## Path B: Self-Hosting

**Goal:** The assembler and linker run ON SLOW-32, producing SLOW-32 binaries.

**What it gives us:**

- The classic "compiler writes itself" milestone
- Proof that the architecture is complete enough for real systems work
- A toolchain small enough to understand completely (~5000 lines of C)
- Independence from host tools for the assemble/link stage
- Educational value: students see the full bootstrap chain

**What already exists:**

- Assembler (slow32asm.c, ~2500 lines) and linker (s32-ld.c, ~2200 lines)

  are plain C with no external dependencies beyond libc

- LLVM backend compiles C to SLOW-32
- Runtime has malloc/realloc, stdio, string functions
- MMIO provides file I/O (open, read, write, close)
- Hardened error handling (Feb 2026)

**What's needed:**

1. **Dynamic arrays** -- replace all MAX_* static limits with

   realloc-based growth (prerequisite: the tools must work with
   only as much memory as the input requires)

2. **Audit libc dependencies** -- verify every libc function used by

   the assembler/linker is implemented in the SLOW-32 runtime

3. **File I/O paths** -- assembler reads .s, writes .s32o; linker

   reads .s32o/.s32a, writes .s32x; all via MMIO file ops

4. **Memory budget** -- profile peak memory on real inputs; SLOW-32

   address space is 256MB (28-bit), minus code/stack/MMIO

5. **Bootstrap test** -- compile slow32asm.c with LLVM, run resulting

   binary in emulator, assemble a test program, verify output matches
   host-built assembler output (bit-identical .s32o)

## Control Points

The paths diverge and converge at well-defined points:

| Layer | Path A (ELF) | Path B (Self-Host) | Shared? |
|-------|-------------|-------------------|---------|
| Source language | C, Pascal, Forth, BASIC | same | Yes |
| LLVM frontend | clang | same | Yes |
| LLVM backend | same .td files, same ISel | same | Yes |
| llc output | `-filetype=obj` (ELF .o) | `-filetype=asm` (.s text) | No |
| Assembler | LLVM integrated / GAS | slow32asm | No |
| Object format | ELF .o | .s32o | No |
| Linker | lld with linker script | s32-ld | No |
| Executable format | ELF | .s32x | No |
| Emulator loader | ELF loader (new) | s32x_loader.h | No |
| Emulator core | same instruction execution | same | Yes |
| MMIO / I/O | same ring buffer protocol | same | Yes |
| Runtime (crt0, libc) | same source, different link | same | Yes |
| Debugging | GDB via RSP | printf / trace flags | No |

**Key insight:** The LLVM backend is the expensive, complex piece. It's
100% shared. The divergence is only in the "last mile" -- the part
between llc output and the emulator loading the binary. Both paths
can coexist with a build flag or script option.

## Emulator Convergence

The emulators need to load both formats. The loader becomes:

```c
if (magic == S32X_MAGIC) {
    load_s32x(filename, ...);    // existing path
} else if (magic == ELF_MAGIC) {
    load_elf(filename, ...);     // new path
}
```

Both loaders populate the same `cpu_state_t`. The emulator core doesn't
care which format the binary came from. Metadata extraction:

| Metadata | .s32x source | ELF source |
|----------|-------------|------------|
| Entry point | header.entry | e_entry |
| Code limit | header.code_limit | end of .text PT_LOAD |
| Data sections | header sections | PT_LOAD segments |
| BSS | section with mem_size > size | PT_LOAD with filesz < memsz |
| Stack base | header.stack_base | `__stack_base` symbol or default |
| Heap base | header.heap_base | `__heap_base` symbol or linker script |
| MMIO base | header.mmio_base | `__mmio_base` symbol |
| Symbols | optional .symtab section | ELF .symtab |

## Phased Execution

### Phase 1: Foundation (Current)

- [x] Assembler/linker hardening (error detection, bounds checking)
- [ ] **Dynamic arrays refactor** (remove all MAX_* static limits)
- [ ] Audit assembler/linker libc usage against runtime

### Phase 2: Self-Hosting Bootstrap

- [ ] Compile slow32asm.c to SLOW-32, run in emulator
- [ ] Compile s32-ld.c to SLOW-32, run in emulator
- [ ] Bootstrap test: assemble + link a program using self-hosted tools,

      verify bit-identical output vs host tools

- [ ] Performance baseline: how long does self-hosted assembly take?

### Phase 3: ELF Path

- [ ] Test `llc -mtriple=slow32 -filetype=obj` -- see what works
- [ ] Fix MC layer gaps (missing relocation types, encoding bugs)
- [ ] Write minimal ELF loader for emulators
- [ ] Create lld linker script for SLOW-32 memory layout
- [ ] End-to-end test: source -> ELF -> emulator

### Phase 4: Debugging

- [ ] GDB RSP stub in emulators (register/memory read, breakpoints)
- [ ] DWARF info passthrough (LLVM already generates it)
- [ ] Source-level debugging of SLOW-32 programs

### Phase 5: Convergence

- [ ] Build scripts support `--format=elf` and `--format=s32x`
- [ ] CI tests both paths
- [ ] Self-hosted tools can produce both formats
- [ ] Documentation: "Getting Started" covers both paths

## Design Principles

1. **The LLVM backend is the crown jewel.** Both paths depend on it.

   Invest there first. Every improvement benefits both paths.

2. **The custom format is not legacy.** It's deliberately simple,

   fully self-hostable, and educational. It stays.

3. **ELF is not a replacement.** It's a parallel output for when you

   need production toolchain integration, debugging, or credibility.

4. **The emulators are format-agnostic.** They execute instructions.

   The loader is a plugin, not a core concern.

5. **Self-hosting is the proof.** When the assembler can assemble itself

   on SLOW-32, the architecture is "real" in a way that no amount of
   ELF support can match.
