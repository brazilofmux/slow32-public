# Stage 01: Forth Assembler + Archiver

## What Is Here

A two-pass assembler written in Forth that reads SLOW-32 assembly
source (`.s`) and produces object files (`.s32o`) with relocations
and symbol tables.

An archiver written in Forth that creates and manipulates `.s32a`
archive files (the SLOW-32 equivalent of Unix `.a` static libraries).

| File | Description |
|------|-------------|
| `asm.fth` | The assembler (~1200 lines of Forth) |
| `ar.fth` | The archiver (~600 lines of Forth) |
| `crt0_minimal.s` | Minimal C runtime startup code |
| `mmio_minimal.s` | Minimal MMIO runtime (putchar, open, read, write, memset, memcpy) |
| `test*.s` | Assembly test sources |
| `run-regression-as.sh` | Assembler regression test runner |
| `run-regression-ar.sh` | Archiver regression test runner |
| `run-test*.fth` | Forth scripts that drive assembly of individual tests |

## What It Needs

- Stage 00 emulator (`s32-emu`)
- Forth kernel (`forth/kernel.s32x`) and prelude (`forth/prelude.fth`)

## What It Produces

`.s32o` object files -- the linkable format with relocations. Also
provides `crt0_minimal.s` and `mmio_minimal.s`, the bootstrap runtime
sources used by all later stages.

`.s32a` archive files from `.s32o` object files. The archiver supports
create, list, extract, replace, delete, move, verbose list, and print
member.

## Supported ISA Subset

The Stage 01 assembler implements the **Bootstrap Core** subset of the SLOW-32 ISA. It is designed to be narrow and consistent, avoiding dialect variations like RISC-V aliases.

### Instructions

-   **Arithmetic**: `add`, `sub`, `mul`, `mulh`, `mulhu`
-   **Logical**: `xor`, `or`, `and`, `sll`, `srl`, `sra`
-   **Comparison**: `slt`, `sltu`, `seq`, `sne`, `sgt`, `sgtu`, `sle`, `sleu`, `sge`, `sgeu`
-   **Immediate**: `addi`, `ori`, `andi`, `slli`, `srli`, `srai`, `xori`, `slti`, `sltiu`
-   **Upper Immediate**: `lui`
-   **Memory**: `ldb`, `ldh`, `ldw`, `ldbu`, `ldhu`, `stb`, `sth`, `stw`
    -   Also supports aliases: `lb`, `lh`, `lw`, `lbu`, `lhu`
-   **Control Flow**: `jal`, `jalr`, `beq`, `bne`, `blt`, `bge`, `bltu`, `bgeu`
-   **System**: `debug`, `yield`, `halt`

### Pseudo-Instructions

-   `li rd, imm`
-   `la rd, sym`
-   `mv rd, rs`
-   `j target`
-   `ret`
-   `call target`
-   `tail target`
-   `nop`
-   `not rd, rs`
-   `neg rd, rs`
-   `seqz rd, rs`
-   `snez rd, rs`
-   `bgt`, `ble`, `bgtu`, `bleu` (reversed branches)

### Registers

Supports both numeric (`r0`..`r31`) and ABI names:
-   `zero` (r0)
-   `rv` (r1)
-   `t0` (r2)
-   `a0`-`a7` (r3-r10)
-   `s0`-`s17` (r11-r28)
-   `sp` (r29)
-   `fp` (r30)
-   `lr` (r31)

## How To Test

```bash
selfhost/stage01/run-regression-as.sh test1
selfhost/stage01/run-regression-as.sh test3
selfhost/stage01/run-regression-ar.sh test3
```
