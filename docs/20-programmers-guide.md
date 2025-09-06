# Programmer's Guide

## Revision History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0.0 | 2025-09-05 | S. Dennis | Initial public release |
| 0.9.0 | 2025-09-01 | S. Dennis | Updated calling conventions |
| 0.5.0 | 2025-08-28 | S. Dennis | First draft |

## Register set and conventions
- 32 general-purpose registers; `r0 = 0` (hard-wired)
- Call/return: `r31` is link register (set by `JAL`/`JALR`)
- Suggested ABI (tweak to match compiler backend):
  - Return: `r1`
  - Args: `r3–r10`
  - Saved: `r11–r28`
  - Stack pointer: `r29`; frame pointer: `r30`

## Memory model
- Separate execute-only code and read/write data; **W^X** enforced by the host/emulator
- Address space is tightly bounded; null page is write-protected (reads allowed)
- No self-modifying code

## Control flow
- `JAL`, `JALR` for calls; `HALT` to stop; `YIELD` for timing/cooperative scheduling

## Comparison without flags
- Use explicit compare (`SLT/SLTU/SEQ/SNE/…`) + branch (`BEQ/BNE/BLT/BGE/…`)
- No carry/overflow flags—favor wide types or explicit sequences when needed

## Exceptions & faults
- No hardware interrupts
- Faults (e.g., div-by-zero) are routed via **IST metadata** in the `.s32x` file; the loader/emulator delivers control

## I/O and host facilities
- **Now**: `DEBUG reg` emits a byte/word to host debug stream
- **Next**: **MMIO ring buffers** (shared memory) and **TRAP** calls for structured host services (timers, files, sockets)

## Calling convention (draft)
- Callee-saved: `r11–r28`, `r30`
- Caller-saved: everything else
- Return in `r1`; spill to stack via `r29`

## Example: Hello via DEBUG
```asm
        .section .rodata
msg:    .ascii  "Hello, SLOW-32!\n\0"

        .section .text
        .globl  _start
_start:
        li      r3, msg
.loop:
        ldbu    r2, r3+0
        beq     r2, r0, .done
        debug   r2
        addi    r3, r3, 1
        jal     r0, .loop
.done:
        halt
```