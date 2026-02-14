# Stage 1: Hand-Assembled Forth Kernel

## Goal

A working Forth REPL as a .s32x binary, assembled entirely by hand (no assembler tool needed). This provides the interactive programming environment for stages 2-4.

## Approach: Use the Existing Kernel

The existing Forth kernel (`forth/kernel.s`) is already a complete, tested implementation:

- 5,254 lines of SLOW-32 assembly
- ~150 words (all assembly-defined primitives)
- Direct Threaded Code (DTC) architecture
- ANS Forth Core + Core Extensions complete
- 20/20 Forth tests, 51/51 regression tests passing

For a **pure bootstrap** (no assembler), we need a *minimal* kernel — hand-assembled as raw bytes. For a **practical bootstrap**, we simply cross-compile the existing kernel with the existing assembler.

## Minimal Bootstrap Kernel

A minimal kernel needs ~35 primitives and ~500-800 machine instructions:

### Essential Primitives (~35 words)

**Inner interpreter (3):**

- `EXIT` — return from colon definition
- `DOCOL` — enter colon definition (sets up IP)
- `LIT` — push inline literal to data stack

**Execution (1):**

- `EXECUTE` — call execution token

**Stack manipulation (6):**

- `DUP`, `DROP`, `SWAP`, `OVER` — basic stack ops
- `>R`, `R>` — return stack transfer

**Arithmetic (5):**

- `+`, `-`, `*` — basic math
- `AND`, `OR` — bitwise (XOR can be defined later)

**Comparison (3):**

- `=`, `<`, `>` — comparisons (others derived from these)

**Memory (4):**

- `@`, `!` — word fetch/store
- `C@`, `C!` — byte fetch/store

**I/O (3):**

- `EMIT` — output character (via DEBUG instruction)
- `KEY` — input character (via GETCHAR MMIO)
- `TYPE` — output string (can be Forth-defined from EMIT)

**Control flow (2):**

- `BRANCH` — unconditional branch (offset follows in thread)
- `0BRANCH` — conditional branch (if TOS == 0)

**Compiler (8):**

- `:` — start definition
- `;` — end definition
- `COMMA` (`,`) — compile cell to dictionary
- `ALLOT` — advance HERE
- `[` / `]` — switch interpret/compile state
- `IMMEDIATE` — mark word as immediate
- `CREATE` — make new dictionary entry

**Parser (4):**

- `WORD` — parse next token
- `FIND` — look up word in dictionary
- `NUMBER` — convert string to number
- `INTERPRET` — the outer interpreter loop

**Variables (4):**

- `STATE`, `BASE`, `HERE`, `LATEST`

### Not Needed for Minimal Bootstrap

These are in the existing kernel but can be added later via Forth definitions or deferred:

- `PICK`, `DEPTH`, `DSP@` — advanced stack inspection
- `/`, `MOD`, `/MOD` — division (can use shift-subtract algorithm in Forth)
- `LSHIFT`, `RSHIFT` — can defer until assembler needs them
- `DO`/`LOOP` — can use `BEGIN`/`UNTIL` instead
- `CATCH`/`THROW` — exception handling
- File I/O words — not needed until stage 5
- Search order words — single wordlist suffices

## Threading Model

The existing kernel uses **Direct Threaded Code (DTC)**:

```
Register allocation:
  r26 = IP  (Instruction Pointer — points into thread)
  r27 = RSP (Return Stack Pointer)
  r28 = DSP (Data Stack Pointer)
  r25 = W   (Working register — current XT)
  r29 = SP  (System stack, preserved for C interop)
```

**NEXT** (the inner interpreter):
```asm
next:
    ldw  r25, r26, 0      # W = *IP (load execution token)
    addi r26, r26, 4      # IP++
    ldw  r2, r25, 0       # r2 = *W (load code field)
    jalr r0, r2, 0        # jump to code
```

**DOCOL** (enter colon definition):
```asm
docol:
    addi r27, r27, -4     # RSP--
    stw  r27, r26, 0      # push IP to return stack
    addi r26, r25, 4      # IP = W + 4 (skip code field)
    jal  r0, next         # continue
```

**EXIT** (return from colon definition):
```asm
exit_word:
    ldw  r26, r27, 0      # IP = pop return stack
    addi r27, r27, 4      # RSP++
    jal  r0, next         # continue
```

## Dictionary Structure

Each word in the dictionary has this layout:

```
+0: Link pointer (4 bytes) — points to previous word's link field
+4: Name length (1 byte) — bit 7 = IMMEDIATE flag
+5: Name string (N bytes) — uppercase ASCII
+?: Padding to 4-byte alignment
+?: Execution Token (XT) — points to code field
    Code field (4 bytes) — address of machine code
    [Parameter field...] — thread for colon defs, data for variables
```

The dictionary is a singly-linked list. `LATEST` points to the most recently defined word. `FIND` traverses the list comparing names.

## Hand-Assembly Process

For a pure bootstrap, each instruction must be manually encoded as a 32-bit little-endian word. The process is:

1. **Write assembly on paper** — use the ISA encoding reference
2. **Encode each instruction** — compute the 32-bit value by hand
3. **Construct the .s32x file** — 64-byte header + section data
4. **Write as binary** — use a hex editor or a tiny host program

The .s32x header fields for a minimal kernel:

```
magic       = 0x53333258
version     = 1
endian      = 0x01
machine     = 0x32
entry       = 0x00000000  (code starts at address 0)
nsections   = 3           (.text, .data, .bss)
sec_offset  = 64          (immediately after header)
...
code_limit  = (end of .text)
stack_base  = 0x0FFFFFF0
mem_size    = 0x10000000
heap_base   = (after BSS)
```

### Example: Encoding `DUP` (push TOS again)

```asm
# DUP ( a -- a a )
# DSP in r28
dup_word:
    ldw   r3, r28, 0        # r3 = TOS
    addi  r28, r28, -4      # make room
    stw   r28, r3, 0        # store copy
    jal   r0, next           # NEXT
```

Instruction 1: `LDW r3, r28, 0` — opcode 0x32, rd=3, rs1=28, imm=0
```
= 0x32 | (3 << 7) | (28 << 15) | (0 << 20)
= 0x32 | 0x180 | 0xE0000 | 0
= 0x000E01B2
```

(And so on for each instruction.)

## Practical Shortcut

Rather than hand-assembling ~800 instructions, use the existing kernel:

```bash
cd forth && bash build.sh
# Produces forth/kernel.s32x — a complete Forth system
```

This binary is built by the existing assembler/linker from `forth/kernel.s` and can serve directly as stage 1. The hand-assembly path exists for those who want a fully auditable bootstrap from first principles.

## Memory Requirements

| Region | Size | Purpose |
|--------|------|---------|
| .text | ~6 KB | Kernel code (~1,400 instructions) |
| .data | ~2 KB | Variables, TIB, strings |
| .bss | ~66 KB | Dictionary space (65,536 bytes) + stacks |
| Stack | 1 KB | Data stack (256 cells) |
| Return stack | 1 KB | Return stack (256 cells) |

The minimal kernel is much smaller — ~2 KB of code, ~1 KB of data.

## Verification

1. Boot the kernel, verify `ok>` prompt appears
2. Test basic arithmetic: `3 4 + .` should print `7`
3. Test colon definitions: `: SQUARE DUP * ; 5 SQUARE .` should print `25`
4. Test string output: `." Hello" CR`
5. Verify `WORD`, `FIND`, `NUMBER` work correctly
6. Define a few utility words to confirm the compiler works

## What Stage 1 Provides to Stage 2

The Forth kernel gives stage 2 (the assembler) everything it needs:

- An interactive REPL for development and testing
- `CREATE` / `DOES>` for defining assembler word syntax
- `,` (comma) for emitting binary data
- `ALLOT` for reserving space
- String handling via `WORD`, `FIND`
- Arithmetic and bitwise operations for instruction encoding
- File I/O (if using the full kernel with MMIO) for writing .s32o files
