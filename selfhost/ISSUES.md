# Code Review Issues: SLOW-32 Self-Hosting Toolchain

This document captures bugs, inconsistencies, and opportunities for improvement identified during the code review of the `./selfhost` directory.

## Stage 0: Emulator (`s32-emu.c`)

### 1. [BUG] Pathname Truncation in `OP_MMIO_OPEN`
The implementation of `OP_MMIO_OPEN` (and `OP_MMIO_STAT`) incorrectly truncates the last character of the provided pathname:
```c
uint32_t plen = (length < sizeof(path_buf)) ? length : sizeof(path_buf) - 1;
memcpy(path_buf, data + off, plen);
path_buf[plen] = '\0';
if (plen > 0) path_buf[plen - 1] = '\0'; // <--- BUG: Truncates last char
```
If the guest provides a non-NUL-terminated string (standard for Forth), the last character of the filename is lost. If it *is* NUL-terminated, the truncation is redundant but safe. This makes opening files with 1-character names impossible and potentially breaks all file I/O for standard Forth strings.

### 2. [BUG] `OP_MMIO_STAT` Layout Overlap
The `ctime` field in the `stat` result layout is corrupted:
```c
/* ctime */
tmp = (uint64_t)st.st_ctim.tv_sec;  memcpy(buf + 96, &tmp, 8);
t32 = (uint32_t)st.st_ctim.tv_nsec; memcpy(buf + 104 - 4, &t32, 4); // <--- BUG: Overwrites buf[100..103]
```
`buf + 104 - 4` is `100`. Since the `sec` field at `96` is 8 bytes long, it occupies `96..103`. Writing `nsec` at `100` overwrites the upper 4 bytes of `sec`.

### 3. [CRITICAL] Lack of Memory Bounds Checking
The emulator performs no bounds checking on guest memory accesses (`rd32`, `wr32`, `LDB`, `STW`, etc.). A malicious or buggy guest program can read/write anywhere in the host process's memory space.

### 4. [STABILITY] Stack Allocation Risk in `load_s32x`
`load_s32x` allocates memory based on the header's `mem_size` but does not ensure it covers the `stack_base`. If `stack_base` is outside the allocated `mem_size`, the emulator will segfault on the first stack access.

### 5. [PORTABILITY] Host Endianness Dependency
The memory access helpers (`rd32`, `wr32`) use `memcpy` directly into host integers, making the emulator non-portable to Big Endian hosts.

---

## Stage 2: Forth Assembler (`asm.fth`)

### 6. [BUG] `.byte` and `.word` Limited to Single Values
The `DO-BYTE` and `DO-WORD` directives only process the first token on a line:
```fth
: DO-BYTE   GET-TOK PARSE-IMM 255 AND EMIT-B ;
```
Lines like `.byte 1, 2, 3` will only emit the value `1`, and the rest of the line is ignored. This is a significant limitation for data-heavy assembly.

### 7. [INCONSISTENCY] PC-Relative Reference Points
The reference point for PC-relative offsets differs between instructions:
- `JAL` (0x40): Relative to `PC`.
- `BEQ/BNE/...` (0x48+): Relative to `PC + 4`.
While the assembler compensates for this in `PARSE-TARGET` vs `PARSE-BTARGET`, it is an inconsistent design that complicates manual assembly and debugging.

---

## Stage 4: C Compiler (`cc.fth`)

### 8. [CROSS-STAGE BUG] String Truncation in Self-Host
The C compiler emits strings as multiple values on a single `.byte` line:
```fth
EMIT-INDENT S" .byte " OUT-STR
tmp-b @ 0 ?DO
    I 0<> IF 44 OUT-CHAR 32 OUT-CHAR THEN
    str-pool tmp-a @ + I + C@ OUT-NUM
LOOP
```
Because the Stage 2 assembler (Issue #6) only parses the first value, **all string literals in compiled programs will be truncated to a single character** when using the self-hosted toolchain.

### 9. [DOC] Misleading Type Encoding Comments
Comments in `cc.fth` claim that `TY-STRUCT` indices and array counts overlap in the type word (bits 16-31), but the code actually uses disjoint ranges (14-21 for structs, 22-31 for arrays). The comments should be updated to reflect the actual (correct) implementation.

### 11. [CLEANUP] Dead Code and Redundancy in `EMIT-LI-R2`
The `EMIT-LI-R2` function contains redundant calculations and dead code that sets `tmp-a` but never uses it, differing from the cleaner `EMIT-LI-R1`:
```fth
DUP 16 RSHIFT MASK12 AND  tmp-a !  \ Dead code
DUP MASK12 AND tmp-b !
```
This should be cleaned up to match the logic in `EMIT-LI-R1`.

---

## Documentation Opportunities

### 10. `ISA-ENCODING.md` Errors
- The "Operation" column for `SLTI` is a copy-paste error from `ADDI`.
- Opcode ranges for R-type vs I-type are not clearly defined, leading to "I-type opcodes" like `0x18` (SGT) being used as R-type instructions.
