# SLOW-32 Forth Kernel

## Status
Stage 4: Bootstrap Prelude. Self-extending Forth with ~50 standard vocabulary words loaded at startup.

## Architecture
- **Direct Threaded Code**: IP points to a list of XTs.
- **Registers**:
  - `r26`: IP
  - `r27`: RSP
  - `r28`: DSP
  - `r25`: W
  - `r29`: System Stack (preserved)

## Primitives (Assembly)
- **Control**: `EXIT`, `EXECUTE`, `BYE`, `BRANCH`, `0BRANCH`
- **Stack**: `DUP`, `DROP`, `SWAP`, `OVER`, `>R`, `R>`, `R@`, `2>R`, `2R>`, `2R@`, `DEPTH`, `DSP@`, `DSTKTOP`, `PICK`
- **Arithmetic**: `+`, `-`, `*`, `/`, `MOD`, `/MOD`, `NEGATE`, `1+`, `1-`, `2/`, `S>D`, `D+`, `D-`, `UM/MOD`, `UM*`, `M*`
- **Logic**: `AND`, `OR`, `XOR`, `INVERT`, `LSHIFT`, `RSHIFT`
- **Comparison**: `=`, `<>`, `<`, `>`, `0=`, `0<`, `U<`
- **Memory**: `!`, `@`, `C!`, `C@`, `C,`, `2!`, `2@`
- **I/O**: `EMIT`, `KEY`, `TYPE`, `.`, `.S`, `CR`, `ACCEPT`
- **Compiler**: `:`, `;`, `IMMEDIATE`, `,`, `ALLOT`, `[`, `]`, `CREATE`, `DOES>`, `S"`, `."`, `'`, `[']`, `LIT`, `CHAR`, `[CHAR]`, `RECURSE`, `POSTPONE`
- **Control flow**: `IF`, `ELSE`, `THEN`, `BEGIN`, `AGAIN`, `UNTIL`, `WHILE`, `REPEAT`
- **Loops**: `DO`, `?DO`, `LOOP`, `+LOOP`, `I`, `J`, `UNLOOP`, `LEAVE`
- **Variables**: `STATE`, `BASE`, `BASE!`, `LATEST`, `HERE`, `TIB`, `TOIN`, `NTIB`
- **Parser**: `WORD`, `FIND`, `NUMBER`, `PARSE-WORD`, `PARSE`, `INTERPRET`
- **Strings**: `COUNT`
- **Pictured Output**: `<#`, `HOLD`, `#>`
- **System**: `ABORT`, `EVALUATE`
- **Other**: `HELLO`, `PROMPTS-ON`

## Prelude Words (Forth)
Loaded automatically from `prelude.fth` at startup:
- **Stack**: `ROT`, `-ROT`, `NIP`, `TUCK`, `2DUP`, `2DROP`, `2SWAP`, `2OVER`, `?DUP`
- **Arithmetic**: `ABS`, `MIN`, `MAX`, `2*`
- **Constants**: `TRUE`, `FALSE`, `BL`
- **Cell ops**: `CELLS`, `CELL+`, `CHARS`, `CHAR+`
- **Output**: `SPACE`, `SPACES`
- **Base**: `DECIMAL`, `HEX`
- **Memory**: `+!`, `FILL`, `ERASE`, `MOVE`, `BLANK`
- **Comparison**: `<=`, `>=`, `0>`, `0<>`, `U>`
- **Compiler**: `LITERAL`
- **Defining**: `VARIABLE`, `CONSTANT`, `2VARIABLE`, `2CONSTANT`, `VALUE`, `TO`, `DEFER`, `IS`, `ACTION-OF`, `MARKER`, `BUFFER:`
- **Control flow**: `CASE`, `OF`, `ENDOF`, `ENDCASE`
- **Pictured Output**: `MU/MOD`, `#`, `#S`, `SIGN`, `U.`, `.R`, `U.R`, `HOLDS`
- **Core Arithmetic**: `SM/REM`, `FM/MOD`, `*/MOD`, `*/`, `>NUMBER`
- **Double-Number**: `D>S`, `DNEGATE`, `DABS`, `D0=`, `D0<`, `D=`, `D<`, `M+`, `D.`, `D.R`
- **Strings**: `CMOVE`, `CMOVE>`, `/STRING`, `COMPARE`, `SEARCH`, `PLACE`, `-TRAILING`
- **System**: `>BODY`, `SOURCE`, `WITHIN`, `ALIGNED`, `ALIGN`, `ABORT"`, `>IN`, `PAD`, `REFILL`, `NOOP`
- **Stack**: `ROLL`
- **I/O**: `.(` (immediate display)
- **Compiler**: `[DEFINED]`, `[UNDEFINED]`
- **Comments**: `\` (backslash line comment), `(` (paren comment)

## Dictionary Structure
- **Header**:
  - `Link` (4 bytes): Pointer to previous word.
  - `Length` (1 byte): Name length (bit 7 = IMMEDIATE flag).
  - `Name` (N bytes): Name string (uppercase).
  - `Padding`: Align to 4 bytes.
  - `XT` (4 bytes): Execution Token (address of code).

## Bootstrap Mechanism
The `prelude.fth` file is piped to stdin before interactive input:
```bash
cat prelude.fth - | emulator kernel.s32x
```
During prelude loading, prompts are suppressed (`var_prompt_enabled=0`). The prelude's last line runs `PROMPTS-ON` to enable the "ok> " prompt for interactive use.

## Usage
```bash
cd forth && bash build.sh
```
This assembles, links, and runs the kernel with the prelude loaded.

## Bugs Fixed
- **Blank line EOF**: Empty lines (just `\n`) were treated as EOF. Fixed ACCEPT to return -1 on true EOF, and cold_start to check for -1 instead of 0.
- **INVERT 12-bit**: `not` pseudo-instruction used `xori rd, rs, -1` which only XORs bottom 12 bits (XORI uses zero-extended immediate). Fixed to use `addi r2, r0, -1` then `xor`.
- **MMIO output buffering**: Switched all output to `debug` instruction (immediate, unbuffered).
