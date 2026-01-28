# SLOW-32 Forth Kernel

## Status
Stage 2: Dictionary & Core Primitives (FIND, EXECUTE) are working.

## Architecture
- **Direct Threaded Code**: IP points to a list of XTs.
- **Registers**:
  - `r26`: IP
  - `r27`: RSP
  - `r28`: DSP
  - `r25`: W
  - `r29`: System Stack (preserved)

## Primitives Implemented
- **Control**: `NEXT`, `DOCOL`, `EXIT`, `EXECUTE`, `BYE`.
- **Stack**: `DUP`, `DROP`, `SWAP`, `LIT`.
- **I/O**: `EMIT`, `KEY`.
- **Dictionary**: `FIND`.

## Dictionary Structure
- **Header**:
  - `Link` (4 bytes): Pointer to previous word.
  - `Length` (1 byte): Name length.
  - `Name` (N bytes): Name string.
  - `Padding`: Align to 4 bytes.
  - `XT` (4 bytes): Execution Token (address of code).
- **Alignment**: Uses `.align 2` (4-byte alignment) to match 32-bit words.

## Usage
Currently runs a cold start sequence that searches for "HELLO" in the dictionary and executes it.

## Next Steps (Stage 3)
- Implement `INTERPRET` (Outer Interpreter).
- Implement `WORD` (Parser).
- Implement `NUMBER` (Parser).
- Create the Read-Eval-Print Loop (REPL).