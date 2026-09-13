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
- **Memory**: `!`, `@`, `C!`, `C@`, `C,`, `2!`, `2@`, `MOVE`, `SEARCH`
- **I/O**: `EMIT`, `KEY`, `TYPE`, `.`, `.S`, `CR`, `ACCEPT`
- **Compiler**: `:`, `;`, `:NONAME`, `IMMEDIATE`, `,`, `ALLOT`, `[`, `]`, `CREATE`, `DOES>`, `S"`, `."`, `C"`, `S\"`, `'`, `[']`, `LIT`, `CHAR`, `[CHAR]`, `RECURSE`, `POSTPONE`
- **Control flow**: `IF`, `ELSE`, `THEN`, `BEGIN`, `AGAIN`, `UNTIL`, `WHILE`, `REPEAT`
- **Loops**: `DO`, `?DO`, `LOOP`, `+LOOP`, `I`, `J`, `UNLOOP`, `LEAVE`
- **Variables**: `STATE`, `BASE`, `BASE!`, `LATEST`, `HERE`, `TIB`, `TOIN`, `NTIB`
- **Parser**: `WORD`, `FIND`, `NUMBER`, `PARSE-WORD`, `PARSE`, `INTERPRET`
- **Strings**: `COUNT`
- **Pictured Output**: `<#`, `HOLD`, `#>`
- **Exception**: `CATCH`, `THROW`
- **Search-Order**: `FORTH-WORDLIST`, `GET-CURRENT`, `SET-CURRENT`, `GET-ORDER`, `SET-ORDER`, `SEARCH-WORDLIST`, `WORDLIST`
- **System**: `ABORT`, `EVALUATE`
- **Facility**: `MS`
- **Tube** (docs/TUBE.md): `TUBE-INIT`, `TUBE-OPEN`, `TUBE-CLOSE`, `TUBE-PRESENT`, `TUBE-INFO`, `TUBE-STATUS`, `TUBE-KEYS`
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
- **Compiler**: `LITERAL`, `COMPILE,`
- **Defining**: `VARIABLE`, `CONSTANT`, `2VARIABLE`, `2CONSTANT`, `VALUE`, `TO`, `DEFER`, `IS`, `ACTION-OF`, `MARKER`, `BUFFER:`
- **Control flow**: `CASE`, `OF`, `ENDOF`, `ENDCASE`
- **Pictured Output**: `MU/MOD`, `#`, `#S`, `SIGN`, `U.`, `.R`, `U.R`, `HOLDS`
- **Core Arithmetic**: `SM/REM`, `FM/MOD`, `*/MOD`, `*/`, `>NUMBER`
- **Double-Number**: `D>S`, `DNEGATE`, `DABS`, `D0=`, `D0<`, `D=`, `D<`, `M+`, `D.`, `D.R`
- **Strings**: `CMOVE`, `CMOVE>`, `/STRING`, `COMPARE`, `PLACE`, `-TRAILING`
  (`SEARCH` and `MOVE` are kernel primitives)
- **System**: `>BODY`, `SOURCE`, `WITHIN`, `ALIGNED`, `ALIGN`, `ABORT"`, `>IN`, `PAD`, `REFILL`, `NOOP`
- **Stack**: `ROLL`
- **I/O**: `.(` (immediate display)
- **Compiler**: `[DEFINED]`, `[UNDEFINED]`
- **Search-Order**: `DEFINITIONS`, `ALSO`, `PREVIOUS`, `FORTH`, `ONLY`, `ORDER`
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
S32_STDIN_PREFIX=prelude.fth emulator kernel.s32x   # prelude first, then your terminal
```
During prelude loading, prompts are suppressed (`var_prompt_enabled=0`). The prelude's last line runs `PROMPTS-ON` to enable the "ok> " prompt for interactive use.

## Usage
```bash
cd forth && bash build.sh
```
This assembles, links, and runs the kernel with the prelude loaded.

## The Tube (vec words)

`tube.fth` is the 1987-desk fence post: the DVG names in their own
wordlist over the tube's `vec` mode. `MOVE` means "beam" only while the
`TUBE` vocabulary is in the search order; `PRESENT` appends `END`, ships
the display list, bumps the generation, and wipes for the next frame.
`GLASS-KEY` polls the viewer's make/break queue (key-downs only).

```bash
cat prelude.fth tube.fth > /tmp/pre.fth; S32_STDIN_PREFIX=/tmp/pre.fth ../tools/emulator/slow32-fast kernel.s32x
# in another terminal: ../tools/s32-crt-mac   (or s32-crt)
```
```forth
TUBE-ON .            \ -1 = glass ready; degrades to a message without a tube
ALSO TUBE
WIPE 2048 2048 MOVE 3000 3000 DRAW PRESENT
```

`ship.fth` is the demo the fence post asked for: a ship flown from the
glass (arrows rotate, up thrusts, ESC lands at `ok>`), whose words —
`SHIP`, `STEP`, `SHAPE` — are small and `DEFER`red so you can redefine
them at the prompt and `FLY` again with position, velocity, and heading
intact. The arcade stays on while you rewire it.

## Bugs Fixed

- **Blank line EOF**: Empty lines (just `\n`) were treated as EOF. Fixed ACCEPT to return -1 on true EOF, and cold_start to check for -1 instead of 0.
- **INVERT 12-bit**: `not` pseudo-instruction used `xori rd, rs, -1` which only XORs bottom 12 bits (XORI uses zero-extended immediate). Fixed to use `addi r2, r0, -1` then `xor`.
- **MMIO output buffering**: Switched all output to `debug` instruction (immediate, unbuffered).

## Interactive use: the prelude goes in front of stdin, not through a pipe

The C++ engines (slow32, slow32-fast, slow32-dbt) honour `S32_STDIN_PREFIX=FILE`:
the guest's reads of fd 0 are served from FILE first, then from the real
stdin. So `S32_STDIN_PREFIX=prelude.fth emu kernel.s32x` gives a kernel with
the prelude loaded and your terminal behind it, and the emulator is the only
process on that terminal: BYE or Ctrl-D ends it, nothing lingers. The older
`cat prelude.fth - | emu kernel.s32x` left `cat` holding the terminal after
BYE (a shell waits for every member of a pipeline), and inside a container's
`-t` the two readers did not behave the same way twice. Pipes of files that
end (the test harnesses) are unaffected. qemu-system-slow32 has its own MMIO
implementation and no prefix; `s32forth` uses slow32-fast.

## Container

`slow32:forth` (`Dockerfile.forth` at the tree root, FROM `slow32:base`)
carries this kernel -- assembled and linked in the image from `kernel.s`
by `build.sh`, which honours `S32_AS`, `S32_LD`, `S32_RT` and
`FORTH_RUN=0` -- with `prelude.fth` and `tube.fth`, and forthc (see
`../forthc/README.md`).  `s32forth` runs the kernel with the prelude
loaded, any files named, then stdin:

    podman run --rm -i slow32:forth s32forth              # a session
    podman run --rm -v $(pwd):/data slow32:forth s32forth prog.fth

`s32forth` is the emulator alone on the terminal with
`S32_STDIN_PREFIX` pointing at the prelude (plus any files named).
`tests/run-tests.sh` honours `EMU`, `S32_FORTH_KERNEL` and
`S32_FORTH_PRELUDE`, which is how ~/builder runs it inside the image
before pushing.
