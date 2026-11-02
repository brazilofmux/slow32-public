# SLOW BASIC

The BASIC of QBasic 1.1, with GW-BASIC's line-numbered habits: line
numbers optional, labels, real SUB/FUNCTION, optional LET, DATA/READ,
typed variables (`%` 16-bit INTEGER, `&` LONG, `!` or none SINGLE, `#`
DOUBLE, `$` STRING), TYPE records, ON ERROR, file I/O, and the GW-BASIC
graphics statements drawn on the tube.  **[LANGUAGE.md](LANGUAGE.md)
is the dialect, rule by rule**, with QBasic 1.1 as its authority.  It is
an interpreter of about 12,800 lines of C in `src/`: each program unit
is lowered to flat code (`src/lower.c`) and run; built with clang for
SLOW-32 and linked against the MMIO libc, so it runs on any of the
emulators.  The design and the stage-by-stage record are in
[PLAN.md](PLAN.md).

## Build

```bash
./build.sh            # clang -O1 per file, then s32-ld --mmio 64K --stack-size 1M --heap-size 16M
OPT=-O2 ./build.sh    # another optimization level
```

`build.sh` compiles each `src/*.c` with `~/llvm-project/build/bin/clang`
and `llc`, assembles with `tools/assembler/slow32asm`, and links
`runtime/crt0.s32o`, the objects, `libc_mmio.s32a` and `libs32.s32a`
into `sbasic.s32x`.  The `.s32x` is tracked in git; a rebuild changes
it, so restore it (`git checkout -- sbasic.s32x`) unless you mean to
commit a new one.

There is no script here for building with stage08 cc.  That build was
done by hand on 2026-08-24 (all 13 translation units, output identical
to the clang build on the 45 tests; recorded in
[docs/plans/engine-room.md](../docs/plans/engine-room.md), milestone 3).

## Run

```bash
../tools/emulator/slow32-fast sbasic.s32x tests/hello.bas      # load a file and run it
printf 'x = 6 * 7\nPRINT "x ="; x\nRUN\n' | ../tools/emulator/slow32-fast sbasic.s32x
```

With a file argument it loads the file and runs it.  Without one it
reads standard input as a stream: each line is added to the program
until one of the commands `RUN`, `LIST`, `NEW`, `LOAD file`,
`SAVE file`, `DELETE n[-m]` or `BYE` arrives.  There is no prompt and
no immediate mode: a line is never executed on its own, only by `RUN`.
Lines after `RUN` are what the program's `INPUT` reads.
`$INCLUDE: 'file.bas'` (also in a `'` or `REM` comment, QBasic style)
splices in another file, nested up to 16 deep.

The emulator prints its own status lines (`Starting execution`, `HALT`,
instruction counts) around the program's output; the test script
filters them.

### Graphics

`SCREEN 1/2/7/8/9/12/13` opens a framebuffer on the tube
([docs/TUBE.md](../docs/TUBE.md), §4); `PSET`, `PRESET`, `LINE`
(with `B`/`BF`), `CIRCLE`, `PAINT`, `PALETTE`, `POINT` and `CLS` draw
on it, and `INKEY$` reads the viewer's keys.  To watch it, run the
program and attach a viewer from another terminal in the same
directory (the emulator writes `tube.port` there):

```bash
../tools/emulator/slow32-fast sbasic.s32x demos/bounce.bas
../tools/s32-crt-mac/s32-crt-mac       # macOS; or ../tools/s32-crt/s32-crt
```

No viewer is needed: drawing works headless, and
`S32_TUBE_DUMP=<dir>` journals a hash per presented frame.  Under
`--deny tube` it prints `No tube: graphics unavailable` once and the
drawing statements do nothing.  Drawing with no `SCREEN` open is
`Illegal function call`.  `SCREEN 0` closes the framebuffer.

## Tests

```bash
./tests/run-tests.sh                                  # slow32-fast
EMU=$PWD/../tools/dbt/slow32-dbt ./tests/run-tests.sh  # another engine
```

Each `tests/*.bas` is piped in followed by `RUN` and its output
compared with `tests/<name>.expected` (`--update` rewrites them from the
current output, for a change that has been checked).  45/45 on
2026-11-01.

```bash
python3 tests/nbs.py -j 8                    # the NBS suite vs QBasic 1.1
python3 tests/nbs.py -j 8 --dir DIR          # any directory of programs
```

`tests/nbs.py` runs each program on SLOW BASIC and on QBasic 1.1 under
dos-monster and compares the output byte for byte; the corpora and the
current numbers are in [LANGUAGE.md](LANGUAGE.md#how-it-is-checked).  `graphics.bas` checks its
own drawing through `POINT` and needs no viewer.  `bench/` holds four
timing programs (fib, nested loops, sieve, string concatenation) with
no expected output.

## What it does not do

- `SOUND`, `PLAY`, `CHAIN`, `KEY`, `FIELD`, `WAIT`, `OUT`, `PCOPY`,
  `COMMON`, `ENVIRON` and `VIEW PRINT` parse and do nothing, as do
  `ON TIMER` and `ON KEY`.  `DECLARE` is parsed and ignored.
- `SHELL` prints `SHELL not supported:` and the command.
- `PEEK`/`POKE` address a private 64 KB array, not machine memory.
  `STICK`, `STRIG` and `INP` return 0.
- `CIRCLE` has no arcs or aspect ratio; `LINE`'s style argument is
  ignored.  `DRAW` is not implemented.
- Fixed limits: 2,048 program lines of 255 characters, 64 arrays of up
  to 8 dimensions, 128 SUB/FUNCTIONs, 32 TYPEs, 16 open files,
  4,096-character strings; recursion about 330 FUNCTION calls deep.
- The differences from QBasic that are known are listed in
  [LANGUAGE.md](LANGUAGE.md#differences-from-qbasic).

## Pointers

- [PLAN.md](PLAN.md) -- design, stages 1-7, statement lists.
- [demos/bounce.bas](demos/bounce.bas) -- the graphics demo.
- [docs/plans/1987-desk.md](../docs/plans/1987-desk.md) -- why the
  graphics statements exist.
- [pitstop/llvm_ast_stmt_free_o1_miscompile.md](../pitstop/llvm_ast_stmt_free_o1_miscompile.md)
  -- the old `-O1` failure, fixed in the AST code.
- There is no `ISSUES.md` for this directory yet.
