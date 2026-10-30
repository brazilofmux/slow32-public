# SLOW-32 xBase — the interpreter

A dot-prompt interpreter for the xBase language, compatible with dBASE
III Plus (Ashton-Tate, 1986), with a number of additions from CA-Clipper
and FoxPro: `LOCAL`, arrays
(`DECLARE`/`DIMENSION` and the `A*()` functions), the low-level file
functions (`FOPEN`, `FREAD`, ...), `SCAN`/`ENDSCAN`, `SCATTER`/`GATHER`,
`DEFINE MENU`/`POPUP`. It is written in C, compiled by clang for
SLOW-32 and linked against the MMIO C runtime, and it runs on any of the
emulators. It reads and writes dBASE III `.DBF` files and `.DBT` memo
files, and `.FRM` report and `.LBL` label definitions. It has run real
programs, including the user's own, and is used as an oracle for them.

## Build

    ./build.sh

`build.sh` compiles each `src/*.c` with `~/llvm-project/build/bin/clang`
(`OPT`, default `-O1`), assembles with `../tools/assembler/slow32asm` and
links `xbase.s32x` with `../tools/linker/s32-ld` (`--mmio 64K`, a 64 MB
heap). `xbase.s32x` is checked in, so a rebuild shows as a modified file.
`src/csv_import.c` is generated from `src/csv_import.rl` by
`./gen_csv_parser.sh` (needs `ragel`); the generated file is checked in.

## Run

    ../tools/emulator/slow32-fast xbase.s32x               # full screen, term service
    ../tools/emulator/slow32-fast --deny term xbase.s32x   # line mode, as the tests run

There are no command-line arguments: it starts at the dot prompt and
reads commands from stdin; a program is run with `DO <name>` (the `.PRG`
in the current directory). Files are created in the current directory.

```
. CREATE PEOPLE
Field name: NAME / C / 12, AGE / N / 3 / 0, then a blank name
. USE PEOPLE
. APPEND BLANK
. REPLACE NAME WITH "Ada", AGE WITH 36
. APPEND BLANK
. REPLACE NAME WITH "Grace", AGE WITH 45
. LIST
Record# NAME         AGE
      1 Ada           36
      2 Grace         45
. INDEX ON NAME TO PNAME
2 record(s) indexed.
. SEEK "Grace"
. ? FOUND(), RECNO(), TRIM(NAME)
.T. 2 Grace
. QUIT
```

## Tests

    ./tests/run-tests.sh              # 109/109 passed (2026-10-29), ~3 s
    ./tests/run-host-date-numeric.sh  # date and N-field code, host compiler

`run-tests.sh` feeds each `tests/*.txt` to the interpreter on slow32-fast
with the terminal service denied and compares the output with
`tests/expected/<name>.expected` (105 scripts), then runs three timed
`INKEY` cases with the term service. `EMU` selects another engine. The
expected files are this interpreter's own output, not output captured
from dBASE III.

## What it supports

[docs/STATUS.md](docs/STATUS.md) is the inventory: about 80 commands,
124 built-in functions, the SET options, operators and scopes. In short:
navigation and editing, `LIST`/`DISPLAY`, `COUNT`/`SUM`/`AVERAGE`/
`CALCULATE`, `SORT`, `COPY TO` and `APPEND FROM` (also `DELIMITED`),
10 work areas with `alias->field` and `SET RELATION`, `SET FILTER`,
memory variables with `PRIVATE`/`PUBLIC`/`LOCAL`, macro substitution,
`.PRG` programs with procedures and user-defined functions, `ON ERROR`/
`RETRY`, `@ SAY/GET`/`READ`, `BROWSE`/`EDIT`, menus, `REPORT FORM`,
`LABEL FORM`, and B+ tree indexes with `SEEK`/`FIND`.

## What it does not

- `MODIFY REPORT` and `MODIFY LABEL` print "not implemented", and
  any other `MODIFY` (`STRUCTURE`, `COMMAND`) does nothing at all.
- `UPDATE ON` searches the target for each key rather than walking two
  sorted files, so it needs neither sorted (`RANDOM` is accepted); `JOIN`
  and `TOTAL` leave memo fields out of the file they write.
- `.NDX` files are this interpreter's own B+ tree format (`NDX2`), not
  dBASE III's: an index built by dBASE cannot be opened; rebuild it
  with `INDEX ON`. No `.NTX`, `.MDX` or `.CDX`.
- `CREATE` makes the file but does not leave it in use; `USE` it.
- Fixed limits (`src/*.h`): 2,000 lines per program file, 255
  characters per line (continuations included), 256 memory variables,
  10 work areas, 7 indexes per work area, 100-byte index keys, 128
  fields, 32 nested calls.
- Several SET options are accepted and ignored (`STATUS`, `SCOREBOARD`,
  `MENU`, `INTENSITY`, `FUNCTION`, `PRINT`, `HELP`); the table in
  STATUS.md marks them. "Printer" output goes to stdout.

## More

- [docs/STATUS.md](docs/STATUS.md) -- commands, functions, SET options.
- [docs/STAGES.md](docs/STAGES.md) and the six `docs/STAGE*.md` files --
  the original build-up plan; [docs/ROADMAP.md](docs/ROADMAP.md) -- the
  phased plan after it (done, kept as the record).
- [docs/TEACHERS-PET-ANALYSIS.md](docs/TEACHERS-PET-ANALYSIS.md) -- the
  gap analysis against a 1980s dBASE application that drove the roadmap.
- [docs/OPTIMIZATION-AND-COMPLETENESS.md](docs/OPTIMIZATION-AND-COMPLETENESS.md)
  -- an earlier plan, superseded by ROADMAP.md.
- [ISSUES.md](ISSUES.md) -- open and resolved items, including the
  2026-09-05 performance rounds.

## Trademarks

dBASE and Clipper (CA-Clipper) are trademarks of their respective owners.
SLOW-32 xBase is not affiliated with or endorsed by them; the names appear
here only to say which programs and files it is compatible with.
