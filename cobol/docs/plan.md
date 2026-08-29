# Plan

No code until this document says a stage has started. The stages
are ordered so each one is a claim the project can make, and so
work that shares machinery lands together. Sizes are honest
guesses in cobc370's units: **S** an afternoon, **M** a day, **L**
several.

## Stage 0 — specifications (this directory)

Done when the docs here match the rulings and someone can implement
Stage 1 without inventing a product. **This is the current stage.**

## Stage 1 — host skeleton + PICTURE + hello **L**

- Directory layout as in [architecture.md](architecture.md)
- Fixed-format and free-format readers
- Recursive-descent parser for Identification Division,
  `WORKING-STORAGE` elementary items, `DISPLAY` of literals,
  `STOP RUN` / `GOBACK`
- Ragel `picture.rl` re-hosted; software edit descriptor, not `ED`
- Emit SLOW-32 assembler, link, run on the emulator
- Refuse unimplemented with a message
- Tests: hello, a refused undeclared name, a refused 74-only verb
  (`ALTER`)

Done: a `.cbl` becomes a `.s32x` that prints a line.

## Stage 2 — data division + MOVE + COMP integer **L**

- Groups, `REDEFINES`, `OCCURS` (fixed, three levels is enough;
  85 allows seven, grow when a program asks)
- Qualification `OF`/`IN`
- Conversion matrix, alphanumeric and COMP integer hot cases
- `IF` / `END-IF` on those
- Structured `PERFORM`

Done: `MOVE` and `ADD` of `PIC S9(8) COMP` and `PIC X(n)` match
GnuCOBOL `-std=cobol85` on checked-in tests.

## Stage 3 — decimal library + edited MOVE **L**

- Canonical numeric, `COMP-3`, DISPLAY numeric, scale, `ROUNDED`,
  `ON SIZE ERROR`
- Edited pictures, de-edit
- `COMPUTE`

Done: cobc370's `arith`/`compute`/`edtest`-shaped tests, rewritten
as 85, green against GnuCOBOL. Majesty amounts (`pic s9(9)v99
comp-3`) round-trip.

## Stage 4 — line sequential I-O **M**

- `SELECT` / `FD` / `OPEN` / `READ` / `WRITE` / `CLOSE`
- `ORGANIZATION IS LINE SEQUENTIAL`
- `ASSIGN` to literal and to data-name
- `AT END` / `END-READ` / `OPTIONAL`

Done: read `data/descriptions_fixed_width.txt` (from majesty, in
place or a stripped fixture), write a copy, diff.

## Stage 5 — indexed I-O **L**

- Default path in [indexed.md](indexed.md)
- `gl039` compiles and runs
- Random `READ` by `desc-id`

Done: gl039 then a tiny reader prints descriptions by id.

## Stage 6 — Report Writer, cheap half **L**

- [report-writer.md](report-writer.md) v1 subset
- Print files are line sequential

Done: **gl022, gl023, gl030** match `reports_cobol/`. This is the
first product claim. GnuCOBOL can still be on the rest of the
majesty path.

## Stage 7 — SCREEN SECTION **M–L**

- [screen.md](screen.md) against `term.h`
- `CBL_GET_SCR_SIZE`

Done: `usescreen.cbl` and `menu.cbl` run.

## Stage 8 — sequential V / RDW **M**

- [framing.md](framing.md)
- Round-trip with tapemgr

Done: a V file this compiler writes is read by tapemgr; a V file
tapemgr writes (or cobc370's `vrec` output, ASCII-translated if
needed) is read back.

## Stage 9 — v1 close

- C ABI `CALL` as used by gl030 (`c_lineartofielded`) if not
  already pulled in by Stage 6
- `FUNCTION ALL INTRINSIC` at the subset gl030 uses
- Document remaining majesty programs as Stage 10+
- cobc370 still untouched

**v1 is done.** GnuCOBOL is retired for the journal and both charts
of accounts, and for the two screen programs.

## After v1 (not scheduled, each when a program asks)

- Rest of majesty batch (`gl024`–`gl043`, relative I-O, `w001`)
- Full Nucleus Level 2 (`STRING`, `INSPECT`, `CORRESPONDING`,
  abbreviated conditions, nested programs, `INITIALIZE`, reference
  modification)
- Report Writer `CONTROL`/`SUM`
- Alternate keys
- dBase-compatible writer filter
- CCVS-85 NC/SQ/IC as a pass/fail suite
- Intrinsic functions beyond what majesty calls

Sort-Merge, Debug, Communication stay out.

## What not to start a stage with

- Copying `fortran/src/hir_*.h`
- Copying `cobc370.c`
- A dBase-openable file as the first indexed test
- Report Writer `SUM` before gl030 prints
- SSA
