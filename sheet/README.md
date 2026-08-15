# SLOW-32 Sheet

A 1-2-3-shaped calc. Interactive mode is a `term.h` grid (arrows,
Enter to edit, `/` for Save/Load/Quit). `--line` is the old REPL,
which the tests use.

```bash
./build.sh
../tools/emulator/slow32-fast sheet.s32x
../tools/emulator/slow32-fast sheet.s32x --line
./tests/run-tests.sh
```

```
> A1=10
A1: 10
> A2=20
A2: 20
> A3==A1+A2
A3: 30    [A1+A2]
> A4==@SUM(A1:A3)
A4: 60    [@SUM(A1:A3)]
> LIST
> SAVE books.sht
> QUIT
```

- Numbers, labels, formulas (`=` or `+` prefix)
- A1 refs, `$A$1` accepted (same cell — fill/copy later)
- `+ - * / ( )`, unary minus
- `@SUM @AVG @MIN @MAX @COUNT` over ranges or lists
- Cycle detection (`#CYCLE!`) and `#DIV/0!`
- Text `.sht` save/load (keeps formulas)
- Lotus `.wk1` / `.wks` subset: INTEGER, NUMBER, LABEL; FORMULA
  reads the cached value. Formulas become numbers on WK1 export.
- dBase III `.dbf` interchange: columns A.. become fields A..,
  used rows become records. SAVE/LOAD pick the format from the
  extension.

Grid is A–Z by rows 1–64. No macros, no 3D workbooks, no graphs.
