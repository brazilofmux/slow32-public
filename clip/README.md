# s32-clip

Clipper for SLOW-32: a `.prg` becomes a `.s32x`.  The plan:
[docs/plans/clipper.md](../docs/plans/clipper.md) (steps 0-2 done, 4
under way).

s32-clip parses the program with dbase's own parser at compile time and
writes C: control flow is C, and expressions are C calls on the
interpreter's own operations (`ast_eval_binop`, `ast_eval_name`, ...),
so `STR`, `MOD`, dates and field names keep interpreter semantics by
construction.  The generated program links the dBase sources minus the
REPL, which runs what is not compiled yet (most commands) and what never
can be (macros).

`tests/run-oracle.sh` is the gate: every program of the corpus
(`clip/tests`, `dbase/tests/*.PRG`) run by the interpreter and compiled
must print the same bytes (45 of 45 that compile, 2026-10-29).  On
`tests/bench1.prg` scaled up 50x, the compiled program runs 7.6x faster
than the interpreter under slow32-dbt (0.18 s against 1.37 s).  A real
dBASE III report suite (a ledger's import, balances and twelve reports,
run locally) compiles whole -- the program files it DOes become compiled
procedures -- and prints the same reports, leaving identical tables,
2.9x to 3.6x faster per phase (11.5 s to 3.4 s for the suite).

```bash
./build.sh
./compile-prg.sh tests/hello.prg
../tools/emulator/slow32-fast -q out/hello.s32x
./tests/run-tests.sh
```

Cwd matters the same way it does for the interpreter: `DO file` loads
`.prg` files from the current directory. `SET PROCEDURE TO foo` inlines
`foo.prg` from the source directory at compile time when that file
exists, so the compiled image does not need it at runtime.

Majesty (`~/majesty/dbase/`, after the CREATE/TXT setup in
`run_dbase_s32.sh`):

```bash
./compile-prg.sh ~/majesty/dbase/import.prg out/import.s32x
./compile-prg.sh ~/majesty/dbase/balances.prg out/balances.s32x
./compile-prg.sh ~/majesty/dbase/bal_run.prg out/bal_run.s32x
cd ~/majesty/tmp/dbase-all   # util.prg, CONFIG.MEM, .DBF/.TXT live here
~/slow-32/tools/emulator/slow32-fast -q ~/slow-32/clip/out/import.s32x
~/slow-32/tools/emulator/slow32-fast -q ~/slow-32/clip/out/bal_run.s32x
```

`import.prg` and `balances.prg` lower their `DO WHILE` merge/scan to
C. `SET PROCEDURE TO util` compiles `util.prg` into the same image
(`PAD_ZERO` is a real C function the evaluator calls). `bal_run.prg`
is a three-liner (`RESTORE` / `DO balances` / `QUIT`). The other
report drivers (`reports_all`, `pl`, `bs`, `journal`, `activity`,
`runbal`) also compile; they `DO` the remaining `.prg` files through
the interpreter.

Compiled: assignment, `STORE`, `?` / `??`, `IF/ELSEIF/ENDIF`,
`DO WHILE/ENDDO`, `DO CASE`, `FOR/NEXT`, `SCAN/ENDSCAN`,
`TEXT/ENDTEXT`, `BEGIN SEQUENCE/BREAK/RECOVER/END`,
`FUNCTION`/`PROCEDURE` (with `PARAMETERS`/`PRIVATE`/`LOCAL`/`RETURN`; a
`LOCAL` shadows the outer variable for the call and restores it after),
and every expression without a macro.  `DO name` of a procedure compiled
into the image is a direct call; with `WITH` it goes through the
interpreter's `DO`, which reaches compiled procedures through a hook and
passes variables by reference as dBASE does.  Errors name the file and
line, as the interpreter's do.  Other single-line commands become
`clip_cmd()` and ride the interpreter (`USE`, `REPLACE`, `DO file`,
`INDEX`, `RESTORE`, …).  `SUSPEND` is refused: a compiled program has no
interactive mode.

| Program          | How it compiles                                      |
|------------------|------------------------------------------------------|
| `tests/*.prg`    | C control flow + compiled UDFs                       |
| `import.prg`     | C loops + compiled `util.prg` + `clip_cmd` for DB    |
| `bal_run.prg`    | `RESTORE` + `DO balances` via the interpreter        |
| `util.prg`       | compiled into whoever `SET PROCEDURE TO util`s       |


A full report-suite A/B against the interpreter (`import` / `bal_run` /
`runbal_run` / `reports_all` on `slow32-dbt`) produced byte-identical
output for eleven of twelve reports. The twelfth differs because
nested `DO reports_all` overflows the interpreter's IF stack; the
compiled driver matches a standalone `DO activity`. Workdirs and
`.prn` files stay in `out/` (gitignored). Do not copy private
datasets into this tree.
