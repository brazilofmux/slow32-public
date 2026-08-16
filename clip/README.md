# s32-clip

First-slice Clipper: a `.prg` becomes a `.s32x`.

Control flow is C (`IF`, `DO WHILE`, `EXIT`, `LOOP`). Expressions
still go through the existing dBase evaluator, so `STR`, `MOD`,
dates, and field names keep interpreter semantics. The generated
program links the dBase sources minus the REPL.

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

This slice: assignment, `STORE`, `?` / `??`, `IF/ELSEIF/ENDIF`,
`DO WHILE/ENDDO`, `DO CASE`, `FUNCTION`/`PROCEDURE` (with
`PARAMETERS`/`PRIVATE`/`RETURN`), `SET …`. Unknown single-line
commands become `clip_cmd()` and ride the existing interpreter
(`USE`, `REPLACE`, `DO file`, `INDEX`, `RESTORE`, …).

| Program          | How it compiles                                      |
|------------------|------------------------------------------------------|
| `tests/*.prg`    | C control flow + compiled UDFs                       |
| `import.prg`     | C loops + compiled `util.prg` + `clip_cmd` for DB    |
| `bal_run.prg`    | `RESTORE` + `DO balances` via the interpreter        |
| `util.prg`       | compiled into whoever `SET PROCEDURE TO util`s       |

`FOR`/`SCAN`/`TEXT`/`BEGIN SEQUENCE` are still compile errors.

A full report-suite A/B against the interpreter (`import` / `bal_run` /
`runbal_run` / `reports_all` on `slow32-dbt`) produced byte-identical
output for eleven of twelve reports. The twelfth differs because
nested `DO reports_all` overflows the interpreter's IF stack; the
compiled driver matches a standalone `DO activity`. Workdirs and
`.prn` files stay in `out/` (gitignored). Do not copy private
datasets into this tree.
