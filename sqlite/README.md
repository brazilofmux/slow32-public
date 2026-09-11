# SQLite on SLOW-32

Pristine SQLite 3.51.0 (the amalgamation, unpatched: the backend lowers the
32-bit atomic builtins it uses to plain loads and stores), built as a library
for programs on the target, plus the smoke test and SQLite's own shell.

    ./build.sh            # out/libsqlite3.s32a, out/sqlite3.h, out/sqlite3_test.s32x, out/sqlite3.s32x
    ./build.sh --no-shell

## The library

Link it like libc, before the libc archives:

    s32-ld --mmio 64K --code-size 2M --stack-size 256K --heap-size 32M -o app.s32x \
        runtime/crt0.s32o app.s32o sqlite/out/libsqlite3.s32a \
        runtime/libc_mmio.s32a runtime/libs32.s32a

`#include "sqlite3.h"` from `sqlite/out`. The VFS (`slow32_vfs.c`) maps onto
the MMIO libc's open/read/write/lseek/ftruncate/unlink; no locking (one
process), no WAL (it would need exclusive locking mode without shared
memory), no extensions, no threads. Database files are ordinary SQLite
files: the host's `sqlite3` reads what the guest writes and vice versa.
Double-quoted string literals are off (`SQLITE_DQS=0`): quote strings with
single quotes.

Built at -Os: a JAL reaches +/-1MB. The clang backend has no large
code model, so -O2 (995KB library) put libc out of main's reach;
-Os is 697KB and fits. s32-ld can veneer out-of-range JALs (GitHub
issue 74); the clang build still uses -Os.

## The shell

`out/sqlite3.s32x` is SQLite's `shell.c` with a few POSIX stubs
(`shell_slow32.c`, `compat/`): no `system`, `popen`, symbolic links or
users, and `isatty` says no, so it reads standard input as a script.

    printf "create table t(a); insert into t values(1),(2); select sum(a) from t;\n" \
        | slow32-dbt sqlite/out/sqlite3.s32x mydb.db

`.mode box`, `.schema`, `.tables`, `explain query plan`, `pragma
integrity_check` and the rest work as on the host.

## Built by stage08

The self-hosted compiler builds the same three artifacts from the same
pristine sources:

    ./build-stage08.sh    # out/stage08/{libsqlite3.s32a,sqlite3_test.s32x,sqlite3.s32x}

`./check-stage08.sh` is the acceptance test: it builds both ways and
diffs the two programs' output -- unfiltered, byte for byte -- so there
are no golden files to go stale.  Run it after any stage08 front-end
change.

It compiles under `slow32-dbt` (about a minute for the amalgamation),
assembles and links with the host tools, and uses stage08's own libc and
crt0 rather than the runtime's.  The smoke test and the shell print output
byte-identical to the clang build's; a diff of the two shells over a script
is the acceptance test (selfhost ISSUES-67 lists what it took: the
amalgamation is the largest input stage08 has taken).  Two differences to
know about: stage08 still has no `-Os`. Far calls are `-mlong-calls`
(`lui`+`addi`+`jalr`). s32-ld can veneer a short `jal` that sits outside
±1MB (GitHub issue 74); `LONGCALLS=` on `build-stage08.sh` is the A/B.

## What it found

Bringing the shell up exposed a backend bug: a function with no locals that
forwards eight register arguments plus a ninth on the stack stored the ninth
over its own saved return address and returned to address 0
(`sqlite3_create_function`; regression/tests/bug-stack-arg-over-lr).
