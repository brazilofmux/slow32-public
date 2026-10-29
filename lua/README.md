# lua — Lua 5.4.7 on SLOW-32

The upstream Lua 5.4.7 interpreter (`src/lua.h`: `LUA_RELEASE` "Lua
5.4.7"), from Lua.org, compiled for SLOW-32 by clang and the LLVM
backend and linked against the runtime's MMIO libc.  The core and all
ten standard libraries are built unchanged apart from the configuration
listed below.  Lua's own `lua.c` and `luac.c` are not built; in their
place `src/main.c` is a small driver that reads a whole program from
stdin and runs it.  A prebuilt `lua.s32x` is tracked.

## Build

```bash
cd lua
./build.sh              # -O1 by default; OPT=-O2 ./build.sh to change it
```

`build.sh` uses `~/llvm-project/build/bin/clang` and `llc`, the tree's
`slow32asm` and `s32-ld`, and links `crt0.s32o`, every `src/*.c` except
`lua.c`/`luac.c`, `libc_mmio.s32a` and `libs32.s32a` with `--mmio 64K
--stack-size 128K --data-size 5M`.  It writes `lua.s32x` (tracked) and
`.ll`/`.s`/`.s32o` files beside the sources in `src/` (not tracked).

## Run

The program comes on stdin; command-line arguments are ignored and
`arg` is nil.

```bash
echo 'print("Hello from Lua!")' | ../tools/emulator/slow32-fast lua.s32x
../tools/emulator/slow32-fast lua.s32x < script.lua
echo '1+2' | ../tools/emulator/slow32-fast lua.s32x      # prints 3
```

Input that is a single line is first tried as `return <line>` and its
values printed, the way the interactive `lua` does; anything else is
loaded as one chunk named `stdin`.  An error is reported on stderr
(`stdin:1: boom`) and the exit status is 1.  The engines print their own
status lines after the program's output.

## Tests

```bash
bash tests/run-tests.sh
EMULATOR=$PWD/../tools/dbt/slow32-dbt bash tests/run-tests.sh
```

Each `tests/*.lua` is piped to the interpreter and its stdout compared
with `tests/*.expected`: arithmetic, control, coroutines, empty input,
functions, io, metatables, pcall, strings, tables, varargs.  On
2026-10-29: 11 passed, 0 failed, on slow32-fast and slow32-dbt, with the
tracked binary and with a fresh `./build.sh`.  The harness does not
filter the `Cycles:` line that the reference interpreter
(`tools/emulator/slow32`) prints, so with that engine every test reports
FAIL although the program output matches.

## What was changed for SLOW-32

- `src/luaconf.h`: `LUA_USE_C89`; `lua_Integer` is a 32-bit `int`
  (`LUA_INT_INT`) while `lua_Number` stays a 64-bit `double`;
  `lua_getlocaledecpoint()` is always `'.'`; `LUAI_MAXSTACK` is 15000
  (upstream 1000000).
- `src/loslib.c`: `os.execute` reports no shell, and `os.tmpname`
  always fails.
- `src/main.c`: the stdin driver above, new.
- The VM's computed-goto dispatch (`ljumptab.h`) is on, as upstream has
  it for GNU-compatible compilers; it needed the backend's BlockAddress
  lowering, see [../docs/LLVM_BLOCKADDRESS_BUG.md](../docs/LLVM_BLOCKADDRESS_BUG.md).

## What works, and what to watch for

base, package, coroutine, table, io, os, string, math, utf8 and debug
are all opened.  Checked on SLOW-32: string formatting and patterns,
`string.pack`, `utf8`, the `math` functions, `io.open`/`io.lines`/write
on host files through MMIO, `os.remove`, `os.rename`, `os.getenv`,
`os.time`, `os.date`, `os.clock`, `dofile`, `require` of `.lua` files
on `package.path`, `pcall`/`error`, `debug.traceback`.

Things a user would trip over:

- Integers are 32 bits: `math.maxinteger` is 2147483647 and
  `math.maxinteger + 1` wraps to -2147483648.  Floats are full doubles.
- Recursion depth is bounded by the 15000-slot stack; a 100000-deep
  Lua recursion ends in "stack overflow".
- stdin is consumed before the program starts, so `io.read()` returns
  nil; there is no interactive prompt.
- No C modules (`package.loadlib`, `.so` searchers), no `os.execute`
  of commands, no `io.popen`, no `os.tmpname`.
- No `luac`; precompiled chunks are not produced here.

Open items are in [ISSUES.md](ISSUES.md) (lua ISSUES-6, allocator
fragmentation under long GC runs, is the one still open, plus a missing
test for a failing `print` in single-line mode).

## Third-party code

`src/` is Lua 5.4.7, Copyright (C) 1994-2024 Lua.org, PUC-Rio, under
the MIT licence; the full notice is in the repository's
[NOTICE](../NOTICE), which also covers `lua.s32x` built from it.
`src/main.c`, `build.sh` and `tests/` are this project's.
