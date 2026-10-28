# regression/

The regression suite and the differential harnesses built around it.  Each
script's header says why it exists, what it compares and how it was
mutation-tested; this file is the map.

## The suite

```bash
./run-tests.sh                  # every test under tests/
./run-tests.sh feature-arithmetic asm-branch-relax   # named tests only
./run-tests-docker.sh           # the same, inside the slow32:toolchain image
```

Each `tests/<name>/` holds one source -- `test.c`, `test.cpp` or `test.s` --
and `expected.txt`.  Optional files: `args.txt` (guest argv), `env.txt`
(`NAME=value` lines), `stdin.sh` (feeds stdin), `expected_exit.txt` (an
expected fault or nonzero exit), `expected.hash` (tube frame hashes).
Results land in `results/<name>/`; a full run clears `results/`, a named run
does not.  Tool locations are overridable from the environment (see the top
of `run-tests.sh`).  Run it before committing any LLVM backend change.

## Engine differentials

Vary the engine, hold the program fixed; every engine is diffed against the
reference interpreter (`tools/emulator/slow32`).  Run all of them after any
emulator, DBT or qemu change.

| Script | Programs |
|--------|----------|
| `run-differential.sh` | the suite above, built by clang, on slow32-fast, slow32-dbt and qemu |
| `run-kit-differential.sh` | stage08's test corpus built with the kit toolchain (`KIT=`, default `~/s32x`) |
| `run-kit-tools-differential.sh` | the kit's cc/as/ld/ar/dis/dump run over their own sources; artifacts byte-compared |
| `run-kit-islisp.sh` | the kit's `islisp.s32x` over `islisp/tests` |
| `fuzz/isafuzz.py run -n N` | random machine code, no compiler involved (`reduce SEED` shrinks a failure) |

The clang-built suite and the kit corpus catch different engine bugs
(DBT-15 passed the first and failed the second), so run both.

## Compiler and library differentials

| Script | Compares |
|--------|----------|
| `run-stage08-fuzz.sh [N]` | random C (`fuzz/cfuzz.py`), stage08 cc vs clang at -O2 and -Os |
| `run-stage07-differential.sh` | stage08's tools built by stage07 vs by stage08 |
| `run-libc-differential.sh` | `libc-tests/` against `runtime/` and the stage08 libc (and the host, for `HOSTLEG` tests) |
| `run-as-differential.sh` | host `slow32asm` vs the self-hosted `s32-as`, object bytes |
| `run-ld-differential.sh` | host `s32-ld` vs the self-hosted `s32-ld`, program behaviour |
| `run-builtins64-differential.sh` | `selfhost/stage08/builtins64.s` vs `runtime/builtins.c` |
| `run-tools-smoke.sh` | each self-hosted tool produces output on a real input |
| `run-libcxx.sh` | `libcxx/*.cpp` on libc++, at -O0 and -O2, on every engine |
| `run-llvm-verify.sh` | the LLVM machine verifier over runtime, libcob, libf77 and SQLite; run after any `llvm/lib/Target/SLOW32` change |
| `run-cobc-asan.sh` | s32-cobc under AddressSanitizer over a COBOL corpus |

`../sqlite/check-stage08.sh` (SQLite built by clang and by stage08 cc must
print the same bytes) is the matching gate for stage08 front-end changes.
