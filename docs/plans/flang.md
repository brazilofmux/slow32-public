# flang on SLOW-32

Status 2026-11-06: **a proof, and it works.** Fortran 90 through flang, the real
`flang-rt`, the SLOW-32 LLVM backend and the machine; no push decided yet.
Fortran 90/95 is meant to come this way, with `fortran/` staying what it is, a
stage08 and DBT stress test.

## What it does today

- FCVS 2.1, through the same `fortran/tests/run-fcvs.sh` as f77: **190 of 192**.
  The other two, FM905 and FM907, are list-directed output: the same values,
  flang's `2.5` to gfortran's `2.50000000`, which the standard leaves to the
  processor.
- `flang/tests/f90t.f90`: modules, type extension, defined operators, recursion,
  optional and assumed-shape arguments, allocatable arrays and deferred-length
  strings, `matmul`, `reshape`, masks, internal I/O, files with `newunit`,
  `complex`, math intrinsics.  Formatted lines are byte-identical to gfortran;
  list-directed ones differ as above.
- `flang/tests/cplx.f90`: `bind(c)` with C both ways, `COMPLEX(4)` and `(8)`.
- gfortran's own execute tests (`flang/tests/gfortran-suite.py`; LLVM's test suite
  carries them, in `~/refs/llvm-test-suite`, never in git), gfortran 16 as the
  oracle: **2,441 of 2,784 agree**, where gfortran has a verdict; a test of several
  files, 142 of them with C, is built with the rest of it (`dg-additional-sources`,
  `dg-compile-aux-modules`), and a test with a target condition runs when DejaGnu would
  run it here (`applies`, which stops on a name it has no answer for; 54 do not apply).
  `INTEGER(16)`, `REAL(10)` and `(16)` do not exist here.  This corpus found, in a day, everything in "what it found".
  `flang/tests/gfortran-native.py` puts the same tests through native flang from
  these sources: 335 of the 343 left fail there too, and those are flang's.
  Failing here alone: `ieee/ieee_1`, `comparisons_3`, `large_3`, which expect flags
  this machine does not have; `common_align_2`, 8-byte alignment; `PR49268`, a
  Cray pointer is 32 bits; `c_sizeof_1`, 4 is right; `bind-c-intent-out`, reads out
  of bounds; `pr66311`, a literal that wants `INTEGER(16)`.  (`secnds` compares clocks in
  single precision: native flang fails it too, most runs.)
    None of the programs of several files is on that list: the 86 in `c-interop/`, which
  check descriptors from C, agree or fail as native flang does, so descriptors have the
  layout C expects on 32 bits.  flang's header is not gfortran's, so their C is compiled
  with two of gfortran's type names defined and `stdint.h` included (`C_FOR_FLANG`);
  that leaves `CFI_type_mask` in 2 tests, and `__int128` in 1, which this machine lacks.
  They called `mkdir` with mode bits `<sys/stat.h>` did not define; it does now.
`regression/run-flang-corpus.sh` holds a run to `flang/tests/gfortran-verdicts.txt`.
- `flang/tests/gfortran-axes.py`, a report: what the gate does not vary.  Of the 2,620
  programs flang builds, every engine agrees on 2,619 (`matmul_15` outlasts the
  reference interpreter's 3 minutes), and the machine verifier passes all of them.
  Its first run: `cshift_2` needs more than the linker's 64 KB stack, and passed on
  the DBT by writing past `stack_end`, where only the interpreter faults (SPEC 6.2);
  `s32flang` now links 8 MB, a host's.  And `bounds_check_fail_5` was running without
  its `-fcheck`, because the runner read `dg-options` and not `dg-additional-options`,
  the oracle's flags too.  With them read, 25 more tests want a flag flang lacks.
  `--variants` builds each at other levels, flang's and llc's apart, the verifier on,
  and links it with stage08's `s32-ld`.  Its first run: `llc` never returned on a
  float compare stored, loaded and branched on, which only `-O0` Fortran leaves in
  (`llvm-backend/test/fp-branch-cc.ll`); `.word` refused a quoted name
  (`asm-quoted-symbol`); `OUT_OF_RANGE` of a `REAL(8)` went to `REAL(16)` whether or
  not there is one (`flang/tests/oor.f90`); `s32-ld` had fixed archive tables.
  Differ still, 16 of 2,620, none ours: `noreturn-4` links only when optimized, as it
  says it must; native flang does the same with
  `aliasing_array_result_1` (`-O1`), `inline_matmul_1`, `matmul_5`, `ptr_func_assign_1`,
  `transfer_array_intrinsic_4` and `typebound_call_32` (`-O0`); `whole_file_14` and
  `bind-c-intent-out` read what they may not, `c_ptr_tests_10` prints an address,
  `unlimited_polymorphic_31` its options, `c_char_tests_5` cheats its prototypes;
  `internal_dummy_2` needs a trampoline until optimized; `out_of_range_1`, `reduce_1`
  and `zero_sized_1` fail at every level, here and natively.
  stage08's `s32-as` reads only stage08's output, by ruling, so it is no axis.
- Reference LAPACK 3.12.1 (`regression/run-lapack.sh`, the standing gate, runs
  `flang/tests/lapack/run.py`; BSD, in `~/refs/lapack`, never in git), built by its own Makefiles, its own test suite, gfortran beside it with
  `-ffp-contract=off`, which is the arithmetic this machine has: **5,199,980 tests in
  94 suites, the same number on both, one over the threshold on both, the same one**
  (`DGGES`, `INFO=9`).  The suites print the same digits but for how wide a list-directed
  integer is.  About 11 times slower than native on the DBT.
  Runs with the DBT's `-I`: its stubs call the host's libm, allowed to be a last place
  off, and on them the order-20 matrix of `ssep` fails its threshold.  Native flang
  fails it the same on Apple's libm and passes on musl, so that is a marginal
  eigenproblem and not the machine; a verdict should not depend on a host's libm.
  `run.py` adds a line to its copies of `cgeqp3rk.f` and `zgeqp3rk.f`, which in 3.12.1
  test `MAXC2NRM` before setting it (the real ones set it): without the line the count
  of tests moves with what the stack holds.
- `flang/tests/run-tests.sh`: `abi` (Fortran's layouts against clang's, with C on
  the other side) and `wide` (what is wider than `index`), each engine.

```sh
ninja -C ~/llvm-project/build-flang flang llc   # configure: see below
flang/build.sh                                  # runtime + modules, ~30 s
flang/s32flang prog.f90 prog.s32x
OUT=/tmp/fc S32_F77=$PWD/flang/fcvs-f77 S32_LD=$PWD/flang/fcvs-ld fortran/tests/run-fcvs.sh
```

`build-flang` is its own directory beside the clang build, 5,229 steps:
`cmake -S llvm -B build-flang -G Ninja -DCMAKE_BUILD_TYPE=Release
-DLLVM_ENABLE_PROJECTS="clang;mlir;flang" -DLLVM_TARGETS_TO_BUILD=SLOW32
-DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=SLOW32
-DLLVM_DEFAULT_TARGET_TRIPLE=slow32-unknown-none -DLLVM_INCLUDE_TESTS=OFF
-DLLVM_INCLUDE_BENCHMARKS=OFF -DLLVM_INCLUDE_EXAMPLES=OFF
-DMLIR_ENABLE_EXECUTION_ENGINE=OFF -DFLANG_INCLUDE_TESTS=OFF
-DCLANG_INCLUDE_TESTS=OFF -DLLVM_ENABLE_ZSTD=OFF -DLLVM_ENABLE_ZLIB=OFF`.
The changes to flang itself are `llvm-backend/patches/07-flang.patch`.

`flang/tests/gfortran-native.py` wants a native flang from the same sources, in
`build-flang-native`: the configure above with `-DLLVM_TARGETS_TO_BUILD=AArch64`,
`-DLLVM_ENABLE_RUNTIMES=flang-rt` and `-DDEFAULT_SYSROOT=$(xcrun --show-sdk-path)`,
no default triple, then `ninja flang runtimes`.

The patch leaves 64-bit targets as they were, checked 2026-11-06 against upstream
built pristine on arm64: of 3,222 corpus files the IR is the same bytes for 3,220.
The others are `nint_2`, `llvm.llround` for `llvm.lround`, the same call there, and
`unlimited_polymorphic_31`, which prints flang's own version and options.  The modules
differ by one private parameter, `__address_kind = 8`, and their checksums.
Since then one change reaches a 64-bit target: where `REAL(16)` is disabled, macOS
included, `OUT_OF_RANGE` of a `REAL(8)` no longer needs it (native `-O0` did not link).

## The real work: flang assumes a 64-bit target

Its docs say so ("flang is not supported on 32 bit CPUs").  A small test passes
by luck: an `i64` length's low word lands in the register a 32-bit `size_t`
reads.  A descriptor does not: flang laid one out `{ptr, i64, i32, i8.., [n x [3
x i64]]}` and the runtime has the rank at offset 12, so walking the dimensions
ran off the top of memory.  What it took:

- `Optimizer/Support/TargetModel.h`: the sizes of the target's `long` and
  `size_t`, and whether its `long double` is `double`, set from the triple by
  `fir::setTargetTriple` and read by the models in `RTBuilder.h` and
  `DescriptorModel.h`.  A 64-bit target keeps the host's sizes.
- `Runtime/api-int64.h`: on LP64 Linux `int64_t`, `long` and `ptrdiff_t` are one
  type, so a model keyed on the C++ type cannot tell "follows the target" from
  "really 64 bits".  The runtime's headers say `RT_API_INT64` for the second:
  `long long` in the compiler, `int64_t` under `FLANG_RT_BUILD`.  The macOS build
  produces the same IR as the stopgap this replaced, for all 195 files of
  `flang/tests/ir-corpus.sh`.  `flang/build-linux.sh` builds it on Linux (Alpine,
  aarch64; `-j2`, g++ on flang runs a 24 GB VM out of memory at `-j4`).  Of 197 files
  from each host, 196 are the same bytes, as are all the modules.  The other, FM828,
  is one constant a last bit apart, by all appearances the host's libm through
  flang's folding of `CEXP`.  **Ruled acceptable:** libm differences between hosts
  may leak through; libms are improving, and this does not pin them.
- `FIROpPatterns.cpp`: dimension fields were read at the type asked for, not the
  field's, a 64-bit load from a 4-byte field.
- `TypeConverter.cpp`: `indexType()` was a fixed 64 against MLIR's own index.
- `Target.cpp`: the SLOW-32 entry, `COMPLEX` as clang has `_Complex`.
- `lock.h`: no threads, real files.

## What it found

FCVS passed through all of this; it is Fortran 77, with no descriptors in types.

- Semantics sized descriptors and procedure pointers for a 64-bit machine, and
  aligned 8-byte types at 8 where clang has 4: `storage_size` said 64 bytes for
  elements 36 apart, and the runtime walks the type tables by those numbers.
  (`MaxDescriptorSizeInBytes` was called harmless padding here.  It was not.)
- `c_ptr` and `c_funptr` held 64-bit addresses, `c_long` and `c_size_t` were 64
  bits, and two type-table structs disagreed with the runtime's.
- An `INTEGER(8)` DO variable beyond 32 bits miscounted, in loops and array
  constructors; `NINT` to `INTEGER(8)` read a 32-bit `lround` as 64 bits.
- `REAL(16)` followed the host's `long double`, so hosts could disagree.
- `do i = 0, 1, huge(0)` ran zero times, the trip count overflowing a 32-bit index.
- A `character(c_char), value` above 127 reached a C `char` with the wrong sign.
- `SetRecl` took a `size_t`, cutting a RECL above 4 GB.
- `ieee_support_flag`, `_halting`, `_standard` and `_rounding` answered true for what
  the machine does not have (docs/SPEC.md); `FREE` did not compile.
- Not flang's: a zero-sized global (an empty struct) never linked, from C either,
  in both toolchains (selfhost ISSUES-112).
- `BESSEL_YN`'s array form went from -Inf to NaN past overflow, on any target; a test
  with a target condition, not run until the conditions were evaluated.
- Not flang's, found by LAPACK: `.comm` was a strong definition, so a common block in
  two files did not link.  The corpus then skipped its programs of several files; LAPACK
  has no other kind.  Now `S32O_BIND_COMMON` (docs/file-formats.md), all four tools.
- Not flang's, wrong-code, found by LAPACK: `llvm.powi` had no libcall, so LLVM called
  `pow` with the exponent converted to the base's type.  `(-1.0)**16777217` was `+1`,
  from C's `__builtin_powif` too; small powers were a last place off everyone else's,
  enough to fail `csep`.  Now `__powisf2`, `__powidf2` (`runtime/powi.c`).

## Still wrong

- A loop over every `INTEGER(4)`, by a step of 1, is 2**32 iterations: the unsigned
  count wraps to zero and it does not run.
- That correctness costs: an `INTEGER(4)` loop has about two more instructions per
  entry (3.7% on loops of 7 iterations, a kernel made to show it), and a variable
  step calls `__udivsi3`, there being no unsigned divide (11% on the same kind).
- `bessel_yn` in `REAL(4)` is a unit in the last place off, enough to fail
  `bessel_5` through the recurrence (runtime ISSUES-29).

## Not done

- `trampoline` is left out of the runtime: internal procedures passed as arguments
  need code made at run time, and W^X forbids it.  `llc` says so; 2 of the corpus.
  `EXECUTE_COMMAND_LINE` answers `cmdstat = -1`, no command processor.
- `REAL(4)` and `(8)` are all there is: 2, 3, 10 and 16 are off.
- `INTEGER(16)` and `UNSIGNED(16)` are off, by ruling: a 128-bit integer is a compile
  error here, as on other 32-bit targets, and `SELECTED_INT_KIND(30)` is -1.  Bit
  operations and array moves did work, LLVM splitting them, and 16 tests that passed
  no longer apply.  Multiplying, dividing, conversions and I/O had nothing behind them:
  no `__multi3` or `__divti3`, no `__int128` in clang's target for the runtime.  With no
  carry flag each word of those is its own run of compares.  A 64-bit machine gets them
  for the price of what 64 bits cost here.
- `flang-rt` by its own CMake, and a driver that is `flang` itself rather than `-fc1`.
- The image: `slow32:toolchain` carries flang (`/opt/llvm/bin/flang`), flang-rt and the
  intrinsic modules (`/opt/slow32/flang`), and `s32flang prog.f90 [more.f90 more.c ...]
  -o prog.s32x` (`docker/bin/s32flang`, `-fc1` underneath).  Its `flang-builder` stage
  reconfigures the image's own LLVM build with mlir and flang, so LLVM is built once;
  g++ wants about 8 GB a compile there (`FLANG_JOB_GB`).  The image's flang-rt is the
  Mac's byte for byte (`build.sh` maps the source paths and pins the collation that
  orders the archive), and flang's own output differs only in the source file's name.
  Checked in the image: the tests above on dbt, slow32-fast and qemu; 81 of the 85
  `fortran/tests/f77` programs build and run through it.  It adds about 220 MB.
- Speed and code size are unmeasured.  `hello` is 1.2 MB; the runtime and libc++
  are most of it.
