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
  oracle: **2,299 of 2,592 agree**, where gfortran has a verdict and the test is one
  file.  Of the 293 left, LLVM's `DisabledFiles.cmake` lists 271 as failing on
  flang natively.  This corpus found, in a day, everything in "what it found".
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
- Not flang's: a zero-sized global (an empty struct) never linked, from C either,
  in both toolchains (selfhost ISSUES-112).

## Still wrong

- A loop over every `INTEGER(4)`, by a step of 1, is 2**32 iterations: the unsigned
  count wraps to zero and it does not run.
- That correctness costs: an `INTEGER(4)` loop has about two more instructions per
  entry (3.7% on loops of 7 iterations, a kernel made to show it), and a variable
  step calls `__udivsi3`, there being no unsigned divide (11% on the same kind).
- `INTEGER(16)` I/O does not link: no 128-bit type in clang's SLOW-32 target.
- `bessel_yn` in `REAL(4)` is a unit in the last place off, enough to fail
  `bessel_5` through the recurrence (runtime ISSUES-29).

## Not done

- `trampoline` is left out of the runtime: internal procedures passed as arguments
  need code made at run time, and W^X forbids it.  `llc` says so; 2 of the corpus.
  `EXECUTE_COMMAND_LINE` answers `cmdstat = -1`, no command processor.
- `REAL(4)` and `(8)` are all there is: 2, 3, 10 and 16 are off.
- `flang-rt` by its own CMake, a driver (`flang` itself, not `-fc1`), an image.
  Only `slow32-dbt` and `slow32-fast` have been run.
- Speed and code size are unmeasured.  `hello` is 1.2 MB; the runtime and libc++
  are most of it.
