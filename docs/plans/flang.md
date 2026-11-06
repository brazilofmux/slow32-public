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
  `flang/tests/ir-corpus.sh`.  `flang/build-linux.sh` builds it on Linux; whether
  that build produces the same IR is **not yet known**.
- `FIROpPatterns.cpp`: dimension fields were read at the type asked for, not the
  field's, a 64-bit load from a 4-byte field.
- `TypeConverter.cpp`: `indexType()` was a fixed 64 against MLIR's own index.
- `Target.cpp`: the SLOW-32 entry, `COMPLEX` as clang has `_Complex`.
- `lock.h`: no threads, real files.

Known left in this family: `c_ptr` holds its address in `integer(int64)`
(`__fortran_builtins.f90`), and `MaxDescriptorSizeInBytes` over-approximates
(upstream's FIXME; harmless, only padding).

## Not done

- `execute` (no processes here) and `trampoline` (internal procedures passed as
  arguments need code made at run time; W^X forbids it) are left out of the
  runtime.  Which programs that refuses has not been measured.
- `REAL(10)`, `REAL(16)`: `long double` is `double` here, and flang's kinds have
  not been checked against it.
- A Fortran 90+ corpus: FCVS is F77, and `f90t` is one program.  gfortran is the
  oracle.
- `flang-rt` by its own CMake, a driver (`flang` itself, not `-fc1`), an image.
  Only `slow32-dbt` and `slow32-fast` have been run.
- Speed and code size are unmeasured.  `hello` is 1.2 MB; the runtime and libc++
  are most of it.
