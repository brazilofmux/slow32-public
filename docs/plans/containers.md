# Container images: keyed bases, the kit from the tree

2026-11-08.  Goals, from the user:
1. Break `slow32:toolchain` into parts that spot instances can build.
2. Map the image dependencies so that a set of base images stays stable.
3. Simplify and unify the kit build.

Rulings: the kit is built by ~/builder from the tree, which reverses "the kit
is built on the Mac against ~/llvm-project".  ~/timezone moved into the tree
as `tzlookup/` (47affdf4), so nothing in the kit comes from outside it.

## Where it is (round 2026-11-07)

```
emulator -> base -+-> toolchain   LLVM+clang, MLIR+flang, libc++, flang-rt, runtime, cc-x64/a64  (2h49m amd64, 4h19m arm64)
                  +-> cobol, fortran, basic, xbase   builder stage FROM toolchain (for clang and gcc)
                  +-> pascal, forth
                  +-> kit         COPY of ~/s32x, built on the Mac, pushed from the Mac, twice a round
```

- **Toolchain:** one job of nearly the whole round, and a spot interruption
  loses all of it.  It rebuilds whenever any input moves, including inputs
  flang never reads.
- **Kit:** a hand-made drop.  basic and xbase copy `cc.s32x` out of it.  So the
  round waits on a Mac upload, and the kit is pushed twice: before the round,
  on the old base, and again after it, on the new one.

## Two facts the design rests on

1. **flang does not use our backend.**  `s32flang` runs `flang -fc1 ...
   -emit-llvm` and hands the IR to the clang build's `llc`.  flang needs to
   change only when the LLVM pin, `07-flang.patch` or the target's data layout
   changes.  Backend work, which moves often, never needs a new flang.  The
   same pin keeps the IR it writes readable by the `llc` beside it.
2. **The kit can be built from the tree.**
   - stage08 builds from the checked-in stage07, on the emulator, and
     reproduces byte for byte (`cc.s32x` a5df327b on the Mac).
   - The apps have build scripts: `lua`, `nano`, `prolog`, `islisp`,
     `sqlite`, `xbase`, `sbasic`, `forth`, `dfsort` (s32sort), `examples`
     (benchmark_core, mdfix, phonetic, validatecsv), `tzlookup/slow32`.
   - The BASIC side is `sbc/build-kit.sh`, with the kit's own tools.
   - `regal.s32x` is the user's and is never in the image.

## Where it goes

```
emulator -> base -+-> llvm-<key>    /opt/llvm: clang, llc, the binutils-like tools.  Key = pin + llvm-backend/ (SLOW32, clang, patches 01-05) + its Dockerfile
                  |                 artifact image, ECR only
                  +-> flang-<key>   /opt/llvm/bin/flang alone.  Key = pin + patches 01-04 + 07 + datalayout + its Dockerfile
                  |                 artifact image, ECR only; the long job, and rare
                  +-> selfhost      stage08 cc.s32x, tools, libc; cc-x64/cc-a64.  From the tree, minutes
                  +-> toolchain     base + COPY llvm, flang, selfhost(cross) + build libc++, flang-rt, runtime
                  +-> kit           base + COPY selfhost + apps built with llvm's clang + BASIC side on the kit's tools
                  +-> cobol, fortran, basic, xbase   builder stages FROM base + COPY llvm (not the 3.2 GB toolchain); cc.s32x from kit
                  +-> pascal, forth  unchanged
```

**Keys.**  A key is a hash of every input of the image: files, build args, the
Dockerfile.
- The builder computes it (a script in this tree, `docker/image-key.sh NAME`,
  so the definition lives beside the Dockerfile).
- It looks for `slow32:llvm-<key>` in ECR and builds only if it is missing.
- A round that touches no backend file builds neither llvm nor flang.
- A key must cover everything that changes the output.  A missed input ships
  stale code with green gates, so each key has a check: build at HEAD, change
  one listed input, and the key must move.

**Spot.**
- Every job except llvm and flang fits in under an hour.
- llvm and flang, when they do rebuild, can use `sccache` on S3.  An
  interrupted spot job then resumes from the cache, and a backend-only change
  recompiles just the SLOW32 target.  This is a later step: the keys come
  first, and they are most of the win.

**The kit in a round.**  The kit is a normal job, after selfhost and llvm and
before basic and xbase.  So they take this round's `cc.s32x`, and the
two-push dance ends.  ~/s32x on the Mac becomes something extracted from the
image (`docker cp`), not the input to it.  The kit gate is unchanged
(`run-kit-*.sh` inside the image).

## Order

Each step is checked locally (short builds on the Mac, long ones on Kagura)
before the builder agent wires it into its jobs.

1. **selfhost image**, and the toolchain takes cc-x64/a64 from it.  Small, and
   the kit needs it.
2. **kit from the tree** (`Dockerfile.kit` builds what ~/s32x holds).  The
   check: its `cc.s32x` is a5df327b, its tools equal the Mac kit's, and the kit
   gate passes.  The apps come from the image's clang and are not
   byte-identical to the Mac's; their suites decide.
3. **llvm and flang as keyed images**, the toolchain assembled from them,
   `docker/image-key.sh`.  The builder adds the existence check.
4. **The language images' builder stages** FROM base + llvm instead of
   toolchain.
5. **sccache** for llvm and flang, if the rebuilds that remain still hurt.

## Open

- The flang key's "datalayout" input: the string comes from
  `SLOW32TargetMachine.cpp`, so the key hashes the line that defines it, not
  the whole file.  If that turns out fragile, key flang on all of
  `llvm-backend/` and accept more rebuilds.
- `s32cc`, the printf-made helper in the toolchain's final stage, would read
  better as `docker/bin/s32cc` like the other drivers.
