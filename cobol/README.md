# cobol — COBOL 85 for SLOW-32

Status: **v1 done and then some** (2026-08-30, Stages 1-63). majesty's
`batch.sh` now runs *every* COBOL report step on SLOW-32 -- charts,
journal, balances and activity pipelines, eighteen programs -- and all
twelve of its reports come out byte-identical to the all-GnuCOBOL run;
usescreen and menu (with taskdt over clinkages and dateutil.c) paint
and accept on the term service. What the compiler covers: the Data
Division as a tree, the whole MOVE matrix including editing and
de-editing, COMPUTE and the arithmetic verbs with ROUNDED / SIZE ERROR
/ REMAINDER, conditions, IF and every PERFORM form, line sequential,
fixed sequential and indexed files, STRING, CALL / LINKAGE / USING on
the SLOW-32 C ABI, the Report Writer entire (Stage 62; CODE and
REPORTS ARE out by choice), SCREEN SECTION, EVALUATE / INSPECT /
INITIALIZE / reference modification / every X3.23a-1989 intrinsic
function, sequential mode V behind the IBM RDW that tapemgr
round-trips, COPY, the command line, OCCURS DEPENDING ON, SEARCH;
91/91 tests, GnuCOBOL agreeing on every program that can run without
a tty, and the NIST CCVS-85 at 348 of 348 compiling and all 348
matching GnuCOBOL's tally, 8068 of 8175 tests passing and none failing
(the other 107 are the suite's own deletions and visual-inspection
tests, the same count as GnuCOBOL's; ISSUES-44). Stages in [docs/plan.md](docs/plan.md);
what the rest of the corpus needs, in
[docs/majesty-corpus.md](docs/majesty-corpus.md) "Stage 12+".

A host cross-compiler in the tree's ordinary universe (like `fortran/`
and `clip/`, not `selfhost/`). It reads COBOL 85 plus the implementor
extensions majesty already writes, and emits SLOW-32 assembler. Success
is retiring GnuCOBOL from `~/majesty`'s report path.

This is not a backend for [`~/cobc370`](../../cobc370/README.md).
cobc370 is COBOL 74 for MVS 3.8j and stays that way. The two compilers
may borrow ideas; they do not share a parser. The 74/85 differences are
subtle and damning.

## Why this one

`docs/plans/1987-desk.md` §8: *"GnuCOBOL is a compiler story; a COBOL
that `WRITE`s `.DBF` is a business story."* The refinement, 2026-08-29:
reuse the **machinery** of DBF/NDX (slots, btree, an honest delete-byte
when we opt in). File-level compatibility with dBase is a nice-to-have,
not an invariant. COBOL can legally describe records dBase cannot store.

The desk-changing job is majesty's general ledger: the same reports,
produced on this machine, without GnuCOBOL.

## Rulings

Settled in the 2026-08-29 design conversation. Defended in the docs
under `docs/`.

1. **Separate compiler.** No copy of `cobc370.c`. No shared front end.
2. **SLOW-32 is the only target.** x86-64 and aarch64 are reached
   through `slow32-dbt`, as with every other language here.
3. **Ordinary universe.** Host cross-compiler. Host `strtod`, host
   oracles, Ragel on the host. Not self-hosted.
4. **Not SSA/BURG.** COBOL is a data-description language with verbs,
   not an Algol. The IR is the symbol table. See
   [docs/architecture.md](docs/architecture.md).
5. **UTF-8 sources. No EBCDIC on this ISA.** Source text is UTF-8;
   fixed-form columns count code points (`-fixed-columns=bytes` for
   byte columns). Alphanumeric data is bytes in the native collating
   sequence -- `HIGH-VALUE` is `0xFF` -- and text in it is UTF-8 by
   convention; national data (`PIC N`) is UTF-16 big-endian. COBOL 85
   on the PC side was ASCII, and without national data there was no
   point in code pages; the ruling was always about the character set
   of the machine, not a refusal of non-ASCII text. Data formats that
   cross from the mainframe are tricky and are settled one at a time:
   packed-decimal sign nibbles (`C`/`D`/`F`) are the same bytes as
   IBM's, zoned DISPLAY signs are not (ASCII overpunch `p`..`y`, not
   EBCDIC zones), and COMP-1/COMP-2 cannot mean what they mean on
   z/OS, whose floating point is IBM hexadecimal, not IEEE (COMP-1 here
   is RM/COBOL's binary integer; COMP-2 is refused). See
   [docs/dialect.md](docs/dialect.md) and [docs/national.md](docs/national.md).
6. **Framing is an FD fact.** Line sequential, RDW-framed V, fixed
   sequential, relative, and indexed are different. Do not conflate a
   newline with a record length. See [docs/framing.md](docs/framing.md).
7. **SCREEN SECTION is in the dialect** even though it is not in the
   1985 text. Majesty already writes it. See [docs/screen.md](docs/screen.md).
8. **No COBOL 2002.** Majesty reaches C through 2002 user-defined
   functions today; the corpus is rewritten to 1985 `CALL`s rather
   than the compiler taught `FUNCTION-ID`. The only 2002 syntax kept
   is `BY VALUE`/`RETURNING` on `CALL`, as the seam to C. See
   [docs/functions.md](docs/functions.md).

## Layout

    README.md         this file
    docs/             requirements, architecture, plans, rulings
    src/s32-cobc.c    the host compiler: reader, tokenizer, parser, Sym[],
                      lowering, emitter -- one translation unit, which
                      #includes its parts from src/cobc/*.h in order
                      (diag, reader, tokenizer ... esql, divisions, driver);
                      the parts share one set of statics and are not
                      headers to include anywhere else
    src/picture.rl    PICTURE scanner, Ragel -G2 (re-hosted from cobc370);
                      picture_scan.c is the generated output, checked in;
                      gen_picture.sh regenerates it
    src/picture.c     PICTURE analysis: category, digits, scale, sign,
                      width, and the software edit descriptor
    libcob/cobrt.h    the field descriptor both sides read (cat, usage,
                      digits, scale, flags, size, picture)
    libcob/kern.h     the hookable kernels (docs/dbt-hooks.md): numeric
                      fetch and store, the software edit descriptor
                      applied and reversed; compiled into libcob and
                      into slow32-dbt
    libcob/libcob.c   guest runtime, built by the SLOW-32 C toolchain
    libcob/casemap.h  Unicode simple case mappings for UPPER-CASE and
                      LOWER-CASE, generated and checked in;
                      gen_casemap.py regenerates it from libutf's
                      UnicodeData.txt
    ISSUES.md         open items, ranked, and closed ones with the lesson
    tests/            run-tests.sh; fixed/ free/ programs with .expected;
                      ccvs-histogram.sh ranks NIST CCVS-85 first refusals
                      (a .link beside one names its subprograms and C);
                      subs/ subprogram units; c/ C called from tests;
                      bad/ programs that must be refused; pictures.txt;
                      data/ fixtures, copied fresh for every program run;
                      2002/ COBOL 2002 (Stage B) programs, run with -std=2002;
                      warn/ -warn-74 behavior points; majesty-functions.sh
                      checks majesty's original 2002 functions vs GnuCOBOL
    build.sh          host build of s32-cobc + libcob
    compile.sh        .cbl -> .s32x (assemble + link with libcob and libc)

## Build, run, test

PATH install (optional, for majesty and friends):

    ln -sfn ~/slow-32/cobol/s32-cobol ~/bin/s32-cobol
    # then: s32-cobol -free prog.cbl -o prog.s32x

    ./build.sh                                   # out/s32-cobc, libcob/libcob.s32o
    ./compile.sh -free prog.cbl -o prog.s32x     # majesty is free-format
    ./compile.sh -free gl030.cbl clinkages.cbl dateutil.c -I ~/majesty/src/h -o gl030.s32x
    ../tools/emulator/slow32 prog.s32x           # or slow32-fast, slow32-dbt
    ./tests/run-tests.sh                         # 5 gates; GnuCOBOL oracle from
                                                 # gnucobol:4.0-builder/-runtime
                                                 # (podman/docker), or a host cobc
    ./tests/majesty-functions.sh                 # 2002 functions on real code

`s32-cobc [-free|-fixed] [-std=85|-std=2002] [-warn-74] [-fnsig] [-o out.s] source.cbl`.
`-std=2002` adds the COBOL 2002 modules landed so far (docs/standards.md,
Stage B); `-fnsig` only writes the user functions' signature files
(docs/functions.md), which `compile.sh -std=2002` does first. Fixed format is the
default (the standard's reference format); majesty passes `-free`,
as it already does to GnuCOBOL.

## Reading order

1. [docs/requirements.md](docs/requirements.md) — product, dialect, done
2. [docs/architecture.md](docs/architecture.md) — shape, toys, IR
3. [docs/functions.md](docs/functions.md) — the finding the first
   draft missed, and the corpus rewrite that answers it
4. [docs/plan.md](docs/plan.md) — stages
5. [docs/standards.md](docs/standards.md) — after v1: standards first,
   dialects second, COBOL 2002's practical half, OO deferred
6. [docs/behavior-points.md](docs/behavior-points.md) — the `bp()`
   registry behind `-std` and `-warn-74`
7. The rest, as the stage needs them

## Container

`slow32:cobol` (`Dockerfile.cobol` at the tree root, FROM `slow32:base`)
carries `s32-cobc`, `libcob.s32o` and `s32cob`, which is `compile.sh`
pointed at the `/opt/slow32` install through the `S32_COBC`, `S32_LIBCOB`,
`S32_AS`, `S32_LD`, `S32_RT` and `S32_RT_INCLUDE` knobs that `compile.sh`,
`cctool.sh` and `tests/run-tests.sh` all honour:

    podman run --rm -v $(pwd):/data slow32:cobol s32cob -free prog.cbl -o prog.s32x
    podman run --rm -v $(pwd):/data slow32:cobol s32run prog.s32x

The image has no C compiler (s32-cobc and libcob are built in a stage FROM
`slow32:toolchain`), so a `.c` input to `s32cob` is refused there; use the
toolchain image for those.  The suite runs inside the image with the tree
mounted -- `ORACLE=0 EMU=/usr/local/bin/slow32` plus the knobs above -- and
reports the two host-compiler gates as SKIPPED, which is what ~/builder
does before pushing it.

## License

MIT, same as the rest of this repository ([`LICENSE`](../LICENSE)).
David M. Gay's `dtoa` in the SLOW-32 runtime is the Lucent notice,
not MIT; see [`NOTICE`](../NOTICE).
