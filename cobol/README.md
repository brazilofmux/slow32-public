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
a tty, and the NIST CCVS-85 at 348 of 348 compiling, 8049 of 8160
tests passing. Stages in [docs/plan.md](docs/plan.md);
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
5. **ASCII.** `HIGH-VALUE` is `0xFF`. No EBCDIC on this ISA.
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
                      lowering, emitter -- one file until it earns a split
    src/picture.rl    PICTURE scanner, Ragel -G2 (re-hosted from cobc370);
                      picture_scan.c is the generated output, checked in;
                      gen_picture.sh regenerates it
    src/picture.c     PICTURE analysis: category, digits, scale, sign,
                      width, and the software edit descriptor
    libcob/cobrt.h    the field descriptor both sides read (cat, usage,
                      digits, scale, flags, size, picture)
    libcob/cobedit.h  the software edit descriptor, applied and reversed
    libcob/libcob.c   guest runtime, built by the SLOW-32 C toolchain
    ISSUES.md         open items, ranked, and closed ones with the lesson
    tests/            run-tests.sh; fixed/ free/ programs with .expected;
                      ccvs-histogram.sh ranks NIST CCVS-85 first refusals
                      (a .link beside one names its subprograms and C);
                      subs/ subprogram units; c/ C called from tests;
                      bad/ programs that must be refused; pictures.txt;
                      data/ fixtures, copied fresh for every program run
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
    ./tests/run-tests.sh                         # 3 gates; GnuCOBOL oracle from
                                                 # gnucobol:4.0-builder/-runtime
                                                 # (podman/docker), or a host cobc

`s32-cobc [-free|-fixed] [-o out.s] source.cbl`. Fixed format is the
default (the standard's reference format); majesty passes `-free`,
as it already does to GnuCOBOL.

## Reading order

1. [docs/requirements.md](docs/requirements.md) — product, dialect, done
2. [docs/architecture.md](docs/architecture.md) — shape, toys, IR
3. [docs/functions.md](docs/functions.md) — the finding the first
   draft missed, and the corpus rewrite that answers it
4. [docs/plan.md](docs/plan.md) — stages
5. The rest, as the stage needs them
