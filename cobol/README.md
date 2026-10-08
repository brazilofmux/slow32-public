# cobol — standard COBOL for SLOW-32

`s32-cobc` is a host cross-compiler from COBOL to SLOW-32 assembler,
with `libcob`, its guest runtime. It reads COBOL 85 by default and,
behind a `-std` switch, the 2002, 2014 and 2023 editions as far as
they have landed; Micro Focus's and GnuCOBOL's own forms are taken
only under `-dialect=mf` / `-dialect=gnucobol`. Its brief is
**preservation**: the programs of the '80s and '90s, and the standard
they were written to, running on a machine that will run anywhere
QEMU does.

Status, 2026-10-07:

- **COBOL 85 to NIST's satisfaction.** All 348 CCVS-85 programs
  compile and run to their summary; 8,068 of 8,175 tests pass and
  none fails (the other 107 are the suite's own deletions and
  visual-inspection tests, the count GnuCOBOL shows); every program's
  tally matches GnuCOBOL's.
- **COBOL 2002-2023, rule by rule.** `docs/conformance/` gives every
  syntax and general rule of the sections swept a disposition --
  test, refused, n/a with the ruling, gap -- and
  [coverage.md](docs/conformance/coverage.md) (generated) says which
  of the 2023 text's 324 numbered elements are covered (302 as of
  this date). The work queue is
  [docs/plans/standard-queue.md](docs/plans/standard-queue.md): 51
  items, taken in order, 44 of them done (file sharing in its first
  stage); the rest are ruled, staged or deferred there.
- **Real programs, byte for byte.** majesty (the user's ledger) runs
  its month-end on SLOW-32 from the database to the reports; the Open
  Systems accounting suite's papers match their references; ACAS
  3.01.07's sales, purchase, stock and IRS cycles run. Each is a gate.
- **Embedded SQL** (`EXEC SQL`) on SQLite and on PostgreSQL over the
  guest's own TCP with SCRAM, gated by the NIST SQL Test Suite's
  embedded-COBOL programs ([docs/esql.md](docs/esql.md)).
- **An optimizing back end for the hot statements.** A census picks
  the numeric items that can live as machine words; arithmetic, moves,
  compares, IF/EVALUATE, in-line PERFORMs and DISPLAY over them lower
  to stage08's HIR (SSA, graph-colouring register allocation) as
  islands inside the emitted text; `-fno-hir` turns it off and a gate
  compares the two on every run ([docs/plans/hir.md](docs/plans/hir.md),
  [docs/state-2026-10-03.md](docs/state-2026-10-03.md)). The decimal
  kernels are also DBT hooks ([../docs/dbt-hooks.md](../docs/dbt-hooks.md)).
- **The harness**: about 1,200 programs under `tests/` with their
  expected output, each also run under GnuCOBOL 4 and diffed, and
  since 2026-10-07 under gcobol (GCC 15) as a second, informational
  oracle; 700-odd programs that must be refused, each with the one
  message the text calls for; generators that judge random programs
  by the 85 rules written as code; the compiler under the sanitizers.
  What the oracles cannot judge -- screens, tty, locks within a run
  unit -- says so in the source ("no oracle").

The record of how it got here: [docs/plan.md](docs/plan.md) (Stages
1-63, the 85 compiler, by 2026-08-30),
[docs/state-2026-09-30.md](docs/state-2026-09-30.md) (the first month
after: 2002, ESQL, the walls), [docs/state-2026-10-03.md](docs/state-2026-10-03.md)
(HIR), and the queue since.

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

The desk-changing job was majesty's general ledger: the same reports,
produced on this machine, without GnuCOBOL. Done (2026-08-30); since
then the job is the standard itself, and the programs written to it.

## Rulings

Settled in the 2026-08-29 design conversation and amended since;
defended in the docs under `docs/`.

1. **Separate compiler.** No copy of `cobc370.c`. No shared front end.
2. **SLOW-32 is the only target.** x86-64 and aarch64 are reached
   through `slow32-dbt`, as with every other language here.
3. **Ordinary universe.** Host cross-compiler. Host `strtod`, host
   oracles, Ragel on the host. Not self-hosted -- but `libcob` must
   stay buildable by the self-hosted stage08 cc
   (`tests/selfhost-libcob.sh`), so a machine without LLVM has the
   runtime.
4. **Not SSA/BURG for the language.** COBOL is a data-description
   language with verbs, not an Algol. The IR is the symbol table
   ([docs/architecture.md](docs/architecture.md)). The HIR islands
   (2026-10) are the exception the measurements earned: the hot
   numeric statements, where the register code's answer is provably
   the decimal stack's.
5. **UTF-8 sources. No EBCDIC on this ISA.** Source text is UTF-8;
   fixed-form columns count code points (`-fixed-columns=bytes` for
   byte columns). Alphanumeric data is bytes in the native collating
   sequence -- `HIGH-VALUE` is `0xFF` -- and text in it is UTF-8 by
   convention; national data (`PIC N`) is UTF-16 big-endian. Data
   formats that cross from the mainframe are settled one at a time:
   packed-decimal sign nibbles are IBM's bytes, zoned DISPLAY signs
   are ASCII overpunch, and COMP-1/COMP-2 cannot mean what they mean
   on z/OS (COMP-1 is RM/COBOL's binary integer by default,
   `-fcomp1=float` for Micro Focus's; the IEEE usages of 2014 are
   FLOAT-BINARY-32/64 and friends). See [docs/dialect.md](docs/dialect.md),
   [docs/national.md](docs/national.md), [docs/usage.md](docs/usage.md).
6. **Framing is an FD fact.** Line sequential, RDW-framed V, fixed
   sequential, relative, and indexed are different. Do not conflate a
   newline with a record length. See [docs/framing.md](docs/framing.md).
7. **SCREEN SECTION is in the dialect** even though it is not in the
   1985 text; since 2002 it is in the standard, and the text's reading
   is the default where GnuCOBOL's and Micro Focus's count differs
   (behavior-points.md, "dialect behaviours"). See [docs/screen.md](docs/screen.md).
8. **Standards first, dialects second** (2026-09-27, replacing "no
   COBOL 2002"). One compiler, one `-std` switch: `-std=85` the
   default; each later edition accepts the one before plus its own
   additions, and is where that edition's *removals* go behind the
   switch. Object orientation is deferred; VALIDATE, the Communication
   and Debug modules, STANDARD-BINARY arithmetic and asynchronous
   messaging are out by ruling, with the ruling written down (the
   queue, "Out by ruling"; docs/standards.md). A form that is
   GnuCOBOL's or Micro Focus's alone is accepted only under its
   `-dialect=` switch and is never the default; majesty's 2002
   user-defined functions are compiled as written under `-std=2002`
   ([docs/standards.md](docs/standards.md), [docs/functions.md](docs/functions.md)).
9. **The text is the authority; implementations are oracles.** When
   GnuCOBOL, gcobol, Micro Focus's ADIS or the IBM compiler on MVT
   disagrees with the text, the text wins and the expected output is
   corrected by hand, with the section cited
   ([docs/oracles.md](docs/oracles.md)). The NIST tests outrank the 85
   text where the two differ.

## Layout

    README.md         this file
    docs/             requirements, architecture, plans, rulings;
                      docs/conformance/ the rule-by-rule pages and the
                      generated coverage.md; docs/plans/ the work queues
    src/s32-cobc.c    the host compiler: reader, tokenizer, parser, Sym[],
                      lowering, emitter -- one translation unit, which
                      #includes its parts from src/cobc/*.h in order
                      (diag, reader, tokenizer ... esql, divisions, driver);
                      the parts share one set of statics and are not
                      headers to include anywhere else
    src/hir/          the HIR back end the islands lower to: SSA, the
                      optimizer, LICM, BURG selection, the register
                      allocator -- a copy of stage08's, kept converged
                      by hand (hir.h records the divergences)
    src/picture.rl    PICTURE scanner, Ragel -G2 (re-hosted from cobc370);
                      picture_scan.c is the generated output, checked in;
                      gen_picture.sh regenerates it
    src/lex.rl        the token scanner, Ragel -G2: one grammar for 8.3's
                      lexical elements, read one lexeme at a time by the
                      text-word scanner (copy.h) and the tokenizer;
                      lex_scan.c is the generated output, checked in;
                      gen_lex.sh regenerates it; lex.h the lexeme kinds
    src/picture.c     PICTURE analysis: category, digits, scale, sign,
                      width, and the software edit descriptor
    src/cobc/xid_tables.h  Annex B's identifier characters as two
                      compressed DFAs over UTF-8, built by libutf's
                      gen/classify
    libcob/cobrt.h    the field descriptor both sides read (cat, usage,
                      digits, scale, flags, size, picture), the file
                      connector, the screen record
    libcob/kern.h     the hookable kernels: numeric fetch and store, the
                      software edit descriptor applied and reversed;
                      compiled into libcob and into slow32-dbt
    libcob/libcob.c   guest runtime, built by the SLOW-32 C toolchain:
                      I/O (btree.h for indexed files), the decimal stack,
                      the intrinsic functions, screens, the Report Writer's
                      runtime, exceptions, file sharing and record locks
    libcob/esql.c     embedded SQL: the SQLite binding and the PostgreSQL
                      client (pgwire.c, scram.c)
    libcob/entries.s  the runtime's entries written out by hand (PERFORM's
                      push and exit), appended to libcob.c's assembly
    libcob/casemap.h  Unicode simple case mappings for UPPER-CASE and
                      LOWER-CASE, generated from libutf's UnicodeData.txt
    ISSUES.md         open items, ranked, and closed ones with the lesson
                      (cited as "cobol ISSUES-n", never a bare #n)
    tests/            run-tests.sh; fixed/ free/ programs with .expected;
                      2002/ 2014/ 2023/ programs run under that -std and
                      the oracle's nearest; warn/ behavior points and the
                      -std=2023 flag-14 warnings; bad/ programs that must
                      be refused, one message each; gen/ the generators;
                      ecsites/ exception sites; census/ the census's unit
                      tests; subs/ subprogram units; c/ C called from
                      tests; copy/ copybooks; data/ fixtures, copied fresh
                      for every run; witness/ 74-style programs for the
                      historical compilers; the oracle scripts
                      (ccvs-run.sh, nist-sql-run.sh, mfcheck.sh,
                      cpmcheck.sh, mvscheck.sh, adischeck.sh);
                      majesty-functions.sh; selfhost-libcob.sh;
                      sanitize.sh; the differential checks (pic, wide,
                      kern, scredit)
    bench/            the kernels docs/performance.md measures
    build.sh          host build of s32-cobc + libcob
    compile.sh        .cbl -> .s32x (assemble + link with libcob and libc)
    cctool.sh         the C side of a mixed program, through the same knobs

## Build, run, test

PATH install (optional, for majesty and friends):

    ln -sfn ~/slow-32/cobol/s32-cobol ~/bin/s32-cobol
    # then: s32-cobol -free prog.cbl -o prog.s32x

    ./build.sh                                   # out/s32-cobc, libcob/libcob.s32o
    ./compile.sh -free prog.cbl -o prog.s32x     # majesty is free-format
    ./compile.sh -free -std=2002 prog.cbl -o prog.s32x
    ./compile.sh -free gl030.cbl clinkages.cbl dateutil.c -I ~/majesty/src/h -o gl030.s32x
    ../tools/emulator/slow32 prog.s32x           # or slow32-fast, slow32-dbt
    ./tests/run-tests.sh                         # the gates; GnuCOBOL from
                                                 # gnucobol:4.0-builder/-runtime
                                                 # (podman/docker) or a host cobc;
                                                 # gcobol:15 if present (GCOBOL=0 off,
                                                 # GCOBOL=strict to fail on it)
    ./tests/majesty-functions.sh                 # the one -std=2002 majesty build
    ./tests/selfhost-libcob.sh                   # libcob built by the self-hosted cc,
                                                 # the suite's programs run on it
    ./tests/ccvs-run.sh                          # NIST CCVS-85 (gate 5 holds its totals)

`s32-cobc [-free|-fixed] [-std=85|2002|2014|2023] [-dialect=mf|gnucobol]
[-warn-74] [-warn-extensions] [-I dir] [-D name[=value]] [-m] [-fnsig]
[-fixed-columns=bytes] [-fbinary-byteorder=native] [-fcomp1=binary|float]
[-fno-hir] [-o out.s] source.cbl`; `s32-cobc` alone prints the list.
Fixed format is the default (the standard's reference format); majesty
passes `-free`, as it already does to GnuCOBOL. The harness takes about
nine minutes with both oracles; the other gates (majesty,
majesty-functions, Open Systems, CCVS, selfhost-libcob, ACAS) run after
it before anything is committed, and none of the compiler's sources is
edited while they run (the sanitizer gate compiles them mid-run).

## Reading order

1. [docs/requirements.md](docs/requirements.md) — product, dialect, done
2. [docs/architecture.md](docs/architecture.md) — shape, toys, IR
3. [docs/standards.md](docs/standards.md) — standards first, dialects
   second; the `-std` rows; what is out by ruling
4. [docs/plans/standard-queue.md](docs/plans/standard-queue.md) — the
   road to 2023, item by item, with the rulings
5. [docs/conformance/README.md](docs/conformance/README.md) — the pages,
   the legend, and coverage.md
6. [docs/behavior-points.md](docs/behavior-points.md) — the `bp()`
   registry behind `-std`, `-dialect` and `-warn-74`
7. [docs/oracles.md](docs/oracles.md) — GnuCOBOL, gcobol, the
   historical compilers, and where each is wrong
8. [docs/state-2026-09-30.md](docs/state-2026-09-30.md),
   [docs/state-2026-10-03.md](docs/state-2026-10-03.md) — the state
   reports; [docs/plan.md](docs/plan.md) — the 85 stages
9. The rest, as the item needs them: [docs/esql.md](docs/esql.md),
   [docs/plans/hir.md](docs/plans/hir.md), [docs/performance.md](docs/performance.md),
   [docs/screen.md](docs/screen.md), [docs/report-writer.md](docs/report-writer.md),
   [docs/refusals.md](docs/refusals.md)

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
reports the host-compiler gates as SKIPPED, which is what ~/builder
does before pushing it.

## License

MIT, same as the rest of this repository ([`LICENSE`](../LICENSE)).
David M. Gay's `dtoa` in the SLOW-32 runtime is the Lucent notice,
not MIT; see [`NOTICE`](../NOTICE).
