# Majesty corpus

`~/majesty` is this compiler's production corpus, the way the 30
MVS programs were cobc370's. It is not in this tree. Do not copy
private datasets here.

The map of the programs is `~/majesty/docs/cobol-programs.md` and
`~/majesty/COBOL_PIPELINE.md`. What follows is the compiler-facing
summary: which language and I-O facts each cluster forces.

## The v1 gate

| program | why it is on the critical path |
|---|---|
| **gl039** | line sequential → indexed (`desc-id`). First keyed file. |
| **gl022** | line sequential in, line sequential print out, Report Writer, per-company `ASSIGN` to a data-name. Chart of accounts by number. |
| **gl023** | same, by name. |
| **gl030** | line sequential + indexed random read + Report Writer journal. Several detail groups, edited amounts, `FUNCTION` / `CALL` of `c_lineartofielded`. Load-bearing. |
| **clinkages** | subprogram wrappers over `dateutil.c` (after the rewrite; `FUNCTION-ID`s today); `CALL … BY VALUE … BY REFERENCE … RETURNING` to C. Pulled in by gl030. |
| **usescreen** | SCREEN SECTION, `CBL_GET_SCR_SIZE`, `TO`/`FROM`, edited PIC, `BLANK WHEN ZERO`, `BINARY-CHAR`. |
| **menu** | several screens, `USING`, `AUTO`, `HIGHLIGHT`, `UNDERLINE`, nested `EVALUATE`, `PERFORM WITH TEST AFTER`; `CALL`s `taskdt`. |
| **taskdt** | pulled in by menu: `STRING WITH POINTER`, `INSPECT TALLYING`, `INITIALIZE`, reference modification, `CURRENT-DATE`, `LENGTH`, `REDEFINES`+`OCCURS`, a third screen. The expensive part of the screen gate. |

Matching `reports_cobol/chartofaccounts1-*.prn`,
`chartofaccounts2-*.prn`, `journal-*.prn` is the report proof.

## I-O inventory (whole `src/cobol/`)

**LINE SEQUENTIAL** — almost everything in the batch path, including
all report print files.

**INDEXED** — `gl039`, `gl030`, `gl036`, `ldglacct`, `ldgltrans`,
`crglacct`, `crglacpd`, `crgltrans`, `exglacct`, `exgltrans`.

**RELATIVE** — `ldglentry`, `exglentry`, `crglentry`. After v1
unless pulled in.

**SEQUENTIAL** (record, not line) — `w001` (`OPTIONAL` world file),
`glacpost`. Framing test material; not v1 reports.

**Report Writer** — `gl008`, `gl015`, `gl016`, `gl022`, `gl023`,
`gl030`, `gl036`, `gl042`, `gl043`. None of these use `CONTROL` or
`SUM`; see [report-writer.md](report-writer.md).

**SCREEN SECTION** — `usescreen`, `menu`, `taskdt`.

**C ABI** — `signed-int`, `signed-short`, `unsigned-short`,
`binary-char`, `POINTER`. The C is in `~/majesty/src/c/` —
`dateutil.c` (v1), `crc.c`, `rs.c`, `csvgen.c`, `csvparser.c` — built
into `libmajesty_c.a`. It is reached through COBOL wrappers in
`clinkages.cbl` (`c_lineartofielded`, `c_fieldedtolinear`,
`c_isvaliddate`, …) — user functions today, subprograms after the
rewrite in [functions.md](functions.md). `COMP-3` is common on
amounts.

**Parameters** — `ACCEPT … FROM ARGUMENT-VALUE` / `ARGUMENT-NUMBER` in
nine programs (`gl024`, `gl034`, `gl036`, `gl038`, `gl040`, `gl042`,
`gl043`, …). None of the v1 gates; gl024 is the first that will.

**SORT verb** — `dist01`, `gl008`, `glacpost`, `ldglentry`. Not v1.

## How the programs run today, and after

GnuCOBOL builds **one** shared object: `MAJESTY.so` =
`src/cobol/libmajesty_cobol.a` (every `PROGRAM-ID` and `FUNCTION-ID`)
+ `src/c/libmajesty_c.a`. `batch.sh` runs a step as

    cobcrun -M MAJESTY gl030 "$@"

The single image is a `cobcrun` packaging choice. Retirement replaces
`run_cobol()` with one `.s32x` per program, run under the emulator
**from the majesty directory**, because every `ASSIGN` is relative
(`data/…`, `tmp/…`, `reports_cobol/…`) and the emulator's file service
opens paths against the host's cwd. The precedent is already in the
tree: `run_dbase_s32.sh` does `(cd "$workdir" && slow32-dbt … dbase.s32x)`.
Each `.s32x` links the subprograms its `CALL` literals name and the
SLOW-32 build of `dateutil.c`.

## Pipeline shape

`batch.sh` chains many small programs with host `sort` between them.
The compiler does not replace `sort`. A successful retirement of
GnuCOBOL still has a shell script (or a SLOW-32 equivalent later)
driving `gl024` then `sort` then `gl025` …. v1 does not require the
whole chain: gl039 + gl022 + gl023 + gl030 is the first cut that
prints a real journal.

Year-end closing entries are synthesized at report time, not stored.
That is an accounting fact in the COBOL, not a compiler fact. The
oracle `.prn` files already include it.

## Free-format facts the scanner must accept

- `*> ` comment to end of line
- lowercase reserved words
- no Area A/B
- `end-if`, `end-read`, `end-write`, `end-perform`
- `repository. function all intrinsic.`
- `sharing with all other` on SELECT
- `copy 'world.cpy'` in an FD (`w001`)
- `_` inside user-words (`ltf_lineardate`, `is_valid`)
- `end program name.`; several units per file
- `block contains n records` on line-sequential FDs (ignored)
- *(pre-rewrite only, not to be implemented:* `if … then`,
  `end function`, `repository.`, `name(args)` invocation — see
  [functions.md](functions.md)*)*

Fixed-format remains a separate source mode for CCVS-85 and for
anyone bringing cobc370-shaped tests across (rewritten, not
compiled as 74).

## Stage 12+ — the rest of the corpus, measured (2026-08-29)

Every `src/cobol/*.cbl` through `s32-cobc -free -m`, and the first
thing that stopped each one. 22 of 58 compile today, the v1 gates
among them (`clinkages`, `gl022`, `gl023`, `gl030`, `gl039`, `menu`,
`taskdt`, `usescreen`, `today`) and thirteen more that were never
gates (`gl025`–`gl029`, `gl031`–`gl033`, `gl035`, `gl037`, `gl041`,
`testcsvgen`, `testrs`). Ranked by what unblocks most:

| blocks | first refusal | programs |
|---|---|---|
| ~~8~~ | ~~`COPY`~~ **landed** (Stage 12, 2026-08-29): with `-I src/copy` seven of the eight compile; exglentry moves to the RELATIVE row | ~~crglacct, crglacpd, crgltrans, exglacct, exglentry, ldglacct, ldgltrans, w001~~ |
| 7 | `FUNCTION-ID` -- the legacy pure-COBOL date family, a retirement candidate rather than a rewrite (the C `du_*` path replaced it) | fielded_to_linear, linear_to_fielded, floor-div, floor-divmod, holidays, isleapyear, isvaliddate |
| ~~4~~ | ~~`ACCEPT FROM ARGUMENT-NUMBER` / `ARGUMENT-VALUE`~~ **landed** (Stage 13): gl024 and gl038 compile, gl024's outputs match GnuCOBOL's byte for byte; gl042/gl043 move on to their next blocker | ~~gl024, gl038, gl042, gl043~~ |
| 2 | `REPOSITORY` -- callers of that date family | exgltrans, jerm |
| 3 | `RELATIVE KEY` (relative I-O) | crglentry, ldglentry, exglentry |
| ~~2~~ | ~~`OCCURS DEPENDING ON`~~ **landed** (Stage 14): gl040 compiles, gl034 moves to `SEARCH` | ~~gl034, gl040~~ |
| 2 | `SPECIAL-NAMES` clauses | damm, gl008 |
| ~~1~~ | ~~a subscripted `SOURCE` in a report field~~ gl036 compiles as of Stage 13 (a `VALUE`-only report field was the last stop) | ~~gl036~~ |
| ~~3~~ | ~~`SEARCH`~~ **landed** (Stage 15); gl042/gl043 also needed their `EXIT PARAGRAPH` rewritten to 1985 | ~~gl042, gl043, gl034~~ |
| 1 | a report field without a PICTURE (a group field with children) | gl015, gl016 (retired programs, not in the build) |
| 1 | `SD` -- an in-program `SORT` | glacpost |
| 1 | a numeric item of more than 18 digits (`ws-temp`) | dist01 |
| 1 | `FUNCTION INTEGER-OF-DATE` | jerm2 |
| 1 | `USAGE BINARY-INT` | testcrc |
| 2 | `XML` / `JSON` verbs (GnuCOBOL extensions) | usexml, usejson |

After COPY (2026-08-29): 29 of 58 compile; after the command line and two sweep fixes, 32 of 58; after OCCURS DEPENDING ON, 33 of 58; after SEARCH (and the EXIT PARAGRAPH rewrite), **36 of 58.** The copybooks also
brought `packed-decimal` before `pic`, a trailing `+` in a numeric
picture (`9(9)v99+`, numeric-edited: moved to a numeric item before
arithmetic, as the programs already do) and level 88 directly under
an 01, all of which the compiler already took.

Two things this table settles. First, `COPY` is the one item that
moves eight programs at once and is plain 1985 (Library module), so
it leads Stage 12 together with `ACCEPT FROM ARGUMENT-VALUE`, which
gl024 needs before the journal pipeline can leave GnuCOBOL. Second,
each first refusal hides the next: gl036, once its `SOURCE` takes a
subscript, will meet whatever comes after. The sweep is one command
(`for f in src/cobol/*.cbl; do s32-cobc -free -m -o /dev/null $f; done`)
and should be re-run at the start of every later stage.

`batch.sh` today: `run_s32` runs every COBOL report step on SLOW-32 --
the charts, the journal pipeline, the balances pipeline and the
activity pipeline -- each pipeline in its own working directory under
`s32x/` with its own description index (`s32x/build.sh` compiles the
eighteen binaries; `S32_EMU` names the emulator). Only `today` and the
interactive `menu` still say `cobcrun`.
