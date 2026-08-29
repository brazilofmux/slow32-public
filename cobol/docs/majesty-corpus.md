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
| **usescreen** | SCREEN SECTION, `CBL_GET_SCR_SIZE`, `TO`/`FROM`, edited PIC, `BLANK WHEN ZERO`. |
| **menu** | several screens, `USING`, `AUTO`, `HIGHLIGHT`, `UNDERLINE`. |

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

**C ABI** — `signed-int`, `signed-short`, `POINTER`, `CALL` of
`c_lineartofielded` / `c_fieldedtolinear` and the `c/` library
(`crc`, `rs`, date utilities). `COMP-3` is also common on amounts.

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

Fixed-format remains a separate source mode for CCVS-85 and for
anyone bringing cobc370-shaped tests across (rewritten, not
compiled as 74).
