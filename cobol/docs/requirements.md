# Requirements

## Product test

GnuCOBOL is no longer on `~/majesty`'s report path. The programs under
`~/majesty/src/cobol/` that produce `reports_cobol/` compile with this
compiler, run on SLOW-32, and print the same figures.

That is a stronger test than "a COBOL 85 compiler exists." A GnuCOBOL
port that emits C and then clang would be a compiler story and would
not change the job. This compiler is interesting because the reports
run *here*, next to dBase, nano, and the rest of the desk.

Two tiers of "the same figures", and they are different tests:

- **Byte-identical to `reports_cobol/*.prn`.** Same COBOL source,
  same data, our compiler versus GnuCOBOL. Valid as a byte compare
  because both sides write `\n`-terminated line-sequential print
  files with no CR, no form feeds and no trailing blanks (measured on
  the journal: 549 lines, zero of each). This is the v1 gate.
- **Cross-stack parity**, `~/majesty/tests/compare_reports.sh`: C++
  versus COBOL, and dBase beside them. It is *normalised* — headers
  and company names dropped, commas removed, whitespace collapsed,
  only the shared columns kept — because the stacks format
  differently (one balance sheet is 4196, 4332 and 4142 bytes
  across them). That is the business check, and it is what a report
  must still pass on the day we ever choose to differ from
  GnuCOBOL's formatting.

Until that retirement, GnuCOBOL remains the differential oracle for
the majesty programs. The 1985 text remains authority where the two
disagree. Same rule cobc370 used against IBM ANS COBOL and GnuCOBOL;
see [oracles.md](oracles.md).

## Dialect

The language is **ANSI X3.23-1985** (COBOL 85) plus the implementor
modules majesty already uses and SLOW-32 already has a home for:

| module | in the 1985 text? | why it is here |
|---|---|---|
| Nucleus, Table Handling | yes | the language |
| Sequential I-O | yes | |
| Relative I-O | yes | `crglentry` / `ldglentry` / `exglentry` |
| Indexed I-O | yes | `gl039`, then `gl030`'s random reads |
| Inter-Program Communication | yes | `CALL`, including C ABI |
| Library (`COPY`) | yes | `src/copy/` |
| Report Writer | yes (optional module) | every `gl0xx` report |
| Intrinsic Function module (1989) | amendment | `FUNCTION ALL INTRINSIC` |
| `ORGANIZATION IS LINE SEQUENTIAL` | no; implementor | the whole majesty batch path |
| SCREEN SECTION | no; Micro Focus / GnuCOBOL | `usescreen`, `menu`, `taskdt` |
| `USAGE COMP-5`, `signed-int`, `binary-char`, `POINTER` | no; C ABI | `CALL` of `du_*` in `src/c/dateutil.c` |
| `CALL … BY VALUE / RETURNING` | no; C ABI (2002 syntax) | `clinkages.cbl` reaching `du_*`; the only 2002 kept, and only at the C seam |
| `ACCEPT FROM ARGUMENT-VALUE` | no; implementor | nine `YYYYMM` programs; after v1 |

Fixed-format source is required (CCVS-85, and anyone coming from
cobc370). Free-format is required (majesty). A program is one or the
other; mixing in one file is refused.

Character set is ASCII. Overpunch signs are an implementor DISPLAY
convention, documented when first implemented, not EBCDIC `X'C5'`.

`COMP` past nine digits is legal. SLOW-32 has 64-bit arithmetic;
cobc370's refusal was a 370 fact (no 64-bit) and does not travel.

## v1 done

Not CCVS-85 NC complete. This, in order, is the claim v1 is allowed
to make:

1. **gl039** — line sequential in, indexed out, random read back.
2. **gl022, gl023, gl030** — reports byte-identical to current
   `~/majesty/reports_cobol/` (chart of accounts two ways; journal).
   gl030 is the load-bearing one: line sequential, indexed random
   read, Report Writer, **and** `CALL 'c_lineartofielded'` into
   `clinkages.cbl` over `dateutil.c` — after the corpus rewrite in
   [functions.md](functions.md), which lands in `~/majesty` first.
3. **`usescreen.cbl` and `menu.cbl`** on the existing `term.h` service.
   `menu` calls `taskdt()`, which is where `STRING WITH POINTER`,
   `INSPECT TALLYING`, `INITIALIZE`, reference modification,
   `EVALUATE`, `CURRENT-DATE` and a third screen enter v1. That is
   most of the language's cost, and it is inside a 160-line menu.
4. **Sequential V** files that `~/majesty/src/cpp_standalone/tapemgr.cpp`
   can read: IBM RDW, no newline, no delete byte. See [framing.md](framing.md).
5. **cobc370 untouched.**

Relative I-O, the rest of the majesty pipeline, dBase-openable files,
alternate keys, `OCCURS DEPENDING ON` in an FD, and Report Writer
`CONTROL`/`SUM` are after v1, each when a program asks. Majesty's
report writer does **not** use `CONTROL` or `SUM`; totals are
Procedure Division `COMP-3` items named in `SOURCE`. That is the
cheap half of Report Writer, and it is enough for v1.

## Deliberately out

Each with a reason, the cobc370 way:

- **Communication.** Nothing on this machine to bind it to.
- **Debug (`WITH DEBUGGING MODE`).** Null level is conforming. A
  statement-label table, if wanted, is an implementor diagnostic, not
  the module.
- **Sort-Merge as a language module.** The batch path sorts with host
  `sort` between programs (and MVS sorts in JCL). Four programs do
  use the `SORT` verb — `dist01`, `gl008`, `glacpost`, `ldglentry` —
  none on the v1 path. An in-program `SORT` is a later library around
  qsort-shaped code. Null level is conforming.
- **EBCDIC, packed-decimal hardware, base locator cells.** 370 facts.
- **COBOL 2002.** `FUNCTION-ID`, `REPOSITORY`, `IF … THEN`: the
  corpus is rewritten to 1985 forms rather than the compiler taught
  them. The one exception is `BY VALUE`/`RETURNING` on `CALL`, kept
  as the C seam. See [functions.md](functions.md).
- **Sharing cobc370's parser.** See [borrowing.md](borrowing.md).
- **Self-hosting.** Ordinary universe.
- **File-level dBase compatibility as an invariant.** See
  [indexed.md](indexed.md). A restricted writer that refuses what
  dBase cannot store is a later filter on the FD, not the default
  path.
- **SSA, BURG, IRC register allocation as the program IR.** See
  [architecture.md](architecture.md). COMPUTE on `USAGE COMP`
  integers may use a thin native-integer path; that is a hot case,
  not a copied `hir_*.h`.
