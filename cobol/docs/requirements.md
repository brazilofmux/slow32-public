# Requirements

## Product test

GnuCOBOL is no longer on `~/majesty`'s report path. The programs under
`~/majesty/src/cobol/` that produce `reports_cobol/` compile with this
compiler, run on SLOW-32, and print the same figures.

That is a stronger test than "a COBOL 85 compiler exists." A GnuCOBOL
port that emits C and then clang would be a compiler story and would
not change the job. This compiler is interesting because the reports
run *here*, next to dBase, nano, and the rest of the desk.

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
| `USAGE COMP-5`, `signed-int`, `POINTER` | no; C ABI | `CALL` of `c_lineartofielded` and friends |

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
   read, Report Writer.
3. **`usescreen.cbl` and `menu.cbl`** on the existing `term.h` service.
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
- **Sort-Merge as a language module.** Majesty sorts with host `sort`
  (and MVS sorts in JCL). An in-program `SORT` is a later library
  around qsort-shaped code, not v1. Null level is conforming.
- **EBCDIC, packed-decimal hardware, base locator cells.** 370 facts.
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
