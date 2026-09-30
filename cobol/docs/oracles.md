# Oracles

cobc370's rule, reused: the standard is authority; an implementation
is an oracle; when they disagree, the text wins and the expected
output is corrected by hand. GnuCOBOL is not authoritative
everywhere. cobc370 found it wrong on `PERFORM … AFTER` reset
order (it follows 85, which was the *other* standard), on ODO
receivers, on DISPLAY of signed DISPLAY items, and on a long list
of Report Writer page rules.

Here GnuCOBOL is usually following 85, which is this compiler's
standard. It is still not the text.

## Where the oracle lives now (2026-08-30)

GnuCOBOL is uninstalled from every host. The harness reaches it
through two container images, `gnucobol:4.0-builder` (`cobc`) and
`gnucobol:4.0-runtime` (runs the built program), under podman or
docker, with the slow-32 tree bind-mounted at its own absolute path
so the same command lines work on both sides. The work directory is
under `cobol/out/` for that reason (`/tmp` is not shareable with a
podman machine on macOS). About 0.2 s per container launch; the
whole harness takes ~11 s. If neither an image nor a host `cobc` is
found the harness says so on its last line -- `NO ORACLE` -- rather
than passing quietly on `.expected` alone.

## The GnuCOBOL that made the oracles

`cobc (GnuCOBOL) 4.0-early-dev.0`, invoked by majesty as

    cobc -free -O3 -m -fimplicit-init -I../copy

**No `-std` flag.** Every `.prn` in `reports_cobol/` is default-dialect
output, not `-std=cobol85` output. So the rule below — `-std=cobol85`
on oracle compiles — applies to *portable* unit tests. For the majesty
gate the oracle is the `.prn` file itself, produced under majesty's
own flags; a recompiled oracle for a majesty program must use those
same flags, or the comparison is against something majesty never ran.
`-m` builds a module and `-fimplicit-init` initialises it on load;
both are about `cobcrun -M MAJESTY`, not about the language.

This is the same 4.0 trunk line in which cobc370 found GnuCOBOL
**comparing a signed `COMP-3` item against a literal wrongly** for
some widths (12, 15 and 18 digits; `DIFFERENTIAL-TESTING.md`, "When
the oracle is the one that is wrong"). Majesty amounts are `S9(9)V99`
— eleven digits — and not affected, but a test that widens a packed
item must know the oracle can be wrong there.

## Authority

- **ANSI X3.23-1985** (and the 1989 intrinsic-function amendment,
  where we claim it). The full text is public: it was adopted for
  federal use as **FIPS PUB 21-2**, which carries the standard entire
  (824 pages, searchable), and NIST still hosts it --
  <https://nvlpubs.nist.gov/nistpubs/Legacy/FIPS/fipspub21-2.pdf>.
  Cite it by the standard's own page numbers (VII-54 is Sequential
  I-O, the WRITE statement). The 1974 text is FIPS PUB 21-1 at the
  same place, which is where cobc370 reads its standard. **FIPS PUB
  21-3** is the full text of ANSI X3.23a-1989, the Intrinsic Function
  Module (88 pages): `fipspub21-3.pdf`, same place. FIPS 21-4 (1995) is
  a 12-page adoption notice only; the 1993 corrections amendment
  (X3.23b-1993) is not public, and neither is any later standard
  (2002, 2014, 2023 are sold, the INCITS adoptions on the ANSI
  webstore). Licensed copies of all of them are held outside the tree
  since 2026-09-28: X3.23b-1993 (bundled in ANSI INCITS 23-1985
  (R2001)), 1989:2002 with its two 2006 corrigenda, 1989:2014, and
  1989:2023. Cite them by clause and page, never quote at length.
  Found 2026-09-27, before which rulings
  here rested on the NIST cases and GnuCOBOL with the text cited from
  knowledge.
- Implementor modules (LINE SEQUENTIAL, SCREEN SECTION, COMP-5)
  have no ISO text in 1985. LINE SEQUENTIAL and SCREEN SECTION do in
  ISO/IEC 1989:2023 (held outside the tree; SCREEN SECTION is already
  in 2002, LINE SEQUENTIAL arrives in 2023; COMP-5 is still absent), which makes 2023 a second source for them --
  cited by clause, never quoted at length. GnuCOBOL's behaviour plus a note in
  [dialect.md](dialect.md) is the spec, until we write a tighter
  one. Divergences from GnuCOBOL on those modules are product
  decisions and must be listed.

## Oracles

| class | oracle | notes |
|---|---|---|
| Majesty reports, same source | current `~/majesty/reports_cobol/*.prn` | byte-identical is v1 done. A positional byte diff is valid here because both sides are `\n`-terminated line-sequential print files with no CR/FF/trailing blanks (measured) — unlike cobc370's ASA case, where two compilers encoded identical spacing differently and only `batch-compare` was fair. If the 85 text disagrees with a `.prn`, stop and decide; do not silently "fix" the report. |
| Majesty reports, cross-stack | `~/majesty/tests/compare_reports.sh` | normalised data-content parity against C++ (and dBase). Not byte-identical by design. The check that outlives any formatting decision. |
| Portable 85 programs | GnuCOBOL `-std=cobol85` | default for unit tests |
| Report Writer fit / LINE-COUNTER | 85 text first | GnuCOBOL's RW was a bad oracle for cobc370; for majesty v1 the *output files* are still the gate, because those files are the product. A new RW test that is not a majesty report should be derived from the text. |
| Sequential V / RDW | tapemgr + cobc370 `tests/vrec` files | framing, not language |
| Indexed default path | GnuCOBOL indexed files **only where they agree**, plus a read-back of our own | GnuCOBOL's indexed implementation is not VSAM and not DBF. Status `02` on duplicates: cobc370 followed the standard against GnuCOBOL. Same reflex here. |
| SCREEN SECTION | GnuCOBOL on a real tty, plus dBase Stage 4 behaviour where they overlap | no ISO text |
| CCVS-85 | NIST CCVS-85 via GnuCOBOL's extracted modules | a **histogram of missing features**, as `cobc370/bin/cobc-ccvs` does, not a v1 score. Later, a pass/fail suite for NC/SQ/IC. |

## Documented divergences from GnuCOBOL

Where the 85 text and GnuCOBOL disagree, the `.expected` file carries
the text's answer and a `.oracle-expected` file beside it carries
GnuCOBOL's, so the harness still checks both (it reports "oracle
agrees with its documented divergence").

| test | statement | text | GnuCOBOL 4.0-early-dev |
|---|---|---|---|
| `fixed/indexed` | `REWRITE` of an absent key, ACCESS DYNAMIC | status **23** (record not found; 21 is the *sequential-access* sequence error) | 21 |
| `free/vrec` | `WRITE` with `DEPENDING ON` past `RECORD IS VARYING ... TO n` | status **44**, nothing written | clamps to n, status 00 |
| (not a test) | mode-V bytes on disk | IBM RDW: length includes the 4-byte header, then two zero bytes -- tapemgr's and cobc370's | length excludes the header |
| `free/odomove` | MOVE to a group ending in an OCCURS DEPENDING ON table whose DEPENDING ON item is outside the group | the receiving length is the **maximum** (X3.23-1985 general rules for OCCURS) | the current length |
| `free/altkey` | READ under an alternate key WITH DUPLICATES, the next record having the same key | status **02** on that READ, whether it followed a START, a random READ or a READ NEXT (4.5.4: "equal to the value of that same key in the next record") | 02 only when the READ NEXT followed another READ NEXT |
| `free/nestuse` (no oracle) | a containing program's `USE GLOBAL` procedure invoked for a contained program's I/O | runs, control returns after the statement (X3.23 USE general rules; NIST IC233A/IC234A agree) | 4.0-early-dev hangs after the procedure; the harness now times every oracle run out at 60 s |
| `free/picmix` | `MOVE 12300 TO` an item `PICTURE ZZZPP`; `45678` likewise, moved back to `9(5)` | `123`; `456` and `45600` (X3.23 5.3.9: P scaling positions, the stored digits the high ones) | `  1`; `  4` and `00400` -- 4.0-early-dev scales by the P count twice |
| `free/numalnum` (oracle refuses it) | a non-integer numeric item MOVEd to an alphanumeric item | the digits as stored (what NIST NC105A/NC114M/NC124A test for) | 4.0-early-dev with its default configuration refuses the MOVE ("invalid MOVE statement"), though it runs the NIST programs under their own configuration |
| `free/notrunc` | an arithmetic result below zero stored into an unsigned COMP-5 item (`SUBTRACT 5 FROM u5` holding 3) | the **magnitude**, 2 (the 85 rule for an unsigned receiver, the same one both agree on for MOVE) | 4294967294, the value modulo 2^32 -- but only in place: `SUBTRACT s9 FROM u5 GIVING u5` with the same operands gives 5. A native-binary fast path showing through, not a rule |
| `free/sqst` | `OPEN INPUT f REVERSED`, four fixed-length records | the last record first (X3.23 OPEN: REVERSED positions at the end, READ delivers the previous record) | 4.0-early-dev reads forward, REVERSED ignored |
| `free/moveall` | `MOVE ALL "123"` to a `PIC 99V99` item; `MOVE ALL "12"` to a `PIC 9(5)` item | **31.00**, the text's own example (XVII-82, X3J4 interpretation B-23): the literal repeated to the item's character positions, then moved as an unsigned integer; and **12121** likewise | 12.00 and 21212 |
| `2002/userfn`, `2002/userfnx` | a user-defined function's numeric parameter given an expression (`twice(a + 4)`) or a negative literal (`twice(-7)`) | BY CONTENT into a copy described like the parameter, as for COMPUTE (2023 8.4.3.2.4 rule 5b; 14.8.2.3.3 rule 2a): **50** and **-14** | 0 and 1400 |
| `free/lenrefmod` | `FUNCTION LENGTH (a(s:l))` with a computed start or length | the part's length, 4 for `a(3:4)` (the argument is the reference-modified item) | the whole item's, 10 |
| `2002/intr2002` | `FUNCTION TEST-NUMVAL-C ("EUR 12.50", "EUR")` | **0**: a currency string, a space, digits conform (2002 15.55.2 rule 5; 15.76) | 2, though its own NUMVAL-C converts the same string to 12.50 |
| `fixed/utf8cols` (no oracle) | a fixed-form line whose UTF-8 characters put column 72 past byte 72 | columns count characters (the user's ruling, docs/dialect.md) | counts bytes: the literal runs into the sequence area |
| (not a test) | a numeric literal as a CALL argument (`CALL X USING 1234`, by reference or by content) | its digits, read through the callee's picture (the 85 text leaves the literal's class to the callee) | a 4-byte big-endian binary: a `PIC 9(4)` callee reads `0042` |
| (not a test) | `CALL 'twice'` when the program is `TWICE` | found: program-names are words, case is not significant (the static link folds them the same way) | not found (a case-sensitive symbol lookup) |
| `fixed/rwcode` (no oracle) | Report Writer `RD ... CODE "A1"` (one report, or two sharing a file) | each record begins with the code, the lines' columns after it (X3.23-1985 XIII 3.6.4; 2023 13.18.12.4) | 4.0-early-dev writes an empty print file for any report with a CODE clause |
| `free/moveonce` | `MOVE g TO cnt h2`, `g` a group over an OCCURS DEPENDING ON table whose DEPENDING ON item is `cnt` (2) | `g` identified once, at its current length: `cnt` gets 1 (from "31"), `h2` "31" (X3.23-1985 MOVE general rule 2; 2023 14.9.25.4 rule 1) | the whole group, maximum length, to `cnt`, then the group again at `cnt`'s new value for `h2`: 3 and "31a" |
| (not a test; tests/pictures.txt) | PICTURE order rules over 7,368 generated pictures | Table 10 and 13.18.40.3 rules 12a, 27, 29 (docs/conformance/picture.md) | agrees on 7,313; accepts 22 the rules forbid (`++Z`, `.$$`, `+P`) and refuses 33 the chart allows (`9P+`, `$P9`, `$++`, and `$ZZ`/`$**`, reading the leading `$` as a trailing one) |
| `2002/movecorr` (no oracle) | `MOVE CORRESPONDING` where a same-named pair would be an invalid MOVE (noninteger to alphanumeric, alphabetic to numeric, integer to alphabetic) | the pair does not correspond and is skipped; the valid pairs move (2023 14.7.6 rule 2; X3.23-1985 6.4.3 rule 2 says the same) | 4.0-early-dev refuses the statement ("invalid MOVE statement"), one error per such pair |
| `free/compn` | a `PIC 9(n) COMP-X` item: `ADD 50` to a `9(2)` item holding 90; `DISPLAY` of a `9(5)` item | Micro Focus's rule (docs/usage.md; COMP-X is MF's usage, so its reference decides): what the bytes hold, **140**, and shown at the field's capacity, as COMP-5 is (`00070000`) | truncates to the picture, 40, and shows the picture's digits, `70000` |
| (not a test) | relative slots on disk | the same 4-byte RDW per slot, zero for an empty slot; slot = 4 + maximum record (docs/indexed.md) | an 8-byte native `size_t` length per slot, 0 for empty |

## What we will not do

- Use cobc370 as an oracle for 85 semantics.
- Use IBM ANS COBOL (IKFCBL00) as an oracle for 85 or for ASCII.
- Treat GnuCOBOL default dialect as 85. cobc370 already had to
  pass `-std=mvs` so `V` did not grow a decimal point. Here:
  `-std=cobol85` (or the current GnuCOBOL name for that) on every
  oracle compile, documented in the test harness when it exists.
- Check in majesty's private datasets. Tests that need them run
  against `~/majesty` in place, the way clip's majesty A/B does.

## Differential testing

cobc370's `DIFFERENTIAL-TESTING.md`: unit tests missed a COMP
alignment error that selected zero of 24,525 transactions at
RC=0000. The majesty monthly batch — several dozen COBOL steps,
figures that already match C++ and dBase — is the same kind of
net. v1's last gate is that batch, not a green unit-test file.

Until the compiler can run that batch, smaller programs with
checked-in expected output carry the language. The report
byte-compare against `reports_cobol/` is the first differential
that matters.

## Report Writer, Stage 62 (2026-08-30)

Four divergences from GnuCOBOL 4, each pinned by a fixture's
`.oracle-expected`; the 85 text as cobc370 derived it (IKFCBL00
concurring) wins:

- REPORT FOOTING: GnuCOBOL takes a fresh page; Table 5 places it in
  the current page's footing area when it fits (free/rptctl).
- LINE n NEXT PAGE on a detail: GnuCOBOL ignores it when a body group
  is already on the page; rule 3c starts one (free/rptnext).
- SUM ... UPON: GnuCOBOL feeds the counter from every GENERATE; the
  text only from the named details (free/rptuse).
- A sum counter after TERMINATE: GnuCOBOL leaves the final value;
  2.20.4(8) resets it with its footing's processing (free/rptctl).

## Intrinsic functions, Stage 63 (2026-08-30)

- The ALL subscript (X3.23a-1989: `FUNCTION SUM(TBL(ALL))`) is refused
  by GnuCOBOL 4 outright (`unexpected ALL`); ours expands the table's
  elements at compile time (free/fnall). NIST never exercises it.
- Result precision is implementor-defined; ours is a signed 18-digit
  decimal, scale 9 for the fractional class. GnuCOBOL computes in long
  double. Every NIST window and every value in free/intrinsics agrees
  after the receiver's truncation.

## Intrinsic functions, COBOL 2002 (2026-09-28, ISSUES-52)

Under `-std=2002`: ABS, EXP, EXP10, PI, SIGN, FRACTION-PART,
HIGHEST- and LOWEST-ALGEBRAIC (compile-time, from the argument's
picture or native range), BYTE-LENGTH (compile-time), YEAR-TO-YYYY,
DATE-TO-YYYYMMDD, DAY-TO-YYYYDDD, TEST-DATE-YYYYMMDD,
TEST-DAY-YYYYDDD, NUMVAL-F and TEST-NUMVAL, -C and -F. The three NUMVAL
formats are one scanner in libcob, so TEST-NUMVAL's error positions and
the conversions cannot disagree. 2002/intr2002 agrees with GnuCOBOL
everywhere but TEST-NUMVAL-C with a currency string (the table above).
The rest of 2002's list waits on its module -- NATIONAL, BOOLEAN,
exception handling, locales, ISO/IEC 14651 ordering -- and 2014's and
2023's additions (TRIM, CONCAT, the FORMATTED- family ...) are refused
naming their edition.

Found with it: NUMVAL-C's argument-2, a currency string, was parsed and
ignored (the 1989 text has it too); it is honoured now, through the
same scanner.
