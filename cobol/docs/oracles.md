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
  where we claim it)
- Implementor modules (LINE SEQUENTIAL, SCREEN SECTION, COMP-5)
  have no ISO text. GnuCOBOL's behaviour plus a note in
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
| (not a test) | a numeric literal as a CALL argument (`CALL X USING 1234`, by reference or by content) | its digits, read through the callee's picture (the 85 text leaves the literal's class to the callee) | a 4-byte big-endian binary: a `PIC 9(4)` callee reads `0042` |
| (not a test) | `CALL 'twice'` when the program is `TWICE` | found: program-names are words, case is not significant (the static link folds them the same way) | not found (a case-sensitive symbol lookup) |
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
