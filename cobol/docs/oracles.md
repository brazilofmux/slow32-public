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
| Majesty reports | current `~/majesty/reports_cobol/*.prn` | byte-identical is v1 done. Produced today by GnuCOBOL. If the 85 text disagrees with a `.prn`, stop and decide; do not silently "fix" the report. |
| Portable 85 programs | GnuCOBOL `-std=cobol85` | default for unit tests |
| Report Writer fit / LINE-COUNTER | 85 text first | GnuCOBOL's RW was a bad oracle for cobc370; for majesty v1 the *output files* are still the gate, because those files are the product. A new RW test that is not a majesty report should be derived from the text. |
| Sequential V / RDW | tapemgr + cobc370 `tests/vrec` files | framing, not language |
| Indexed default path | GnuCOBOL indexed files **only where they agree**, plus a read-back of our own | GnuCOBOL's indexed implementation is not VSAM and not DBF. Status `02` on duplicates: cobc370 followed the standard against GnuCOBOL. Same reflex here. |
| SCREEN SECTION | GnuCOBOL on a real tty, plus dBase Stage 4 behaviour where they overlap | no ISO text |
| CCVS-85 | NIST CCVS-85 via GnuCOBOL's extracted modules | a **histogram of missing features**, as `cobc370/bin/cobc-ccvs` does, not a v1 score. Later, a pass/fail suite for NC/SQ/IC. |

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
