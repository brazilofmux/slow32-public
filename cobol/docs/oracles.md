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
| `free/altkey` | READ under an alternate key WITH DUPLICATES, the next record having the same key | status **02** on that READ, whether it followed a START, a random READ or a READ NEXT (4.5.4: "equal to the value of that same key in the next record") | 02 only when the READ NEXT followed another READ NEXT |
| `free/nestuse` (no oracle) | a containing program's `USE GLOBAL` procedure invoked for a contained program's I/O | runs, control returns after the statement (X3.23 USE general rules; NIST IC233A/IC234A agree) | 4.0-early-dev hangs after the procedure; the harness now times every oracle run out at 60 s |
| `free/picmix` | `MOVE 12300 TO` an item `PICTURE ZZZPP`; `45678` likewise, moved back to `9(5)` | `123`; `456` and `45600` (X3.23 5.3.9: P scaling positions, the stored digits the high ones) | `  1`; `  4` and `00400` -- 4.0-early-dev scales by the P count twice |
| `free/numalnum` (oracle refuses it) | a non-integer numeric item MOVEd to an alphanumeric item | the digits as stored (what NIST NC105A/NC114M/NC124A test for) | 4.0-early-dev with its default configuration refuses the MOVE ("invalid MOVE statement"), though it runs the NIST programs under their own configuration |
| `free/notrunc` | an arithmetic result below zero stored into an unsigned COMP-5 item (`SUBTRACT 5 FROM u5` holding 3) | the **magnitude**, 2 (the 85 rule for an unsigned receiver, the same one both agree on for MOVE) | 4294967294, the value modulo 2^32 -- but only in place: `SUBTRACT s9 FROM u5 GIVING u5` with the same operands gives 5. A native-binary fast path showing through, not a rule |
| `free/sqst` | `OPEN INPUT f REVERSED`, four fixed-length records | the last record first (X3.23 OPEN: REVERSED positions at the end, READ delivers the previous record) | 4.0-early-dev reads forward, REVERSED ignored |
| `free/moveall` | `MOVE ALL "123"` to a `PIC 99V99` item; `MOVE ALL "12"` to a `PIC 9(5)` item | **31.00**, the text's own example (XVII-82, X3J4 interpretation B-23): the literal repeated to the item's character positions, then moved as an unsigned integer; and **12121** likewise | 12.00 and 21212 |
| `2002/userfn`, `2002/userfnx` | a user-defined function's numeric parameter given an expression (`twice(a + 4)`) or a negative literal (`twice(-7)`) | BY CONTENT into a copy described like the parameter, as for COMPUTE (2023 8.4.3.2.4 rule 5b; 14.8.2.3.3 rule 2a): **50** and **-14** | 0 and 1400 |
| `free/callexc` | `CALL` of a program that is not there, ON EXCEPTION DISPLAY ... NOT ON EXCEPTION DISPLAY ... | the ON EXCEPTION phrase, then the end of the CALL (2023 14.9.4.4 general rule 3h1) | both phrases, when ON EXCEPTION falls through; tests/gen/gen-flow.py counts the extra line apart |
| `2002/userfnnest` | the same, the expression holding a user function call itself: `twice(twice(a) + 1)`, as an argument, a subscript, a condition's operand | the inner call made first, its result in the expression: **86** | 0, so the subscripts, conditions and the UNTIL loop that use it go astray |
| `2002/userfnonce` | a function with a side effect, `bump(bump(0) + 0)`: an expression argument beginning with a call | each call made once, the argument the inner result: **7** | 4 (the expression passed as 0, as in userfn); every other call counted the same |
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
| `free/comp12` | COMP-1 and COMP-2 (IEEE floats) | the values GnuCOBOL computes, with three differences, all by Micro Focus's rules (docs/usage.md): `DISPLAY` in MF's form, `.15000000E 01` for 1.5; a statement with a float operand or receiver computes wholly in double, so `COMPUTE dec = 2 / 3 * s1` gives **1.00**; and `MOVE 0.0001` gives the nearest double, `.100000000000000005E-03` | `1.5`; 0.99, `2 / 3` taken in decimal first; `9.999999999999999E-5`, a unit in the last place below the nearest |
| `free/arithexpr` | `COMPUTE r = 2 ** 3 ** 2`; `COMPUTE r = z ** -1` with `z` zero | **64**: consecutive operations of one level run left to right, exponentiation included (X3.23-1985 6.2.3 (2); 2023 8.8.1.2 rule 3; Micro Focus's reference says the same); and a **size error**: zero to a power not above zero (6.2.3 (5)a; rule 6a) | 512, right to left; and 0 |
| `free/compxmf` | `MOVE -1` to a `PIC XX COMP-X` item; `ADD 50` ON SIZE ERROR to a `PIC 9(2) COMP-X` item holding 90 | Micro Focus's rules for its own usage (docs/usage.md): **65535**, the negative value in two's complement "as if the item had been signed"; and the **size error**, the 9s deciding it, the item left at 90 (without the phrase the capacity takes 140) | 1, the magnitude; and 40, no size error, the item truncated to its picture |
| `free/comp5x` | the bytes of `PIC XX COMP-5` holding 258 | **02 01**: COMP-5 is stored in the machine's order, the X's only making it unsigned (Micro Focus's COMP-X and COMP-5 page) | 01 02, big-endian, as if it were COMP-X; every value agrees |
| `free/setcond` | `SET c TO TRUE`, `c` a condition-name on an edited item with an alphanumeric literal (`z,zz9.99` with `" 12.5"`; `xxbxx` with `"abcd"`) | the characters as written, `[ 12.5   ]` and `[abcd ]`, by the VALUE clause's rules (2023 14.9.39.4 rule 6; 13.18.63.3 rules 4 and 7-8), so the condition is true afterwards | edits them as a MOVE would, `[   12.50]` and `[ab cd]`, and the condition is then false |
| `free/copyquote` | `REPLACE =="abc"== BY =="rep"==` over `'abc'`; `=="it""s"==` over `'it''s'` | replaced: the two quotation marks match each other and a doubled quote is one (2002 and 2023 7.2.4.4 rule 8c4, COPY's rule 9c4) | not replaced: a literal matches only in the quote it was written with |
| `2002/fnreturn` | the intrinsics' returned values, 169 calls | as the oracle, but for four: EXP(20), EXP10(10.5) to the 15 significant digits a double holds (native arithmetic: an implementor-defined approximation, 2023 15.34.4, 15.35.4); ANNUITY(0.1, 1) exactly 1.1; NUMVAL-F("1.5E3") 0, its exponent unsigned where 15.69.3 requires the sign | about 34 digits; 1.0999999999; 1500 (though its own TEST-NUMVAL-F finds the same error at position 5) |
| `free/rwsign` | a report group entry with SUM (or SOURCE) and no COLUMN | not presented: the counter sums, nothing prints (X3.23-1985 XIII 3.11.4 rule 1) | presented at column 1 |
| (not a test) | relative slots on disk | the same 4-byte RDW per slot, zero for an empty slot; slot = 4 + maximum record (docs/indexed.md) | an 8-byte native `size_t` length per slot, 0 for empty |
| `free/divremse` (found by tests/gen) | `DIVIDE 7 INTO 1000 GIVING q REMAINDER r ON SIZE ERROR`, `q` a `9(2)` item: a size error on the quotient | both `q` and `r` unchanged (X3.23-1985 VI-81 DIVIDE general rule 8a: no remainder calculation is meaningful); rules 6 and 8b (remainder from the truncated quotient under ROUNDED; a size error in the remainder alone) agree | `q` unchanged, but `r` stored: 6, the remainder of the quotient it did not store |
| `free/divremu` (found by tests/gen/arith85.py) | `DIVIDE -7 BY 2 GIVING q REMAINDER r`, `q` an unsigned `9`, `r` an `S9` | under `-std=85`, the remainder is the dividend less the product of the quotient **as stored** (its magnitude in an unsigned item) and the divisor (X3.23-1985 VI-81, DIVIDE rule 6): -13, a size error, `r` unchanged; under `-std=2002` a signed subsidiary quotient (2002 and 2023 14.9.12, general rules 6c and 7): -1. The user's ruling, 2026-09-30: each edition as its text says; CCVS is silent | the signed quotient under both, -1: the later editions' rule; under `-std=cobol2002` it agrees (tests/2002/divremu) |
| `free/editins` (found by tests/gen) | a simple insertion character (`,` `B` `0` `/`) in an edited picture: `Z/ZZZZZZ.ZZ` holding 3387.1, `***/999999.9999`, `*999.9B99` | one embedded in a zero-suppression or floating string, or immediately right of it, is part of the string and takes the replacement character while suppression lasts; one outside such a string is itself (X3.23-1985 VI-34, VI-35, editing rules 7 and 8): `    3387.10`, `****027843.4190`, `1472.3 19` | keeps the insertion character in the suppressed part (`' /  3387.10'`, `***/027843.4190`), and prints a B anywhere in a check-protected picture as `*` (`1472.3*19`); agrees on a `0` inside a floating string, which this compiler had wrong |
| `free/negcmp` (found by tests/gen) | a relation condition against a negative numeric literal with more integer digits than the subject: `n00 >= -316940`, `n00` an `S9(4)V9(3)` item holding 9884.108 | the literal's algebraic value: true (X3.23-1985 VI-55: the comparison is by algebraic value, and the number of digits a literal represents is not significant); the same value in an item, or as `- 316940`, agrees | false, as if the literal were unsigned; with the value in an item, or a negative literal within the subject's digits, it agrees |
| `free/inspord` (found by tests/gen) | an INSPECT with several REPLACING phrases: `replacing all "b" by "1" all "1b" by "1b" all "1" by "a" after initial "1"` over `,Bb1baB` | the comparison cycle goes position by position; at each, the phrases are tried in the order written and the first match wins, the next cycle starting right of it (X3.23-1985 VI-96, general rule 6); a LEADING run starts where the phrase was first eligible (rule 13c): `,B11baB` | applies each phrase over the whole item before the next: `,B111aB`; the same in the FIRST and LEADING cases of the test. tests/gen checks every generated INSPECT against tests/gen/inspect85.py, these rules written out, rather than against the oracle |
| `free/strovf` (found by tests/gen) | a STRING whose POINTER is past the receiver and whose sources give nothing to move: `string s delimited by ","` into a 6-character item with the pointer at 9, `s` beginning with the comma | no overflow: the test is made "before each move of a character" (X3.23-1985 VI-133, STRING general rule 9; the same words in 2002 and 2023, and NIST's NC217A tests the pointer only with characters to move); `N`, the receiver and pointer unchanged. With a character to move it is the overflow, as everywhere | the overflow on entry, whatever the sources hold. MS COBOL 5.0 does the same, and this compiler did too until 2026-09-30. A POINTER below 1 breaks rule 5 (undefined): there all three give the overflow at once |
| `free/fnargrm` (found by majesty's csv2fw port) | FUNCTION LENGTH of a reference modification with a computed start and a literal length: `length(t(p:1))`, `t` an 8-character item | 1: the length "specifies the size of the data item to be used in the operation" (X3.23-1985 IV-23, reference modification general rule 4b) | 8, the whole item; with a literal start, `t(2:1)`, it agrees. The test's NUMVAL, REVERSE and NUMVAL-C lines, the defect this compiler had, agree |
| `2002/constent` (found by the X-COBOL survey, ISSUES 120) | a constant entry `CONSTANT AS BYTE-LENGTH OF rec-key(1)`, `rec-key` a `PIC X(8) OCCURS 5` element | **8**: the BYTE-LENGTH function's value for that element (2023 13.10.4 rule 5); its subscripts are literals (rule 3) because every occurrence has the one size, so the name without them is an element too | 40, the whole table, subscripted or not. GnuCOBOL 4.0-early-dev also dies with SIGSEGV on a constant-name defined twice with the same value, which rule 9 allows (tests/2002/constdup, no oracle) |
| `2002/concatfig` (no oracle; X-COBOL survey, ISSUES 120) | a concatenation with a figurative operand: `ZERO & "1" & ZERO` | **010**: rule 1 lets either operand be a figurative constant, of the other's class (2002 and 2023 8.8.3.2 rule 1; general rule 1a) | refused: "only literals with the same category can be concatenated". `2002/trimchars` has no oracle either: 4.0-early-dev has TRIM's 2014 form, not 2023's characters to delete |
| `free/progscope` (no oracle; ISSUES 120) | CALLs between contained programs, in and out of scope (2023 8.4.6.3) | a program out of scope is not found: ON EXCEPTION; in scope, it runs and the phrase is not taken | 4.0-early-dev takes ON EXCEPTION after some CALLs that succeeded, calls a containing program from inside it although it is not COMMON (`deep` calling `box`), and dies with SIGSEGV there |
| `2002/rmode` (ISSUES 120) | `COMPUTE r ROUNDED MODE PROHIBITED = 3.000`, `r` a `PIC S9` | stored, 3: the value is exactly representable, and PROHIBITED raises the size error only when it is not (2023 14.7.4.3 rule 7) | the size error, as if the literal's decimal places alone made it inexact; the other seven modes agree |
| `2002/comp5x8` (ISSUES 120) | `PIC X(8) COMP-5` holding 2^64 - 1, and 258 | Micro Focus's reading (docs/usage.md): eight bytes in the machine's order, unsigned, all they hold -- **18446744073709551615**, and 258's bytes 02 01 00 ... from the low end, BINARY-DOUBLE UNSIGNED's layout | 8446744073709551615, the twentieth digit lost (19 digits kept), and the bytes big-endian, as for `free/comp5x`; its -std=mf the same |
| `free/envvar` (ISSUES 120) | ACCEPT ... FROM ENVIRONMENT-VALUE with ON EXCEPTION and NOT ON EXCEPTION, the variable present; and with no name chosen, or no such variable | the variable present: NOT ON EXCEPTION runs. Absent: ON EXCEPTION, and the item -- "undefined" by Micro Focus's ACCEPT rule 8 -- is left as it was (**ruling**) | runs neither phrase when the variable is present; blanks the item on the exception |

## A second witness: Microsoft COBOL 5.0 (2026-09-30)

Microsoft COBOL 5.0 (1993) is Micro Focus's compiler under Microsoft's
name. It is a COBOL 85 implementation of the period, run under the x86
project's DOS translator from the user's own diskettes
(`~/x86/disks/cobol50`). `tests/mfcheck.sh NAME...` converts a
tests/free program to fixed format with `$SET ANS85`, compiles, links
and runs it there, and prints its output beside ours. MS COBOL DISPLAYs
a numeric item without the point and with a trailing sign, so those
lines compare by digits and sign. Like GnuCOBOL it is a witness, not the
authority: the text decides.

On the ten programs of the 2026-09-30 disputes and fixes, it agrees with
this compiler throughout. That includes every case above where GnuCOBOL
differs:

| test | MS COBOL 5.0 |
|---|---|
| `free/divremu` | **as this compiler under `-std=85`**: the remainder from the quotient as stored, a size error on -7 / 2 into `9`, -13 into `S99`, -1.99 (the 85 text, VI-81 rule 6) |
| `free/divremse` | as this compiler: both receivers unchanged on the quotient's size error (rule 8a), rules 6 and 8b, the 22-digit product |
| `free/inspord` | byte for byte as this compiler: the comparison cycle, position by position (VI-96, rule 6) |
| `free/strovf` | **as GnuCOBOL**, against the text: the overflow on entry when the pointer is past the receiver, even with nothing to move. Its other three cases agree |
| `free/editins` | byte for byte as this compiler: insertion characters in the suppressed part, B outside a `*` string, a truncated zero's sign |
| `free/negcmp` | as this compiler: a negative literal's algebraic value |
| `free/abbrnot` | as this compiler: the text's five abbreviated examples equal their expansions |
| `free/odorecv` | as this compiler: OCCURS rule 3a and 3b |
| `free/mulwide`, `free/computewide`, `free/divround18` | as this compiler, digit for digit |

## A 74-era witness: MS-COBOL 4.65 (2026-09-30)

Microsoft MS-COBOL 4.65 (1982) is a subset of X3.23-1974 for CP/M-80, run
under the Z80 project's CP/M machine (`~/z80/z80-monster`, the compiler
in `~/z80/disks/mscobol`). `tests/cpmcheck.sh NAME|PATH...` converts a
free-format program to fixed format (uppercase outside literals, a
paragraph name before the first statement), compiles it, links it with
L80, runs it, and prints the listing's diagnostics and the screen beside
our output.

It is a witness for 74 practice, never for an 85 rule, and a narrow one:

- No 85 syntax: no scope terminators, no inline PERFORM, statements need
  a paragraph. Most tests/free programs are 85 programs and are refused.
- No `REMAINDER` and no `ON SIZE ERROR`: the listing marks both
  "UNRECOGNIZABLE ELEMENT IS IGNORED" and the program runs without them,
  so it says nothing about `free/divremu`.
- `INSPECT ... REPLACING` with more than one phrase stops the compiler
  ("Stack Underflow", a compiler error in phase 2), so it says nothing
  about `free/inspord`. Single phrases (`ALL`, `LEADING`, `FIRST`,
  `BEFORE`/`AFTER INITIAL`, `TALLYING ... CHARACTERS`) agree with this
  compiler.

On the `free/editins` pictures, it differs from the 85 reading in two ways:

| case | MS-COBOL 4.65 | reading |
|---|---|---|
| an insertion character inside a zero-suppression string (`Z/ZZZZZZ.ZZ`, `***/999999.9999`, `*0*9999999.999`, `**B909B9999.9999`) | kept while suppression lasts: `' /  3387.10'`, `***/027843.4190` | the 74 text (II-24, rule 8) and the 85 text (VI-35) say the same words: such characters "are part of the string". This compiler, MS COBOL 5.0 and edit85.py read that as replaced with the string; MS-COBOL 4.65 reads it as GnuCOBOL does. Not a 74-to-85 change: editing is absent from the 85 text's list of changes that may affect existing programs (XVII-54 to 56) |
| `$0$$.99` holding .42 | `' 0 $.42'` | contrary to both texts: the positions before the floating symbol are spaces (74 II-23, 85 VI-34, rule 7) |
| a value truncated to zero into a signed edited item: -32745520.019 into `$9+`, -500 into `99CR` | the sending value's sign: `$0-`, `00CR` | 74 says only "data item positive or zero" (II-22); 85 adds to rule 7 that the value used for editing is the value after truncation (VI-34), which is zero, so `$0+` and `00  `. A 74 program that relied on the old sign meets 85 here |

Simple insertion outside a suppression string (`/999`, `0999`,
`B999,999`) and `*999.9B99` agree with this compiler, as does
`witness/negcmp`. `tests/witness` keeps its recorded output.

## A pre-74 witness: IBM ANS COBOL on MVT (2026-09-30)

IBM's ANS COBOL compiler (IKFCBL00, the 1968 language with IBM's
extensions) runs on MVS 3.8j on the user's TK5 system, operated by
`~/mvsops` (`tk5-up`, `tk5-down`). `tests/mvscheck.sh PROG.cbl...` sends
each program as one job (COBUCL into a temporary library, then a RUN
step, as `~/cobc370/bench/run.sh` does) and prints its output beside
ours. It never boots or stops the system. `tests/witness/` holds
fixed-format programs both old compilers accept, with each one's
recorded output.

It is older than the 74 text: there is no INSPECT, and `/` is not a
PICTURE character. Where it runs:

| program | ANS COBOL (MVT) | reading |
|---|---|---|
| `witness/editinb` (the `free/editins` cases, `B` for `/`) | as this compiler on 12 of 13: insertion characters in the suppressed part, B outside a `*` string, the truncated zero's sign (`$0+`, `00  `) | |
| the same, `$0$$.99` holding .42 | `$0 $.42` | contrary to the text (VI-34, rule 7; 74 II-23), as MS-COBOL 4.65's `' 0 $.42'` is |
| `witness/divremu`: DIVIDE REMAINDER into an unsigned quotient | no size error; the remainder from the signed quotient: -7 / 2 gives Q=3 R=-1, -95 / 7 gives Q=13 R=-04, -7.39 / 3 gives Q=2.4 R=-0.19 | the 74 text (II-61, rule 5) and the 85 text (VI-81, rule 6) take the quotient item as stored; this compiler and MS COBOL 5.0 follow them under `-std=85`. IBM's practice is the reading that 2002 adopted and GnuCOBOL follows. A program from an IBM shop that depends on it gets that reading here under `-std=2002` |
| `witness/negcmp` | as this compiler: a negative literal's algebraic value | |

So on the remainder question the texts of 74 and 85, MS COBOL 5.0 and this
compiler stand on one side, IBM's MVT compiler, GnuCOBOL and the 2002 text
on the other.

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
  text only from the named details (2002/rptuse).
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
