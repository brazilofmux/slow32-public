# Borrowing from cobc370

`~/cobc370` is a finished COBOL 74 compiler for MVS 3.8j. This
compiler is COBOL 85 for SLOW-32. cobc370.c stays COBOL 74.
`THE-SEAM.md` already forbids forking that file for a second
backend; a second *language* is even less a fork.

Borrowing is not free. The 74/85 splits cobc370 already hit in
production (reset order of `PERFORM VARYING … AFTER`; receiving-side
ODO; GnuCOBOL-as-85 vs the 1974 text) will silently miscompile if a
parser is shared.

## Cheap (ideas, maybe the PICTURE scan)

- **Oracle discipline.** The standard is authority. An implementation
  is an oracle. When they disagree, follow the text and correct the
  expected output by hand. Six production bugs came out of that in
  cobc370 (`DIFFERENTIAL-TESTING.md`).
- **Refuse with a message.** Unimplemented is a diagnostic, never
  silence. Bad fixtures that cannot become valid as coverage grows
  (`bad-undeclared`, `bad-duplicate`).
- **The seam's shape.** Front end builds `Sym[]`, `Stmt[]`, `File[]`,
  `Report[]`, (here) `Screen[]`. Front end emits no assembler.
- **Ragel on PICTURE, hand scanner on source.** `picture.rl` only
  tokenises; `picture.c` assigns meaning. The surrounding token
  stream has context flags and continuation. Keep that split.
- **`pic_scan` itself.** The PICTURE character-string language barely
  moved 74→85. The scanner can be re-hosted. `pic_analyse` cannot be
  copied as-is: it emits S/370 `ED`/`EDMK` masks in CP037. Keep the
  category/digits/scale/edited synthesis; replace the mask with a
  software edit descriptor.
- **`expr_shape`.** Digits and scale synthesized up the tree, target
  scale inherited down. Rewrite; do not copy. The 85 intermediate
  rules may differ.
- **Parameterized runtimes.** `COBSTR`/`COBUNS`/INSPECT operation
  tables: the runtime works in bytes, the compiler builds the block.
- **Report Writer as generated per-group renderers** plus a small
  state block, against the standard's tables, not against GnuCOBOL's
  approximations.
- **V-record RDW cell in front of the record.** Same four bytes
  tapemgr writes. The QSAM around it does not travel.
- **Report every error, not only the first.** cobc370 did this
  (its #41) with a recipe that transfers as a design: recover at the
  next period, whether the error was inside a sentence or inside a
  data entry; drop the failed sentence or entry; generate nothing once
  anything has failed; cap the listing (thirty there); keep the
  jump-out path for what cannot be recovered. See ISSUES-41 here.
- **Scale equality is the guard on every in-place fast path**, and an
  unsigned zoned compare may use a byte compare with one documented
  behavioural difference. Both are cobc370 audit findings and both
  bear on ISSUES-24 and ISSUES-26, where the fast paths here live.
- **Every truncation path must be fatal, not absorbed**, and a limit
  whose reason was never tested hides other bugs. Three silent
  truncations sat behind one literal-continuation bug there. Bears on
  ISSUES-27 (this compiler under AddressSanitizer with its tables
  forced to grow).
- **Language survey before Nucleus Level 2.** cobc370 implemented
  what the corpus used, then closed the standard. Majesty *is* the
  corpus here. CCVS-85 is later.

## Measured 74/85 differences (cobc370, 2026-08/09)

cobc370 is now a finished compiler with an IBM oracle, and its record
states the places where the two standard years part. These are the
crossover list: in each, *this* compiler must do the other thing.

- **`VALUE` in, or under, `OCCURS`.** Illegal in '74, legal in '85.
  cobc370 refuses both forms (IKF2149I, measured on TK5). Here both
  are accepted and every occurrence is initialised, which GnuCOBOL
  agrees with. Do not copy that refusal.
- **`PERFORM VARYING ... AFTER` reset order.** '85 reversed it.
  Already in Damning below; cobc370 followed the 1974 text against
  GnuCOBOL, which means GnuCOBOL was right for *us*.
- **`OCCURS DEPENDING ON` on a receiving group.** '74 uses the current
  count, '85 the maximum. Already in Damning.
- **Table nesting.** Three levels in '74, seven in '85.
- **Spellings '85 added**: `FILE STATUS` against a bare `STATUS IS`,
  `PADDING CHARACTER`, `RECORD DELIMITER`, `NOT INVALID KEY`,
  `CALL ... BY REFERENCE`, `ADVANCING` by identifier. cobc370 refuses
  them by policy ("a CCVS failure that is a correct refusal of a
  COBOL-85 spelling is a pass"); they are ours to implement.
- **The oracle flips.** cobc370 corrected GnuCOBOL four times by
  following the 1974 text. In the first three items above GnuCOBOL
  follows '85, so for this compiler GnuCOBOL turns from the oracle
  that was wrong into the oracle that was right. Its disagreements
  still have to be adjudicated, but the prior is reversed.
- **`COMP-1` / `COMP-2` is a dialect collision, not a gap.** Neither
  standard year defines them. cobc370 implements IBM hexadecimal
  floating point (#40). Here `COMP-1` is RM/COBOL's binary integer
  with a PICTURE, because the Open Systems suite's items carry one,
  and `COMP-2`/`FLOAT-SHORT`/`FLOAT-LONG` are refused with a message.
  A program written for IBM means something different here, silently.
  Real floating point, when a program asks, is SLOW-32's IEEE
  hardware, not hex.

## Confirmed by running their tests here

`tests/inspect3.cbl` compiles and runs unmodified under `s32-cobc`
(`-fixed`) and under GnuCOBOL `-std=cobol85`, and both reproduce
cobc370's `inspect3.expected` byte for byte, all seven lines. That
matters in both directions: their expectations were hand-derived from
II-68 to II-70 because IKFCBL00 has `EXAMINE` and no `INSPECT`, and
ours were reached separately (Stage 35, ISSUES-17: a per-phrase
runtime regressed twelve NIST tests before the one-pass rule was
right). Two readings of the same paragraphs, one answer. Recorded as
cobc370 issue 42.

The discriminator, if either runtime is ever restructured:
`INSPECT` of `'AABAA'` `TALLYING c1 FOR ALL 'A' c2 FOR ALL 'AA'` gives
`c1=4 c2=0` under one pass and `c1=4 c2=2` under a pass per phrase.

## Damning (do not copy)

- The Procedure Division parser. 85 scope terminators, `EVALUATE`,
  nested programs, free-format.
- `PERFORM` semantics, especially `VARYING … AFTER`.
- ODO on a receiving group (74 current count vs 85 maximum).
- COMP size table (halfword/fullword, refuse >9 digits).
- `wslen > 64K` as a front-end diagnostic (BL-cell limit).
- Any emission of `AP`/`SP`/`ED`/`PACK`/`BALR`/`USING`/`DROP`.
- EBCDIC translation tables, overpunch as `X'C5'`, ASA carriage
  control as the print path.
- VSAM ACB/RPL, QSAM DCB merge, base locator cells, SPIE exits.
- ALTER.
- The file `cobc370.c` itself.

## THE-SEAM, applied

`THE-SEAM.md` said: a second backend begins *in that tree*, as a
sibling, never as a copy; extract the interface when the second
caller reaches for it. We are not that second backend. We are a
different language on a different machine. The seam is a *lesson*
(keep assembler out of the front end; decimal synthesis is the
retargeting work). It is not a work order to split cobc370.c.

If someone later wants cobc370 to emit SLOW-32, that split happens
in `~/cobc370`, and it targets 74, EBCDIC, and a 370-shaped
runtime. It would still not be this compiler.

## What cobc370 can still test for us

`bin/cobc-ccvs` ranks missing features against NIST CCVS-85 by
feeding programs to a 74 compiler. Once this compiler exists, a
sibling script here should do the same job against *this* front
end. The histogram idea is the borrow; the binary is not.

cobc370's sequential-V tests (`tests/vrec`) plus tapemgr are the
RDW round-trip. The programs will not compile here (74, EBCDIC),
but the *files they write* are an oracle for framing.
