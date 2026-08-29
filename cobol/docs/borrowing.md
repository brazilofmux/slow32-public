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
- **Language survey before Nucleus Level 2.** cobc370 implemented
  what the corpus used, then closed the standard. Majesty *is* the
  corpus here. CCVS-85 is later.

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
