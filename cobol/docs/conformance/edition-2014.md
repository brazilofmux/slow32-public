# The 2014 edition's substantive changes: 2014 Annex E.2 items 1-29, E.3 items 7, 18, 19

Audited 2026-10-07 (docs/plans/standard-queue.md item 29). The 2014
edition's Annex E.2 lists the changes from 2002 that can affect an
existing program; one row per item here says what this compiler does
under `-std=2002` and under `-std=2014`, and which rows needed a change.
Three of E.3 (changes that affect no existing program) are added at the
end because the queue named them unverified. Test 2014/e2audit (no
oracle: GnuCOBOL 4 keeps status 00 for item 7's CLOSE); bad tests as
named.

How the edition is told apart: `-std=2014` (docs/standards.md) sets
`g_std` to 2014, and the items that change behaviour test it; under
`-std=2002` the 2002 behaviour stands. Nothing here is a behaviour point
(docs/behavior-points.md): a point marks an extension or a deviation
taken on purpose, and every one of these is the edition's own rule.

| item | the change | -std=2002 | -std=2014 |
|---|---|---|---|
| 1 | ACTIVE-CLASS restrictions (object orientation) | **n/a**: objects are out of scope (docs/standards.md) | the same |
| 2 | alphabetic items only where a rule allows them | each statement's own operand rule (the sweeps): UNSTRING refuses an alphabetic sender or delimiter (14.9.48.3 rule 2); STRING and INSPECT take one, their rules asking for usage DISPLAY or NATIONAL, which an alphabetic item is | the same |
| 3 | ARITHMETIC IS STANDARD obsolete, processor-dependent | **refused** ("removed in 2023: write NATIVE"); NATIVE and STANDARD-DECIMAL are implemented, STANDARD-BINARY refused by ruling (options.md 11.9.5) | the same |
| 4, 5 | one coded character value for SPACE, ZERO, QUOTE, the editing characters, the digits, the currency symbol, in the alphanumeric and national sets | holds: ASCII in UTF-8 and UTF-16BE, one value each | the same |
| 6 | two Unicode letters (U+2118, U+212E) no longer in user-defined words | holds: extended letters are Annex B's of 2023 (lexical.md, 2026-10-07), which has neither | the same |
| 7 | CLOSE WITH NO REWIND, or CLOSE UNIT, of a file not on unit media sets I-O status 07 | NO REWIND closes with 00; REEL/UNIT gave 07 already | **changed**: NO REWIND closes and sets 07 (`cob_close_norewind`); REEL/UNIT 07 as before. Test 2014/e2audit |
| 8 | the composite of operands ignores floating-point literals | the composite is not computed from literals' sizes here (arithmetic.md: the wide stack's 38 digits are the limit); a floating-point literal is held as a fixed-point value of at most 31 digits (identifiers.md) | the same |
| 9 | two CURRENCY SIGN clauses with equivalent symbols need identical strings | **n/a**: one CURRENCY SIGN clause per unit is implemented (environment.md) | the same |
| 10 | a hexadecimal literal is no currency symbol, and a currency string only with PICTURE SYMBOL | the symbol X"24" was taken as "$" | **changed**: a hexadecimal literal as the symbol is **refused**: bad/std2014-hex-currency (`Tok.hex`); as the string with PICTURE SYMBOL it is taken, as before |
| 11 | an arithmetic result nearer zero than a floating-point numeric-edited receiver can hold is EC-SIZE-TRUNCATION | COMPUTE into `+9.9E+99` raises it for 1E-120 (ON SIZE ERROR, or the checked condition); a MOVE of such a value leaves the item blank and raises nothing (MOVE has no size error) | the same. Found on the way: a FLOAT-LONG value moved or computed into such an item was cut to the picture's fixed scale first, so 1.5E-20 arrived as zero; the float now goes in with its exact decimal digits and exponent (`cob_wput_x`). Test 2014/e2audit |
| 12 | EXTERNAL only at level 01 in WORKING-STORAGE | **refused** otherwise already (data-division.md) | the same |
| 13 | FUNCTION ALL INTRINSIC reserves the 2014 functions' names too | the 2002 names were not reserved either: `01 upper-case` compiled under ALL INTRINSIC | **changed** for both editions: an intrinsic function's name is **refused** as a user-defined word in the REPOSITORY's scope (2023 12.3.8.3 rule 12): bad/std2002-all-intrinsic-name; the 2014 names (TRIM, the date and time functions) only under -std=2014: bad/std2014-all-intrinsic-name |
| 14 | the case-mapping table | **n/a**: ASCII case mapping (UPPER-CASE, LOWER-CASE: functions.md) | the same |
| 15 | LINAGE-COUNTER qualified when two files have LINAGE | **refused** unqualified: "LINAGE-COUNTER is ambiguous" | the same |
| 16 | MOVE of an alphanumeric-edited item to itself undefined | whatever the move does; nothing to decide | the same |
| 17 | MOVE of a variable-length group to itself: as through a temporary | the same storage, the same length: the result is the item unchanged either way | the same |
| 18 | EC-FUNCTION-NOT-FOUND, EC-FUNCTION-ARG-OMITTED, EC-OO-ARG-OMITTED | the names exist (exceptions.md); EC-FUNCTION-NOT-FOUND raised by ADDRESS OF FUNCTION since item 26 | the same |
| 19 | the Communication facility, debugging lines and WITH DEBUGGING MODE, PADDING CHARACTER removed | Communication: out of scope; debugging lines and DEBUGGING MODE taken (BP-O12); PADDING CHARACTER read and ignored | **changed**: debugging lines, WITH DEBUGGING MODE and PADDING CHARACTER are **refused** under -std=2014: bad/std2014-debug-line, -debugging-mode, -padding |
| 20 | overlapping operands: a statement's own rules decide first | the statements evaluate their senders before storing (move.md GR 1, free/moveonce) | the same |
| 21 | a comma or period ending a PICTURE is a separator when a space follows | `PIC 99, VALUE ZERO` takes the comma as a separator: the scanner | the same. Test 2014/e2audit |
| 22 | STANDARD-COMPARE, EC-ORDER-NOT-SUPPORTED, ORDER TABLE not required without ISO/IEC 14651 | implemented 2026-10-09 (docs/plans/locale.md step 1): the DUCET of Unicode 16.0 under the UCA answers to 'ISO_14651_2020_TABLE1', the one ORDER TABLE taken; levels 1-4; a level the table has not is EC-ORDER-NOT-SUPPORTED, fatal | the same |
| 23 | required features made optional (screens, Report Writer, locales, ...) | the implemented ones stay (A.4 list); nothing changes | the same |
| 24 | the reserved words added: FARTHEST-FROM-ZERO, FLOAT-BINARY-32/64/128, FLOAT-DECIMAL-16/34, FLOAT-INFINITY, FLOAT-NOT-A-NUMBER(-QUIET/-SIGNALING), FUNCTION-POINTER, IN-ARITHMETIC-RANGE, NEAREST-TO-ZERO, `<>` | user-defined words (2002's own additions are not reserved either: ISSUES-43's survey) | **changed**: **refused** as names: bad/std2014-reserved-word (`user_word`, diag.h) |
| 25 | subscripting and qualification restricted where they made no sense | each rule its own (the sweeps) | the same |
| 26 | SET index limits are the OCCURS clause's; EC-RANGE-INDEX narrowed | **ruling** stands: no range is enforced on an index value, an element outside the table is EC-BOUND-SUBSCRIPT when used (exceptions.md) | the same |
| 27 | STANDARD-COMPARE's default table | **n/a** (item 22) | the same |
| 28 | TERMINATE with a non-integer VARYING: EC-REPORT-VARYING | **n/a**: Report Writer VARYING is not implemented (refusals.md) | the same |
| 29 | WRITE's LOCK and RETRY phrases before AT EOP | **n/a**: record locking and RETRY are not implemented | the same |
| E.3 7 | currency symbols equivalent as the repertoire has it | **n/a**: one CURRENCY SIGN clause | the same |
| E.3 18 | currency symbols from outside the COBOL character repertoire in a PICTURE | a multi-byte (UTF-8) symbol is **refused** ("one character"); the PICTURE is scanned by bytes | the same: **gap**, kept with the picture scanner's byte characters (docs/dialect.md); WITH PICTURE SYMBOL gives such a currency its string |
| E.3 19 | a PICTURE of up to 63 characters (was 50) | 50 | **changed**: 63 (`pic_len_check`). Test 2014/e2audit |

Found on the way, outside the list: a floating-point literal below
1E-31 (1.0E-200) overran the tokenizer's buffer -- the zeros after the
point were not counted against the 31-digit limit. Refused now:
bad/std2002-float-literal-tiny.
