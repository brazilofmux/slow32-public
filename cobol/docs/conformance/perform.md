# 14.9.28 PERFORM statement

Swept 2026-09-28 (ISSUES-96). X3.23-1985: VI-107 (PERFORM, formats
1-4). 2023 formats: 1 out-of-line, 2 inline, 3 exception-checking.
CCVS-85 exercises formats 1-2 at length: NC101A-NC108M (TIMES, THRU),
NC201A and NC401M (inline, WITH TEST AFTER), NC231A-NC236A (VARYING,
AFTER) -- all 348 programs match GnuCOBOL's tally.

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | no TEST phrase means TEST BEFORE | **test**: fixed/control, CCVS NC201A |
| 2 | identifiers numeric elementary; the TIMES identifier an integer (85 rule 4 the same) | **refused**: bad/perform-times-nonint -- the TIMES integer was not checked before this sweep; a non-numeric operand was |
| 3 | literals numeric | **refused** by the operand check ("an arithmetic operand must be numeric") |
| 4 | VARYING an index-name: FROM/BY identifiers integers, FROM literal positive, BY literal nonzero (85 rule 7) | **refused**: bad/perform-index-from -- not checked before this sweep |
| 5 | FROM an index-name: the VARYING item and a BY identifier integers, a BY literal an integer (85 rule 8) | **refused**: bad/perform-from-index -- not checked before this sweep |
| 6 | the BY literal is not zero (85 rule 9) | **refused**: bad/perform-by-zero -- not checked before this sweep |
| 7 | the conditions may be any conditional expression | **test**: the conditions tests throughout |
| 8 | UNTIL EXIT not with VARYING, nor with TEST | **refused**: bad/std2002-until-exit-varying (was "'exit' is not declared"), bad/std2002-until-exit-test |
| 9 | at least six AFTER phrases | **test**: free/vary4 has three; eight are allowed |
| 10 | THRU and THROUGH are the same | **test**: fixed/control |
| 11 | a range that names a declarative procedure stays in one declarative section (85 rule 11) | **refused**: bad/perform-thru-decl -- not checked before this sweep |
| 12, 13 | procedure-names in the same source element | **refused**: "'x' is not a paragraph or section" |
| 14 | a file-name at most once in the WHEN phrases unless with an exception-name | **n/a** for now: WHEN with a file-name alone is a named gap (docs/refusals.md) |
| 15 | an exception-name once in the WHEN phrases, unless with different file-names | **refused**: bad/std2002-ecp-dup |
| 16 | a WHEN file-name goes with an EC-I-O name | **refused**: bad/std2002-ecp-file-io |
| 85 only | an in-line PERFORM VARYING takes no AFTER (X3.23-1985 rule 2; 2023 allows it) | **refused** under -std=85: bad/perform-inline-after-85 -- accepted before this sweep, and GnuCOBOL accepts it too; three of this project's own tests used it and were rewritten out-of-line. No Open Systems, CCVS or majesty program uses it |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | the range of a PERFORM | **test**: fixed/control, CCVS NC1xx |
| 2 | overlapping ranges are undefined | **ruling**: the runtime's PERFORM stack abandons an active range re-entered (libcob cob_perform_push); Open Systems relies on the classic behaviour |
| 3 | VARYING an index-name FROM an identifier that is not positive is EC-RANGE-PERFORM-VARYING | **test**: 2002/perfvary -- not raised before this sweep |
| 4-8 | out-of-line and inline alike; the return mechanism; THRU; falling into a range; basic PERFORM | **test**: fixed/control, CCVS NC101A-NC108M |
| 9 | TIMES: the count taken at the start, zero or negative runs nothing | **test**: fixed/control, CCVS |
| 10 | UNTIL, TEST BEFORE and AFTER | **test**: fixed/control, CCVS NC201A |
| 11 | UNTIL EXIT never true | **test**: 2002/exitperform |
| 12, 13 | VARYING and AFTER, the sequence of operation, both TEST forms | **test**: free/vary4, CCVS NC231A-NC236A |
| 14 | the implicit TURN, PUSH/POP ALL around the phrases | **test**: 2002/ecpreview (ISSUES-94 E4-E7) |
| 15 | control to imperative-statement-1 | **test**: 2002/ecperform |
| 16 | FINALLY; EXIT PERFORM there goes past END-PERFORM; no transfer out of it | **test**: 2002/exitperform; **refused**: bad/std2002-finally-goto (GO TO, EXIT PARAGRAPH, EXIT SECTION) -- not checked before this sweep |
| 17 | a WHEN takes the condition by USE rule 3c-3g; USE ignored | **test**: 2002/ecpreview (E8, E11) |
| 18 | WHEN OTHER for a condition no WHEN names | **test**: 2002/ecperform; a fatal one never goes there (2002/ecpfatal) |
| 19 | WHEN COMMON after the phrase | **test**: 2002/ecperform |
| 20 | resume after the raising statement, or the fatal rules | **test**: 2002/ecprecur, ecpfatal2 |
| 21 | raises in the phrases are not taken by them | **test**: 2002/ecperform; RAISE there is refused (bad/std2002-ecp-raise) |
| 22 | after END-PERFORM, implicitly enabled checking is off again | **test**: 2002/ecpreview (E5) |

## Found by this sweep

Syntax rules 2 (the TIMES integer), 4, 5, 6 and 11 and 1985's rule 2
were not enforced; general rule 3 (EC-RANGE-PERFORM-VARYING) was not
raised, and nothing stopped a transfer out of FINALLY (rule 16).
UNTIL EXIT in a VARYING phrase said "'exit' is not declared". CCVS-85,
the Open Systems suite and majesty are unaffected.
