# 14.9.14 EXIT statement

Swept 2026-09-28 (ISSUES-96). X3.23-1985: VI-86 (EXIT), X-33 (EXIT
PROGRAM). Formats: 1 simple, 2 PROGRAM, 3 PERFORM [CYCLE], 4 PARAGRAPH /
SECTION.

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | a simple EXIT is a sentence by itself, the only one in its paragraph (85 rules 1-2 the same) | **refused**: bad/exit-not-alone -- was accepted before this sweep |
| 2 | no EXIT PROGRAM in a declarative whose USE is GLOBAL (85 rule 2 the same) | **refused**: bad/exit-program-global -- was accepted before this sweep |
| 3 | RAISING EXCEPTION names a level-3 exception-name; an EC-USER one must be in the RAISING phrase of the procedure division header | **gap**: EXIT PROGRAM RAISING is refused by name (bad/std2002-exit-program-raising); the propagation it needs is not implemented |
| 4 | RAISING identifier-1 is a sending operand | **n/a**: an object reference -- object orientation, deferred |
| 5 | identifier-1's object-reference constraints | **n/a**: object orientation |
| 6 | RAISING LAST only in a declarative or a WHEN phrase | **gap**: as rule 3 |
| 7 | EXIT PROGRAM only in a program's procedure division | **refused**: bad/std2002-exit-program-function -- was accepted before this sweep |
| 8 | EXIT PERFORM only inside an inline or exception-checking PERFORM; no CYCLE in the latter | **refused**: bad/std2002-exit-perform-outside, bad/std2002-exit-cycle-ecp |
| 9 | EXIT SECTION only in a section | **refused**: bad/std2002-exit-section-nosec |
| 10 | EXIT PARAGRAPH only in a paragraph | **refused**: bad/std2002-exit-paragraph-nopara |
| 85 only | EXIT PROGRAM is the last of a sequence of imperative statements (X3.23-1985 EXIT PROGRAM rule 1; not in 2023) | **refused** under -std=85: bad/exit-program-not-last |
| -- | EXIT PERFORM, PARAGRAPH, SECTION are COBOL 2002 | **refused** under -std=85: bad/exit-perform-85 |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | a simple EXIT does nothing; it gives a point a procedure-name | **test**: fixed/exitprog (PERFORM ... THRU an EXIT paragraph) |
| 2 | EXIT PROGRAM in a program no caller controls continues as CONTINUE (85 rule 1 the same) | **test**: fixed/exitprog, the oracle agreeing -- **fixed by this sweep**: it used to end the program. libcob counts activations (cob_called) |
| 3 | in a called program, as GOBACK's rules 3-4 | **test**: fixed/exitprog (returns to the caller) |
| 4 | EXIT PERFORM in an exception-checking PERFORM goes before FINALLY, or END-PERFORM | **test**: 2002/exitperform, 2002/ecpfatal2 |
| 5a | EXIT PERFORM leaves the innermost inline PERFORM | **test**: 2002/exitperform |
| 5b | EXIT PERFORM CYCLE ends this pass of it | **test**: 2002/exitperform |
| 6 | EXIT PARAGRAPH goes to the paragraph's end, before its return | **test**: 2002/exitperform |
| 7 | EXIT SECTION goes to an empty paragraph after the section's last, before its return | **test**: 2002/exitperform; the letter of the rule (from a PERFORMed last paragraph it falls into the section's end, then the next section) checked by the Stage B review |

## Found by this sweep

Three syntax rules were not enforced (1, 2, 7), one 85-only rule was not
(EXIT PROGRAM last), and general rule 2 was wrong: EXIT PROGRAM in the
run unit's first program ended it. The CYCLE message read backwards
("is not in an exception-checking PERFORM" for one that is); it now says
CYCLE is not allowed there. None of the 229 Open Systems programs, the
CCVS-85 suite or majesty breaks rule 1 or 7.
