# 14.9.33 RESUME statement

Implemented 2026-10-07 (docs/plans/standard-queue.md item 42). COBOL
2002 and later; under -std=85 refused (bad/resume-85). Test 2002/resume
(no oracle: GnuCOBOL 4 has no RESUME).

How: RESUME AT NEXT STATEMENT leaves the procedure as its end would --
a declarative section's exit (as EXIT SECTION), a WHEN phrase's return to
its resume point (cob_ecp_pop) -- after setting a mark (cob_resume_mark)
that the return point reads where it would otherwise end the run for a
fatal condition (cob_ec_abort_unless_resumed; cob_ecp_pop): RESUME is the
recovery from a fatal condition the text provides. RESUME AT
procedure-name is a GO TO out of the declarative, its PERFORM frame
dropped first (cob_perform_exit), so the next raise finds the declarative
inactive.

## Syntax rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | only in a declarative, or in a WHEN phrase of an exception-checking PERFORM, there with NEXT STATEMENT | **refused**: bad/std2002-resume-outside, -when-proc |
| 2 | not in a declarative whose USE is GLOBAL | **refused**: bad/std2002-resume-global |
| 3 | procedure-name-1 in the nondeclarative part | **refused**: bad/std2002-resume-decl-proc |

## General rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | in a global declarative, as CONTINUE | **n/a**: rule 2 refuses the statement there |
| 2a | NEXT STATEMENT after an exception condition: the implicit CONTINUE after the statement that raised it (a CALL's for a propagated one; the lowest-level statement) | **test**: 2002/resume (a fatal EC-BOUND-REF-MOD taken twice by a declarative, the run going on after each MOVE; a WHEN phrase's RESUME); a propagated condition's CALL is the statement the condition arrives at (control.h) |
| 2b | a declarative performed from the nondeclarative part: after the PERFORM's range | the declarative's exit returns where the PERFORM put it: **ruling**: the same code path, a PERFORM of a declarative being refused here (use.md rule 3) |
| 3 | procedure-name-1: as GO TO | **test**: 2002/resume (`RESUME AT recovery` on the second raise) |
