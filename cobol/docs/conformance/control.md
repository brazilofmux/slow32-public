# STOP, GOBACK, CONTINUE, CANCEL: 14.9.42, 14.9.18, 14.9.9, 14.9.5

Swept 2026-09-30. X3.23-1985: STOP 6.24, CANCEL (the Inter-Program
Communication module) 5.3; 1985 has no GOBACK (BP-E2, IBM's) and no
EXIT PROGRAM RAISING. 2002: 14.8.38, 14.8.17, 14.8.8, 14.8.5. 2023: as the
title.

## STOP (2023 14.9.42; 2002 14.8.38; 1985 6.24)

| rule | paraphrase | disposition |
|---|---|---|
| 1985 SR 1 | STOP literal: not an ALL literal | **refused**: bad/stop-all-literal (it was refused as "needs RUN or a literal") |
| 1985 SR 2; 2002 SR 1 | STOP RUN is the last statement of a consecutive sequence of imperative statements in its sentence | **refused**: bad/stop-not-last, bad/std2002-stop-not-last. `STOP RUN DISPLAY "x"` compiled before, the DISPLAY dead. ELSE, WHEN, a scope terminator or the period end the sequence. CCVS, majesty and the papers have no such sentence. 2023 rewords the rule ("the last statement in any discrete block of code") |
| 1985 SR 3 | STOP literal: a numeric literal is an unsigned integer | **refused**: bad/stop-literal-signed |
| STOP literal | obsolete in 1985, deleted in 2002 | BP-O3: displayed, and the run goes on (the operator's resume). Refused under `-std=2002` |
| 2002 format: WITH ERROR / NORMAL STATUS | the status to the operating system | **test**: 2002/stopstatus (exit status 4 from an item; the harness now checks a `.exitcode` beside a test). An integer is the exit status; ERROR alone 1, NORMAL alone 0; an alphanumeric value goes to standard error, then 1 or 0 (GR 5: the implementor's). These are GnuCOBOL's exit statuses. The phrase was refused as "'with' is not a COBOL verb". **refused** under `-std=85`: bad/stop-status-85 |
| SR 2-3 | identifier-1 an integer, or an item of usage display or national; a numeric literal an integer | **refused** otherwise |
| GR 1 | normal run unit termination: files closed | `cob_stop_run` |
| extensions | `STOP RUN RETURNING n`, `STOP RUN identifier` (RM/COBOL, the Open Systems suite) | BP-E6 |

## GOBACK (2023 14.9.18; 2002 14.8.17)

| rule | paraphrase | disposition |
|---|---|---|
| 1985 | no GOBACK | BP-E2: accepted, as IBM has it |
| SR 1 | not in a declarative whose USE has GLOBAL | **refused**: bad/std2002-goback-global-use |
| SR 2 | RAISING names a level-3 exception-name; an EC-USER one is in the header's RAISING | **test**: 2002/ecraising (implemented 2026-10-06, standard-queue item 3); **refused**: bad/std2002-goback-raising |
| SR 3-4 | RAISING identifier: an object reference | **n/a**: object orientation, deferred (refused by name) |
| GR 1b | RAISING: the condition is raised in the caller if checking for it is enabled there, after the result is returned, and the caller continues as its CALL's rules say; LAST hands over the condition raised, EC-RAISING-NOT-SPECIFIED in place of an EC-USER one the header does not list, nothing when none is raised | **test**: 2002/ecraising: the caller's USE declarative (sub1), its WHEN (case last), the fatal end when neither (the checked case, run by hand); a caller not checking the condition ignores it (sub1 called with checking off); nspec, EC-RAISING-NOT-SPECIFIED. How: the callee leaves the name with libcob (`cob_ec_propagate`); every CALL and function invocation compiled at -std=2002 asks (`cob_ec_propagated`) for it against the caller's enabled names -- exact, then its level-2 group, then EC-ALL -- and dispatches as a raise of its own (`emit_ec_propagated`, control.h). RAISING LAST is for a declarative or a WHEN phrase (bad/std2002-exit-program-raising) |
| WITH ... STATUS (2023 GR 3) | in a program no caller controls, as STOP RUN with the status phrase; under a caller the return, the phrase idle | **implemented** under -std=2023 (queue item 31, 2026-10-07): 2023/stmts2023 (its `.exitcode` 7; a called program's GOBACK WITH STATUS 9 returns); **refused** under -std=2002 and -std=2014: bad/std2002-goback-status, bad/std2014-goback-status |
| GR 1-3 | a called program returns; a main program operates as STOP RUN | **test**: free/dyncall and every subprogram test |

## CONTINUE (2023 14.9.9; 2002 14.8.8)

| rule | paraphrase | disposition |
|---|---|---|
| SR 1, GR 1 | a no-operation statement, wherever a statement may be | **test**: throughout (ecsites, the EVALUATE tests) |
| AFTER ... SECONDS (GR 1-2) | the run suspended that long; the value in a 9(n)V9(m) temporary of the implementor's n and m; a negative value zero and EC-CONTINUE-LESS-THAN-ZERO, nonfatal | **implemented** under -std=2023 (queue item 31, 2026-10-07): 2023/stmts2023 (a hundredth, zero, -1 with the condition checked and a declarative); **ruling**: 9(9)V99, more is the maximum, the suspension the libc's nanosleep (the MMIO sleep service); **refused** under -std=2002 and -std=2014: bad/std2002-continue-after, bad/std2014-continue-after |

## CANCEL (2023 14.9.5; 2002 14.8.5; 1985 5.3)

| rule | paraphrase | disposition |
|---|---|---|
| 1985 SR 1-2; 2023 SR 1-2 | literal-1 alphanumeric; identifier-1 alphanumeric (or national) | **refused**: bad/cancel-numeric-literal, bad/cancel-numeric-item |
| SR 3 | a program prototype | **n/a**: REPOSITORY program prototypes are not implemented |
| GR 1-3 | the next CALL finds the program in its initial state | **test**: free/dyncall, 2002/cancelrules (WORKING-STORAGE back to its VALUEs) |
| GR 4 | the programs it contains are canceled too, the last first | **test**: 2002/cancelrules. Fixed in this sweep: a contained program kept its state |
| GR 5 | an active program is not canceled; EC-PROGRAM-CANCEL-ACTIVE when checked, the implementor's result otherwise | **test**: 2002/cancelactive. Unchecked, the CANCEL does nothing: it used to reset the WORKING-STORAGE of the program still running. GnuCOBOL stops the run. Under `-std=85` the activity count is not kept, and CANCEL cannot tell |
| GR 7 | a program not called, or already canceled: no action | **test**: free/dyncall (`CANCEL` of a name that is no program) |
| GR 8 | EXTERNAL data unchanged | the cancel routine restores the program's own records only |
| GR 9 | an implicit CLOSE of each open internal file | **test**: 2002/cancelrules (the second OPEN OUTPUT after CANCEL gets 00, not 41). Fixed in this sweep. EXTERNAL files are not closed |
