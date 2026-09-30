# The arithmetic statements: 14.7.7 and ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE

Swept 2026-09-29 (ISSUES-101). X3.23-1985: VI-68..VI-81 and the
statements' own pages (6.4.4 Arithmetic Statements; ADD 6.6, COMPUTE,
DIVIDE, MULTIPLY, SUBTRACT). 2023: 14.7.7, 14.9.2, 14.9.8, 14.9.12,
14.9.26, 14.9.44. CCVS-85 exercises these at length (the NC arithmetic
programs, 348 programs matching GnuCOBOL's tally), which is why the
general rules were probed for timing and boundaries rather than for
arithmetic itself.

## 14.7.7 common rules

| rule | paraphrase | disposition |
|---|---|---|
| 1 | operands need not share a description; conversion and alignment supplied | **test**: fixed/arith, fixed/compute, CCVS NC |
| 2 | the composite of operands at most 31 digits (18 in 1985) | **refused** past 31 in both editions: bad/arith-composite; 19-31 under -std=85 taken as BP-E14 (warn/ext-every) -- majesty's dist01; under -std=2002 computed on the wide path (docs/wide.md, 2002/wide2). Not checked before this sweep |
| 3 | standard-decimal/-binary arithmetic | **n/a**: 2014's ARITHMETIC clause |
| 4a | the initial evaluation into an intermediate; senders identified at the start; a size error there changes no receiver | **test**: free/arithrules (senders once), free/remrnd, 2002/ecsize |
| 4b | each receiver identified as it is reached, left to right; a size error leaves only that one unchanged | **test**: free/arithrules -- ADD 1 TO i t (i) adds to the new i's element, and of three receivers only the overflowing one is kept; the oracle agrees |
| NOTE 3 | overlapping sender and receiver still defined | **test**: free/arithrules (ADD a TO a) |

## Per statement

| rule | paraphrase | disposition |
|---|---|---|
| ADD, SUBTRACT SR 1 | composite: every operand, GIVING items apart; CORRESPONDING by pair | **refused** as above |
| MULTIPLY, DIVIDE SR 3 (85) | composite: the receiving items (DIVIDE: not the REMAINDER) | **refused** as above |
| all | identifiers numeric; literals numeric; only a GIVING receiver numeric-edited | **refused**: "'e' is not numeric", "an arithmetic operand must be numeric" |
| CORRESPONDING | groups; pairs by 14.7.6 | **test**: free/corr |
| DIVIDE formats 4-5 | REMAINDER with one GIVING item | **refused**: bad/divide-remainder-two -- accepted before this sweep |
| DIVIDE GR | REMAINDER from the quotient before ROUNDED; division by zero a size error | **test**: free/remrnd, 2002/eczdiv |
| ROUNDED MODE | 2014's | **refused** with the right edition now: bad/rounded-mode (it said 2002) |
| COMPUTE | no composite restriction; one evaluation, stored in each receiver | **test**: fixed/compute |

## 14.6.13.2 incompatible data (EC-DATA-INCOMPATIBLE)

| rule | paraphrase | disposition |
|---|---|---|
| 2 | a numeric sending item failing the NUMERIC class test, referenced: EC-DATA-INCOMPATIBLE | **test**: 2002/ecincompat (ADD), 2002/ecincompat2 (COMPUTE); also MOVE's numeric sender and relation operands. Never raised before this sweep. Since ISSUES-103 every other statement that reads numeric content raises it too: DISPLAY, STRING senders, subscripts and reference-modification positions, SET, PERFORM TIMES and VARYING, GO TO DEPENDING, function arguments, INITIALIZE ... BY, CALL BY VALUE, STOP RUN -- one program per site in the harness's ecsites gate (tests/ecsites/sites.txt) |
| the class test | NUMERIC on a packed item | **test**: free/packedclass (the oracle agrees) -- IS NUMERIC on a packed item was always true before |
| 1, 3-6 | boolean content, floating point, de-editing, dynamic items | boolean: the boolean sweep's checks; floating and dynamic: **n/a** (2014) |

## Found by this sweep

The composite of operands was not checked, DIVIDE REMAINDER took several
GIVING items, ROUNDED MODE was assigned to the wrong edition,
EC-DATA-INCOMPATIBLE was never raised, and IS NUMERIC on packed data was
always true. The timing and size-error rules all held. CCVS-85, the Open
Systems suite and majesty are unaffected.
