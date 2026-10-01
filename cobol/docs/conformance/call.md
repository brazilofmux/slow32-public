# The CALL parameter family: 14.2 PROCEDURE DIVISION header, 14.9.4 CALL, 14.8.2-3; 8.4.6.3 program-name scope

Implemented 2026-09-29 (ISSUES-98). This page covers the parameters and
the returning item; the rest of CALL (ON EXCEPTION, CANCEL, the program
registry) predates the sweep and is not yet swept rule by rule.

## How it is carried

- Arguments 1-8 in r3-r10, 9-16 on the stack at the callee's entry
  sp + 0, 4, ..., as the C ABI has them; the caller reserves that area
  around the call (this compiler keeps its own frame's lr at sp + 0).
- BY REFERENCE: the address. BY CONTENT: the address of a copy. BY
  VALUE: an integer word.
- In the called program each USING item is a LINKAGE record reached
  through a cell. BY VALUE: the cell points at a copy in the
  activation's own frame, above the fixed part, the value stored into
  it as into the item -- so a RECURSIVE program's parameter is its own.
- OMITTED passes a NULL address. Every CALL compiled -std=2002 records
  its argument count in `cob_call_nargs`; a program with OPTIONAL
  parameters reads it at entry, and a parameter past the count gets a
  NULL cell (a trailing argument omitted). A program entered from C
  sees -1 and takes every parameter as given.
- PROCEDURE DIVISION RETURNING of a program: the caller allocates the
  returning item (14.2.3 GR 6 NOTE 1) and leaves its address in
  `cob_call_retaddr`; the program takes it as its returning item's cell.
  Every -std=2002 program says at exit whether it had a returning item
  (`cob_call_returned`), so CALL ... RETURNING of a C function (its
  result in r1, the extension majesty's bridge uses) is told from one of
  a COBOL program. A program called without RETURNING writes its result
  to a scratch item.

## 14.2 PROCEDURE DIVISION header

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | parameters level 01/77 LINKAGE, once each, no BASED or REDEFINES | **refused**: "must be a level 01 or 77 item of the LINKAGE SECTION", bad/std2002-using-twice (was accepted) |
| SR 2 | BY VALUE parameters numeric or pointer (object: n/a) | **refused**: bad/std2002-using-value-alnum |
| SR 3-4 | RETURNING in a function; allowed in a program | **test**: 2002/userfn, 2002/callreturning |
| SR 5-6 | the returning item level 01/77 LINKAGE, not BASED or REDEFINES, not a parameter | **refused**: bad/std2002-returning-ws, -returning-using |
| SR 7-13 | RAISING, object orientation | **n/a** (RAISING: the named EXIT RAISING gap) |
| GR 2-4 | positional correspondence; OPTIONAL admits OMITTED; BY REFERENCE / BY VALUE carry over | **test**: 2002/callparams (the oracle agrees) |
| GR 6-7 | the returning item is the caller's; its initial value undefined | **test**: 2002/callreturning |
| GR 8-9 | by reference the same storage; by content a copy | **test**: throughout (CCVS IC) |

## 14.9.4 CALL (the parameter rules)

| rule | paraphrase | disposition |
|---|---|---|
| SR 20 | BY CONTENT not omitted for a receiving-capable identifier... | existing behaviour; not re-swept |
| SR 21-23 | BY VALUE on both sides; numeric (or pointer) identifiers; numeric literals | **test**: 2002/callparams; **refused**: bad/std2002-call-omitted-value |
| SR 24 | OMITTED only for an OPTIONAL parameter | the callee is compiled apart, so not checked at the CALL; the argument arrives as a NULL address |
| GR 4 | RETURNING: the result placed in identifier-3 | **test**: 2002/callreturning (no oracle: GnuCOBOL 4 does not implement program RETURNING) |
| GR 11 | OMITTED, or a trailing argument not passed: the omitted-argument condition true | **test**: 2002/callparams (both ways) |
| GR 12 | an omitted parameter referenced (not as an argument, not in the condition): EC-PROGRAM-ARG-OMITTED | **test**: 2002/ecargomit (emit_item_addr, when checked) |
| 8.8.4.8 | identifier IS [NOT] OMITTED | **test**: 2002/callparams; **refused**: bad/std2002-omitted-not-param |

## 14.8.2-3 conformance

A plain CALL (no prototype, 14.8.2.3.3 rule 1) wants a BY VALUE
parameter of the argument's length: the bytes pass. Here the value
passes and is stored into the parameter as COMPUTE would store it --
the same result for a conforming pair, and rule 2a's for the prototype
case. A non-conforming pair (a binary argument, a DISPLAY parameter) is
undefined; GnuCOBOL copies the bytes, this compiler converts.

## 8.4.6.3 the scope of program-names (implemented 2026-10-01, ISSUES-120)

A scan of the source before compiling builds the tree of programs, with
COMMON and RECURSIVE; a contained program's entry is a local symbol, and
each unit carries a table of the contained programs it may name, which
the registry's lookups (CALL identifier, ON EXCEPTION, CANCEL) consult.

| rule | paraphrase | disposition |
|---|---|---|
| 1 | a contained program without COMMON: named by its container only, or by itself when RECURSIVE | **test**: free/progscope (a sibling and a nested cousin get ON EXCEPTION). Every program could be called from anywhere before |
| 2 | COMMON: by everything inside its container, but not by itself or what it contains unless RECURSIVE | **test**: free/progscope (a sibling, and a program two levels down) |
| 3 | an outermost program: from anywhere in the run unit | **test**: every CALL of a separately compiled program |
| (14.9.4) | CALL identifier and CANCEL under the same rules | **test**: free/progscope (CALL identifier both ways). GnuCOBOL takes ON EXCEPTION after CALLs that succeeded here, and lets an out-of-scope program be called (docs/oracles.md) |

## Limits

16 CALL arguments (the staging slots), 32 USING items; functions keep
seven parameters and no BY VALUE (bad/std2002-call-17-args).
