# The CALL parameter family: 14.2 PROCEDURE DIVISION header, 14.9.4 CALL, 14.8.2-3; 8.4.6.3 program-name scope; 11.5 FUNCTION-ID, 11.10 PROGRAM-ID, 12.3.8 REPOSITORY (prototypes, AS literal)

Implemented 2026-09-29 (ISSUES-98). This page covers the parameters and
the returning item; the rest of CALL (ON EXCEPTION, CANCEL, the program
registry) predates the sweep and is not yet swept rule by rule. The
prototypes, AS literals and CALL's format 2 were added 2026-10-06
(docs/plans/standard-queue.md item 8), with the function side's BY
VALUE, OPTIONAL and sixteen parameters. Not swept here: 11.6, 11.7,
11.8, 11.9.

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
  A function's result goes the same way since 2026-10-06 (it used to
  ride after the arguments, which held functions to seven parameters);
  a function's arguments take CALL's path above, so a function has
  sixteen parameters, OPTIONAL ones, and BY VALUE ones -- for which the
  caller converts the argument into a copy described as the parameter
  is, as COMPUTE or MOVE would (14.8.2.3.3 rule 2), and passes the
  copy's address, the copy being its own and discarded.
- The external repository (12.3.8): a function's compile writes
  `name.s32fn`, a program's at -std=2002 `name.s32pg`, beside the
  output -- its parameters' and returning item's descriptions, how each
  is passed, which are OPTIONAL. A prototype in the group supplies the
  same in memory; a definition later in the group must conform to it
  and replaces it. A caller finds a signature in the group first, then
  beside the output, beside the source, then on -I.
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
| SR 7 | RAISING names level-3 EC-USER exception-names | **test**: 2002/ecraising (implemented 2026-10-06, standard-queue item 3: the names an EXIT PROGRAM / GOBACK RAISING of this unit may hand its caller, and what RAISING LAST turns into EC-RAISING-NOT-SPECIFIED); **refused**: bad/std2002-procedure-raising (a name that is not EC-USER), an object-class or interface name by name |
| SR 8-9, 12-13 | object orientation | **n/a** |
| SR 10-11 | a function or program prototype's procedure division is its header | **refused**: bad/std2002-proto-body ("a function prototype has no statements"); a program prototype the same |
| GR 2 (BY VALUE, function) | BY VALUE parameters of a function | **test**: 2002/fnproto (scale: 7 and 1.5 into 9(3) and 9V9, converted; twelve: four BY VALUE after eight BY REFERENCE) |
| GR 3 (OPTIONAL, function) | OPTIONAL parameters of a function, OMITTED or left off | **test**: 2002/fnproto (opt-sum, IS NOT OMITTED); **refused**: bad/std2002-fn-omitted-notopt |
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

## 14.9.4 CALL format 2: through a program prototype (implemented 2026-10-06)

`CALL program-prototype-name`, `CALL literal AS program-prototype-name`
and `CALL literal AS NESTED`. The arguments are checked against the
signature -- the prototype's, an earlier definition's, or the external
repository's -- and converted where the standard converts them. Test:
2002/pgproto (no oracle: GnuCOBOL 4 has no program prototypes).

| rule | paraphrase | disposition |
|---|---|---|
| format 2 | BY CONTENT or BY VALUE may be an arithmetic expression | **test**: 2002/pgproto (`a + 1`, `b / 2` BY CONTENT; `b + 1` BY VALUE) |
| SR 13 | NESTED only in a program definition | **refused**: "the NESTED phrase is a program definition's" |
| SR 14 | a restricted program-pointer's prototype | **test**: 2002/pgpointer (CALL through a PROGRAM-POINTER TO greet checks and converts the arguments by greet's signature; docs/conformance/usage.md) |
| SR 15 | NESTED: a literal, naming a contained or common program | **test**: 2002/pgproto (inner); **refused**: bad/std2002-call-nested-unknown, and an identifier with NESTED |
| SR 16 | the prototype-name is a program-specifier of the REPOSITORY | **refused**: "CALL ... AS takes NESTED or a program-prototype-name of the REPOSITORY"; a bare word that is neither a data item nor a program-specifier is a plain CALL identifier error |
| SR 17-18 | sending operands; no ANY LENGTH argument | **test**; ANY LENGTH as format 1 |
| SR 19, 21 | BY REFERENCE / BY CONTENT to a BY REFERENCE parameter; BY VALUE to BY VALUE | **refused**: "argument k of 'p' is BY VALUE, the parameter BY REFERENCE" and the reverse |
| SR 20 | BY CONTENT not omitted for a receiving-capable identifier | an identifier with no BY phrase goes BY REFERENCE and must then conform (14.8.2.3.2); an expression or literal is BY CONTENT |
| SR 22 | BY VALUE: class numeric (object, pointer) | **test**: 2002/pgproto; as format 1 |
| SR 24 | OMITTED for an OPTIONAL parameter | **refused** through a signature: "argument k of 'p' is OMITTED, but the parameter is not OPTIONAL" (format 1 cannot tell: the callee is compiled apart) |
| GR 1 | the program: literal-1 / identifier-1 the externalized name; a bare prototype-name, the name the REPOSITORY gives it (AS literal, else the name) | **test**: 2002/pgproto (`call area` reaches "pg-area"; `call "pg-area" as area`) |
| NESTED, a program later in the group | | **gap**: the signature of a contained program defined after the CALL is not known when the CALL is compiled (the group is parsed once, in order), so its arguments pass as format 1's; an earlier common program's is used |

## 11.5 FUNCTION-ID, 11.10 PROGRAM-ID: AS literal and IS PROTOTYPE

| rule | paraphrase | disposition |
|---|---|---|
| 11.5 / 11.10 format 1, AS | the externalized name: the entry symbol, the registry name CALL literal finds, the signature file's name | **test**: 2002/fnproto (`scale as "fn-scale"`), 2002/pgproto (`area as "pg-area"`) |
| 11.5.3, 11.10.3 rule 1 | a nonempty alphanumeric literal, no figurative constant | **refused**: "AS takes a nonempty alphanumeric literal" |
| 11.5 / 11.10 format 2 | IS PROTOTYPE: a signature for this group, no code, not the main program | **test**: 2002/fnproto, 2002/pgproto (prototypes ahead of the main program); a definition must conform: bad/std2002-proto-mismatch, bad/std2002-pgproto-mismatch |
| 11.10 format 2 | a program prototype is not contained | **refused**: "a program prototype is not contained in a program" |
| 11.10.3 rules 2-7 | INITIAL, COMMON, RECURSIVE | the earlier sweep (docs/conformance/control.md, recursion) |

## 12.3.8 REPOSITORY

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | a name specified twice: identically | **gap**: not checked (the last entry's AS literal is taken) |
| SR 2 | the AS literals alphanumeric or national, nonempty, no figurative constant | **refused**: "REPOSITORY FUNCTION/PROGRAM ... AS takes a nonempty alphanumeric literal" |
| SR 3-9 | classes, interfaces | **n/a**: object orientation |
| format | FUNCTION name [AS literal], one name per specifier; the intrinsic form lists names and ends INTRINSIC | **refused**: bad/std2002-repository-list (the list form used to be taken for user functions) |
| SR 10 | FUNCTION name [AS literal]: a prototype or earlier definition in the group, or the external repository | **refused**: bad/std2002-fn-nosig (used to be refused at the invocation) |
| SR 11 | the function's own name is ignored | **test**: 2002/userfn (half-of names itself) |
| SR 12-13 | intrinsic names not user-defined words | the earlier sweep (docs/conformance/functions.md) |
| SR 14 | PROGRAM name [AS literal]: a prototype or earlier definition, or the external repository | **refused**: "REPOSITORY PROGRAM x: no prototype or earlier definition in this compilation group, and no x.s32pg ..." |
| SR 15 | the program's own name, or a containing program's, is ignored | **test**: a program naming itself (checked by hand) |
| SR 16 | PROPERTY | **n/a**: object orientation |
| GR 1, 3-4 | classes | **n/a** |
| GR 2 | AS: the externalized name | **test**: 2002/fnproto (`function sc as "fn-scale"`), 2002/pgproto |

## 14.8.2-3 conformance

A plain CALL (no prototype, 14.8.2.3.3 rule 1) wants a BY VALUE
parameter of the argument's length: the bytes pass. Here the value
passes and is stored into the parameter as COMPUTE would store it --
the same result for a conforming pair, and rule 2a's for the prototype
case. A non-conforming pair (a binary argument, a DISPLAY parameter) is
undefined; GnuCOBOL copies the bytes, this compiler converts.

With a signature (a function, or a program through a program-specifier
or NESTED), 2026-10-06:

| rule | paraphrase | disposition |
|---|---|---|
| 14.8.2.1 | as many arguments as parameters, but for trailing OPTIONAL ones | **refused**: "the function 'f' takes n arguments (the last ones OPTIONAL), not m"; the program the same |
| 14.8.2.2 rule 1 | a group BY REFERENCE: the parameter a group or alphanumeric, no longer than the argument | **refused**: "the parameter is a group of n bytes, longer than ..." |
| 14.8.2.2 rule 2 | a group BY CONTENT: as MOVE | **test**: the copy is made by MOVE |
| 14.8.2.3.2 rule 2 | BY REFERENCE through a signature: the same PICTURE, USAGE, JUSTIFIED, BLANK WHEN ZERO, SIGN (ANY LENGTH matching any length) | **refused**: bad/std2002-call-sig-mismatch; functions as before (2002/fnargbad) |
| 14.8.2.3.3 rule 2a | BY CONTENT or BY VALUE to a numeric parameter: as COMPUTE | **test**: 2002/pgproto (12.5 for 9(3)V9 gives 12.5, `b / 2` 2.0), 2002/fnproto (BY VALUE 1.5 into 9V9) |
| 14.8.2.3.3 rule 2b | to an index parameter: as SET | **gap**: an index parameter is not a signature's concern yet |
| 14.8.2.3.3 rule 2d | otherwise as MOVE | **test**: the copy is made by MOVE |
| 14.8.3 | the returning item as the signature describes it | **refused**: "RETURNING 'x' is not described as the program's returning item is", "the program 'p' has no RETURNING item" |

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

16 CALL arguments (the staging slots), 32 USING items; a function
takes 16 (bad/std2002-call-17-args); 32 program-specifiers and 32
function-specifiers in a REPOSITORY; 64 program and 128 function
signatures known to one compile.
