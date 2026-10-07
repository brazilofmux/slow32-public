# Compiler directives: conditional compilation (7.3.5-7.3.8, 7.3.11, 7.3.13, 7.3.16), CALL-CONVENTION, LEAP-SECOND, LISTING, PAGE (7.3.9, 7.3.17-19), PROPAGATE (7.3.21), REF-MOD-ZERO-LENGTH (7.3.23), and the 2023 directives COBOL-WORDS (7.3.10), DISPLAY (7.3.12), FLAG-14 (7.3.15), POP and PUSH (7.3.20, 7.3.22)

Swept 2026-10-06 (docs/plans/standard-queue.md item 6), when the three
directives were implemented. ISO/IEC 1989:2023: 7.3.5 conditional
compilation, 7.3.6 compile-time arithmetic, 7.3.7 compile-time boolean
expressions, 7.3.8 constant conditional expressions and the defined
condition, 7.3.11 DEFINE, 7.3.13 EVALUATE, 7.3.16 IF; and 13.10's
CONSTANT ... FROM, which reads a compilation variable.

How: the directives are evaluated in the first step of text
manipulation, as the text words go by with each COPY's library text
expanded in its place (src/cobc/copy.h, `cond_directive`). So a
directive applies to the text after it (7.3.4 rule 5), a COPY in an
omitted branch is never read, and a variable defined in library text is
known after the COPY. The conditional directives leave nothing in the
text; omitted text is dropped before REPLACE and the parser see it.

## The general rules (7.3.3-7.3.8)

| rule | paraphrase | disposition |
|---|---|---|
| 7.3.3 SR 1-2, 5 | one line each, after spaces only; `>>` then optional space | **test**: 2002/condcomp (`>>  IF`, indented directives) |
| 7.3.3 SR 3-4 | in fixed form in the program-text area; an inline comment may follow | **test**: the reader (free and fixed forms, `*>` ends the directive) |
| 7.3.3 SR 8 | anywhere, in source or library text | **test**: 2002/condcomp (DEFINE and IF in the copybook condcomp-lib) |
| 7.3.3 SR 10 | no concatenation, figurative constant or floating-point literal in a directive | **refused**: no such form is read in a directive (a word that is no compilation variable is refused by name) |
| 7.3.4 GR 1 | a directive line is not changed by COPY REPLACING or REPLACE | **test**: directive lines are TW_DIR words, which REPLACING and REPLACE pass over |
| 7.3.4 GR 5 | a directive applies to the text that follows it, whatever the flow | **test**: 2002/condcomp and condcomp2 (CONSTANT FROM before and after an OVERRIDE) |
| 7.3.6 SR 1 | arithmetic: no exponentiation, operands fixed-point literals (or compilation variables, 7.3.11 GR 1), no division by zero | **refused**: `**`, a non-numeric operand, bad/std2002-define-divzero |
| 7.3.6 SR 2, GR 2 | precision, mode of arithmetic: the implementor's | **ruling**: long double; a result past 18 digits is refused |
| 7.3.6 GR 3 | the result truncated to its integer part | **test**: 2002/condcomp (WIDTH AS LEVEL * 4 + 1) |
| 7.3.7 | compile-time boolean expressions | **gap**: a boolean literal or variable is taken as an operand and as a boolean condition; the boolean operators (B-AND ...) in a directive are refused as not taken |
| 7.3.8 SR 1a | a relation between literals or literal expressions of one category; non-numeric compared for (in)equality only | **test**: 2002/condcomp; **refused**: bad/std2002-if-mixed-category, bad/std2002-if-alnum-greater |
| 7.3.8 SR 1b | a boolean condition of boolean literals | **ruling**: true when any bit is 1 |
| 7.3.8 SR 1c | the defined condition | **test**: 2002/condcomp (IS DEFINED, IS NOT DEFINED) |
| 7.3.8 SR 1d | complex conditions, AND, OR, NOT, parentheses; no abbreviated combined relations | **test**: 2002/condcomp (AND, NOT (...)) |
| 7.3.8 GR 2 | non-numeric literals compared by encoding, unequal lengths unequal | **test**: 2002/condcomp ("SLOW" is not "slow") |
| 7.3.8.4 SR 1 | the variable in a defined condition is not a directive word | **refused** where it is defined (bad/std2002-define-word) |

## DEFINE (7.3.11)

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | the name is not a compiler-directive word | **refused**: bad/std2002-define-word |
| SR 2 | a second DEFINE of a name: after OFF, or with OVERRIDE, or the same value | **refused**: bad/std2002-define-redefine; **test**: 2002/condcomp (OVERRIDE) |
| SR 3-4 | arithmetic and boolean expressions as 7.3.6, 7.3.7 | **test** (arithmetic); boolean expressions a **gap** (above) |
| GR 1 | after DEFINE the name stands where a literal of its category may, in a defined condition, in CONSTANT FROM | **test**: 2002/condcomp |
| GR 2 | after OFF only a defined condition may name it | **refused**: bad/std2002-define-off-used |
| GR 3 | OVERRIDE sets it unconditionally | **test**: 2002/condcomp |
| GR 4 | PARAMETER: from the operating environment, by the implementor's method; none, not defined | **ruling**: the compiler's `-D name[=value]` option (compile.sh passes it); a value of digits is numeric, any other alphanumeric, none is 1; no `-D`, not defined -- **test**: 2002/condcomp without -D (and checked by hand with `-D FROMENV=yes`) |
| GR 5 | a single numeric literal is a literal, not an expression | **test**: 2002/condcomp (`LEVEL AS 3`) |
| GR 6-8 | the value of the expression or literal | **test**: 2002/condcomp |

## EVALUATE (7.3.13)

| rule | paraphrase | disposition |
|---|---|---|
| SR 2-7 | each phrase begins on a new line and is on that line | **test**: each phrase is a directive line of its own |
| SR 8 | the text may be any lines, directives included | **test**: 2002/condcomp (an IF nested in a WHEN) |
| SR 9 | the phrases of one EVALUATE in one library text or all in source | **refused**: "the phrases of an >>EVALUATE are all in one library text" and "the library text ends inside" |
| SR 11-12 | operands of one category; THROUGH numeric | **refused**: bad/std2002-evaluate-thru-alnum |
| GR 1 | the text is subject to COPY and REPLACE | **test**: the text words go on to REPLACE |
| GR 2-7 | format 1: the subject against each WHEN in turn, THROUGH inclusive, OTHER, by encoding | **test**: 2002/condcomp |
| GR 8-10 | format 2, TRUE: each WHEN's condition in turn | **test**: 2002/condcomp |

## IF (7.3.16)

| rule | paraphrase | disposition |
|---|---|---|
| SR 1-6 | IF, ELSE, END-IF each on a line of its own; any text between, directives included | **test**: 2002/condcomp, condcomp2 |
| SR 7 | the phrases of one IF in one library text or all in source | **refused**: "the phrases of an >>IF are all in one library text"; an IF open at the end of the text: bad/std2002-if-unclosed |
| GR 1 | the text is subject to COPY and REPLACE | **test**: as EVALUATE |
| GR 2-3 | true: text-1 in, text-2 out; false: the reverse | **test**: 2002/condcomp, condcomp2 (GnuCOBOL agrees but for CONSTANT FROM: docs/oracles.md) |

## CONSTANT ... FROM (13.10)

| rule | paraphrase | disposition |
|---|---|---|
| format 2 | `01 name CONSTANT [IS GLOBAL] FROM compilation-variable` | **test**: 2002/condcomp (the value in effect where the entry stands); **refused** for a name no DEFINE made: bad/std2002-constant-from |

## The small directives (7.3.9, 7.3.17, 7.3.18, 7.3.19)

Implemented 2026-10-06 (standard-queue item 7). None of them changes
the program here, for the reasons in the table; each is read and its
syntax checked. Test: 2002/smalldir (GnuCOBOL 4 as the oracle).

| rule | paraphrase | disposition |
|---|---|---|
| CALL-CONVENTION GR 1-2 | COBOL, the default, maps names as with no AS phrase; another name is the implementor's | **ruling**: COBOL is the one convention; any other name is refused, bad/std2002-call-convention |
| CALL-CONVENTION GR 3 | the implementor may use it for other details | **n/a**: none |
| LEAP-SECOND SR 1 | outside a compilation unit | **refused**: bad/std2002-leap-second-inside (a contained program's END PROGRAM does not end the containing unit) |
| LEAP-SECOND GR 1-7 | ON: a 60th second may be reported; OFF: never | **ruling**: the run-time clock is POSIX time, which has no leap second; a seconds value is never above 59 with either |
| LISTING GR 1 | no listing produced: the directive is ignored | **ruling**: no listing; ON or OFF checked, anything else refused (bad/std2002-listing-operand) |
| LISTING GR 2-5 | listing rules | **n/a**: no listing |
| PAGE SR 1-2, GR 1-3 | comment-text unchecked; no effect without a listing | **test**: 2002/smalldir (a quote left open in the comment-text) |

## PROPAGATE (7.3.21)

Implemented 2026-10-06 (standard-queue item 3, with EXIT PROGRAM and
GOBACK RAISING: docs/conformance/exit.md, control.md). Test:
2002/ecraising, case prop (run by hand: the propagated condition is fatal
in the caller, which has no handler for it, so the run ends there as
14.9.28 rule 20 says). The directive is read in the first text
manipulation step, with the others; what it leaves for the parser is a
mark at each unit that begins while it is on.

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | not inside a compilation unit | **refused**: bad/std2002-propagate-inside |
| GR 1, 3 | ON: propagation enabled for the units that follow, until OFF or the end of the group; OFF the reverse | **test**: 2002/ecraising (lib/ecpropagate.cbl: sub5 after `>>PROPAGATE ON`, sub6 before it, which does not propagate) |
| GR 2 | a condition raised in such a unit and handled by neither a statement's exception phrase nor a declarative is propagated as GOBACK RAISING LAST in a declarative for it would | **test**: 2002/ecraising case prop: where the fatal end would be, the unit returns with the condition and the caller takes it up (goto_set.h `emit_ec_dispatch`, the `g_propagate` arm) |
| GR 4 | the default is OFF | **test**: every other subprogram test |

## REF-MOD-ZERO-LENGTH (7.3.23)

Implemented 2026-10-07 (standard-queue item 24), taken under -std=2014
though 2023's: the zero-length items it completes are 2014's. Read with
the directives and left in the stream for the parser as >>TURN is, so
it is positional (control.h `apply_turn` sets `g_refmod_zero`). The
semantics -- what a part of length zero is and does -- are in
refmod.md, "8.5.4 Zero-length items". Tests 2014/zerolen, zerolen2.

| rule | paraphrase | disposition |
|---|---|---|
| 7.3.23.2 | ON or OFF | **refused**: bad/std2014-refmod-zero-arg; under -std=2002, bad/std2002-refmod-zero-directive |
| 7.3.23.3 GR 1 | omitted or OFF: a reference modification of length zero is EC-BOUND-REF-MOD | **test**: 2014/zerolen2 (OFF after ON, checking on: the declarative runs); **refused** when the zero is written: bad/std2014-refmod-zero-written |
| 7.3.23.1 | ON: a resultant item may be zero-length | **test**: 2014/zerolen (a computed zero, a table element's part), zerolen2 (a written `(3:0)`, national and boolean parts) |

## The 2023 directives: COBOL-WORDS (7.3.10), DISPLAY (7.3.12), FLAG-14 (7.3.15), POP and PUSH (7.3.20, 7.3.22)

Implemented 2026-10-07 (standard-queue item 34), each under `-std=2023`
and refused naming the edition otherwise (bad/std2014-cobol-words,
-flag-14, -push). Test 2023/directives (no oracle: GnuCOBOL 4 has none
of these), warn/flag14-all (every FLAG-14 option flagged once at least;
the harness's gate 4 knows `flag14-*`).

### COBOL-WORDS (7.3.10)

Read at the text manipulation stage into a table the tokenizer applies
to every word after it (`cobol_words_apply`): a synonym (EQUATE) or a
substitute (SUBSTITUTE) becomes the standard word, its spelling kept
for messages; an undefined (UNDEFINE) or substituted-for word is marked
a user word, which no keyword test matches (`is_word`, `at_operand`) and
`user_word` lets through; a RESERVEd word is refused as a user-defined
name. A directive affects no other directive (rule 6): the directive
lines are read before the table applies.

| rule | paraphrase | disposition |
|---|---|---|
| 7.3.10.3 rule 1 | before the first IDENTIFICATION DIVISION; any number of them | **test**: directives (four); **refused**: bad/std2023-cobol-words-late |
| 7.3.10.3 rule 2 | alphanumeric literals, not hexadecimal, no space, case-insensitive | **refused**: bad/std2023-cobol-words-space, -literal; **gap**: a hexadecimal literal of the same bytes is taken |
| 7.3.10.3 rule 3 | literal-1, -3, -4: a reserved word, a context-sensitive word or a function name | **gap**: the context-sensitive words are not tabled, so only the shape of a COBOL word is checked |
| 7.3.10.3 rule 4 | literal-2, -5, -6: a user-defined word, not reserved | **refused**: bad/std2023-cobol-words-reserved (a reserved word), the shape of a COBOL word checked (8.3.1) |
| 7.3.10.3 rule 5 | one word in one directive only | **refused**: bad/std2023-cobol-words-dup |
| 7.3.10.4 rule 2 | EQUATE: a synonym | **test**: directives (`SHOW` for DISPLAY) |
| 7.3.10.4 rule 3 | UNDEFINE: the word a user word | **test**: directives (an item named `page`) |
| 7.3.10.4 rule 4 | SUBSTITUTE: the one word for the other, the other freed | **test**: directives (`DO` for PERFORM, an item named `perform`); **ruling**: a scope terminator is its own word (END-PERFORM stays END-PERFORM) |
| 7.3.10.4 rule 5 | RESERVE: no user-defined word | **refused**: bad/std2023-cobol-words-reserve |

### DISPLAY (7.3.12)

No listing is produced, so the compile-time device is the standard
error (rules 2, 5-6): one line per directive, `file:line: >>DISPLAY`
and the operands in order (rules 1, 4), a number as %.18Lg, a literal
as written. Probed by hand (the harness drops a test's stderr).

| rule | paraphrase | disposition |
|---|---|---|
| 7.3.12.2-3 | literals, compile-time arithmetic and boolean expressions, PARAMETER variable-name; UPON device or LISTING | **test**: directives (`"k is " K " and " K * 2 + 1 UPON LISTING`); **refused**: bad/std2023-display-parameter |
| 7.3.12.4 rule 3 | PARAMETER: the value from the environment (`-D name=value`), no transfer without one | probed: `-D BUILD=42` prints, an unknown name prints nothing |

### FLAG-14 (7.3.15)

A warning mechanism (rule 1) for the forms 2023 changed from 2014 (E.2):
each option a flag the parser turns on and off where the directive
stands (positional, as >>TURN), `[F14-OPTION]` in the warning's text so
a test can count them; all off by default (rule 5); the directive is
applied at every data entry too, so a VALUE option set before the data
division is in force there. The two options the directive evaluates --
EVALUATE and COMPILE-TIME-ARITHMETIC-EXPRESSIONS -- are met at the text
manipulation stage, which keeps its own copy of the flags.

| option (rule 4) | flagged | disposition |
|---|---|---|
| ALL | every option | **test**: warn/flag14-all |
| COMPILE-TIME-ARITHMETIC-EXPRESSIONS | a compile-time division (E.2 item 6: the mode of arithmetic is the implementor's now; multiplication and addition are exact either way) | **test**: flag14-all |
| EVALUATE | an >>EVALUATE with a >>WHEN and a >>WHEN OTHER | **test**: flag14-all |
| I-O-DECLARATIVE | a statement that could take INVALID KEY without it, or a READ without AT END, while a USE procedure for an open mode (any; INPUT or I-O for AT END) is declared in the compilation group | **test**: flag14-all; **ruling**: any such declarative in the group, not only one that applies to the file |
| I-O-STATUS-04, I-O-STATUS-07 | a FILE STATUS item compared with "04" or "07" | **test**: flag14-all |
| NUM-ED-ZERO-FIGCONST, VALUE-ZERO | VALUE ZERO of a numeric-edited item (both options name it) | **test**: flag14-all (one item, both flagged); NUM-ED-ZERO-FIG-CONSTANT is taken as rule 4g spells it |
| READ-PREVIOUS | a READ PREVIOUS | **test**: flag14-all |
| REF-MOD-ZERO-LENGTH | a reference modification while no >>REF-MOD-ZERO-LENGTH has been written and EC-BOUND-REF-MOD is checked | **test**: flag14-all (flagged once; not after the directive is written) |
| VALUE-EDITING | a numeric literal as the VALUE of a numeric-edited item | **test**: flag14-all |
| VALUE-FIG-CON-LENGTH | a figurative constant as the VALUE of an item with no length | **n/a**: no item without a length takes a VALUE here (ANY LENGTH is LINKAGE's); the option is accepted and never flags; VALUE-FIG-CON-NO-LENGTH (rule 4k's spelling) taken too |
| WRITE-END-OF-PAGE | a WRITE to a LINAGE file without END-OF-PAGE | **test**: flag14-all |
| 7.3.15.2-3 | options then ON or OFF; between clauses or statements | **test**: flag14-all (two options turned off part way); **refused**: bad/std2023-flag-14-option, -onoff; **gap**: the position rule is not checked |

### PUSH and POP (7.3.22, 7.3.20)

Each directive's state is its own stack. DEFINE (the whole table of
compilation variables), PROPAGATE and COBOL-WORDS are saved and restored
at the text manipulation stage; SOURCE by the reader (the reference
format); TURN (the checking state, `ecs_copy`), REF-MOD-ZERO-LENGTH and
FLAG-14 by the parser where the directive stands. CALL-CONVENTION,
LEAP-SECOND, LISTING and DISPLAY have no state here and are accepted.

| rule | paraphrase | disposition |
|---|---|---|
| 7.3.22.3 rule 1, 7.3.20.3 rule 1 | not EVALUATE, IF, PAGE, POP or PUSH | **refused**: bad/std2023-push-if, -pop-unknown (not a directive) |
| 7.3.22.4 rules 1, 3 | the state saved, the directive still in effect; every instance of a DEFINE | **test**: directives (K redefined under a PUSH, back after the POP) |
| 7.3.20.4 rule 1 | restored | **test**: directives (TURN: checking off and back on; SOURCE: fixed and back to free) |
| 7.3.20.4 rule 2 | a POP with nothing pushed: unsuccessful, warned of | **test**: directives (a warning on the standard error, the program compiles) |
| 7.3.20.4 rule 3, 7.3.22.4 rule 2 | ALL | **test**: directives (PUSH ALL / POP ALL around a REF-MOD-ZERO-LENGTH ON) |
| 7.3.22.3 rules 3-4, 7.3.20.3 rules 3-4 | ALL only in a compilation unit between clauses or statements; not in an exception-checking PERFORM | **gap**: not checked |
