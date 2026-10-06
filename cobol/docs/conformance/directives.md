# Compiler directives: conditional compilation (7.3.5-7.3.8, 7.3.11, 7.3.13, 7.3.16) and CALL-CONVENTION, LEAP-SECOND, LISTING, PAGE (7.3.9, 7.3.17-19)

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
