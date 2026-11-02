# The SLOW BASIC language

SLOW BASIC is the BASIC of QBasic 1.1 (QuickBASIC 4.5's interpreter),
with GW-BASIC's line-numbered habits and a few extensions of its own.
This file says which, rule by rule.  It is what a compiler for the
language (planned: `s32-sbc`) is to agree with, and what the
interpreter is held to.

## Authority

Ruled 2026-11-01:

1. **QBasic 1.1 is the authority** wherever it has the feature.  Its
   behaviour is not taken from manuals: each rule below was probed on
   QBasic itself, run headless under dos-monster (`~/x86`), and the
   suites below compare whole programs against it.
2. **GW-BASIC** decides line-numbered behaviour QBasic lacks.  (No
   runnable GW-BASIC oracle yet; its MIT-licensed source is in
   `~/refs/basic/GW-BASIC`.)
3. **Every deviation is written down here** -- under *Extensions* when
   SLOW BASIC has more, under *Differences* when it behaves otherwise.

## Programs

Line numbers are optional and are labels; so are `name:` labels.
`GOTO`, `GOSUB`, `RETURN`, `RESUME`, `ON ... GOTO/GOSUB` and `RESTORE`
take either.  `IF c THEN 60` and `ELSE 60` are jumps.  A single-line
`IF` takes the rest of the line, colons and all.  `GO TO` and `GO SUB`
may be spaced.  `STOP` and `SYSTEM` end a program run from a file.

Block statements (`IF ... END IF`, `DO ... LOOP`, `WHILE ... WEND`,
`SELECT CASE`, `SUB`, `FUNCTION`) nest as in QuickBASIC.  A `GOTO` may
go anywhere in its program unit -- into a block, out of a loop and
back -- because every unit is run as flat code with explicit jumps
(`src/lower.c`).

### FOR and NEXT

`FOR` and `NEXT` are statements, matched as they run, not a block:

- `NEXT` steps the most recent running loop of its variable (or the
  most recent loop if it names none), dropping loops left above it by a
  `GOTO`.  `NEXT J, I` is `NEXT J` then `NEXT I`.  So a `NEXT` may stand
  inside an `IF` (`IF c THEN PRINT x;: NEXT Z`), and two `FOR`s may share
  one `NEXT`.
- A `FOR` replaces a running loop only if it is its own (the loop
  entered again by `GOTO`); an inner `FOR` of the same variable leaves
  the outer loop running.
- The limit and step are evaluated once; the variable is the counter,
  so the body may change it.
- A loop that runs no times skips past its `NEXT` in the text.  A `FOR`
  with no `NEXT` of its variable anywhere after it is `FOR without
  NEXT` when the program loads; a `NEXT` with no `FOR` before it,
  `NEXT without FOR`.
- A `NEXT` reached with no loop running (a `GOTO` into the body) steps
  by 0 towards a limit of 0, as QBasic's never-initialized loop does.

## Types

| Suffix | Type | Size | `AS` | `DEF`*type* |
|---|---|---|---|---|
| `%` | INTEGER | 16 bits | `INTEGER` | `DEFINT` |
| `&` | LONG | 32 bits | `LONG` | `DEFLNG` |
| `!` or none | SINGLE | IEEE single | `SINGLE` | `DEFSNG` |
| `#` | DOUBLE | IEEE double | `DOUBLE` | `DEFDBL` |
| `$` | STRING | up to 4,096 | `STRING [* n]` | `DEFSTR` |

Names without a suffix are SINGLE unless a `DEF`*type* says otherwise.
`A` and `A!` are one variable.  TYPE records hold fields of these
types.

### Arithmetic

- Integer arithmetic keeps the wider type of its operands (LONG if
  either is) and is an `Overflow` past that type's range: `32767 + 1`
  and `200 * 200` are Overflows, `32767& + 1` is 32768.
- `/` is always a float; `\` and `MOD` work on integers, rounded half to
  even, and truncate.  `^` is always a float (`2 ^ 15` is 32768).  `0 ^`
  a negative power, and a negative number `^` a fractional one, are
  `Illegal function call`.
- A float result is a SINGLE unless a DOUBLE is among the operands.  It
  is computed in double precision and rounded to a single only when it
  is stored -- into a variable, an array element, a field, a `FOR`
  variable, by `READ` or `INPUT` -- or printed.  So `16777216! + 1 -
  16777216!` is 1, and `Y# = X / 3` keeps every digit.  A single out of
  range is an `Overflow` when it becomes one (`Y# = 1E+38 * 10` is
  fine; `PRINT 1E+38 * 10` is not).
- Conversion to an integer rounds half to even (`CINT(2.5)` is 2,
  `X% = 32767.5` is an Overflow).  The logical operators work on such
  integers: `3.1 AND -1.5` is `3 AND -2`, 2.
- Comparisons give -1 or 0 and chain left to right.

Precedence, tightest first: `^` (left to right), negation, `*` `/`,
`\`, `MOD`, `+` `-`, the comparisons, `NOT`, `AND`, `OR`, `XOR`, `EQV`,
`IMP`.  So `-2^2` is -4, `2^3^2` is 64, `NOT 1=2` is -1.

### Literals

An integer literal is an INTEGER up to 32767, a LONG beyond (or with
`&`).  `&H`, `&O` and `&B` literals up to `&HFFFF` are 16-bit INTEGERs
(`&HFFFF` is -1), longer ones LONGs.  A literal with a point or an
exponent is a SINGLE unless it has `#`, a `D` exponent, or more than 7
mantissa digits counting leading zeros (`0.0000001` and `3.14159265`
are DOUBLEs, `1234567.` a SINGLE).  A literal out of its type's range is
an `Overflow` when the program loads.

## Output

`PRINT` writes numbers QBasic's way: a space or the minus sign before,
a space after; up to 7 significant digits for an INTEGER, LONG or
SINGLE and 16 for a DOUBLE; fixed point while the digits fit, else
`1.234568E+07` or `1.234567890123457D+16`; no `0` before the point
(`.5`); never `-0`.  `STR$` is the same without the trailing space.
`WRITE` writes numbers without either space.

The screen is 80 columns (`WIDTH` changes it):

- `,` moves to the next 14-column zone; past the last zone that fits,
  to a new line.  In a file the zones go on without wrapping.
- An item that does not fit in what is left of the line starts the
  next one; a string longer than a line wraps by character.  Filling
  column 80 exactly leaves the wrap pending, so no blank line follows.
- `TAB(n)` to a column already passed goes to the next line; `n` past
  the width wraps around it.  A `PRINT` ending in `TAB()` or `SPC()`
  ends no line.
- `CHR$(13)` and `CHR$(10)` start a new line, `CHR$(9)` goes to the next
  stop of 8, `CHR$(7)` beeps (it reaches a terminal and nothing else).

`LPRINT` writes to standard error, with its own column (`LPOS`).

## Input

`INPUT [;] ["prompt" {;|,}] v, ...`: a `;` after the prompt adds `? `,
a `,` does not; `INPUT;` keeps the cursor on the line.  Fields are
separated by commas; a string may be quoted.  A field that is not a
number where one is wanted, or too few or too many fields, is `Redo from
start` and the prompt again.  When standard input is not a terminal the
line read is echoed, so a run with its keys piped in reads like the
screen.

`DATA` items are raw text: unquoted strings, empty items and `+` signs
are fine, and `READ` decides by its variable's type.  A quoted item
read into a number is a `Syntax error`.

## Random numbers

`RND` is QuickBASIC's generator: the same numbers as QBasic for the
same program.  `RND(0)` repeats the last number, `RND(-n)` reseeds from
the bits of `n`, `RANDOMIZE n` reseeds from `n`, and `RANDOMIZE` alone
asks `Random-number seed (-32768 to 32767)?`.

## Arrays

An array used before any `DIM` has subscripts up to 10 in each of its
dimensions.  A `DIM` with constant bounds makes its array before the
program runs (QuickBASIC's static arrays), so a reference may come
before it; `OPTION BASE` counts in the order of the text.

## Errors

`ERR` is QuickBASIC's error number (11 Division by zero, 53 File not
found, ...) and the messages are its wording.  `ERL` is the last line
number before the statement that failed, 0 in a program without line
numbers.  `ON ERROR GOTO` traps in the main program; an error in a
procedure comes out of its `CALL`.

`DEF FN` functions are global; `FNM` with no parentheses calls a
`DEF FNM` with no parameters, and an `FN` name with no `DEF FN` is
`Function not defined`.

## Extensions

Beyond QBasic, kept on purpose:

- Functions: `ATN2`, `ASIN`, `ACOS`, `SINH`, `COSH`, `TANH`, `ASINH`,
  `ACOSH`, `ATANH`, `LOG10`, `LOG2`, `ROUND`, `MIN`, `MAX`, `FLOOR`,
  `CEIL`, `INSTRREV`, `TRIM$`, `REPLACE$`, `REVERSE$`, `BIN$`, `JOIN$`,
  `FILEEXISTS`, `DIR$`, `CURDIR$`.  (In QBasic these names are arrays.)
- Statements: `SPLIT s$, delim$, array$()`, `TRACE ON/OFF` (statement
  line numbers), `OPTION EXPLICIT`, `$INCLUDE`.
- `&` between expressions concatenates them as strings.
- Graphics on the tube: `SCREEN`, `PSET`, `PRESET`, `LINE`, `CIRCLE`,
  `PAINT`, `PALETTE`, `POINT` (see README.md).

## Differences from QBasic

Known, and kept until there is a reason:

- **Intermediate precision.** QBasic keeps intermediates in the 8087's
  80-bit format; SLOW BASIC in double.  A single's seventh digit can
  differ after a long computation (NBS P138, the Lunar game).
- **Checks QBasic makes when it loads a program** that SLOW BASIC does
  not: a `DIM` after the array was used, a `DEF FN` or label defined
  twice, an exponent too long to read, some syntax QBasic refuses.
  Where both refuse a program they do so in their own words.
- **`PRINT USING`** places a `+` sign and reuses its format for several
  values differently.
- **`STRING * n`** is accepted and the length ignored: a fixed-length
  string is not padded or cut.
- `SOUND`, `PLAY`, `CHAIN`, `KEY`, `FIELD`, `WAIT`, `OUT`, `PCOPY`,
  `COMMON`, `ENVIRON`, `VIEW PRINT`, `ON TIMER` and `ON KEY` parse and
  do nothing; `SHELL` is not supported; `PEEK`/`POKE` address a private
  64 KB array.

## How it is checked

- `tests/run-tests.sh` -- the 45 unit programs (`--update` rewrites the
  expected outputs after a checked change).
- `tests/nbs.py` -- programs run on SLOW BASIC and on QBasic 1.1, output
  compared byte for byte (QBasic's cached in `~/.cache/sbasic-oracle`).
  On 2026-11-01:

  | Corpus (`~/refs/basic/nbs-ecma55-test`) | Same output | Same error | Both refuse | Differ |
  |---|---|---|---|---|
  | `NBS/`, 208 Minimal BASIC tests | 119 | 58 | 7 | 1 (P138) |
  | `basic_computer_games/`, 99 | 86 | -- | -- | 1 (lunar) |
  | `fast/`, 64 feature tests | 49 | 6 | 8 | 0 |

  The rest are programs QBasic stops on with a load-time check SLOW
  BASIC lacks, or does not finish.
