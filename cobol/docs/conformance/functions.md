# Intrinsic functions: 15

Swept 2026-09-30, in two parts: the clause's general rules (15.1-15.3)
and every implemented function's argument rules, then the returned
values and the ALL subscript. X3.23a-1989 (the
Intrinsic Function module, FIPS 21-3): 2.2-2.3 and the definitions.
2002: 15. 2023: 15.

## Part 2: returned values and ALL

**Returned values.** `2002/fnreturn` calls every implemented function
over a grid of correct arguments, 169 calls, generated, each line
labelled with its call. The oracle agrees on all but four, recorded in
`.oracle-expected` and docs/oracles.md:
- EXP(20) and EXP10(10.5) differ past the 15 significant digits
  computed here in double. For native arithmetic the value is an
  implementor-defined approximation. Past 15 digits the value is zeros,
  not the double's binary noise, so every engine gives the same answer.
  EXP(50) is left out: the guest libm the interpreters run is several
  ulps off there, and its 15th digit differs from the DBT's.
- ANNUITY(0.1, 1) is exactly 1.1 here.
- NUMVAL-F("1.5E3") has an unsigned exponent, which 15.69.3 does not
  allow.

Found and fixed on the way:
- **NUMVAL ignored a trailing CR or DB.** Its second format allows them
  (15.67.3), and `NUMVAL("7 CR")` was 7. A conforming string now takes
  its value from the same scanner TEST-NUMVAL uses.
- **PI was 3.141592654**, rounded to nine decimals. It is now the 31
  decimals the rule's expression gives (15.73.3 rule 1).
- **FUNCTION E, a 2002 function (15.22 there, 15.27 in 2023), was
  missing.** It is added, to 31 decimals likewise. With it, every 2002
  function is implemented except LOCALE-COMPARE, LOCALE-DATE,
  LOCALE-TIME, LOCALE-TIME-FROM-SECONDS and STANDARD-COMPARE, which are
  refused, naming what they need.
- **MAX and MIN over strings padded the result to the widest
  argument.** Its size is the selected argument's (15.59.4 rule 3).
- **TEST-NUMVAL-C("1,,2") said 2.** The character in error is the
  second comma, 3: the first comma is valid if a digit follows it, and
  the one after is where the string stops being valid.

**ALL** (15.3; X3.23a-1989 2.2). ALL is now a subscript in any position:
- `t(ALL, ALL)`, `t(2, ALL)`, `t(ALL, i + 1)`, with qualifiers.
- The rightmost ALL varies fastest.
- The dimension with OCCURS DEPENDING ON runs to the DEPENDING ON
  item's current value.
- It works for MAX, MIN, ORD-MAX and ORD-MIN over strings too.

Before, only `name(ALL)` on a one-dimension table was taken. Its
elements were counted to the OCCURS maximum whatever the DEPENDING ON
item held, and MAX and MIN over strings saw the first element only.
**test**: 2002/fnallsub. There is no oracle: GnuCOBOL 4.0-early-dev refuses
ALL there.

## What changed

- **The functions computed in double were held to nine integer
  digits.** SQRT, LOG, LOG10, EXP, EXP10, the trigonometric functions,
  MEAN, MEDIAN, VARIANCE, STANDARD-DEVIATION, ANNUITY, PRESENT-VALUE and
  RANDOM returned a sign and 18 digits at scale 9, clamped to
  ±999999999.999999999. So `MEAN(2000000000 3000000000)` was 999999999,
  and `SQRT` of a 20-digit number came back clamped. They now return
  their value as wide as it is:
  - MEAN, MEDIAN and VARIANCE are exact decimal arithmetic, as MIDRANGE
    already was.
  - The rest go to 15 significant digits, what a double holds, so
    `EXP(LOG(5))` is 5 and not 4.999….
  - FACTORIAL is exact to 33!, the largest within 38 digits; it stopped
    the run past 19!.
  - A statement using any of these computes on the wide stack.
  **test**: 2002/fnvalues, oracle agreeing. GnuCOBOL gets `MEAN(2000000000
  3000000000)` wrong itself: it prints 500000000, having cut the leading
  digit.
- **EC-ARGUMENT-FUNCTION was never raised.** The table knew the name, but
  no function set it. MOD and REM by zero, and FACTORIAL out of range,
  stopped the run with a fatal message. The functions computed in double
  returned NaN or saturated. Now every function notes an argument, or a
  returned value, outside its rules. With the condition checked it is
  raised after the function; unchecked, the result is the implementor's
  (15.3), and here it is 0, as GnuCOBOL's is. **test**: 2002/fnargbad (the
  unchecked results, oracle agreeing); the exception-sites gate's
  `argfn.txt`, 39 sites, one program each, a correct argument beside each
  kind of incorrect one ("not raised").
- **Argument classes were not checked.** `COMPUTE r = FUNCTION CHAR(65)`,
  `FUNCTION SQRT(x)` of an alphanumeric item, `FUNCTION MOD(2.5, 2)`,
  `FUNCTION UPPER-CASE` of a numeric item, `FUNCTION MAX` of a numeric
  and an alphanumeric item: all were accepted. They are refused now, as
  below. GnuCOBOL refuses the first two and accepts the rest.
- **An integer argument may be an arithmetic expression** (15.3 rule 6;
  X3.23a-1989 2.2 (4)). INTEGER-OF-DATE, DATE-OF-INTEGER, DAY-OF-INTEGER
  and INTEGER-OF-DAY took only an item or a literal, so the common
  `FUNCTION DATE-OF-INTEGER(FUNCTION INTEGER-OF-DATE(d) + 30)` was
  refused. **test**: 2002/fnvalues.

## 15.2 Types of functions

| rule | paraphrase | disposition |
|---|---|---|
| alphanumeric, national, boolean functions | used where a sending item of that class may be | **refused** in arithmetic: bad/fn-alnum-in-arith ("FUNCTION CHAR is an alphanumeric function, not numeric") |
| numeric and integer functions | numeric, signed; an integer function has no decimals | as the table's scale, or a calendar or length function |
| 1989: a numeric function only in an arithmetic expression | X3.23a-1989 2.3 (2) | **ruling**: accepted anywhere a numeric sending item may be, as 2002 allows; `MOVE FUNCTION NUMVAL(x) TO y` is everywhere in real code |
| 1989: a numeric function not where an integer is required | X3.23a-1989 2.3 (2) | **refused**: bad/fn-numeric-fn-as-integer (`CHAR(FUNCTION SQRT(4))`); an integer function (ORD, INTEGER, MOD, ...) is accepted |

## 15.3 Arguments

| rule | paraphrase | disposition |
|---|---|---|
| 1-2 alphabetic, alphanumeric | an item of class alphabetic or alphanumeric, or a literal | **refused**: bad/fn-alnum-arg (UPPER-CASE of a numeric item), bad/std2002-fn-numval-numeric; ORD, REVERSE, NUMVAL, NUMVAL-C, NUMVAL-F and the TEST-NUMVALs likewise |
| 6 integer | an integer item, or an expression that always gives an integer | **refused** for an item or literal that is not one: bad/fn-integer-arg, bad/fn-integer-literal. An expression is checked at run time: a fraction is an incorrect argument. MOD, FACTORIAL, CHAR, ANNUITY's second, RANDOM, the calendar functions, YEAR-TO-YYYY, DATE-TO-YYYYMMDD, DAY-TO-YYYYDDD and the TEST-DATE/TEST-DAY functions |
| 10 numeric | an arithmetic expression or a numeric item | **refused**: bad/fn-numeric-arg |
| MAX, MIN, ORD-MAX, ORD-MIN | one class throughout, alphabetic mixing with alphanumeric; not boolean (15.59.3, 15.63.3, 15.71.3, 15.72.3) | **refused**: bad/fn-max-mixed |
| incorrect values | EC-ARGUMENT-FUNCTION; unchecked, the implementor's result | as above. 1989 left the value undefined and had no exception condition: under `-std=85` the result is the same 0, with no condition to check |
| ALL subscript | every element as an argument, rightmost ALL varying fastest; ODO's current range | **test**: free/fnall (one dimension), 2002/fnallsub (the general form; part 2 below) |

## The value rules, function by function

Each rule below sets EC-ARGUMENT-FUNCTION, and each is a site in
`tests/ecsites/argfn.txt`.

| function | rule | |
|---|---|---|
| ACOS, ASIN (15.8, 15.10) | from -1 to +1 | site |
| ANNUITY (15.9) | argument-1 ≥ 0; argument-2 a positive integer | site |
| CHAR (15.15) | 1 to the 256 positions of the sequence; unchecked, a space | site |
| DATE-OF-INTEGER, DAY-OF-INTEGER (15.22, 15.24) | an integer date, 1 to 3067671 | site |
| DATE-TO-YYYYMMDD, DAY-TO-YYYYDDD, YEAR-TO-YYYY (15.23, 15.25, 15.100) | argument-1 in range; the window's last year from 1700 to 9999 | site |
| EXP, EXP10 (15.34, 15.35) | a value that fits 38 digits | site (`EXP(300)`) |
| FACTORIAL (15.36) | an integer ≥ 0; 33! at most, the largest within 38 digits | site |
| INTEGER-OF-DATE, INTEGER-OF-DAY (15.46, 15.47) | a valid date, the year from 1601 | site |
| LOG, LOG10 (15.55, 15.56) | greater than zero | site |
| MOD (15.64) | integers; argument-2 not zero | site |
| NUMVAL-C, TEST-NUMVAL-C argument-2 ANYCASE (15.68, 15.94; 2014) | the currency string matched in either case | **test**: 2002/numvalanycase (implemented 2026-10-06, standard-queue item 14; no oracle: GnuCOBOL 4 has no ANYCASE); the LOCALE phrase stays with locale support (item 45) |
| NUMVAL, NUMVAL-C, NUMVAL-F (15.67-15.69) | the formats | site. Unchecked, the value is still the digits read up to the first character out of place, as before this sweep; the rule leaves it to the implementor, and real programs may lean on it |
| PRESENT-VALUE (15.74) | argument-1 greater than -1 | site |
| RANDOM (15.75) | a seed of zero or a positive integer | site |
| REM (15.77) | argument-2 not zero | site |
| SQRT (15.84) | zero or more | site |

## The 2014 international date and time functions (2023 15.3.1-15.3.3, 15.17, 15.38-15.41, 15.48, 15.79, 15.80, 15.92)

Implemented 2026-10-07 (standard-queue item 27), under -std=2014. Tests
2014/dtformat (GnuCOBOL 4 agrees, except that it writes the decimal
separator into the data of a basic fractional time format: docs/
oracles.md), 2014/dtformat2 (no oracle: national formats, which GnuCOBOL
refuses; DECIMAL-POINT IS COMMA; the current time's fraction and
SECONDS-PAST-MIDNIGHT, which GnuCOBOL reads from the real clock;
EC-ARGUMENT-FUNCTION); the clock pinned by COB_CURRENT_DATE, the offset
then zero.

How: libcob/dtfmt.h, included by the compiler and the runtime, parses a
format into its kind (date, time, combined), basic or extended, the
date's form (calendar, ordinal, week), the fraction digits, local / UTC
/ offset, and the data's length. The compiler checks the format literal
(argument-1 is a literal in every one of these functions) and sizes the
result by it, national when the literal is; the runtime renders
(`dt_render`) from an integer date, seconds past midnight at scale 9 and
an offset in minutes, and scans (`dt_scan`) data by the format,
returning the 1-based position of the first character at which an error
can be seen. The clock is `cob_clock`'s: the libc `localtime`, which on
SLOW-32 is the MMIO GETTZ service, so the local offset is the host's.

| rule | paraphrase | disposition |
|---|---|---|
| 15.3.1.2-7 the six date formats | YYYYMMDD, YYYY-MM-DD; YYYYDDD, YYYY-DDD; YYYYWwwD, YYYY-Www-D; years 1601-9999, the week rules (week 1 holds January 4; 53 weeks when January 4 is a Sunday, or a Saturday in a leap year) | **test**: dtformat (2027-01-01 is 2026-W53-5, 2024-12-30 is 2025-W01-1; 2025-W53 refused at its second digit) |
| 15.3.3 the time formats | hhmmss, hh:mm:ss, with a fraction (the separator in the extended data only; a period, or a comma under DECIMAL-POINT IS COMMA); local, Z, +hhmm / +hh:mm; a zero sign with zero subfields in sending data | **test**: dtformat, dtformat2 (the comma); **ruling**: the fraction may have any number of digits (the text asks for at least nine), the ninth and after rendered as zeros |
| 15.3.3.7 combined formats | date T time, both basic or both extended | **test**; **refused**: bad/std2014-dtfmt-mixed |
| 15.17 COMBINED-DATETIME | argument-1 + argument-2 / 100000 | **test**: dtformat (155508.452965) |
| 15.38 FORMATTED-CURRENT-DATE | the current date and time by a combined format; the time's accuracy the implementor's | **test**: dtformat, dtformat2; **ruling**: to the hundredth of a second, as CURRENT-DATE |
| 15.39-15.41 FORMATTED-DATE, -DATETIME, -TIME | a date, a date and a time, a time by a format; the offset argument at most 1439 in magnitude, only with a UTC or offset format, 0 when omitted; UTC: the time adjusted by the offset; an offset format: both written as given | **test**: dtformat; **ruling**: under a UTC format the date rolls with the adjusted time (a combined value is one instant); **refused**: bad/std2014-dtfmt-kind, -time-kind, -offset, -not-literal, -bad-format |
| 15.48, 15.79 INTEGER-OF-FORMATTED-DATE, SECONDS-FROM-FORMATTED-TIME | the date, or H * 3600 + M * 60 + S, from data in the format (a combined format's other part validated, not used) | **test**: dtformat; data out of the format is EC-ARGUMENT-FUNCTION and 0 (dtformat2); **refused**: bad/std2014-dtfmt-type (the data of the format's type) |
| 15.80 SECONDS-PAST-MIDNIGHT | the local time of day in seconds; the precision the implementor's | **test**: dtformat2; **ruling**: to the hundredth; never 86,400 or more (LEAP-SECOND is always off here, directives.md) |
| 15.92 TEST-FORMATTED-DATETIME | 0, or the position of the first error | **test**: dtformat (the text's 20051314 -> 6 and 15990316 -> 2; a bad separator; February 29; a bad offset) |
| 15.38.3 rule 1 and siblings | argument-1 a literal, alphanumeric or national; the result of its type | **test**: dtformat2 (national); **refused**: bad/std2014-dtfmt-not-literal |
| 15.40.3 rule 6, 15.41.3 rule 5 | no offset argument with a local format | **refused**: bad/std2014-dtfmt-offset |
| edition | 2014's | **refused** under -std=2002: bad/std2002-formatted-date, -combined-datetime |

## TRIM (2014; 2023 15.96), taken as BP-E27

COBOL 2014's, beyond both editions this compiler implements; IBM, Micro
Focus and GnuCOBOL all have it, and the X-COBOL survey met six programs
that use it, so it is taken with a warning under `-warn-extensions`
(ISSUES-120); the 2014 date and time functions came 2026-10-07 (above), the rest of 2014's
still refused naming the edition.

| rule | paraphrase | disposition |
|---|---|---|
| 15.96.3 rule 1 | argument-1 alphabetic, alphanumeric or national | **test**: 2002/trim, 2002/trimchars (national); **refused**: bad/std2002-trim-numeric. A literal is taken too |
| 15.96.3 rules 2-3 | argument-2, one character of argument-1's class; a space by default | **test**: 2002/trimchars; **refused**: bad/std2002-trim-chars. Literals only: an item as argument-2 is refused |
| 15.96.4 rules 1-3 | LEADING, TRAILING, or both | **test**: 2002/trim (identical to GnuCOBOL) |
| 15.96.4 rule 4 | nothing left: a result of length zero | **test**: 2002/trim, 2002/trimchars |
| 15.96.4 rule 5 | several argument-2s, each completely, in order | **test**: 2002/trimchars (`"*" "-"` and `"-" "*"` differ) |
