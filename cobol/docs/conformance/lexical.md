# 8.3.3.3 Numeric literals, fixed-point and floating-point; the token scanner

Swept 2026-10-06 (docs/plans/standard-queue.md item 15), when the
scanner moved to Ragel -G2 and floating-point literals came with it.
2002: 8.3.1.2.2; 2023: 8.3.3.3.2 (fixed-point), 8.3.3.3.3
(floating-point). The alphanumeric, boolean and national literals are
swept in national-boolean.md (8.3.3.4-5); the rest of 8.3 -- words,
separators (8.3.2), the reference format's own rules -- is not swept
rule by rule here.

How: src/lex.rl is one grammar for 8.3's lexical elements, generating
lex_scan.c (checked in; gen_lex.sh). Both readers of the source take
one lexeme at a time from it -- the text-word scanner of copy.h, which
COPY and REPLACE work on (7.2.2.5: a literal with its delimiters, `(`,
`)`, `:`, `==` and a separating period, comma or semicolon are
text-words of their own, everything else runs together between
separators), and the tokenizer, which keeps the lexemes apart. Until
then the two scanned by hand, each its own copy of the rules. What is
context stays with the callers, as the ruling has it: a PICTURE after
PIC, EXEC SQL text, the boundary that lets a sign or a leading point
begin a number (the line's start, a space, `(` or `=` before it),
DECIMAL-POINT IS COMMA's swap.

## 8.3.3.3.2 Fixed-point numeric literals

| rule | paraphrase | disposition |
|---|---|---|
| 1 | at least one digit | **test**: throughout; `.` alone is a separator |
| 2 | one sign, leftmost | **test**: `-5`, `+.5`; a sign not at a boundary is an operator (`A-1`) |
| 3 | one decimal point, not rightmost | **test**: `.5`, `12.5`; `5.` is 5 and a period (the period rule, 8.3.2) |
| 4 | the value; 1 to 31 digits | **refused** past 31 (18 under -std=85): "numeric literal has more than 31 digits" |

## 8.3.3.3.3 Floating-point numeric literals (implemented 2026-10-06)

The tokenizer writes a floating-point literal as the fixed-point literal
it is worth, exactly (tokenizer.h `numlit_float_fixed`): 1.5E+3 is 1500,
1.5E-3 is 0.0015, so every reader of a numeric literal takes it as
before. Test: 2002/floatlit (GnuCOBOL 4 agrees).

| rule | paraphrase | disposition |
|---|---|---|
| 1 | two fixed-point literals joined by E, no spaces | **test**: 2002/floatlit (`1.5E+3`, `.5E2`, `123.456e-2`) |
| 2 | the significand signed or not, with a decimal point, 1 to 36 digits | **test**; `3E5` has no point and is a word (the scanner's rule); **refused** past 36 digits |
| 3 | the exponent signed or not, at most four digits, no point; its range the implementor's | **refused** past four digits; **ruling**: the range is the one that keeps the value within 31 digits (bad/std2002-fplit-range) |
| 4 | a zero significand: a zero exponent, no minus sign | **refused**: bad/std2002-fplit-zero |
| 5 | the value: the significand times ten to the exponent | **test**: 2002/floatlit (a VALUE, MOVE, COMPUTE, a condition) |
| -std=85 | floating-point literals are 2002's | **refused** naming the edition |
