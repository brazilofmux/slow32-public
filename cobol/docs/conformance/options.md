# 11.9 OPTIONS paragraph: ARITHMETIC, DEFAULT ROUNDED, ENTRY-CONVENTION, FLOAT-BINARY, FLOAT-DECIMAL, INITIALIZE, INTERMEDIATE ROUNDING

Swept 2026-10-06 (docs/plans/standard-queue.md item 16). 2002: 11.7
(ARITHMETIC and ENTRY-CONVENTION only); 2014 added DEFAULT ROUNDED,
FLOAT-BINARY, FLOAT-DECIMAL and INTERMEDIATE ROUNDING; 2023 INITIALIZE
and the removal of ARITHMETIC IS STANDARD. Test: 2002/optionspara (no
oracle: GnuCOBOL 4 takes the paragraph but not DEFAULT ROUNDED).

| rule | paraphrase | disposition |
|---|---|---|
| 11.9.3 SR 1 | a terminating period when any clause is written | **refused**: bad/std2002-options-period |
| 11.9.4 GR 1 | the clauses hold for the contained source elements unless overridden | **test**: 2002/optionspara (inner inherits NEAREST-EVEN, inner2 says TRUNCATION; esql.h's UnitSave carries the default) |
| 11.9.5 ARITHMETIC GR 1 | NATIVE: the implementor's techniques for expressions and functions, native arithmetic (8.8.1.3) for the statements | **test**: 2002/optionspara -- what this compiler does (docs/wide.md: the narrow and wide stacks) |
| 11.9.5 GR 2-3 | STANDARD-BINARY (obsolete), STANDARD-DECIMAL: the intermediates of 8.8.1.4-5 | **refused** by name: not those intermediates |
| 11.9.5 GR 4 | NATIVE implied | **test**: every program |
| 2002 ARITHMETIC IS STANDARD | obsolete in 2014, removed in 2023 | **refused**: bad/std2002-options-standard, naming NATIVE |
| 11.9.6 DEFAULT ROUNDED GR 1-2 | the mode of a ROUNDED without MODE; NEAREST-AWAY-FROM-ZERO implied | **test**: 2002/optionspara (NEAREST-EVEN: 2.5 to 2, 3.5 to 4; a MODE phrase still its own; TRUNCATION) -- 2014's, marked BP-E29 as ROUNDED MODE is |
| 11.9.7 ENTRY-CONVENTION SR 1 | only in a function, a prototype, a class or an outermost program | **refused**: bad/std2002-options-entry-nested |
| 11.9.7 GR 2-4 | COBOL: the names as 8.3.2.2 maps them, the rest the implementor's; another name the implementor's; COBOL implied | **test**: `ENTRY-CONVENTION IS COBOL`; **ruling**: COBOL is the one convention here, as >>CALL-CONVENTION has it (directives.md): another name is refused |
| 11.9.8-9 FLOAT-BINARY, FLOAT-DECIMAL | the endianness and encoding implied for the standard floating-point usages | **refused** by name: those usages are standard-queue item 20 |
| 11.9.10 INITIALIZE | 2023 | **refused** by name: standard-queue item 30 |
| 11.9.11 INTERMEDIATE ROUNDING | 2014 | **refused** by name: standard-queue item 22 |
