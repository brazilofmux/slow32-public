# Open questions

Rulings belong in [requirements.md](requirements.md) and
[architecture.md](architecture.md). This file is what is *not* yet
ruled. An implementer who hits one should stop and ask, not guess
quietly.

## Product

None outstanding for v1. Retiring GnuCOBOL for gl022/gl023/gl030
plus the two screen programs is the definition of done.

## Dialect

**Source format selection.** Majesty is free-format. CCVS-85 is
fixed. GnuCOBOL takes `-free`/`-fixed`; majesty's Makefile passes
`-free` explicitly. Options: a flag with default fixed (the
standard's reference format); a flag with default free (the corpus);
a heuristic (`*> ` or lowercase in the first non-comment line). The
flag-with-an-explicit-Makefile is the least surprising and is what
majesty already does. Not ruled, but leaning that way.

**Oracle dialect for majesty programs.** Resolved by measurement:
the `.prn` files are GnuCOBOL 4.0-early-dev **default** dialect, not
`-std=cobol85`, because majesty's build passes no `-std`. A
recompiled oracle for a majesty program uses majesty's flags. See
[oracles.md](oracles.md).

**Compiler command name.** `cobc` collides with GnuCOBOL. cobc370
was named to avoid that. Candidates: `s32-cobc`, `cobc85`,
`slow32cobc`. Not ruled.

## Functions

**Activation semantics of `FUNCTION-ID` programs.** COBOL 2002 gives
each activation fresh `WORKING-STORAGE`; GnuCOBOL follows that. No
majesty function depends on state surviving between calls (`taskdt`
rebuilds everything from `CURRENT-DATE`), so re-initialise-on-entry
and static-with-a-note both satisfy the corpus. Stage 6 decides;
whichever it is, say so in a diagnostic if a function ever declares
`INITIAL`/`RECURSIVE` explicitly. See [functions.md](functions.md).

**Linking granularity.** One `.s32x` per `PROGRAM-ID`, each linking
the function modules its `REPOSITORY` reaches (gl030 → clinkages;
menu → taskdt → clinkages) plus `dateutil.c`. Ruled for v1; a single
image holding every program, GnuCOBOL's `MAJESTY.so` shape, is not
needed and not planned.

## Numeric

**Canonical representation in `libcob`.** Software packed-18
(cobc370-shaped, 10 bytes, sign nibble) versus scaled i64 (digits
≤ 18 always fit in 64 bits unsigned, sign separate). i64 is the
machine. Packed-18 is closer to COBOL's stored COMP-3 and to
cobc370 tests. A hybrid (i64 in expressions, COMP-3 in memory) is
the likely answer. Not ruled; Stage 3 decides with a measurement.

## I-O

**Indexed on-disk layout.** New format versus a copy of dBase
`.NDX` plus a private data file. Compatibility with dBase is not
a v1 goal, so a private format is allowed. Provenance if dBase
btree source is copied (f77-style stamp). Not ruled; Stage 5
decides.

**Relative I-O in v1.** The gate programs do not need it.
`crglentry` and friends do. Leave after v1 unless a gate program
grows a relative file.

**Line sequential and `\r`.** Strip trailing CR on read: yes, as
[framing.md](framing.md) says. Strip CR in the payload's interior:
no.

## Screen

**`UNDERLINE`.** `term.h` has normal/bold/reverse, not underline.
v1 may paint without it. Adding a term opcode is a runtime change
that nano and dBase do not need. Not ruled; Stage 8.

## Report Writer

**GnuCOBOL `.prn` vs 85 fit tests.** v1 gates on majesty's current
`.prn`. If implementing the page engine from the 85 tables changes
a blank line that GnuCOBOL emitted "wrong," do we change the
report or follow GnuCOBOL to keep retirement byte-identical?
cobc370 followed the text against GnuCOBOL. Majesty's `.prn` files
are also compared to C++ and dBase outputs. If all three already
agree, that agreement is the product, even if a table in the
standard would have spaced a line differently. If they disagree
among themselves, stop. Not fully ruled; the first mismatch
decides.

## Borrowing

**`pic_scan` as a copied file vs a rewrite.** The Ragel is 80
lines. A rewrite is cheaper than a shared header across
`~/cobc370` and `cobol/`. Prefer rewrite with the same split.
Not ruled; Stage 1.
