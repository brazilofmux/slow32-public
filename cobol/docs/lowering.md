# Lowering

After `Sym[]` exists, each verb is a lowering: either a short
instruction sequence or a call into `libcob` with a descriptor the
compiler built. That is cobc370's `STRING`/`UNSTRING`/`INSPECT`
pattern, applied generally.

## Conversion matrix

The interesting "instruction selection" is not BURG. It is:

```
(src category, usage, digits, scale, signed, edited)
        ×
(dst category, usage, digits, scale, signed, edited, justified, blank-when-zero)
        →
path
```

Categories: alphabetic, alphanumeric, alphanumeric-edited, numeric
(binary / packed / display / comp-5), numeric-edited, index, group.

Every `MOVE` and every store from `COMPUTE` indexes this table. A
path is one of: inline, library, or "refuse with a message."
Unimplemented cells stay refused; that is how cobc370 grew without
silence.

COBOL 85 de-editing (numeric-edited → numeric) is a library path
and a language requirement. It is one of the things IBM ANS COBOL
could not do.

## Hot cases (inline)

Worth being a compiler:

- Alphanumeric `MOVE` of known length: word copies / `memcpy`
- Group `MOVE` of equal layout: same
- `PIC S9(n) COMP` or `COMP-5`, n ≤ 9, same scale: i32 `add`/`sub`/
  `mul`/`div`
- Same, n ≤ 18: i64. (cobc370 could not; this machine can.)
- `IF` on those integers, and alphanumeric compares of equal length
- Structured `PERFORM … TIMES/UNTIL/VARYING … END-PERFORM`,
  `IF … END-IF`, `EVALUATE` — COBOL 85 control flow lowering to an
  ordinary CFG is honest. Paragraph `PERFORM` / `PERFORM … THRU` is
  a label range, like cobc370; do not pretend it is a function.

`USAGE COMP-3` add of two identical pictures *may* inline later if
measurement says so. v1 may send all COMP-3 through the canonical
numeric library. The bench that would change that is majesty's
balance pipeline, not a synthetic AP.

## Library paths

- DISPLAY numeric ↔ COMP ↔ COMP-3, scale alignment, `ROUNDED`,
  `ON SIZE ERROR`
- edited MOVE, de-edit, `BLANK WHEN ZERO`, `JUSTIFIED`
- `STRING` / `UNSTRING` / `INSPECT` / `SEARCH` / `SEARCH ALL`
- `INITIALIZE`
- reference modification (`FOO(3:5)`) as a descriptor on a
  sending/receiving item, not a new type
- I-O (every organization)
- Report Writer group render / page advance
- SCREEN `DISPLAY`/`ACCEPT`

Descriptors are compiler-built parameter blocks in WORKING-STORAGE,
the cobc370 `COBSTR` way: the runtime works in bytes and pictures
and knows nothing about the statement that called it.

## Expressions

`COMPUTE` and arithmetic statements: a small AST (cobc370's `Node`:
symbol, literal, add/sub/mul/div/pow/neg). Scales are compile-time
attributes (`expr_shape`). Evaluation is onto the canonical numeric,
except the COMP integer hot case, which evaluates in i32/i64 and
only converts if the receiver is not the same.

Intermediate precision: COBOL 85 has rules; GnuCOBOL has unbounded
intermediates that cobc370 already diverged from on 74. Follow the
85 text. Where GnuCOBOL disagrees, the text wins and the oracle is
hand-corrected, as cobc370 did for `PERFORM … AFTER` and ODO.

## Control flow

COBOL 85 scope terminators mean the parser can build structured
statements. Lower those to branches the way any compiler would.

Paragraphs and sections still exist. They are labels. `PERFORM` of a
range is cobc370's exit-cell mechanism (or an equivalent). `GO TO
DEPENDING ON` is a branch table. `ALTER` is refused: it is not in
85.

## CALL

SLOW-32 ABI (`docs/CALLING_CONVENTION.md`): `r3`–`r10` arguments,
`r1`/`r2` return, `r29` sp, `r31` lr. `USING` items default to
`BY REFERENCE` (address of the data item). The C bridge in
`clinkages.cbl` uses the full form and v1 needs all of it:

    call 'du_lineartofielded' using by value     ltf_lineardate
                                    by reference ltf_fieldeddate
                              returning isvalid.

`BY VALUE` of a `signed-int`/`signed-short`/`unsigned-short` is the
value widened to a word in an argument register; `BY REFERENCE` is
an address; `RETURNING` takes `r1` into a `signed-int`. `SYNC` inside
the group handed by reference must reproduce the C struct layout.
`USAGE POINTER` is a word. Nested programs and `EXTERNAL` are after
v1.

Dynamic `CALL identifier` is later; majesty's `CALL` sites in the
report path are of literals (including `'CBL_GET_SCR_SIZE'`).

## COBOL-to-COBOL `CALL` — whose convention?

GnuCOBOL is a transpiler, so every convention in the corpus today is
C's: a COBOL `CALL` *is* a C call because that is all GnuCOBOL can
emit. That is an artifact, not a property of the programs. A compiler
that is COBOL all the way down defines its own COBOL-to-COBOL
convention — cobc370 uses IBM's (R1 → list of parameter addresses)
because MVS and `DYNALOAD` require it, and nothing here requires that.

**Ruling for v1:** one convention, the SLOW-32 C ABI, for every
`CALL`. `CALL 'name' USING a b` passes the addresses of `a` and `b`
in `r3`, `r4`; the callee's `LINKAGE SECTION` items are those
addresses; more than eight go to the stack exactly as the ABI says;
`GOBACK` returns. Reasons: the C seam in `clinkages.cbl` then costs
nothing extra, `dateutil.c` and any future C link with no thunk, and
the corpus's largest `USING` list on the v1 path is two items. If a
COBOL-only convention (an address list, cobc370-style) ever earns its
keep — many-argument calls, or a `CALL identifier` table — it is a
change to this section, not to the corpus.

`c_lineartofielded` and `taskdt` are COBOL subprograms after the
rewrite in [functions.md](functions.md). No function-invocation form
exists in this compiler: `name(args)` in an expression is a
diagnostic naming the rewrite.

## Two cells the corpus forces

- `PIC X(6)` → `PIC S9(3)V99 COMP-5` (usescreen: `move amount-in to
  amount`). Alphanumeric to numeric is not a standard-conforming
  `MOVE`; GnuCOBOL performs it as a numeric conversion of the text.
  Implement that cell, as GnuCOBOL does, and name it in the matrix.
- `----,---,--9.99` with a **negative** source. This is gl030's
  picture, and the one that exposed cobc370's floating-sign bug: a
  floating minus printed at the far left instead of against the
  digits when the first nonzero digit lands at or after the
  significance starter (`DIFFERENTIAL-TESTING.md`, GL035/GL036).
  gl030 never sees a negative amount, so the bug survived it there.
  Test negatives before believing the picture.
