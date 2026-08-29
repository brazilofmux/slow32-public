# Architecture

COBOL is a data-description language that happens to have verbs. The
compiler is a lowering of those verbs against a symbol table, plus a
handful of parameterized runtimes. It is not an Algol pipeline with a
COBOL lexer.

Fortran 77 copied `selfhost` HIR because Fortran is expression-shaped.
Clipper compiled control flow and left the rest to the dBase engine.
This compiler is closer to Clipper than to Fortran, with cobc370's
`Sym[]` as the IR.

## Universe

Host cross-compiler. Ragel, a C compiler, and the oracles run on the
host. The emitted program is a `.s32x` linked with `libcob` (when that
exists) and the SLOW-32 libc. Same split as cobc370 (compile on the
Mac, run on the guest) and as f77.

SLOW-32 is the only backend. Other silicon is `slow32-dbt`'s problem.

## Pipeline

```
fixed-format / free-format reader
        │
hand recursive-descent parser
        │
        ▼
   Sym[]     data items, pictures, files, screens, reports   ← the IR
   Stmt[]    Procedure Division, already scoped (END-IF etc.)
   File[]    SELECT / FD / framing
   Screen[]  SCREEN SECTION field tables
   Report[]  RD + groups
        │
        ▼
lowering, per verb:
   hot case inlined into SLOW-32 assembler
   else call libcob_* with a compiler-built descriptor
        │
        ▼
s32-ld  +  libcob.s32a  +  libc
```

The seam is cobc370's, in *shape* not in code: the back end reads
tables the front end built, and the front end emits no assembler.
See `~/cobc370/docs/THE-SEAM.md`. A second backend is not planned;
the seam exists so lowering can be tested against the tables without
the parser, and so I/O framing cannot leak into PICTURE analysis.

## Which toys earn their keep

| toy | verdict |
|---|---|
| **Ragel -G2** | Yes, narrowly. PICTURE (cobc370 already proved this), and field-input validation under a SCREEN PICTURE. Not the source lexer: Area A/B, continuation, `COPY`, `lex_picture` vs words. |
| **Recursive descent** | Yes, the whole front end. COBOL 85 scope terminators make this easier than 74's period-terminated mess. |
| **Attribute grammars** | As `PicInfo` + `expr_shape` + a conversion matrix, not a Knuth engine. Category, usage, digits, scale, signedness, justification, editedness. |
| **SSA** | No, not as the program IR. WORKING-STORAGE is globals. `REDEFINES` is aliasing. Group `MOVE` is overlapping `memcpy`. Paragraph `PERFORM` is a label range. |
| **Lowering** | Yes. After `Sym[]` exists, this *is* the compiler. |
| **BURG** | No. The "instruction selection" is conversion selection: `(src category, usage, scale) × (dst …) → path`. A decision table. |
| **Register allocation** | Only for a thin `USAGE COMP` COMPUTE slice. Packed/zoned values live at known WORKING-STORAGE offsets. |

Fortran's copied `hir_*.h` is not used here. Putting COBOL through it
would spend its life loading and storing overlapping byte ranges.

## Canonical numeric

SLOW-32 has no `AP`/`SP`/`ED`. Decimal is a library.

One canonical numeric in the middle (software packed-18, or a 64-bit
scaled integer when digits ≤ 18, which is the whole COBOL numeric
universe). Convert in at loads, convert out at stores, compute in the
middle. cobc370 did the same thing with packed work areas `WK0..WK5`
because the hardware *was* packed; here the work area is the library's
problem and the hot cases skip it.

`USAGE COMP` with digits ≤ 9 is i32. Digits ≤ 18 is i64. `COMP-3` and
DISPLAY numeric go through the canonical. `COMP-5` / `signed-int` /
`signed-short` are the SLOW-32 C ABI, for `CALL` of existing C.

Edited MOVE and COBOL 85 de-editing are library paths. They are why
cobc370 existed on MVS (IBM ANS COBOL could not de-edit). They stay
important.

## Three state machines, not one interpreter

| machine | extra state | compiled from |
|---|---|---|
| **PICTURE** | none (pure function) | character-string → category, size, edit descriptor |
| **Report Writer** | LINE-COUNTER, PAGE-COUNTER, fit, control-break level | RD + groups; see [report-writer.md](report-writer.md) |
| **SCREEN SECTION** | current field, insert vs overwrite, AUTO | field table; see [screen.md](screen.md) |

Same compiling shape: LINE / COLUMN / PIC / SOURCE-FROM-TO-USING
become a table of slots, then a small engine. Do not smash them
together. Report Writer moves paper. SCREEN SECTION moves focus.
Both paint through existing services (stdio / print files; `term.h`).

## Runtime (`libcob`, later)

Parameterized routines, cobc370-style (`COBSTR`, `COBUNS`, INSPECT's
operation table):

- decimal arithmetic, edit, de-edit, `ROUNDED`, `ON SIZE ERROR`
- `STRING` / `UNSTRING` / `INSPECT` / `SEARCH`
- `INITIALIZE`, reference modification as a descriptor
- sequential / line sequential / RDW-V
- relative, indexed
- Report Writer page engine
- SCREEN ACCEPT loop

Hot cases inlined by the compiler are listed in [lowering.md](lowering.md).

## Layout later

When code exists, the directory should look like f77 / cobc370, not
like a copy of either:

    cobol/
      README.md
      docs/           these files
      src/            host compiler (C, plus picture.rl)
      libcob/         guest runtime, compiled by the SLOW-32 C toolchain
      tests/          programs + expected; majesty reports as a later gate

`src/` does not include `selfhost` headers.
