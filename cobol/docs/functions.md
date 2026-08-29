# User-defined functions and the C bridge

**This module is missing from X3.23-1985 and is load-bearing for v1.**
It was found by reading the gate programs transitively rather than by
name: `gl030` and `menu` both stand on it.

## What majesty actually does

The C date library is not reached by `CALL` from the report programs.
It is reached through **COBOL user-defined functions** — `FUNCTION-ID`
programs, a COBOL 2002 facility that GnuCOBOL implements — which are
declared in `REPOSITORY` and invoked inside expressions:

```cobol
repository.
    function c_lineartofielded
    function all intrinsic.
...
    move c_lineartofielded(ltf_lineardate) to result
```

`c_lineartofielded` is defined in `~/majesty/src/cobol/clinkages.cbl`:

```cobol
function-id. c_lineartofielded.
...
linkage section.
01  ltf_lineardate       usage signed-int.
01  result.
    05  ltf_fieldeddate.
        10 year       sync usage   signed-short.
        10 month      sync usage unsigned-short.
        ...
    05  ltf_bool           pic x.
        88  is_valid       value 'Y'.
procedure division using ltf_lineardate returning result.
    call 'du_lineartofielded' using by value ltf_lineardate
                                    by reference ltf_fieldeddate
                              returning isvalid.
```

and `du_lineartofielded` is C, in `~/majesty/src/c/dateutil.c`, built
into `libmajesty_c.a`. So the chain for the journal is

    gl030  →  FUNCTION c_lineartofielded (COBOL, clinkages.cbl)
           →  CALL 'du_lineartofielded' (C, src/c/dateutil.c)

and for the menu it is

    menu   →  FUNCTION taskdt() (COBOL, taskdt.cbl, zero arguments)
           →  FUNCTION c_fieldedtolinear / c_lineartofielded
           →  CALL 'du_fieldedtolinear' / 'du_lineartofielded'

Earlier drafts of these specs said "`CALL` of `c_lineartofielded`".
That is wrong on both counts: it is a `FUNCTION`, and the `CALL`
underneath it targets a `du_*` name.

## What the compiler must therefore implement for v1

**Function programs.** `FUNCTION-ID` / `END FUNCTION`; a `LINKAGE
SECTION`; `PROCEDURE DIVISION [USING …] RETURNING item`; `GOBACK`.
Zero-argument functions exist (`taskdt`). The returned item may be a
**group** (`result` above), which the caller then `MOVE`s to a
same-shaped group.

**Invocation.** `name(args)` and `name()` inside any expression or
`MOVE` source, resolved through the program's `REPOSITORY`. A name not
in the repository and not an intrinsic is an error, not a data-name.

**Repository.** `FUNCTION name` entries beside `FUNCTION ALL
INTRINSIC`. Several function programs share one source file
(`clinkages.cbl` holds four); a file may hold many `FUNCTION-ID`s and
`PROGRAM-ID`s, each closed by `END FUNCTION` / `END PROGRAM`.

**`CALL` with the C ABI, in full:** `USING BY VALUE item` (scalar in an
argument register), `USING BY REFERENCE item` (address), and
`RETURNING item` (the C return value into a `signed-int`). The
SLOW-32 ABI is `r3`–`r10` arguments, `r1` return; `signed-short` and
`unsigned-short` `BY VALUE` are widened to a word, which is what
`du_isvaliddate(int, int, int)` expects. `SYNC` inside the `FIELDEDDATE`
group must match the C struct layout `dateutil.h` declares.

**The C library on SLOW-32.** `dateutil.c` is portable C: `stdio.h`,
`stdbool.h`, `limits.h`, `memory.h`, one `memset`. All four headers
exist in `~/slow-32/runtime/include/`. Compile it with the SLOW-32
clang into a `libmajesty_c.s32a` (or just the object) and link it into
every program that reaches a `du_*` name. `rs.c`, `csvgen.c`,
`csvparser.c` and `crc.c` are not on the v1 path and are not compiled
until a program asks.

## Activation semantics — an open question, and a real one

COBOL 2002 makes functions recursive and gives each activation its own
initial `WORKING-STORAGE`. GnuCOBOL follows that. cobc370 never faced
it. `taskdt` keeps `option`, `todays-date` and a `time-stamp` in
`WORKING-STORAGE` and relies on nothing surviving between calls, so
"fresh per activation" is safe for v1. Whether to *implement* it as
fresh (re-initialise on entry) or as static-with-a-note is Stage 6's
decision; either satisfies the corpus, and the difference is only
observable by a function that expects state to persist, which none
does. Listed in [open-questions.md](open-questions.md).

## Where it sits in the dialect

An implementor module of the same rank as SCREEN SECTION and LINE
SEQUENTIAL: real COBOL (2002, not 1985), documented, tested, and named
in diagnostics as an extension. It goes in the dialect because the
product test cannot be met without it, not because it is pretty.

## Linking shape

One `.s32x` per program, as cobc370 makes one load module per program.
Each links the function modules its `REPOSITORY` names — `gl030` links
`clinkages`; `menu` links `taskdt`, which links `clinkages` — plus the
SLOW-32 build of `dateutil.c`. GnuCOBOL's single `MAJESTY.so` holding
every program is a packaging choice of `cobcrun -M`, not a language
fact; see [majesty-corpus.md](majesty-corpus.md).
