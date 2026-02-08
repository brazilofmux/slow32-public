# SLOW BASIC — Implementation Plan

## Context

MY-BASIC (basic/) served its purpose — it found the JALR eviction bug — but it's someone else's dialect. I wanted a custom BASIC that feels like the BASICs they grew up with (CoCo, GWBASIC, QuickBASIC, Business BASIC): recursive-descent parser, no line numbers, labels, real SUB/FUNCTION, optional LET, DATA/READ, stronger typing. SLOW-32 can't do SOUND/COLOR/DRAW, but it has full file I/O, floating-point, and plenty of memory for an interpreter.

**Name**: SLOW BASIC
**Directory**: `sbasic/` (parallel to `forth/`)

## Core Design Decisions

- **Parser**: Recursive-descent. Optional LET works because when the parser sees an identifier followed by `=`, it knows it's assignment.
- **Execution**: Tree-walk interpreter (parse → AST → evaluate). Simple, debuggable, sufficient for BASIC on an emulator.
- **Types**: INTEGER (32-bit), DOUBLE (64-bit IEEE float), STRING (heap-allocated). Type suffixes: `%`=int, `#`=double, `$`=string. Bare names default to double.
- **Strings**: Reference-counted. No circular refs possible in BASIC, so refcount is sufficient — no mark-sweep needed.
- **Scope**: Local by default in SUB/FUNCTION. `SHARED` keyword to access globals. Variables auto-create with default values (0 or "").
- **Errors**: No setjmp/longjmp on SLOW-32. Every eval function returns an error code, checked via `EVAL_CHECK()` macro.
- **Build**: Per-file compilation at `-O1` (avoids LLC hangs), linked with `libc_mmio.s32a` + `--mmio 64K`.

## Source Layout

```
sbasic/
  src/
    main.c        Entry point, REPL, file loading
    lexer.h/c     Tokenizer (case-insensitive keywords, type suffixes)
    ast.h/c       AST node types, constructors, destructors
    parser.h/c    Recursive-descent parser
    value.h/c     Value types, string refcounting, type coercion
    env.h/c       Scope chain (local/global/shared lookup)
    eval.h/c      Tree-walk evaluator
    builtin.h/c   Built-in functions (ABS, LEN, LEFT$, etc.)
    error.h/c     Error codes, formatting, line numbers
    array.h/c     Arrays (Stage 3)
    format.h/c    PRINT USING engine (Stage 4)
    fileio.h/c    File I/O handles (Stage 5)
    type.h/c      TYPE records (Stage 6)
  build.sh        Auto-discovers src/*.c, compiles each, links
  tests/          Test .bas programs + .expected output per stage
```

## Staged Implementation

### Stage 1: Core BASIC ✅ COMPLETE

The minimum to feel like BASIC. After this stage, you can write real programs.

**Language features:**
- `PRINT` with `;` (concatenate), `,` (tab zones), expressions
- `INPUT "prompt"; var`
- Variables: `x = 10`, `LET x = 10`, `name$ = "hello"`, `count% = 5`
- Arithmetic: `+ - * / \ MOD ^`, string concatenation with `+`
- Comparison: `= <> < > <= >=`
- Logical: `AND OR NOT`
- `IF ... THEN ... [ELSE ...] END IF` (block form)
- `IF ... THEN statement` (single-line form)
- `FOR var = start TO end [STEP step] ... NEXT`
- `WHILE expr ... WEND`
- `REM` and `'` comments, `END`
- Built-ins: `ABS INT SGN SQR LEN CHR$ ASC STR$ VAL LEFT$ RIGHT$ MID$`
- REPL: `RUN LIST NEW LOAD SAVE BYE`

**Tests**: hello.bas, fizzbuzz.bas, strings.bas, arithmetic.bas, loops.bas, ifelse.bas (6/6 passing)

### Stage 2: Program Structure ✅ COMPLETE

Structured programming: real procedures, richer control flow.

**Language features:**
- `SUB name(params) ... END SUB`
- `FUNCTION name(params) ... END FUNCTION` (return via `name = expr`)
- `CALL sub(args)` or just `sub args`
- Labels: `label_name:`
- `GOTO label`, `GOSUB label / RETURN`
- `DO [WHILE|UNTIL expr] ... LOOP [WHILE|UNTIL expr]`
- `SELECT CASE expr / CASE val / CASE IS < val / CASE v1 TO v2 / CASE ELSE / END SELECT`
- `EXIT FOR/WHILE/DO/SUB/FUNCTION`
- `CONST name = value`
- Multi-statement lines with `:`
- `SHARED var1, var2$` in SUB/FUNCTION
- `DECLARE SUB/FUNCTION` (forward declarations)

**Key design**: SUB/FUNCTION definitions collected in a first pass before execution. GOTO/GOSUB use label-to-statement-index maps. Block evaluator uses index-based `pc` for GOTO support.

**Tests**: subs.bas, doloop.bas, select.bas, gotogosub.bas, constshared.bas, exitloop.bas (12/12 total passing)

### Stage 3: Arrays and DATA (~1,400 lines added) ✅ COMPLETE

**Language features:**
- `DIM var(size)`, `DIM var(r, c)` (multi-dimensional, up to 8D)
- `DIM var(size) AS INTEGER/DOUBLE/STRING`
- `OPTION BASE 0/1`
- `ERASE var`, `REDIM var(n)`, `REDIM PRESERVE var(n)`
- `LBOUND(var)`, `UBOUND(var)`
- `DATA val1, val2, "str", ...`
- `READ var1, var2$`
- `RESTORE` / `RESTORE label`

**Key design**: DATA values collected into a flat pool during the first pass of eval_program. `data_read_ptr` advances through the pool. RESTORE resets it (optionally to a label's data_offset). Arrays stored in a global table (MAX_ARRAYS=64) as flat row-major value arrays with dimension metadata.

**Files**: array.h/c (new), updated error.h/c, lexer.h/c, ast.h/c, parser.c, eval.c

**Tests**: arrays.bas, dataread.bas (14/14 total passing)

### Stage 4: Rich Standard Library (~1,750 lines added) ✅ COMPLETE

**String functions**: `INSTR UCASE$ LCASE$ LTRIM$ RTRIM$ TRIM$ SPACE$ STRING$ HEX$ OCT$ REPLACE$`

**Math functions**: `SIN COS TAN ATN ATN2 EXP LOG LOG10 FIX CINT CDBL RND RANDOMIZE` (all trig/exp/log use inline Taylor series — no libc math dependency)

**Formatting**: `PRINT USING "format"; expr; expr` with `# . + - $$ ** ^^^^` for numbers, `\ \ ! &` for strings. Custom format engine avoids broken `%*.*f` varargs on SLOW-32.

**Other**: `TAB(n) SPC(n) TIMER SWAP`

**Files**: updated builtin.h/c, lexer.h/c, ast.h/c, parser.c, eval.c

**Tests**: stringfuncs.bas, mathfuncs.bas, printusing.bas (17/17 total passing)

### Stage 5: File I/O (~1,640 lines added) ✅ COMPLETE

- `OPEN file$ FOR INPUT/OUTPUT/APPEND AS #n`
- `CLOSE #n`, `PRINT #n`, `INPUT #n`, `LINE INPUT #n`, `WRITE #n`
- `EOF(n)`, `FREEFILE()`
- `NAME old$ AS new$`, `KILL file$`

**Files**: fileio.h/c (new), updated error.h/c, lexer.h/c, ast.h/c, parser.c, eval.c, builtin.c

**Tests**: fileio.bas (18/18 total passing)

### Stage 6: Advanced Features (~1,640 lines added) ✅ COMPLETE

- `TYPE name / field AS type / END TYPE` with `var.field` access
- `DIM var AS TypeName` — creates typed record instance
- `ON ERROR GOTO label / RESUME / RESUME NEXT`
- `ERR`, `ERL`, `ERROR n`
- `DEFINT A-Z`, `DEFDBL`, `DEFSTR` (statement parsed; runtime type map maintained)
- Line continuation with `_`
- `ON expr GOTO/GOSUB label1, label2, ...`

**Files**: updated error.h/c, lexer.h/c, ast.h/c, parser.c, eval.c

**Tests**: types.bas, onerror.bas, advanced.bas (21/21 total passing)

## Cumulative Size

| Stage | Delta | Total | Milestone |
|-------|-------|-------|-----------|
| 1 | ~4,500 | ~4,500 | Feels like BASIC ✅ |
| 2 | ~1,700 | ~6,200 | Real programs ✅ |
| 3 | ~1,400 | ~7,600 | Data structures ✅ |
| 4 | ~1,750 | ~9,350 | Rich library ✅ |
| 5 | ~1,640 | ~11,000 | File I/O ✅ |
| 6 | ~1,640 | ~12,600 | Feature complete ✅ |

## Verification

After each stage:
1. `cd sbasic && bash build.sh` — must compile cleanly
2. `printf 'PRINT "hello"\nRUN\n' | ../tools/emulator/slow32-fast sbasic.s32x` — smoke test
3. Run stage test programs: `bash tests/run-tests.sh`
4. Compare output against `.expected` files
5. Run previous stage tests (no regressions)

## Implementation Order

Start with Stage 1. Each stage is a complete, working interpreter. We implement, test, and verify each stage before moving to the next. The build script auto-discovers `src/*.c` so adding files in later stages requires no build changes.
