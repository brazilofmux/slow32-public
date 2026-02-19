# Stage 12: New Self-Hosting C Compiler

## Architecture Overview

A multi-pass compiler replacing stage11's single-pass emit-as-you-go design.
The goal is proper separation of concerns: each layer has a single job,
making the compiler easier to extend, optimize, and debug.

```
Source code
    │
    ▼
┌─────────────────────┐
│  Scanner/Lexer       │  Ragel -G2 (DONE)
│  c_lexer.rl          │  68 token types, 1278 lines raw output
└─────────┬───────────┘
          │ token stream
          ▼
┌─────────────────────┐
│  Parser              │  Recursive descent (no LALR)
│  Produces AST        │  malloc'd tree nodes
└─────────┬───────────┘
          │ AST
          ▼
┌─────────────────────┐
│  Attribute Grammar   │  Single traversal over AST
│  + Type Checking     │  Compute types, sizes, offsets
│  + Semantic Analysis │  Check errors, resolve symbols
└─────────┬───────────┘
          │ attributed AST
          ▼
┌─────────────────────┐
│  AST → HIR           │  High-level IR
│                      │  Structured control flow (if/while/for)
│                      │  Source-level operations
└─────────┬───────────┘
          │ HIR
          ▼
┌─────────────────────┐
│  HIR Optimizations   │  Constant folding, dead code elimination,
│                      │  inlining, loop-invariant code motion
└─────────┬───────────┘
          │ optimized HIR
          ▼
┌─────────────────────┐
│  HIR → MIR           │  Medium-level IR
│                      │  Basic blocks, explicit control flow graph
│                      │  SSA form (φ-nodes)
│                      │  Virtual registers (unlimited)
└─────────┬───────────┘
          │ MIR (SSA)
          ▼
┌─────────────────────┐
│  MIR Optimizations   │  SSA-based: copy propagation, CSE, GVN,
│                      │  dead store elimination, strength reduction
│                      │  CFG: unreachable block elimination
│                      │  Dataflow: liveness, reaching definitions
└─────────┬───────────┘
          │ optimized MIR
          ▼
┌─────────────────────┐
│  MIR → LIR           │  Low-level IR
│                      │  Instruction selection (pattern matching)
│                      │  Virtual registers → machine operations
│                      │  SLOW-32 specific: lui/addi pairs, etc.
└─────────┬───────────┘
          │ LIR
          ▼
┌─────────────────────┐
│  LIR Optimizations   │  Register allocation (linear scan or graph)
│  + Register Alloc    │  Peephole optimizations
│                      │  Stack frame layout
└─────────┬───────────┘
          │ allocated LIR
          ▼
┌─────────────────────┐
│  Assembly Emission   │  Emit SLOW-32 .s text
│                      │  Labels, directives, relocations
└─────────────────────┘
```

## Implementation Phases

We do NOT build all layers at once. We grow the compiler incrementally,
always keeping it working end-to-end.

### Phase 1: Narrow Spike (AST + tree-walk codegen)

Prove the architecture works with the smallest useful subset.

**Subset of C:**
- `int` variables (global and local)
- Integer literals
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Assignment: `=`
- `return` statement
- `if`/`else`
- `while` loops
- Function definitions and calls (no varargs)
- `void` return type

**Layers active:**
- Lexer: Ragel (done)
- Parser → AST: recursive descent
- Codegen: direct AST tree-walk → assembly (skip all IR layers)

**No HIR/MIR/LIR yet.** Tree-walk codegen is the simplest way to get
working output. It generates terrible code, but it works.

**Exit criteria:** Compile and run a program with functions, if/else,
while, arithmetic. Maybe factorial or fibonacci.

### Phase 2: Pointers, Arrays, Structs

Extend the AST and tree-walk codegen to handle:
- Pointer types and dereferencing (`*p`, `&x`)
- Arrays and subscripting (`a[i]`)
- `char` type
- Structs (definition, member access, `.` and `->`)
- String literals
- `for` loops, `do`/`while`
- `break`, `continue`
- Type casting
- `sizeof`

**Exit criteria:** Can compile non-trivial C programs (e.g., string
manipulation, linked list traversal).

### Phase 3: Attribute Grammar + Type System

Add a proper type system via attribute grammar traversal:
- Type inference and checking in single AST pass
- Struct layout computation (offsets, sizes, alignment)
- Implicit type conversions (char → int promotion)
- Pointer arithmetic scaling
- Function signature checking
- `typedef` support
- `enum` support

**Exit criteria:** Type errors caught at compile time. Struct member
access generates correct offsets.

### Phase 4: Preprocessor

Handle the subset of cpp that s32-cc needs:
- `#define` (object-like macros with integer values)
- `#include` (textual inclusion)
- `#ifdef` / `#ifndef` / `#else` / `#endif`
- `#line` (silently skip)

This can be a separate pass before the lexer, or integrated into the
token stream (like stage11's approach).

### Phase 5: AST → HIR

Introduce the first IR layer:
- HIR nodes: if/while/for as structured control flow
- Temporaries for sub-expressions
- Explicit address-of for lvalues
- Function calls with explicit argument marshalling
- Global/local variable references as named entities

**Exit criteria:** AST → HIR → tree-walk codegen produces same results.

### Phase 6: HIR → MIR (SSA)

Lower structured control flow to CFG:
- Basic blocks with explicit predecessors/successors
- φ-nodes at join points
- SSA construction (dominance frontiers or simple algorithm)
- Virtual registers (infinite supply)
- Explicit loads/stores for memory operations

**Exit criteria:** Correct SSA form. Programs still produce same output.

### Phase 7: MIR Optimizations

SSA enables clean dataflow analysis:
- Copy propagation
- Constant propagation and folding
- Common subexpression elimination (CSE)
- Dead code elimination
- Simple strength reduction (multiply by power-of-2 → shift)

**Exit criteria:** Optimized MIR produces same results, smaller/faster code.

### Phase 8: MIR → LIR (Instruction Selection)

Map MIR operations to SLOW-32 instructions:
- Pattern matching: add(reg, imm) → addi
- Large constants: lui + addi pairs
- Comparison → slt/seq/sne/etc.
- Memory ops: ldw/stw/ldb/stb with offset
- Function calls: argument registers r3-r10, return in r1-r2

**Exit criteria:** LIR is a sequence of SLOW-32 instructions with
virtual registers.

### Phase 9: Register Allocation

Map virtual registers to physical registers:
- Linear scan allocator (simpler than graph coloring)
- Spill to stack when registers exhausted
- Callee-save/caller-save conventions
- Special registers: r0=zero, r29=sp, r30=fp, r31=lr

SLOW-32 has 32 registers with r1-r28 usable, so register pressure
is low. Linear scan should work well.

**Exit criteria:** All virtual registers mapped to physical registers.
Programs still produce correct output.

### Phase 10: LIR Optimizations + Assembly Emission

Final polish:
- Peephole optimizations (redundant loads, dead stores)
- Branch optimization (remove jumps to next instruction)
- Stack frame layout and prolog/epilog generation
- Emit .s assembly text

**Exit criteria:** Complete compiler pipeline working end-to-end.

### Phase 11: Self-Hosting

The ultimate goal: stage12 compiler compiles itself.
- Incrementally add C features needed by the compiler's own source
- Fixed-point proof: gen2.s == gen3.s
- Performance comparison with stage11

## SLOW-32 Target Notes

Things that make this target easier than most:
- 32 registers (generous — less spilling)
- No instruction ordering constraints (no pipeline hazards)
- Simple addressing modes (reg + immediate only)
- No condition codes (comparisons produce values in registers)
- 32-bit only (no 64-bit complexity in the compiler itself)
- Fixed-width 32-bit instructions

Things to watch:
- 12-bit immediate range (-2048 to +2047) — need lui+addi for larger
- No barrel shifter for variable shifts (but sll/srl/sra exist)
- 8 argument registers (r3-r10) — stack spill for args 9+
- W^X memory protection (code segment is execute-only)

## AST Node Design (Phase 1)

Nodes are malloc'd structs. Each node has a `kind` field and
type-specific fields. Using a flat struct with a union-like
layout (since s32-cc doesn't have unions, we use separate fields
and only access the ones relevant to each kind).

```
Node kinds (Phase 1):
  ND_NUM        - integer literal (val)
  ND_VAR        - variable reference (name, offset, is_global)
  ND_BINOP      - binary operation (op, lhs, rhs)
  ND_UNARY      - unary operation (op, operand)
  ND_ASSIGN     - assignment (lhs, rhs)
  ND_CALL       - function call (name, args[])
  ND_RETURN     - return statement (expr)
  ND_IF         - if/else (cond, then_body, else_body)
  ND_WHILE      - while loop (cond, body)
  ND_BLOCK      - compound statement (stmts[])
  ND_FUNC       - function definition (name, params[], body, locals[])
  ND_PROGRAM    - top-level (funcs[], globals[])
```

## File Organization

```
stage12/
  c_lexer.rl          Ragel source
  c_lexer_gen.c        Generated lexer (raw ragel -G2)
  gen_lexer.sh         Regenerate lexer
  ast.h                AST node types and constructors
  parser.h             Recursive-descent parser → AST
  codegen.h            Tree-walk codegen (Phase 1) / IR pipeline (later)
  s12cc.c              Main driver (read source, lex, parse, codegen, write)
  run-tests.sh         Test runner
  tests/
    lex_test.c         Lexer token tests (95 tests, passing)
    test_duff.c        Duff's device test (passing)
    test_ast_*.c       AST/parser tests (Phase 1+)
```

## Bootstrapping Strategy

- Stage 11 s32-cc compiles stage 12 source code
- Stage 12 compiler initially targets the same subset as stage 11
- Once stage 12 can compile itself, it becomes self-hosting
- We iterate between stage 11 (fix/extend) and stage 12 (build up)
- Stage 11 is the safety net — always a working compiler to fall back to

## Constraints from s32-cc (bootstrap compiler)

Stage 12 source must be compilable by stage 11's s32-cc:
- No block-scoped declarations (vars at function top only)
- No unions (use flat structs with unused fields per kind)
- No string/array initializers
- No function-like macros
- 8 argument limit per function call
- Single merged source via #include
- No unsigned type tracking
