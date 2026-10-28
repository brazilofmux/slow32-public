# Types and the attribute grammar (stage08 front end)

Status: **plan**, 2026-10-28.  Step S1 is next.  The C11 survey
(`selfhost/c-conformance/`) is the measure of progress; its
`probe.sh --check` gates every step.

## Why

A type in the front end is one `int`: twelve bits of base type (int,
char, ..., or a struct index), a pointer depth added as `TY_PTR`
multiples, and an unsigned flag.  It cannot say `const`, an array and
its length, a function and its parameters, a pointer to an array, or
`_Bool`.  Those live in side tables beside the declaration
(`ps_type_arrcount`, `ps_type_fpbase`, `ps_lvolb`, `arr_cols`, ...) or
nowhere.  Most of the survey's findings trace to that:

- **Checks that cannot be written** (most of the 137 ACCEPTS): nothing
  can tell `const int *` from `int *`, or what arguments a call needs.
- **Wrong code** (W2, W11-W13): `_Bool` is a typedef of `unsigned
  char`; `sizeof(int (*)[3])` is 12; a row of a 2-D array is an `int *`,
  so `2[m][1]` scales by 4; bit-fields promote by their storage type.

`sema.h` is described as an L-attributed tree walk, and is one, but
its only attribute is that `int`, and expression types are set half in
the parser as nodes are built, half patched in sema.  This plan gives
the walk a real attribute domain and makes it the one place where
expression types are decided and constraints are checked.

## Two type systems

The survey of raw encoding uses (2026-10-28) found 560 sites; 350 of
them are in the four code generators (SLOW-32, x86-64, AArch64), which
see HIR, not C.  A machine type -- int, char, short, long long, float,
double, pointer, signed or unsigned -- is what HIR needs, and the
existing `int` encoding says exactly that.  So:

- **C types** (`cty`): a handle into an interned table, built by the
  parser for declarations and by sema for expressions.  Everything the
  standard talks about.
- **Machine types** (`ty`, today's encoding): what HIR and the back ends
  use.  `ct_mty(cty)` derives one from the other.  The back ends are
  not touched.

### The C type table

    handle = (index << 3) | qualifiers      const 1, volatile 2, restrict 4
                                            (_Atomic: a kind, see below)

    ct_kind[i]    VOID BOOL CHAR SCHAR UCHAR SHORT USHORT INT UINT LONG
                  ULONG LLONG ULLONG FLOAT DOUBLE LDOUBLE ENUM
                  PTR ARRAY FUNC STRUCT UNION ATOMIC
    ct_base[i]    PTR: pointee; ARRAY: element; FUNC: return; ATOMIC: the type
    ct_count[i]   ARRAY: length (-1 incomplete); STRUCT/UNION/ENUM: tag index
    ct_parm[i]    FUNC: first entry in ct_plist (a parameter-type list)
    ct_nparm[i]   FUNC: count; -1 when declared without a prototype
    ct_flags[i]   FUNC: variadic
    ct_size[i], ct_align[i]   cached

Records are hash-consed: one record per distinct unqualified type, so
two unqualified types are the same type iff their handles are equal,
and qualification is a mask on the handle (`ct_unqual(t) = t & ~7`).
"Compatible" (6.2.7) is then a short recursive comparison only where
it differs from identity: incomplete arrays, functions without
prototypes, enum against its integer type.

The table follows the tools' style: parallel global arrays growing
through `s32vec.h` (no ceiling), ints and `char *` only, so stage07
compiles it.

## The attribute grammar

One walk over each function body after it is parsed (as today), owned
by sema.  Inherited attributes flow down, synthesized ones up; every
check is a predicate on attributes at one node.

**Synthesized**, on every expression node:

| attribute | meaning |
|---|---|
| `cty` | the expression's C type, before lvalue conversion |
| `lv` | is an lvalue (6.3.2.1p1) |
| `mod` | is a modifiable lvalue: lvalue, not const-qualified (recursively, for struct members), not an array, not incomplete |
| `npc` | is a null pointer constant (6.3.2.3p3) |
| `icx`, `cval` | is an integer constant expression (6.6p6), and its value (64-bit) |
| `acx` | is an arithmetic or address constant (6.6p7-9), for initializers |

**Inherited**:

| attribute | meaning |
|---|---|
| `ctx` | how the parent uses the value: rvalue, lvalue (`&`, `++`, left of `=`), `sizeof`/`_Alignof` operand (no decay), condition |
| `want` | the type it is converted to "as if by assignment" (6.5.16.1): the parameter type at a call, the declared type in an initializer, the return type at `return` |

**Conversions are made explicit.**  Where the standard converts --
lvalue conversion and array/function decay (6.3.2.1), the integer
promotions and usual arithmetic conversions (6.3.1.1, 6.3.1.8),
conversion as if by assignment -- sema inserts an `ND_CAST` with the
target type, so the lowering never infers a conversion again.  That
is where W2 (`_Bool`: conversion to it is `!= 0`, 6.3.1.2), bit-field
promotion (W12) and the `sizeof` result types fall out.

**Equations, by node** (the full table lands with S2; the shape):

| node | `cty` | checks (clause) |
|---|---|---|
| var | declared type | declared (6.5.1p2) |
| `a[i]` | element of `*(a + i)` | one pointer to complete object, one integer (6.5.2.1p1) |
| call | return type | callee is a function or pointer to one; with a prototype: argument count, each argument assignable to its parameter (6.5.2.2p2) |
| `.` `->` | member's type, plus the base's qualifiers | struct/union (pointer) operand, member exists (6.5.2.3p1-2) |
| `++` `--` `=` `op=` | the lvalue's unqualified type | operand `mod` (6.5.2.4p1, 6.5.3.1p1, 6.5.16p2) |
| `&` | pointer to operand | function designator, `[]`/`*` result, or lvalue not bit-field / register (6.5.3.2p1) |
| `*` | pointee | operand has pointer type (6.5.3.2p2) |
| `+` `-` | usual conversions, or pointer | arithmetic, or one pointer to complete object and one integer, or (`-`) two compatible pointers (6.5.6p2-3) |
| shifts, `%`, `&` `^` `|`, `~` | promoted left / usual | integer operands (6.5.7p2, 6.5.5p2, 6.5.10-12) |
| relational, equality | `int` | arithmetic, compatible pointers, or pointer and null (6.5.8p2, 6.5.9p2) |
| `?:` | 6.5.15p3-6 | scalar condition; operand pairs as listed |
| `=` | left, unqualified | 6.5.16.1p1's six cases: arithmetic, compatible struct, pointers with the left's qualifiers a superset, `void *`, null constant, `_Bool` from pointer |
| cast | the named type | scalar to scalar, or `void` (6.5.4p2-3) |

## Source locations

A diagnostic is only as good as its line.  Nodes carry none today, and
the lexer's line counts through every included header (the survey's
W15: `__LINE__` after an `#include` is wrong, and so are stage08's own
error lines).  The preprocessor will keep a map from its output
position to (file, line) -- the `#line` and `#include` bookkeeping
`__FILE__`, `__LINE__` and `#line` need anyway -- and `nd_new` records
the position.  Errors read `file:line: error: ...`.

## Steps

Every step is gated: stage08's suite with the fixed point, and
`probe.sh --check` (no probe worse).  Steps marked *neutral* must also
compile `s12cc.c` and `sqlite3.c` to the same bytes as before.

- **S1** *neutral*.  The table, its constructors, `ct_mty`.  The parser
  builds a `cty` for every declaration beside the old `ty` (locals,
  globals, parameters, members, typedefs, casts, `sizeof`).  A debug
  check (`S12CC_CTY_CHECK=1`) asserts `ct_mty(cty) == ty` everywhere
  both exist; run over SQLite and the compiler itself, it proves the
  new construction against the old one before anything reads it.
- **S2** *neutral*.  Sema computes the synthesized attributes for every
  expression; parser-side expression typing moves into sema.  The same
  assertion, now over every expression.
- **S3**.  The lowering reads `cty` where `ty` cannot say what it must:
  pointer arithmetic and subscripts scale by the element's size, `sizeof`
  and `_Alignof`, conversion to `_Bool`, bit-field promotion.  The
  explicit conversions go in.  Expected: W2, W11, W12 and the 2-D `2[m]`
  turn PASS; SQLite and the corpus unchanged.
- **S4**.  Source locations: the preprocessor's line map, `__LINE__`,
  `__FILE__`, `#line`, and `file:line:` on every diagnostic.
- **S5**.  The checks.  Constraint violations are errors, as clang's
  `-pedantic-errors` makes them (the probes' oracle); the tree's own C
  (SQLite, DOOM, the libcs, the tools, the apps) must pass them, and
  whatever it trips on is examined one at a time -- a real bug is fixed
  where it is, an extension in use is a ruling recorded here.  Then
  redeclaration and compatible-type checks (6.2.7, 6.7), and
  `_Static_assert` on a real evaluation.
- **S6**.  On the attributes: a typed constant evaluator for
  initializers (W6), `_Generic`, `_Alignof`/`_Alignas`, `__func__`,
  pointer-to-array declarators, function-typed parameters.

## Constraints on the work

- stage07 builds stage08: the new code keeps stage07's dialect (ints
  and `char *`, no block-scoped arrays, no `void *` returns) and goes
  into new functions rather than growing `hl_expr`, `hl_stmt` or
  `parse_top_decl` -- stage07 miscompiled a grown `hl_expr`
  (ISSUES-100).
- `src/` is shared with the cross compilers: `check-host-frontend.sh`
  after every change.
- Aesthetic: one way to do each thing.  Side tables the table replaces
  go when their last reader does, not left beside it.
