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

The table is `src/ctype.h`: parallel global arrays of fixed capacity,
as the compiler's other tables (an error when one fills), ints and
`char *` only, so stage07 compiles it.

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

## Progress and findings

**S1** (2026-10-28, d54ecf5e): C types on every declaration and variable
node; `S12CC_CTY_CHECK=1` lists declarations whose C type the parser
cannot yet build (none in the compiler's source; 225 in SQLite, mostly
function-pointer objects, whose machine type is `int`).

**S2, first part** (2026-10-28): sema computes every expression's C
type by the 6.3/6.5 rules (`sema_cty_node`), and the same check now
reports where that C type's machine type differs from the one the node
carries -- where the old typing departs from C.  Over SQLite, after the
known families are set aside, every remaining case is one of these:

| departure | count in SQLite | effect |
|---|---|---|
| a comparison's machine type is `unsigned int` when its operands compare unsigned: the bit says how to compare, not what the result is | ~1,660 | the bit leaks: `n + (u < v)` is unsigned arithmetic, so `(n + (u < v)) < 0` is never true |
| a pointer difference `p - q` is typed as the pointer | ~85, and what is computed from them | the value is right (the lowering divides); its type is not -- `(p - q) & 7`, `sizeof (p - q)` |
| a function name is typed `int` | 176 | the designator has no type to check against |
| `&a` on an array is a pointer to the element's pointer | 1 | `&a + 1` steps by a pointer, not by the array |
| `c ? s1 : s2` on two structs is `int` | 1 | (and `(c ? s1 : s2).m` is refused) |

These are S3's list: when the lowering reads C types, each becomes
right by construction.  Output was byte-identical throughout.

**S2, second part** (2026-10-28): C types for struct members (qualified
by their structure's qualifiers), casts, constants, and functions --
`ps_fpctypes` beside every stored parameter list, `ps_fcty` per
function, so a call has its callee's return type and a function name
its function type; function-pointer declarators are pointers to
functions (`ct_mty_agrees`: their machine type is spelled `int` or
`int *`).  Global arrays sized by their initializer get their length.
Over SQLite, nodes whose C type is rebuilt from the machine type fell
from 84,000 to under 300, and declarations the parser cannot type from
661 to 105, the rest scattered over rarer paths.

**S2, attributes** (2026-10-28): `attr` on every expression node --
`AT_LV` lvalue, `AT_MOD` modifiable lvalue (not an array, function,
const, incomplete, or a struct with a const member at any depth),
`AT_NPC` null pointer constant, `AT_ICX` integer constant expression
with its value in `cval`/`cval_hi`, evaluated in 64 bits and converted
to the expression's type (6.6p6: casts of floating constants included;
division by zero and shifts past the width are not constant).  Checked
against the AST optimizer, an evaluator written apart: after
`optimize()`, every node it folded must hold sema's value -- 0
disagreements over SQLite and the compiler itself.  This step found
selfhost ISSUES-101: three more ints in `Node` changed SQLite's output,
because dead-static removal gave up silently when the heap was full.

**S3, `_Bool`** (2026-10-28): conversion to `_Bool` is a test against 0
(6.3.1.2), decided by the C type -- the machine type cannot tell `_Bool`
from `unsigned char`.  sema writes it as `e != 0` at assignment,
initialization, casts, `_Bool` parameters and returns; the lowering
normalizes the values it computes itself (compound assignment, `++`,
`--`); the parser's constant evaluator and static initializers do the
same; `<stdbool.h>` defines `bool` as `_Bool` (7.18), where it was a
typedef of `unsigned char`.  The nine W2 probes pass; SQLite and the
compiler, which use no `_Bool`, compile to the same bytes.

**S3, comparisons** (2026-10-28): a comparison's type is `int`; whether
its operands compare unsigned is the operation's (`AT_UCMP`), read by
the lowering (`hl_cmp_ty`) and the AST folder.  The unsigned bit had
been the result's, and leaked into the arithmetic and comparisons
around it.  The rule for which comparisons are unsigned is unchanged
by design, and is itself a finding for later: for pointers it follows
the pointee's signedness, so `char *` comparisons are signed -- harmless
while every address is below 2^31, but not C (6.5.8p5 compares
addresses).  Disagreements over SQLite: 1,977 -> 310.  SQLite's code
changed in four functions, each a comparison result feeding a
comparison or a 64-bit widening; probe `6.5.8-result-is-int` pins it.

**S3, pointer differences** (2026-10-28): `p - q` is `ptrdiff_t`
(6.5.6p9), signed and pointer-wide, where it was typed as the pointer;
on `u8 *` operands `(p - q) / 2` divided unsigned and `(p - q) < 0`
could not be true.  SQLite's code changed in six functions, each a
difference widened to 64 bits (now sign- rather than zero-extended);
probe `6.5.6-ptrdiff-signed`.  Disagreements: 310 -> 199.

**S3, `sizeof`** (2026-10-28): `sizeof expr` runs sema on its operand
and, when the operand is a value an operator computed, takes its C
type's size -- `sizeof(+c)`, `sizeof(d + d)`, `sizeof(1 << ll)` (W12);
unary `+` now promotes.  An object operand (an lvalue, an array, a
function) keeps the size its declaration recorded, because the
declarations' C types are still being completed, and twice in this step
a wrong C type became a wrong size: a `(**name)` declarator was typed
with one pointer, so in SQLite `sizeof(aExt[0])` became 1 and an
allocation lost its `* 4` -- no gate exercises that path; it was caught
reading the assembly diff -- and a global array of function pointers
was typed as a pointer, so `sizeof one / sizeof one[0]` was 1.  Both are
fixed (`ps_fnptr_cty` counts stars, the array gets its type), with a
probe.  **Rule for the rest of S3: read every assembly diff in SQLite
before trusting a switch to C types; a gate that passes does not cover
the code paths a probe never wrote.**

**S3, bit-field promotion** (2026-10-28): a bit-field promotes by its
width, not its declared type (6.3.1.1p2): narrower than `int` it is an
`int`, as wide an unsigned one is `unsigned int`; a wider (long long)
one keeps its type -- clang's reading of what 6.3.1.1 leaves to the
implementation, `nd_bf_promoted`.  Sema writes the promotion as a cast
on each operand that promotes (arithmetic, comparison, `?:` arms, unary
`-`/`~`, a variadic argument), and unary `+` takes it in the parser, so
`x.u - 2 < 0` on an `unsigned u : 4` is true at last.  The member node
keeps its declared type: that is what the load and the store read.
SQLite changed in two compares (a 2-bit and a 4-bit field against 2,
now signed; the same answer for every value they hold); the compiler
itself, byte-identical.  Probes `6.3.1.1-bitfield-promote`,
`6.7.2.1-bitfield-promote`, `6.3.1.1-bitfield-widths`.

**S3, bit-field layout** (2026-10-28, W13): bit-fields are laid out as
clang lays them out, so a structure is the same bytes to both compilers
-- the next free bit unless a field would cross a boundary its type is
aligned to, `:0` to the next boundary, bits sharing bytes with the
members beside them, alignment from named fields only (`bf_alloc`, one
bit cursor where three words of unit state were).  The old layout
opened a fresh unit whenever the type changed or a field did not fit:
`{unsigned x : 3; _Bool f : 1;}` was 8 bytes to clang's 4.  Checked on
SQLite by a generated program that prints, for every structure with
bit-fields (62 of its 73 fields), each member's offset and the bytes
each field occupies when set to all ones: 134 lines differed from
clang's build, now none that bit-fields cause.  What is left there is
not layout: stage08 does not define `__STDC_VERSION__`, so SQLite's
`FLEXARRAY` is `[1]` (W15), and a typedef'd `unsigned char[16]` member
is aligned to 4 (`6.7.8-typedef-array-align`).  run-interop-llvm.sh
gains a bit-field structure each side writes and the other reads.

**Typedef'd array members** (2026-10-28): a member whose type is a
typedef'd array is aligned as its element type -- the type was still
spelled as a pointer when the struct parser aligned it, so SQLite's
`yDbMask` (`unsigned char[16]`) sat at 148 in `Vdbe` where clang puts
it at 146 -- and in a union it is an array at all: it was a pointer
there, so a union of one was 4 bytes and storing past them crashed.
SQLite: three offsets in `sqlite3VdbeUsesBtree` and `sqlite3VdbeExec`.
The layout program now differs from clang only where `FLEXARRAY` does.
Probe `6.7.8-typedef-array-align`.

**S3, pointers to arrays** (2026-10-28): the lowering takes a pointer's
step from its C type when the pointee is an array (`hl_pointee_size`):
`int (*q)[3]` is spelled `int *`, so `q + 1`, `q++`, `q -= 2` stepped 4
bytes and `q - p` divided by 4.  `*q` is the array, decayed -- the
address q holds, not a load (`hl_is_array_value`), its machine type the
decayed C type's (`sema_deref_mty`), its `sizeof` the row's.  The
parser's own rows (`a[i]` on a 2-D array, already scaled) are not
asked.  For that the declarations had to carry the type: parameters and
members of `T (*name)[N]` were typed as function pointers (the
declarator goes through that path, and `ps_obj_cty` took its C type,
which "agrees" with int); 2-D globals without an initializer and every
2-D local were registered flat (`ps_local_rows_cty`); and a local
`T (*name)[N]` was refused (`ps_local_rowptr`).  `2[m][1]` falls out.
A 2-D local gets its rows only after its initializer: the stores the
parser writes for one step through it element by element, and the
first build, typing the local first, made `char rows[2][4] = {"ab",
"cde"}` store words a row apart -- stage08's own test_2d_init caught it;
SQLite has no such local, and the probe's int one took another path.
SQLite and the compiler byte-identical: nothing there took these paths.
Probe `6.5.6-pointer-to-array`.

**S3, constant expressions** (2026-10-28, W6): a constant is first
parsed as an expression and typed by sema; when sema finds an integer
constant expression its value is sema's, in the expression's type
(`ps_const_typed` in front of `parse_const_int`), so `0x7FFFFFFF * 2LL`
is a long long and `(unsigned char)300` is 44.  A floating static
initializer is folded by `sema_fp_const` -- literals, integer constants,
casts, unary minus, `+ - * /`, `?:`, a float result rounded at each
step.  What sema does not take (address constants, `&((T *)0)->m`) goes
back to the token evaluator, the lexer rewound (`ps_lex_mark` now
nests).  Lone literals skip the tree: SQLite's tables of them ran the
compiler out of memory.  Found on the way: a float-suffixed literal in a
static initializer stored 0 (`float g = 2.5f`), its token holding float
bits the FP path read as a double's; and no function in the compiler may
return a double -- stage07 cannot, and the first version's helpers came
back 0.  SQLite and the compiler byte-identical.  Probes
`6.6-arith-const-double`, `6.6-arith-const-llong`, `6.6-const-typed`.

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
